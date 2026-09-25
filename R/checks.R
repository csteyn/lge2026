# checks.R --------------------------------------------------------------------
# Data-quality and model checks, returned AS DATA and published on the site.
# A check never silently fixes anything: it reports what it saw, how many
# rows or voters are affected, and whether that is acceptable.
#
# status: pass | warn | fail     severity: critical | major | minor

check_row <- function(id, area, description, n_affected, detail = "", warn_if = n_affected > 0,
                      fail_if = FALSE, severity = "major") {
  tibble(id = id, area = area, description = description, n_affected = as.numeric(n_affected),
         detail = detail, severity = severity,
         status = case_when(fail_if ~ "fail", warn_if ~ "warn", TRUE ~ "pass"))
}

#' Human-confirmed 2026 participation facts (data-raw/manual/party_status_2026.csv)
read_party_status <- function(path) {
  ps <- read_csv(path, comment = "#", col_types = cols(.default = col_character()), show_col_types = FALSE)
  allowed <- c("absent_confirmed", "renamed", "distinct", "disputed")
  bad <- ps |> filter(!status %in% allowed | is.na(evidence) | !nzchar(evidence) |
                        (status %in% c("renamed", "distinct") & (is.na(name_2026) | !nzchar(name_2026))))
  if (nrow(bad)) stop("party_status_2026.csv: rows need a status in {", paste(allowed, collapse = ", "),
                      "}, evidence, and name_2026 when renamed or distinct. Problem rows: ",
                      paste(bad$muni_code, bad$party, collapse = "; "), call. = FALSE)
  ps |> mutate(party = party_key(party), name_2026 = party_key(name_2026))
}

#' Split C15's absences into unconfirmed, disputed and confirmed
classify_absences <- function(notable, party_status = NULL) {
  ps <- party_status %||% tibble(muni_code = character(), party = character(), status = character())
  notable |>
    left_join(select(ps, muni_code, party, status), by = c("muni_code", "party")) |>
    mutate(status = coalesce(status, "unconfirmed"))
}

run_checks <- function(inputs, groups, transfer, baseline, summaries, cfg, seat_validation = NULL,
                       party_status = NULL, premium = NULL, entrant_table = NULL, poll_comparison = NULL,
                       entrant_total = NULL) {
  lge <- inputs$lge2021
  imp_turnout <- lge |> filter(ballot == "PR") |> distinct(muni_code, vd, registered, spoilt, vd_valid) |>
    filter((vd_valid + spoilt) / registered > 1.05)
  dup <- lge |> count(vd, ballot, party) |> filter(n > 1)
  unknown_ballot <- lge |> filter(str_starts(ballot, "UNKNOWN")) |> distinct(ballot)

  in_new <- inputs$vd2026$vd
  leak <- inputs$npe2024 |> summarise(v = sum(votes), .by = vd) |>
    mutate(lost = !vd %in% in_new)
  imputed <- baseline |> distinct(muni_code, vd, registered, imputed) |> filter(imputed != "none")
  reg_total <- sum(distinct(baseline, vd, registered)$registered)

  national <- groups |> filter(origin == "national") |> distinct(group)
  defaults <- setdiff(national$group, transfer$coefs$group[!transfer$coefs$default_used])

  reg_src <- baseline |> distinct(muni_code, vd, reg_source) |> count(reg_source)
  reg_imp <- baseline |> distinct(muni_code, vd, reg_imputed) |> filter(reg_imputed)
  t_imp <- baseline |> distinct(muni_code, vd, turnout_source) |> filter(turnout_source == "imputed")
  derived_seats <- inputs$councils |> filter(str_detect(seats_source, "DERIVED"))
  seat_ward_mismatch <- inputs$councils |> filter(n_wards != ceiling(total_seats / 2))
  ties <- sum(summaries$meta$remainder_ties)

  out <- bind_rows(
    check_row("C01", "IEC results", "Voting districts with turnout above 105% (dropped from the turnout baseline)",
              nrow(imp_turnout), paste(head(imp_turnout$vd, 10), collapse = ", ")),
    check_row("C02", "IEC results", "Duplicate VD x ballot x party rows", nrow(dup),
              fail_if = nrow(dup) > 0, severity = "critical"),
    check_row("C03", "IEC results", "Unrecognised ballot types", nrow(unknown_ballot),
              paste(unknown_ballot$ballot, collapse = ", "), fail_if = nrow(unknown_ballot) > 0),
    check_row("C04", "Geography", "Share of 2024 votes in voting districts absent from the 2026 layer",
              round(100 * sum(leak$v[leak$lost]) / sum(leak$v), 2),
              "percent of votes; these carry no information into 2026",
              warn_if = sum(leak$v[leak$lost]) / sum(leak$v) > 0.005, severity = "minor"),
    check_row("C05", "Geography", "2026 voting districts with no history (imputed from ward or municipality)",
              nrow(imputed), sprintf("%.1f%% of registered voters", 100 * sum(imputed$registered) / reg_total),
              warn_if = sum(imputed$registered) / reg_total > 0.05, severity = "minor"),
    check_row("C12", "Geography", "2026 voting districts whose registration did not come from the MDB layer",
              nrow(reg_imp), paste(sprintf("%s: %d", reg_src$reg_source, reg_src$n), collapse = "; "),
              warn_if = nrow(reg_imp) > 0,
              fail_if = any(str_detect(reg_src$reg_source, "median") & reg_src$n > 0.05 * sum(reg_src$n)),
              severity = "major"),
    check_row("C13", "Model", "2026 voting districts with no 2021 turnout (imputed)", nrow(t_imp),
              severity = "minor", warn_if = FALSE),
    check_row("C06", "Model", "Modelled national parties with no fitted transfer (local premium drawn from a distribution, L022)",
              length(defaults),
              if (!is.null(premium) && nrow(premium))
                paste(sprintf("%s: premium %.2f ± %.2f (%s)", premium$group, premium$post_mean, premium$post_sd,
                              if_else(premium$wards >= 1 & str_detect(premium$source, "^prior \\+"),
                                      paste(premium$wards, "by-election wards"), "prior only")),
                      collapse = "; ")
              else paste(defaults, collapse = ", "),
              severity = "minor"),
    check_row("C07", "Councils", "Council sizes derived rather than taken from the MEC notice",
              nrow(derived_seats), paste(derived_seats$muni_code, collapse = ", "),
              fail_if = nrow(derived_seats) > 0, severity = "critical"),
    check_row("C08", "Councils", "Councils where wards != ceiling(seats / 2)", nrow(seat_ward_mismatch),
              paste(seat_ward_mismatch$muni_code, collapse = ", "), fail_if = nrow(seat_ward_mismatch) > 0),
    check_row("C09", "Seats", "Simulated councils decided by an exact remainder tie (broken by seeded lot)",
              ties, severity = "minor", warn_if = ties > 0),
    check_row("C10", "Candidates", "Candidate-list table rows that did not parse (quarantined)",
              nrow(inputs$candidate_quarantine %||% tibble()),
              paste0(inputs$candidate_status %||% "candidate lists not parsed",
                     if (is.null(inputs$contests)) "; every party assumed to contest every ward (A07)" else ""),
              warn_if = is.null(inputs$contests) || nrow(inputs$candidate_quarantine %||% tibble()) > 0)
  )

  if (cfg$mode == "live" && !is.null(cfg$expected)) {
    got <- inputs$vd2026 |>
      left_join(select(inputs$municipalities, muni_code, province_code), by = "muni_code") |>
      summarise(wards = n_distinct(ward_id), voting_districts = n_distinct(vd), .by = province_code)
    for (pc in intersect(names(cfg$expected), got$province_code)) {
      e <- cfg$expected[[pc]]; g <- filter(got, province_code == pc)
      off <- abs(g$wards - e$wards) + abs(g$voting_districts - e$voting_districts)
      out <- bind_rows(out, check_row(
        "C11", "Geography", sprintf("%s ward and voting-district counts match the IEC's published totals", pc),
        off, sprintf("wards %d (IEC %d); voting districts %d (IEC %d)", g$wards, e$wards,
                     g$voting_districts, e$voting_districts),
        fail_if = off > 0, severity = "critical"))
    }
  }

  if (!is.null(seat_validation)) {
    sv <- seat_validation
    bad <- sv |> filter(status != "exact")
    out <- bind_rows(out, check_row(
      "C14", "Seats", "Published 2021 councils our allocator does not reproduce exactly (IEC Seat Calculation Detail)",
      nrow(bad), if (nrow(bad)) paste(sprintf("%s: %s", bad$muni_code, bad$detail), collapse = " | ")
                 else sprintf("%d of %d councils exact, including %d with excessive seats", sum(sv$status == "exact"),
                              nrow(sv), sum(nzchar(sv$excessive_iec))),
      fail_if = nrow(bad) > 0, severity = "critical"))
  }

  if (!is.null(inputs$pr_lists)) {
    notable <- bind_rows(filter(inputs$lge2021, ballot == "PR"), inputs$npe2024) |>
      summarise(votes = sum(votes), .by = c(muni_code, party)) |>
      mutate(share = votes / sum(votes), .by = muni_code) |>
      filter(share >= 0.02, party != "INDEPENDENT") |>
      anti_join(inputs$pr_lists, by = c("muni_code", "party"))
    cl <- classify_absences(notable, party_status)
    open_items <- cl |> filter(status != "absent_confirmed")
    fmt <- function(d) paste(sprintf("%s %s (%.0f%%)", d$muni_code, d$party, 100 * d$share), collapse = "; ")
    detail <- paste(c(
      if (any(cl$status == "unconfirmed")) paste("unconfirmed:", fmt(filter(cl, status == "unconfirmed"))),
      if (any(cl$status == "disputed")) paste("DISPUTED (sources disagree, see register):", fmt(filter(cl, status == "disputed"))),
      if (any(cl$status == "absent_confirmed")) sprintf("%d confirmed absent", sum(cl$status == "absent_confirmed"))
    ), collapse = " | ")
    out <- bind_rows(out, check_row(
      "C15", "Candidates",
      "Parties with 2%+ in 2021 or 2024 that are not on a 2026 PR list there, and not confirmed absent",
      nrow(open_items), detail, severity = "major"))
  }

  if (!is.null(inputs$pr_lists)) {
    nm <- near_miss_names(groups, inputs$pr_lists, party_status)
    out <- bind_rows(out, check_row(
      "C17", "Candidates", "Parties marked not standing whose name is close to a 2026 list name (same party? rule in the register)",
      nrow(nm), paste(sprintf("%s ~ %s", nm$party, nm$name_2026), collapse = "; "), severity = "major"))
  }

  if (!is.null(entrant_table) && nrow(entrant_table)) {
    wide_prior_only <- entrant_table |> filter(councils >= 11, !str_starts(basis, "override")) |> distinct(party)
    out <- bind_rows(out, check_row(
      "C18", "Newcomers", "Parties standing in 11+ councils with no history, modelled from the class prior (a sourced override may describe them better)",
      nrow(wide_prior_only),
      sprintf("%d newcomer cases modelled; %s", nrow(entrant_table), paste(wide_prior_only$party, collapse = ", ")),
      severity = "minor"))
  }

  if (!is.null(entrant_total) && nrow(entrant_total$simulated)) {
    et <- entrant_total
    over <- et$simulated |> filter(median_total > et$hist_p95) |> arrange(desc(median_total))
    out <- bind_rows(out, check_row(
      "C20", "Newcomers", "Councils where newcomers' simulated combined share (median) exceeds the 95th percentile of past councils' newcomer totals",
      nrow(over),
      sprintf("history: 95th percentile %.1f%%, maximum %.1f%% (%d council-elections); %s",
              100 * et$hist_p95, 100 * et$hist_max, nrow(et$history),
              paste(sprintf("%s %.1f%%", over$muni_code, 100 * over$median_total), collapse = ", ")),
      fail_if = nrow(over) > 0, severity = "critical"))
  }

  if (!is.null(poll_comparison) && nrow(poll_comparison)) {
    pc <- poll_comparison
    out <- bind_rows(out, check_row(
      "C19", "Polls", "Model vote shares outside a poll's stated margin of error (a cross-check: past Western Cape poll errors were larger than their margins, L041-L042)",
      sum(pc$outside_moe),
      paste(sprintf("%s %s: model %.0f%%, %s %.0f%% (+/-%.1f)", pc$scope, pc$party, 100 * pc$model_median, pc$pollster,
                    100 * pc$share, 100 * pc$moe95), collapse = "; "),
      severity = "minor"))
  }

  exp_c <- cfg$expected
  if (cfg$mode == "live" && !is.null(inputs$candidate_counts) && !is.null(exp_c)) {
    for (pc in names(exp_c)) {
      e <- exp_c[[pc]]
      if (is.null(e$candidates_pr)) next
      got <- setNames(inputs$candidate_counts$n, inputs$candidate_counts$section)
      off <- abs(coalesce(got["PR"], 0L) - e$candidates_pr) + abs(coalesce(got["Ward"], 0L) - e$candidates_ward)
      out <- bind_rows(out, check_row(
        "C16", "Candidates", sprintf("%s candidate rows parsed vs the IEC's published totals", pc), off,
        sprintf("PR %d (IEC %d); ward %d (IEC %d)", got["PR"], e$candidates_pr, got["Ward"], e$candidates_ward),
        warn_if = off > 0, fail_if = off > 5, severity = "major"))
    }
  }

  if (cfg$mode == "demo") {
    rec <- transfer$coefs |> inner_join(DEMO_TRUTH, by = "group", suffix = c("_fit", "_true")) |>
      filter(group != "OTHER")
    worst <- max(abs(rec$b_fit - rec$b_true))
    out <- bind_rows(out, check_row(
      "D01", "Demo", "Transfer slope recovery against the known synthetic truth (max |b_fit - b_true|)",
      round(worst, 3), "slopes are attenuated by noise in the predictor; see MODEL-LOG L003",
      warn_if = worst > 0.1, fail_if = worst > 0.3, severity = "minor"))
  }
  out
}
