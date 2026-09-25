# backtest.R --------------------------------------------------------------------
# Predict the 2021 local election exactly as we predict 2026, using only what
# was known before it, then score the prediction against what happened
# (MODEL-LOG L026).
#
#   role                         2026 forecast        2021 backtest
#   translation learned from     2019 NPE -> 2021 LGE 2014 NPE -> 2016 LGE
#   latest national vote         2024 NPE             2019 NPE
#   previous local election      2021 LGE             2016 LGE
#   who is standing              2026 candidate lists 2021 results (same information)
#   truth                        4 Nov 2026           2021 results and IEC seat calculations
#
# The model never sees any 2021 total. Two lessons from the Johannesburg
# model's backtest shape this: the spatial structure of support transfers
# between cycles but the overall "political weather" does not; and a backtest
# given the real citywide result looks five times better than a blind one.
#
# The same functions run the forecast and the backtest. Only the orchestration
# in run_chain() mirrors _targets.R; if the two drift apart, test-backtest.R
# fails.

empty_swing <- function() tibble(party = character(), scope = character(), mean = numeric(), sd = numeric())

# 1. Inputs ----------------------------------------------------------------------

#' Who stood in the truth election, from its own results (the backtest
#' equivalent of the 2026 candidate lists)
standing_from_results <- function(lge_truth) {
  list(
    pr_lists = lge_truth |> filter(ballot == "PR", votes > 0) |> distinct(muni_code, party),
    contests = lge_truth |> filter(ballot == "Ward", votes > 0) |> distinct(muni_code, ward_id, party)
  )
}

assemble_backtest_inputs <- function(npe_prev, lge_prev, npe_latest, lge_truth, councils,
                                     municipalities, seat_truth = NULL, label = "backtest") {
  # national-election rows belong to a council through their VD number
  vd_muni <- bind_rows(distinct(lge_truth, vd, muni_code), distinct(lge_prev, vd, muni_code)) |>
    distinct(vd, .keep_all = TRUE)
  by_vd <- function(npe) npe |> left_join(rename(vd_muni, m2 = muni_code), by = "vd") |>
    mutate(muni_code = coalesce(m2, muni_code)) |> select(-m2)
  keep <- municipalities$muni_code
  scope <- function(x) filter(x, muni_code %in% keep)

  st <- standing_from_results(scope(lge_truth))
  vd_new <- scope(lge_truth) |> filter(ballot == "PR") |>
    distinct(muni_code, ward_id, vd, registered) |> distinct(muni_code, vd, .keep_all = TRUE)
  list(
    label = label,
    inputs = list(npe_prev = scope(by_vd(npe_prev)), lge_prev = scope(lge_prev),
                  npe_latest = scope(by_vd(npe_latest)), vd_new = vd_new,
                  councils = scope(councils), contests = st$contests, pr_lists = st$pr_lists,
                  municipalities = municipalities),
    truth = backtest_truth(scope(lge_truth), scope(councils), seat_truth)
  )
}

#' What actually happened: seats, ward winners and PR shares
backtest_truth <- function(lge_truth, councils, seat_truth = NULL) {
  winners <- lge_truth |> filter(ballot == "Ward") |>
    summarise(votes = sum(votes), .by = c(muni_code, ward_id, party)) |>
    slice_max(votes, n = 1, by = c(muni_code, ward_id), with_ties = FALSE) |>
    select(muni_code, ward_id, winner = party)
  if (is.null(seat_truth)) { # recompute with the validated allocator (C14)
    seat_truth <- lge_truth |> filter(ballot %in% c("PR", "Ward"), party != "INDEPENDENT") |>
      summarise(votes = sum(votes), .by = c(muni_code, party)) |>
      nest(.by = muni_code) |>
      mutate(alloc = map2(muni_code, data, \(m, d) {
        w <- winners |> filter(muni_code == m, winner %in% d$party) |> count(winner)
        other_w <- sum(winners$muni_code == m) - sum(w$n)
        allocate_seats(setNames(d$votes, d$party), setNames(w$n, w$winner),
                       councils$total_seats[councils$muni_code == m], no_list_wards = other_w) |>
          select(party, seats)
      })) |>
      select(muni_code, alloc) |> unnest(alloc)
  }
  pr <- lge_truth |> filter(ballot == "PR") |>
    summarise(votes = sum(votes), .by = c(muni_code, party)) |>
    mutate(share = votes / sum(votes), .by = muni_code)
  list(seats = select(seat_truth, muni_code, party, seats), winners = winners, pr = pr)
}

# 2. The model chain ----------------------------------------------------------------

#' Everything before the simulation: done once per intercept option.
#' intercepts = "fitted" carries each party's learned local premium `a` into
#' the next cycle; "zero" keeps only the learned spatial structure `b`
#' (the backtest showed the premiums do not transfer between cycles, L027).
prepare_chain <- function(inp, cfg, intercepts = c("fitted", "zero")) {
  intercepts <- match.arg(intercepts)
  groups <- define_party_groups(inp$lge_prev, inp$npe_latest, cfg, inp$pr_lists)
  ow <- other_composition(groups, inp$lge_prev, inp$npe_latest)
  tr <- fit_transfer(inp$npe_prev, inp$lge_prev, groups, cfg)
  if (intercepts == "zero") tr$coefs$a <- 0
  cg <- if (is.null(inp$contests)) NULL else inp$contests |>
    inner_join(select(groups, muni_code, party, group), by = c("muni_code", "party")) |>
    filter(group != "ABSENT") |> distinct(muni_code, ward_id, group)
  base <- build_baseline(inp$npe_latest, inp$lge_prev, inp$vd_new, groups, tr, cg)
  # with zero intercepts there is no learned premium to draw an unfitted party's from
  prem <- if (intercepts == "fitted") local_premium(groups, tr) else NULL
  list(inp = inp, groups = groups, other_w = ow, transfer = tr, baseline = base, premium = prem,
       intercepts = intercepts)
}

simulate_chain <- function(prep, cfg, n_draws = 1000, sd_override = list()) {
  cfg$model$n_draws <- n_draws
  cfg$model$sd <- utils::modifyList(cfg$model$sd, sd_override)
  base <- prep$baseline
  prov <- draw_province_effects(sort(unique(base$group)), empty_swing(), cfg, prep$premium)
  sims <- map(split(base, base$muni_code), \(b) simulate_municipality(
    b, filter(prep$inp$councils, muni_code == b$muni_code[1]), prov, prep$other_w, empty_swing(),
    prep$transfer, cfg))
  c(prep[setdiff(names(prep), "inp")], list(sims = sims, sd_used = cfg$model$sd, n_draws = n_draws))
}

run_chain <- function(inp, cfg, n_draws = 1000, sd_override = list(), intercepts = "fitted") {
  simulate_chain(prepare_chain(inp, cfg, intercepts), cfg, n_draws, sd_override)
}

# 3. Scoring --------------------------------------------------------------------------

crps_sample <- function(x, y) mean(abs(x - y)) - 0.5 * mean(abs(outer(x, x, "-")))

#' Randomised PIT for a discrete outcome: uniform if the forecast is calibrated
pit_discrete <- function(x, y, u = runif(1)) mean(x < y) + u * mean(x == y)

score_chain <- function(chain, truth, seed = 1) {
  set.seed(seed)
  # seats: every party that won seats or had a real chance of one
  seats <- imap_dfr(chain$sims, \(sim, m) {
    s <- sim$seats
    act <- truth$seats |> filter(muni_code == m, seats > 0)
    cand <- union(colnames(s)[colMeans(s > 0) >= 0.05], act$party)
    map_dfr(cand, \(p) {
      x <- if (p %in% colnames(s)) s[, p] else rep(0L, nrow(s))
      y <- coalesce(act$seats[match(p, act$party)], 0L)
      q <- quantile(x, c(0.05, 0.25, 0.75, 0.95), type = 1, names = FALSE)
      sx <- if (length(x) > 600) sample(x, 600) else x # CRPS on a subsample: O(n^2)
      tibble(muni_code = m, party = p, actual = y, median = median(x), q05 = q[1], q95 = q[4],
             in50 = y >= q[2] & y <= q[3], in90 = y >= q[1] & y <= q[4],
             pit = pit_discrete(x, y), crps = crps_sample(sx, y),
             modelled = p %in% colnames(s))
    })
  })
  # council control
  control <- imap_dfr(chain$sims, \(sim, m) {
    B <- sim$total_seats; maj <- floor(B / 2) + 1
    act <- truth$seats |> filter(muni_code == m)
    actual <- if (max(act$seats) >= maj) paste(act$party[which.max(act$seats)], "majority") else "No majority"
    pr <- control_outcomes(sim$seats, B)
    p <- coalesce(pr$prob[match(actual, pr$outcome)], 0)
    tibble(muni_code = m, actual = actual, p_actual = p,
           brier = (1 - p)^2 + sum(pr$prob[pr$outcome != actual]^2),
           modal_correct = pr$outcome[1] == actual)
  })
  # ward winners
  wards <- imap_dfr(chain$sims, \(sim, m) {
    w <- as_tibble(sim$winners) |> pivot_longer(everything(), names_to = "ward_id", values_to = "party") |>
      count(ward_id, party) |> mutate(p = n / nrow(sim$winners), .by = ward_id)
    truth$winners |> filter(muni_code == m) |>
      left_join(rename(w, winner = party), by = c("ward_id", "winner")) |>
      mutate(p = coalesce(p, 0)) |>
      left_join(w |> summarise(sum_p2 = sum(p^2), top = party[which.max(p)], .by = ward_id), by = "ward_id") |>
      transmute(muni_code, ward_id, winner, p_actual = p,
                brier = (1 - p)^2 + coalesce(sum_p2, 0) - p^2,
                log_score = log(pmax(p, 0.5 / chain$n_draws)),
                modal_correct = coalesce(top == winner, FALSE))
  })
  blind <- seats |> filter(!modelled, actual > 0)
  m <- filter(seats, modelled)
  summary <- tibble(
    seat_intervals = nrow(seats),
    coverage50 = mean(seats$in50), coverage90 = mean(seats$in90),
    # randomised-PIT coverage: exact under calibration even for small whole
    # numbers, where quantile intervals over-cover (L027); modelled parties only
    pit_cov50 = mean(m$pit >= 0.25 & m$pit <= 0.75), pit_cov90 = mean(m$pit >= 0.05 & m$pit <= 0.95),
    pit_low10 = mean(m$pit < 0.1), pit_high10 = mean(m$pit > 0.9),
    seat_crps = mean(seats$crps), seat_mae_median = mean(abs(seats$median - seats$actual)),
    pit_ks_p = suppressWarnings(stats::ks.test(seats$pit, "punif")$p.value),
    control_brier = mean(control$brier), control_modal_correct = mean(control$modal_correct),
    ward_brier = mean(wards$brier), ward_log_score = mean(wards$log_score),
    ward_modal_correct = mean(wards$modal_correct),
    new_entrant_seats = sum(blind$actual)
  )
  list(seats = seats, control = control, wards = wards, blind = blind, summary = summary)
}

# 4. The simple rules the model must beat (SCORING.md) ----------------------------------

naive_rule <- function(inp, truth, source = c("previous_local", "national_as_is")) {
  source <- match.arg(source)
  vd_ward <- distinct(inp$vd_new, vd, muni_code, ward_id)
  src <- if (source == "previous_local") inp$lge_prev else mutate(inp$npe_latest, ballot = "PR")
  ward_votes <- if (source == "previous_local") filter(src, ballot == "Ward") else src
  winners <- ward_votes |> select(-ward_id, -muni_code) |>
    inner_join(vd_ward, by = "vd") |>
    semi_join(inp$contests, by = c("muni_code", "ward_id", "party")) |>
    summarise(votes = sum(votes), .by = c(muni_code, ward_id, party)) |>
    slice_max(votes, n = 1, by = c(muni_code, ward_id), with_ties = FALSE) |>
    select(muni_code, ward_id, pred = party)
  combined <- src |> filter(ballot %in% c("PR", "Ward")) |>
    mutate(votes = if (source == "national_as_is") 2L * votes else votes) |>
    semi_join(inp$pr_lists, by = c("muni_code", "party")) |>
    summarise(votes = sum(votes), .by = c(muni_code, party))
  seats <- combined |> nest(.by = muni_code) |>
    mutate(alloc = map2(muni_code, data, \(m, d) {
      w <- winners |> filter(muni_code == m, pred %in% d$party) |> count(pred)
      nw <- sum(inp$vd_new$muni_code == m & !duplicated(inp$vd_new[c("muni_code", "ward_id")]))
      allocate_seats(setNames(d$votes, d$party), setNames(w$n, w$pred),
                     inp$councils$total_seats[inp$councils$muni_code == m],
                     no_list_wards = max(0, nw - sum(w$n))) |> select(party, pred_seats = seats)
    })) |> select(muni_code, alloc) |> unnest(alloc)
  cmp <- full_join(seats, rename(truth$seats, actual = seats), by = c("muni_code", "party")) |>
    mutate(across(c(pred_seats, actual), \(x) coalesce(as.integer(x), 0L))) |>
    filter(pred_seats > 0 | actual > 0)
  ctrl <- cmp |> nest(.by = muni_code) |>
    mutate(ok = map2_lgl(muni_code, data, \(m, d) {
      B <- inp$councils$total_seats[inp$councils$muni_code == m]; maj <- floor(B / 2) + 1
      f <- \(v) if (max(v) >= maj) d$party[which.max(v)] else "none"
      f(d$pred_seats) == f(d$actual)
    }))
  wr <- truth$winners |> left_join(winners, by = c("muni_code", "ward_id"))
  tibble(rule = source, seat_mae = mean(abs(cmp$pred_seats - cmp$actual)),
         control_correct = mean(ctrl$ok), ward_correct = mean(coalesce(wr$pred == wr$winner, FALSE)))
}

# 5. Estimating the A02 shock sizes ----------------------------------------------------

#' Council-level surprises, split into a province-wide part per party (the
#' "weather") and a council-specific part. Their spreads are direct
#' estimates of sd.province_party and sd.muni_party.
estimate_shock_sds <- function(chain, truth, min_councils = 3) {
  pred <- chain$baseline |>
    mutate(p = exp(eta_pr), w = registered * turnout) |>
    summarise(pred = sum(p * w) / sum(w), .by = c(muni_code, group))
  act <- truth$pr |>
    left_join(select(chain$groups, muni_code, party, group), by = c("muni_code", "party")) |>
    filter(!is.na(group), group != "ABSENT") |>
    summarise(votes = sum(votes), .by = c(muni_code, group)) |>
    mutate(act = votes / sum(votes), .by = muni_code)
  e <- inner_join(pred, act, by = c("muni_code", "group")) |>
    filter(act > 0, pred > 0) |>
    mutate(e = (log(act) - mean(log(act))) - (log(pred) - mean(log(pred))), .by = muni_code)
  named <- e |> filter(group != "OTHER")
  prov <- named |> summarise(u = mean(e), n = n(), .by = group) |> filter(n >= min_councils)
  within <- named |> inner_join(prov, by = "group") |> mutate(v = e - u)
  list(
    province = sd(prov$u), muni = sd(within$v),
    n_parties = nrow(prov), n_cells = nrow(within),
    by_party = arrange(prov, desc(abs(u))), residuals = e
  )
}

# 6. The pre-registered experiment (MODEL-LOG L027) ---------------------------------------
#
# Written down before the results were seen:
#   candidates  intercepts {fitted, zero} x province spread {0.10, 0.20, 0.35, 0.55}
#               x council spread {0.15, 0.30, 0.45}: 24 settings
#   choose      the lowest mean seat CRPS over all party x council cases,
#               among settings whose PIT coverage at 90% (modelled parties)
#               lies in [0.80, 0.97]
#   report      everything else, without optimising it

backtest_grid <- function(bt, cfg, provinces = c(0.10, 0.20, 0.35, 0.55), munis = c(0.15, 0.30, 0.45),
                          intercepts = c("fitted", "zero"), n_draws = 400) {
  map_dfr(intercepts, \(ic) {
    prep <- prepare_chain(bt$inputs, cfg, ic)
    tidyr::expand_grid(province = provinces, muni = munis) |>
      pmap_dfr(\(province, muni) {
        ch <- simulate_chain(prep, cfg, n_draws, list(province_party = province, muni_party = muni))
        sc <- score_chain(ch, bt$truth)
        mutate(sc$summary, intercepts = ic, province = province, muni = muni, .before = 1)
      })
  }) |>
    mutate(eligible = pit_cov90 >= 0.80 & pit_cov90 <= 0.97,
           chosen = eligible & seat_crps == min(seat_crps[eligible]))
}

# 7. Everything together ------------------------------------------------------------------

run_backtest <- function(bt, cfg) {
  n <- cfg$backtest$n_draws %||% 1000
  chain_cfg <- run_chain(bt$inputs, cfg, n)
  sc_cfg <- score_chain(chain_cfg, bt$truth)
  sds <- estimate_shock_sds(chain_cfg, bt$truth)
  chain_est <- run_chain(bt$inputs, cfg, n,
                         sd_override = list(province_party = sds$province, muni_party = sds$muni))
  sc_est <- score_chain(chain_est, bt$truth)
  naive <- bind_rows(naive_rule(bt$inputs, bt$truth, "previous_local"),
                     naive_rule(bt$inputs, bt$truth, "national_as_is"))
  model_rows <- bind_rows(
    mutate(sc_cfg$summary, rule = sprintf("model, config spreads (%.2f / %.2f)",
                                          cfg$model$sd$province_party, cfg$model$sd$muni_party)),
    mutate(sc_est$summary, rule = sprintf("model, estimated spreads (%.2f / %.2f)", sds$province, sds$muni))
  ) |> transmute(rule, seat_mae = seat_mae_median, control_correct = control_modal_correct,
                 ward_correct = ward_modal_correct)
  grid <- backtest_grid(bt, cfg, n_draws = cfg$backtest$grid_draws %||% 400)
  list(
    label = bt$label, grid = grid,
    summary = bind_rows(mutate(sc_cfg$summary, spreads = "config"), mutate(sc_est$summary, spreads = "estimated")),
    versus_rules = bind_rows(model_rows, naive),
    shock_sds = tibble(parameter = c("province_party", "muni_party"),
                       config = c(cfg$model$sd$province_party, cfg$model$sd$muni_party),
                       estimated = c(sds$province, sds$muni),
                       based_on = c(sprintf("%d parties", sds$n_parties), sprintf("%d party x council cells", sds$n_cells))),
    party_surprise = sds$by_party,
    seats = sc_cfg$seats, seats_estimated = sc_est$seats, control = sc_cfg$control, blind = sc_cfg$blind,
    transfer = chain_cfg$transfer$coefs
  )
}

#' Demo mode has no earlier cycle, so the demo backtest is IN-SAMPLE (it
#' predicts the 2021 synthetic election from itself). It exercises the code
#' path only; its scores mean nothing.
assemble_demo_backtest <- function(d) {
  assemble_backtest_inputs(npe_prev = d$npe2019, lge_prev = d$lge2021, npe_latest = d$npe2019,
                           lge_truth = d$lge2021, councils = d$councils,
                           municipalities = d$municipalities, label = "demo (in-sample, code test only)")
}

#' Live backtest inputs: 2014 NPE (bulk), 2016 LGE, 2019 NPE, and the 2021
#' results with the IEC's official 2021 seat calculations as the seat truth.
assemble_live_backtest <- function(cfg, npe2014_file, lge2016_files, npe_files, lge2021, seat_calc_files, munis) {
  prov_names <- unique(munis$province[munis$province_code %in% cfg$model$provinces])
  model_munis <- filter(munis, province_code %in% cfg$model$provinces)
  sc <- map(seat_calc_files, read_iec_seat_calc)
  councils21 <- map_dfr(sc, "meta") |> select(muni_code, total_seats) |>
    left_join(lge2021 |> filter(ballot == "Ward") |> distinct(muni_code, ward_id) |> count(muni_code, name = "n_wards"),
              by = "muni_code")
  seat_truth <- map_dfr(sc, \(x) mutate(x$parties, muni_code = x$meta$muni_code)) |>
    filter(!is.na(seats)) |> select(muni_code, party, seats)
  assemble_backtest_inputs(
    npe_prev = read_iec_npe_bulk(npe2014_file, "NPE2014", prov_names),
    lge_prev = map_dfr(lge2016_files, read_iec_lge_csv, election = "LGE2016"),
    npe_latest = read_iec_npe_portal(npe_files[str_detect(npe_files, "npe2019")], "NPE2019", prov_names),
    lge_truth = lge2021, councils = councils21, municipalities = model_munis,
    seat_truth = seat_truth, label = "2021 local election, predicted blind from 2014, 2016 and 2019")
}

write_backtest_outputs <- function(res) {
  dir.create(path_public(), showWarnings = FALSE, recursive = TRUE)
  tabs <- list(backtest_summary = mutate(res$summary, label = res$label), backtest_grid = res$grid,
               backtest_versus_rules = res$versus_rules, backtest_shock_sds = res$shock_sds,
               backtest_party_surprise = res$party_surprise, backtest_seats = res$seats,
               backtest_seats_estimated = res$seats_estimated, backtest_control = res$control,
               backtest_blind = res$blind, backtest_transfer = res$transfer)
  imap_chr(tabs, \(x, nm) { f <- path_public(paste0(nm, ".csv")); write_csv(x, f); f })
}
