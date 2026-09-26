# entrants.R --------------------------------------------------------------------
# Parties standing in a council with no history there (MODEL-LOG L035).
#
# In the blind 2021 backtest, parties the model could not see won 46 Western
# Cape seats. For 2026 the candidate lists name every newcomer (125 party x
# council cases, 56 parties); what is unknown is how well each will do.
#
#   1. Past newcomers: every party that stood in a council in 2016 or 2021
#      with no votes there in the previous local or latest national election.
#   2. Classes: breadth (councils stood in: 1 / 2-10 / 11+) x ward coverage
#      (all wards / partial); a class with fewer than `min_cell` examples
#      falls back to breadth alone, then to all newcomers.
#   3. Each simulated election draws each newcomer's council share from its
#      class's historical distribution. Draws for one party are correlated
#      across councils (rho, estimated from past newcomers standing in
#      several councils), because a party's appeal is partly common.
#   4. A party-specific prior can override its class only through a sourced
#      row in data-raw/manual/entrant_priors.csv (for example, polls for a
#      national organisation standing on its own for the first time).
#
# Shares are council-level (ward and PR ballots combined). A newcomer's share
# is taken proportionally from all other parties, as in A12.

#' Which parties in `results_t` had no history in their council
first_timers <- function(results_t, lge_hist, npe_hist, year) {
  hist <- bind_rows(lge_hist, npe_hist) |> filter(votes > 0) |>
    distinct(muni_code, party) |> mutate(key = name_key(party))
  results_t <- results_t |> filter(ballot %in% c("PR", "Ward"))
  standing <- results_t |> filter(votes > 0, party != "INDEPENDENT") |> distinct(muni_code, party)
  shares <- results_t |> summarise(votes = sum(votes), .by = c(muni_code, party)) |>
    mutate(share = votes / sum(votes), .by = muni_code)
  n_wards <- results_t |> filter(ballot == "Ward") |> distinct(muni_code, ward_id) |> count(muni_code, name = "n_wards")
  size <- results_t |> filter(ballot == "PR") |> distinct(muni_code, vd, registered) |>
    summarise(registered = sum(registered, na.rm = TRUE), .by = muni_code)
  covered <- results_t |> filter(ballot == "Ward", votes > 0) |> distinct(muni_code, party, ward_id) |>
    count(muni_code, party, name = "wards_contested")
  standing |>
    mutate(key = name_key(party)) |>
    anti_join(hist, by = c("muni_code", "key")) |>
    left_join(shares, by = c("muni_code", "party")) |>
    left_join(covered, by = c("muni_code", "party")) |>
    left_join(n_wards, by = "muni_code") |>
    left_join(size, by = "muni_code") |>
    left_join(count(standing, party, name = "councils"), by = "party") |>
    mutate(wards_contested = coalesce(wards_contested, 0L), coverage = wards_contested / n_wards,
           history_in_province = key %in% hist$key, year = year) |>
    select(year, muni_code, party, share, coverage, councils, registered, history_in_province)
}

#' Newcomer model v2 (MODEL-LOG L044). A party-weighted regression of each
#' past newcomer's council share (log-odds) on ward coverage, full coverage,
#' breadth (log councils stood in), council size (log registered voters) and
#' history elsewhere in the province. Each party-year counts once, however
#' many councils it stood in, so no single party can dominate (the L043 flaw).
#' Draws are logit-normal with the residual spread split into a component
#' shared by a party across councils (rho) and a council-specific one.
entrant_features <- function(d) {
  d |> mutate(all_wards = coverage >= 0.99, lg_councils = log(pmax(councils, 1)),
              lg_size = log(pmax(registered, 100)), history = as.numeric(history_in_province))
}

estimate_entrant_model <- function(ft) {
  d <- ft |> filter(!is.na(share), share > 0, !is.na(registered)) |> entrant_features() |>
    mutate(y = qlogis(pmin(share, 0.99)), pid = paste(year, party), w = 1 / n(), .by = c(year, party))
  fit <- lm(y ~ coverage + all_wards + lg_councils + lg_size + history, data = d, weights = w)
  e <- residuals(fit)
  sd_res <- sqrt(sum(d$w * e^2) / sum(d$w))
  # party-level share of residual variance, from party-years in several councils
  r <- d |> mutate(e = e) |> filter(n() >= 2, .by = pid)
  rho <- if (n_distinct(r$pid) >= 5) {
    pm <- r |> summarise(m = mean(e), v = var(e), n = n(), .by = pid)
    max(0, min(0.95, (var(pm$m) - mean(pm$v / pm$n, na.rm = TRUE)) / sd_res^2))
  } else 0.5
  cf <- summary(fit)$coefficients
  list(type = "regression", fit = fit, sd = sd_res, rho = rho, n_cases = nrow(d), n_parties = n_distinct(d$pid),
       years = sort(unique(d$year)), history = d |> summarise(total = sum(share), .by = c(year, muni_code)),
       coefficients = tibble(term = rownames(cf), estimate = cf[, 1], se = cf[, 2]),
       r2 = summary(fit)$r.squared)
}

# kept for older call sites and tests
estimate_entrant_prior <- function(ft, ...) estimate_entrant_model(ft)

#' Does the model fitted on one set of years agree with the pooled fit? (C21)
entrant_model_stability <- function(ft) {
  yrs <- sort(unique(ft$year))
  if (length(yrs) < 2) return(tibble())
  a <- estimate_entrant_model(filter(ft, year == min(yrs)))$coefficients
  b <- estimate_entrant_model(ft)$coefficients
  inner_join(a, b, by = "term", suffix = c("_first", "_pooled")) |>
    mutate(z = (estimate_first - estimate_pooled) / sqrt(se_first^2 + se_pooled^2), differs = abs(z) > 2)
}

#' Newcomers in the target election: on a council's list, not in its groups
find_entrants <- function(pr_lists, contests, groups, vd_new = NULL) {
  if (is.null(pr_lists)) return(tibble())
  size <- if (!is.null(vd_new)) vd_new |> distinct(muni_code, vd, registered) |>
    summarise(registered = sum(registered, na.rm = TRUE), .by = muni_code) else tibble(muni_code = character(), registered = numeric())
  known_elsewhere <- unique(name_key(groups$party))
  n_wards <- contests |> distinct(muni_code, ward_id) |> count(muni_code, name = "n_wards")
  pr_lists |> filter(party != "INDEPENDENT") |>
    anti_join(distinct(groups, muni_code, party), by = c("muni_code", "party")) |>
    left_join(contests |> distinct(muni_code, party, ward_id) |> count(muni_code, party, name = "w"),
              by = c("muni_code", "party")) |>
    left_join(n_wards, by = "muni_code") |>
    mutate(coverage = coalesce(w, 0L) / n_wards) |>
    left_join(count(pr_lists, party, name = "councils"), by = "party") |>
    left_join(size, by = "muni_code") |>
    mutate(history_in_province = name_key(party) %in% known_elsewhere) |>
    select(muni_code, party, coverage, councils, registered, history_in_province)
}

#' Correlated share draws for every newcomer: a list of draws x party
#' matrices, one per council. Logit-normal around the regression prediction;
#' a sourced override replaces the prediction for its party.
draw_entrant_shares <- function(entrants, prior, n_draws, seed, overrides = NULL, max_total = 0.8) {
  if (!nrow(entrants)) return(list())
  set.seed(seed)
  parties <- unique(entrants$party)
  z_party <- matrix(rnorm(n_draws * length(parties)), n_draws, dimnames = list(NULL, parties))
  mu <- predict(prior$fit, newdata = entrant_features(entrants))
  rho <- prior$rho; s <- prior$sd
  cols <- lapply(seq_len(nrow(entrants)), \(i) {
    p <- entrants$party[i]
    z <- sqrt(rho) * z_party[, p] + sqrt(1 - rho) * rnorm(n_draws)
    ov <- if (!is.null(overrides)) overrides[overrides$party == p, ] else NULL
    if (!is.null(ov) && nrow(ov)) { # sourced party-specific prior: log-normal through median and q90
      sh <- qlnorm(pnorm(z), log(ov$median[1]), (log(ov$q90[1]) - log(ov$median[1])) / qnorm(0.9))
    } else sh <- plogis(mu[i] + s * z)
    pmin(sh, 0.95)
  })
  split(seq_len(nrow(entrants)), entrants$muni_code) |>
    map(\(i) {
      m <- do.call(cbind, cols[i]); colnames(m) <- entrants$party[i]
      m * pmin(1, max_total / pmax(rowSums(m), 1e-9))
    })
}

#' Table of newcomers for publication
describe_entrants <- function(entrants, prior, draws, overrides = NULL) {
  if (!nrow(entrants)) return(tibble())
  entrants |> mutate(
    basis = map_chr(party, \(p) {
      if (!is.null(overrides) && p %in% overrides$party) paste("override:", overrides$source[overrides$party == p][1])
      else sprintf("model v2 (%d past newcomers, %d party-years, %s)", prior$n_cases, prior$n_parties,
                   paste(prior$years, collapse = "+")) }),
    share_median = map2_dbl(muni_code, party, \(m, p) median(draws[[m]][, p])),
    share_q05 = map2_dbl(muni_code, party, \(m, p) quantile(draws[[m]][, p], 0.05)),
    share_q95 = map2_dbl(muni_code, party, \(m, p) quantile(draws[[m]][, p], 0.95)))
}

read_entrant_overrides <- function(path) {
  if (!file.exists(path)) return(NULL)
  x <- read_csv(path, comment = "#", show_col_types = FALSE, col_types = "cddcc")
  if (!nrow(x)) return(NULL)
  bad <- x |> filter(is.na(source) | !nzchar(source) | !(median > 0 & q90 > median))
  if (nrow(bad)) stop("entrant_priors.csv: every row needs a source and 0 < median < q90: ",
                      paste(bad$party, collapse = ", "), call. = FALSE)
  mutate(x, party = party_key(party))
}

#' Newcomers' combined share per council, simulated against history (L043).
#' Historical totals: every past newcomer's share summed per council and year.
entrant_total_check <- function(entrant_draws, first_timer_data) {
  hist <- first_timer_data |> filter(!is.na(share)) |>
    summarise(total = sum(share), newcomers = n(), .by = c(year, muni_code))
  sim <- imap_dfr(entrant_draws, \(m, code) tibble(muni_code = code, newcomers = ncol(m),
                                                  median_total = median(rowSums(m)),
                                                  q95_total = quantile(rowSums(m), 0.95)))
  list(history = hist, simulated = sim,
       hist_p95 = if (nrow(hist)) unname(quantile(hist$total, 0.95)) else NA_real_,
       hist_max = if (nrow(hist)) max(hist$total) else NA_real_)
}
