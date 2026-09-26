# noise.R -----------------------------------------------------------------------
# Noise centring (MODEL-LOG L047): a pre-registered experiment and a closure test.
#
# The share decomposition (L046) found the simulation step itself lowering the
# largest party's share by several points. The simulation adds ward and VD
# noise with one spread for every party on the log scale; through the softmax
# that lowers a dominant party's expected share (convexity). Whether this is
# a bias or a real feature depends on whether the fitted translation predicts
# the median log share (then the noise is needed to reproduce the aggregate)
# or the expected share (then the noise double-counts). Two tests decide:
#
#   1. The 2021 backtest, current model ("none") against mean-preserving noise
#      ("mean"), same random numbers, every other setting as in config.yml.
#   2. A closure test: can each fitted translation reproduce the local election
#      it was fitted on? Predicted in-sample from the national vote with the
#      translation at full strength, among the parties it was fitted for,
#      weighted by each district's actual votes for those parties. "none"
#      corresponds to the noisy prediction, "mean" to the deterministic one
#      (mean-preserving noise leaves each district's expected share unchanged).
#
# Rule, fixed before any result (L047): the alternative is adopted if it
# passes the guards (90% coverage in [0.80, 0.97]; ward Brier no more than
# 0.005 worse; council-control Brier no more than 0.02 worse) AND either
# (a) the 90% bootstrap interval of its seat-CRPS difference lies wholly below
# zero, or (b) that interval includes zero and it reproduces the fitted
# elections better in the closure test (mean absolute error over the two
# largest parties' provincial shares, averaged over the two cycles). If the
# interval lies wholly above zero, or a guard fails, nothing changes.

#' In-sample reproduction of one national-to-local cycle
closure_cycle <- function(npe_prev, lge_prev, groups, cfg, label, npe_next = NULL, frac = NULL,
                          K = 400, seed = 1, province_label = "Western Cape") {
  fit <- fit_transfer(npe_prev, lge_prev, groups, cfg, npe_next = npe_next, frac = frac)
  tr <- apply_transfer_variant(fit, cfg$model$transfer_b %||% "fitted", shrink = 1)
  sd_local <- sqrt(fit$sd_ward^2 + fit$sd_vd^2)
  d <- fit$xy |> inner_join(select(tr$coefs, group, a, b), by = "group") |>
    mutate(eta = a + b * clr_npe, votes = total_lge * share_lge) |>
    mutate(subtotal = sum(votes), .by = c(muni_code, vd))
  keys <- distinct(d, muni_code, vd) |> mutate(row = row_number())
  grp <- sort(unique(d$group))
  E <- matrix(-Inf, nrow(keys), length(grp), dimnames = list(NULL, grp))
  idx <- cbind(match(paste(d$muni_code, d$vd), paste(keys$muni_code, keys$vd)), match(d$group, grp))
  E[idx] <- d$eta
  tot <- d |> distinct(muni_code, vd, subtotal)
  tot <- tot$subtotal[match(paste(keys$muni_code, keys$vd), paste(tot$muni_code, tot$vd))]
  p_det <- softmax_rows(E)
  p_noisy <- matrix(0, nrow(E), ncol(E))
  for (k in seq_len(K)) {
    e <- withr::with_seed(seed + k, matrix(rnorm(length(E), 0, sd_local), nrow(E)))
    p_noisy <- p_noisy + softmax_rows(E + e)
  }
  p_noisy <- p_noisy / K
  long <- function(p, what) as_tibble(p * tot) |> mutate(muni_code = keys$muni_code) |>
    pivot_longer(-muni_code, names_to = "party", values_to = "votes") |> mutate(what = what)
  pred <- bind_rows(long(p_det, "deterministic"), long(p_noisy, "noisy"))
  act <- d |> transmute(muni_code, party = group, votes, what = "actual")
  all <- bind_rows(pred, act)
  bind_rows(
    all |> summarise(votes = sum(votes), .by = c(what, party)) |> mutate(scope = province_label),
    all |> filter(muni_code == "CPT") |> summarise(votes = sum(votes), .by = c(what, party)) |> mutate(scope = "CPT")
  ) |>
    mutate(share = votes / sum(votes), .by = c(scope, what)) |>
    select(scope, party, what, share) |>
    pivot_wider(names_from = what, values_from = share) |>
    mutate(cycle = label, sd_local = sd_local, .before = 1) |>
    arrange(scope, desc(actual))
}

closure_test <- function(cycles) bind_rows(cycles)

#' Mean absolute error of the two largest parties' provincial shares, per
#' prediction type, averaged over cycles
closure_mae <- function(closure, province_label = "Western Cape") {
  closure |> filter(scope == province_label) |>
    group_by(cycle) |> slice_max(actual, n = 2) |> ungroup() |>
    summarise(none = mean(abs(noisy - actual)), mean = mean(abs(deterministic - actual)), .by = cycle) |>
    summarise(none = mean(none), mean = mean(mean))
}

#' 2021 backtest: current noise against mean-preserving noise
backtest_noise_grid <- function(bt, cfg, entrant_prior = NULL, n_draws = 400) {
  ep <- if (isTRUE(cfg$model$entrants)) entrant_prior else NULL
  prep <- prepare_chain(bt$inputs, cfg, "fitted", entrant_prior = ep)
  actual <- bt$truth$pr |> summarise(votes = sum(votes), .by = party) |> mutate(share = votes / sum(votes))
  share_of <- \(t, p, col = "median") { i <- match(p, t$party); if (is.na(i)) NA_real_ else t[[col]][i] }
  runs <- map(c("none", "mean"), \(mode) {
    c2 <- cfg; c2$model$noise_centring <- mode
    ch <- simulate_chain(prep, c2, n_draws)
    sc <- score_chain(ch, bt$truth)
    ps <- province_vote_share(ch$sims, prep$baseline)
    list(summary = mutate(sc$summary, noise_centring = mode,
                          da_pred = share_of(ps, "DEMOCRATIC ALLIANCE"), da_actual = share_of(actual, "DEMOCRATIC ALLIANCE", "share"),
                          anc_pred = share_of(ps, "AFRICAN NATIONAL CONGRESS"), anc_actual = share_of(actual, "AFRICAN NATIONAL CONGRESS", "share"),
                          .before = 1),
         seats = select(sc$seats, muni_code, party, crps))
  })
  g <- bind_rows(map(runs, "summary"))
  cur <- g$noise_centring == (cfg$model$noise_centring %||% "none")
  ref <- runs[[which(cur)]]$seats
  g |> bind_cols(bind_rows(map(runs, \(r) {
    d <- inner_join(rename(ref, c0 = crps), rename(r$seats, c1 = crps), by = c("muni_code", "party")) |>
      summarise(diff = sum(c1 - c0), n = n(), .by = muni_code)
    b <- withr::with_seed(1, replicate(2000, { i <- sample.int(nrow(d), replace = TRUE); sum(d$diff[i]) / sum(d$n[i]) }))
    tibble(crps_diff = sum(d$diff) / sum(d$n), crps_diff_lo = unname(quantile(b, 0.05)), crps_diff_hi = unname(quantile(b, 0.95)))
  }))) |>
    mutate(current = cur, sd_local = sqrt(prep$transfer$sd_ward^2 + prep$transfer$sd_vd^2),
           with_newcomers = !is.null(ep))
}

#' The L047 rule, applied mechanically
noise_decision <- function(grid, closure, cfg) {
  cur <- cfg$model$noise_centring %||% "none"
  alt <- setdiff(c("none", "mean"), cur)
  gc <- grid[grid$noise_centring == cur, ]
  ga <- grid[grid$noise_centring == alt, ]
  mae <- closure_mae(closure)
  guards <- ga$pit_cov90 >= 0.80 & ga$pit_cov90 <= 0.97 &
    ga$ward_brier <= gc$ward_brier + 0.005 & ga$control_brier <= gc$control_brier + 0.02
  closure_better <- mae[[alt]] < mae[[cur]]
  verdict <- if (!guards) "keep: a guard fails" else
    if (ga$crps_diff_hi < 0) "adopt: better on seats" else
      if (ga$crps_diff_lo > 0) "keep: worse on seats" else
        if (closure_better) "adopt: tie on seats; reproduces the fitted elections better" else
          "keep: tie on seats; does not reproduce the fitted elections better"
  tibble(current = cur, alternative = alt,
         crps_diff = ga$crps_diff, crps_diff_lo = ga$crps_diff_lo, crps_diff_hi = ga$crps_diff_hi,
         cov90 = ga$pit_cov90, ward_brier_diff = ga$ward_brier - gc$ward_brier,
         control_brier_diff = ga$control_brier - gc$control_brier, guards_pass = guards,
         closure_mae_current = mae[[cur]], closure_mae_alternative = mae[[alt]],
         adopt = startsWith(verdict, "adopt"), verdict = verdict)
}

write_noise_outputs <- function(grid, closure, decision) {
  dir.create(path_public(), showWarnings = FALSE, recursive = TRUE)
  files <- c(backtest_noise_grid = path_public("backtest_noise_grid.csv"),
             closure = path_public("closure.csv"), noise_decision = path_public("noise_decision.csv"))
  write_csv(grid, files[["backtest_noise_grid"]]); write_csv(closure, files[["closure"]])
  write_csv(decision, files[["noise_decision"]])
  unname(files)
}
