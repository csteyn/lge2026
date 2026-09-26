# calibration.R -----------------------------------------------------------------
# O12 + O13: a pre-registered experiment (MODEL-LOG L049).
#
# L048 found two defects that offset each other in the 2021 backtest:
#   O13  the translation, predicting its own local election in-sample without
#        noise, overstates the DA's provincial share (~2 points) and
#        understates the ANC's (1-2 points), in both cycles;
#   O12  one pooled noise spread, dominated by small parties' sampling noise,
#        lowers a dominant party's expected share (convexity).
# Five variants, all else as in config.yml (newcomers included), same random
# numbers, 2021 backtest:
#
#   A  current: intercepts as fitted, pooled spreads, noise centred on log shares
#   B  intercepts calibrated to reproduce the fitted election's totals (O13)
#   C  B + mean-preserving noise
#   D  B + party-specific spreads (O12)
#   E  B + party-specific spreads + mean-preserving noise
#
# Closure test for every variant: each cycle's translation (2014 -> 2016 and
# 2019 -> 2021) predicting in-sample, at full strength, the election it was
# fitted on, with the variant's own intercepts and noise (mean-preserving
# noise leaves each district's expected share unchanged, so its closure is the
# prediction without noise). Error: mean absolute error over the two largest
# parties' shares, for the province and for Cape Town, averaged over scopes
# and cycles.
#
# RULE, fixed before any result (L049). A variant QUALIFIES if all hold:
#   1. closure error at most 1.0 point;
#   2. 90% coverage (modelled parties) in [0.80, 0.97];
#   3. ward Brier no more than 0.005 worse than the current model's;
#   4. council-control Brier no more than 0.02 worse;
#   5. seat CRPS not demonstrably worse: the 90% bootstrap interval of its
#      difference from the current model does not lie wholly above zero.
# Among qualifying variants the lowest seat CRPS is adopted. If none
# qualifies, nothing changes. Criterion 5 deliberately lets a variant that
# reproduces its own elections win without beating the current model on the
# 2021 backtest, whose result L048 showed came from offsetting errors; a
# variant demonstrably worse on seats still cannot.

calibration_variants <- function() tibble::tribble(
  ~variant, ~premium_calibration, ~spreads, ~noise_centring, ~description,
  "A", "none",   "pooled", "none", "as fitted; pooled spreads",
  "B", "totals", "pooled", "none", "calibrated intercepts",
  "C", "totals", "pooled", "mean", "calibrated intercepts; mean-preserving noise",
  "D", "totals", "party",  "none", "calibrated intercepts; party spreads",
  "E", "totals", "party",  "mean", "calibrated intercepts; party spreads; mean-preserving noise"
)

variant_cfg <- function(cfg, premium_calibration, spreads, noise_centring) {
  cfg$model$premium_calibration <- premium_calibration
  cfg$model$spreads <- spreads
  cfg$model$noise_centring <- noise_centring
  cfg
}

current_variant <- function(cfg) {
  v <- calibration_variants()
  i <- which(v$premium_calibration == (cfg$model$premium_calibration %||% "none") &
               v$spreads == (cfg$model$spreads %||% "pooled") &
               v$noise_centring == (cfg$model$noise_centring %||% "none"))
  if (length(i) != 1) stop("config.yml's premium_calibration / spreads / noise_centring match no L049 variant", call. = FALSE)
  v$variant[i]
}

#' In-sample reproduction of one fitted cycle under one variant
closure_for_variant <- function(fit, cfg, variant, premium_calibration, spreads, noise_centring, label,
                                K = 300, seed = 1, province_label = "Western Cape") {
  tr <- apply_transfer_variant(fit, cfg$model$transfer_b %||% "fitted", shrink = 1,
                               calibrate = identical(premium_calibration, "totals"))
  d <- fit$xy |> inner_join(select(tr$coefs, group, a, b), by = "group") |>
    mutate(eta = a + b * clr_npe, votes = total_lge * share_lge) |>
    mutate(subtotal = sum(votes), .by = c(muni_code, vd))
  keys <- distinct(d, muni_code, vd, subtotal)
  grp <- sort(unique(d$group))
  E <- matrix(-Inf, nrow(keys), length(grp), dimnames = list(NULL, grp))
  E[cbind(match(paste(d$muni_code, d$vd), paste(keys$muni_code, keys$vd)), match(d$group, grp))] <- d$eta
  sd_g <- if (identical(spreads, "party")) {
    sp <- fit$sd_party; i <- match(grp, sp$group)
    coalesce(sp$sd_total[i], median(sp$sd_total))
  } else rep(sqrt(fit$sd_ward^2 + fit$sd_vd^2), length(grp))
  p <- if (identical(noise_centring, "mean")) softmax_rows(E) else {
    m <- matrix(0, nrow(E), ncol(E))
    for (k in seq_len(K)) m <- m + softmax_rows(E + withr::with_seed(seed + k,
      matrix(rnorm(length(E), 0, rep(sd_g, each = nrow(E))), nrow(E))))
    m / K
  }
  pred <- as_tibble(p * keys$subtotal) |> mutate(muni_code = keys$muni_code) |>
    pivot_longer(-muni_code, names_to = "party", values_to = "predicted")
  act <- d |> transmute(muni_code, party = group, actual = votes)
  both <- full_join(summarise(pred, predicted = sum(predicted), .by = c(muni_code, party)),
                    summarise(act, actual = sum(actual), .by = c(muni_code, party)), by = c("muni_code", "party")) |>
    mutate(across(c(predicted, actual), \(x) coalesce(x, 0)))
  bind_rows(
    both |> summarise(across(c(predicted, actual), sum), .by = party) |> mutate(scope = province_label),
    both |> filter(muni_code == "CPT") |> select(-muni_code) |> mutate(scope = "CPT")
  ) |>
    mutate(across(c(predicted, actual), \(x) x / sum(x)), .by = scope) |>
    mutate(cycle = label, variant = variant, .before = 1) |>
    select(cycle, variant, scope, party, actual, predicted)
}

closure_all_variants <- function(fits, cfg, K = 300) {
  v <- calibration_variants()
  imap_dfr(fits, \(fit, label) pmap_dfr(select(v, -description), \(variant, premium_calibration, spreads, noise_centring)
    closure_for_variant(fit, cfg, variant, premium_calibration, spreads, noise_centring, label, K = K)))
}

closure_error <- function(closure) {
  closure |>
    group_by(cycle, variant, scope) |> slice_max(actual, n = 2, with_ties = FALSE) |> ungroup() |>
    summarise(err = mean(abs(predicted - actual)), .by = c(cycle, variant, scope)) |>
    summarise(closure_mae = mean(err), .by = variant)
}

#' 2021 backtest of every variant, paired with the current one
backtest_calibration_grid <- function(bt, cfg, entrant_prior = NULL, n_draws = 400) {
  ep <- if (isTRUE(cfg$model$entrants)) entrant_prior else NULL
  actual <- bt$truth$pr |> summarise(votes = sum(votes), .by = party) |> mutate(share = votes / sum(votes))
  share_of <- \(t, p, col = "median") { i <- match(p, t$party); if (is.na(i)) NA_real_ else t[[col]][i] }
  v <- calibration_variants()
  cur <- current_variant(cfg)
  runs <- pmap(v, \(variant, premium_calibration, spreads, noise_centring, description) {
    c2 <- variant_cfg(cfg, premium_calibration, spreads, noise_centring)
    prep <- prepare_chain(bt$inputs, c2, "fitted", entrant_prior = ep)
    ch <- simulate_chain(prep, c2, n_draws)
    sc <- score_chain(ch, bt$truth)
    ps <- province_vote_share(ch$sims, prep$baseline)
    list(summary = mutate(sc$summary, variant = variant, description = description,
                          premium_calibration = premium_calibration, spreads = spreads, noise_centring = noise_centring,
                          da_pred = share_of(ps, "DEMOCRATIC ALLIANCE"), da_actual = share_of(actual, "DEMOCRATIC ALLIANCE", "share"),
                          anc_pred = share_of(ps, "AFRICAN NATIONAL CONGRESS"), anc_actual = share_of(actual, "AFRICAN NATIONAL CONGRESS", "share"),
                          .before = 1),
         seats = select(sc$seats, muni_code, party, crps))
  })
  g <- bind_rows(map(runs, "summary"))
  ref <- runs[[match(cur, v$variant)]]$seats
  g |> bind_cols(bind_rows(map(runs, \(r) {
    d <- inner_join(rename(ref, c0 = crps), rename(r$seats, c1 = crps), by = c("muni_code", "party")) |>
      summarise(diff = sum(c1 - c0), n = n(), .by = muni_code)
    b <- withr::with_seed(1, replicate(2000, { i <- sample.int(nrow(d), replace = TRUE); sum(d$diff[i]) / sum(d$n[i]) }))
    tibble(crps_diff = sum(d$diff) / sum(d$n), crps_diff_lo = unname(quantile(b, 0.05)), crps_diff_hi = unname(quantile(b, 0.95)))
  }))) |>
    mutate(current = variant == cur, with_newcomers = !is.null(ep))
}

#' The L049 rule, applied mechanically
calibration_decision <- function(grid, closure, threshold = 0.01) {
  ce <- closure_error(closure)
  g <- grid |> left_join(ce, by = "variant")
  cur <- g$current
  g |> mutate(
    closure_ok = closure_mae <= threshold,
    coverage_ok = pit_cov90 >= 0.80 & pit_cov90 <= 0.97,
    ward_ok = ward_brier <= ward_brier[cur] + 0.005,
    control_ok = control_brier <= control_brier[cur] + 0.02,
    seats_ok = !(crps_diff_lo > 0),
    qualifies = !current & closure_ok & coverage_ok & ward_ok & control_ok & seats_ok,
    adopt = qualifies & seat_crps == if (any(qualifies)) min(seat_crps[qualifies]) else -Inf
  )
}

write_calibration_outputs <- function(decision, closure) {
  dir.create(path_public(), showWarnings = FALSE, recursive = TRUE)
  f <- c(path_public("calibration_decision.csv"), path_public("closure_variants.csv"))
  write_csv(decision, f[1]); write_csv(closure, f[2])
  f
}
