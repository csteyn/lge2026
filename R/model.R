# model.R -----------------------------------------------------------------------
# Model v0.1: "2024 national vote, translated into a local election, with
# honest noise at every level where South African elections actually vary."
#
#   1. Party groups. Per municipality, parties above a vote-share threshold are
#      modelled individually; everything else is pooled as OTHER.
#   2. Transfer. How a national-election vote turns into a local-election vote
#      is learned from the one observed pair in the WP-era geography:
#      2019 NPE -> 2021 LGE, at voting-district level, on the centred
#      log-ratio (clr) scale. Fitted per party: clr_LGE = a + b * clr_NPE.
#   3. Baseline. The transfer is applied to 2024 NPE shares for every 2026
#      voting district. Local-only parties (in the 2021 LGE, absent from
#      national ballots) keep their 2021 share as a carve-out.
#   4. Simulation. Each draw adds province, municipality, ward and VD shocks
#      on the log scale, renormalises by softmax, applies the ward/PR ticket
#      split and turnout, finds ward winners, and runs the Schedule 1 seat
#      allocation. Coalition arithmetic is computed on the resulting councils.
#
# What v0.1 does NOT do is logged in MODEL-LOG.md and on the site's
# assumptions page. The honest summary: shock sizes at province and
# municipality level are judgement (config.yml), not estimates.

# 1. Party groups ---------------------------------------------------------------

define_party_groups <- function(lge_prev, npe_latest, cfg, pr_lists = NULL) {
  min_share <- cfg$model$parties_min_share
  max_parties <- cfg$model$max_parties_per_muni

  shares <- bind_rows(
    lge_prev |> filter(ballot == "PR") |> mutate(src = "lge"),
    npe_latest |> mutate(src = "npe")
  ) |>
    summarise(votes = sum(votes), .by = c(muni_code, src, party)) |>
    mutate(share = votes / sum(votes), .by = c(muni_code, src))

  if (nrow(shares) == 0) stop("define_party_groups(): no parties left in any municipality; ",
                              "check the scoped results and candidate tables", call. = FALSE)
  best <- shares |>
    summarise(share = max(share), in_npe = any(src == "npe" & votes > 0),
              in_lge = any(src == "lge" & votes > 0), .by = c(muni_code, party))

  if (!is.null(pr_lists) && nrow(pr_lists)) {
    # Only parties on the 2026 ballot, but only in municipalities the
    # candidate data actually covers; elsewhere keep everyone (L013).
    # Parties not standing are kept, marked absent, so their votes can be
    # removed later. Dropping them here sent their votes to OTHER (L021).
    covered <- unique(pr_lists$muni_code)
    standing <- mutate(distinct(pr_lists, muni_code, party), standing = TRUE)
    best <- best |>
      left_join(standing, by = c("muni_code", "party")) |>
      mutate(standing = coalesce(standing, !muni_code %in% covered))
  } else {
    best <- mutate(best, standing = TRUE)
  }

  if (nrow(best) == 0) stop("define_party_groups(): no parties left in any municipality; ",
                            "check the scoped results and candidate tables", call. = FALSE)
  named <- best |>
    filter(standing, share >= min_share, party != "INDEPENDENT") |>
    slice_max(share, n = max_parties, by = muni_code, with_ties = FALSE) |>
    mutate(group = party)

  best |>
    left_join(select(named, muni_code, party, group), by = c("muni_code", "party")) |>
    mutate(
      group = if_else(standing, coalesce(group, "OTHER"), "ABSENT"),
      origin = case_when(group == "ABSENT" ~ "absent_2026",
                         group == "OTHER" ~ "pooled",
                         in_npe ~ "national", TRUE ~ "local_only")
    )
}

#' Weights for splitting OTHER's votes back into real parties before seats are
#' allocated (OTHER is not a party and must never win seats as a bloc).
other_composition <- function(groups, lge_prev, npe_latest) {
  pooled <- groups |> filter(group == "OTHER", party != "INDEPENDENT")
  bind_rows(lge_prev |> filter(ballot == "PR"), npe_latest) |>
    semi_join(pooled, by = c("muni_code", "party")) |>
    summarise(votes = sum(votes), .by = c(muni_code, party)) |>
    mutate(weight = votes / sum(votes), .by = muni_code) |>
    select(muni_code, party, weight)
}

# 2. Shares and the NPE -> LGE transfer -----------------------------------------

#' Collapse results to party groups and return smoothed VD log-shares
group_shares <- function(results, groups, alpha = 0.5) {
  observed <- results |>
    left_join(select(groups, muni_code, party, group), by = c("muni_code", "party")) |>
    mutate(group = coalesce(group, "OTHER")) |>
    filter(group != "ABSENT") |> # not standing in 2026: removed, others renormalised (A12)
    summarise(votes = sum(votes), .by = c(muni_code, vd, group))
  groups <- filter(groups, group != "ABSENT")
  # full VD x group grid within each municipality (a zero is information)
  grid <- distinct(observed, muni_code, vd) |>
    inner_join(distinct(groups, muni_code, group), by = "muni_code", relationship = "many-to-many")
  grid |>
    left_join(observed, by = c("muni_code", "vd", "group")) |>
    mutate(votes = coalesce(votes, 0)) |>
    mutate(
      total = sum(votes),
      share = (votes + alpha) / (total + alpha * n()),
      log_share = log(share),
      clr = log_share - mean(log_share),
      .by = c(muni_code, vd)
    )
}

#' Where the local election falls between the national elections either side
#' of it, as a fraction of the interval (L044)
interp_frac <- function(prev, local, nxt) {
  as.numeric(as.Date(local) - as.Date(prev)) / as.numeric(as.Date(nxt) - as.Date(prev))
}

fit_transfer <- function(npe_prev, lge_prev, groups, cfg, npe_next = NULL, frac = NULL) {
  # Local-only parties are removed from the LGE side and the rest renormalised,
  # mirroring build_baseline(), where national parties share (1 - local mass).
  g <- groups |> filter(origin != "local_only")
  local <- groups |> filter(origin == "local_only")
  if (nrow(g) == 0) stop("fit_transfer(): no national party groups to fit; groups table is empty upstream",
                         call. = FALSE)
  x <- group_shares(npe_prev, g) |> select(muni_code, vd, group, clr_npe = clr, total_npe = total, share_npe = share)
  # Premium basis "interpolated" (L044): compare the local vote with the
  # national vote interpolated to the local election's date, between the
  # national elections either side, so the swing between elections is not
  # learned as a local premium (the suspected cause of the DA's 2021
  # overshoot and the ANC's 2026 undershoot, O10).
  if (!is.null(npe_next) && !is.null(frac)) {
    x2 <- group_shares(npe_next, g) |> select(muni_code, vd, group, clr_next = clr)
    x <- inner_join(x, x2, by = c("muni_code", "vd", "group")) |>
      mutate(clr_npe = (1 - frac) * clr_npe + frac * clr_next) |> select(-clr_next)
  }
  y <- lge_prev |>
    filter(ballot == "PR") |>
    anti_join(local, by = c("muni_code", "party")) |>
    group_shares(g) |>
    select(muni_code, vd, group, clr_lge = clr, total_lge = total, share_lge = share)
  wards <- lge_prev |> distinct(muni_code, vd, ward_id)
  # A group must have real votes in BOTH elections in a municipality to inform
  # the fit there; otherwise we would be regressing smoothing noise on
  # smoothing noise (e.g. a party founded after the earlier NPE).
  support <- inner_join(
    x |> semi_join(filter(npe_prev, votes > 0) |> left_join(select(g, muni_code, party, group), by = c("muni_code", "party")),
                   by = c("muni_code", "group")) |> distinct(muni_code, group),
    y |> semi_join(filter(lge_prev, votes > 0, ballot == "PR") |> left_join(select(g, muni_code, party, group), by = c("muni_code", "party")),
                   by = c("muni_code", "group")) |> distinct(muni_code, group),
    by = c("muni_code", "group")
  )
  xy <- inner_join(x, y, by = c("muni_code", "vd", "group")) |>
    semi_join(support, by = c("muni_code", "group")) |>
    left_join(wards, by = c("muni_code", "vd")) |>
    filter(total_npe > 0, total_lge > 0)

  min_vds <- cfg$model$transfer_min_vds
  coefs <- xy |>
    nest(.by = group) |>
    mutate(
      n_vd = map_int(data, nrow),
      fit = map2(data, n_vd, \(d, n) if (n >= min_vds) lm(clr_lge ~ clr_npe, data = d, weights = total_lge) else NULL),
      a = map_dbl(fit, \(f) if (is.null(f)) 0 else unname(coef(f)[1])),
      b = map_dbl(fit, \(f) if (is.null(f)) 1 else unname(coef(f)[2])),
      default_used = map_lgl(fit, is.null)
    )

  resid <- coefs |>
    mutate(res = map2(data, fit, \(d, f) if (is.null(f)) NULL else mutate(d, e = residuals(f)))) |>
    select(group, res) |>
    unnest(res)

  # Variance decomposition of residuals: between wards vs within wards.
  ward_means <- resid |> summarise(e_w = weighted.mean(e, total_lge), .by = c(group, muni_code, ward_id))
  within <- resid |> left_join(ward_means, by = c("group", "muni_code", "ward_id")) |> mutate(e_vd = e - e_w)
  sd_ward <- sd(ward_means$e_w, na.rm = TRUE)
  sd_vd <- sd(within$e_vd, na.rm = TRUE)

  # Party-specific local spreads (O12, L049). The pooled spreads above are
  # unweighted over every party-district cell, so small parties' sampling noise
  # (few votes, zeros smoothed to tiny shares) dominates them, and the same
  # spread is then given to a party on 85% of a district. Here each party's
  # spread is its OWN residual variance, weighted by its votes, net of the
  # expected sampling variance of the two log shares, (1 - s) / (n s). Where
  # sampling noise is more than half a party's raw residual variance, or it
  # has fewer than transfer_min_vds districts, the net figure is unreliable
  # (it can go to zero for small parties) and the median of the reliable
  # parties' spreads is used instead. The ward/district split follows the
  # pooled ratio. Used when config model.spreads is "party". On synthetic data
  # with one structural spread of 0.15, the pooled estimate is about 0.31 and
  # the party-specific ones about 0.17 (tests/testthat/test-calibration.R).
  f_ward <- sd_ward^2 / (sd_ward^2 + sd_vd^2)
  sd_party <- resid |>
    mutate(w = total_lge * share_lge,
           samp = (1 - share_lge) / (total_lge * share_lge) + (1 - share_npe) / (total_npe * share_npe)) |>
    summarise(n_vd = n(), v_raw = sum(w * (e - sum(w * e) / sum(w))^2) / sum(w),
              v_samp = sum(w * samp) / sum(w), .by = group) |>
    mutate(reliable = v_samp <= 0.5 * v_raw & n_vd >= (cfg$model$transfer_min_vds %||% 30),
           sd_total = if_else(reliable, sqrt(pmax(v_raw - v_samp, 0.05^2)), NA_real_))
  fallback <- if (any(sd_party$reliable)) median(sd_party$sd_total, na.rm = TRUE) else sqrt(sd_ward^2 + sd_vd^2)
  sd_party <- sd_party |>
    mutate(sd_total = coalesce(sd_total, fallback),
           sd_ward = sqrt(f_ward) * sd_total, sd_vd = sqrt(1 - f_ward) * sd_total)

  # Errors-in-variables correction (L039). The 2019 shares are themselves
  # sampled: a share p among n votes has log-scale sampling variance of about
  # (1 - p) / (n p). The fraction of a party's observed spatial variance that
  # is real (its reliability, lambda) attenuates the fitted slope, b = b_true
  # x lambda (L003: 0.78 fitted against 0.90 true on demo data). Corrected
  # slope b / lambda; intercept re-set so the line keeps the weighted means.
  # The noise of the clr mean term is ignored (small with several parties).
  wvar <- function(x, w) sum(w * (x - sum(w * x) / sum(w))^2) / sum(w)
  corr <- xy |>
    summarise(noise = weighted.mean((1 - share_npe) / (total_npe * share_npe), total_lge),
              var_x = wvar(clr_npe, total_lge),
              mx = weighted.mean(clr_npe, total_lge), my = weighted.mean(clr_lge, total_lge),
              a_one = weighted.mean(clr_lge - clr_npe, total_lge), .by = group) |>
    mutate(lambda = pmin(1, pmax(0.2, 1 - noise / var_x)))
  coefs <- coefs |> left_join(corr, by = "group") |>
    mutate(b_corr = if_else(default_used, 1, pmin(b / lambda, 2)),
           a_corr = if_else(default_used, 0, my - b_corr * mx),
           a_one = if_else(default_used, 0, a_one),
           lambda = coalesce(lambda, 1))

  list(
    coefs = select(coefs, group, n_vd, a, b, default_used, lambda, a_corr, b_corr, a_one),
    sd_ward = sd_ward,
    sd_vd = sd_vd,
    sd_party = sd_party,
    n_pairs = nrow(xy),
    xy = select(xy, muni_code, vd, group, clr_npe, clr_lge, total_lge, share_lge) # for the closure test (L047)
  )
}

#' Choose how the fitted translation is carried forward (L039):
#'   b_mode "fitted"     slopes and intercepts as fitted (attenuated slopes)
#'          "corrected"  errors-in-variables corrected slopes, matching intercepts
#'          "one"        slope fixed at 1 (the national pattern unsquashed)
#'   shrink              multiplies every fitted premium (1 = as fitted)
apply_transfer_variant <- function(transfer, b_mode = c("fitted", "corrected", "one"), shrink = 1,
                                   calibrate = FALSE) {
  b_mode <- match.arg(b_mode)
  cf <- transfer$coefs
  if (b_mode == "corrected") cf <- mutate(cf, a = if_else(default_used, a, a_corr), b = if_else(default_used, b, b_corr))
  if (b_mode == "one") cf <- mutate(cf, a = if_else(default_used, a, a_one), b = 1)
  if (calibrate) cf <- calibrate_intercepts(transfer$xy, cf)   # O13, L049: before halving
  cf <- mutate(cf, a = if_else(default_used, a, a * shrink))
  transfer$coefs <- cf
  transfer$variant <- list(b_mode = b_mode, shrink = shrink, calibrate = calibrate)
  transfer
}

#' Intercepts calibrated to reproduce the fitted election's totals (O13, L049).
#'
#' The fitted premium is a vote-weighted mean of district log-ratios, and the
#' mean of log ratios is not the log ratio of the totals: predicting its own
#' local election in-sample, the translation overstated the DA's provincial
#' share by about 2 points and understated the ANC's by 1 to 2 in both cycles
#' (L048). Here each fitted party's intercept is adjusted, with its slope
#' fixed, until the in-sample prediction without noise reproduces every
#' party's total votes among the parties in the fit (iterative proportional
#' fitting on the intercepts). The common level is anchored to the original
#' intercepts' mean, which the softmax ignores anyway.
calibrate_intercepts <- function(xy, coefs, iter = 200, tol = 1e-9) {
  d <- xy |> inner_join(select(coefs, group, a, b, default_used), by = "group") |>
    mutate(votes = total_lge * share_lge)
  keys <- distinct(d, muni_code, vd)
  grp <- sort(unique(d$group))
  r <- match(paste(d$muni_code, d$vd), paste(keys$muni_code, keys$vd)); c <- match(d$group, grp)
  X <- matrix(-Inf, nrow(keys), length(grp)); X[cbind(r, c)] <- d$b * d$clr_npe
  V <- matrix(0, nrow(keys), length(grp)); V[cbind(r, c)] <- d$votes
  tot <- rowSums(V); actual <- colSums(V)
  cf <- coefs |> filter(group %in% grp)
  a_new <- cf$a[match(grp, cf$group)]
  free <- !cf$default_used[match(grp, cf$group)] & actual > 0
  a0 <- mean(a_new[free])
  for (it in seq_len(iter)) {
    pred <- colSums(softmax_rows(sweep(X, 2, a_new, "+")) * tot)
    step <- log(actual) - log(pmax(pred, 1e-12))
    a_new[free] <- a_new[free] + step[free]
    a_new[free] <- a_new[free] - mean(a_new[free]) + a0
    if (max(abs(step[free])) < tol) break
  }
  cal <- tibble(group = grp[free], a_cal = a_new[free])
  coefs |> left_join(cal, by = "group") |>
    mutate(a_uncal = a, a = coalesce(a_cal, a)) |> select(-a_cal)
}

# 3. Baseline for every 2026 voting district --------------------------------------

#' Ward/PR ticket-split ratio per municipality and group, from the last LGE
ward_pr_ratio <- function(lge_prev, groups) {
  lge_prev |>
    filter(ballot %in% c("PR", "Ward")) |>
    left_join(select(groups, muni_code, party, group), by = c("muni_code", "party")) |>
    mutate(group = coalesce(group, "OTHER")) |>
    filter(group != "ABSENT") |>
    summarise(votes = sum(votes), .by = c(muni_code, ballot, group)) |>
    mutate(share = votes / sum(votes), .by = c(muni_code, ballot)) |>
    select(-votes) |>
    pivot_wider(names_from = ballot, values_from = share, values_fill = 0) |>
    mutate(log_ward_ratio = pmin(pmax(log((Ward + 1e-3) / (PR + 1e-3)), -1.5), 1.5)) |>
    select(muni_code, group, log_ward_ratio)
}

#' Weighted mean that ignores pairs where either value or weight is missing.
#' (stats::weighted.mean(na.rm = TRUE) drops missing VALUES only; one missing
#' WEIGHT still returns NA, MODEL-LOG L015.)
wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) NA_real_ else sum(x[ok] * w[ok]) / sum(w[ok])
}

#' Registered voters per 2026 VD, from the best available source (L016):
#'   1. MDB REGPOP on the 2026 VD layer (a 2024 snapshot)
#'   2. the IEC's own 2024 count for the same VD number (national results file)
#'   3. the ward median, then the municipality median
#' The source is recorded per VD and summarised in check C12.
impute_registration <- function(vd_new, fallback = NULL) {
  out <- vd_new |> mutate(registered = if_else(registered > 0, registered, NA_real_),
                          reg_source = if_else(is.na(registered), NA_character_, "MDB REGPOP"))
  if (!is.null(fallback) && nrow(fallback)) {
    fb <- fallback |> filter(registered > 0) |> distinct(muni_code, vd, .keep_all = TRUE) |>
      select(muni_code, vd, reg_fb = registered)
    out <- out |> left_join(fb, by = c("muni_code", "vd")) |>
      mutate(reg_source = if_else(is.na(registered) & !is.na(reg_fb), "IEC 2024 count", reg_source),
             registered = coalesce(registered, reg_fb)) |>
      select(-reg_fb)
  }
  out |>
    mutate(reg_source = if_else(is.na(registered), "ward median", reg_source)) |>
    mutate(registered = coalesce(registered, median(registered, na.rm = TRUE)), .by = c(muni_code, ward_id)) |>
    mutate(reg_source = if_else(is.na(registered), "municipality median", reg_source)) |>
    mutate(registered = coalesce(registered, median(registered, na.rm = TRUE)), .by = muni_code) |>
    mutate(reg_imputed = reg_source != "MDB REGPOP")
}

build_baseline <- function(npe_latest, lge_prev, vd_new, groups, transfer, contests = NULL, ward_ratio = TRUE) {
  vd_new <- impute_registration(vd_new, fallback = distinct(npe_latest, muni_code, vd, registered))
  national <- groups |> filter(origin != "local_only")
  local <- groups |> filter(origin == "local_only") |> distinct(muni_code, group)

  # (a) national parties: transferred clr from the latest NPE
  nat <- group_shares(npe_latest, national) |>
    left_join(transfer$coefs, by = "group") |>
    mutate(a = coalesce(a, 0), b = coalesce(b, 1),
           eta = a + b * clr) |>
    mutate(log_p = eta - log(sum(exp(eta))), .by = c(muni_code, vd)) |>
    select(muni_code, vd, group, log_p)

  # (b) local-only parties: carry their last LGE PR share
  loc <- if (nrow(local)) {
    group_shares(filter(lge_prev, ballot == "PR"), groups) |>
      semi_join(local, by = c("muni_code", "group")) |>
      select(muni_code, vd, group, share)
  } else tibble(muni_code = character(), vd = character(), group = character(), share = numeric())

  local_mass <- loc |> summarise(local = sum(share), .by = c(muni_code, vd))

  hist <- nat |>
    left_join(local_mass, by = c("muni_code", "vd")) |>
    mutate(eta_pr = log_p + log1p(-coalesce(local, 0))) |>
    select(muni_code, vd, group, eta_pr) |>
    bind_rows(loc |> transmute(muni_code, vd, group, eta_pr = log(share)))

  # turnout baseline from the last LGE (PR ballot), dropping impossible values
  turnout <- lge_prev |>
    filter(ballot == "PR") |>
    distinct(muni_code, vd, registered, spoilt, vd_valid) |>
    mutate(turnout = (vd_valid + spoilt) / registered) |>
    filter(is.finite(turnout), turnout > 0.02, turnout <= 1.05) |>
    mutate(turnout = pmin(turnout, 0.98)) |>
    select(muni_code, vd, turnout)

  # (c) every 2026 VD; impute VDs with no history from their 2026 ward
  base <- vd_new |>
    left_join(hist, by = c("muni_code", "vd"), relationship = "one-to-many") |>
    left_join(turnout, by = c("muni_code", "vd"))

  have_hist <- base |> filter(!is.na(group))
  ward_fill <- have_hist |>
    summarise(eta_w = wmean(eta_pr, registered), t_w = wmean(turnout, registered),
              .by = c(muni_code, ward_id, group))
  muni_fill <- have_hist |>
    summarise(eta_m = wmean(eta_pr, registered), t_m = wmean(turnout, registered),
              .by = c(muni_code, group))

  missing <- base |> filter(is.na(group)) |> select(-group, -eta_pr, -turnout) |>
    left_join(distinct(muni_fill, muni_code, group), by = "muni_code", relationship = "many-to-many") |>
    left_join(ward_fill, by = c("muni_code", "ward_id", "group")) |>
    left_join(muni_fill, by = c("muni_code", "group")) |>
    mutate(eta_pr = coalesce(eta_w, eta_m), turnout = coalesce(t_w, t_m),
           imputed = if_else(is.na(eta_w), "municipality", "ward")) |>
    select(-eta_w, -eta_m, -t_w, -t_m)

  # Averaging LOG shares across a ward is a geometric mean, so by Jensen's
  # inequality imputed VDs summed to < 1 (0.97 seen in the demo; MODEL-LOG L004).
  # Renormalise every VD so exp(eta_pr) sums to exactly 1.
  overall_turnout <- median(turnout$turnout, na.rm = TRUE)
  out <- bind_rows(mutate(have_hist, imputed = "none"), missing) |>
    mutate(turnout_source = if_else(is.na(turnout), "imputed", "2021 LGE"), .by = c(muni_code, vd)) |>
    # any share still missing: municipality average for that party, else ~0
    mutate(eta_pr = coalesce(eta_pr, wmean(eta_pr, registered)), .by = c(muni_code, group)) |>
    mutate(eta_pr = coalesce(eta_pr, log(1e-4))) |>
    mutate(eta_pr = eta_pr - log(sum(exp(eta_pr))), .by = c(muni_code, vd)) |>
    mutate(turnout = coalesce(turnout, wmean(turnout, registered)), .by = muni_code) |>
    mutate(turnout = coalesce(turnout, overall_turnout)) |>
    left_join(ward_pr_ratio(lge_prev, groups), by = c("muni_code", "group")) |>
    mutate(log_ward_ratio = if (ward_ratio) coalesce(log_ward_ratio, 0) else 0) # ratio off: L039

  # contestation: which groups have a ward candidate in which 2026 ward
  if (is.null(contests)) {
    out$contests_ward <- TRUE
  } else {
    out <- out |>
      left_join(mutate(distinct(contests, muni_code, ward_id, group), contests_ward = TRUE),
                by = c("muni_code", "ward_id", "group")) |>
      mutate(contests_ward = coalesce(contests_ward, group == "OTHER"))
  }
  out
}

# 4. Simulation -----------------------------------------------------------------

softmax_rows <- function(eta) {
  mx <- eta[cbind(seq_len(nrow(eta)), max.col(eta, ties.method = "first"))]
  z <- exp(eta - mx)
  z / rowSums(z)
}

#' Offsets that make the local (ward + VD) noise mean-preserving (L047).
#'
#' The simulation adds noise with ONE spread for every party on the log scale.
#' Because shares are a softmax of log-scale values (convex), that noise raises
#' small parties' expected shares and lowers a dominant party's, even though
#' every party's noise is centred on zero: a party on 85% of a district loses
#' share on average. The offsets solve E[softmax(E + delta + e)] = softmax(E)
#' row by row, by fixed-point iteration over K fixed noise draws, so the
#' baseline share becomes the expected share instead of the median of the
#' log share. Used only when config model.noise_centring is "mean".
mean_preserving_offsets <- function(E, sd_local, K = 400, iter = 8, seed = 1) {
  target <- softmax_rows(E)
  delta <- matrix(0, nrow(E), ncol(E))
  sd_local <- rep_len(sd_local, ncol(E))       # one spread, or one per party (L049)
  if (any(!is.finite(sd_local)) || all(sd_local <= 0)) return(delta)
  for (it in seq_len(iter)) {
    m <- matrix(0, nrow(E), ncol(E))
    for (k in seq_len(K)) {
      e <- withr::with_seed(seed + k, matrix(rnorm(length(E), 0, rep(sd_local, each = nrow(E))), nrow(E)))
      m <- m + softmax_rows(E + delta + e)
    }
    m <- m / K
    delta <- delta + log(pmax(target, 1e-300)) - log(pmax(m, 1e-300))
    delta <- delta - rowMeans(delta) # softmax ignores a constant per row
  }
  delta
}

#' Province-wide shocks shared by every municipality within a draw
# Local premium for parties without a fitted transfer (MODEL-LOG L022) --------
#
# A party that did not stand in both the 2019 national and 2021 local
# elections has no fitted `a` (how much better or worse it does in local
# elections than its national vote implies). Rather than assume a = 0 with
# certainty, `a` is treated as unknown:
#   prior     a ~ N(mean, sd) of the `a` values fitted for other parties
#   evidence  by-election results since 2021 (step 2, to be added)
# and the belief is drawn afresh in every simulation, so its uncertainty
# reaches the seat probabilities. Because `a` is a property of the party, it
# is shared by every council in a draw, like the province-level shocks.

transfer_prior <- function(transfer) {
  fitted <- transfer$coefs |> filter(!default_used, group != "OTHER")
  if (nrow(fitted) < 3) {
    return(list(mean = 0, sd = 0.3, n = nrow(fitted),
                note = "fewer than 3 fitted parties: fallback prior N(0, 0.3)"))
  }
  list(mean = mean(fitted$a), sd = sd(fitted$a), n = nrow(fitted),
       note = sprintf("mean and sd of a over %d fitted parties", nrow(fitted)))
}

local_premium <- function(groups, transfer, evidence = NULL) {
  prior <- transfer_prior(transfer)
  fitted <- transfer$coefs |> filter(!default_used) |> pull(group)
  groups |>
    filter(origin == "national", !group %in% fitted) |>
    distinct(group) |>
    mutate(prior_mean = prior$mean, prior_sd = prior$sd, prior_note = prior$note,
           # evidence columns always present (blank without evidence), so every
           # consumer sees one schema whether or not by-elections exist
           wards = 0L, theta = NA_real_, se_sampling = NA_real_, se = NA_real_,
           post_mean = prior_mean, post_sd = prior_sd, source = "prior only")
}

draw_province_effects <- function(groups_all, swing_priors, cfg, premium = NULL) {
  n <- cfg$model$n_draws
  sd_p <- cfg$model$sd$province_party
  priors <- swing_priors |> filter(scope == "province")
  withr::with_seed(cfg$model$seed, {
    eff <- map(groups_all, \(g) {
      pr <- priors |> filter(party == g)
      m <- if (nrow(pr)) pr$mean[1] else 0
      s <- if (nrow(pr)) pr$sd[1] else sd_p
      lp <- if (!is.null(premium)) filter(premium, group == g) else tibble()
      if (nrow(lp)) { # unknown local premium: shift the mean, add its variance
        m <- m + lp$post_mean[1]
        s <- sqrt(s^2 + lp$post_sd[1]^2)
      }
      rnorm(n, m, s)
    })
    turnout <- rnorm(n, 0, cfg$model$sd$turnout_province)
  })
  mat <- do.call(cbind, eff)
  colnames(mat) <- groups_all
  list(party = mat, turnout = turnout)
}

simulate_municipality <- function(base_m, council, prov, other_w, swing_priors, transfer, cfg,
                                  entrant_shares = NULL, entrant_contests = NULL) {
  code <- base_m$muni_code[1]
  n_draws <- cfg$model$n_draws
  sdc <- cfg$model$sd
  sd_ward <- sdc$ward_party %||% transfer$sd_ward
  sd_vd <- sdc$vd_party %||% transfer$sd_vd
  B <- council$total_seats

  wide <- base_m |>
    select(vd, ward_id, registered, turnout, group, eta_pr, log_ward_ratio, contests_ward) |>
    arrange(vd, group)
  groups <- sort(unique(wide$group))
  vds <- wide |> distinct(vd, ward_id, registered, turnout) |> arrange(vd)
  E <- wide |> select(vd, group, eta_pr) |> pivot_wider(names_from = group, values_from = eta_pr) |>
    arrange(vd) |> select(all_of(groups)) |> as.matrix()
  E[is.na(E)] <- log(1e-4)
  C <- wide |> select(vd, group, contests_ward) |> pivot_wider(names_from = group, values_from = contests_ward) |>
    arrange(vd) |> select(all_of(groups)) |> as.matrix()
  C[is.na(C)] <- FALSE
  ratio <- wide |> distinct(group, log_ward_ratio) |> arrange(match(group, groups)) |> pull(log_ward_ratio)
  # L049: one spread per party (config model.spreads: pooled | party), unless
  # config fixes the spreads; newcomers keep the pooled spreads
  sw_p <- rep(sd_ward, length(groups)); sv_p <- rep(sd_vd, length(groups))
  if (identical(cfg$model$spreads, "party") && !is.null(transfer$sd_party) &&
      is.null(sdc$ward_party) && is.null(sdc$vd_party)) {
    sp <- transfer$sd_party
    i <- match(groups, sp$group)
    sw_p <- coalesce(sp$sd_ward[i], median(sp$sd_ward)); sv_p <- coalesce(sp$sd_vd[i], median(sp$sd_vd))
  }
  # L047: optionally re-centre the local noise so each VD's expected shares
  # equal its baseline shares (config model.noise_centring: none | mean)
  if (identical(cfg$model$noise_centring, "mean"))
    E <- E + mean_preserving_offsets(E, sqrt(sw_p^2 + sv_p^2), seed = cfg$model$seed + 7L)

  # Newcomers (L035). They have no baseline, so they are inserted AFTER the
  # established parties' shares have been computed with all their shocks:
  # each newcomer takes its drawn council share (varied across wards and VDs
  # with the ward and VD spreads, re-centred so the council total equals the
  # draw exactly) and everyone else is scaled down. Inserting them before the
  # shocks instead diluted them by about 14% (L035).
  ent <- if (!is.null(entrant_shares)) setdiff(colnames(entrant_shares), groups) else character()
  n_e <- length(ent)
  C_ent <- if (n_e) vapply(ent, \(p) {
    if (is.null(entrant_contests)) return(rep(TRUE, nrow(vds)))
    vds$ward_id %in% entrant_contests$ward_id[entrant_contests$muni_code == code & entrant_contests$party == p]
  }, logical(nrow(vds))) else NULL
  if (n_e) C_ent <- matrix(C_ent, nrow(vds), n_e, dimnames = list(NULL, ent))

  # Refuse bad inputs here, with names, rather than fail inside the seat maths
  bad <- c(
    if (length(B) != 1 || !is.finite(B) || B < 1) sprintf("council size is %s", paste(B, collapse = ",")),
    if (!is.finite(sd_ward)) "ward spread not estimable (set model.sd.ward_party in config.yml)",
    if (!is.finite(sd_vd)) "VD spread not estimable (set model.sd.vd_party in config.yml)",
    if (any(!is.finite(vds$registered))) sprintf("%d VDs with missing registration", sum(!is.finite(vds$registered))),
    if (any(!is.finite(vds$turnout))) sprintf("%d VDs with missing turnout", sum(!is.finite(vds$turnout))),
    if (any(is.na(vds$ward_id))) sprintf("%d VDs with no 2026 ward", sum(is.na(vds$ward_id))),
    if (n_e && nrow(entrant_shares) < cfg$model$n_draws) "fewer newcomer draws than simulation draws"
  )
  if (length(bad)) stop("simulate_municipality(", code, "): ", paste(bad, collapse = "; "), call. = FALSE)

  wards <- sort(unique(vds$ward_id))
  w_idx <- match(vds$ward_id, wards)
  n_v <- nrow(vds); n_p <- length(groups); n_w <- length(wards)
  reg <- vds$registered
  t_logit <- qlogis(pmin(pmax(vds$turnout, 0.02), 0.98))

  muni_prior <- swing_priors |> filter(scope == code)
  muni_mean <- map_dbl(groups, \(g) { r <- muni_prior |> filter(party == g); if (nrow(r)) r$mean[1] else 0 })
  muni_sd <- map_dbl(groups, \(g) { r <- muni_prior |> filter(party == g); if (nrow(r)) r$sd[1] else sdc$muni_party })

  # OTHER is split into real parties by fixed weights before seats are allocated
  ow <- other_w |> filter(muni_code == code)
  seat_parties <- c(setdiff(groups, "OTHER"), ent, setdiff(ow$party, c(groups, ent)))
  all_groups <- c(groups, ent)

  seats <- matrix(0L, n_draws, length(seat_parties), dimnames = list(NULL, seat_parties))
  winners <- matrix(NA_character_, n_draws, n_w, dimnames = list(NULL, wards))
  ward_share <- array(NA_real_, c(n_draws, n_w, n_p + n_e), dimnames = list(NULL, wards, all_groups))
  pr_share <- matrix(NA_real_, n_draws, n_p + n_e, dimnames = list(NULL, all_groups))
  turnout_out <- numeric(n_draws)
  n_ties <- 0L

  prov_party <- prov$party[, groups, drop = FALSE]
  set.seed(cfg$model$seed + sum(utf8ToInt(code)))
  for (d in seq_len(n_draws)) {
    shift <- prov_party[d, ] + rnorm(n_p, muni_mean, muni_sd)
    ward_eff <- matrix(rnorm(n_w * n_p, 0, rep(sw_p, each = n_w)), n_w, n_p)
    eta <- E + rep(shift, each = n_v) + ward_eff[w_idx, , drop = FALSE] +
      matrix(rnorm(n_v * n_p, 0, rep(sv_p, each = n_v)), n_v, n_p)
    p_pr <- softmax_rows(eta)
    eta_w <- eta + rep(ratio, each = n_v)
    eta_w[!C] <- -Inf
    p_w <- softmax_rows(eta_w)

    t <- plogis(t_logit + prov$turnout[d] + rnorm(1, 0, sdc$turnout_municipality) +
                  rnorm(n_v, 0, sdc$turnout_vd))
    voters <- reg * t
    if (n_e) {
      # newcomer noise from its own stream, so established parties see exactly
      # the same random numbers with or without newcomers (paired, L036)
      m <- withr::with_seed(cfg$model$seed + 100000L + d, exp(
        matrix(rnorm(n_w * n_e, 0, sd_ward), n_w, n_e)[w_idx, , drop = FALSE] +
          matrix(rnorm(n_v * n_e, 0, sd_vd), n_v, n_e)))
      m <- sweep(m, 2, colSums(m * voters) / sum(voters), "/")   # voter-weighted mean exactly 1
      s_vd <- sweep(m, 2, entrant_shares[d, ent], "*")
      s_vd <- s_vd * pmin(1, 0.95 / pmax(rowSums(s_vd), 1e-12))  # never more than 95% of a VD
      p_pr <- cbind(p_pr * (1 - rowSums(s_vd)), s_vd)
      s_w <- s_vd * C_ent                                          # ward ballot: only where standing
      p_w <- cbind(p_w * (1 - rowSums(s_w)), s_w)
    }
    v_pr <- voters * p_pr
    v_w <- voters * p_w
    wv <- rowsum(v_w, w_idx, reorder = TRUE)
    win <- all_groups[max.col(wv, ties.method = "first")]

    combined <- colSums(v_pr) + colSums(v_w)
    names(combined) <- all_groups
    # split OTHER into its constituent parties
    votes <- combined[setdiff(all_groups, "OTHER")]
    if ("OTHER" %in% groups && nrow(ow)) {
      extra <- combined[["OTHER"]] * ow$weight
      names(extra) <- ow$party
      votes <- c(votes, extra[setdiff(names(extra), names(votes))])
    }
    # a ward "won by OTHER" goes to OTHER's largest constituent
    win_real <- if (nrow(ow)) replace(win, win == "OTHER", ow$party[which.max(ow$weight)]) else win
    wins <- table(factor(win_real, levels = names(votes)))
    alloc <- schedule1(round(votes), setNames(as.integer(wins), names(wins)), B)
    n_ties <- n_ties + alloc$tie

    seats[d, names(alloc$seats)] <- alloc$seats
    winners[d, ] <- win_real
    ward_share[d, , ] <- wv / rowSums(wv)
    pr_share[d, ] <- colSums(v_pr) / sum(v_pr)
    turnout_out[d] <- sum(voters) / sum(reg)
  }

  list(muni_code = code, total_seats = B, groups = all_groups, wards = wards, entrants = ent,
       seats = seats, winners = winners, ward_share = ward_share,
       pr_share = pr_share, turnout = turnout_out, n_ties = n_ties)
}

# 5. Summaries --------------------------------------------------------------------

summarise_simulation <- function(sim, cfg) {
  code <- sim$muni_code
  majority <- floor(sim$total_seats / 2) + 1
  s <- sim$seats
  keep <- colnames(s)[colSums(s) > 0]
  s <- s[, keep, drop = FALSE]

  seat_summary <- tibble(
    muni_code = code, party = keep,
    seats_median = apply(s, 2, median),
    seats_q05 = apply(s, 2, quantile, 0.05, type = 1),
    seats_q95 = apply(s, 2, quantile, 0.95, type = 1),
    seats_mean = colMeans(s),
    p_any_seat = colMeans(s > 0),
    p_majority = colMeans(s >= majority),
    p_largest = colMeans(s == apply(s, 1, max))
  ) |> arrange(desc(seats_mean))

  seat_dist <- as_tibble(s) |>
    pivot_longer(everything(), names_to = "party", values_to = "seats") |>
    count(party, seats, name = "draws") |>
    mutate(prob = draws / nrow(s), muni_code = code, .before = 1)

  ward_probs <- as_tibble(sim$winners) |>
    pivot_longer(everything(), names_to = "ward_id", values_to = "party") |>
    count(ward_id, party, name = "draws") |>
    mutate(p_win = draws / nrow(sim$winners), muni_code = code, .before = 1) |>
    select(-draws)

  vote_share <- tibble(
    muni_code = code, party = sim$groups,
    pr_share_median = apply(sim$pr_share, 2, median),
    pr_share_q05 = apply(sim$pr_share, 2, quantile, 0.05),
    pr_share_q95 = apply(sim$pr_share, 2, quantile, 0.95)
  )

  list(
    seat_summary = seat_summary,
    seat_dist = seat_dist,
    control = mutate(control_outcomes(s, sim$total_seats), muni_code = code, .before = 1),
    coalitions = mutate(coalition_summary(s, sim$total_seats, cfg$model$coalition_max_parties),
                        muni_code = code, .before = 1),
    ward_probs = ward_probs,
    vote_share = vote_share,
    meta = tibble(muni_code = code, total_seats = sim$total_seats, majority = majority,
                  n_wards = length(sim$wards), n_draws = nrow(s),
                  turnout_median = median(sim$turnout),
                  turnout_q05 = quantile(sim$turnout, 0.05), turnout_q95 = quantile(sim$turnout, 0.95),
                  remainder_ties = sim$n_ties)
  )
}

bind_summaries <- function(summaries) {
  nm <- names(summaries[[1]])
  set_names(map(nm, \(k) bind_rows(map(summaries, k))), nm)
}
