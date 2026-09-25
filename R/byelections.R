# byelections.R -----------------------------------------------------------------
# Step 2 of O5 (MODEL-LOG L023): estimate the local premium `a` of parties
# with no fitted national-to-local translation from by-elections since 2021.
#
# A by-election is a real local election in the current period. For every
# voting district of a by-election ward in which target party T stood:
#
#   observed   y = log(votes_T) - log(sum of votes of the reference parties)
#   predicted  y_hat = (eta_T + rho_T) - log(sum_R exp(eta_R + rho_R))
#
# eta: the district's 2024 national share, translated with the fitted
# transfer (T gets a = 0, b = 1); rho: the council's 2021 ward/PR ratio
# (by-elections are ward elections). Reference parties R are the fitted
# parties that also stood, so parties with no history cannot distort the
# comparison. The residual y - y_hat is T's premium plus noise.
#
# Wards are the independent units (districts in a ward share one campaign):
# residuals are vote-weighted within a ward, then averaged across wards.
#
# Systematic by-election distortions (very low turnout, local candidates,
# change since 2024 common to all parties) are measured with a PLACEBO: the
# same estimate for each fitted party, whose true premium is already in the
# model and so should come out near zero. Their excess dispersion becomes a
# systematic-error term added to T's standard error.

map_byelection_parties <- function(byelections, abbreviations) {
  byelections |>
    mutate(ward_id = as.character(ward_id), vd = as.character(vd),
           muni_code = muni_code_from_label(muni_name)) |>
    left_join(select(abbreviations, abbr, party_full = party), by = c("party" = "abbr")) |>
    filter(!is.na(party_full)) |>
    mutate(party = party_full) |> select(-party_full)
}

#' Predicted log-share (before normalisation) per VD x group, 2024 base
byelection_eta <- function(npe_latest, groups, transfer, ward_ratio) {
  national <- groups |> filter(origin == "national")
  group_shares(npe_latest, national) |>
    left_join(select(transfer$coefs, group, a, b, default_used), by = "group") |>
    mutate(fitted = coalesce(!default_used, FALSE), a = if_else(fitted, a, 0), b = if_else(fitted, b, 1),
           eta = a + b * clr) |>
    left_join(ward_ratio, by = c("muni_code", "group")) |>
    mutate(eta_ward = eta + coalesce(log_ward_ratio, 0)) |>
    select(muni_code, vd, group, eta_ward, fitted)
}

#' Ward-level residuals for one target party
premium_residuals <- function(be, eta, target, since = as.Date("1900-01-01"), exclude_ref = character()) {
  be <- be |> filter(date >= since)
  wards_with_t <- be |> filter(party == target, votes > 0) |> distinct(eeid, ward_id)
  if (!nrow(wards_with_t)) return(tibble())
  obs <- be |> semi_join(wards_with_t, by = c("eeid", "ward_id")) |>
    summarise(votes = sum(votes), .by = c(eeid, date, muni_code, ward_id, vd, party))
  pred <- eta |> filter(group == target | (fitted & group != target & !group %in% exclude_ref))
  obs |>
    inner_join(pred, by = c("muni_code", "vd", "party" = "group")) |>
    mutate(role = if_else(party == target, "T", "R")) |>
    summarise(
      has_t = any(role == "T"), has_r = any(role == "R"),
      y = log(sum(votes[role == "T"]) + 0.5) - log(sum(votes[role == "R"]) + 0.5),
      y_hat = eta_ward[role == "T"][1] - log(sum(exp(eta_ward[role == "R"]))),
      w = sum(votes), .by = c(eeid, date, muni_code, ward_id, vd)) |>
    filter(has_t, has_r) |>
    mutate(r = y - y_hat) |>
    summarise(r = weighted.mean(r, w), votes = sum(w), vds = n(), .by = c(eeid, date, muni_code, ward_id)) |>
    mutate(target = target, .before = 1)
}

#' Combine: estimate, placebo systematic error, precision-weighted posterior
estimate_local_premium <- function(be, eta, premium_prior, transfer, since, min_wards = 3) {
  fitted <- transfer$coefs |> filter(!default_used, group != "OTHER") |> pull(group)
  placebo <- map_dfr(fitted, \(p) {
    r <- premium_residuals(be, eta, p, since, exclude_ref = character())
    if (nrow(r) < min_wards) return(tibble())
    tibble(party = p, wards = nrow(r), mean = mean(r$r), se = sd(r$r) / sqrt(nrow(r)))
  })
  sys_sd <- if (nrow(placebo) >= 2) sqrt(max(0, mean(placebo$mean^2) - mean(placebo$se^2))) else 0

  targets <- premium_prior$group
  ward_res <- map_dfr(targets, \(t) premium_residuals(be, eta, t, since))
  est <- if (nrow(ward_res)) {
    ward_res |> summarise(wards = n(), theta = mean(r), se_sampling = sd(r) / sqrt(n()), .by = target)
  } else { # no evidence at all in the window: every party keeps its prior
    tibble(target = character(), wards = integer(), theta = double(), se_sampling = double())
  }
  post <- premium_prior |>
    select(-any_of(c("wards", "theta", "se_sampling", "se"))) |>
    left_join(est, by = c("group" = "target")) |>
    mutate(
      wards = coalesce(wards, 0L),
      use = wards >= min_wards & is.finite(se_sampling),
      se = if_else(use, sqrt(se_sampling^2 + sys_sd^2), NA_real_),
      post_mean = if_else(use, (prior_mean / prior_sd^2 + theta / se^2) / (1 / prior_sd^2 + 1 / se^2), prior_mean),
      post_sd = if_else(use, sqrt(1 / (1 / prior_sd^2 + 1 / se^2)), prior_sd),
      source = if_else(use, sprintf("prior + %d by-election wards since %s", wards, since),
                       sprintf("prior only (%d by-election wards; need %d)", wards, min_wards))
    )
  list(premium = post, ward_residuals = ward_res, placebo = placebo, systematic_sd = sys_sd, since = since)
}

#' Pipeline entry point: prior, updated with by-election evidence where it exists
local_premium_with_evidence <- function(groups, transfer, byelections, abbreviations, npe_latest,
                                        baseline, munis, cfg) {
  prior <- local_premium(groups, transfer)
  empty <- list(premium = prior, ward_residuals = tibble(), placebo = tibble(),
                systematic_sd = NA_real_, since = NA)
  if (is.null(byelections) || !nrow(byelections) || !nrow(prior)) return(empty)
  be <- map_byelection_parties(byelections, abbreviations) |> filter(muni_code %in% munis)
  if (!nrow(be)) return(empty)
  eta <- byelection_eta(npe_latest, groups, transfer, distinct(baseline, muni_code, group, log_ward_ratio))
  estimate_local_premium(be, eta, prior, transfer,
                         since = as.Date(cfg$model$byelection_since %||% "2024-05-29"),
                         min_wards = cfg$model$byelection_min_wards %||% 3)
}
