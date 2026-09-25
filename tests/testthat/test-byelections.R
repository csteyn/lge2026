# L023: the by-election estimate of a party's local premium.

make_case <- function(offset, n_wards = 6, seed = 1) {
  set.seed(seed)
  vds <- tibble(ward_id = rep(sprintf("W%02d", seq_len(n_wards)), each = 2), vd = sprintf("V%03d", seq_len(2 * n_wards)))
  eta <- vds |> cross_join(tibble(group = c("T", "R1", "R2"), fitted = c(FALSE, TRUE, TRUE))) |>
    mutate(muni_code = "M", eta_ward = rnorm(n(), 0, 0.5))
  be <- eta |>
    mutate(true = eta_ward + if_else(group == "T", offset, 0)) |>
    mutate(votes = round(5000 * exp(true) / sum(exp(true))), .by = vd) |>
    transmute(eeid = 1, date = as.Date("2025-01-01"), muni_code, ward_id, vd, party = group, votes)
  list(eta = select(eta, muni_code, vd, group, eta_ward, fitted), be = be)
}

test_that("a known local premium is recovered from by-election residuals", {
  k <- make_case(offset = 0.5)
  r <- premium_residuals(k$be, k$eta, "T")
  expect_equal(nrow(r), 6)
  expect_equal(mean(r$r), 0.5, tolerance = 0.01)
})

test_that("placebo parties come out near zero, and the posterior weights by precision", {
  k <- make_case(offset = 0.5)
  prior <- tibble(group = "T", prior_mean = -0.3, prior_sd = 0.4, prior_note = "", post_mean = -0.3,
                  post_sd = 0.4, source = "prior only")
  tr <- list(coefs = tibble(group = c("R1", "R2"), a = 0, b = 1, default_used = FALSE, n_vd = 50))
  e <- estimate_local_premium(k$be, k$eta, prior, tr, since = as.Date("2024-05-29"))
  expect_true(all(abs(e$placebo$mean) < 0.02))
  expect_equal(e$systematic_sd, 0)
  p <- e$premium
  w_prior <- 1 / 0.4^2; w_data <- 1 / p$se^2
  expect_equal(p$post_mean, (-0.3 * w_prior + p$theta * w_data) / (w_prior + w_data))
  expect_lt(p$post_sd, 0.4)
  expect_match(p$source, "prior \\+ 6 by-election wards")
})

test_that("too little evidence leaves the prior untouched, and old by-elections are excluded", {
  k <- make_case(offset = 0.5, n_wards = 2)
  prior <- tibble(group = "T", prior_mean = -0.3, prior_sd = 0.4, prior_note = "", post_mean = -0.3,
                  post_sd = 0.4, source = "prior only")
  tr <- list(coefs = tibble(group = c("R1", "R2"), a = 0, b = 1, default_used = FALSE, n_vd = 50))
  e <- estimate_local_premium(k$be, k$eta, prior, tr, since = as.Date("2024-05-29"))
  expect_equal(e$premium$post_mean, -0.3); expect_match(e$premium$source, "prior only")
  e2 <- estimate_local_premium(k$be, k$eta, prior, tr, since = as.Date("2025-06-01"))
  expect_equal(e2$premium$wards, 0L)
})

test_that("conservative name keys match only trivial variants (L024)", {
  known <- c("UMKHONTO WESIZWE", "THE ORGANIC HUMANITY MOVEMENT", "LAND PARTY", "UNITED SOUTH AFRICA")
  expect_equal(harmonise_party_names(c("UMKHONTO WESIZWE PARTY", "ORGANIC HUMANITY MOVEMENT", "LAND",
                                       "UNITED PROGRESSIVE PARTY SOUTH AFRICA"), known),
               c("UMKHONTO WESIZWE", "THE ORGANIC HUMANITY MOVEMENT", "LAND PARTY",
                 "UNITED PROGRESSIVE PARTY SOUTH AFRICA"))
})
