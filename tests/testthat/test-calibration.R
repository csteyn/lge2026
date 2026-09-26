# L049: calibrated intercepts (O13), party-specific spreads (O12), and the rule.

cfg <- yaml::read_yaml(test_path("..", "..", "config.yml"))
d <- make_demo_data()
g <- define_party_groups(d$lge2021, d$npe2024, cfg, d$pr_lists)
fit <- fit_transfer(d$npe2019, d$lge2021, g, cfg)

test_that("calibrated intercepts reproduce the fitted election's totals exactly, without noise", {
  tr <- apply_transfer_variant(fit, "one", shrink = 1, calibrate = TRUE)
  cl <- closure_for_variant(fit, cfg, "B", "totals", "pooled", "mean", "demo")   # "mean" = no-noise expectation
  prov <- filter(cl, scope == "Western Cape")
  fitted <- tr$coefs$group[!tr$coefs$default_used]
  expect_equal(prov$predicted[prov$party %in% fitted], prov$actual[prov$party %in% fitted], tolerance = 1e-6)
  unc <- closure_for_variant(fit, cfg, "A", "none", "pooled", "mean", "demo") |> filter(scope == "Western Cape")
  expect_gt(max(abs(unc$predicted - unc$actual)), 1e-4)                            # the fitted means do not
  expect_true(all(c("a_uncal") %in% names(tr$coefs)))
})

test_that("party spreads recover the structural spread that the pooled estimate inflates", {
  # the demo's local elections are generated with ONE structural spread of 0.15 (R/demo.R)
  sp <- fit$sd_party
  expect_true(all(c("group", "sd_total", "sd_ward", "sd_vd", "reliable") %in% names(sp)))
  expect_true(all(sp$sd_total > 0.10 & sp$sd_total < 0.25))
  expect_gt(sqrt(fit$sd_ward^2 + fit$sd_vd^2), 0.28)               # pooled: inflated by sampling noise
  expect_true(any(!sp$reliable))                                    # small parties fall back to the median
  expect_equal(sp$sd_ward^2 + sp$sd_vd^2, sp$sd_total^2, tolerance = 1e-10)
})

test_that("party spreads reach the simulation and change it; pooled reproduces the old draws", {
  bt <- assemble_demo_backtest(d)
  c0 <- variant_cfg(cfg, "none", "pooled", "none"); c1 <- variant_cfg(cfg, "none", "party", "none")
  p0 <- prepare_chain(bt$inputs, c0, "fitted"); p1 <- prepare_chain(bt$inputs, c1, "fitted")
  a <- simulate_chain(p0, c0, 20)$sims[[1]]$pr_share; b <- simulate_chain(p1, c1, 20)$sims[[1]]$pr_share
  expect_false(isTRUE(all.equal(a, b)))
  c0$model$spreads <- NULL
  expect_equal(simulate_chain(p0, c0, 20)$sims[[1]]$pr_share, a)                 # default = pooled
})

test_that("the L049 rule is applied as written", {
  grid <- tibble(variant = LETTERS[1:5], seat_crps = c(0.38, 0.39, 0.37, 0.385, 0.40),
                 pit_cov90 = c(0.88, 0.88, 0.88, 0.99, 0.88), ward_brier = 0.15,
                 control_brier = c(0.24, 0.25, 0.24, 0.24, 0.24),
                 crps_diff = seat_crps - 0.38, crps_diff_lo = c(0, -0.01, -0.02, -0.01, 0.005),
                 crps_diff_hi = c(0, 0.03, 0.001, 0.02, 0.04), current = variant == "A")
  cl <- tidyr::expand_grid(cycle = c("x", "y"), variant = LETTERS[1:5], scope = c("Western Cape", "CPT"),
                           party = c("P", "Q")) |>
    mutate(actual = if_else(party == "P", 0.6, 0.3),
           predicted = actual + case_when(variant == "A" ~ 0.03, variant == "B" ~ 0.005, variant == "C" ~ 0.02,
                                          variant == "D" ~ 0.002, TRUE ~ 0))
  dec <- calibration_decision(grid, cl)
  expect_equal(dec$qualifies, c(FALSE, TRUE, FALSE, FALSE, FALSE))   # C fails closure, D coverage, E seats
  expect_equal(dec$variant[dec$adopt], "B")
  expect_equal(sum(calibration_decision(mutate(grid, crps_diff_lo = 0.001), cl)$adopt), 0)
})

test_that("the current configuration maps to exactly one variant", {
  expect_equal(current_variant(cfg), "A")
  expect_error(current_variant(variant_cfg(cfg, "none", "party", "mean")), "match no")
})
