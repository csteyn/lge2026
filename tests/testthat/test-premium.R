# L044: premiums measured against the national vote interpolated to the local election's date.

cfg <- yaml::read_yaml(test_path("..", "..", "config.yml"))
d <- make_demo_data()
g <- define_party_groups(d$lge2021, d$npe2024, cfg, d$pr_lists)

test_that("the interpolation fraction is where the local election falls between national elections", {
  expect_equal(interp_frac("2019-05-08", "2021-11-01", "2024-05-29"), 908 / 1848)
  expect_equal(interp_frac(cfg$dates$npe2014, cfg$dates$lge2016, cfg$dates$npe2019), 819 / 1827)
})

test_that("interpolating at fraction 0 reproduces the earlier-election fit; at fraction 1 it uses the later one", {
  base <- fit_transfer(d$npe2019, d$lge2021, g, cfg)
  f0 <- fit_transfer(d$npe2019, d$lge2021, g, cfg, npe_next = d$npe2024, frac = 0)
  expect_equal(f0$coefs$a, base$coefs$a, tolerance = 1e-8)
  expect_equal(f0$coefs$b, base$coefs$b, tolerance = 1e-8)
  f1 <- fit_transfer(d$npe2019, d$lge2021, g, cfg, npe_next = d$npe2024, frac = 1)
  expect_false(isTRUE(all.equal(f1$coefs$a, base$coefs$a)))
})

test_that("the premium experiment applies its pre-registered rule", {
  bt <- assemble_demo_backtest(d)
  pg <- backtest_premium_grid(bt, cfg, n_draws = 40)
  expect_equal(nrow(pg), 4); expect_equal(sum(pg$current), 1)
  expect_true(all(c("da_pred", "da_actual", "anc_pred", "anc_actual") %in% names(pg)))
  expect_true(all(!pg$adopt | (pg$eligible & pg$seat_crps < pg$seat_crps[pg$current])))
  if (!any(pg$eligible)) expect_equal(sum(pg$adopt), 0)
})

test_that("the interpolated basis reaches the backtest chain when the two national elections differ", {
  # the demo backtest uses one national election twice, so interpolation is a no-op there
  bt2 <- assemble_backtest_inputs(npe_prev = d$npe2019, lge_prev = d$lge2021, npe_latest = d$npe2024,
                                  lge_truth = d$lge2021, councils = d$councils, municipalities = d$municipalities,
                                  interp_frac_value = 0.5)
  e <- prepare_chain(bt2$inputs, cfg, "fitted", premium_basis = "earlier")
  i <- prepare_chain(bt2$inputs, cfg, "fitted", premium_basis = "interpolated")
  expect_false(isTRUE(all.equal(e$transfer$coefs$a, i$transfer$coefs$a)))
})
