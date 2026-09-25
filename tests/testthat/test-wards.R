# L039: attenuation correction, transfer variants and the ward experiment.

cfg <- yaml::read_yaml(test_path("..", "..", "config.yml"))
d <- make_demo_data()
g <- define_party_groups(d$lge2021, d$npe2024, cfg, d$pr_lists)
tr <- fit_transfer(d$npe2019, d$lge2021, g, cfg)

test_that("the errors-in-variables correction moves slopes towards the known truth", {
  x <- inner_join(tr$coefs, DEMO_TRUTH, by = "group", suffix = c("", "_true")) |> filter(group != "OTHER")
  expect_lt(mean(abs(x$b_corr - x$b_true)), mean(abs(x$b - x$b_true)))
  expect_true(all(x$lambda > 0 & x$lambda <= 1))
})

test_that("defaults leave the model exactly unchanged; variants change only what they should", {
  same <- apply_transfer_variant(tr, "fitted", 1)
  expect_equal(same$coefs$a, tr$coefs$a); expect_equal(same$coefs$b, tr$coefs$b)
  one <- apply_transfer_variant(tr, "one", 1)
  expect_true(all(one$coefs$b[!one$coefs$default_used] == 1))
  half <- apply_transfer_variant(tr, "fitted", 0.5)
  expect_equal(half$coefs$a[!half$coefs$default_used], tr$coefs$a[!tr$coefs$default_used] / 2)
  cor <- apply_transfer_variant(tr, "corrected", 1)
  expect_equal(cor$coefs$b[!cor$coefs$default_used], tr$coefs$b_corr[!tr$coefs$default_used])
  b0 <- build_baseline(d$npe2024, d$lge2021, d$vd2026, g, tr, NULL, ward_ratio = FALSE)
  expect_true(all(b0$log_ward_ratio == 0))
})

test_that("the ward experiment applies its pre-registered rule", {
  bt <- assemble_demo_backtest(d)
  wg <- backtest_ward_grid(bt, cfg, NULL, n_draws = 40)
  expect_equal(nrow(wg), 12); expect_equal(sum(wg$current), 1)
  if (any(wg$eligible)) expect_equal(sum(wg$best), 1) else expect_equal(sum(wg$adopt), 0) # none eligible: no change
  expect_true(all(!wg$adopt | (wg$eligible & wg$ward_brier < wg$ward_brier[wg$current])))
  sc <- score_chain(run_chain(bt$inputs, cfg, 40), bt$truth)
  wd <- ward_diagnostics(sc, naive_rule(bt$inputs, bt$truth, "previous_local"), naive_rule(bt$inputs, bt$truth, "national_as_is"))
  expect_true(all(c("model_pick", "national_pick", "previous_pick", "case") %in% names(wd)))
  expect_equal(nrow(wd), nrow(bt$truth$winners))
})
