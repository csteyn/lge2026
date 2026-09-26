# L026: the backtest machinery.

test_that("discrete PIT and sample CRPS behave as proper scores", {
  x <- c(rep(2L, 50), rep(3L, 50))
  expect_equal(pit_discrete(x, 1L, u = 0.5), 0)
  expect_equal(pit_discrete(x, 4L, u = 0.5), 1)
  expect_equal(pit_discrete(x, 2L, u = 0.5), 0.25)
  expect_equal(crps_sample(rep(5, 10), 5), 0)
  expect_gt(crps_sample(x, 7L), crps_sample(x, 3L))
})

d <- make_demo_data()
bt <- assemble_demo_backtest(d)
cfg <- yaml::read_yaml(test_path("..", "..", "config.yml"))

test_that("backtest inputs follow the forecast contract, and standing comes from the truth year", {
  expect_setequal(names(bt$inputs), c("npe_prev", "lge_prev", "npe_latest", "vd_new", "councils",
                                      "contests", "pr_lists", "municipalities", "interp_frac"))
  expect_true(bt$inputs$interp_frac > 0 && bt$inputs$interp_frac < 1)
  expect_true(all(c("seats", "winners", "pr") %in% names(bt$truth)))
  expect_equal(sum(bt$truth$seats$seats), sum(d$councils$total_seats))
})

test_that("the estimator recovers an injected province-wide surprise", {
  chain <- run_chain(bt$inputs, cfg, n_draws = 50)
  base_sd <- estimate_shock_sds(chain, bt$truth)$province
  shifted <- bt$truth
  shift <- c(`ALPHA PARTY` = 0.4, `BRAVO PARTY` = -0.4, `CHARLIE ALLIANCE` = 0.3, `DELTA FRONT` = -0.3)
  shifted$pr <- shifted$pr |>
    mutate(votes = votes * exp(coalesce(shift[party], 0))) |>
    mutate(share = votes / sum(votes), .by = muni_code)
  est <- estimate_shock_sds(chain, shifted)$province
  expect_gt(est, base_sd + 0.15)
  expect_lt(est, 0.6)
})

test_that("scoring runs end to end and in-sample 'previous local' is perfect (sanity)", {
  cfg$backtest <- list(n_draws = 200)
  r <- run_backtest(bt, cfg)
  expect_true(all(c("summary", "versus_rules", "shock_sds", "seats") %in% names(r)))
  expect_equal(r$versus_rules$seat_mae[r$versus_rules$rule == "previous_local"], 0)
  expect_true(all(r$summary$coverage90 >= 0 & r$summary$coverage90 <= 1))
})

test_that("zero intercepts keep the learned spatial structure but drop the learned premium (L027)", {
  p0 <- prepare_chain(bt$inputs, cfg, "zero"); p1 <- prepare_chain(bt$inputs, cfg, "fitted")
  expect_true(all(p0$transfer$coefs$a == 0))
  expect_equal(p0$transfer$coefs$b, p1$transfer$coefs$b)
  expect_null(p0$premium)
})

test_that("the grid applies the pre-registered selection rule", {
  g <- backtest_grid(bt, cfg, provinces = c(0.1, 0.3), munis = c(0.15), intercepts = c("fitted", "zero"), n_draws = 60)
  expect_equal(nrow(g), 4)
  expect_true(all(c("pit_cov90", "seat_crps", "eligible", "chosen") %in% names(g)))
  if (any(g$eligible)) {
    expect_equal(sum(g$chosen), 1)
    expect_equal(g$seat_crps[g$chosen], min(g$seat_crps[g$eligible]))
  }
})

test_that("scoring is exact and deterministic, however the call is written (L036)", {
  set.seed(5); x <- rpois(500, 3)
  for (y in c(0L, 3L, 9L)) expect_equal(crps_int(x, y), crps_sample(x, y), tolerance = 1e-12)
  # expected randomised-PIT coverage equals the long-run average of random PITs
  u <- mean(replicate(20000, { p <- pit_discrete(x, 4L); p >= 0.05 & p <= 0.95 }))
  expect_equal(pit_in(x, 4L, 0.05, 0.95), u, tolerance = 0.01)
  ch <- run_chain(bt$inputs, cfg, n_draws = 120)
  inline <- score_chain(run_chain(bt$inputs, cfg, n_draws = 120), bt$truth)$summary
  stored <- score_chain(ch, bt$truth)$summary
  expect_equal(inline$seat_crps, stored$seat_crps)
  expect_equal(inline$pit_cov90, stored$pit_cov90)
})

test_that("all models are scored on the same cases, so extra easy cases cannot dilute a score (L037)", {
  ch <- run_chain(bt$inputs, cfg, n_draws = 100)
  s1 <- score_chain(ch, bt$truth)
  ch2 <- ch # the same forecast plus a party it gives zero seats (as a model with more parties would)
  ch2$sims <- lapply(ch2$sims, \(s) { s$seats <- cbind(s$seats, `EXTRA PARTY` = 0L); s })
  s2 <- score_chain(ch2, bt$truth)
  expect_equal(s2$summary$seat_intervals, s1$summary$seat_intervals)
  expect_equal(s2$summary$seat_crps, s1$summary$seat_crps)
  expect_equal(nrow(s1$seats), nrow(distinct(bind_rows(select(bt$truth$cases, muni_code, party),
                                                     select(filter(bt$truth$seats, seats > 0), muni_code, party)))))
  expect_true(all(c("established") %in% names(bt$truth$cases)))
})
