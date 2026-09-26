# L047: noise centring and the closure test.

cfg <- yaml::read_yaml(test_path("..", "..", "config.yml"))
d <- make_demo_data()

test_that("symmetric log-scale noise lowers a dominant party's expected share; the offsets undo it", {
  E <- log(rbind(c(0.85, 0.10, 0.03, 0.02), c(0.40, 0.35, 0.20, 0.05)))
  off <- mean_preserving_offsets(E, 0.7, K = 300, seed = 3)
  mc <- function(M, seed) { m <- 0; for (k in 1:3000) m <- m + softmax_rows(M + withr::with_seed(seed + k, matrix(rnorm(length(M), 0, 0.7), nrow(M)))); m / 3000 }
  raw <- mc(E, 1e5); fixed <- mc(E + off, 2e5)                 # fresh draws, not the fitting ones
  expect_lt(raw[1, 1], 0.85 - 0.02)                             # the convexity bias exists
  expect_equal(fixed, softmax_rows(E), tolerance = 0.01)        # and is removed in expectation
  expect_equal(unname(rowSums(off)), c(0, 0), tolerance = 1e-10)
})

test_that("mean-preserving noise changes the simulation only when switched on", {
  bt <- assemble_demo_backtest(d)
  prep <- prepare_chain(bt$inputs, cfg, "fitted")
  c0 <- cfg; c0$model$noise_centring <- "none"
  c1 <- cfg; c1$model$noise_centring <- "mean"
  a <- province_vote_share(simulate_chain(prep, c0, 30)$sims, prep$baseline)
  b <- province_vote_share(simulate_chain(prep, c1, 30)$sims, prep$baseline)
  top <- a$party[1]
  expect_gt(b$median[b$party == top], a$median[a$party == top])  # the largest party gains
})

test_that("the closure test reproduces shares and reports both predictions", {
  g <- define_party_groups(d$lge2021, d$npe2024, cfg, d$pr_lists)
  cl <- closure_cycle(d$npe2019, d$lge2021, g, cfg, "demo", K = 50)
  expect_true(all(c("cycle", "scope", "party", "actual", "deterministic", "noisy", "sd_local") %in% names(cl)))
  s <- cl |> summarise(across(c(actual, deterministic, noisy), sum), .by = scope)
  expect_equal(s$actual, rep(1, nrow(s))); expect_equal(s$deterministic, rep(1, nrow(s))); expect_equal(s$noisy, rep(1, nrow(s)))
  lead <- cl$party[which.max(cl$actual)]
  expect_lt(cl$noisy[cl$party == lead], cl$deterministic[cl$party == lead])
})

test_that("the L047 rule is applied as written", {
  grid <- function(diff, lo, hi, cov = 0.9, wb = 0, cb = 0)
    tibble(noise_centring = c("none", "mean"), pit_cov90 = c(0.9, cov), ward_brier = c(0.15, 0.15 + wb),
           control_brier = c(0.3, 0.3 + cb), crps_diff = c(0, diff), crps_diff_lo = c(0, lo), crps_diff_hi = c(0, hi))
  cl <- function(better) tibble(cycle = rep(c("a", "b"), each = 2), scope = "Western Cape", party = rep(c("X", "Y"), 2),
                                actual = c(0.6, 0.2, 0.6, 0.2),
                                deterministic = if (better) c(0.6, 0.2, 0.6, 0.2) else c(0.5, 0.3, 0.5, 0.3),
                                noisy = c(0.55, 0.25, 0.55, 0.25))
  c0 <- cfg; c0$model$noise_centring <- "none"
  expect_true(noise_decision(grid(-0.02, -0.03, -0.01), cl(FALSE), c0)$adopt)         # better on seats
  expect_false(noise_decision(grid(0.02, 0.01, 0.03), cl(TRUE), c0)$adopt)            # worse on seats
  expect_true(noise_decision(grid(0.001, -0.01, 0.01), cl(TRUE), c0)$adopt)           # tie; closure better
  expect_false(noise_decision(grid(-0.001, -0.01, 0.01), cl(FALSE), c0)$adopt)        # tie; closure worse
  expect_false(noise_decision(grid(-0.02, -0.03, -0.01, cb = 0.03), cl(TRUE), c0)$adopt) # control guard
  expect_false(noise_decision(grid(-0.02, -0.03, -0.01, cov = 0.99), cl(TRUE), c0)$adopt) # coverage guard
})

test_that("the backtest grid pairs the two variants", {
  bt <- assemble_demo_backtest(d)
  g <- backtest_noise_grid(bt, cfg, NULL, n_draws = 30)
  expect_equal(nrow(g), 2); expect_equal(sum(g$current), 1)
  expect_equal(g$crps_diff[g$current], 0)
  expect_equal(g$crps_diff, g$seat_crps - g$seat_crps[g$current], tolerance = 1e-8)
})
