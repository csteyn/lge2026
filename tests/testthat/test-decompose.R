# L046: from the 2024 result to the forecast, step by step (a diagnostic).

cfg <- yaml::read_yaml(test_path("..", "..", "config.yml"))
d <- make_demo_data()
bt <- assemble_backtest_inputs(npe_prev = d$npe2019, lge_prev = d$lge2021, npe_latest = d$npe2024,
                               lge_truth = d$lge2021, councils = d$councils, municipalities = d$municipalities)
prep <- prepare_chain(bt$inputs, cfg, "fitted")
no_swing <- tibble(party = character(), scope = character(), mean = numeric())

test_that("every deterministic step is a full distribution, and step 0 is the 2024 count", {
  dc <- decompose_shares(bt$inputs$npe_latest, prep$groups, prep$baseline, prep$premium, no_swing)
  sums <- dc |> filter(step_no <= 5) |> summarise(total = sum(share), .by = c(scope, step_no))
  expect_true(all(abs(sums$total - 1) < 1e-9))
  g <- distinct(prep$groups, muni_code, party, group)
  direct <- bt$inputs$npe_latest |> left_join(g, by = c("muni_code", "party")) |>
    mutate(group = coalesce(group, "OTHER")) |> filter(group != "ABSENT") |>
    summarise(votes = sum(votes), .by = group) |> mutate(share = votes / sum(votes))
  s0 <- dc |> filter(scope == "Western Cape", step_no == 0)
  expect_equal(s0$share[match(direct$group, s0$party)], direct$share, tolerance = 1e-12)
})

test_that("steps with nothing to add change nothing", {
  dc <- decompose_shares(bt$inputs$npe_latest, prep$groups, prep$baseline, premium = NULL, swing_priors = no_swing)
  w <- dc |> filter(step_no %in% 3:5) |> select(scope, party, step_no, share) |>
    tidyr::pivot_wider(names_from = step_no, values_from = share)
  expect_equal(w$`4`, w$`3`, tolerance = 1e-12)   # no premiums, no swing priors
  expect_equal(w$`5`, w$`4`, tolerance = 1e-12)   # no newcomers
})

test_that("newcomers enter at their median share and scale everyone else down", {
  code <- prep$baseline$muni_code[1]
  draws <- setNames(list(matrix(rep(c(0.10, 0.12, 0.08), 10), ncol = 1, dimnames = list(NULL, "NEW ONE"))), code)
  dc <- decompose_shares(bt$inputs$npe_latest, prep$groups, prep$baseline, NULL, no_swing, entrant_draws = draws)
  here <- dc |> filter(scope == code)
  expect_equal(here$share[here$party == "NEW ONE" & here$step_no == 5], 0.10, tolerance = 1e-12)
  est <- here |> filter(party != "NEW ONE", step_no %in% 4:5) |> select(party, step_no, share) |>
    tidyr::pivot_wider(names_from = step_no, values_from = share)
  expect_equal(est$`5`, est$`4` * 0.9, tolerance = 1e-12)
  expect_true(all(here$newcomer[here$party == "NEW ONE"]))
})

test_that("the last step reports the simulation's own medians", {
  ch <- simulate_chain(prep, cfg, 40)
  ps <- province_vote_share(ch$sims, prep$baseline)
  dc <- decompose_shares(bt$inputs$npe_latest, prep$groups, prep$baseline, prep$premium, no_swing,
                         province_share = ps)
  s6 <- dc |> filter(scope == "Western Cape", step_no == 6)
  expect_equal(s6$share[match(ps$party, s6$party)], ps$median)
})
