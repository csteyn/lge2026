# L035: newcomer module.

test_that("first-timers are found by conservative name key, with coverage and breadth", {
  hist_lge <- tibble(muni_code = "A", party = c("OLD PARTY", "THE TIDY MOVEMENT"), votes = 10L)
  hist_npe <- tibble(muni_code = "B", party = "NATIONAL ONE", votes = 5L)
  res <- tibble(muni_code = c("A", "A", "A", "A", "B", "B"), ward_id = c("A1", "A1", "A2", NA, "B1", "B1"),
                ballot = c("Ward", "Ward", "Ward", "PR", "Ward", "Ward"),
                party = c("OLD PARTY", "TIDY MOVEMENT", "NEW LOCALS", "NEW LOCALS", "NATIONAL ONE", "NEW LOCALS"),
                votes = c(50L, 20L, 30L, 40L, 60L, 10L))
  ft <- first_timers(res, hist_lge, hist_npe, 2021)
  expect_setequal(paste(ft$muni_code, ft$party), c("A NEW LOCALS", "B NEW LOCALS"))  # TIDY = THE TIDY MOVEMENT
  a <- filter(ft, muni_code == "A")
  expect_equal(a$coverage, 0.5); expect_equal(a$councils, 2L); expect_false(a$history_in_province)
  expect_equal(a$share, 70 / 140)
})

test_that("the prior estimates cross-council correlation and falls back for thin classes", {
  set.seed(3)
  base <- tibble(year = 2016, party = rep(sprintf("P%02d", 1:15), each = 3), muni_code = rep(c("A", "B", "C"), 15),
                 coverage = 1, history_in_province = FALSE) |>
    mutate(councils = 3L, share = exp(rep(rnorm(15, -4, 1), each = 3) + rnorm(45, 0, 0.1)))
  pr <- estimate_entrant_prior(base, min_cell = 8)
  expect_gt(pr$rho, 0.8)                                    # shares move together within a party
  expect_equal(pr$rho_source, "estimated")
  expect_match(entrant_pool(pr, councils = 1, coverage = 1)$basis, "all newcomers")  # empty class -> fallback
})

test_that("draws are correlated across a party's councils, capped, and overrides apply", {
  ex <- tibble(share = exp(seq(-7, -1, length.out = 60)), cls = entrant_class(3, 1), breadth = "2-10 councils")
  pr <- list(examples = ex, rho = 0.9, min_cell = 8)
  ent <- tibble(muni_code = c("A", "B", "A"), party = c("WIDE", "WIDE", "BIG"), coverage = 1, councils = c(3L, 3L, 3L))
  ov <- tibble(party = "BIG", median = 0.5, q90 = 0.7, source = "test", date_added = "x")
  d <- draw_entrant_shares(ent, pr, 2000, 1, overrides = ov, max_total = 0.8)
  expect_gt(cor(qnorm(rank(d$A[, "WIDE"]) / 2001), qnorm(rank(d$B[, "WIDE"]) / 2001)), 0.8)
  expect_true(all(rowSums(d$A) <= 0.8 + 1e-9))
  expect_equal(median(d$B[, "WIDE"]), median(ex$share), tolerance = 0.3)
})

test_that("the simulation reproduces each drawn newcomer share exactly and respects ward contests", {
  cfg <- yaml::read_yaml(test_path("..", "..", "config.yml")); cfg$model$n_draws <- 150
  d <- make_demo_data()
  g <- define_party_groups(d$lge2021, d$npe2024, cfg, d$pr_lists); ow <- other_composition(g, d$lge2021, d$npe2024)
  tr <- fit_transfer(d$npe2019, d$lge2021, g, cfg); base <- build_baseline(d$npe2024, d$lge2021, d$vd2026, g, tr, NULL)
  sw <- tibble(party = character(), scope = character(), mean = numeric(), sd = numeric())
  prov <- draw_province_effects(sort(unique(base$group)), sw, cfg, NULL)
  b <- filter(base, muni_code == "DEM3"); council <- filter(d$councils, muni_code == "DEM3")
  ent <- find_entrants(d$pr_lists, d$contests, g)
  expect_equal(ent$party, "KILO MOVEMENT")
  es <- cbind(`KILO MOVEMENT` = seq(0.02, 0.2, length.out = 150))
  w <- unique(b$ward_id); ec <- tibble(muni_code = "DEM3", party = "KILO MOVEMENT", ward_id = w[1:4])
  sim <- simulate_municipality(b, council, prov, ow, sw, tr, cfg, entrant_shares = es, entrant_contests = ec)
  expect_equal(unname(sim$pr_share[, "KILO MOVEMENT"]), unname(es[, 1]), tolerance = 1e-10)
  expect_equal(sum(sim$winners[, setdiff(w, ec$ward_id)] == "KILO MOVEMENT"), 0)
  expect_true(all(rowSums(sim$seats) == council$total_seats))
})

test_that("newcomers are paired: established parties see the same random numbers (L036)", {
  cfg <- yaml::read_yaml(test_path("..", "..", "config.yml")); cfg$model$n_draws <- 80
  d <- make_demo_data()
  g <- define_party_groups(d$lge2021, d$npe2024, cfg, d$pr_lists); ow <- other_composition(g, d$lge2021, d$npe2024)
  tr <- fit_transfer(d$npe2019, d$lge2021, g, cfg); base <- build_baseline(d$npe2024, d$lge2021, d$vd2026, g, tr, NULL)
  sw <- tibble(party = character(), scope = character(), mean = numeric(), sd = numeric())
  prov <- draw_province_effects(sort(unique(base$group)), sw, cfg, NULL)
  b <- filter(base, muni_code == "DEM3"); council <- filter(d$councils, muni_code == "DEM3")
  tiny <- cbind(`KILO MOVEMENT` = rep(1e-9, 80))                 # a newcomer with no votes
  with <- simulate_municipality(b, council, prov, ow, sw, tr, cfg, entrant_shares = tiny)
  without <- simulate_municipality(b, council, prov, ow, sw, tr, cfg)
  expect_equal(with$seats[, colnames(without$seats)], without$seats)
  expect_equal(with$winners, without$winners)
})

test_that("newcomers' combined share is compared with history (L043)", {
  ft <- tibble(year = 2021, muni_code = rep(c("A", "B", "C"), each = 2), share = c(0.01, 0.02, 0.03, 0.01, 0.02, 0.02))
  draws <- list(A = cbind(P = rep(0.30, 50), Q = rep(0.05, 50)), B = cbind(P = rep(0.01, 50)))
  et <- entrant_total_check(draws, ft)
  expect_equal(et$hist_max, 0.04)
  expect_equal(et$simulated$median_total[et$simulated$muni_code == "A"], 0.35)
  expect_true(et$simulated$median_total[et$simulated$muni_code == "A"] > et$hist_p95)
  expect_false(et$simulated$median_total[et$simulated$muni_code == "B"] > et$hist_p95)
})
