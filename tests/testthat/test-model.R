cfg <- list(mode = "demo", model = list(
  n_draws = 100, seed = 1, parties_min_share = 0.02, max_parties_per_muni = 8,
  transfer_min_vds = 30, coalition_max_parties = 8,
  sd = list(province_party = 0.1, muni_party = 0.12, ward_party = NULL, vd_party = NULL,
            turnout_province = 0.15, turnout_municipality = 0.1, turnout_vd = 0.15)))
d <- make_demo_data()
g <- define_party_groups(d$lge2021, d$npe2024, cfg, d$pr_lists)
tr <- fit_transfer(d$npe2019, d$lge2021, g, cfg)
base <- build_baseline(d$npe2024, d$lge2021, d$vd2026, g, tr, NULL)

test_that("softmax rows sum to one and respect -Inf", {
  e <- rbind(c(1, 2, -Inf), c(0, 0, 0))
  p <- softmax_rows(e)
  expect_equal(rowSums(p), c(1, 1))
  expect_equal(p[1, 3], 0)
})

test_that("baseline shares sum to one in every 2026 VD, imputed or not (L004)", {
  s <- base |> summarise(s = sum(exp(eta_pr)), .by = c(muni_code, vd))
  expect_equal(range(s$s), c(1, 1), tolerance = 1e-9)
  expect_true(all(unique(d$vd2026$vd) %in% base$vd))
})

test_that("transfer slopes are recovered approximately (attenuation is expected, L003)", {
  rec <- inner_join(tr$coefs, DEMO_TRUTH, by = "group", suffix = c("_fit", "_true")) |>
    filter(group != "OTHER")
  expect_lt(max(abs(rec$b_fit - rec$b_true)), 0.3)
  expect_true(all(rec$b_fit <= rec$b_true + 0.1))
})

test_that("a party absent from the earlier NPE gets the default transfer", {
  expect_false("GOLF CONGRESS" %in% tr$coefs$group[!tr$coefs$default_used])
})

test_that("every simulated council is filled exactly, and OTHER never holds seats", {
  ow <- other_composition(g, d$lge2021, d$npe2024)
  swing <- tibble(party = character(), scope = character(), mean = numeric(), sd = numeric())
  prov <- draw_province_effects(sort(unique(base$group)), swing, cfg)
  b <- filter(base, muni_code == "DEM2")
  sim <- simulate_municipality(b, filter(d$councils, muni_code == "DEM2"), prov, ow, swing, tr, cfg)
  expect_true(all(rowSums(sim$seats) == 13))
  expect_false("OTHER" %in% colnames(sim$seats))
  expect_false(any(sim$winners == "OTHER"))
})

test_that("parties without a fitted transfer get an uncertain local premium (L022)", {
  prem <- local_premium(g, tr)
  expect_true("GOLF CONGRESS" %in% prem$group)          # founded after the 2019 NPE
  expect_false("ALPHA PARTY" %in% prem$group)           # fitted
  pr <- transfer_prior(tr)
  fitted_a <- tr$coefs$a[!tr$coefs$default_used & tr$coefs$group != "OTHER"]
  expect_equal(pr$sd, sd(fitted_a))
  sw <- tibble(party = character(), scope = character(), mean = numeric(), sd = numeric())
  cfg2 <- cfg; cfg2$model$n_draws <- 4000
  prov <- draw_province_effects(sort(unique(base$group)), sw, cfg2, prem)
  expected_sd <- sqrt(cfg2$model$sd$province_party^2 + pr$sd^2)
  expect_equal(sd(prov$party[, "GOLF CONGRESS"]), expected_sd, tolerance = 0.05)
  expect_equal(mean(prov$party[, "GOLF CONGRESS"]), pr$mean, tolerance = 0.02)
  expect_equal(sd(prov$party[, "ALPHA PARTY"]), cfg2$model$sd$province_party, tolerance = 0.05)
})
