# L015: missing values must be repaired upstream or refused with a named cause.

test_that("wmean ignores missing weights as well as missing values", {
  expect_equal(wmean(c(1, 3, NA), c(1, NA, 1)), 1)
  expect_true(is.na(wmean(c(NA, NA), c(1, 1))))
  expect_true(is.na(weighted.mean(c(1, 3), c(1, NA), na.rm = TRUE)))  # the base R trap
})

test_that("registration falls back MDB -> IEC 2024 count -> ward median -> municipality median (L016)", {
  v <- tibble(muni_code = "A", ward_id = c("1", "1", "2", "3"), vd = c("a", "b", "c", "d"),
              registered = c(1000, NA, NA, NA))
  fb <- tibble(muni_code = "A", vd = c("b", "x"), registered = c(700, 900))
  out <- impute_registration(v, fb)
  expect_equal(out$registered, c(1000, 700, 850, 850))
  expect_equal(out$reg_source, c("MDB REGPOP", "IEC 2024 count", "municipality median", "municipality median"))
  expect_equal(out$reg_imputed, c(FALSE, TRUE, TRUE, TRUE))
  # a whole municipality with no MDB counts is rescued by the IEC counts
  all_na <- tibble(muni_code = "C", ward_id = "1", vd = c("p", "q"), registered = NA_real_)
  out2 <- impute_registration(all_na, tibble(muni_code = "C", vd = c("p", "q"), registered = c(1200, 800)))
  expect_equal(out2$registered, c(1200, 800))
})

test_that("REGPOP stored as text with separators is read, not dropped", {
  expect_equal(suppressWarnings(as.numeric(str_remove_all(c("1 234", "1,234", "", NA), "[^0-9.]"))),
               c(1234, 1234, NA, NA))
})

test_that("non-finite votes are refused with party names", {
  expect_error(schedule1(c(A = 100, B = NaN), total_seats = 5), "non-finite vote totals for: B")
})

test_that("a baseline built from inputs with missing registration is complete", {
  cfg <- list(mode = "demo", model = list(parties_min_share = 0.02, max_parties_per_muni = 8, transfer_min_vds = 30))
  d <- make_demo_data()
  d$vd2026$registered[c(1, 5, 9)] <- NA
  g <- define_party_groups(d$lge2021, d$npe2024, cfg, d$pr_lists)
  tr <- fit_transfer(d$npe2019, d$lge2021, g, cfg)
  base <- build_baseline(d$npe2024, d$lge2021, d$vd2026, g, tr, NULL)
  expect_false(anyNA(base$registered)); expect_false(anyNA(base$turnout)); expect_false(anyNA(base$eta_pr))
  expect_equal(sum(distinct(base, vd, reg_imputed)$reg_imputed), 3)
})

test_that("simulation refuses unusable inputs with a readable reason", {
  cfg <- list(mode = "demo", model = list(n_draws = 5, seed = 1, parties_min_share = 0.02, max_parties_per_muni = 8,
    transfer_min_vds = 30, sd = list(province_party = 0.1, muni_party = 0.1, ward_party = NULL, vd_party = NULL,
    turnout_province = 0.1, turnout_municipality = 0.1, turnout_vd = 0.1)))
  d <- make_demo_data()
  g <- define_party_groups(d$lge2021, d$npe2024, cfg, d$pr_lists)
  tr <- fit_transfer(d$npe2019, d$lge2021, g, cfg)
  base <- build_baseline(d$npe2024, d$lge2021, d$vd2026, g, tr, NULL) |> filter(muni_code == "DEM2")
  ow <- other_composition(g, d$lge2021, d$npe2024)
  sw <- tibble(party = character(), scope = character(), mean = numeric(), sd = numeric())
  prov <- draw_province_effects(sort(unique(base$group)), sw, cfg)
  tr_bad <- tr; tr_bad$sd_ward <- NA_real_
  expect_error(simulate_municipality(base, filter(d$councils, muni_code == "DEM2"), prov, ow, sw, tr_bad, cfg),
               "ward spread not estimable")
  expect_error(simulate_municipality(base, filter(d$councils, muni_code == "NOPE"), prov, ow, sw, tr, cfg),
               "council size")
})

test_that("votes of a party not standing in 2026 are renormalised away, not given to OTHER (L021)", {
  cfg <- list(model = list(parties_min_share = 0.06, max_parties_per_muni = 10))
  lge <- tibble(muni_code = "A", vd = "v1", ballot = "PR", party = c("BIG", "MID", "GONE", "TINY"), votes = c(600, 250, 100, 50))
  npe <- select(lge, -ballot)
  g <- define_party_groups(lge, npe, cfg, pr_lists = tibble(muni_code = "A", party = c("BIG", "MID", "TINY")))
  expect_equal(g$group[g$party == "GONE"], "ABSENT")
  expect_equal(g$origin[g$party == "GONE"], "absent_2026")
  s <- group_shares(npe, g, alpha = 0)
  expect_false("ABSENT" %in% s$group)
  # 1 000 votes minus GONE's 100 leaves 900 among standing parties
  expect_equal(s$share[s$group == "OTHER"], 50 / 900)   # not 150 / 1 000
  expect_equal(s$share[s$group == "BIG"], 600 / 900)
  expect_false("GONE" %in% other_composition(g, lge, npe)$party)
})
