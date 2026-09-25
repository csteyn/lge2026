# L041: polls as a cross-check.

test_that("the provincial vote is the sum of council PR votes, weighted by voters", {
  s1 <- list(muni_code = "A", turnout = c(0.5, 0.5), pr_share = matrix(c(0.6, 0.6, 0.4, 0.4), 2, dimnames = list(NULL, c("X", "Y"))))
  s2 <- list(muni_code = "B", turnout = c(0.5, 0.5), pr_share = matrix(c(0.2, 0.2, 0.8, 0.8), 2, dimnames = list(NULL, c("X", "Y"))))
  base <- tibble(muni_code = c("A", "B"), vd = c("1", "2"), registered = c(3000, 1000))
  ps <- province_vote_share(list(A = s1, B = s2), base)
  expect_equal(ps$median[ps$party == "X"], (0.6 * 1500 + 0.2 * 500) / 2000)
})

test_that("polls are matched by area and party, and gaps beyond the margin are flagged", {
  polls <- tibble(pollster = "P", fieldwork = "x", scope = c("Western Cape", "Cape Town"), party = c("X", "X"),
                  share = c(0.40, 0.50), moe95 = 0.05)
  prov <- tibble(party = "X", median = 0.50, q05 = 0.45, q95 = 0.55)
  vs <- tibble(muni_code = "CPT", party = "X", pr_share_median = 0.52, pr_share_q05 = 0.47, pr_share_q95 = 0.57)
  pc <- compare_polls(polls, prov, vs)
  expect_equal(pc$outside_moe[pc$scope == "Western Cape"], TRUE)   # 10 points > 5
  expect_equal(pc$outside_moe[pc$scope == "Cape Town"], FALSE)     # 2 points
})
