# Hand-verifiable cases first; real published councils go in
# test-official-seats.R once the IEC Seat Calculation Detail files are held.

test_that("quota is floor(A / (B - C - D)) + 1", {
  # Johannesburg 2021, as published and reproduced by the Johannesburg model:
  # A = 1,834,260, B = 270, C = D = 0  ->  Q = 6,794.
  v <- c(p1 = 1834260 - 3, p2 = 1, p3 = 1, p4 = 1)
  expect_equal(quota_allocate(v, 270)$quota, 6794L)
})

test_that("largest remainder, worked by hand", {
  # 10 seats, 10,000 votes -> Q = 1,001
  # A 5,500 / 1,001 = 5.494  B 3,100 -> 3.097  C 1,400 -> 1.399
  # floors 5 + 3 + 1 = 9; one seat left; largest remainder is A (.494)
  out <- allocate_seats(c(A = 5500, B = 3100, C = 1400), total_seats = 10)
  expect_equal(out$seats, c(6L, 3L, 1L))
  expect_equal(attr(out, "quota"), 1001L)
})

test_that("ward seats are deducted from entitlement, not added", {
  out <- allocate_seats(c(A = 5500, B = 3100, C = 1400), c(A = 4, B = 1), total_seats = 10)
  expect_equal(out$seats, c(6L, 3L, 1L))
  expect_equal(out$pr_seats, c(2L, 2L, 1L))
})

test_that("excessive ward wins: party keeps wards, others recomputed, council fixed", {
  # Round 1: Q = 1,001 -> A 3 (3.047), B 4 (3.996), C 3 (2.947)
  # A won 4 wards >= entitlement 3 -> A fixed at 4, no list seats.
  # Round 2 over 6 seats for B + C: 6,950 votes, Q = 1,159
  #   B 3.451 -> 3, C 2.545 -> 2, one left by remainder -> C. B 3, C 3.
  out <- allocate_seats(c(A = 3050, B = 4000, C = 2950), c(A = 4, B = 1), total_seats = 10)
  expect_equal(out$seats, c(4L, 3L, 3L))
  expect_equal(out$pr_seats, c(0L, 2L, 3L))
  expect_true(out$excessive[out$party == "A"])
  expect_equal(sum(out$seats), 10L)
  expect_equal(attr(out, "rounds"), 2L)
})

test_that("wards EQUAL to entitlement do not trigger the excessive-seat rule (L019)", {
  # This test previously asserted the opposite, following a documented
  # reading of "equal to or greater". The IEC's 2021 calculations for all 25
  # Western Cape councils show the trigger is strictly greater.
  # A: 3,050 votes -> entitlement 3; wins exactly 3 wards -> normal allocation
  out <- allocate_seats(c(A = 3050, B = 4000, C = 2950), c(A = 3, B = 2), total_seats = 10)
  expect_false(out$excessive[out$party == "A"])
  expect_equal(out$seats, c(3L, 4L, 3L))
  expect_equal(attr(out, "rounds"), 1L)
})

test_that("parties with no wards and no seats are never excessive (A03, closed by L019)", {
  out <- allocate_seats(c(A = 5500, B = 4400, Tiny = 100), c(A = 3, B = 2), total_seats = 10)
  expect_false(out$excessive[out$party == "Tiny"])
})

test_that("independent and no-list ward seats come out of the pool", {
  out <- allocate_seats(c(A = 5500, B = 3100, C = 1400), total_seats = 12,
                        independent_wards = 1, no_list_wards = 1)
  expect_equal(sum(out$seats), 10L)
})

test_that("coalition enumeration finds minimal winning coalitions", {
  # 11 seats, majority 6. Draw 1: A5 B4 C2 -> A+B, A+C, B+C(6) all minimal.
  # Draw 2: A6 B3 C2 -> A alone is winning; A+B is winning but not minimal.
  m <- rbind(c(A = 5, B = 4, C = 2), c(A = 6, B = 3, C = 2))
  cs <- coalition_summary(m, total_seats = 11)
  p <- setNames(cs$p_minimal_winning, cs$coalition)
  expect_equal(unname(p["A"]), 0.5)
  expect_equal(unname(p["A + B"]), 0.5)
  expect_equal(unname(p["B + C"]), 0.5)
  expect_false("A + B + C" %in% cs$coalition)

  ctl <- control_outcomes(m, 11)
  expect_equal(ctl$prob[ctl$outcome == "A majority"], 0.5)
})
