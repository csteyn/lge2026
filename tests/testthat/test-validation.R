# L017, L019: the allocator must reproduce published councils exactly.
# Fixtures are the IEC's 2021 Seat Calculation Detail figures for all 25
# Western Cape councils (public results), extracted with read_iec_seat_calc().

official <- function(m) {
  list(parties = readr::read_csv(test_path("fixtures", sprintf("seatcalc_2021_%s.csv", m)), show_col_types = FALSE),
       meta = readr::read_csv(test_path("fixtures", sprintf("seatcalc_2021_%s_meta.csv", m)), show_col_types = FALSE))
}
reproduce <- function(o) {
  p <- o$parties
  allocate_seats(setNames(p$votes, p$party), setNames(p$ward_seats, p$party), o$meta$total_seats,
                 o$meta$independent_wards, o$meta$no_list_wards)
}

test_that("Cape Town 2021 is reproduced exactly: 50 parties, quota 7849, 231 seats", {
  o <- official("CPT"); ours <- reproduce(o)
  expect_equal(attr(ours, "quota"), 7849L)
  expect_equal(ours$seats, o$parties$seats[match(ours$party, o$parties$party)])
  expect_equal(ours$seats[ours$party == "DEMOCRATIC ALLIANCE"], 135L)
  expect_equal(sum(ours$seats), 231L)
})

test_that("Laingsburg 2021 is reproduced exactly, including the excessive-seat party", {
  o <- official("WC051"); ours <- reproduce(o)
  expect_equal(ours$seats, o$parties$seats[match(ours$party, o$parties$party)])
  expect_equal(ours$party[ours$excessive], o$parties$party[o$parties$marked_excessive])
  expect_equal(attr(ours, "rounds"), 2L)
})

test_that("every Western Cape council of 2021 is reproduced exactly (25 of 25)", {
  codes <- sub("^seatcalc_2021_(.+)_meta\\.csv$", "\\1",
               list.files(test_path("fixtures"), pattern = "_meta\\.csv$"))
  expect_length(codes, 25)
  for (m in codes) {
    o <- official(m); ours <- reproduce(o)
    expect_equal(ours$seats, o$parties$seats[match(ours$party, o$parties$party)], label = m)
    expect_setequal(ours$party[ours$excessive], o$parties$party[o$parties$marked_excessive])
  }
})

test_that("Beaufort West 2021: equality with entitlement is not excessive (the decisive case)", {
  o <- official("WC053"); ours <- reproduce(o)
  # ANC won 4 wards with an entitlement of exactly 4
  expect_equal(ours$seats[ours$party == "AFRICAN NATIONAL CONGRESS"], 4L)
  expect_equal(ours$seats[ours$party == "DEMOCRATIC ALLIANCE"], 4L)
  expect_equal(ours$seats[ours$party == "GOOD"], 1L)
  expect_false(any(ours$excessive))
})

test_that("the IEC workbook reader works on the raw file when present", {
  f <- file.path("..", "..", "data", "raw", "iec", "lge", "2021", "seat_calculation_detail", "CPT.xls")
  skip_if_not(file.exists(f), "raw IEC workbook not present (live data only)")
  expect_equal(validate_seat_allocator(f)$status, "exact")
})
