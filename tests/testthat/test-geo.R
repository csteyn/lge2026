# Fixture uses the field names of the live MDB layers exactly as returned on
# 2026-09-24, so a future schema change fails here rather than mid-pipeline.

test_that("MDB 2026 layers standardise with their real field names (L011)", {
  sq <- function(x) sf::st_polygon(list(matrix(c(x, 0, x + 1, 0, x + 1, 1, x, 1, x, 0), ncol = 2, byrow = TRUE)))
  wards <- sf::st_sf(
    FID = 1:2, Province = "WC", Municipali = "City of Cape Town", CAT_B = "CPT",
    WardID = c("19100001", "19100002"), WardLink = NA, MUNICNAME = "City of Cape Town",
    DISTRICT = NA, DISTRICTCO = NA, DATE = NA, WardNo = 1:2,
    geometry = sf::st_sfc(sq(0), sq(1), crs = 4326))
  vds <- sf::st_sf(
    FID = 1:3, Province = "WC", Municipali = "City of Cape Town", MunicCode = "CPT",
    WardId = c(19100001, 19100001, 19100002), VDNumber = c(97090001, 97090002, 97090003),
    sType = NA, CAT_B = "CPT", WardNo = c(1, 1, 2), WardPop = NA, REGPOP = c(1200, 800, 1500),
    WardLink = NA, MUNICNAME = "City of Cape Town", DISTRICT = NA, VotingStat = "X",
    geometry = sf::st_sfc(sq(0), sq(0.5), sq(1), crs = 4326))
  fw <- tempfile(fileext = ".geojson"); fv <- tempfile(fileext = ".geojson")
  sf::st_write(wards, fw, quiet = TRUE); sf::st_write(vds, fv, quiet = TRUE)

  w <- standardise_mdb_wards(fw)
  expect_equal(w$ward_id, c("19100001", "19100002"))
  v <- standardise_mdb_vds(fv)
  expect_equal(v$vd, c("97090001", "97090002", "97090003"))
  expect_equal(v$ward_id, c("19100001", "19100001", "19100002"))
  expect_equal(v$muni_code, rep("CPT", 3))
  expect_equal(v$registered, c(1200, 800, 1500))
})
