# L013: an empty or unplaceable candidate parse must never remove parties.

wards <- sf::st_sf(muni_code = "WC011", ward_id = c("10101001", "10101002"),
                   geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1))))

test_that("empty or section-less candidate parses become 'unavailable', not 'nobody stands'", {
  empty <- tibble(muni_code = character(), party = character(), section = character(), ward_id = character())
  expect_null(usable_candidates(empty, wards)$pr_lists)
  nosec <- tibble(muni_code = "WC011", party = "GOOD", section = NA_character_, ward_id = NA_character_)
  r <- usable_candidates(nosec, wards)
  expect_null(r$pr_lists); expect_null(r$contests)
  expect_match(r$status, "could not be told apart")
})

test_that("candidate data filters parties only where it has coverage", {
  cfg <- list(model = list(parties_min_share = 0.01, max_parties_per_muni = 10))
  lge <- tibble(muni_code = c("A", "A", "B", "B"), ballot = "PR", party = c("P1", "P2", "P1", "P2"), votes = c(60, 40, 50, 50))
  npe <- tibble(muni_code = c("A", "A", "B", "B"), party = c("P1", "P2", "P1", "P2"), votes = c(55, 45, 50, 50))
  g <- define_party_groups(lge, npe, cfg, pr_lists = tibble(muni_code = "A", party = "P1"))
  expect_setequal(g$party[g$muni_code == "A" & g$group != "ABSENT"], "P1")
  expect_equal(g$group[g$muni_code == "A" & g$party == "P2"], "ABSENT")  # kept and marked, not dropped (L021)
  expect_setequal(g$party[g$muni_code == "B"], c("P1", "P2"))
  expect_error(define_party_groups(lge[0, ], npe[0, ], cfg), "no parties left")
})

test_that("candidate rows parse in the real IEC layout: list order vs 8-digit ward id (L018)", {
  lines <- c(
    "                     LGE2026 Candidate Lists - 16 Sep 2026 - Western Cape",
    "Municipality              Party                        Ward \\ List Order IDNumber        Fullname          Surname",
    "CPT - City of Cape Town   ABORIGINAL GORILLA FIGHTERS                   1 630923****08*   AUBREY            GOLIATH",
    "CPT - City of Cape Town   ABORIGINAL GORILLA FIGHTERS            19100013 650823****08*   SULAIMAN          SIMONS",
    "WC023 - Drakenstein   PAN AFRICANIST CONGRESS OF AZANIA          10203032 102010****0 *",
    "WC024 - Stellenbosch   SOME PARTY   garbled row without order or id"
  )
  out <- parse_candidate_lines(lines, "test")
  expect_equal(out$section, c("PR", "Ward", "Ward"))
  expect_equal(out$ward_id, c(NA, "19100013", "10203032"))
  expect_equal(out$list_order, c(1L, NA, NA))
  expect_equal(out$birth_year, c(1963L, 1965L, NA))  # "102010" is not a date
  expect_equal(nrow(attr(out, "quarantine")), 1)      # the garbled table row is counted
  expect_false(any(str_detect(names(out), "^id")))
})

test_that("bilingual party names map onto results names", {
  known <- c("VRYHEIDSFRONT PLUS", "CAPE INDEPENDENCE PARTY", "GOOD")
  expect_equal(harmonise_party_names(c("VRYHEIDSFRONT PLUS | FREEDOM FRONT PLUS",
                                       "CAPE INDEPENDENCE PARTY / KAAPSE ONAFHANKLIKHEIDS PARTY",
                                       "GOOD", "BRAND NEW PARTY"), known),
               c("VRYHEIDSFRONT PLUS", "CAPE INDEPENDENCE PARTY", "GOOD", "BRAND NEW PARTY"))
})

test_that("the party-status register validates and classifies C15 absences (L020)", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("muni_code,party,status,name_2026,evidence,source,date",
               "A,GONE PARTY,absent_confirmed,,not on list,x,2026-09-24",
               "A,SPLIT PARTY,disputed,,sources disagree,x,2026-09-24"), f)
  ps <- read_party_status(f)
  notable <- tibble(muni_code = "A", party = c("GONE PARTY", "SPLIT PARTY", "QUIET PARTY"), share = 0.05)
  cl <- classify_absences(notable, ps)
  expect_equal(cl$status, c("absent_confirmed", "disputed", "unconfirmed"))

  bad <- tempfile(fileext = ".csv")
  writeLines(c("muni_code,party,status,name_2026,evidence,source,date",
               "A,X,renamed,,has evidence,x,2026-09-24"), bad)
  expect_error(read_party_status(bad), "name_2026 when renamed")
})

test_that("a confirmed rename maps the 2026 name onto the results name", {
  expect_equal(harmonise_party_names(c("NEW NAME", "GOOD"), known = c("OLD NAME", "GOOD"),
                                     aliases = c(`NEW NAME` = "OLD NAME")),
               c("OLD NAME", "GOOD"))
})
