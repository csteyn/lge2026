test_that("LGE reader treats TotalValidVotes as party votes and recomputes VD totals", {
  f <- tempfile(fileext = ".csv")
  writeLines(c(
    "Province,Municipality,Ward,VotingDistrict,VotingStationName,Registered Voters,BallotType,Spoilt Votes,PartyName,TotalValidVotes,Generated Datetime",
    "Western Cape,CPT - City of Cape Town,Ward 19100001,97090001,\"SCHOOL, THE\",1000,PR,5,DEMOCRATIC ALLIANCE,400,2021/11/05",
    "Western Cape,CPT - City of Cape Town,Ward 19100001,97090001,\"SCHOOL, THE\",1000,PR,5,AFRICAN NATIONAL CONGRESS,100,2021/11/05",
    "Western Cape,CPT - City of Cape Town,Ward 19100001,97090001,\"SCHOOL, THE\",1000,Ward,7,DEMOCRATIC ALLIANCE,380,2021/11/05"
  ), f)
  out <- read_iec_lge_csv(f, "LGE2021")
  expect_equal(out$muni_code[1], "CPT")
  expect_equal(out$ward_id[1], "19100001")
  expect_equal(out$vd_valid[out$ballot == "PR"], c(500L, 500L))
  expect_equal(out$vd_valid[out$ballot == "Ward"], 380L)
})

test_that("NPE portal lines survive unquoted commas and trailing columns", {
  lines <- c(
    "Western Cape,CPT - City of Cape Town,97090001,SMITH, JOHN PRIMARY,1000,5,900,DEMOCRATIC ALLIANCE,600,2024-06-01 10:00",
    "Western Cape,CPT - City of Cape Town,97090001,SMITH, JOHN PRIMARY,1000,5,900,AFRICAN NATIONAL CONGRESS,300,",
    "Western Cape,CPT - City of Cape Town,97090002,HALL,800,2,500,GOOD,500",
    "this line is garbage"
  )
  out <- parse_npe_lines(lines, "NPE2024")
  expect_equal(nrow(out), 3)
  expect_equal(out$station[1], "SMITH, JOHN PRIMARY")
  expect_equal(out$votes, c(600L, 300L, 500L))
  expect_equal(out$registered[3], 800L)
  expect_equal(out$vd_valid[1:2], c(900L, 900L))
  expect_length(attr(out, "unparsed"), 1)
})

test_that("by-election JSON flattens to one row per VD x party", {
  payload <- list(WardResults = list(list(
    WardID = 19100077,
    VotingDistrictResults = list(list(
      VotingDistrictID = 97091234,
      PartyBallotResults = list(
        list(ID = 1, CandidateName = "A", TotalValidVotes = 300),
        list(ID = 2, CandidateName = "B", TotalValidVotes = 200)
      )
    ))
  )))
  out <- flatten_byelection(payload, 5000, "2024-03-06T00:00:00", 1, "CPT", "Western Cape",
                            tibble(party_id = 1:2, party = c("DA", "ANC")))
  expect_equal(nrow(out), 2)
  expect_equal(out$party, c("DA", "ANC"))
  expect_equal(out$votes, c(300L, 200L))
})

test_that("the HTML-as-data guard handles binary files and real HTML (L010)", {
  xls <- tempfile(fileext = ".xls")
  writeBin(as.raw(c(0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1, 0xFF, 0xFE, 0x80, 0x41)), xls)
  expect_false(looks_like_html(xls))
  pdf <- tempfile(fileext = ".pdf")
  writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(c(0xE2, 0xE3, 0xCF, 0xD3))), pdf)
  expect_false(looks_like_html(pdf))
  page <- tempfile(fileext = ".csv")
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw("\n  <!DOCTYPE html><html><body>Error</body></html>")), page)
  expect_true(looks_like_html(page))
})

test_that("national lines are pre-filtered by province, falling back loudly (L014)", {
  lines <- c("Western Cape,CPT - x,1,S,10,0,9,DA,5", "Gauteng,JHB - x,2,S,10,0,9,DA,5",
             "WESTERN CAPE,WC011 - y,3,S,10,0,9,GOOD,4")
  expect_length(filter_npe_lines(lines, "Western Cape"), 2)
  expect_warning(out <- filter_npe_lines(lines, "Limpopo"), "No national-election lines matched")
  expect_length(out, 3)
  expect_length(filter_npe_lines(lines, NULL), 3)
})
