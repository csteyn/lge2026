# fetch.R -----------------------------------------------------------------------
# Live data collection. Collection scope is NATIONAL (config: collect$provinces)
# even though the first website publishes only the Western Cape.
#
# Endpoint knowledge comes largely from the Johannesburg model's SOURCES.md
# (https://github.com/Psi-am-i/jhb-election-model, credited in SOURCES.md).
# As of 2026-09-24 none of these URLs has been exercised from THIS codebase:
# the first live run is the verification, and every attempt is logged to
# data/fetch_log.csv so failures are visible rather than silent.

# --- IEC: per-municipality local-election reports (served without a browser) ---

iec_lge_url <- function(election_id, report, province_path, muni, ext) {
  sprintf("https://results.elections.org.za/home/LGEPublicReports/%s/%s/%s/%s.%s",
          election_id, utils::URLencode(report), province_path, muni, ext)
}

fetch_lge_reports <- function(munis, cfg, years = names(cfg$iec$lge_ids),
                              reports = cfg$iec$lge_reports) {
  jobs <- tidyr::expand_grid(munis, year = years, report = names(reports)) |>
    mutate(ext = unlist(reports[report]),
           id = unlist(cfg$iec$lge_ids[year]),
           url = iec_lge_url(id, report, iec_path, muni_code, ext),
           dest = path_raw("iec", "lge", year, str_replace_all(tolower(report), " ", "_"),
                           paste0(muni_code, ".", ext)))
  log <- pmap_dfr(select(jobs, url, dest), download_file)
  write_fetch_log(log, "iec_lge")
  log
}

write_fetch_log <- function(log, source) {
  f <- file.path("data", "fetch_log.csv")
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  log |> mutate(source = source, attempted_utc = format(Sys.time(), tz = "UTC"), .before = 1) |>
    write_csv(f, append = file.exists(f))
}

# --- IEC: national/provincial bulk exports need a real browser session ------------

#' Return the path of a file the user must download by hand, or stop with
#' exact instructions. Nothing is guessed silently.
manual_file <- function(name, url, how) {
  f <- path_manual(name)
  if (!file.exists(f)) {
    stop("Manual download needed: ", name, "\n  from: ", url, "\n  how:  ", how,
         "\n  save as: ", f, call. = FALSE)
  }
  if (!any(read_manifest()$file == f)) record_manifest(f, url, "manual browser download")
  f
}

read_manifest <- function() {
  f <- file.path("data", "manifest.csv")
  if (file.exists(f)) read_csv(f, show_col_types = FALSE) else tibble(file = character())
}

npe_zip <- function(year, cfg, ballot = "provincial") {
  id <- cfg$iec$npe_ids[[as.character(year)]][[ballot]]
  manual_file(
    sprintf("npe%s_%s.zip", year, ballot),
    sprintf("https://results.elections.org.za/home/NPEPublicReports/%s/Downloadable%%20Results/%s.zip",
            id, str_to_title(ballot)),
    "open the URL in a browser (the portal refuses scripted requests), save the zip"
  )
}

# --- Municipal Demarcation Board: ArcGIS FeatureServer layers --------------------

fetch_mdb_layer <- function(service, dest, cfg, where = "1=1", geometry = TRUE,
                            max_offset = NULL, page = 1000, overwrite = FALSE) {
  if (file.exists(dest) && !overwrite) return(dest)
  url <- sprintf("%s/%s/FeatureServer/0/query", cfg$mdb$arcgis_base, service)
  features <- list(); offset <- 0
  repeat {
    q <- list(where = where, outFields = "*", f = "geojson", outSR = "4326",
              returnGeometry = tolower(as.character(geometry)),
              resultOffset = offset, resultRecordCount = page)
    if (!is.null(max_offset)) q$maxAllowableOffset <- max_offset
    resp <- httr2::request(url) |> httr2::req_url_query(!!!q) |>
      httr2::req_retry(max_tries = 3) |> httr2::req_perform()
    batch <- httr2::resp_body_json(resp, simplifyVector = FALSE)$features
    features <- c(features, batch)
    if (length(batch) < page) break
    offset <- offset + page
    Sys.sleep(0.3)
  }
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(list(type = "FeatureCollection", features = features), dest,
                       auto_unbox = TRUE, digits = NA, null = "null")
  record_manifest(dest, url, paste("where:", where))
  dest
}

#' Find a column by pattern, or fail listing what IS there (field names were
#' learned second-hand and must be confirmed on first contact).
pick_col <- function(df, pattern, what) {
  hit <- names(df)[str_detect(names(df), regex(pattern, ignore_case = TRUE))]
  if (!length(hit)) stop("No ", what, " column matching '", pattern, "'. Columns: ",
                         paste(names(df), collapse = ", "), call. = FALSE)
  hit[1]
}

standardise_mdb_wards <- function(path) {
  w <- sf::read_sf(path)
  tibble::as_tibble(w) |>
    transmute(muni_code = str_to_upper(.data[[pick_col(w, "^cat_?b$", "municipality")]]),
              ward_id = as.character(.data[[pick_col(w, "^ward_?id$", "ward id")]]),
              geometry = geometry) |>
    sf::st_as_sf()
}

standardise_mdb_vds <- function(path, wards = NULL) {
  # Field names confirmed from the live VotingDistricts2026_Final layer on
  # 2026-09-24: CAT_B, WardId, VDNumber, WardNo, REGPOP (no Split_VD; L011).
  v <- sf::read_sf(path) |> sf::st_drop_geometry()
  v |>
    transmute(
      muni_code = str_to_upper(.data[[pick_col(v, "^cat_?b$", "municipality")]]),
      ward_id = as.character(.data[[pick_col(v, "^ward_?id$", "ward id")]]),
      vd = as.character(.data[[pick_col(v, "^vd_?num(ber)?$", "VD number")]]),
      # tolerate "1 234" or "1,234" stored as text (a blank or unparseable value is NA)
      registered = suppressWarnings(as.numeric(str_remove_all(
        as.character(.data[[pick_col(v, "^regpop$", "registration")]]), "[^0-9.]")))
    ) |>
    mutate(across(c(ward_id, vd), \(x) str_remove(x, "\\.0+$")))  # guard against numeric ids
}

# --- IEC by-elections: static dashboard JSON ---------------------------------------

fetch_byelections <- function(cfg, year = 2021) {
  # Total function: never throws. Collected for v0.2; a failure here must not
  # stop the forecast (MODEL-LOG L012: an event returned an empty body, and
  # httr2 errors when asked to read one).
  base <- "https://results.elections.org.za/dashboards/byelection/MapsJason"
  failures <- character()
  get <- function(u) {
    out <- tryCatch({
      r <- httr2::request(u) |> httr2::req_error(is_error = \(x) FALSE) |> httr2::req_perform()
      Sys.sleep(0.3)
      if (httr2::resp_status(r) >= 400 || !httr2::resp_has_body(r)) NULL else {
        txt <- httr2::resp_body_string(r)
        if (!nzchar(str_trim(txt))) NULL else jsonlite::fromJSON(txt, simplifyVector = FALSE)
      }
    }, error = function(e) NULL)
    if (is.null(out)) failures <<- c(failures, u)
    out
  }
  result <- tryCatch({
    events <- get(sprintf("%s/%s/ByElections.js", base, year))
    parties <- get(sprintf("%s/%s/PartyList.js", base, year)) |>
      map_dfr(\(p) tibble(party_id = as.integer(p$ID), party = party_key(p$Name %||% "")))
    map_dfr(events %||% list(), \(ev) {
      eeid <- ev$ID %||% ev$EEID %||% ev$Eeid
      nat <- get(sprintf("%s/%s/EEID%sBigMapsNational.js", base, eeid, eeid))
      map_dfr(nat$ProvinceResults %||% list(), \(pr) map_dfr(pr$MunicipalityResults %||% list(), \(m) {
        payload <- get(sprintf("%s/%s/EEID%sMunic%s.js", base, eeid, eeid, m$MunicipalityID))
        tryCatch(flatten_byelection(payload, eeid, ev$dtStartDate, m$MunicipalityID, m$Municipality %||% "",
                                    pr$Province %||% "", parties), error = function(e) tibble())
      }))
    })
  }, error = function(e) { failures <<- c(failures, paste("fatal:", conditionMessage(e))); tibble() })
  if (length(failures)) {
    write_fetch_log(tibble(url = failures, dest = NA_character_, ok = FALSE, status = NA_integer_,
                           bytes = 0, note = "by-election fetch failed or empty"), "iec_byelections")
  }
  attr(result, "failures") <- failures
  result
}

# --- Stats SA ward-level population estimates (2020 wards) -------------------------

fetch_statssa_ward_product <- function() {
  url <- "https://www.statssa.gov.za/wp-content/uploads/2025/11/Ward-Product_Locked-spreadsheets.zip"
  download_file(url, path_raw("statssa", "ward_product_2022.zip"))
}

# --- Candidate lists ----------------------------------------------------------------

candidate_pdf <- function(province_code) {
  url <- sprintf("https://www.elections.org.za/pw/Documents/Candidate%%20Lists/LGE2026/LGE2026%%20Certified%%20Candidate%%20List%%20-%%20%s.pdf",
                 province_code)
  dest <- path_raw("iec", "candidates", paste0("LGE2026_", province_code, ".pdf"))
  res <- download_file(url, dest)
  if (res$ok) return(dest)
  manual_file(basename(dest), url, "the site may refuse scripts; download in a browser")
}

#' Write every municipality code found in the MDB 2026 ward layer to a CSV
#' template, to extend data-raw/manual/municipalities.csv beyond the Western
#' Cape. Names and IEC path codes still need a human to confirm.
seed_municipality_list <- function(wards_file, out = "data-raw/manual/municipalities_national_template.csv") {
  standardise_mdb_wards(wards_file) |> sf::st_drop_geometry() |> distinct(muni_code) |>
    arrange(muni_code) |> write_csv(out)
  out
}

#' The 2014 national results are not on the results portal (the usual URL
#' pattern returns "not found"); the IEC publishes them on its main site.
npe2014_zip <- function() {
  manual_file(
    "npe2014_bulk.zip",
    "https://www.elections.org.za/content/Elections/Downloadable-results/2014-National-and-Provincial-Elections--Complete-voting-district-level-results-data-(zipped-CSV)/",
    "open the page in a browser and download the zipped CSV (about 7 MB); it contains both ballots"
  )
}
