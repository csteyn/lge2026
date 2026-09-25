# utils.R ---------------------------------------------------------------------
# Shared helpers: configuration, paths, polite downloading, provenance.
#
# Design rule for the whole project: every raw file that enters the pipeline
# gets a row in data/manifest.csv (url, time, bytes, sha256, note). The data
# itself is gitignored; the manifest is committed. A rebuild can then prove it
# is working from the same bytes as a published run.

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)

read_config <- function(path = "config.yml") {
  cfg <- yaml::read_yaml(path)
  stopifnot(cfg$mode %in% c("demo", "live"))
  cfg
}

path_raw <- function(...) file.path("data", "raw", ...)
path_manual <- function(...) file.path("data", "raw", "manual_downloads", ...)
path_public <- function(...) file.path("outputs", "public", ...)

sha256_file <- function(path) digest::digest(file = path, algo = "sha256")

#' Append one provenance row to data/manifest.csv
record_manifest <- function(path, url = NA_character_, note = "") {
  row <- tibble(
    file = path,
    url = url,
    retrieved_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    bytes = file.size(path),
    sha256 = sha256_file(path),
    note = note
  )
  manifest <- file.path("data", "manifest.csv")
  dir.create(dirname(manifest), showWarnings = FALSE, recursive = TRUE)
  write_csv(row, manifest, append = file.exists(manifest))
  invisible(row)
}

#' Does a payload look like an HTML page rather than data?
#' The IEC portal answers a missing report with an HTML error page AND HTTP 200,
#' so status codes alone cannot be trusted (documented by the Johannesburg
#' model, DATA-QUALITY item 12).
looks_like_html <- function(path) {
  first <- readBin(path, "raw", n = 512)
  # Keep printable ASCII only. Binary files (.xls, .pdf, .zip) contain bytes
  # that are not valid text, and any string function on them fails
  # (MODEL-LOG L010). An HTML page starts with ASCII, so nothing is lost.
  ascii <- first[first >= as.raw(0x09) & first <= as.raw(0x7e)]
  txt <- tolower(rawToChar(ascii))
  grepl("^\\s*(<!doctype html|<html)", txt)
}

#' Polite download with provenance and an HTML-as-data guard.
#'
#' Returns a one-row tibble describing the attempt; never throws on HTTP
#' failure, so a sweep over hundreds of files finishes and reports every
#' failure instead of stopping at the first.
download_file <- function(url, dest, pause = 0.5, overwrite = FALSE,
                          user_agent = "lge2026 open election model (research; contact in README)") {
  if (file.exists(dest) && !overwrite) {
    return(tibble(url = url, dest = dest, ok = TRUE, status = NA_integer_,
                  bytes = file.size(dest), note = "cached"))
  }
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(dest)))
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent(user_agent) |>
      httr2::req_timeout(120) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(path = tmp),
    error = function(e) e
  )
  Sys.sleep(pause) # the IEC portal WAFs rapid-fire requests
  if (inherits(resp, "error")) {
    return(tibble(url = url, dest = dest, ok = FALSE, status = NA_integer_,
                  bytes = 0, note = conditionMessage(resp)))
  }
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    return(tibble(url = url, dest = dest, ok = FALSE, status = status,
                  bytes = 0, note = paste("HTTP", status)))
  }
  # A sweep over hundreds of files must never stop on one odd file
  is_html <- tryCatch(looks_like_html(tmp), error = function(e) NA)
  if (is.na(is_html)) {
    return(tibble(url = url, dest = dest, ok = FALSE, status = status,
                  bytes = file.size(tmp), note = "could not inspect payload"))
  }
  if (is_html && !str_detect(dest, "\\.html?$")) {
    return(tibble(url = url, dest = dest, ok = FALSE, status = status,
                  bytes = file.size(tmp), note = "HTML page served as data (HTTP 200)"))
  }
  file.copy(tmp, dest, overwrite = TRUE)
  record_manifest(dest, url)
  tibble(url = url, dest = dest, ok = TRUE, status = status,
         bytes = file.size(dest), note = "")
}

#' Normalise an IEC column header: "Total Valid Votes" -> "TOTALVALIDVOTES"
normalise_header <- function(x) {
  x |>
    str_remove("^\ufeff") |>
    str_to_upper() |>
    str_replace("^GENERATED.*", "GENERATED_DATETIME") |>
    str_remove_all("[^A-Z0-9_]")
}

#' Guess the encoding of an IEC file. Seen in the wild: UTF-8 (with and
#' without BOM), CP850 and UTF-16LE (Johannesburg DATA-QUALITY item 3).
sniff_encoding <- function(path) {
  head_bytes <- readBin(path, "raw", n = 4)
  if (length(head_bytes) >= 2 && identical(head_bytes[1:2], as.raw(c(0xff, 0xfe)))) return("UTF-16LE")
  if (length(head_bytes) >= 2 && identical(head_bytes[1:2], as.raw(c(0xfe, 0xff)))) return("UTF-16BE")
  txt <- readBin(path, "raw", n = file.size(path))
  if (validUTF8(rawToChar(txt[txt != as.raw(0)]))) "UTF-8" else "CP850"
}

#' The IEC municipality field looks like "CPT - City of Cape Town"; keep the code.
muni_code_from_label <- function(x) str_to_upper(str_trim(str_extract(x, "^[^-]+")))

#' Canonical party key: upper case, squashed whitespace, straight quotes.
party_key <- function(x) {
  x |>
    str_to_upper() |>
    str_replace_all("[\u2019`]", "'") |>
    str_squish()
}

#' Short, stable id for a model run: date + git commit when available.
run_id <- function() {
  sha <- tryCatch(system("git rev-parse --short HEAD", intern = TRUE, ignore.stderr = TRUE),
                  error = function(e) character(), warning = function(w) character())
  paste0(format(Sys.time(), "%Y%m%d-%H%M"), if (length(sha)) paste0("-", sha) else "-nogit")
}

git_sha <- function() {
  sha <- tryCatch(system("git rev-parse --short HEAD", intern = TRUE, ignore.stderr = TRUE),
                  error = function(e) character(), warning = function(w) character())
  if (length(sha)) sha else "uncommitted"
}
