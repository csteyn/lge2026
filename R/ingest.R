# ingest.R ----------------------------------------------------------------------
# Turn raw IEC files into tidy tables with one consistent schema:
#
#   election, province, muni_code, ward_id, vd, ballot, registered, spoilt,
#   party, votes, vd_valid
#
# `votes` is ALWAYS the party's votes and `vd_valid` is ALWAYS recomputed by
# summing parties within vd x ballot. Never trust a file's "TotalValidVotes"
# to mean what it says (Johannesburg DATA-QUALITY item 1: in the LGE files it
# is the party's votes, in the NPE portal files it is the VD total).

# --- Local government elections: per-municipality "Downloadable Party Results"

read_iec_lge_csv <- function(path, election = NA_character_) {
  enc <- sniff_encoding(path)
  raw <- read_csv(path, col_types = cols(.default = col_character()),
                  locale = locale(encoding = enc), name_repair = "minimal",
                  show_col_types = FALSE, progress = FALSE)
  names(raw) <- normalise_header(names(raw))
  need <- c("PROVINCE", "MUNICIPALITY", "WARD", "VOTINGDISTRICT", "BALLOTTYPE",
            "REGISTEREDVOTERS", "SPOILTVOTES", "TOTALVALIDVOTES", "PARTYNAME")
  missing <- setdiff(need, names(raw))
  if (length(missing)) {
    stop(basename(path), ": missing columns ", paste(missing, collapse = ", "),
         ". Header drift between years is a known IEC issue; extend normalise_header().",
         call. = FALSE)
  }
  raw |>
    transmute(
      election = election,
      province = str_trim(PROVINCE),
      muni_code = muni_code_from_label(MUNICIPALITY),
      ward_id = str_extract(WARD, "\\d{8}"),
      vd = str_trim(VOTINGDISTRICT),
      ballot = normalise_ballot(BALLOTTYPE),
      registered = as.integer(REGISTEREDVOTERS),
      spoilt = as.integer(SPOILTVOTES),
      party = party_key(PARTYNAME),
      votes = as.integer(TOTALVALIDVOTES) # the PARTY's votes, despite the name
    ) |>
    mutate(vd_valid = sum(votes), .by = c(vd, ballot))
}

normalise_ballot <- function(x) {
  x <- str_to_upper(str_squish(x))
  case_when(
    str_detect(x, "^DC") ~ "DC40",
    str_detect(x, "^PR") ~ "PR",
    str_detect(x, "^WARD") ~ "Ward",
    TRUE ~ paste0("UNKNOWN:", x) # surfaced by check_unknown_ballots()
  )
}

# --- National & provincial elections: results-portal bulk export (2019, 2024)
#
# One row per VD x party. Voting-station names are NOT quoted, so a comma in a
# name shifts every later column (Johannesburg DATA-QUALITY item 2). We anchor
# the parse at both ends with a regular expression instead of splitting on
# commas: the first three fields are fixed, the numeric tail is fixed, and the
# greedy middle is the station name, commas and all.

npe_line_pattern <- paste0(
  "^([^,]*),([^,]*),([^,]*),(.*),\\s*(\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(\\d+)\\s*,",
  "([^,]*[^,\\d\\s][^,]*),\\s*(\\d+)\\s*(?:,.*)?$"
)

parse_npe_lines <- function(lines, election = NA_character_) {
  m <- str_match(lines, npe_line_pattern)
  ok <- !is.na(m[, 1])
  parsed <- tibble(
    election = election,
    province = str_trim(m[ok, 2]),
    muni_code = muni_code_from_label(m[ok, 3]),
    ward_id = NA_character_, # the portal layout does not carry wards
    vd = str_trim(m[ok, 4]),
    station = str_trim(m[ok, 5]),
    ballot = "PR",
    registered = as.integer(m[ok, 6]),
    spoilt = as.integer(m[ok, 7]),
    vd_valid_file = as.integer(m[ok, 8]),
    party = party_key(m[ok, 9]),
    votes = as.integer(m[ok, 10])
  ) |>
    mutate(vd_valid = sum(votes), .by = vd)
  attr(parsed, "unparsed") <- lines[!ok]
  parsed
}

#' Keep only lines whose first field is one of the given province names
#' (case-insensitive). If none match, the labels differ from what we expect:
#' return everything and warn, rather than silently return nothing.
filter_npe_lines <- function(lines, provinces) {
  if (is.null(provinces) || !length(provinces)) return(lines)
  first <- str_to_lower(str_trim(str_extract(lines, "^[^,]*")))
  keep <- first %in% str_to_lower(provinces)
  if (!any(keep)) {
    warning("No national-election lines matched provinces ", paste(provinces, collapse = ", "),
            "; parsing all lines. First field looks like: ", first[2], call. = FALSE)
    return(lines)
  }
  lines[keep]
}

# --- 2014 national results: "bulk" layout on www.elections.org.za
#
# Properly quoted CSV, both ballots in one file (ELECTORAL EVENT column), no
# station names, and VALID VOTES is the PARTY's votes (Johannesburg SOURCES.md
# and DATA-QUALITY.md). Header spelling is matched with underscores and
# spaces removed.

read_iec_npe_bulk <- function(path, election, provinces = NULL, event = "PROVINCIAL") {
  if (str_detect(path, "\\.zip$")) {
    inner <- utils::unzip(path, list = TRUE)$Name
    inner <- inner[str_detect(inner, "(?i)\\.csv$")][1]
    dir <- tempfile(); utils::unzip(path, files = inner, exdir = dir)
    path <- file.path(dir, inner)
  }
  raw <- read_csv(path, col_types = cols(.default = col_character()), name_repair = "minimal",
                  locale = locale(encoding = sniff_encoding(path)), show_col_types = FALSE, progress = FALSE)
  names(raw) <- str_remove_all(normalise_header(names(raw)), "_")
  need <- c("ELECTORALEVENT", "PROVINCE", "MUNICIPALITY", "VOTINGDISTRICT", "REGISTEREDVOTERS",
            "SPOILTVOTES", "PARTYNAME", "VALIDVOTES")
  missing <- setdiff(need, names(raw))
  if (length(missing)) stop(basename(path), ": not the 2014 bulk layout; missing ", paste(missing, collapse = ", "),
                            call. = FALSE)
  out <- raw |>
    filter(str_detect(str_to_upper(ELECTORALEVENT), event)) |>
    filter(is.null(provinces) | str_to_lower(str_trim(PROVINCE)) %in% str_to_lower(provinces)) |>
    transmute(election = election, province = str_trim(PROVINCE),
              muni_code = muni_code_from_label(MUNICIPALITY), ward_id = NA_character_,
              vd = str_trim(VOTINGDISTRICT), ballot = "PR",
              registered = as.integer(REGISTEREDVOTERS), spoilt = as.integer(SPOILTVOTES),
              party = party_key(PARTYNAME), votes = as.integer(VALIDVOTES)) |>
    mutate(vd_valid = sum(votes), .by = vd)
  if (!nrow(out)) stop(basename(path), ": no ", event, " rows for ", paste(provinces, collapse = ", "), call. = FALSE)
  out
}

read_iec_npe_portal <- function(path, election, provinces = NULL) {
  if (str_detect(path, "\\.zip$")) {
    inner <- utils::unzip(path, list = TRUE)$Name
    inner <- inner[str_detect(inner, "(?i)\\.csv$")][1]
    dir <- tempfile(); utils::unzip(path, files = inner, exdir = dir)
    path <- file.path(dir, inner)
  }
  lines <- read_lines(path, locale = locale(encoding = sniff_encoding(path)), progress = FALSE)
  lines <- filter_npe_lines(lines[-1], provinces) # drop header, keep modelled provinces
  out <- parse_npe_lines(lines, election)
  rm(lines); gc(verbose = FALSE)
  unparsed <- attr(out, "unparsed")
  unparsed <- unparsed[str_trim(unparsed) != ""]
  if (length(unparsed)) {
    warning(basename(path), ": ", length(unparsed), " line(s) did not parse; ",
            "first: ", unparsed[1], call. = FALSE)
  }
  # The file states the VD total; our recomputed total must agree with it.
  disagree <- out |> distinct(vd, vd_valid, vd_valid_file) |> filter(vd_valid != vd_valid_file)
  attr(out, "vd_total_disagreements") <- nrow(disagree)
  out |> select(-vd_valid_file)
}

# --- By-elections: the IEC results dashboard's static JSON
#
# Structure (as documented by the Johannesburg model's fetch_byelections.py):
# WardResults[] -> VotingDistrictResults[] -> PartyBallotResults[]

flatten_byelection <- function(payload, eeid, date, muni_id, muni_name, province,
                               parties = tibble(party_id = integer(), party = character())) {
  if (is.null(payload) || !length(payload$WardResults)) return(tibble())
  map_dfr(payload$WardResults, \(w) {
    map_dfr(w$VotingDistrictResults, \(d) {
      map_dfr(d$PartyBallotResults, \(r) tibble(
        eeid = eeid, date = as.Date(substr(date, 1, 10)), province = province,
        muni_id = muni_id, muni_name = muni_name,
        ward_id = as.character(w$WardID), vd = as.character(d$VotingDistrictID),
        party_id = as.integer(r$ID), candidate = r$CandidateName %||% NA_character_,
        votes = as.integer(r$TotalValidVotes)
      ))
    })
  }) |>
    left_join(parties, by = "party_id")
}

# --- Candidate lists: IEC certified candidate list PDFs (one per province)
#
# Layout confirmed on LGE2026_WC.pdf (16 Sep 2026, 271 pages; MODEL-LOG L018):
# one table, no section headings. Columns: Municipality | Party |
# Ward \ List Order | IDNumber (masked) | Fullname | Surname. The third column
# holds EITHER a PR list position (1, 2, 3, ...) OR an 8-digit ward ID
# (19100013), and that is what separates ward candidates from PR lists.
#
# The masked ID keeps the date of birth (YYMMDD). We derive a birth year and
# drop the ID string: the model needs nothing more (POPIA minimality). Names
# are kept as printed in one field; splitting given names from surname is not
# needed by the model and fails when the PDF squeezes the columns together.

candidate_line_pattern <- paste0(
  "^\\s*(?<muni>[A-Z]{2,4}\\d{0,3})\\s*-\\s*(?<muniname>.+?)\\s{2,}(?<party>.+?)\\s{2,}",
  "(?<order>\\d{1,8})\\s+(?<id>\\d{6}\\*{4}[\\d ]{1,3}\\*)(?:\\s+(?<names>\\S.*?))?\\s*$"
)

parse_candidate_lines <- function(lines, source = NA_character_, election_year = 2026) {
  m <- str_match(lines, candidate_line_pattern)
  # Any table row counts, not only rows with a well-formed ID: a malformed
  # row must be quarantined and counted, never silently skipped (L018).
  looks_like_candidate <- str_detect(lines, "^\\s*[A-Z]{2,4}\\d{0,3}\\s*-\\s")
  ok <- !is.na(m[, 1])
  order <- m[ok, "order"]
  yy <- as.integer(substr(m[ok, "id"], 1, 2))
  mm <- as.integer(substr(m[ok, "id"], 3, 4)); dd <- as.integer(substr(m[ok, "id"], 5, 6))
  real_date <- mm %in% 1:12 & dd %in% 1:31 # "102010" is not a date: no age rather than a wrong one
  parsed <- tibble(
    source = source,
    muni_code = m[ok, "muni"],
    party = party_key(m[ok, "party"]),
    section = if_else(nchar(order) == 8, "Ward", "PR"),
    ward_id = if_else(nchar(order) == 8, order, NA_character_),
    list_order = if_else(nchar(order) == 8, NA_integer_, as.integer(order)),
    full_name = str_squish(coalesce(m[ok, "names"], "")),
    birth_year = if_else(!real_date, NA_integer_,
                         if_else(2000L + yy > election_year - 18L, 1900L + yy, 2000L + yy))
  )
  attr(parsed, "quarantine") <- tibble(source = source, line = lines[looks_like_candidate & !ok])
  parsed
}

read_candidate_pdf <- function(path) {
  pages <- pdftools::pdf_text(path)
  lines <- unlist(str_split(pages, "\n"))
  parse_candidate_lines(lines, source = basename(path))
}
