# assemble.R ------------------------------------------------------------------
# Build the model's input contract from raw files. Demo mode returns the same
# list from make_demo_data(), so everything downstream is shared:
#
#   npe2019, npe2024 : election, province, muni_code, vd, ballot, registered,
#                      spoilt, party, votes, vd_valid
#   lge2021          : as above plus ward_id and ballot in {PR, Ward, DC40}
#   vd2026           : muni_code, ward_id, vd, registered
#   councils         : muni_code, total_seats, n_wards, seats_source
#   contests         : muni_code, ward_id, party   (NULL until candidates parsed)
#   pr_lists         : muni_code, party            (NULL until candidates parsed)
#   municipalities   : province, muni_code, muni_name
#   wards_geo        : sf, muni_code, ward_id

assemble_live_inputs <- function(cfg, lge_files, npe_files, wards_file, vds_file, candidate_files = NULL,
                                 party_status = NULL) {
  munis <- read_csv("data-raw/manual/municipalities.csv", show_col_types = FALSE, comment = "#")
  scope <- munis |> filter(province_code %in% cfg$collect$provinces)
  lge2021 <- map_dfr(lge_files, read_iec_lge_csv, election = "LGE2021")
  wards <- standardise_mdb_wards(wards_file)
  vd2026 <- standardise_mdb_vds(vds_file) |> select(muni_code, ward_id, vd, registered)

  # National files label municipalities in their own way. VD numbers are
  # nationally unique, so assign each national-election VD to a municipality
  # through the VD itself where possible, and only fall back to the label.
  vd_muni <- bind_rows(distinct(vd2026, vd, muni_code), distinct(lge2021, vd, muni_code)) |>
    distinct(vd, .keep_all = TRUE)
  by_vd <- function(npe) {
    npe |> left_join(rename(vd_muni, muni_by_vd = muni_code), by = "vd") |>
      mutate(muni_code = coalesce(muni_by_vd, muni_code)) |> select(-muni_by_vd)
  }
  # Raw national files stay on disk (collection is national); only the
  # modelled provinces are parsed into memory (L014).
  prov_names <- unique(munis$province[munis$province_code %in% cfg$model$provinces])
  npe2019 <- read_iec_npe_portal(npe_files[str_detect(npe_files, "npe2019")], "NPE2019", prov_names) |> by_vd()
  npe2024 <- read_iec_npe_portal(npe_files[str_detect(npe_files, "npe2024")], "NPE2024", prov_names) |> by_vd()

  n_wards <- sf::st_drop_geometry(wards) |> count(muni_code, name = "n_wards")
  councils <- read_csv("data-raw/manual/council_seats_2026.csv", show_col_types = FALSE, comment = "#") |>
    select(-any_of("n_wards")) |>
    right_join(n_wards, by = "muni_code") |>
    mutate(seats_source = if_else(is.na(total_seats), "DERIVED 2 x wards (unverified)", seats_source),
           total_seats = coalesce(as.integer(total_seats), 2L * n_wards))

  cands <- if (length(candidate_files)) map_dfr(candidate_files, read_candidate_pdf) else NULL
  known <- unique(c(lge2021$party, npe2024$party))
  renamed <- filter(party_status %||% tibble(status = character()), status == "renamed")
  aliases <- setNames(renamed$party, renamed$name_2026)
  cand <- usable_candidates(cands, wards, known, aliases)
  contests <- cand$contests
  pr_lists <- cand$pr_lists

  list(npe2019 = npe2019, lge2021 = lge2021, npe2024 = npe2024, vd2026 = vd2026,
       councils = councils, contests = contests, pr_lists = pr_lists,
       municipalities = munis, wards_geo = wards,
       candidate_quarantine = if (!is.null(cands)) attr(cands, "quarantine"),
       candidate_status = cand$status, candidate_counts = cand$counts)
}

#' Restrict every table to the municipalities being modelled
scope_inputs <- function(inputs, munis) {
  keep <- munis$muni_code
  map(inputs, \(x) if (is.data.frame(x) && "muni_code" %in% names(x)) filter(x, muni_code %in% keep) else x)
}

#' Map candidate-list party names onto the names used in the results files.
#' The candidate PDF prints some names bilingually ("VRYHEIDSFRONT PLUS |
#' FREEDOM FRONT PLUS", "X / Y") where results use one form (L018). Exact match
#' first, then each part of a split name; anything else is kept as printed
#' (a party new since 2024 legitimately has no match).
#' Conservative name key: drop a leading "THE", a trailing "PARTY", and
#' punctuation. Two names with the same key are treated as one party
#' ("UMKHONTO WESIZWE PARTY" = "UMKHONTO WESIZWE"; L024).
name_key <- function(x) {
  x |> str_to_upper() |> str_remove("^THE\\s+") |> str_remove("\\s+PARTY$") |>
    str_replace_all("[^A-Z0-9 ]", " ") |> str_squish()
}

#' Parties marked not standing whose name is CLOSE to a name on the same
#' council's 2026 list, for a human to rule on (check C17). Pairs already
#' ruled on in the register (renamed / distinct) are excluded.
near_miss_names <- function(groups, pr_lists, party_status = NULL) {
  if (is.null(pr_lists)) return(tibble())
  broad <- function(x) str_squish(str_remove_all(name_key(x), "\\b(OF|SOUTH AFRICA|SA|WITH)\\b"))
  ruled <- (party_status %||% tibble(status = character(), party = character(), name_2026 = character())) |>
    filter(status %in% c("renamed", "distinct")) |> distinct(party, name_2026)
  groups |> filter(origin == "absent_2026") |> distinct(muni_code, party) |>
    inner_join(rename(distinct(pr_lists, muni_code, party), name_2026 = party), by = "muni_code",
               relationship = "many-to-many") |>
    mutate(a = broad(party), b = broad(name_2026),
           dist = mapply(\(x, y) adist(x, y)[1, 1], a, b) / pmax(nchar(a), nchar(b), 1)) |>
    filter(nzchar(a), nzchar(b), str_detect(b, fixed(a)) | str_detect(a, fixed(b)) | dist <= 0.2) |>
    filter(party != name_2026) |>
    anti_join(ruled, by = c("party", "name_2026")) |>
    distinct(party, name_2026) |> arrange(party)
}

harmonise_party_names <- function(names, known, aliases = character()) {
  # aliases: named character vector, 2026 name -> results name, from the
  # "renamed" rows of party_status_2026.csv (human-confirmed only)
  map_chr(names, \(n) {
    if (n %in% names(aliases)) return(aliases[[n]])
    if (n %in% known) return(n)
    same_key <- known[name_key(known) == name_key(n)]
    if (length(same_key) == 1) return(same_key)
    parts <- str_squish(str_split(n, "\\s+[|/]\\s+")[[1]])
    hit <- parts[parts %in% known]
    if (length(hit)) hit[1] else n
  })
}

#' Turn parsed candidate rows into contest and PR-list tables, or into NULL
#' with a reason. An empty or unplaceable parse means "candidate data
#' unavailable", never "no party is standing" (L013).
usable_candidates <- function(cands, wards, known_parties = character(), aliases = character()) {
  none <- function(why) list(contests = NULL, pr_lists = NULL, status = why)
  if (is.null(cands)) return(none("candidate lists not available"))
  if (nrow(cands) == 0) return(none("candidate PDF parsed to 0 rows; layout not yet understood"))
  if (!"ward_id" %in% names(cands) || all(is.na(cands$section))) {
    return(none(sprintf("%d candidate rows parsed, but ward and PR rows could not be told apart", nrow(cands))))
  }
  cands <- cands |> mutate(party = harmonise_party_names(party, known_parties, aliases))
  pr <- cands |> filter(section == "PR") |> distinct(muni_code, party)
  wd <- cands |> filter(section == "Ward", !is.na(ward_id)) |>
    semi_join(sf::st_drop_geometry(wards), by = c("muni_code", "ward_id")) |>
    distinct(muni_code, ward_id, party)
  n_ward_rows <- sum(cands$section == "Ward")
  list(contests = if (nrow(wd)) wd, pr_lists = if (nrow(pr)) pr,
       counts = count(cands, section),
       status = sprintf("parsed %d PR and %d ward candidate rows; %d PR-list and %d ward-contest pairs used",
                        sum(cands$section == "PR"), n_ward_rows, nrow(pr), nrow(wd)))
}
