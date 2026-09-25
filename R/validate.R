# validate.R --------------------------------------------------------------------
# Does our Schedule 1 allocator reproduce the IEC's published councils?
#
# The IEC's "Seat Calculation Detail" workbook (one per council) has:
#   Sheet 1: each party's combined ward + PR votes, the quota arithmetic and,
#            where the excessive-seat rule applied, the recalculation
#            (the excessive party is marked with "*")
#   Sheet 2: the official result: total, ward and PR seats per party
# Layout confirmed on WC051 (Laingsburg) and CPT (Cape Town), 2021 (L017).
# Labels differ between the two layouts, so we find cells by text, not position.

read_iec_seat_calc <- function(path) {
  rd <- function(sheet) readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text",
                                           .name_repair = "minimal")
  s1 <- rd(1); s2 <- rd(2)
  row_with <- function(x, pattern) which(apply(x, 1, \(r) any(str_detect(r, regex(pattern, ignore_case = TRUE)), na.rm = TRUE)))
  value_after <- function(pattern) {
    r <- row_with(s1, pattern)[1]
    if (is.na(r)) return(NA_real_)
    v <- suppressWarnings(as.numeric(unlist(s1[r, ])))
    v[!is.na(v)][1]
  }
  # Council code: read from the "Municipality:" row only. Searching the whole
  # sheet picked up a party name in three councils (L019).
  mrow <- unlist(s1[row_with(s1, "^Municipality:?$")[1], ])
  muni_cell <- mrow[str_detect(mrow, "^[A-Z]{2,4}\\d{0,3} - ") %in% TRUE][1]

  # The party-name column moves between layouts (column 1 in WC051, column 2
  # in CPT's Sheet 2), so locate it from the header cell every time.
  table_at <- function(x) {
    h <- row_with(x, "^Party Name$")[1]
    ncol_name <- which(str_detect(unlist(x[h, ]), "^Party Name$"))[1]
    body <- x[(h + 1):nrow(x), ]
    end <- which(is.na(body[[ncol_name]]) | str_detect(body[[ncol_name]], "^Total"))[1] - 1
    list(header = unlist(x[h, ]), body = body[seq_len(end), ], name_col = ncol_name)
  }

  # Sheet 1: votes per party
  t1 <- table_at(s1)
  vcol <- which(str_detect(t1$header, "^Total Valid Votes$"))[1]
  votes <- tibble(name = t1$body[[t1$name_col]], votes = as.numeric(t1$body[[vcol]])) |>
    transmute(party = party_key(str_remove(name, "\\s*\\*\\s*$")),
              marked_excessive = str_detect(name, "\\*\\s*$"), votes)

  # Sheet 2: official seats
  t2 <- table_at(s2)
  col_of <- function(p) which(str_detect(t2$header, regex(p, ignore_case = TRUE)))[1]
  seats <- tibble(name = t2$body[[t2$name_col]],
                  seats = as.integer(t2$body[[col_of("^Total Party Seats")]]),
                  ward_seats = as.integer(t2$body[[col_of("^Ward Seats")]]),
                  pr_seats = as.integer(t2$body[[col_of("^PR List Seats")]])) |>
    transmute(party = party_key(str_remove(name, "\\s*\\*\\s*$")), seats, ward_seats, pr_seats)

  list(
    meta = tibble(muni_code = muni_code_from_label(muni_cell),
                  total_seats = value_after("Total Seats Available"),
                  independent_wards = value_after("Independent Ward Councillors"),
                  no_list_wards = value_after("no PR List"),
                  quota_published = value_after("^Quota")),
    parties = full_join(votes, seats, by = "party")
  )
}

#' Run our allocator on the IEC's own inputs and compare with its result.
#' One row per council; zero mismatched seats is the only acceptable outcome.
validate_seat_allocator <- function(paths) {
  map_dfr(paths, \(p) {
    sc <- tryCatch(read_iec_seat_calc(p), error = function(e) e)
    if (inherits(sc, "error")) {
      return(tibble(file = basename(p), muni_code = NA_character_, parties = NA_integer_,
                    seats_mismatched = NA_integer_, quota_ours = NA_integer_, quota_iec = NA_real_,
                    excessive_ours = NA_character_, excessive_iec = NA_character_,
                    status = "unreadable", detail = conditionMessage(sc)))
    }
    pt <- sc$parties |> filter(!is.na(votes))
    m <- sc$meta
    ours <- allocate_seats(setNames(pt$votes, pt$party), setNames(coalesce(pt$ward_seats, 0L), pt$party),
                           m$total_seats, coalesce(m$independent_wards, 0), coalesce(m$no_list_wards, 0))
    cmp <- ours |> select(party, seats_ours = seats) |>
      left_join(select(pt, party, seats_iec = seats), by = "party")
    bad <- cmp |> filter(seats_ours != coalesce(seats_iec, 0L))
    exc_iec <- paste(pt$party[pt$marked_excessive], collapse = ", ")
    exc_ours <- paste(ours$party[ours$excessive], collapse = ", ")
    tibble(file = basename(p), muni_code = m$muni_code, parties = nrow(pt),
           seats_mismatched = sum(abs(bad$seats_ours - coalesce(bad$seats_iec, 0L))),
           quota_ours = attr(ours, "quota"), quota_iec = m$quota_published,
           excessive_ours = exc_ours, excessive_iec = exc_iec,
           status = if_else(nrow(bad) == 0 && exc_ours == exc_iec, "exact", "MISMATCH"),
           detail = if (nrow(bad)) paste(sprintf("%s ours %d, IEC %d", bad$party, bad$seats_ours,
                                                 coalesce(bad$seats_iec, 0L)), collapse = "; ") else "")
  })
}
