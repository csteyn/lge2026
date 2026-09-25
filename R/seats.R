# seats.R ---------------------------------------------------------------------
# Council seat allocation, Local Government: Municipal Structures Act 117 of
# 1998, Schedule 1 (as amended by Act 3 of 2021).
#
# As printed on the IEC's own "Seat Calculation Detail" reports:
#
#   Q = (A / (B - C - D)) + 1, disregarding fractions
#   A  total valid votes for all parties, WARD AND PR BALLOTS ADDED TOGETHER
#      (excluding independents and parties without a PR list)
#   B  total seats in the council
#   C  ward seats won by independent candidates
#   D  ward seats won by parties that have no PR list
#
# Each party's entitlement is floor(votes / Q); unallocated seats go by
# largest remainder. A party's PR seats are its entitlement minus the wards it
# won. Winning a ward does not ADD a seat; it decides which of the party's
# entitled seats is filled by a ward councillor.
#
# Excessive seats (Act 3 of 2021): a party whose ward wins EXCEED its
# entitlement keeps its wards and gets no list seats; the quota is recomputed
# for the remaining parties over the remaining seats, with the council size
# fixed. The IEC prints this as Q = (A - B) / (C - (D + E + F)) + 1.
#
# EVIDENCE, not reading (MODEL-LOG L019): the trigger is STRICTLY GREATER.
# We first followed a documented reading of "equal to or greater". Tested
# against the IEC's published 2021 calculation for all 25 Western Cape
# councils, that reading mislabelled 6 councils and changed the seats in one
# (Beaufort West: ANC won 4 wards with an entitlement of exactly 4; treating
# that as excessive moved a seat from the DA to GOOD). With the strict trigger
# all 25 councils are reproduced exactly, including the 3 genuine cases
# (Mossel Bay, Oudtshoorn, Laingsburg). This follows how the IEC applies the
# Act, which is what decides real councils; we have not ourselves read the
# statute text on this point.

# Known simplifications (logged in MODEL-LOG.md):
#   * exact remainder ties are broken by votes, then by a seeded lot, and the
#     occurrence is flagged; Schedule 1 says ties are decided by lot
#   * a party is assumed to have enough list candidates to fill its PR seats
#   * district councils (40% PR ballot) are not handled here

#' Quota + largest remainder over a fixed number of seats
quota_allocate <- function(votes, seats, rng_seed = NULL) {
  stopifnot(is.numeric(votes), !is.null(names(votes)), length(seats) == 1, is.finite(seats), seats >= 0)
  if (any(!is.finite(votes))) {
    stop("non-finite vote totals for: ", paste(names(votes)[!is.finite(votes)], collapse = ", "), call. = FALSE)
  }
  if (seats == 0 || length(votes) == 0) {
    return(list(seats = setNames(integer(length(votes)), names(votes)),
                quota = NA_integer_, tie = FALSE))
  }
  total <- sum(votes)
  quota <- floor(total / seats) + 1
  exact <- votes / quota
  base <- floor(exact)
  remainder <- exact - base
  shortfall <- seats - sum(base)
  if (shortfall > length(votes)) {
    stop(sprintf("largest-remainder shortfall %d exceeds the number of parties %d; ",
                 shortfall, length(votes)),
         "vote totals are too small for this council size", call. = FALSE)
  }
  # order: remainder, then votes, then a seeded lot
  lot <- if (is.null(rng_seed)) seq_along(votes) else withr::with_seed(rng_seed, sample.int(length(votes)))
  ord <- order(-remainder, -votes, lot)
  winners <- ord[seq_len(shortfall)]
  # flag an exact tie straddling the cut-off
  tie <- shortfall > 0 && shortfall < length(votes) &&
    isTRUE(all.equal(remainder[ord[shortfall]], remainder[ord[shortfall + 1]])) &&
    votes[ord[shortfall]] == votes[ord[shortfall + 1]]
  base[winners] <- base[winners] + 1
  list(seats = setNames(as.integer(base), names(votes)), quota = as.integer(quota), tie = tie)
}

#' Full Schedule 1 allocation for one council
#'
#' @param votes named numeric: combined ward + PR votes per eligible party
#' @param ward_wins named integer: wards won per eligible party (missing = 0)
#' @param total_seats B
#' @param independent_wards C
#' @param no_list_wards D
#' @return tibble, one row per party, with attributes `quota`, `tie`, `rounds`
allocate_seats <- function(votes, ward_wins = integer(), total_seats,
                           independent_wards = 0L, no_list_wards = 0L,
                           rng_seed = NULL) {
  core <- schedule1(votes, ward_wins, total_seats, independent_wards, no_list_wards, rng_seed)
  parties <- names(votes) # captured first: inside tibble(), `votes` becomes the new column
  out <- tibble(
    party = parties,
    votes = as.numeric(votes),
    ward_seats = as.integer(core$wins),
    seats = as.integer(core$seats),
    pr_seats = as.integer(core$seats - core$wins),
    excessive = parties %in% core$excessive
  )
  attr(out, "quota") <- core$quota
  attr(out, "tie") <- core$tie
  attr(out, "rounds") <- core$rounds
  out
}

#' Lean core used inside the simulation loop (no tibble overhead).
#' Returns list(seats, wins, excessive, quota, tie, rounds).
schedule1 <- function(votes, ward_wins = integer(), total_seats,
                      independent_wards = 0L, no_list_wards = 0L, rng_seed = NULL) {
  parties <- names(votes)
  wins <- setNames(integer(length(parties)), parties)
  known <- intersect(names(ward_wins), parties)
  wins[known] <- as.integer(ward_wins[known])
  if (sum(ward_wins) > sum(wins)) {
    stop("ward wins supplied for parties with no vote total: ",
         paste(setdiff(names(ward_wins)[ward_wins > 0], parties), collapse = ", "), call. = FALSE)
  }

  available <- total_seats - independent_wards - no_list_wards
  active <- parties
  fixed <- setNames(integer(0), character(0))
  first_quota <- NA_integer_
  any_tie <- FALSE
  rounds <- 0L

  repeat {
    rounds <- rounds + 1L
    alloc <- quota_allocate(votes[active], available, rng_seed)
    if (rounds == 1L) first_quota <- alloc$quota
    any_tie <- any_tie || alloc$tie
    excessive <- active[wins[active] > alloc$seats[active]] # strictly greater (L019)
    if (length(excessive) == 0) break
    fixed <- c(fixed, wins[excessive])
    available <- available - sum(wins[excessive])
    active <- setdiff(active, excessive)
    if (available < 0) stop("excessive ward wins exceed council size", call. = FALSE)
  }

  entitlement <- setNames(integer(length(parties)), parties)
  entitlement[names(alloc$seats)] <- alloc$seats
  entitlement[names(fixed)] <- fixed

  stopifnot(sum(entitlement) + independent_wards + no_list_wards == total_seats,
            all(entitlement - wins >= 0))
  list(seats = entitlement, wins = wins, excessive = names(fixed),
       quota = first_quota, tie = any_tie, rounds = rounds)
}

#' Compare our allocation with a published IEC seat calculation.
#' `official` is a tibble(party, seats) typed from the Seat Calculation Detail
#' report. Returns the rows that disagree (zero rows = exact reproduction).
seat_discrepancies <- function(ours, official) {
  full_join(select(ours, party, seats_model = seats),
            select(official, party, seats_official = seats), by = "party") |>
    mutate(across(starts_with("seats_"), \(x) coalesce(as.integer(x), 0L))) |>
    filter(seats_model != seats_official)
}
