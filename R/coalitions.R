# coalitions.R ------------------------------------------------------------------
# Coalition arithmetic over simulated councils.
#
# For each draw we know every party's seats. A coalition is WINNING if its
# seats reach the majority threshold floor(B/2) + 1. It is MINIMAL WINNING if
# dropping any member makes it lose, which is equivalent to: dropping its
# smallest member makes it lose (the smallest member is the one whose removal
# leaves the most seats). Parties with zero seats in a draw can never be part
# of a minimal winning coalition in that draw, which falls out of the same
# test automatically.
#
# Arithmetic only: whether parties WOULD govern together is politics, not
# counting, and the site says so wherever these numbers appear.

#' @param seat_mat integer matrix, draws x parties, column names = parties
#' @param total_seats council size B
#' @param max_parties enumerate subsets of at most this many of the largest
#'   parties (2^k subsets). Parties outside the set are reported as excluded.
coalition_summary <- function(seat_mat, total_seats, max_parties = 10) {
  majority <- floor(total_seats / 2) + 1
  mean_seats <- colMeans(seat_mat)
  keep <- names(sort(mean_seats[mean_seats > 0], decreasing = TRUE))
  keep <- head(keep, max_parties)
  excluded <- setdiff(names(mean_seats)[mean_seats > 0], keep)
  m <- seat_mat[, keep, drop = FALSE]
  k <- length(keep)
  if (k == 0) return(tibble())

  # every non-empty subset as a logical membership matrix (subsets x parties)
  ids <- seq_len(2^k - 1)
  member <- t(vapply(ids, \(i) bitwAnd(i, 2^(seq_len(k) - 1)) > 0, logical(k)))
  if (k == 1) member <- matrix(member, ncol = 1)

  seats_by_coalition <- m %*% t(member * 1L)            # draws x subsets
  smallest <- vapply(ids, \(i) {
    cols <- which(member[i, ])
    if (length(cols) == 1) m[, cols] else do.call(pmin, as.data.frame(m[, cols, drop = FALSE]))
  }, numeric(nrow(m)))                                    # draws x subsets
  if (is.null(dim(smallest))) smallest <- matrix(smallest, nrow = nrow(m))

  winning <- seats_by_coalition >= majority
  minimal <- winning & (seats_by_coalition - smallest) < majority

  tibble(
    coalition = apply(member, 1, \(r) paste(keep[r], collapse = " + ")),
    n_parties = rowSums(member),
    p_minimal_winning = colMeans(minimal),
    p_winning = colMeans(winning),
    median_seats = apply(seats_by_coalition, 2, median),
    majority = majority,
    excluded_parties = paste(excluded, collapse = ", ")
  ) |>
    filter(p_minimal_winning > 0) |>
    arrange(desc(p_minimal_winning))
}

#' Council control per draw: which party (if any) holds a majority alone
control_outcomes <- function(seat_mat, total_seats) {
  majority <- floor(total_seats / 2) + 1
  top <- max.col(seat_mat, ties.method = "first")
  has_majority <- seat_mat[cbind(seq_len(nrow(seat_mat)), top)] >= majority
  outcome <- if_else(has_majority, paste(colnames(seat_mat)[top], "majority"), "No majority")
  tibble(outcome = outcome) |>
    count(outcome, name = "draws") |>
    mutate(prob = draws / nrow(seat_mat)) |>
    arrange(desc(prob))
}
