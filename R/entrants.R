# entrants.R --------------------------------------------------------------------
# Parties standing in a council with no history there (MODEL-LOG L035).
#
# In the blind 2021 backtest, parties the model could not see won 46 Western
# Cape seats. For 2026 the candidate lists name every newcomer (125 party x
# council cases, 56 parties); what is unknown is how well each will do.
#
#   1. Past newcomers: every party that stood in a council in 2016 or 2021
#      with no votes there in the previous local or latest national election.
#   2. Classes: breadth (councils stood in: 1 / 2-10 / 11+) x ward coverage
#      (all wards / partial); a class with fewer than `min_cell` examples
#      falls back to breadth alone, then to all newcomers.
#   3. Each simulated election draws each newcomer's council share from its
#      class's historical distribution. Draws for one party are correlated
#      across councils (rho, estimated from past newcomers standing in
#      several councils), because a party's appeal is partly common.
#   4. A party-specific prior can override its class only through a sourced
#      row in data-raw/manual/entrant_priors.csv (for example, polls for a
#      national organisation standing on its own for the first time).
#
# Shares are council-level (ward and PR ballots combined). A newcomer's share
# is taken proportionally from all other parties, as in A12.

#' Which parties in `results_t` had no history in their council
first_timers <- function(results_t, lge_hist, npe_hist, year) {
  hist <- bind_rows(lge_hist, npe_hist) |> filter(votes > 0) |>
    distinct(muni_code, party) |> mutate(key = name_key(party))
  results_t <- results_t |> filter(ballot %in% c("PR", "Ward"))
  standing <- results_t |> filter(votes > 0, party != "INDEPENDENT") |> distinct(muni_code, party)
  shares <- results_t |> summarise(votes = sum(votes), .by = c(muni_code, party)) |>
    mutate(share = votes / sum(votes), .by = muni_code)
  n_wards <- results_t |> filter(ballot == "Ward") |> distinct(muni_code, ward_id) |> count(muni_code, name = "n_wards")
  covered <- results_t |> filter(ballot == "Ward", votes > 0) |> distinct(muni_code, party, ward_id) |>
    count(muni_code, party, name = "wards_contested")
  standing |>
    mutate(key = name_key(party)) |>
    anti_join(hist, by = c("muni_code", "key")) |>
    left_join(shares, by = c("muni_code", "party")) |>
    left_join(covered, by = c("muni_code", "party")) |>
    left_join(n_wards, by = "muni_code") |>
    left_join(count(standing, party, name = "councils"), by = "party") |>
    mutate(wards_contested = coalesce(wards_contested, 0L), coverage = wards_contested / n_wards,
           history_in_province = key %in% hist$key, year = year) |>
    select(year, muni_code, party, share, coverage, councils, history_in_province)
}

entrant_class <- function(councils, coverage) {
  breadth <- case_when(councils <= 1 ~ "1 council", councils <= 10 ~ "2-10 councils", TRUE ~ "11+ councils")
  paste(breadth, if_else(coverage >= 0.99, "all wards", "partial"), sep = ", ")
}

#' Class distributions and cross-council correlation from past newcomers
estimate_entrant_prior <- function(ft, min_cell = 8, rho_default = 0.5, min_pairs = 10) {
  ft <- ft |> filter(!is.na(share), share > 0) |>
    mutate(cls = entrant_class(councils, coverage), breadth = str_remove(cls, ",.*$"))
  # normal scores within class, then the correlation of a party's scores
  # across the councils it stood in
  z <- ft |> mutate(z = qnorm((rank(share) - 0.5) / n()), .by = cls)
  pairs <- z |> filter(n() >= 2, .by = c(year, party)) |>
    nest(.by = c(year, party)) |>
    mutate(p = map(data, \(d) { cmb <- utils::combn(nrow(d), 2); tibble(a = d$z[cmb[1, ]], b = d$z[cmb[2, ]]) })) |>
    select(p) |> unnest(p)
  rho <- if (nrow(pairs) >= min_pairs) max(0, cor(c(pairs$a, pairs$b), c(pairs$b, pairs$a))) else rho_default
  list(examples = ft, rho = rho, rho_pairs = nrow(pairs), min_cell = min_cell,
       rho_source = if (nrow(pairs) >= min_pairs) "estimated" else "default (too few pairs)",
       classes = ft |> summarise(n = n(), median = median(share), q90 = quantile(share, 0.9),
                                 max = max(share), .by = cls) |> arrange(cls))
}

#' The historical shares a newcomer is drawn from (with fallbacks)
entrant_pool <- function(prior, councils, coverage) {
  cls <- entrant_class(councils, coverage)
  ex <- prior$examples
  pool <- ex$share[ex$cls == cls]
  if (length(pool) >= prior$min_cell) return(list(pool = pool, basis = cls))
  br <- str_remove(cls, ",.*$")
  pool <- ex$share[ex$breadth == br]
  if (length(pool) >= prior$min_cell) return(list(pool = pool, basis = paste(br, "(pooled coverage)")))
  list(pool = ex$share, basis = "all newcomers")
}

#' Newcomers in the target election: on a council's list, not in its groups
find_entrants <- function(pr_lists, contests, groups) {
  if (is.null(pr_lists)) return(tibble())
  n_wards <- contests |> distinct(muni_code, ward_id) |> count(muni_code, name = "n_wards")
  pr_lists |> filter(party != "INDEPENDENT") |>
    anti_join(distinct(groups, muni_code, party), by = c("muni_code", "party")) |>
    left_join(contests |> distinct(muni_code, party, ward_id) |> count(muni_code, party, name = "w"),
              by = c("muni_code", "party")) |>
    left_join(n_wards, by = "muni_code") |>
    mutate(coverage = coalesce(w, 0L) / n_wards) |>
    left_join(count(pr_lists, party, name = "councils"), by = "party") |>
    select(muni_code, party, coverage, councils)
}

#' Correlated share draws for every newcomer: a list of draws x party matrices,
#' one per council
draw_entrant_shares <- function(entrants, prior, n_draws, seed, overrides = NULL, max_total = 0.8) {
  if (!nrow(entrants)) return(list())
  set.seed(seed)
  parties <- unique(entrants$party)
  z_party <- matrix(rnorm(n_draws * length(parties)), n_draws, dimnames = list(NULL, parties))
  rho <- prior$rho
  cols <- pmap(entrants, \(muni_code, party, coverage, councils) {
    u <- pnorm(rho * z_party[, party] + sqrt(1 - rho^2) * rnorm(n_draws))
    ov <- if (!is.null(overrides)) filter(overrides, .data$party == !!party) else tibble()
    if (nrow(ov)) { # sourced party-specific prior: log-normal through median and q90
      s <- qlnorm(u, log(ov$median[1]), (log(ov$q90[1]) - log(ov$median[1])) / qnorm(0.9))
    } else {
      s <- unname(quantile(entrant_pool(prior, councils, coverage)$pool, u, type = 7))
    }
    pmin(s, 0.95)
  })
  out <- split(seq_len(nrow(entrants)), entrants$muni_code) |>
    map(\(i) {
      m <- do.call(cbind, cols[i]); colnames(m) <- entrants$party[i]
      tot <- rowSums(m)
      m * pmin(1, max_total / pmax(tot, 1e-9)) # newcomers together never exceed max_total
    })
  out
}

#' Table of newcomers for publication
describe_entrants <- function(entrants, prior, draws, overrides = NULL) {
  if (!nrow(entrants)) return(tibble())
  entrants |> mutate(
    basis = pmap_chr(list(party, councils, coverage), \(p, k, c) {
      if (!is.null(overrides) && p %in% overrides$party) paste("override:", overrides$source[overrides$party == p][1])
      else entrant_pool(prior, k, c)$basis }),
    share_median = map2_dbl(muni_code, party, \(m, p) median(draws[[m]][, p])),
    share_q05 = map2_dbl(muni_code, party, \(m, p) quantile(draws[[m]][, p], 0.05)),
    share_q95 = map2_dbl(muni_code, party, \(m, p) quantile(draws[[m]][, p], 0.95)))
}

read_entrant_overrides <- function(path) {
  if (!file.exists(path)) return(NULL)
  x <- read_csv(path, comment = "#", show_col_types = FALSE, col_types = "cddcc")
  if (!nrow(x)) return(NULL)
  bad <- x |> filter(is.na(source) | !nzchar(source) | !(median > 0 & q90 > median))
  if (nrow(bad)) stop("entrant_priors.csv: every row needs a source and 0 < median < q90: ",
                      paste(bad$party, collapse = ", "), call. = FALSE)
  mutate(x, party = party_key(party))
}
