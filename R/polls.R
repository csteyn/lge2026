# polls.R -----------------------------------------------------------------------
# Polls are a published CROSS-CHECK, not a model input (MODEL-LOG L041).
# Before 2024, Western Cape polls missed the provincial result by large
# margins in both directions (DA -11 to +9 points, EFF up to +10; L042), so
# there is no consistent bias to correct, and letting polls narrow the
# province spread would replace a tested assumption with a larger one.

#' The model's provincial PR vote: council PR votes summed in every
#' simulated election, then summarised across elections
province_vote_share <- function(sims, baseline) {
  reg <- baseline |> distinct(muni_code, vd, registered) |> summarise(registered = sum(registered), .by = muni_code)
  map_dfr(sims, \(s) {
    voters <- s$turnout * reg$registered[reg$muni_code == s$muni_code]
    as_tibble(s$pr_share * voters) |> mutate(draw = row_number()) |>
      pivot_longer(-draw, names_to = "party", values_to = "votes")
  }) |>
    summarise(votes = sum(votes), .by = c(draw, party)) |>
    mutate(share = votes / sum(votes), .by = draw) |>
    summarise(median = median(share), q05 = quantile(share, 0.05), q95 = quantile(share, 0.95), .by = party) |>
    arrange(desc(median))
}

read_polls <- function(path) {
  if (!file.exists(path)) return(tibble())
  read_csv(path, comment = "#", show_col_types = FALSE) |> mutate(party = party_key(party))
}

#' Poll against model, for the province and for Cape Town
compare_polls <- function(polls, prov_share, vote_share, province_label = "Western Cape") {
  if (!nrow(polls)) return(tibble())
  model <- bind_rows(
    mutate(prov_share, scope = province_label),
    vote_share |> filter(muni_code == "CPT") |>
      transmute(party, median = pr_share_median, q05 = pr_share_q05, q95 = pr_share_q95, scope = "Cape Town")
  )
  polls |> inner_join(rename(model, model_median = median, model_q05 = q05, model_q95 = q95), by = c("scope", "party")) |>
    mutate(difference = model_median - share, outside_moe = abs(difference) > moe95)
}
