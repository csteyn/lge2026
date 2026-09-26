# decompose.R -------------------------------------------------------------------
# From the 2024 result to the forecast, one step at a time (MODEL-LOG L046, O10).
#
# A DIAGNOSTIC only. Each step is computed from objects the forecast already
# uses, nothing here feeds back into the forecast, and no setting is changed
# on the strength of it without a pre-registered backtest. Its job is to show
# WHERE the forecast departs from the last result, party by party:
#
#   0 the 2024 provincial-ballot result, in the model's party groups
#   1 the same VD shares on the 2026 VD layer and registration (2024 turnout);
#     a closure check, expected to change almost nothing
#   2 ... with each VD's turnout from the 2021 local election
#   3 ... after the national-to-local translation (learned premiums, parties
#     that stand only locally, OTHER, VDs with no history)
#   4 ... plus the central local premiums of parties with no fitted
#     translation, and any sourced swing priors
#   5 ... plus newcomers at their median council shares (the sum of medians
#     approximates the median total)
#   6 the simulation's median. Medians of skewed distributions do not sum to
#     one, and symmetric noise on the log scale pulls large parties' shares
#     down (convexity), so the change from 5 to 6 measures that effect.

decomposition_steps <- c(
  "2024 result",
  "on 2026 voting districts and registration",
  "with 2021 local-election turnout",
  "national-to-local translation",
  "premiums of parties without a fitted translation",
  "newcomers",
  "simulation median"
)

decompose_shares <- function(npe_latest, groups, baseline, premium = NULL, swing_priors = NULL,
                             entrant_draws = list(), province_share = NULL, vote_share = NULL,
                             province_label = "Western Cape") {
  steps <- decomposition_steps
  g <- groups |> distinct(muni_code, party, group)
  vds <- baseline |> distinct(muni_code, vd, registered, turnout)
  agg <- function(d, step) { # d: muni_code, group, votes
    bind_rows(
      d |> summarise(votes = sum(votes), .by = group) |> mutate(scope = province_label),
      d |> summarise(votes = sum(votes), .by = c(muni_code, group)) |> rename(scope = muni_code)
    ) |>
      mutate(share = votes / sum(votes), .by = scope) |>
      transmute(scope, party = group, step = step, share)
  }

  # 0-2: the 2024 vote by VD in model groups (ABSENT removed, A12; parties
  # outside the groups pooled as OTHER, as the model does)
  npe <- npe_latest |> filter(muni_code %in% vds$muni_code)
  v24 <- npe |>
    left_join(g, by = c("muni_code", "party")) |>
    mutate(group = coalesce(group, "OTHER")) |>
    filter(group != "ABSENT") |>
    summarise(votes = sum(votes), .by = c(muni_code, vd, group))
  t24 <- npe |> distinct(muni_code, vd, registered, spoilt, vd_valid) |>
    mutate(t24 = (vd_valid + spoilt) / registered) |>
    filter(is.finite(t24), t24 > 0, t24 <= 1.05) |>
    distinct(muni_code, vd, .keep_all = TRUE) |> select(muni_code, vd, t24)
  on26 <- v24 |> mutate(share = votes / sum(votes), .by = c(muni_code, vd)) |>
    inner_join(vds, by = c("muni_code", "vd"))
  s0 <- agg(select(v24, muni_code, group, votes), steps[1])
  s1 <- on26 |> inner_join(t24, by = c("muni_code", "vd")) |>
    transmute(muni_code, group, votes = registered * t24 * share) |> agg(steps[2])
  s2 <- on26 |> transmute(muni_code, group, votes = registered * turnout * share) |> agg(steps[3])

  # 3: the baseline itself (every 2026 VD)
  bw <- baseline |> mutate(voters = registered * turnout)
  s3 <- bw |> transmute(muni_code, group, votes = voters * exp(eta_pr)) |> agg(steps[4])

  # 4: central shifts the simulation adds on the log scale
  prem <- if (!is.null(premium) && nrow(premium)) select(premium, group, m_prem = post_mean) else
    tibble(group = character(), m_prem = numeric())
  sp <- swing_priors %||% tibble(party = character(), scope = character(), mean = numeric())
  prov_sw <- sp |> filter(scope == "province") |> distinct(party, .keep_all = TRUE) |> select(group = party, m_prov = mean)
  muni_sw <- sp |> filter(scope != "province") |> distinct(scope, party, .keep_all = TRUE) |>
    select(muni_code = scope, group = party, m_muni = mean)
  b4 <- bw |>
    left_join(prem, by = "group") |> left_join(prov_sw, by = "group") |>
    left_join(muni_sw, by = c("muni_code", "group")) |>
    mutate(eta = eta_pr + coalesce(m_prem, 0) + coalesce(m_prov, 0) + coalesce(m_muni, 0)) |>
    mutate(p = exp(eta - max(eta)), p = p / sum(p), .by = c(muni_code, vd))
  c4 <- b4 |> summarise(votes = sum(voters * p), .by = c(muni_code, group))
  s4 <- agg(c4, steps[5])

  # 5: newcomers at their median council shares; everyone else scaled down
  ent <- if (length(entrant_draws)) imap_dfr(entrant_draws, \(m, code)
    tibble(muni_code = code, group = colnames(m), med = apply(m, 2, median))) else tibble()
  c5 <- if (nrow(ent)) {
    cv <- b4 |> distinct(muni_code, vd, voters) |> summarise(voters = sum(voters), .by = muni_code)
    tot <- ent |> summarise(total = sum(med), .by = muni_code)
    bind_rows(
      c4 |> left_join(tot, by = "muni_code") |> mutate(votes = votes * (1 - coalesce(total, 0))) |> select(-total),
      ent |> left_join(cv, by = "muni_code") |> transmute(muni_code, group, votes = voters * med))
  } else c4
  s5 <- agg(c5, steps[6])

  # 6: what the forecast reports
  s6 <- bind_rows(
    if (!is.null(province_share)) transmute(province_share, scope = province_label, party, share = median),
    if (!is.null(vote_share)) transmute(vote_share, scope = muni_code, party, share = pr_share_median)
  )
  if (nrow(s6)) s6 <- mutate(s6, step = steps[7])

  bind_rows(s0, s1, s2, s3, s4, s5, s6) |>
    mutate(step_no = match(step, steps) - 1L, newcomer = party %in% (if (nrow(ent)) ent$group else character())) |>
    tidyr::complete(tidyr::nesting(scope, party, newcomer), tidyr::nesting(step_no, step), fill = list(share = 0)) |>
    arrange(scope, party, step_no) |>
    select(scope, party, newcomer, step_no, step, share)
}
