# backtest.R --------------------------------------------------------------------
# Predict the 2021 local election exactly as we predict 2026, using only what
# was known before it, then score the prediction against what happened
# (MODEL-LOG L026).
#
#   role                         2026 forecast        2021 backtest
#   translation learned from     2019 NPE -> 2021 LGE 2014 NPE -> 2016 LGE
#   latest national vote         2024 NPE             2019 NPE
#   previous local election      2021 LGE             2016 LGE
#   who is standing              2026 candidate lists 2021 results (same information)
#   truth                        4 Nov 2026           2021 results and IEC seat calculations
#
# The model never sees any 2021 total. Two lessons from the Johannesburg
# model's backtest shape this: the spatial structure of support transfers
# between cycles but the overall "political weather" does not; and a backtest
# given the real citywide result looks five times better than a blind one.
#
# The same functions run the forecast and the backtest. Only the orchestration
# in run_chain() mirrors _targets.R; if the two drift apart, test-backtest.R
# fails.

empty_swing <- function() tibble(party = character(), scope = character(), mean = numeric(), sd = numeric())

# 1. Inputs ----------------------------------------------------------------------

#' Who stood in the truth election, from its own results (the backtest
#' equivalent of the 2026 candidate lists)
standing_from_results <- function(lge_truth) {
  list(
    pr_lists = lge_truth |> filter(ballot == "PR", votes > 0) |> distinct(muni_code, party),
    contests = lge_truth |> filter(ballot == "Ward", votes > 0) |> distinct(muni_code, ward_id, party)
  )
}

assemble_backtest_inputs <- function(npe_prev, lge_prev, npe_latest, lge_truth, councils,
                                     municipalities, seat_truth = NULL, label = "backtest", interp_frac_value = 0.5) {
  # national-election rows belong to a council through their VD number
  vd_muni <- bind_rows(distinct(lge_truth, vd, muni_code), distinct(lge_prev, vd, muni_code)) |>
    distinct(vd, .keep_all = TRUE)
  by_vd <- function(npe) npe |> left_join(rename(vd_muni, m2 = muni_code), by = "vd") |>
    mutate(muni_code = coalesce(m2, muni_code)) |> select(-m2)
  keep <- municipalities$muni_code
  scope <- function(x) filter(x, muni_code %in% keep)

  st <- standing_from_results(scope(lge_truth))
  lp <- scope(lge_prev); nl <- scope(by_vd(npe_latest))
  vd_new <- scope(lge_truth) |> filter(ballot == "PR") |>
    distinct(muni_code, ward_id, vd, registered) |> distinct(muni_code, vd, .keep_all = TRUE)
  list(
    label = label,
    inputs = list(interp_frac = interp_frac_value, npe_prev = scope(by_vd(npe_prev)), lge_prev = scope(lge_prev),
                  npe_latest = scope(by_vd(npe_latest)), vd_new = vd_new,
                  councils = scope(councils), contests = st$contests, pr_lists = st$pr_lists,
                  municipalities = municipalities),
    truth = c(backtest_truth(scope(lge_truth), scope(councils), seat_truth),
              list(cases = scoring_cases(st$pr_lists, lp, nl)))
  )
}

#' The cases every forecast and simple rule is scored on (L037): every party
#' on a council's ballot, whatever any model does. `established` marks
#' parties with 2%+ in that council at the previous local or the latest
#' national election, fixed before the forecast; coverage is measured on them.
scoring_cases <- function(pr_lists, lge_prev, npe_latest, threshold = 0.02) {
  prev <- bind_rows(filter(lge_prev, ballot == "PR"), npe_latest) |>
    summarise(votes = sum(votes), .by = c(muni_code, election, party)) |>
    mutate(share = votes / sum(votes), .by = c(muni_code, election)) |>
    summarise(prev_share = max(share), .by = c(muni_code, party))
  pr_lists |> distinct(muni_code, party) |> filter(party != "INDEPENDENT") |>
    left_join(prev, by = c("muni_code", "party")) |>
    mutate(established = coalesce(prev_share, 0) >= threshold) |> select(-prev_share)
}

#' What actually happened: seats, ward winners and PR shares
backtest_truth <- function(lge_truth, councils, seat_truth = NULL) {
  winners <- lge_truth |> filter(ballot == "Ward") |>
    summarise(votes = sum(votes), .by = c(muni_code, ward_id, party)) |>
    slice_max(votes, n = 1, by = c(muni_code, ward_id), with_ties = FALSE) |>
    select(muni_code, ward_id, winner = party)
  if (is.null(seat_truth)) { # recompute with the validated allocator (C14)
    seat_truth <- lge_truth |> filter(ballot %in% c("PR", "Ward"), party != "INDEPENDENT") |>
      summarise(votes = sum(votes), .by = c(muni_code, party)) |>
      nest(.by = muni_code) |>
      mutate(alloc = map2(muni_code, data, \(m, d) {
        w <- winners |> filter(muni_code == m, winner %in% d$party) |> count(winner)
        other_w <- sum(winners$muni_code == m) - sum(w$n)
        allocate_seats(setNames(d$votes, d$party), setNames(w$n, w$winner),
                       councils$total_seats[councils$muni_code == m], no_list_wards = other_w) |>
          select(party, seats)
      })) |>
      select(muni_code, alloc) |> unnest(alloc)
  }
  pr <- lge_truth |> filter(ballot == "PR") |>
    summarise(votes = sum(votes), .by = c(muni_code, party)) |>
    mutate(share = votes / sum(votes), .by = muni_code)
  list(seats = select(seat_truth, muni_code, party, seats), winners = winners, pr = pr)
}

# 2. The model chain ----------------------------------------------------------------

#' Everything before the simulation: done once per intercept option.
#' intercepts = "fitted" carries each party's learned local premium `a` into
#' the next cycle; "zero" keeps only the learned spatial structure `b`
#' (the backtest showed the premiums do not transfer between cycles, L027).
prepare_chain <- function(inp, cfg, intercepts = c("fitted", "zero"), entrant_prior = NULL, entrant_overrides = NULL,
                          b_mode = cfg$model$transfer_b %||% "fitted", ward_ratio = cfg$model$ward_ratio %||% TRUE,
                          shrink = cfg$model$premium_shrink %||% 1,
                          premium_basis = cfg$model$premium_basis %||% "earlier") {
  intercepts <- match.arg(intercepts)
  groups <- define_party_groups(inp$lge_prev, inp$npe_latest, cfg, inp$pr_lists)
  ow <- other_composition(groups, inp$lge_prev, inp$npe_latest)
  interp <- premium_basis == "interpolated"
  tr <- apply_transfer_variant(fit_transfer(inp$npe_prev, inp$lge_prev, groups, cfg,
                                            npe_next = if (interp) inp$npe_latest, frac = if (interp) inp$interp_frac %||% 0.5),
                               b_mode, shrink, calibrate = identical(cfg$model$premium_calibration, "totals"))
  if (intercepts == "zero") tr$coefs$a <- 0
  cg <- if (is.null(inp$contests)) NULL else inp$contests |>
    inner_join(select(groups, muni_code, party, group), by = c("muni_code", "party")) |>
    filter(group != "ABSENT") |> distinct(muni_code, ward_id, group)
  base <- build_baseline(inp$npe_latest, inp$lge_prev, inp$vd_new, groups, tr, cg, ward_ratio = ward_ratio)
  # with zero intercepts there is no learned premium to draw an unfitted party's from
  prem <- if (intercepts == "fitted") local_premium(groups, tr) else NULL
  entrants <- if (!is.null(entrant_prior)) find_entrants(inp$pr_lists, inp$contests, groups, base) else tibble()
  list(inp = inp, groups = groups, other_w = ow, transfer = tr, baseline = base, premium = prem,
       intercepts = intercepts, entrants = entrants, entrant_prior = entrant_prior,
       entrant_overrides = entrant_overrides)
}

simulate_chain <- function(prep, cfg, n_draws = 1000, sd_override = list()) {
  cfg$model$n_draws <- n_draws
  cfg$model$sd <- utils::modifyList(cfg$model$sd, sd_override)
  base <- prep$baseline
  prov <- draw_province_effects(sort(unique(base$group)), empty_swing(), cfg, prep$premium)
  ed <- if (nrow(prep$entrants %||% tibble())) draw_entrant_shares(prep$entrants, prep$entrant_prior, n_draws,
                                                                  cfg$model$seed + 7, prep$entrant_overrides) else list()
  sims <- map(split(base, base$muni_code), \(b) simulate_municipality(
    b, filter(prep$inp$councils, muni_code == b$muni_code[1]), prov, prep$other_w, empty_swing(),
    prep$transfer, cfg, entrant_shares = ed[[b$muni_code[1]]], entrant_contests = prep$inp$contests))
  c(prep[setdiff(names(prep), "inp")], list(sims = sims, sd_used = cfg$model$sd, n_draws = n_draws))
}

run_chain <- function(inp, cfg, n_draws = 1000, sd_override = list(), intercepts = "fitted") {
  simulate_chain(prepare_chain(inp, cfg, intercepts), cfg, n_draws, sd_override)
}

# 3. Scoring --------------------------------------------------------------------------

crps_sample <- function(x, y) mean(abs(x - y)) - 0.5 * mean(abs(outer(x, x, "-")))

#' Exact CRPS for whole-number outcomes (seats): the CRPS integral is a sum
#' over integers of (F(k) - 1{y <= k})^2. Identical to crps_sample() on all
#' draws, but exact, deterministic and fast (L036).
crps_int <- function(x, y) {
  lo <- min(x, y); hi <- max(x, y)
  if (lo == hi) return(0)
  k <- lo:(hi - 1)
  Fk <- ecdf(x)(k)
  sum((Fk - (y <= k))^2)
}

#' Probability that a randomised PIT falls in [a, b], computed exactly: the
#' coverage a calibrated forecast would show, with no random draw (L036)
pit_in <- function(x, y, a, b) {
  lo <- mean(x < y); p <- mean(x == y)
  if (p == 0) return(as.numeric(lo >= a & lo <= b))
  u_lo <- min(max((a - lo) / p, 0), 1); u_hi <- min(max((b - lo) / p, 0), 1)
  u_hi - u_lo
}

#' Randomised PIT for a discrete outcome: uniform if the forecast is calibrated
pit_discrete <- function(x, y, u = runif(1)) mean(x < y) + u * mean(x == y)

score_chain <- function(chain, truth, seed = 1) {
  force(chain) # evaluate the simulation BEFORE seeding: R is lazy, and the
  set.seed(seed) # simulation reseeds the generator itself (L036)
  # seats: every party that won seats or had a real chance of one
  seats <- imap_dfr(chain$sims, \(sim, m) {
    s <- sim$seats
    act <- truth$seats |> filter(muni_code == m, seats > 0)
    # common case set (L037): every party on the ballot plus every seat winner,
    # the same for every model; the old rule (seat winners plus parties this
    # model gave a 5% chance) differed between models and diluted averages
    cand <- if (!is.null(truth$cases)) union(truth$cases$party[truth$cases$muni_code == m], act$party)
            else union(colnames(s)[colMeans(s > 0) >= 0.05], act$party)
    map_dfr(cand, \(p) {
      x <- if (p %in% colnames(s)) s[, p] else rep(0L, nrow(s))
      y <- coalesce(act$seats[match(p, act$party)], 0L)
      q <- quantile(x, c(0.05, 0.25, 0.75, 0.95), type = 1, names = FALSE)
      tibble(muni_code = m, party = p, actual = y, median = median(x), q05 = q[1], q95 = q[4],
             in50 = y >= q[2] & y <= q[3], in90 = y >= q[1] & y <= q[4],
             pit = pit_discrete(x, y), crps = crps_int(x, y),
             p_in50 = pit_in(x, y, 0.25, 0.75), p_in90 = pit_in(x, y, 0.05, 0.95),
             p_low10 = pit_in(x, y, 0, 0.1), p_high10 = pit_in(x, y, 0.9, 1),
             modelled = p %in% colnames(s),
             established = if (!is.null(truth$cases))
               isTRUE(truth$cases$established[truth$cases$muni_code == m & truth$cases$party == p][1]) else TRUE)
    })
  })
  # council control
  control <- imap_dfr(chain$sims, \(sim, m) {
    B <- sim$total_seats; maj <- floor(B / 2) + 1
    act <- truth$seats |> filter(muni_code == m)
    actual <- if (max(act$seats) >= maj) paste(act$party[which.max(act$seats)], "majority") else "No majority"
    pr <- control_outcomes(sim$seats, B)
    p <- coalesce(pr$prob[match(actual, pr$outcome)], 0)
    tibble(muni_code = m, actual = actual, p_actual = p,
           brier = (1 - p)^2 + sum(pr$prob[pr$outcome != actual]^2),
           modal_correct = pr$outcome[1] == actual)
  })
  # ward winners
  wards <- imap_dfr(chain$sims, \(sim, m) {
    w <- as_tibble(sim$winners) |> pivot_longer(everything(), names_to = "ward_id", values_to = "party") |>
      count(ward_id, party) |> mutate(p = n / nrow(sim$winners), .by = ward_id)
    truth$winners |> filter(muni_code == m) |>
      left_join(rename(w, winner = party), by = c("ward_id", "winner")) |>
      mutate(p = coalesce(p, 0)) |>
      left_join(w |> summarise(sum_p2 = sum(p^2), top = party[which.max(p)], .by = ward_id), by = "ward_id") |>
      transmute(muni_code, ward_id, winner, p_actual = p, model_pick = top,
                brier = (1 - p)^2 + coalesce(sum_p2, 0) - p^2,
                log_score = log(pmax(p, 0.5 / chain$n_draws)),
                modal_correct = coalesce(top == winner, FALSE))
  })
  blind <- seats |> filter(!modelled, actual > 0)
  m <- filter(seats, established) # coverage on a model-independent set fixed in advance (L037)
  summary <- tibble(
    seat_intervals = nrow(seats),
    coverage50 = mean(seats$in50), coverage90 = mean(seats$in90),
    # randomised-PIT coverage: exact under calibration even for small whole
    # numbers, where quantile intervals over-cover (L027); modelled parties only
    # exact expected randomised-PIT coverage (no random draw, L036)
    pit_cov50 = mean(m$p_in50), pit_cov90 = mean(m$p_in90),
    pit_low10 = mean(m$p_low10), pit_high10 = mean(m$p_high10),
    seat_crps = mean(seats$crps), seat_mae_median = mean(abs(seats$median - seats$actual)),
    pit_ks_p = suppressWarnings(stats::ks.test(seats$pit, "punif")$p.value),
    control_brier = mean(control$brier), control_modal_correct = mean(control$modal_correct),
    ward_brier = mean(wards$brier), ward_log_score = mean(wards$log_score),
    ward_modal_correct = mean(wards$modal_correct),
    new_entrant_seats = sum(blind$actual)
  )
  list(seats = seats, control = control, wards = wards, blind = blind, summary = summary)
}

# 4. The simple rules the model must beat (SCORING.md) ----------------------------------

naive_rule <- function(inp, truth, source = c("previous_local", "national_as_is")) {
  source <- match.arg(source)
  vd_ward <- distinct(inp$vd_new, vd, muni_code, ward_id)
  src <- if (source == "previous_local") inp$lge_prev else mutate(inp$npe_latest, ballot = "PR")
  ward_votes <- if (source == "previous_local") filter(src, ballot == "Ward") else src
  winners <- ward_votes |> select(-ward_id, -muni_code) |>
    inner_join(vd_ward, by = "vd") |>
    semi_join(inp$contests, by = c("muni_code", "ward_id", "party")) |>
    summarise(votes = sum(votes), .by = c(muni_code, ward_id, party)) |>
    slice_max(votes, n = 1, by = c(muni_code, ward_id), with_ties = FALSE) |>
    select(muni_code, ward_id, pred = party)
  combined <- src |> filter(ballot %in% c("PR", "Ward")) |>
    mutate(votes = if (source == "national_as_is") 2L * votes else votes) |>
    semi_join(inp$pr_lists, by = c("muni_code", "party")) |>
    summarise(votes = sum(votes), .by = c(muni_code, party))
  seats <- combined |> nest(.by = muni_code) |>
    mutate(alloc = map2(muni_code, data, \(m, d) {
      w <- winners |> filter(muni_code == m, pred %in% d$party) |> count(pred)
      nw <- sum(inp$vd_new$muni_code == m & !duplicated(inp$vd_new[c("muni_code", "ward_id")]))
      allocate_seats(setNames(d$votes, d$party), setNames(w$n, w$pred),
                     inp$councils$total_seats[inp$councils$muni_code == m],
                     no_list_wards = max(0, nw - sum(w$n))) |> select(party, pred_seats = seats)
    })) |> select(muni_code, alloc) |> unnest(alloc)
  cmp <- full_join(seats, rename(truth$seats, actual = seats), by = c("muni_code", "party")) |>
    mutate(across(c(pred_seats, actual), \(x) coalesce(as.integer(x), 0L)))
  cmp <- if (!is.null(truth$cases)) { # the common case set, as for the model (L037)
    truth$cases |> select(muni_code, party) |>
      full_join(filter(cmp, pred_seats > 0 | actual > 0), by = c("muni_code", "party")) |>
      mutate(across(c(pred_seats, actual), \(x) coalesce(x, 0L)))
  } else filter(cmp, pred_seats > 0 | actual > 0)
  ctrl <- cmp |> nest(.by = muni_code) |>
    mutate(ok = map2_lgl(muni_code, data, \(m, d) {
      B <- inp$councils$total_seats[inp$councils$muni_code == m]; maj <- floor(B / 2) + 1
      f <- \(v) if (max(v) >= maj) d$party[which.max(v)] else "none"
      f(d$pred_seats) == f(d$actual)
    }))
  wr <- truth$winners |> left_join(winners, by = c("muni_code", "ward_id"))
  out <- tibble(rule = source, seat_mae = mean(abs(cmp$pred_seats - cmp$actual)),
                control_correct = mean(ctrl$ok), ward_correct = mean(coalesce(wr$pred == wr$winner, FALSE)))
  attr(out, "wards") <- select(wr, muni_code, ward_id, pred)
  out
}

# 5. Estimating the A02 shock sizes ----------------------------------------------------

#' Council-level surprises, split into a province-wide part per party (the
#' "weather") and a council-specific part. Their spreads are direct
#' estimates of sd.province_party and sd.muni_party.
estimate_shock_sds <- function(chain, truth, min_councils = 3) {
  pred <- chain$baseline |>
    mutate(p = exp(eta_pr), w = registered * turnout) |>
    summarise(pred = sum(p * w) / sum(w), .by = c(muni_code, group))
  act <- truth$pr |>
    left_join(select(chain$groups, muni_code, party, group), by = c("muni_code", "party")) |>
    filter(!is.na(group), group != "ABSENT") |>
    summarise(votes = sum(votes), .by = c(muni_code, group)) |>
    mutate(act = votes / sum(votes), .by = muni_code)
  e <- inner_join(pred, act, by = c("muni_code", "group")) |>
    filter(act > 0, pred > 0) |>
    mutate(e = (log(act) - mean(log(act))) - (log(pred) - mean(log(pred))), .by = muni_code)
  named <- e |> filter(group != "OTHER")
  prov <- named |> summarise(u = mean(e), n = n(), .by = group) |> filter(n >= min_councils)
  within <- named |> inner_join(prov, by = "group") |> mutate(v = e - u)
  list(
    province = sd(prov$u), muni = sd(within$v),
    n_parties = nrow(prov), n_cells = nrow(within),
    by_party = arrange(prov, desc(abs(u))), residuals = e
  )
}

# 6. The pre-registered experiment (MODEL-LOG L027) ---------------------------------------
#
# Written down before the results were seen:
#   candidates  intercepts {fitted, zero} x province spread {0.10, 0.20, 0.35, 0.55}
#               x council spread {0.15, 0.30, 0.45}: 24 settings
#   choose      the lowest mean seat CRPS over all party x council cases,
#               among settings whose PIT coverage at 90% (modelled parties)
#               lies in [0.80, 0.97]
#   report      everything else, without optimising it

backtest_grid <- function(bt, cfg, provinces = c(0.10, 0.20, 0.35, 0.55), munis = c(0.15, 0.30, 0.45),
                          intercepts = c("fitted", "zero"), n_draws = 400) {
  map_dfr(intercepts, \(ic) {
    prep <- prepare_chain(bt$inputs, cfg, ic)
    tidyr::expand_grid(province = provinces, muni = munis) |>
      pmap_dfr(\(province, muni) {
        ch <- simulate_chain(prep, cfg, n_draws, list(province_party = province, muni_party = muni))
        sc <- score_chain(ch, bt$truth)
        mutate(sc$summary, intercepts = ic, province = province, muni = muni, .before = 1)
      })
  }) |>
    mutate(eligible = pit_cov90 >= 0.80 & pit_cov90 <= 0.97,
           chosen = eligible & seat_crps == min(seat_crps[eligible]))
}

# 7. Newcomer module, pre-registered comparison (MODEL-LOG L035) ---------------------------
#
# Written down before the result was seen: the 2021 backtest is run with and
# without the newcomer module, at the chosen settings (fitted premiums,
# spreads from config.yml). The newcomer prior comes from 2016's newcomers
# ONLY (2021's own would be circular). Adopt the module if seat CRPS is lower
# with it AND PIT coverage at 90% (modelled parties) lies in [0.80, 0.97].

backtest_entrants <- function(bt, cfg, entrant_prior, n_draws, without) {
  if (is.null(entrant_prior)) return(tibble(note = "no newcomer prior (2011 or 2016 results missing)"))
  prep <- prepare_chain(bt$inputs, cfg, "fitted", entrant_prior = entrant_prior)
  with <- score_chain(simulate_chain(prep, cfg, n_draws), bt$truth)
  # paired difference on the common cases, with a bootstrap over councils
  d <- inner_join(select(without$seats, muni_code, party, c0 = crps), select(with$seats, muni_code, party, c1 = crps),
                  by = c("muni_code", "party")) |> summarise(diff = sum(c1 - c0), n = n(), .by = muni_code)
  boot <- withr::with_seed(1, replicate(2000, { i <- sample.int(nrow(d), replace = TRUE); sum(d$diff[i]) / sum(d$n[i]) }))
  bind_rows(mutate(without$summary, module = "without newcomers"),
            mutate(with$summary, module = "with newcomers")) |>
    mutate(crps_diff = c(NA, sum(d$diff) / sum(d$n)), crps_diff_lo = c(NA, quantile(boot, 0.05)),
           crps_diff_hi = c(NA, quantile(boot, 0.95))) |>
    mutate(newcomer_cases = c(0L, nrow(prep$entrants)), rho = entrant_prior$rho,
           adopt = module == "with newcomers" & seat_crps < min(seat_crps[module == "without newcomers"]) &
             pit_cov90 >= 0.80 & pit_cov90 <= 0.97, .before = 1)
}

# 8. The ward-winner gap (O7): pre-registered experiment (MODEL-LOG L039) -----------------
#
# Written down before any result: 12 variants = slopes {fitted, corrected for
# attenuation, fixed at 1} x ward/PR ticket-splitting ratio {on, off} x
# learned premiums {as fitted, halved}, all at the spreads and newcomer setting
# in config.yml. Rule: choose the lowest mean ward Brier score among variants
# whose mean seat CRPS is at most 0.005 worse than the current model's and whose
# established-party 90% coverage is in [0.80, 0.97]; adopt it only if its ward
# Brier score beats the current model's. If none qualifies, nothing changes.
# Everything else is reported.

backtest_ward_grid <- function(bt, cfg, entrant_prior = NULL, n_draws = 400) {
  ep <- if (isTRUE(cfg$model$entrants)) entrant_prior else NULL
  g <- tidyr::expand_grid(b_mode = c("fitted", "corrected", "one"), ward_ratio = c(TRUE, FALSE), shrink = c(1, 0.5)) |>
    pmap_dfr(\(b_mode, ward_ratio, shrink) {
      prep <- prepare_chain(bt$inputs, cfg, "fitted", entrant_prior = ep, b_mode = b_mode,
                            ward_ratio = ward_ratio, shrink = shrink)
      sc <- score_chain(simulate_chain(prep, cfg, n_draws), bt$truth)
      mutate(sc$summary, b_mode = b_mode, ward_ratio = ward_ratio, shrink = shrink, .before = 1)
    })
  cur <- g$b_mode == "fitted" & g$ward_ratio & g$shrink == 1
  g |> mutate(current = cur,
              eligible = seat_crps <= seat_crps[cur] + 0.005 & pit_cov90 >= 0.80 & pit_cov90 <= 0.97,
              best = eligible & ward_brier == min(ward_brier[eligible]),
              adopt = best & !current & ward_brier < ward_brier[cur])
}

#' Which wards the model loses, and whether the simple rules win them
ward_diagnostics <- function(sc, naive_prev, naive_nat) {
  sc$wards |>
    left_join(rename(attr(naive_nat, "wards"), national_pick = pred), by = c("muni_code", "ward_id")) |>
    left_join(rename(attr(naive_prev, "wards"), previous_pick = pred), by = c("muni_code", "ward_id")) |>
    mutate(model_right = modal_correct, national_right = coalesce(national_pick == winner, FALSE),
           case = case_when(model_right & national_right ~ "both right",
                            !model_right & national_right ~ "model wrong, national vote right",
                            model_right & !national_right ~ "model right, national vote wrong",
                            TRUE ~ "both wrong"))
}

# 9. Premium basis (O10): pre-registered experiment (MODEL-LOG L044) ------------------------
#
# Written down before any result: 4 variants = premium basis {earlier national
# vote (current), national vote interpolated to the local election's date} x
# premium strength {halved (current), full}, at every other current setting.
# Rule: choose the lowest mean seat CRPS among variants whose established-party
# 90% coverage is in [0.80, 0.97] and whose ward Brier score is at most 0.005
# worse than the current model's; adopt it only if it beats the current model
# on seat CRPS. If none qualifies, nothing changes. For explanation, each
# variant also reports the DA's and ANC's 2021 provincial PR share, predicted
# against actual.

backtest_premium_grid <- function(bt, cfg, n_draws = 400) {
  actual <- bt$truth$pr |> summarise(votes = sum(votes), .by = party) |> mutate(share = votes / sum(votes))
  cur_shrink <- cfg$model$premium_shrink %||% 1
  runs <- tidyr::expand_grid(premium_basis = c("earlier", "interpolated"), shrink = sort(unique(c(cur_shrink, 1)))) |>
    pmap(\(premium_basis, shrink) {
      prep <- prepare_chain(bt$inputs, cfg, "fitted", shrink = shrink, premium_basis = premium_basis)
      ch <- simulate_chain(prep, cfg, n_draws)
      sc <- score_chain(ch, bt$truth)
      ps <- province_vote_share(ch$sims, prep$baseline)
      # named share_of, not pick: inside mutate() dplyr's own pick() would win
      share_of <- \(t, p) { i <- match(p, t$party); if (is.na(i)) NA_real_ else t$median[i] }
      list(summary = mutate(sc$summary, premium_basis = premium_basis, shrink = shrink,
             da_pred = share_of(ps, "DEMOCRATIC ALLIANCE"), da_actual = share_of(actual |> rename(median = share), "DEMOCRATIC ALLIANCE"),
             anc_pred = share_of(ps, "AFRICAN NATIONAL CONGRESS"), anc_actual = share_of(actual |> rename(median = share), "AFRICAN NATIONAL CONGRESS"),
             .before = 1),
           seats = select(sc$seats, muni_code, party, crps))
    })
  g <- bind_rows(map(runs, "summary"))
  cur <- g$premium_basis == (cfg$model$premium_basis %||% "earlier") & g$shrink == cur_shrink
  # Reported, not part of the rule (added in L045, after the rule had been
  # applied once): each variant's paired difference in seat CRPS from the
  # current model, with a 90% bootstrap interval over councils, as for the
  # newcomer test. The variants share random numbers, so the pairing is exact.
  ref <- runs[[which(cur)]]$seats
  g <- bind_cols(g, bind_rows(map(runs, \(r) {
    d <- inner_join(rename(ref, c0 = crps), rename(r$seats, c1 = crps), by = c("muni_code", "party")) |>
      summarise(diff = sum(c1 - c0), n = n(), .by = muni_code)
    b <- withr::with_seed(1, replicate(2000, { i <- sample.int(nrow(d), replace = TRUE); sum(d$diff[i]) / sum(d$n[i]) }))
    tibble(crps_diff = sum(d$diff) / sum(d$n), crps_diff_lo = unname(quantile(b, 0.05)), crps_diff_hi = unname(quantile(b, 0.95)))
  })))
  g |> mutate(current = cur,
              eligible = pit_cov90 >= 0.80 & pit_cov90 <= 0.97 & ward_brier <= ward_brier[cur] + 0.005,
              best = eligible & seat_crps == if (any(eligible)) min(seat_crps[eligible]) else -Inf,
              adopt = best & !current & seat_crps < seat_crps[cur])
}

# 10. Everything together -----------------------------------------------------------------

run_backtest <- function(bt, cfg, entrant_prior = NULL) {
  n <- cfg$backtest$n_draws %||% 1000
  # The headline is the model as configured, newcomers included when they are
  # on (O11, resolved in L048); the newcomer test keeps its own chain without.
  ep <- if (isTRUE(cfg$model$entrants)) entrant_prior else NULL
  prep_cfg <- prepare_chain(bt$inputs, cfg, "fitted", entrant_prior = ep)
  chain_cfg <- simulate_chain(prep_cfg, cfg, n)
  sc_cfg <- score_chain(chain_cfg, bt$truth)
  sds <- estimate_shock_sds(chain_cfg, bt$truth)
  chain_est <- simulate_chain(prep_cfg, cfg, n,
                              sd_override = list(province_party = sds$province, muni_party = sds$muni))
  sc_est <- score_chain(chain_est, bt$truth)
  np <- naive_rule(bt$inputs, bt$truth, "previous_local"); nn <- naive_rule(bt$inputs, bt$truth, "national_as_is")
  naive <- bind_rows(np, nn)
  wards_diag <- ward_diagnostics(sc_cfg, np, nn)
  model_rows <- bind_rows(
    mutate(sc_cfg$summary, rule = sprintf("model, config spreads (%.2f / %.2f)",
                                          cfg$model$sd$province_party, cfg$model$sd$muni_party)),
    mutate(sc_est$summary, rule = sprintf("model, estimated spreads (%.2f / %.2f)", sds$province, sds$muni))
  ) |> transmute(rule, seat_mae = seat_mae_median, control_correct = control_modal_correct,
                 ward_correct = ward_modal_correct)
  grid <- backtest_grid(bt, cfg, n_draws = cfg$backtest$grid_draws %||% 400)
  sc_without <- if (is.null(ep)) sc_cfg else score_chain(run_chain(bt$inputs, cfg, n), bt$truth)
  newcomers <- backtest_entrants(bt, cfg, entrant_prior, n, sc_without)
  ward_grid <- backtest_ward_grid(bt, cfg, entrant_prior, n_draws = cfg$backtest$grid_draws %||% 400)
  premium_grid <- backtest_premium_grid(bt, cfg, n_draws = cfg$backtest$grid_draws %||% 400)
  list(
    label = bt$label, grid = grid, newcomers = newcomers, ward_grid = ward_grid, wards = wards_diag,
    premium_grid = premium_grid,
    summary = bind_rows(mutate(sc_cfg$summary, spreads = "config"), mutate(sc_est$summary, spreads = "estimated")),
    versus_rules = bind_rows(model_rows, naive),
    shock_sds = tibble(parameter = c("province_party", "muni_party"),
                       config = c(cfg$model$sd$province_party, cfg$model$sd$muni_party),
                       estimated = c(sds$province, sds$muni),
                       based_on = c(sprintf("%d parties", sds$n_parties), sprintf("%d party x council cells", sds$n_cells))),
    party_surprise = sds$by_party,
    seats = sc_cfg$seats, seats_estimated = sc_est$seats, control = sc_cfg$control, blind = sc_cfg$blind,
    transfer = chain_cfg$transfer$coefs
  )
}

#' Demo mode has no earlier cycle, so the demo backtest is IN-SAMPLE (it
#' predicts the 2021 synthetic election from itself). It exercises the code
#' path only; its scores mean nothing.
assemble_demo_backtest <- function(d) {
  assemble_backtest_inputs(npe_prev = d$npe2019, lge_prev = d$lge2021, npe_latest = d$npe2019,
                           lge_truth = d$lge2021, councils = d$councils,
                           municipalities = d$municipalities, label = "demo (in-sample, code test only)")
}

#' Live backtest inputs: 2014 NPE (bulk), 2016 LGE, 2019 NPE, and the 2021
#' results with the IEC's official 2021 seat calculations as the seat truth.
assemble_live_backtest <- function(cfg, npe2014_file, lge2016_files, npe_files, lge2021, seat_calc_files, munis,
                                   lge2011_files = NULL) {
  prov_names <- unique(munis$province[munis$province_code %in% cfg$model$provinces])
  model_munis <- filter(munis, province_code %in% cfg$model$provinces)
  sc <- map(seat_calc_files, read_iec_seat_calc)
  councils21 <- map_dfr(sc, "meta") |> select(muni_code, total_seats) |>
    left_join(lge2021 |> filter(ballot == "Ward") |> distinct(muni_code, ward_id) |> count(muni_code, name = "n_wards"),
              by = "muni_code")
  seat_truth <- map_dfr(sc, \(x) mutate(x$parties, muni_code = x$meta$muni_code)) |>
    filter(!is.na(seats)) |> select(muni_code, party, seats)
  assemble_backtest_inputs(
    npe_prev = read_iec_npe_bulk(npe2014_file, "NPE2014", prov_names),
    lge_prev = map_dfr(lge2016_files, read_iec_lge_csv, election = "LGE2016"),
    npe_latest = read_iec_npe_portal(npe_files[str_detect(npe_files, "npe2019")], "NPE2019", prov_names),
    lge_truth = lge2021, councils = councils21, municipalities = model_munis,
    seat_truth = seat_truth, label = "2021 local election, predicted blind from 2014, 2016 and 2019",
    interp_frac_value = interp_frac(cfg$dates$npe2014, cfg$dates$lge2016, cfg$dates$npe2019)) |>
    c(list(lge2011 = if (length(lge2011_files)) map_dfr(lge2011_files, read_iec_lge_csv, election = "LGE2011") |>
                       filter(muni_code %in% model_munis$muni_code)))
}

#' Past newcomers: 2016's (needs 2011) and 2021's, from the backtest data.
#' The backtest may use only 2016's; the 2026 forecast pools both.
assemble_first_timers <- function(bt, lge2021) {
  i <- bt$inputs
  ft16 <- if (!is.null(bt$lge2011) && nrow(bt$lge2011)) first_timers(i$lge_prev, bt$lge2011, i$npe_prev, 2016)
  ft21 <- first_timers(filter(lge2021, muni_code %in% i$municipalities$muni_code), i$lge_prev, i$npe_latest, 2021)
  bind_rows(ft16, ft21)
}

write_backtest_outputs <- function(res) {
  dir.create(path_public(), showWarnings = FALSE, recursive = TRUE)
  tabs <- list(backtest_summary = mutate(res$summary, label = res$label), backtest_grid = res$grid,
               backtest_newcomers = res$newcomers, backtest_ward_grid = res$ward_grid,
               backtest_premium_grid = res$premium_grid,
               backtest_wards = res$wards,
               backtest_versus_rules = res$versus_rules, backtest_shock_sds = res$shock_sds,
               backtest_party_surprise = res$party_surprise, backtest_seats = res$seats,
               backtest_seats_estimated = res$seats_estimated, backtest_control = res$control,
               backtest_blind = res$blind, backtest_transfer = res$transfer)
  imap_chr(tabs, \(x, nm) { f <- path_public(paste0(nm, ".csv")); write_csv(x, f); f })
}
