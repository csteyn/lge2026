# _targets.R --------------------------------------------------------------------
# Run with targets::tar_make(); inspect with targets::tar_visnetwork().
# Then render the site with:  quarto render site
#
# mode: demo -> synthetic inputs; mode: live -> fetch/assemble real inputs.
# Both produce the same `inputs` contract (R/assemble.R), so every model and
# publishing target is shared and the demo exercises the real code path.

library(targets)
library(tarchetypes)
# Loaded here as well as in tar_option_set(): R/demo.R builds tibbles at
# source time, before targets attaches the packages for each target.
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(stringr); library(tibble)
})

tar_option_set(
  packages = c("dplyr", "tidyr", "purrr", "readr", "stringr", "tibble", "sf"),
  seed = 20261104,
  # Drop each result from memory once downstream targets have it, instead of
  # holding the whole pipeline in RAM (MODEL-LOG L014: the worker process was
  # killed after parsing the national files).
  memory = "transient"
)
tar_source("R")

mode <- yaml::read_yaml("config.yml")$mode

input_targets <- if (mode == "demo") {
  list(tar_target(inputs, make_demo_data(cfg$model$seed)),
       tar_target(seat_validation, NULL),
       tar_target(byelections, NULL),
       tar_target(backtest_inputs, assemble_demo_backtest(inputs)),
       tar_target(first_timer_data, make_demo_first_timers()))
} else {
  list(
    tar_target(munis_csv, "data-raw/manual/municipalities.csv", format = "file"),
    tar_target(munis_seed, read_csv(munis_csv, comment = "#", show_col_types = FALSE)),
    tar_target(lge_fetch, fetch_lge_reports(filter(munis_seed, province_code %in% cfg$collect$provinces),
                                            cfg, years = "2021")),
    tar_target(lge_files, lge_fetch |> filter(ok, str_detect(dest, "downloadable_party_results")) |>
                 pull(dest), format = "file"),
    tar_target(seat_calc_files, lge_fetch |> filter(ok, str_detect(dest, "seat_calculation_detail")) |>
                 pull(dest), format = "file"),
    # Does our allocator reproduce every published 2021 council? (check C14)
    tar_target(seat_validation, validate_seat_allocator(seat_calc_files)),
    tar_target(npe_files, c(npe_zip(2019, cfg), npe_zip(2024, cfg)), format = "file"),
    tar_target(wards_file, fetch_mdb_layer(cfg$mdb$layers$wards2026, path_raw("mdb", "wards2026.geojson"),
                                           cfg, max_offset = 0.0003), format = "file"),
    tar_target(vds_file, fetch_mdb_layer(cfg$mdb$layers$vds2026, path_raw("mdb", "vds2026.geojson"),
                                         cfg, geometry = FALSE), format = "file"),
    tar_target(candidate_files, unlist(map(cfg$publish$provinces, \(p)
      tryCatch(candidate_pdf(p), error = function(e) { message(conditionMessage(e)); NULL })))),
    # Collected now, used from v0.2. error = "continue": a failure here is
    # recorded in tar_meta() but cannot stop the forecast.
    tar_target(byelections, fetch_byelections(cfg), error = "continue"),
    tar_target(statssa, fetch_statssa_ward_product(), error = "continue"),
    # --- backtest data (L026). error = "continue": missing backtest data must
    # never stop the forecast; the backtest page then says what is missing.
    tar_target(lge2016_fetch, fetch_lge_reports(filter(munis_seed, province_code %in% cfg$model$provinces),
                                                cfg, years = "2016", reports = cfg$iec$lge_reports[1]),
               error = "continue"),
    tar_target(lge2016_files, lge2016_fetch |> filter(ok, str_detect(dest, "downloadable_party_results")) |>
                 pull(dest), format = "file", error = "continue"),
    tar_target(npe2014_file, npe2014_zip(), format = "file", error = "continue"),
    # 2011 results identify 2016's newcomers, the out-of-sample prior for the
    # newcomer test (L035). Optional: without them the test reports it could not run.
    tar_target(lge2011_fetch, fetch_lge_reports(filter(munis_seed, province_code %in% cfg$model$provinces),
                                                cfg, years = "2011", reports = cfg$iec$lge_reports[1])),
    tar_target(lge2011_files, lge2011_fetch |> filter(ok, str_detect(dest, "downloadable_party_results")) |>
                 pull(dest)),
    tar_target(backtest_inputs, assemble_live_backtest(cfg, npe2014_file, lge2016_files, npe_files,
                                                       inputs$lge2021, seat_calc_files, munis_seed, lge2011_files),
               error = "continue"),
    tar_target(first_timer_data, assemble_first_timers(backtest_inputs, inputs$lge2021), error = "continue"),
    tar_target(inputs, assemble_live_inputs(cfg, lge_files, npe_files, wards_file, vds_file, candidate_files,
                                            party_status))
  )
}

list(
  tar_target(config_file, "config.yml", format = "file"),
  tar_target(cfg, read_config(config_file)),
  # human-confirmed 2026 participation (absent / renamed / disputed), check C15
  tar_target(party_status_file, "data-raw/manual/party_status_2026.csv", format = "file"),
  tar_target(party_status, read_party_status(party_status_file)),
  input_targets,

  # --- scope and model -----------------------------------------------------------
  tar_target(model_munis, if (cfg$mode == "demo") inputs$municipalities else
    filter(inputs$municipalities, province_code %in% cfg$model$provinces)),
  tar_target(scoped, scope_inputs(inputs, model_munis)),
  tar_target(groups, define_party_groups(scoped$lge2021, scoped$npe2024, cfg, scoped$pr_lists)),
  tar_target(other_w, other_composition(groups, scoped$lge2021, scoped$npe2024)),
  tar_target(transfer, {
    interp <- identical(cfg$model$premium_basis, "interpolated")   # L044
    apply_transfer_variant(
      fit_transfer(scoped$npe2019, scoped$lge2021, groups, cfg, npe_next = if (interp) scoped$npe2024,
                   frac = if (interp) interp_frac(cfg$dates$npe2019, cfg$dates$lge2021, cfg$dates$npe2024)),
      cfg$model$transfer_b %||% "fitted", cfg$model$premium_shrink %||% 1)
  }),
  tar_target(contests_g, if (is.null(scoped$contests)) NULL else scoped$contests |>
               inner_join(select(groups, muni_code, party, group), by = c("muni_code", "party")) |>
               distinct(muni_code, ward_id, group)),
  tar_target(baseline, build_baseline(scoped$npe2024, scoped$lge2021, scoped$vd2026, groups, transfer, contests_g,
                                      ward_ratio = cfg$model$ward_ratio %||% TRUE)),
  tar_target(swing_file, "data-raw/manual/swing_priors.csv", format = "file"),
  tar_target(swing, read_csv(swing_file, comment = "#", show_col_types = FALSE, col_types = "ccddcc")),
  # parties without a fitted transfer: local premium as a distribution (L022)
  tar_target(abbrev_file, "data-raw/manual/byelection_abbreviations.csv", format = "file"),
  tar_target(premium_estimate, local_premium_with_evidence(
    groups, transfer, byelections,
    read_csv(abbrev_file, comment = "#", show_col_types = FALSE),
    scoped$npe2024, baseline, model_munis$muni_code, cfg)),
  tar_target(premium, premium_estimate$premium),
  tar_target(prov_effects, draw_province_effects(sort(unique(baseline$group)), swing, cfg, premium)),

  # newcomers (L035): prior from past newcomers; used in the forecast only
  # when config model.entrants is true (set by the pre-registered test)
  tar_target(entrant_prior, estimate_entrant_model(first_timer_data)),
  tar_target(backtest_entrant_prior, {
    ft <- filter(first_timer_data, year == 2016)
    if (nrow(ft) >= 20) estimate_entrant_model(ft) else NULL
  }),
  tar_target(entrant_overrides_file, "data-raw/manual/entrant_priors.csv", format = "file"),
  tar_target(entrant_overrides, read_entrant_overrides(entrant_overrides_file)),
  # Newcomers are always drawn, as a diagnostic: check C20 must be able to test
  # a model that is switched off. The forecast uses them only when
  # model.entrants is true (L044). Council sizes come from the baseline's
  # registration (imputed where the MDB field is empty, C12).
  tar_target(entrants_all, find_entrants(scoped$pr_lists, scoped$contests, groups, baseline)),
  tar_target(entrant_stability, entrant_model_stability(first_timer_data)),
  tar_target(entrant_draws_all, draw_entrant_shares(entrants_all, entrant_prior, cfg$model$n_draws, cfg$model$seed + 7,
                                                    entrant_overrides)),
  tar_target(entrant_draws, if (isTRUE(cfg$model$entrants)) entrant_draws_all else list()),
  tar_target(entrant_table, describe_entrants(entrants_all, entrant_prior, entrant_draws_all, entrant_overrides) |>
               mutate(used_in_forecast = isTRUE(cfg$model$entrants))),

  # one branch per municipality
  tar_group_by(baseline_m, baseline, muni_code),
  tar_target(sims, simulate_municipality(
    baseline_m, filter(scoped$councils, muni_code == baseline_m$muni_code[1]),
    prov_effects, other_w, swing, transfer, cfg,
    entrant_shares = entrant_draws[[baseline_m$muni_code[1]]], entrant_contests = scoped$contests),
    pattern = map(baseline_m), iteration = "list"),
  tar_target(summaries, bind_summaries(map(sims, summarise_simulation, cfg = cfg))),

  # polls: a published cross-check, not an input (L041)
  tar_target(polls_file, "data-raw/manual/polls.csv", format = "file"),
  tar_target(polls, read_polls(polls_file)),
  tar_target(province_share, province_vote_share(sims, baseline)),
  tar_target(poll_comparison, compare_polls(polls, province_share, summaries$vote_share)),
  # from the 2024 result to the forecast, step by step: a diagnostic (L046, O10)
  tar_target(share_decomposition, decompose_shares(scoped$npe2024, groups, baseline, premium, swing,
                                                   entrant_draws, province_share, summaries$vote_share)),

  # --- checks and publication ------------------------------------------------------
  tar_target(checks, run_checks(scoped, groups, transfer, baseline, summaries, cfg, seat_validation,
                                  party_status, premium, entrant_table, poll_comparison,
                                  entrant_total_check(entrant_draws_all, first_timer_data), entrant_stability)),
  tar_target(assumptions_file, "data-raw/manual/assumptions.csv", format = "file"),
  tar_target(errata_file, "ERRATA.md", format = "file"),
  tar_target(public, {
    assumptions_file
    write_public_outputs(summaries, checks, scoped, cfg, transfer, seat_validation, party_status, premium,
                         premium_estimate, entrant_table, province_share, poll_comparison, share_decomposition)
  }, format = "file"),
  # --- backtest: predict 2021 blind, score it, estimate the A02 spreads (L026)
  tar_target(backtest, run_backtest(backtest_inputs, cfg, backtest_entrant_prior), error = "continue"),
  tar_target(backtest_public, write_backtest_outputs(backtest), format = "file", error = "continue"),
  # --- L047: noise centring, a pre-registered experiment, and a closure test
  tar_target(noise_grid, backtest_noise_grid(backtest_inputs, cfg, backtest_entrant_prior,
                                             cfg$backtest$grid_draws %||% 400), error = "continue"),
  tar_target(closure, {
    interp <- identical(cfg$model$premium_basis, "interpolated")
    bi <- backtest_inputs$inputs
    closure_test(list(
      closure_cycle(bi$npe_prev, bi$lge_prev, define_party_groups(bi$lge_prev, bi$npe_latest, cfg, bi$pr_lists), cfg,
                    "2014 national to 2016 local", npe_next = if (interp) bi$npe_latest, frac = if (interp) bi$interp_frac),
      closure_cycle(scoped$npe2019, scoped$lge2021, groups, cfg, "2019 national to 2021 local",
                    npe_next = if (interp) scoped$npe2024,
                    frac = if (interp) interp_frac(cfg$dates$npe2019, cfg$dates$lge2021, cfg$dates$npe2024))))
  }, error = "continue"),
  tar_target(noise_verdict, noise_decision(noise_grid, closure, cfg), error = "continue"),
  tar_target(noise_public, write_noise_outputs(noise_grid, closure, noise_verdict), format = "file", error = "continue"),

  tar_target(site_pages, {
    public; errata_file
    sync_site("site")
  })
)
