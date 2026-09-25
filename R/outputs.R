# outputs.R -------------------------------------------------------------------
# Everything the website shows is written here as plain CSV / GeoJSON under
# outputs/public/ (committed, CC BY 4.0) and copied to outputs/archive/<run>/,
# so any past forecast can be reproduced and compared with the current one.

write_public_outputs <- function(summaries, checks, inputs, cfg, transfer, seat_validation = NULL,
                                 party_status = NULL, premium = NULL, premium_estimate = NULL) {
  dir.create(path_public(), showWarnings = FALSE, recursive = TRUE)
  id <- run_id()
  meta <- tibble(
    run_id = id, run_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), git_sha = git_sha(),
    mode = cfg$mode, n_draws = cfg$model$n_draws, seed = cfg$model$seed,
    sd_ward_used = cfg$model$sd$ward_party %||% transfer$sd_ward,
    sd_vd_used = cfg$model$sd$vd_party %||% transfer$sd_vd,
    model_version = cfg$model$version
  )
  munis <- inputs$municipalities |> semi_join(summaries$meta, by = "muni_code") |>
    left_join(select(inputs$councils, muni_code, seats_source), by = "muni_code")

  tables <- list(
    run_meta = meta, municipalities = munis,
    seat_summary = summaries$seat_summary, seat_dist = summaries$seat_dist,
    control = summaries$control, coalitions = summaries$coalitions,
    ward_probs = summaries$ward_probs, vote_share = summaries$vote_share,
    council_meta = summaries$meta, checks = checks, transfer = transfer$coefs,
    assumptions = read_csv("data-raw/manual/assumptions.csv", show_col_types = FALSE)
  )
  if (!is.null(seat_validation)) tables$seat_validation <- seat_validation
  if (!is.null(party_status)) tables$party_status <- party_status
  if (!is.null(premium)) tables$local_premium <- premium
  if (!is.null(premium_estimate) && nrow(premium_estimate$ward_residuals)) {
    tables$byelection_residuals <- premium_estimate$ward_residuals
    tables$byelection_placebo <- premium_estimate$placebo
  }
  files <- imap_chr(tables, \(x, nm) { f <- path_public(paste0(nm, ".csv")); write_csv(x, f); f })

  geo <- path_public("wards.geojson")
  if (file.exists(geo)) file.remove(geo)
  inputs$wards_geo |> semi_join(summaries$meta, by = "muni_code") |>
    sf::st_write(geo, quiet = TRUE)

  # forecast history: one row per run x municipality x outcome
  hist_f <- path_public("history.csv")
  summaries$control |> mutate(run_id = id, run_utc = meta$run_utc, mode = cfg$mode, .before = 1) |>
    write_csv(hist_f, append = file.exists(hist_f))

  arch <- file.path("outputs", "archive", id)
  dir.create(arch, recursive = TRUE, showWarnings = FALSE)
  file.copy(c(files, geo), arch)
  c(files, geo, hist_f)
}
