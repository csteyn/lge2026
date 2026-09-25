# demo.R ------------------------------------------------------------------------
# Synthetic elections for fictional municipalities and fictional parties.
#
# Purpose: exercise every stage of the pipeline and the website before real
# data is held, and give the tests a realistic target. Party and place names
# are deliberately fictional so that no demo output can be mistaken for a
# forecast about real parties. Every page rendered in demo mode carries a
# banner saying so.
#
# The generator has a known "truth" for the NPE -> LGE transfer
# (DEMO_TRUTH below), so fit_transfer() can be checked for recovery.

DEMO_TRUTH <- tibble(
  group = c("ALPHA PARTY", "BRAVO PARTY", "CHARLIE ALLIANCE", "DELTA FRONT", "ECHO MOVEMENT", "OTHER"),
  a = c(0.10, -0.15, 0.20, -0.05, 0.00, 0.00),
  b = c(1.00, 0.95, 1.05, 0.90, 1.00, 0.90)
)

demo_municipalities <- tibble(
  province = "Demo Province",
  muni_code = c("DEM1", "DEM2", "DEM3"),
  muni_name = c("Demo Metro", "Karoo Demo", "Coastal Demo"),
  n_wards = c(30L, 7L, 12L),
  vds_per_ward = c(3L, 3L, 3L),
  total_seats = c(59L, 13L, 23L),
  # municipality-level party means on the log scale
  mu = list(
    c(`ALPHA PARTY` = 1.6, `BRAVO PARTY` = 0.9, `CHARLIE ALLIANCE` = 0.2, `DELTA FRONT` = -0.4, `ECHO MOVEMENT` = -0.6, OTHER = -0.3),
    c(`ALPHA PARTY` = 0.8, `BRAVO PARTY` = 0.9, `CHARLIE ALLIANCE` = 0.7, `DELTA FRONT` = -0.8, `ECHO MOVEMENT` = -1.0, OTHER = -0.5),
    c(`ALPHA PARTY` = 1.1, `BRAVO PARTY` = 0.8, `CHARLIE ALLIANCE` = 0.1, `DELTA FRONT` = -0.2, `ECHO MOVEMENT` = -1.2, OTHER = -0.4)
  )
)

demo_other_parties <- c("HOTEL PARTY", "INDIA PARTY", "JULIET PARTY")

make_demo_data <- function(seed = 1104) {
  withr::with_seed(seed, {
    vds <- demo_municipalities |>
      select(province, muni_code, n_wards, vds_per_ward, mu) |>
      mutate(ward = map(n_wards, seq_len)) |>
      unnest(ward) |>
      mutate(ward_id = sprintf("%s%05d", c(DEM1 = "991", DEM2 = "992", DEM3 = "993")[muni_code], ward)) |>
      uncount(vds_per_ward, .id = "k") |>
      mutate(vd = sprintf("9%07d", row_number()),
             registered = round(runif(n(), 600, 2800)))

    parties <- DEMO_TRUTH$group
    ward_fx <- vds |> distinct(muni_code, ward_id) |>
      mutate(fx = map(ward_id, \(w) setNames(rnorm(length(parties), 0, 0.45), parties)))
    lat <- vds |>
      left_join(ward_fx, by = c("muni_code", "ward_id")) |>
      mutate(eta = pmap(list(mu, fx), \(m, f) m + f + rnorm(length(parties), 0, 0.25)))

    shares_from <- function(eta_list) map(eta_list, \(e) exp(e) / sum(exp(e)))
    clr <- function(e) e - mean(e)

    votes_long <- function(df, share_col, turnout_mean, election, ballot = "PR") {
      df |>
        mutate(turnout = plogis(qlogis(turnout_mean) + rnorm(n(), 0, 0.3)),
               valid = round(registered * turnout * 0.985),
               spoilt = round(registered * turnout * 0.015)) |>
        select(province, muni_code, ward_id, vd, registered, spoilt, valid, share = all_of(share_col)) |>
        mutate(share = map(share, \(s) enframe(s, "party", "p"))) |>
        unnest(share) |>
        mutate(votes = as.integer(rmultinom(1, valid[1], p)[, 1]), .by = vd) |>
        mutate(election = election, ballot = ballot) |>
        split_other() |>
        mutate(vd_valid = sum(votes), .by = c(vd, ballot)) |>
        select(election, province, muni_code, ward_id, vd, ballot, registered, spoilt, party, votes, vd_valid)
    }

    # 2019 NPE
    lat <- lat |> mutate(s19 = shares_from(eta))
    npe2019 <- votes_long(lat, "s19", 0.62, "NPE2019")

    # 2021 LGE: known transfer, plus a local-only party in DEM2 and DEM3,
    # plus ward-ballot ticket splitting
    lat <- lat |>
      mutate(eta21 = map(eta, \(e) {
        z <- DEMO_TRUTH$a + DEMO_TRUTH$b * clr(e) + rnorm(length(e), 0, 0.15)
        setNames(z, parties)
      }),
      local = if_else(muni_code %in% c("DEM2", "DEM3"), runif(n(), 0.05, 0.25), 0),
      s21 = map2(eta21, local, \(e, l) {
        p <- exp(e) / sum(exp(e)) * (1 - l)
        if (l > 0) c(p, `FOXTROT CIVIC` = l) else p
      }),
      s21w = map(s21, \(s) { r <- s * c(1.02, 1.05, 0.93, 0.9, 1, 0.9, 1.1)[seq_along(s)]; r / sum(r) }))
    lge2021 <- bind_rows(
      votes_long(lat, "s21", 0.46, "LGE2021", "PR"),
      votes_long(lat, "s21w", 0.46, "LGE2021", "Ward")
    ) |> mutate(registered = registered)

    # 2024 NPE: a national swing and a new party (GOLF CONGRESS)
    swing <- c(`ALPHA PARTY` = 0.05, `BRAVO PARTY` = -0.25, `CHARLIE ALLIANCE` = 0.15,
               `DELTA FRONT` = -0.10, `ECHO MOVEMENT` = 0, OTHER = 0)
    lat <- lat |> mutate(s24 = map(eta, \(e) {
      z <- e + swing + rnorm(length(e), 0, 0.1)
      p <- exp(z) / sum(exp(z))
      g <- runif(1, 0.01, 0.06)
      c(p * (1 - g), `GOLF CONGRESS` = g)
    }))
    npe2024 <- votes_long(lat, "s24", 0.58, "NPE2024")

    # 2026 voting districts: existing ones plus ~6% new (split) districts
    vd2026 <- vds |> select(muni_code, ward_id, vd, registered) |>
      mutate(registered = round(registered * runif(n(), 1.0, 1.08)))
    splits <- vd2026 |> slice_sample(prop = 0.06) |>
      mutate(vd = sprintf("8%07d", row_number()), registered = round(registered * 0.4))
    vd2026 <- bind_rows(vd2026, splits)
  })

  councils <- demo_municipalities |>
    transmute(province, muni_code, muni_name, total_seats, n_wards, seats_source = "synthetic")

  contests <- vd2026 |> distinct(muni_code, ward_id) |>
    cross_join(tibble(party = c(DEMO_TRUTH$group[-6], "GOLF CONGRESS", "FOXTROT CIVIC"))) |>
    filter(!(party == "FOXTROT CIVIC" & muni_code == "DEM1"),
           !(party == "GOLF CONGRESS" & muni_code == "DEM2"))
  pr_lists <- contests |> distinct(muni_code, party) |>
    bind_rows(expand_grid(muni_code = demo_municipalities$muni_code, party = demo_other_parties))

  list(npe2019 = npe2019, lge2021 = lge2021, npe2024 = npe2024, vd2026 = vd2026,
       councils = councils, contests = contests, pr_lists = pr_lists,
       municipalities = select(demo_municipalities, province, muni_code, muni_name),
       wards_geo = demo_ward_geometry(distinct(vd2026, muni_code, ward_id)))
}

# "OTHER" in the latent model is really three tiny parties
split_other <- function(df) {
  other <- df |> filter(party == "OTHER")
  if (!nrow(other)) return(df)
  w <- c(0.5, 0.3, 0.2)
  pieces <- other |>
    mutate(split = map(votes, \(v) tibble(party = demo_other_parties,
                                          votes = as.integer(rmultinom(1, v, w)[, 1])))) |>
    select(-party, -votes) |>
    unnest(split)
  bind_rows(filter(df, party != "OTHER"), pieces)
}

# Square wards on a grid, one block per municipality, in a fictional CRS
demo_ward_geometry <- function(wards) {
  wards |>
    arrange(muni_code, ward_id) |>
    mutate(i = row_number() - 1, .by = muni_code) |>
    mutate(ncol = ceiling(sqrt(n())), .by = muni_code) |>
    mutate(x0 = (i %% ncol) + c(DEM1 = 0, DEM2 = 8, DEM3 = 13)[muni_code],
           y0 = -(i %/% ncol)) |>
    mutate(geometry = sf::st_sfc(map2(x0, y0, \(x, y) sf::st_polygon(list(matrix(
      c(x, y, x + 0.95, y, x + 0.95, y + 0.95, x, y + 0.95, x, y), ncol = 2, byrow = TRUE)))))) |>
    select(muni_code, ward_id, geometry) |>
    sf::st_as_sf()
}
