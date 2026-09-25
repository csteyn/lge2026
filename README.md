# lge2026: an open forecast of South Africa's 2026 municipal elections

A voting-district-level, probabilistic model of council seats for the 4 November 2026 local government elections, built in R (tidyverse, `targets`, Quarto). Data collection is national; the first published site covers the 25 Western Cape councils.

The approach follows the open [Johannesburg model](https://joburg.whysoserious.city) and shares its principles: every input is traceable, every assumption is listed, every error is logged, and the site shows the current count of all three on every page.

**Status: v0.1.** The pipeline has run end to end on real Western Cape data (24 September 2026). The seat allocator reproduces the IEC's published 2021 councils exactly. Not yet ready to publish: see the open items in MODEL-LOG.

## Quick start

```r
install.packages(c("tidyverse", "targets", "tarchetypes", "sf", "httr2", "jsonlite",
                   "digest", "pdftools", "readxl", "yaml", "withr", "testthat",
                   "knitr", "rmarkdown", "renv"))
renv::init()                       # then renv::snapshot() to lock versions
testthat::test_dir("tests/testthat")
targets::tar_make()                # config.yml has mode: demo
```
```sh
quarto render site                 # site/_site/index.html
```

Keep the project in an ordinary local folder rather than a cloud-synced one (Google Drive, OneDrive, Dropbox). The pipeline writes many small files into `_targets/`, and Git keeps its history in `.git/`; sync clients can lock or duplicate files mid-write, which corrupts both in ways that are hard to diagnose. Push to GitHub for backup instead.

## Going live

1. In `config.yml` set `mode: live`.
2. Download the two national bulk exports in a browser (the IEC portal refuses scripts) into `data/raw/manual_downloads/` as `npe2019_provincial.zip` and `npe2024_provincial.zip`. The pipeline stops with the exact URLs if they are missing.
3. Fill in `data-raw/manual/council_seats_2026.csv` from the MEC section 12 notices. Until you do, check C07 fails and the site says so.
4. `targets::tar_make()`. Read `data/fetch_log.csv` and the checks page before believing anything.
5. For the backtest (optional; the forecast runs without it): download the 2014 national results in a browser from the IEC's main site and save them as `data/raw/manual_downloads/npe2014_bulk.zip`. The pipeline stops that step with the exact address if the file is missing, and the 2016 local results download automatically.
6. To collect beyond the Western Cape, extend `data-raw/manual/municipalities.csv` (`seed_municipality_list()` writes every municipality code from the MDB ward layer as a template).

## Layout

| Path | What |
|---|---|
| `_targets.R` | the pipeline; `tar_visnetwork()` draws it |
| `R/fetch.R`, `R/ingest.R`, `R/assemble.R` | collect, parse, and assemble inputs into one contract |
| `R/model.R`, `R/seats.R`, `R/coalitions.R` | transfer, baseline, simulation, Schedule 1, coalition arithmetic |
| `R/checks.R`, `R/outputs.R`, `R/site.R` | checks as data, public outputs and archive, site preparation |
| `R/demo.R` | synthetic data with a known truth |
| `data-raw/manual/` | human-entered reference data, each row with a source |
| `outputs/public/` | everything the site shows (committed, CC BY 4.0) |
| `site/` | the Quarto website |
| `METHODOLOGY.md`, `MODEL-LOG.md`, `ERRATA.md`, `SCORING.md`, `SOURCES.md` | the record, also published as site pages |

## Repository and publication

The repository is public: code, model log, assumptions, checks and the scoring commitment. The forecast outputs (`outputs/public/`, `outputs/archive/`) are kept out of Git, and the site workflow runs only when started by hand, until publication is decided (MODEL-LOG L029). The scoring commitment is frozen by the tag `prereg-2026` and a GitHub Release made from it.

## Contributing

Found an error? Open an issue with the page, the number, and what you think it should be. Confirmed errors in published forecasts are listed in ERRATA with their effect; errors caught earlier go in MODEL-LOG.

## Licence

Code MIT; forecasts, outputs and text CC BY 4.0 (see `LICENSE.md`). Raw source data belongs to its publishers and is not redistributed.
