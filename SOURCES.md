# Data sources

Where every input comes from, how it is fetched, and what is known to be wrong with it. **Verified** means our own code has fetched and parsed the file. **Inherited** means the recipe comes from the Johannesburg model's [SOURCES.md](https://github.com/Psi-am-i/jhb-election-model/blob/main/SOURCES.md) and [DATA-QUALITY.md](https://github.com/Psi-am-i/jhb-election-model/blob/main/DATA-QUALITY.md) and has not yet been exercised here. As of 2026-09-24 nothing is verified (MODEL-LOG O1).

We are indebted to that project's documentation, which saved days of discovery. It is licensed free to use with attribution and a link back: <https://joburg.whysoserious.city>.

| Input | Where | How | Status |
|---|---|---|---|
| 2021 local results, VD level, both ballots | `results.elections.org.za/home/LGEPublicReports/1091/Downloadable Party Results/{prov}/{muni}.csv` | `fetch_lge_reports()`; scripted download works for report files | Inherited |
| 2021 seat calculations (validation) | same pattern, `Seat Calculation Detail`, `.xls` | `fetch_lge_reports()` | Inherited |
| 2019 and 2024 national results, VD level (provincial ballot) | `.../NPEPublicReports/{827 or 1335}/Downloadable Results/Provincial.zip` | **manual browser download** into `data/raw/manual_downloads/`; `npe_zip()` stops with instructions if missing | Inherited |
| 2026 wards | MDB ArcGIS FeatureServer `MDBWards2026` | `fetch_mdb_layer()` | Inherited |
| 2026 voting districts, ward link, registration | FeatureServer `VotingDistricts2026_Final` (`WardNo`, `Split_VD`, `REGPOP`) | `fetch_mdb_layer()`, attributes only | Inherited |
| By-elections since 2021 | static JSON under `results.elections.org.za/dashboards/byelection/MapsJason/` | `fetch_byelections()`; collected, used from v0.2 | Inherited |
| Ward population by age, sex, population group (2020 wards) | Stats SA Ward-level Small Area Population Estimates 2022 | `fetch_statssa_ward_product()`; collected, used from v0.2 | Inherited |
| 2026 candidate lists | IEC certified candidate list PDFs, one per province | `candidate_pdf()`, with a manual fallback | URL pattern seen for the Eastern Cape only |
| 2026 council sizes | MEC section 12 notices, provincial gazettes | typed into `data-raw/manual/council_seats_2026.csv` | To do |

## Known problems in the published data

From the Johannesburg model's DATA-QUALITY record, handled in `R/ingest.R` and `R/utils.R`:

- `TotalValidVotes` in the local results files is the **party's** votes, not the VD total. We always recompute totals.
- Voting-station names in national exports are not quoted, so commas shift columns. We parse from both ends of each line.
- Encodings vary between UTF-8, CP850 and UTF-16 across years. We sniff each file.
- A missing IEC report returns an HTML page with HTTP 200. We check the payload.
- Election ids are opaque and differ per ballot; a wrong id returns a different election without error.
- The 2026 VD layer's `REGPOP` is a 2024 registration snapshot (A06).
- IEC province path codes are not the obvious ones: `WP` for the Western Cape.

## Provenance

Every downloaded file is recorded in `data/manifest.csv` with its URL, time, size and SHA-256. Raw data is not committed; the manifest is.
