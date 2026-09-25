# site.R ------------------------------------------------------------------------
# Prepare the Quarto site from committed outputs. Runs in the pipeline AND in
# CI before `quarto render`, so the site never depends on raw data.
#
#  * copies the project's markdown records (methodology, log, errata, ...)
#    into site/ as pages, so there is ONE source of truth for each
#  * generates one page per published municipality from a shared partial
#  * writes the run ledger shown at the top of every page

sync_site <- function(site_dir = "site") {
  pages <- c(METHODOLOGY = "methodology", `MODEL-LOG` = "log", ERRATA = "corrections",
             SCORING = "scoring", SOURCES = "sources")
  titles <- c(methodology = "How the model works", log = "Model log",
              corrections = "Corrections", scoring = "How this forecast will be scored",
              sources = "Data sources")
  for (src in names(pages)) {
    md <- read_lines(paste0(src, ".md"))
    md <- md[-which(str_detect(md, "^# "))[1]] # title comes from front matter
    # links between the records point at .md files in the repo, .qmd pages on the site
    for (k in names(pages)) md <- str_replace_all(md, fixed(paste0("(", k, ".md)")), paste0("(", pages[[k]], ".qmd)"))
    write_lines(c("---", sprintf('title: "%s"', titles[[pages[[src]]]]), "---", "",
                  "{{< include _ledger.md >}}", "", md),
                file.path(site_dir, paste0(pages[[src]], ".qmd")))
  }

  munis <- read_csv(path_public("municipalities.csv"), show_col_types = FALSE)
  # Git does not store empty folders, so site/m/ may not exist in a fresh
  # checkout (MODEL-LOG L008). Create it rather than assume it.
  dir.create(file.path(site_dir, "m"), showWarnings = FALSE, recursive = TRUE)
  unlink(list.files(file.path(site_dir, "m"), pattern = "\\.qmd$", full.names = TRUE))
  for (i in seq_len(nrow(munis))) {
    write_lines(c(
      "---", sprintf('title: "%s"', munis$muni_name[i]), "---", "",
      "{{< include ../_ledger.md >}}", "",
      "```{r}", sprintf('muni <- "%s"', munis$muni_code[i]), "```", "",
      "{{< include ../_partials/_municipality.qmd >}}"
    ), file.path(site_dir, "m", paste0(munis$muni_code[i], ".qmd")))
  }

  write_ledger(site_dir)
  invisible(munis$muni_code)
}

write_ledger <- function(site_dir) {
  meta <- read_csv(path_public("run_meta.csv"), show_col_types = FALSE)
  checks <- read_csv(path_public("checks.csv"), show_col_types = FALSE)
  assumptions <- read_csv(path_public("assumptions.csv"), show_col_types = FALSE)
  n_corr <- sum(str_detect(read_lines("ERRATA.md"), "^## E\\d+"))
  n_open <- sum(assumptions$status == "open")
  n_flag <- sum(checks$status != "pass")
  plural <- function(n, word) sprintf("%d %s%s", n, word, if (n == 1) "" else "s")
  when <- format(as.POSIXct(meta$run_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%d %B %Y, %H:%M UTC")

  # Markdown, not raw HTML, so Quarto rewrites the root-relative links for
  # pages at any depth (site/ and site/m/).
  banner <- if (meta$mode == "demo") c(
    "::: {.demo-banner}",
    "**Synthetic demo.** Fictional parties and places, generated to test the pipeline. Nothing on this site is a forecast.",
    ":::", "") else character()
  write_lines(c(
    banner,
    "::: {.ledger}",
    sprintf("[Run %s]{.ledger-item} [Model %s, commit `%s`]{.ledger-item} [%s](/checks.qmd){.ledger-item} [%s](/assumptions.qmd){.ledger-item} [%s](/corrections.qmd){.ledger-item}",
            when, meta$model_version, meta$git_sha, plural(n_flag, "check flagged") |> str_replace("check flaggeds", "checks flagged"),
            plural(n_open, "open assumption"), plural(n_corr, "correction")),
    ":::"
  ), file.path(site_dir, "_ledger.md"))
}
