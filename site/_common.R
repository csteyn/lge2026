# Shared by every page. Reads only committed outputs, never raw data.
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(purrr); library(ggplot2); library(forcats)
})

site_root <- if (file.exists("_quarto.yml")) "." else ".."
pub <- function(f) file.path(site_root, "..", "outputs", "public", f)
rd <- function(f) read_csv(pub(f), show_col_types = FALSE)

meta         <- rd("run_meta.csv")
municipalities <- rd("municipalities.csv")
seat_summary <- rd("seat_summary.csv")
seat_dist    <- rd("seat_dist.csv")
control      <- rd("control.csv")
coalitions   <- rd("coalitions.csv")
ward_probs   <- rd("ward_probs.csv") |> mutate(ward_id = as.character(ward_id))
council_meta <- rd("council_meta.csv")
wards_geo    <- sf::read_sf(pub("wards.geojson")) |> mutate(ward_id = as.character(ward_id))

pct <- function(x, digits = 0) paste0(formatC(100 * x, format = "f", digits = digits), "%")
pct_clip <- function(x) case_when(x < 0.01 ~ "<1%", x > 0.99 ~ ">99%", TRUE ~ pct(x))

# Party colours. Sourced colours come from parties.csv; any other party gets a
# fallback colour assigned ONCE over every party in the outputs (sorted by
# name), so a party keeps the same colour on every page.
party_table <- read_csv(file.path(site_root, "..", "data-raw", "manual", "parties.csv"),
                        comment = "#", show_col_types = FALSE)
fallback <- c("#3F7FA6", "#C07A1F", "#2E8B6E", "#8C5A9E", "#B5553A", "#6B8E23",
              "#C44E7A", "#4B5FA8", "#8D6E3F", "#3C9AA0", "#A0522D", "#5F7F3A")
all_parties <- sort(unique(c(seat_summary$party, ward_probs$party)))
known <- party_table |> filter(!is.na(colour))
global_colours <- setNames(rep(NA_character_, length(all_parties)), all_parties)
hit <- intersect(all_parties, known$party)
global_colours[hit] <- known$colour[match(hit, known$party)]
global_colours[is.na(global_colours)] <- rep(fallback, length.out = sum(is.na(global_colours)))
party_colours <- function(parties = all_parties) c(global_colours, `No majority` = "#C3CBC8")

knitr::opts_chunk$set(dev.args = list(bg = "#F3F5F4"))

theme_lge <- function() {
  theme_minimal(base_size = 12) +
    theme(
      text = element_text(colour = "#1E2B33"),
      plot.title.position = "plot",
      plot.background = element_rect(fill = "#F3F5F4", colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#CCD5D2", linewidth = 0.3),
      axis.title = element_text(size = 10, colour = "#4A5A63"),
      legend.position = "top", legend.justification = "left",
      strip.text = element_text(face = "bold", hjust = 0)
    )
}
