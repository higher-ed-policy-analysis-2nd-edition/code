#=========================================================================
# Maps13.R
# Section 13.3: Choropleth Maps
# R translation of Maps13.do
#
# Package substitutions:
#   Stata maptile/spmap (+ statastates) -> maps::map("state") converted
#   to sf via sf::st_as_sf(), joined on state name, plotted with
#   ggplot2::geom_sf(). All CRAN-standard, apt-available packages.
#
# KNOWN LIMITATION (flag for print review): base R's `maps` state
# database covers the lower 48 + DC only -- Alaska and Hawaii are not
# included as insets the way Stata's maptile renders them. If AK/HI
# need to appear in the printed figure, this requires a cartogram-style
# package (e.g. tigris + manual repositioning) that is CRAN-only and not
# installable in this validation sandbox; flagged here rather than
# silently dropped.
#=========================================================================

if (!exists("root_dir")) {
  # Mirrors Stata_code13.do's own username-conditional path logic
  # (`if c(username) == "marvi"`) -- defaults to the real Chapter 13
  # folder on the author's machine regardless of R's current working
  # directory, and falls back to getwd() for anyone else running this
  # standalone.
  if (Sys.getenv("USERNAME") == "marvi") {
    root_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 13"
    data_dir <- file.path(root_dir, "Data", "Stata")  # matches Stata's global data_dir
  } else {
    root_dir <- getwd()
    data_dir <- file.path(root_dir, "Data")
  }
  graphs_dir <- file.path(root_dir, "Output", "figures")
  tables_dir <- file.path(root_dir, "Output", "tables")
  logdir     <- file.path(root_dir, "Output", "logs")
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(maps)
})

# Local-file-with-GitHub-fallback loader -- if the .dta isn't found
# locally (e.g. a cloud-sync placeholder that hasn't downloaded yet),
# pull it directly from the chapter's public data repo instead of
# hanging on an unresolved local path.
load_ch13_dta <- function(filename, data_dir) {
  local_path <- file.path(data_dir, filename)
  if (!file.exists(local_path)) {
    url <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch13/", filename)
    cat("Local file not found -- downloading from GitHub:", url, "\n")
    dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
    download.file(url, local_path, mode = "wb", quiet = TRUE)
  }
  # IMPORTANT: copy to a plain local temp folder before reading. haven's
  # underlying ReadStat parser can become extremely slow (sometimes
  # appearing to hang indefinitely) when reading directly from a
  # Dropbox/OneDrive-synced path on Windows, even once the file is fully
  # downloaded -- the sync client's filesystem driver intercepts each of
  # ReadStat's many small low-level reads. A plain file.exists()/
  # file.size() check bypasses this (metadata-only), which is why those
  # returned instantly while read_dta() itself did not. Copying first
  # avoids the synced path entirely for the actual parse.
  tmp_path <- file.path(tempdir(), filename)
  file.copy(local_path, tmp_path, overwrite = TRUE)
  haven::read_dta(tmp_path)
}


cat("Maps13.R running:", format(Sys.time()), "\n")

pubexport <- function(p, gname, w = 9, h = 6) {
  # NOTE: wider default than the chapter's other pubexport() copies
  # (w = 8, h = 5.8 elsewhere) -- intentional, not drift: a full US
  # choropleth reads better at a wider aspect ratio than a coefficient
  # plot or trend line. See Technical Appendix 13 for the cross-script
  # comparison of all pubexport() defaults used in this chapter.
  ggsave(file.path(graphs_dir, paste0(gname, "_R.svg")), p, width = w, height = h)
  ggsave(file.path(graphs_dir, paste0(gname, "_R.pdf")), p, width = w, height = h)
  ggsave(file.path(graphs_dir, paste0(gname, "_R.png")), p, width = w, height = h, dpi = 300)
  # Register the live ggplot object (keyed by its canonical export
  # name, not its R variable name) for PolicymakerDeck13.R to pull
  # from later -- avoids the variable-name-collision risk that would
  # occur if the deck script instead grabbed figures by loose global
  # variable name (e.g. "fig13_15" is reused across two different
  # sub-scripts for two different figures).
  if (exists("fig13_registry", envir = .GlobalEnv)) {
    assign(gname, p, envir = get("fig13_registry", envir = .GlobalEnv))
  }
}

# Base state boundary layer (lower 48 + DC -- see limitation note above)
us_sf <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  mutate(state_lower = tolower(ID))

#-------------------------------------------------------------------------
# Figure 13.1: State Appropriations per Capita by State, FY 2017
#-------------------------------------------------------------------------
d1 <- load_ch13_dta("Example_13_1.dta", data_dir)

d1_2017 <- d1 %>%
  filter(fy == 2017) %>%
  mutate(state_lower = tolower(state_name))

ymax_row <- d1_2017[which.max(d1_2017$y_pop), ]
ymin_row <- d1_2017[which.min(d1_2017$y_pop), ]
hi_state <- toupper(ymax_row$state_name)
lo_state <- toupper(ymin_row$state_name)
ymax_fmt <- format(round(ymax_row$y_pop), big.mark = ",")
ymin_fmt <- format(round(ymin_row$y_pop), big.mark = ",")

map1_data <- us_sf %>% left_join(d1_2017, by = "state_lower")

# 5-bin quantile classification (Stata: nquantiles(5)), grayscale-safe
map1_data$y_pop_bin <- cut(map1_data$y_pop,
                            breaks = quantile(map1_data$y_pop, probs = seq(0, 1, 0.2), na.rm = TRUE),
                            include.lowest = TRUE)

fig13_1 <- ggplot(map1_data) +
  geom_sf(aes(fill = y_pop_bin), color = "gray30", linewidth = 0.2) +
  scale_fill_grey(start = 0.92, end = 0.15, na.value = "white", name = "Dollars") +
  labs(title = "State Appropriations per Capita, 2017 (in dollars)",
       subtitle = paste0("Highest: ", hi_state, " ($", ymax_fmt, "). Lowest: ", lo_state, " ($", ymin_fmt, ").")) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        legend.position = "right") +
  labs(caption = "Source: SHEF Data, FY2017.")

pubexport(fig13_1, "fig13_1_appro_percapita_2017")

#-------------------------------------------------------------------------
# Figure 13.2: Percent Change in State Appropriations per FTE Enrollment,
# FY 2009-FY 2017
#-------------------------------------------------------------------------
d2 <- load_ch13_dta("Example_13_2.dta", data_dir)

d2 <- d2 %>% mutate(state_lower = tolower(statename))

hi_row2 <- d2[which.max(d2$pctchnge), ]
lo_row2 <- d2[which.min(d2$pctchnge), ]
hi_state2 <- toupper(hi_row2$statename)
lo_state2 <- toupper(lo_row2$statename)
pmax_fmt <- sprintf("%.1f", hi_row2$pctchnge)
pmin_fmt <- sprintf("%.1f", lo_row2$pctchnge)

map2_data <- us_sf %>% left_join(d2, by = "state_lower")

map2_data$pctchnge_bin <- cut(map2_data$pctchnge,
                               breaks = quantile(map2_data$pctchnge, probs = seq(0, 1, 0.2), na.rm = TRUE),
                               include.lowest = TRUE)

fig13_2 <- ggplot(map2_data) +
  geom_sf(aes(fill = pctchnge_bin), color = "gray30", linewidth = 0.2) +
  scale_fill_grey(start = 0.92, end = 0.15, na.value = "white", name = "% Change") +
  labs(title = "Percent Change in State Appropriations per FTE Enrollment\nBetween FY 2009 & FY 2017",
       subtitle = paste0("Largest increase: ", hi_state2, " (", pmax_fmt,
                          "%). Largest decrease: ", lo_state2, " (", pmin_fmt, "%).")) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        legend.position = "right") +
  labs(caption = "Source: SHEF Data, FY2009-FY2017.")

pubexport(fig13_2, "fig13_2_pctchange_appro_fte")

cat("Maps13.R completed:", format(Sys.time()), "\n")
cat("NOTE: AK/HI not shown -- see limitation note in file header.\n")
