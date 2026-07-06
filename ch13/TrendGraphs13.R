#=========================================================================
# TrendGraphs13.R
# Section 13.4: Trend Graphs and Simple Comparisons
# R translation of TrendGraphs13.do
#
# Package substitutions:
#   Stata lgraph (group-mean line plot, user-written) -> dplyr::group_by/
#   summarise + ggplot2::geom_line(). ggplot2/dplyr are both native,
#   CRAN-standard R packages available in this environment.
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


cat("TrendGraphs13.R running:", format(Sys.time()), "\n")

# Book's B&W print theme -- grayscale-safe line patterns (solid/dashed),
# mirroring Stata's scheme(s2mono)/bw option used throughout this chapter.
theme_springer <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}

pubexport <- function(p, gname, w = 8, h = 5.8) {
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

#-------------------------------------------------------------------------
# Figure 13.3: State Appropriations per Population, FY 1980-2018
# (Maryland vs. all other states)
#-------------------------------------------------------------------------
d1 <- load_ch13_dta("Example_13_1.dta", data_dir)

if (!"MD" %in% names(d1)) d1$MD <- ifelse(d1$fips == 24, 1, 0)
d1$MD_lbl <- factor(d1$MD, levels = c(0, 1), labels = c("All Other States", "Maryland"))

fig13_3_data <- d1 %>%
  group_by(fy, MD_lbl) %>%
  summarise(y_pop = mean(y_pop, na.rm = TRUE), .groups = "drop")

fig13_3 <- ggplot(fig13_3_data, aes(x = fy, y = y_pop, linetype = MD_lbl)) +
  geom_line(linewidth = 0.9, color = "black") +
  scale_x_continuous(breaks = seq(1980, 2018, 3)) +
  scale_linetype_manual(values = c("Maryland" = "dashed", "All Other States" = "solid")) +
  labs(title = "State Appropriations Per Population\nFY 1980-2018",
       x = "Fiscal Year", y = "Dollars", linetype = NULL) +
  theme_springer()

pubexport(fig13_3, "fig13_3_md_v_nation")

#-------------------------------------------------------------------------
# Figure 13.4: State Appropriations per FTE, Maryland vs. All Other
# SREB States, FY 1980-2018
#-------------------------------------------------------------------------
if (!"MDSREB" %in% names(d1)) {
  d1$MDSREB <- ifelse(d1$region_compact == 1, 0, NA)
  d1$MDSREB[d1$fips == 24] <- 1
}
d1$MDSREB_lbl <- factor(d1$MDSREB, levels = c(0, 1),
                         labels = c("All Other SREB States", "Maryland"))

fig13_4_data <- d1 %>%
  filter(region_compact == 1) %>%
  group_by(fy, MDSREB_lbl) %>%
  summarise(yfte = mean(yfte, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(MDSREB_lbl))

fig13_4 <- ggplot(fig13_4_data, aes(x = fy, y = yfte, linetype = MDSREB_lbl)) +
  geom_line(linewidth = 0.9, color = "black") +
  scale_x_continuous(breaks = seq(1980, 2018, 2)) +
  scale_linetype_manual(values = c("Maryland" = "dashed", "All Other SREB States" = "solid")) +
  labs(title = "State Appropriations Per FTE\nFY 1980-2018",
       x = "Fiscal Year", y = "Dollars", linetype = NULL) +
  theme_springer() +
  theme(axis.text.x = element_text(size = 7))

pubexport(fig13_4, "fig13_4_md_v_sreb")

#-------------------------------------------------------------------------
# Figure 13.5: Colorado Net Tuition Revenue per FTE Before and After
# Senate Bill 189, vs. All Other States
#-------------------------------------------------------------------------
d3 <- load_ch13_dta("Example_13_3.dta", data_dir)

if (!"netuit_fte" %in% names(d3)) d3$netuit_fte <- d3$netuit / d3$fte
if (!"T" %in% names(d3)) d3$T <- ifelse(d3$state == "CO", 1, 0)
if (!"fy" %in% names(d3)) d3$fy <- d3$year

d3$T_lbl <- factor(d3$T, levels = c(0, 1), labels = c("All Other States", "Colorado"))

fig13_5_data <- d3 %>%
  filter(fy > 1999) %>%
  group_by(fy, T_lbl) %>%
  summarise(netuit_fte = mean(netuit_fte, na.rm = TRUE), .groups = "drop")

fig13_5 <- ggplot(fig13_5_data, aes(x = fy, y = netuit_fte, linetype = T_lbl)) +
  geom_line(linewidth = 0.9, color = "black") +
  geom_vline(xintercept = 2005, linetype = "dotted", color = "gray40") +
  scale_x_continuous(breaks = seq(2000, 2016, 2)) +
  scale_linetype_manual(values = c("Colorado" = "dashed", "All Other States" = "solid")) +
  labs(title = "Colorado's Net Tuition Revenue Per FTE\nBefore and After Colorado Senate Bill 189",
       x = "Fiscal Year", y = "Dollars", linetype = NULL) +
  theme_springer()

pubexport(fig13_5, "fig13_5_co_sb189_v_all")

#-------------------------------------------------------------------------
# Figure 13.6: Colorado Net Tuition Revenue per FTE Before and After
# Senate Bill 189, vs. All Other WICHE States
#-------------------------------------------------------------------------
if (!"COWICHE" %in% names(d3)) {
  d3$COWICHE <- ifelse(d3$region_compact == 2, 0, NA)
  d3$COWICHE[d3$state == "CO"] <- 1
}
d3$COWICHE_lbl <- factor(d3$COWICHE, levels = c(0, 1),
                          labels = c("All Other WICHE States", "Colorado"))

fig13_6_data <- d3 %>%
  filter(region_compact == 2, fy > 1999) %>%
  group_by(fy, COWICHE_lbl) %>%
  summarise(netuit_fte = mean(netuit_fte, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(COWICHE_lbl))

fig13_6 <- ggplot(fig13_6_data, aes(x = fy, y = netuit_fte, linetype = COWICHE_lbl)) +
  geom_line(linewidth = 0.9, color = "black") +
  geom_vline(xintercept = 2005, linetype = "dotted", color = "gray40") +
  scale_x_continuous(breaks = seq(2000, 2016, 2)) +
  scale_linetype_manual(values = c("Colorado" = "dashed", "All Other WICHE States" = "solid")) +
  labs(title = "Colorado's Net Tuition Revenue Per FTE\nBefore and After Colorado Senate Bill 189",
       x = "Fiscal Year", y = "Dollars", linetype = NULL) +
  theme_springer()

pubexport(fig13_6, "fig13_6_co_sb189_v_wiche")

cat("TrendGraphs13.R completed:", format(Sys.time()), "\n")
cat("Figures written to:", graphs_dir, "\n")
