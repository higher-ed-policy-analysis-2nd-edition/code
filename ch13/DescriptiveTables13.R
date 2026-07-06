#=========================================================================
# DescriptiveTables13.R
# Section 13.2: Presenting Descriptive Statistics
# R translation of DescriptiveTables13.do
#
# Called by:  R_code13.R
# Inherits:   root_dir, graphs_dir, tables_dir, logdir, data_dir
#             (re-derived below if run standalone)
#
# Package substitutions (see chapter README for full rationale):
#   Stata dtable/asdoc -> base R aggregate() + officer::body_add_table()
#   (docx export). officer is a native, CRAN-standard R package -- no
#   external/CRAN-blocked dependency involved for this sub-script.
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
  dir.create(graphs_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(logdir,     showWarnings = FALSE, recursive = TRUE)
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(officer)
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


cat("DescriptiveTables13.R running:", format(Sys.time()), "\n")

#-------------------------------------------------------------------------
# 13.2.1 Descriptive Statistics in Microsoft Word Tables
#-------------------------------------------------------------------------
d <- load_ch13_dta("Example_13_1.dta", data_dir)

# rescale y, x1, x3, x4, x5 to millions (Stata: rescale ..., millions)
# rescale y, x1, x3, x4, x5 to millions (Stata: rescale ..., millions)
# IMPORTANT: Stata's rescale command is DESTRUCTIVE -- it overwrites the
# variable in place, it does not create a new scaled copy. Confirmed
# against Chapter13_Stata_output.log: "sum y x1 x2 x3 x4 x5" run
# immediately after the four rescale calls shows y's mean as 1072.642
# (millions), not the raw dollar figure -- so every downstream use of
# x1/x3/x4/x5 in this script (including the derived _pop/_fte variables
# below) uses the RESCALED values. x2 was never rescaled and stays raw
# throughout. Replicated here by overwriting in place, not by creating
# parallel _m columns, so the derived-variable arithmetic below matches
# the Stata log exactly (including x1fte and x5_pop rounding to ~0 and
# ~6 respectively once expressed in millions).
d <- d %>%
  mutate(
    y  = y  / 1e6,
    x1 = x1 / 1e6,
    x3 = x3 / 1e6,
    x4 = x4 / 1e6,
    x5 = x5 / 1e6
  )

cat("\n-- sum y x1 x2 x3 x4 x5 (rescaled) --\n")
print(summary(d[, c("y", "x1", "x2", "x3", "x4", "x5")]))

#-------------------------------------------------------------------------
# Derived per-capita / per-FTE variables
# (idempotent: only created if not already present, mirroring the
# Stata capture confirm variable pattern)
#-------------------------------------------------------------------------
if (!"x1fte" %in% names(d)) d$x1fte  <- d$x1 / d$x2
if (!"x3_pop" %in% names(d)) d$x3_pop <- d$x3 / d$x5
if (!"x4_pop" %in% names(d)) d$x4_pop <- d$x4 / d$x5
if (!"x5_pop" %in% names(d)) d$x5_pop <- d$x5

desc_vars <- c("y_pop", "x1fte", "x3_pop", "x4_pop", "x5_pop")

tab13_1 <- d %>%
  summarise(across(all_of(desc_vars),
                    list(Mean = ~mean(.x, na.rm = TRUE),
                         Median = ~median(.x, na.rm = TRUE)))) %>%
  tidyr::pivot_longer(everything(),
                       names_to = c("Variable", "Statistic"),
                       names_pattern = "(.*)_(Mean|Median)") %>%
  tidyr::pivot_wider(names_from = Statistic, values_from = value) %>%
  mutate(Mean = round(Mean, 0), Median = round(Median, 0))

cat("\n-- Table 13.1 Descriptive Statistics --\n")
print(tab13_1)

# Export Table 13.1 to Word (officer -- native R, CRAN-standard)
doc <- read_docx()
doc <- doc %>%
  body_add_par("Table 13.1 Descriptive Statistics", style = "heading 1") %>%
  body_add_table(as.data.frame(tab13_1), style = "table_template")
print(doc, target = file.path(tables_dir, "Table13_1_dtable.docx"))

#-------------------------------------------------------------------------
# Grouped comparison table (Maryland vs. all other states, by decade)
#-------------------------------------------------------------------------
if (!"MD" %in% names(d)) {
  d$MD <- ifelse(d$fips == 24, 1, 0)
}
d$MD_lbl <- factor(d$MD, levels = c(0, 1),
                    labels = c("All Other States", "Maryland"))

if (!"decade" %in% names(d)) {
  d <- d %>%
    mutate(decade = case_when(
      fy >= 1980 & fy <= 1989 ~ 1,
      fy >= 1990 & fy <= 1999 ~ 2,
      fy >= 2000 & fy <= 2009 ~ 3,
      fy >= 2010 & fy <= 2018 ~ 4,
      TRUE ~ 0
    ))
}
d$decade_lbl <- factor(d$decade, levels = 1:4,
                        labels = c("1980 to 1989", "1990 to 1999",
                                   "2000 to 2009", "2010 to 2018"))

tab13_2 <- d %>%
  filter(!is.na(decade_lbl)) %>%
  group_by(decade_lbl, MD_lbl) %>%
  summarise(mean_y_pop = round(mean(y_pop, na.rm = TRUE), 0), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = MD_lbl, values_from = mean_y_pop)

cat("\n-- Table 13.2 Average State Appropriations per Population --\n")
print(tab13_2)

doc2 <- read_docx()
doc2 <- doc2 %>%
  body_add_par("Table 13.2 Average State Appropriations per Population",
               style = "heading 1") %>%
  body_add_table(as.data.frame(tab13_2), style = "table_template")
print(doc2, target = file.path(tables_dir, "Table13_2_table.docx"))

cat("\nDescriptiveTables13.R completed:", format(Sys.time()), "\n")
