# ================================================================
# Chapter 4 - Creating Datasets and Managing Data
# R Translation of Complete Stata Code
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-
#         edition/tree/main/code/ch4
# Author: Marvin A. Titus
# Date: November 16, 2025
# ================================================================

# Script tested in R 4.4.x
# Required packages: readxl, writexl, haven, dplyr, tidyr, psych, plm

# ----------------------------------------------------------------
# Install any missing packages (run once)
# ----------------------------------------------------------------
required_pkgs <- c("readxl", "writexl", "haven", "dplyr", "tidyr", "psych", "plm")
new_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs)

suppressPackageStartupMessages({
  library(readxl)   # read_excel()         — replaces: import excel
  library(writexl)  # write_xlsx()         — replaces: export excel
  library(haven)    # read_dta/write_dta() — replaces: use/save *.dta
  library(dplyr)    # data manipulation    — replaces: gen, drop, order, keep
  library(tidyr)    # pivot_longer/wider   — replaces: sreshape / reshape
  library(psych)    # describe()           — replaces: summarize, detail
  library(plm)      # pdata.frame()        — replaces: xtset / xtdes / xtsum
})

# ================================================================
# WORKING DIRECTORY AND OUTPUT PATHS
# Paths switch automatically by username, mirroring the Stata logic.
# ================================================================

user <- Sys.info()[["user"]]

if (user == "marvi") {
  log_path <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 4/Output/logs/Chapter4_R_output.log"
  dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)
} else {
  log_path <- "Output/logs/Chapter4_R_output.log"
  dir.create("Output/logs", showWarnings = FALSE, recursive = TRUE)
}

# Open log — sink() captures all console output to a text file
# Equivalent to: log using "...", replace text
sink(log_path, split = TRUE)   # split = TRUE also prints to console
cat("Chapter 4 log opened:", format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n\n")

# ----------------------------------------------------------------
# Helper: safe download
# Wraps download.file() in tryCatch so the script continues and
# reports clearly if a file cannot be fetched (e.g. no internet).
# Equivalent to Stata's: copy "url" "localfile", replace
# ----------------------------------------------------------------
safe_download <- function(url, dest) {
  tryCatch(
    download.file(url, dest, mode = "wb", quiet = TRUE),
    error   = function(e) message("  [download failed — ", basename(dest),
                                   "]: ", conditionMessage(e)),
    warning = function(w) message("  [download warning — ", basename(dest),
                                   "]: ", conditionMessage(w))
  )
}

# ----------------------------------------------------------------
# Helper: parse NCES multi-header Excel table
# NCES Digest tables have a title row, sub-header rows, and column-
# number rows before the data. This function:
#   1. Reads the raw sheet with no header assumptions
#   2. Detects the data-start row (first row with "United States" or a state)
#   3. Scans up to 5 rows above data_start for four-digit year values
#   4. Builds clean "PREFIX_YYYY" column names
#   5. Deduplicates (NCES tables often have two side-by-side panels)
#   6. Returns a clean data frame with State + PREFIX_YYYY columns
# ----------------------------------------------------------------
parse_nces_wide <- function(file, sheet, prefix, filter_state_col = TRUE) {
  raw <- read_excel(file, sheet = sheet, col_names = FALSE)

  # Find where real data begins
  data_start <- which(grepl("United States|Alabama", raw[[1]],
                             ignore.case = TRUE))[1]
  if (is.na(data_start)) stop("Could not find data start row in ", file)

  # Scan up to 8 rows above data_start for four-digit year values
  # (some NCES sheets have deeper header stacks than others)
  year_row <- NULL
  for (offset in 1:8) {
    if (data_start - offset < 1) break
    candidate <- as.character(unlist(raw[data_start - offset, ]))
    if (any(!is.na(candidate) & grepl("^[0-9]{4}$", candidate))) {
      year_row <- candidate
      break
    }
  }
  if (is.null(year_row)) {
    cat("  [parse_nces_wide] Could not locate a year row in", file,
        "— returning empty data frame.\n")
    return(data.frame(State = character(0)))
  }

  # Build column names
  col_names    <- character(ncol(raw))
  col_names[1] <- "State"
  id_col <- which(!is.na(year_row) & grepl("^[0-9]{4}$", year_row))
  for (i in seq_along(id_col)) {
    col_names[id_col[i]] <- paste0(prefix, "_", year_row[id_col[i]])
  }

  # Deduplicate: keep first occurrence of each year column
  seen <- character()
  for (i in seq_along(col_names)) {
    nm <- col_names[i]
    if (is.na(nm) || nm == "") {
      col_names[i] <- ""          # normalise NA → ""
    } else if (nm == "State") {
      next
    } else if (nm %in% seen) {
      col_names[i] <- ""          # mark duplicate as filler
    } else {
      seen <- c(seen, nm)
    }
  }
  empty_idx <- which(is.na(col_names) | col_names == "")
  col_names[empty_idx] <- paste0("drop_", seq_along(empty_idx))

  # Slice to data rows and apply names
  df <- raw[data_start:nrow(raw), ]
  names(df) <- col_names

  # Keep State + prefix columns; drop blank/aggregate rows
  df <- df |>
    select(State, starts_with(paste0(prefix, "_"))) |>
    filter(!is.na(State),
           !grepl("^\\s*$",            State),
           !grepl("\\.\\.\\.|United States|^[0-9]+$|^NOTE|^Source",
                  State, ignore.case = TRUE)) |>
    mutate(across(starts_with(paste0(prefix, "_")), as.numeric))

  df
}

# ================================================================
# Section 4.2.1: Primary Data Entry
# ================================================================
cat("*===============================================================================\n")
cat("* Section 4.2.1: Primary Data Entry\n")
cat("*===============================================================================\n\n")

# ----------------------------------------------------------------
# Simple data entry
# Equivalent to: input variable_x variable_y variable_z ... end
# ----------------------------------------------------------------
df_primary <- data.frame(
  variable_x = c(31, 25, 35, 38, 30),
  variable_y = c(57, 68, 60, 59, 59),
  variable_z = c(18, 12, 13, 17, 15)
)

# Display the data — equivalent to: list
cat(". list\n\n")
print(df_primary)
cat("\n")

# Save the dataset — equivalent to: save "Example_1_0.dta", replace
haven::write_dta(df_primary, "Example_1_0.dta")
cat("file Example_1_0.dta saved\n\n")

# Export to CSV — equivalent to: export delimited using "Example_1.csv"
write.csv(df_primary, "Example_1.csv", row.names = FALSE)
cat("file Example_1.csv saved\n\n")

# ----------------------------------------------------------------
# Importing data from CSV file
# Equivalent to: insheet using "Example_1.csv", comma
# ----------------------------------------------------------------
df_from_csv <- read.csv("Example_1.csv")
cat(". insheet using Example_1.csv\n\n")
print(df_from_csv)
cat("\n")

# ----------------------------------------------------------------
# Note on iefieldkit equivalent:
# R packages for electronic data collection / CAPI workflows:
# ODKr, surveyHQ, or REDCapR. See:
# https://github.com/worldbank/iefieldkit
# ----------------------------------------------------------------

# ================================================================
# Section 4.2.2: Secondary Data — Cross-Sectional Data
# ================================================================
cat("*===============================================================================\n")
cat("* Section 4.2.2: Secondary Data - Cross-Sectional Data\n")
cat("*===============================================================================\n\n")

# ----------------------------------------------------------------
# NCES Digest Table 302.50
# High school graduates enrolled in PSE by state, 2012
# Equivalent to: copy "..." + import excel
# ----------------------------------------------------------------
url_302_50 <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                     "2nd-edition/data/main/ch4/tabn302_50.xlsx")
safe_download(url_302_50, "tabn302_50.xlsx")

if (file.exists("tabn302_50.xlsx") && file.size("tabn302_50.xlsx") > 0) {
  sheets_302_50 <- excel_sheets("tabn302_50.xlsx")
  cat("  Available sheets in tabn302_50.xlsx:", paste(sheets_302_50, collapse = ", "), "\n")
  sheet_302_50 <- sheets_302_50[grep("reformat", sheets_302_50, ignore.case = TRUE)[1]]
  if (is.na(sheet_302_50)) sheet_302_50 <- sheets_302_50[1]
  cat("  Reading sheet:", sheet_302_50, "\n\n")
  df_crosssec <- read_excel("tabn302_50.xlsx", sheet = sheet_302_50)
} else {
  cat("  [Using synthetic data for tabn302_50]\n\n")
  state_names_51 <- c(state.name, "District of Columbia")
  fips_51 <- c(1,2,4,5,6,8,9,10,12,13,15,16,17,18,19,20,21,22,23,
               24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
               42,44,45,46,47,48,49,50,51,53,54,55,56,11)
  df_crosssec <- data.frame(
    stateid   = 1:51,
    State     = state_names_51,
    Total     = as.integer(runif(51, 5000, 450000)),
    Public    = as.integer(runif(51, 4000, 420000)),
    Private   = as.integer(runif(51,  100,  35000)),
    anystate  = as.integer(runif(51, 3000, 265000)),
    homestate = as.integer(runif(51, 2000, 231000)),
    anyrate   = as.integer(runif(51,   43,     79)),
    homerate  = as.integer(runif(51,   29,     73)),
    stringsAsFactors = FALSE
  )
}

# View structure — equivalent to: browse / describe
cat(". describe\n\n")
cat("  Observations:", nrow(df_crosssec), "\n")
cat("  Variables:   ", ncol(df_crosssec), "\n\n")
print(head(df_crosssec))
cat("\n")

# ----------------------------------------------------------------
# Adding FIPS codes and state abbreviations
# Equivalent to: statastates, name(State)
# ----------------------------------------------------------------
state_lookup <- data.frame(
  State        = c(state.name, "District of Columbia"),
  state_abbrev = c(state.abb, "DC"),
  state_fips   = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18,
                   19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
                   32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45,
                   46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 11),
  stringsAsFactors = FALSE
)

df_crosssec <- df_crosssec |>
  left_join(state_lookup, by = "State") |>
  relocate(state_abbrev, state_fips, .before = State)

cat(". statastates + order state_abbrev state_fips\n\n")
print(head(df_crosssec))
cat("\n")

# ----------------------------------------------------------------
# Variable labels — equivalent to: lab var varname "label"
# ----------------------------------------------------------------

# Apply labels only to columns that actually exist
label_map <- list(
  stateid      = "State ID number",
  state_abbrev = "State abbreviation",
  state_fips   = "FIPS code",
  State        = "State name",
  Total        = "Total number of graduates from HS located in the state",
  Public       = "Number of graduates from public HS located in the state",
  Private      = "Number of graduates from private HS located in the state",
  anystate     = "Number of 1st-time freshmen graduating from HS enrolled in any state",
  homestate    = "Number of 1st-time freshmen graduating from HS enrolled in home state",
  anyrate      = "Estimated rate of HS graduates going to college in any state",
  homerate     = "Estimated rate of HS graduates going to college in home state"
)
for (v in intersect(names(label_map), names(df_crosssec))) {
  attr(df_crosssec[[v]], "label") <- label_map[[v]]
}

# View labels — equivalent to: describe
cat(". describe (with labels)\n\n")
for (v in names(df_crosssec)) {
  lbl <- attr(df_crosssec[[v]], "label")
  cat(sprintf("  %-15s  %s\n", v, if (!is.null(lbl)) lbl else ""))
}
cat("\n")

# Save — equivalent to: save "US high school graduates…"
haven::write_dta(df_crosssec,
  "US high school graduates in 2012 enrolled in PSE, by state.dta")
cat("file saved: US high school graduates in 2012 enrolled in PSE, by state.dta\n\n")

# ================================================================
# Section 4.2.2: Secondary Data — Time Series Data
# ================================================================
cat("*===============================================================================\n")
cat("* Section 4.2.2: Secondary Data - Time Series Data\n")
cat("*===============================================================================\n\n")

url_302_10 <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                     "2nd-edition/data/main/ch4/tabn302_10.xlsx")
safe_download(url_302_10, "tabn302_10.xlsx")

if (file.exists("tabn302_10.xlsx") && file.size("tabn302_10.xlsx") > 0) {
  sheets_302_10 <- excel_sheets("tabn302_10.xlsx")
  cat("  Available sheets in tabn302_10.xlsx:", paste(sheets_302_10, collapse = ", "), "\n")
  sheet_302_10 <- sheets_302_10[grep("reformat", sheets_302_10, ignore.case = TRUE)[1]]
  if (is.na(sheet_302_10)) sheet_302_10 <- sheets_302_10[1]
  cat("  Reading sheet:", sheet_302_10, "\n\n")
  df_ts <- read_excel("tabn302_10.xlsx", sheet = sheet_302_10) |>
    arrange(year)
} else {
  cat("  [Using synthetic data for tabn302_10]\n\n")
  df_ts <- data.frame(year = 1960:2016, totalpct = round(runif(57, 40, 70), 1))
}

# Equivalent to: tsdes
cat(". tsdes\n\n")
cat("  Time variable: year\n")
cat("  Range:        ", min(df_ts$year), "to", max(df_ts$year), "\n")
cat("  Delta:         1 year\n")
cat("  Observations: ", nrow(df_ts), "\n\n")

haven::write_dta(df_ts,
  "Percent of US high school graduates in PSE, 1960 to 2016.dta")
cat("file saved: Percent of US high school graduates in PSE, 1960 to 2016.dta\n\n")

# ================================================================
# Section 4.2.2: Secondary Data — Panel Data (Wide Format)
# ================================================================
cat("*===============================================================================\n")
cat("* Section 4.2.2: Secondary Data - Panel Data (Wide Format)\n")
cat("*===============================================================================\n\n")

url_304_70 <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                     "2nd-edition/data/main/ch4/tabn304_70.xlsx")
safe_download(url_304_70, "tabn304_70.xlsx")

if (file.exists("tabn304_70.xlsx") && file.size("tabn304_70.xlsx") > 0) {
  sheets_304_70 <- excel_sheets("tabn304_70.xlsx")
  cat("  Available sheets in tabn304_70.xlsx:", paste(sheets_304_70, collapse = ", "), "\n")
  sheet_ugrad <- sheets_304_70[grep("ndergrad", sheets_304_70, ignore.case = TRUE)[1]]
  if (is.na(sheet_ugrad)) sheet_ugrad <- sheets_304_70[1]
  cat("  Reading sheet:", sheet_ugrad, "\n\n")
  df_wide <- parse_nces_wide("tabn304_70.xlsx", sheet_ugrad, "Ugrad")
} else {
  cat("  [Using synthetic data for tabn304_70]\n\n")
  ugrad_years <- c(2000, 2010, 2012, 2015, 2016, 2017)
  df_wide <- data.frame(
    State = c(state.name, "District of Columbia"),
    setNames(as.data.frame(
      matrix(as.integer(runif(51 * length(ugrad_years), 50000, 2e6)),
             51, length(ugrad_years))),
      paste0("Ugrad_", ugrad_years)),
    stringsAsFactors = FALSE
  )
}

cat(". describe (wide format)\n\n")
cat("  Observations:", nrow(df_wide), "  Variables:", ncol(df_wide), "\n")
cat("  Names:", paste(names(df_wide), collapse = ", "), "\n\n")
print(head(df_wide))
cat("\n")

haven::write_dta(df_wide, "Undergraduate enrollment data - Wide.dta")
cat("file saved: Undergraduate enrollment data - Wide.dta\n\n")

# ----------------------------------------------------------------
# Convert from wide to long format
# Equivalent to: sreshape long Ugrad, i(id) j(year)
# ----------------------------------------------------------------
year_cols <- grep("^Ugrad_", names(df_wide), value = TRUE)

df_long <- df_wide |>
  pivot_longer(
    cols         = all_of(year_cols),
    names_to     = "year",
    names_prefix = "Ugrad_",
    values_to    = "Ugrad"
  ) |>
  mutate(year = as.integer(year),
         id   = as.integer(factor(State))) |>
  arrange(id, year)

cat(". sreshape long Ugrad, i(id) j(year)\n\n")
cat("  Observations after reshape:", nrow(df_long), "\n")
cat("  Variables:", paste(names(df_long), collapse = ", "), "\n\n")
print(head(df_long, 10))
cat("\n")

# Declare as panel — equivalent to: xtset id year, yearly
pdf_long <- pdata.frame(df_long, index = c("id", "year"))

cat(". xtdes\n\n")
cat("  Panel variable: id\n")
cat("  Time variable:  year,", min(df_long$year), "to", max(df_long$year), "\n")
cat("  n =", length(unique(df_long$id)), "panels\n")
cat("  T =", length(unique(df_long$year)), "periods\n")
cat("  N =", nrow(df_long), "observations\n\n")

haven::write_dta(df_long, "Undergraduate enrollment data - Long.dta")
cat("file saved: Undergraduate enrollment data - Long.dta\n\n")

# ================================================================
# Section 4.2.2: Creating Additional Panel Variables
# ================================================================
cat("*===============================================================================\n")
cat("* Section 4.2.2: Creating Additional Panel Variables\n")
cat("*===============================================================================\n\n")

url_219_20 <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                     "2nd-edition/data/main/ch4/tabn219_20.xlsx")
safe_download(url_219_20, "tabn219_20.xlsx")

if (file.exists("tabn219_20.xlsx") && file.size("tabn219_20.xlsx") > 0) {
  sheets_219_20 <- excel_sheets("tabn219_20.xlsx")
  cat("  Available sheets in tabn219_20.xlsx:", paste(sheets_219_20, collapse = ", "), "\n")
  sheet_hsgrad <- sheets_219_20[
    grep("hsgrad|hs.grad|grad", sheets_219_20, ignore.case = TRUE)[1]]
  if (is.na(sheet_hsgrad)) sheet_hsgrad <- sheets_219_20[1]
  cat("  Reading sheet:", sheet_hsgrad, "\n\n")
  df_hsgrad_wide <- parse_nces_wide("tabn219_20.xlsx", sheet_hsgrad, "HSGrad")
} else {
  cat("  [Using synthetic data for tabn219_20]\n\n")
  hsgrad_years <- c(2000, 2005, 2010, 2012, 2015, 2017)
  df_hsgrad_wide <- data.frame(
    State = c(state.name, "District of Columbia"),
    setNames(as.data.frame(
      matrix(as.integer(runif(51 * length(hsgrad_years), 30000, 500000)),
             51, length(hsgrad_years))),
      paste0("HSGrad_", hsgrad_years)),
    stringsAsFactors = FALSE
  )
}

haven::write_dta(df_hsgrad_wide, "HSGrad - Wide.dta")
cat("file saved: HSGrad - Wide.dta\n\n")

# Reshape to long — equivalent to: sreshape long HSGrad, i(id) j(year)
hsgrad_year_cols <- grep("^HSGrad_", names(df_hsgrad_wide), value = TRUE)

if (length(hsgrad_year_cols) == 0) {
  cat("  [Warning] No HSGrad_YYYY columns found — check sheet layout.\n")
  cat("  Column names found:", paste(names(df_hsgrad_wide), collapse = ", "), "\n\n")
  df_hsgrad_long <- data.frame(State = character(0), year = integer(0),
                                HSGrad = numeric(0), id = integer(0))
} else {
  df_hsgrad_long <- df_hsgrad_wide |>
    pivot_longer(
      cols         = all_of(hsgrad_year_cols),
      names_to     = "year",
      names_prefix = "HSGrad_",
      values_to    = "HSGrad"
    ) |>
    mutate(year = as.integer(year),
           id   = as.integer(factor(State))) |>
    arrange(id, year)
}

# Declare as panel — equivalent to: xtset id year, yearly
pdata.frame(df_hsgrad_long, index = c("id", "year"))

haven::write_dta(df_hsgrad_long, "HSGrad - Long.dta")
cat("file saved: HSGrad - Long.dta\n\n")

# ================================================================
# Section 4.2.2: Joining Panel Datasets
# ================================================================
cat("*===============================================================================\n")
cat("* Section 4.2.2: Joining Panel Datasets\n")
cat("*===============================================================================\n\n")

# Joining two panel datasets
# Equivalent to: joinby id year using "First-Time - Long.dta"
df_joined <- df_hsgrad_long
# Uncomment to join when file is available:
# df_firsttime <- haven::read_dta("First-Time - Long.dta")
# df_joined <- inner_join(df_hsgrad_long, df_firsttime, by = c("id", "year"))

cat(". xtdes (HSGrad - Long)\n\n")
cat("  Panel variable: id\n")
cat("  Time variable:  year,", min(df_joined$year), "to", max(df_joined$year), "\n")
cat("  n =", length(unique(df_joined$id)), "panels\n")
cat("  N =", nrow(df_joined), "observations\n\n")

# Joining multiple panel datasets
# Equivalent to: joinby id year using "... need" + "... merit"
df_panel <- haven::read_dta("Undergraduate enrollment data - Long.dta") |>
  mutate(across(everything(), haven::zap_labels),  # strip all Stata labels
         id   = as.integer(id),
         year = as.integer(year))

if (file.exists("Undergraduate state financial aid - need.dta")) {
  df_need  <- haven::read_dta("Undergraduate state financial aid - need.dta")
  df_panel <- inner_join(df_panel, df_need,  by = c("id", "year"))
}
if (file.exists("Undergraduate state financial aid - merit.dta")) {
  df_merit <- haven::read_dta("Undergraduate state financial aid - merit.dta")
  df_panel <- inner_join(df_panel, df_merit, by = c("id", "year"))
}

# Declare as panel
pdf_panel <- pdata.frame(df_panel, index = c("id", "year"))

cat(". xtdes (combined panel)\n\n")
cat("  Panel variable: id\n")
cat("  Time variable:  year,", min(df_panel$year), "to", max(df_panel$year), "\n")
cat("  n =", length(unique(df_panel$id)), "panels\n")
cat("  N =", nrow(df_panel), "observations\n\n")

haven::write_dta(df_panel, "Example_4_2_2_Panel.dta")
cat("file saved: Example_4_2_2_Panel.dta\n\n")

# ================================================================
# Additional Data Management and Panel Data Commands
# ================================================================
cat("*===============================================================================\n")
cat("* Additional Data Management and Panel Data Commands\n")
cat("*===============================================================================\n\n")

df_panel <- haven::read_dta("Example_4_2_2_Panel.dta") |>
  mutate(across(everything(), haven::zap_labels),  # strip all Stata labels
         id   = as.integer(id),
         year = as.integer(year))

# Equivalent to: describe
cat(". describe\n\n")
cat("  Observations:", nrow(df_panel), "\n")
cat("  Variables:   ", ncol(df_panel), "\n")
cat("  Names:", paste(names(df_panel), collapse = ", "), "\n\n")

# Equivalent to: summarize
cat(". summarize\n\n")
print(summary(df_panel))
cat("\n")

# Equivalent to: summarize, detail
cat(". summarize, detail\n\n")
print(psych::describe(df_panel))
cat("\n")

# Equivalent to: misstable summarize
cat(". misstable summarize\n\n")
miss_tbl <- data.frame(
  Variable = names(df_panel),
  Missing  = sapply(df_panel, function(x) sum(is.na(x))),
  Total    = nrow(df_panel),
  Pct_Miss = sapply(df_panel, function(x) round(mean(is.na(x)) * 100, 2))
)
miss_tbl <- miss_tbl[miss_tbl$Missing > 0, ]
if (nrow(miss_tbl) == 0) {
  cat("  No missing values.\n\n")
} else {
  print(miss_tbl, row.names = FALSE)
  cat("\n")
}

# Equivalent to: list in 1/10
cat(". list in 1/10\n\n")
print(head(df_panel, 10))
cat("\n")

# ----------------------------------------------------------------
# Panel data diagnostics
# ----------------------------------------------------------------

# Defensive coercion: ensure id, year, and all numeric-intended columns
# are plain R types before pdata.frame() and xtsum.
# If Ugrad came through as character (e.g. NCES footnote symbols in cells),
# as.numeric() converts it, producing NAs for any non-numeric strings.
df_panel <- df_panel |>
  mutate(
    id   = as.integer(id),
    year = as.integer(year),
    # Coerce columns that are numeric or haven_labelled; leave character alone
    across(where(function(x) is.numeric(x) && !inherits(x, "integer")),
           function(x) suppressWarnings(as.numeric(x)))
  ) |>
  mutate(id = as.integer(id), year = as.integer(year))

# Diagnostic: show class and NA count for each column
cat("  Column types in df_panel:\n")
for (v in names(df_panel)) {
  cat(sprintf("    %-15s  class: %-12s  NAs: %d\n",
              v, class(df_panel[[v]])[1], sum(is.na(df_panel[[v]]))))
}
cat("\n")

pdf_panel    <- pdata.frame(df_panel, index = c("id", "year"))
t_per_panel  <- df_panel |> dplyr::count(id, name = "T_i")

cat(". xtdes\n\n")
cat("  Panel variable: id\n")
cat("  Time variable:  year,", min(df_panel$year), "to", max(df_panel$year), "\n")
cat("  n =", length(unique(df_panel$id)), "\n")
cat("  T: min =", min(t_per_panel$T_i), " max =", max(t_per_panel$T_i), "\n")
cat("  Balanced:", ifelse(n_distinct(t_per_panel$T_i) == 1, "Yes", "No"), "\n\n")

# Equivalent to: xtsum
cat(". xtsum\n\n")
# Force all numeric columns to plain double immediately before computing SDs.
# This guards against haven_labelled, pseries, or any other wrapper class
# that causes sd() to return NA or plm::lag() to fail.
for (v in names(df_panel)) {
  if (!is.character(df_panel[[v]]) && !is.integer(df_panel[[v]])) {
    df_panel[[v]] <- as.numeric(df_panel[[v]])
  }
}
df_panel$id   <- as.integer(df_panel$id)
df_panel$year <- as.integer(df_panel$year)

numeric_vars <- names(df_panel)[
  sapply(df_panel, is.numeric) & !names(df_panel) %in% c("id", "year")]
for (v in numeric_vars) {
  x_overall <- df_panel[[v]]
  grp_means  <- df_panel |>
    group_by(id) |> summarise(m = mean(.data[[v]], na.rm = TRUE), .groups = "drop")
  x_between  <- grp_means$m
  x_within   <- x_overall -
    ave(x_overall, df_panel$id, FUN = function(z) mean(z, na.rm = TRUE))
  cat(sprintf("  %-20s  Overall sd: %8.3f  Between sd: %8.3f  Within sd: %8.3f\n",
              v,
              sd(x_overall, na.rm = TRUE),
              sd(x_between, na.rm = TRUE),
              sd(x_within,  na.rm = TRUE)))
}
cat("\n")

# ----------------------------------------------------------------
# Creating lagged and differenced variables
# Equivalent to: gen Ugrad_lag1 = L.Ugrad  /  gen Ugrad_diff = D.Ugrad
# Use dplyr::lag() within panel groups rather than plm::lag(), because
# plm::lag(k=1) shifts by one calendar year — which fails for non-
# consecutive panels (e.g. 2000, 2010, 2012, 2015, 2016, 2017) where
# the internal vector would need length 18 but only 6 obs exist per panel.
# dplyr::lag() shifts by row position within each group, matching Stata's
# L. operator behaviour for unbalanced/non-consecutive panels.
# ----------------------------------------------------------------
df_panel <- df_panel |>
  arrange(id, year) |>
  group_by(id) |>
  mutate(
    Ugrad_lag1 = dplyr::lag(Ugrad, 1),
    Ugrad_diff = Ugrad - dplyr::lag(Ugrad, 1)
  ) |>
  ungroup()
attr(df_panel$Ugrad_lag1, "label") <- "Undergraduate enrollment (t-1)"
attr(df_panel$Ugrad_diff, "label") <- "Change in undergraduate enrollment"

# Rebuild pdf_panel with the new columns
pdf_panel <- pdata.frame(df_panel, index = c("id", "year"))

cat(". list id year Ugrad Ugrad_lag1 Ugrad_diff in 1/20\n\n")
print(head(df_panel[, c("id", "year", "Ugrad", "Ugrad_lag1", "Ugrad_diff")], 20))
cat("\n")

# ----------------------------------------------------------------
# Creating per-student variables
# Equivalent to: gen need_per_student = need / Ugrad
# ----------------------------------------------------------------
if ("need" %in% names(pdf_panel) && "merit" %in% names(pdf_panel)) {
  pdf_panel$need_per_student  <- as.numeric(pdf_panel$need)  /
                                  as.numeric(pdf_panel$Ugrad)
  pdf_panel$merit_per_student <- as.numeric(pdf_panel$merit) /
                                  as.numeric(pdf_panel$Ugrad)
  attr(pdf_panel$need_per_student,  "label") <- "Need-based aid per undergraduate student"
  attr(pdf_panel$merit_per_student, "label") <- "Merit-based aid per undergraduate student"

  cat(". summarize need_per_student merit_per_student\n\n")
  print(summary(as.data.frame(pdf_panel)[,
                  c("need_per_student", "merit_per_student")]))
  cat("\n")
}

# ----------------------------------------------------------------
# Subsetting data (commented out — mirrors Stata)
# Equivalent to: keep if year >= 2010
# df_panel <- df_panel |> filter(year >= 2010)
# Equivalent to: keep if inlist(state_fips, 9, 23, ...)
# df_panel <- df_panel |> filter(state_fips %in% c(9,23,25,33,44,50,34,36,42))
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# Exporting data
# Equivalent to: export delimited + export excel
# ----------------------------------------------------------------
# Export using df_panel (the plain data frame, not the pdata.frame).
# as.data.frame(pdf_panel) converts year and id to pseries factors
# whose codes (1,2,3…) rather than actual values (2000,2010…) are
# saved by write_dta — corrupting the panel structure on re-read.
write.csv(df_panel,  "Example_4_2_2_Panel.csv",  row.names = FALSE)
writexl::write_xlsx(df_panel, "Example_4_2_2_Panel.xlsx")
cat("files saved: Example_4_2_2_Panel.csv, Example_4_2_2_Panel.xlsx\n\n")

haven::write_dta(df_panel, "Example_4_2_2_Panel.dta")
cat("file saved: Example_4_2_2_Panel.dta\n\n")

# ================================================================
# Additional Useful Panel Data Examples
# ================================================================
cat("*===============================================================================\n")
cat("* Additional Useful Panel Data Examples\n")
cat("*===============================================================================\n\n")

df_ts <- haven::read_dta(
  "Percent of US high school graduates in PSE, 1960 to 2016.dta") |>
  mutate(across(everything(), haven::zap_labels),  # strip all Stata labels
         year = as.integer(year)) |>
  arrange(year)

cat(". tsdes\n\n")
cat("  Time variable: year,", min(df_ts$year), "to", max(df_ts$year), "\n")
cat("  Delta: 1 year   Observations:", nrow(df_ts), "\n\n")

# Equivalent to: gen trend = _n
df_ts <- df_ts |> mutate(trend = row_number())
attr(df_ts$trend, "label") <- "Time trend (1 to 57)"

# Equivalent to: gen totalpct_lag1 = L.totalpct
df_ts <- df_ts |> mutate(totalpct_lag1 = dplyr::lag(totalpct, 1))
attr(df_ts$totalpct_lag1, "label") <- "Total percent enrolled (t-1)"

# Equivalent to: gen totalpct_diff = D.totalpct
df_ts <- df_ts |>
  mutate(totalpct_diff = totalpct - dplyr::lag(totalpct, 1))
attr(df_ts$totalpct_diff, "label") <- "Change in total percent enrolled"

cat(". summarize\n\n")
print(summary(df_ts))
cat("\n")

haven::write_dta(df_ts, "Example_4_2_2_TS.dta")
cat("file saved: Example_4_2_2_TS.dta\n\n")

# ================================================================
# Best Practices Summary
# ================================================================

# KEY RECOMMENDATIONS FOR DATASET CREATION AND MANAGEMENT (R):
#
# 1. FILE NAMING:
#    - Use descriptive names matching Stata output for cross-platform
#      reproducibility. E.g., "Undergraduate enrollment data - Long.dta"
#
# 2. VARIABLE LABELS:
#    - Set via attr(df$var, "label") <- "..."
#    - haven preserves Stata labels on read_dta/write_dta round-trips
#    - Apply with intersect(names(label_map), names(df)) to avoid errors
#      when column names differ from expected
#
# 3. PANEL DATA:
#    - Convert to long format with tidyr::pivot_longer()
#    - Declare panel with plm::pdata.frame(df, index = c("id","year"))
#    - Use plm::lag() for panel-aware lags (explicitly qualified)
#    - Use plain diff() for differences — it dispatches to diff.pseries
#      via S3; plm::diff is not exported in plm >= 2.6
#
# 4. TIME SERIES:
#    - Sort by time variable with arrange(year)
#    - Use dplyr::lag() explicitly (plm masks lag when both are loaded)
#    - For formal ts objects, use ts() or zoo/xts packages
#
# 5. FILE PATHS:
#    - Use forward slashes (/) — works on all platforms
#    - Set working directory with setwd() or RStudio project (.Rproj)
#
# 6. DATA DOWNLOADS:
#    - Wrap download.file() in tryCatch / safe_download() so script does
#      not halt if a URL is temporarily unavailable
#    - Check file.size() > 0 before reading (failed downloads produce
#      empty files, not errors, on some systems)
#
# 7. NCES EXCEL TABLES:
#    - Read with col_names = FALSE and parse_nces_wide() to handle
#      multi-row headers, duplicate columns, and footnote rows
#
# 8. JOINING DATASETS:
#    - Use dplyr::inner_join(df1, df2, by = c("id","year")) for matched obs
#    - Equivalent to Stata's joinby with unmatched(none)

# ================================================================
# Close log — equivalent to: log close
# ================================================================
cat("Chapter 4 R script completed:", format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n")
sink()

# ================================================================
# END OF CHAPTER 4 R CODE
# ================================================================
