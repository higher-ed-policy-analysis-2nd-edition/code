# ================================================================
# Chapter 4 - Creating Datasets and Managing Data
# Complete R translation of Stata_Code4.do
# Higher Education Policy Analysis Using Quantitative Techniques 
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/ch4
# Author: Marvin A. Titus (original Stata)
# Date: November 17, 2025
# ================================================================

# Script tested in R >= 4.0
# Compatible with R version 4.0 or later

# NOTE: Set working directory if you want to save .dta/.xlsx files persistently:
# ch4data <- "C:/Users/YourName/Documents/book-materials/ch4/data"
# dir.create(ch4data, recursive = TRUE, showWarnings = FALSE)
# setwd(ch4data)

# ----------------------------------------------------------------
# OUTPUT DIRECTORY AND LOG FILE
# ----------------------------------------------------------------

log_dir  <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 4/Output/logs"
log_file <- file.path(log_dir, "Chapter4_R_output.log")

if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = TRUE)
stopifnot("Log directory could not be created — check path and permissions:" =
            dir.exists(log_dir))

# Suppress ANSI colour/cursor escape codes from crayon and cli (used by
# tidyverse). Without these, message() output in the log contains raw
# escape sequences (e.g. "G3;" fragments) instead of plain text.
options(crayon.enabled    = FALSE)
options(cli.num_ansi_colors = 0)

log_con <- file(log_file, open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message", append = TRUE)

cat("Chapter 4 R log\n")
cat("Log file:", log_file, "\n")
cat("Opened: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("-", 60), "\n")

# ----------------------------------------------------------------
# REQUIRED PACKAGES
# ----------------------------------------------------------------
required_packages <- c(
  "tidyverse", "haven", "readxl", "plm", "writexl"
)

new_pkgs <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) {
  cat("Installing missing packages: ", paste(new_pkgs, collapse = ", "), "\n")
  install.packages(new_pkgs, dependencies = TRUE)
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(readxl)
  library(plm)
  library(writexl)
})

# ----------------------------------------------------------------
# HELPER FUNCTIONS
# ----------------------------------------------------------------

# Helper: downcast double to integer when appropriate
downcast_double <- function(x) {
  if (!is.double(x)) return(x)
  tol <- .Machine$double.eps^0.5
  finite_idx <- is.finite(x)
  if (!any(finite_idx)) return(x)
  diffs <- abs(x[finite_idx] - round(x[finite_idx]))
  if (all(diffs < tol)) {
    max_abs <- max(abs(x[finite_idx]), na.rm = TRUE)
    if (!is.na(max_abs) && max_abs < .Machine$integer.max) {
      return(as.integer(x))
    }
  }
  x
}

# Helper: Add state FIPS codes and abbreviations
# This replicates the functionality of Stata's statastates command
add_state_identifiers <- function(df, state_col = "State") {
  # US state reference data
  state_data <- data.frame(
    state_name = state.name,
    state_abbrev = state.abb,
    state_fips = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 
                   23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 
                   39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
    stringsAsFactors = FALSE
  )
  
  # Handle D.C. if present
  if ("District of Columbia" %in% df[[state_col]] || "D.C." %in% df[[state_col]]) {
    state_data <- rbind(
      state_data,
      data.frame(state_name = "District of Columbia", state_abbrev = "DC", 
                 state_fips = 11, stringsAsFactors = FALSE)
    )
  }
  
  # Merge with input data
  df_merged <- df %>%
    left_join(state_data, by = setNames("state_name", state_col))
  
  # Reorder columns to place state identifiers at the front
  state_cols <- c("state_abbrev", "state_fips")
  other_cols <- setdiff(names(df_merged), state_cols)
  df_merged <- df_merged %>% select(all_of(c(state_cols, other_cols)))
  
  return(df_merged)
}

# ----------------------------------------------------------------
# Section 4.2.1: Primary Data
# ----------------------------------------------------------------

cat("\n=== Section 4.2.1: Primary Data ===", "\n")

# Example 4.2.1: Creating a dataset using manual input
# R equivalent of Stata's 'input' command

example_4_2_1 <- data.frame(
  variable_x = c(31, 25, 35, 38, 30),
  variable_y = c(57, 68, 60, 59, 59),
  variable_z = c(18, 12, 13, 17, 15)
)

# Display the data
cat("Example 4.2.1 - Manually created dataset:", "\n")
print(example_4_2_1)

# In R, you can use View() to see data in spreadsheet-like format (interactive)
# View(example_4_2_1)

# Save the dataset or export to CSV
# write_csv(example_4_2_1, "Example_4_2_1.csv")
# write_dta(example_4_2_1, "Example_4_2_1.dta")

# Alternative: Import data from CSV file
# example_4_2_1 <- read_csv("Example_4_2_1.csv")

# ----------------------------------------------------------------
# Section 4.2.2: Secondary Data - Cross-Sectional Dataset
# ----------------------------------------------------------------

cat("\n=== Section 4.2.2: Cross-Sectional Dataset ===", "\n")

# Example: Creating a cross-sectional dataset from NCES Digest Table
# Data source: NCES Digest of Education Statistics, Table 302.50

# Download the reformatted Excel file from GitHub
url_302_50 <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/tabn302_50.xlsx"
dest_302_50 <- file.path(tempdir(), "tabn302_50.xlsx")
download.file(url_302_50, dest_302_50, mode = "wb")

# Import the Excel file
tab302_50 <- read_excel(dest_302_50, sheet = "reformatted")

# View the data structure
cat("Structure of Table 302.50 data:", "\n")
str(tab302_50)

# Add FIPS codes and state abbreviations
# First, ensure we have a State column and create stateid if needed
if ("State" %in% names(tab302_50)) {
  if (!"stateid" %in% names(tab302_50)) {
    tab302_50 <- tab302_50 %>% 
      mutate(stateid = row_number()) %>%
      select(stateid, everything())
  }
  
  # Add state identifiers
  tab302_50 <- add_state_identifiers(tab302_50, state_col = "State")
}

# Add variable labels using attributes
attr(tab302_50$stateid, "label") <- "State id"
attr(tab302_50$state_abbrev, "label") <- "State abbreviation"
attr(tab302_50$state_fips, "label") <- "FIPS code"
attr(tab302_50$State, "label") <- "State name"

if ("Total" %in% names(tab302_50)) {
  attr(tab302_50$Total, "label") <- "Total graduates from HS located in the state"
}
if ("Public" %in% names(tab302_50)) {
  attr(tab302_50$Public, "label") <- "Public graduates from HS located in the state"
}
if ("Private" %in% names(tab302_50)) {
  attr(tab302_50$Private, "label") <- "Private graduates from HS located in the state"
}
if ("anystate" %in% names(tab302_50)) {
  attr(tab302_50$anystate, "label") <- "1st-time freshmen graduating from HS enrolled in any state"
}
if ("homestate" %in% names(tab302_50)) {
  attr(tab302_50$homestate, "label") <- "1st-time freshmen graduating from HS enrolled in home state"
}
if ("anyrate" %in% names(tab302_50)) {
  attr(tab302_50$anyrate, "label") <- "Percent of HS completers enrolled in PSE in any state"
}
if ("homerate" %in% names(tab302_50)) {
  attr(tab302_50$homerate, "label") <- "Percent of HS completers enrolled in PSE in home state"
}

# Apply downcast to appropriate columns
tab302_50 <- tab302_50 %>% mutate(across(where(is.double), ~ downcast_double(.x)))

cat("Cross-sectional dataset created: rows=", nrow(tab302_50), " cols=", ncol(tab302_50), "\n")

# Verify the structure with labels
cat("Final structure:", "\n")
str(tab302_50)

# Save the dataset with a descriptive name
# write_dta(tab302_50, "US high school graduates in 2012 enrolled in PSE, by state.dta")
# write_csv(tab302_50, "US high school graduates in 2012 enrolled in PSE, by state.csv")

# ----------------------------------------------------------------
# Section 4.2.2 (continued): Time-Series Dataset
# ----------------------------------------------------------------

cat("\n=== Section 4.2.2: Time-Series Dataset ===", "\n")

# Example: Creating a time-series dataset (1960-2016)
# Data source: NCES Digest of Education Statistics, Table 302.10

# Download the reformatted Excel file from GitHub
url_302_10 <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/tabn302_10.xlsx"
dest_302_10 <- file.path(tempdir(), "tabn302_10.xlsx")
download.file(url_302_10, dest_302_10, mode = "wb")

# Import the reformatted worksheet
tab302_10 <- read_excel(dest_302_10, sheet = "reformatted")

# Apply downcast to appropriate columns
tab302_10 <- tab302_10 %>% mutate(across(where(is.double), ~ downcast_double(.x)))

# Ensure year is numeric for time-series operations
if ("year" %in% names(tab302_10)) {
  tab302_10 <- tab302_10 %>% mutate(year = as.integer(year))
}

# Add variable labels
if ("year" %in% names(tab302_10)) {
  attr(tab302_10$year, "label") <- "Year"
}
if ("totalpct" %in% names(tab302_10)) {
  attr(tab302_10$totalpct, "label") <- "Percent of HS graduates enrolled in PSE"
}

cat("Time-series dataset created: rows=", nrow(tab302_10), " cols=", ncol(tab302_10), "\n")
cat("Year range:", min(tab302_10$year, na.rm = TRUE), "to",
    max(tab302_10$year, na.rm = TRUE), "\n")

# Save the time-series dataset
# write_dta(tab302_10, "Percent of US high school graduates in PSE, 1960 to 2016.dta")
# write_dta(tab302_10, "Example_4_2_2_TS.dta")
# write_csv(tab302_10, "Example_4_2_2_TS.csv")

# ----------------------------------------------------------------
# Section 4.2.2 (continued): Panel Dataset (Cross-Sectional Time-Series)
# ----------------------------------------------------------------

cat("\n=== Section 4.2.2: Panel Dataset ===", "\n")

# Example: Creating a panel dataset of undergraduate enrollment by state
# Data source: NCES Digest of Education Statistics, Table 304.70

# Download the Excel file from GitHub
url_304_70 <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/tabn304_70.xlsx"
dest_304_70 <- file.path(tempdir(), "tabn304_70.xlsx")
download.file(url_304_70, dest_304_70, mode = "wb")

# Check available sheets
available_sheets <- excel_sheets(dest_304_70)
cat("Available sheets: ", paste(available_sheets, collapse = ", "), "\n")

# Import the undergraduate enrollment worksheet
# Try multiple possible sheet names
undergrad_sheet <- NULL
possible_names <- c("Undergrads", "Undergrad", "Undergraduate", "HSGrad")

for (sheet_name in possible_names) {
  if (sheet_name %in% available_sheets) {
    undergrad_sheet <- sheet_name
    break
  }
}

if (is.null(undergrad_sheet)) {
  # If no expected sheet found, use first sheet
  undergrad_sheet <- available_sheets[1]
  cat("Using first available sheet: ", undergrad_sheet, "\n")
}

tab304_70_wide <- read_excel(dest_304_70, sheet = undergrad_sheet)

cat("Sheet '", undergrad_sheet, "' - wide format: rows=", nrow(tab304_70_wide),
    " cols=", ncol(tab304_70_wide), "\n", sep = "")

# Save the dataset in wide format (optional)
# write_dta(tab304_70_wide, "Undergraduate enrollment data - Wide.dta")

# Reshape from wide to long format
# Identify year columns (could be Ugrad*, HSGrad*, or numeric years)
year_cols <- grep("^(Ugrad|HSGrad|X)[0-9]", names(tab304_70_wide), value = TRUE)

if (length(year_cols) == 0) {
  # Fallback: look for any numeric columns
  year_cols <- names(tab304_70_wide)[sapply(tab304_70_wide, is.numeric)]
  cat("Using numeric columns for reshaping: ", paste(year_cols, collapse = ", "), "\n")
}

# Determine the value name and prefix based on column names
if (any(grepl("^Ugrad", year_cols))) {
  value_name <- "Ugrad"
  name_prefix <- "Ugrad"
} else if (any(grepl("^HSGrad", year_cols))) {
  value_name <- "HSGrad"
  name_prefix <- "HSGrad"
} else {
  value_name <- "value"
  name_prefix <- "X"
}

tab304_70_long <- tab304_70_wide %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = value_name,
    names_prefix = name_prefix
  ) %>%
  mutate(year = as.integer(gsub("[^0-9]", "", year)))

# Apply downcast to appropriate columns
tab304_70_long <- tab304_70_long %>% mutate(across(where(is.double), ~ downcast_double(.x)))

# Ensure id column exists and is integer
if ("id" %in% names(tab304_70_long)) {
  tab304_70_long <- tab304_70_long %>% mutate(id = as.integer(id))
}

cat("Data in long format (from '", undergrad_sheet, "' sheet): rows=",
    nrow(tab304_70_long), " cols=", ncol(tab304_70_long), "\n", sep = "")

# Declare the dataset as panel data using plm
if (all(c("id", "year") %in% names(tab304_70_long))) {
  pdata_ugrad <- pdata.frame(tab304_70_long, index = c("id", "year"))
  
  # Display panel data structure summary
  cat("\nPanel data structure (", undergrad_sheet, " sheet):", "\n")
  cat("Number of unique units (states): ", pdim(pdata_ugrad)$nT$n, "\n")
  cat("Time periods: ", pdim(pdata_ugrad)$nT$T, "\n")
  cat("Balanced panel: ", pdim(pdata_ugrad)$balanced, "\n")
} else {
  pdata_ugrad <- NULL
  warning("Cannot create panel data structure: 'id' and/or 'year' columns missing")
}

# Save the dataset in long format (optional)
# write_dta(tab304_70_long, paste0(undergrad_sheet, " - Long.dta"))

# ----------------------------------------------------------------
# Section 4.2.2 (continued): Merging Multiple Variables into Panel Dataset
# ----------------------------------------------------------------

cat("\n=== Section 4.2.2: Merging Multiple Variables ===", "\n")

# Example: Adding high school graduates data to the panel

# Check if there's a separate HSGrad sheet or if we need to work with existing data
hsgrad_sheet <- NULL
hsgrad_possible_names <- c("HSGrad", "HSGrads", "HS_Grad", "High_School_Graduates")

for (sheet_name in hsgrad_possible_names) {
  if (sheet_name %in% available_sheets) {
    hsgrad_sheet <- sheet_name
    break
  }
}

if (!is.null(hsgrad_sheet) && hsgrad_sheet != undergrad_sheet) {
  # Load separate HSGrad sheet
  tab304_70_hsgrad_wide <- read_excel(dest_304_70, sheet = hsgrad_sheet)
  
  cat("High school graduates data (wide format): rows=", nrow(tab304_70_hsgrad_wide),
      " cols=", ncol(tab304_70_hsgrad_wide), "\n", sep = "")
  
  # Save in wide format (optional)
  # write_dta(tab304_70_hsgrad_wide, "HSGrad - Wide.dta")
  
  # Reshape to long format
  hsgrad_year_cols <- grep("^HSGrad", names(tab304_70_hsgrad_wide), value = TRUE)
  
  tab304_70_hsgrad_long <- tab304_70_hsgrad_wide %>%
    pivot_longer(
      cols = all_of(hsgrad_year_cols),
      names_to = "year",
      values_to = "HSGrad",
      names_prefix = "HSGrad"
    ) %>%
    mutate(year = as.integer(gsub("[^0-9]", "", year)))
  
} else {
  # If HSGrad is the only sheet or same as undergrad sheet, use the already loaded data
  cat("Note: Using data from '", undergrad_sheet, "' sheet for high school graduates", "\n")
  tab304_70_hsgrad_wide <- tab304_70_wide
  tab304_70_hsgrad_long <- tab304_70_long
  
  # Rename the value column to HSGrad if needed
  if (value_name != "HSGrad" && value_name %in% names(tab304_70_hsgrad_long)) {
    tab304_70_hsgrad_long <- tab304_70_hsgrad_long %>%
      rename(HSGrad = all_of(value_name))
  }
}

# Apply downcast to appropriate columns
tab304_70_hsgrad_long <- tab304_70_hsgrad_long %>% 
  mutate(across(where(is.double), ~ downcast_double(.x)))

# Ensure id column exists and is integer
if ("id" %in% names(tab304_70_hsgrad_long)) {
  tab304_70_hsgrad_long <- tab304_70_hsgrad_long %>% mutate(id = as.integer(id))
}

if (!is.null(hsgrad_sheet) && hsgrad_sheet != undergrad_sheet) {
  cat("High school graduates data (long format): rows=", nrow(tab304_70_hsgrad_long),
      " cols=", ncol(tab304_70_hsgrad_long), "\n", sep = "")
} else {
  cat("Using same data structure for both enrollment and HS graduates", "\n")
}

# Declare as panel data
if (all(c("id", "year") %in% names(tab304_70_hsgrad_long))) {
  pdata_hsgrad <- pdata.frame(tab304_70_hsgrad_long, index = c("id", "year"))
} else {
  pdata_hsgrad <- NULL
}

# Save in long format (optional)
# write_dta(tab304_70_hsgrad_long, "HSGrad - Long.dta")

# ----------------------------------------------------------------
# Merge multiple variables
# ----------------------------------------------------------------

cat("\nMerging multiple datasets...", "\n")

# Start with the primary data
complete_panel <- tab304_70_long

# Merge high school graduates data (only if it's from a different sheet)
if (!is.null(hsgrad_sheet) && hsgrad_sheet != undergrad_sheet &&
    all(c("id", "year") %in% names(complete_panel)) && 
    all(c("id", "year") %in% names(tab304_70_hsgrad_long))) {
  
  # Get columns from hsgrad that aren't already in complete_panel (except id and year)
  hsgrad_cols_to_merge <- setdiff(names(tab304_70_hsgrad_long), 
                                   c(names(complete_panel), "id", "year"))
  hsgrad_cols_to_merge <- c("id", "year", hsgrad_cols_to_merge, "HSGrad")
  hsgrad_cols_to_merge <- unique(hsgrad_cols_to_merge)
  hsgrad_cols_to_merge <- hsgrad_cols_to_merge[hsgrad_cols_to_merge %in% names(tab304_70_hsgrad_long)]
  
  complete_panel <- complete_panel %>%
    left_join(
      tab304_70_hsgrad_long %>% select(all_of(hsgrad_cols_to_merge)),
      by = c("id", "year"),
      suffix = c("", "_hsgrad")
    )
  
  cat("Merged high school graduates data from separate sheet", "\n")
} else if (is.null(hsgrad_sheet) || hsgrad_sheet == undergrad_sheet) {
  cat("Single sheet contains all data - no separate merge needed", "\n")
}

# Note: The following datasets need to be downloaded separately:
# - "Undergraduate state financial aid - need.dta"
# - "Undergraduate state financial aid - merit.dta"

# Example merge code for financial aid data (if files are available):
# need_aid <- read_dta("Undergraduate state financial aid - need.dta")
# complete_panel <- complete_panel %>%
#   left_join(need_aid, by = c("id", "year"))
# cat("Merged need-based financial aid data", "\n")

# merit_aid <- read_dta("Undergraduate state financial aid - merit.dta")
# complete_panel <- complete_panel %>%
#   left_join(merit_aid, by = c("id", "year"))
# cat("Merged merit-based financial aid data", "\n")

# Apply downcast to final merged dataset
complete_panel <- complete_panel %>% mutate(across(where(is.double), ~ downcast_double(.x)))

# Declare final dataset as panel data
if (all(c("id", "year") %in% names(complete_panel))) {
  pdata_complete <- pdata.frame(complete_panel, index = c("id", "year"))
  
  # Display panel data structure summary
  cat("\nComplete panel data structure:", "\n")
  cat("Number of unique units (states): ", pdim(pdata_complete)$nT$n, "\n")
  cat("Time periods: ", pdim(pdata_complete)$nT$T, "\n")
  cat("Total observations: ", nrow(complete_panel), "\n")
  cat("Balanced panel: ", pdim(pdata_complete)$balanced, "\n")
} else {
  pdata_complete <- NULL
}

# View the first few observations
cat("\nFirst 10 observations of merged dataset:", "\n")
print(head(complete_panel, 10))

# Save the complete panel dataset (optional)
# write_dta(complete_panel, "Complete_Panel_Dataset.dta")
# write_csv(complete_panel, "Complete_Panel_Dataset.csv")

# ----------------------------------------------------------------
# Summary and Cleanup
# ----------------------------------------------------------------

cat("\n=== Chapter 4 Processing Complete ===", "\n")
cat("\nKey objects created:", "\n")
cat("- example_4_2_1: Manually created dataset (Example 4.2.1)", "\n")
cat("- tab302_50: Cross-sectional HS graduates data (Table 302.50)", "\n")
cat("- tab302_10: Time-series enrollment percentages (Table 302.10)", "\n")
cat("- tab304_70_wide: Undergraduate enrollment, wide format", "\n")
cat("- tab304_70_long: Undergraduate enrollment, long format", "\n")
cat("- pdata_ugrad: Panel data frame for undergraduate enrollment", "\n")
cat("- tab304_70_hsgrad_long: High school graduates, long format", "\n")
cat("- complete_panel: Merged panel dataset", "\n")
cat("- pdata_complete: Panel data frame for complete merged data", "\n")

cat("\nNote: All datasets referenced in this code are available at:", "\n")
cat("https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch4", "\n")
cat("\nFor detailed documentation, see the README.md file in the code repository.", "\n")

# ----------------------------------------------------------------
# Optional: Clean up temporary files
# ----------------------------------------------------------------
# unlink(dest_302_50)
# unlink(dest_302_10)
# unlink(dest_304_70)

# ----------------------------------------------------------------
# CLOSE LOG
# ----------------------------------------------------------------
cat("\nChapter 4 log closed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
sink(type = "message")
sink()
close(log_con)

# ================================================================
# END OF CHAPTER
# ================================================================
