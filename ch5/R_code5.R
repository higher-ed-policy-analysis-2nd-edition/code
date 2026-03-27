# ================================================================
# Chapter 5 - Getting to Know Thy Data
# Complete R Code
# Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-
# edition/tree/main/code/ch5
# Author: Marvin A. Titus
# Date: November 10, 2025
# ================================================================

# Script tested in R 4.4.2
# Compatible with R version 4.2.0 or later

# ===============================================================================
# REQUIRED PACKAGES
# Install once by running the block below, then comment it out
# ===============================================================================

# install.packages(c(
#   "haven",      # read/write Stata .dta files
#   "readxl",     # read Excel files
#   "dplyr",      # data manipulation
#   "tidyr",      # reshaping and pivoting
#   "stringr",    # string operations
#   "forcats",    # factor handling
#   "ggplot2",    # graphics
#   "scales",     # axis formatting in ggplot2
#   "patchwork",  # combine ggplot2 panels
#   "naniar",     # missing data summaries and visualization
#   "mice",       # missing data patterns (md.pattern)
#   "VIM",        # missing data aggregation plots
#   "plm",        # panel data structure (equivalent to xtset)
#   "tigris",     # US state FIPS codes (equivalent to statastates)
#   "pryr"        # object.size (memory inspection)
# ))

library(haven)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(scales)
library(patchwork)
library(naniar)
library(mice)
library(VIM)
library(plm)
library(tigris)

# ===============================================================================
# IMPORTANT: Set working directory (customize this for your system)
# ===============================================================================

# Option 1: set globally at the top of the script
# setwd("C:/Users/YourName/Documents/book-materials/ch5/data")

# Option 2: use here::here() for project-relative paths (recommended)
# library(here)
# data_path <- here("ch5", "data")

# Working directory is set above; suppress getwd() from appearing in the log
# so that machine-specific paths do not clutter the output file.
wd <- getwd()   # stored silently for reference; not printed

# ===============================================================================
# OUTPUT DIRECTORIES AND LOG FILE
# ===============================================================================

# Log file: absolute path to Dropbox location
log_dir  <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 5/Ouput/logs"
log_file <- file.path(log_dir, "Chapter5_R_output.log")


# Create directories if they do not already exist.
# stopifnot() halts with a clear message BEFORE sink is open, so any
# failure is always visible in the console — not silently swallowed.
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = TRUE)
stopifnot("Log directory could not be created — check path and permissions:" =
            dir.exists(log_dir))
message("Log directory verified: ", log_dir)

# Open log — sink() redirects console output to the log file.
# split = TRUE echoes output to the console simultaneously.
# A second sink(type = "message") captures warnings and messages as well.
log_con <- file(log_file, open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message", append = TRUE)

cat("Chapter 5 R log\n")

cat("Log file:  ", log_file, "\n")
cat("Opened:    ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("-", 60), "\n")

# ===============================================================================
# Section 5.2: Getting to Know the Structure of Our Datasets
# ===============================================================================

# ----------------------------------------------------------------
# Time series dataset: inspect structure and storage types
# ----------------------------------------------------------------

# R does not require clearing memory between datasets, but removing
# objects explicitly keeps the workspace clean — equivalent to clear all
rm(list = ls())

# Download the time series dataset from GitHub
# Equivalent to: copy "url" "file.dta", replace
ts_url <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                 "2nd-edition/data/main/ch4/Example_4_2_2_TS.dta")
download.file(ts_url, "Example_4_2_2_TS.dta", mode = "wb")

ts_df <- read_dta("Example_4_2_2_TS.dta")

# Equivalent to: describe
# Shows variable names, types, and labels
str(ts_df)
glimpse(ts_df)    # tidyverse-style compact view

# Equivalent to: compress
# R manages memory automatically; haven imports numeric vars as double by default.
# Inspect current memory usage, then convert types to minimize footprint.
cat("Object size before type conversion:", format(object.size(ts_df), units = "Kb"), "\n")

# Stata stored year as float; convert to integer — equivalent to compress
ts_df <- ts_df |>
  mutate(year = as.integer(year))

cat("Object size after type conversion: ", format(object.size(ts_df), units = "Kb"), "\n")

# Confirm types after conversion — equivalent to second describe
str(ts_df)

# save (commented out — equivalent to * save "...", replace)
# write_dta(ts_df, "Example_4_2_2_TS.dta")

# ----------------------------------------------------------------
# 🔹 Panel dataset: inspect structure, convert types
# ----------------------------------------------------------------

# Download Example_5_0.dta from GitHub
panel_url <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                    "2nd-edition/data/main/ch5/Example_5_0.dta")
download.file(panel_url, "Example_5_0.dta", mode = "wb")

df50 <- read_dta("Example_5_0.dta")

# View storage types — id imported as double (was float in Stata)
str(df50)
glimpse(df50)

cat("Object size before conversion:", format(object.size(df50), units = "Kb"), "\n")

# compress equivalent: convert id and year to integer; trim State string
df50 <- df50 |>
  mutate(
    id    = as.integer(id),   # recast int id — was float → byte → now int
    year  = as.integer(year),
    State = str_trim(State)   # equivalent to string compression
  )

cat("Object size after conversion: ", format(object.size(df50), units = "Kb"), "\n")

str(df50)

# save (commented out)
# write_dta(df50, "Example_5_0.dta")

rm(df50)

# ===============================================================================
# Section 5.2 (continued): SHEEO Finance Data Example
# ===============================================================================

# ----------------------------------------------------------------
# Download and import SHEEO state higher education finance data
# ----------------------------------------------------------------

# Equivalent to: copy "url" "file.xlsx", replace  +  import excel ... firstrow
sheeo_url <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                    "2nd-edition/data/main/ch5/Example_5_1.xlsx")
download.file(sheeo_url, "Example_5_1.xlsx", mode = "wb")

sheeo <- read_excel("Example_5_1.xlsx", sheet = "reformatted", col_names = TRUE)

# Always inspect actual column names immediately after import
cat("\nActual column names in sheeo (from Excel firstrow):\n")
print(names(sheeo))
glimpse(sheeo)

# ----------------------------------------------------------------
# Drop pre-recession years and non-state observations
# ----------------------------------------------------------------

# Equivalent to: drop if FY < 2010
sheeo <- sheeo |> filter(FY >= 2010)

# Inspect FY 2010 — equivalent to: list if FY == 2010
sheeo |> filter(FY == 2010) |> print(n = Inf)

# Drop U.S. aggregate row and D.C. — equivalent to drop if State == "U.S." / "D.C."
sheeo <- sheeo |>
  filter(!State %in% c("U.S.", "D.C."))

# ----------------------------------------------------------------
# 🔹 Create state FIPS codes using tigris (equivalent to statastates)
# ----------------------------------------------------------------

# tigris::fips_codes provides state name, abbreviation, and FIPS code
fips_ref <- fips_codes |>
  distinct(state_name, state_code, state) |>   # one row per state
  rename(State       = state_name,
         state_fips  = state_code,
         state_abbrev = state)

# Left join FIPS codes onto SHEEO data by state name
sheeo <- sheeo |>
  left_join(fips_ref, by = "State") |>
  mutate(state_fips = as.integer(state_fips))

# Alternative: create a sequential numeric state id from state names
# Equivalent to: egen stateid = group(State)
sheeo <- sheeo |>
  mutate(stateid = as.integer(factor(State)))

# ----------------------------------------------------------------
# Declare as panel dataset — equivalent to: xtset state_fips FY, yearly
# ----------------------------------------------------------------

# pdata.frame() from plm creates a panel-aware data frame
sheeo_panel <- pdata.frame(sheeo, index = c("state_fips", "FY"))

# Confirm panel structure
pdim(sheeo_panel)   # equivalent to xtdescribe / xtdes

# save (commented out)
# write_dta(sheeo, "Example_5_2.dta")

rm(fips_ref)

# ===============================================================================
# Section 5.3: Getting to Know Our Data
# ===============================================================================

# ----------------------------------------------------------------
# Loading the HSLS:09 public-use student dataset
# ----------------------------------------------------------------

# Use the public-use HSLS:09 dataset (2017 Student File), available at:
# https://nces.ed.gov/datalab/onlinecodebook
# R has no equivalent to set maxvar; it handles wide datasets natively.
# Keep: STU_ID X1SEX X1RACE X1SES X1SESQ5 X4ATPRLVLA S3CLGPELL P1TUITION
# Alternatively, use the pre-truncated version from GitHub (below).

rm(list = ls())

trunc_url <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                    "2nd-edition/data/main/ch5/Public_use_HSLS_09_truncated.dta")
download.file(trunc_url, "Public_use_HSLS_09_truncated.dta", mode = "wb")

hsls_full <- read_dta("Public_use_HSLS_09_truncated.dta")

keep_vars <- c("STU_ID", "X1SEX", "X1RACE", "X1SES", "X1SESQ5",
               "X4ATPRLVLA", "S3CLGPELL", "P1TUITION")

# Equivalent to: keep STU_ID X1SEX X1RACE ...
hsls_full <- hsls_full |> select(all_of(keep_vars))

# If the full file is unavailable, download the pre-truncated version
ex53_url <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                   "2nd-edition/data/main/ch5/Example_5_3.dta")
download.file(ex53_url, "Example_5_3.dta", mode = "wb")

hsls <- read_dta("Example_5_3.dta")

# ----------------------------------------------------------------
# 🔹 Inspect missing data coding with codebook equivalent
# ----------------------------------------------------------------

# Equivalent to: codebook S3CLGPELL
# Shows type, range, unique values, value labels, and tabulation
str(hsls["S3CLGPELL"])
summary(hsls$S3CLGPELL)

# Full tabulation including NCES special codes (-9, -8, -7, -4)
table(hsls$S3CLGPELL, useNA = "always")

# Display value labels if present (haven imports them as labelled vectors)
attr(hsls$S3CLGPELL, "labels")

# ----------------------------------------------------------------
# Recode NCES missing codes to R NA — equivalent to: mvdecode _all, mv(-9=.)
# ----------------------------------------------------------------

# Replace -9 with NA across all numeric columns
hsls <- hsls |>
  mutate(across(where(is.numeric), ~ na_if(., -9)))

# Verify recoding for S3CLGPELL
table(hsls$S3CLGPELL, useNA = "always")

# save (commented out)
# write_dta(hsls, "Example_5_4.dta")

# ===============================================================================
# Section 5.4: Missing Data Analysis
# ===============================================================================

# ----------------------------------------------------------------
# 🔹 Tabulate missing values — equivalent to: mdesc
# ----------------------------------------------------------------

rm(list = ls())

ex541_url <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                    "2nd-edition/data/main/ch5/Example_5_4_1.dta")
download.file(ex541_url, "Example_5_4_1.dta", mode = "wb")

hsls <- read_dta("Example_5_4_1.dta")

# naniar::miss_var_summary() is the closest equivalent to mdesc
# Returns: variable, n_miss, pct_miss
miss_var_summary(hsls)

# Also display total N alongside missing counts (full mdesc output)
miss_var_summary(hsls) |>
  mutate(n_total    = nrow(hsls),
         n_complete = n_total - n_miss) |>
  select(variable, n_miss, n_total, n_complete, pct_miss) |>
  print(n = Inf)

# ----------------------------------------------------------------
# Explore missingness patterns — equivalent to: misstable tree / patterns
# ----------------------------------------------------------------

# mice::md.pattern() — equivalent to misstable patterns
# Rows = unique patterns; 1 = observed, 0 = missing; rightmost col = # missing vars
md.pattern(hsls, rotate.names = TRUE)

# VIM::aggr() — visual equivalent to misstable tree (shows marginal % + combinations)
aggr(hsls,
     col      = c("steelblue", "tomato"),
     numbers  = TRUE,
     sortVars = TRUE,
     labels   = names(hsls),
     cex.axis = 0.7,
     gap      = 3,
     ylab     = c("Proportion missing", "Pattern"))

# naniar upset plot — shows combinations of missingness (like misstable patterns)
gg_miss_upset(hsls)

# ===============================================================================
# Section 5.4 (continued): Missing Data by Categorical Variables
# ===============================================================================

# ----------------------------------------------------------------
# 🔹 Missingness by subgroup — equivalent to: bysort X1SESQ5 : missings table
# ----------------------------------------------------------------

# Equivalent to: bysort X1SESQ5 : missings table
# Counts and proportions of missing values within each SES quintile
hsls |>
  group_by(X1SESQ5) |>
  summarise(
    n_obs        = n(),
    across(where(is.numeric),
           list(n_miss   = ~ sum(is.na(.)),
                pct_miss = ~ mean(is.na(.)) * 100),
           .names = "{.col}_{.fn}")
  ) |>
  print(n = Inf)

# Equivalent to: bysort X1RACE : missings table
hsls |>
  group_by(X1RACE) |>
  summarise(
    n_obs        = n(),
    across(where(is.numeric),
           list(n_miss   = ~ sum(is.na(.)),
                pct_miss = ~ mean(is.na(.)) * 100),
           .names = "{.col}_{.fn}")
  ) |>
  print(n = Inf)

# ===============================================================================
# Section 5.4 (continued): Panel Missing Analysis — xtmis equivalent (Legacy)
# ===============================================================================

# ----------------------------------------------------------------
# Missing values by panel unit — equivalent to: xtmis grantlow, id(unitid_str)
# ----------------------------------------------------------------

# Note: xtmis (Nguyen 2008) reports missing obs by group in Stata.
# The R equivalent is a grouped summary. xtmis has been superseded by
# xtmispanel (Roudane 2026); see Section 5.4.2 below for the full R equivalent.

ex54_url <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                   "2nd-edition/data/main/ch5/Example_5_4.dta")
download.file(ex54_url, "Example_5_4.dta", mode = "wb")

ipeds <- read_dta("Example_5_4.dta")

# Create string unitid (equivalent to: tostring unitid, generate(unitid_str))
ipeds <- ipeds |>
  mutate(unitid_str = as.character(unitid))

# xtmis equivalent: missingness frequency by institution
ipeds |>
  group_by(unitid_str) |>
  summarise(
    obs      = n(),
    n_miss   = sum(is.na(grantlow)),
    pct_miss = mean(is.na(grantlow)) * 100,
    n_obs    = sum(!is.na(grantlow)),
    pct_obs  = mean(!is.na(grantlow)) * 100,
    .groups  = "drop"
  ) |>
  arrange(desc(pct_miss)) |>
  print(n = Inf)

# ===============================================================================
# Section 5.4.1: Testing for Missing Completely at Random (MCAR)
# ===============================================================================

# ----------------------------------------------------------------
# 🔹 Little's MCAR test — equivalent to: mcartest S3CLGPELL P1TUITION
# ----------------------------------------------------------------

# Reload and recode Example_5_3
ex53_url <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                   "2nd-edition/data/main/ch5/Example_5_3.dta")
download.file(ex53_url, "Example_5_3.dta", mode = "wb")

hsls_mcar <- read_dta("Example_5_3.dta") |>
  mutate(across(where(is.numeric), ~ na_if(., -9)))

# Test 1: Little's MCAR test (equal variances) — equivalent to: mcartest S3CLGPELL P1TUITION
# naniar::mcar_test() implements Little (1988)
mcar_result <- mcar_test(hsls_mcar |> select(S3CLGPELL, P1TUITION))
print(mcar_result)
cat("Chi-squared:", mcar_result$statistic, "\n")
cat("df:         ", mcar_result$df,        "\n")
cat("p-value:    ", mcar_result$p.value,   "\n")

# Test 2: MCAR with unequal variances — equivalent to: mcartest ..., unequal
# naniar::mcar_test() uses maximum likelihood which accommodates unequal covariances.
# For an explicit unequal-variance formulation, re-run with the full variable set:
mcar_result_full <- mcar_test(
  hsls_mcar |> select(S3CLGPELL, P1TUITION, X1SEX, X1RACE, X1SES, X1SESQ5, X4ATPRLVLA)
)
print(mcar_result_full)

# Test 3: Covariate-dependent missingness (CDM) — equivalent to:
#   mcartest S3CLGPELL P1TUITION = i.X1RACE if X1RACE != ., unequal emoutput nolog
# Approach: logistic regression of each variable's missingness indicator on X1RACE

hsls_cdm <- hsls_mcar |>
  filter(!is.na(X1RACE)) |>
  mutate(
    miss_S3CLGPELL = as.integer(is.na(S3CLGPELL)),
    miss_P1TUITION = as.integer(is.na(P1TUITION)),
    X1RACE         = factor(X1RACE)
  )

# CDM logistic model for S3CLGPELL
cdm_s3 <- glm(miss_S3CLGPELL ~ X1RACE, data = hsls_cdm, family = binomial)
cat("\n--- CDM: S3CLGPELL ~ X1RACE ---\n")
summary(cdm_s3)

# CDM logistic model for P1TUITION
cdm_p1 <- glm(miss_P1TUITION ~ X1RACE, data = hsls_cdm, family = binomial)
cat("\n--- CDM: P1TUITION ~ X1RACE ---\n")
summary(cdm_p1)

# Likelihood-ratio test (chi-squared) for each CDM model
# Equivalent to the CDM chi-square distance reported by mcartest
cdm_s3_null <- glm(miss_S3CLGPELL ~ 1, data = hsls_cdm, family = binomial)
cdm_p1_null <- glm(miss_P1TUITION ~ 1, data = hsls_cdm, family = binomial)

cat("\nCDM LR test — S3CLGPELL:\n")
print(anova(cdm_s3_null, cdm_s3, test = "LRT"))

cat("\nCDM LR test — P1TUITION:\n")
print(anova(cdm_p1_null, cdm_p1, test = "LRT"))

# ===============================================================================
# Section 5.4.2: Panel-Specific Missing Data Analysis — xtmispanel equivalent
# ===============================================================================

# ----------------------------------------------------------------
# Setup: load SHEEO panel
# ----------------------------------------------------------------

# xtmispanel (Roudane 2026) has no direct CRAN equivalent as of this writing.
# The three modules demonstrated in the chapter are replicated below using
# combinations of dplyr, naniar, ggplot2, and plm.
# Results are substantively identical to the Stata output.

ex52_url <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                   "2nd-edition/data/main/ch5/Example5_2.dta")
download.file(ex52_url, "Example_5_2.dta", mode = "wb")

sheeo <- read_dta("Example_5_2.dta")

# Note on panel range: the GitHub Example5_2.dta spans FY 1980–2024. A locally
# generated file (from Section 5.2 with filter(FY >= 2010)) covers FY 2010–2024.
# The full-range file is used here because it contains pre-2001 observations
# where Appropriations is missing, enabling a meaningful missing-data demonstration.
# Filter with: sheeo <- sheeo |> filter(FY >= 2010)  if you prefer the
# chapter's stated panel (FY 2010–2024, N = 750, all variables complete).

# ── Print column names so users can verify the loaded data ──────────────────
# The chapter uses "NetTuition" (Stata abbreviation for NetTuitionandFeeRevenue)
# and "Appropriations" as the two demonstration variables. The actual column
# names in the R data frame are the full Excel header strings.
cat("\nActual column names in sheeo:\n")
print(names(sheeo))
glimpse(sheeo)

# Equivalent to chapter's: xtmispanel NetTuition Appropriations, detect/test/graph
# "NetTuition" in Stata abbreviates "NetTuitionandFeeRevenue"; use the full
# name in R. Update these if your local Example_5_2.dta uses different headers.
vars_of_interest <- c("NetTuitionandFeeRevenue", "Appropriations")

# Derive all numeric finance variables (used for Test 2 auxiliary predictors)
id_cols      <- c("state_fips", "FY", "stateid", "state_abbrev", "state", "State")
vars_numeric <- sheeo |>
  select(-any_of(id_cols)) |>
  select(where(is.numeric)) |>
  names()

cat("\nVars of interest (chapter Section 5.4.2):", vars_of_interest, "\n")
cat("All numeric vars:", vars_numeric, "\n")

# Declare as panel — equivalent to: xtset state_fips FY, yearly
sheeo_panel <- pdata.frame(sheeo, index = c("state_fips", "FY"))
pdim(sheeo_panel)

# ----------------------------------------------------------------
# 🔹 Module 1: Detection — equivalent to: xtmispanel ..., detect
# ----------------------------------------------------------------

# --- Table 1: Variable-level missingness summary ---
cat("\n=== MODULE 1: DETECTION ===\n")
cat("\n--- Table 1: By Variable ---\n")

miss_by_var <- sheeo |>
  select(all_of(vars_of_interest)) |>
  miss_var_summary() |>
  mutate(n_total    = nrow(sheeo),
         n_complete = n_total - n_miss) |>
  select(variable, n_miss, n_total, n_complete, pct_miss)

print(miss_by_var)

# --- Table 2: By panel unit (state) ---
cat("\n--- Table 2: By Panel Unit (State) ---\n")

miss_by_state <- sheeo |>
  group_by(state_fips) |>
  summarise(
    across(all_of(vars_of_interest),
           list(n_miss   = ~ sum(is.na(.)),
                pct_miss = ~ round(mean(is.na(.)) * 100, 2)),
           .names = "{.col}_{.fn}"),
    n_periods = n(),
    .groups   = "drop"
  ) |>
  arrange(desc(.data[[paste0(vars_of_interest[1], "_pct_miss")]]))

print(miss_by_state, n = Inf)

# --- Table 3: By time period (fiscal year) ---
cat("\n--- Table 3: By Time Period (FY) ---\n")

miss_by_fy <- sheeo |>
  group_by(FY) |>
  summarise(
    across(all_of(vars_of_interest),
           list(n_miss   = ~ sum(is.na(.)),
                pct_miss = ~ round(mean(is.na(.)) * 100, 2)),
           .names = "{.col}_{.fn}"),
    n_states  = n(),
    .groups   = "drop"
  ) |>
  arrange(FY)

print(miss_by_fy, n = Inf)

# ----------------------------------------------------------------
# 🔹 Module 2: Mechanism test — equivalent to: xtmispanel ..., test
# ----------------------------------------------------------------

cat("\n=== MODULE 2: MECHANISM TESTS ===\n")

# Test 1: Little's MCAR test
# Requires ≥2 variables with missing values; NetTuition is complete so
# this test cannot run — equivalent to Stata's "insufficient variation" message.
cat("\n--- Test 1: Little's MCAR test ---\n")

n_incomplete_vars <- sheeo |>
  select(all_of(vars_of_interest)) |>
  summarise(across(everything(), ~ any(is.na(.)))) |>
  sum()

if (n_incomplete_vars >= 2) {
  mcar_panel <- mcar_test(sheeo |> select(all_of(vars_of_interest)))
  cat("Chi-squared:", mcar_panel$statistic, "\n")
  cat("df:         ", mcar_panel$df,        "\n")
  cat("p-value:    ", mcar_panel$p.value,   "\n")
} else {
  cat("Test 1 cannot run: fewer than 2 variables have missing values.\n")
  cat(paste0("(Only 1 of ", length(vars_of_interest), " variables has missing data.)\n"))
}

# Test 2: MAR logistic regression — equivalent to xtmispanel's Test 2
# Regress each variable's missingness indicator on all other observed variables.
# Predictors: complete auxiliary vars only (no co-missing vars, no state FE).
cat("\n--- Test 2: MAR Logistic Regression ---\n")

target_var <- vars_of_interest[1]           # Appropriations
miss_col   <- paste0("miss_", target_var)

# Restrict auxiliary predictors to vars that are complete across all rows
# (co-missing predictors cause a constant outcome → non-convergence)
other_vars <- setdiff(vars_numeric, target_var)
complete_other_vars <- other_vars[
  sapply(other_vars, function(v) !any(is.na(sheeo[[v]])))
]

# Exclude FY if it perfectly predicts missingness (complete separation)
sheeo_test <- sheeo |>
  mutate(!!miss_col := as.integer(is.na(.data[[target_var]])))

fy_sep <- sheeo_test |>
  group_by(FY) |>
  summarise(r = mean(.data[[miss_col]]), .groups = "drop") |>
  pull(r) |> (\(x) all(x %in% c(0, 1)))()

predictor_vars <- if (fy_sep) complete_other_vars else c("FY", complete_other_vars)

predictor_formula <- paste(miss_col, "~", paste(predictor_vars, collapse = " + "))
cat("MAR logistic formula:", predictor_formula, "\n")

mar_model <- glm(as.formula(predictor_formula), data = sheeo_test, family = binomial)
mar_null  <- glm(as.formula(paste(miss_col, "~ 1")),  data = sheeo_test, family = binomial)

lr_test   <- anova(mar_null, mar_model, test = "LRT")
chi2_val  <- lr_test$Deviance[2]
p_val     <- lr_test$`Pr(>Chi)`[2]
pseudo_r2 <- 1 - as.numeric(logLik(mar_model)) / as.numeric(logLik(mar_null))

cat(sprintf("Variable: %s  chi2 = %.2f  p = %.4f  Pseudo-R² = %.4f\n",
            target_var, chi2_val, p_val, pseudo_r2))
cat(sprintf("Conclusion: %s\n",
            ifelse(p_val < 0.05, "NOT MCAR (reject H0)", "Cannot reject MCAR")))

# Test 3: Pattern classification (monotone vs. arbitrary / non-monotone)
cat("\n--- Test 3: Missingness Pattern Classification ---\n")

# md.pattern() from mice identifies monotone vs. non-monotone (arbitrary) patterns
pattern_mat <- md.pattern(sheeo |> select(all_of(vars_of_interest)),
                          plot = FALSE)
cat("Missing-value pattern matrix:\n")
print(pattern_mat)

# Check for monotone pattern: missingness in var j implies missingness in var k
# (i.e., columns of the pattern matrix are nested)
is_monotone <- function(pat_mat) {
  # Remove summary rows/cols; check if patterns are monotone
  p <- pat_mat[-nrow(pat_mat), -ncol(pat_mat)]
  all(apply(p, 1, function(x) all(diff(x) >= 0) | all(diff(x) <= 0)))
}

pattern_type <- if (is_monotone(pattern_mat)) "Monotone" else "Arbitrary (Non-monotone)"
cat(sprintf("Pattern type: %s\n", pattern_type))

# Overall mechanism summary
cat("\n=== OVERALL RECOMMENDATION ===\n")
# Overall recommendation mirroring xtmispanel's OVERALL RECOMMENDATION output
mechanism <- case_when(
  n_incomplete_vars < 2 ~ "Test 1 not computable; see Test 2 results",
  p_val >= 0.05         ~ "MCAR (cannot reject H0)",
  pseudo_r2 < 0.10      ~ "Possibly MAR",
  TRUE                  ~ "Possibly MNAR"
)
cat(sprintf("\nOverall mechanism: %s\n", mechanism))
cat(sprintf("Pattern type: %s\n", pattern_type))
cat(sprintf("If MNAR: consider selection models or sensitivity analysis.\n"))
cat(sprintf("If MAR / MNAR with non-monotone pattern: MICE recommended.\n"))

# ----------------------------------------------------------------
# 🔹 Module 5: Visualization — equivalent to: xtmispanel ..., graph
# ----------------------------------------------------------------

# --- xtmis_heatmap: panel unit × time period missingness heatmap ---
# Equivalent to graph display xtmis_heatmap

heatmap_data <- sheeo |>
  select(state_fips, FY, any_of(vars_of_interest)) |>
  pivot_longer(cols = -c(state_fips, FY),
               names_to  = "variable",
               values_to = "value") |>
  group_by(state_fips, FY) |>
  summarise(pct_missing = mean(is.na(value)) * 100, .groups = "drop")

p_heatmap <- ggplot(heatmap_data,
                    aes(x = factor(FY), y = factor(state_fips),
                        fill = pct_missing)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "steelblue", high = "tomato",
                      name = "% Missing",
                      limits = c(0, 100)) +
  labs(
    title    = "Missingness Heatmap: SHEEO Panel",
    subtitle = paste("Variables:", paste(vars_of_interest, collapse = ", ")),
    x        = "Fiscal Year",
    y        = "State FIPS Code"
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y  = element_text(size = 6),
        panel.grid   = element_blank())

print(p_heatmap)

# --- Bar chart: missingness by variable ---
p_barvar <- sheeo |>
  select(all_of(vars_of_interest)) |>
  miss_var_summary() |>
  ggplot(aes(x = reorder(variable, pct_miss), y = pct_miss)) +
  geom_col(fill = "steelblue", width = 0.6) +
  coord_flip() +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title = "Missingness by Variable",
       x = NULL, y = "% Missing") +
  theme_minimal(base_size = 10)

# --- Bar chart: missingness by panel unit (top 15 states) ---
# Use the first variable with missing values for the panel-unit bar chart
pct_col_name <- paste0(vars_of_interest[1], "_pct_miss")

p_barpanel <- miss_by_state |>
  slice_max(.data[[pct_col_name]], n = 15) |>
  ggplot(aes(x = reorder(factor(state_fips), .data[[pct_col_name]]),
             y = .data[[pct_col_name]])) +
  geom_col(fill = "tomato", width = 0.6) +
  coord_flip() +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title = paste("Top 15 States:", vars_of_interest[1], "Missingness"),
       x = "State FIPS", y = "% Missing") +
  theme_minimal(base_size = 10)

# --- Bar chart: missingness by time period ---
p_bartime <- miss_by_fy |>
  ggplot(aes(x = FY, y = .data[[pct_col_name]])) +
  geom_col(fill = "darkorange", width = 0.7) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title = paste(vars_of_interest[1], "Missingness by Fiscal Year"),
       x = "Fiscal Year", y = "% Missing") +
  theme_minimal(base_size = 10)

# --- Combined dashboard (xtmis_combined equivalent) ---
# patchwork assembles multiple ggplot panels into one figure
p_combined <- (p_heatmap | (p_barvar / p_barpanel / p_bartime)) +
  plot_annotation(
    title    = "xtmispanel Dashboard — SHEEO Panel Missing Data Diagnostics",
    subtitle = paste0(pdim(sheeo_panel)$n, " states × FY ",
                      min(sheeo$FY), "\u2013", max(sheeo$FY),
                      " | Variables: ", paste(vars_of_interest, collapse = ", "))
  ) &
  theme(plot.title    = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 10))

print(p_combined)


# ===============================================================================
# END OF CHAPTER 5 CODE
# ===============================================================================

# KEY RECOMMENDATIONS FOR GETTING TO KNOW THY DATA IN R:
#
# 1. STORAGE TYPES AND MEMORY:
#    - haven::read_dta() imports everything as double by default; use
#      as.integer(), as.character() etc. to convert as needed
#    - Use object.size() or lobstr::obj_size() to inspect memory footprint
#    - R manages memory automatically; no equivalent to Stata's compress is
#      required, but type conversion (double → integer) saves space on large files
#
# 2. MISSING DATA CODING IN SECONDARY DATA:
#    - Use attr(df$var, "labels") to inspect NCES value labels imported by haven
#    - Use na_if(., -9) to replace a single code, or:
#        mutate(across(where(is.numeric), ~ na_if(., -9)))
#      to replace -9 with NA across all numeric columns at once
#    - Check: table(df$var, useNA = "always") before and after recoding
#
# 3. MISSING DATA ANALYSIS TOOLS — CROSS-SECTIONAL / SURVEY DATA:
#    - naniar::miss_var_summary()     → mdesc
#    - mice::md.pattern()             → misstable patterns / misstable tree
#    - VIM::aggr()                    → combined visual summary
#    - naniar::gg_miss_upset()        → upset plot of missingness combinations
#    - group_by() + summarise(is.na)  → bysort X1SESQ5 : missings table
#    - Install: install.packages(c("naniar", "mice", "VIM"))
#
# 4. MISSING DATA ANALYSIS TOOLS — PANEL DATA:
#    - Grouped dplyr summaries        → xtmis (legacy)
#    - xtmispanel has no CRAN equivalent as of early 2026; the detect /
#      test / graph modules are replicated above using dplyr + naniar + ggplot2
#    - plm::pdata.frame()             → xtset (declares panel structure)
#    - plm::pdim()                    → xtdescribe / xtdes
#
# 5. MCAR TESTING:
#    - naniar::mcar_test()            → mcartest (basic Little's MCAR)
#    - glm(miss_var ~ covariates)     → mcartest ..., unequal / CDM variant
#    - anova(null_model, full_model, test = "LRT") → LR chi-squared for CDM
#    - p < 0.05 in mcar_test() → reject MCAR; consider MAR or MNAR
#
# 6. PANEL STRUCTURE BEST PRACTICES:
#    - Always declare panel with pdata.frame(df, index = c("panelvar", "timevar"))
#    - Use pdim() to check balance (equivalent to xtdescribe)
#    - Panel operators in plm: lag(x, 1), diff(x, 1) (equivalent to L.x, D.x)
#    - For state-level data, use FIPS codes from tigris::fips_codes for merges
#
# 7. DATA SOURCES IN THIS CHAPTER:
#    - SHEEO SHEF data: https://shef.sheeo.org/data-downloads/
#    - HSLS:09 public-use file: https://nces.ed.gov/datalab/onlinecodebook
#    - All example datasets: https://github.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/
#
# 8. KEY R PACKAGES AND STATA EQUIVALENTS:
#    - haven        → use / save (.dta files)
#    - readxl       → import excel
#    - dplyr        → keep / drop / gen / replace / bysort
#    - naniar       → mdesc / missingplot / xtmispanel (detect + graph)
#    - mice         → misstable patterns / mi (multiple imputation)
#    - VIM          → misstable tree (visual)
#    - plm          → xtset / xtdescribe / xtreg
#    - tigris       → statastates
#    - ggplot2      → twoway / graph export
#    - patchwork    → graph combine

# ===============================================================================
# CLOSE LOG
# ===============================================================================
cat("\nChapter 5 log closed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
sink(type = "message")   # close message sink first
sink()                   # close output sink
close(log_con)           # release file handle
