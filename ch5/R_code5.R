# ============================================================================
# Chapter 5 - Getting to Know Thy Data
# R Translation of Complete Stata Code
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch5
# Author: Marvin A. Titus
# Date: November 10, 2025
# NOTE: Code development was assisted by Claude (Anthropic). The author
#       provided specifications and reviewed, tested, and validated all code.
# ============================================================================

# Script tested in R 4.4.x
# Required packages: haven, readxl, dplyr, tidyr, plm,
#                    naniar, mice, ggplot2, scales

# ----------------------------------------------------------------------------
# Install any missing packages (run once)
# ----------------------------------------------------------------------------
install_if_missing <- function(pkgs) {
  to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(to_install) > 0)
    install.packages(to_install, dependencies = TRUE)
}

install_if_missing(c("haven", "readxl", "dplyr", "tidyr", "plm",
                      "naniar", "mice", "ggplot2", "scales"))

suppressPackageStartupMessages({
  library(haven)    # read_dta / write_dta
  library(readxl)   # read_excel
  library(dplyr)    # data manipulation
  library(tidyr)    # pivot / complete
  library(plm)      # pdata.frame / panel diagnostics
  library(naniar)   # miss_var_summary, gg_miss_var, miss_case_table
  library(mice)     # md.pattern  — replaces: misstable patterns
  library(ggplot2)  # all graphs
  library(scales)   # percent_format
})

# ============================================================================
# GLOBAL GGPLOT2 THEME
# Monochrome; approximates Stata s2mono for Springer B&W print.
# ============================================================================

theme_springer <- function(base_size = 11) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold", hjust = 0.5, size = base_size),
      plot.subtitle     = element_text(hjust = 0.5,   size = base_size - 1),
      plot.caption      = element_text(hjust = 0,     size = base_size - 3),
      legend.background = element_rect(fill = "white", color = NA),
      strip.background  = element_rect(fill = "grey90", color = "grey50")
    )
}
theme_set(theme_springer())

# ============================================================================
# WORKING DIRECTORY AND OUTPUT PATHS
# Paths switch automatically by username — mirrors the Stata logic.
# ============================================================================

user <- Sys.info()[["user"]]

if (user == "marvi") {
  graphs_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 5/Output/graphs"
  log_path   <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 5/Output/logs/Chapter5_R_output.log"
} else {
  graphs_dir <- "Output/graphs"
  log_path   <- "Output/logs/Chapter5_R_output.log"
}
dir.create(graphs_dir,        showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)

# Open log — equivalent to: log using "...", replace text
sink(log_path, split = TRUE)
cat("Chapter 5 log opened:", format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n")
cat("Graphs directory:    ", graphs_dir, "\n\n")

options(warn = 1)   # print warnings immediately (Stata default)

# ----------------------------------------------------------------
# Helper: safe download (continues if URL is unreachable)
# ----------------------------------------------------------------
safe_download <- function(url, dest) {
  tryCatch(
    download.file(url, dest, mode = "wb", quiet = TRUE),
    error   = function(e) message("  [download failed — ", basename(dest), "]: ",
                                   conditionMessage(e)),
    warning = function(w) message("  [download warning — ", basename(dest), "]: ",
                                   conditionMessage(w))
  )
}

# ----------------------------------------------------------------
# Helper: save ggplot to graphs_dir
# Saves to tempdir() first then copies to Dropbox to avoid sync-lock
# overwrite failures. print() sends plot to RStudio Plots pane.
# ----------------------------------------------------------------
save_fig <- function(plot, filename, width_px = 1200, height_px = 900, dpi = 150) {
  final_path <- file.path(graphs_dir, filename)
  dir.create(graphs_dir, showWarnings = FALSE, recursive = TRUE)

  # 1. Print to RStudio Plots pane (screen device)
  print(plot)

  # 2. Save to local temp file first — avoids Dropbox file-lock blocking overwrite
  tmp_path <- file.path(tempdir(), filename)
  ggplot2::ggsave(filename = tmp_path,
                  plot     = plot,
                  width    = width_px / dpi,
                  height   = height_px / dpi,
                  dpi      = dpi,
                  device   = "png")

  # 3. Copy from temp to Dropbox destination, overwriting any locked file
  ok <- file.copy(from = tmp_path, to = final_path, overwrite = TRUE)
  if (ok) {
    cat("file", final_path, "saved as PNG format\n")
  } else {
    cat("WARNING: temp file created but copy to Dropbox failed:", final_path, "\n")
    cat("  Temp file available at:", tmp_path, "\n")
  }
}

# ============================================================================
# Section 5.2: Getting to Know the Structure of Our Datasets
# ============================================================================
cat("*===============================================================================\n")
cat("* Section 5.2: Getting to Know the Structure of Our Datasets\n")
cat("*===============================================================================\n\n")

# ----------------------------------------------------------------
# Time series dataset: describe and compress
# Equivalent to: use "Example_4_2_2_TS.dta" + describe + compress
# ----------------------------------------------------------------
safe_download(
  "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/Example_4_2_2_TS.dta",
  "Example_4_2_2_TS.dta")

df_ts <- haven::read_dta("Example_4_2_2_TS.dta") |>
  mutate(across(everything(), haven::zap_labels))

# Equivalent to: describe
cat(". describe (Example_4_2_2_TS.dta)\n\n")
cat(sprintf("  Observations: %d\n  Variables:    %d\n\n",
            nrow(df_ts), ncol(df_ts)))
for (v in names(df_ts)) {
  cat(sprintf("  %-15s  type: %-10s  range: %s to %s\n",
              v, class(df_ts[[v]])[1],
              format(min(df_ts[[v]], na.rm = TRUE)),
              format(max(df_ts[[v]], na.rm = TRUE))))
}
cat("\n")

# Equivalent to: compress
# In R there is no in-place compression; we report optimal storage types.
cat(". compress (R equivalent: report storage types and suggest optimisations)\n\n")
for (v in names(df_ts)) {
  x   <- df_ts[[v]]
  cur <- class(x)[1]
  opt <- if (is.numeric(x) && all(x == as.integer(x), na.rm = TRUE)) "integer" else cur
  if (cur != opt) {
    df_ts[[v]] <- as.integer(x)
    cat(sprintf("  variable %s was %s, now %s\n", v, cur, opt))
  }
}
cat("\n")

cat(". describe (after compress)\n\n")
for (v in names(df_ts)) {
  cat(sprintf("  %-15s  type: %s\n", v, class(df_ts[[v]])[1]))
}
cat("\n")

# ----------------------------------------------------------------
# Panel dataset: describe, compress, and recast
# Equivalent to: use "Example_5_0.dta" + describe + compress + recast int id
# ----------------------------------------------------------------
safe_download(
  "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_0.dta",
  "Example_5_0.dta")

df_50 <- haven::read_dta("Example_5_0.dta") |>
  mutate(across(everything(), haven::zap_labels))

cat(". describe (Example_5_0.dta — before compress)\n\n")
cat(sprintf("  Observations: %d\n  Variables:    %d\n\n",
            nrow(df_50), ncol(df_50)))
for (v in names(df_50)) {
  cat(sprintf("  %-15s  type: %-12s  label: %s\n",
              v, class(df_50[[v]])[1],
              ifelse(!is.null(attr(df_50[[v]], "label")),
                     attr(df_50[[v]], "label"), "")))
}
cat("\n")

# Equivalent to: compress
cat(". compress\n\n")
for (v in names(df_50)) {
  x   <- df_50[[v]]
  cur <- class(x)[1]
  if (is.numeric(x) && !is.character(x)) {
    if (all(x == as.integer(x), na.rm = TRUE)) {
      rng <- range(x, na.rm = TRUE)
      opt <- if (rng[1] >= -128   && rng[2] <= 127)   "byte (int8)"
             else if (rng[1] >= -32768 && rng[2] <= 32767) "integer"
             else "integer"
      if (cur != "integer") {
        df_50[[v]] <- as.integer(x)
        cat(sprintf("  variable %s was %s, now integer\n", v, cur))
      }
    }
  }
}
cat("\n")

# Equivalent to: recast int id
# compress already coerced id to integer; confirm explicitly
df_50$id <- as.integer(df_50$id)
cat(". recast int id\n\n")

cat(". describe (after compress + recast)\n\n")
for (v in names(df_50)) {
  cat(sprintf("  %-15s  type: %s\n", v, class(df_50[[v]])[1]))
}
cat("\n")

# ============================================================================
# Section 5.2 (continued): SHEEO Finance Data Example
# ============================================================================
cat("*===============================================================================\n")
cat("* Section 5.2 (continued): SHEEO Finance Data Example\n")
cat("*===============================================================================\n\n")

# Download and import SHEEO data
# Equivalent to: copy "..." + import excel, sheet("reformatted") firstrow
safe_download(
  "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_1.xlsx",
  "Example_5_1.xlsx")

if (file.exists("Example_5_1.xlsx") && file.size("Example_5_1.xlsx") > 0) {
  sheets_51 <- excel_sheets("Example_5_1.xlsx")
  sheet_51  <- sheets_51[grep("reformat", sheets_51, ignore.case = TRUE)[1]]
  if (is.na(sheet_51)) sheet_51 <- sheets_51[1]
  df_sheeo <- read_excel("Example_5_1.xlsx", sheet = sheet_51)
} else {
  cat("  [Using synthetic SHEEO data]\n\n")
  state_names_51 <- c(state.name, "District of Columbia")
  fips_51 <- c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,
               24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
               42,44,45,46,47,48,49,50,51,53,54,55,56)
  df_sheeo <- expand.grid(State = state_names_51, FY = 2010:2024,
                          stringsAsFactors = FALSE) |>
    mutate(
      NetTuition    = round(runif(n(), 1e8, 5e9)),
      StateTaxFunds = round(runif(n(), 1e8, 8e9)),
      StatePubFunds = round(runif(n(), 1e6, 1e9)),
      NetFTEStudent = round(runif(n(), 1e4, 2e6))
    )
}

# Equivalent to: drop if FY < 2010
df_sheeo <- df_sheeo |> filter(FY >= 2010)

# Equivalent to: list if FY == 2010
cat(". list if FY == 2010\n\n")
print(df_sheeo |> filter(FY == 2010) |> as.data.frame())
cat("\n")

# Equivalent to: drop if State == "U.S." / "D.C."
df_sheeo <- df_sheeo |>
  filter(!State %in% c("U.S.", "D.C."))

# Equivalent to: statastates, name(State) nogenerate
# Join FIPS codes and abbreviations from built-in lookup
state_lookup <- data.frame(
  State        = c(state.name, "District of Columbia"),
  state_abbrev = c(state.abb, "DC"),
  state_fips   = c(1,2,4,5,6,8,9,10,12,13,15,16,17,18,19,20,21,22,23,
                   24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
                   42,44,45,46,47,48,49,50,51,53,54,55,56,11),
  stringsAsFactors = FALSE
)
df_sheeo <- df_sheeo |> left_join(state_lookup, by = "State")

# Equivalent to: compress
df_sheeo <- df_sheeo |>
  mutate(across(where(is.numeric), function(x) {
    if (all(x == as.integer(x), na.rm = TRUE)) as.integer(x) else x
  }))

# Equivalent to: xtset state_fips FY, yearly
df_sheeo <- df_sheeo |>
  mutate(state_fips = as.integer(state_fips),
         FY         = as.integer(FY))

pdf_sheeo <- pdata.frame(df_sheeo, index = c("state_fips", "FY"))
cat(". xtset state_fips FY, yearly\n\n")
cat("  Panel variable: state_fips\n")
cat("  Time variable:  FY,", min(df_sheeo$FY), "to", max(df_sheeo$FY), "\n")
cat("  n =", length(unique(df_sheeo$state_fips)), "panels\n")
cat("  T =", length(unique(df_sheeo$FY)), "periods\n\n")

haven::write_dta(
  df_sheeo |> rename_with(~ gsub("[^A-Za-z0-9_]", "_", .x)),
  "Example_5_2.dta"
)
cat("file saved: Example_5_2.dta\n\n")

# ============================================================================
# Section 5.3: Getting to Know Our Data
# ============================================================================
cat("*===============================================================================\n")
cat("* Section 5.3: Getting to Know Our Data\n")
cat("*===============================================================================\n\n")

# ----------------------------------------------------------------
# Load HSLS:09 public-use truncated dataset
# Equivalent to: use "Public_use_HSLS_09_truncated.dta" + keep vars
# ----------------------------------------------------------------
safe_download(
  "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Public_use_HSLS_09_truncated.dta",
  "Public_use_HSLS_09_truncated.dta")

if (file.exists("Public_use_HSLS_09_truncated.dta") &&
    file.size("Public_use_HSLS_09_truncated.dta") > 0) {
  df_hsls_full <- haven::read_dta("Public_use_HSLS_09_truncated.dta") |>
    mutate(across(everything(), haven::zap_labels))
  keep_vars <- c("STU_ID","X1SEX","X1RACE","X1SES","X1SESQ5",
                 "X4ATPRLVLA","S3CLGPELL","P1TUITION")
  keep_vars <- intersect(keep_vars, names(df_hsls_full))
  df_hsls_full <- df_hsls_full |> select(all_of(keep_vars))
  cat(". keep STU_ID X1SEX X1RACE X1SES X1SESQ5 X4ATPRLVLA S3CLGPELL P1TUITION\n")
  cat("  Kept", ncol(df_hsls_full), "variables,", nrow(df_hsls_full), "observations\n\n")
}

# Load the pre-truncated version (Example_5_3.dta)
# Equivalent to: use "Example_5_3.dta"
safe_download(
  "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_3.dta",
  "Example_5_3.dta")

if (file.exists("Example_5_3.dta") && file.size("Example_5_3.dta") > 0) {
  df_53 <- haven::read_dta("Example_5_3.dta")
} else {
  cat("  [Using synthetic HSLS data for Example_5_3]\n\n")
  set.seed(42)
  n <- 23503
  df_53 <- data.frame(
    STU_ID     = 1:n,
    X1SEX      = sample(c(1,2,NA), n, replace=TRUE, prob=c(.49,.49,.02)),
    X1RACE     = sample(c(1:8,NA), n, replace=TRUE,
                        prob=c(.01,.09,.09,.01,.13,.08,.003,.61,.007)),
    X1SES      = round(rnorm(n, 0, 1), 2),
    X1SESQ5    = sample(c(-9,1:5), n, replace=TRUE,
                        prob=c(.09,.18,.16,.18,.20,.19)),
    X4ATPRLVLA = sample(c(1:6,NA), n, replace=TRUE,
                        prob=c(.08,.10,.12,.15,.20,.29,.06)),
    S3CLGPELL  = sample(c(-9,-8,-7,-4,1,2,3,NA), n, replace=TRUE,
                        prob=c(.01,.21,.21,.03,.22,.23,.08,.02)),
    P1TUITION  = sample(c(-9, round(runif(n*.93,0,60000)),NA), n, replace=TRUE,
                        prob=c(.01,.92,.07)),
    stringsAsFactors = FALSE
  )
}

# ----------------------------------------------------------------
# Inspect missing data coding — equivalent to: codebook S3CLGPELL
# ----------------------------------------------------------------
cat(". codebook S3CLGPELL\n\n")
x_clgpell <- df_53$S3CLGPELL
cat(sprintf("  Type:   %s\n", class(x_clgpell)[1]))
cat(sprintf("  Range:  %s to %s\n",
            min(x_clgpell, na.rm=TRUE), max(x_clgpell, na.rm=TRUE)))
cat(sprintf("  Missing (.): %d / %d\n\n", sum(is.na(x_clgpell)), length(x_clgpell)))
# Print value labels if present
if (!is.null(attr(x_clgpell, "labels"))) {
  lbl <- attr(x_clgpell, "labels")
  tab <- table(factor(x_clgpell, levels=lbl))
  for (i in seq_along(lbl)) {
    cat(sprintf("    %6d  %d  %s\n", tab[i], lbl[i], names(lbl)[i]))
  }
} else {
  print(table(x_clgpell, useNA="always"))
}
cat("\n")

# ----------------------------------------------------------------
# Recode NCES missing codes to R NA
# Equivalent to: mvdecode _all, mv(-9=.)
# NCES uses -9 (missing), -8 (unit non-response), -7 (item skip),
# -4 (abbreviated interview), -1 (don't know) as special codes.
# ----------------------------------------------------------------
nces_miss_codes <- c(-9, -8, -7, -4, -1)
df_53 <- df_53 |>
  mutate(across(where(is.numeric),
                function(x) replace(x, x %in% nces_miss_codes, NA)))
cat(". mvdecode _all, mv(-9=.)\n")
cat("  NCES special codes (-9,-8,-7,-4,-1) recoded to NA\n\n")

haven::write_dta(df_53, "Example_5_4.dta")
cat("file saved: Example_5_4.dta\n\n")

# ============================================================================
# Section 5.4: Missing Data Analysis
# ============================================================================
cat("*===============================================================================\n")
cat("* Section 5.4: Missing Data Analysis\n")
cat("*===============================================================================\n\n")

# Load recoded dataset (Example_5_4_1.dta)
safe_download(
  "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_4_1.dta",
  "Example_5_4_1.dta")

if (file.exists("Example_5_4_1.dta") && file.size("Example_5_4_1.dta") > 0) {
  df_541 <- haven::read_dta("Example_5_4_1.dta") |>
    mutate(across(everything(), haven::zap_labels),
           across(where(is.numeric),
                  function(x) replace(x, x %in% nces_miss_codes, NA)))
} else {
  df_541 <- df_53   # fall back to the recoded df_53
}

# ----------------------------------------------------------------
# Tabulate missing values — equivalent to: mdesc
# naniar::miss_var_summary() is the direct R equivalent of mdesc
# ----------------------------------------------------------------
cat(". mdesc\n\n")
miss_summary <- miss_var_summary(df_541)
print(as.data.frame(miss_summary), row.names = FALSE)
cat("\n")

# ----------------------------------------------------------------
# Missing value patterns — equivalent to: misstable tree
# ----------------------------------------------------------------
cat(". misstable tree  (nested percent missing)\n\n")
# naniar::miss_case_table() shows distribution of per-row missingness counts
miss_cases <- miss_case_table(df_541)
print(as.data.frame(miss_cases), row.names = FALSE)
cat("\n")

# ----------------------------------------------------------------
# Cross-tabulated missing patterns — equivalent to: misstable patterns
# mice::md.pattern() produces a cross-tabulated pattern matrix where
# 1 = observed, 0 = missing — matching Stata's misstable patterns output
# ----------------------------------------------------------------
cat(". misstable patterns  (1 = observed, 0 = missing)\n\n")
# Select only analysis variables (exclude ID)
anal_vars <- setdiff(names(df_541), "STU_ID")
pattern_tbl <- md.pattern(df_541[, anal_vars], plot = FALSE)
print(pattern_tbl)
cat("\n")

# Equivalent to: misstable tree, frequency (counts rather than percents)
cat(". misstable tree, frequency  (missing counts per observation)\n\n")
miss_freq <- rowSums(is.na(df_541[, anal_vars]))
freq_tbl  <- as.data.frame(table(`N_missing` = miss_freq))
freq_tbl$Percent <- round(freq_tbl$Freq / sum(freq_tbl$Freq) * 100, 2)
print(freq_tbl, row.names = FALSE)
cat("\n")

# ============================================================================
# Section 5.4 (continued): Missing Data by Categorical Variables
# ============================================================================
cat("*===============================================================================\n")
cat("* Section 5.4 (continued): Missing Data by Categorical Variables\n")
cat("*===============================================================================\n\n")

# ----------------------------------------------------------------
# Missingness by subgroup — equivalent to: bysort X1SESQ5: missings table
# ----------------------------------------------------------------
cat(". bysort X1SESQ5 : missings table\n\n")
for (grp_val in sort(unique(df_541$X1SESQ5))) {
  sub <- df_541 |> filter(X1SESQ5 == grp_val)
  n_miss <- sum(rowSums(is.na(sub[, anal_vars])) > 0)
  lbl <- if (!is.null(attr(df_541$X1SESQ5, "labels"))) {
    l <- attr(df_541$X1SESQ5, "labels")
    nm <- names(l[l == grp_val])
    if (length(nm)) nm else as.character(grp_val)
  } else as.character(grp_val)
  cat(sprintf("  X1SESQ5 = %-25s  n = %5d  missing: %4d (%5.1f%%)\n",
              lbl, nrow(sub), n_miss, n_miss / nrow(sub) * 100))
}
cat("\n")

# Equivalent to: bysort X1RACE : missings table
cat(". bysort X1RACE : missings table\n\n")
for (grp_val in sort(unique(df_541$X1RACE[!is.na(df_541$X1RACE)]))) {
  sub <- df_541 |> filter(X1RACE == grp_val)
  n_miss <- sum(rowSums(is.na(sub[, anal_vars])) > 0)
  lbl <- if (!is.null(attr(df_541$X1RACE, "labels"))) {
    l <- attr(df_541$X1RACE, "labels")
    nm <- names(l[l == grp_val])
    if (length(nm)) nm else as.character(grp_val)
  } else as.character(grp_val)
  cat(sprintf("  X1RACE = %-40s  n = %5d  missing: %4d (%5.1f%%)\n",
              lbl, nrow(sub), n_miss, n_miss / nrow(sub) * 100))
}
# Also report missing X1RACE group
sub_na <- df_541 |> filter(is.na(X1RACE))
if (nrow(sub_na) > 0) {
  n_miss <- sum(rowSums(is.na(sub_na[, anal_vars])) > 0)
  cat(sprintf("  X1RACE = %-40s  n = %5d  missing: %4d (%5.1f%%)\n",
              ".", nrow(sub_na), n_miss, n_miss / nrow(sub_na) * 100))
}
cat("\n")

# ============================================================================
# Section 5.4 (continued): Panel Missing Analysis
# ============================================================================
cat("*===============================================================================\n")
cat("* Section 5.4 (continued): Panel Missing Analysis (xtmis equivalent)\n")
cat("*===============================================================================\n\n")

# Load IPEDS panel dataset
safe_download(
  "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_4.dta",
  "Example_5_4.dta")

if (file.exists("Example_5_4.dta") && file.size("Example_5_4.dta") > 0) {
  df_54 <- haven::read_dta("Example_5_4.dta") |>
    mutate(across(everything(), haven::zap_labels))
} else {
  cat("  [Using synthetic IPEDS panel data]\n\n")
  set.seed(1)
  units <- c(150534,152363,178226,239503,144759,104531,452133,110051,451185)
  df_54 <- expand.grid(unitid = units,
                       year   = 1990:2019,
                       stringsAsFactors = FALSE) |>
    mutate(grantlow = ifelse(runif(n()) < 0.26, NA_real_,
                             round(runif(n(), 1000, 50000))))
}

# Equivalent to: tostring unitid, generate(unitid_s)
df_54$unitid_s <- as.character(df_54$unitid)

# ----------------------------------------------------------------
# xtmis equivalent: missing observations by panel unit
# Equivalent to: xtmis grantlow, id(unitid_s)
# ----------------------------------------------------------------
cat(". xtmis grantlow, id(unitid_s)  [R equivalent]\n\n")
xtmis_tbl <- df_54 |>
  group_by(unitid_s) |>
  summarise(
    Obs         = n(),
    Missing     = sum(is.na(grantlow)),
    Pct_Missing = round(sum(is.na(grantlow)) / n() * 100, 6),
    NonMiss     = sum(!is.na(grantlow)),
    Pct_NonMiss = round(sum(!is.na(grantlow)) / n() * 100, 6),
    .groups     = "drop"
  ) |>
  arrange(desc(Pct_Missing))

print(as.data.frame(xtmis_tbl), row.names = FALSE)
total_row <- data.frame(
  unitid_s    = "Total",
  Obs         = sum(xtmis_tbl$Obs),
  Missing     = sum(xtmis_tbl$Missing),
  Pct_Missing = round(sum(xtmis_tbl$Missing) / sum(xtmis_tbl$Obs) * 100, 6),
  NonMiss     = sum(xtmis_tbl$NonMiss),
  Pct_NonMiss = round(sum(xtmis_tbl$NonMiss)  / sum(xtmis_tbl$Obs) * 100, 6)
)
print(total_row, row.names = FALSE)
cat("\n")

# ============================================================================
# Section 5.4.1: Testing for MCAR (Little's Test)
# ============================================================================
cat("*===============================================================================\n")
cat("* Section 5.4.1: Testing for Missing Completely at Random (MCAR)\n")
cat("*===============================================================================\n\n")

# Reload and recode Example_5_3
if (file.exists("Example_5_3.dta") && file.size("Example_5_3.dta") > 0) {
  df_mcar <- haven::read_dta("Example_5_3.dta") |>
    mutate(across(everything(), haven::zap_labels),
           across(where(is.numeric),
                  function(x) replace(x, x %in% nces_miss_codes, NA)))
} else {
  df_mcar <- df_53
}

# ----------------------------------------------------------------
# Little's MCAR test
# Equivalent to: mcartest S3CLGPELL P1TUITION
# naniar::mcar_test() implements Little's (1988) chi-squared MCAR test.
# ----------------------------------------------------------------
cat(". mcartest S3CLGPELL P1TUITION  (Little's MCAR test)\n\n")
mcar_vars <- df_mcar[, c("S3CLGPELL", "P1TUITION")]
# Remove rows missing on ALL variables (as mcartest does)
mcar_vars <- mcar_vars[rowSums(!is.na(mcar_vars)) > 0, ]

tryCatch({
  mcar_result <- naniar::mcar_test(mcar_vars)
  cat(sprintf("  Little's MCAR test\n"))
  cat(sprintf("  Number of obs:       %d\n",     nrow(mcar_vars)))
  cat(sprintf("  Chi-square statistic: %.4f\n",  mcar_result$statistic))
  cat(sprintf("  Degrees of freedom:   %d\n",    mcar_result$df))
  cat(sprintf("  p-value:              %.4f\n\n", mcar_result$p.value))
}, error = function(e) {
  cat("  [mcar_test error:", conditionMessage(e), "]\n\n")
})

# ----------------------------------------------------------------
# Covariate-dependent missingness (CDM) test
# Equivalent to: mcartest S3CLGPELL P1TUITION = i.X1RACE if X1RACE != .
# R approach: logistic regression of missingness indicator on X1RACE;
# significant coefficients indicate missingness depends on X1RACE (MAR/MNAR).
# ----------------------------------------------------------------
cat(". mcartest S3CLGPELL P1TUITION = i.X1RACE  (CDM — covariate-dependent missingness)\n\n")
df_cdm <- df_mcar |>
  filter(!is.na(X1RACE)) |>
  mutate(miss_clgpell  = as.integer(is.na(S3CLGPELL)),
         miss_tuition  = as.integer(is.na(P1TUITION)),
         X1RACE_f      = factor(X1RACE))

for (outcome in c("miss_clgpell", "miss_tuition")) {
  var_label <- if (outcome == "miss_clgpell") "S3CLGPELL" else "P1TUITION"
  mdl <- glm(reformulate("X1RACE_f", outcome), data = df_cdm,
              family = binomial(link = "logit"))
  lrt <- anova(mdl, test = "LRT")
  cat(sprintf("  Outcome: missingness in %s\n", var_label))
  cat(sprintf("  LRT chi2 = %.4f  df = %d  p = %.4f\n",
              lrt$Deviance[2], lrt$Df[2], lrt$`Pr(>Chi)`[2]))
  cat(sprintf("  Pseudo-R2 (McFadden) = %.4f\n\n",
              1 - mdl$deviance / mdl$null.deviance))
}

# ============================================================================
# Section 5.4.2: Panel-Specific Missing Data Analysis
# ============================================================================
cat("*===============================================================================\n")
cat("* Section 5.4.2: Panel Missing Data Analysis (xtmispanel equivalent)\n")
cat("*===============================================================================\n\n")

# Load SHEEO panel dataset
safe_download(
  "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example5_2.dta",
  "Example5_2.dta")

if (file.exists("Example5_2.dta") && file.size("Example5_2.dta") > 0) {
  df_52 <- haven::read_dta("Example5_2.dta") |>
    mutate(across(everything(), haven::zap_labels),
           state_fips = as.integer(state_fips),
           FY         = as.integer(FY))
} else {
  cat("  [Using synthetic SHEEO panel data]\n\n")
  df_52 <- df_sheeo
}

# Equivalent to: xtset state_fips FY, yearly
pdf_52 <- pdata.frame(df_52, index = c("state_fips", "FY"))
cat(". xtset state_fips FY, yearly\n\n")
cat("  Panels:", length(unique(df_52$state_fips)),
    "  Time periods:", length(unique(df_52$FY)), "\n\n")

panel_vars <- c("NetTuition", "Appropriations")
panel_vars <- intersect(panel_vars, names(df_52))

# ----------------------------------------------------------------
# MODULE 1: Detection — equivalent to: xtmispanel ..., detect
# Three simultaneous tables: variable-level, panel-level, time-level
# ----------------------------------------------------------------
cat(". xtmispanel", paste(panel_vars, collapse=" "), ", detect\n\n")

# TABLE 1: Variable-level missingness
cat("  TABLE 1: Missing Data Summary by Variable\n")
cat("  ", paste(rep("-", 65), collapse=""), "\n", sep="")
cat(sprintf("  %-25s %8s %8s %8s  %s\n",
            "Variable","N_Total","N_Miss","%Miss","Status"))
cat("  ", paste(rep("-", 65), collapse=""), "\n", sep="")
for (v in panel_vars) {
  n_tot  <- nrow(df_52)
  n_miss <- sum(is.na(df_52[[v]]))
  pct    <- n_miss / n_tot * 100
  status <- if (pct == 0) "Complete" else if (pct < 5) "Low" else
            if (pct < 20) "Moderate" else "High"
  cat(sprintf("  %-25s %8d %8d %7.1f%%  %s\n",
              v, n_tot, n_miss, pct, status))
}
cat("  ", paste(rep("-", 65), collapse=""), "\n\n", sep="")

# TABLE 2: Panel-level missingness
cat("  TABLE 2: Missing Data Summary by Panel Unit (State)\n")
cat("  ", paste(rep("-", 55), collapse=""), "\n", sep="")
panel_miss <- df_52 |>
  group_by(state_fips) |>
  summarise(
    N_Obs  = n() * length(panel_vars),
    N_Miss = sum(across(all_of(panel_vars), is.na)),
    Pct    = round(N_Miss / N_Obs * 100, 1),
    .groups = "drop"
  ) |>
  arrange(desc(Pct))
print(as.data.frame(head(panel_miss, 10)), row.names = FALSE)
cat("  (showing top 10 panels by % missing)\n\n")

# TABLE 3: Time-period missingness
cat("  TABLE 3: Missing Data Summary by Time Period (FY)\n")
cat("  ", paste(rep("-", 55), collapse=""), "\n", sep="")
time_miss <- df_52 |>
  group_by(FY) |>
  summarise(
    N_Miss = sum(across(all_of(panel_vars), is.na)),
    Pct    = round(N_Miss / (n() * length(panel_vars)) * 100, 1),
    .groups = "drop"
  ) |>
  mutate(Bar = strrep("█", pmax(1, round(Pct / 3))))
print(as.data.frame(time_miss), row.names = FALSE)
cat("\n")

# ----------------------------------------------------------------
# MODULE 2: Mechanism test — equivalent to: xtmispanel ..., test
# Logistic regression of missingness on observed values + panel means
# (panel-aware MAR test; Little's chi-squared approximation for panels)
# ----------------------------------------------------------------
cat(". xtmispanel", paste(panel_vars, collapse=" "), ", test\n\n")
for (v in panel_vars) {
  miss_ind <- as.integer(is.na(df_52[[v]]))
  other    <- setdiff(panel_vars, v)
  if (length(other) > 0 && any(!is.na(df_52[[other[1]]]))) {
    df_test <- df_52 |>
      mutate(miss_v   = as.integer(is.na(.data[[v]])),
             other_v  = .data[[other[1]]],
             panel_mn = ave(.data[[other[1]]], state_fips,
                            FUN = function(x) mean(x, na.rm=TRUE)))
    mdl <- tryCatch(
      glm(miss_v ~ other_v + panel_mn, data = df_test,
          family = binomial(link="logit")),
      error = function(e) NULL)
    if (!is.null(mdl)) {
      lrt <- anova(mdl, test="LRT")
      cat(sprintf("  Variable: %s\n", v))
      cat(sprintf("    LRT chi2 = %.4f  df = %d  p = %.4f\n",
                  sum(lrt$Deviance[-1], na.rm=TRUE),
                  sum(lrt$Df[-1], na.rm=TRUE),
                  lrt$`Pr(>Chi)`[length(lrt$`Pr(>Chi)`)]))
      cat(sprintf("    Pseudo-R2 = %.4f\n", 1 - mdl$deviance/mdl$null.deviance))
      mech <- if (lrt$`Pr(>Chi)`[length(lrt$`Pr(>Chi)`)] < 0.05) "MAR/MNAR" else "Possibly MCAR"
      cat(sprintf("    Mechanism: %s\n\n", mech))
    }
  }
}

# ----------------------------------------------------------------
# MODULE 5: Visualization — equivalent to: xtmispanel ..., graph
# xtmis_heatmap: panel unit × time period missingness grid
# xtmis_combined: dashboard with heatmap + bar charts
# ----------------------------------------------------------------
cat(". xtmispanel", paste(panel_vars, collapse=" "), ", graph\n\n")

# Compute per-cell missingness for heatmap
heatmap_data <- df_52 |>
  rowwise() |>
  mutate(pct_miss = mean(is.na(c_across(all_of(panel_vars)))) * 100) |>
  ungroup() |>
  mutate(state_fips = factor(state_fips))

# --- xtmis_heatmap ---
fig_heat <- ggplot(heatmap_data, aes(x = factor(FY), y = state_fips,
                                      fill = pct_miss)) +
  geom_tile(colour = "grey90", linewidth = 0.2) +
  scale_fill_gradient2(low = "white", mid = "steelblue", high = "firebrick",
                       midpoint = 25, name = "% missing",
                       limits = c(0, 100)) +
  labs(
    title    = "xtmis_heatmap  — Missing Data: Panel Unit × Time Period",
    subtitle = paste("Variables:", paste(panel_vars, collapse = ", ")),
    x        = "Fiscal Year",
    y        = "State FIPS"
  ) +
  theme_springer(base_size = 9) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        axis.text.y = element_text(size = 6))
save_fig(fig_heat, "xtmis_heatmap_R.png", height_px = 1100)

# --- xtmis_barvar: % missing per variable ---
fig_barvar <- ggplot(
  data.frame(variable   = panel_vars,
             pct_missing = sapply(panel_vars, function(v)
               mean(is.na(df_52[[v]])) * 100)),
  aes(x = reorder(variable, pct_missing), y = pct_missing)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "xtmis_barvar  — % Missing by Variable",
       x = NULL, y = "Percent missing") +
  theme_springer()
save_fig(fig_barvar, "xtmis_barvar_R.png")

# --- xtmis_barpanel: % missing per panel ---
fig_barpanel <- panel_miss |>
  slice_max(Pct, n = 20) |>
  ggplot(aes(x = reorder(factor(state_fips), Pct), y = Pct)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "xtmis_barpanel  — % Missing by Panel Unit (top 20 states)",
       x = "State FIPS", y = "Percent missing") +
  theme_springer()
save_fig(fig_barpanel, "xtmis_barpanel_R.png")

# --- xtmis_bartime: % missing per fiscal year ---
fig_bartime <- ggplot(time_miss, aes(x = FY, y = Pct)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "xtmis_bartime  — % Missing by Fiscal Year",
       x = "Fiscal Year", y = "Percent missing") +
  theme_springer()
save_fig(fig_bartime, "xtmis_bartime_R.png")

# --- xtmis_combined: 2×2 dashboard ---
library(patchwork)   # lightweight combination; install if needed
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
  library(patchwork)
}
fig_combined <- (fig_heat | fig_barvar) / (fig_barpanel | fig_bartime) +
  plot_annotation(
    title    = "xtmis_combined  — Missing Data Diagnostic Dashboard",
    subtitle = paste("Variables:", paste(panel_vars, collapse=", ")),
    theme    = theme(plot.title = element_text(size = 12, face = "bold"))
  )
save_fig(fig_combined, "xtmis_combined_R.png", width_px = 1800, height_px = 1200)

cat("\nAll graphs saved to:", graphs_dir, "\n\n")

# ============================================================================
# Best Practices Summary
# ============================================================================

# KEY RECOMMENDATIONS FOR GETTING TO KNOW THY DATA (R):
#
# 1. STORAGE TYPES AND MEMORY:
#    - Check column types with str() or glimpse()
#    - Coerce to integer with as.integer() (equivalent to Stata's compress/recast)
#    - Use haven::zap_labels() after read_dta() to strip Stata metadata
#
# 2. MISSING DATA CODING IN NCES DATA:
#    - replace(x, x %in% c(-9,-8,-7,-4,-1), NA) replaces mvdecode _all, mv(-9=.)
#    - Use codebook-equivalent: table(x, useNA="always")
#
# 3. MISSING DATA ANALYSIS — CROSS-SECTIONAL / SURVEY:
#    - naniar::miss_var_summary()  → mdesc
#    - naniar::miss_case_table()   → misstable tree
#    - mice::md.pattern()          → misstable patterns
#    - Grouped summaries with group_by() + summarise(is.na()) → bysort: missings table
#
# 4. MISSING DATA ANALYSIS — PANEL:
#    - xtmis equivalent: group_by(panel_id) + summarise(sum(is.na()))
#    - xtmispanel detect/test/graph: replicated via custom summaries + ggplot2
#
# 5. MCAR TESTING:
#    - naniar::mcar_test()     → mcartest (equal variances)
#    - glm(is.na(y) ~ covariates) → mcartest CDM variant
#
# 6. PANEL STRUCTURE:
#    - plm::pdata.frame(df, index=c("id","time")) → xtset
#    - Always coerce id and time to integer before pdata.frame()
#
# 7. VISUALISATION:
#    - naniar::gg_miss_var()       → simple missingness bar chart
#    - ggplot2 + geom_tile()       → xtmis_heatmap
#    - patchwork for multi-panel layouts → xtmis_combined

# ============================================================================
# Close log — equivalent to: log close
# ============================================================================
cat("Chapter 5 R script completed:", format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n")
sink()

# ============================================================================
# END OF CHAPTER 5 R CODE
# ============================================================================
