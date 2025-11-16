# ================================================================
# Chapter 5 - Getting to Know Thy Data
# Complete R translation of Stata_Code5.do
# Higher Education Policy Analysis Using Quantitative Techniques 
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/ch5
# Author: Marvin A. Titus (original Stata)
# Date: November 16, 2025
# ================================================================

# Script tested in R >= 4.0
# NOTE: Set working directory if you want to save .dta/.xlsx files persistently:
# ch5data <- "C:/Users/YourName/Documents/book-materials/ch5/data"
# dir.create(ch5data, recursive = TRUE, showWarnings = FALSE)
# setwd(ch5data)

# ----------------------------------------------------------------
# REQUIRED PACKAGES
# ----------------------------------------------------------------
required_packages <- c(
  "tidyverse", "haven", "readxl", "plm", "naniar", 
  "mice", "DescTools", "car"
)

new_pkgs <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) {
  message("Installing missing packages: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs, dependencies = TRUE)
}

library(tidyverse)
library(haven)
library(readxl)
library(plm)
library(naniar)
library(mice)
library(DescTools)
library(car)

# Optional packages for MCAR test fallback methods
optional_packages <- c("BaylorEdPsych", "MissMech")
optional_installed <- optional_packages[optional_packages %in% installed.packages()[, "Package"]]

if ("BaylorEdPsych" %in% optional_installed) {
  tryCatch(library(BaylorEdPsych), error = function(e) NULL)
}
if ("MissMech" %in% optional_installed) {
  tryCatch(library(MissMech), error = function(e) NULL)
}

message("Optional MCAR packages available: ", paste(optional_installed, collapse = ", "))

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

# Helper: recode -9 to NA
recode_minus9_to_na <- function(df) {
  num_vars <- names(df)[sapply(df, is.numeric)]
  for (v in num_vars) {
    df[[v]] <- ifelse(!is.na(df[[v]]) & df[[v]] == -9, NA, df[[v]])
  }
  df
}

# ----------------------------------------------------------------
# Section 5.2: Getting to Know the Structure of Our Datasets
# ----------------------------------------------------------------

# Download Example_4_2_2_TS.dta (time series)
url_ts <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/Example_4_2_2_TS.dta"
dest_ts <- file.path(tempdir(), "Example_4_2_2_TS.dta")
download.file(url_ts, dest_ts, mode = "wb")
ts_data <- read_dta(dest_ts)
ts_data <- ts_data %>% mutate(across(where(is.double), ~ downcast_double(.x)))

message("Loaded Example_4_2_2_TS.dta: rows=", nrow(ts_data), " cols=", ncol(ts_data))

# Download Example_5_0.dta (panel example)
url_5_0 <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_0.dta"
dest_5_0 <- file.path(tempdir(), "Example_5_0.dta")
download.file(url_5_0, dest_5_0, mode = "wb")
ex5_0 <- read_dta(dest_5_0)
ex5_0 <- ex5_0 %>% mutate(across(where(is.double), ~ downcast_double(.x)))
if ("id" %in% names(ex5_0)) ex5_0 <- ex5_0 %>% mutate(id = as.integer(id))

message("Loaded Example_5_0.dta: rows=", nrow(ex5_0), " cols=", ncol(ex5_0))

# ----------------------------------------------------------------
# Section 5.2 (continued): SHEEO Finance Data Example
# ----------------------------------------------------------------

# Download SHEEO Example_5_1.xlsx
url_5_1 <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_1.xlsx"
dest_5_1 <- file.path(tempdir(), "Example_5_1.xlsx")
download.file(url_5_1, dest_5_1, mode = "wb")
sheeo <- read_excel(dest_5_1, sheet = "reformatted")

# Filter data: keep FY >= 2010 and exclude U.S. and D.C.
if ("FY" %in% names(sheeo)) sheeo <- sheeo %>% filter(FY >= 2010)
if ("State" %in% names(sheeo)) sheeo <- sheeo %>% filter(!(State %in% c("U.S.", "D.C.")))

sheeo <- sheeo %>% mutate(across(where(is.double), ~ downcast_double(.x)))

# Create state_fips identifier if not present
if (!"state_fips" %in% names(sheeo) && "State" %in% names(sheeo)) {
  sheeo <- sheeo %>% mutate(state_fips = as.integer(factor(State)))
}

# Create panel data frame if state_fips and FY are available
if (all(c("state_fips", "FY") %in% names(sheeo))) {
  pdata_sheeo <- pdata.frame(sheeo, index = c("state_fips", "FY"))
} else {
  pdata_sheeo <- NULL
}

message("Loaded SHEEO Example_5_1.xlsx: rows=", nrow(sheeo), " cols=", ncol(sheeo))

# ----------------------------------------------------------------
# Section 5.3: Analyzing Missing Data Patterns
# ----------------------------------------------------------------

# Download HSLS:09 truncated dataset
url_hsls_trunc <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Public_use_HSLS_09_truncated.dta"
dest_hsls_trunc <- file.path(tempdir(), "Public_use_HSLS_09_truncated.dta")
download.file(url_hsls_trunc, dest_hsls_trunc, mode = "wb")
hsls_trunc <- read_dta(dest_hsls_trunc)

# Keep only the variables of interest
vars_keep <- c("STU_ID", "X1SEX", "X1RACE", "X1SES", "X1SESQ5", 
               "X4ATPRLVLA", "S3CLGPELL", "P1TUITION")
hsls_small <- hsls_trunc %>% select(any_of(vars_keep))

message("Loaded HSLS truncated: rows=", nrow(hsls_small), " cols=", ncol(hsls_small))

# Download Example_5_3.dta
url_5_3 <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_3.dta"
dest_5_3 <- file.path(tempdir(), "Example_5_3.dta")
download.file(url_5_3, dest_5_3, mode = "wb")
ex5_3 <- read_dta(dest_5_3)
ex5_3 <- recode_minus9_to_na(ex5_3)

message("Loaded Example_5_3.dta: rows=", nrow(ex5_3), " cols=", ncol(ex5_3))

# Download Example_5_4.dta
url_5_4 <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_4.dta"
dest_5_4 <- file.path(tempdir(), "Example_5_4.dta")
download.file(url_5_4, dest_5_4, mode = "wb")
ex5_4 <- read_dta(dest_5_4)
if ("unitid" %in% names(ex5_4)) ex5_4 <- ex5_4 %>% mutate(unitid_str = as.character(unitid))

message("Loaded Example_5_4.dta: rows=", nrow(ex5_4), " cols=", ncol(ex5_4))

# Download Example_5_4_1.dta
url_5_4_1 <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_4_1.dta"
dest_5_4_1 <- file.path(tempdir(), "Example_5_4_1.dta")
download.file(url_5_4_1, dest_5_4_1, mode = "wb")
ex5_4_1 <- read_dta(dest_5_4_1)
ex5_4_1 <- recode_minus9_to_na(ex5_4_1)

message("Loaded Example_5_4_1.dta: rows=", nrow(ex5_4_1), " cols=", ncol(ex5_4_1))

# ----------------------------------------------------------------
# Section 5.3.1: Missingness Summaries
# ----------------------------------------------------------------

# Missingness summary using naniar
message("Missingness summary (naniar::miss_var_summary):")
print(naniar::miss_var_summary(ex5_4_1))

# Missingness patterns using mice
message("Missingness patterns (mice::md.pattern):")
print(head(mice::md.pattern(ex5_4_1, plot = FALSE), 10))

# Custom missingness pattern frequencies
pattern_df <- ex5_4_1 %>%
  transmute(across(everything(), ~ if_else(is.na(.), 1L, 0L))) %>%
  unite("pattern", everything(), sep = "") %>%
  count(pattern, sort = TRUE)

message("Missingness pattern frequencies (top 10):")
print(head(pattern_df, 10))

# ----------------------------------------------------------------
# Section 5.4.1: Testing for Missing Completely at Random (MCAR)
# ----------------------------------------------------------------

# Prepare variables for MCAR test: S3CLGPELL and P1TUITION
vars_test <- c("S3CLGPELL", "P1TUITION")
present_test_vars <- vars_test[vars_test %in% names(ex5_3)]

if (length(present_test_vars) < 1) {
  stop("MCAR test variables not found in Example_5_3.")
}

# Create test dataframe
test_df <- ex5_3 %>% select(any_of(present_test_vars)) %>% as.data.frame()

# Remove rows where all test variables are missing
rows_all_na <- apply(test_df, 1, function(r) all(is.na(r)))
message(sum(rows_all_na), " rows have all test variables missing and will be removed.")
test_df2 <- test_df[!rows_all_na, , drop = FALSE]

# Initialize results list
mc_result <- list()
mc_result$note <- "MCAR test hierarchy: naniar -> BaylorEdPsych -> MissMech -> CDM logistic"

# Run naniar::mcar_test (primary method)
if (requireNamespace("naniar", quietly = TRUE)) {
  message("Running naniar::mcar_test()...")
  tryCatch({
    mcar_out <- naniar::mcar_test(test_df2)
    mc_result$method_naniar <- mcar_out
    message("naniar::mcar_test completed.")
    print(mcar_out)
  }, error = function(e) {
    mc_result$naniar_error <- e$message
    message("naniar::mcar_test failed: ", e$message)
  })
} else {
  message("naniar not installed. Install with: install.packages('naniar')")
  mc_result$naniar_error <- "naniar not installed"
}

# Fallback: BaylorEdPsych::LittleMCAR
if (is.null(mc_result$method_naniar) && "BaylorEdPsych" %in% optional_installed) {
  message("Attempting BaylorEdPsych::LittleMCAR() as fallback...")
  tryCatch({
    little_res <- BaylorEdPsych::LittleMCAR(test_df2)
    mc_result$method_baylor <- little_res
    message("BaylorEdPsych::LittleMCAR completed.")
  }, error = function(e) {
    mc_result$baylor_error <- e$message
    message("BaylorEdPsych::LittleMCAR failed: ", e$message)
  })
}

# Fallback: MissMech MCAR functions
if (is.null(mc_result$method_naniar) && is.null(mc_result$method_baylor) && 
    "MissMech" %in% optional_installed) {
  message("Attempting MissMech MCAR functions as fallback...")
  tryCatch({
    if ("TestMCAR" %in% ls("package:MissMech")) {
      mm_res <- MissMech::TestMCAR(test_df2)
      mc_result$method_missmech <- mm_res
      message("MissMech::TestMCAR completed.")
    } else if ("LittleMCAR" %in% ls("package:MissMech")) {
      mm_res <- MissMech::LittleMCAR(test_df2)
      mc_result$method_missmech <- mm_res
      message("MissMech::LittleMCAR completed.")
    } else {
      mc_result$missmech_error <- "No TestMCAR/LittleMCAR exported in MissMech namespace"
      message(mc_result$missmech_error)
    }
  }, error = function(e) {
    mc_result$missmech_error <- e$message
    message("MissMech MCAR attempt failed: ", e$message)
  })
}

# ----------------------------------------------------------------
# Section 5.4.1 (continued): CDM Logistic Diagnostics Fallback
# ----------------------------------------------------------------

# Run CDM-like diagnostics if no formal MCAR test succeeded
if (is.null(mc_result$method_naniar) && is.null(mc_result$method_baylor) && 
    is.null(mc_result$method_missmech)) {
  
  message("No formal MCAR test available. Running CDM-like diagnostics (missingness ~ covariates).")
  
  # Create missingness indicators
  ex5_3 <- ex5_3 %>%
    mutate(
      miss_S3CLG = as.integer(is.na(S3CLGPELL)),
      miss_P1TU = as.integer(is.na(P1TUITION))
    )
  
  # Specify covariates (can be modified as needed)
  covariates <- c("X1RACE")
  covariates_present <- covariates[covariates %in% names(ex5_3)]
  
  cdms <- list()
  
  for (mvar in c("miss_S3CLG", "miss_P1TU")) {
    if (!mvar %in% names(ex5_3)) next
    
    # Build formula
    rhs <- paste0("factor(", paste(covariates_present, collapse = ") + factor("), ")")
    fmla <- as.formula(paste0(mvar, " ~ ", rhs))
    
    # Build complete-case subset for this model
    needed_vars <- all.vars(fmla)
    df_sub <- ex5_3 %>% 
      dplyr::select(all_of(unique(c(needed_vars, mvar)))) %>% 
      tidyr::drop_na()
    
    message("Fitting ", mvar, " on ", length(needed_vars), " covariates; using ", 
            nrow(df_sub), " complete cases.")
    
    # Fit null and full models
    null_fit <- tryCatch(
      glm(as.formula(paste0(mvar, " ~ 1")), family = binomial(), data = df_sub), 
      error = function(e) e
    )
    full_fit <- tryCatch(
      glm(fmla, family = binomial(), data = df_sub), 
      error = function(e) e
    )
    
    if (inherits(null_fit, "error") || inherits(full_fit, "error")) {
      cdms[[mvar]] <- list(error = "model fitting failed")
      next
    }
    
    # Likelihood ratio test
    lr_table <- tryCatch(
      anova(null_fit, full_fit, test = "Chisq"), 
      error = function(e) e
    )
    
    pval <- NA
    if (!inherits(lr_table, "error")) {
      pval <- as.numeric(tail(lr_table[, "Pr(>Chi)"], 1))
    }
    
    cdms[[mvar]] <- list(
      formula = fmla, 
      n = nrow(df_sub), 
      lr_table = lr_table, 
      p_value = pval, 
      full_coef = coef(summary(full_fit))
    )
    
    if (!inherits(lr_table, "error")) print(lr_table)
    cat(sprintf("LR p-value for %s: %s\n\n", mvar, 
                ifelse(is.na(pval), "NA", signif(pval, 6))))
  }
  
  # Fisher combination of p-values (approximate joint test)
  pvals <- vapply(cdms, function(x) {
    if (!is.null(x$p_value)) x$p_value else NA_real_
  }, numeric(1))
  pvals <- pvals[is.finite(pvals)]
  
  if (length(pvals) > 0) {
    fisher_chi2 <- -2 * sum(log(pvals))
    fisher_df <- 2 * length(pvals)
    fisher_p <- pchisq(fisher_chi2, df = fisher_df, lower.tail = FALSE)
    
    mc_result$CDM <- list(
      individual = cdms, 
      fisher = list(
        chi2 = fisher_chi2, 
        df = fisher_df, 
        p = fisher_p, 
        individual_p = pvals
      )
    )
    
    cat("Combined CDM Fisher p-value:", signif(fisher_p, 6), "\n")
  } else {
    mc_result$CDM <- list(individual = cdms, fisher = "no p-values")
  }
}

# ----------------------------------------------------------------
# MCAR Test Results Summary and Export
# ----------------------------------------------------------------

message("\n=== MCAR / CDM Results Summary ===")
print(mc_result)

# Save results to RDS file
output_file <- file.path(tempdir(), "MCAR_test_result.rds")
saveRDS(mc_result, file = output_file)
message("MCAR / CDM results saved to: ", output_file)

# ----------------------------------------------------------------
# Clean up (optional)
# ----------------------------------------------------------------
# If you want to remove large objects from the workspace uncomment:
# rm(list = ls())
# gc()

message("\nChapter 5 processing complete.")
message("Key objects: ex5_0, sheeo, pdata_sheeo, hsls_small, ex5_3, ex5_4, ex5_4_1, mc_result")

# ================================================================
# END OF CHAPTER
# ================================================================