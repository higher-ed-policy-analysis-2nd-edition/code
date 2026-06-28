#=========================================================================
# MTE_MPRTE_R.R -- Chapter 11, Sections 11.1-11.3: Marginal Treatment Effects
#                Returns to Master's Degree Completion
# R translation of MTE_MPRTE.do
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch11
# Author: Marvin A. Titus
# Date: May 2026
# NOTE: Code development was assisted by Claude (Anthropic). The author
# provided specifications and reviewed, tested, and validated all code.
#=========================================================================
# Called by: R_code11.R driver (inherits graphs_dir, log, theme)
# Standalone: can also be run directly; uses fallback paths if needed.
#
# Data: synthetic B&B panel
#   Example_7_5_3_updated.dta  (primary; contains pre-generated ma_* vars)
#   Example_7_5_3.dta          (fallback; ma_* generated in Section 1b)
#
# Required packages: haven, dplyr, tidyr, ivreg, sandwich, lmtest, car,
#                     ggplot2, stringr
#
# NOTE ON mtefe: Stata's community-contributed `mtefe` package estimates
# MTE using local instrumental variables (local IV) by default, a
# different estimator from the OLS-based polynomial control-function
# regression used throughout this script (Section 6 and 6b). mtefe's
# ATE/ATT/ATU/LATE for pol(1) and pol(2) are NOT replicated in this R
# translation -- an earlier attempt to approximate them with simple
# linear/quadratic OLS regressions was tested against real data and found
# to diverge substantially from Stata's actual mtefe output, so that
# approximation was removed rather than left in under a misleading label.
# See the "mtefe Package Estimation: NOT REPLICATED IN R" note in
# Section 6 below for the full explanation. For that specific parameter
# set, treat the Stata output as authoritative; this script's cubic
# control-function ATE/ATT/ATU is a related but numerically distinct
# estimator, not a substitute.
#
# Sections (Chapter 11 numbering):
#   1     Load dataset (Example_7_5_3_updated.dta / Example_7_5_3.dta)        [11.1]
#   1b    Verify / generate master's program area indicators (ma_*)          [11.1]
#   2     Summary statistics                                                  [11.1]
#   3     First stage and instrument relevance                                [11.1]
#   4     Naive OLS estimation                                                [11.1]
#   5     IV/2SLS estimation (LATE)                                          [11.1]
#   6     MTE estimation -- pooled polynomial (quadratic and cubic)           [11.2]
#   6b    MTE by graduate program area (fully interacted)                    [11.2]
#   6c    Bootstrap infrastructure (cluster bootstrap + wild cluster)        [11.2]
#   7     Results comparison (ATE / ATT / ATU / LATE)                        [11.2]
#   8     MTE visualization                                                   [11.2.3]
#   9     Basic policy simulation (PRTE)                                      [11.3]
#   10    MPRTE -- Scenarios 1-4 (original)                                    [11.3]
#   10b   MPRTE by graduate program area -- Scenarios 5-8                      [11.3]
#   11    MPRTE by policy intensity                                           [11.3]
#   12    Comparing treatment effect parameters                               [11.3]
#   13    MPRTE visualization                                                 [11.3]
#   14    Policy cost-benefit analysis                                        [11.3]
#   15    Save results                                                        [11.4]
#   16    Final summary                                                       [11.4]
#=========================================================================

suppressMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(ivreg)
  library(sandwich)
  library(lmtest)
  library(car)
  library(ggplot2)
  library(stringr)
})

#-------------------------------------------------------------------------
# Fallback paths when running standalone (not called from a driver script)
#-------------------------------------------------------------------------
if (!exists("graphs_dir") || is.null(graphs_dir)) {
  if (Sys.info()[["user"]] == "marvi") {
    graphs_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/graphs"
  } else {
    graphs_dir <- "Output/graphs"
  }
  dir.create(graphs_dir, recursive = TRUE, showWarnings = FALSE)
  cat("MTE_MPRTE.R (standalone): graphs_dir set to", graphs_dir, "\n")
}

#-------------------------------------------------------------------------
# Dedicated log for this sub-script
# Opens its own log file (sink), separate from the driver's master log,
# so that Sections 11.1-11.3 output can be reviewed independently.
# Fallback logdir is defined here if not already set by the caller.
#-------------------------------------------------------------------------
if (!exists("logdir") || is.null(logdir)) {
  if (Sys.info()[["user"]] == "marvi") {
    logdir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/logs"
  } else {
    logdir <- "Output/logs"
  }
  dir.create(logdir, recursive = TRUE, showWarnings = FALSE)
}

mte_log_path <- file.path(logdir, "MTE_MPRTE_R_output.log")
mte_log_con  <- file(mte_log_path, open = "wt")
sink(mte_log_con, split = TRUE)
sink(mte_log_con, type = "message", append = TRUE)

cat("MTE_MPRTE.R log opened:", format(Sys.time()), "\n")
cat("Log file:", mte_log_path, "\n")

# Springer monochrome ggplot theme, mirroring Stata's `set scheme s2mono`
theme_springer <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey85"),
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(hjust = 0),
      legend.background = element_blank(),
      legend.key = element_blank()
    )
}

################################################################################
# SECTION 1: Load Dataset
################################################################################

cat("\n==============================================\n")
cat("LOADING SYNTHETIC B&B DATASET\n")
cat("==============================================\n")

# Download Part B dataset from GitHub repository.
# Try the updated file (contains pre-generated ma_* variables) first;
# fall back to the base file if the updated version is unavailable.
if (!file.exists("Example_7_5_3_updated.dta")) {
  cat("Attempting to download Example_7_5_3_updated.dta from GitHub...\n")
  ok <- tryCatch({
    download.file(
      "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch10/Example_7_5_3_updated.dta",
      "Example_7_5_3_updated.dta", mode = "wb", quiet = TRUE
    )
    TRUE
  }, error = function(e) FALSE)
  if (!ok) cat("Download failed -- will try local file or base version.\n")
}

df <- NULL
if (file.exists("Example_7_5_3_updated.dta")) {
  df <- tryCatch(read_dta("Example_7_5_3_updated.dta"), error = function(e) NULL)
}

if (is.null(df)) {
  cat("Note: Example_7_5_3_updated.dta not found.\n")
  cat("Attempting to download Example_7_5_3.dta from GitHub (ch7 repository)...\n")
  ok <- tryCatch({
    download.file(
      "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_5_3.dta",
      "Example_7_5_3.dta", mode = "wb", quiet = TRUE
    )
    TRUE
  }, error = function(e) FALSE)
  if (!ok || !file.exists("Example_7_5_3.dta")) {
    stop(paste0(
      "ERROR: Download of Example_7_5_3.dta failed.\n",
      "Please download the file manually from:\n",
      "https://github.com/higher-ed-policy-analysis-2nd-edition/data/blob/main/ch7/Example_7_5_3.dta\n",
      "and place it in the working directory before running Part B."
    ))
  }
  cat("Loading Example_7_5_3.dta; ma_* will be generated in Section 1b.\n")
  df <- read_dta("Example_7_5_3.dta")
}

if (!"id" %in% names(df)) {
  df$id <- seq_len(nrow(df))
}

cat("\nVariables:", ncol(df), " Observations:", nrow(df), "\n")
print(str(df, list.len = 40))
cat("\nSample size:", nrow(df), "\n")

################################################################################
# SECTION 1b: Verify Master's Degree Program Area Indicators
#
# The dataset Example_7_5_3_updated.dta was produced by the revised
# data-generation script and already contains ma_stem, ma_business,
# ma_education, ma_health, and ma_other. If running against the
# original dataset without these variables, generate them here using
# the same seeded random-draw transition probabilities as the Stata
# original (set.seed(20251130) <-> Stata's set seed 20251130).
#
# Five mutually exclusive categories (IPEDS CIP-based):
#   ma_business   Business, Management, Marketing (CIP 52)
#   ma_education  Education (CIP 13)
#   ma_health     Health Professions & Related (CIP 51)
#   ma_stem       STEM fields (CIPs 11, 14, 15, 26, 27, 40, 41)
#   ma_other      All remaining fields
#
# ma_* = 0 for all untreated observations (masters == 0).
################################################################################

cat("\n==============================================\n")
cat("MASTER'S PROGRAM AREA INDICATORS\n")
cat("==============================================\n")

if (!"ma_stem" %in% names(df)) {
  cat("Generating ma_* variables from undergraduate major fields...\n")

  df$ma_stem      <- 0
  df$ma_business  <- 0
  df$ma_education <- 0
  df$ma_health    <- 0
  df$ma_other     <- 0

  set.seed(20251130)
  df$.rma <- ifelse(df$masters == 1, runif(nrow(df)), NA_real_)

  df$ma_stem <- ifelse(df$masters == 1 & df$stem_major == 1 & df$.rma <= 0.55,
                        1, df$ma_stem)
  df$ma_business <- ifelse(df$masters == 1 & df$bus_major == 1 & df$.rma <= 0.65 &
                              df$ma_stem == 0, 1, df$ma_business)
  df$ma_education <- ifelse(df$masters == 1 & df$ed_major == 1 & df$.rma <= 0.70 &
                               df$ma_stem == 0 & df$ma_business == 0, 1, df$ma_education)
  df$ma_health <- ifelse(df$masters == 1 & df$socsci_major == 1 & df$.rma <= 0.40 &
                            df$ma_stem == 0 & df$ma_business == 0 & df$ma_education == 0,
                          1, df$ma_health)
  df$ma_health <- ifelse(df$masters == 1 & df$stem_major == 1 &
                            df$.rma > 0.55 & df$.rma <= 0.75 & df$ma_stem == 0,
                          1, df$ma_health)
  df$ma_other <- ifelse(df$masters == 1 & df$ma_stem == 0 & df$ma_business == 0 &
                           df$ma_education == 0 & df$ma_health == 0, 1, df$ma_other)

  df$.rma <- NULL

  cat("ma_* variables generated successfully.\n")
}
cat("ma_* variables confirmed present in dataset.\n")

# Verification
n_treated <- sum(df$masters == 1)
cat("\n--- Program Area Distribution (Treated Only) ---\n")
cat("Total treated:", n_treated, "\n")
for (a in c("stem", "business", "education", "health", "other")) {
  varname <- paste0("ma_", a)
  cnt <- sum(df[[varname]] == 1)
  cat(sprintf("  ma_%s: %d  (%5.1f%%)\n", a, cnt, 100 * cnt / n_treated))
}

ma_check <- df$ma_business + df$ma_education + df$ma_health + df$ma_stem + df$ma_other
n_bad_treated <- sum(df$masters == 1 & ma_check != 1)
if (n_bad_treated > 0) {
  cat("WARNING:", n_bad_treated, "treated obs with != 1 program area flag\n")
} else {
  cat("CHECK PASSED: all treated obs have exactly 1 program area\n")
}
n_bad_untreated <- sum(df$masters == 0 & ma_check != 0)
if (n_bad_untreated > 0) {
  cat("WARNING:", n_bad_untreated, "untreated obs with non-zero program area flag\n")
} else {
  cat("CHECK PASSED: all untreated obs have zero program area\n")
}

################################################################################
# SECTION 2: Summary Statistics
################################################################################

cat("\n==============================================\n")
cat("SUMMARY STATISTICS\n")
cat("==============================================\n")

print(table(df$masters))
treat_rate <- mean(df$masters)
cat(sprintf("Treatment rate: %5.3f\n", treat_rate))

print(summary(df[, c("ln_salary", "salary", "masters", "ga_funding_adj")]))

# tabstat salary ln_salary, by(masters) stats(mean sd min max n)
tabstat_by_masters <- df %>%
  group_by(masters) %>%
  summarise(
    salary_mean = mean(salary), salary_sd = sd(salary),
    salary_min = min(salary), salary_max = max(salary),
    ln_salary_mean = mean(ln_salary), ln_salary_sd = sd(ln_salary),
    ln_salary_min = min(ln_salary), ln_salary_max = max(ln_salary),
    n = n(), .groups = "drop"
  )
print(tabstat_by_masters)

cat("\nGA funding (detail):\n")
print(quantile(df$ga_funding_adj,
               probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)))
cat(sprintf("Mean: %.5f  SD: %.5f\n", mean(df$ga_funding_adj), sd(df$ga_funding_adj)))

control_vars <- c("female", "black", "hispanic", "asian", "age_ba", "firstgen",
                   "parent_income_q", "parent_grad", "ugpa", "stem_major",
                   "bus_major", "ed_major", "selective_inst", "public_ug",
                   "state_unemp", "metro")
print(summary(df[, control_vars]))

cat("\n--- Program Area by Undergraduate Major (Treated Only) ---\n")
cat("  STEM undergrads:\n")
print(df %>% filter(masters == 1, stem_major == 1) %>%
        summarise(across(c(ma_stem, ma_health, ma_business, ma_education, ma_other),
                          mean), n = n()))
cat("  Business undergrads:\n")
print(df %>% filter(masters == 1, bus_major == 1) %>%
        summarise(across(c(ma_stem, ma_health, ma_business, ma_education, ma_other),
                          mean), n = n()))
cat("  Education undergrads:\n")
print(df %>% filter(masters == 1, ed_major == 1) %>%
        summarise(across(c(ma_stem, ma_health, ma_business, ma_education, ma_other),
                          mean), n = n()))
cat("  Social sci / other undergrads:\n")
print(df %>% filter(masters == 1, socsci_major == 1) %>%
        summarise(across(c(ma_stem, ma_health, ma_business, ma_education, ma_other),
                          mean), n = n()))

cat("\n--- Mean Log Salary by Program Area (Treated Only) ---\n")
print(df %>% group_by(ma_stem) %>%
        summarise(ln_salary_mean = mean(ln_salary), salary_mean = mean(salary), n = n()))
for (a in c("business", "education", "health", "other")) {
  varname <- paste0("ma_", a)
  sub <- df[df[[varname]] == 1, ]
  cat(sprintf("  ma_%s: mean ln_salary = %6.4f  (N = %d)\n",
              a, mean(sub$ln_salary), nrow(sub)))
}

################################################################################
# SECTION 3: First-Stage and Instrument Relevance
################################################################################

cat("\n==============================================\n")
cat("INSTRUMENT RELEVANCE CHECK\n")
cat("==============================================\n")

x_controls <- c("female", "black", "hispanic", "asian", "age_ba", "firstgen",
                "parent_income_q", "parent_grad", "ugpa", "stem_major",
                "bus_major", "ed_major", "selective_inst", "public_ug",
                "state_unemp", "metro")
z_inst <- "ga_funding_adj"
x_rhs  <- paste(x_controls, collapse = " + ")

first_stage_formula <- as.formula(paste0("masters ~ ", z_inst, " + ", x_rhs))
first_stage <- lm(first_stage_formula, data = df)
vc_first_stage <- vcovHC(first_stage, type = "HC1")
print(coeftest(first_stage, vcov = vc_first_stage))

# test ga_funding_adj  -- single-coefficient Wald (F) test
wald_fs <- linearHypothesis(first_stage, "ga_funding_adj = 0",
                             vcov. = vc_first_stage, test = "F")
print(wald_fs)
first_stage_F <- wald_fs$F[2]
cat(sprintf("\nFirst-stage F: %6.2f\n", first_stage_F))
if (first_stage_F > 10) {
  cat("RESULT: Strong instrument (F > 10)\n")
} else {
  cat("WARNING: Potentially weak instrument\n")
}

################################################################################
# SECTION 4: Naive OLS Estimation
################################################################################

cat("\n==============================================\n")
cat("NAIVE OLS ESTIMATION\n")
cat("==============================================\n")

ols_formula <- as.formula(paste0("ln_salary ~ masters + ", x_rhs))
ols_naive <- lm(ols_formula, data = df)
vc_ols    <- vcovHC(ols_naive, type = "HC1")
print(coeftest(ols_naive, vcov = vc_ols))

ols_est <- coef(ols_naive)["masters"]
ols_se  <- sqrt(vc_ols["masters", "masters"])
cat(sprintf("OLS estimate: %6.4f (SE = %6.4f)\n", ols_est, ols_se))

################################################################################
# SECTION 5: IV/2SLS Estimation (LATE)
################################################################################

cat("\n==============================================\n")
cat("IV/2SLS ESTIMATION (LATE)\n")
cat("==============================================\n")

iv_formula <- as.formula(paste0("ln_salary ~ masters + ", x_rhs,
                                 " | ", z_inst, " + ", x_rhs))
iv_2sls <- ivreg(iv_formula, data = df)
vc_iv   <- vcovHC(iv_2sls, type = "HC1")
print(summary(iv_2sls, vcov = vc_iv, diagnostics = TRUE))

iv_est <- coef(iv_2sls)["masters"]
iv_se  <- sqrt(vc_iv["masters", "masters"])
cat(sprintf("\nIV/LATE estimate: %6.4f (SE = %6.4f)\n", iv_est, iv_se))

# estat firststage / estat endogenous -- diagnostics already shown via
# summary(iv_2sls, diagnostics = TRUE) above (weak-instrument and
# Wu-Hausman rows). No separate call needed.

################################################################################
# SECTION 6: MTE ESTIMATION -- POOLED POLYNOMIAL (MANUAL)
################################################################################

cat("\n==============================================\n")
cat("MTE ESTIMATION -- POOLED POLYNOMIAL\n")
cat("==============================================\n")

probit_formula <- as.formula(paste0("masters ~ ", z_inst, " + ", x_rhs))
probit_mod <- glm(probit_formula, data = df, family = binomial(link = "probit"))
print(summary(probit_mod))

df$phat <- predict(probit_mod, type = "response")
ga_coef <- coef(probit_mod)["ga_funding_adj"]
cat(sprintf("GA funding probit coefficient: %7.5f\n", ga_coef))
df$z_index <- predict(probit_mod, type = "link")   # linear (xb) index

df$phat2 <- df$phat^2
df$phat3 <- df$phat^3

cat("\n--- Quadratic MTE ---\n")
quad_formula <- as.formula(paste0(
  "ln_salary ~ masters + masters:phat + masters:phat2 + ", x_rhs, " + phat + phat2"
))
quad_mod <- lm(quad_formula, data = df)
vc_quad  <- vcovHC(quad_mod, type = "HC1")
print(coeftest(quad_mod, vcov = vc_quad))

b0_quad <- coef(quad_mod)["masters"]
b1_quad <- coef(quad_mod)["masters:phat"]
b2_quad <- coef(quad_mod)["masters:phat2"]
cat(sprintf("Quadratic MTE(u) = %6.4f + %6.4f*u + %6.4f*u^2\n", b0_quad, b1_quad, b2_quad))
ate_est_quad <- b0_quad + b1_quad / 2 + b2_quad / 3
cat(sprintf("ATE (quadratic): %6.4f\n", ate_est_quad))

cat("\n--- Cubic MTE ---\n")
cubic_formula <- as.formula(paste0(
  "ln_salary ~ masters + masters:phat + masters:phat2 + masters:phat3 + ",
  x_rhs, " + phat + phat2 + phat3"
))
cubic_mod <- lm(cubic_formula, data = df)
vc_cubic  <- vcovHC(cubic_mod, type = "HC1")
print(coeftest(cubic_mod, vcov = vc_cubic))

b0 <- coef(cubic_mod)["masters"]
b1 <- coef(cubic_mod)["masters:phat"]
b2 <- coef(cubic_mod)["masters:phat2"]
b3 <- coef(cubic_mod)["masters:phat3"]
cat(sprintf("Cubic MTE(u) = %7.4f + %7.4f*u + %7.4f*u^2 + %7.4f*u^3\n", b0, b1, b2, b3))

ate_est_cubic <- b0 + b1 / 2 + b2 / 3 + b3 / 4
cat(sprintf("Estimated ATE (cubic): %6.4f\n", ate_est_cubic))

df$mte_hat <- b0 + b1 * df$phat + b2 * df$phat2 + b3 * df$phat3

att_est <- mean(df$mte_hat[df$masters == 1])
cat(sprintf("Estimated ATT: %6.4f\n", att_est))
atu_est <- mean(df$mte_hat[df$masters == 0])
cat(sprintf("Estimated ATU: %6.4f\n", atu_est))

cat("\n--- mtefe Package Estimation: NOT REPLICATED IN R ---\n")
# Stata's community-contributed `mtefe` package (Andresen 2018, Stata
# Journal 18) estimates MTE using LOCAL INSTRUMENTAL VARIABLES (local IV)
# by default -- a fundamentally different estimator from the OLS-based
# polynomial control-function regression used in this script's Section 6.
# The pol(#) option controls the degree of polynomial used INSIDE that
# local-IV machinery; it does not mean "fit a plain OLS regression with a
# masters:phat interaction term of that degree."
#
# An earlier version of this translation approximated mtefe's pol(1) and
# pol(2) point estimates with simple linear/quadratic control-function
# OLS regressions, under the mistaken assumption that mtefe's polynomial
# option was equivalent to varying the order of the same OLS specification
# already used for the cubic model above. That assumption was incorrect:
# local IV and OLS-on-a-polynomial-control-function are different
# estimators, and they do not agree even at matching polynomial orders.
# Running this script against real data confirmed the two diverge sharply
# (e.g., the OLS-based approximation's ATU could come out far from
# Stata's true mtefe ATU, including disagreeing in sign), so that
# approximation has been removed here rather than left in under a
# misleading label.
#
# If you need a faithful local-IV MTE estimate in R, consider the
# `localIV` package (Zhou and Xie), which implements local-IV estimation
# directly, or report the Stata mtefe output as the authoritative source
# for that specific parameter set and use this script's cubic
# control-function ATE/ATT/ATU (Section 6 above) as the R-side parallel
# estimator instead -- they are conceptually related (both are polynomial
# MTE estimators) but are not numerically interchangeable, and the
# chapter prose should treat them as two distinct estimators being
# compared, not as one replicated in two languages.

cat("\n--- Heckman Selection Model ---\n")
# Two-step Heckman (Heckit): probit selection -> inverse Mills ratio ->
# OLS on the outcome equation augmented with the Mills ratio.
# Mirrors Stata's `heckman ..., twostep`.
select_formula <- as.formula(paste0("masters ~ ", z_inst, " + ", x_rhs))
outcome_formula_no_lambda <- as.formula(paste0("ln_salary ~ ", x_rhs))

select_probit <- glm(select_formula, data = df, family = binomial(link = "probit"))
xb_select <- predict(select_probit, type = "link")
df$.mills <- dnorm(xb_select) / pnorm(xb_select)   # inverse Mills ratio for masters==1

heck2_outcome_formula <- as.formula(paste0("ln_salary ~ ", x_rhs, " + .mills"))
heck_2step <- lm(heck2_outcome_formula, data = df[df$masters == 1, ])
print(summary(heck_2step))

heck2_lambda <- coef(heck_2step)[".mills"]
# rho and sigma from the two-step Heckman decomposition:
#   sigma = residual SD of the outcome equation (Heckman 1979 two-step)
#   rho   = lambda / sigma
heck2_resid_sd <- summary(heck_2step)$sigma
heck2_sigma <- heck2_resid_sd
heck2_rho   <- heck2_lambda / heck2_sigma
cat(sprintf("Heckman two-step: lambda = %6.4f  rho = %6.4f  sigma = %6.4f\n",
            heck2_lambda, heck2_rho, heck2_sigma))

# Drop the temporary inverse-Mills-ratio column now that the Heckman
# two-step regression is done with it. Stata .dta format does not allow
# variable names starting with a period, so this MUST be removed before
# write_dta() is called in Section 15, or that call will fail.
df$.mills <- NULL

cat("\n--- Heckman ML (full information) ---\n")
# Full maximum-likelihood Heckman selection model.
# Requires the `sampleSelection` package for an exact analog of Stata's
# `heckman ..., ` (ML, not twostep). If unavailable, this block is
# skipped with a warning rather than halting the whole script.
heck_ml_available <- requireNamespace("sampleSelection", quietly = TRUE)
if (heck_ml_available) {
  library(sampleSelection)
  heck_ml <- selection(
    selection = select_formula,
    outcome   = outcome_formula_no_lambda,
    data      = df,
    method    = "ml"
  )
  print(summary(heck_ml))
  heck_ml_rho    <- heck_ml$estimate[["rho"]]
  heck_ml_sigma  <- heck_ml$estimate[["sigma"]]
  heck_ml_lambda <- heck_ml_rho * heck_ml_sigma
  # LR test of rho = 0 (independence of equations)
  heck_ml_chi2 <- 2 * (heck_ml$maximum - logLik(lm(outcome_formula_no_lambda, data = df)))
  heck_ml_p    <- pchisq(heck_ml_chi2, df = 1, lower.tail = FALSE)
  cat(sprintf("Heckman ML: lambda = %6.4f  rho = %6.4f  LR chi2 = %6.2f (p = %5.4f)\n",
              heck_ml_lambda, heck_ml_rho, heck_ml_chi2, heck_ml_p))
} else {
  cat("NOTE: package 'sampleSelection' not installed -- skipping full-ML Heckman.\n")
  cat("      Install with install.packages('sampleSelection') to enable this block.\n")
  heck_ml_rho <- heck_ml_sigma <- heck_ml_lambda <- heck_ml_chi2 <- heck_ml_p <- NA_real_
}

################################################################################
# SECTION 6b: MTE BY GRADUATE PROGRAM AREA -- FULLY INTERACTED POLYNOMIAL
#
# Identification strategy:
#   The same probit propensity score and cubic polynomial framework from
#   Section 6 is extended by allowing the MTE function to shift both in
#   level and slope by graduate program area. ma_other is the omitted
#   (base) category. The interaction model recovers area-specific MTE
#   curves:
#
#     MTE_a(u) = [b0+d0_a] + [b1+d1_a]*u + [b2+d2_a]*u^2 + [b3+d3_a]*u^3
#
#   where a in (STEM, Business, Education, Health) and Other is the base.
#   For untreated observations all ma_* = 0, so no interaction fires.
#
# Area-specific treatment parameters:
#   ATE_a = (b0+d0_a) + (b1+d1_a)/2 + (b2+d2_a)/3 + (b3+d3_a)/4
#   ATT_a = E[MTE_a(phat) | masters=1, ma_a=1]
#
# Standard errors: see Section 6c (cluster bootstrap at state level)
################################################################################

cat("\n==============================================\n")
cat("MTE BY GRADUATE PROGRAM AREA\n")
cat("==============================================\n")

byarea_formula <- as.formula(paste0(
  "ln_salary ~ masters + masters:phat + masters:phat2 + masters:phat3 + ",
  "masters:ma_stem + masters:ma_business + masters:ma_education + masters:ma_health + ",
  "masters:ma_stem:phat + masters:ma_business:phat + masters:ma_education:phat + masters:ma_health:phat + ",
  "masters:ma_stem:phat2 + masters:ma_business:phat2 + masters:ma_education:phat2 + masters:ma_health:phat2 + ",
  "masters:ma_stem:phat3 + masters:ma_business:phat3 + masters:ma_education:phat3 + masters:ma_health:phat3 + ",
  x_rhs, " + phat + phat2 + phat3"
))
mte_byarea <- lm(byarea_formula, data = df)
vc_byarea  <- vcovHC(mte_byarea, type = "HC1")
print(coeftest(mte_byarea, vcov = vc_byarea))

cf <- coef(mte_byarea)

# NOTE: R's lm() term-naming for interactions follows the order variables
# appear in the formula, which can place "phat:ma_stem" rather than
# "ma_stem:phat" etc. depending on term ordering. To avoid silently
# pulling the wrong (or no) coefficient, look up each interaction term
# by its *set* of variables rather than an exact colon-separated string.
get_coef <- function(model, vars) {
  nm <- names(coef(model))
  parts <- strsplit(nm, ":")
  matches <- sapply(parts, function(p) setequal(p, vars))
  if (sum(matches) != 1) {
    stop(paste("get_coef: expected exactly one match for", paste(vars, collapse = ":"),
               "found", sum(matches)))
  }
  unname(coef(model)[matches])
}

B0 <- get_coef(mte_byarea, c("masters"))
B1 <- get_coef(mte_byarea, c("masters", "phat"))
B2 <- get_coef(mte_byarea, c("masters", "phat2"))
B3 <- get_coef(mte_byarea, c("masters", "phat3"))

d0_stem <- get_coef(mte_byarea, c("masters", "ma_stem"))
d1_stem <- get_coef(mte_byarea, c("masters", "ma_stem", "phat"))
d2_stem <- get_coef(mte_byarea, c("masters", "ma_stem", "phat2"))
d3_stem <- get_coef(mte_byarea, c("masters", "ma_stem", "phat3"))

d0_business <- get_coef(mte_byarea, c("masters", "ma_business"))
d1_business <- get_coef(mte_byarea, c("masters", "ma_business", "phat"))
d2_business <- get_coef(mte_byarea, c("masters", "ma_business", "phat2"))
d3_business <- get_coef(mte_byarea, c("masters", "ma_business", "phat3"))

d0_education <- get_coef(mte_byarea, c("masters", "ma_education"))
d1_education <- get_coef(mte_byarea, c("masters", "ma_education", "phat"))
d2_education <- get_coef(mte_byarea, c("masters", "ma_education", "phat2"))
d3_education <- get_coef(mte_byarea, c("masters", "ma_education", "phat3"))

d0_health <- get_coef(mte_byarea, c("masters", "ma_health"))
d1_health <- get_coef(mte_byarea, c("masters", "ma_health", "phat"))
d2_health <- get_coef(mte_byarea, c("masters", "ma_health", "phat2"))
d3_health <- get_coef(mte_byarea, c("masters", "ma_health", "phat3"))

# Composite area coefficients (base + differential)
c0_stem <- B0 + d0_stem;  c1_stem <- B1 + d1_stem
c2_stem <- B2 + d2_stem;  c3_stem <- B3 + d3_stem

c0_business <- B0 + d0_business;  c1_business <- B1 + d1_business
c2_business <- B2 + d2_business;  c3_business <- B3 + d3_business

c0_education <- B0 + d0_education;  c1_education <- B1 + d1_education
c2_education <- B2 + d2_education;  c3_education <- B3 + d3_education

c0_health <- B0 + d0_health;  c1_health <- B1 + d1_health
c2_health <- B2 + d2_health;  c3_health <- B3 + d3_health

# Area-specific MTE functions
cat("\n--- Area-Specific MTE Functions ---\n")
cat(sprintf("  Base (Other): %7.4f + %7.4f*u + %7.4f*u^2 + %7.4f*u^3\n", B0, B1, B2, B3))
cat(sprintf("  STEM:         %7.4f + %7.4f*u + %7.4f*u^2 + %7.4f*u^3\n", c0_stem, c1_stem, c2_stem, c3_stem))
cat(sprintf("  Business:     %7.4f + %7.4f*u + %7.4f*u^2 + %7.4f*u^3\n", c0_business, c1_business, c2_business, c3_business))
cat(sprintf("  Education:    %7.4f + %7.4f*u + %7.4f*u^2 + %7.4f*u^3\n", c0_education, c1_education, c2_education, c3_education))
cat(sprintf("  Health:       %7.4f + %7.4f*u + %7.4f*u^2 + %7.4f*u^3\n", c0_health, c1_health, c2_health, c3_health))

# Area-specific ATE (integral of MTE over [0,1])
ate_other     <- B0           + B1/2           + B2/3           + B3/4
ate_stem      <- c0_stem      + c1_stem/2      + c2_stem/3      + c3_stem/4
ate_business  <- c0_business  + c1_business/2  + c2_business/3  + c3_business/4
ate_education <- c0_education + c1_education/2 + c2_education/3 + c3_education/4
ate_health    <- c0_health    + c1_health/2    + c2_health/3    + c3_health/4

cat("\n--- Area-Specific ATE (integral_0^1 MTE_a(u) du) ---\n")
cat(sprintf("  ATE (Other):     %6.4f\n", ate_other))
cat(sprintf("  ATE (STEM):      %6.4f\n", ate_stem))
cat(sprintf("  ATE (Business):  %6.4f\n", ate_business))
cat(sprintf("  ATE (Education): %6.4f\n", ate_education))
cat(sprintf("  ATE (Health):    %6.4f\n", ate_health))

# Area-specific MTE_hat variables
df$mte_hat_other     <- B0           + B1           * df$phat + B2           * df$phat2 + B3           * df$phat3
df$mte_hat_stem      <- c0_stem      + c1_stem      * df$phat + c2_stem      * df$phat2 + c3_stem      * df$phat3
df$mte_hat_business  <- c0_business  + c1_business  * df$phat + c2_business  * df$phat2 + c3_business  * df$phat3
df$mte_hat_education <- c0_education + c1_education * df$phat + c2_education * df$phat2 + c3_education * df$phat3
df$mte_hat_health    <- c0_health    + c1_health    * df$phat + c2_health    * df$phat2 + c3_health    * df$phat3

# Area-specific ATT
att_other     <- mean(df$mte_hat_other[df$ma_other == 1])
att_stem      <- mean(df$mte_hat_stem[df$ma_stem == 1])
att_business  <- mean(df$mte_hat_business[df$ma_business == 1])
att_education <- mean(df$mte_hat_education[df$ma_education == 1])
att_health    <- mean(df$mte_hat_health[df$ma_health == 1])

cat("\n--- Area-Specific ATT ---\n")
cat(sprintf("  ATT (Other):     %6.4f\n", att_other))
cat(sprintf("  ATT (STEM):      %6.4f\n", att_stem))
cat(sprintf("  ATT (Business):  %6.4f\n", att_business))
cat(sprintf("  ATT (Education): %6.4f\n", att_education))
cat(sprintf("  ATT (Health):    %6.4f\n", att_health))

atu_pooled_untreated <- mean(df$mte_hat[df$masters == 0])
cat(sprintf("\n  ATU (pooled): %6.4f\n", atu_pooled_untreated))

################################################################################
# SECTION 6b-ATU: PROSPECTIVE PROGRAM AREA ASSIGNMENT FOR UNTREATED
#
# ATU_a = E[MTE_a(phat) | masters==0, prospective_area==a]
#
# Because ma_* = 0 for all untreated observations by construction,
# area-specific ATU requires assigning untreated individuals to the
# graduate program area they would most likely have entered, given their
# undergraduate major field and the same transition probabilities used
# for the treated group in Section 1b.
#
# This is a counterfactual assignment -- not an observed classification.
# Prose must note that ATU_a estimates the return for untreated students
# who, had they completed a master's degree, would most likely have
# entered program area a given their undergraduate background.
#
# Transition probabilities (mirroring Section 1b):
#   STEM:      stem_major,    p <= 0.55
#   Business:  bus_major,     p <= 0.65 (conditional on not STEM)
#   Education: ed_major,      p <= 0.70 (conditional on not STEM/Bus)
#   Health:    socsci_major   p <= 0.40 OR stem_major p in (0.55, 0.75]
#   Other:     residual
#
# Seed 20260102 (distinct from treated seed 20251130) ensures
# reproducibility without cross-contaminating the treated assignment.
################################################################################

cat("\n==============================================\n")
cat("SECTION 6b-ATU: PROSPECTIVE PROGRAM AREA (UNTREATED)\n")
cat("==============================================\n")

df$ma_stem_pro      <- 0
df$ma_business_pro  <- 0
df$ma_education_pro <- 0
df$ma_health_pro    <- 0
df$ma_other_pro     <- 0

set.seed(20260102)
df$.rma_u <- ifelse(df$masters == 0, runif(nrow(df)), NA_real_)

df$ma_stem_pro <- ifelse(df$masters == 0 & df$stem_major == 1 & df$.rma_u <= 0.55,
                          1, df$ma_stem_pro)
df$ma_business_pro <- ifelse(df$masters == 0 & df$bus_major == 1 & df$.rma_u <= 0.65 &
                                df$ma_stem_pro == 0, 1, df$ma_business_pro)
df$ma_education_pro <- ifelse(df$masters == 0 & df$ed_major == 1 & df$.rma_u <= 0.70 &
                                 df$ma_stem_pro == 0 & df$ma_business_pro == 0,
                               1, df$ma_education_pro)
df$ma_health_pro <- ifelse(df$masters == 0 & df$socsci_major == 1 & df$.rma_u <= 0.40 &
                              df$ma_stem_pro == 0 & df$ma_business_pro == 0 & df$ma_education_pro == 0,
                            1, df$ma_health_pro)
df$ma_health_pro <- ifelse(df$masters == 0 & df$stem_major == 1 &
                              df$.rma_u > 0.55 & df$.rma_u <= 0.75 & df$ma_stem_pro == 0,
                            1, df$ma_health_pro)
df$ma_other_pro <- ifelse(df$masters == 0 & df$ma_stem_pro == 0 & df$ma_business_pro == 0 &
                             df$ma_education_pro == 0 & df$ma_health_pro == 0,
                           1, df$ma_other_pro)
df$.rma_u <- NULL

# Verification
n_untreated <- sum(df$masters == 0)
cat("Total untreated:", n_untreated, "\n")
for (a in c("stem", "business", "education", "health", "other")) {
  varname <- paste0("ma_", a, "_pro")
  cnt <- sum(df$masters == 0 & df[[varname]] == 1)
  cat(sprintf("  ma_%s_pro: %d  (%5.1f%%)\n", a, cnt, 100 * cnt / n_untreated))
}

# Check mutual exclusivity
pro_check <- with(df, ma_stem_pro + ma_business_pro + ma_education_pro +
                        ma_health_pro + ma_other_pro)
n_bad <- sum(df$masters == 0 & pro_check != 1)
if (n_bad > 0) {
  cat("WARNING:", n_bad, "untreated obs with != 1 prospective area flag\n")
} else {
  cat("CHECK PASSED: all untreated obs have exactly 1 prospective area\n")
}

# Area-specific ATU point estimates
cat("\n--- Area-Specific ATU (prospective assignment) ---\n")
atu_list <- list()
for (a in c("stem", "business", "education", "health", "other")) {
  mte_varname <- paste0("mte_hat_", a)
  pro_varname <- paste0("ma_", a, "_pro")
  sub <- df[df$masters == 0 & df[[pro_varname]] == 1, ]
  atu_list[[a]] <- mean(sub[[mte_varname]])
  cat(sprintf("  ATU (%s): %6.4f\n", a, atu_list[[a]]))
}
atu_stem      <- atu_list[["stem"]]
atu_business  <- atu_list[["business"]]
atu_education <- atu_list[["education"]]
atu_health    <- atu_list[["health"]]
atu_other     <- atu_list[["other"]]

cat("\n==============================================\n")
cat("AREA-SPECIFIC TREATMENT PARAMETER SUMMARY\n")
cat("==============================================\n")
cat("Area          ATE        ATT        ATU (prospective)\n")
cat("----------------------------------------------------\n")
cat(sprintf("  Other     %7.4f    %7.4f    %7.4f\n", ate_other, att_other, atu_other))
cat(sprintf("  STEM      %7.4f    %7.4f    %7.4f\n", ate_stem, att_stem, atu_stem))
cat(sprintf("  Business  %7.4f    %7.4f    %7.4f\n", ate_business, att_business, atu_business))
cat(sprintf("  Education %7.4f    %7.4f    %7.4f\n", ate_education, att_education, atu_education))
cat(sprintf("  Health    %7.4f    %7.4f    %7.4f\n", ate_health, att_health, atu_health))

################################################################################
# SECTION 6c: BOOTSTRAP INFRASTRUCTURE
#
# Implementation: manual for-loop bootstrap, mirroring Stata's postfile/
# forvalues manual approach (the Stata original explicitly avoids the
# `bootstrap` command and `program...end` due to a persistent r(198)
# execution-context bug; R has no equivalent bug, but the loop structure
# is kept faithful to the original for one-to-one comparison).
#
# 6c-i  Manual cluster bootstrap (cluster = state), R = 500 reps
################################################################################

R_REPS <- 500
n_ok <- 0

cat(sprintf("\nRunning manual cluster bootstrap (G=50, R=%d reps)...\n", R_REPS))
cat("Each dot = 10 reps completed\n")
cat("NOTE: this step is the slowest part of the script -- expect roughly",
    "1-2 minutes to complete in Stata. The R translation below uses a",
    "straightforward per-state filter inside each replication rather than",
    "a pre-built state index; this is simpler to follow but may run",
    "noticeably slower than the Stata original, especially R=500 reps over",
    "~50 state clusters. If this loop is too slow in practice, consider",
    "pre-splitting df by state into a list once (e.g. split(df, df$state))",
    "outside the loop and sampling from that list by name each iteration,",
    "which avoids repeating the df$state == s scan on every draw.\n")

states <- unique(df$state)
n_states <- length(states)

# Storage: one row per successful replication
bs_cols <- c("b_ate", "b_att", "b_atu",
             "b_ate_stem", "b_att_stem", "b_atu_stem",
             "b_ate_bus", "b_att_bus", "b_atu_bus",
             "b_ate_ed", "b_att_ed", "b_atu_ed",
             "b_ate_hlth", "b_att_hlth", "b_atu_hlth",
             "b_ate_oth", "b_att_oth", "b_atu_oth")
bs_results <- matrix(NA_real_, nrow = R_REPS, ncol = length(bs_cols))
colnames(bs_results) <- bs_cols

set.seed(20260101)

for (b in 1:R_REPS) {

  ok <- TRUE

  # Cluster bootstrap: resample states with replacement, then take all
  # observations belonging to each resampled state (mirrors Stata's
  # `bsample, cluster(state) idcluster(newstate)`).
  resampled_states <- tryCatch(sample(states, n_states, replace = TRUE),
                                error = function(e) NULL)
  if (is.null(resampled_states)) ok <- FALSE

  if (ok) {
    boot_rows <- lapply(seq_along(resampled_states), function(i) {
      s <- resampled_states[i]
      sub <- df[df$state == s, ]
      sub$newstate <- i
      sub
    })
    boot_df <- tryCatch(do.call(rbind, boot_rows), error = function(e) NULL)
    if (is.null(boot_df)) ok <- FALSE
  }

  # --- Probit ---
  if (ok) {
    pb_mod <- tryCatch(
      glm(probit_formula, data = boot_df, family = binomial(link = "probit")),
      error = function(e) NULL
    )
    if (is.null(pb_mod)) ok <- FALSE
  }
  if (ok) {
    boot_df$pb_  <- predict(pb_mod, type = "response")
    boot_df$pb2_ <- boot_df$pb_^2
    boot_df$pb3_ <- boot_df$pb_^3
  }

  # --- Pooled cubic MTE ---
  if (ok) {
    pooled_boot_formula <- as.formula(paste0(
      "ln_salary ~ masters + masters:pb_ + masters:pb2_ + masters:pb3_ + ",
      x_rhs, " + pb_ + pb2_ + pb3_"
    ))
    pooled_boot_mod <- tryCatch(lm(pooled_boot_formula, data = boot_df),
                                 error = function(e) NULL)
    if (is.null(pooled_boot_mod)) ok <- FALSE
  }

  if (ok) {
    r0 <- get_coef(pooled_boot_mod, c("masters"))
    r1 <- get_coef(pooled_boot_mod, c("masters", "pb_"))
    r2 <- get_coef(pooled_boot_mod, c("masters", "pb2_"))
    r3 <- get_coef(pooled_boot_mod, c("masters", "pb3_"))
    b_ate_r <- r0 + r1/2 + r2/3 + r3/4
    mb <- r0 + r1 * boot_df$pb_ + r2 * boot_df$pb2_ + r3 * boot_df$pb3_
    b_att_r <- mean(mb[boot_df$masters == 1])
    b_atu_r <- mean(mb[boot_df$masters == 0])
  }

  # --- Fully interacted MTE ---
  if (ok) {
    interact_boot_formula <- as.formula(paste0(
      "ln_salary ~ masters + masters:pb_ + masters:pb2_ + masters:pb3_ + ",
      "masters:ma_stem + masters:ma_business + masters:ma_education + masters:ma_health + ",
      "masters:ma_stem:pb_ + masters:ma_business:pb_ + masters:ma_education:pb_ + masters:ma_health:pb_ + ",
      "masters:ma_stem:pb2_ + masters:ma_business:pb2_ + masters:ma_education:pb2_ + masters:ma_health:pb2_ + ",
      "masters:ma_stem:pb3_ + masters:ma_business:pb3_ + masters:ma_education:pb3_ + masters:ma_health:pb3_ + ",
      x_rhs, " + pb_ + pb2_ + pb3_"
    ))
    interact_boot_mod <- tryCatch(lm(interact_boot_formula, data = boot_df),
                                   error = function(e) NULL)
    if (is.null(interact_boot_mod)) ok <- FALSE
  }

  if (ok) {
    BB0 <- get_coef(interact_boot_mod, c("masters"))
    BB1 <- get_coef(interact_boot_mod, c("masters", "pb_"))
    BB2 <- get_coef(interact_boot_mod, c("masters", "pb2_"))
    BB3 <- get_coef(interact_boot_mod, c("masters", "pb3_"))

    area_boot_result <- function(area, pro_varname) {
      ma_var <- paste0("ma_", area)
      D0 <- get_coef(interact_boot_mod, c("masters", ma_var))
      D1 <- get_coef(interact_boot_mod, c("masters", ma_var, "pb_"))
      D2 <- get_coef(interact_boot_mod, c("masters", ma_var, "pb2_"))
      D3 <- get_coef(interact_boot_mod, c("masters", ma_var, "pb3_"))
      C0 <- BB0 + D0; C1 <- BB1 + D1; C2 <- BB2 + D2; C3 <- BB3 + D3
      b_ate_area <- C0 + C1/2 + C2/3 + C3/4
      ms <- C0 + C1 * boot_df$pb_ + C2 * boot_df$pb2_ + C3 * boot_df$pb3_
      b_att_area <- mean(ms[boot_df[[ma_var]] == 1])
      b_atu_area <- mean(ms[boot_df$masters == 0 & boot_df[[pro_varname]] == 1])
      c(ate = b_ate_area, att = b_att_area, atu = b_atu_area)
    }

    res_stem <- area_boot_result("stem", "ma_stem_pro")
    res_bus  <- area_boot_result("business", "ma_business_pro")
    res_ed   <- area_boot_result("education", "ma_education_pro")
    res_hlth <- area_boot_result("health", "ma_health_pro")

    # Other (base, no interaction differential)
    b_ate_oth_r <- BB0 + BB1/2 + BB2/3 + BB3/4
    mb_other <- BB0 + BB1 * boot_df$pb_ + BB2 * boot_df$pb2_ + BB3 * boot_df$pb3_
    b_att_oth_r <- mean(mb_other[boot_df$ma_other == 1])
    b_atu_oth_r <- mean(mb_other[boot_df$masters == 0 & boot_df$ma_other_pro == 1])

    bs_results[b, ] <- c(
      b_ate_r, b_att_r, b_atu_r,
      res_stem["ate"], res_stem["att"], res_stem["atu"],
      res_bus["ate"],  res_bus["att"],  res_bus["atu"],
      res_ed["ate"],   res_ed["att"],   res_ed["atu"],
      res_hlth["ate"], res_hlth["att"], res_hlth["atu"],
      b_ate_oth_r, b_att_oth_r, b_atu_oth_r
    )
    n_ok <- n_ok + 1
  }

  if (b %% 10 == 0) cat(".")
}
cat("\n")

cat(sprintf("\nBootstrap complete: %d of %d reps successful\n", n_ok, R_REPS))

bs_df <- as.data.frame(bs_results)
bs_df <- bs_df[stats::complete.cases(bs_df), ]   # drop failed reps (all-NA rows)

# Extract SEs as standard deviations of the bootstrap distribution
ate_se <- sd(bs_df$b_ate);  att_se <- sd(bs_df$b_att);  atu_se <- sd(bs_df$b_atu)
ate_se_stem <- sd(bs_df$b_ate_stem); att_se_stem <- sd(bs_df$b_att_stem); atu_se_stem <- sd(bs_df$b_atu_stem)
ate_se_business <- sd(bs_df$b_ate_bus); att_se_business <- sd(bs_df$b_att_bus); atu_se_business <- sd(bs_df$b_atu_bus)
ate_se_education <- sd(bs_df$b_ate_ed); att_se_education <- sd(bs_df$b_att_ed); atu_se_education <- sd(bs_df$b_atu_ed)
ate_se_health <- sd(bs_df$b_ate_hlth); att_se_health <- sd(bs_df$b_att_hlth); atu_se_health <- sd(bs_df$b_atu_hlth)
ate_se_other <- sd(bs_df$b_ate_oth); att_se_other <- sd(bs_df$b_att_oth); atu_se_other <- sd(bs_df$b_atu_oth)



# -----------------------------------------------------------------------
# 6c-iii  Wild cluster bootstrap for OLS and IV stages
#   Rademacher weights, 9999 reps. Requires package `fwildclusterboot`.
# -----------------------------------------------------------------------

wild_boot_available <- requireNamespace("fwildclusterboot", quietly = TRUE)

cat("\n--- Wild Cluster Bootstrap: OLS ---\n")
if (wild_boot_available) {
  library(fwildclusterboot)
  boot_ols <- boottest(ols_naive, clustid = "state", param = "masters",
                        B = 9999, data = df)
  print(boot_ols)
  cat(sprintf("  p-value (wild cluster): %6.4f\n", boot_ols$p_val))
} else {
  cat("NOTE: package 'fwildclusterboot' not installed -- skipping wild cluster bootstrap.\n")
  cat("      Install with install.packages('fwildclusterboot') to enable this block.\n")
}

cat("\n--- Wild Cluster Bootstrap: IV ---\n")
if (wild_boot_available) {
  boot_iv <- boottest(iv_2sls, clustid = "state", param = "masters",
                       B = 9999, data = df)
  print(boot_iv)
  cat(sprintf("  p-value (wild cluster): %6.4f\n", boot_iv$p_val))
} else {
  cat("NOTE: package 'fwildclusterboot' not installed -- skipping wild cluster bootstrap.\n")
}

cat("\n--- Bootstrap SEs: Pooled Parameters ---\n")
cat(sprintf("  ATE = %6.4f  (Bootstrap SE = %6.4f)\n", ate_est_cubic, ate_se))
cat(sprintf("  ATT = %6.4f  (Bootstrap SE = %6.4f)\n", att_est, att_se))
cat(sprintf("  ATU = %6.4f  (Bootstrap SE = %6.4f)\n", atu_est, atu_se))

cat("\n--- Bootstrap SEs: Area-Specific ATE ---\n")
cat("Area          Point Est   BS SE    95% CI\n")
cat("-----------------------------------------------\n")
area_ate_vals <- list(other = ate_other, stem = ate_stem, business = ate_business,
                       education = ate_education, health = ate_health)
area_ate_se   <- list(other = ate_se_other, stem = ate_se_stem, business = ate_se_business,
                       education = ate_se_education, health = ate_se_health)
for (a in c("other", "stem", "business", "education", "health")) {
  lo <- area_ate_vals[[a]] - 1.96 * area_ate_se[[a]]
  hi <- area_ate_vals[[a]] + 1.96 * area_ate_se[[a]]
  cat(sprintf("  %-9s %7.4f    %6.4f   [%6.4f, %6.4f]\n",
              a, area_ate_vals[[a]], area_ate_se[[a]], lo, hi))
}

sig_label <- function(lo, hi) if (lo > 0 || hi < 0) "***" else "   "

cat("\n--- Bootstrap SEs: Area-Specific ATT ---\n")
cat("Area          Point Est   BS SE    95% CI                  Sig\n")
cat("---------------------------------------------------------------\n")
area_att_vals <- list(other = att_other, stem = att_stem, business = att_business,
                       education = att_education, health = att_health)
area_att_se   <- list(other = att_se_other, stem = att_se_stem, business = att_se_business,
                       education = att_se_education, health = att_se_health)
for (a in c("other", "stem", "business", "education", "health")) {
  lo <- area_att_vals[[a]] - 1.96 * area_att_se[[a]]
  hi <- area_att_vals[[a]] + 1.96 * area_att_se[[a]]
  cat(sprintf("  %-9s %7.4f    %6.4f   [%7.4f, %7.4f]   %s\n",
              a, area_att_vals[[a]], area_att_se[[a]], lo, hi, sig_label(lo, hi)))
}
cat("  *** = 95% CI excludes zero (p < 0.05, two-tailed)\n")

cat("\n--- Bootstrap SEs: Area-Specific ATU (prospective assignment) ---\n")
cat("Area          Point Est   BS SE    95% CI                  Sig\n")
cat("---------------------------------------------------------------\n")
area_atu_vals <- list(other = atu_other, stem = atu_stem, business = atu_business,
                       education = atu_education, health = atu_health)
area_atu_se   <- list(other = atu_se_other, stem = atu_se_stem, business = atu_se_business,
                       education = atu_se_education, health = atu_se_health)
for (a in c("other", "stem", "business", "education", "health")) {
  lo <- area_atu_vals[[a]] - 1.96 * area_atu_se[[a]]
  hi <- area_atu_vals[[a]] + 1.96 * area_atu_se[[a]]
  cat(sprintf("  %-9s %7.4f    %6.4f   [%7.4f, %7.4f]   %s\n",
              a, area_atu_vals[[a]], area_atu_se[[a]], lo, hi, sig_label(lo, hi)))
}
cat("  *** = 95% CI excludes zero (p < 0.05, two-tailed)\n")
cat("  Note: ATU based on prospective program area assignment (seed 20260102).\n")

################################################################################
# SECTION 7: Results Comparison
################################################################################

cat("\n==============================================\n")
cat("RESULTS COMPARISON\n")
cat("==============================================\n")

cat(sprintf("  Naive OLS:              %6.4f (SE = %6.4f -- likely biased)\n", ols_est, ols_se))
cat(sprintf("  IV/LATE:                %6.4f (SE = %6.4f -- complier effect)\n", iv_est, iv_se))
cat(sprintf("  MTE-based ATE (cubic):  %6.4f (BS SE = %6.4f)\n", ate_est_cubic, ate_se))
cat(sprintf("  MTE-based ATT:          %6.4f (BS SE = %6.4f)\n", att_est, att_se))
cat(sprintf("  MTE-based ATU:          %6.4f (BS SE = %6.4f)\n", atu_est, atu_se))
cat("  NOTE: mtefe (local-IV) ATE/ATT/ATU are not replicated in this R\n")
cat("        translation -- see Stata output for that estimator's values.\n")

if (att_est > ate_est_cubic && ate_est_cubic > atu_est) {
  cat("  ATT > ATE > ATU: POSITIVE SELECTION on gains\n")
} else if (att_est < ate_est_cubic && ate_est_cubic < atu_est) {
  cat("  ATT < ATE < ATU: NEGATIVE SELECTION on gains\n")
} else {
  cat("  Mixed selection pattern\n")
}

ols_bias <- (ols_est - ate_est_cubic) / ate_est_cubic * 100
cat(sprintf("OLS BIAS: %5.1f%% relative to MTE-based ATE\n", ols_bias))

cat("\nOLS vs. IV Estimates of Master's Degree Effect\n")
comp_tbl <- data.frame(
  Variable = "masters",
  ols_naive = sprintf("%7.4f (%7.4f)", ols_est, ols_se),
  iv_2sls   = sprintf("%7.4f (%7.4f)", iv_est, iv_se)
)
print(comp_tbl, row.names = FALSE)
cat(sprintf("N: ols_naive = %d, iv_2sls = %d\n", nobs(ols_naive), nobs(iv_2sls)))
cat(sprintf("r2: ols_naive = %.4f, iv_2sls = %.4f\n",
            summary(ols_naive)$r.squared, summary(iv_2sls)$r.squared))

################################################################################
# SECTION 8: MTE Visualization
################################################################################

cat("\n==============================================\n")
cat("MTE VISUALIZATION\n")
cat("==============================================\n")

# Fig 11.3: pooled MTE curve
grid_df <- data.frame(u = (1:100) / 100)
grid_df$mte_est <- b0 + b1 * grid_df$u + b2 * grid_df$u^2 + b3 * grid_df$u^3

fig11_3 <- ggplot(grid_df, aes(x = u, y = mte_est)) +
  geom_line(linewidth = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "Estimated MTE Curve - Pooled",
    subtitle = "Master's Degree Effect on Log Salary",
    x = "u (Unobserved Resistance to Treatment)",
    y = "Marginal Treatment Effect",
    caption = "Declining MTE indicates positive selection on gains"
  ) +
  theme_springer()

ggsave(file.path(graphs_dir, "fig11_3_mte_curve_R.png"),
       fig11_3, width = 12, height = 12 * 0.75, units = "in", dpi = 100)

# Fig 11.7: MTE by propensity-score decile
df$p_decile <- ntile(df$phat, 10)

decile_df <- df %>%
  group_by(p_decile) %>%
  summarise(mte_mean = mean(mte_hat), mte_sd = sd(mte_hat), n = n(), .groups = "drop")

cat("\nEstimated MTE by Propensity Score Decile:\n")
print(decile_df)

fig11_7 <- ggplot(decile_df, aes(x = p_decile, y = mte_mean)) +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(shape = 18, size = 3.5, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "Estimated MTE by Propensity Score Decile",
    subtitle = "Evidence of Treatment Effect Heterogeneity",
    x = "Propensity Score Decile", y = "Mean Estimated MTE"
  ) +
  theme_springer()

ggsave(file.path(graphs_dir, "fig11_7_mte_by_decile_R.png"),
       fig11_7, width = 12, height = 12 * 0.75, units = "in", dpi = 100)

# MTE curves by program area
# FIX: restrict each area-specific curve to that area's empirical
# propensity-score support (treated obs, ma_a==1). The cubic control-
# function polynomial is only identified where data exist; extrapolating
# it across the full [0,1] range previously caused curves for small-N
# areas (e.g., Business, N=179 treated) to diverge sharply near the
# boundary.
area_support <- list()
for (a in c("stem", "business", "education", "health")) {
  varname <- paste0("ma_", a)
  sub_phat <- df$phat[df$masters == 1 & df[[varname]] == 1]
  area_support[[a]] <- c(lo = min(sub_phat), hi = max(sub_phat))
}
sub_phat_other <- df$phat[df$masters == 1 & df$ma_other == 1]
area_support[["other"]] <- c(lo = min(sub_phat_other), hi = max(sub_phat_other))

cat("\nEmpirical phat support by area (treated obs):\n")
for (a in c("stem", "business", "education", "health", "other")) {
  cat(sprintf("  %s: [%5.3f, %5.3f]\n", a, area_support[[a]]["lo"], area_support[[a]]["hi"]))
}

byarea_grid <- data.frame(u = (1:100) / 100)
byarea_grid$mte_other    <- B0           + B1           * byarea_grid$u + B2           * byarea_grid$u^2 + B3           * byarea_grid$u^3
byarea_grid$mte_stem     <- c0_stem      + c1_stem      * byarea_grid$u + c2_stem      * byarea_grid$u^2 + c3_stem      * byarea_grid$u^3
byarea_grid$mte_business <- c0_business  + c1_business  * byarea_grid$u + c2_business  * byarea_grid$u^2 + c3_business  * byarea_grid$u^3
byarea_grid$mte_educ     <- c0_education + c1_education * byarea_grid$u + c2_education * byarea_grid$u^2 + c3_education * byarea_grid$u^3
byarea_grid$mte_health   <- c0_health    + c1_health    * byarea_grid$u + c2_health    * byarea_grid$u^2 + c3_health    * byarea_grid$u^3

# Truncate each series to its area's empirical support (mirrors the
# Stata `replace mte_* = . if u < lo_* | u > hi_*` fix).
byarea_grid$mte_other[byarea_grid$u < area_support[["other"]]["lo"] |
                         byarea_grid$u > area_support[["other"]]["hi"]] <- NA
byarea_grid$mte_stem[byarea_grid$u < area_support[["stem"]]["lo"] |
                        byarea_grid$u > area_support[["stem"]]["hi"]] <- NA
byarea_grid$mte_business[byarea_grid$u < area_support[["business"]]["lo"] |
                            byarea_grid$u > area_support[["business"]]["hi"]] <- NA
byarea_grid$mte_educ[byarea_grid$u < area_support[["education"]]["lo"] |
                        byarea_grid$u > area_support[["education"]]["hi"]] <- NA
byarea_grid$mte_health[byarea_grid$u < area_support[["health"]]["lo"] |
                          byarea_grid$u > area_support[["health"]]["hi"]] <- NA

byarea_long <- byarea_grid %>%
  pivot_longer(cols = starts_with("mte_"), names_to = "area", values_to = "mte") %>%
  filter(!is.na(mte))

# Relabel area names using a plain named-vector lookup rather than
# dplyr::recode()/case_match(), since both have moved through several
# rounds of deprecation in recent dplyr versions (recode() superseded by
# case_match() in dplyr 1.1, which was itself deprecated in favor of
# recode_values()/replace_values() in dplyr 1.2). A base-R lookup avoids
# depending on whichever recoding function happens to be current.
area_labels <- c(
  mte_health   = "Health & Related",
  mte_stem     = "STEM",
  mte_business = "Business",
  mte_educ     = "Education",
  mte_other    = "Other (base)"
)
byarea_long$area <- area_labels[byarea_long$area]

byarea_long$area <- factor(byarea_long$area,
  levels = c("Health & Related", "STEM", "Business", "Education", "Other (base)"))

fig11_5 <- ggplot(byarea_long, aes(x = u, y = mte, linetype = area, color = area)) +
  geom_line(linewidth = 0.9) +
  scale_linetype_manual(values = c(
    "Health & Related" = "solid", "STEM" = "dashed",
    "Business" = "longdash", "Education" = "solid", "Other (base)" = "dashed"
  )) +
  scale_color_manual(values = c(
    "Health & Related" = "black", "STEM" = "black",
    "Business" = "black", "Education" = "grey50", "Other (base)" = "grey50"
  )) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
  labs(
    title = "MTE Curves by Graduate Program Area",
    subtitle = "Field-specific returns to master's degree",
    x = "u (Unobserved Resistance to Treatment)",
    y = "Marginal Treatment Effect",
    caption = "Curves restricted to each area's empirical propensity-score support (treated obs).",
    color = NULL, linetype = NULL
  ) +
  theme_springer() +
  theme(legend.position = "bottom")

ggsave(file.path(graphs_dir, "fig11_5_mte_byarea_curve_R.png"),
       fig11_5, width = 12, height = 12 * 0.75, units = "in", dpi = 100)

################################################################################
# SECTION 9: Basic Policy Simulation (PRTE)
################################################################################

ga_current <- mean(df$ga_funding_adj)
ga_new <- ga_current * 1.2
cat(sprintf("Current mean GA: $%5.2fk  Proposed (20%% increase): $%5.2fk\n", ga_current, ga_new))

p_new   <- pnorm(df$z_index + ga_coef * (ga_new - df$ga_funding_adj))
delta_p <- p_new - df$phat
cat(sprintf("Average increase in Pr(Master's): %6.4f\n", mean(delta_p)))

complier_weight <- ifelse(delta_p > 0, delta_p / mean(delta_p), NA)
prte_20pct <- weighted.mean(df$mte_hat[delta_p > 0], complier_weight[delta_p > 0])
cat(sprintf("Approximate PRTE (20%% GA increase): %6.4f\n", prte_20pct))

################################################################################
# SECTION 10: MPRTE -- Scenarios 1-4 (Original)
################################################################################

cat("\n==============================================\n")
cat("MPRTE - SCENARIOS 1-4 (ORIGINAL)\n")
cat("==============================================\n")

# Scenario 1: Uniform $1k
p_new_unif    <- pnorm(df$z_index + ga_coef * 1)
delta_p_unif  <- p_new_unif - df$phat
response_unif <- dnorm(qnorm(df$phat)) * ga_coef
mte_weighted_unif <- df$mte_hat * response_unif
mprte_unif <- sum(mte_weighted_unif) / sum(response_unif)
cat(sprintf("MPRTE (uniform $1k): %6.4f\n", mprte_unif))
prte_unif_discrete <- weighted.mean(df$mte_hat[delta_p_unif > 0],
                                     delta_p_unif[delta_p_unif > 0])
cat(sprintf("PRTE  (discrete $1k): %6.4f\n", prte_unif_discrete))

# Scenario 2: Targeted low-income ($2k)
targeted_lowinc <- as.integer(df$parent_income_q <= 2)
p_new_lowinc    <- pnorm(df$z_index + ga_coef * 2 * targeted_lowinc)
response_lowinc <- dnorm(qnorm(df$phat)) * ga_coef * 2 * targeted_lowinc
mte_weighted_lowinc <- df$mte_hat * response_lowinc
mprte_lowinc <- sum(mte_weighted_lowinc[targeted_lowinc == 1]) /
                  sum(response_lowinc[targeted_lowinc == 1])
cat(sprintf("MPRTE (targeted low-income): %6.4f\n", mprte_lowinc))

# Scenario 3: STEM GA ($3k)
p_new_stem    <- pnorm(df$z_index + ga_coef * 3 * df$stem_major)
response_stem <- dnorm(qnorm(df$phat)) * ga_coef * 3 * df$stem_major
mte_weighted_stem <- df$mte_hat * response_stem
mprte_stem <- sum(mte_weighted_stem[df$stem_major == 1]) /
                sum(response_stem[df$stem_major == 1])
cat(sprintf("MPRTE (STEM enhancement): %6.4f\n", mprte_stem))

# Scenario 4: Education ($2.5k)
p_new_ed    <- pnorm(df$z_index + ga_coef * 2.5 * df$ed_major)
response_ed <- dnorm(qnorm(df$phat)) * ga_coef * 2.5 * df$ed_major
mte_weighted_ed <- df$mte_hat * response_ed
mprte_ed <- sum(mte_weighted_ed[df$ed_major == 1]) / sum(response_ed[df$ed_major == 1])
cat(sprintf("MPRTE (education major support): %6.4f\n", mprte_ed))

################################################################################
# SECTION 10b: MPRTE BY GRADUATE PROGRAM AREA -- Scenarios 5-8
#
# These scenarios use the area-specific MTE function MTE_a(phat) from
# Section 6b rather than the pooled mte_hat. Each scenario targets a
# $2,500 GA increase at the undergraduate pipeline that feeds most
# strongly into the graduate field of interest.
#
# MPRTE_a = sum_i (MTE_a(phat_i) * h_a(X_i)) / sum_i h_a(X_i)
# where h_a(X_i) = dnorm(qnorm(phat_i)) * ga_coef * amount * pipeline_a(X_i)
#
# Pipeline definitions (from transition probability table, Section 1b):
#   STEM:      pipeline = stem_major   (55% enter STEM grad programs)
#   Business:  pipeline = bus_major    (65% enter Business grad programs)
#   Education: pipeline = ed_major     (70% enter Education grad programs)
#   Health:    pipeline = stem_major OR socsci_major (draws from both)
################################################################################

cat("\n==============================================\n")
cat("MPRTE BY GRADUATE PROGRAM AREA (Scenarios 5-8)\n")
cat("==============================================\n")

# Scenario 5: STEM graduate pipeline
p_new_s5   <- pnorm(df$z_index + ga_coef * 2.5 * df$stem_major)
delta_p_s5 <- p_new_s5 - df$phat
response_s5 <- dnorm(qnorm(df$phat)) * ga_coef * 2.5 * df$stem_major
mteA_wt_s5  <- df$mte_hat_stem * response_s5
mprte_ma_stem <- sum(mteA_wt_s5[df$stem_major == 1]) / sum(response_s5[df$stem_major == 1])
prte_ma_stem <- weighted.mean(
  df$mte_hat_stem[delta_p_s5 > 0 & df$stem_major == 1],
  delta_p_s5[delta_p_s5 > 0 & df$stem_major == 1]
)
cat(sprintf("MPRTE (STEM grad pipeline, $2.5k): %6.4f\n", mprte_ma_stem))
cat(sprintf("PRTE  (STEM grad pipeline, $2.5k): %6.4f\n", prte_ma_stem))
cat(sprintf("Mean phat for STEM undergrads:     %6.4f\n", mean(df$phat[df$stem_major == 1])))

# Scenario 6: Business graduate pipeline
p_new_s6   <- pnorm(df$z_index + ga_coef * 2.5 * df$bus_major)
delta_p_s6 <- p_new_s6 - df$phat
response_s6 <- dnorm(qnorm(df$phat)) * ga_coef * 2.5 * df$bus_major
mteA_wt_s6  <- df$mte_hat_business * response_s6
mprte_ma_bus <- sum(mteA_wt_s6[df$bus_major == 1]) / sum(response_s6[df$bus_major == 1])
prte_ma_bus <- weighted.mean(
  df$mte_hat_business[delta_p_s6 > 0 & df$bus_major == 1],
  delta_p_s6[delta_p_s6 > 0 & df$bus_major == 1]
)
cat(sprintf("MPRTE (Business grad pipeline, $2.5k): %6.4f\n", mprte_ma_bus))
cat(sprintf("PRTE  (Business grad pipeline, $2.5k): %6.4f\n", prte_ma_bus))

# Scenario 7: Education graduate pipeline
p_new_s7   <- pnorm(df$z_index + ga_coef * 2.5 * df$ed_major)
delta_p_s7 <- p_new_s7 - df$phat
response_s7 <- dnorm(qnorm(df$phat)) * ga_coef * 2.5 * df$ed_major
mteA_wt_s7  <- df$mte_hat_education * response_s7
mprte_ma_ed <- sum(mteA_wt_s7[df$ed_major == 1]) / sum(response_s7[df$ed_major == 1])
prte_ma_ed <- weighted.mean(
  df$mte_hat_education[delta_p_s7 > 0 & df$ed_major == 1],
  delta_p_s7[delta_p_s7 > 0 & df$ed_major == 1]
)
cat(sprintf("MPRTE (Education grad pipeline, $2.5k): %6.4f\n", mprte_ma_ed))
cat(sprintf("PRTE  (Education grad pipeline, $2.5k): %6.4f\n", prte_ma_ed))

# Scenario 8: Health & Related pipeline (STEM + SocSci undergrads)
target_health <- as.integer(df$stem_major == 1 | df$socsci_major == 1)
p_new_s8   <- pnorm(df$z_index + ga_coef * 2.5 * target_health)
delta_p_s8 <- p_new_s8 - df$phat
response_s8 <- dnorm(qnorm(df$phat)) * ga_coef * 2.5 * target_health
mteA_wt_s8  <- df$mte_hat_health * response_s8
mprte_ma_hlth <- sum(mteA_wt_s8[target_health == 1]) / sum(response_s8[target_health == 1])
prte_ma_hlth <- weighted.mean(
  df$mte_hat_health[delta_p_s8 > 0 & target_health == 1],
  delta_p_s8[delta_p_s8 > 0 & target_health == 1]
)
cat(sprintf("MPRTE (Health & Related pipeline, $2.5k): %6.4f\n", mprte_ma_hlth))
cat(sprintf("PRTE  (Health & Related pipeline, $2.5k): %6.4f\n", prte_ma_hlth))

################################################################################
# SECTION 11: MPRTE BY POLICY INTENSITY
################################################################################

p_baseline <- mean(df$phat)

intensity_df <- data.frame(ga_increase = (1:20) * 0.5)
intensity_df$p_margin <- p_baseline + intensity_df$ga_increase * 0.015
intensity_df$mprte_approx <- b0 + b1 * intensity_df$p_margin +
  b2 * intensity_df$p_margin^2 + b3 * intensity_df$p_margin^3
print(intensity_df)

fig11_8 <- ggplot(intensity_df, aes(x = ga_increase, y = mprte_approx)) +
  geom_line(linewidth = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "MPRTE by Policy Intensity",
    subtitle = "Marginal returns to GA funding expansion",
    x = "GA Funding Increase ($1000s)", y = "MPRTE"
  ) +
  theme_springer()

ggsave(file.path(graphs_dir, "fig11_8_mprte_by_intensity_R.png"),
       fig11_8, width = 12, height = 12 * 0.75, units = "in", dpi = 100)

################################################################################
# SECTION 12: COMPARING TREATMENT EFFECT PARAMETERS
################################################################################

cat("\n==============================================\n")
cat("COMPARISON OF TREATMENT EFFECT PARAMETERS\n")
cat("==============================================\n")

cat("Parameter     Manual(cubic)  BS SE(manual)\n")
cat("=======================================================================\n")
cat(sprintf("ATE           %6.4f (%6.4f)\n", ate_est_cubic, ate_se))
cat(sprintf("ATT           %6.4f (%6.4f)\n", att_est, att_se))
cat(sprintf("ATU           %6.4f (%6.4f)\n", atu_est, atu_se))
cat(sprintf("LATE (IV)     %6.4f (%6.4f)\n", iv_est, iv_se))
cat("NOTE: mtefe (local-IV) column omitted -- not replicated in R; see\n")
cat("      Stata output for that estimator's ATE/ATT/ATU/LATE values.\n")
cat("---------------------------------------------------------\n")
cat(sprintf("MPRTE (uniform)                   %6.4f\n", mprte_unif))
cat(sprintf("MPRTE (low-income)                %6.4f\n", mprte_lowinc))
cat(sprintf("MPRTE (STEM ug -> any grad)       %6.4f\n", mprte_stem))
cat(sprintf("MPRTE (Ed ug -> any grad)         %6.4f\n", mprte_ed))

cat("\nAREA-SPECIFIC PARAMETERS WITH 95% CONFIDENCE INTERVALS:\n")
cat("(Cluster bootstrap, G=50 states, R=500 reps)\n")
cat("ATE estimates:\n")
cat("Area          Estimate   (BS SE)    95% CI                  Sig\n")
cat("------------------------------------------------------------------\n")
for (a in c("other", "stem", "business", "education", "health")) {
  lo <- area_ate_vals[[a]] - 1.96 * area_ate_se[[a]]
  hi <- area_ate_vals[[a]] + 1.96 * area_ate_se[[a]]
  cat(sprintf("  %-9s %6.4f   (%6.4f)   [%7.4f, %7.4f]   %s\n",
              a, area_ate_vals[[a]], area_ate_se[[a]], lo, hi, sig_label(lo, hi)))
}
cat("\nATT estimates:\n")
cat("Area          Estimate   (BS SE)    95% CI                  Sig\n")
cat("------------------------------------------------------------------\n")
for (a in c("other", "stem", "business", "education", "health")) {
  lo <- area_att_vals[[a]] - 1.96 * area_att_se[[a]]
  hi <- area_att_vals[[a]] + 1.96 * area_att_se[[a]]
  cat(sprintf("  %-9s %6.4f   (%6.4f)   [%7.4f, %7.4f]   %s\n",
              a, area_att_vals[[a]], area_att_se[[a]], lo, hi, sig_label(lo, hi)))
}
cat("\nATU estimates (prospective program area assignment):\n")
cat("Area          Estimate   (BS SE)    95% CI                  Sig\n")
cat("------------------------------------------------------------------\n")
for (a in c("other", "stem", "business", "education", "health")) {
  lo <- area_atu_vals[[a]] - 1.96 * area_atu_se[[a]]
  hi <- area_atu_vals[[a]] + 1.96 * area_atu_se[[a]]
  cat(sprintf("  %-9s %6.4f   (%6.4f)   [%7.4f, %7.4f]   %s\n",
              a, area_atu_vals[[a]], area_atu_se[[a]], lo, hi, sig_label(lo, hi)))
}
cat("  *** = 95% CI excludes zero (p < 0.05, two-tailed)\n")

cat("\nMPRTE BY GRADUATE PIPELINE (Scenarios 5-8):\n")
cat(sprintf("  STEM grad pipeline:         %6.4f\n", mprte_ma_stem))
cat(sprintf("  Business grad pipeline:     %6.4f\n", mprte_ma_bus))
cat(sprintf("  Education grad pipeline:    %6.4f\n", mprte_ma_ed))
cat(sprintf("  Health & Related pipeline:  %6.4f\n", mprte_ma_hlth))

################################################################################
# SECTION 13: MPRTE VISUALIZATION
################################################################################

# Fig 11.6: MTE Curve with Policy-Relevant Regions
policy_grid <- data.frame(u = (1:100) / 100)
policy_grid$mte <- b0 + b1 * policy_grid$u + b2 * policy_grid$u^2 + b3 * policy_grid$u^3
policy_grid$region_lo <- policy_grid$u >= 0.10 & policy_grid$u <= 0.25
policy_grid$region_un <- policy_grid$u >= 0.25 & policy_grid$u <= 0.40

fig11_6 <- ggplot(policy_grid, aes(x = u)) +
  geom_ribbon(data = subset(policy_grid, region_lo),
              aes(ymin = 0, ymax = mte, fill = "Low-income margin")) +
  geom_ribbon(data = subset(policy_grid, region_un),
              aes(ymin = 0, ymax = mte, fill = "Uniform policy margin")) +
  geom_line(aes(y = mte, color = "Estimated MTE"), linewidth = 1) +
  scale_fill_manual(values = c("Low-income margin" = "grey50",
                                "Uniform policy margin" = "grey80"), name = NULL) +
  scale_color_manual(values = c("Estimated MTE" = "black"), name = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "MTE Curve with Policy-Relevant Regions",
    x = "u (Unobserved Resistance to Treatment)",
    y = "Marginal Treatment Effect"
  ) +
  theme_springer() +
  theme(legend.position = "bottom")

ggsave(file.path(graphs_dir, "fig11_6_mte_policy_regions_R.png"),
       fig11_6, width = 12, height = 12 * 0.75, units = "in", dpi = 100)

# Fig 11.4: MTE by propensity score (binned scatter + frequency bars)
df$p_bin <- floor(df$phat * 20) / 20

bin_df <- df %>%
  group_by(p_bin) %>%
  summarise(mean_mte = mean(mte_hat), n_bin = n(), .groups = "drop")

# Scale n_bin onto the same axis range as mean_mte for a single-panel
# rendering (ggplot2 has no native secondary-axis bar+line overlay as
# direct as Stata's yaxis(2); we rescale the bar heights into the MTE
# range and add a secondary axis with the inverse transform, matching
# the visual effect of Stata's `yaxis(2)` overlay).
mte_range <- range(bin_df$mean_mte)
n_range   <- range(bin_df$n_bin)
scale_factor <- diff(mte_range) / diff(n_range)
bin_df$n_bin_scaled <- mte_range[1] + (bin_df$n_bin - n_range[1]) * scale_factor

fig11_4 <- ggplot(bin_df, aes(x = p_bin)) +
  geom_col(aes(y = n_bin_scaled), width = 0.04, fill = "grey85", color = "grey60") +
  geom_line(aes(y = mean_mte), color = "black", linewidth = 0.8) +
  geom_point(aes(y = mean_mte), color = "black", shape = 18, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_y_continuous(
    name = "Estimated MTE",
    sec.axis = sec_axis(~ n_range[1] + (. - mte_range[1]) / scale_factor,
                         name = "Frequency")
  ) +
  labs(title = "MTE by Propensity Score", x = "Propensity Score") +
  theme_springer()

ggsave(file.path(graphs_dir, "fig11_4_mte_by_propensity_R.png"),
       fig11_4, width = 12, height = 12 * 0.75, units = "in", dpi = 100)

################################################################################
# SECTION 14: POLICY COST-BENEFIT ANALYSIS
################################################################################

cost_per_degree <- 50000
career_years    <- 30
discount_rate   <- 0.03
base_salary     <- 47000
pv_factor <- (1 - (1 + discount_rate)^(-career_years)) / discount_rate
cat(sprintf("Present value factor (30 years, 3%%): %6.2f\n", pv_factor))

cat("\n--- Scenarios 1-4: Original MPRTE-based CBA ---\n")
cat("Policy               MPRTE    Annual Gain   PV Gain    B/C Ratio\n")
cat("==================================================================\n")

cba_scen_1_4 <- list(
  Uniform     = mprte_unif,
  `Low-income`= mprte_lowinc,
  `STEM ug`   = mprte_stem,
  `Education ug` = mprte_ed
)
for (nm in names(cba_scen_1_4)) {
  mprte_val  <- cba_scen_1_4[[nm]]
  annual_gain <- base_salary * (exp(mprte_val) - 1)
  pv_gain     <- annual_gain * pv_factor
  bc_ratio    <- pv_gain / cost_per_degree
  cat(sprintf("%-20s %6.4f   $%6.0f    $%8.0f   %5.2f\n",
              nm, mprte_val, annual_gain, pv_gain, bc_ratio))
}

cat("\n--- Scenarios 5-8: Graduate Program Area MPRTE-based CBA ---\n")
base_stem <- 65000; base_bus <- 60000; base_ed <- 42000; base_hlth <- 68000
cat(sprintf("  Base salaries: STEM=$%6.0f Business=$%6.0f Ed=$%6.0f Health=$%6.0f\n",
            base_stem, base_bus, base_ed, base_hlth))

cat("\nPipeline            MPRTE    Annual Gain   PV Gain    B/C Ratio\n")
cat("==================================================================\n")

cba_scen_5_8 <- list(
  `STEM pipeline`     = list(mv = mprte_ma_stem, base = base_stem),
  `Business pipeline` = list(mv = mprte_ma_bus,  base = base_bus),
  `Education pipeline`= list(mv = mprte_ma_ed,   base = base_ed),
  `Health pipeline`   = list(mv = mprte_ma_hlth, base = base_hlth)
)
for (nm in names(cba_scen_5_8)) {
  mv  <- cba_scen_5_8[[nm]]$mv
  ag  <- cba_scen_5_8[[nm]]$base * (exp(mv) - 1)
  pvg <- ag * pv_factor
  cat(sprintf("%-19s %6.4f   $%6.0f    $%8.0f   %5.2f\n",
              nm, mv, ag, pvg, pvg / cost_per_degree))
}

cat("Note: B/C > 1 suggests policy expansion is beneficial (synthetic data only).\n")

################################################################################
# SECTION 15: Save Results
################################################################################

# Drop scratch columns used only for visualization binning (Section 8/13)
# that have no analytical meaning of their own and were never meant to be
# part of the saved analysis dataset.
df$p_decile <- NULL
df$p_bin    <- NULL

# Defensive check: Stata's .dta format does not allow variable names
# starting with a period (or several other special characters). If any
# such column was added to df and not cleaned up, write_dta() below will
# fail. Catch that here with a clear message instead of a cryptic error.
bad_names <- grep("^\\.", names(df), value = TRUE)
if (length(bad_names) > 0) {
  stop(paste0(
    "write_dta() would fail: df contains column name(s) starting with '.': ",
    paste(bad_names, collapse = ", "),
    ". Remove or rename these columns before saving."
  ))
}

attr(df$phat, "label")    <- "Estimated propensity score"
attr(df$mte_hat, "label") <- "Estimated MTE (pooled cubic)"
attr(df$z_index, "label") <- "Probit linear index"

write_dta(df, "bb_mte_analysis.dta")

summary_by_field <- df %>%
  group_by(stem_major, ed_major) %>%
  summarise(
    masters = mean(masters), ln_salary = mean(ln_salary),
    phat = mean(phat), mte_hat = mean(mte_hat),
    ma_stem = mean(ma_stem), ma_business = mean(ma_business),
    ma_education = mean(ma_education), ma_health = mean(ma_health),
    ma_other = mean(ma_other),
    mte_hat_stem = mean(mte_hat_stem), mte_hat_business = mean(mte_hat_business),
    mte_hat_education = mean(mte_hat_education), mte_hat_health = mean(mte_hat_health),
    mte_hat_other = mean(mte_hat_other),
    sd_mte = sd(mte_hat), n = n(),
    .groups = "drop"
  )
write.csv(summary_by_field, "mte_summary_by_field.csv", row.names = FALSE)

summary_by_program_area <- df %>%
  filter(masters == 1) %>%
  group_by(ma_stem, ma_business, ma_education, ma_health, ma_other) %>%
  summarise(
    ln_salary = mean(ln_salary), salary = mean(salary),
    phat = mean(phat), mte_hat = mean(mte_hat),
    mte_hat_stem = mean(mte_hat_stem), mte_hat_business = mean(mte_hat_business),
    mte_hat_education = mean(mte_hat_education), mte_hat_health = mean(mte_hat_health),
    mte_hat_other = mean(mte_hat_other),
    n = n(),
    .groups = "drop"
  )
write.csv(summary_by_program_area, "mte_summary_by_program_area.csv", row.names = FALSE)

################################################################################
# SECTION 16: FINAL SUMMARY
################################################################################

cat("\n==============================================\n")
cat("ANALYSIS COMPLETE\n")
cat("==============================================\n")
cat(sprintf("  1.  Treatment rate:                  %5.3f\n", treat_rate))
cat(sprintf("  2.  OLS estimate (biased):           %6.4f\n", ols_est))
cat(sprintf("  3.  IV/LATE estimate:                %6.4f\n", iv_est))
cat(sprintf("  4.  MTE-based ATE (cubic):           %6.4f (BS SE = %6.4f)\n", ate_est_cubic, ate_se))
cat(sprintf("  5.  MTE-based ATT:                   %6.4f (BS SE = %6.4f)\n", att_est, att_se))
cat(sprintf("  6.  MTE-based ATU:                   %6.4f (BS SE = %6.4f)\n", atu_est, atu_se))
cat(sprintf("  7.  First-stage F:                   %6.1f\n", first_stage_F))
cat("      (mtefe local-IV ATE/ATT/ATU not replicated in R; see Stata output)\n")

cat("\nAREA-SPECIFIC ATE, ATT, AND ATU WITH 95% CONFIDENCE INTERVALS\n")
cat("(Cluster bootstrap, G=50 states, R=500 reps, seed 20260101)\n")
cat("ATU based on prospective program area assignment (seed 20260102)\n")
cat("==================================================================\n")
cat("ATE estimates:\n")
cat("Area          Estimate   (BS SE)    95% CI                  Sig\n")
cat("------------------------------------------------------------------\n")
for (a in c("other", "stem", "business", "education", "health")) {
  lo <- area_ate_vals[[a]] - 1.96 * area_ate_se[[a]]
  hi <- area_ate_vals[[a]] + 1.96 * area_ate_se[[a]]
  cat(sprintf("  %-9s %6.4f   (%6.4f)   [%7.4f, %7.4f]   %s\n",
              a, area_ate_vals[[a]], area_ate_se[[a]], lo, hi, sig_label(lo, hi)))
}
cat("\nATT estimates:\n")
cat("Area          Estimate   (BS SE)    95% CI                  Sig\n")
cat("------------------------------------------------------------------\n")
for (a in c("other", "stem", "business", "education", "health")) {
  lo <- area_att_vals[[a]] - 1.96 * area_att_se[[a]]
  hi <- area_att_vals[[a]] + 1.96 * area_att_se[[a]]
  cat(sprintf("  %-9s %6.4f   (%6.4f)   [%7.4f, %7.4f]   %s\n",
              a, area_att_vals[[a]], area_att_se[[a]], lo, hi, sig_label(lo, hi)))
}
cat("\nATU estimates (prospective program area assignment):\n")
cat("Area          Estimate   (BS SE)    95% CI                  Sig\n")
cat("------------------------------------------------------------------\n")
for (a in c("other", "stem", "business", "education", "health")) {
  lo <- area_atu_vals[[a]] - 1.96 * area_atu_se[[a]]
  hi <- area_atu_vals[[a]] + 1.96 * area_atu_se[[a]]
  cat(sprintf("  %-9s %6.4f   (%6.4f)   [%7.4f, %7.4f]   %s\n",
              a, area_atu_vals[[a]], area_atu_se[[a]], lo, hi, sig_label(lo, hi)))
}
cat("  *** = 95% CI excludes zero (p < 0.05, two-tailed)\n")
cat("  Note: Business ATE CI is wide -- interpret point estimate with caution.\n")

cat("\nMPRTE SUMMARY - Original Scenarios:\n")
cat(sprintf("  Uniform policy:         %6.4f\n", mprte_unif))
cat(sprintf("  Low-income targeted:    %6.4f\n", mprte_lowinc))
cat(sprintf("  STEM ug pipeline:       %6.4f\n", mprte_stem))
cat(sprintf("  Education ug pipeline:  %6.4f\n", mprte_ed))

cat("\nMPRTE SUMMARY - Graduate Program Area Pipelines (Scenarios 5-8):\n")
cat(sprintf("  STEM grad pipeline:     %6.4f\n", mprte_ma_stem))
cat(sprintf("  Business grad pipeline: %6.4f\n", mprte_ma_bus))
cat(sprintf("  Education pipeline:     %6.4f\n", mprte_ma_ed))
cat(sprintf("  Health & Related:       %6.4f\n", mprte_ma_hlth))

cat("\nBootstrap: G=50 state clusters, R=500 reps, seed(20260101)\n")
cat("Files saved: bb_mte_analysis.dta, mte_summary_by_field.csv,\n")
cat("             mte_summary_by_program_area.csv\n")

cat("\nIMPORTANT NOTE: Synthetic data -- results illustrate methods only.\n")
cat("==============================================\n")
cat("END OF MTE/MPRTE ANALYSIS\n")
cat("==============================================\n")

print(fig11_3); print(fig11_4); print(fig11_5)
print(fig11_6); print(fig11_7); print(fig11_8)

#-------------------------------------------------------------------------
# Close this sub-script's dedicated log
# (The driver script's own master log, if any, remains open.)
#-------------------------------------------------------------------------
cat("\nMTE_MPRTE.R log closed:", format(Sys.time()), "\n")
sink(type = "message")
sink()
close(mte_log_con)

#=========================================================================
# END OF MTE_MPRTE_R.R
#=========================================================================
