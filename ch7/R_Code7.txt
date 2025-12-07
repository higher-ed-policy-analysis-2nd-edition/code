#================================================================
# Chapter 7 - Introduction to Intermediate Statistical Techniques
# Complete R Code
# Higher Education Policy Analysis Using Quantitative Techniques 
# (2nd Edition)
#================================================================
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch7
# README.md - Detailed instructions and documentation
#
# Important Note: Before running any code, you must:
#   1. Download data files from the data repository
#   2. Save them to your local working directory
#   3. Change all file paths in the code to match your directory structure
#
# Author: Marvin A. Titus
# Date: December 2025
#================================================================

# Script tested in R 4.4.0
# Compatible with R version 4.0 or later

#========================================================================
# REQUIRED PACKAGES
#========================================================================

# Install packages if not already installed
required_packages <- c(
  "haven",       # Reading Stata .dta files
  "dplyr",       # Data manipulation
  "plm",         # Panel data models (fixed/random effects)
  "lmtest",      # Diagnostic tests (coeftest, bptest)
  "sandwich",    # Robust standard errors (vcovHC, vcovCL)
  "car",         # linearHypothesis for joint tests
  "AER",         # IV/2SLS estimation (ivreg)
  "margins",     # Marginal effects
  "ggplot2",     # Visualization
  "fixest",      # Fast fixed effects (alternative to plm)
  "lme4"         # Mixed effects models (alternative for RE)
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

invisible(lapply(required_packages, install_if_missing))

# Load packages
library(haven)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(car)
library(AER)
library(margins)
library(ggplot2)
library(fixest)
library(lme4)

#========================================================================
# IMPORTANT: Set working directory (customize this for your system)
#========================================================================

# setwd("C:/Users/YourName/Documents/book-materials/ch7/data")

#========================================================================
#========================================================================
#
#                    SECTION 7.2: REVIEW OF OLS REGRESSION
#
#========================================================================
#========================================================================

#========================================================================
# Section 7.2.2: Bivariate and Multivariate OLS Regression
#========================================================================

# Download state-level panel dataset (50 states × 27 years, 1990-2016)
url_7_2_2 <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_2_2.dta"
download.file(url_7_2_2, "Example_7_2_2.dta", mode = "wb")

data_7_2_2 <- read_dta("Example_7_2_2.dta")

# Create per FTE variables (dividing aggregate amounts by enrollment)
data_7_2_2 <- data_7_2_2 %>%
  mutate(
    netuit_fte = netuit / fte,
    stapr_fte = stapr / fte
  )

#------------------------------------------------------------------------
# Bivariate OLS Regression
#------------------------------------------------------------------------

# Bivariate regression for single year (2016)
# Tests relationship between state appropriations and net tuition per FTE
data_2016 <- filter(data_7_2_2, year == 2016)
model_bivariate <- lm(netuit_fte ~ stapr_fte, data = data_2016)
summary(model_bivariate)

# Expected results: Negative coefficient (~-0.35), R² ≈ 0.13, F ≈ 7.19

#------------------------------------------------------------------------
# Multivariate OLS Regression
#------------------------------------------------------------------------

# Create squared term to test for non-linear (quadratic) relationship
data_7_2_2 <- data_7_2_2 %>%
  mutate(stapr_fte2 = stapr_fte * stapr_fte)

data_2016 <- filter(data_7_2_2, year == 2016)

# Add polynomial term and additional control variable (per capita income)
model_multivariate <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income, 
                         data = data_2016)
summary(model_multivariate)

# Expected results: R² increases to ~0.28 with additional variables

#========================================================================
# Section 7.2.3: Pooled OLS Regression
#========================================================================

# Pooled OLS uses all years of data (1990-2016) not just 2016
# This increases N from 50 to 1,350 observations
model_pooled1 <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income, 
                    data = data_7_2_2)
summary(model_pooled1)

# Add categorical control for regional compact membership
# factor() creates dummy variables for each category
model_pooled2 <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income + 
                      factor(region_compact), 
                    data = data_7_2_2)
summary(model_pooled2)

#------------------------------------------------------------------------
# Interaction Terms in Pooled OLS
#------------------------------------------------------------------------

# EXAMPLE 1: Categorical × Categorical Interaction
# * operator creates all combinations of region_compact and ugradmerit
# Tests whether merit aid effects differ by region
model_interact1 <- lm(netuit_fte ~ stapr_fte + 
                        factor(region_compact) * factor(ugradmerit), 
                      data = data_7_2_2)
summary(model_interact1)

# Store models to test whether adding interactions improves fit
model1 <- lm(netuit_fte ~ stapr_fte + factor(region_compact), 
             data = data_7_2_2)
model2 <- lm(netuit_fte ~ stapr_fte + factor(region_compact) * factor(ugradmerit), 
             data = data_7_2_2)

# Likelihood ratio test: Are interaction terms jointly significant?
# (Using F-test for nested models)
anova(model1, model2)

# Alternative: Wald test of interaction terms only
linearHypothesis(model2, matchCoefs(model2, ":"))

# EXAMPLE 2: Categorical × Continuous Interaction
# Tests whether appropriations effect varies by tuition-setting authority
model_interact2 <- lm(netuit_fte ~ factor(ugradmerit) + factor(region_compact) + 
                        stapr_fte * factor(tuitset), 
                      data = data_7_2_2)
summary(model_interact2)

# Test interaction terms
linearHypothesis(model_interact2, matchCoefs(model_interact2, "stapr_fte:"))

# EXAMPLE 3: Continuous × Continuous Interaction
# Tests whether appropriations effect varies by level of need-based aid
model_interact3 <- lm(netuit_fte ~ factor(region_compact) + 
                        stapr_fte * state_needFTE, 
                      data = data_7_2_2)
summary(model_interact3)

# Calculate marginal effect of appropriations at different aid levels
# at() specifies values: 0, 3000, 6000, 9000, 10000
cat("\n--- Marginal Effects of stapr_fte at Different state_needFTE Levels ---\n")
margins_result <- margins(model_interact3, 
                          variables = "stapr_fte",
                          at = list(state_needFTE = seq(0, 10000, by = 3000)))
summary(margins_result)

# Create visualization showing how relationship changes
# Generate predictions for plotting
pred_data <- expand.grid(
  stapr_fte = c(0, 10000),
  state_needFTE = seq(0, 10000, by = 3000),
  region_compact = levels(factor(data_7_2_2$region_compact))[1]
)
pred_data$predicted <- predict(model_interact3, newdata = pred_data)

ggplot(pred_data, aes(x = stapr_fte, y = predicted, 
                      color = factor(state_needFTE), 
                      group = state_needFTE)) +
  geom_line() +
  labs(title = "Predicted Net Tuition by State Appropriations and Need-Based Aid",
       x = "State Appropriations per FTE",
       y = "Predicted Net Tuition per FTE",
       color = "Need-Based Aid\nper FTE") +
  theme_minimal()

#------------------------------------------------------------------------
# Testing Regression Assumptions
#------------------------------------------------------------------------

# Fit model for diagnostics
model_diag <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income + 
                   factor(region_compact), 
                 data = data_7_2_2)

# Create residual-versus-fitted plot to check for heteroscedasticity
# Funnel shape indicates violation of constant variance assumption
plot(fitted(model_diag), residuals(model_diag),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)

# Breusch-Pagan test for heteroscedasticity
bptest(model_diag)

# Robust standard errors correct for heteroscedasticity
# Results are valid even if constant variance assumption is violated
cat("\n--- OLS with Robust (HC1) Standard Errors ---\n")
coeftest(model_diag, vcov = vcovHC(model_diag, type = "HC1"))

# Test for heteroscedasticity across states using Levene's test
data_7_2_2$residuals <- residuals(lm(netuit_fte ~ stapr_fte + stapr_fte2 + 
                                       pc_income + factor(region_compact), 
                                     data = data_7_2_2))
leveneTest(residuals ~ factor(state), data = data_7_2_2)

# Cluster-robust standard errors account for within-state correlation
# Use when observations within same state are not independent
cat("\n--- OLS with Cluster-Robust Standard Errors (by state) ---\n")
coeftest(model_diag, vcov = vcovCL(model_diag, cluster = data_7_2_2$state))

#========================================================================
#========================================================================
#
#                 SECTION 7.3: FIXED-EFFECTS REGRESSION
#
#========================================================================
#========================================================================

#========================================================================
# Section 7.3.1: Fixed-Effects Dummy Variable (FEDV) Estimation
#========================================================================

# Continue using state-level panel data (Example_7_2_2.dta) for state FE models

# Fixed-effects with state dummy variables (FEDV approach)
# factor(stateid) creates dummy for each state, controlling for state differences
model_fedv <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income + factor(stateid), 
                 data = data_7_2_2)

# Display results with cluster-robust standard errors
cat("\n--- FEDV Model with Cluster-Robust Standard Errors ---\n")
coeftest(model_fedv, vcov = vcovCL(model_fedv, cluster = data_7_2_2$state))

# Test whether state dummies are jointly significant
# If significant, state fixed effects are needed
# Use nested model comparison (F-test)
model_no_fe <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income, 
                  data = data_7_2_2)

cat("\n--- Joint Test of State Fixed Effects ---\n")
anova(model_no_fe, model_fedv)

# Alternative using fixest package (more efficient, absorbs fixed effects)
# Produces same coefficients but doesn't display all dummy variables
model_feols <- feols(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income | stateid, 
                     data = data_7_2_2,
                     cluster = ~stateid)
summary(model_feols)

# Now switch to institutional-level panel dataset for institution FE models
# Different dataset: institutions observed over multiple years
url_7_3_1 <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_3_1.dta"
download.file(url_7_3_1, "Example_7_3_1.dta", mode = "wb")

data_7_3_1 <- read_dta("Example_7_3_1.dta")

# Institutional-level fixed effects example
# Controls for time-invariant institution characteristics
# eg = education & general expenditures (dependent variable)
model_inst_fe <- feols(eg ~ statea + tuition + totfteiarep + ftfac + ptfac + D | opeid5_new, 
                       data = data_7_3_1,
                       cluster = ~opeid5_new)
summary(model_inst_fe)

#========================================================================
# Section 7.3.2: Within-Group Estimator
#========================================================================

# Identify time variable in dataset (may be 'year', 'fyear', 'acadyr', etc.)
# Check available variables
cat("\n--- Variables in institutional dataset ---\n")
print(names(data_7_3_1))

# Create a time index if not present (using row number within each institution)
data_7_3_1 <- data_7_3_1 %>%
  group_by(opeid5_new) %>%
  mutate(time_index = row_number()) %>%
  ungroup()

# Declare panel data structure for plm
pdata_inst <- pdata.frame(data_7_3_1, index = c("opeid5_new", "time_index"))

# plm with model="within" uses within transformation
# Automatically removes time-invariant characteristics
# More information provided than feols (within/between R²)
model_within <- plm(eg ~ statea + tuition + totfteiarep + ftfac + ptfac, 
                    data = pdata_inst,
                    model = "within")

# Display results with cluster-robust standard errors
cat("\n--- Within-Group Estimator with Cluster-Robust SE ---\n")
coeftest(model_within, vcov = vcovHC(model_within, type = "HC1", cluster = "group"))

# Display additional statistics
cat("\nWithin R-squared:", summary(model_within)$r.squared["rsq"], "\n")

#========================================================================
#========================================================================
#
#                SECTION 7.4: RANDOM-EFFECTS REGRESSION
#
#========================================================================
#========================================================================

# Return to state-level panel data
data_7_2_2 <- read_dta("Example_7_2_2.dta")

# Recreate per FTE variables
data_7_2_2 <- data_7_2_2 %>%
  mutate(
    netuit_fte = netuit / fte,
    stapr_fte = stapr / fte,
    stapr_fte2 = stapr_fte * stapr_fte
  )

# Random-effects assumes unit effects uncorrelated with predictors
# GLS estimation is more efficient than FE if assumption holds

# Method 1: Using plm (standard output, no cluster-robust SE)
pdata_state <- pdata.frame(data_7_2_2, index = c("stateid", "year"))

model_re <- plm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income, 
                data = pdata_state,
                model = "random")

cat("\n--- Random Effects Model (plm) ---\n")
print(summary(model_re))

# Method 2: Using lme4 for mixed model with cluster-robust inference
# This is more numerically stable for random effects
library(lme4)

model_re_lmer <- lmer(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income + 
                        (1 | stateid), 
                      data = data_7_2_2)

cat("\n--- Random Effects Model (lme4) ---\n")
print(summary(model_re_lmer))

# Breusch-Pagan Lagrange Multiplier test: Is random effects better than pooled OLS?
# Null hypothesis: variance of random effects = 0
# If rejected, use random effects instead of pooled OLS
cat("\n--- Breusch-Pagan LM Test for Random Effects ---\n")
plmtest(model_re, type = "bp")

#========================================================================
# Section 7.4.1: The Hausman Test
#========================================================================

# Hausman test: Should we use fixed or random effects?
# Null hypothesis: Random effects estimates are consistent
# If rejected, use fixed effects

# For institutional-level data
data_7_3_1 <- read_dta("Example_7_3_1.dta")

# Create time index within each institution
data_7_3_1 <- data_7_3_1 %>%
  group_by(opeid5_new) %>%
  mutate(time_index = row_number()) %>%
  ungroup()

# Log-transformed variables work better for Hausman test
# Reduces influence of outliers and improves test properties
data_7_3_1 <- data_7_3_1 %>%
  mutate(
    lneg = log(eg),
    lnstatea = log(statea),
    lntuition = log(tuition),
    lntotfteiarep = log(totfteiarep),
    lnftfac = log(ftfac),
    lnptfac = log(ptfac)
  )

#------------------------------------------------------------------------
# Method 1: Standard Hausman Test using plm (if numerical stability allows)
#------------------------------------------------------------------------

pdata_inst <- pdata.frame(data_7_3_1, index = c("opeid5_new", "time_index"))

# Fixed effects model
model_fe_ln <- plm(lneg ~ lnstatea + lntuition + lntotfteiarep + lnftfac + ptfac, 
                   data = pdata_inst,
                   model = "within")

cat("\n--- Fixed Effects Model (Log Variables) ---\n")
print(summary(model_fe_ln))

# Try Hausman test with tryCatch to handle potential errors
cat("\n--- Hausman Test ---\n")
tryCatch({
  model_re_ln <- plm(lneg ~ lnstatea + lntuition + lntotfteiarep + lnftfac + ptfac, 
                     data = pdata_inst,
                     model = "random")
  print(phtest(model_fe_ln, model_re_ln))
}, error = function(e) {
  cat("Note: Standard Hausman test encountered numerical issues.\n")
  cat("Using Mundlak approach instead (see below).\n\n")
})

#------------------------------------------------------------------------
# Method 2: Mundlak Approach (Cluster-Robust Hausman Test)
#------------------------------------------------------------------------

# The Mundlak approach adds group means of time-varying variables to RE model
# If group means are jointly significant, use fixed effects

cat("\n--- Mundlak Approach (Cluster-Robust Hausman Test) ---\n")

# Calculate group means
data_7_3_1 <- data_7_3_1 %>%
  group_by(opeid5_new) %>%
  mutate(
    mean_lnstatea = mean(lnstatea, na.rm = TRUE),
    mean_lntuition = mean(lntuition, na.rm = TRUE),
    mean_lntotfteiarep = mean(lntotfteiarep, na.rm = TRUE),
    mean_lnftfac = mean(lnftfac, na.rm = TRUE),
    mean_ptfac = mean(ptfac, na.rm = TRUE)
  ) %>%
  ungroup()

# Mundlak RE model using lme4 (more stable)
model_mundlak <- lmer(lneg ~ lnstatea + lntuition + lntotfteiarep + lnftfac + ptfac +
                        mean_lnstatea + mean_lntuition + mean_lntotfteiarep + 
                        mean_lnftfac + mean_ptfac + (1 | opeid5_new), 
                      data = data_7_3_1)

cat("\n--- Mundlak Model Summary ---\n")
print(summary(model_mundlak))

# Joint test of group mean coefficients using likelihood ratio test
# If significant, reject RE in favor of FE
model_re_simple <- lmer(lneg ~ lnstatea + lntuition + lntotfteiarep + lnftfac + ptfac +
                          (1 | opeid5_new), 
                        data = data_7_3_1)

cat("\n--- Likelihood Ratio Test of Group Means (Mundlak Test) ---\n")
cat("H0: Random effects is consistent (group means coefficients = 0)\n")
cat("Ha: Fixed effects is preferred (group means coefficients ≠ 0)\n\n")
print(anova(model_re_simple, model_mundlak))

cat("\nInterpretation: If p-value < 0.05, reject H0 and use fixed effects.\n")

#========================================================================
#========================================================================
#
#     SECTION 7.5: INSTRUMENTAL VARIABLES AND TWO-STAGE LEAST SQUARES
#
#========================================================================
#========================================================================

rm(list = ls())  # Clear workspace

#========================================================================
# Section 7.5.3: Application - Master's Degree Completion and Salary
#========================================================================

# This section demonstrates IV/2SLS estimation using the relationship
# between master's degree completion and salary outcomes.
#
# Endogenous Variable: Master's degree completion (masters)
# Instrument: State Graduate Assistantship (GA) Funding (ga_funding_adj)
# Outcome: Log salary (ln_salary)
#
# Note: This uses a synthetic dataset for pedagogical purposes.
# Results should not be interpreted as having policy implications.

#------------------------------------------------------------------------
# Load Data
#------------------------------------------------------------------------

# Download synthetic dataset for IV/2SLS demonstration
url_7_5_3 <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_5_3.dta"
download.file(url_7_5_3, "Example_7_5_3.dta", mode = "wb")

data_iv <- read_dta("Example_7_5_3.dta")

# Alternative: Load CSV version
# url_csv <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_5_3.csv"
# data_iv <- read.csv(url_csv)

#------------------------------------------------------------------------
# Summary Statistics
#------------------------------------------------------------------------

cat("\n--- Sample Characteristics ---\n")
table(data_iv$masters)
prop.table(table(data_iv$masters))

cat("\n--- Key Variables ---\n")
summary(data_iv[, c("ln_salary", "masters", "ga_funding_adj", "p_masters")])

cat("\n--- Salary by Master's Degree Status ---\n")
data_iv %>%
  group_by(masters) %>%
  summarise(
    mean_salary = mean(salary, na.rm = TRUE),
    sd_salary = sd(salary, na.rm = TRUE),
    mean_ln_salary = mean(ln_salary, na.rm = TRUE),
    sd_ln_salary = sd(ln_salary, na.rm = TRUE),
    n = n()
  ) %>%
  print()

#------------------------------------------------------------------------
# OLS Estimation (Potentially Biased)
#------------------------------------------------------------------------

cat("\n==============================================\n")
cat("OLS ESTIMATION\n")
cat("(Potentially biased due to endogeneity)\n")
cat("==============================================\n")

# Define control variables
X_controls <- c("female", "black", "hispanic", "asian", "age_ba", "firstgen",
                "parent_income_q", "parent_grad", "ugpa", "stem_major", 
                "bus_major", "ed_major", "selective_inst", "public_ug", 
                "state_unemp", "metro")

# Create formula
ols_formula <- as.formula(paste("ln_salary ~ masters +", 
                                 paste(X_controls, collapse = " + ")))

# OLS regression
model_ols <- lm(ols_formula, data = data_iv)

# Display with robust standard errors
cat("\n--- OLS Results with Robust Standard Errors ---\n")
ols_robust <- coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC1"))
print(ols_robust)

ols_est <- coef(model_ols)["masters"]
ols_se <- sqrt(vcovHC(model_ols, type = "HC1")["masters", "masters"])

cat(sprintf("\nOLS Estimate: %.4f (SE = %.4f)\n", ols_est, ols_se))

#------------------------------------------------------------------------
# First-Stage Regression
#------------------------------------------------------------------------

cat("\n==============================================\n")
cat("FIRST-STAGE REGRESSION\n")
cat("(Testing Instrument Relevance)\n")
cat("==============================================\n")

# First stage: Regress endogenous variable on instrument and controls
fs_formula <- as.formula(paste("masters ~ ga_funding_adj +", 
                                paste(X_controls, collapse = " + ")))
model_first_stage <- lm(fs_formula, data = data_iv)

# Display with robust standard errors
cat("\n--- First-Stage Results with Robust Standard Errors ---\n")
fs_robust <- coeftest(model_first_stage, vcov = vcovHC(model_first_stage, type = "HC1"))
print(fs_robust)

# Store first-stage results
fs_coef <- coef(model_first_stage)["ga_funding_adj"]
fs_se <- sqrt(vcovHC(model_first_stage, type = "HC1")["ga_funding_adj", "ga_funding_adj"])
fs_t <- fs_coef / fs_se
fs_F <- fs_t^2

cat("\nFirst-Stage Results:\n")
cat(sprintf("  GA Funding coefficient: %7.4f\n", fs_coef))
cat(sprintf("  Standard error:         %7.4f\n", fs_se))
cat(sprintf("  t-statistic:            %7.2f\n", fs_t))
cat(sprintf("  Partial F-statistic:    %7.1f\n", fs_F))
cat("\n  Stock-Yogo threshold:   F > 10\n")

if (fs_F > 10) {
  cat(sprintf("  RESULT: Strong instrument (F = %.1f > 10)\n", fs_F))
} else {
  cat(sprintf("  WARNING: Potentially weak instrument (F = %.1f)\n", fs_F))
}

#------------------------------------------------------------------------
# IV/2SLS Estimation
#------------------------------------------------------------------------

cat("\n==============================================\n")
cat("IV/2SLS ESTIMATION\n")
cat("==============================================\n")

# IV/2SLS using ivreg command from AER package
# Syntax: outcome ~ exogenous | instruments for endogenous
iv_formula <- as.formula(paste("ln_salary ~", 
                                paste(X_controls, collapse = " + "),
                                "+ masters | ",
                                paste(X_controls, collapse = " + "),
                                "+ ga_funding_adj"))

model_iv <- ivreg(iv_formula, data = data_iv)

# Display with robust standard errors
cat("\n--- IV/2SLS Results with Robust Standard Errors ---\n")
iv_robust <- coeftest(model_iv, vcov = vcovHC(model_iv, type = "HC1"))
print(iv_robust)

iv_est <- coef(model_iv)["masters"]
iv_se <- sqrt(vcovHC(model_iv, type = "HC1")["masters", "masters"])

cat("\nIV/2SLS Results:\n")
cat(sprintf("  Coefficient on masters: %.4f\n", iv_est))
cat(sprintf("  Standard error:         %.4f\n", iv_se))
cat(sprintf("  95%% CI: [%.4f, %.4f]\n", iv_est - 1.96*iv_se, iv_est + 1.96*iv_se))

#========================================================================
# Section 7.5.4: Assessing Instrument Validity
#========================================================================

cat("\n==============================================\n")
cat("ASSESSING INSTRUMENT VALIDITY\n")
cat("==============================================\n")

#------------------------------------------------------------------------
# First-Stage F-Statistic
#------------------------------------------------------------------------

cat("\n--- First-Stage Diagnostics ---\n")
cat("Tests whether instrument is sufficiently strong\n")

# Summary of IV model includes first-stage diagnostics
summary_iv <- summary(model_iv, diagnostics = TRUE)
print(summary_iv$diagnostics)

# The first-stage F-statistic should exceed the Stock-Yogo threshold of 10
# to avoid weak instrument bias

#------------------------------------------------------------------------
# Endogeneity Test (Durbin-Wu-Hausman)
#------------------------------------------------------------------------

cat("\n--- Endogeneity Test (Durbin-Wu-Hausman) ---\n")
cat("H0: Variable is exogenous (OLS is consistent)\n")
cat("Ha: Variable is endogenous (IV is needed)\n\n")

# Manual Wu-Hausman test using first-stage residuals
# Add first-stage residuals to OLS model and test significance
data_iv$fs_residuals <- residuals(model_first_stage)

hausman_formula <- as.formula(paste("ln_salary ~ masters + fs_residuals +", 
                                     paste(X_controls, collapse = " + ")))
model_hausman <- lm(hausman_formula, data = data_iv)

# Test coefficient on first-stage residuals
hausman_test <- coeftest(model_hausman, vcov = vcovHC(model_hausman, type = "HC1"))
cat("Wu-Hausman Test (coefficient on first-stage residuals):\n")
print(hausman_test["fs_residuals", ])

# If we reject the null hypothesis (p < 0.05), this confirms that
# the variable is endogenous and IV estimation is warranted

#------------------------------------------------------------------------
# Comparison of Estimates
#------------------------------------------------------------------------

cat("\n==============================================\n")
cat("COMPARISON OF ESTIMATES\n")
cat("==============================================\n")

# Create comparison table
cat("\nOLS vs. IV/2SLS: Effect of Master's Degree on Log Salary\n")
cat("--------------------------------------------------------\n")
cat(sprintf("                    OLS          IV/2SLS\n"))
cat(sprintf("Coefficient      %9.4f     %9.4f\n", ols_est, iv_est))
cat(sprintf("Std. Error       %9.4f     %9.4f\n", ols_se, iv_se))
cat(sprintf("N                %9d     %9d\n", nobs(model_ols), nobs(model_iv)))
cat(sprintf("R-squared        %9.4f     %9.4f\n", 
            summary(model_ols)$r.squared, 
            summary(model_iv)$r.squared))

cat("\nSummary:\n")
cat(sprintf("  OLS estimate:  %7.4f (SE = %.4f)\n", ols_est, ols_se))
cat(sprintf("  IV estimate:   %7.4f (SE = %.4f)\n", iv_est, iv_se))

diff <- ols_est - iv_est
cat(sprintf("  Difference:    %7.4f\n", diff))

cat("\nInterpretation:\n")
cat("  The OLS estimate exceeds the IV estimate, indicating upward bias\n")
cat("  due to positive selection on unobservables. Students who complete\n")
cat("  master's degrees have higher unobserved ability and motivation,\n")
cat("  which independently increases salary.\n")

#------------------------------------------------------------------------
# Manual 2SLS (Pedagogical Demonstration)
#------------------------------------------------------------------------

cat("\n==============================================\n")
cat("MANUAL 2SLS (for understanding)\n")
cat("==============================================\n")

cat("NOTE: This manual approach is for pedagogical purposes only.\n")
cat("      Standard errors are INCORRECT with manual 2SLS.\n")
cat("      Always use ivreg for proper inference.\n\n")

# Stage 1: Predict treatment using instrument
masters_hat <- fitted(model_first_stage)

# Stage 2: Regress outcome on predicted treatment
data_iv$masters_hat <- masters_hat
manual_formula <- as.formula(paste("ln_salary ~ masters_hat +", 
                                    paste(X_controls, collapse = " + ")))
model_manual_2sls <- lm(manual_formula, data = data_iv)

manual_iv <- coef(model_manual_2sls)["masters_hat"]
cat(sprintf("Manual 2SLS estimate: %.4f\n", manual_iv))
cat(sprintf("ivreg estimate:       %.4f\n", iv_est))
cat("(Coefficients should be identical)\n")

# Clean up
data_iv$masters_hat <- NULL
data_iv$fs_residuals <- NULL

#================================================================
# END OF CHAPTER 7 CODE
#================================================================

cat("\n==============================================\n")
cat("Chapter 7 R Code Complete\n")
cat("==============================================\n")
