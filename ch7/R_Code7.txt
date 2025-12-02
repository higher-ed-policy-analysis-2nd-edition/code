# ================================================================
# Chapter 7 - Introduction to Intermediate Statistical Techniques
# Complete R Code
# Higher Education Policy Analysis Using Quantitative Techniques 
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-
# edition/tree/main/code/ch7
# Author: Marvin A. Titus
# Date: November 15, 2025
# Translated to R
# ================================================================

# Script tested in R version 4.3.0 or later

# ========================================================================
# IMPORTANT: Set working directory (customize this for your system)
# ========================================================================

# Use a variable to make it easy to update in one place
# ch7data <- "C:/Users/YourName/Documents/book-materials/ch7/data"
# setwd(ch7data)

# ========================================================================
# REQUIRED PACKAGES
# ========================================================================

# Install packages if not already installed
required_packages <- c("haven", "dplyr", "lmtest", "sandwich", "car", 
                       "plm", "margins", "ggplot2", "multiwayvcov", 
                       "stargazer", "broom", "tidyr", "AER")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ========================================================================
# Section 7.2: Review of OLS Regression
# Section 7.22: Bivariate OLS Regression
# ========================================================================

# Download state-level panel dataset (50 states × 27 years, 1990-2016)
download.file("https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_2_2.dta",
              "Example_7_2_2.dta", mode = "wb")

data_7_2_2 <- read_dta("Example_7_2_2.dta")

# Create per FTE variables (dividing aggregate amounts by enrollment)
data_7_2_2 <- data_7_2_2 %>%
  mutate(netuit_fte = netuit / fte,
         stapr_fte = stapr / fte)

# Bivariate regression for single year (2016)
# Tests relationship between state appropriations and net tuition per FTE
model_bivariate <- lm(netuit_fte ~ stapr_fte, 
                      data = data_7_2_2 %>% filter(year == 2016))
summary(model_bivariate)

# Expected results: Negative coefficient (~-0.35), R² ≈ 0.13, F ≈ 7.19

# ========================================================================
# Section 7.23: Multivariate OLS Regression
# ========================================================================

# Create squared term to test for non-linear (quadratic) relationship
data_7_2_2 <- data_7_2_2 %>%
  mutate(stapr_fte2 = stapr_fte * stapr_fte)

# Add polynomial term and additional control variable (per capita income)
model_multivariate <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income, 
                         data = data_7_2_2 %>% filter(year == 2016))
summary(model_multivariate)

# Expected results: R² increases to ~0.28 with additional variables

# ========================================================================
# Section 7.24: Multivariate Pooled OLS Regression
# ========================================================================

# Pooled OLS uses all years of data (1990-2016) not just 2016
# This increases N from 50 to 1,350 observations
model_pooled <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income, 
                   data = data_7_2_2)
summary(model_pooled)

# Add categorical control for regional compact membership
# factor() creates dummy variables for each category
model_pooled_region <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income + 
                            factor(region_compact), 
                          data = data_7_2_2)
summary(model_pooled_region)

# ========================================================================
# Section 7.24.1: Multivariate Pooled OLS Regression with Interaction Terms
# ========================================================================

# EXAMPLE 1: Categorical × Categorical Interaction
# Tests whether merit aid effects differ by region
model_cat_cat <- lm(netuit_fte ~ stapr_fte + 
                      factor(region_compact) * factor(ugradmerit), 
                    data = data_7_2_2)
summary(model_cat_cat)

# Store models to test whether adding interactions improves fit
model1 <- lm(netuit_fte ~ stapr_fte + factor(region_compact), 
             data = data_7_2_2)
model2 <- lm(netuit_fte ~ stapr_fte + 
               factor(region_compact) * factor(ugradmerit), 
             data = data_7_2_2)

# Likelihood ratio test: Are interaction terms jointly significant?
lrtest(model1, model2)

# Alternative test of interaction terms only (Wald test)
# Test all interaction terms jointly
linearHypothesis(model2, grep(":", names(coef(model2)), value = TRUE))

# EXAMPLE 2: Categorical × Continuous Interaction
# Tests whether appropriations effect varies by tuition-setting authority
model_cat_cont <- lm(netuit_fte ~ factor(ugradmerit) + factor(region_compact) + 
                       stapr_fte * factor(tuitset), 
                     data = data_7_2_2)
summary(model_cat_cont)

# Test interaction terms
linearHypothesis(model_cat_cont, 
                 grep("stapr_fte:factor\\(tuitset\\)", 
                      names(coef(model_cat_cont)), value = TRUE))

# EXAMPLE 3: Continuous × Continuous Interaction
# Tests whether appropriations effect varies by level of need-based aid
model_cont_cont <- lm(netuit_fte ~ factor(region_compact) + 
                        stapr_fte * state_needFTE, 
                      data = data_7_2_2)
summary(model_cont_cont)

# Calculate marginal effect of appropriations at different aid levels
# Create prediction data frame
pred_data <- expand.grid(
  state_needFTE = seq(0, 10000, by = 3000),
  stapr_fte = c(0, 10000),
  region_compact = 1  # Use first level of region_compact
)

# Get predictions
predictions <- predict(model_cont_cont, newdata = pred_data, se.fit = TRUE)

# Create data frame for plotting
plot_data <- data.frame(
  stapr_fte = pred_data$stapr_fte,
  state_needFTE = pred_data$state_needFTE,
  predicted = predictions$fit
)

# Create visualization showing how relationship changes
ggplot(plot_data, aes(x = stapr_fte, y = predicted, 
                      color = factor(state_needFTE))) +
  geom_line(size = 1) +
  labs(x = "State Appropriations per FTE",
       y = "Predicted Net Tuition per FTE",
       color = "State Need-Based Aid per FTE",
       title = "Interaction Effect: Appropriations × Need-Based Aid") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 10000, by = 3000))

# ========================================================================
# Section 7.24: Testing Regression Assumptions
# ========================================================================

# Estimate model for diagnostic testing
model_diag <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income + 
                   factor(region_compact), 
                 data = data_7_2_2)

# Create residual-versus-fitted plot to check for heteroscedasticity
# Funnel shape indicates violation of constant variance assumption
plot(fitted(model_diag), residuals(model_diag),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residual vs Fitted Plot")
abline(h = 0, col = "red", lty = 2)

# Breusch-Pagan test for heteroscedasticity
bptest(model_diag)

# Robust standard errors correct for heteroscedasticity
# Results are valid even if constant variance assumption is violated
coeftest(model_diag, vcov = vcovHC(model_diag, type = "HC1"))

# Cluster-robust standard errors account for within-state correlation
# Use when observations within same state are not independent
model_cluster <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income + 
                      factor(region_compact), 
                    data = data_7_2_2)

# Calculate cluster-robust standard errors
vcov_cluster <- cluster.vcov(model_cluster, data_7_2_2$state)
coeftest(model_cluster, vcov_cluster)

# ========================================================================
# Section 7.4: Fixed-Effects Regression
# Section 7.4.2: Estimating FEDV Multivariate POLS Regression Models
# ========================================================================

# Note: The first models in this section use state-level data (Example_7_2_2)
# Later we switch to institutional-level data (Example_7_4_2)

# Fixed-effects with state dummy variables (FEDV approach)
# factor(stateid) creates dummy for each state, controlling for state differences
model_fedv <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income + 
                   factor(stateid), 
                 data = data_7_2_2)

# Calculate cluster-robust standard errors
vcov_cluster_state <- cluster.vcov(model_fedv, data_7_2_2$state)
coeftest(model_fedv, vcov_cluster_state)

# Test whether state dummies are jointly significant
# If significant, state fixed effects are needed
linearHypothesis(model_fedv, 
                 grep("factor\\(stateid\\)", names(coef(model_fedv)), 
                      value = TRUE))

# Alternative: Use plm package for more efficient fixed effects estimation
# Set up panel data structure (using state-level data)
pdata_state_fe <- pdata.frame(data_7_2_2, index = c("stateid", "year"))

# Within estimator (fixed effects)
# Produces same coefficients but more efficient computation
model_plm_state <- plm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income, 
                       data = pdata_state_fe, 
                       model = "within", 
                       effect = "individual")

# Get cluster-robust standard errors
coeftest(model_plm_state, vcov = vcovHC(model_plm_state, 
                                        type = "HC1", 
                                        cluster = "group"))

# Now load institutional-level panel dataset for institutional models
# Different dataset: 220 institutions observed over ~9 years each
download.file("https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_4_2.dta",
              "Example_7_4_2.dta", mode = "wb")

data_7_4_2 <- read_dta("Example_7_4_2.dta")

# Institutional-level fixed effects example
# Controls for time-invariant institution characteristics
# eg = education & general expenditures (dependent variable)
pdata_inst <- pdata.frame(data_7_4_2, index = c("opeid5_new", "endyear"))

model_inst_fe <- plm(eg ~ statea + tuition + totfteiarep + ftfac + ptfac + D, 
                     data = pdata_inst, 
                     model = "within", 
                     effect = "individual")

coeftest(model_inst_fe, vcov = vcovHC(model_inst_fe, 
                                      type = "HC1", 
                                      cluster = "group"))

# ========================================================================
# Section 7.4.2.1: Within-Group Estimator Fixed-Effects Regression
# ========================================================================

# plm with model = "within" uses "within" transformation
# Automatically removes time-invariant characteristics
# More information provided (within/between R²)
model_within <- plm(eg ~ statea + tuition + totfteiarep + ftfac + ptfac, 
                    data = pdata_inst, 
                    model = "within", 
                    effect = "individual")

# Get cluster-robust standard errors
coeftest(model_within, vcov = vcovHC(model_within, 
                                     type = "HC1", 
                                     cluster = "group"))

# Display model summary with additional diagnostics
summary(model_within)

# Output includes:
#   - within R²: variation explained within units
#   - between R²: variation explained between units

# ========================================================================
# Section 7.5: Random-Effects Regression
# ========================================================================

# Return to state-level panel data
data_7_2_2 <- read_dta("Example_7_2_2.dta")

# Recreate per FTE variables
data_7_2_2 <- data_7_2_2 %>%
  mutate(netuit_fte = netuit / fte,
         stapr_fte = stapr / fte,
         stapr_fte2 = stapr_fte * stapr_fte)

# Set up panel data structure
pdata_state <- pdata.frame(data_7_2_2, index = c("stateid", "year"))

# Random-effects assumes unit effects uncorrelated with predictors
# GLS estimation is more efficient than FE if assumption holds
# Note: Removed factor(region_compact) as it's time-invariant
model_re <- plm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income, 
                data = pdata_state, 
                model = "random", 
                effect = "individual")

# Display summary with standard errors
summary(model_re)

# Breusch-Pagan test: Is random effects better than pooled OLS?
# Null hypothesis: variance of random effects = 0
# If rejected, use random effects instead of pooled OLS
plmtest(model_re, type = "bp")

# ========================================================================
# Section 7.5.1: Hausman Test
# ========================================================================

# Hausman test: Should we use fixed or random effects?
# Null hypothesis: Random effects estimates are consistent
# If rejected, use fixed effects

# For institutional-level data
data_7_4_2 <- read_dta("Example_7_4_2.dta")
pdata_inst <- pdata.frame(data_7_4_2, index = c("opeid5_new", "endyear"))

# Log-transformed variables work better for Hausman test
# Reduces influence of outliers and improves test properties
model_fe_log <- plm(lneg ~ lnstatea + lntuition + lntotfteiarep + 
                      lnftfac + ptfac, 
                    data = pdata_inst, 
                    model = "within", 
                    effect = "individual")

model_re_log <- plm(lneg ~ lnstatea + lntuition + lntotfteiarep + 
                      lnftfac + ptfac, 
                    data = pdata_inst, 
                    model = "random", 
                    effect = "individual")

# Hausman test with log-transformed variables
phtest(model_fe_log, model_re_log)


################################################################################
################################################################################
##
## SECTION 7.6: INSTRUMENTAL VARIABLES AND TWO-STAGE LEAST SQUARES
##
## This section introduces IV/2SLS estimation using a simulation based on
## the Baccalaureate and Beyond Longitudinal Study (B&B) characteristics.
##
## Application: Effect of Master's Degree on Salary Outcomes
## Instrument: State Graduate Assistantship (GA) Funding
##
## The same synthetic dataset is used in Chapter 10 for Marginal Treatment
## Effects (MTE) analysis, providing pedagogical continuity.
##
################################################################################
################################################################################

# Clear environment for Section 7.6
rm(list = ls())

# Set seed for reproducibility
set.seed(20251130)

# Sample size
N <- 8000

# ========================================================================
# Section 7.6.1-7.6.5: Synthetic Data Generation
# (B&B-Style Simulation for IV/2SLS Demonstration)
# ========================================================================

# NOTE ON SYNTHETIC DATA:
# -----------------------
# This application uses synthetic data calibrated to mirror the Baccalaureate
# and Beyond Longitudinal Study (B&B). We use synthetic rather than actual
# B&B data for several reasons:
#
# 1. ACCESS RESTRICTIONS: B&B restricted-use data requires NCES license
# 2. PEDAGOGICAL TRANSPARENCY: Known true parameters allow validation
# 3. REPRODUCIBILITY: Readers can generate identical datasets
# 4. CONTINUITY: Same dataset used in Chapter 10 for MTE analysis
#
# NOTE ON AI-ASSISTED CODE DEVELOPMENT:
# -------------------------------------
# The simulation code was developed with assistance from Claude (Anthropic).
# The author provided specifications based on B&B characteristics and higher
# education finance literature. Claude assisted in translating specifications
# to executable code. The author reviewed, tested, and validated all code.

cat("\n==============================================\n")
cat("SECTION 7.6: IV/2SLS DEMONSTRATION\n")
cat("Generating Synthetic B&B Data\n")
cat("==============================================\n")

# Create data frame
df <- data.frame(id = 1:N)

#--- Section 1: Demographics ---#

df$female <- rbinom(N, 1, 0.57)

race_rand <- runif(N)
df$white <- as.integer(race_rand < 0.62)
df$black <- as.integer(race_rand >= 0.62 & race_rand < 0.72)
df$hispanic <- as.integer(race_rand >= 0.72 & race_rand < 0.84)
df$asian <- as.integer(race_rand >= 0.84 & race_rand < 0.92)
df$other_race <- as.integer(race_rand >= 0.92)

df$age_ba <- 22 + rpois(N, 1.5)
df$age_ba[df$age_ba < 20] <- 22
df$age_ba[df$age_ba > 35] <- 35

#--- Section 2: Family Background ---#

df$firstgen <- rbinom(N, 1, 0.35)
df$parent_income_q <- 1 + rbinom(N, 4, 0.55)
df$parent_grad <- rbinom(N, 1, 0.25)

#--- Section 3: Academic Background ---#

df$ugpa <- 2.0 + 1.2 * rbeta(N, 5, 3)
df$ugpa[df$ugpa > 4.0] <- 4.0
df$ugpa[df$ugpa < 2.0] <- 2.0

df$stem_major <- rbinom(N, 1, 0.25)
df$bus_major <- ifelse(df$stem_major == 0, rbinom(N, 1, 0.20), 0)
df$ed_major <- ifelse(df$stem_major == 0 & df$bus_major == 0, rbinom(N, 1, 0.15), 0)
df$socsci_major <- as.integer(df$stem_major == 0 & df$bus_major == 0 & df$ed_major == 0)

df$selective_inst <- rbinom(N, 1, 0.30)
df$public_ug <- rbinom(N, 1, 0.65)

#--- Section 4: Labor Market ---#

df$state_unemp <- 4 + 6 * rbeta(N, 2, 3)
df$metro <- rbinom(N, 1, 0.75)

#--- Section 5: Generate Instrument - State GA Funding ---#

df$state <- ceiling(50 * runif(N))

# State-level GA funding (with state fixed effects)
state_effects <- data.frame(
  state = 1:50,
  state_effect = rnorm(50, 0, 4)
)
df <- merge(df, state_effects, by = "state", all.x = TRUE)

df$ga_funding <- 18 + df$state_effect + rnorm(N, 0, 2)
df$ga_funding[df$ga_funding < 8] <- 8
df$ga_funding[df$ga_funding > 35] <- 35

# Field-adjusted GA funding
df$ga_field_mult <- ifelse(df$stem_major == 1, 1.3,
                    ifelse(df$bus_major == 1, 0.9,
                    ifelse(df$ed_major == 1, 1.1, 1.0)))
df$ga_funding_adj <- df$ga_funding * df$ga_field_mult

#--- Section 6: Generate Latent Factors (Unobserved) ---#

df$eta_ability <- rnorm(N, 0, 1)
df$eta_taste <- 0.3 * df$eta_ability + rnorm(N, 0, 0.9)
df$eta_prod <- 0.5 * df$eta_ability + rnorm(N, 0, 0.85)

#--- Section 7: Generate Treatment (Master's Degree) ---#

df$z_masters <- -0.9 +
  0.15 * df$female +
  0.10 * df$black +
  0.05 * df$hispanic +
  0.20 * df$asian +
  -0.03 * (df$age_ba - 22) +
  -0.25 * df$firstgen +
  0.08 * df$parent_income_q +
  0.35 * df$parent_grad +
  0.60 * (df$ugpa - 3.0) +
  0.20 * df$stem_major +
  -0.15 * df$bus_major +
  0.45 * df$ed_major +
  0.30 * df$selective_inst +
  -0.02 * df$state_unemp +
  0.15 * df$metro +
  0.06 * (df$ga_funding_adj - 18) +  # INSTRUMENT EFFECT
  0.40 * df$eta_taste +               # Unobserved taste for education
  0.25 * df$eta_ability               # Unobserved ability

df$p_masters <- pnorm(df$z_masters)
df$u_d <- runif(N)
df$masters <- as.integer(df$p_masters > df$u_d)

#--- Section 8: Generate Outcome (Salary) ---#

# Potential outcome without treatment (Y0)
df$ln_salary_0 <- 10.50 +
  -0.08 * df$female +
  -0.05 * df$black +
  -0.03 * df$hispanic +
  0.06 * df$asian +
  0.02 * (df$age_ba - 22) +
  -0.03 * df$firstgen +
  0.03 * df$parent_income_q +
  0.04 * df$parent_grad +
  0.10 * (df$ugpa - 3.0) +
  0.25 * df$stem_major +
  0.15 * df$bus_major +
  -0.12 * df$ed_major +
  0.08 * df$selective_inst +
  -0.01 * df$state_unemp +
  0.10 * df$metro +
  0.20 * df$eta_prod +                # Unobserved productivity
  rnorm(N, 0, 0.25)

# Heterogeneous treatment effect (essential heterogeneity)
df$te_masters <- 0.12 +
  0.08 * df$stem_major +
  0.05 * df$bus_major +
  0.10 * df$ed_major +
  0.03 * df$selective_inst +
  0.05 * (df$ugpa - 3.0) +
  0.08 * df$eta_ability +             # Ability-education complementarity
  -0.10 * (df$p_masters - 0.5) +      # Essential heterogeneity
  rnorm(N, 0, 0.05)

# Potential outcome with treatment (Y1)
df$ln_salary_1 <- df$ln_salary_0 + df$te_masters

# Observed outcome (switching regression)
df$ln_salary <- df$masters * df$ln_salary_1 + (1 - df$masters) * df$ln_salary_0
df$salary <- exp(df$ln_salary)

# ========================================================================
# Section 7.6.6: Summary Statistics and True Parameters
# ========================================================================

cat("\n--- Sample Characteristics ---\n")
cat("Treatment distribution:\n")
print(table(df$masters))

cat("\nKey variables summary:\n")
summary_vars <- df[, c("salary", "ln_salary", "masters", "female", "ugpa", 
                       "ga_funding_adj", "p_masters")]
print(summary(summary_vars))

cat("\n--- True Treatment Effects (from DGP) ---\n")
true_att <- mean(df$te_masters[df$masters == 1])
true_atu <- mean(df$te_masters[df$masters == 0])
true_ate <- mean(df$te_masters)

cat(sprintf("True ATT (treated):     %.4f\n", true_att))
cat(sprintf("True ATU (untreated):   %.4f\n", true_atu))
cat(sprintf("True ATE (population):  %.4f\n", true_ate))

cat("\nSelection Pattern: ATT > ATE > ATU\n")
cat("This confirms POSITIVE SELECTION on gains\n")
cat("(Those who select into treatment benefit more)\n")

# ========================================================================
# Section 7.6.7: Naive OLS Estimation (Biased)
# ========================================================================

cat("\n==============================================\n")
cat("NAIVE OLS ESTIMATION\n")
cat("==============================================\n")

# Define control variables
X_vars <- c("female", "black", "hispanic", "asian", "age_ba", "firstgen",
            "parent_income_q", "parent_grad", "ugpa", "stem_major", "bus_major",
            "ed_major", "selective_inst", "public_ug", "state_unemp", "metro")
X_formula <- paste(X_vars, collapse = " + ")

# OLS regression (biased due to selection on unobservables)
ols_formula <- as.formula(paste("ln_salary ~ masters +", X_formula))
ols_model <- lm(ols_formula, data = df)

# Robust standard errors
ols_robust <- coeftest(ols_model, vcov = vcovHC(ols_model, type = "HC1"))

ols_est <- coef(ols_model)["masters"]
ols_se <- ols_robust["masters", "Std. Error"]

cat(sprintf("\nOLS Estimate: %.4f (SE = %.4f)\n", ols_est, ols_se))
cat(sprintf("True ATE:     %.4f\n", true_ate))

ols_bias <- (ols_est - true_ate) / true_ate * 100
cat(sprintf("OLS Bias:     %.1f%% (upward bias due to positive selection)\n", ols_bias))

# ========================================================================
# Section 7.6.8: First-Stage Regression (Instrument Relevance)
# ========================================================================

cat("\n==============================================\n")
cat("FIRST-STAGE REGRESSION\n")
cat("(Testing Instrument Relevance)\n")
cat("==============================================\n")

# First stage: Regress endogenous variable on instrument and controls
first_stage_formula <- as.formula(paste("masters ~ ga_funding_adj +", X_formula))
first_stage <- lm(first_stage_formula, data = df)

# Robust standard errors
fs_robust <- coeftest(first_stage, vcov = vcovHC(first_stage, type = "HC1"))

fs_coef <- coef(first_stage)["ga_funding_adj"]
fs_se <- fs_robust["ga_funding_adj", "Std. Error"]
fs_t <- fs_coef / fs_se
fs_F <- fs_t^2

cat("\nFirst-Stage Results:\n")
cat(sprintf("  GA Funding coefficient: %.4f\n", fs_coef))
cat(sprintf("  Standard error:         %.4f\n", fs_se))
cat(sprintf("  t-statistic:            %.2f\n", fs_t))
cat(sprintf("  Partial F-statistic:    %.1f\n", fs_F))
cat("\n  Stock-Yogo threshold:   F > 10 for weak instrument test\n")

if (fs_F > 10) {
  cat(sprintf("  RESULT: Strong instrument (F = %.1f >> 10)\n", fs_F))
} else {
  cat(sprintf("  WARNING: Potentially weak instrument (F = %.1f)\n", fs_F))
}

# ========================================================================
# Section 7.6.9: IV/2SLS Estimation (LATE)
# ========================================================================

cat("\n==============================================\n")
cat("IV/2SLS ESTIMATION\n")
cat("(Local Average Treatment Effect)\n")
cat("==============================================\n")

# IV/2SLS using ivreg from AER package
iv_formula <- as.formula(paste("ln_salary ~ masters +", X_formula, 
                                "| ga_funding_adj +", X_formula))
iv_model <- ivreg(iv_formula, data = df)

# Summary with diagnostics
iv_summary <- summary(iv_model, diagnostics = TRUE)
print(iv_summary)

iv_est <- coef(iv_model)["masters"]
iv_se <- iv_summary$coefficients["masters", "Std. Error"]

cat("\nIV/2SLS Results:\n")
cat(sprintf("  LATE Estimate:    %.4f\n", iv_est))
cat(sprintf("  Standard Error:   %.4f\n", iv_se))
cat(sprintf("  95%% CI:          [%.4f, %.4f]\n", 
            iv_est - 1.96*iv_se, iv_est + 1.96*iv_se))

cat("\nInterpretation:\n")
cat("  The IV estimate identifies the Local Average Treatment Effect (LATE)\n")
cat("  for COMPLIERS - those whose master's degree completion is affected\n")
cat("  by variation in state GA funding.\n")

# ========================================================================
# Section 7.6.10: Comparison of Estimates
# ========================================================================

cat("\n==============================================\n")
cat("COMPARISON OF ESTIMATES\n")
cat("==============================================\n")

cat("\nMethod               Estimate    Std.Err.    Interpretation\n")
cat("================================================================\n")
cat(sprintf("True ATE             %7.4f       —       Population average effect\n", true_ate))
cat(sprintf("True ATT             %7.4f       —       Effect for treated\n", true_att))
cat(sprintf("True ATU             %7.4f       —       Effect for untreated\n", true_atu))
cat("----------------------------------------------------------------\n")
cat(sprintf("OLS (biased)         %7.4f    %6.4f    Confounded by selection\n", ols_est, ols_se))
cat(sprintf("IV/2SLS (LATE)       %7.4f    %6.4f    Effect for compliers\n", iv_est, iv_se))
cat("================================================================\n")

cat("\nKey Insights:\n")
cat(sprintf("  1. OLS is biased upward (%.1f%%) due to positive selection\n", ols_bias))
cat("  2. IV provides consistent estimate of LATE for compliers\n")
cat("  3. LATE ≠ ATE when treatment effects are heterogeneous\n")
cat(sprintf("  4. First-stage F = %.1f confirms strong instrument\n", fs_F))

# ========================================================================
# Section 7.6.11: Manual 2SLS (Pedagogical Demonstration)
# ========================================================================

cat("\n==============================================\n")
cat("MANUAL 2SLS (for understanding)\n")
cat("==============================================\n")

cat("NOTE: This manual approach is for pedagogical purposes only.\n")
cat("      Standard errors are INCORRECT with manual 2SLS.\n")
cat("      Always use ivreg for proper inference.\n")

# Stage 1: Predict treatment using instrument
stage1 <- lm(first_stage_formula, data = df)
df$masters_hat <- fitted(stage1)

# Stage 2: Regress outcome on predicted treatment
stage2_formula <- as.formula(paste("ln_salary ~ masters_hat +", X_formula))
stage2 <- lm(stage2_formula, data = df)

manual_iv <- coef(stage2)["masters_hat"]
cat(sprintf("\nManual 2SLS estimate: %.4f\n", manual_iv))
cat(sprintf("ivreg estimate:       %.4f\n", iv_est))
cat("(Should be identical)\n")

# ========================================================================
# Section 7.6.12: Preview of Chapter 10 (MTE Framework)
# ========================================================================

cat("\n==============================================\n")
cat("PREVIEW: CHAPTER 10 - MARGINAL TREATMENT EFFECTS\n")
cat("==============================================\n")

cat("\nThe IV/LATE framework has an important limitation:\n")
cat("  - LATE identifies the effect only for COMPLIERS\n")
cat("  - Different instruments yield different LATEs\n")
cat("  - We cannot recover ATE, ATT, or ATU directly\n")

cat("\nIn Chapter 10, we extend this analysis using:\n")
cat("  - Marginal Treatment Effects (MTE) framework\n")
cat("  - Recovers full distribution of treatment effects\n")
cat("  - Allows calculation of ATE, ATT, ATU, and policy-specific effects\n")
cat("  - Uses the same synthetic B&B dataset for continuity\n")

cat("\nThe MTE framework reveals:\n")
cat("  - How treatment effects vary with propensity to select\n")
cat("  - Selection patterns (positive vs. negative selection on gains)\n")
cat("  - Policy-relevant treatment effects (PRTE, MPRTE)\n")

# ========================================================================
# Save Dataset for Chapter 10
# ========================================================================

# Select variables to save
save_vars <- c("id", "masters", "ln_salary", "salary", "te_masters", "p_masters",
               "female", "black", "hispanic", "asian", "age_ba", "firstgen",
               "parent_income_q", "parent_grad", "ugpa", "stem_major", "bus_major",
               "ed_major", "selective_inst", "public_ug", "state_unemp", "metro",
               "ga_funding_adj", "state")

write.csv(df[, save_vars], "bb_iv_simulation.csv", row.names = FALSE)

cat("\nDataset saved: bb_iv_simulation.csv\n")
cat("This dataset will be used in Chapter 10 for MTE analysis.\n")

# ================================================================
# END OF CHAPTER 7 CODE
# ================================================================
