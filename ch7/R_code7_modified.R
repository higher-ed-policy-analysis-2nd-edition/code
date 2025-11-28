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
                       "stargazer", "broom", "tidyr")

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

# Levene's test for heteroscedasticity across states
# First create residuals
data_7_2_2$residuals <- NA
data_7_2_2$residuals[as.numeric(names(residuals(model_diag)))] <- residuals(model_diag)

# Test if variance differs by state
leveneTest(residuals ~ factor(state), 
           data = data_7_2_2 %>% filter(!is.na(residuals)))

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

# Note: R's plm package has numerical instability issues when computing 
# cluster-robust standard errors for random effects models.
# Stata handles this more gracefully. For cluster-robust inference with
# panel data in R, fixed effects models are more reliable.

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

# Note: Random effects estimation with level variables often encounters
# numerical instability issues in R due to multicollinearity/scaling.
# We proceed directly to log-transformed variables which work more reliably.

# Estimate both models with level variables (FE works, but RE may fail in R)
model_fe <- plm(eg ~ statea + tuition + totfteiarep + ftfac + ptfac, 
                data = pdata_inst, 
                model = "within", 
                effect = "individual")

# Attempting RE with level variables often fails in R:
# model_re_inst <- plm(eg ~ statea + tuition + totfteiarep + ftfac + ptfac, 
#                      data = pdata_inst, 
#                      model = "random", 
#                      effect = "individual")
# Error: system is computationally singular

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

# Note: Cluster-robust Hausman test requires custom implementation
# The standard phtest doesn't support clustering
# For a cluster-robust version, you would need to implement a bootstrap procedure

# Clean up
rm(list = ls())

# ================================================================
# END OF CHAPTER 7 CODE
# ================================================================