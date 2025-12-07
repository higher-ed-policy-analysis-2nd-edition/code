################################################################################
# MTE and MPRTE Analysis: Effect of Master's Degree on Salary Outcomes
# Chapter 10 - Marginal Treatment Effects
#
# Instrument: State-Funded Graduate Assistantship (GA) Dollar Amount
# 
# Based on synthetic data mirroring NCES B&B Longitudinal Study characteristics
# and higher education finance literature (Titus 2007; Bound, Lovenheim & 
# Turner 2010; Zhang 2005; Ehrenberg et al. 2007)
#
# Author: Marvin A. Titus
# Date: December 2025
# Purpose: Demonstrate MTE/MPRTE framework for textbook Chapter 10
#
# Data Source: Synthetic dataset mirroring NCES B&B
# https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_5_3.dta
#
# Note: Because a synthetic dataset is used in this application, the results
# are intended to illustrate MTE/MPRTE estimation methods and should not be
# viewed as having policy implications.
#
# R Translation of Stata_code10b_modified.do
# Updated to include localIV package (R equivalent of Stata's mtefe)
################################################################################

# Clear workspace
rm(list = ls())

# Set seed for reproducibility
set.seed(20251130)

# =============================================================================
# Load Required Packages
# =============================================================================

# Install packages if not already installed
required_packages <- c("haven", "dplyr", "tidyr", "ggplot2", "lmtest", 
                       "sandwich", "AER", "sampleSelection", "margins",
                       "gridExtra", "scales", "localIV")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

cat("\n==============================================\n")
cat("REQUIRED PACKAGES LOADED\n")
cat("==============================================\n")

################################################################################
# SECTION 1: Load Synthetic Dataset
################################################################################

cat("\n==============================================\n")
cat("LOADING SYNTHETIC B&B DATASET\n")
cat("==============================================\n")

# Download and load synthetic dataset
url <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_5_3.dta"
temp_file <- tempfile(fileext = ".dta")
download.file(url, temp_file, mode = "wb")
data <- haven::read_dta(temp_file)

# Generate observation ID if not present
if (!"id" %in% names(data)) {
  data$id <- 1:nrow(data)
}

# Display dataset information
cat("\nDataset dimensions:", nrow(data), "observations,", ncol(data), "variables\n")
cat("Sample size:", nrow(data), "\n")

# Show variable names
cat("\nVariables in dataset:\n")
print(names(data))

################################################################################
# SECTION 2: Summary Statistics
################################################################################

cat("\n==============================================\n")
cat("SUMMARY STATISTICS\n")
cat("==============================================\n")

# Treatment variable
cat("\n--- Treatment: Master's Degree Completion ---\n")
print(table(data$masters))
cat("\nTreatment summary:\n")
print(summary(data$masters))

# Store treatment rate
treat_rate <- mean(data$masters, na.rm = TRUE)
cat(sprintf("Treatment rate: %.3f\n", treat_rate))

# Key variables
cat("\n--- Key Variables ---\n")
key_vars <- data %>% 
  select(ln_salary, salary, masters, ga_funding_adj) %>%
  summary()
print(key_vars)

# Outcome by treatment status
cat("\n--- Salary by Master's Degree Status ---\n")
salary_by_treatment <- data %>%
  group_by(masters) %>%
  summarise(
    mean_salary = mean(salary, na.rm = TRUE),
    sd_salary = sd(salary, na.rm = TRUE),
    mean_ln_salary = mean(ln_salary, na.rm = TRUE),
    sd_ln_salary = sd(ln_salary, na.rm = TRUE),
    n = n()
  )
print(salary_by_treatment)

# Instrument summary
cat("\n--- Instrument: GA Funding ---\n")
print(summary(data$ga_funding_adj))
cat("Standard deviation:", sd(data$ga_funding_adj, na.rm = TRUE), "\n")

# Control variables
cat("\n--- Control Variables ---\n")
control_vars <- c("female", "black", "hispanic", "asian", "age_ba", "firstgen",
                  "parent_income_q", "parent_grad", "ugpa", "stem_major", 
                  "bus_major", "ed_major", "selective_inst", "public_ug", 
                  "state_unemp", "metro")
print(summary(data[, control_vars]))

################################################################################
# SECTION 3: First-Stage and Instrument Relevance
################################################################################

cat("\n==============================================\n")
cat("INSTRUMENT RELEVANCE CHECK\n")
cat("==============================================\n")

# Define control variables formula
X_controls <- c("female", "black", "hispanic", "asian", "age_ba", "firstgen",
                "parent_income_q", "parent_grad", "ugpa", "stem_major", 
                "bus_major", "ed_major", "selective_inst", "public_ug", 
                "state_unemp", "metro")

# First-stage regression
cat("\n--- First-Stage Regression ---\n")
first_stage_formula <- as.formula(paste("masters ~ ga_funding_adj +", 
                                         paste(X_controls, collapse = " + ")))
first_stage <- lm(first_stage_formula, data = data)

# Robust standard errors
first_stage_robust <- coeftest(first_stage, vcov = vcovHC(first_stage, type = "HC1"))
print(first_stage_robust)

# Test instrument strength (F-test for ga_funding_adj)
first_stage_F <- (first_stage_robust["ga_funding_adj", "t value"])^2
cat(sprintf("\nFirst-stage F-statistic: %.2f (should be >> 10)\n", first_stage_F))

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

ols_formula <- as.formula(paste("ln_salary ~ masters +", 
                                 paste(X_controls, collapse = " + ")))
ols_naive <- lm(ols_formula, data = data)

# Robust standard errors
ols_robust <- coeftest(ols_naive, vcov = vcovHC(ols_naive, type = "HC1"))
print(ols_robust)

ols_est <- coef(ols_naive)["masters"]
ols_se <- ols_robust["masters", "Std. Error"]

cat(sprintf("\nOLS estimate: %.4f (SE = %.4f)\n", ols_est, ols_se))
cat("Note: Likely biased upward due to positive selection on unobservables\n")

################################################################################
# SECTION 5: IV/2SLS Estimation (LATE)
################################################################################

cat("\n==============================================\n")
cat("IV/2SLS ESTIMATION (LATE)\n")
cat("==============================================\n")

# IV regression using AER package
iv_formula2 <- as.formula(paste("ln_salary ~ masters +", 
                                 paste(X_controls, collapse = " + "),
                                 "| ga_funding_adj +",
                                 paste(X_controls, collapse = " + ")))

iv_2sls <- ivreg(iv_formula2, data = data)

# Summary with diagnostics
iv_summary <- summary(iv_2sls, diagnostics = TRUE)
print(iv_summary)

iv_est <- coef(iv_2sls)["masters"]
iv_se <- iv_summary$coefficients["masters", "Std. Error"]

cat(sprintf("\nIV/LATE estimate: %.4f (SE = %.4f)\n", iv_est, iv_se))
cat("Interpretation: Effect for compliers induced by GA funding variation\n")

# First-stage diagnostics
cat("\n--- First-Stage Diagnostics ---\n")
cat("Weak instruments test (F-statistic):", iv_summary$diagnostics["Weak instruments", "statistic"], "\n")

# Wu-Hausman endogeneity test
cat("\n--- Endogeneity Test ---\n")
cat("Wu-Hausman test statistic:", iv_summary$diagnostics["Wu-Hausman", "statistic"], "\n")
cat("Wu-Hausman p-value:", iv_summary$diagnostics["Wu-Hausman", "p-value"], "\n")

################################################################################
# SECTION 6: MTE ESTIMATION
################################################################################

cat("\n==============================================\n")
cat("MTE ESTIMATION\n")
cat("==============================================\n")

# ------------------------------------------------------------------------------
# Approach 1: Parametric MTE via Polynomial in Propensity Score (Manual)
# ------------------------------------------------------------------------------

cat("\n--- Approach 1: Polynomial MTE (Manual) ---\n")

# Step 1: Estimate propensity score via probit
probit_formula <- as.formula(paste("masters ~ ga_funding_adj +", 
                                    paste(X_controls, collapse = " + ")))
probit_model <- glm(probit_formula, data = data, family = binomial(link = "probit"))
summary(probit_model)

# Predicted probabilities (propensity scores)
data$phat <- predict(probit_model, type = "response")

# Store probit coefficient for GA funding
ga_coef <- coef(probit_model)["ga_funding_adj"]
cat(sprintf("GA funding coefficient in probit: %.5f\n", ga_coef))

# Store linear index for policy simulations
data$z_index <- predict(probit_model, type = "link")

# Step 2: Generate polynomial terms
data$phat2 <- data$phat^2
data$phat3 <- data$phat^3

# Step 3: Estimate switching regression with quadratic interactions
cat("\n--- Quadratic MTE Specification ---\n")
quad_formula <- as.formula(paste("ln_salary ~ masters + masters:phat + masters:phat2 +",
                                  paste(X_controls, collapse = " + "),
                                  "+ phat + phat2"))
quad_model <- lm(quad_formula, data = data)
quad_robust <- coeftest(quad_model, vcov = vcovHC(quad_model, type = "HC1"))
print(quad_robust)

# Extract quadratic coefficients
b0_quad <- coef(quad_model)["masters"]
b1_quad <- coef(quad_model)["masters:phat"]
b2_quad <- coef(quad_model)["masters:phat2"]

cat(sprintf("\nQuadratic MTE(u) = %.4f + %.4f*u + %.4f*u²\n", b0_quad, b1_quad, b2_quad))

# Calculate ATE (quadratic): integral from 0 to 1
ate_est_quad <- b0_quad + b1_quad/2 + b2_quad/3
cat(sprintf("Estimated ATE (quadratic): %.4f\n", ate_est_quad))

# Step 4: Estimate cubic specification
cat("\n--- Cubic MTE Specification ---\n")
cubic_formula <- as.formula(paste("ln_salary ~ masters + masters:phat + masters:phat2 + masters:phat3 +",
                                   paste(X_controls, collapse = " + "),
                                   "+ phat + phat2 + phat3"))
cubic_model <- lm(cubic_formula, data = data)
cubic_robust <- coeftest(cubic_model, vcov = vcovHC(cubic_model, type = "HC1"))
print(cubic_robust)

# Extract cubic coefficients
b0 <- coef(cubic_model)["masters"]
b1 <- coef(cubic_model)["masters:phat"]
b2 <- coef(cubic_model)["masters:phat2"]
b3 <- coef(cubic_model)["masters:phat3"]

cat(sprintf("\nCubic MTE(u) = %.4f + %.4f*u + %.4f*u² + %.4f*u³\n", b0, b1, b2, b3))

# Calculate ATE (cubic): b0 + b1/2 + b2/3 + b3/4
ate_est_cubic <- b0 + b1/2 + b2/3 + b3/4
cat(sprintf("Estimated ATE (cubic): %.4f\n", ate_est_cubic))

# Create MTE function variable
data$mte_hat <- b0 + b1*data$phat + b2*data$phat2 + b3*data$phat3

# ATT and ATU via numerical approximation
att_est <- mean(data$mte_hat[data$masters == 1], na.rm = TRUE)
cat(sprintf("Estimated ATT: %.4f\n", att_est))

atu_est <- mean(data$mte_hat[data$masters == 0], na.rm = TRUE)
cat(sprintf("Estimated ATU: %.4f\n", atu_est))

# ------------------------------------------------------------------------------
# Approach 2: MTE Estimation Using localIV Package (R equivalent of mtefe)
# ------------------------------------------------------------------------------

cat("\n--- Approach 2: localIV Package Estimation ---\n")
cat("(R equivalent of Stata's mtefe package)\n\n")

# Prepare data for localIV
# localIV requires: y (outcome), d (treatment), z (instrument), x (covariates)

# Create covariate matrix
X_matrix <- as.matrix(data[, X_controls])

# Run localIV with polynomial approximation
# The localIV function estimates MTE using local instrumental variables approach

cat("Running localIV estimation...\n")

# localIV estimation using FORMULA syntax
# NOTE: localIV uses formula interface, NOT separate Y, D, Z, X arguments
# - selection: treatment ~ covariates + instrument
# - outcome: outcome ~ covariates (NO instrument)
# - Use ace() function to extract ATE, ATT, ATU

# Build selection formula (includes instrument)
selection_formula <- as.formula(paste("masters ~ ga_funding_adj +", 
                                       paste(X_controls, collapse = " + ")))

# Build outcome formula (excludes instrument)
outcome_formula <- as.formula(paste("ln_salary ~", 
                                     paste(X_controls, collapse = " + ")))

cat("Selection formula:", deparse(selection_formula), "\n")
cat("Outcome formula:", deparse(outcome_formula), "\n\n")

localiv_result_2 <- tryCatch({
  mte(
    selection = selection_formula,
    outcome = outcome_formula,
    data = data,
    method = "localIV",
    bw = 0.25
  )
}, error = function(e) {
  cat("Error in localIV/mte:", e$message, "\n")
  NULL
})

# Extract and display results if successful
if (!is.null(localiv_result_2)) {
  cat("\nlocalIV Results:\n")
  print(summary(localiv_result_2$ps_model))
  
  # Extract treatment effect parameters using ace() function
  # Note: localIV uses ace() function, not direct object access
  localiv_ate_2 <- ace(localiv_result_2, "ate")
  localiv_att_2 <- ace(localiv_result_2, "att")
  localiv_atu_2 <- ace(localiv_result_2, "atu")
  
  cat(sprintf("\nlocalIV Treatment Effect Estimates:\n"))
  cat(sprintf("  ATE:  %.4f\n", localiv_ate_2))
  cat(sprintf("  ATT:  %.4f\n", localiv_att_2))
  cat(sprintf("  ATU:  %.4f\n", localiv_atu_2))
  
  # MPRTE calculation
  localiv_mprte <- tryCatch({
    ace(localiv_result_2, "mprte")
  }, error = function(e) NA)
  
  if (!is.na(localiv_mprte)) {
    cat(sprintf("  MPRTE: %.4f\n", localiv_mprte))
  }
} else {
  # Set defaults if localIV failed
  localiv_ate_2 <- NA
  localiv_att_2 <- NA
  localiv_atu_2 <- NA
}

# localIV estimation with normal selection model for comparison
cat("\nRunning localIV estimation (method = 'normal')...\n")

localiv_result_1 <- tryCatch({
  mte(
    selection = selection_formula,
    outcome = outcome_formula,
    data = data,
    method = "normal"            # Normal selection model (Heckman)
  )
}, error = function(e) {
  cat("Error in localIV/mte (normal):", e$message, "\n")
  NULL
})

if (!is.null(localiv_result_1)) {
  cat("\nlocalIV Results (Normal Selection Model):\n")
  print(summary(localiv_result_1$ps_model))
  
  # Extract using ace() function
  localiv_ate_1 <- ace(localiv_result_1, "ate")
  localiv_att_1 <- ace(localiv_result_1, "att")
  localiv_atu_1 <- ace(localiv_result_1, "atu")
  
  cat(sprintf("\nlocalIV (Normal) Treatment Effect Estimates:\n"))
  cat(sprintf("  ATE:  %.4f\n", localiv_ate_1))
  cat(sprintf("  ATT:  %.4f\n", localiv_att_1))
  cat(sprintf("  ATU:  %.4f\n", localiv_atu_1))
} else {
  localiv_ate_1 <- NA
  localiv_att_1 <- NA
  localiv_atu_1 <- NA
}

# Generate MTE curve plot from localIV if available
if (!is.null(localiv_result_2)) {
  cat("\nGenerating localIV MTE plot...\n")
  
  # Create MTE curve data from localIV using mte_at() function
  u_grid <- seq(0.05, 0.95, length.out = 19)
  
  # Evaluate MTE at different values of u
  mte_values <- tryCatch({
    mte_at(u_grid, model = localiv_result_2)
  }, error = function(e) {
    cat("Error generating MTE values:", e$message, "\n")
    NULL
  })
  
  if (!is.null(mte_values)) {
    # Create plot
    mte_df <- data.frame(u = u_grid, mte = mte_values)
    
    p_localiv_mte <- ggplot(mte_df, aes(x = u, y = mte)) +
      geom_line(color = "darkred", linewidth = 1.2) +
      geom_point(color = "darkred", size = 2) +
      labs(
        title = "MTE Curve from localIV Package",
        subtitle = "R equivalent of Stata's mtefe",
        x = "u (Unobserved Resistance to Treatment)",
        y = "Marginal Treatment Effect"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
      )
    
    print(p_localiv_mte)
    ggsave("mte_curve_localiv.png", p_localiv_mte, width = 10, height = 6, dpi = 300)
  }
}

# Comparison: Manual Polynomial vs localIV
cat("\n--- Comparison: Manual Polynomial vs. localIV ---\n")
cat("Parameter     Manual (Cubic)    localIV (semipar)  localIV (normal)\n")
cat("===================================================================\n")
cat(sprintf("ATE           %.4f            %.4f             %.4f\n", 
            ate_est_cubic, 
            ifelse(is.na(localiv_ate_2), NA, localiv_ate_2),
            ifelse(is.na(localiv_ate_1), NA, localiv_ate_1)))
cat(sprintf("ATT           %.4f            %.4f             %.4f\n", 
            att_est, 
            ifelse(is.na(localiv_att_2), NA, localiv_att_2),
            ifelse(is.na(localiv_att_1), NA, localiv_att_1)))
cat(sprintf("ATU           %.4f            %.4f             %.4f\n", 
            atu_est, 
            ifelse(is.na(localiv_atu_2), NA, localiv_atu_2),
            ifelse(is.na(localiv_atu_1), NA, localiv_atu_1)))

cat("\nNote: Differences between methods arise from:\n")
cat("  - Different polynomial/smoothing specifications\n")
cat("  - Different estimation approaches (OLS vs local IV vs normal selection)\n")
cat("  - Different weighting schemes for ATT/ATU integration\n")
cat("  - localIV uses formula interface: mte(selection=..., outcome=..., data=...)\n")

# ------------------------------------------------------------------------------
# Approach 3: Heckman Selection Model
# ------------------------------------------------------------------------------

cat("\n--- Approach 3: Heckman Selection Model ---\n")

# Note: In R, we use sampleSelection package
# The Heckman model treats masters as the selection variable

# Prepare formulas for selection model
selection_formula <- as.formula(paste("masters ~ ga_funding_adj +", 
                                       paste(X_controls, collapse = " + ")))
outcome_formula <- as.formula(paste("ln_salary ~", 
                                     paste(X_controls, collapse = " + ")))

# Two-step Heckman estimation
heckman_2step <- heckit(selection_formula, outcome_formula, data = data, method = "2step")
print(summary(heckman_2step))

heck2_rho <- heckman_2step$rho
heck2_sigma <- heckman_2step$sigma
heck2_lambda <- heck2_rho * heck2_sigma

cat("\nHeckman 2-step results:\n")
cat(sprintf("  lambda (selection correction): %.4f\n", heck2_lambda))
cat(sprintf("  rho (correlation): %.4f\n", heck2_rho))
cat(sprintf("  sigma: %.4f\n", heck2_sigma))

# Maximum likelihood estimation
heckman_ml <- heckit(selection_formula, outcome_formula, data = data, method = "ml")
print(summary(heckman_ml))

heck_ml_rho <- heckman_ml$rho
heck_ml_sigma <- heckman_ml$sigma
heck_ml_lambda <- heck_ml_rho * heck_ml_sigma

cat("\nHeckman ML results:\n")
cat(sprintf("  lambda (selection correction): %.4f\n", heck_ml_lambda))
cat(sprintf("  rho (correlation): %.4f\n", heck_ml_rho))
cat(sprintf("  sigma: %.4f\n", heck_ml_sigma))

cat("\nIMPORTANT: In Heckman selection models:\n")
cat("  - 'masters' appears in the SELECTION equation, not outcome equation\n")
cat("  - Lambda (inverse Mills ratio) corrects for selection bias\n")
cat("  - Lambda is NOT the treatment effect\n")
cat("  - To get treatment effects, use MTE framework\n")

################################################################################
# SECTION 7: Results Comparison
################################################################################

cat("\n==============================================\n")
cat("RESULTS COMPARISON\n")
cat("==============================================\n")

cat("\nESTIMATED PARAMETERS:\n")
cat(sprintf("  Naive OLS:           %.4f (likely biased by selection)\n", ols_est))
cat(sprintf("  IV/LATE:             %.4f (complier effect)\n", iv_est))
cat(sprintf("  MTE-based ATE:       %.4f (manual polynomial)\n", ate_est_cubic))
cat(sprintf("  MTE-based ATT:       %.4f (manual polynomial)\n", att_est))
cat(sprintf("  MTE-based ATU:       %.4f (manual polynomial)\n", atu_est))

if (!is.na(localiv_ate_2)) {
  cat(sprintf("  localIV ATE:         %.4f (localIV package)\n", localiv_ate_2))
  cat(sprintf("  localIV ATT:         %.4f (localIV package)\n", localiv_att_2))
  cat(sprintf("  localIV ATU:         %.4f (localIV package)\n", localiv_atu_2))
}

# Selection pattern check
cat("\nSelection Pattern Check:\n")
if (att_est > ate_est_cubic & ate_est_cubic > atu_est) {
  cat(sprintf("  ATT (%.4f) > ATE (%.4f) > ATU (%.4f)\n", att_est, ate_est_cubic, atu_est))
  cat("  Confirms POSITIVE SELECTION on gains\n")
} else if (att_est < ate_est_cubic & ate_est_cubic < atu_est) {
  cat(sprintf("  ATT (%.4f) < ATE (%.4f) < ATU (%.4f)\n", att_est, ate_est_cubic, atu_est))
  cat("  Indicates NEGATIVE SELECTION on gains\n")
} else {
  cat(sprintf("  ATT = %.4f, ATE = %.4f, ATU = %.4f\n", att_est, ate_est_cubic, atu_est))
  cat("  Mixed selection pattern\n")
}

# Bias analysis
ols_bias <- (ols_est - ate_est_cubic) / ate_est_cubic * 100

cat("\nOLS BIAS ANALYSIS:\n")
cat(sprintf("  OLS coefficient:     %.4f\n", ols_est))
cat(sprintf("  MTE-based ATE:       %.4f\n", ate_est_cubic))
cat(sprintf("  Percent difference:  %.1f%%\n", ols_bias))

################################################################################
# SECTION 8: MTE Visualization
################################################################################

cat("\n==============================================\n")
cat("MTE VISUALIZATION\n")
cat("==============================================\n")

# Calculate MTE at grid of u values
mte_curve_data <- data.frame(
  u = seq(0.01, 1, length.out = 100)
)
mte_curve_data$mte_est <- b0 + b1*mte_curve_data$u + b2*mte_curve_data$u^2 + b3*mte_curve_data$u^3

# Plot MTE curve
p_mte_curve <- ggplot(mte_curve_data, aes(x = u, y = mte_est)) +
  geom_line(color = "navy", linewidth = 1.2) +
  labs(
    title = "Estimated MTE Curve (Manual Polynomial)",
    subtitle = "Master's Degree Effect on Log Salary",
    x = "u (Unobserved Resistance to Treatment)",
    y = "Marginal Treatment Effect",
    caption = "Declining MTE indicates positive selection on gains\nATT > ATE > ATU when MTE is decreasing in u"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(p_mte_curve)
ggsave("mte_curve.png", p_mte_curve, width = 10, height = 6, dpi = 300)

# MTE by propensity score decile
data$p_decile <- ntile(data$phat, 10)

mte_by_decile <- data %>%
  group_by(p_decile) %>%
  summarise(
    mte_mean = mean(mte_hat, na.rm = TRUE),
    mte_sd = sd(mte_hat, na.rm = TRUE),
    n = n()
  )

cat("\nEstimated MTE by Propensity Score Decile:\n")
print(mte_by_decile)

p_mte_decile <- ggplot(mte_by_decile, aes(x = p_decile, y = mte_mean)) +
  geom_point(color = "navy", size = 4) +
  geom_line(color = "navy", linewidth = 1) +
  labs(
    title = "Estimated MTE by Propensity Score Decile",
    subtitle = "Evidence of Treatment Effect Heterogeneity",
    x = "Propensity Score Decile",
    y = "Mean Estimated MTE",
    caption = "Increasing MTE with lower decile indicates positive selection"
  ) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(p_mte_decile)
ggsave("mte_by_decile.png", p_mte_decile, width = 10, height = 6, dpi = 300)

################################################################################
# SECTION 9: Basic Policy Simulation (PRTE)
################################################################################

cat("\n==============================================\n")
cat("POLICY SIMULATION: INCREASE GA FUNDING\n")
cat("==============================================\n")

ga_current <- mean(data$ga_funding_adj, na.rm = TRUE)
ga_new <- ga_current * 1.2

cat(sprintf("Current mean GA funding: $%.2fk\n", ga_current))
cat(sprintf("Proposed GA funding (20%% increase): $%.2fk\n", ga_new))

# Calculate new propensity scores under policy
data$p_new <- pnorm(data$z_index + ga_coef * (ga_new - data$ga_funding_adj))
data$delta_p <- data$p_new - data$phat

delta_p_mean <- mean(data$delta_p, na.rm = TRUE)
cat(sprintf("Average increase in Pr(Master's): %.4f\n", delta_p_mean))

# PRTE calculation
data$complier_weight <- ifelse(data$delta_p > 0, data$delta_p / delta_p_mean, NA)
prte_20pct <- weighted.mean(data$mte_hat[data$delta_p > 0], 
                             data$complier_weight[data$delta_p > 0], 
                             na.rm = TRUE)
cat(sprintf("Approximate PRTE (20%% GA increase): %.4f\n", prte_20pct))

# Clean up
data$p_new <- NULL
data$delta_p <- NULL
data$complier_weight <- NULL

################################################################################
# SECTION 10: MARGINAL POLICY-RELEVANT TREATMENT EFFECTS (MPRTE)
################################################################################

cat("\n==============================================\n")
cat("MARGINAL POLICY-RELEVANT TREATMENT EFFECTS\n")
cat("==============================================\n")

# ------------------------------------------------------------------------------
# Scenario 1: Uniform GA Funding Increase
# ------------------------------------------------------------------------------

cat("\n--- Scenario 1: Uniform $1,000 GA Funding Increase ---\n")

data$p_new_unif <- pnorm(data$z_index + ga_coef * 1)
data$delta_p_unif <- data$p_new_unif - data$phat
data$response_unif <- dnorm(qnorm(data$phat)) * ga_coef
data$mte_weighted_unif <- data$mte_hat * data$response_unif

mprte_unif_num <- sum(data$mte_weighted_unif, na.rm = TRUE)
mprte_unif_den <- sum(data$response_unif, na.rm = TRUE)
mprte_unif <- mprte_unif_num / mprte_unif_den

cat(sprintf("MPRTE (uniform $1k increase): %.4f\n", mprte_unif))

marginal_region_mte <- mean(data$mte_hat[data$phat > 0.25 & data$phat < 0.40], na.rm = TRUE)
cat(sprintf("Average MTE for marginal region (p=0.25-0.40): %.4f\n", marginal_region_mte))

prte_discrete <- weighted.mean(data$mte_hat[data$delta_p_unif > 0],
                                data$delta_p_unif[data$delta_p_unif > 0],
                                na.rm = TRUE)
cat(sprintf("PRTE (discrete $1k increase): %.4f\n", prte_discrete))

# Clean up
data$p_new_unif <- NULL
data$delta_p_unif <- NULL
data$response_unif <- NULL
data$mte_weighted_unif <- NULL

# ------------------------------------------------------------------------------
# Scenario 2: Targeted Funding for Low-Income Students
# ------------------------------------------------------------------------------

cat("\n--- Scenario 2: Targeted $2,000 Increase for Low-Income (Q1-Q2) ---\n")

data$targeted_lowinc <- as.numeric(data$parent_income_q <= 2)
data$p_new_lowinc <- pnorm(data$z_index + ga_coef * 2 * data$targeted_lowinc)
data$delta_p_lowinc <- data$p_new_lowinc - data$phat
data$response_lowinc <- dnorm(qnorm(data$phat)) * ga_coef * 2 * data$targeted_lowinc
data$mte_weighted_lowinc <- data$mte_hat * data$response_lowinc

mprte_lowinc_num <- sum(data$mte_weighted_lowinc[data$targeted_lowinc == 1], na.rm = TRUE)
mprte_lowinc_den <- sum(data$response_lowinc[data$targeted_lowinc == 1], na.rm = TRUE)
mprte_lowinc <- mprte_lowinc_num / mprte_lowinc_den

cat(sprintf("MPRTE (targeted low-income): %.4f\n", mprte_lowinc))

# Clean up
data$targeted_lowinc <- NULL
data$p_new_lowinc <- NULL
data$delta_p_lowinc <- NULL
data$response_lowinc <- NULL
data$mte_weighted_lowinc <- NULL

# ------------------------------------------------------------------------------
# Scenario 3: Enhanced STEM GA Funding
# ------------------------------------------------------------------------------

cat("\n--- Scenario 3: Enhanced $3,000 STEM GA Funding ---\n")

data$p_new_stem <- pnorm(data$z_index + ga_coef * 3 * data$stem_major)
data$delta_p_stem <- data$p_new_stem - data$phat
data$response_stem <- dnorm(qnorm(data$phat)) * ga_coef * 3 * data$stem_major
data$mte_weighted_stem <- data$mte_hat * data$response_stem

mprte_stem_num <- sum(data$mte_weighted_stem[data$stem_major == 1], na.rm = TRUE)
mprte_stem_den <- sum(data$response_stem[data$stem_major == 1], na.rm = TRUE)
mprte_stem <- mprte_stem_num / mprte_stem_den

cat(sprintf("MPRTE (STEM enhancement): %.4f\n", mprte_stem))

# Clean up
data$p_new_stem <- NULL
data$delta_p_stem <- NULL
data$response_stem <- NULL
data$mte_weighted_stem <- NULL

# ------------------------------------------------------------------------------
# Scenario 4: Education Major Support (Teacher Pipeline)
# ------------------------------------------------------------------------------

cat("\n--- Scenario 4: Education Major GA Support ($2,500) ---\n")

data$p_new_ed <- pnorm(data$z_index + ga_coef * 2.5 * data$ed_major)
data$delta_p_ed <- data$p_new_ed - data$phat
data$response_ed <- dnorm(qnorm(data$phat)) * ga_coef * 2.5 * data$ed_major
data$mte_weighted_ed <- data$mte_hat * data$response_ed

mprte_ed_num <- sum(data$mte_weighted_ed[data$ed_major == 1], na.rm = TRUE)
mprte_ed_den <- sum(data$response_ed[data$ed_major == 1], na.rm = TRUE)
mprte_ed <- mprte_ed_num / mprte_ed_den

cat(sprintf("MPRTE (education major support): %.4f\n", mprte_ed))

ed_mean_phat <- mean(data$phat[data$ed_major == 1], na.rm = TRUE)
cat(sprintf("Mean propensity for ed majors: %.4f\n", ed_mean_phat))

# Clean up
data$p_new_ed <- NULL
data$delta_p_ed <- NULL
data$response_ed <- NULL
data$mte_weighted_ed <- NULL

################################################################################
# SECTION 11: MPRTE BY POLICY INTENSITY
################################################################################

cat("\n==============================================\n")
cat("MPRTE BY POLICY INTENSITY\n")
cat("==============================================\n")

# Create grid for policy intensity analysis
p_baseline <- mean(data$phat, na.rm = TRUE)

mprte_intensity_data <- data.frame(
  ga_increase = seq(0.5, 10, by = 0.5)
)
mprte_intensity_data$p_margin <- p_baseline + mprte_intensity_data$ga_increase * 0.015
mprte_intensity_data$mprte_approx <- b0 + b1*mprte_intensity_data$p_margin + 
                                      b2*mprte_intensity_data$p_margin^2 + 
                                      b3*mprte_intensity_data$p_margin^3

print(mprte_intensity_data)

p_mprte_intensity <- ggplot(mprte_intensity_data, aes(x = ga_increase, y = mprte_approx)) +
  geom_line(color = "navy", linewidth = 1.2) +
  labs(
    title = "MPRTE by Policy Intensity",
    subtitle = "Marginal returns to GA funding expansion",
    x = "GA Funding Increase ($1000s)",
    y = "MPRTE",
    caption = "MPRTE pattern depends on selection mechanism\nand where policy operates on MTE curve"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(p_mprte_intensity)
ggsave("mprte_by_intensity.png", p_mprte_intensity, width = 10, height = 6, dpi = 300)

################################################################################
# SECTION 12: COMPARING TREATMENT EFFECT PARAMETERS
################################################################################

cat("\n==============================================\n")
cat("COMPARISON OF TREATMENT EFFECT PARAMETERS\n")
cat("==============================================\n")

cat("\nParameter          Manual       localIV\n")
cat("===========================================\n")
cat(sprintf("ATE                %.4f      %.4f\n", ate_est_cubic, 
            ifelse(is.na(localiv_ate_2), NA, localiv_ate_2)))
cat(sprintf("ATT                %.4f      %.4f\n", att_est, 
            ifelse(is.na(localiv_att_2), NA, localiv_att_2)))
cat(sprintf("ATU                %.4f      %.4f\n", atu_est, 
            ifelse(is.na(localiv_atu_2), NA, localiv_atu_2)))
cat(sprintf("LATE (IV)          %.4f\n", iv_est))
cat("-------------------------------------------\n")
cat(sprintf("MPRTE (uniform)              %.4f\n", mprte_unif))
cat(sprintf("MPRTE (low-income)           %.4f\n", mprte_lowinc))
cat(sprintf("MPRTE (STEM)                 %.4f\n", mprte_stem))
cat(sprintf("MPRTE (education)            %.4f\n", mprte_ed))

cat("\nKey insight: Different parameters answer different policy questions\n")
cat("  - ATE: Effect of universal mandatory policy\n")
cat("  - ATT: Effect for current participants (selection already occurred)\n")
cat("  - ATU: Effect if we could induce ALL non-participants\n")
cat("  - LATE: Effect for those induced by instrument variation\n")
cat("  - MPRTE: Effect for those at the MARGIN of a specific policy\n")

################################################################################
# SECTION 13: MPRTE VISUALIZATION
################################################################################

cat("\n--- Generating MPRTE Visualizations ---\n")

# MTE curve with policy-relevant regions
mte_curve_data$region_lowinc <- mte_curve_data$u >= 0.10 & mte_curve_data$u <= 0.25
mte_curve_data$region_uniform <- mte_curve_data$u >= 0.25 & mte_curve_data$u <= 0.40

p_mte_regions <- ggplot(mte_curve_data, aes(x = u, y = mte_est)) +
  geom_ribbon(data = subset(mte_curve_data, region_lowinc),
              aes(ymin = min(mte_est) - 0.5, ymax = mte_est), 
              fill = "darkred", alpha = 0.3) +
  geom_ribbon(data = subset(mte_curve_data, region_uniform),
              aes(ymin = min(mte_est) - 0.5, ymax = mte_est), 
              fill = "navy", alpha = 0.3) +
  geom_line(color = "navy", linewidth = 1.2) +
  labs(
    title = "MTE Curve with Policy-Relevant Regions",
    subtitle = "Different policies target different margins",
    x = "u (Unobserved Resistance to Treatment)",
    y = "Marginal Treatment Effect",
    caption = "MPRTE = MTE evaluated at the policy-specific margin"
  ) +
  annotate("text", x = 0.175, y = max(mte_curve_data$mte_est) * 0.9, 
           label = "Low-income\nmargin", color = "darkred", size = 3) +
  annotate("text", x = 0.325, y = max(mte_curve_data$mte_est) * 0.7, 
           label = "Uniform policy\nmargin", color = "navy", size = 3) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(p_mte_regions)
ggsave("mte_policy_regions.png", p_mte_regions, width = 10, height = 6, dpi = 300)

# MTE by propensity score with distribution
data$p_bin <- floor(data$phat * 20) / 20

mte_by_pbin <- data %>%
  group_by(p_bin) %>%
  summarise(
    mean_mte = mean(mte_hat, na.rm = TRUE),
    n_bin = n()
  )

p_mte_propensity <- ggplot(mte_by_pbin, aes(x = p_bin)) +
  geom_col(aes(y = n_bin / max(n_bin) * max(mean_mte)), 
           fill = "gray70", alpha = 0.5, width = 0.04) +
  geom_point(aes(y = mean_mte), color = "navy", size = 3, shape = 18) +
  geom_line(aes(y = mean_mte), color = "navy", linewidth = 1) +
  scale_y_continuous(
    name = "Estimated MTE",
    sec.axis = sec_axis(~ . / max(mte_by_pbin$mean_mte) * max(mte_by_pbin$n_bin), 
                        name = "Frequency")
  ) +
  labs(
    title = "MTE by Propensity Score",
    subtitle = "MPRTE depends on where policy operates",
    x = "Propensity Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(p_mte_propensity)
ggsave("mte_by_propensity.png", p_mte_propensity, width = 10, height = 6, dpi = 300)

################################################################################
# SECTION 14: POLICY COST-BENEFIT ANALYSIS
################################################################################

cat("\n==============================================\n")
cat("POLICY COST-BENEFIT ILLUSTRATION\n")
cat("==============================================\n")

cost_per_degree <- 50000
career_years <- 30
discount_rate <- 0.03
base_salary <- 47000

pv_factor <- (1 - (1 + discount_rate)^(-career_years)) / discount_rate
cat(sprintf("Present value factor (30 years, 3%%): %.2f\n", pv_factor))

cat("\nPolicy               MPRTE    Annual Gain   PV Gain    B/C Ratio\n")
cat("==================================================================\n")

scenarios <- list(
  list(name = "Uniform", mprte = mprte_unif),
  list(name = "Low-income", mprte = mprte_lowinc),
  list(name = "STEM", mprte = mprte_stem),
  list(name = "Education", mprte = mprte_ed)
)

for (scen in scenarios) {
  annual_gain <- base_salary * (exp(scen$mprte) - 1)
  pv_gain <- annual_gain * pv_factor
  bc_ratio <- pv_gain / cost_per_degree
  
  cat(sprintf("%-20s %.4f   $%6.0f    $%8.0f   %5.2f\n", 
              scen$name, scen$mprte, annual_gain, pv_gain, bc_ratio))
}

cat("\nNote: B/C ratio > 1 suggests policy expansion is beneficial\n")
cat("      These calculations are illustrative only (synthetic data)\n")
cat("      Real analysis would require actual cost and outcome data\n")

################################################################################
# SECTION 15: Save Results
################################################################################

# Save analysis dataset
saveRDS(data, "bb_mte_analysis.rds")

# Export summary statistics by field
mte_summary <- data %>%
  group_by(stem_major, ed_major) %>%
  summarise(
    masters = mean(masters, na.rm = TRUE),
    ln_salary = mean(ln_salary, na.rm = TRUE),
    phat = mean(phat, na.rm = TRUE),
    mte_hat = mean(mte_hat, na.rm = TRUE),
    sd_mte = sd(mte_hat, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

write.csv(mte_summary, "mte_summary_by_field.csv", row.names = FALSE)

################################################################################
# SECTION 16: FINAL SUMMARY
################################################################################

cat("\n==============================================\n")
cat("ANALYSIS COMPLETE\n")
cat("==============================================\n")
cat("Key findings:\n")
cat(sprintf("  1. Treatment rate: %.3f\n", treat_rate))
cat(sprintf("  2. OLS estimate: %.4f (likely biased)\n", ols_est))
cat(sprintf("  3. IV/LATE estimate: %.4f\n", iv_est))
cat(sprintf("  4. MTE-based ATE: %.4f (manual polynomial)\n", ate_est_cubic))
cat(sprintf("  5. MTE-based ATT: %.4f (manual polynomial)\n", att_est))
cat(sprintf("  6. MTE-based ATU: %.4f (manual polynomial)\n", atu_est))

if (!is.na(localiv_ate_2)) {
  cat(sprintf("  7. localIV ATE: %.4f (localIV package)\n", localiv_ate_2))
  cat(sprintf("  8. localIV ATT: %.4f (localIV package)\n", localiv_att_2))
  cat(sprintf("  9. localIV ATU: %.4f (localIV package)\n", localiv_atu_2))
}

cat(sprintf("  10. First-stage F = %.1f (strong instrument)\n", first_stage_F))
cat("  11. MPRTE varies by policy scenario\n")

cat("\nMPRTE SUMMARY:\n")
cat(sprintf("  - Uniform policy:    %.4f\n", mprte_unif))
cat(sprintf("  - Low-income target: %.4f\n", mprte_lowinc))
cat(sprintf("  - STEM enhancement:  %.4f\n", mprte_stem))
cat(sprintf("  - Education support: %.4f\n", mprte_ed))

cat("\nKEY INSIGHTS:\n")
cat("  - PRTE averages over all compliers induced by policy change\n")
cat("  - MPRTE is the effect at the margin (infinitesimal change)\n")
cat("  - Selection pattern determines how returns change with expansion\n")
cat("  - Cost-effectiveness depends on WHERE policy operates on MTE curve\n")

cat("\nFiles saved:\n")
cat("  - bb_mte_analysis.rds (analysis dataset)\n")
cat("  - mte_summary_by_field.csv (summary statistics)\n")
cat("  - mte_curve.png (MTE curve - manual polynomial)\n")
cat("  - mte_by_decile.png (MTE by propensity decile)\n")
cat("  - mprte_by_intensity.png (MPRTE by policy intensity)\n")
cat("  - mte_policy_regions.png (MTE with policy regions)\n")
cat("  - mte_by_propensity.png (MTE by propensity score)\n")

if (!is.na(localiv_ate_2)) {
  cat("  - mte_curve_localiv.png (MTE curve from localIV)\n")
}

cat("\n==============================================\n")
cat("END OF MTE/MPRTE ANALYSIS\n")
cat("==============================================\n")

cat("\nIMPORTANT NOTE:\n")
cat("Because a synthetic dataset is used in this application, the results\n")
cat("are intended to illustrate MTE/MPRTE estimation methods and should\n")
cat("not be viewed as having policy implications.\n")

################################################################################
# APPENDIX: Create Summary Results Table
################################################################################

# Create comprehensive results table
results_table <- data.frame(
  Parameter = c("OLS", "IV/LATE", "ATE (manual)", "ATT (manual)", "ATU (manual)",
                "ATE (localIV)", "ATT (localIV)", "ATU (localIV)",
                "MPRTE (uniform)", "MPRTE (low-income)", "MPRTE (STEM)", "MPRTE (education)"),
  Estimate = c(ols_est, iv_est, ate_est_cubic, att_est, atu_est,
               ifelse(is.na(localiv_ate_2), NA, localiv_ate_2),
               ifelse(is.na(localiv_att_2), NA, localiv_att_2),
               ifelse(is.na(localiv_atu_2), NA, localiv_atu_2),
               mprte_unif, mprte_lowinc, mprte_stem, mprte_ed),
  Interpretation = c(
    "Biased by positive selection",
    "Effect for compliers",
    "Population average effect (manual)",
    "Effect for treated (manual)",
    "Effect for untreated (manual)",
    "Population average effect (localIV)",
    "Effect for treated (localIV)",
    "Effect for untreated (localIV)",
    "Marginal effect, uniform policy",
    "Marginal effect, low-income target",
    "Marginal effect, STEM enhancement",
    "Marginal effect, education support"
  )
)

cat("\n==============================================\n")
cat("SUMMARY RESULTS TABLE\n")
cat("==============================================\n")
print(results_table)

# Save results table
write.csv(results_table, "mte_results_summary.csv", row.names = FALSE)

################################################################################
# APPENDIX: Command Mapping - Stata mtefe vs R localIV
################################################################################

cat("\n==============================================\n")
cat("COMMAND MAPPING: Stata mtefe vs R localIV\n")
cat("==============================================\n")

cat("
+----------------------------------+------------------------------------------+
| Stata (mtefe)                    | R (localIV package)                      |
+----------------------------------+------------------------------------------+
| mtefe y x (d = z), pol(2)        | mte(selection=d~x+z, outcome=y~x, data)  |
| _b[effects:ate]                  | ace(mod, 'ate')                          |
| _b[effects:att]                  | ace(mod, 'att')                          |
| _b[effects:atut]                 | ace(mod, 'atu')                          |
| _b[effects:late]                 | (use ivreg for LATE)                     |
| mtefe ..., bootreps(50)          | (bootstrap not built-in; use boot pkg)   |
| mtefe ..., mte                   | mte_at(u_grid, model=mod)                |
| MPRTE                            | ace(mod, 'mprte', policy=...)            |
+----------------------------------+------------------------------------------+

IMPORTANT: 
- Package name is 'localIV' but main function is 'mte()'
- Uses FORMULA interface: selection = d ~ x + z, outcome = y ~ x
- Instrument goes in selection formula, NOT outcome formula
- Extract effects using ace() function, not object$ATE
- method = 'localIV' for semiparametric, 'normal' for Heckman

Notes:
- localIV uses local polynomial regression; mtefe uses global polynomial
- Bootstrap inference requires additional coding in R
- Both packages estimate MTE curve and integrate for ATE/ATT/ATU
")

################################################################################
# END OF SCRIPT
################################################################################
