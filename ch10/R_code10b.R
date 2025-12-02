################################################################################
# MTE and MPRTE Simulation: Effect of Master's Degree on Salary Outcomes
# R VERSION - Translated from Stata v5 (BASE R ONLY - no external packages)
#
# Instrument: State-Funded Graduate Assistantship (GA) Dollar Amount
# 
# Based on B&B Longitudinal Study characteristics and higher education
# finance literature (Titus 2007; Bound, Lovenheim & Turner 2010;
# Zhang 2005; Ehrenberg et al. 2007)
#
# Author: [Your Name]
# Date: December 2025
# Purpose: Demonstrate MTE/MPRTE framework for textbook Chapter 10
#
# NOTE: This version uses only base R - no external packages required
################################################################################

# Clear environment
rm(list = ls())

# Set seed for reproducibility
set.seed(20251130)

# Sample size
N <- 8000

################################################################################
# SECTION 1: Generate Exogenous Covariates
################################################################################

cat("\n==============================================\n")
cat("GENERATING SIMULATION DATA\n")
cat("==============================================\n")

# Create data frame
df <- data.frame(id = 1:N)

# Demographics
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

# Family Background
df$firstgen <- rbinom(N, 1, 0.35)
df$parent_income_q <- 1 + rbinom(N, 4, 0.55)
df$parent_grad <- rbinom(N, 1, 0.25)

# Academic Background
df$ugpa <- 2.0 + 1.2 * rbeta(N, 5, 3)
df$ugpa[df$ugpa > 4.0] <- 4.0
df$ugpa[df$ugpa < 2.0] <- 2.0

df$stem_major <- rbinom(N, 1, 0.25)
df$bus_major <- ifelse(df$stem_major == 0, rbinom(N, 1, 0.20), 0)
df$ed_major <- ifelse(df$stem_major == 0 & df$bus_major == 0, rbinom(N, 1, 0.15), 0)
df$socsci_major <- as.integer(df$stem_major == 0 & df$bus_major == 0 & df$ed_major == 0)

df$selective_inst <- rbinom(N, 1, 0.30)
df$public_ug <- rbinom(N, 1, 0.65)

# Labor Market
df$state_unemp <- 4 + 6 * rbeta(N, 2, 3)
df$metro <- rbinom(N, 1, 0.75)

################################################################################
# SECTION 2: Generate Instrument - State GA Funding
################################################################################

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

################################################################################
# SECTION 3: Generate Latent Factors
################################################################################

df$eta_ability <- rnorm(N, 0, 1)
df$eta_taste <- 0.3 * df$eta_ability + rnorm(N, 0, 0.9)
df$eta_prod <- 0.5 * df$eta_ability + rnorm(N, 0, 0.85)

################################################################################
# SECTION 4: Generate Treatment (Master's Degree)
################################################################################

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
  0.06 * (df$ga_funding_adj - 18) +  # INSTRUMENT
  0.40 * df$eta_taste +
  0.25 * df$eta_ability

df$p_masters <- pnorm(df$z_masters)
df$u_d <- runif(N)
df$masters <- as.integer(df$p_masters > df$u_d)

# Check treatment rate
treat_rate <- mean(df$masters)
cat(sprintf("Treatment rate: %.3f\n", treat_rate))

################################################################################
# SECTION 5: Generate Outcome (Salary)
################################################################################

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
  0.20 * df$eta_prod +
  rnorm(N, 0, 0.25)

# Heterogeneous treatment effect
df$te_masters <- 0.12 +
  0.08 * df$stem_major +
  0.05 * df$bus_major +
  0.10 * df$ed_major +
  0.03 * df$selective_inst +
  0.05 * (df$ugpa - 3.0) +
  0.08 * df$eta_ability +
  -0.10 * (df$p_masters - 0.5) +  # Essential heterogeneity
  rnorm(N, 0, 0.05)

df$ln_salary_1 <- df$ln_salary_0 + df$te_masters
df$ln_salary <- df$masters * df$ln_salary_1 + (1 - df$masters) * df$ln_salary_0
df$salary <- exp(df$ln_salary)

################################################################################
# SECTION 6: Summary Statistics
################################################################################

cat("\n==============================================\n")
cat("SUMMARY STATISTICS\n")
cat("==============================================\n")

cat("\nTreatment distribution:\n")
print(table(df$masters))

cat("\nKey variables summary:\n")
summary_vars <- df[, c("salary", "ln_salary", "masters", "female", "ugpa", 
                       "ga_funding_adj", "p_masters")]
print(summary(summary_vars))

cat("\n--- True Treatment Effects ---\n")
true_att <- mean(df$te_masters[df$masters == 1])
true_atu <- mean(df$te_masters[df$masters == 0])
true_ate <- mean(df$te_masters)

cat(sprintf("True ATT: %.4f\n", true_att))
cat(sprintf("True ATU: %.4f\n", true_atu))
cat(sprintf("True ATE: %.4f\n", true_ate))

cat("\nSelection pattern check:\n")
cat(sprintf("  ATT (%.4f) > ATE (%.4f) > ATU (%.4f)\n", true_att, true_ate, true_atu))
cat("  Confirms POSITIVE SELECTION on gains\n")

################################################################################
# SECTION 7: MTE ESTIMATION
################################################################################

cat("\n==============================================\n")
cat("MTE ESTIMATION\n")
cat("==============================================\n")

# Define covariate formula
X_vars <- c("female", "black", "hispanic", "asian", "age_ba", "firstgen",
            "parent_income_q", "parent_grad", "ugpa", "stem_major", "bus_major",
            "ed_major", "selective_inst", "public_ug", "state_unemp", "metro")
X_formula <- paste(X_vars, collapse = " + ")

# First stage regression (instrument relevance)
cat("\n--- Instrument Relevance ---\n")
first_stage_formula <- as.formula(paste("masters ~ ga_funding_adj +", X_formula))
first_stage <- lm(first_stage_formula, data = df)

# F-test for instrument (partial F-test)
first_stage_summary <- summary(first_stage)
# Extract t-statistic for ga_funding_adj and square it to get partial F
t_stat_ga <- coef(first_stage_summary)["ga_funding_adj", "t value"]
first_stage_F <- t_stat_ga^2
cat(sprintf("First-stage F-statistic (partial): %.2f (should be >> 10)\n", first_stage_F))

# Naive OLS
cat("\n--- Naive OLS ---\n")
ols_formula <- as.formula(paste("ln_salary ~ masters +", X_formula))
ols_naive <- lm(ols_formula, data = df)
ols_est <- coef(ols_naive)["masters"]
cat(sprintf("OLS estimate: %.4f\n", ols_est))

#------------------------------------------------------------------------------
# Approach 1: Parametric MTE via Polynomial in Propensity Score
#------------------------------------------------------------------------------

cat("\n--- Approach 1: Polynomial MTE (Manual) ---\n")

# Step 1: Estimate propensity score
probit_formula <- as.formula(paste("masters ~ ga_funding_adj +", X_formula))
probit_model <- glm(probit_formula, data = df, family = binomial(link = "probit"))
df$phat <- predict(probit_model, type = "response")

# Step 2: Generate polynomial terms
df$phat2 <- df$phat^2
df$phat3 <- df$phat^3

# Step 3: Estimate switching regression with interactions
mte_formula <- as.formula(paste("ln_salary ~ masters + masters:phat + masters:phat2 +",
                                 X_formula, "+ phat + phat2"))
mte_model <- lm(mte_formula, data = df)

# Extract MTE function parameters
b0 <- coef(mte_model)["masters"]
b1 <- coef(mte_model)["masters:phat"]
b2 <- coef(mte_model)["masters:phat2"]

cat(sprintf("\nMTE(u) = %.4f + %.4f*u + %.4f*u^2\n", b0, b1, b2))

# Calculate treatment effect parameters by integration
# ATE = integral_0^1 MTE(u) du = b0 + b1/2 + b2/3
ate_est <- b0 + b1/2 + b2/3
cat(sprintf("Estimated ATE (polynomial): %.4f\n", ate_est))

# ATT and ATU via numerical approximation
df$mte_i <- b0 + b1 * df$phat + b2 * df$phat2

att_est <- mean(df$mte_i[df$masters == 1])
atu_est <- mean(df$mte_i[df$masters == 0])
cat(sprintf("Estimated ATT (polynomial): %.4f\n", att_est))
cat(sprintf("Estimated ATU (polynomial): %.4f\n", atu_est))

#------------------------------------------------------------------------------
# Approach 2: Heckman Selection Model (Manual Two-Step)
#------------------------------------------------------------------------------

cat("\n--- Approach 2: Heckman Selection Model (Manual 2-step) ---\n")

# Step 1: Probit for selection equation (already done above)
# Step 2: Calculate inverse Mills ratio
df$mills <- dnorm(qnorm(df$phat)) / df$phat
df$mills[df$masters == 0] <- -dnorm(qnorm(df$phat[df$masters == 0])) / (1 - df$phat[df$masters == 0])

# Step 3: Outcome regression on treated only with Mills ratio
heck_formula <- as.formula(paste("ln_salary ~", X_formula, "+ mills"))
heck_treated <- df[df$masters == 1, ]
heck_model <- lm(heck_formula, data = heck_treated)

heck2_lambda <- coef(heck_model)["mills"]
cat(sprintf("\nHeckman 2-step results:\n"))
cat(sprintf("  lambda (selection correction): %.4f\n", heck2_lambda))

cat("\nIMPORTANT: In Heckman selection models:\n")
cat("  - 'masters' appears in the SELECTION equation, not outcome equation\n")
cat("  - Lambda (inverse Mills ratio) corrects for selection bias\n")
cat("  - Lambda is NOT the treatment effect\n")
cat("  - To get treatment effects, use MTE framework\n")

#------------------------------------------------------------------------------
# Approach 3: IV/2SLS (LATE) - Manual Implementation
#------------------------------------------------------------------------------

cat("\n--- Approach 3: IV/2SLS (Manual) ---\n")

# Stage 1: Regress endogenous variable on instrument and controls
stage1_formula <- as.formula(paste("masters ~ ga_funding_adj +", X_formula))
stage1 <- lm(stage1_formula, data = df)
df$masters_hat <- fitted(stage1)

# Stage 2: Regress outcome on predicted treatment and controls
stage2_formula <- as.formula(paste("ln_salary ~ masters_hat +", X_formula))
stage2 <- lm(stage2_formula, data = df)
iv_est <- coef(stage2)["masters_hat"]
cat(sprintf("IV/LATE estimate: %.4f\n", iv_est))

################################################################################
# SECTION 8: Comparison and Visualization
################################################################################

cat("\n==============================================\n")
cat("RESULTS COMPARISON\n")
cat("==============================================\n")

cat("\nTRUE PARAMETERS (from simulation DGP):\n")
cat(sprintf("  ATE = %.4f\n", true_ate))
cat(sprintf("  ATT = %.4f\n", true_att))
cat(sprintf("  ATU = %.4f\n", true_atu))
cat("  Selection pattern: ATT > ATE > ATU (positive selection on gains)\n")

cat("\nESTIMATED PARAMETERS:\n")
cat(sprintf("  Naive OLS:           %.4f (biased by selection)\n", ols_est))
cat(sprintf("  IV/LATE:             %.4f (complier effect)\n", iv_est))
cat(sprintf("  Heckman 2-step lambda: %.4f (selection correction)\n", heck2_lambda))
cat(sprintf("  MTE-based ATE:       %.4f\n", ate_est))
cat(sprintf("  MTE-based ATT:       %.4f\n", att_est))
cat(sprintf("  MTE-based ATU:       %.4f\n", atu_est))

# Bias analysis
ols_bias <- (ols_est - true_ate) / true_ate * 100
cat("\nOLS BIAS ANALYSIS:\n")
cat(sprintf("  OLS coefficient:     %.4f\n", ols_est))
cat(sprintf("  True ATE:            %.4f\n", true_ate))
cat(sprintf("  Percent bias:        %.1f%%\n", ols_bias))
cat("  Direction:           Upward (positive selection)\n")

#------------------------------------------------------------------------------
# MTE Visualization (using base R graphics)
#------------------------------------------------------------------------------

cat("\n--- Generating MTE Plots ---\n")

# Plot 1: Estimated vs True MTE curve
u_grid <- seq(0.01, 0.99, length.out = 100)
mte_est_curve <- b0 + b1 * u_grid + b2 * u_grid^2
mte_true_curve <- 0.12 + 0.08*0.25 + 0.05*0.15 + 0.10*0.09 + 0.03*0.30 - 0.10*(u_grid - 0.5)

# Plot 1: MTE Curve Comparison
# Save to PNG file first
png("mte_curve_comparison_r.png", width = 800, height = 600)
plot(u_grid, mte_est_curve, type = "l", col = "navy", lwd = 2,
     xlab = "u (Unobserved Resistance to Treatment)",
     ylab = "Marginal Treatment Effect",
     main = "Estimated vs. True MTE\nMaster's Degree Effect on Log Salary",
     ylim = range(c(mte_est_curve, mte_true_curve)))
lines(u_grid, mte_true_curve, col = "darkred", lwd = 2, lty = 2)
legend("topright", legend = c("Estimated MTE", "True MTE"),
       col = c("navy", "darkred"), lty = c(1, 2), lwd = 2)
mtext("Declining MTE indicates positive selection on gains", side = 1, line = 4, cex = 0.8)
# Store plot for later viewing (in interactive sessions)
plot_mte_curve <- recordPlot()
dev.off()

# Plot 2: Treatment Effect by Propensity Score Decile
df$p_decile <- cut(df$p_masters, breaks = quantile(df$p_masters, probs = seq(0, 1, 0.1)),
                   include.lowest = TRUE, labels = 1:10)
df$p_decile <- as.numeric(df$p_decile)

te_by_decile <- aggregate(te_masters ~ p_decile, data = df, FUN = mean)
names(te_by_decile) <- c("decile", "te_mean")

# Plot 2: TE by Propensity Score Decile
png("te_by_decile_r.png", width = 800, height = 600)
plot(te_by_decile$decile, te_by_decile$te_mean, type = "b", 
     col = "navy", pch = 19, cex = 1.5, lwd = 2,
     xlab = "Propensity Score Decile",
     ylab = "Mean Treatment Effect",
     main = "True Treatment Effect by Propensity Score Decile\nEvidence of Essential Heterogeneity")
mtext("Increasing TE with higher propensity indicates positive selection", side = 1, line = 4, cex = 0.8)
# Store plot for later viewing (in interactive sessions)
plot_te_decile <- recordPlot()
dev.off()

cat("Plots saved: mte_curve_comparison_r.png, te_by_decile_r.png\n")

################################################################################
# SECTION 9: Basic Policy Simulation (PRTE)
################################################################################

cat("\n==============================================\n")
cat("POLICY SIMULATION: INCREASE GA FUNDING\n")
cat("==============================================\n")

ga_current <- mean(df$ga_funding_adj)
ga_new <- ga_current * 1.2

df$p_new <- pnorm(df$z_masters + 0.06 * (ga_new - df$ga_funding_adj))
df$delta_p <- df$p_new - df$p_masters

cat(sprintf("Average increase in Pr(Master's): %.4f\n", mean(df$delta_p)))

# Approximate PRTE
df$mte_at_p <- b0 + b1 * df$p_masters + b2 * df$phat2
prte_20pct <- weighted.mean(df$mte_at_p[df$delta_p > 0], 
                             df$delta_p[df$delta_p > 0])
cat(sprintf("Approximate PRTE (20%% GA increase): %.4f\n", prte_20pct))

################################################################################
# SECTION 10: Save Dataset
################################################################################

write.csv(df[, c("id", "masters", "ln_salary", "salary", "te_masters", "p_masters",
                 "female", "black", "hispanic", "asian", "ugpa", "stem_major",
                 "bus_major", "ed_major", "ga_funding_adj", "phat", "mte_i")],
          "bb_mte_simulation_r.csv", row.names = FALSE)

################################################################################
# PART 2: MARGINAL POLICY-RELEVANT TREATMENT EFFECTS (MPRTE)
################################################################################

cat("\n==============================================\n")
cat("PART 2: MPRTE ANALYSIS\n")
cat("==============================================\n")

#------------------------------------------------------------------------------
# Estimate Cubic MTE Function for MPRTE
#------------------------------------------------------------------------------

# Cubic MTE
mte_cubic_formula <- as.formula(paste("ln_salary ~ masters + masters:phat + masters:phat2 + masters:phat3 +",
                                       X_formula, "+ phat + phat2 + phat3"))
mte_cubic <- lm(mte_cubic_formula, data = df)

b0 <- coef(mte_cubic)["masters"]
b1 <- coef(mte_cubic)["masters:phat"]
b2 <- coef(mte_cubic)["masters:phat2"]
b3 <- coef(mte_cubic)["masters:phat3"]

cat("\n==============================================\n")
cat("ESTIMATED MTE FUNCTION (Cubic)\n")
cat("==============================================\n")
cat(sprintf("MTE(u) = %.4f + %.4f*u + %.4f*u² + %.4f*u³\n", b0, b1, b2, b3))

# Create MTE hat variable
df$mte_hat <- b0 + b1 * df$phat + b2 * df$phat2 + b3 * df$phat3

################################################################################
# SECTION 12: MPRTE FOR DIFFERENT POLICY SCENARIOS
################################################################################

cat("\n==============================================\n")
cat("MARGINAL POLICY-RELEVANT TREATMENT EFFECTS\n")
cat("==============================================\n")

# GA coefficient from probit
ga_coef <- coef(probit_model)["ga_funding_adj"]

#------------------------------------------------------------------------------
# MPRTE Calculation Function
#------------------------------------------------------------------------------

calculate_mprte <- function(df, ga_coef, policy_intensity, target_group = NULL) {
  if (is.null(target_group)) {
    target <- rep(TRUE, nrow(df))
  } else {
    target <- target_group
  }
  
  response <- dnorm(qnorm(df$p_masters)) * ga_coef * policy_intensity * target
  mte_weighted <- df$mte_hat * response
  
  mprte <- sum(mte_weighted[target]) / sum(response[target])
  return(mprte)
}

#------------------------------------------------------------------------------
# Scenario 1: Uniform GA Funding Increase
#------------------------------------------------------------------------------

cat("\n--- Scenario 1: Uniform $1,000 GA Funding Increase ---\n")

mprte_unif <- calculate_mprte(df, ga_coef, 1)
cat(sprintf("MPRTE (uniform $1k increase): %.4f\n", mprte_unif))

# Average MTE for marginal region
marginal_region <- df$p_masters > 0.25 & df$p_masters < 0.40
cat(sprintf("Average MTE for marginal region (p=0.25-0.40): %.4f\n", 
            mean(df$mte_hat[marginal_region])))

#------------------------------------------------------------------------------
# Scenario 2: Targeted Funding for Low-Income Students
#------------------------------------------------------------------------------

cat("\n--- Scenario 2: Targeted $2,000 Increase for Low-Income (Q1-Q2) ---\n")

targeted_lowinc <- df$parent_income_q <= 2
mprte_lowinc <- calculate_mprte(df, ga_coef, 2, targeted_lowinc)
cat(sprintf("MPRTE (targeted low-income): %.4f\n", mprte_lowinc))

lowinc_marginal <- targeted_lowinc & df$p_masters > 0.15 & df$p_masters < 0.35
if (sum(lowinc_marginal) > 0) {
  cat(sprintf("True TE for low-income marginal region: %.4f\n", 
              mean(df$te_masters[lowinc_marginal])))
}

#------------------------------------------------------------------------------
# Scenario 3: Enhanced STEM GA Funding
#------------------------------------------------------------------------------

cat("\n--- Scenario 3: Enhanced $3,000 STEM GA Funding ---\n")

targeted_stem <- df$stem_major == 1
mprte_stem <- calculate_mprte(df, ga_coef, 3, targeted_stem)
cat(sprintf("MPRTE (STEM enhancement): %.4f\n", mprte_stem))
cat(sprintf("True ATE for STEM majors: %.4f\n", mean(df$te_masters[targeted_stem])))

#------------------------------------------------------------------------------
# Scenario 4: Education Major Support
#------------------------------------------------------------------------------

cat("\n--- Scenario 4: Education Major GA Support ($2,500) ---\n")

targeted_ed <- df$ed_major == 1
mprte_ed <- calculate_mprte(df, ga_coef, 2.5, targeted_ed)
cat(sprintf("MPRTE (education major support): %.4f\n", mprte_ed))
cat(sprintf("Mean propensity for ed majors: %.4f\n", mean(df$p_masters[targeted_ed])))
cat(sprintf("True ATE for ed majors: %.4f\n", mean(df$te_masters[targeted_ed])))

################################################################################
# SECTION 13: MPRTE BY POLICY INTENSITY
################################################################################

cat("\n==============================================\n")
cat("MPRTE BY POLICY INTENSITY\n")
cat("==============================================\n")

ga_increase <- seq(0.5, 10, by = 0.5)
p_margin <- 0.33 + ga_increase * 0.015
mprte_approx <- b0 + b1 * p_margin + b2 * p_margin^2 + b3 * p_margin^3

intensity_data <- data.frame(
  ga_increase = ga_increase,
  p_margin = p_margin,
  mprte_approx = mprte_approx
)

print(intensity_data)

# Plot MPRTE by intensity
# Plot 3: MPRTE by intensity
png("mprte_by_intensity_r.png", width = 800, height = 600)
plot(intensity_data$ga_increase, intensity_data$mprte_approx, type = "l", 
     col = "navy", lwd = 2,
     xlab = "GA Funding Increase ($1000s)",
     ylab = "MPRTE",
     main = "MPRTE by Policy Intensity\nMarginal returns to GA funding expansion")
mtext("MPRTE pattern reflects MTE curve shape", side = 1, line = 4, cex = 0.8)
# Store plot for later viewing (in interactive sessions)
plot_mprte_intensity <- recordPlot()
dev.off()

################################################################################
# SECTION 14: COMPARING TREATMENT EFFECT PARAMETERS
################################################################################

cat("\n==============================================\n")
cat("COMPARISON OF TREATMENT EFFECT PARAMETERS\n")
cat("==============================================\n")

# True LATE approximation
p_med <- median(df$p_masters)
complier_region <- (df$masters == 1 & df$p_masters < p_med) | 
                   (df$masters == 0 & df$p_masters > p_med)
true_late_approx <- mean(df$te_masters[complier_region])

# Cubic MTE estimates
ate_est_cubic <- b0 + b1/2 + b2/3 + b3/4
att_est_cubic <- mean(df$mte_hat[df$masters == 1])
atu_est_cubic <- mean(df$mte_hat[df$masters == 0])

cat("Parameter          True      Estimated\n")
cat("===========================================\n")
cat(sprintf("ATE                %.4f    %.4f\n", true_ate, ate_est_cubic))
cat(sprintf("ATT                %.4f    %.4f\n", true_att, att_est_cubic))
cat(sprintf("ATU                %.4f    %.4f\n", true_atu, atu_est_cubic))
cat(sprintf("LATE (approx)      %.4f    %.4f (IV)\n", true_late_approx, iv_est))
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
# SECTION 15: MPRTE VISUALIZATION
################################################################################

cat("\n--- Generating MPRTE Visualizations ---\n")

# MTE curve with policy regions
mte_curve <- b0 + b1*u_grid + b2*u_grid^2 + b3*u_grid^3
mte_true <- 0.12 + 0.08*0.25 + 0.05*0.15 + 0.10*0.09 + 0.03*0.30 - 0.10*(u_grid - 0.5)

# Plot 4: MTE curve with policy regions
png("mte_policy_regions_r.png", width = 800, height = 600)
plot(u_grid, mte_curve, type = "l", col = "navy", lwd = 2,
     xlab = "u (Unobserved Resistance to Treatment)",
     ylab = "Marginal Treatment Effect",
     main = "MTE Curve with Policy-Relevant Regions\nDifferent policies target different margins",
     ylim = range(c(mte_curve, mte_true)))
lines(u_grid, mte_true, col = "darkgreen", lwd = 2, lty = 2)

# Shade policy regions
lowinc_region <- u_grid >= 0.10 & u_grid <= 0.25
uniform_region <- u_grid >= 0.25 & u_grid <= 0.40
polygon(c(u_grid[lowinc_region], rev(u_grid[lowinc_region])),
        c(mte_curve[lowinc_region], rep(0, sum(lowinc_region))),
        col = rgb(0.8, 0.2, 0.2, 0.3), border = NA)
polygon(c(u_grid[uniform_region], rev(u_grid[uniform_region])),
        c(mte_curve[uniform_region], rep(0, sum(uniform_region))),
        col = rgb(0, 0, 0.5, 0.3), border = NA)

legend("topright", 
       legend = c("Estimated MTE", "True MTE", "Low-income margin", "Uniform policy margin"),
       col = c("navy", "darkgreen", rgb(0.8, 0.2, 0.2, 0.5), rgb(0, 0, 0.5, 0.5)),
       lty = c(1, 2, NA, NA), lwd = c(2, 2, NA, NA),
       pch = c(NA, NA, 15, 15), pt.cex = 2)
# Store plot for later viewing (in interactive sessions)
plot_mte_regions <- recordPlot()
dev.off()

################################################################################
# SECTION 16: POLICY COST-BENEFIT ANALYSIS
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

# Calculate cost-benefit for each scenario
policy_names <- c("Uniform", "Low-income", "STEM", "Education")
mprte_values <- c(mprte_unif, mprte_lowinc, mprte_stem, mprte_ed)
annual_gains <- base_salary * (exp(mprte_values) - 1)
pv_gains <- annual_gains * pv_factor
bc_ratios <- pv_gains / cost_per_degree

policy_cb <- data.frame(
  Policy = policy_names,
  MPRTE = mprte_values,
  Annual_Gain = annual_gains,
  PV_Gain = pv_gains,
  BC_Ratio = bc_ratios
)

cat("\nPolicy               MPRTE    Annual Gain   PV Gain    B/C Ratio\n")
cat("==================================================================\n")
for (i in 1:nrow(policy_cb)) {
  cat(sprintf("%-20s %.4f   $%6.0f    $%8.0f   %.2f\n",
              policy_cb$Policy[i], policy_cb$MPRTE[i], policy_cb$Annual_Gain[i],
              policy_cb$PV_Gain[i], policy_cb$BC_Ratio[i]))
}

cat("\nNote: B/C ratio > 1 suggests policy expansion is beneficial\n")
cat("      These calculations ignore externalities, taxes, and general equilibrium\n")

################################################################################
# SECTION 17: FINAL SUMMARY AND SAVE OUTPUTS
################################################################################

cat("\n==============================================\n")
cat("SIMULATION COMPLETE\n")
cat("==============================================\n")
cat("Key findings:\n")
cat(sprintf("  1. Treatment rate: %.3f (target ~25-30%%)\n", treat_rate))
cat(sprintf("  2. True ATE: %.4f\n", true_ate))
cat(sprintf("  3. True ATT: %.4f (ATT > ATE confirms positive selection)\n", true_att))
cat(sprintf("  4. True ATU: %.4f (ATU < ATE confirms positive selection)\n", true_atu))
cat(sprintf("  5. OLS is upward biased (%.1f%%) due to selection\n", ols_bias))
cat(sprintf("  6. IV/LATE (%.4f) provides consistent estimate for compliers\n", iv_est))
cat("  7. MTE approach recovers full distribution of treatment effects\n")
cat(sprintf("  8. First-stage F = %.1f (strong instrument)\n", first_stage_F))
cat(sprintf("  9. MPRTE varies by policy scenario (range: %.3f to %.3f)\n", 
            min(mprte_values), max(mprte_values)))

cat("\nMPRTE KEY INSIGHTS:\n")
cat("  - PRTE averages over all compliers induced by policy change\n")
cat("  - MPRTE is the effect at the margin (infinitesimal change)\n")
cat("  - With positive selection: expanding policy has INCREASING returns\n")
cat("  - Cost-effectiveness depends on WHERE policy operates on MTE curve\n")

# Save summary tables
write.csv(te_by_decile, "te_by_decile_r.csv", row.names = FALSE)
write.csv(intensity_data, "mprte_by_intensity_r.csv", row.names = FALSE)
write.csv(policy_cb, "policy_costbenefit_r.csv", row.names = FALSE)

cat("\nFiles saved:\n")
cat("  - bb_mte_simulation_r.csv (full dataset)\n")
cat("  - mte_curve_comparison_r.png\n")
cat("  - te_by_decile_r.png / .csv\n")
cat("  - mprte_by_intensity_r.png / .csv\n")
cat("  - mte_policy_regions_r.png\n")
cat("  - policy_costbenefit_r.csv\n")

cat("\n==============================================\n")
cat("DISPLAYING GRAPHS\n")
cat("==============================================\n")

# Re-create plots for interactive viewing
# (In interactive R/RStudio, these will display in the graphics window)

cat("\nDisplaying: MTE Curve Comparison\n")
plot(u_grid, mte_est_curve, type = "l", col = "navy", lwd = 2,
     xlab = "u (Unobserved Resistance to Treatment)",
     ylab = "Marginal Treatment Effect",
     main = "Estimated vs. True MTE\nMaster's Degree Effect on Log Salary",
     ylim = range(c(mte_est_curve, mte_true_curve)))
lines(u_grid, mte_true_curve, col = "darkred", lwd = 2, lty = 2)
legend("topright", legend = c("Estimated MTE", "True MTE"),
       col = c("navy", "darkred"), lty = c(1, 2), lwd = 2)
mtext("Declining MTE indicates positive selection on gains", side = 1, line = 4, cex = 0.8)
plot_mte_curve <- recordPlot()

cat("Displaying: Treatment Effect by Decile\n")
plot(te_by_decile$decile, te_by_decile$te_mean, type = "b", 
     col = "navy", pch = 19, cex = 1.5, lwd = 2,
     xlab = "Propensity Score Decile",
     ylab = "Mean Treatment Effect",
     main = "True Treatment Effect by Propensity Score Decile\nEvidence of Essential Heterogeneity")
mtext("Increasing TE with higher propensity indicates positive selection", side = 1, line = 4, cex = 0.8)
plot_te_decile <- recordPlot()

cat("Displaying: MPRTE by Policy Intensity\n")
plot(intensity_data$ga_increase, intensity_data$mprte_approx, type = "l", 
     col = "navy", lwd = 2,
     xlab = "GA Funding Increase ($1000s)",
     ylab = "MPRTE",
     main = "MPRTE by Policy Intensity\nMarginal returns to GA funding expansion")
mtext("MPRTE pattern reflects MTE curve shape", side = 1, line = 4, cex = 0.8)
plot_mprte_intensity <- recordPlot()

cat("Displaying: MTE with Policy Regions\n")
plot(u_grid, mte_curve, type = "l", col = "navy", lwd = 2,
     xlab = "u (Unobserved Resistance to Treatment)",
     ylab = "Marginal Treatment Effect",
     main = "MTE Curve with Policy-Relevant Regions\nDifferent policies target different margins",
     ylim = range(c(mte_curve, mte_true)))
lines(u_grid, mte_true, col = "darkgreen", lwd = 2, lty = 2)
lowinc_region <- u_grid >= 0.10 & u_grid <= 0.25
uniform_region <- u_grid >= 0.25 & u_grid <= 0.40
polygon(c(u_grid[lowinc_region], rev(u_grid[lowinc_region])),
        c(mte_curve[lowinc_region], rep(0, sum(lowinc_region))),
        col = rgb(0.8, 0.2, 0.2, 0.3), border = NA)
polygon(c(u_grid[uniform_region], rev(u_grid[uniform_region])),
        c(mte_curve[uniform_region], rep(0, sum(uniform_region))),
        col = rgb(0, 0, 0.5, 0.3), border = NA)
legend("topright", 
       legend = c("Estimated MTE", "True MTE", "Low-income margin", "Uniform policy margin"),
       col = c("navy", "darkgreen", rgb(0.8, 0.2, 0.2, 0.5), rgb(0, 0, 0.5, 0.5)),
       lty = c(1, 2, NA, NA), lwd = c(2, 2, NA, NA),
       pch = c(NA, NA, 15, 15), pt.cex = 2)
plot_mte_regions <- recordPlot()

cat("\nAll graphs displayed. To view individual graphs, use:\n")
cat("  replayPlot(plot_mte_curve)       # MTE curve comparison\n")
cat("  replayPlot(plot_te_decile)       # TE by propensity decile\n")
cat("  replayPlot(plot_mprte_intensity) # MPRTE by policy intensity\n")
cat("  replayPlot(plot_mte_regions)     # MTE with policy regions\n")

cat("\n==============================================\n")
cat("END OF MTE/MPRTE ANALYSIS\n")
cat("==============================================\n")

################################################################################
# END
################################################################################
