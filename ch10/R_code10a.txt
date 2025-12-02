#=======================================================
# Chapter 10 - Causal Inference Techniques
# Complete R Code (Translated from Stata)
# Higher Education Policy Analysis Using Quantitative
# Techniques (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-
# 2nd-edition/tree/main/code/ch10
# Author: Marvin A. Titus
# Date: November 30, 2025
#=======================================================
# Script tested in R 4.4+
# Compatible with R version 4.0 or later
#=======================================================
# IMPORTANT: Set working directory (customize this for
# your system)
#=======================================================
# ch10data <- "C:/Users/YourName/Documents/book-materials/ch10/data"
# setwd(ch10data)
#=======================================================
# Analysis of Georgia's Higher Education Consolidation
# Policy Using Multiple Causal Inference Techniques
#=======================================================

# Clear workspace
rm(list = ls())

#=======================================================
# Load Required Packages
#=======================================================

# Install packages if needed (uncomment to install)
# install.packages(c("tidyverse", "fixest", "did", "synthdid",
#                    "Synth", "glmnet", "ggplot2", "modelsummary",
#                    "broom", "sandwich", "lmtest"))

library(tidyverse)    # Data manipulation and visualization
library(fixest)       # Fast fixed effects estimation
library(did)          # Callaway-Sant'Anna DiD
library(synthdid)     # Synthetic DiD
library(Synth)        # Synthetic control method
library(glmnet)       # LASSO regression
library(modelsummary) # Results tables
library(broom)        # Tidy model output

#=======================================================
# Section 10.3.1: Data Structure and Variable Construction
# (Application: TWFE DiD for Georgia Consolidation)
#=======================================================

# Download data from GitHub repository
data_url <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch10/Example_10_3_1.csv"
df <- read_csv(data_url, show_col_types = FALSE)

# Clean column names (lowercase, remove spaces)
names(df) <- names(df) %>%
  str_to_lower() %>%
  str_replace_all(" ", "") %>%
  str_replace_all("_+", "_")

# Clean state names
df <- df %>%
  mutate(state = str_trim(state))

# Create SREB indicator (16 Southern states)
sreb_states <- c("Alabama", "Arkansas", "Delaware", "Florida",
                 "Georgia", "Kentucky", "Louisiana", "Maryland",
                 "Mississippi", "North Carolina", "Oklahoma",
                 "South Carolina", "Tennessee", "Texas",
                 "Virginia", "West Virginia")

df <- df %>%
  filter(state %in% sreb_states)

# Create FIPS codes for panel identification
fips_lookup <- c(
  "Alabama" = 1, "Arkansas" = 5, "Delaware" = 10, "Florida" = 12,
  "Georgia" = 13, "Kentucky" = 21, "Louisiana" = 22, "Maryland" = 24,
  "Mississippi" = 28, "North Carolina" = 37, "Oklahoma" = 40,
  "South Carolina" = 45, "Tennessee" = 47, "Texas" = 48,
  "Virginia" = 51, "West Virginia" = 54
)

df <- df %>%
  mutate(fips = fips_lookup[state])

# Treatment state indicator (Georgia = 1)
df <- df %>%
  mutate(treat_state = as.integer(state == "Georgia"))

# Post-treatment period (2018 onwards)
df <- df %>%
  mutate(post = as.integer(fy >= 2018))

# DiD interaction term
df <- df %>%
  mutate(did = treat_state * post)

# Placebo test indicators
df <- df %>%
  mutate(
    post_placebo = as.integer(fy >= 2012),
    did_placebo = treat_state * post_placebo
  )

# Log-transformed variables
df <- df %>%
  mutate(
    lngenop = log(general_public_operations),
    lntotsup = log(total_state_support),
    lnfinaid = log(total_financial_aid),
    lntuifee = log(net_tuition_and_fee_revenue),
    lnfte = log(net_fte_enrollment)
  )

# Define control variables (for formula construction)
controls <- c("lntotsup", "lnfinaid", "lntuifee", "lnfte")

#=======================================================
# Section 10.3.2: TWFE Estimation Results
# (Two-Way Fixed Effects DiD)
#=======================================================

# Two-Way Fixed Effects (TWFE) DiD using fixest
# Equivalent to: xtreg lngenop did $controls i.fy, fe vce(cluster fips)
twfe_did <- feols(
  lngenop ~ did + lntotsup + lnfinaid + lntuifee + lnfte | fips + fy,
  data = df,
  cluster = ~fips
)

summary(twfe_did)

# Placebo test (pre-treatment falsification)
placebo_test <- feols(
  lngenop ~ did_placebo + lntotsup + lnfinaid + lntuifee + lnfte | fips + fy,
  data = df %>% filter(fy < 2018),
  cluster = ~fips
)

summary(placebo_test)

#=======================================================
# Section 10.3.3: Parallel Trends Assessment
#=======================================================

# Visual inspection - trends plot
trends_data <- df %>%
  group_by(treat_state, fy) %>%
  summarize(lngenop = mean(lngenop, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = factor(treat_state,
                        levels = c(0, 1),
                        labels = c("Control States", "Georgia")))

parallel_trends_plot <- ggplot(trends_data, aes(x = fy, y = lngenop,
                                                 color = group,
                                                 linetype = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2018, linetype = "dotted") +
  scale_color_manual(values = c("navy", "maroon")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  labs(
    title = "Parallel Trends: Treatment vs Control",
    x = "Fiscal Year",
    y = "Log Operating Expenses",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(parallel_trends_plot)

# Formal pre-trends test
# Equivalent to: reghdfe lngenop c.treat_state#c.fy $controls if fy < 2018,
#                absorb(fips fy) vce(cluster fips)
pretrends_test <- feols(
  lngenop ~ treat_state:fy + lntotsup + lnfinaid + lntuifee + lnfte | fips + fy,
  data = df %>% filter(fy < 2018),
  cluster = ~fips
)

summary(pretrends_test)

# Test: treat_state:fy = 0
cat("\nPre-trends test (H0: parallel trends):\n")
print(wald(pretrends_test, "treat_state"))

#=======================================================
# Section 10.3.4: Robustness Checks
#=======================================================

# Alternative treatment timing (2013)
df <- df %>%
  mutate(
    post_2013 = as.integer(fy >= 2013),
    did_2013 = treat_state * post_2013
  )

robust_2013 <- feols(
  lngenop ~ did_2013 + lntotsup + lnfinaid + lntuifee + lnfte | fips + fy,
  data = df,
  cluster = ~fips
)

summary(robust_2013)

# Alternative treatment timing (2015)
df <- df %>%
  mutate(
    post_2015 = as.integer(fy >= 2015),
    did_2015 = treat_state * post_2015
  )

robust_2015 <- feols(
  lngenop ~ did_2015 + lntotsup + lnfinaid + lntuifee + lnfte | fips + fy,
  data = df,
  cluster = ~fips
)

summary(robust_2015)

# Exclude border states
border_states <- c("Florida", "Alabama", "South Carolina",
                   "Tennessee", "North Carolina")

df <- df %>%
  mutate(border = as.integer(state %in% border_states))

robust_noborder <- feols(
  lngenop ~ did + lntotsup + lnfinaid + lntuifee + lnfte | fips + fy,
  data = df %>% filter(border == 0),
  cluster = ~fips
)

summary(robust_noborder)

# Weighted regression by enrollment
# Create time-invariant weight (mean enrollment by state)
df <- df %>%
  group_by(fips) %>%
  mutate(mean_fte = mean(net_fte_enrollment, na.rm = TRUE)) %>%
  ungroup()

robust_weighted <- feols(
  lngenop ~ did + lntotsup + lnfinaid + lntuifee + lnfte | fips + fy,
  data = df,
  weights = ~mean_fte,
  cluster = ~fips
)

summary(robust_weighted)

#=======================================================
# Section 10.4.2-10.4.3: LASSO-Residualized DiD
# (Machine Learning Approach: Specification & Results)
#=======================================================

# Create interaction terms for LASSO selection
df <- df %>%
  mutate(
    lntotsup_post = lntotsup * post,
    lntotsup_treat = lntotsup * treat_state,
    lnfinaid_post = lnfinaid * post,
    lnfinaid_treat = lnfinaid * treat_state,
    lntuifee_post = lntuifee * post,
    lntuifee_treat = lntuifee * treat_state,
    lnfte_post = lnfte * post,
    lnfte_treat = lnfte * treat_state
  )

# Prepare data for LASSO
lasso_vars <- c("did", "post", "treat_state",
                "lntotsup", "lnfinaid", "lntuifee", "lnfte",
                "lntotsup_post", "lntotsup_treat",
                "lnfinaid_post", "lnfinaid_treat",
                "lntuifee_post", "lntuifee_treat",
                "lnfte_post", "lnfte_treat")

# Remove rows with missing values
lasso_data <- df %>%
  select(lngenop, all_of(lasso_vars), fips, fy) %>%
  drop_na()

# Create model matrix
X <- as.matrix(lasso_data[, lasso_vars])
y <- lasso_data$lngenop

# LASSO with cross-validation
set.seed(12345)
cv_lasso <- cv.glmnet(X, y, alpha = 1, nfolds = 10)

# Extract selected variables (non-zero coefficients)
lasso_coef <- coef(cv_lasso, s = "lambda.min")
selected_vars <- rownames(lasso_coef)[which(lasso_coef != 0)]
selected_vars <- selected_vars[selected_vars != "(Intercept)"]

cat("\nLASSO-selected variables:\n")
print(selected_vars)

# Post-LASSO OLS with fixed effects
# Build formula dynamically
if ("did" %in% selected_vars) {
  # Ensure 'did' is included even if not selected
  other_selected <- setdiff(selected_vars, "did")
  lasso_formula <- as.formula(
    paste("lngenop ~ did +", paste(other_selected, collapse = " + "), "| fips + fy")
  )
} else {
  lasso_formula <- as.formula(
    paste("lngenop ~ did +", paste(selected_vars, collapse = " + "), "| fips + fy")
  )
}

lasso_did <- feols(
  lasso_formula,
  data = lasso_data,
  cluster = ~fips
)

summary(lasso_did)

#=======================================================
# Section 10.5.3: SCM Application to Georgia Consolidation
# (Synthetic Control Method)
#=======================================================

# Prepare data for synth command
synth_data <- df %>%
  filter(!is.na(lngenop) & !is.na(lntotsup) & !is.na(lnfinaid) &
           !is.na(lntuifee) & !is.na(lnfte)) %>%
  as.data.frame()

# Prepare data for Synth package
dataprep_out <- dataprep(
  foo = synth_data,
  predictors = c("lntotsup", "lnfinaid", "lntuifee", "lnfte"),
  predictors.op = "mean",
  time.predictors.prior = 2005:2017,
  special.predictors = list(
    list("lngenop", 2005, "mean"),
    list("lngenop", 2010, "mean"),
    list("lngenop", 2015, "mean")
  ),
  dependent = "lngenop",
  unit.variable = "fips",
  unit.names.variable = "state",
  time.variable = "fy",
  treatment.identifier = 13,  # Georgia FIPS
  controls.identifier = unique(synth_data$fips[synth_data$fips != 13]),
  time.optimize.ssr = 2005:2017,
  time.plot = 2005:max(synth_data$fy)
)

# Run synthetic control
synth_out <- synth(data.prep.obj = dataprep_out, method = "BFGS")

# Synth tables
synth.tables <- synth.tab(dataprep.res = dataprep_out,
                          synth.res = synth_out)

print(synth.tables$tab.pred)
print(synth.tables$tab.w)

# Path plot
path.plot(synth.res = synth_out,
          dataprep.res = dataprep_out,
          Ylab = "Log Operating Expenses",
          Xlab = "Fiscal Year",
          Legend = c("Georgia", "Synthetic Georgia"),
          Legend.position = "bottomright")
abline(v = 2018, lty = 2)

# Gap plot
gaps.plot(synth.res = synth_out,
          dataprep.res = dataprep_out,
          Ylab = "Gap in Log Operating Expenses",
          Xlab = "Fiscal Year")
abline(v = 2018, lty = 2)
abline(h = 0, lty = 3)

# Calculate treatment effect
synth_gap <- dataprep_out$Y1plot - (dataprep_out$Y0plot %*% synth_out$solution.w)
synth_results <- data.frame(
  year = as.numeric(rownames(dataprep_out$Y1plot)),
  treated = as.numeric(dataprep_out$Y1plot),
  synthetic = as.numeric(dataprep_out$Y0plot %*% synth_out$solution.w),
  gap = as.numeric(synth_gap)
)

# Post-treatment average effect
post_treatment_effect <- synth_results %>%
  filter(year >= 2018) %>%
  summarize(
    mean_gap = mean(gap),
    sd_gap = sd(gap),
    n = n()
  )

cat("\nPost-treatment average effect (SCM):\n")
print(post_treatment_effect)

#=======================================================
# Section 10.6.3: SDID Results - Single State Analysis
# (Synthetic Difference-in-Differences)
#=======================================================

# Prepare data for synthdid package
# synthdid requires long-format data with: unit, time, outcome, treatment
sdid_data <- df %>%
  filter(!is.na(lngenop)) %>%
  mutate(
    treat_sdid = as.integer(fips == 13 & fy >= 2018)
  ) %>%
  select(fips, fy, lngenop, treat_sdid) %>%
  arrange(fips, fy) %>%
  as.data.frame()

# Check for balanced panel
panel_check <- sdid_data %>%
  group_by(fips) %>%
  summarize(n = n(), .groups = "drop")

if (length(unique(panel_check$n)) == 1) {
  
  # Use panel.matrices with long-format data
  # Arguments: data frame, unit column, time column, outcome column, treatment column
  sdid_setup <- panel.matrices(
    sdid_data,
    unit = 1,      # fips is column 1
    time = 2,      # fy is column 2
    outcome = 3,   # lngenop is column 3
    treatment = 4  # treat_sdid is column 4
  )
  
  # Run SDID estimation
  sdid_est <- synthdid_estimate(sdid_setup$Y, sdid_setup$N0, sdid_setup$T0)
  
  cat("\nSDID Estimate:\n")
  print(summary(sdid_est))
  
  # Plot SDID
  plot(sdid_est)
  
} else {
  cat("\nNote: SDID requires balanced panel. Unequal observations per unit detected.\n")
  print(panel_check)
}

#=======================================================
# Section 10.2.4 & 10.3.3: Event Study Specifications
# (Part of Parallel Trends Assessment)
#=======================================================

# Create relative time indicators
df <- df %>%
  mutate(
    rel_time = case_when(
      treat_state == 1 ~ fy - 2018,
      TRUE ~ 0
    )
  )

# Event study using fixest's i() function
# Reference period is typically -1 (year before treatment)
event_study <- feols(
  lngenop ~ i(rel_time, treat_state, ref = -1) +
    lntotsup + lnfinaid + lntuifee + lnfte | fips + fy,
  data = df,
  cluster = ~fips
)

summary(event_study)

# Plot event study coefficients using fixest's iplot
iplot(event_study,
      main = "Event Study: Georgia Consolidation",
      xlab = "Years Relative to Treatment",
      ylab = "Effect on Log Operating Expenses")
abline(h = 0, lty = 2)

# Alternative: ggplot version
event_coefs <- broom::tidy(event_study, conf.int = TRUE) %>%
  filter(str_detect(term, "rel_time")) %>%
  mutate(
    rel_time = as.numeric(str_extract(term, "-?\\d+"))
  ) %>%
  bind_rows(tibble(rel_time = -1, estimate = 0,
                   conf.low = 0, conf.high = 0))  # Add reference period

event_study_plot <- ggplot(event_coefs, aes(x = rel_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "red") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, color = "navy") +
  geom_point(size = 3, color = "navy") +
  labs(
    title = "Event Study: Effect on Log Operating Expenses",
    x = "Years Relative to Treatment (2018)",
    y = "Coefficient Estimate"
  ) +
  theme_minimal()

print(event_study_plot)

# Note: Callaway-Sant'Anna DiD is designed for staggered adoption with
# multiple treated units. For single treated unit (Georgia), use TWFE,
# SCM, or SDID. See Section 10.7 for CS DiD with staggered adoption.

#=======================================================
# Section 10.7.3-10.7.5: Multi-State Staggered Analysis
# (Staggered Adoption Designs with Callaway-Sant'Anna)
#=======================================================

# Extension: Three-state staggered consolidation design
#   - Georgia (FIPS 13): 2013
#   - Wisconsin (FIPS 55): 2018
#   - Pennsylvania (FIPS 42): 2022

# Import expanded dataset (48 states)
staggered_url <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch10/Example_10_7_3.csv"
df_staggered <- read_csv(staggered_url, show_col_types = FALSE)

# Clean column names (lowercase, remove spaces)
names(df_staggered) <- names(df_staggered) %>%
  str_to_lower() %>%
  str_replace_all(" ", "") %>%
  str_replace_all("_+", "_")

# Create logged variables (handle different column naming conventions)
# Check for column name variations
if ("generalpublicoperations" %in% names(df_staggered)) {
  df_staggered <- df_staggered %>%
    mutate(
      lngenop = log(generalpublicoperations),
      lntotsup = log(totalstatesupport),
      lnfinaid = log(totalfinancialaid),
      lntuifee = log(nettuitionandfeerevenue),
      lnfte = log(netfteenrollment)
    )
} else {
  df_staggered <- df_staggered %>%
    mutate(
      lngenop = log(general_public_operations),
      lntotsup = log(total_state_support),
      lnfinaid = log(total_financial_aid),
      lntuifee = log(net_tuition_and_fee_revenue),
      lnfte = log(net_fte_enrollment)
    )
}

# Drop missing observations
df_staggered <- df_staggered %>%
  filter(!is.na(lngenop) & !is.na(lntotsup) & !is.na(lnfinaid) &
           !is.na(lntuifee) & !is.na(lnfte))

# Create staggered treatment variable (gyear)
df_staggered <- df_staggered %>%
  mutate(
    gyear = case_when(
      fips == 13 ~ 2013L,  # Georgia
      fips == 55 ~ 2018L,  # Wisconsin
      fips == 42 ~ 2022L,  # Pennsylvania
      TRUE ~ 0L            # Never treated
    )
  )

# Balance panel: keep only complete state-year combinations
obs_counts <- df_staggered %>%
  group_by(fips) %>%
  summarize(obs_count = n(), .groups = "drop")

mode_count <- as.numeric(names(sort(table(obs_counts$obs_count),
                                     decreasing = TRUE)[1]))

balanced_fips <- obs_counts %>%
  filter(obs_count == mode_count) %>%
  pull(fips)

df_staggered_balanced <- df_staggered %>%
  filter(fips %in% balanced_fips)

cat("\nPanel structure after balancing:\n")
cat("Number of states:", length(unique(df_staggered_balanced$fips)), "\n")
cat("Time periods:", min(df_staggered_balanced$fy), "to",
    max(df_staggered_balanced$fy), "\n")

# CSDID estimation (baseline - no controls)
cs_staggered <- att_gt(
  yname = "lngenop",
  tname = "fy",
  idname = "fips",
  gname = "gyear",
  data = df_staggered_balanced,
  est_method = "dr",
  control_group = "nevertreated"
)

summary(cs_staggered)

# Overall ATT
cs_stag_simple <- aggte(cs_staggered, type = "simple", na.rm = TRUE)
summary(cs_stag_simple)

# Group-specific effects
cs_stag_group <- aggte(cs_staggered, type = "group", na.rm = TRUE)
summary(cs_stag_group)

# Event study
cs_stag_event <- aggte(cs_staggered, type = "dynamic", na.rm = TRUE)
summary(cs_stag_event)
ggdid(cs_stag_event, title = "Staggered DiD Event Study")

# Calendar time effects
cs_stag_calendar <- aggte(cs_staggered, type = "calendar", na.rm = TRUE)
summary(cs_stag_calendar)

# SDID robustness check with staggered treatment
# Create binary treatment indicator
df_staggered_balanced <- df_staggered_balanced %>%
  mutate(
    treatment = case_when(
      fips == 13 & fy >= 2013 ~ 1L,
      fips == 55 & fy >= 2018 ~ 1L,
      fips == 42 & fy >= 2022 ~ 1L,
      TRUE ~ 0L
    )
  )

# Note: Standard synthdid doesn't directly handle staggered adoption
# For staggered SDID, consider staggered_synthdid package or manual cohort analysis
cat("\nNote: For staggered SDID, run separate analyses by cohort or use\n")
cat("specialized packages like 'staggered' or implement cohort-specific SDIDs.\n")

#=======================================================
# Section 10.8.2: Permutation Inference
# (Sensitivity Analysis)
#=======================================================

# Permutation test function
run_permutation_test <- function(data, n_reps = 1000, seed = 12345) {
  set.seed(seed)
  
  # Get actual treatment effect
  actual_model <- feols(
    lngenop ~ did + lntotsup + lnfinaid + lntuifee + lnfte | fips + fy,
    data = data,
    cluster = ~fips
  )
  actual_coef <- coef(actual_model)["did"]
  
  # Storage for permutation coefficients
  perm_coefs <- numeric(n_reps)
  
  # Get unique states for permutation
  states <- unique(data$fips)
  control_states <- states[states != 13]  # Exclude Georgia
  
  cat("Running permutation test with", n_reps, "replications...\n")
  
  for (i in 1:n_reps) {
    # Randomly assign treatment to one control state
    fake_treated <- sample(control_states, 1)
    
    perm_data <- data %>%
      mutate(
        perm_treat = as.integer(fips == fake_treated),
        perm_did = perm_treat * post
      )
    
    perm_model <- tryCatch({
      feols(
        lngenop ~ perm_did + lntotsup + lnfinaid + lntuifee + lnfte | fips + fy,
        data = perm_data,
        cluster = ~fips
      )
    }, error = function(e) NULL)
    
    if (!is.null(perm_model)) {
      perm_coefs[i] <- coef(perm_model)["perm_did"]
    } else {
      perm_coefs[i] <- NA
    }
    
    if (i %% 100 == 0) cat("Completed", i, "replications\n")
  }
  
  # Remove NAs
  perm_coefs <- perm_coefs[!is.na(perm_coefs)]
  
  # Calculate p-value (two-sided)
  p_value <- mean(abs(perm_coefs) >= abs(actual_coef))
  
  list(
    actual_coef = actual_coef,
    perm_coefs = perm_coefs,
    p_value = p_value
  )
}

# Run permutation test (reduce reps for speed; increase for publication)
perm_results <- run_permutation_test(df, n_reps = 500)

cat("\nPermutation Test Results:\n")
cat("Actual coefficient:", round(perm_results$actual_coef, 4), "\n")
cat("Permutation p-value:", round(perm_results$p_value, 4), "\n")

# Histogram of permutation distribution
perm_hist <- ggplot(data.frame(coef = perm_results$perm_coefs), aes(x = coef)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(xintercept = perm_results$actual_coef, color = "red",
             linewidth = 1.2, linetype = "dashed") +
  labs(
    title = "Permutation Distribution of DiD Coefficient",
    x = "Coefficient",
    y = "Frequency"
  ) +
  annotate("text", x = perm_results$actual_coef, y = Inf,
           label = paste("Actual =", round(perm_results$actual_coef, 3)),
           vjust = 2, hjust = -0.1, color = "red") +
  theme_minimal()

print(perm_hist)

#=======================================================
# Section 10.8.3: Leave-One-Out Analysis
# (Sensitivity Analysis)
#=======================================================

# Leave-one-out analysis
control_fips <- unique(df$fips[df$treat_state == 0])

loo_results <- tibble(
  excluded_fips = integer(),
  coefficient = numeric(),
  std_error = numeric()
)

for (s in control_fips) {
  loo_model <- feols(
    lngenop ~ did + lntotsup + lnfinaid + lntuifee + lnfte | fips + fy,
    data = df %>% filter(fips != s),
    cluster = ~fips
  )
  
  loo_results <- loo_results %>%
    add_row(
      excluded_fips = s,
      coefficient = coef(loo_model)["did"],
      std_error = se(loo_model)["did"]
    )
}

# Add state names for display
fips_to_state <- df %>%
  select(fips, state) %>%
  distinct()

loo_results <- loo_results %>%
  left_join(fips_to_state, by = c("excluded_fips" = "fips"))

cat("\nLeave-One-Out Analysis:\n")
print(loo_results %>% arrange(excluded_fips))

# Plot LOO results
loo_plot <- ggplot(loo_results, aes(x = reorder(state, coefficient),
                                     y = coefficient)) +
  geom_point(size = 3, color = "navy") +
  geom_errorbar(aes(ymin = coefficient - 1.96 * std_error,
                    ymax = coefficient + 1.96 * std_error),
                width = 0.2, color = "navy") +
  geom_hline(yintercept = coef(twfe_did)["did"],
             linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Leave-One-Out Sensitivity Analysis",
    subtitle = "Red line = full sample estimate",
    x = "Excluded State",
    y = "DiD Coefficient"
  ) +
  theme_minimal()

print(loo_plot)

#=======================================================
# Section 10.9: Results Summary
# (Interpretation and Policy Implications)
#=======================================================

# Compare main methods
cat("\n" , rep("=", 60), "\n", sep = "")
cat("RESULTS SUMMARY\n")
cat(rep("=", 60), "\n\n", sep = "")

# Create comparison table
models_list <- list(
  "TWFE DiD" = twfe_did,
  "LASSO DiD" = lasso_did
)

# Print comparison table
modelsummary(
  models_list,
  stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  coef_rename = c("did" = "DiD Effect"),
  title = "Comparison of DiD Estimation Methods"
)

# Export results to CSV
results_twfe <- tidy(twfe_did, conf.int = TRUE) %>%
  mutate(model = "TWFE DiD")

results_lasso <- tidy(lasso_did, conf.int = TRUE) %>%
  mutate(model = "LASSO DiD")

results_combined <- bind_rows(results_twfe, results_lasso)

write_csv(results_combined, "results_combined.csv")
cat("\nResults exported to 'results_combined.csv'\n")

# Summary statistics
cat("\nKey Findings:\n")
cat("TWFE DiD coefficient on treatment:", round(coef(twfe_did)["did"], 4), "\n")
cat("LASSO DiD coefficient on treatment:", round(coef(lasso_did)["did"], 4), "\n")

if (exists("post_treatment_effect")) {
  cat("SCM average post-treatment gap:", round(post_treatment_effect$mean_gap, 4), "\n")
}

cat("\n", rep("=", 60), "\n", sep = "")
cat("END OF CHAPTER 10 CODE\n")
cat(rep("=", 60), "\n", sep = "")

#=======================================================
# Notes:
# - Script sections aligned with Chapter 10 outline
# - Section 10.3.1: Data structure and variable construction
# - Section 10.3.2: TWFE estimation results
# - Section 10.3.3: Parallel trends assessment
# - Section 10.3.4: Robustness checks
# - Section 10.4.2-10.4.3: LASSO-residualized DiD
# - Section 10.5.3: SCM application
# - Section 10.6.3: SDID results (single-state)
# - Section 10.7.3-10.7.5: Multi-state staggered analysis (CS DiD)
# - Section 10.8.2: Permutation inference
# - Section 10.8.3: Leave-one-out analysis
# - Section 10.9: Results summary and interpretation
#
# R Package Equivalents to Stata Commands:
# - xtreg ... fe vce(cluster) → fixest::feols() with | and cluster
# - reghdfe → fixest::feols()
# - synth → Synth::synth()
# - sdid → synthdid::synthdid_estimate()
# - csdid → did::att_gt()
# - lasso2 → glmnet::cv.glmnet()
# - esttab → modelsummary::modelsummary()
#
# Note: Callaway-Sant'Anna DiD (did::att_gt) is designed for
# staggered adoption with multiple treated units. For single
# treated unit analysis, use TWFE, SCM, or SDID instead.
#
# Theoretical sections (10.1, 10.2, etc.) covered in chapter text
# This script provides empirical implementation
#=======================================================
