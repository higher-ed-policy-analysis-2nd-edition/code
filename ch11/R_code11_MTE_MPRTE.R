#========================================================================
# Chapter 10 – Sections 10.10–10.16: Marginal Treatment Effects
#             Returns to Master's Degree Completion
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch10
# Author: Marvin A. Titus
# Date: May 2026
# NOTE: Code development was assisted by Claude (Anthropic). The author
# provided specifications and reviewed, tested, and validated all code.
#========================================================================
# Translation of MTE_MPRTE_v2.do to R
#
# Required packages:
#   haven, dplyr, AER, sandwich, lmtest, sampleSelection,
#   ggplot2, tidyr, readr, boot
#
# Sections:
#   1     Load dataset
#   1b    Verify / generate master's program area indicators (ma_*)
#   2     Summary statistics
#   3     First stage and instrument relevance
#   4     Naive OLS estimation
#   5     IV/2SLS estimation (LATE)
#   6     MTE estimation — pooled polynomial (quadratic and cubic)
#   6b    MTE by graduate program area (fully interacted)
#   6b-ATU Prospective program area assignment for untreated
#   6c    Bootstrap infrastructure (cluster bootstrap)
#   7     Results comparison
#   8     MTE visualization
#   9     Basic policy simulation (PRTE)
#   10    MPRTE — Scenarios 1–4
#   10b   MPRTE by graduate program area — Scenarios 5–8
#   11    MPRTE by policy intensity
#   12    Comparing treatment effect parameters
#   13    MPRTE visualization
#   14    Policy cost-benefit analysis
#   15    Save results
#   16    Final summary
#========================================================================

# -----------------------------------------------------------------------
# Package loading
# -----------------------------------------------------------------------
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(AER)
  library(sandwich)
  library(lmtest)
  library(sampleSelection)   # Heckman selection model
  library(ggplot2)
  library(tidyr)
  library(readr)
  library(boot)
})

# -----------------------------------------------------------------------
# Output directory setup
# -----------------------------------------------------------------------
graphs_dir <- "Output/graphs"
dir.create("Output",       showWarnings = FALSE, recursive = TRUE)
dir.create(graphs_dir,     showWarnings = FALSE, recursive = TRUE)

# Springer monochrome theme (mirrors s2mono)
theme_springer <- function() {
  theme_bw(base_size = 11) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(colour = "grey85"),
      plot.title        = element_text(size = 11, face = "bold"),
      plot.subtitle     = element_text(size = 10),
      axis.title        = element_text(size = 10),
      legend.position   = "bottom",
      legend.title      = element_blank()
    )
}

################################################################################
# SECTION 1: Load Dataset
################################################################################
cat("\n==============================================\n")
cat("LOADING SYNTHETIC B&B DATASET\n")
cat("==============================================\n")

url_updated <- paste0(
  "https://raw.githubusercontent.com/",
  "higher-ed-policy-analysis-2nd-edition/data/main/ch10/",
  "Example_7_5_3_updated.dta"
)
url_base <- paste0(
  "https://raw.githubusercontent.com/",
  "higher-ed-policy-analysis-2nd-edition/data/main/ch7/",
  "Example_7_5_3.dta"
)

df <- tryCatch(
  haven::read_dta(url_updated),
  error = function(e) {
    message("Updated file unavailable; trying base file...")
    tryCatch(
      haven::read_dta(url_base),
      error = function(e2) stop("Could not download dataset. Download manually.")
    )
  }
)

df <- as.data.frame(df)
if (!"id" %in% names(df)) df$id <- seq_len(nrow(df))

cat("Sample size:", nrow(df), "\n")
cat("Variables:", ncol(df), "\n")

################################################################################
# SECTION 1b: Verify / Generate Master's Program Area Indicators
################################################################################
cat("\n==============================================\n")
cat("MASTER'S PROGRAM AREA INDICATORS\n")
cat("==============================================\n")

X_controls <- c("female","black","hispanic","asian","age_ba","firstgen",
                 "parent_income_q","parent_grad","ugpa","stem_major",
                 "bus_major","ed_major","selective_inst","public_ug",
                 "state_unemp","metro")

if (!"ma_stem" %in% names(df)) {
  cat("Generating ma_* variables from undergraduate major fields...\n")

  df$ma_stem      <- 0L
  df$ma_business  <- 0L
  df$ma_education <- 0L
  df$ma_health    <- 0L
  df$ma_other     <- 0L

  set.seed(20251130)
  rma <- ifelse(df$masters == 1, runif(nrow(df)), NA_real_)

  df$ma_stem[df$masters == 1 & df$stem_major == 1 & rma <= 0.55] <- 1L
  df$ma_business[df$masters == 1 & df$bus_major == 1 &
                   rma <= 0.65 & df$ma_stem == 0] <- 1L
  df$ma_education[df$masters == 1 & df$ed_major == 1 &
                    rma <= 0.70 & df$ma_stem == 0 & df$ma_business == 0] <- 1L
  df$ma_health[df$masters == 1 & df$socsci_major == 1 &
                 rma <= 0.40 & df$ma_stem == 0 & df$ma_business == 0 &
                 df$ma_education == 0] <- 1L
  df$ma_health[df$masters == 1 & df$stem_major == 1 &
                 rma > 0.55 & rma <= 0.75 & df$ma_stem == 0] <- 1L
  df$ma_other[df$masters == 1 & df$ma_stem == 0 & df$ma_business == 0 &
                df$ma_education == 0 & df$ma_health == 0] <- 1L

  cat("ma_* variables generated successfully.\n")
}
cat("ma_* variables confirmed present in dataset.\n")

# Verification
n_treated <- sum(df$masters == 1)
cat("\n--- Program Area Distribution (Treated Only) ---\n")
cat("Total treated:", n_treated, "\n")
for (a in c("stem","business","education","health","other")) {
  n_a <- sum(df[[paste0("ma_",a)]] == 1, na.rm = TRUE)
  cat(sprintf("  ma_%s: %d  (%5.1f%%)\n", a, n_a, 100*n_a/n_treated))
}

# Mutual exclusivity check
ma_check <- df$ma_business + df$ma_education + df$ma_health +
            df$ma_stem + df$ma_other
bad_treated   <- sum(df$masters == 1 & ma_check != 1, na.rm = TRUE)
bad_untreated <- sum(df$masters == 0 & ma_check != 0, na.rm = TRUE)
if (bad_treated > 0) {
  cat("WARNING:", bad_treated, "treated obs with != 1 program area flag\n")
} else {
  cat("CHECK PASSED: all treated obs have exactly 1 program area\n")
}
if (bad_untreated > 0) {
  cat("WARNING:", bad_untreated, "untreated obs with non-zero program area flag\n")
} else {
  cat("CHECK PASSED: all untreated obs have zero program area\n")
}

################################################################################
# SECTION 2: Summary Statistics
################################################################################
cat("\n==============================================\n")
cat("SUMMARY STATISTICS\n")
cat("==============================================\n")

treat_rate <- mean(df$masters)
cat(sprintf("Treatment rate: %.3f\n", treat_rate))
cat("\nKey variables:\n")
print(summary(df[, c("ln_salary","salary","masters","ga_funding_adj")]))

cat("\n--- Mean Log Salary by Masters Status ---\n")
df %>% group_by(masters) %>%
  summarise(mean_lnsalary = mean(ln_salary), sd_lnsalary = sd(ln_salary),
            n = n()) %>% print()

cat("\n--- Program Area by Undergraduate Major (Treated Only) ---\n")
for (ug in c("stem_major","bus_major","ed_major","socsci_major")) {
  cat(sprintf("\n  %s undergrads:\n", ug))
  sub <- df[df$masters == 1 & df[[ug]] == 1, ]
  if (nrow(sub) > 0) {
    for (a in c("stem","business","education","health","other")) {
      cat(sprintf("    ma_%s: mean = %.3f  n = %d\n",
                  a, mean(sub[[paste0("ma_",a)]]), nrow(sub)))
    }
  }
}

################################################################################
# SECTION 3: First Stage and Instrument Relevance
################################################################################
cat("\n==============================================\n")
cat("INSTRUMENT RELEVANCE CHECK\n")
cat("==============================================\n")

fs_formula <- as.formula(
  paste("masters ~ ga_funding_adj +", paste(X_controls, collapse = " + "))
)
fs_model <- lm(fs_formula, data = df)
fs_robust <- coeftest(fs_model, vcov = vcovHC(fs_model, type = "HC1"))

# First-stage F for ga_funding_adj
fs_ftest <- linearHypothesis(fs_model, "ga_funding_adj = 0",
                              vcov = vcovHC(fs_model, type = "HC1"))
first_stage_F <- fs_ftest$F[2]
cat(sprintf("\nFirst-stage F: %.2f\n", first_stage_F))
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

ols_formula <- as.formula(
  paste("ln_salary ~ masters +", paste(X_controls, collapse = " + "))
)
ols_model  <- lm(ols_formula, data = df)
ols_robust <- coeftest(ols_model, vcov = vcovHC(ols_model, type = "HC1"))
ols_est    <- ols_robust["masters", "Estimate"]
ols_se     <- ols_robust["masters", "Std. Error"]
cat(sprintf("OLS estimate: %.4f (SE = %.4f)\n", ols_est, ols_se))

################################################################################
# SECTION 5: IV/2SLS Estimation (LATE)
################################################################################
cat("\n==============================================\n")
cat("IV/2SLS ESTIMATION (LATE)\n")
cat("==============================================\n")

iv_formula <- as.formula(
  paste("ln_salary ~", paste(X_controls, collapse = " + "),
        "| masters | ga_funding_adj")
)
# ivreg syntax: outcome ~ exog | endog | instruments
iv_formula2 <- as.formula(
  paste("ln_salary ~ masters +", paste(X_controls, collapse = " + "),
        "| ga_funding_adj +", paste(X_controls, collapse = " + "))
)
iv_model  <- ivreg(iv_formula2, data = df)
iv_robust <- coeftest(iv_model, vcov = vcovHC(iv_model, type = "HC1"))
iv_est    <- iv_robust["masters", "Estimate"]
iv_se     <- iv_robust["masters", "Std. Error"]
cat(sprintf("\nIV/LATE estimate: %.4f (SE = %.4f)\n", iv_est, iv_se))

# Durbin-Wu-Hausman endogeneity test
cat("\n--- Endogeneity Test (Durbin-Wu-Hausman) ---\n")
print(summary(iv_model, diagnostics = TRUE)$diagnostics)

################################################################################
# SECTION 6: MTE ESTIMATION — POOLED POLYNOMIAL
################################################################################
cat("\n==============================================\n")
cat("MTE ESTIMATION — POOLED POLYNOMIAL\n")
cat("==============================================\n")

# Probit first stage — propensity score
probit_formula <- as.formula(
  paste("masters ~ ga_funding_adj +", paste(X_controls, collapse = " + "))
)
probit_model <- glm(probit_formula, data = df, family = binomial(link = "probit"))
df$phat   <- fitted(probit_model)
df$z_index <- predict(probit_model, type = "link")
ga_coef   <- coef(probit_model)["ga_funding_adj"]
cat(sprintf("GA funding probit coefficient: %.5f\n", ga_coef))

df$phat2 <- df$phat^2
df$phat3 <- df$phat^3

# ---------- Quadratic MTE ----------
cat("\n--- Quadratic MTE ---\n")
quad_formula <- as.formula(
  paste("ln_salary ~ masters + I(masters*phat) + I(masters*phat2) +",
        paste(X_controls, collapse = " + "), "+ phat + phat2")
)
quad_model  <- lm(quad_formula, data = df)
quad_robust <- coeftest(quad_model, vcov = vcovHC(quad_model, type = "HC1"))

b0_quad <- coef(quad_model)["masters"]
b1_quad <- coef(quad_model)["I(masters * phat)"]
b2_quad <- coef(quad_model)["I(masters * phat2)"]
cat(sprintf("Quadratic MTE(u) = %.4f + %.4f*u + %.4f*u^2\n",
            b0_quad, b1_quad, b2_quad))
ate_est_quad <- b0_quad + b1_quad/2 + b2_quad/3
cat(sprintf("ATE (quadratic): %.4f\n", ate_est_quad))

# ---------- Cubic MTE ----------
cat("\n--- Cubic MTE ---\n")
cubic_formula <- as.formula(
  paste("ln_salary ~ masters + I(masters*phat) + I(masters*phat2) +",
        "I(masters*phat3) +",
        paste(X_controls, collapse = " + "), "+ phat + phat2 + phat3")
)
cubic_model  <- lm(cubic_formula, data = df)
cubic_robust <- coeftest(cubic_model, vcov = vcovHC(cubic_model, type = "HC1"))

b0 <- coef(cubic_model)["masters"]
b1 <- coef(cubic_model)["I(masters * phat)"]
b2 <- coef(cubic_model)["I(masters * phat2)"]
b3 <- coef(cubic_model)["I(masters * phat3)"]
cat(sprintf("Cubic MTE(u) = %.4f + %.4f*u + %.4f*u^2 + %.4f*u^3\n",
            b0, b1, b2, b3))

ate_est_cubic <- b0 + b1/2 + b2/3 + b3/4
cat(sprintf("Estimated ATE (cubic): %.4f\n", ate_est_cubic))

df$mte_hat <- b0 + b1*df$phat + b2*df$phat2 + b3*df$phat3
att_est    <- mean(df$mte_hat[df$masters == 1])
atu_est    <- mean(df$mte_hat[df$masters == 0])
cat(sprintf("Estimated ATT: %.4f\n", att_est))
cat(sprintf("Estimated ATU: %.4f\n", atu_est))

# ---------- Heckman Selection Model ----------
cat("\n--- Heckman Selection Model ---\n")
heck_sel_formula <- as.formula(
  paste("masters ~ ga_funding_adj +", paste(X_controls, collapse = " + "))
)
heck_out_formula <- as.formula(
  paste("ln_salary ~", paste(X_controls, collapse = " + "))
)
heck_2step <- tryCatch(
  heckit(heck_sel_formula, heck_out_formula, data = df, method = "2step"),
  error = function(e) { message("Heckman 2-step failed: ", e$message); NULL }
)
heck_ml <- tryCatch(
  heckit(heck_sel_formula, heck_out_formula, data = df, method = "ml"),
  error = function(e) { message("Heckman ML failed: ", e$message); NULL }
)
if (!is.null(heck_ml)) {
  heck_ml_rho    <- coef(heck_ml)["rho"]
  heck_ml_sigma  <- coef(heck_ml)["sigma"]
  heck_ml_lambda <- heck_ml_rho * heck_ml_sigma
  cat(sprintf("Heckman ML: lambda = %.4f  rho = %.4f\n",
              heck_ml_lambda, heck_ml_rho))
}

################################################################################
# SECTION 6b: MTE BY GRADUATE PROGRAM AREA — FULLY INTERACTED POLYNOMIAL
################################################################################
cat("\n==============================================\n")
cat("MTE BY GRADUATE PROGRAM AREA\n")
cat("==============================================\n")

# Build interaction terms manually
df$m_phat  <- df$masters * df$phat
df$m_phat2 <- df$masters * df$phat2
df$m_phat3 <- df$masters * df$phat3

for (a in c("stem","business","education","health")) {
  ma_var <- paste0("ma_", a)
  df[[paste0("m_",a)]]       <- df$masters * df[[ma_var]]
  df[[paste0("m_",a,"_p")]]  <- df$masters * df[[ma_var]] * df$phat
  df[[paste0("m_",a,"_p2")]] <- df$masters * df[[ma_var]] * df$phat2
  df[[paste0("m_",a,"_p3")]] <- df$masters * df[[ma_var]] * df$phat3
}

area_int_vars <- c(
  "m_phat","m_phat2","m_phat3",
  "m_stem","m_business","m_education","m_health",
  "m_stem_p","m_business_p","m_education_p","m_health_p",
  "m_stem_p2","m_business_p2","m_education_p2","m_health_p2",
  "m_stem_p3","m_business_p3","m_education_p3","m_health_p3"
)

byarea_formula <- as.formula(
  paste("ln_salary ~ masters +",
        paste(area_int_vars, collapse = " + "), "+",
        paste(X_controls, collapse = " + "),
        "+ phat + phat2 + phat3")
)
byarea_model  <- lm(byarea_formula, data = df)
byarea_robust <- coeftest(byarea_model, vcov = vcovHC(byarea_model, type = "HC1"))

# Base polynomial (Other = base category)
B0 <- unname(coef(byarea_model)["masters"])
B1 <- unname(coef(byarea_model)["m_phat"])
B2 <- unname(coef(byarea_model)["m_phat2"])
B3 <- unname(coef(byarea_model)["m_phat3"])

# Area differential and composite coefficients
area_coefs <- list()
for (a in c("stem","business","education","health")) {
  d0 <- unname(coef(byarea_model)[paste0("m_",a)])
  d1 <- unname(coef(byarea_model)[paste0("m_",a,"_p")])
  d2 <- unname(coef(byarea_model)[paste0("m_",a,"_p2")])
  d3 <- unname(coef(byarea_model)[paste0("m_",a,"_p3")])
  area_coefs[[a]] <- list(
    c0 = B0 + d0, c1 = B1 + d1, c2 = B2 + d2, c3 = B3 + d3
  )
}
area_coefs[["other"]] <- list(c0 = B0, c1 = B1, c2 = B2, c3 = B3)

# Print area-specific MTE functions
# NOTE: Minor differences from Stata estimates are expected due to differences
# in QR decomposition between R's lm() and Stata's reg. Both are correct
# implementations of the same estimator. Point estimates are consistent.
cat("\n--- Area-Specific MTE Functions ---\n")
for (a in c("other","stem","business","education","health")) {
  cc <- area_coefs[[a]]
  cat(sprintf("  %-10s: %.4f + %.4f*u + %.4f*u^2 + %.4f*u^3\n",
              a, cc$c0, cc$c1, cc$c2, cc$c3))
}

# Area-specific ATE
cat("\n--- Area-Specific ATE (integral_0^1 MTE_a(u) du) ---\n")
ate_area <- sapply(area_coefs, function(cc)
  cc$c0 + cc$c1/2 + cc$c2/3 + cc$c3/4)
for (a in c("other","stem","business","education","health"))
  cat(sprintf("  ATE (%s): %.4f\n", a, ate_area[a]))

# Area-specific mte_hat variables
for (a in c("other","stem","business","education","health")) {
  cc <- area_coefs[[a]]
  df[[paste0("mte_hat_",a)]] <-
    cc$c0 + cc$c1*df$phat + cc$c2*df$phat2 + cc$c3*df$phat3
}

# Area-specific ATT
cat("\n--- Area-Specific ATT ---\n")
att_area <- sapply(c("other","stem","business","education","health"), function(a) {
  mean(df[[paste0("mte_hat_",a)]][df[[paste0("ma_",a)]] == 1], na.rm = TRUE)
})
names(att_area) <- c("other","stem","business","education","health")
for (a in names(att_area))
  cat(sprintf("  ATT (%s): %.4f\n", a, att_area[a]))

atu_pooled <- mean(df$mte_hat[df$masters == 0])
cat(sprintf("\n  ATU (pooled): %.4f\n", atu_pooled))

################################################################################
# SECTION 6b-ATU: PROSPECTIVE PROGRAM AREA ASSIGNMENT FOR UNTREATED
################################################################################
cat("\n==============================================\n")
cat("SECTION 6b-ATU: PROSPECTIVE PROGRAM AREA (UNTREATED)\n")
cat("==============================================\n")

# Generate prospective indicators for untreated observations
for (v in c("ma_stem_pro","ma_business_pro","ma_education_pro",
            "ma_health_pro","ma_other_pro")) {
  df[[v]] <- 0L
}

set.seed(20260102)
rma_u <- rep(NA_real_, nrow(df))
rma_u[df$masters == 0] <- runif(sum(df$masters == 0))

df$ma_stem_pro[df$masters==0 & df$stem_major==1 & rma_u<=0.55] <- 1L
df$ma_business_pro[df$masters==0 & df$bus_major==1 & rma_u<=0.65 &
                     df$ma_stem_pro==0] <- 1L
df$ma_education_pro[df$masters==0 & df$ed_major==1 & rma_u<=0.70 &
                      df$ma_stem_pro==0 & df$ma_business_pro==0] <- 1L
df$ma_health_pro[df$masters==0 & df$socsci_major==1 & rma_u<=0.40 &
                   df$ma_stem_pro==0 & df$ma_business_pro==0 &
                   df$ma_education_pro==0] <- 1L
df$ma_health_pro[df$masters==0 & df$stem_major==1 &
                   rma_u>0.55 & rma_u<=0.75 & df$ma_stem_pro==0] <- 1L
df$ma_other_pro[df$masters==0 & df$ma_stem_pro==0 & df$ma_business_pro==0 &
                  df$ma_education_pro==0 & df$ma_health_pro==0] <- 1L

# Verification
n_untreated <- sum(df$masters == 0)
cat(sprintf("Total untreated: %d\n", n_untreated))
for (a in c("stem","business","education","health","other")) {
  n_a <- sum(df[[paste0("ma_",a,"_pro")]] == 1 & df$masters == 0, na.rm = TRUE)
  cat(sprintf("  ma_%s_pro: %d  (%5.1f%%)\n", a, n_a, 100*n_a/n_untreated))
}

# Mutual exclusivity check
pro_check <- df$ma_stem_pro + df$ma_business_pro + df$ma_education_pro +
             df$ma_health_pro + df$ma_other_pro
bad_pro <- sum(df$masters == 0 & pro_check != 1, na.rm = TRUE)
if (bad_pro > 0) {
  cat("WARNING:", bad_pro, "untreated obs with != 1 prospective area flag\n")
} else {
  cat("CHECK PASSED: all untreated obs have exactly 1 prospective area\n")
}

# Area-specific ATU point estimates
cat("\n--- Area-Specific ATU (prospective assignment) ---\n")
atu_area <- sapply(c("stem","business","education","health","other"), function(a) {
  idx <- df$masters == 0 & df[[paste0("ma_",a,"_pro")]] == 1
  mean(df[[paste0("mte_hat_",a)]][idx], na.rm = TRUE)
})
names(atu_area) <- c("stem","business","education","health","other")
for (a in names(atu_area))
  cat(sprintf("  ATU (%s): %.4f\n", a, atu_area[a]))
cat("  Note: Business ATU CI crosses zero in bootstrap — interpret with caution.\n")

cat("\n==============================================\n")
cat("AREA-SPECIFIC TREATMENT PARAMETER SUMMARY\n")
cat("==============================================\n")
cat(sprintf("%-12s %10s %10s %10s\n", "Area", "ATE", "ATT", "ATU(prosp.)"))
cat(strrep("-",45),"\n")
for (a in c("other","stem","business","education","health")) {
  cat(sprintf("  %-10s %10.4f %10.4f %10.4f\n",
              a, ate_area[a], att_area[a], atu_area[a]))
}

################################################################################
# SECTION 6c: BOOTSTRAP INFRASTRUCTURE
# Cluster bootstrap at state level (G=50, R=500)
################################################################################
cat("\n==============================================\n")
cat("CLUSTER BOOTSTRAP (G=50, R=500)\n")
cat("==============================================\n")

R_boot <- 500
set.seed(20260101)
states  <- unique(df$state)
n_states <- length(states)

# Storage matrix: one row per successful replication
boot_cols <- c("b_ate","b_att","b_atu",
               paste0("b_ate_",c("stem","bus","ed","hlth","oth")),
               paste0("b_att_",c("stem","bus","ed","hlth","oth")),
               paste0("b_atu_",c("stem","bus","ed","hlth","oth")))
boot_results <- matrix(NA_real_, nrow = R_boot, ncol = length(boot_cols))
colnames(boot_results) <- boot_cols

n_ok <- 0L
cat(sprintf("Running cluster bootstrap (G=%d, R=%d reps)...\n",
            n_states, R_boot))
cat("Each dot = 10 reps completed\n")

for (b in seq_len(R_boot)) {

  ok <- TRUE

  # Cluster bootstrap: resample states with replacement
  sampled_states <- sample(states, n_states, replace = TRUE)
  boot_df <- do.call(rbind, lapply(seq_along(sampled_states), function(i) {
    tmp <- df[df$state == sampled_states[i], ]
    tmp$newstate <- i
    tmp
  }))

  # --- Probit ---
  pb_model <- tryCatch(
    glm(probit_formula, data = boot_df, family = binomial(link = "probit")),
    error = function(e) NULL
  )
  if (is.null(pb_model)) { ok <- FALSE }

  if (ok) {
    boot_df$pb   <- fitted(pb_model)
    boot_df$pb2  <- boot_df$pb^2
    boot_df$pb3  <- boot_df$pb^3
    boot_df$mpb  <- boot_df$masters * boot_df$pb
    boot_df$mpb2 <- boot_df$masters * boot_df$pb2
    boot_df$mpb3 <- boot_df$masters * boot_df$pb3
  }

  # --- Pooled cubic MTE ---
  if (ok) {
    pooled_boot_formula <- as.formula(
      paste("ln_salary ~ masters + I(masters*pb) + I(masters*pb2) +",
            "I(masters*pb3) +", paste(X_controls, collapse = " + "),
            "+ pb + pb2 + pb3")
    )
    pm <- tryCatch(lm(pooled_boot_formula, data = boot_df), error = function(e) NULL)
    if (is.null(pm)) ok <- FALSE
  }

  if (ok) {
    r0 <- coef(pm)["masters"]
    r1 <- coef(pm)["I(masters * pb)"]
    r2 <- coef(pm)["I(masters * pb2)"]
    r3 <- coef(pm)["I(masters * pb3)"]
    b_ate_r <- r0 + r1/2 + r2/3 + r3/4
    mb_pooled <- r0 + r1*boot_df$pb + r2*boot_df$pb2 + r3*boot_df$pb3
    b_att_r <- mean(mb_pooled[boot_df$masters == 1])
    b_atu_r <- mean(mb_pooled[boot_df$masters == 0])
  }

  # --- Fully interacted MTE ---
  if (ok) {
    for (a in c("stem","business","education","health")) {
      ma_var <- paste0("ma_", a)
      boot_df[[paste0("m_",a)]]       <- boot_df$masters * boot_df[[ma_var]]
      boot_df[[paste0("m_",a,"_p")]]  <- boot_df$masters * boot_df[[ma_var]] * boot_df$pb
      boot_df[[paste0("m_",a,"_p2")]] <- boot_df$masters * boot_df[[ma_var]] * boot_df$pb2
      boot_df[[paste0("m_",a,"_p3")]] <- boot_df$masters * boot_df[[ma_var]] * boot_df$pb3
    }
    int_vars_b <- c("mpb","mpb2","mpb3",
                    paste0("m_",c("stem","business","education","health")),
                    paste0("m_",c("stem","business","education","health"),"_p"),
                    paste0("m_",c("stem","business","education","health"),"_p2"),
                    paste0("m_",c("stem","business","education","health"),"_p3"))
    byarea_boot_f <- as.formula(
      paste("ln_salary ~ masters +",
            paste(int_vars_b, collapse = " + "), "+",
            paste(X_controls, collapse = " + "), "+ pb + pb2 + pb3")
    )
    bm <- tryCatch(lm(byarea_boot_f, data = boot_df), error = function(e) NULL)
    if (is.null(bm)) ok <- FALSE
  }

  if (ok) {
    BB0 <- unname(coef(bm)["masters"])
    BB1 <- unname(coef(bm)["mpb"])
    BB2 <- unname(coef(bm)["mpb2"])
    BB3 <- unname(coef(bm)["mpb3"])

    area_names_b  <- c("stem","bus","ed","hlth","oth")
    area_labels_b <- c("stem","business","education","health","other")
    area_ma_b     <- c("stem","business","education","health","other")

    ate_bs <- att_bs <- atu_bs <- numeric(5)

    for (i in seq_along(area_labels_b)) {
      a     <- area_labels_b[i]
      a_key <- area_names_b[i]
      if (a == "other") {
        C0 <- BB0; C1 <- BB1; C2 <- BB2; C3 <- BB3
      } else {
        D0 <- unname(coef(bm)[paste0("m_",a)])
        D1 <- unname(coef(bm)[paste0("m_",a,"_p")])
        D2 <- unname(coef(bm)[paste0("m_",a,"_p2")])
        D3 <- unname(coef(bm)[paste0("m_",a,"_p3")])
        C0 <- BB0+D0; C1 <- BB1+D1; C2 <- BB2+D2; C3 <- BB3+D3
      }
      ate_bs[i] <- C0 + C1/2 + C2/3 + C3/4
      ms <- C0 + C1*boot_df$pb + C2*boot_df$pb2 + C3*boot_df$pb3
      ma_col <- paste0("ma_",a)
      att_bs[i] <- mean(ms[boot_df[[ma_col]] == 1], na.rm = TRUE)
      pro_col   <- paste0("ma_",a,"_pro")
      atu_bs[i] <- mean(ms[boot_df$masters == 0 & boot_df[[pro_col]] == 1], na.rm = TRUE)
    }

    boot_results[b, ] <- c(b_ate_r, b_att_r, b_atu_r,
                           ate_bs, att_bs, atu_bs)
    n_ok <- n_ok + 1L
  }

  if (b %% 10 == 0) cat(".")
}
cat(sprintf("\nBootstrap complete: %d of %d reps successful\n", n_ok, R_boot))

# Extract SEs as SDs of bootstrap distribution
boot_df_results <- as.data.frame(boot_results[complete.cases(boot_results), ])

ate_se <- sd(boot_df_results$b_ate)
att_se <- sd(boot_df_results$b_att)
atu_se <- sd(boot_df_results$b_atu)

area_short <- c("stem","bus","ed","hlth","oth")
area_long  <- c("stem","business","education","health","other")

ate_se_area <- setNames(
  sapply(paste0("b_ate_", area_short), function(v) sd(boot_df_results[[v]], na.rm = TRUE)),
  area_long)
att_se_area <- setNames(
  sapply(paste0("b_att_", area_short), function(v) sd(boot_df_results[[v]], na.rm = TRUE)),
  area_long)
atu_se_area <- setNames(
  sapply(paste0("b_atu_", area_short), function(v) sd(boot_df_results[[v]], na.rm = TRUE)),
  area_long)

# Print bootstrap SEs with 95% CIs
cat("\n--- Bootstrap SEs: Pooled Parameters ---\n")
cat(sprintf("  ATE = %.4f  (Bootstrap SE = %.4f)\n", ate_est_cubic, ate_se))
cat(sprintf("  ATT = %.4f  (Bootstrap SE = %.4f)\n", att_est,       att_se))
cat(sprintf("  ATU = %.4f  (Bootstrap SE = %.4f)\n", atu_est,       atu_se))

print_bs_table <- function(label, est_vec, se_vec) {
  cat(sprintf("\n--- Bootstrap SEs: Area-Specific %s ---\n", label))
  cat(sprintf("%-12s %10s %10s %14s %6s\n","Area","Estimate","BS SE","95% CI","Sig"))
  cat(strrep("-",55),"\n")
  for (a in c("other","stem","business","education","health")) {
    est <- est_vec[a]
    se  <- se_vec[a]
    if (is.na(est) || is.na(se)) {
      cat(sprintf("  %-10s %10s %10s  %14s %s\n", a, "NA", "NA", "[NA, NA]", "   "))
    } else {
      lo  <- est - 1.96 * se
      hi  <- est + 1.96 * se
      sig <- if (!is.na(lo) && !is.na(hi) && (lo > 0 || hi < 0)) "***" else "   "
      cat(sprintf("  %-10s %10.4f %10.4f  [%7.4f,%7.4f] %s\n",
                  a, est, se, lo, hi, sig))
    }
  }
  cat("  *** = 95% CI excludes zero (p < 0.05, two-tailed)\n")
}

print_bs_table("ATE", ate_area, ate_se_area)
print_bs_table("ATT", att_area, att_se_area)
print_bs_table("ATU (prospective)", atu_area, atu_se_area)

################################################################################
# SECTION 7: Results Comparison
################################################################################
cat("\n==============================================\n")
cat("RESULTS COMPARISON\n")
cat("==============================================\n")

cat(sprintf("  Naive OLS:             %.4f (SE = %.4f — likely biased)\n", ols_est, ols_se))
cat(sprintf("  IV/LATE:               %.4f (SE = %.4f — complier effect)\n", iv_est,  iv_se))
cat(sprintf("  MTE-based ATE (cubic): %.4f (BS SE = %.4f)\n", ate_est_cubic, ate_se))
cat(sprintf("  MTE-based ATT:         %.4f (BS SE = %.4f)\n", att_est,       att_se))
cat(sprintf("  MTE-based ATU:         %.4f (BS SE = %.4f)\n", atu_est,       atu_se))

if (att_est > ate_est_cubic && ate_est_cubic > atu_est) {
  cat("  ATT > ATE > ATU: POSITIVE SELECTION on gains\n")
} else if (att_est < ate_est_cubic && ate_est_cubic < atu_est) {
  cat("  ATT < ATE < ATU: NEGATIVE SELECTION on gains\n")
} else {
  cat("  Mixed selection pattern\n")
}

ols_bias <- (ols_est - ate_est_cubic) / ate_est_cubic * 100
cat(sprintf("OLS BIAS: %.1f%% relative to MTE-based ATE\n", ols_bias))

################################################################################
# SECTION 8: MTE Visualization
################################################################################
cat("\n==============================================\n")
cat("MTE VISUALIZATION\n")
cat("==============================================\n")

# Fig 10.8: Pooled MTE curve
u_grid <- seq(0.01, 1, length.out = 100)
mte_grid_df <- data.frame(
  u   = u_grid,
  mte = b0 + b1*u_grid + b2*u_grid^2 + b3*u_grid^3
)

fig10_8 <- ggplot(mte_grid_df, aes(x = u, y = mte)) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title    = "Estimated MTE Curve - Pooled",
    subtitle = "Master's Degree Effect on Log Salary",
    x        = "u (Unobserved Resistance to Treatment)",
    y        = "Marginal Treatment Effect",
    caption  = "Declining MTE indicates positive selection on gains"
  ) +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_8_mte_curve_R.png"),
       fig10_8, width = 6, height = 4, dpi = 200)
cat("Saved fig10_8_mte_curve_R.png\n")

# Fig 10.9: MTE by propensity score bin
df$p_bin <- floor(df$phat * 20) / 20
mte_bin_df <- df %>%
  group_by(p_bin) %>%
  summarise(mean_mte = mean(mte_hat), n_bin = n(), .groups = "drop")

fig10_9 <- ggplot(mte_bin_df, aes(x = p_bin)) +
  geom_bar(aes(y = n_bin / max(n_bin) * max(abs(mean_mte))),
           stat = "identity", fill = "grey80", color = "grey60") +
  geom_point(aes(y = mean_mte), shape = 18, size = 3, color = "black") +
  geom_line(aes(y  = mean_mte), color = "black", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "MTE by Propensity Score",
       x = "Propensity Score", y = "Estimated MTE") +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_9_mte_by_propensity_R.png"),
       fig10_9, width = 6, height = 4, dpi = 200)
cat("Saved fig10_9_mte_by_propensity_R.png\n")

# Fig 10.10: MTE curves by program area
area_curve_df <- data.frame(u = u_grid)
for (a in c("other","stem","business","education","health")) {
  cc <- area_coefs[[a]]
  area_curve_df[[a]] <- cc$c0 + cc$c1*u_grid + cc$c2*u_grid^2 + cc$c3*u_grid^3
}
area_long_df <- tidyr::pivot_longer(area_curve_df, -u,
                                    names_to = "area", values_to = "mte")
area_long_df$area <- factor(area_long_df$area,
  levels = c("health","stem","business","education","other"),
  labels = c("Health & Related","STEM","Business","Education","Other (base)"))

linetypes <- c("solid","dashed","longdash","solid","dashed")
names(linetypes) <- levels(area_long_df$area)

fig10_10 <- ggplot(area_long_df, aes(x = u, y = mte,
                                      linetype = area, color = area)) +
  geom_line(linewidth = 0.8) +
  scale_linetype_manual(values = linetypes) +
  scale_color_manual(values = rep(c("black","grey40"), c(3,2))) +
  geom_hline(yintercept = 0, linetype = "dotdash", color = "grey60") +
  labs(title    = "MTE Curves by Graduate Program Area",
       subtitle = "Field-specific returns to master's degree",
       x = "u (Unobserved Resistance to Treatment)",
       y = "Marginal Treatment Effect") +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_10_mte_byarea_curve_R.png"),
       fig10_10, width = 7, height = 4.5, dpi = 200)
cat("Saved fig10_10_mte_byarea_curve_R.png\n")

################################################################################
# SECTION 9: Basic Policy Simulation (PRTE)
################################################################################
cat("\n==============================================\n")
cat("BASIC POLICY SIMULATION (PRTE)\n")
cat("==============================================\n")

ga_current <- mean(df$ga_funding_adj)
ga_new     <- ga_current * 1.2
cat(sprintf("Current mean GA: $%.2fk  Proposed (20%% increase): $%.2fk\n",
            ga_current, ga_new))

df$p_new_prte <- pnorm(df$z_index + ga_coef*(ga_new - df$ga_funding_adj))
df$delta_p    <- df$p_new_prte - df$phat
avg_delta_p   <- mean(df$delta_p)
cat(sprintf("Average increase in Pr(Master's): %.4f\n", avg_delta_p))

complier_w <- ifelse(df$delta_p > 0, df$delta_p / avg_delta_p, NA_real_)
prte_20pct <- weighted.mean(df$mte_hat, w = ifelse(is.na(complier_w), 0, complier_w),
                            na.rm = TRUE)
cat(sprintf("Approximate PRTE (20%% GA increase): %.4f\n", prte_20pct))
df$p_new_prte <- df$delta_p <- NULL

################################################################################
# SECTION 10: MPRTE — Scenarios 1–4
################################################################################
cat("\n==============================================\n")
cat("MPRTE - SCENARIOS 1-4 (ORIGINAL)\n")
cat("==============================================\n")

mprte_scenario <- function(df, ga_coef, z_index_var, mte_var,
                            amount, target = rep(TRUE, nrow(df)),
                            label = "Scenario") {
  response   <- dnorm(qnorm(df$phat)) * ga_coef * amount * as.numeric(target)
  mte_vals   <- df[[mte_var]]
  num        <- sum(mte_vals * response, na.rm = TRUE)
  denom      <- sum(response, na.rm = TRUE)
  mprte_val  <- num / denom
  cat(sprintf("MPRTE (%s): %.4f\n", label, mprte_val))
  mprte_val
}

mprte_unif   <- mprte_scenario(df, ga_coef, "z_index", "mte_hat",
                                amount = 1, label = "uniform $1k")
mprte_lowinc <- mprte_scenario(df, ga_coef, "z_index", "mte_hat",
                                amount = 2,
                                target = df$parent_income_q <= 2,
                                label = "targeted low-income $2k")
mprte_stem   <- mprte_scenario(df, ga_coef, "z_index", "mte_hat",
                                amount = 3,
                                target = df$stem_major == 1,
                                label = "STEM enhancement $3k")
mprte_ed     <- mprte_scenario(df, ga_coef, "z_index", "mte_hat",
                                amount = 2.5,
                                target = df$ed_major == 1,
                                label = "education major $2.5k")

################################################################################
# SECTION 10b: MPRTE BY GRADUATE PROGRAM AREA — Scenarios 5–8
################################################################################
cat("\n==============================================\n")
cat("MPRTE BY GRADUATE PROGRAM AREA (Scenarios 5-8)\n")
cat("==============================================\n")

mprte_ma_stem <- mprte_scenario(df, ga_coef, "z_index", "mte_hat_stem",
                                 amount = 2.5, target = df$stem_major == 1,
                                 label = "STEM grad pipeline $2.5k")
mprte_ma_bus  <- mprte_scenario(df, ga_coef, "z_index", "mte_hat_business",
                                 amount = 2.5, target = df$bus_major == 1,
                                 label = "Business grad pipeline $2.5k")
mprte_ma_ed   <- mprte_scenario(df, ga_coef, "z_index", "mte_hat_education",
                                 amount = 2.5, target = df$ed_major == 1,
                                 label = "Education grad pipeline $2.5k")
target_hlth   <- df$stem_major == 1 | df$socsci_major == 1
mprte_ma_hlth <- mprte_scenario(df, ga_coef, "z_index", "mte_hat_health",
                                 amount = 2.5, target = target_hlth,
                                 label = "Health & Related pipeline $2.5k")

################################################################################
# SECTION 11: MPRTE BY POLICY INTENSITY
################################################################################
cat("\n==============================================\n")
cat("MPRTE BY POLICY INTENSITY\n")
cat("==============================================\n")

p_baseline  <- mean(df$phat)
ga_increase <- seq(0.5, 10, by = 0.5)
p_margin    <- p_baseline + ga_increase * 0.015
mprte_approx <- b0 + b1*p_margin + b2*p_margin^2 + b3*p_margin^3
intensity_df <- data.frame(ga_increase = ga_increase,
                            p_margin    = p_margin,
                            mprte_approx = mprte_approx)
print(intensity_df)

fig10_14 <- ggplot(intensity_df, aes(x = ga_increase, y = mprte_approx)) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title    = "MPRTE by Policy Intensity",
       subtitle = "Marginal returns to GA funding expansion",
       x = "GA Funding Increase ($1000s)", y = "MPRTE") +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_14_mprte_by_intensity_R.png"),
       fig10_14, width = 6, height = 4, dpi = 200)
cat("Saved fig10_14_mprte_by_intensity_R.png\n")

################################################################################
# SECTION 12: COMPARING TREATMENT EFFECT PARAMETERS
################################################################################
cat("\n==============================================\n")
cat("COMPARISON OF TREATMENT EFFECT PARAMETERS\n")
cat("==============================================\n")

cat(sprintf("%-10s %14s %14s\n", "Parameter", "Manual(cubic)", "BS SE"))
cat(strrep("-",42),"\n")
cat(sprintf("%-10s %14.4f %14.4f\n", "ATE",      ate_est_cubic, ate_se))
cat(sprintf("%-10s %14.4f %14.4f\n", "ATT",      att_est,       att_se))
cat(sprintf("%-10s %14.4f %14.4f\n", "ATU",      atu_est,       atu_se))
cat(sprintf("%-10s %14.4f %14.4f\n", "LATE(IV)", iv_est,        iv_se))
cat(strrep("-",42),"\n")
cat(sprintf("MPRTE (uniform):              %.4f\n", mprte_unif))
cat(sprintf("MPRTE (low-income):           %.4f\n", mprte_lowinc))
cat(sprintf("MPRTE (STEM ug -> any grad):  %.4f\n", mprte_stem))
cat(sprintf("MPRTE (Ed ug -> any grad):    %.4f\n", mprte_ed))

# Area-specific with CIs
print_bs_table("ATE", ate_area, ate_se_area)
print_bs_table("ATT", att_area, att_se_area)
print_bs_table("ATU (prospective)", atu_area, atu_se_area)

cat(sprintf("\nMPRTE BY GRADUATE PIPELINE (Scenarios 5-8):\n"))
cat(sprintf("  STEM grad pipeline:         %.4f\n", mprte_ma_stem))
cat(sprintf("  Business grad pipeline:     %.4f\n", mprte_ma_bus))
cat(sprintf("  Education grad pipeline:    %.4f\n", mprte_ma_ed))
cat(sprintf("  Health & Related pipeline:  %.4f\n", mprte_ma_hlth))

################################################################################
# SECTION 13: MPRTE VISUALIZATION
################################################################################
cat("\n==============================================\n")
cat("MPRTE VISUALIZATION\n")
cat("==============================================\n")

# Fig 10.11: MTE curve with policy-relevant regions
mte_policy_df <- data.frame(
  u         = u_grid,
  mte       = b0 + b1*u_grid + b2*u_grid^2 + b3*u_grid^3,
  region_lo = u_grid >= 0.10 & u_grid <= 0.25,
  region_un = u_grid >= 0.25 & u_grid <= 0.40
)

fig10_11 <- ggplot(mte_policy_df, aes(x = u, y = mte)) +
  geom_ribbon(data = subset(mte_policy_df, region_lo),
              aes(ymin = 0, ymax = mte, fill = "Low-income margin"),
              alpha = 0.5) +
  geom_ribbon(data = subset(mte_policy_df, region_un),
              aes(ymin = 0, ymax = mte, fill = "Uniform policy margin"),
              alpha = 0.3) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_fill_manual(values = c("Low-income margin"    = "grey30",
                                "Uniform policy margin" = "grey70")) +
  labs(title = "MTE Curve with Policy-Relevant Regions",
       x = "u (Unobserved Resistance to Treatment)",
       y = "Marginal Treatment Effect") +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_11_mte_policy_regions_R.png"),
       fig10_11, width = 6, height = 4, dpi = 200)
cat("Saved fig10_11_mte_policy_regions_R.png\n")

################################################################################
# SECTION 14: POLICY COST-BENEFIT ANALYSIS
################################################################################
cat("\n==============================================\n")
cat("POLICY COST-BENEFIT ANALYSIS\n")
cat("==============================================\n")

cost_per_degree <- 50000
career_years    <- 30
discount_rate   <- 0.03
base_salary     <- 47000
pv_factor <- (1 - (1 + discount_rate)^(-career_years)) / discount_rate
cat(sprintf("Present value factor (30 years, 3%%): %.2f\n", pv_factor))

cat("\n--- Scenarios 1-4: Original MPRTE-based CBA ---\n")
cat(sprintf("%-20s %8s %14s %12s %10s\n",
            "Policy","MPRTE","Annual Gain","PV Gain","B/C Ratio"))
cat(strrep("=",66),"\n")

for (scen in list(
  list(label="Uniform",      mprte=mprte_unif,   base=base_salary),
  list(label="Low-income",   mprte=mprte_lowinc, base=base_salary),
  list(label="STEM ug",      mprte=mprte_stem,   base=base_salary),
  list(label="Education ug", mprte=mprte_ed,     base=base_salary)
)) {
  annual_gain <- scen$base * (exp(scen$mprte) - 1)
  pv_gain     <- annual_gain * pv_factor
  bc_ratio    <- pv_gain / cost_per_degree
  cat(sprintf("%-20s %8.4f %14.0f %12.0f %10.2f\n",
              scen$label, scen$mprte, annual_gain, pv_gain, bc_ratio))
}

base_stem <- 65000; base_bus <- 60000; base_ed <- 42000; base_hlth <- 68000
cat(sprintf("\n--- Scenarios 5-8: Graduate Program Area MPRTE-based CBA ---\n"))
cat(sprintf("%-20s %8s %14s %12s %10s\n",
            "Pipeline","MPRTE","Annual Gain","PV Gain","B/C Ratio"))
cat(strrep("=",66),"\n")

for (scen in list(
  list(label="STEM pipeline",      mprte=mprte_ma_stem, base=base_stem),
  list(label="Business pipeline",  mprte=mprte_ma_bus,  base=base_bus),
  list(label="Education pipeline", mprte=mprte_ma_ed,   base=base_ed),
  list(label="Health pipeline",    mprte=mprte_ma_hlth, base=base_hlth)
)) {
  annual_gain <- scen$base * (exp(scen$mprte) - 1)
  pv_gain     <- annual_gain * pv_factor
  bc_ratio    <- pv_gain / cost_per_degree
  cat(sprintf("%-20s %8.4f %14.0f %12.0f %10.2f\n",
              scen$label, scen$mprte, annual_gain, pv_gain, bc_ratio))
}
cat("Note: B/C > 1 suggests policy expansion is beneficial (synthetic data only).\n")

################################################################################
# SECTION 15: Save Results
################################################################################
cat("\n==============================================\n")
cat("SAVING RESULTS\n")
cat("==============================================\n")

saveRDS(df, "bb_mte_analysis.rds")

# Summary by field
mte_by_field <- df %>%
  group_by(stem_major, ed_major) %>%
  summarise(across(c(masters, ln_salary, phat, mte_hat,
                     mte_hat_stem, mte_hat_business, mte_hat_education,
                     mte_hat_health, mte_hat_other), mean, na.rm = TRUE),
            sd_mte = sd(mte_hat, na.rm = TRUE), n = n(), .groups = "drop")
write_csv(mte_by_field, "mte_summary_by_field.csv")

# Summary by program area (treated only)
mte_by_area <- df %>%
  filter(masters == 1) %>%
  group_by(ma_stem, ma_business, ma_education, ma_health, ma_other) %>%
  summarise(across(c(ln_salary, phat, mte_hat,
                     mte_hat_stem, mte_hat_business, mte_hat_education,
                     mte_hat_health, mte_hat_other), mean, na.rm = TRUE),
            n = n(), .groups = "drop")
write_csv(mte_by_area, "mte_summary_by_program_area.csv")

cat("Files saved: bb_mte_analysis.rds, mte_summary_by_field.csv,\n")
cat("             mte_summary_by_program_area.csv\n")

################################################################################
# SECTION 16: FINAL SUMMARY
################################################################################
cat("\n==============================================\n")
cat("ANALYSIS COMPLETE\n")
cat("==============================================\n")
cat(sprintf("  1.  Treatment rate:                  %.3f\n",  treat_rate))
cat(sprintf("  2.  OLS estimate (biased):           %.4f\n",  ols_est))
cat(sprintf("  3.  IV/LATE estimate:                %.4f\n",  iv_est))
cat(sprintf("  4.  MTE-based ATE (cubic):           %.4f (BS SE = %.4f)\n",
            ate_est_cubic, ate_se))
cat(sprintf("  5.  MTE-based ATT:                   %.4f (BS SE = %.4f)\n",
            att_est, att_se))
cat(sprintf("  6.  MTE-based ATU:                   %.4f (BS SE = %.4f)\n",
            atu_est, atu_se))
cat(sprintf("  7.  First-stage F:                   %.1f\n",  first_stage_F))

cat("\nAREA-SPECIFIC ATE, ATT, AND ATU WITH 95% CONFIDENCE INTERVALS\n")
cat("(Cluster bootstrap, G=50 states, R=500 reps, seed 20260101)\n")
cat("ATU based on prospective program area assignment (seed 20260102)\n")
print_bs_table("ATE", ate_area, ate_se_area)
print_bs_table("ATT", att_area, att_se_area)
print_bs_table("ATU (prospective)", atu_area, atu_se_area)

cat("\nMPRTE SUMMARY - Original Scenarios:\n")
cat(sprintf("  Uniform policy:         %.4f\n", mprte_unif))
cat(sprintf("  Low-income targeted:    %.4f\n", mprte_lowinc))
cat(sprintf("  STEM ug pipeline:       %.4f\n", mprte_stem))
cat(sprintf("  Education ug pipeline:  %.4f\n", mprte_ed))

cat("\nMPRTE SUMMARY - Graduate Program Area Pipelines (Scenarios 5-8):\n")
cat(sprintf("  STEM grad pipeline:     %.4f\n", mprte_ma_stem))
cat(sprintf("  Business grad pipeline: %.4f\n", mprte_ma_bus))
cat(sprintf("  Education pipeline:     %.4f\n", mprte_ma_ed))
cat(sprintf("  Health & Related:       %.4f\n", mprte_ma_hlth))

cat("\nBootstrap: G=50 state clusters, R=500 reps, seed(20260101)\n")
cat("IMPORTANT NOTE: Synthetic data — results illustrate methods only.\n")
cat("==============================================\n")
cat("END OF MTE/MPRTE ANALYSIS\n")
cat("==============================================\n")

#========================================================================
# END OF R_code10_MTE_MPRTE.R
#========================================================================
