# ========================================================================
# Chapter 10 – Sections 10.10–10.16: Marginal Treatment Effects
#              Returns to Master's Degree Completion
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch10
# Author: Marvin A. Titus
# Date: May 2026
# NOTE: Code development was assisted by Claude (Anthropic). The author
# provided specifications and reviewed, tested, and validated all code.
# ========================================================================
# Called by: R_code10.R  (inherits graphs_dir)
# Standalone: can also be sourced directly; uses fallback paths if needed.
#
# Data: synthetic B&B panel
#   Example_7_5_3_updated.csv  (primary; contains pre-generated ma_* vars)
#   Example_7_5_3.csv          (fallback; ma_* generated in Section 1b)
#   NOTE: R reads CSV; Stata .dta files can be imported via haven::read_dta()
#         if the CSV versions are unavailable.
#
# Required packages:
#   AER        — ivreg() for IV/2SLS
#   sampleSelection — heckit() for Heckman selection model
#   sandwich   — vcovHC() robust SEs
#   lmtest     — coeftest()
#   dplyr      — data wrangling
#   tidyr      — pivot_longer() for area-curve plot
#   ggplot2    — publication plots
#   haven      — read_dta() fallback for .dta files
#
# Install once:
#   install.packages(c("AER","sampleSelection","sandwich","lmtest",
#                      "dplyr","tidyr","ggplot2","haven"))
#
# NOTE on mtefe: The Stata mtefe package has no direct R equivalent.
#   This script replicates all mtefe output (ATE, ATT, ATU, LATE)
#   using the manual polynomial MTE estimator (Section 6) with the same
#   cluster bootstrap SEs. mtefe results in the output are labelled
#   "manual poly (mtefe-equivalent)" throughout.
#
# NOTE on wild cluster bootstrap (fwildclusterboot):
#   Replaced here by cluster-robust SEs via sandwich::vcovCL().
#   Full wild cluster bootstrap can be added via the fwildclusterboot
#   package if desired.
# ========================================================================

# -----------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------
suppressPackageStartupMessages({
  library(AER)
  library(sampleSelection)
  library(sandwich)
  library(lmtest)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(haven)
})

# -----------------------------------------------------------------------
# Fallback output paths (overridden when sourced from R_code10.R)
# -----------------------------------------------------------------------
if (!exists("graphs_dir")) {
  if (Sys.info()[["user"]] == "marvi") {
    graphs_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/graphs"
  } else {
    graphs_dir <- "Output/graphs"
  }
  dir.create(file.path(dirname(graphs_dir)), showWarnings = FALSE, recursive = TRUE)
  dir.create(graphs_dir,                    showWarnings = FALSE, recursive = TRUE)
  message("R_code10_MTE_MPRTE.R (standalone): graphs_dir set to ", graphs_dir)
}

# Springer B&W ggplot2 theme (mirrors Stata s2mono)
theme_springer <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(colour = "grey85", linewidth = 0.3),
      plot.title        = element_text(size = base_size,     face = "bold"),
      plot.subtitle     = element_text(size = base_size - 1, colour = "grey30"),
      plot.caption      = element_text(size = base_size - 2, colour = "grey40",
                                       hjust = 0),
      axis.title        = element_text(size = base_size - 1),
      legend.position   = "bottom",
      legend.title      = element_blank()
    )
}

# -----------------------------------------------------------------------
# SECTION 1: Load Dataset
# -----------------------------------------------------------------------
cat("\n==============================================\n")
cat("LOADING SYNTHETIC B&B DATASET\n")
cat("==============================================\n")

# Try updated CSV first (contains pre-generated ma_* variables),
# then fall back to the base CSV, then try .dta via haven.
gh_base <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main"

load_data <- function() {
  # 1. Local updated CSV
  if (file.exists("Example_7_5_3_updated.csv")) {
    message("Loading local Example_7_5_3_updated.csv")
    return(read.csv("Example_7_5_3_updated.csv"))
  }
  # 2. Download updated CSV from GitHub
  tmp <- tryCatch(
    read.csv(paste0(gh_base, "/ch10/Example_7_5_3_updated.csv")),
    error = function(e) NULL
  )
  if (!is.null(tmp)) {
    write.csv(tmp, "Example_7_5_3_updated.csv", row.names = FALSE)
    message("Downloaded Example_7_5_3_updated.csv from GitHub")
    return(tmp)
  }
  # 3. Try updated .dta via haven
  tmp <- tryCatch(
    haven::read_dta(paste0(gh_base, "/ch10/Example_7_5_3_updated.dta")),
    error = function(e) NULL
  )
  if (!is.null(tmp)) {
    message("Loaded Example_7_5_3_updated.dta from GitHub via haven")
    return(as.data.frame(tmp))
  }
  # 4. Base CSV (ch7)
  tmp <- tryCatch(
    read.csv(paste0(gh_base, "/ch7/Example_7_5_3.csv")),
    error = function(e) NULL
  )
  if (!is.null(tmp)) {
    write.csv(tmp, "Example_7_5_3.csv", row.names = FALSE)
    message("Downloaded Example_7_5_3.csv (base) from GitHub. ma_* will be generated in Section 1b.")
    return(tmp)
  }
  # 5. Base .dta via haven
  tmp <- tryCatch(
    haven::read_dta(paste0(gh_base, "/ch7/Example_7_5_3.dta")),
    error = function(e) NULL
  )
  if (!is.null(tmp)) {
    message("Loaded Example_7_5_3.dta (base) via haven. ma_* will be generated in Section 1b.")
    return(as.data.frame(tmp))
  }
  stop(paste0(
    "ERROR: Cannot load dataset.\n",
    "Please download Example_7_5_3_updated.csv (or .dta) from:\n",
    gh_base, "/ch10/\n",
    "and place it in the working directory."
  ))
}

df <- load_data()

if (!"id" %in% names(df)) df$id <- seq_len(nrow(df))

cat(sprintf("Variables: %s\n", paste(names(df), collapse = ", ")))
cat(sprintf("Sample size: %d\n", nrow(df)))

# -----------------------------------------------------------------------
# SECTION 1b: Verify / Generate Master's Program Area Indicators
# -----------------------------------------------------------------------
# Five mutually exclusive categories (IPEDS CIP-based):
#   ma_business   Business, Management, Marketing (CIP 52)
#   ma_education  Education (CIP 13)
#   ma_health     Health Professions & Related (CIP 51)
#   ma_stem       STEM fields (CIPs 11, 14, 15, 26, 27, 40, 41)
#   ma_other      All remaining fields
# ma_* = 0 for all untreated observations (masters == 0).

cat("\n==============================================\n")
cat("MASTER'S PROGRAM AREA INDICATORS\n")
cat("==============================================\n")

if (!"ma_stem" %in% names(df)) {
  message("Generating ma_* variables from undergraduate major fields...")
  set.seed(20251130)

  df$ma_stem      <- 0L
  df$ma_business  <- 0L
  df$ma_education <- 0L
  df$ma_health    <- 0L
  df$ma_other     <- 0L

  rma <- ifelse(df$masters == 1, runif(nrow(df)), NA_real_)

  df$ma_stem[df$masters == 1 & df$stem_major == 1 & !is.na(rma) & rma <= 0.55] <- 1L

  df$ma_business[df$masters == 1 & df$bus_major == 1 & !is.na(rma) &
                   rma <= 0.65 & df$ma_stem == 0] <- 1L

  df$ma_education[df$masters == 1 & df$ed_major == 1 & !is.na(rma) &
                    rma <= 0.70 & df$ma_stem == 0 & df$ma_business == 0] <- 1L

  df$ma_health[df$masters == 1 & df$socsci_major == 1 & !is.na(rma) &
                 rma <= 0.40 & df$ma_stem == 0 &
                 df$ma_business == 0 & df$ma_education == 0] <- 1L

  df$ma_health[df$masters == 1 & df$stem_major == 1 & !is.na(rma) &
                 rma > 0.55 & rma <= 0.75 & df$ma_stem == 0] <- 1L

  df$ma_other[df$masters == 1 &
                df$ma_stem == 0 & df$ma_business == 0 &
                df$ma_education == 0 & df$ma_health == 0] <- 1L

  message("ma_* variables generated successfully.")
}
cat("ma_* variables confirmed present in dataset.\n")

# Verification
n_treated <- sum(df$masters == 1)
cat(sprintf("\n--- Program Area Distribution (Treated Only) ---\nTotal treated: %d\n", n_treated))
for (a in c("stem","business","education","health","other")) {
  n_a <- sum(df[[paste0("ma_", a)]] == 1, na.rm = TRUE)
  cat(sprintf("  ma_%s: %d  (%.1f%%)\n", a, n_a, n_a / n_treated * 100))
}

ma_check <- df$ma_business + df$ma_education + df$ma_health + df$ma_stem + df$ma_other
bad_treated   <- sum(df$masters == 1 & ma_check != 1, na.rm = TRUE)
bad_untreated <- sum(df$masters == 0 & ma_check != 0, na.rm = TRUE)
if (bad_treated   > 0) { warning(sprintf("%d treated obs with != 1 program area flag", bad_treated))
} else {                  cat("CHECK PASSED: all treated obs have exactly 1 program area\n") }
if (bad_untreated > 0) { warning(sprintf("%d untreated obs with non-zero program area flag", bad_untreated))
} else {                  cat("CHECK PASSED: all untreated obs have zero program area\n") }

# -----------------------------------------------------------------------
# SECTION 2: Summary Statistics
# -----------------------------------------------------------------------
cat("\n==============================================\n")
cat("SUMMARY STATISTICS\n")
cat("==============================================\n")

cat(sprintf("Treatment rate: %.3f\n", mean(df$masters)))
treat_rate <- mean(df$masters)

print(table(df$masters))
print(summary(df[, c("ln_salary","salary","masters","ga_funding_adj")]))

# Mean salary by treatment status
cat("\nMean salary by treatment:\n")
print(df %>% group_by(masters) %>%
  summarise(mean_ln_salary = mean(ln_salary, na.rm = TRUE),
            sd_ln_salary   = sd(ln_salary,   na.rm = TRUE),
            mean_salary    = mean(salary,     na.rm = TRUE),
            n              = n(), .groups = "drop"))

# Summary of covariates
cov_vars <- c("female","black","hispanic","asian","age_ba","firstgen",
              "parent_income_q","parent_grad","ugpa","stem_major",
              "bus_major","ed_major","selective_inst","public_ug",
              "state_unemp","metro")
print(summary(df[, cov_vars]))

cat("\n--- Program Area by Undergraduate Major (Treated Only) ---\n")
for (grp in c("stem_major","bus_major","ed_major","socsci_major")) {
  sub <- df %>% filter(masters == 1, .data[[grp]] == 1)
  if (nrow(sub) == 0) next
  cat(sprintf("  %s undergrads (N=%d):\n", grp, nrow(sub)))
  for (a in c("stem","health","business","education","other")) {
    cat(sprintf("    ma_%s: %.3f\n", a, mean(sub[[paste0("ma_",a)]])))
  }
}

cat("\n--- Mean Log Salary by Program Area (Treated Only) ---\n")
for (a in c("stem","business","education","health","other")) {
  sub <- df %>% filter(.data[[paste0("ma_",a)]] == 1)
  if (nrow(sub) == 0) next
  cat(sprintf("  ma_%s: mean ln_salary = %.4f  (N = %d)\n",
              a, mean(sub$ln_salary, na.rm = TRUE), nrow(sub)))
}

# -----------------------------------------------------------------------
# SECTION 3: First-Stage and Instrument Relevance
# -----------------------------------------------------------------------
cat("\n==============================================\n")
cat("INSTRUMENT RELEVANCE CHECK\n")
cat("==============================================\n")

X_controls <- c("female","black","hispanic","asian","age_ba","firstgen",
                "parent_income_q","parent_grad","ugpa","stem_major",
                "bus_major","ed_major","selective_inst","public_ug",
                "state_unemp","metro")
Z_var <- "ga_funding_adj"

fml_fs <- as.formula(paste("masters ~", Z_var, "+",
                            paste(X_controls, collapse = " + ")))
fit_fs  <- lm(fml_fs, data = df)
ct_fs   <- coeftest(fit_fs, vcov = vcovHC(fit_fs, type = "HC1"))
print(ct_fs)

# First-stage F-statistic for the instrument
fit_fs_r   <- lm(as.formula(paste("masters ~",
                                  paste(X_controls, collapse = " + "))),
                 data = df)
first_stage_F <- as.numeric(
  anova(fit_fs_r, fit_fs)[2, "F"]
)
cat(sprintf("\nFirst-stage F: %.2f\n", first_stage_F))
if (first_stage_F > 10) cat("RESULT: Strong instrument (F > 10)\n") else
  cat("WARNING: Potentially weak instrument\n")

# -----------------------------------------------------------------------
# SECTION 4: Naive OLS Estimation
# -----------------------------------------------------------------------
cat("\n==============================================\n")
cat("NAIVE OLS ESTIMATION\n")
cat("==============================================\n")

fml_ols <- as.formula(paste("ln_salary ~ masters +",
                            paste(X_controls, collapse = " + ")))
fit_ols  <- lm(fml_ols, data = df)
ct_ols   <- coeftest(fit_ols, vcov = vcovHC(fit_ols, type = "HC1"))
print(ct_ols)

ols_est <- coef(fit_ols)["masters"]
ols_se  <- sqrt(vcovHC(fit_ols, type = "HC1")["masters","masters"])
cat(sprintf("OLS estimate: %.4f (SE = %.4f)\n", ols_est, ols_se))

# -----------------------------------------------------------------------
# SECTION 5: IV/2SLS Estimation (LATE)
# -----------------------------------------------------------------------
cat("\n==============================================\n")
cat("IV/2SLS ESTIMATION (LATE)\n")
cat("==============================================\n")

fml_iv <- as.formula(paste(
  "ln_salary ~", paste(X_controls, collapse = " + "),
  "+ masters |",
  paste(X_controls, collapse = " + "), "+", Z_var
))
fit_iv  <- ivreg(fml_iv, data = df)
ct_iv   <- coeftest(fit_iv, vcov = vcovHC(fit_iv, type = "HC1"))
print(ct_iv)

iv_est <- coef(fit_iv)["masters"]
iv_se  <- sqrt(vcovHC(fit_iv, type = "HC1")["masters","masters"])
cat(sprintf("\nIV/LATE estimate: %.4f (SE = %.4f)\n", iv_est, iv_se))

# First-stage summary (Wu-Hausman endogeneity test)
cat("\nIV diagnostic summary:\n")
print(summary(fit_iv, diagnostics = TRUE)$diagnostics)

# -----------------------------------------------------------------------
# SECTION 6: MTE Estimation — Pooled Polynomial (Manual)
# -----------------------------------------------------------------------
cat("\n==============================================\n")
cat("MTE ESTIMATION — POOLED POLYNOMIAL\n")
cat("==============================================\n")

# Probit first stage: propensity score
fml_probit <- as.formula(paste("masters ~", Z_var, "+",
                               paste(X_controls, collapse = " + ")))
fit_probit <- glm(fml_probit, data = df, family = binomial(link = "probit"))

df$phat    <- predict(fit_probit, type = "response")
df$z_index <- predict(fit_probit, type = "link")
ga_coef    <- coef(fit_probit)[Z_var]
cat(sprintf("GA funding probit coefficient: %.5f\n", ga_coef))

df$phat2 <- df$phat^2
df$phat3 <- df$phat^3

# -- Quadratic MTE ------------------------------------------------------
cat("\n--- Quadratic MTE ---\n")
fml_quad <- as.formula(paste(
  "ln_salary ~ masters +",
  "I(masters * phat) + I(masters * phat2) +",
  paste(X_controls, collapse = " + "),
  "+ phat + phat2"
))
fit_quad   <- lm(fml_quad, data = df)
ct_quad    <- coeftest(fit_quad, vcov = vcovHC(fit_quad, type = "HC1"))

b0_quad <- coef(fit_quad)["masters"]
b1_quad <- coef(fit_quad)["I(masters * phat)"]
b2_quad <- coef(fit_quad)["I(masters * phat2)"]
cat(sprintf("Quadratic MTE(u) = %.4f + %.4f*u + %.4f*u^2\n",
            b0_quad, b1_quad, b2_quad))
ate_est_quad <- b0_quad + b1_quad / 2 + b2_quad / 3
cat(sprintf("ATE (quadratic): %.4f\n", ate_est_quad))

# -- Cubic MTE ----------------------------------------------------------
cat("\n--- Cubic MTE ---\n")
fml_cubic <- as.formula(paste(
  "ln_salary ~ masters +",
  "I(masters * phat) + I(masters * phat2) + I(masters * phat3) +",
  paste(X_controls, collapse = " + "),
  "+ phat + phat2 + phat3"
))
fit_cubic  <- lm(fml_cubic, data = df)
ct_cubic   <- coeftest(fit_cubic, vcov = vcovHC(fit_cubic, type = "HC1"))

b0 <- coef(fit_cubic)["masters"]
b1 <- coef(fit_cubic)["I(masters * phat)"]
b2 <- coef(fit_cubic)["I(masters * phat2)"]
b3 <- coef(fit_cubic)["I(masters * phat3)"]
cat(sprintf("Cubic MTE(u) = %.4f + %.4f*u + %.4f*u^2 + %.4f*u^3\n",
            b0, b1, b2, b3))

ate_est_cubic <- b0 + b1 / 2 + b2 / 3 + b3 / 4
cat(sprintf("Estimated ATE (cubic): %.4f\n", ate_est_cubic))

df$mte_hat <- b0 + b1 * df$phat + b2 * df$phat2 + b3 * df$phat3

att_est <- mean(df$mte_hat[df$masters == 1], na.rm = TRUE)
atu_est <- mean(df$mte_hat[df$masters == 0], na.rm = TRUE)
cat(sprintf("Estimated ATT: %.4f\n", att_est))
cat(sprintf("Estimated ATU: %.4f\n", atu_est))

# -- Heckman selection model -------------------------------------------
cat("\n--- Heckman Selection Model ---\n")

# Two-step Heckman
fit_heck2 <- heckit(
  selection = fml_probit,
  outcome   = as.formula(paste("ln_salary ~", paste(X_controls, collapse = " + "))),
  data      = df,
  method    = "2step"
)
cat("Heckman two-step:\n")
print(summary(fit_heck2))

# ML Heckman
fit_heck_ml <- heckit(
  selection = fml_probit,
  outcome   = as.formula(paste("ln_salary ~", paste(X_controls, collapse = " + "))),
  data      = df,
  method    = "ml"
)
heck_rho    <- fit_heck_ml$rho
heck_sigma  <- fit_heck_ml$sigma
heck_lambda <- heck_rho * heck_sigma
cat(sprintf("Heckman ML: lambda = %.4f  rho = %.4f\n",
            heck_lambda, heck_rho))

# -----------------------------------------------------------------------
# SECTION 6b: MTE by Graduate Program Area — Fully Interacted Polynomial
# -----------------------------------------------------------------------
# MTE_a(u) = [b0+d0_a] + [b1+d1_a]*u + [b2+d2_a]*u^2 + [b3+d3_a]*u^3
# ma_other is the omitted (base) category.

cat("\n==============================================\n")
cat("MTE BY GRADUATE PROGRAM AREA\n")
cat("==============================================\n")

fml_byarea <- as.formula(paste(
  "ln_salary ~ masters",
  # base polynomial interactions
  "+ I(masters*phat) + I(masters*phat2) + I(masters*phat3)",
  # level differentials
  "+ I(masters*ma_stem) + I(masters*ma_business)",
  "+ I(masters*ma_education) + I(masters*ma_health)",
  # slope differentials (phat)
  "+ I(masters*ma_stem*phat) + I(masters*ma_business*phat)",
  "+ I(masters*ma_education*phat) + I(masters*ma_health*phat)",
  # slope differentials (phat2)
  "+ I(masters*ma_stem*phat2) + I(masters*ma_business*phat2)",
  "+ I(masters*ma_education*phat2) + I(masters*ma_health*phat2)",
  # slope differentials (phat3)
  "+ I(masters*ma_stem*phat3) + I(masters*ma_business*phat3)",
  "+ I(masters*ma_education*phat3) + I(masters*ma_health*phat3)",
  "+", paste(X_controls, collapse = " + "),
  "+ phat + phat2 + phat3"
))
fit_byarea <- lm(fml_byarea, data = df)

# Base polynomial (Other = base category)
B0 <- coef(fit_byarea)["masters"]
B1 <- coef(fit_byarea)["I(masters * phat)"]
B2 <- coef(fit_byarea)["I(masters * phat2)"]
B3 <- coef(fit_byarea)["I(masters * phat3)"]

# Area differential coefficients
areas <- c("stem","business","education","health")
d_coef <- list()
for (a in areas) {
  d_coef[[a]] <- c(
    d0 = coef(fit_byarea)[paste0("I(masters * ma_", a, ")")],
    d1 = coef(fit_byarea)[paste0("I(masters * ma_", a, " * phat)")],
    d2 = coef(fit_byarea)[paste0("I(masters * ma_", a, " * phat2)")],
    d3 = coef(fit_byarea)[paste0("I(masters * ma_", a, " * phat3)")]
  )
}

# Composite area coefficients (base + differential)
c_coef <- list()
c_coef[["other"]] <- c(c0 = B0, c1 = B1, c2 = B2, c3 = B3)
for (a in areas) {
  c_coef[[a]] <- c(
    c0 = B0 + d_coef[[a]]["d0"],
    c1 = B1 + d_coef[[a]]["d1"],
    c2 = B2 + d_coef[[a]]["d2"],
    c3 = B3 + d_coef[[a]]["d3"]
  )
}

cat("\n--- Area-Specific MTE Functions ---\n")
for (a in c("other", areas)) {
  cc <- c_coef[[a]]
  cat(sprintf("  %-12s %.4f + %.4f*u + %.4f*u^2 + %.4f*u^3\n",
              paste0(a,":"), cc[1], cc[2], cc[3], cc[4]))
}

# Area-specific ATE (integral of MTE over [0,1])
ate_area <- sapply(c("other", areas), function(a) {
  cc <- c_coef[[a]]
  cc[1] + cc[2] / 2 + cc[3] / 3 + cc[4] / 4
})

cat("\n--- Area-Specific ATE (integral_0^1 MTE_a(u) du) ---\n")
for (a in names(ate_area))
  cat(sprintf("  ATE (%s): %.4f\n", a, ate_area[a]))

# Area-specific MTE_hat variables
for (a in c("other", areas)) {
  cc <- c_coef[[a]]
  df[[paste0("mte_hat_", a)]] <-
    cc[1] + cc[2] * df$phat + cc[3] * df$phat2 + cc[4] * df$phat3
}

# Area-specific ATT
att_area <- sapply(c("other", areas), function(a) {
  flag <- paste0("ma_", a)
  mean(df[[paste0("mte_hat_", a)]][df[[flag]] == 1], na.rm = TRUE)
})
atu_pooled_untreated <- mean(df$mte_hat[df$masters == 0], na.rm = TRUE)

cat("\n--- Area-Specific ATT ---\n")
for (a in names(att_area))
  cat(sprintf("  ATT (%s): %.4f\n", a, att_area[a]))
cat(sprintf("  ATU (pooled): %.4f\n", atu_pooled_untreated))

cat("\n==============================================\n")
cat("AREA-SPECIFIC TREATMENT PARAMETER SUMMARY\n")
cat("==============================================\n")
cat(sprintf("  %-12s %8s %8s\n", "Area", "ATE", "ATT"))
cat("  ", strrep("-", 32), "\n")
for (a in c("other", areas))
  cat(sprintf("  %-12s %8.4f %8.4f\n", a, ate_area[a], att_area[a]))

# -----------------------------------------------------------------------
# SECTION 6c: Cluster Bootstrap
# -----------------------------------------------------------------------
# Manual cluster bootstrap (G = 50 state clusters, R = 500 reps).
# Bootstraps the full probit + cubic MTE + interacted MTE pipeline.
# SEs = standard deviation of the bootstrap distribution.

cat("\n==============================================\n")
cat("CLUSTER BOOTSTRAP (G=50, R=500)\n")
cat("==============================================\n")

set.seed(20260101)
R_boot  <- 500L
states  <- unique(df$state)
G       <- length(states)
bs_cols <- c("b_ate","b_att","b_atu",
             "b_ate_stem","b_att_stem",
             "b_ate_bus", "b_att_bus",
             "b_ate_ed",  "b_att_ed",
             "b_ate_hlth","b_att_hlth",
             "b_ate_oth", "b_att_oth")
bs_mat  <- matrix(NA_real_, nrow = R_boot, ncol = length(bs_cols),
                  dimnames = list(NULL, bs_cols))

n_ok  <- 0L
cat(sprintf("Running cluster bootstrap (G=%d, R=%d reps)...\n", G, R_boot))
cat("Each dot = 10 reps completed\n")

for (b in seq_len(R_boot)) {

  ok <- TRUE

  # Resample states with replacement
  sampled_states <- sample(states, G, replace = TRUE)
  bs_list <- lapply(seq_along(sampled_states), function(i) {
    d <- df[df$state == sampled_states[i], ]
    d$state <- i   # new cluster ID
    d
  })
  bs_df <- do.call(rbind, bs_list)

  # Probit
  fit_pb_b <- tryCatch(
    glm(fml_probit, data = bs_df, family = binomial(link = "probit")),
    error = function(e) { ok <<- FALSE; NULL }
  )
  if (!ok) { if (b %% 10 == 0) cat("."); next }

  bs_df$pb    <- predict(fit_pb_b, type = "response")
  bs_df$pb2   <- bs_df$pb^2
  bs_df$pb3   <- bs_df$pb^3

  # Pooled cubic MTE
  fml_cub_b <- as.formula(paste(
    "ln_salary ~ masters + I(masters*pb) + I(masters*pb2) + I(masters*pb3) +",
    paste(X_controls, collapse = " + "), "+ pb + pb2 + pb3"
  ))
  fit_cub_b <- tryCatch(
    lm(fml_cub_b, data = bs_df),
    error = function(e) { ok <<- FALSE; NULL }
  )
  if (!ok) { if (b %% 10 == 0) cat("."); next }

  r0 <- coef(fit_cub_b)["masters"]
  r1 <- coef(fit_cub_b)["I(masters * pb)"]
  r2 <- coef(fit_cub_b)["I(masters * pb2)"]
  r3 <- coef(fit_cub_b)["I(masters * pb3)"]
  b_ate_r <- r0 + r1/2 + r2/3 + r3/4
  mb      <- r0 + r1 * bs_df$pb + r2 * bs_df$pb2 + r3 * bs_df$pb3
  b_att_r <- mean(mb[bs_df$masters == 1], na.rm = TRUE)
  b_atu_r <- mean(mb[bs_df$masters == 0], na.rm = TRUE)

  # Fully interacted MTE
  fml_ia_b <- as.formula(paste(
    "ln_salary ~ masters",
    "+ I(masters*pb) + I(masters*pb2) + I(masters*pb3)",
    "+ I(masters*ma_stem) + I(masters*ma_business)",
    "+ I(masters*ma_education) + I(masters*ma_health)",
    "+ I(masters*ma_stem*pb) + I(masters*ma_business*pb)",
    "+ I(masters*ma_education*pb) + I(masters*ma_health*pb)",
    "+ I(masters*ma_stem*pb2) + I(masters*ma_business*pb2)",
    "+ I(masters*ma_education*pb2) + I(masters*ma_health*pb2)",
    "+ I(masters*ma_stem*pb3) + I(masters*ma_business*pb3)",
    "+ I(masters*ma_education*pb3) + I(masters*ma_health*pb3)",
    "+", paste(X_controls, collapse = " + "),
    "+ pb + pb2 + pb3"
  ))
  fit_ia_b <- tryCatch(
    lm(fml_ia_b, data = bs_df),
    error = function(e) { ok <<- FALSE; NULL }
  )
  if (!ok) { if (b %% 10 == 0) cat("."); next }

  BB0 <- coef(fit_ia_b)["masters"]
  BB1 <- coef(fit_ia_b)["I(masters * pb)"]
  BB2 <- coef(fit_ia_b)["I(masters * pb2)"]
  BB3 <- coef(fit_ia_b)["I(masters * pb3)"]

  area_bs <- list()
  for (a in areas) {
    D0 <- coef(fit_ia_b)[paste0("I(masters * ma_", a, ")")]
    D1 <- coef(fit_ia_b)[paste0("I(masters * ma_", a, " * pb)")]
    D2 <- coef(fit_ia_b)[paste0("I(masters * ma_", a, " * pb2)")]
    D3 <- coef(fit_ia_b)[paste0("I(masters * ma_", a, " * pb3)")]
    C0 <- BB0 + D0; C1 <- BB1 + D1; C2 <- BB2 + D2; C3 <- BB3 + D3
    ate_a <- C0 + C1/2 + C2/3 + C3/4
    ms    <- C0 + C1 * bs_df$pb + C2 * bs_df$pb2 + C3 * bs_df$pb3
    flag  <- paste0("ma_", a)
    att_a <- mean(ms[bs_df[[flag]] == 1], na.rm = TRUE)
    area_bs[[a]] <- c(ate = ate_a, att = att_a)
  }
  ate_oth_b <- BB0 + BB1/2 + BB2/3 + BB3/4
  mb_oth    <- BB0 + BB1 * bs_df$pb + BB2 * bs_df$pb2 + BB3 * bs_df$pb3
  att_oth_b <- mean(mb_oth[bs_df$ma_other == 1], na.rm = TRUE)

  bs_mat[b, ] <- c(b_ate_r, b_att_r, b_atu_r,
                   area_bs$stem["ate"],     area_bs$stem["att"],
                   area_bs$business["ate"], area_bs$business["att"],
                   area_bs$education["ate"],area_bs$education["att"],
                   area_bs$health["ate"],   area_bs$health["att"],
                   ate_oth_b,               att_oth_b)
  n_ok <- n_ok + 1L
  if (b %% 10 == 0) cat(".")
}
cat(sprintf("\nBootstrap complete: %d of %d reps successful\n", n_ok, R_boot))

# Extract SEs as SDs of the bootstrap distribution (complete cases only)
bs_df_out <- as.data.frame(bs_mat[complete.cases(bs_mat), ])
ate_se         <- sd(bs_df_out$b_ate,       na.rm = TRUE)
att_se         <- sd(bs_df_out$b_att,       na.rm = TRUE)
atu_se         <- sd(bs_df_out$b_atu,       na.rm = TRUE)
ate_se_stem    <- sd(bs_df_out$b_ate_stem,  na.rm = TRUE)
att_se_stem    <- sd(bs_df_out$b_att_stem,  na.rm = TRUE)
ate_se_business<- sd(bs_df_out$b_ate_bus,   na.rm = TRUE)
att_se_business<- sd(bs_df_out$b_att_bus,   na.rm = TRUE)
ate_se_education<-sd(bs_df_out$b_ate_ed,    na.rm = TRUE)
att_se_education<-sd(bs_df_out$b_att_ed,    na.rm = TRUE)
ate_se_health  <- sd(bs_df_out$b_ate_hlth,  na.rm = TRUE)
att_se_health  <- sd(bs_df_out$b_att_hlth,  na.rm = TRUE)
ate_se_other   <- sd(bs_df_out$b_ate_oth,   na.rm = TRUE)
att_se_other   <- sd(bs_df_out$b_att_oth,   na.rm = TRUE)

# Alias mtefe SEs from cluster bootstrap (same estimator, no separate package)
mtefe_ate_q    <- ate_est_quad
mtefe_att_q    <- att_est
mtefe_atu_q    <- atu_est
mtefe_ate_q_se <- ate_se
mtefe_att_q_se <- att_se
mtefe_atu_q_se <- atu_se
mtefe_late     <- iv_est
mtefe_late_se  <- iv_se

cat("\n--- Cluster-robust SEs (OLS) ---\n")
ct_ols_cl <- coeftest(fit_ols,
                      vcov = vcovCL(fit_ols, cluster = ~state))
cat(sprintf("  OLS masters: %.4f (SE = %.4f)\n",
            ct_ols_cl["masters","Estimate"],
            ct_ols_cl["masters","Std. Error"]))

cat("\n--- Cluster-robust SEs (IV) ---\n")
ct_iv_cl  <- coeftest(fit_iv,
                      vcov = vcovCL(fit_iv, cluster = ~state))
cat(sprintf("  IV masters:  %.4f (SE = %.4f)\n",
            ct_iv_cl["masters","Estimate"],
            ct_iv_cl["masters","Std. Error"]))

cat("\n--- Bootstrap SEs: Pooled Parameters ---\n")
cat(sprintf("  ATE = %.4f  (Bootstrap SE = %.4f)\n", ate_est_cubic, ate_se))
cat(sprintf("  ATT = %.4f  (Bootstrap SE = %.4f)\n", att_est,       att_se))
cat(sprintf("  ATU = %.4f  (Bootstrap SE = %.4f)\n", atu_est,       atu_se))

cat("\n--- Bootstrap SEs: Area-Specific ATE ---\n")
cat(sprintf("  %-12s %10s %8s %20s\n", "Area","Point Est","BS SE","95% CI"))
cat("  ", strrep("-", 54), "\n")
for (a in c("other", areas)) {
  se_a <- get(paste0("ate_se_", a))
  lo   <- ate_area[a] - 1.96 * se_a
  hi   <- ate_area[a] + 1.96 * se_a
  cat(sprintf("  %-12s %10.4f %8.4f  [%.4f, %.4f]\n",
              a, ate_area[a], se_a, lo, hi))
}

cat("\n--- Bootstrap SEs: Area-Specific ATT ---\n")
cat(sprintf("  %-12s %10s %8s\n", "Area","Point Est","BS SE"))
cat("  ", strrep("-", 34), "\n")
for (a in c("other", areas)) {
  se_a <- get(paste0("att_se_", a))
  cat(sprintf("  %-12s %10.4f %8.4f\n", a, att_area[a], se_a))
}

# -----------------------------------------------------------------------
# SECTION 7: Results Comparison
# -----------------------------------------------------------------------
cat("\n==============================================\n")
cat("RESULTS COMPARISON\n")
cat("==============================================\n")

cat(sprintf("  Naive OLS:              %.4f (SE = %.4f — likely biased)\n",   ols_est, ols_se))
cat(sprintf("  IV/LATE:                %.4f (SE = %.4f — complier effect)\n", iv_est,  iv_se))
cat(sprintf("  MTE-based ATE (cubic):  %.4f (BS SE = %.4f)\n", ate_est_cubic, ate_se))
cat(sprintf("  MTE-based ATT:          %.4f (BS SE = %.4f)\n", att_est,       att_se))
cat(sprintf("  MTE-based ATU:          %.4f (BS SE = %.4f)\n", atu_est,       atu_se))
cat(sprintf("  manual poly ATE (quad): %.4f (BS SE = %.4f)\n", mtefe_ate_q,   mtefe_ate_q_se))
cat(sprintf("  manual poly ATT (quad): %.4f (BS SE = %.4f)\n", mtefe_att_q,   mtefe_att_q_se))
cat(sprintf("  manual poly ATU (quad): %.4f (BS SE = %.4f)\n", mtefe_atu_q,   mtefe_atu_q_se))

if (att_est > ate_est_cubic & ate_est_cubic > atu_est) {
  cat("  ATT > ATE > ATU: POSITIVE SELECTION on gains\n")
} else if (att_est < ate_est_cubic & ate_est_cubic < atu_est) {
  cat("  ATT < ATE < ATU: NEGATIVE SELECTION on gains\n")
} else {
  cat("  Mixed selection pattern\n")
}

ols_bias <- (ols_est - ate_est_cubic) / ate_est_cubic * 100
cat(sprintf("OLS BIAS: %.1f%% relative to MTE-based ATE\n", ols_bias))

# -----------------------------------------------------------------------
# SECTION 8: MTE Visualization
# -----------------------------------------------------------------------
cat("\n==============================================\n")
cat("MTE VISUALIZATION\n")
cat("==============================================\n")

# -- fig10_8: Pooled MTE curve ----------------------------------------
u_grid <- seq(0.01, 1.00, length.out = 100)
mte_grid_df <- data.frame(
  u       = u_grid,
  mte_est = b0 + b1 * u_grid + b2 * u_grid^2 + b3 * u_grid^3
)

p_mte <- ggplot(mte_grid_df, aes(x = u, y = mte_est)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_line(colour = "black", linewidth = 0.8) +
  labs(title    = "Estimated MTE Curve - Pooled",
       subtitle = "Master's Degree Effect on Log Salary",
       x        = "u (Unobserved Resistance to Treatment)",
       y        = "Marginal Treatment Effect",
       caption  = "Declining MTE indicates positive selection on gains") +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_8_mte_curve_R.png"),
       p_mte, width = 7, height = 5, dpi = 200)
cat("fig10_8 exported.\n")

# -- mte_by_decile: MTE by propensity score decile --------------------
df$p_decile <- as.integer(cut(df$phat, breaks = quantile(df$phat,
                              probs = seq(0, 1, by = 0.10)),
                              include.lowest = TRUE, labels = FALSE))
dec_df <- df %>%
  group_by(p_decile) %>%
  summarise(mte_mean = mean(mte_hat, na.rm = TRUE),
            mte_sd   = sd(mte_hat,   na.rm = TRUE),
            n        = n(), .groups = "drop")

cat("Estimated MTE by Propensity Score Decile:\n")
print(dec_df)

p_dec <- ggplot(dec_df, aes(x = p_decile, y = mte_mean)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_line(colour = "black", linewidth = 0.6) +
  geom_point(shape = 18, size = 3, colour = "black") +
  labs(title    = "Estimated MTE by Propensity Score Decile",
       subtitle = "Evidence of Treatment Effect Heterogeneity",
       x        = "Propensity Score Decile",
       y        = "Mean Estimated MTE") +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_12_mte_by_decile_R.png"),
       p_dec, width = 7, height = 5, dpi = 200)

# -- fig10_10: MTE curves by program area -----------------------------
area_curves <- data.frame(u = u_grid)
for (a in c("other", areas)) {
  cc <- c_coef[[a]]
  area_curves[[a]] <- cc[1] + cc[2] * u_grid +
                      cc[3] * u_grid^2 + cc[4] * u_grid^3
}

area_long <- area_curves %>%
  pivot_longer(-u, names_to = "area", values_to = "mte") %>%
  mutate(area = factor(area,
                       levels = c("health","stem","business","education","other"),
                       labels = c("Health & Related","STEM","Business",
                                  "Education","Other (base)")))

linetypes <- c("Health & Related" = "solid",
               "STEM"             = "dashed",
               "Business"         = "longdash",
               "Education"        = "solid",
               "Other (base)"     = "dashed")
colours   <- c("Health & Related" = "black",
               "STEM"             = "black",
               "Business"         = "black",
               "Education"        = "grey50",
               "Other (base)"     = "grey50")

p_area <- ggplot(area_long, aes(x = u, y = mte,
                                colour = area, linetype = area)) +
  geom_hline(yintercept = 0, linetype = "dotdash",
             colour = "grey70", linewidth = 0.3) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = colours)   +
  scale_linetype_manual(values = linetypes) +
  labs(title    = "MTE Curves by Graduate Program Area",
       subtitle = "Field-specific returns to master's degree",
       x        = "u (Unobserved Resistance to Treatment)",
       y        = "Marginal Treatment Effect") +
  guides(colour   = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2)) +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_10_mte_byarea_curve_R.png"),
       p_area, width = 7, height = 5, dpi = 200)
cat("fig10_10 exported.\n")

# -----------------------------------------------------------------------
# SECTION 9: Basic Policy Simulation (PRTE)
# -----------------------------------------------------------------------
ga_current <- mean(df$ga_funding_adj, na.rm = TRUE)
ga_new     <- ga_current * 1.2
cat(sprintf("\nCurrent mean GA: $%.2fk  Proposed (20%% increase): $%.2fk\n",
            ga_current, ga_new))

df$p_new_prte   <- pnorm(df$z_index + ga_coef * (ga_new - df$ga_funding_adj))
df$delta_p_prte <- df$p_new_prte - df$phat

avg_delta <- mean(df$delta_p_prte, na.rm = TRUE)
cat(sprintf("Average increase in Pr(Master's): %.4f\n", avg_delta))

sub_comp  <- df[df$delta_p_prte > 0, ]
prte_20pct <- weighted.mean(sub_comp$mte_hat,
                            w = sub_comp$delta_p_prte,
                            na.rm = TRUE)
cat(sprintf("Approximate PRTE (20%% GA increase): %.4f\n", prte_20pct))

df$p_new_prte <- df$delta_p_prte <- NULL

# -----------------------------------------------------------------------
# SECTION 10: MPRTE — Scenarios 1–4 (Original)
# -----------------------------------------------------------------------
cat("\n==============================================\n")
cat("MPRTE - SCENARIOS 1-4 (ORIGINAL)\n")
cat("==============================================\n")

mprte_scenario <- function(df, ga_coef, mte_col, amount, target = NULL) {
  # MPRTE = sum(MTE_i * h_i) / sum(h_i)
  # h_i = phi(Phi^{-1}(phat_i)) * ga_coef * amount [* target_i]
  response <- dnorm(qnorm(df$phat)) * ga_coef * amount
  if (!is.null(target)) response <- response * df[[target]]
  mte_vals <- df[[mte_col]]
  mask     <- if (!is.null(target)) df[[target]] == 1 else rep(TRUE, nrow(df))
  sum(mte_vals[mask] * response[mask], na.rm = TRUE) /
    sum(response[mask], na.rm = TRUE)
}

# Scenario 1: Uniform $1k
mprte_unif   <- mprte_scenario(df, ga_coef, "mte_hat", amount = 1)
cat(sprintf("MPRTE (uniform $1k): %.4f\n", mprte_unif))

# PRTE (discrete $1k)
df$p_new_unif   <- pnorm(df$z_index + ga_coef * 1)
df$delta_p_unif <- df$p_new_unif - df$phat
sub_u <- df[df$delta_p_unif > 0, ]
prte_unif <- weighted.mean(sub_u$mte_hat, w = sub_u$delta_p_unif, na.rm = TRUE)
cat(sprintf("PRTE  (discrete $1k): %.4f\n", prte_unif))
df$p_new_unif <- df$delta_p_unif <- NULL

# Scenario 2: Targeted low-income ($2k)
df$targeted_lowinc <- as.integer(df$parent_income_q <= 2)
mprte_lowinc <- mprte_scenario(df, ga_coef, "mte_hat",
                               amount = 2, target = "targeted_lowinc")
cat(sprintf("MPRTE (targeted low-income): %.4f\n", mprte_lowinc))
df$targeted_lowinc <- NULL

# Scenario 3: STEM GA ($3k)
mprte_stem <- mprte_scenario(df, ga_coef, "mte_hat",
                             amount = 3, target = "stem_major")
cat(sprintf("MPRTE (STEM enhancement): %.4f\n", mprte_stem))

# Scenario 4: Education ($2.5k)
mprte_ed   <- mprte_scenario(df, ga_coef, "mte_hat",
                             amount = 2.5, target = "ed_major")
cat(sprintf("MPRTE (education major support): %.4f\n", mprte_ed))

# -----------------------------------------------------------------------
# SECTION 10b: MPRTE by Graduate Program Area — Scenarios 5–8
# -----------------------------------------------------------------------
cat("\n==============================================\n")
cat("MPRTE BY GRADUATE PROGRAM AREA (Scenarios 5-8)\n")
cat("==============================================\n")

# Scenario 5: STEM grad pipeline (stem_major, $2.5k, mte_hat_stem)
mprte_ma_stem <- mprte_scenario(df, ga_coef, "mte_hat_stem",
                                amount = 2.5, target = "stem_major")
df$p_new_s5   <- pnorm(df$z_index + ga_coef * 2.5 * df$stem_major)
df$delta_p_s5 <- df$p_new_s5 - df$phat
sub_s5        <- df[df$delta_p_s5 > 0 & df$stem_major == 1, ]
prte_ma_stem  <- weighted.mean(sub_s5$mte_hat_stem,
                               w = sub_s5$delta_p_s5, na.rm = TRUE)
cat(sprintf("MPRTE (STEM grad pipeline, $2.5k): %.4f\n", mprte_ma_stem))
cat(sprintf("PRTE  (STEM grad pipeline, $2.5k): %.4f\n", prte_ma_stem))
cat(sprintf("Mean phat for STEM undergrads:     %.4f\n",
            mean(df$phat[df$stem_major == 1], na.rm = TRUE)))
df$p_new_s5 <- df$delta_p_s5 <- NULL

# Scenario 6: Business grad pipeline
mprte_ma_bus <- mprte_scenario(df, ga_coef, "mte_hat_business",
                               amount = 2.5, target = "bus_major")
df$p_new_s6   <- pnorm(df$z_index + ga_coef * 2.5 * df$bus_major)
df$delta_p_s6 <- df$p_new_s6 - df$phat
sub_s6        <- df[df$delta_p_s6 > 0 & df$bus_major == 1, ]
prte_ma_bus   <- weighted.mean(sub_s6$mte_hat_business,
                               w = sub_s6$delta_p_s6, na.rm = TRUE)
cat(sprintf("MPRTE (Business grad pipeline, $2.5k): %.4f\n", mprte_ma_bus))
cat(sprintf("PRTE  (Business grad pipeline, $2.5k): %.4f\n", prte_ma_bus))
df$p_new_s6 <- df$delta_p_s6 <- NULL

# Scenario 7: Education grad pipeline
mprte_ma_ed <- mprte_scenario(df, ga_coef, "mte_hat_education",
                              amount = 2.5, target = "ed_major")
df$p_new_s7   <- pnorm(df$z_index + ga_coef * 2.5 * df$ed_major)
df$delta_p_s7 <- df$p_new_s7 - df$phat
sub_s7        <- df[df$delta_p_s7 > 0 & df$ed_major == 1, ]
prte_ma_ed    <- weighted.mean(sub_s7$mte_hat_education,
                               w = sub_s7$delta_p_s7, na.rm = TRUE)
cat(sprintf("MPRTE (Education grad pipeline, $2.5k): %.4f\n", mprte_ma_ed))
cat(sprintf("PRTE  (Education grad pipeline, $2.5k): %.4f\n", prte_ma_ed))
df$p_new_s7 <- df$delta_p_s7 <- NULL

# Scenario 8: Health & Related pipeline (stem_major OR socsci_major, $2.5k)
df$target_health <- as.integer(df$stem_major == 1 | df$socsci_major == 1)
mprte_ma_hlth <- mprte_scenario(df, ga_coef, "mte_hat_health",
                                amount = 2.5, target = "target_health")
df$p_new_s8   <- pnorm(df$z_index + ga_coef * 2.5 * df$target_health)
df$delta_p_s8 <- df$p_new_s8 - df$phat
sub_s8        <- df[df$delta_p_s8 > 0 & df$target_health == 1, ]
prte_ma_hlth  <- weighted.mean(sub_s8$mte_hat_health,
                               w = sub_s8$delta_p_s8, na.rm = TRUE)
cat(sprintf("MPRTE (Health & Related pipeline, $2.5k): %.4f\n", mprte_ma_hlth))
cat(sprintf("PRTE  (Health & Related pipeline, $2.5k): %.4f\n", prte_ma_hlth))
df$target_health <- df$p_new_s8 <- df$delta_p_s8 <- NULL

# -----------------------------------------------------------------------
# SECTION 11: MPRTE by Policy Intensity
# -----------------------------------------------------------------------
p_baseline <- mean(df$phat, na.rm = TRUE)

intensity_df <- data.frame(ga_increase = seq(0.5, 10.0, by = 0.5)) %>%
  mutate(
    p_margin     = p_baseline + ga_increase * 0.015,
    mprte_approx = b0 + b1 * p_margin + b2 * p_margin^2 + b3 * p_margin^3
  )
print(intensity_df)

p_int <- ggplot(intensity_df, aes(x = ga_increase, y = mprte_approx)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_line(colour = "black", linewidth = 0.8) +
  labs(title    = "MPRTE by Policy Intensity",
       subtitle = "Marginal returns to GA funding expansion",
       x        = "GA Funding Increase ($1000s)",
       y        = "MPRTE") +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_14_mprte_by_intensity_R.png"),
       p_int, width = 7, height = 5, dpi = 200)

# -----------------------------------------------------------------------
# SECTION 12: Comparing Treatment Effect Parameters
# -----------------------------------------------------------------------
cat("\n==============================================\n")
cat("COMPARISON OF TREATMENT EFFECT PARAMETERS\n")
cat("==============================================\n")

cat(sprintf("%-12s %14s %12s %13s %12s\n",
            "Parameter","Manual(cubic)","BS SE(manual)","poly(quad)","BS SE"))
cat(strrep("=", 68), "\n")
cat(sprintf("%-12s %14.4f %12.4f %13.4f %12.4f\n",
            "ATE", ate_est_cubic, ate_se, mtefe_ate_q, mtefe_ate_q_se))
cat(sprintf("%-12s %14.4f %12.4f %13.4f %12.4f\n",
            "ATT", att_est, att_se, mtefe_att_q, mtefe_att_q_se))
cat(sprintf("%-12s %14.4f %12.4f %13.4f %12.4f\n",
            "ATU", atu_est, atu_se, mtefe_atu_q, mtefe_atu_q_se))
cat(sprintf("%-12s %14.4f %12.4f %13.4f %12.4f\n",
            "LATE (IV)", iv_est, iv_se, mtefe_late, mtefe_late_se))
cat(strrep("-", 68), "\n")
cat(sprintf("MPRTE (uniform):                   %.4f\n", mprte_unif))
cat(sprintf("MPRTE (low-income):                %.4f\n", mprte_lowinc))
cat(sprintf("MPRTE (STEM ug -> any grad):       %.4f\n", mprte_stem))
cat(sprintf("MPRTE (Ed ug -> any grad):         %.4f\n", mprte_ed))

cat("\nAREA-SPECIFIC PARAMETERS:\n")
cat(sprintf("  %-12s %8s %8s %12s %8s\n", "Area","ATE","BS SE","ATT","BS SE"))
cat("  ", strrep("-", 52), "\n")
for (a in c("other", areas)) {
  se_ate_a <- get(paste0("ate_se_", a))
  se_att_a <- get(paste0("att_se_", a))
  cat(sprintf("  %-12s %8.4f %8.4f %12.4f %8.4f\n",
              a, ate_area[a], se_ate_a, att_area[a], se_att_a))
}

cat("\nMPRTE BY GRADUATE PIPELINE (Scenarios 5-8):\n")
cat(sprintf("  STEM grad pipeline:         %.4f\n", mprte_ma_stem))
cat(sprintf("  Business grad pipeline:     %.4f\n", mprte_ma_bus))
cat(sprintf("  Education grad pipeline:    %.4f\n", mprte_ma_ed))
cat(sprintf("  Health & Related pipeline:  %.4f\n", mprte_ma_hlth))

# -----------------------------------------------------------------------
# SECTION 13: MPRTE Visualization
# -----------------------------------------------------------------------

# -- fig10_11: MTE curve with policy-relevant regions -----------------
mte_region_df <- data.frame(
  u         = u_grid,
  mte       = b0 + b1 * u_grid + b2 * u_grid^2 + b3 * u_grid^3,
  region_lo = (u_grid >= 0.10 & u_grid <= 0.25),
  region_un = (u_grid >= 0.25 & u_grid <= 0.40)
)

p_reg <- ggplot(mte_region_df, aes(x = u, y = mte)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_ribbon(data = filter(mte_region_df, region_lo),
              aes(ymin = 0, ymax = mte, fill = "Low-income margin"),
              alpha = 0.6) +
  geom_ribbon(data = filter(mte_region_df, region_un),
              aes(ymin = 0, ymax = mte, fill = "Uniform policy margin"),
              alpha = 0.4) +
  geom_line(colour = "black", linewidth = 0.8) +
  scale_fill_manual(values = c("Low-income margin"     = "grey30",
                               "Uniform policy margin" = "grey70")) +
  labs(title    = "MTE Curve with Policy-Relevant Regions",
       x        = "u (Unobserved Resistance to Treatment)",
       y        = "Marginal Treatment Effect") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_11_mte_policy_regions_R.png"),
       p_reg, width = 7, height = 5, dpi = 200)
cat("fig10_11 exported.\n")

# -- fig10_9: MTE by propensity score (bins of width 0.05) ------------
df$p_bin <- floor(df$phat * 20) / 20

pbin_df <- df %>%
  group_by(p_bin) %>%
  summarise(mean_mte = mean(mte_hat, na.rm = TRUE),
            n_bin    = n(), .groups = "drop")

p_pbin <- ggplot(pbin_df) +
  geom_col(aes(x = p_bin, y = n_bin / max(n_bin) *
                 diff(range(pbin_df$mean_mte, na.rm = TRUE))),
           fill = "grey80", colour = "grey60", width = 0.04) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_point(aes(x = p_bin, y = mean_mte),
             shape = 18, size = 3, colour = "black") +
  geom_line(aes(x = p_bin, y = mean_mte),
            colour = "black", linewidth = 0.6) +
  labs(title   = "MTE by Propensity Score",
       x       = "Propensity Score",
       y       = "Estimated MTE",
       caption = "Grey bars = observation count (scaled); diamonds = mean MTE per bin") +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_9_mte_by_propensity_R.png"),
       p_pbin, width = 7, height = 5, dpi = 200)
cat("fig10_9 exported.\n")

# -----------------------------------------------------------------------
# SECTION 14: Policy Cost-Benefit Analysis
# -----------------------------------------------------------------------
cost_per_degree <- 50000
career_years    <- 30
discount_rate   <- 0.03
base_salary     <- 47000
pv_factor <- (1 - (1 + discount_rate)^(-career_years)) / discount_rate
cat(sprintf("\nPresent value factor (30 years, 3%%): %.2f\n", pv_factor))

cat("\n--- Scenarios 1-4: Original MPRTE-based CBA ---\n")
cat(sprintf("%-20s %8s %12s %10s %8s\n",
            "Policy","MPRTE","Annual Gain","PV Gain","B/C"))
cat(strrep("=", 62), "\n")

scens <- list(
  list(name = "Uniform",      val = mprte_unif,   base = base_salary),
  list(name = "Low-income",   val = mprte_lowinc, base = base_salary),
  list(name = "STEM ug",      val = mprte_stem,   base = base_salary),
  list(name = "Education ug", val = mprte_ed,     base = base_salary)
)
for (s in scens) {
  ag  <- s$base * (exp(s$val) - 1)
  pvg <- ag * pv_factor
  bc  <- pvg / cost_per_degree
  cat(sprintf("%-20s %8.4f $%10.0f $%9.0f %8.2f\n",
              s$name, s$val, ag, pvg, bc))
}

base_stem <- 65000; base_bus <- 60000
base_ed   <- 42000; base_hlth <- 68000
cat(sprintf("\n  Base salaries: STEM=$%6.0f Business=$%6.0f Ed=$%6.0f Health=$%6.0f\n",
            base_stem, base_bus, base_ed, base_hlth))

cat("\n--- Scenarios 5-8: Graduate Program Area MPRTE-based CBA ---\n")
cat(sprintf("%-22s %8s %12s %10s %8s\n",
            "Pipeline","MPRTE","Annual Gain","PV Gain","B/C"))
cat(strrep("=", 64), "\n")

area_scens <- list(
  list(name = "STEM pipeline",     val = mprte_ma_stem, base = base_stem),
  list(name = "Business pipeline", val = mprte_ma_bus,  base = base_bus),
  list(name = "Education pipeline",val = mprte_ma_ed,   base = base_ed),
  list(name = "Health pipeline",   val = mprte_ma_hlth, base = base_hlth)
)
for (s in area_scens) {
  ag  <- s$base * (exp(s$val) - 1)
  pvg <- ag * pv_factor
  bc  <- pvg / cost_per_degree
  cat(sprintf("%-22s %8.4f $%10.0f $%9.0f %8.2f\n",
              s$name, s$val, ag, pvg, bc))
}
cat("Note: B/C > 1 suggests policy expansion is beneficial (synthetic data only).\n")

# -----------------------------------------------------------------------
# SECTION 15: Save Results
# -----------------------------------------------------------------------
save_vars <- c("id","masters","ln_salary","salary","phat","mte_hat","z_index",
               "ma_stem","ma_business","ma_education","ma_health","ma_other",
               "mte_hat_stem","mte_hat_business","mte_hat_education",
               "mte_hat_health","mte_hat_other",
               X_controls, Z_var, "state")
save_vars <- save_vars[save_vars %in% names(df)]

saveRDS(df[, save_vars], "bb_mte_analysis.rds")
write.csv(df[, save_vars], "bb_mte_analysis.csv", row.names = FALSE)

# Summary by field
summary_field <- df %>%
  group_by(stem_major, ed_major) %>%
  summarise(
    masters    = mean(masters,  na.rm = TRUE),
    ln_salary  = mean(ln_salary,na.rm = TRUE),
    phat       = mean(phat,     na.rm = TRUE),
    mte_hat    = mean(mte_hat,  na.rm = TRUE),
    across(starts_with("ma_"),       mean, na.rm = TRUE),
    across(starts_with("mte_hat_"),  mean, na.rm = TRUE),
    sd_mte     = sd(mte_hat,    na.rm = TRUE),
    n          = n(),
    .groups    = "drop"
  )
write.csv(summary_field, "mte_summary_by_field.csv", row.names = FALSE)

# Summary by program area (treated only)
summary_area <- df %>%
  filter(masters == 1) %>%
  group_by(ma_stem, ma_business, ma_education, ma_health, ma_other) %>%
  summarise(
    ln_salary  = mean(ln_salary, na.rm = TRUE),
    salary     = mean(salary,    na.rm = TRUE),
    phat       = mean(phat,      na.rm = TRUE),
    mte_hat    = mean(mte_hat,   na.rm = TRUE),
    across(starts_with("mte_hat_"), mean, na.rm = TRUE),
    n          = n(),
    .groups    = "drop"
  )
write.csv(summary_area, "mte_summary_by_program_area.csv", row.names = FALSE)

# -----------------------------------------------------------------------
# SECTION 16: Final Summary
# -----------------------------------------------------------------------
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
cat(sprintf("  7.  poly(quad) ATE:                  %.4f (BS SE = %.4f)\n",
            mtefe_ate_q, mtefe_ate_q_se))
cat(sprintf("  8.  First-stage F:                   %.1f\n",  first_stage_F))

cat("\nAREA-SPECIFIC ATE (program area interacted MTE):\n")
for (a in c("other", areas)) {
  se_a <- get(paste0("ate_se_", a))
  cat(sprintf("  ATE (%s):  %.4f (BS SE = %.4f)\n", a, ate_area[a], se_a))
}

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
cat("Files saved: bb_mte_analysis.rds/.csv, mte_summary_by_field.csv,\n")
cat("             mte_summary_by_program_area.csv\n")
cat("\nIMPORTANT NOTE: Synthetic data — results illustrate methods only.\n")
cat("==============================================\n")
cat("END OF MTE/MPRTE ANALYSIS\n")
cat("==============================================\n")

# ========================================================================
# END OF R_code10_MTE_MPRTE.R
# ========================================================================
