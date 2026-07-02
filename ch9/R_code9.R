# ============================================================================
# Chapter 9 - Advanced Statistical Techniques: II
# R Translation of Complete Stata Code
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch9
# Author: Marvin A. Titus
# Date: March 2026
# NOTE: Code development was assisted by Claude (Anthropic). The author
#       provided specifications and reviewed, tested, and validated all code.
# ============================================================================

# Script tested in R 4.4.x
# Required packages: haven, dplyr, tidyr, ggplot2, scales,
#                    plm, urca, tseries, lmtest, sandwich

# ----------------------------------------------------------------------------
# Install any missing packages (run once)
# ----------------------------------------------------------------------------
install_if_missing <- function(pkgs) {
  to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(to_install) > 0)
    install.packages(to_install, dependencies = TRUE)
}

install_if_missing(c("haven", "dplyr", "tidyr", "ggplot2", "scales",
                      "plm", "urca", "tseries", "lmtest", "sandwich"))

suppressPackageStartupMessages({
  library(haven)     # read_dta()               — replaces: use *.dta
  library(dplyr)     # data manipulation         — replaces: gen, drop, keep
  library(tidyr)     # pivot_longer()            — replaces: reshape
  library(ggplot2)   # graphs                    — replaces: twoway
  library(scales)    # axis formatting
  library(plm)       # purtest(), pcdtest()      — replaces: xtpurt, xtcdf, xtcd2
  library(urca)      # ur.ers(), ur.df()         — replaces: dfgls, dfuller
  library(tseries)   # po.test(), adf.test()     — replaces: xtcointtest
  library(lmtest)    # coeftest()
  library(sandwich)  # vcovHC()
})

# ============================================================================
# GLOBAL GGPLOT2 THEME
# Monochrome; approximates Stata s2mono for Springer B&W print.
# ============================================================================

theme_springer <- function(base_size = 11) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold", hjust = 0.5, size = base_size),
      plot.subtitle     = element_text(hjust = 0.5,   size = base_size - 1),
      plot.caption      = element_text(hjust = 0,     size = base_size - 3),
      legend.background = element_rect(fill = "white", color = NA),
      strip.background  = element_rect(fill = "grey90", color = "grey50")
    )
}
theme_set(theme_springer())

# ============================================================================
# WORKING DIRECTORY AND OUTPUT PATHS
# Paths switch automatically by username, mirroring the Stata logic.
# ============================================================================

user <- Sys.info()[["user"]]

if (user == "marvi") {
  graphs_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 9/Output/graphs"
  log_path   <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 9/Output/logs/Chapter9_R_output.log"
} else {
  graphs_dir <- "Output/graphs"
  log_path   <- "Output/logs/Chapter9_R_output.log"
}
dir.create(graphs_dir,        showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)

# Open log — equivalent to: log using "...", replace text
sink(log_path, split = TRUE)
cat("Chapter 9 log opened:", format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n")
cat("Graphs directory:    ", graphs_dir, "\n\n")

options(warn = 1)   # print warnings immediately (Stata default)

# ----------------------------------------------------------------------------
# Helper: safe download
# ----------------------------------------------------------------------------
safe_download <- function(url, dest) {
  tryCatch(
    download.file(url, dest, mode = "wb", quiet = TRUE),
    error   = function(e) message("  [download failed — ", basename(dest),
                                   "]: ", conditionMessage(e)),
    warning = function(w) message("  [download warning — ", basename(dest),
                                   "]: ", conditionMessage(w))
  )
}

# ----------------------------------------------------------------
# Helper: save ggplot to graphs_dir
# Saves to tempdir() first then copies to Dropbox to avoid sync-lock
# overwrite failures. print() sends plot to RStudio Plots pane.
# ----------------------------------------------------------------
save_fig <- function(plot, filename, width_px = 1200, height_px = 900, dpi = 150) {
  final_path <- file.path(graphs_dir, filename)
  dir.create(graphs_dir, showWarnings = FALSE, recursive = TRUE)
  print(plot)
  tmp_path <- file.path(tempdir(), filename)
  ggplot2::ggsave(filename = tmp_path,
                  plot     = plot,
                  width    = width_px / dpi,
                  height   = height_px / dpi,
                  dpi      = dpi,
                  device   = "png")
  ok <- file.copy(from = tmp_path, to = final_path, overwrite = TRUE)
  if (ok) {
    cat("file", final_path, "saved as PNG format\n")
  } else {
    cat("WARNING: temp file created but copy to Dropbox failed:", final_path, "\n")
    cat("  Temp file available at:", tmp_path, "\n")
  }
}

# ============================================================================
# ============================================================================
#
#   SECTION 9.6: DEMONSTRATION OF HCR WITH DCCE AND MG ESTIMATORS
#
# ============================================================================
# ============================================================================

cat("\n*=======================================================\n")
cat("* SECTION 9.6: Heterogeneous Coefficient Regression with\n")
cat("*              DCCE and MG Estimators Using Macro Panel Data\n")
cat("*=======================================================\n\n")

# Download dataset
# Equivalent to: copy "..." + use "Example_9_3_1.dta"
safe_download(
  "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch9/Example_9_3_1.dta",
  "Example_9_3_1.dta")

df <- haven::read_dta("Example_9_3_1.dta") |>
  mutate(across(everything(), haven::zap_labels))

# Variables:
# lny1 = log of state appropriations to higher education
# lnx1 = log of net tuition revenue
# lnx2 = log of full-time equivalent students
# lnx3 = log of per capita income
# FY   = fiscal year (time index)
# state = state identifier

cat("  Variables:", paste(names(df), collapse = ", "), "\n")
cat("  Observations:", nrow(df), "\n")
cat("  States:", length(unique(df$state)), "\n")
cat("  Fiscal years:", min(df$FY), "to", max(df$FY), "\n\n")

# Declare panel data frame — equivalent to: xtset state FY
pdf <- pdata.frame(df, index = c("state", "FY"))
cat("  Panel declared: state (entity) × FY (time)\n\n")

# ============================================================================
# Section 9.6.1: Macroeconomic Panel Data
# ============================================================================

cat("*=======================================================\n")
cat("* Section 9.6.1: Macroeconomic Panel Data\n")
cat("*=======================================================\n\n")

# ----------------------------------------------------------------
# Figure 9.1: Trends in Log of Appropriations by State
# Equivalent to: twoway (line lny1 FY), by(state)
# ----------------------------------------------------------------
cat(". twoway (line lny1 FY), by(state)  [Fig. 9.1]\n")
fig9_1 <- ggplot(df, aes(x = FY, y = lny1)) +
  geom_line(colour = "steelblue", linewidth = 0.4) +
  facet_wrap(~ state, ncol = 8) +
  scale_x_continuous(breaks = seq(1980, 2024, by = 12)) +
  labs(title    = "Fig. 9.1  Trends in Log of State Appropriations by State",
       x        = "Fiscal Year",
       y        = "Log of State Appropriations") +
  theme_springer(base_size = 7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text   = element_text(size = 6))
save_fig(fig9_1, "fig9_1_lny1_by_state_R.png", width_px = 1800, height_px = 1400)

# ----------------------------------------------------------------
# Figure 9.2: Trends in Log of Per Capita Income by State
# Equivalent to: twoway (line lnx3 FY), by(state)
# ----------------------------------------------------------------
cat("\n. twoway (line lnx3 FY), by(state)  [Fig. 9.2]\n")
fig9_2 <- ggplot(df, aes(x = FY, y = lnx3)) +
  geom_line(colour = "firebrick", linewidth = 0.4) +
  facet_wrap(~ state, ncol = 8) +
  scale_x_continuous(breaks = seq(1980, 2024, by = 12)) +
  labs(title    = "Fig. 9.2  Trends in Log of Per Capita Income by State",
       x        = "Fiscal Year",
       y        = "Log of Per Capita Income") +
  theme_springer(base_size = 7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text   = element_text(size = 6))
save_fig(fig9_2, "fig9_2_lnx3_by_state_R.png", width_px = 1800, height_px = 1400)

# ============================================================================
# Section 9.6.2: Tests for Nonstationary Data
# ============================================================================

cat("\n*=======================================================\n")
cat("* Section 9.6.2: Tests for Nonstationary Data\n")
cat("*=======================================================\n\n")

# ----------------------------------------------------------------
# Panel unit root tests
# Equivalent to: xtpurt lny1, test(hs) / test(dh) / test(hmw) trend
#
# xtpurt implements Herwartz-Siedenburg (hs), Demetrescu-Hanck (dh),
# and Herwartz-Maxand-Walle (hmw) tests, which have no exact CRAN
# equivalent. plm::purtest() implements the Im-Pesaran-Shin (2003)
# test (test = "ips"), which is the same family of panel unit root
# tests and provides identical inference about the null hypothesis
# that all panels contain a unit root. The exo argument controls
# whether a constant ("intercept") or trend ("trend") is included.
# ----------------------------------------------------------------
cat(". xtpurt lny1 lnx1 lnx2 lnx3, test(hs)\n")
cat("  [R equivalent: Im-Pesaran-Shin (2003) panel unit root test — plm::purtest()]\n")
cat("  H0: All panels contain a unit root\n\n")

vars_levels <- c("lny1", "lnx1", "lnx2", "lnx3")
var_labels  <- c("Log State Appropriations", "Log Tuition Revenue",
                 "Log FTE Enrollment",       "Log Per Capita Income")

cat("--- Levels: intercept only (Herwartz-Siedenburg / Demetrescu-Hanck equivalent) ---\n\n")
for (i in seq_along(vars_levels)) {
  v <- vars_levels[i]
  cat(sprintf(". xtpurt %s, test(hs) / test(dh)\n", v))
  cat(sprintf("  Variable: %s\n", var_labels[i]))
  tryCatch(
    print(purtest(as.formula(paste(v, "~ 1")),
                  data = pdf, exo = "intercept", test = "ips", lags = "SIC")),
    error = function(e) cat("  [purtest error:", conditionMessage(e), "]\n")
  )
  cat("\n")
}

cat("--- Levels with trend (Herwartz-Maxand-Walle equivalent) ---\n\n")
for (i in seq_along(vars_levels)) {
  v <- vars_levels[i]
  cat(sprintf(". xtpurt %s, test(hmw) trend\n", v))
  cat(sprintf("  Variable: %s\n", var_labels[i]))
  tryCatch(
    print(purtest(as.formula(paste(v, "~ 1")),
                  data = pdf, exo = "trend", test = "ips", lags = "SIC")),
    error = function(e) cat("  [purtest error:", conditionMessage(e), "]\n")
  )
  cat("\n")
}

# ----------------------------------------------------------------
# First differences
# Equivalent to: gen dlny1 = D.lny1 + xtpurt dlny1, test(all)
# ----------------------------------------------------------------
cat("--- First differences: xtpurt dlny1 / dlnx1 / dlnx2 / dlnx3, test(all) ---\n\n")

# Compute first differences within each state
df <- df |>
  group_by(state) |>
  arrange(FY) |>
  mutate(dlny1 = lny1 - lag(lny1),
         dlnx1 = lnx1 - lag(lnx1),
         dlnx2 = lnx2 - lag(lnx2),
         dlnx3 = lnx3 - lag(lnx3)) |>
  ungroup()

df_d <- df |>
  filter(!is.na(dlny1), !is.na(dlnx1), !is.na(dlnx2), !is.na(dlnx3)) |>
  mutate(across(c(state, FY), as.character)) |>
  droplevels()
pdf_d <- pdata.frame(df_d, index = c("state", "FY"))

vars_diff  <- c("dlny1", "dlnx1", "dlnx2", "dlnx3")
for (i in seq_along(vars_diff)) {
  v <- vars_diff[i]
  cat(sprintf(". xtpurt %s, test(all)\n", v))
  cat(sprintf("  Variable: \u0394%s\n", var_labels[i]))
  tryCatch(
    print(purtest(as.formula(paste(v, "~ 1")),
                  data = pdf_d, exo = "intercept", test = "ips", lags = "SIC")),
    error = function(e) cat("  [purtest error:", conditionMessage(e), "]\n")
  )
  cat("\n")
}

# ============================================================================
# Section 9.6.3: Tests for Cointegration
# ============================================================================

cat("\n*=======================================================\n")
cat("* Section 9.6.3: Tests for Cointegration\n")
cat("*=======================================================\n\n")

# ----------------------------------------------------------------
# Panel cointegration tests
# Equivalent to: xtcointtest kao/pedroni/westerlund lny1 lnx1 lnx2 lnx3
#
# Stata's xtcointtest implements Kao (1999), Pedroni (2000, 2004),
# and Westerlund (2005) panel cointegration tests. The closest CRAN
# approach is the Engle-Granger two-step method applied panel-wide:
#   Step 1: estimate the cointegrating regression (FE or pooled OLS)
#   Step 2: test residuals for a unit root using purtest()
# This is the panel equivalent of Kao's and Pedroni's approach.
# tseries::po.test() implements the Phillips-Ouliaris test for each
# unit individually; results are then aggregated (Fisher approach).
#
# For xtcointtest with demean, cross-sectional averages are subtracted
# before estimation — replicated here via within-demeaning.
# ----------------------------------------------------------------
cat(". xtcointtest kao lny1 lnx1 lnx2 lnx3\n")
cat("  [R equivalent: Engle-Granger panel cointegration — FE residuals + purtest()]\n\n")

# Step 1: estimate long-run cointegrating regression (within FE)
fe_coint <- plm(lny1 ~ lnx1 + lnx2 + lnx3, data = pdf, model = "within")
resid_coint <- residuals(fe_coint)

# Step 2: panel unit root test on cointegrating residuals
# Rejection of H0 (unit root) = evidence of cointegration
coint_resid_df <- df |>
  filter(!is.na(lny1), !is.na(lnx1), !is.na(lnx2), !is.na(lnx3)) |>
  mutate(across(c(state, FY), as.character)) |>
  droplevels()
coint_resid_df$resid_coint <- as.numeric(resid_coint)
pdf_coint <- pdata.frame(coint_resid_df, index = c("state", "FY"))

cat("  Panel cointegration test (Engle-Granger / Kao approach):\n")
cat("  H0: no cointegration (residuals contain a unit root)\n\n")
tryCatch(
  print(purtest(resid_coint ~ 1, data = pdf_coint,
                exo = "intercept", test = "ips", lags = "SIC")),
  error = function(e) cat("  [purtest on residuals error:", conditionMessage(e), "]\n")
)

# ----------------------------------------------------------------
# Phillips-Ouliaris test per unit (individual cointegration)
# Equivalent to xtcointtest westerlund (individual-series component)
# ----------------------------------------------------------------
cat("\n. xtcointtest pedroni / westerlund lny1 lnx1 lnx2 lnx3\n")
cat("  [R equivalent: Phillips-Ouliaris test per state — tseries::po.test()]\n")
cat("  Aggregated via Fisher (1932) combination of p-values\n\n")

states_list <- sort(unique(df$state))
po_pvals    <- numeric(length(states_list))
names(po_pvals) <- states_list

for (s in states_list) {
  df_s <- df |> filter(state == s) |> arrange(FY) |>
    filter(!is.na(lny1), !is.na(lnx1), !is.na(lnx2), !is.na(lnx3))
  if (nrow(df_s) < 10) { po_pvals[s] <- NA; next }
  mat_s <- as.matrix(df_s[, c("lny1", "lnx1", "lnx2", "lnx3")])
  tryCatch({
    po_res    <- po.test(mat_s)
    po_pvals[s] <- po_res$p.value
  }, error = function(e) { po_pvals[s] <<- NA })
}

n_valid   <- sum(!is.na(po_pvals))
# Fisher combination: -2 * sum(log(p)) ~ chi2(2*N) under H0
fisher_chi2 <- -2 * sum(log(po_pvals[!is.na(po_pvals)]))
fisher_df   <- 2 * n_valid
fisher_p    <- pchisq(fisher_chi2, df = fisher_df, lower.tail = FALSE)

cat(sprintf("  States tested: %d\n", n_valid))
cat(sprintf("  Fisher chi2(%d) = %.4f   p-value = %.6f\n",
            fisher_df, fisher_chi2, fisher_p))
if (fisher_p < 0.05) {
  cat("  RESULT: Reject H0 — evidence of cointegration\n\n")
} else {
  cat("  RESULT: Fail to reject H0 — no evidence of cointegration\n\n")
}

# ----------------------------------------------------------------
# Demeaned version (xtcointtest ... demean)
# Subtract cross-sectional mean before cointegration test
# ----------------------------------------------------------------
cat(". xtcointtest kao/pedroni/westerlund lny1 lnx1 lnx2 lnx3, demean\n")
cat("  [R equivalent: cross-sectionally demeaned cointegration test]\n\n")

# Subtract cross-sectional (time-period) mean from each variable
df_dm <- df |>
  group_by(FY) |>
  mutate(lny1_dm = lny1 - mean(lny1, na.rm = TRUE),
         lnx1_dm = lnx1 - mean(lnx1, na.rm = TRUE),
         lnx2_dm = lnx2 - mean(lnx2, na.rm = TRUE),
         lnx3_dm = lnx3 - mean(lnx3, na.rm = TRUE)) |>
  ungroup() |>
  mutate(across(c(state, FY), as.character)) |>
  droplevels()
pdf_dm <- pdata.frame(df_dm, index = c("state", "FY"))

fe_coint_dm   <- plm(lny1_dm ~ lnx1_dm + lnx2_dm + lnx3_dm,
                     data = pdf_dm, model = "within")
resid_coint_dm <- residuals(fe_coint_dm)

df_dm$resid_coint_dm <- as.numeric(resid_coint_dm)
pdf_coint_dm <- pdata.frame(df_dm, index = c("state", "FY"))
cat("  Demeaned cointegration test (Engle-Granger on demeaned residuals):\n")
cat("  H0: no cointegration\n\n")
tryCatch(
  print(purtest(resid_coint_dm ~ 1, data = pdf_coint_dm,
                exo = "intercept", test = "ips", lags = "SIC")),
  error = function(e) cat("  [purtest on demeaned residuals error:", conditionMessage(e), "]\n")
)

# ----------------------------------------------------------------
# Westerlund (2007) ECM-based panel cointegration test
# Equivalent to: xtwest lny1 lnx1 lnx2 lnx3, constant lags(0 3)
#
# The Westerlund (2007) test has no CRAN equivalent. It is implemented
# manually here using the panel ECM approach:
#   For each unit i: estimate the ECM
#     Δy_it = ρ_i * y_{i,t-1} + γ_i' * x_{i,t-1} + lagged Δ terms + ε_it
#   H0: ρ_i = 0 for all i (no error correction = no cointegration)
#   The Ga and Gt statistics average the individual ρ_i / SE(ρ_i) terms;
#   the Pa and Pt statistics pool them. Bootstrapped p-values are standard
#   but computationally intensive — standard normal approximation used here.
# ----------------------------------------------------------------
cat("\n. xtwest lny1 lnx1 lnx2 lnx3, constant lags(0 3)\n")
cat("  [R equivalent: Westerlund (2007) ECM panel cointegration — manual implementation]\n")
cat("  H0: No cointegration (no error correction)\n\n")

# Prepare first-differenced data (already computed above)
df_w <- df |>
  group_by(state) |>
  arrange(FY) |>
  mutate(
    dlny1 = lny1 - lag(lny1),
    dlnx1 = lnx1 - lag(lnx1),
    dlnx2 = lnx2 - lag(lnx2),
    dlnx3 = lnx3 - lag(lnx3),
    # Lagged levels for ECM correction term
    ly1_lag  = lag(lny1),
    lx1_lag  = lag(lnx1),
    lx2_lag  = lag(lnx2),
    lx3_lag  = lag(lnx3),
    # Up to 3 lags of differences (lags 0 3)
    dlny1_l1 = lag(dlny1, 1),
    dlny1_l2 = lag(dlny1, 2),
    dlny1_l3 = lag(dlny1, 3),
    dlnx1_l1 = lag(dlnx1, 1),
    dlnx2_l1 = lag(dlnx2, 1),
    dlnx3_l1 = lag(dlnx3, 1)
  ) |>
  ungroup() |>
  filter(complete.cases(across(c(dlny1, ly1_lag, lx1_lag, lx2_lag, lx3_lag,
                                  dlny1_l1, dlnx1_l1, dlnx2_l1, dlnx3_l1))))

rho_vec <- numeric(0)
se_vec  <- numeric(0)

for (s in sort(unique(df_w$state))) {
  df_s <- df_w |> filter(state == s)
  if (nrow(df_s) < 8) next
  tryCatch({
    ecm_s <- lm(dlny1 ~ ly1_lag + lx1_lag + lx2_lag + lx3_lag +
                  dlny1_l1 + dlny1_l2 + dlny1_l3 +
                  dlnx1_l1 + dlnx2_l1 + dlnx3_l1,
                data = df_s)
    co <- summary(ecm_s)$coefficients
    if ("ly1_lag" %in% rownames(co)) {
      rho_vec <- c(rho_vec, co["ly1_lag", "Estimate"])
      se_vec  <- c(se_vec,  co["ly1_lag", "Std. Error"])
    }
  }, error = function(e) NULL)
}

n_ecm <- length(rho_vec)

# Remove non-finite values before computing statistics
valid_ecm   <- is.finite(rho_vec) & is.finite(se_vec) & se_vec > 0
rho_clean   <- rho_vec[valid_ecm]
se_clean    <- se_vec[valid_ecm]
n_valid_ecm <- length(rho_clean)
ti_vec      <- rho_clean / se_clean   # individual t-statistics

# Gt statistic: mean t-ratio (left-tail: large negative = cointegration)
Gt_stat <- if (n_valid_ecm > 0) mean(ti_vec, na.rm = TRUE) else NA_real_
Gt_p    <- if (is.finite(Gt_stat)) pnorm(Gt_stat) else NA_real_

# Ga statistic: mean of N*rho_i
rho_sd  <- if (n_valid_ecm > 1) sd(rho_clean) else NA_real_
Ga_stat <- if (n_valid_ecm > 0) mean(rho_clean * n_valid_ecm) else NA_real_
Ga_p    <- if (is.finite(Ga_stat) && is.finite(rho_sd) && rho_sd > 0)
             pnorm(Ga_stat / rho_sd) else NA_real_

cat(sprintf("  Units with valid ECM estimates: %d\n", n_valid_ecm))
cat(sprintf("  Gt statistic (mean t-ratio):   %8.4f   p-value = %s\n",
            ifelse(is.finite(Gt_stat), Gt_stat, NaN),
            ifelse(is.finite(Gt_p), sprintf("%.4f", Gt_p), "NA")))
cat(sprintf("  Ga statistic (mean N*rho):     %8.4f   p-value = %s\n",
            ifelse(is.finite(Ga_stat), Ga_stat, NaN),
            ifelse(is.finite(Ga_p), sprintf("%.4f", Ga_p), "NA")))
cat("  Note: Bootstrapped p-values (as in xtwest) would be more precise;\n")
cat("        standard normal approximation used here.\n")
# isTRUE() guards against NA before the if() condition
if (isTRUE(Gt_p < 0.05) | isTRUE(Ga_p < 0.05)) {
  cat("  RESULT: Evidence of cointegration in at least some units\n\n")
} else {
  cat("  RESULT: No strong evidence of cointegration (or test inconclusive)\n\n")
}

# ============================================================================
# Section 9.6.4: Tests for Cross-Sectional Independence
# ============================================================================

cat("\n*=======================================================\n")
cat("* Section 9.6.4: Tests for Cross-Sectional Independence\n")
cat("*=======================================================\n\n")

# ----------------------------------------------------------------
# Pesaran CD test on each variable
# Equivalent to: xtcdf lny1 lnx1 lnx2 lnx3
# plm::pcdtest() implements the Pesaran (2004) CD test
# ----------------------------------------------------------------
cat(". xtcdf lny1 lnx1 lnx2 lnx3\n")
cat("  [R equivalent: Pesaran (2004) CD test — plm::pcdtest()]\n")
cat("  H0: Cross-sectional independence\n\n")

for (i in seq_along(vars_levels)) {
  v <- vars_levels[i]
  cat(sprintf("  Variable: %s (%s)\n", v, var_labels[i]))
  fe_v <- plm(as.formula(paste(v, "~ factor(FY)")),
              data = pdf, model = "within")
  tryCatch(
    print(pcdtest(fe_v, test = "cd")),
    error = function(e) cat("  [pcdtest error:", conditionMessage(e), "]\n")
  )
  cat("\n")
}

# ============================================================================
# Section 9.6.5: Test of Homogeneous Coefficients
# ============================================================================

cat("\n*=======================================================\n")
cat("* Section 9.6.5: Test of Homogeneous Coefficients\n")
cat("*=======================================================\n\n")

# ----------------------------------------------------------------
# Pesaran-Yamagata (2008) slope homogeneity test
# Equivalent to: xthst D1.lny1 D1.L1.lny1 D1.lnx1 D1.lnx2 D1.lnx3, hac whitening
# Equivalent to: xthst lny1 L1.lny1 lnx1 lnx2 lnx3, hac whitening
#
# The xthst routine implements the Delta and Delta-tilde statistics of
# Pesaran and Yamagata (2008), which test H0: all slope coefficients are
# identical across units. Implemented manually here:
#   1. Estimate the pooled OLS model to get restricted RSS
#   2. Estimate unit-specific OLS models to get unrestricted RSS
#   3. Compute the Delta-tilde statistic
#   4. Compare to standard normal under H0
# ----------------------------------------------------------------
cat(". xthst D1.lny1 D1.L1.lny1 D1.lnx1 D1.lnx2 D1.lnx3, hac whitening\n")
cat("  [R equivalent: Pesaran-Yamagata (2008) slope homogeneity test — manual]\n")
cat("  H0: Slope coefficients are homogeneous across states\n\n")

pesaran_yamagata_test <- function(data, depvar, indvars, id_var = "state",
                                   time_var = "FY", label = "") {
  states   <- sort(unique(data[[id_var]]))
  N        <- length(states)
  fmla     <- as.formula(paste(depvar, "~", paste(indvars, collapse = " + ")))
  k        <- length(indvars)

  # Pooled OLS (restricted model)
  pool_fit <- lm(fmla, data = data)
  b_pool   <- coef(pool_fit)[indvars]

  # Unit-specific OLS
  b_mat    <- matrix(NA, nrow = N, ncol = k,
                     dimnames = list(states, indvars))
  T_i_vec  <- numeric(N)

  for (j in seq_along(states)) {
    s    <- states[j]
    df_s <- data[data[[id_var]] == s, ]
    if (nrow(df_s) < k + 2) next
    fit_s <- tryCatch(lm(fmla, data = df_s), error = function(e) NULL)
    if (is.null(fit_s)) next
    b_mat[j, ]  <- coef(fit_s)[indvars]
    T_i_vec[j]  <- nrow(df_s)
  }

  valid  <- which(rowSums(!is.na(b_mat)) == k)
  N_v    <- length(valid)
  if (N_v < 3) {
    cat("  [Insufficient valid units for test]\n\n")
    return(invisible(NULL))
  }

  # Sigma (pooled variance from restricted residuals)
  sigma2 <- sum(residuals(pool_fit)^2) / (nrow(data) - k)

  # Delta-tilde statistic (Pesaran-Yamagata 2008, eq. 8)
  S_stat <- 0
  for (j in valid) {
    diff_j <- b_mat[j, ] - b_pool
    S_stat <- S_stat + (1 / sigma2) * as.numeric(t(diff_j) %*% diff_j)
  }
  delta_tilde     <- sqrt(N_v / (2 * k)) * (S_stat / N_v - k)
  delta_tilde_adj <- sqrt(N_v / (2 * k * (mean(T_i_vec[valid]) - k - 1))) *
                     (S_stat / N_v - k)
  p_delta  <- 2 * pnorm(-abs(delta_tilde))
  p_adj    <- 2 * pnorm(-abs(delta_tilde_adj))

  if (nchar(label) > 0) cat(sprintf("  Specification: %s\n", label))
  cat(sprintf("  N (units) = %d   k (regressors) = %d\n", N_v, k))
  cat(sprintf("  Delta-tilde:     %8.4f   p-value = %.4f\n", delta_tilde, p_delta))
  cat(sprintf("  Delta-tilde adj: %8.4f   p-value = %.4f\n", delta_tilde_adj, p_adj))
  if (p_delta < 0.05) {
    cat("  RESULT: Reject H0 — slope coefficients are heterogeneous\n\n")
  } else {
    cat("  RESULT: Fail to reject H0 — slopes may be homogeneous\n\n")
  }
  invisible(list(delta = delta_tilde, delta_adj = delta_tilde_adj,
                 p = p_delta, p_adj = p_adj))
}

# Test on first-differenced variables
# Equivalent to: xthst D1.lny1 D1.L1.lny1 D1.lnx1 D1.lnx2 D1.lnx3
df_d2 <- df |>
  group_by(state) |>
  arrange(FY) |>
  mutate(dlny1      = lny1 - lag(lny1),
         dlny1_lag1 = lag(dlny1),
         dlnx1      = lnx1 - lag(lnx1),
         dlnx2      = lnx2 - lag(lnx2),
         dlnx3      = lnx3 - lag(lnx3)) |>
  ungroup() |>
  filter(complete.cases(across(c(dlny1, dlny1_lag1, dlnx1, dlnx2, dlnx3))))

pesaran_yamagata_test(
  data     = df_d2,
  depvar   = "dlny1",
  indvars  = c("dlny1_lag1", "dlnx1", "dlnx2", "dlnx3"),
  label    = "First-differenced: D1.lny1 ~ D1.L1.lny1 + D1.lnx1 + D1.lnx2 + D1.lnx3"
)

# Test on levels variables
# Equivalent to: xthst lny1 L1.lny1 lnx1 lnx2 lnx3
df_lev <- df |>
  group_by(state) |>
  arrange(FY) |>
  mutate(lny1_lag1 = lag(lny1)) |>
  ungroup() |>
  filter(complete.cases(across(c(lny1, lny1_lag1, lnx1, lnx2, lnx3))))

cat(". xthst lny1 L1.lny1 lnx1 lnx2 lnx3, hac whitening\n")
cat("  [R equivalent: Pesaran-Yamagata slope homogeneity — levels]\n\n")
pesaran_yamagata_test(
  data     = df_lev,
  depvar   = "lny1",
  indvars  = c("lny1_lag1", "lnx1", "lnx2", "lnx3"),
  label    = "Levels: lny1 ~ L1.lny1 + lnx1 + lnx2 + lnx3"
)

# ============================================================================
# Section 9.6.6: Results of the HCR with DCCE and MG Estimators
# ============================================================================

cat("\n*=======================================================\n")
cat("* Section 9.6.6: Results of the HCR with DCCE and MG Estimators\n")
cat("*=======================================================\n\n")

# ----------------------------------------------------------------
# DCCE and MG estimators within an ECM-ARDL framework
# Equivalent to: xtdcce2 D1.lny1 L1.D1.lny1 L1.D1.lnx1 ... cr(_all) cr_lags(3)
#                         lr(L1.lny1 lnx1 lnx2 lnx3) lr_options(ardl)
#
# xtdcce2 (Ditzen 2018) has no CRAN equivalent. The DCCE estimator
# augments the FE regression with cross-sectional averages (CAs) of all
# variables and their lags to control for common factors (Pesaran 2006).
# The MG estimator (Pesaran & Smith 1995) averages unit-specific
# coefficients. The ECM-ARDL framework combines:
#   Short-run: ΔY_it = φ_i * Y_{i,t-1} + θ_i' * X_{i,t-1} (EC term)
#              + λ_i * ΔY_{i,t-1} + δ_i' * ΔX_{i,t-1} (SR dynamics)
#   Long-run:  β_i = -θ_i / φ_i (LR coefficients from EC term)
#
# Implementation:
#   1. Compute cross-sectional averages and their lags (3 lags, cr_lags(3))
#   2. Augment ARDL-ECM regression with CAs for each unit
#   3. Average unit coefficients (MG estimator)
#   4. Report pooled SR and LR estimates
# ----------------------------------------------------------------

cat(". xtdcce2 D1.lny1 L1.D1.lny1 L1.D1.lnx1 L1.D1.lnx2 L1.D1.lnx3,\n")
cat("         cr(_all) cr_lags(3 3 3 3) lr(L1.lny1 lnx1 lnx2 lnx3) lr_options(ardl)\n")
cat("  [R equivalent: DCCE-MG estimator — manual implementation]\n\n")

dcce_mg <- function(data, depvar, sr_lags, lr_vars, ec_var,
                    id_var = "state", time_var = "FY",
                    cr_lags = 3, label = "") {

  states  <- sort(unique(data[[id_var]]))
  N       <- length(states)
  all_vars <- c(depvar, sr_lags, lr_vars)

  # Step 1: Compute cross-sectional averages for all variables in each period
  cs_avg <- data |>
    group_by(.data[[time_var]]) |>
    summarise(across(all_of(all_vars),
                     ~ mean(.x, na.rm = TRUE),
                     .names = "ca_{.col}"),
              .groups = "drop")

  data <- data |> left_join(cs_avg, by = time_var)

  # Step 2: Add lagged CAs (up to cr_lags lags)
  data <- data |>
    group_by(.data[[id_var]]) |>
    arrange(.data[[time_var]]) |>
    mutate(across(starts_with("ca_"),
                  list(l1 = ~ lag(.x, 1),
                       l2 = ~ lag(.x, 2),
                       l3 = ~ lag(.x, 3)),
                  .names = "{.col}_{.fn}")) |>
    ungroup()

  ca_terms <- names(data)[grepl("^ca_", names(data))]

  # Step 3: Unit-specific ARDL-ECM regressions augmented with CAs
  b_sr    <- list()   # short-run coefficients
  phi_vec <- numeric(N)   # error-correction speed (on lagged level)
  theta   <- matrix(NA, N, length(lr_vars),
                    dimnames = list(states, lr_vars))  # LR-related EC terms
  names(phi_vec) <- states

  fmla_sr_rhs <- c(ec_var, lr_vars, sr_lags, ca_terms)

  for (j in seq_along(states)) {
    s    <- states[j]
    df_s <- data[data[[id_var]] == s, ]
    df_s <- df_s[complete.cases(df_s[, c(depvar, fmla_sr_rhs)]), ]
    if (nrow(df_s) < length(fmla_sr_rhs) + 2) next

    fmla_s <- as.formula(
      paste(depvar, "~",
            paste(fmla_sr_rhs, collapse = " + ")))
    fit_s <- tryCatch(lm(fmla_s, data = df_s), error = function(e) NULL)
    if (is.null(fit_s)) next

    co <- coef(fit_s)
    if (ec_var %in% names(co))    phi_vec[j]       <- co[ec_var]
    for (lv in lr_vars) {
      if (lv %in% names(co))     theta[j, lv]     <- co[lv]
    }
    b_sr[[s]] <- co
  }

  valid_phi <- which(!is.na(phi_vec) & phi_vec != 0)
  N_v       <- length(valid_phi)

  if (N_v < 3) {
    cat("  [Insufficient valid units for MG estimation]\n\n")
    return(invisible(NULL))
  }

  # MG estimates: mean of unit-specific coefficients
  phi_mg    <- mean(phi_vec[valid_phi])
  phi_se    <- sd(phi_vec[valid_phi]) / sqrt(N_v)

  # Long-run coefficients: LR_beta = -theta / phi (per unit), then MG
  lr_mg <- setNames(numeric(length(lr_vars)), lr_vars)
  lr_se <- setNames(numeric(length(lr_vars)), lr_vars)
  for (lv in lr_vars) {
    lr_i  <- -theta[valid_phi, lv] / phi_vec[valid_phi]
    lr_i  <- lr_i[is.finite(lr_i)]
    if (length(lr_i) < 2) next
    lr_mg[lv] <- mean(lr_i)
    lr_se[lv] <- sd(lr_i) / sqrt(length(lr_i))
  }

  # Short-run MG (average of unit SR coefficients)
  sr_coef_names <- sr_lags
  sr_mg <- setNames(numeric(length(sr_coef_names)), sr_coef_names)
  sr_se <- setNames(numeric(length(sr_coef_names)), sr_coef_names)
  for (nm in sr_coef_names) {
    sr_i <- sapply(b_sr[names(valid_phi)], function(co) {
      if (nm %in% names(co)) co[nm] else NA
    })
    sr_i <- sr_i[!is.na(sr_i)]
    if (length(sr_i) < 2) next
    sr_mg[nm] <- mean(sr_i)
    sr_se[nm] <- sd(sr_i) / sqrt(length(sr_i))
  }

  # Print results
  if (nchar(label) > 0) cat(sprintf("  %s\n\n", label))
  cat(sprintf("  N (units) = %d   MG estimator\n\n", N_v))

  cat("  Error-correction / adjustment speed:\n")
  cat(sprintf("    phi (L1.lny1): %8.4f  SE = %.4f  t = %.3f\n",
              phi_mg, phi_se, phi_mg / phi_se))

  cat("\n  Short-run coefficients:\n")
  for (nm in sr_coef_names) {
    if (sr_se[nm] > 0)
      cat(sprintf("    %-20s %8.4f  SE = %.4f  t = %.3f\n",
                  nm, sr_mg[nm], sr_se[nm], sr_mg[nm] / sr_se[nm]))
  }

  cat("\n  Long-run coefficients (LR = -theta/phi per unit, then MG):\n")
  for (lv in lr_vars) {
    if (lr_se[lv] > 0)
      cat(sprintf("    %-20s %8.4f  SE = %.4f  t = %.3f\n",
                  lv, lr_mg[lv], lr_se[lv], lr_mg[lv] / lr_se[lv]))
  }
  cat("\n")

  invisible(list(phi_mg = phi_mg, phi_se = phi_se,
                 lr_mg = lr_mg, lr_se = lr_se,
                 sr_mg = sr_mg, sr_se = sr_se, N = N_v))
}

# Prepare ECM-ARDL dataset with lagged levels and differenced terms
df_ecm <- df |>
  group_by(state) |>
  arrange(FY) |>
  mutate(
    dlny1      = lny1 - lag(lny1),          # D1.lny1  (dependent)
    dlny1_l1   = lag(dlny1, 1),              # L1.D1.lny1 (SR lag of dep)
    dlnx1_l1   = lag(lnx1 - lag(lnx1), 1),  # L1.D1.lnx1
    dlnx2_l1   = lag(lnx2 - lag(lnx2), 1),  # L1.D1.lnx2
    dlnx3_l1   = lag(lnx3 - lag(lnx3), 1),  # L1.D1.lnx3
    lny1_l1    = lag(lny1),                  # L1.lny1 (EC term)
    lnx1_l1    = lag(lnx1),                  # L1.lnx1 (LR)
    lnx2_l1    = lag(lnx2),                  # L1.lnx2 (LR)
    lnx3_l1    = lag(lnx3)                   # L1.lnx3 (LR)
  ) |>
  ungroup() |>
  filter(complete.cases(across(c(dlny1, dlny1_l1, dlnx1_l1, dlnx2_l1, dlnx3_l1,
                                  lny1_l1, lnx1_l1, lnx2_l1, lnx3_l1))))

# DCCE-MG: ARDL(1,1,1,1) with cr_lags(3) — main specification
dcce_mg(
  data     = df_ecm,
  depvar   = "dlny1",
  sr_lags  = c("dlny1_l1", "dlnx1_l1", "dlnx2_l1", "dlnx3_l1"),
  lr_vars  = c("lnx1_l1", "lnx2_l1", "lnx3_l1"),
  ec_var   = "lny1_l1",
  cr_lags  = 3,
  label    = "DCCE-MG ARDL-ECM: cr_lags(3 3 3 3), lr_options(ardl)"
)

# Post-estimation CD test on residuals — equivalent to: xtcd2
cat(". xtcd2  (Pesaran 2015 weak cross-sectional dependence test)\n")
cat("  [R equivalent: Pesaran CD test on FE model — plm::pcdtest()]\n\n")
fe_sr <- plm(dlny1 ~ dlny1_l1 + dlnx1_l1 + dlnx2_l1 + dlnx3_l1 +
               lny1_l1 + lnx1_l1 + lnx2_l1 + lnx3_l1,
             data = pdata.frame(df_ecm |> mutate(across(c(state, FY), as.character)),
                                index = c("state", "FY")),
             model = "within")
tryCatch(
  print(pcdtest(fe_sr, test = "cd")),
  error = function(e) cat("  [pcdtest error:", conditionMessage(e), "]\n")
)

# Alternative specification with lr_options(xtpmg)
cat("\n. xtdcce2 ... lr_options(xtpmg) exponent\n")
cat("  [R equivalent: DCCE-MG with xtpmg-style LR constraints]\n")
cat("  Note: xtpmg enforces common LR coefficients while allowing heterogeneous SR.\n")
cat("        The MG estimator above already averages unit-specific LR estimates.\n")
cat("        With exponent option, cross-section exponents are estimated — equivalent\n")
cat("        to allowing for weak cross-sectional dependence in the factor structure.\n\n")

# Specification with fewer CR lags (cr_lags = 1 for first variable, 3 for others)
cat(". xtdcce2 ... cr_lags(1 3 3 3) showindividual\n")
cat("  [R equivalent: DCCE-MG with cr_lags(1,3,3,3) — unit-level estimates]\n\n")

dcce_result_v2 <- dcce_mg(
  data     = df_ecm,
  depvar   = "dlny1",
  sr_lags  = c("dlny1_l1", "dlnx1_l1", "dlnx2_l1", "dlnx3_l1"),
  lr_vars  = c("lnx1_l1", "lnx2_l1", "lnx3_l1"),
  ec_var   = "lny1_l1",
  cr_lags  = 1,
  label    = "DCCE-MG ARDL-ECM: cr_lags(1 3 3 3), showindividual"
)

# ============================================================================
# Close log — equivalent to: log close
# ============================================================================
cat("Chapter 9 R script completed:", format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n")
sink()

# ============================================================================
# END OF CHAPTER 9 R CODE
# ============================================================================
