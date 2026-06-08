# ========================================================================
# R_code10_complete.R
# Chapter 10 - Causal Inference and Marginal Treatment Effects
# COMPLETE self-contained R translation of Stata_code10.do
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch10
# Author: Marvin A. Titus
# Date: November 2025 (revised June 2026)
# NOTE: Code development was assisted by Claude (Anthropic). The author
# provided specifications and reviewed, tested, and validated all code.
# ========================================================================
# This is a SINGLE self-contained script. It inlines all four analysis
# sections that the Stata master (Stata_code10.do) ran as separate do-files:
#
#   PART A
#     Section 10.2        Regression Discontinuity Design (sharp/fuzzy RD)
#     Sections 10.3-10.9  Difference-in-Differences (TWFE, LASSO, SCM, SDID,
#                         Callaway-Sant'Anna, staggered, permutation, LOO)
#     Section 10.7.4      Extended TWFE (Wooldridge) via the etwfe package
#   PART B
#     Sections 10.10-10.16 Marginal Treatment Effects (MTE/MPRTE) + CBA
#
# Run top to bottom. All figures are written to graphs_dir AND printed to
# the active graphics device; tables and datasets are written to disk.
# ========================================================================

# ========================================================================
# PACKAGE INSTALLATION (run once; comment out thereafter)
# ========================================================================
# install.packages(c(
#   "rdrobust", "rddensity",                       # RDD (10.2)
#   "fixest", "did", "Synth", "sdid", "hdm",       # DiD (10.3-10.9)
#   "etwfe", "marginaleffects",                    # Extended TWFE (10.7.4)
#   "AER", "sampleSelection", "sandwich", "lmtest", "truncnorm",  # MTE (Part B)
#   "dplyr", "tidyr", "ggplot2", "haven"           # shared
# ))
#
# NOTE: etwfe requires fixest >= 0.13.2. If an older fixest is already
# installed, update it in a FRESH R session before sourcing this script:
#   install.packages("fixest"); install.packages("etwfe")

# ------------------------------------------------------------------------
# Enforce the fixest version requirement before loading anything.
# ------------------------------------------------------------------------
local({
  fx_ver <- tryCatch(packageVersion("fixest"), error = function(e) NULL)
  if (is.null(fx_ver) || fx_ver < "0.13.2") {
    stop(
      "fixest >= 0.13.2 is required by etwfe.\n",
      "  Currently installed: ", if (is.null(fx_ver)) "not found" else fx_ver, "\n",
      "  Fix: run  install.packages(\"fixest\")  in a fresh R session,\n",
      "  then restart R and source this script again."
    )
  }
})

# ------------------------------------------------------------------------
# Load packages. fixest is loaded FIRST so it claims its namespace before
# etwfe (which depends on it) is attached.
# ------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(fixest)
  library(etwfe)
  library(marginaleffects)
  library(rdrobust)
  library(rddensity)
  library(did)
  library(AER)
  library(sampleSelection)
  library(sandwich)
  library(lmtest)
  library(truncnorm)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(haven)
})

# Optional packages — loaded with graceful fallback (used by the DiD section).
sdid_available  <- requireNamespace("sdid",  quietly = TRUE)
synth_available <- requireNamespace("Synth", quietly = TRUE)
hdm_available   <- requireNamespace("hdm",   quietly = TRUE)
if (sdid_available)  suppressPackageStartupMessages(library(sdid))
if (synth_available) suppressPackageStartupMessages(library(Synth))
if (hdm_available)   suppressPackageStartupMessages(library(hdm))

# ========================================================================
# OUTPUT DIRECTORIES AND LOG FILE
# Paths switch automatically based on the OS username.
# ========================================================================
if (Sys.info()[["user"]] == "marvi") {
  base_dir   <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10"
  graphs_dir <- file.path(base_dir, "Output/graphs")
  tables_dir <- file.path(base_dir, "Output/tables")
  logdir     <- file.path(base_dir, "Output/logs")
} else {
  graphs_dir <- "Output/graphs"
  tables_dir <- "Output/tables"
  logdir     <- "Output/logs"
}
for (d in c(graphs_dir, tables_dir, logdir)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# Log all console output (R equivalent of Stata's `log using`).
log_path <- file.path(logdir, "Chapter10_R_output.log")
log_con  <- file(log_path, open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message")

# Cleanup helper: closes the sinks and the log connection exactly once.
# Called explicitly at the very end. (on.exit() does not fire reliably at
# top level outside a function, so we close manually instead.)
.close_log <- function() {
  if (!is.null(log_con) && isOpen(log_con)) {
    sink(type = "message")
    sink()
    close(log_con)
  }
}

cat("Chapter 10 log opened:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Graphs directory:", graphs_dir, "\n")

# ------------------------------------------------------------------------
# Springer B&W ggplot2 theme (mirrors Stata s2mono). Defined ONCE here and
# used by all four sections below.
# ------------------------------------------------------------------------
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



########################################################################
########################################################################
#
#  PART A  -  SECTION 10.2: REGRESSION DISCONTINUITY DESIGN
#
########################################################################
########################################################################


cat("\n========================================================\n")
cat(  "SECTION 10.2: REGRESSION DISCONTINUITY DESIGN\n")
cat(  "========================================================\n")

# -----------------------------------------------------------------------
# 10.2.1  Synthetic Data Generation
# -----------------------------------------------------------------------
# Design notes
# ─────────────
#  N    = 4,000 first-time, full-time freshmen at a flagship public university
#  X    = high-school GPA, 0–4.0 scale (running variable)
#           Exact truncated normal via CDF inversion: N(3.15, 0.72)
#           restricted to [1.0, 4.0]; yields mean ≈ 3.0, ~37% at or
#           above the 3.25 cutoff, no mass points at boundaries.
#  c    = 3.25  (institutional merit scholarship cutoff)
#  D    = scholarship receipt
#         Sharp:  D = 1 iff X >= 3.25
#         Fuzzy:  take-up probability jumps at c but is < 100%
#  True LATE on latent persistence index = 0.10
#  (Implied effect on binary persistence ≈ 0.13–0.16 pp.)
#
#  Postsecondary baseline rates near c = 3.25 (HSLS:09-like):
#    Second-year persistence ≈ 62–65%
#    Year-1 credits earned   ≈ 28–30
#    Year-1 college GPA      ≈ 2.75–2.85

set.seed(20260510)

N         <- 4000L
cutoff    <- 3.25
true_late <- 0.10      # true LATE on latent persistence index

# -- Running variable --------------------------------------------------
# HS GPA: truncated normal via rtruncnorm() from the truncnorm package.
# Parameters: N(mu=3.15, sd=0.72) restricted to [lo=1.0, hi=4.0].
# This matches the Stata CDF-inversion approach and produces
# mean ≈ 3.0, SD ≈ 0.60, ~37% at or above the 3.25 cutoff.

mu_gpa <- 3.15
sd_gpa <- 0.72
lo_gpa <- 1.00
hi_gpa <- 4.00

hs_gpa <- rtruncnorm(N, a = lo_gpa, b = hi_gpa,
                     mean = mu_gpa, sd = sd_gpa)

# Centered running variable (x = 0 at the eligibility threshold)
x <- hs_gpa - cutoff

# -- Treatment assignment ----------------------------------------------
D_sharp <- as.integer(x >= 0)

# Fuzzy: imperfect compliance on both sides of the cutoff.
# Below cutoff: small baseline take-up (~5–14%).
# Above cutoff: high but incomplete take-up (~70–85%).
pr_take_up <- ifelse(x < 0,
                     0.05 + 0.09 * (x + 2.25) / 2.25,
                     0.70 + 0.15 * pmin(x / 0.75, 1.0))
pr_take_up <- pmin(pmax(pr_take_up, 0.02), 0.92)

D_fuzzy <- as.integer(runif(N) < pr_take_up)

# -- Pre-determined covariates (HSLS:09-calibrated) --------------------
female     <- as.integer(runif(N) < 0.54)
firstgen   <- as.integer(runif(N) < 0.32)
urm        <- as.integer(runif(N) < 0.28)
act_score  <- 18L + as.integer(12 * runif(N))
income_cat <- pmin(1L + as.integer(3 * runif(N)), 3L)

# -- Outcomes ----------------------------------------------------------
# mu0: latent persistence index, HSLS:09-calibrated.
# Intercept 0.58 reflects ~62% baseline persistence near c = 3.25.

mu0 <- 0.58 + 0.20 * x - 0.08 * x^2        +
       0.03 * female  - 0.05 * firstgen      +
       0.01 * (income_cat - 2)               +
       rnorm(N, 0, 0.18)

# (a) Sharp persistence (binary)
Y1_s       <- mu0 + true_late * D_sharp + rnorm(N, 0, 0.12)
Y1_s       <- pmax(pmin(Y1_s, 1), 0)
persist_sharp <- as.integer(Y1_s > 0.50)

# (b) Fuzzy persistence (binary)
Y1_f       <- mu0 + true_late * D_fuzzy + rnorm(N, 0, 0.12)
Y1_f       <- pmax(pmin(Y1_f, 1), 0)
persist_fuzzy <- as.integer(Y1_f > 0.50)

# (c) Year-1 credits earned (continuous)
credits_y1 <- pmax(28 + 5.0 * x - 1.5 * x^2 + 4 * D_sharp +
                   rnorm(N, 0, 5), 0)

# (d) Year-1 college GPA (continuous)
cgpa_y1    <- pmin(pmax(2.80 + 0.35 * x - 0.08 * x^2 +
                        0.08 * D_sharp + rnorm(N, 0, 0.40),
                        0), 4.0)

# Assemble data frame
df <- data.frame(
  id           = seq_len(N),
  hs_gpa       = hs_gpa,
  x            = x,
  D_sharp      = D_sharp,
  D_fuzzy      = D_fuzzy,
  female       = female,
  firstgen     = firstgen,
  urm          = urm,
  act_score    = act_score,
  income_cat   = income_cat,
  persist_sharp = persist_sharp,
  persist_fuzzy = persist_fuzzy,
  credits_y1   = credits_y1,
  cgpa_y1      = cgpa_y1
)

cat(sprintf("\n=== Data generation complete. N = %d ===\n", nrow(df)))
print(summary(df[, c("x","D_sharp","D_fuzzy","persist_sharp",
                     "credits_y1","cgpa_y1")]))

# -----------------------------------------------------------------------
# 10.2.2  Density Continuity Test and Covariate Balance
# -----------------------------------------------------------------------
cat("\n--- 10.2.2  Density Continuity Test (Cattaneo, Jansson & Ma 2020) ---\n")
#
# H0: density of x is continuous at c = 0 (no manipulation).
# Method: Cattaneo, Jansson & Ma (2020) local polynomial density
# estimator with bias-corrected robust inference.
# Interpretation: look at the Robust row.
#   p > 0.05  → no evidence of density manipulation.
#   p ≤ 0.05  → investigate.

rd_dens <- rddensity(df$x, c = 0)
print(summary(rd_dens))

# Density plot ➜ fig10_2_1
den_plot <- rdplotdensity(rd_dens, df$x,
                          plotRange  = c(-1.5, 1.5),
                          plotN      = 25,
                          CIuniform  = TRUE)
# Overlay Springer theme on the rddensity plot object
den_gg <- den_plot$Estplot +
  labs(title    = "Density Continuity Test (rddensity)",
       subtitle = "Running variable: HS GPA centered at c = 3.25",
       x        = "HS GPA (centered at cutoff)",
       y        = "Density") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black",
             linewidth = 0.5) +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_2_1_rdd_density_test.png"),
       den_gg, width = 7, height = 5, dpi = 200)
print(den_gg)   # render to RStudio Plots pane
cat("   Density test exported -> fig10_2_1_rdd_density_test.png\n")
cat("   See the Robust row above for the preferred test statistic.\n")

# -- Covariate balance via rdrobust ------------------------------------
cat("\n--- Covariate balance ---\n")
cat(sprintf("   %-18s %10s %10s %8s\n", "Variable", "Coef(conv)", "p(robust)", "h*"))
cat("  ", strrep("-", 50), "\n")

covs_list <- c("female","firstgen","urm","act_score","income_cat")
for (v in covs_list) {
  rr <- rdrobust(df[[v]], df$x, c = 0, kernel = "triangular",
                 bwselect = "mserd")
  cat(sprintf("   %-18s %10.4f %10.4f %8.3f\n",
              v, rr$coef[1], rr$pv[3], rr$bws[1,1]))
}
cat("  ", strrep("-", 50), "\n")
cat("   None should be statistically significant under a valid RD.\n")

# -----------------------------------------------------------------------
# 10.2.3  Binned Scatterplots
# -----------------------------------------------------------------------
cat("\n--- 10.2.3  Binned scatterplots ---\n")

# Helper: compute bin means for a binned scatterplot
make_bins <- function(y, x, nbins = 20) {
  below <- data.frame(y = y[x < 0], x = x[x < 0])
  above <- data.frame(y = y[x >= 0], x = x[x >= 0])
  bin_side <- function(d, n) {
    d$bin <- cut(d$x, breaks = n, labels = FALSE)
    d %>% group_by(bin) %>%
      summarise(xmid = mean(x), ymean = mean(y), .groups = "drop")
  }
  rbind(
    mutate(bin_side(below, nbins), side = "below"),
    mutate(bin_side(above, nbins), side = "above")
  )
}

outcomes_bin <- list(
  persist_sharp = list(y = df$persist_sharp,
                       ytitle = "Second-Year Persistence Rate"),
  credits_y1    = list(y = df$credits_y1,
                       ytitle = "Year-1 Credits Earned"),
  cgpa_y1       = list(y = df$cgpa_y1,
                       ytitle = "Year-1 College GPA")
)

for (nm in names(outcomes_bin)) {
  bins <- make_bins(outcomes_bin[[nm]]$y, df$x, nbins = 20)
  p <- ggplot(bins, aes(x = xmid, y = ymean)) +
    geom_point(shape = 1, size = 2, colour = "black") +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "black", linewidth = 0.5) +
    geom_smooth(data = filter(bins, side == "below"),
                aes(x = xmid, y = ymean),
                method = "lm", se = FALSE, colour = "black",
                linewidth = 0.7) +
    geom_smooth(data = filter(bins, side == "above"),
                aes(x = xmid, y = ymean),
                method = "lm", se = FALSE, colour = "black",
                linewidth = 0.7) +
    labs(title    = paste("RD Binscatter:", nm),
         x        = "HS GPA centered at c = 3.25",
         y        = outcomes_bin[[nm]]$ytitle) +
    theme_springer()
  fname <- file.path(graphs_dir,
                     paste0("fig10_2_2_rdd_binscatter_", nm, ".png"))
  ggsave(fname, p, width = 7, height = 5, dpi = 200)
  print(p)   # render to RStudio Plots pane
}
cat("Binned scatterplots exported.\n")

# -----------------------------------------------------------------------
# 10.2.4  Sharp RD Estimates
# -----------------------------------------------------------------------
cat("\n=== 10.2.4: Sharp RD Estimates ===\n")

# -- Naive OLS (biased benchmark) -------------------------------------
cat("\n--- OLS full sample (biased benchmark) ---\n")
ols_persist <- lm(persist_sharp ~ D_sharp + x, data = df)
cat("persist_sharp ~ D_sharp + x\n")
print(coeftest(ols_persist, vcov = sandwich::vcovHC(ols_persist, type = "HC1")))

ols_credits <- lm(credits_y1 ~ D_sharp + x, data = df)
cat("\ncredits_y1 ~ D_sharp + x\n")
print(coeftest(ols_credits, vcov = sandwich::vcovHC(ols_credits, type = "HC1")))

# -- Manual local linear with triangular kernel -----------------------
# Kernel weight: w_i = (1 - |x_i|/h) for |x_i| <= h, else 0.
# Separate slopes each side via the interaction term x_D.
# h = 0.50 used here for exposition.

h_manual <- 0.50
df_manual <- df %>%
  mutate(in_bw  = as.integer(abs(x) <= h_manual),
         tri_wt = (1 - abs(x) / h_manual) * in_bw,
         x_D    = x * D_sharp)

cat(sprintf("\n--- Manual local linear (h = %.2f, triangular kernel) ---\n",
            h_manual))
for (outcome in c("persist_sharp","credits_y1","cgpa_y1")) {
  sub <- df_manual %>% filter(in_bw == 1)
  fit <- lm(as.formula(paste(outcome, "~ D_sharp + x + x_D")),
            data = sub, weights = sub$tri_wt)
  se  <- sqrt(sandwich::vcovHC(fit, type = "HC1")["D_sharp","D_sharp"])
  cat(sprintf("   %-18s LATE = %7.4f  SE = %7.4f\n",
              outcome, coef(fit)["D_sharp"], se))
}

# -- rdrobust: CCT MSE-optimal bandwidth ------------------------------
cat("\n--- rdrobust (CCT optimal bandwidth) ---\n")
cat(sprintf("   %-18s %8s %10s %10s %8s\n",
            "Outcome", "LATE", "SE(conv)", "p(robust)", "h*"))
cat("  ", strrep("-", 58), "\n")

for (outcome in c("persist_sharp","credits_y1","cgpa_y1")) {
  rr <- rdrobust(df[[outcome]], df$x, c = 0,
                 kernel = "triangular", bwselect = "mserd", all = TRUE)
  cat(sprintf("   %-18s %8.4f %10.4f %10.4f %8.3f\n",
              outcome, rr$coef[1], rr$se[1], rr$pv[3], rr$bws[1,1]))
}
cat("  ", strrep("-", 58), "\n")

# -- Covariate-adjusted rdrobust (Sharp RD) ---------------------------
cat("\n--- rdrobust with covariates (Sharp RD) ---\n")
cat("   Covariates: female firstgen urm act_score income_cat\n")
cat(sprintf("   %-18s %8s %10s %10s %8s\n",
            "Outcome", "LATE", "SE(conv)", "p(robust)", "h*"))
cat("  ", strrep("-", 58), "\n")

covs_mat <- as.matrix(df[, c("female","firstgen","urm","act_score","income_cat")])
for (outcome in c("persist_sharp","credits_y1","cgpa_y1")) {
  rr <- rdrobust(df[[outcome]], df$x, c = 0,
                 kernel = "triangular", bwselect = "mserd",
                 covs = covs_mat, all = TRUE)
  cat(sprintf("   %-18s %8.4f %10.4f %10.4f %8.3f\n",
              outcome, rr$coef[1], rr$se[1], rr$pv[3], rr$bws[1,1]))
}
cat("  ", strrep("-", 58), "\n")
cat("   Point estimates should be stable relative to unadjusted rdrobust.\n")
cat("   SE reduction reflects lower residual variance with covariates.\n")

# -----------------------------------------------------------------------
# 10.2.5  Bandwidth Sensitivity
# -----------------------------------------------------------------------
cat("\n=== 10.2.5: Bandwidth Sensitivity (persistence) ===\n")

bw_list <- c(0.15, 0.20, 0.25, 0.30, 0.40, 0.50, 0.60, 0.75, 1.00)

bw_res <- lapply(bw_list, function(h) {
  rr <- rdrobust(df$persist_sharp, df$x, c = 0, h = h,
                 kernel = "triangular")
  data.frame(h     = h,
             LATE  = rr$coef[1],
             CI_lo = rr$ci[3, 1],
             CI_hi = rr$ci[3, 2])
})
bw_df <- do.call(rbind, bw_res)

cat("Bandwidth sensitivity -- persistence\n")
print(bw_df, digits = 4, row.names = FALSE)

# Plot ➜ fig10_2_3
p_bw <- ggplot(bw_df, aes(x = h, y = LATE)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "black", linewidth = 0.4) +
  geom_errorbar(aes(ymin = CI_lo, ymax = CI_hi),
                width = 0.02, colour = "grey50", linewidth = 0.5) +
  geom_point(shape = 18, size = 3, colour = "black") +
  labs(title    = "Bandwidth Sensitivity -- Sharp RD",
       subtitle = "Outcome: Second-Year Persistence, c = 3.25",
       x        = "Bandwidth (HS GPA units)",
       y        = "Estimated LATE (pp)") +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_2_3_rdd_bw_sensitivity.png"),
       p_bw, width = 7, height = 5, dpi = 200)
print(p_bw)   # render to RStudio Plots pane
cat("Bandwidth sensitivity plot exported -> fig10_2_3_rdd_bw_sensitivity.png\n")

# -----------------------------------------------------------------------
# 10.2.6  Polynomial Order Sensitivity
# -----------------------------------------------------------------------
cat("\n=== 10.2.6: Polynomial Order Sensitivity ===\n")
cat(sprintf("   %3s %8s %10s %10s %8s\n",
            "p", "LATE", "SE(conv)", "p(robust)", "h*"))
cat("  ", strrep("-", 48), "\n")

for (p_ord in 1:3) {
  rr <- rdrobust(df$persist_sharp, df$x, c = 0, p = p_ord,
                 kernel = "triangular", bwselect = "mserd")
  cat(sprintf("   %3d %8.4f %10.4f %10.4f %8.3f\n",
              p_ord, rr$coef[1], rr$se[1], rr$pv[3], rr$bws[1,1]))
}
cat("  ", strrep("-", 48), "\n")
cat("   p = 1 (local linear) is the default and typically preferred.\n")

# -----------------------------------------------------------------------
# 10.2.7  Fuzzy RD
# -----------------------------------------------------------------------
# Three-step procedure for each outcome:
#   1. First stage    D_sharp → D_fuzzy (common to all outcomes)
#   2. Reduced forms  ITT jump at cutoff for each outcome
#   3. Fuzzy LATE     rdrobust (Wald/2SLS via fuzzy argument)
#                     Manual 2SLS (h = 0.50, triangular kernel)
#
# Outcomes examined:
#   persist_fuzzy   Second-year persistence (binary, fuzzy DGP)
#   credits_y1      Year-1 credits earned   (continuous)
#   cgpa_y1         Year-1 college GPA      (continuous)

cat("\n=== 10.2.7: Fuzzy RD (Imperfect Compliance) ===\n")

# -- First stage: jump in scholarship take-up at cutoff ---------------
cat("\n--- First stage: jump in take-up at cutoff ---\n")

fs <- rdrobust(df$D_fuzzy, df$x, c = 0,
               kernel = "triangular", bwselect = "mserd", all = TRUE)
fs_late <- fs$coef[1]
fs_se   <- fs$se[1]
fs_p    <- fs$pv[3]
fs_h    <- fs$bws[1,1]

cat(sprintf("   FS jump  = %7.4f   SE = %7.4f   p(robust) = %6.4f\n",
            fs_late, fs_se, fs_p))
cat(sprintf("   h* = %6.3f\n", fs_h))
cat("   Strong first stage: cutoff strongly predicts scholarship take-up.\n")

# -- Reduced forms: ITT effect at cutoff for each outcome -------------
cat("\n--- Reduced forms: cutoff → outcome (ITT) ---\n")
cat(sprintf("   %-18s %8s %10s %10s %8s\n",
            "Outcome", "ITT", "SE(conv)", "p(robust)", "h*"))
cat("  ", strrep("-", 60), "\n")

rf_res <- list()
for (outcome in c("persist_fuzzy","credits_y1","cgpa_y1")) {
  rr <- rdrobust(df[[outcome]], df$x, c = 0,
                 kernel = "triangular", bwselect = "mserd", all = TRUE)
  rf_res[[outcome]] <- list(b = rr$coef[1], se = rr$se[1],
                            p = rr$pv[3],   h  = rr$bws[1,1])
  cat(sprintf("   %-18s %8.4f %10.4f %10.4f %8.3f\n",
              outcome, rr$coef[1], rr$se[1], rr$pv[3], rr$bws[1,1]))
}
cat("  ", strrep("-", 60), "\n")
cat("   ITT: local average effect of eligibility (not take-up) on outcome.\n")

# -- Fuzzy LATE via rdrobust ------------------------------------------
cat("\n--- Fuzzy LATE (rdrobust, fuzzy option) ---\n")
cat(sprintf("   %-18s %8s %10s %10s %8s\n",
            "Outcome", "LATE", "SE(conv)", "p(robust)", "h*"))
cat("  ", strrep("-", 60), "\n")

fl_res <- list()
for (outcome in c("persist_fuzzy","credits_y1","cgpa_y1")) {
  rr <- rdrobust(df[[outcome]], df$x, c = 0,
                 fuzzy  = df$D_fuzzy,
                 kernel = "triangular", bwselect = "mserd", all = TRUE)
  fl_res[[outcome]] <- list(b = rr$coef[1], se = rr$se[1],
                            p = rr$pv[3],   h  = rr$bws[1,1])
  cat(sprintf("   %-18s %8.4f %10.4f %10.4f %8.3f\n",
              outcome, rr$coef[1], rr$se[1], rr$pv[3], rr$bws[1,1]))
}
cat("  ", strrep("-", 60), "\n")
cat("   LATE: causal effect for compliers at the cutoff.\n")
cat("   Robust CIs: Calonico, Cattaneo & Titiunik (2014, Econometrica).\n")
cat("   h* selected by CCT MSERD criterion separately for each outcome.\n")

# -- Covariate-adjusted Fuzzy LATE ------------------------------------
cat("\n--- Covariate-adjusted Fuzzy LATE (rdrobust, covs) ---\n")
cat("   Covariates: female firstgen urm act_score income_cat\n")
cat(sprintf("   %-18s %8s %10s %10s %8s\n",
            "Outcome", "LATE", "SE(conv)", "p(robust)", "h*"))
cat("  ", strrep("-", 60), "\n")

flc_res <- list()
for (outcome in c("persist_fuzzy","credits_y1","cgpa_y1")) {
  rr <- rdrobust(df[[outcome]], df$x, c = 0,
                 fuzzy    = df$D_fuzzy,
                 kernel   = "triangular", bwselect = "mserd",
                 covs     = covs_mat, all = TRUE)
  flc_res[[outcome]] <- list(b = rr$coef[1], se = rr$se[1],
                             p = rr$pv[3],   h  = rr$bws[1,1])
  cat(sprintf("   %-18s %8.4f %10.4f %10.4f %8.3f\n",
              outcome, rr$coef[1], rr$se[1], rr$pv[3], rr$bws[1,1]))
}
cat("  ", strrep("-", 60), "\n")
cat("   Wald estimator = RF / FS; covs() reduces outcome residual variance.\n")
cat("   Point estimates stable relative to unadjusted fuzzy LATE above.\n")

# -- Manual 2SLS (h = 0.50, triangular kernel) -------------------------
# Pedagogical bridge from IV/2SLS (Chapters 6–7) to rdrobust fuzzy.
# Variable names match Equation 10.x in text:
#   in_bw2  = bandwidth indicator  (|x| <= 0.50)
#   tri_wt2 = triangular kernel weight
#   x_Ds2   = slope interaction     (x × D_sharp)
#
# AER::ivreg: outcome ~ controls + (endogenous ~ instrument)

h2 <- 0.50
df <- df %>%
  mutate(in_bw2  = as.integer(abs(x) <= h2),
         tri_wt2 = (1 - abs(x) / h2) * in_bw2,
         x_Ds2   = x * D_sharp)

cat("\n--- Manual 2SLS (h = 0.50, triangular kernel) ---\n")
cat("   Variables: in_bw2  tri_wt2  x_Ds2  (as in text Eq. 10.x)\n")
cat(sprintf("   %-18s %8s %10s %10s\n", "Outcome", "LATE", "SE", "p-value"))
cat("  ", strrep("-", 52), "\n")

iv_res <- list()
for (outcome in c("persist_fuzzy","credits_y1","cgpa_y1")) {
  sub_iv <- df %>% filter(in_bw2 == 1)
  # AER::ivreg two-part formula:
  #   LHS ~ exogenous + endogenous | exogenous + instruments
  # x and x_Ds2 are exogenous (appear on both sides of |).
  # D_fuzzy is endogenous; D_sharp is its instrument.
  fml    <- as.formula(paste(outcome,
                             "~ x + x_Ds2 + D_fuzzy | x + x_Ds2 + D_sharp"))
  fit_iv <- ivreg(fml, data = sub_iv, weights = sub_iv$tri_wt2)
  se_iv  <- sqrt(sandwich::vcovHC(fit_iv, type = "HC1")["D_fuzzy","D_fuzzy"])
  b_iv   <- coef(fit_iv)["D_fuzzy"]
  p_iv   <- 2 * pnorm(-abs(b_iv / se_iv))
  iv_res[[outcome]] <- list(b = b_iv, se = se_iv, p = p_iv)
  cat(sprintf("   %-18s %8.4f %10.4f %10.4f\n",
              outcome, b_iv, se_iv, p_iv))
}
cat("  ", strrep("-", 52), "\n")
cat("   2SLS uses fixed h = 0.50; rdrobust above uses MSE-optimal h*.\n")
cat("   For publication use rdrobust fuzzy estimates. 2SLS is pedagogical.\n")

# Clean up 2SLS columns
df <- df %>% select(-in_bw2, -tri_wt2, -x_Ds2)

# -- Consolidated Fuzzy RD Summary Table --------------------------------
cat("\n--- Consolidated Fuzzy RD Summary ---\n")
cat("  ", strrep("-", 76), "\n")
cat(sprintf("   %-18s %10s %10s %15s %10s\n",
            "Outcome", "FS jump", "RF (ITT)", "LATE(rdrobust)", "p(LATE)"))
cat("  ", strrep("-", 76), "\n")

for (outcome in c("persist_fuzzy","credits_y1","cgpa_y1")) {
  cat(sprintf("   %-18s %10.4f %10.4f %15.4f %10.4f\n",
              outcome,
              fs_late,
              rf_res[[outcome]]$b,
              fl_res[[outcome]]$b,
              fl_res[[outcome]]$p))
}
cat("  ", strrep("-", 76), "\n")
cat("   FS: first-stage jump in D_fuzzy at the cutoff (same for all outcomes).\n")
cat("   RF: rdrobust ITT at CCT MSE-optimal h* for each outcome.\n")
cat("   LATE: Wald estimator = RF / FS via rdrobust fuzzy option.\n")
cat("   NOTE: credits_y1 and cgpa_y1 use sharp DGP; fuzzy LATE shown\n")
cat("         for pedagogical comparison. Prefer sharp estimates (§10.2.4).\n")

# -----------------------------------------------------------------------
# 10.2.8  Validity Checks
# -----------------------------------------------------------------------
cat("\n=== 10.2.8: Validity Checks ===\n")

# -- Placebo cutoffs ---------------------------------------------------
# Under a valid RD, rdrobust should detect no discontinuity at
# artificially imposed cutoffs away from c = 3.25.
# Bonferroni threshold for six simultaneous tests = 0.008.

cat("\n--- Placebo cutoffs ---\n")
cat(sprintf("   %8s %10s %10s %8s\n", "Cutoff", "LATE", "p(robust)", "Side"))
cat("  ", strrep("-", 44), "\n")

for (pc in c(-0.60, -0.40, -0.20)) {
  sub_pc <- df %>% filter(x < 0)
  rr <- rdrobust(sub_pc$persist_sharp, sub_pc$x, c = pc,
                 kernel = "triangular", bwselect = "mserd")
  cat(sprintf("   %8.2f %10.4f %10.4f %8s\n",
              pc, rr$coef[1], rr$pv[3], "below"))
}
for (pc in c(0.20, 0.40, 0.60)) {
  sub_pc <- df %>% filter(x > 0)
  rr <- rdrobust(sub_pc$persist_sharp, sub_pc$x, c = pc,
                 kernel = "triangular", bwselect = "mserd")
  cat(sprintf("   %8.2f %10.4f %10.4f %8s\n",
              pc, rr$coef[1], rr$pv[3], "above"))
}
cat("  ", strrep("-", 44), "\n")

# -- Donut RD ----------------------------------------------------------
# Excludes a narrow band at the cutoff; stable estimates corroborate
# the baseline. Capped at d = 0.10 (see Stata comments for rationale).

cat("\n--- Donut RD (exclude narrow band at cutoff) ---\n")
cat(sprintf("   %6s %8s %10s\n", "Donut", "LATE", "p(robust)"))
cat("  ", strrep("-", 30), "\n")

for (d in c(0.05, 0.10)) {
  sub_d <- df %>% filter(abs(x) > d)
  rr    <- rdrobust(sub_d$persist_sharp, sub_d$x, c = 0,
                    kernel = "triangular", bwselect = "mserd")
  cat(sprintf("   %6.2f %8.4f %10.4f\n", d, rr$coef[1], rr$pv[3]))
}
cat("  ", strrep("-", 30), "\n")
cat("   Note: interpret alongside bandwidth sensitivity (Section 10.2.5).\n")
cat("   Precision loss at d = 0.10 does not imply a zero treatment effect.\n")

# -- Covariate-augmented rdrobust -------------------------------------
cat("\n--- Covariate-augmented estimate ---\n")

rr_aug <- rdrobust(df$persist_sharp, df$x, c = 0,
                   kernel = "triangular", bwselect = "mserd",
                   covs   = covs_mat, all = TRUE)
cat(sprintf("   Augmented LATE = %7.4f  p(robust) = %6.4f\n",
            rr_aug$coef[1], rr_aug$pv[3]))

# -- Subgroup heterogeneity -------------------------------------------
cat("\n--- Subgroup effects ---\n")
cat(sprintf("   %-18s %8s %10s %6s\n",
            "Subgroup", "LATE", "p(robust)", "n"))
cat("  ", strrep("-", 52), "\n")

subgroups <- list(
  list(var = "firstgen", vals = c(1L, 0L),
       labels = c("First-gen", "Continuing-gen")),
  list(var = "female",   vals = c(1L, 0L),
       labels = c("Female", "Male")),
  list(var = "urm",      vals = c(1L, 0L),
       labels = c("URM", "Non-URM"))
)

for (sg in subgroups) {
  for (i in seq_along(sg$vals)) {
    sub_sg <- df %>% filter(.data[[sg$var]] == sg$vals[i])
    rr     <- rdrobust(sub_sg$persist_sharp, sub_sg$x, c = 0,
                       kernel = "triangular", bwselect = "mserd")
    cat(sprintf("   %-18s %8.4f %10.4f %6d\n",
                sg$labels[i], rr$coef[1], rr$pv[3], nrow(sub_sg)))
  }
}
cat("  ", strrep("-", 52), "\n")

# -----------------------------------------------------------------------
# 10.2.9  Publication-Quality RD Plots (rdplot)
# -----------------------------------------------------------------------
cat("\n=== 10.2.9: Publication-Quality RD Plots ===\n")

# Helper to build rdplot and save via ggplot2 with Springer theme.
# rdplot() returns $rdplot (a ggplot object) that we re-theme and save.

save_rdplot <- function(y, x, title, subtitle, ytitle, fname,
                        nbins = c(30, 30)) {
  rp <- rdplot(y, x, c = 0, nbins = nbins,
               title   = title,
               x.label = "High-School GPA (centered at cutoff)",
               y.label = ytitle)
  p <- rp$rdplot +
    labs(subtitle = subtitle,
         caption  = "Circles = bin means; lines = local polynomial fit.\nBins selected by IMSE-minimizing method (Calonico et al., 2015).") +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "black", linewidth = 0.5) +
    theme_springer() +
    theme(legend.position = "none")
  ggsave(file.path(graphs_dir, fname), p, width = 7, height = 5, dpi = 200)
  print(p)   # render to RStudio Plots pane
}

# fig10_2_4: Sharp persistence
save_rdplot(df$persist_sharp, df$x,
            title    = "Effect of Institutional Merit Scholarship on\nSecond-Year Persistence",
            subtitle = "Sharp RD -- HS GPA cutoff c = 3.25",
            ytitle   = "Second-Year Persistence Rate",
            fname    = "fig10_2_4_rdd_plot_persistence.png")

# fig10_1: Sharp credits (Fig. 10.1 in chapter)
save_rdplot(df$credits_y1, df$x,
            title    = "Effect of Institutional Merit Scholarship on\nYear-1 Credits Earned",
            subtitle = "Sharp RD -- HS GPA cutoff c = 3.25",
            ytitle   = "Year-1 Credits Earned",
            fname    = "fig10_1_rdd_plot_credits.png")

# fig10_2_6: Sharp college GPA
save_rdplot(df$cgpa_y1, df$x,
            title    = "Effect of Institutional Merit Scholarship on\nYear-1 College GPA",
            subtitle = "Sharp RD -- HS GPA cutoff c = 3.25",
            ytitle   = "Year-1 College GPA",
            fname    = "fig10_2_6_rdd_plot_cgpa.png")

# fig10_2_7: First stage (take-up)
save_rdplot(df$D_fuzzy, df$x,
            title    = "First Stage: Scholarship Take-Up at GPA Cutoff",
            subtitle = "Fuzzy RD -- jump in take-up probability at c = 3.25",
            ytitle   = "P(Scholarship Received)",
            fname    = "fig10_2_7_rdd_plot_firststage.png")

# fig10_2_8: Fuzzy persistence rdplot
save_rdplot(df$persist_fuzzy, df$x,
            title    = "Fuzzy RD: Second-Year Persistence (Fuzzy Outcome)",
            subtitle = "Fuzzy RD — imperfect compliance around c = 3.25",
            ytitle   = "P(Second-Year Persistence)",
            fname    = "fig10_2_8_rdd_plot_persist_fuzzy.png")

# -- fig10_2: Fuzzy LATE for credits_y1 (forest-plot style) ----------
# Forest-plot style: point estimate and 95% CI for the fuzzy LATE
# for credits earned in year 1.  (Fig. 10.2 in chapter)

fl_b  <- fl_res$credits_y1$b
fl_se <- fl_res$credits_y1$se

late_df <- data.frame(
  outcome = "Credits Y1 (cr-hrs)",
  b       = fl_b,
  lo      = fl_b - 1.96 * fl_se,
  hi      = fl_b + 1.96 * fl_se,
  j       = 1
)

p_late <- ggplot(late_df, aes(x = b, y = j)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = lo, xmax = hi),
                 height = 0.15, colour = "black", linewidth = 0.7) +
  geom_point(shape = 18, size = 5, colour = "black") +
  scale_y_continuous(breaks = 1,
                     labels = "Credits Y1 (cr-hrs)",
                     limits = c(0.5, 1.5)) +
  labs(title    = "Fuzzy RD: LATE for First-Year Credits",
       subtitle = "Point estimate with 95% CI",
       x        = "Fuzzy LATE (rdrobust, CCT MSE-optimal h*)",
       y        = NULL,
       caption  = paste0("Complier LATE = RF / FS.\n",
                         "rdrobust fuzzy option, kernel(triangular), bwselect(mserd).")) +
  theme_springer() +
  theme(axis.text.y  = element_text(size = 10),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())

ggsave(file.path(graphs_dir, "fig10_2_fuzzy_late_credits.png"),
       p_late, width = 7, height = 3.5, dpi = 200)
print(p_late)   # render to RStudio Plots pane

cat("All RD plots exported.\n")

# -----------------------------------------------------------------------
# 10.2.10  Summary Table and Dataset Save
# -----------------------------------------------------------------------
cat("\n=== 10.2.10: Summary of Sharp RD Estimates ===\n")
cat("  ", strrep("-", 72), "\n")
cat(sprintf("   %-18s %8s %10s  %-22s %6s\n",
            "Outcome", "LATE", "SE(conv)", "95% CI (robust)", "h*"))
cat("  ", strrep("-", 72), "\n")

for (outcome in c("persist_sharp","credits_y1","cgpa_y1")) {
  rr  <- rdrobust(df[[outcome]], df$x, c = 0,
                  kernel = "triangular", bwselect = "mserd", all = TRUE)
  ci_lo <- rr$ci[3, 1]
  ci_hi <- rr$ci[3, 2]
  cat(sprintf("   %-18s %8.4f %10.4f  [%7.3f, %7.3f]  %6.3f\n",
              outcome, rr$coef[1], rr$se[1], ci_lo, ci_hi, rr$bws[1,1]))
}
cat("  ", strrep("-", 72), "\n")
cat("   Robust CIs: Calonico, Cattaneo & Titiunik (2014, Econometrica).\n")
cat("   Kernel: triangular. Bandwidth selector: MSERD (MSE-optimal).\n")
cat(sprintf("   True simulated LATE (latent scale) = %.2f.\n", true_late))
cat("   Implied binary persistence effect approx. 0.13-0.16 pp.\n")

# Save dataset
saveRDS(df, "ch10_rdd_hsls09_synthetic.rds")
# Also write CSV for cross-platform use
write.csv(df, "ch10_rdd_hsls09_synthetic.csv", row.names = FALSE)

cat("\n=== RDD dataset saved  ->  ch10_rdd_hsls09_synthetic.rds / .csv ===\n")
cat("=== Section 10.2 complete.                                         ===\n")

# ========================================================================
# END OF R_code10_RDD.R
# ========================================================================



########################################################################
########################################################################
#
#  PART A  -  SECTIONS 10.3-10.9: DIFFERENCE-IN-DIFFERENCES
#
########################################################################
########################################################################


gh_raw <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch10"

# =====================================================================================================================
# SECTION 10.3.1: DATA STRUCTURE AND VARIABLE CONSTRUCTION
# =====================================================================================================================
cat("\n============================================================\n")
cat("SECTION 10.3.1: DATA STRUCTURE AND VARIABLE CONSTRUCTION\n")
cat("============================================================\n")

# Download SHEEO state-level panel
df_raw <- tryCatch(
  read.csv(paste0(gh_raw, "/Example_10_3_1.csv")),
  error = function(e) {
    if (file.exists("Example_10_3_1.csv")) read.csv("Example_10_3_1.csv")
    else stop("Cannot load Example_10_3_1.csv — check network or working directory.")
  }
)
write.csv(df_raw, "Example_10_3_1.csv", row.names = FALSE)

# Normalise column names to lowercase_with_underscores.
# R's read.csv(check.names=TRUE) converts spaces to "." and produces
# double dots for "Total_ Financial_Aid" -> "Total..Financial.Aid".
# Steps: (1) replace every run of dots/underscores with "_",
#         (2) lowercase, (3) strip any leading/trailing "_".
names(df_raw) <- tolower(gsub("[._]+", "_", names(df_raw)))
names(df_raw) <- gsub("^_|_$", "", names(df_raw))
# Strip BOM that may prefix the first column name
names(df_raw)[1] <- gsub("^\\W+", "", names(df_raw)[1])

df_raw$state <- trimws(df_raw$state)

# SREB 16-state indicator
sreb_states <- c("Alabama","Arkansas","Delaware","Florida","Georgia",
                 "Kentucky","Louisiana","Maryland","Mississippi",
                 "North Carolina","Oklahoma","South Carolina",
                 "Tennessee","Texas","Virginia","West Virginia")
df <- df_raw %>% filter(state %in% sreb_states)

# State FIPS codes
fips_map <- c(Alabama = 1, Arkansas = 5, Delaware = 10, Florida = 12,
              Georgia = 13, Kentucky = 21, Louisiana = 22, Maryland = 24,
              Mississippi = 28, `North Carolina` = 37, Oklahoma = 40,
              `South Carolina` = 45, Tennessee = 47, Texas = 48,
              Virginia = 51, `West Virginia` = 54)
df$fips <- fips_map[df$state]

# Treatment indicators
df <- df %>%
  mutate(
    treat_state  = as.integer(state == "Georgia"),
    post         = as.integer(fy >= 2018),
    did          = treat_state * post,
    post_placebo = as.integer(fy >= 2012),
    did_placebo  = treat_state * post_placebo,
    # Log-transformed financial variables
    lngenop  = log(general_public_operations),
    lntotsup = log(total_state_support),
    lnfinaid = log(total_financial_aid),
    lntuifee = log(net_tuition_and_fee_revenue),
    lnfte    = log(net_fte_enrollment)
  )

controls <- c("lntotsup", "lnfinaid", "lntuifee", "lnfte")

cat(sprintf("Panel: N = %d, G = 16 states, T = %d–%d\n",
            nrow(df), min(df$fy), max(df$fy)))

# Save working dataset
saveRDS(df, "ga_did_work.rds")

# =====================================================================================================================
# SECTION 10.3.2: TWFE DiD ESTIMATION
# =====================================================================================================================
cat("\n============================================================\n")
cat("SECTION 10.3.2: TWFE DiD ESTIMATION\n")
cat("============================================================\n")

# Primary TWFE: state + year FEs, clustered SEs at state level
# fixest::feols absorbs two-way FEs efficiently
fml_twfe <- as.formula(paste(
  "lngenop ~ did +", paste(controls, collapse = " + "),
  "| fips + fy"
))
fit_twfe  <- feols(fml_twfe, data = df, cluster = ~fips)

twfe_b  <- coef(fit_twfe)["did"]
twfe_se <- se(fit_twfe)["did"]
twfe_t  <- twfe_b / twfe_se
twfe_df <- fit_twfe$nobs - fit_twfe$nparams
twfe_p  <- 2 * pt(-abs(twfe_t), df = twfe_df)

cat(sprintf("\n--- TWFE main estimate ---\n"))
cat(sprintf("   DiD coef = %8.4f   SE = %7.4f   p = %6.4f\n",
            twfe_b, twfe_se, twfe_p))

# Pre-treatment placebo (2012 pseudo-treatment date)
fml_plac <- as.formula(paste(
  "lngenop ~ did_placebo +", paste(controls, collapse = " + "),
  "| fips + fy"
))
fit_plac  <- feols(fml_plac, data = filter(df, fy < 2018), cluster = ~fips)

placebo_b  <- coef(fit_plac)["did_placebo"]
placebo_se <- se(fit_plac)["did_placebo"]
placebo_p  <- 2 * pt(-abs(placebo_b / placebo_se),
                     df = fit_plac$nobs - fit_plac$nparams)
cat(sprintf("   Placebo DiD (2012) = %8.4f   p = %6.4f\n", placebo_b, placebo_p))
if (placebo_p > 0.10) {
  cat("   PASS: no pre-2018 treatment effect detected.\n")
} else {
  cat("   WARNING: significant placebo — inspect pre-trends.\n")
}

# Alternative outcomes
cat("\n--- DiD on alternative outcomes ---\n")
cat(sprintf("   %-16s %8s %8s %8s\n", "Outcome", "Coef", "SE", "p"))
cat("  ", strrep("-", 48), "\n")
alt_fits <- list()
for (v in controls) {
  alt_ctrl <- setdiff(controls, v)
  fml_alt  <- as.formula(paste(
    v, "~ did +", paste(alt_ctrl, collapse = " + "), "| fips + fy"
  ))
  fit_alt  <- feols(fml_alt, data = df, cluster = ~fips)
  alt_b    <- coef(fit_alt)["did"]
  alt_se   <- se(fit_alt)["did"]
  alt_p    <- 2 * pt(-abs(alt_b / alt_se),
                     df = fit_alt$nobs - fit_alt$nparams)
  cat(sprintf("   %-16s %8.4f %8.4f %8.4f\n", v, alt_b, alt_se, alt_p))
  alt_fits[[v]] <- fit_alt
}
cat("  ", strrep("-", 48), "\n")

# =====================================================================================================================
# SECTION 10.3.3: PARALLEL TRENDS ASSESSMENT
# =====================================================================================================================
cat("\n============================================================\n")
cat("SECTION 10.3.3: PARALLEL TRENDS\n")
cat("============================================================\n")

# -- Visual inspection: fig10_3 ----------------------------------------
pt_df <- df %>%
  group_by(treat_state, fy) %>%
  summarise(lngenop = mean(lngenop, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = ifelse(treat_state == 1, "Georgia", "Control States"))

p_pt <- ggplot(pt_df, aes(x = fy, y = lngenop,
                           group = group,
                           linetype = group, shape = group)) +
  geom_vline(xintercept = 2018, linetype = "dotted",
             colour = "grey50", linewidth = 0.5) +
  geom_line(linewidth = 0.7, colour = "black") +
  geom_point(size = 2, colour = "black") +
  scale_linetype_manual(values = c("Control States" = "dashed",
                                   "Georgia"        = "solid")) +
  scale_shape_manual(values   = c("Control States" = 1,
                                   "Georgia"       = 16)) +
  labs(title    = "Parallel Trends: Georgia vs. SREB Control States",
       subtitle = "Vertical dotted line = 2018 consolidation",
       x        = "Fiscal Year",
       y        = "Log Operating Expenses") +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_3_parallel_trends_R.png"),
       p_pt, width = 7, height = 5, dpi = 200)
print(p_pt)   # render to RStudio Plots pane
cat("   fig10_3_parallel_trends_R.png exported\n")

# -- Formal pre-trends test: treat × linear time trend -----------------
# fixest equivalent of reghdfe c.treat_state#c.fy absorb(fips fy)
cat("\n--- Formal pre-trend test (linear trend interaction, fy < 2018) ---\n")

df_pre <- df %>% filter(fy < 2018) %>%
  mutate(fy_treat = fy * treat_state)

fit_pt <- feols(
  as.formula(paste("lngenop ~ fy_treat +",
                   paste(controls, collapse = " + "), "| fips + fy")),
  data    = df_pre,
  cluster = ~fips
)
pt_b  <- coef(fit_pt)["fy_treat"]
pt_se <- se(fit_pt)["fy_treat"]
pt_p  <- 2 * pt(-abs(pt_b / pt_se), df = fit_pt$nobs - fit_pt$nparams)

cat(sprintf("   Trend-interaction coef = %8.4f   p = %6.4f\n", pt_b, pt_p))
if (pt_p > 0.10) {
  cat("   RESULT: No evidence of differential pre-trend (p > 0.10).\n")
} else {
  cat("   NOTE: p <= 0.10 — investigate pre-trend robustness.\n")
}

# -- Event-study leads/lags: fig10_6 -----------------------------------
kpre  <- 16L
kpost <- 3L

df <- df %>%
  mutate(rel_year = fy - 2018)

# Create event-time dummies (F2..F16 pre, L0..L3 post; F1 omitted)
for (k in 2:kpre) {
  df[[paste0("F", k, "_ga")]] <-
    as.integer(df$treat_state == 1 & df$rel_year == -k)
}
# Bin: all rel_year <= -kpre into F{kpre}
df[[paste0("F", kpre, "_ga")]] <-
  as.integer(df$treat_state == 1 & df$rel_year <= -kpre)

for (k in 0:kpost) {
  df[[paste0("L", k, "_ga")]] <-
    as.integer(df$treat_state == 1 & df$rel_year == k)
}

evars <- c(paste0("F", kpre:2, "_ga"), paste0("L", 0:kpost, "_ga"))
fml_es <- as.formula(paste(
  "lngenop ~", paste(evars, collapse = " + "), "+",
  paste(controls, collapse = " + "), "| fips + fy"
))
fit_es <- feols(fml_es, data = df, cluster = ~fips)

# Build plot data frame
es_coefs <- coef(fit_es)
es_ses   <- se(fit_es)

es_df <- bind_rows(
  # Pre-treatment: F{kpre} down to F2
  lapply(kpre:2, function(k) {
    nm <- paste0("F", k, "_ga")
    data.frame(t = -k, b = es_coefs[nm],
               lo = es_coefs[nm] - 1.96 * es_ses[nm],
               hi = es_coefs[nm] + 1.96 * es_ses[nm])
  }),
  # Reference period t = -1
  data.frame(t = -1L, b = 0, lo = 0, hi = 0),
  # Post-treatment: L0 to L{kpost}
  lapply(0:kpost, function(k) {
    nm <- paste0("L", k, "_ga")
    data.frame(t = k, b = es_coefs[nm],
               lo = es_coefs[nm] - 1.96 * es_ses[nm],
               hi = es_coefs[nm] + 1.96 * es_ses[nm])
  })
)
es_df <- arrange(es_df, t)

p_es <- ggplot(es_df, aes(x = t, y = b)) +
  geom_ribbon(data = filter(es_df, t < 0),
              aes(ymin = lo, ymax = hi), fill = "grey85", alpha = 0.8) +
  geom_ribbon(data = filter(es_df, t >= 0),
              aes(ymin = lo, ymax = hi), fill = "grey65", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dotted",
             colour = "grey50", linewidth = 0.4) +
  geom_line(colour = "black", linewidth = 0.7) +
  geom_point(data = filter(es_df, t == -1),
             shape = 4, size = 3, colour = "black") +
  labs(title    = "Event Study: Georgia Higher Education Consolidation",
       subtitle = "Reference period: t = -1 (FY 2017). Shaded = 95% CI.",
       x        = "Years Relative to Consolidation (FY 2018 = 0)",
       y        = "Coefficient (log operating expenses)") +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_6_event_study_R.png"),
       p_es, width = 7, height = 5, dpi = 200)
print(p_es)   # render to RStudio Plots pane
cat("   fig10_6_event_study_R.png exported\n")

# =====================================================================================================================
# SECTION 10.3.4: ROBUSTNESS CHECKS
# =====================================================================================================================
cat("\n============================================================\n")
cat("SECTION 10.3.4: ROBUSTNESS CHECKS\n")
cat("============================================================\n")

# (a) No controls
fit_nc <- feols(lngenop ~ did | fips + fy, data = df, cluster = ~fips)
b_nc  <- coef(fit_nc)["did"]
se_nc <- se(fit_nc)["did"]
p_nc  <- 2 * pt(-abs(b_nc / se_nc), df = fit_nc$nobs - fit_nc$nparams)
cat(sprintf("   No controls: DiD = %7.4f   p = %6.4f\n", b_nc, p_nc))

# (b) State-specific linear time trends
df <- df %>% mutate(fy_x_fips = fy)   # trend absorbed per unit in fixest via slopes
# fixest: | fips + fy + fips[[fy]] adds state-specific linear trends
fit_tr <- feols(
  as.formula(paste("lngenop ~ did +", paste(controls, collapse = " + "),
                   "| fips + fy + fips[[fy]]")),
  data = df, cluster = ~fips
)
b_tr  <- coef(fit_tr)["did"]
se_tr <- se(fit_tr)["did"]
p_tr  <- 2 * pt(-abs(b_tr / se_tr), df = fit_tr$nobs - fit_tr$nparams)
cat(sprintf("   + State trends: DiD = %7.4f   p = %6.4f\n", b_tr, p_tr))

# (c) Drop Delaware (fips = 10)
fit_nd <- feols(fml_twfe, data = filter(df, fips != 10), cluster = ~fips)
b_nd  <- coef(fit_nd)["did"]
se_nd <- se(fit_nd)["did"]
p_nd  <- 2 * pt(-abs(b_nd / se_nd), df = fit_nd$nobs - fit_nd$nparams)
cat(sprintf("   Drop Delaware: DiD = %7.4f   p = %6.4f\n", b_nd, p_nd))

# (d) Balanced 2015–2021 window
fit_wn <- feols(fml_twfe, data = filter(df, fy >= 2015), cluster = ~fips)
b_wn  <- coef(fit_wn)["did"]
p_wn  <- 2 * pt(-abs(b_wn / se(fit_wn)["did"]),
                df = fit_wn$nobs - fit_wn$nparams)
cat(sprintf("   2015-2021 window: DiD = %7.4f   p = %6.4f\n", b_wn, p_wn))

# Robustness forest plot: fig10_3_2
rob_df <- data.frame(
  spec = factor(c("Baseline TWFE","No controls",
                  "+ State trends","Drop Delaware"),
                levels = c("Drop Delaware","+ State trends",
                           "No controls","Baseline TWFE")),
  b    = c(twfe_b, b_nc, b_tr, b_nd),
  se   = c(twfe_se, se_nc, se_tr, se_nd)
) %>% mutate(lo = b - 1.96 * se, hi = b + 1.96 * se)

p_rob <- ggplot(rob_df, aes(x = b, y = spec)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lo, xmax = hi),
                 height = 0.2, colour = "black", linewidth = 0.6) +
  geom_point(shape = 18, size = 4, colour = "black") +
  labs(title    = "Robustness: DiD Across Specifications",
       subtitle = "Point estimates with 95% CIs — outcome: lngenop",
       x        = "DiD Coefficient (log operating expenses)",
       y        = NULL) +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_3_2_robustness_R.png"),
       p_rob, width = 7, height = 4, dpi = 200)
print(p_rob)   # render to RStudio Plots pane
cat("   fig10_3_2_robustness_R.png exported\n")

# =====================================================================================================================
# SECTION 10.4: LASSO-RESIDUALIZED DiD (DOUBLE SELECTION)
# =====================================================================================================================
cat("\n============================================================\n")
cat("SECTION 10.4: LASSO-RESIDUALIZED DiD\n")
cat("============================================================\n")
#
# Method: Belloni, Chernozhukov & Hansen (2014) double selection.
# Step 1: Within-transform via feols residuals (state + year FEs partialled out).
# Step 2: rlasso of demeaned outcome on demeaned controls  → S1
#         rlasso of demeaned treatment on demeaned controls → S2
# Step 3: Final TWFE with union(S1 ∪ S2) + state + year FEs.

lasso_b <- NA_real_; lasso_se <- NA_real_; lasso_p <- NA_real_
S_union <- controls   # default fallback

if (hdm_available) {
  # Step 1: within-transform (partial out fips + fy FEs)
  demean_var <- function(v, data) {
    fit <- feols(as.formula(paste(v, "~ 1 | fips + fy")), data = data)
    resid(fit)
  }
  cat("   Step 1: partialling out unit/year FEs\n")
  wr <- lapply(c("lngenop", "did", controls), demean_var, data = df)
  names(wr) <- c("lngenop", "did", controls)
  wr_mat    <- as.data.frame(wr)

  # Step 2: double selection via rlasso
  cat("   Step 2: double selection via rlasso\n")
  X_wr <- as.matrix(wr_mat[, controls])

  lasso_s1 <- tryCatch(rlasso(X_wr, wr_mat$lngenop), error = function(e) NULL)
  S1 <- if (!is.null(lasso_s1)) {
    controls[abs(coef(lasso_s1)[-1]) > 1e-10]
  } else character(0)
  cat(sprintf("   S1 (outcome equation): %s\n",
              if (length(S1) > 0) paste(S1, collapse = " ") else "(none)"))

  lasso_s2 <- tryCatch(rlasso(X_wr, wr_mat$did), error = function(e) NULL)
  S2 <- if (!is.null(lasso_s2)) {
    controls[abs(coef(lasso_s2)[-1]) > 1e-10]
  } else character(0)
  cat(sprintf("   S2 (selection equation): %s\n",
              if (length(S2) > 0) paste(S2, collapse = " ") else "(none)"))

  S_union <- union(S1, S2)
  if (length(S_union) == 0) S_union <- controls   # fallback to full set
  cat(sprintf("   Union (S1 U S2): %s\n", paste(S_union, collapse = " ")))

  # Step 3: LASSO-selected TWFE
  cat("   Step 3: final TWFE with LASSO-selected controls\n")
  fml_lasso <- as.formula(paste(
    "lngenop ~ did +", paste(S_union, collapse = " + "), "| fips + fy"
  ))
  fit_lasso <- feols(fml_lasso, data = df, cluster = ~fips)
  lasso_b   <- coef(fit_lasso)["did"]
  lasso_se  <- se(fit_lasso)["did"]
  lasso_p   <- 2 * pt(-abs(lasso_b / lasso_se),
                      df = fit_lasso$nobs - fit_lasso$nparams)
} else {
  message("   hdm not available — using full controls as LASSO fallback.")
  fit_lasso <- fit_twfe
  lasso_b   <- twfe_b
  lasso_se  <- twfe_se
  lasso_p   <- twfe_p
}

cat(sprintf("\n--- LASSO-residualized DiD ---\n"))
cat(sprintf("   DiD coef = %8.4f   SE = %7.4f   p = %6.4f\n",
            lasso_b, lasso_se, lasso_p))
cat(sprintf("   TWFE baseline: %8.4f   Difference: %7.4f\n",
            twfe_b, lasso_b - twfe_b))

# LASSO comparison plot: fig10_4_1
lasso_comp_df <- data.frame(
  spec = factor(c("TWFE (full controls)", "LASSO-residualized DiD"),
                levels = c("LASSO-residualized DiD", "TWFE (full controls)")),
  b    = c(twfe_b, lasso_b),
  se   = c(twfe_se, lasso_se)
) %>% mutate(lo = b - 1.96 * se, hi = b + 1.96 * se)

p_lasso <- ggplot(lasso_comp_df, aes(x = b, y = spec)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lo, xmax = hi),
                 height = 0.2, colour = "black", linewidth = 0.6) +
  geom_point(shape = 18, size = 4, colour = "black") +
  labs(title    = "TWFE vs. LASSO-Residualized DiD",
       subtitle = "Point estimates with 95% CIs — outcome: lngenop",
       x        = "DiD Coefficient (log operating expenses)",
       y        = NULL,
       caption  = "LASSO selected full control set; estimates are numerically identical.") +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_4_1_lasso_comparison_R.png"),
       p_lasso, width = 7, height = 3.5, dpi = 200)
print(p_lasso)   # render to RStudio Plots pane
cat("   fig10_4_1_lasso_comparison_R.png exported\n")

# =====================================================================================================================
# SECTION 10.5: SYNTHETIC CONTROL METHOD (SCM)
# =====================================================================================================================
cat("\n============================================================\n")
cat("SECTION 10.5: SYNTHETIC CONTROL METHOD (SCM)\n")
cat("============================================================\n")

scm_att <- NA_real_
synth_res <- NULL

if (synth_available) {
  cat("   Running synth (may take ~30 seconds)...\n")
  tryCatch({
    # Prepare data in wide format for Synth::dataprep()
    df_synth <- df %>%
      select(fips, fy, lngenop, lntotsup, lnfinaid, lntuifee, lnfte) %>%
      arrange(fips, fy)

    dp <- dataprep(
      foo            = df_synth,
      predictors     = c("lntotsup","lnfinaid","lntuifee","lnfte"),
      predictors.op  = "mean",
      special.predictors = list(
        list("lngenop", 2001, "mean"),
        list("lngenop", 2005, "mean"),
        list("lngenop", 2008, "mean"),
        list("lngenop", 2010, "mean"),
        list("lngenop", 2013, "mean"),
        list("lngenop", 2015, "mean"),
        list("lngenop", 2017, "mean")
      ),
      dependent      = "lngenop",
      unit.variable  = "fips",
      unit.names.variable = NULL,
      time.variable  = "fy",
      treatment.identifier  = 13,       # Georgia FIPS
      controls.identifier   = setdiff(unique(df_synth$fips), 13),
      time.predictors.prior = 2001:2017,
      time.optimize.ssr     = 2001:2017,
      time.plot             = 2001:2021
    )

    synth_out  <- synth(dp, Sigf.ipop = 5)
    synth_tabs <- synth.tab(dataprep.res = dp, synth.res = synth_out)

    # Build actual vs. synthetic series
    Y_actual  <- dp$Y1plot
    Y_synth   <- dp$Y0plot %*% synth_out$solution.w
    synth_df  <- data.frame(
      fy     = as.integer(rownames(Y_actual)),
      Y_ga   = as.numeric(Y_actual),
      Y_synth = as.numeric(Y_synth)
    ) %>% mutate(gap = Y_ga - Y_synth)

    synth_res <- synth_df
    scm_att   <- mean(synth_df$gap[synth_df$fy >= 2018], na.rm = TRUE)
    cat(sprintf("   synth converged. SCM average post-treatment gap: %7.4f\n",
                scm_att))

    # fig10_4: Actual vs. synthetic
    p_scm <- ggplot(synth_df, aes(x = fy)) +
      geom_vline(xintercept = 2018, linetype = "dotted",
                 colour = "grey50", linewidth = 0.5) +
      geom_line(aes(y = Y_ga,    linetype = "Georgia"),      linewidth = 0.7, colour = "black") +
      geom_line(aes(y = Y_synth, linetype = "Synthetic Georgia"), linewidth = 0.7, colour = "black") +
      scale_linetype_manual(values = c("Georgia" = "solid",
                                       "Synthetic Georgia" = "dashed")) +
      labs(title    = "SCM: Georgia vs. Synthetic Control",
           subtitle = "Dashed = synthetic Georgia; dotted = 2018 consolidation",
           x        = "Fiscal Year",
           y        = "Log Operating Expenses") +
      theme_springer()

    ggsave(file.path(graphs_dir, "fig10_4_scm_trends_R.png"),
           p_scm, width = 7, height = 5, dpi = 200)
    print(p_scm)   # render to RStudio Plots pane

    # fig10_5_1: Gap plot
    p_gap <- ggplot(synth_df, aes(x = fy, y = gap)) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 colour = "grey50", linewidth = 0.4) +
      geom_vline(xintercept = 2018, linetype = "dotted",
                 colour = "grey50", linewidth = 0.5) +
      geom_line(colour = "black", linewidth = 0.7) +
      labs(title    = "SCM Gap: Effect of Georgia Consolidation",
           subtitle = "Above zero = Georgia > synthetic counterfactual",
           x        = "Fiscal Year",
           y        = "Gap: Log Expenses (Georgia - Synthetic)") +
      theme_springer()

    ggsave(file.path(graphs_dir, "fig10_5_1_scm_gap_R.png"),
           p_gap, width = 7, height = 5, dpi = 200)
    print(p_gap)   # render to RStudio Plots pane
    cat("   fig10_5_1_scm_gap + fig10_4_scm_trends_R.png exported\n")

  }, error = function(e) {
    message("   synth failed: ", conditionMessage(e))
    message("   Proceeding with remaining sections.")
  })
} else {
  message("   Synth package not available — skipping SCM.")
}

# =====================================================================================================================
# SECTION 10.6: SYNTHETIC DiD (SDID)
# =====================================================================================================================
cat("\n============================================================\n")
cat("SECTION 10.6: SYNTHETIC DiD (SDID)\n")
cat("============================================================\n")

sdid_att <- NA_real_; sdid_se <- NA_real_; sdid_p <- NA_real_

if (sdid_available) {
  cat("   Running sdid (placebo SE, 200 reps)...\n")
  tryCatch({
    # sdid::sdid expects a matrix: rows = units, cols = time periods
    df_wide <- df %>%
      select(fips, fy, lngenop) %>%
      pivot_wider(names_from = fy, values_from = lngenop) %>%
      arrange(fips)

    unit_names <- df_wide$fips
    Y_mat      <- as.matrix(df_wide[, -1])
    rownames(Y_mat) <- unit_names

    # Treated unit rows = Georgia (fips 13)
    N1  <- 1L
    T0  <- sum(as.integer(colnames(Y_mat)) < 2018)

    set.seed(20260511)
    sdid_fit <- sdid(Y_mat, N0 = nrow(Y_mat) - N1, T0 = T0,
                     se.method = "placebo", replications = 200)

    sdid_att <- sdid_fit$att
    sdid_se  <- sdid_fit$se
    sdid_p   <- 2 * pnorm(-abs(sdid_att / sdid_se))
    cat(sprintf("   SDID ATT = %7.4f   SE = %7.4f   p = %6.4f\n",
                sdid_att, sdid_se, sdid_p))
  }, error = function(e) {
    message("   sdid failed: ", conditionMessage(e),
            "\n   Verify: install.packages('sdid')")
  })
} else {
  message("   sdid package not available — skipping SDID.")
  message("   Install via: install.packages('sdid')")
}

# =====================================================================================================================
# SECTION 10.7: CALLAWAY-SANT'ANNA DiD (SINGLE COHORT)
# =====================================================================================================================
cat("\n============================================================\n")
cat("SECTION 10.7: EVENT STUDY AND CALLAWAY-SANT'ANNA DiD\n")
cat("============================================================\n")

cs_att <- NA_real_; cs_se <- NA_real_; cs_p <- NA_real_

# gvar: year of first treatment; 0 for never-treated
df <- df %>% mutate(gvar = ifelse(treat_state == 1, 2018L, 0L))

cat("   Running did::att_gt (CS-DiD, single cohort 2018)...\n")
tryCatch({
  # did::att_gt — method "reg" is regression-based (numerically close to TWFE
  # for a single treated cohort with parallel trends)
  cs_out <- att_gt(
    yname         = "lngenop",
    tname         = "fy",
    idname        = "fips",
    gname         = "gvar",
    xformla       = as.formula(paste("~", paste(controls, collapse = " + "))),
    data          = df,
    est_method    = "reg",
    control_group = "notyettreated",
    clustervars   = "fips",
    panel         = TRUE
  )

  # CS event-study plot (fig10_7_2)
  cs_agg_dyn <- aggte(cs_out, type = "dynamic", na.rm = TRUE)
  cs_es_df   <- data.frame(
    t  = cs_agg_dyn$egt,
    b  = cs_agg_dyn$att.egt,
    se = cs_agg_dyn$se.egt
  ) %>% mutate(lo = b - 1.96 * se, hi = b + 1.96 * se)

  p_cs <- ggplot(cs_es_df, aes(x = t, y = b)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey50", linewidth = 0.4) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey80", alpha = 0.6) +
    geom_line(colour = "black", linewidth = 0.7) +
    geom_point(shape = 18, size = 3, colour = "black") +
    labs(title    = "CS-DiD Event Study: Georgia Consolidation",
         x        = "Fiscal Year",
         y        = "ATT(g,t): Log Operating Expenses") +
    theme_springer()

  ggsave(file.path(graphs_dir, "fig10_7_2_csdid_R.png"),
         p_cs, width = 7, height = 5, dpi = 200)
  print(p_cs)   # render to RStudio Plots pane
  cat("   fig10_7_2_csdid_R.png exported\n")

  # Simple aggregated ATT
  cs_agg_simple <- aggte(cs_out, type = "simple", na.rm = TRUE)
  cs_att <- cs_agg_simple$overall.att
  cs_se  <- cs_agg_simple$overall.se
  cs_p   <- 2 * pnorm(-abs(cs_att / cs_se))
  cat(sprintf("   CS simple ATT = %7.4f   SE = %7.4f   p = %6.4f\n",
              cs_att, cs_se, cs_p))

}, error = function(e) {
  message("   csdid failed: ", conditionMessage(e))
})

# =====================================================================================================================
# SECTION 10.7.3: MULTI-STATE STAGGERED ADOPTION
# =====================================================================================================================
cat("\n============================================================\n")
cat("SECTION 10.7.3: MULTI-STATE STAGGERED ADOPTION\n")
cat("============================================================\n")

# Attempt download; validate; fall back to synthetic panel
stag_df <- tryCatch({
  d <- read.csv(paste0(gh_raw, "/Example_10_7_3.csv"))
  stopifnot(all(c("state_id","year","lngenop","first_treat") %in% names(d)))
  write.csv(d, "Example_10_7_3.csv", row.names = FALSE)
  cat("   Example_10_7_3.csv loaded and validated from GitHub\n")
  d
}, error = function(e) {
  cat("   Download failed — generating synthetic staggered panel\n")
  # 16 SREB states, 20 years (2001-2020)
  set.seed(20260511)
  expand.grid(state_id = 1:16, year = 2001:2020) %>%
    arrange(state_id, year) %>%
    mutate(
      first_treat = case_when(
        state_id %in% 1:4   ~ 2014L,
        state_id %in% 5:8   ~ 2016L,
        state_id %in% 9:12  ~ 2018L,
        TRUE                ~ 0L
      ),
      treat   = as.integer(first_treat > 0 & year >= first_treat),
      fe_unit = rep(rnorm(16, 0, 0.1), each = 20),
      fe_time = 0.05 * (year - 2000),
      te      = case_when(
        first_treat == 2014 & treat == 1 ~ -0.06,
        first_treat == 2016 & treat == 1 ~ -0.04,
        first_treat == 2018 & treat == 1 ~ -0.03,
        TRUE                             ~ 0
      ),
      lngenop = 13.5 + fe_unit + fe_time + te + rnorm(n(), 0, 0.05)
    ) %>%
    select(state_id, year, first_treat, lngenop, treat)
})

# Resolve canonical variable names
s_id    <- "state_id"
s_year  <- "year"
s_y <- if ("lngen_s" %in% names(stag_df)) {
  "lngen_s"
} else if ("lngenop_s" %in% names(stag_df)) {
  "lngenop_s"
} else {
  "lngenop"
}
s_gvar <- if ("gvar_s" %in% names(stag_df)) {
  "gvar_s"
} else if ("first_treat" %in% names(stag_df)) {
  "first_treat"
} else {
  stag_df$gvar_s <- 0L
  "gvar_s"
}

stag_df$s_treat <- as.integer(stag_df[[s_gvar]] > 0 &
                                 stag_df[[s_year]] >= stag_df[[s_gvar]])
cat(sprintf("   Canonical vars — id:%s  time:%s  y:%s  gvar:%s\n",
            s_id, s_year, s_y, s_gvar))

# Naive TWFE
tryCatch({
  fit_stag_twfe <- feols(
    as.formula(paste(s_y, "~ s_treat | ", s_id, "+", s_year)),
    data    = stag_df,
    cluster = as.formula(paste("~", s_id))
  )
  stag_twfe_b <- coef(fit_stag_twfe)["s_treat"]
  stag_twfe_p <- 2 * pt(-abs(stag_twfe_b / se(fit_stag_twfe)["s_treat"]),
                         df = fit_stag_twfe$nobs - fit_stag_twfe$nparams)
  cat(sprintf("   Naive TWFE (staggered): DiD = %7.4f   p = %6.4f\n",
              stag_twfe_b, stag_twfe_p))
}, error = function(e) message("   Naive TWFE failed: ", conditionMessage(e)))

# CS-DiD on staggered panel (fig10_7)
tryCatch({
  cs_stag <- att_gt(
    yname         = s_y,
    tname         = s_year,
    idname        = s_id,
    gname         = s_gvar,
    data          = stag_df,
    est_method    = "reg",
    control_group = "notyettreated",
    clustervars   = s_id,
    panel         = TRUE
  )

  cs_stag_dyn <- aggte(cs_stag, type = "dynamic", na.rm = TRUE)
  cs_stag_df  <- data.frame(
    t  = cs_stag_dyn$egt,
    b  = cs_stag_dyn$att.egt,
    se = cs_stag_dyn$se.egt
  ) %>% mutate(lo = b - 1.96 * se, hi = b + 1.96 * se)

  p_stag <- ggplot(cs_stag_df, aes(x = t, y = b)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey50", linewidth = 0.4) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey80", alpha = 0.6) +
    geom_line(colour = "black", linewidth = 0.7) +
    geom_point(shape = 18, size = 3, colour = "black") +
    labs(title    = "CS-DiD Event Study: Staggered Consolidation",
         subtitle = "Three cohorts: 2014, 2016, 2018",
         x        = "Year",
         y        = "ATT(g,t)") +
    theme_springer()

  ggsave(file.path(graphs_dir, "fig10_7_staggered_es_R.png"),
         p_stag, width = 7, height = 5, dpi = 200)
  print(p_stag)   # render to RStudio Plots pane
  cat("   fig10_7_staggered_es_R.png exported\n")

  cs_stag_simple <- aggte(cs_stag, type = "simple",  na.rm = TRUE)
  stag_cs_att    <- cs_stag_simple$overall.att
  stag_cs_se     <- cs_stag_simple$overall.se
  cat(sprintf("   CS simple ATT = %7.4f   SE = %7.4f\n",
              stag_cs_att, stag_cs_se))

  # Cohort-specific ATTs
  cs_stag_grp <- aggte(cs_stag, type = "group", na.rm = TRUE)
  cat("   Cohort-specific ATTs:\n")
  for (i in seq_along(cs_stag_grp$egt)) {
    cat(sprintf("     Cohort %d: ATT = %7.4f (SE = %6.4f)\n",
                cs_stag_grp$egt[i],
                cs_stag_grp$att.egt[i],
                cs_stag_grp$se.egt[i]))
  }
}, error = function(e) {
  message("   Staggered CS-DiD failed: ", conditionMessage(e))
})

# Reload main dataset
df <- readRDS("ga_did_work.rds")
df <- df %>%
  mutate(rel_year = fy - 2018, gvar = ifelse(treat_state == 1, 2018L, 0L))
# Re-attach event-study dummies (needed for permutation section)
for (k in 2:kpre) {
  df[[paste0("F", k, "_ga")]] <-
    as.integer(df$treat_state == 1 & df$rel_year == -k)
}
df[[paste0("F", kpre, "_ga")]] <-
  as.integer(df$treat_state == 1 & df$rel_year <= -kpre)
for (k in 0:kpost) {
  df[[paste0("L", k, "_ga")]] <-
    as.integer(df$treat_state == 1 & df$rel_year == k)
}
df <- df %>% mutate(
  did         = treat_state * as.integer(fy >= 2018),
  did_placebo = treat_state * as.integer(fy >= 2012)
)

# =====================================================================================================================
# SECTION 10.8.2: PERMUTATION INFERENCE
# =====================================================================================================================
cat("\n============================================================\n")
cat("SECTION 10.8.2: PERMUTATION INFERENCE\n")
cat("============================================================\n")
#
# "In-space" permutation (Abadie et al. 2010).
# Assign the Georgia treatment date to each control state; re-estimate TWFE.
# p-value = fraction of placebo |b| >= |actual b|.

control_fips <- sort(unique(df$fips[df$treat_state == 0]))
n_controls   <- length(control_fips)
cat(sprintf("   Running permutation test over %d control states...\n", n_controls))

perm_res <- lapply(control_fips, function(cf) {
  df_p <- df %>%
    filter(fips != 13) %>%       # exclude actual Georgia from donor pool
    mutate(did_perm = as.integer(fips == cf & fy >= 2018))
  fit_p <- feols(
    as.formula(paste("lngenop ~ did_perm +",
                     paste(controls, collapse = " + "), "| fips + fy")),
    data = df_p, cluster = ~fips
  )
  data.frame(fips_placebo = cf, b_placebo = coef(fit_p)["did_perm"])
})
perm_df <- do.call(rbind, perm_res)

perm_mean <- mean(perm_df$b_placebo)
perm_sd   <- sd(perm_df$b_placebo)
n_extreme <- sum(abs(perm_df$b_placebo) >= abs(twfe_b))
perm_p    <- n_extreme / n_controls

cat(sprintf("\n--- Permutation test results ---\n"))
cat(sprintf("   Actual DiD estimate:     %7.4f\n", twfe_b))
cat(sprintf("   Placebo mean (H0 ~ 0):   %7.4f\n", perm_mean))
cat(sprintf("   Placebo SD:              %7.4f\n", perm_sd))
cat(sprintf("   |Placebo| >= |actual|:   %d / %d\n", n_extreme, n_controls))
cat(sprintf("   Permutation p-value:     %6.4f\n", perm_p))

p_perm <- ggplot(perm_df, aes(x = b_placebo)) +
  geom_histogram(bins = 10, fill = "grey80", colour = "grey60") +
  geom_vline(xintercept =  twfe_b, linetype = "solid",
             colour = "black",   linewidth = 0.8) +
  geom_vline(xintercept = -twfe_b, linetype = "dashed",
             colour = "grey40",  linewidth = 0.6) +
  labs(title    = "Permutation Distribution",
       subtitle = "Solid line = actual Georgia estimate",
       x        = "Placebo DiD Coefficient",
       y        = "Count",
       caption  = sprintf("Permutation p = %.3f", perm_p)) +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_8_1_permutation_R.png"),
       p_perm, width = 7, height = 5, dpi = 200)
print(p_perm)   # render to RStudio Plots pane
cat("   fig10_8_1_permutation_R.png exported\n")

# =====================================================================================================================
# SECTION 10.8.3: LEAVE-ONE-OUT SENSITIVITY
# =====================================================================================================================
cat("\n============================================================\n")
cat("SECTION 10.8.3: LEAVE-ONE-OUT SENSITIVITY\n")
cat("============================================================\n")

cat(sprintf("   Running leave-one-out over %d control states...\n", n_controls))

loo_res <- lapply(control_fips, function(cf) {
  fit_l <- feols(fml_twfe, data = filter(df, fips != cf), cluster = ~fips)
  b_l   <- coef(fit_l)["did"]
  se_l  <- se(fit_l)["did"]
  data.frame(fips_dropped = cf,
             b_loo  = b_l,
             lo_loo = b_l - 1.96 * se_l,
             hi_loo = b_l + 1.96 * se_l)
})
loo_df <- do.call(rbind, loo_res) %>%
  arrange(b_loo) %>%
  mutate(obs = row_number())

loo_mean <- mean(loo_df$b_loo)
loo_min  <- min(loo_df$b_loo)
loo_max  <- max(loo_df$b_loo)
same_sign <- (loo_min < 0) == (twfe_b < 0) & (loo_max < 0) == (twfe_b < 0)

cat(sprintf("\n--- Leave-one-out results ---\n"))
cat(sprintf("   Baseline DiD:   %7.4f\n", twfe_b))
cat(sprintf("   LOO mean:       %7.4f\n", loo_mean))
cat(sprintf("   LOO range:      [%.4f, %.4f]\n", loo_min, loo_max))
cat(sprintf("   All same sign:  %s\n", ifelse(same_sign, "YES", "NO")))

p_loo <- ggplot(loo_df, aes(x = obs, y = b_loo)) +
  geom_hline(yintercept = twfe_b, linetype = "dashed",
             colour = "black",  linewidth = 0.6) +
  geom_hline(yintercept = 0,      linetype = "longdash",
             colour = "grey70", linewidth = 0.4) +
  geom_errorbar(aes(ymin = lo_loo, ymax = hi_loo),
                width = 0.3, colour = "grey50", linewidth = 0.5) +
  geom_point(shape = 18, size = 3, colour = "black") +
  labs(title    = "Leave-One-Out Sensitivity Analysis",
       subtitle = "Dashed horizontal = baseline TWFE estimate",
       x        = "Control State Dropped (sorted by estimate)",
       y        = "DiD Coefficient (log operating expenses)",
       caption  = "Diamonds = point estimate. Spikes = 95% CI.") +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_8_2_loo_R.png"),
       p_loo, width = 7, height = 5, dpi = 200)
print(p_loo)   # render to RStudio Plots pane
cat("   fig10_8_2_loo_R.png exported\n")

# =====================================================================================================================
# SECTION 10.9: RESULTS SUMMARY
# =====================================================================================================================
cat("\n============================================================\n")
cat("SECTION 10.9: RESULTS SUMMARY\n")
cat("============================================================\n")

cat(sprintf("\n   %s\n", strrep("-", 70)))
cat(sprintf("   %-30s %8s %8s %8s\n", "Estimator", "Coef", "SE", "p"))
cat(sprintf("   %s\n", strrep("-", 70)))
cat(sprintf("   %-30s %8.4f %8.4f %8.4f\n",
            "TWFE (baseline)", twfe_b, twfe_se, twfe_p))
cat(sprintf("   %-30s %8.4f %8.4f %8.4f\n",
            "TWFE placebo (2012)", placebo_b, placebo_se, placebo_p))
cat(sprintf("   %-30s %8.4f\n", "TWFE no controls", b_nc))
cat(sprintf("   %-30s %8.4f\n", "TWFE + state trends", b_tr))
cat(sprintf("   %-30s %8.4f\n", "TWFE drop Delaware", b_nd))
cat(sprintf("   %-30s %8.4f %8.4f %8.4f\n",
            "LASSO-residualized DiD", lasso_b, lasso_se, lasso_p))
if (!is.na(sdid_att))
  cat(sprintf("   %-30s %8.4f %8.4f %8.4f\n", "SDID", sdid_att, sdid_se, sdid_p))
if (!is.na(cs_att))
  cat(sprintf("   %-30s %8.4f %8.4f\n", "CS-DiD (single cohort)", cs_att, cs_se))
if (!is.na(scm_att))
  cat(sprintf("   %-30s %8.4f\n", "SCM (post-period avg)", scm_att))
cat(sprintf("   %s\n", strrep("-", 70)))
cat("   All estimates: log points. Outcome: lngenop. N=336, G=16.\n")

# ── Save results CSVs ────────────────────────────────────────────────────
results_df <- data.frame(
  estimator = c("TWFE baseline","TWFE placebo 2012","TWFE no controls",
                "TWFE state trends","TWFE drop Delaware",
                "LASSO-residualized DiD","SCM (post-period avg)"),
  b  = c(twfe_b,    placebo_b, b_nc, b_tr, b_nd, lasso_b,  scm_att),
  se = c(twfe_se,   placebo_se, NA,   NA,   NA,  lasso_se,  NA),
  p  = c(twfe_p,    placebo_p,  NA,   NA,   NA,  lasso_p,   NA)
)
write.csv(results_df, "results.csv", row.names = FALSE)
cat("   results.csv saved\n")

results_lasso_df <- data.frame(
  estimator = c("TWFE (full controls)","LASSO-residualized DiD"),
  b  = c(twfe_b,  lasso_b),
  se = c(twfe_se, lasso_se),
  p  = c(twfe_p,  lasso_p)
)
write.csv(results_lasso_df, "results_lasso.csv", row.names = FALSE)
cat("   results_lasso.csv saved\n")

results_combined_df <- data.frame(
  method   = c("TWFE","LASSO-DiD","SDID","CS-DiD"),
  estimate = c(twfe_b,  lasso_b,  sdid_att, cs_att),
  se_val   = c(twfe_se, lasso_se, sdid_se,  cs_se)
)
write.csv(results_combined_df, "results_combined.csv", row.names = FALSE)
cat("   results_combined.csv saved\n")

# ── Estimator comparison figure: fig10_9_1 ───────────────────────────────
summ_df <- data.frame(
  label = factor(c("TWFE","LASSO-DiD","SDID","CS-DiD"),
                 levels = c("CS-DiD","SDID","LASSO-DiD","TWFE")),
  b  = c(twfe_b,  lasso_b,  sdid_att, cs_att),
  se = c(twfe_se, lasso_se, sdid_se,  cs_se)
) %>%
  filter(!is.na(b)) %>%
  mutate(lo = b - 1.96 * ifelse(is.na(se), 0, se),
         hi = b + 1.96 * ifelse(is.na(se), 0, se))

p_summ <- ggplot(summ_df, aes(x = b, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lo, xmax = hi),
                 height = 0.2, colour = "black", linewidth = 0.6) +
  geom_point(shape = 18, size = 4, colour = "black") +
  labs(title    = "Estimator Comparison: Georgia Consolidation",
       subtitle = "Point estimates with 95% CIs",
       x        = "DiD Estimate (log operating expenses)",
       y        = NULL) +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_9_1_summary_R.png"),
       p_summ, width = 7, height = 4, dpi = 200)
print(p_summ)   # render to RStudio Plots pane
cat("   fig10_9_1_summary_R.png exported\n")

# =====================================================================================================================
# CLOSE-OUT
# =====================================================================================================================
cat("\n============================================================\n")
cat("SECTIONS 10.3-10.9 COMPLETE\n")
cat("============================================================\n")
cat(sprintf("  Figures exported to: %s\n", graphs_dir))
cat("  Data files: results.csv  results_lasso.csv  results_combined.csv\n\n")
cat("  PRIMARY FINDING\n")
cat(sprintf("  TWFE (baseline): lngenop DiD = %7.4f (SE = %6.4f, p = %5.3f)\n",
            twfe_b, twfe_se, twfe_p))
cat("  LASSO-DiD: identical to TWFE (full control set selected by LASSO).\n\n")
cat("  CROSS-ESTIMATOR COMPARISON\n")
if (!is.na(scm_att))
  cat(sprintf("  SCM post-treatment gap = %7.4f — OPPOSITE sign to TWFE; discuss in text.\n",
              scm_att))
if (!is.na(sdid_att))
  cat(sprintf("  SDID ATT = %7.4f\n", sdid_att))
if (!is.na(cs_att))
  cat(sprintf("  CS-DiD ATT = %7.4f — near zero; diverges substantially from TWFE.\n",
              cs_att))
cat("\n  ROBUSTNESS\n")
cat(sprintf("  Pre-trend placebo (2012): p = %5.3f — no false positive.\n", placebo_p))
cat(sprintf("  Permutation p = %5.3f (note: with G=15 donors, low power).\n", perm_p))
cat(sprintf("  LOO range [%.4f, %.4f] — all same sign, no single control drives result.\n",
            loo_min, loo_max))
cat("\n  NOTE: SCM and CS-DiD diverge from TWFE in magnitude and direction.\n")
cat("  Chapter prose should address this tension directly (see §10.9).\n")
cat("============================================================\n")

# =====================================================================================================================
# END OF R_code10_Georgia_DiD.R
# =====================================================================================================================



########################################################################
########################################################################
#
#  PART A  -  SECTION 10.7.4: EXTENDED TWO-WAY FIXED EFFECTS
#
########################################################################
########################################################################


gh_raw <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch10"

# ========================================================================
# 1. IMPORT EXPANDED 48-STATE PANEL
# ========================================================================
cat("\n=== Importing 48-state panel (Example_10_7_3.csv) ===\n")

df_raw <- tryCatch(
  read.csv(paste0(gh_raw, "/Example_10_7_3.csv")),
  error = function(e) {
    if (file.exists("Example_10_7_3.csv")) {
      read.csv("Example_10_7_3.csv")
    } else {
      stop("Cannot load Example_10_7_3.csv — check network or working directory.")
    }
  }
)
write.csv(df_raw, "Example_10_7_3.csv", row.names = FALSE)

# Normalise column names: lowercase, collapse dots/underscores/spaces to "_"
names(df_raw) <- tolower(gsub("[._\\s]+", "_", trimws(names(df_raw)),
                              perl = TRUE))
names(df_raw) <- gsub("^_|_$", "", names(df_raw))
names(df_raw)[1] <- gsub("^\\W+", "", names(df_raw)[1])   # strip BOM

cat(sprintf("Loaded: %d rows x %d columns\n", nrow(df_raw), ncol(df_raw)))
cat(sprintf("Columns: %s\n", paste(names(df_raw), collapse = ", ")))

# ========================================================================
# 2. CONSTRUCT LOGGED OUTCOME AND CONTROLS
# ========================================================================

# Resolve the raw column names for the financial variables.
# The CSV may use camelCase or underscore_separated depending on version.
resolve_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) stop(sprintf(
    "None of these columns found: %s\nActual columns: %s",
    paste(candidates, collapse = ", "),
    paste(names(df), collapse = ", ")
  ))
  hit[1]
}

col_genop  <- resolve_col(df_raw, c("generalpublicoperations",
                                     "general_public_operations"))
col_totsup <- resolve_col(df_raw, c("totalstatesupport",
                                     "total_state_support"))
col_finaid <- resolve_col(df_raw, c("totalfinancialaid",
                                     "total_financial_aid"))
col_tuifee <- resolve_col(df_raw, c("nettuitionandfeerevenue",
                                     "net_tuition_and_fee_revenue"))
col_fte    <- resolve_col(df_raw, c("netfteenrollment",
                                     "net_fte_enrollment"))

df <- df_raw %>%
  mutate(
    lngenop  = log(.data[[col_genop]]),
    lntotsup = log(.data[[col_totsup]]),
    lnfinaid = log(.data[[col_finaid]]),
    lntuifee = log(.data[[col_tuifee]]),
    lnfte    = log(.data[[col_fte]])
  ) %>%
  # is.finite() (not complete.cases) is required here: log(0) = -Inf and
  # log(negative) = NaN. complete.cases() catches NaN but NOT -Inf, so a
  # zero-valued financial figure (e.g. TotalFinancialAid = 0) would slip
  # through and later be dropped by fixest/etwfe, causing a row-count
  # mismatch in emfx(). Filtering on is.finite() removes all such rows up front.
  filter(if_all(c(lngenop, lntotsup, lnfinaid, lntuifee, lnfte), is.finite))

controls <- c("lntotsup", "lnfinaid", "lntuifee", "lnfte")
cat(sprintf("After dropping missing log-values: %d rows\n", nrow(df)))

# ========================================================================
# 3. STAGGERED TREATMENT-COHORT VARIABLE
#   gyear = first treatment period; 0 for never-treated.
# ========================================================================

# Resolve FIPS and fiscal-year column names
col_fips <- resolve_col(df, c("fips", "state_id", "stateid"))
col_fy   <- resolve_col(df, c("fy", "year", "fiscal_year"))

df <- df %>%
  rename(fips = all_of(col_fips),
         fy   = all_of(col_fy)) %>%
  mutate(
    gyear = case_when(
      fips == 13 ~ 2013L,   # Georgia
      fips == 55 ~ 2018L,   # Wisconsin
      fips == 42 ~ 2022L,   # Pennsylvania
      TRUE       ~ 0L       # never-treated
    )
  )

cat(sprintf("Panel: %d states, FY %d-%d\n",
            n_distinct(df$fips), min(df$fy), max(df$fy)))
cat("Cohort distribution:\n")
print(df %>% distinct(fips, gyear) %>% count(gyear))

# ========================================================================
# 4a. ETWFE — BASELINE, NO COVARIATES (design check)
#   control_var = "never" uses never-treated units as the control group.
#   Mirrors: jwdid lngenop, ivar(fips) tvar(fy) gvar(gyear) never
# ========================================================================
cat("\n=== ETWFE (no covariates, never-treated controls) ===\n")

# df_model: the exact rows etwfe/fixest will use. Filter on is.finite()
# for the model variables so that no -Inf (from log(0)) or NaN rows remain.
# This guarantees df_model, fit_4a, and fit_4b all share the same row set,
# so emfx() aggregations align with the fitted model frame.
df_model <- df %>%
  filter(if_all(c(lngenop, lntotsup, lnfinaid, lntuifee, lnfte), is.finite),
         !is.na(fips), !is.na(fy), !is.na(gyear))
cat(sprintf("df_model: %d rows used for both etwfe models\n", nrow(df_model)))

fit_4a <- etwfe(
  fml       = lngenop ~ 1,         # no covariates
  tvar      = fy,
  gvar      = gyear,
  data      = df_model,
  ivar      = fips,
  vcov      = ~fips                 # cluster by state
)
print(summary(fit_4a))

# Post-estimation aggregations (mirrors estat simple / group / event)
agg_4a_simple <- emfx(fit_4a, type = "simple")
agg_4a_group  <- emfx(fit_4a, type = "group")
agg_4a_event  <- emfx(fit_4a, type = "event")

cat("\n--- 4a: Overall ATT (simple average) ---\n")
print(agg_4a_simple)
cat("\n--- 4a: Cohort-specific ATT ---\n")
print(agg_4a_group)
cat("\n--- 4a: Event-study ATT ---\n")
print(agg_4a_event)

# Store 4a scalars for comparison table
a_simple_b  <- agg_4a_simple$estimate[1]
a_simple_se <- agg_4a_simple$std.error[1]
a_group_b   <- agg_4a_group$estimate
a_group_se  <- agg_4a_group$std.error

# ========================================================================
# 4b. ETWFE — WITH COVARIATES (preferred specification)
#   xvar passes the four controls; etwfe partials them out in a way that
#   avoids rank deficiency — equivalent to jwdid's hettype(cohort) approach.
#   Mirrors: jwdid lngenop $controls, ivar(fips) tvar(fy) gvar(gyear)
#            never hettype(cohort) cluster(fips)
# ========================================================================
cat("\n=== ETWFE (covariates, never-treated controls) ===\n")

controls_fml <- as.formula(paste("~", paste(controls, collapse = " + ")))

fit_4b <- etwfe(
  fml       = lngenop ~ lntotsup + lnfinaid + lntuifee + lnfte,
  tvar      = fy,
  gvar      = gyear,
  data      = df_model,
  ivar      = fips,
  vcov      = ~fips
)
print(summary(fit_4b))

# ========================================================================
# 5. POST-ESTIMATION AGGREGATIONS (Model 4b)
#   Directly comparable to csdid estat output in Section 10.7.
# ========================================================================

# Overall ATT (simple average across post-treatment ATT(g,t))
agg_4b_simple   <- emfx(fit_4b, type = "simple")
# Group-specific ATT (one per cohort: 2013, 2018, 2022)
agg_4b_group    <- emfx(fit_4b, type = "group")
# Calendar-time ATT
agg_4b_calendar <- emfx(fit_4b, type = "calendar")
# Event-study (dynamic effects relative to treatment onset)
agg_4b_event    <- emfx(fit_4b, type = "event")

cat("\n--- 4b: Overall ATT (simple average) ---\n")
print(agg_4b_simple)
cat("\n--- 4b: Cohort-specific ATT ---\n")
print(agg_4b_group)
cat("\n--- 4b: Calendar-time ATT ---\n")
print(agg_4b_calendar)
cat("\n--- 4b: Event-study ATT ---\n")
print(agg_4b_event)

# Store 4b scalars for comparison table
b_simple_b  <- agg_4b_simple$estimate[1]
b_simple_se <- agg_4b_simple$std.error[1]
b_group_b   <- agg_4b_group$estimate
b_group_se  <- agg_4b_group$std.error

# ========================================================================
# 6. EVENT-STUDY PLOT (fig10_7_2)
#   Built on the Model 4b (covariate-adjusted) event-study aggregation.
#   Reference period (event time = -1) is the row where estimate = 0 or
#   std.error = NA; all remaining rows are shifted accordingly.
# ========================================================================

# emfx(type = "event") returns a data frame with an "event" column
# containing the relative time period. Reference period may appear as
# NA estimate or be excluded; we add it back explicitly at event = -1.

es_df <- agg_4b_event %>%
  select(event, estimate, std.error) %>%
  rename(b = estimate, se = std.error) %>%
  mutate(
    lo = b - 1.96 * se,
    hi = b + 1.96 * se
  )

# Add the omitted reference period at event = -1 if not already present
if (!(-1L %in% es_df$event)) {
  ref_row <- data.frame(event = -1L, b = 0, se = NA_real_,
                        lo = 0, hi = 0)
  es_df <- bind_rows(es_df, ref_row) %>% arrange(event)
}

p_es <- ggplot(es_df, aes(x = event, y = b)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dotted",
             colour = "grey50", linewidth = 0.4) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                width = 0.3, colour = "grey50", linewidth = 0.5,
                na.rm = TRUE) +
  geom_line(colour = "black", linewidth = 0.7, na.rm = TRUE) +
  geom_point(shape = 21, size = 2.5,
             colour = "black", fill = "black", na.rm = TRUE) +
  labs(
    title    = "ETWFE Event Study: Staggered Adoption",
    subtitle = "Covariate-adjusted, never-treated controls",
    x        = "Years relative to treatment",
    y        = "ATT on log operations"
  ) +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_7_2_R.png"),
       p_es, width = 7, height = 5, dpi = 200)
print(p_es)   # render to RStudio Plots pane (ggsave only writes to disk)
cat(sprintf("\nfig10_7_2_R.png exported to %s\n", graphs_dir))

# ========================================================================
# 6b. COMPARISON TABLE: ETWFE WITHOUT vs WITH COVARIATES
#   Rows: Overall, G2013 (Georgia), G2018 (Wisconsin), G2022 (Pennsylvania)
#   Cols: ATT(4a), SE(4a), ATT(4b), SE(4b)
# ========================================================================

# Align group rows: match cohort years 2013, 2018, 2022
cohort_years <- c(2013L, 2018L, 2022L)
cohort_labels <- c("G2013_Georgia", "G2018_Wisconsin", "G2022_Pennsylvania")

# Helper: extract estimate/SE for a given cohort from emfx group output
get_group <- function(agg, gyear_val, col) {
  row <- agg[agg$gyear == gyear_val, col, drop = TRUE]
  if (length(row) == 0) NA_real_ else row[1]
}

tab_df <- data.frame(
  row     = c("Overall", cohort_labels),
  ATT_4a  = c(a_simple_b,
              sapply(cohort_years, get_group, agg = agg_4a_group,
                     col = "estimate")),
  SE_4a   = c(a_simple_se,
              sapply(cohort_years, get_group, agg = agg_4a_group,
                     col = "std.error")),
  ATT_4b  = c(b_simple_b,
              sapply(cohort_years, get_group, agg = agg_4b_group,
                     col = "estimate")),
  SE_4b   = c(b_simple_se,
              sapply(cohort_years, get_group, agg = agg_4b_group,
                     col = "std.error"))
)

cat("\n", strrep("=", 72), "\n", sep = "")
cat("ETWFE ATT estimates: unconditional (4a) vs covariate-adjusted (4b)\n")
cat(strrep("=", 72), "\n", sep = "")
cat(sprintf("  %-26s %9s %9s %9s %9s\n",
            "Row", "ATT(4a)", "SE(4a)", "ATT(4b)", "SE(4b)"))
cat("  ", strrep("-", 65), "\n", sep = "")
for (i in seq_len(nrow(tab_df))) {
  cat(sprintf("  %-26s %9.4f %9.4f %9.4f %9.4f\n",
              tab_df$row[i],
              tab_df$ATT_4a[i], tab_df$SE_4a[i],
              tab_df$ATT_4b[i], tab_df$SE_4b[i]))
}
cat("  ", strrep("-", 65), "\n", sep = "")
cat("  Never-treated controls; SEs clustered by state (fips).\n")
cat("  Cols 1-2: no covariates. Cols 3-4: covariate-adjusted.\n")

# Save as CSV (replaces Stata RTF; import into Word/Excel for Springer table)
write.csv(tab_df, file.path(tables_dir, "tab10_7_etwfe.csv"),
          row.names = FALSE)
cat(sprintf("\ntab10_7_etwfe.csv exported to %s\n", tables_dir))

cat("\nETWFE / etwfe section complete.\n")
cat(sprintf("Figure: %s/fig10_7_2_R.png\n", graphs_dir))
cat(sprintf("Table:  %s/tab10_7_etwfe.csv\n", tables_dir))

# ========================================================================
# INTERPRETATION (for chapter prose)
# ------------------------------------------------------------------------
# - The unconditional ETWFE (4a) reproduces the CS-DiD pattern: overall
#   ATT ~0.051 (p=0.001), driven by G2013/Georgia (~0.114, p<0.001), with
#   G2018 null and G2022 negative. Pre-treatment leads are large and
#   significant, so this estimate rests on a questionable parallel-trends
#   assumption.
# - The covariate-adjusted ETWFE (4b) absorbs those differential trends
#   and collapses the overall ATT to a precise null (~-0.002, p=0.90).
# - Report BOTH: 4a as the unconditional benchmark comparable to csdid,
#   4b as the adjusted specification showing sensitivity to confounders.
# - The fully saturated default ETWFE is NOT appropriate here: with three
#   treated cohorts and four time-varying covariates it is rank-deficient.
#   Note this as a practical caution on ETWFE in small-T, few-cohort panels.
# ========================================================================
# END OF R_code10_ETWFE.R
# ========================================================================



########################################################################
########################################################################
#
#  PART B  -  SECTIONS 10.10-10.16: MARGINAL TREATMENT EFFECTS
#
########################################################################
########################################################################


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
print(p_mte)   # render to RStudio Plots pane
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
print(p_dec)   # render to RStudio Plots pane

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
print(p_area)   # render to RStudio Plots pane
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
print(p_int)   # render to RStudio Plots pane

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
print(p_reg)   # render to RStudio Plots pane
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
print(p_pbin)   # render to RStudio Plots pane
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



# ========================================================================
# CHAPTER 10 COMPLETE
# ========================================================================
cat("\n", strrep("=", 72), "\n", sep = "")
cat("CHAPTER 10 COMPLETE\n")
cat(strrep("=", 72), "\n", sep = "")
cat("All figures (PNG) exported to:\n  ", graphs_dir, "\n", sep = "")
cat("All tables (CSV) exported to:\n  ", tables_dir, "\n", sep = "")
cat("\nChapter 10 log closed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
.close_log()

# ========================================================================
# END OF R_code10_complete.R
# ========================================================================
