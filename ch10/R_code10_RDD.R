# ========================================================================
# Chapter 10 – Section 10.2: Regression Discontinuity Design
#              The Merit-Based Scholarship
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch10
# Author: Marvin A. Titus
# Date: May 2026
# NOTE: Code development was assisted by Claude (Anthropic). The author
# provided specifications and reviewed, tested, and validated all code.
# ========================================================================
# Called by: R_code10.R  (inherits out_root and the directories derived from it)
# Standalone: can also be sourced directly; derives its own paths if needed.
#
# FIGURE FILENAMES ALL END IN "_R"
# --------------------------------
# Every figure this script writes carries an _R suffix, matching the
# convention already used by R_code10_Georgia_DiD.R and R_code10_ETWFE.R.
# Before Aug 2026 this script wrote fig10_1_rdd_plot_credits.png,
# fig10_2_3_rdd_bw_sensitivity.png and the rdplot figures under EXACTLY the
# names Stata's RDD.do writes, into the SAME graphs directory.  Whichever
# language ran last silently won, so the figure that reached the manuscript
# depended on run order.  Three of the names were also stale (fig10_2_1_,
# fig10_2_7_, fig10_2_ prefixes predate the Aug 2026 renumbering), leaving
# orphaned PNGs in Output/graphs that looked like current figures.
#
# The Stata script owns the manuscript figures.  This script's output is the
# R translation readers reproduce, and the two no longer collide.
#
# Required packages:
#   rdrobust  — rdrobust(), rdbwselect(), rdplot()
#   rddensity — rddensity(), rdplotdensity()
#   AER       — ivreg() (manual 2SLS)
#   ggplot2   — publication plots
#   dplyr     — data wrangling
#   truncnorm — rtrnorm() (truncated normal draws)
#
# Install once:
#   install.packages(c("rdrobust","rddensity","AER","ggplot2","dplyr","truncnorm"))
#
# Sections:
#   10.2.1   Synthetic data generation (N = 4,000; seed 20260510)
#   10.2.2   Density continuity test (rddensity) & covariate balance
#   10.2.3   Binned scatterplots
#   10.2.4   Sharp RD: OLS benchmark, manual local linear, rdrobust,
#            covariate-adjusted rdrobust
#   10.2.5   Bandwidth sensitivity (9-point grid)
#   10.2.6   Polynomial order sensitivity (p = 1, 2, 3)
#   10.2.7   Fuzzy RD: first stage, reduced form, Wald/2SLS for all outcomes;
#            covariate-adjusted fuzzy LATE; consolidated summary table
#   10.2.8   Validity checks: placebo cutoffs, donut RD, augmented, subgroup
#   10.2.9   Publication-quality RD plots (rdplot); fuzzy LATE comparison figure
#   10.2.10  Summary table; save Example_10_2_1.rds / .csv
# ========================================================================
#
# WHY THESE NUMBERS DIFFER FROM THE BOOK'S
# --------------------------------------
# The book reports Stata results throughout.  Section 10.2 GENERATES its data,
# and Stata and R draw from different random number streams, so the two
# languages cannot produce the same sample even from the same seed and the
# same data-generating process.  Every estimate here therefore differs from
# the printed one -- e.g. the sharp RD effect on first-year credits is about
# 5.40 here against 3.30 in the book, and the fuzzy LATE about 9.11 against
# 6.41.  Neither is more correct.  What corresponds across the two languages
# is the PROCEDURE: the same estimator, bandwidth selector, kernel, and
# inference method, applied to a sample drawn the same way.
#
# One consequence is worth knowing before you read the covariate-balance
# table below: income_cat rejects at p = 0.042 in this draw and does not in
# the book's.  Section 10.2.1 of the manuscript discusses what to do when a
# single covariate jumps; the guidance printed under the table repeats it.

# EQUATION CROSS-REFERENCES (manuscript Section 10.2)
#   Eq. 10.1  Sharp RD estimand   -> rdrobust(y, x, c = 0, ...)
#   Eq. 10.3  Assignment rule     -> D_sharp <- as.integer(x >= 0)
#   Eq. 10.4  Interacted OLS form (printed in the text to make the logic
#             visible; NOT the estimator behind any reported number)
#   Eq. 10.5  Fuzzy RD (Wald) estimand -> rdrobust(..., fuzzy = D_fuzzy)
#             and the manual 2SLS with in_bw2 / tri_wt2 / x_Ds2
#
# CHANGE LOG
#   2026-10-15a  Sync pass against RDD.do 2026-08-19f: added the end-of-run
#                MANUSCRIPT-FIGURE ROLL CALL and graphs_dir listing (RDD.do
#                item (j)).  chkfig() already ran after each ggsave, but the
#                lines were scattered through the run and there was no single
#                block answering "did Section 10.2 produce its figures?" --
#                the one piece of item (j) the Aug sync did not carry over.
#                Diagnostic output only; no estimation, sample, bandwidth or
#                reported number changed.
#   2026-08-19a  Synchronised with RDD.do version 2026-08-19f:
#                _R suffix on every figure name (see above); Fig. 10.4
#                rebuilt (zero forced onto the axis, robust bias-corrected
#                CI by default via a ci_type switch, corrected caption);
#                density-test and covariate-balance commentary hedged to
#                match the chapter; instrument-validity assumptions added;
#                Eq. 10.x placeholders resolved to Eq. 10.5; dataset written
#                to <out_root>/data/Example_10_2_1.rds instead of the
#                working directory; figure writes verified after export.
# ========================================================================

# -----------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------
suppressPackageStartupMessages({
  library(rdrobust)
  library(rddensity)
  library(AER)
  library(ggplot2)
  library(dplyr)
  library(truncnorm)
})

# -----------------------------------------------------------------------
# Output paths
#
# Everything is DERIVED from out_root.  R objects persist in the global
# environment across sourced scripts exactly as Stata globals persist across
# do-files, so an "if it does not exist, set it" test lets a stale value from
# an earlier run through -- the failure that misdirected RDD.log on the Stata
# side in Aug 2026.  Deriving removes it: to redirect output, change
# out_root, never the individual directories.
# -----------------------------------------------------------------------
if (!exists("out_root")) {
  out_root <- if (Sys.info()[["user"]] == "marvi") {
    "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output"
  } else {
    "Output"
  }
  message("RDD.R (standalone): out_root set to ", out_root)
}
graphs_dir <- file.path(out_root, "graphs")
data_dir   <- file.path(out_root, "data")
dir.create(graphs_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(data_dir,   showWarnings = FALSE, recursive = TRUE)
# -----------------------------------------------------------------------
# Standalone logging.  When this script is sourced by R_code10.R a log is
# already open and these helpers do nothing.  Run on its own, it opens
# <out_root>/logs/R_code10_RDD.log and unwinds the sink on exit, so a failure
# cannot leave the console muted.
# -----------------------------------------------------------------------
if (!exists("ch10_start_log")) {
  ch10_start_log <- function(basename) {
    if (isTRUE(getOption("ch10_logging"))) return(invisible(NULL))
    dir.create(file.path(out_root, "logs"), showWarnings = FALSE,
               recursive = TRUE)
    path <- file.path(out_root, "logs", basename)
    con  <- file(path, open = "wt")
    options(ch10_logging = TRUE, ch10_log_con = con, ch10_log_path = path)
    # Strip ANSI colour codes: RStudio colourises message() output, and the
    # escape sequences land in the file as garbage ("G3;", "gG3;") around
    # every warning.  Plain text only while a log is open.
    options(crayon.enabled = FALSE, cli.num_colors = 1)
    sink(con, split = TRUE)
    sink(con, type = "message")
    cat("Log:", path, "\n")
    cat("Opened:", format(Sys.time()), "\n\n")
    invisible(path)
  }
  ch10_stop_log <- function() {
    # sink.number(type = "message") returns a CONNECTION NUMBER, not a count,
    # and stderr is connection 2 -- so "while (... > 0)" never terminates.
    # Message sinks do not nest, so one reset is all that is needed.
    if (sink.number(type = "message") != 2) sink(type = "message")
    while (sink.number() > 0) sink()
    con <- getOption("ch10_log_con")
    if (!is.null(con)) try(close(con), silent = TRUE)
    options(ch10_logging = NULL, ch10_log_con = NULL, ch10_log_path = NULL)
    invisible(NULL)
  }
}

# NOTE: diagnostics in this script use cat(), not message().  While a log is
# open the message stream is sinked, and RStudio wraps message() output in
# ANSI colour escapes that land in the file as garbage ("G3;", "gG3;").
# cat() goes to the output stream, which stays clean.
# Own the log only if none is currently OPEN.  The guard must test the open
# state, not whether the helper exists: sourcing R_code10.R earlier in the
# same R session leaves ch10_start_log() defined in the global environment
# long after its log was closed, and an exists() test would then skip logging
# entirely and silently.  Same stale-state trap as the path globals.
ch10_own_log <- !isTRUE(getOption("ch10_logging"))
ch10_start_log("R_code10_RDD.log")

cat("R_code10_RDD.R: version 2026-10-15a\n")
cat("  out_root  =", out_root, "\n")
cat("  graphs_dir=", graphs_dir, "\n")



# chkfig(): confirm a figure actually reached disk.  ggsave() returning
# without error is not the same as the file being readable afterwards --
# sync clients, permissions and antivirus can all intervene.
chkfig <- function(path) {
  if (file.exists(path)) {
    cat(sprintf("   OK: %s (%s bytes)\n", basename(path),
                format(file.info(path)$size, big.mark = ",")))
  } else {
    warning("MISSING: ", path, call. = FALSE)
    cat(sprintf("   MISSING: %s\n", basename(path)))
  }
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
#   p > 0.05  → no statistical evidence of a discontinuity in the density of
#               the running variable at the cutoff.  That is CONSISTENT WITH,
#               but does not establish, the absence of precise manipulation.
#               These data were generated without sorting, so the test
#               demonstrates the procedure rather than certifying the design
#               (chapter Section 10.2.1).
#   p ≤ 0.05  → evidence of sorting; investigate before proceeding.

rd_dens <- rddensity(df$x, c = 0)
print(summary(rd_dens))

# Density plot ➜ Fig. 10.2
#
# GRAYSCALE (Springer).  rdplotdensity() hardcodes colour for the two sides
# of the cutoff -- the same behaviour as Stata's rddensity plot option, which
# ignores s2mono.  theme_springer() styles the panel but does NOT touch the
# fitted lines or CI ribbons, so the scales are overridden explicitly here
# and the two sides are distinguished by LINE TYPE (solid below the cutoff,
# dashed above) rather than by colour, so the figure survives B&W printing.
# plotRange must stay INSIDE the support of x.  The 20 Aug 2026 run used a
# fixed c(-1.5, 1.5) while x actually runs about -2.25 to 0.75, so ggplot
# dropped 7 rows off the right-hand end and warned about it.  Deriving the
# range from the data keeps the whole fitted curve on the plot.
#
# CIuniform = TRUE asks for uniform (simultaneous) confidence bands, which
# require a positive semidefinite variance matrix.  Here it is not: the run
# raised "Error in eigen(Sigma, symmetric = TRUE): infinite or missing
# values" inside a try(), then fell back to pointwise intervals anyway.
# Requesting pointwise directly gets the same figure without the error text,
# and matches what the Stata version plots.
x_lo <- max(-1.5, min(df$x, na.rm = TRUE))
x_hi <- min( 1.5, max(df$x, na.rm = TRUE))

den_plot <- rdplotdensity(rd_dens, df$x,
                          plotRange  = c(x_lo, x_hi),
                          plotN      = 25,
                          CIuniform  = FALSE)

# suppressMessages(): scale_colour_grey() replaces the colour scale
# rdplotdensity() already set, and ggplot notes that every run.  The
# replacement is the point -- the note is not worth a line in the log.
den_gg <- suppressMessages(den_plot$Estplot +
  labs(title    = "Density Continuity Test (rddensity)",
       subtitle = "Running variable: HS GPA centered at c = 3.25",
       x        = "HS GPA (centered at cutoff)",
       y        = "Density",
       caption  = paste("Solid line: below the cutoff; dashed line: above.",
                        "Shaded bands are 95% confidence regions.")) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black",
             linewidth = 0.5) +
  scale_colour_grey(start = 0, end = 0) +
  scale_fill_grey(start = 0.55, end = 0.85) +
  theme_springer() +
  theme(legend.position = "none"))

fig10_2 <- file.path(graphs_dir, "fig10_2_rdd_density_test_R.png")
ggsave(fig10_2, den_gg, width = 7, height = 5, dpi = 200)
chkfig(fig10_2)
print(den_gg)
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
cat("   Failing to detect a jump is not the same as establishing that these\n")
cat("   characteristics are identical across the threshold: the tests are\n")
cat("   local and therefore not highly powered.\n")
cat("   If ONE covariate jumps, do not abandon the design on that alone:\n")
cat("   with five tests at the .05 level, the chance that at least one\n")
cat("   rejects by chance is about 23 percent. Check three things instead --\n")
cat("   whether the jump survives other bandwidths and polynomial orders,\n")
cat("   whether its h* is far narrower than the others (a narrow window is a\n")
cat("   small effective sample, not evidence of sorting), and whether adding\n")
cat("   the covariate moves the treatment estimate. A jump that vanishes\n")
cat("   under respecification and leaves the estimate unchanged is noise;\n")
cat("   one that persists and shifts the estimate is a threat to the design.\n")


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
                     paste0("fig10_2_2_rdd_binscatter_", nm, "_R.png"))
  ggsave(fname, p, width = 7, height = 5, dpi = 200)
  chkfig(fname)
  print(p)
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
                 kernel = "triangular", bwselect = "mserd")
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
                 covs = covs_mat)
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

fig_bw <- file.path(graphs_dir, "fig10_2_3_rdd_bw_sensitivity_R.png")
ggsave(fig_bw, p_bw, width = 7, height = 5, dpi = 200)
chkfig(fig_bw)
print(p_bw)

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
               kernel = "triangular", bwselect = "mserd")
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
                 kernel = "triangular", bwselect = "mserd")
  rf_res[[outcome]] <- list(b = rr$coef[1], se = rr$se[1],
                            p = rr$pv[3],   h  = rr$bws[1,1])
  cat(sprintf("   %-18s %8.4f %10.4f %10.4f %8.3f\n",
              outcome, rr$coef[1], rr$se[1], rr$pv[3], rr$bws[1,1]))
}
cat("  ", strrep("-", 60), "\n")
cat("   ITT: local average effect of eligibility (not take-up) on outcome.\n")

# NOTE ON rdrobust's R INTERFACE (differs from the Stata command)
# Stata's -rdrobust ..., all- asks for the Conventional / Bias-corrected /
# Robust rows to be PRINTED.  The R function has no such argument -- passing
# all = TRUE raises "unused argument (all = TRUE)" -- because it always
# returns all three: rr$coef, rr$se, rr$pv and rr$ci are length-3 vectors or
# 3-row matrices, indexed [1] Conventional, [2] Bias-corrected, [3] Robust.
# So rr$coef[1] is the conventional point estimate, rr$pv[3] the robust
# p-value, and rr$ci[3, ] the robust bias-corrected interval -- exactly the
# combination the chapter reports.  Nothing is lost by dropping the argument.

# INSTRUMENT-VALIDITY ASSUMPTIONS (chapter Section 10.2.2 states these four;
# reading the cutoff as an instrument requires all of them)
#   1. Continuity of potential outcomes in x at c, and no precise sorting
#      across c.  Probed by the density test in Section 10.2.2 above.
#   2. A discontinuous first stage: Pr(D = 1) must jump at c.  Estimated
#      above; it is the denominator of Eq. 10.5.
#   3. Exclusion: crossing c affects the outcome ONLY through take-up.
#      VIOLATED BY CONSTRUCTION here for credits_y1 and cgpa_y1, which are
#      generated from D_sharp.
#   4. Monotonicity: crossing c moves students into treatment, never out of
#      it (no defiers).  The complier reading of the LATE rests on it.

# -- Fuzzy LATE via rdrobust ------------------------------------------
cat("\n--- Fuzzy LATE (rdrobust, fuzzy option) ---\n")
cat(sprintf("   %-18s %8s %10s %10s %8s\n",
            "Outcome", "LATE", "SE(conv)", "p(robust)", "h*"))
cat("  ", strrep("-", 60), "\n")

fl_res <- list()
for (outcome in c("persist_fuzzy","credits_y1","cgpa_y1")) {
  rr <- rdrobust(df[[outcome]], df$x, c = 0,
                 fuzzy  = df$D_fuzzy,
                 kernel = "triangular", bwselect = "mserd")
  # ci[3, ] is the robust bias-corrected interval; stored because Fig. 10.4
  # plots it and the loop prints only the conventional SE.
  fl_res[[outcome]] <- list(b  = rr$coef[1], se = rr$se[1],
                            p  = rr$pv[3],   h  = rr$bws[1,1],
                            lo = rr$ci[3,1], hi = rr$ci[3,2])
  cat(sprintf("   %-18s %8.4f %10.4f %10.4f %8.3f\n",
              outcome, rr$coef[1], rr$se[1], rr$pv[3], rr$bws[1,1]))
}
cat("  ", strrep("-", 60), "\n")
cat("   LATE: causal effect for compliers at the cutoff.\n")
cat("   Compliers are a LATENT group: the first stage identifies their share\n")
cat("   at the margin, not which students belong to it.  For credits_y1 and\n")
cat("   cgpa_y1 the exclusion restriction fails by construction; read those\n")
cat("   as Wald arithmetic, not a complier effect.\n")
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
                 covs     = covs_mat)
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
# Variable names operationalise Eq. 10.5 (fuzzy Wald estimand):
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
cat("   Variables: in_bw2  tri_wt2  x_Ds2  (as in text Eq. 10.5)\n")
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
                   covs   = covs_mat)
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
  fpath <- file.path(graphs_dir, fname)
  ggsave(fpath, p, width = 7, height = 5, dpi = 200)
  chkfig(fpath)
  print(p)
}

# fig10_2_4: Sharp persistence
save_rdplot(df$persist_sharp, df$x,
            title    = "Effect of Institutional Merit Scholarship on\nSecond-Year Persistence",
            subtitle = "Sharp RD -- HS GPA cutoff c = 3.25",
            ytitle   = "Second-Year Persistence Rate",
            fname    = "fig10_2_4_rdd_plot_persistence_R.png")

# fig10_1: Sharp credits (Fig. 10.1 in chapter)
save_rdplot(df$credits_y1, df$x,
            title    = "Effect of Institutional Merit Scholarship on\nYear-1 Credits Earned",
            subtitle = "Sharp RD -- HS GPA cutoff c = 3.25",
            ytitle   = "Year-1 Credits Earned",
            fname    = "fig10_1_rdd_plot_credits_R.png")

# fig10_2_6: Sharp college GPA
save_rdplot(df$cgpa_y1, df$x,
            title    = "Effect of Institutional Merit Scholarship on\nYear-1 College GPA",
            subtitle = "Sharp RD -- HS GPA cutoff c = 3.25",
            ytitle   = "Year-1 College GPA",
            fname    = "fig10_2_6_rdd_plot_cgpa_R.png")

# fig10_2_7: First stage (take-up)
save_rdplot(df$D_fuzzy, df$x,
            title    = "First Stage: Scholarship Take-Up at GPA Cutoff",
            subtitle = "Fuzzy RD -- jump in take-up probability at c = 3.25",
            ytitle   = "P(Scholarship Received)",
            fname    = "fig10_3_rdd_plot_firststage_R.png")

# fig10_2_8: Fuzzy persistence rdplot
save_rdplot(df$persist_fuzzy, df$x,
            title    = "Fuzzy RD: Second-Year Persistence (Fuzzy Outcome)",
            subtitle = "Fuzzy RD — imperfect compliance around c = 3.25",
            ytitle   = "P(Second-Year Persistence)",
            fname    = "fig10_2_8_rdd_plot_persist_fuzzy_R.png")

# -- Fig. 10.4: Fuzzy LATE for credits_y1 (forest-plot style) --------
# Point estimate and 95% CI for the fuzzy LATE for credits earned in year 1,
# at the MSE-optimal bandwidth.
#
# WHICH INTERVAL: set ci_type to "robust" or "conventional".
#   robust        rr$ci[3, ] -- rdrobust's bias-corrected interval.  This is
#                 what the chapter's inference rests on (every p-value in
#                 Section 10.2 is the robust one) and what the Section
#                 10.2.10 summary table prints.  It is NOT centred on the
#                 conventional point estimate, because it centres on the
#                 bias-corrected one, so the diamond sits off-centre in the
#                 interval.  That is correct, not a plotting error.
#   conventional  b +/- 1.96*SE.  Symmetric and easier to read, but not the
#                 interval any reported p-value comes from.
ci_type <- "robust"

fl_b  <- fl_res$credits_y1$b
fl_se <- fl_res$credits_y1$se

if (ci_type == "robust") {
  fl_lo <- fl_res$credits_y1$lo
  fl_hi <- fl_res$credits_y1$hi
  ci_label <- "robust bias-corrected 95% CI"
} else {
  fl_lo <- fl_b - 1.96 * fl_se
  fl_hi <- fl_b + 1.96 * fl_se
  ci_label <- "conventional 95% CI"
}

late_df <- data.frame(
  outcome = "Credits Y1 (cr-hrs)",
  b       = fl_b,
  lo      = fl_lo,
  hi      = fl_hi,
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
  # Zero must be visible: the chapter's prose for this figure says the
  # interval does not cross zero, so the axis is forced to include it.  The
  # lower limit is slightly below 0 so the dashed reference line stands clear
  # of the panel edge.
  scale_x_continuous(limits = c(-0.4, max(10, ceiling(fl_hi) + 1)),
                     breaks = seq(0, max(10, ceiling(fl_hi) + 1), by = 2)) +
  labs(title    = "Fuzzy RD: LATE for First-Year Credits",
       subtitle = paste("Point estimate with", ci_label),
       x        = "Fuzzy LATE (rdrobust, CCT MSE-optimal h*)",
       y        = NULL,
       caption  = paste0("Wald ratio: reduced form / first stage.\n",
                         "credits_y1 is generated from D_sharp, so this is Wald ",
                         "arithmetic rather than a complier effect (Sec. 10.2.3).\n",
                         "rdrobust fuzzy option, kernel(triangular), bwselect(mserd).")) +
  theme_springer() +
  theme(axis.text.y  = element_text(size = 10),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())

fig10_4 <- file.path(graphs_dir, "fig10_4_fuzzy_late_credits_R.png")
ggsave(fig10_4, p_late, width = 7, height = 3.5, dpi = 200)
chkfig(fig10_4)
print(p_late)

cat(sprintf("   Fig. 10.4: LATE = %6.3f   %s = [%6.3f, %6.3f]\n",
            fl_b, ci_label, fl_lo, fl_hi))
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
                  kernel = "triangular", bwselect = "mserd")
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

# Save dataset.  Written to <out_root>/data, NOT the working directory, and
# under the filename the chapter's data-availability section gives readers.
saveRDS(df,   file.path(data_dir, "Example_10_2_1.rds"))
write.csv(df, file.path(data_dir, "Example_10_2_1.csv"), row.names = FALSE)

# Roll call: re-confirm the four manuscript figures at the end of the run, so
# one block in the log answers "did Section 10.2 produce its figures?"
# Mirrors RDD.do item (j).  chkfig() fires after each ggsave above, but those
# lines are scattered through 900 lines of output; this gathers them.  The
# names carry the _R suffix because the Stata script owns the unsuffixed
# manuscript figures and the two must never collide.
cat("\n=== Manuscript figures for Section 10.2 ===\n")
for (f in c("fig10_1_rdd_plot_credits_R",
            "fig10_2_rdd_density_test_R",
            "fig10_3_rdd_plot_firststage_R",
            "fig10_4_fuzzy_late_credits_R")) {
  chkfig(file.path(graphs_dir, paste0(f, ".png")))
}
cat("   Directory listed below is the one they were written to:\n")
cat("  ", graphs_dir, "\n")
cat(paste0("   ", list.files(graphs_dir, pattern = "^fig10_.*\\.png$")),
    sep = "\n")
cat("\n")

cat("\n=== RDD dataset saved  ->  ", file.path(data_dir, "Example_10_2_1.rds"),
    " / .csv ===\n", sep = "")
cat("=== Section 10.2 complete.                                         ===\n")

# Close the log only if this script opened it (standalone run).
if (isTRUE(ch10_own_log)) ch10_stop_log()

# ========================================================================
# END OF R_code10_RDD.R
# ========================================================================
