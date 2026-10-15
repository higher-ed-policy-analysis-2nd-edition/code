# ============================================================================
# R_code10.R  —  Chapter 10: Causal Inference
# R translation of Stata_code10.do
# Higher Education Policy Analysis Using Quantitative Techniques (2nd ed.)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch10
# Author: Marvin A. Titus
# Date: November 2025 (revised September 2026)
# NOTE: Code development was assisted by Claude (Anthropic). The author
#       provided specifications and reviewed, tested, and validated all code.
# ============================================================================
# Script tested in R 4.4+
#
# BEFORE YOU RUN
# --------------
# Working directory.  Run this file from the ch10 folder of the repository
# (in RStudio: Session > Set Working Directory, or setwd("C:/.../code/ch10")).
# Paths are relative to that folder: sub-scripts are read from Syntax/R/ and
# output is written to Output/.  Same requirement as Stata_code10.do.
# Packages.  On a new machine set install_packages <- TRUE (Section 2) for
# the first run only; see the note there.
#
# HOW TO RUN
# ----------
# In RStudio: open this file and click Source (or Ctrl+Shift+S).
# LOGGING IS AUTOMATIC.  This script opens <out_root>/logs/R_code10.log and
# tees everything to it, mirroring Stata_code10.do's -log using-.  Console
# output still appears as usual (sink(split = TRUE)), so you lose nothing by
# running it in RStudio.  Each sub-script also opens its own log when run on
# its own -- R_code10_RDD.log, R_code10_Georgia_DiD.log, R_code10_ETWFE.log.
#
# If a run stops on an error, the sink is closed for you by the on-exit
# handler.  Should the console ever go silent anyway, restore it with:
#   if (sink.number(type = "message") != 2) sink(type = "message")
#   while (sink.number() > 0) sink()
#
# Sub-scripts (all in syntax_dir = Syntax/R/):
#   R_code10_RDD.R          Section 10.2         -- Sharp/Fuzzy RD, merit scholarship
#   R_code10_Georgia_DiD.R  Sections 10.3-10.9   -- DiD, SCM, SDID, CS-DiD
#   R_code10_ETWFE.R        Section 10.7.4       -- Extended TWFE (Wooldridge)
#
# NOTE: The instrumental variables, CATE, and marginal treatment effect
# (MTE/MPRTE) material previously developed as "Part B" of this chapter
# now constitutes Chapter 11 and is covered by R_code11.R and its
# sub-scripts (CATE_R.R, MTE_MPRTE_R.R). This chapter (10.2-10.9) covers
# only the institutional/state-level quasi-experimental methods below.
# ============================================================================

# ============================================================================
# 0. OUTPUT DIRECTORIES
#    Paths switch automatically on username. Mirrors Stata_code10.do logic.
# ============================================================================

# One root; every output directory is DERIVED from it, so the paths cannot
# drift apart and a sub-script sourced later inherits a consistent set.  To
# redirect output, change out_root and nothing else.  Mirrors the $out_root
# logic in Stata_code10.do.
username <- Sys.info()[["user"]]

if (username == "marvi") {
  chapter_root <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10"
  out_root     <- file.path(chapter_root, "Output")
  syntax_dir   <- file.path(chapter_root, "Syntax/R")
} else {
  out_root   <- "Output"
  syntax_dir <- "Syntax/R"
}

graphs_dir <- file.path(out_root, "graphs")
tables_dir <- file.path(out_root, "tables")
data_dir   <- file.path(out_root, "data")
log_dir    <- file.path(out_root, "logs")

for (d in c(graphs_dir, tables_dir, data_dir, log_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ============================================================================
# 0b. LOGGING
#
# R has no -log using-, so nothing was ever written to <out_root>/logs: the
# directory was created and left empty.  These helpers close that gap.
#
# ch10_start_log() tees console output AND messages/warnings/errors to a file.
# split = TRUE keeps output visible in the console at the same time.  The
# message stream cannot be split, so warnings and errors go to the file only
# while a log is open -- that is the trade for having them recorded.
#
# ch10_stop_log() unwinds however many sinks are open, so it is safe to call
# twice, and reg.finalizer + the sub-scripts' own guards keep a failed run
# from leaving the console muted.
# ============================================================================

ch10_start_log <- function(basename) {
  if (isTRUE(getOption("ch10_logging"))) return(invisible(NULL))  # already open
  path <- file.path(log_dir, basename)
  con  <- file(path, open = "wt")
  options(ch10_logging = TRUE, ch10_log_con = con, ch10_log_path = path)
  # Strip ANSI colour codes: RStudio colourises message() output, and the
  # escape sequences land in the file as garbage around every warning.
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
  if (!is.null(con)) {
    try(close(con), silent = TRUE)
    cat("Log closed:", getOption("ch10_log_path"), "\n")
  }
  options(ch10_logging = NULL, ch10_log_con = NULL, ch10_log_path = NULL)
  invisible(NULL)
}

ch10_start_log("R_code10.log")
cat("R_code10.R: version 2026-10-15a\n")
cat("  out_root  =", out_root, "\n")
cat("  graphs_dir=", graphs_dir, "\n")
cat("  tables_dir=", tables_dir, "\n")
cat("  data_dir  =", data_dir,   "\n")
cat("  log_dir   =", log_dir,    "\n\n")

cat("Chapter 10 started:", format(Sys.time()), "\n")
cat("Graphs :", graphs_dir, "\n")
cat("Tables :", tables_dir, "\n")
cat("Data   :", data_dir,   "\n\n")

options(warn = 1)   # print warnings immediately (Stata default)

# ============================================================================
# 1. GLOBAL GGPLOT2 THEME
#    Monochrome; approximates Stata s2mono for Springer B&W print.
#    Defined here so all sourced sub-scripts inherit it.
# ============================================================================

library(ggplot2)
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
# 2. PACKAGE INSTALLATIONS
#    Mirrors Stata's: capture ssc install <package>
#
# SET install_packages <- TRUE THE FIRST TIME ON A NEW MACHINE, then leave it
# FALSE.  Installing a package that is already attached (dplyr, ggplot2) makes
# RStudio RESTART THE R SESSION mid-run: on 21 Aug 2026 the restart aborted
# the chapter at the ggpubr install and the whole thing had to be re-sourced.
# The confirmation check below still runs every time and stops with a clear
# message if anything is missing, so leaving this FALSE is safe.
install_packages <- FALSE

if (install_packages) {

install_if_missing <- function(pkgs) {
  to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(to_install) > 0)
    install.packages(to_install, dependencies = TRUE)
}

# -- RDD (Section 10.2) -------------------------------------------------------
# Stata: ssc install rdrobust / rddensity / cmogram
install_if_missing(c(
  "rdrobust",   # rdrobust(), rdplot(), rdbwselect()
  "rddensity",  # rddensity() -- McCrary density test
  "ggplot2",
  "ggpubr"      # multi-panel layout (replaces graph combine)
))

# -- DiD / causal inference (Sections 10.3-10.9) ------------------------------
# Stata: reghdfe / lassopack / sdid / csdid / drdid / jwdid /
#        eventstudyinteract / estout
install_if_missing(c(
  "fixest",    # feols()      -- replaces reghdfe
  "glmnet",    # cv.glmnet()  -- replaces lassopack
  "hdm",       # rlasso()     -- Belloni et al. double-selection
  "did",       # att_gt()     -- Callaway & Sant'Anna (replaces csdid/drdid)
  "etwfe",     # etwfe()      -- Wooldridge Extended TWFE (replaces jwdid)
  "staggered", # staggered()  -- Sun & Abraham (replaces eventstudyinteract)
  "flextable", # Word tables  -- replaces estout .rtf output
  "officer",   # save_as_docx()
  "dplyr", "tidyr", "purrr"
))

# Hard dependencies of the sub-scripts that were missing from both lists.
# Each is loaded with library() rather than requireNamespace(), so an absent
# one aborts mid-chapter instead of failing the check below -- the opposite of
# what Stata_code10.do does, where every package is confirmed up front and the
# master exits before any estimation runs.
install_if_missing(c(
  "AER",             # ivreg()  -- manual 2SLS in Section 10.2.7
  "truncnorm",       # rtruncnorm() -- running variable in Section 10.2.1
  "marginaleffects", # emfx()   -- ETWFE aggregations in Section 10.7.4
  "sandwich"         # vcovHC() -- robust SEs in Sections 10.2 and 10.7.4
))

# synthdid is GitHub-only (NOT on CRAN).  It was previously listed among the
# install.packages() names above, where it could never install -- which is why
# Section 10.6 was skipped in every R run.  It replaces Stata's -sdid-.
if (!requireNamespace("synthdid", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("synth-inference/synthdid", upgrade = "never")
}

# augsynth is GitHub-only (not on CRAN)
if (!requireNamespace("augsynth", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  # upgrade = "never": without it, install_github() stops and asks which of
  # ~20 dependencies to update, which blocks an unattended run.
  remotes::install_github("ebenmichael/augsynth", upgrade = "never")
}

# Synth: mirrors Stata's ssc install synth
if (!requireNamespace("Synth", quietly = TRUE)) {
  tryCatch(
    install.packages("Synth"),
    error = function(e)
      cat("WARNING: Synth install failed. Run install.packages('Synth') manually.\n")
  )
}

}  # end if (install_packages)

# -- Confirm all key packages loaded ------------------------------------------
# Every package a sub-script loads with library() belongs here: library()
# aborts the run, and the point of this block is to fail BEFORE any estimation
# starts, as Stata_code10.do's -which- loop does.  AER, truncnorm,
# marginaleffects and sandwich were absent and would have surfaced as a
# mid-chapter error instead.  Synth and synthdid are deliberately NOT listed:
# both have graceful fallbacks (Sections 10.5 and 10.6 skip with a message),
# and synthdid is GitHub-only, so a missing one is a skipped section, not a
# failed run.
required_pkgs <- c(
  "rdrobust", "rddensity", "AER", "truncnorm",
  "fixest", "glmnet", "hdm", "did", "etwfe", "staggered",
  "marginaleffects", "sandwich",
  "haven", "ggplot2", "flextable", "dplyr"
)
pkg_missing <- required_pkgs[
  !sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(pkg_missing) > 0)
  stop("Packages not found: ", paste(pkg_missing, collapse = ", "),
       "\nSet install_packages <- TRUE near the top of this script and re-run,",
       "\nor install them manually with install.packages().")
cat("All required packages confirmed.\n")
if (!requireNamespace("synthdid", quietly = TRUE)) {
  cat("NOTE: synthdid not installed -- Section 10.6 (SDID) will be skipped.\n")
  cat("      remotes::install_github('synth-inference/synthdid')\n")
}
cat("\n")

# ============================================================================
# ============================================================================
#
#    CAUSAL INFERENCE  (Sections 10.2 - 10.9)
#
# ============================================================================
# ============================================================================

# ----------------------------------------------------------------------------
# SECTION 10.2: REGRESSION DISCONTINUITY DESIGN
#   Sharp and Fuzzy RD -- merit-based scholarship, HS GPA cutoff c = 3.25
#   Script:   RDD.R
#   Produces: fig10_1..fig10_4 and diagnostics, all with an _R suffix
#             Example_10_2_1.rds / .csv in <out_root>/data
# ----------------------------------------------------------------------------

cat(strrep("=", 70), "\n")
cat(" SECTION 10.2: Regression Discontinuity Design\n")
cat(strrep("=", 70), "\n")

chapter_ok <- TRUE
tryCatch({

source(file.path(syntax_dir, "R_code10_RDD.R"))

# ----------------------------------------------------------------------------
# SECTIONS 10.3-10.9: DIFFERENCE-IN-DIFFERENCES
#   Georgia consolidation -- TWFE, LASSO, SCM, SDID, CS-DiD,
#   permutation, leave-one-out
#   Script:   Georgia_DiD.R
#   Data:     Example_10_3_1.csv, Example_10_7_3.csv
#   Produces: fig10_5, fig10_6, fig10_7_sdid, fig10_8, fig10_9 (all _R)
#             plus diagnostics; results.csv, results_lasso.csv,
#             results_combined.csv in <out_root>/tables
# ----------------------------------------------------------------------------

cat(strrep("=", 70), "\n")
cat(" SECTIONS 10.3-10.9: Georgia DiD\n")
cat(strrep("=", 70), "\n")

source(file.path(syntax_dir, "R_code10_Georgia_DiD.R"))

# ----------------------------------------------------------------------------
# SECTION 10.7.4: EXTENDED TWO-WAY FIXED EFFECTS
#   Wooldridge (2021, 2023) ETWFE via etwfe package.
#   Script:   ETWFE.R
#   Data:     Example_10_7_3.csv
#   Produces: tab10_7_etwfe.csv, fig10_7_4_etwfe_event_study_R.png
# ----------------------------------------------------------------------------

cat(strrep("=", 70), "\n")
cat(" SECTION 10.7.4: Extended TWFE\n")
cat(strrep("=", 70), "\n")

source(file.path(syntax_dir, "R_code10_ETWFE.R"))

# ============================================================================
# CLOSING SUMMARY
# Mirrors Stata's final graph-save loop
# ============================================================================

cat(strrep("=", 70), "\n")
cat("All figures saved as .png to:", graphs_dir, "\n\n")
cat("Every R figure name ends in _R, so R output never overwrites the\n")
cat("Stata figures that go into the manuscript.\n\n")
cat("Figure inventory (all names end in _R):\n")
cat("  RDD:         fig10_1_rdd_plot_credits   fig10_2_rdd_density_test\n")
cat("               fig10_3_rdd_plot_firststage  fig10_4_fuzzy_late_credits\n")
cat("               + fig10_2_2 binscatters, fig10_2_3 bandwidth,\n")
cat("                 fig10_2_4, fig10_2_6, fig10_2_8 diagnostics\n")
cat("  Georgia DiD: fig10_5_parallel_trends  fig10_6_scm_trends\n")
cat("               fig10_7_sdid_trends      fig10_8_event_study\n")
cat("               fig10_9_staggered_es\n")
cat("               + fig10_3_2 robustness, fig10_4_1 lasso,\n")
cat("                 fig10_5_1 scm gap, fig10_7_2 csdid,\n")
cat("                 fig10_8_1 permutation, fig10_8_2 loo,\n")
cat("                 fig10_9_1 summary\n")
cat("  ETWFE:       fig10_7_4_etwfe_event_study\n")
cat("Tables:  tab10_7_etwfe.csv, results.csv, results_lasso.csv,\n")
cat("         results_combined.csv  ->", tables_dir, "\n")
cat("Data:    Example_10_2_1.rds / .csv  ->", data_dir, "\n")
cat(strrep("=", 70), "\n")
cat("Chapter 10 complete:", format(Sys.time()), "\n")
cat(strrep("=", 70), "\n")

}, error = function(e) {
  chapter_ok <<- FALSE
  cat("\n*** RUN STOPPED: ", conditionMessage(e), "\n", sep = "")
}, finally = {
  # Always unwind the sink, so an error never leaves the console muted.
  ch10_stop_log()
})

if (!chapter_ok) {
  message("Chapter 10 did not complete. See the log for the failing call.")
}

# ============================================================================
# END OF R_code10.R
# ============================================================================

# ============================================================================
# Chapter 10 Section Map
# ============================================================================
#
#  Causal Inference
#
#  10.2   Regression Discontinuity Design -- Merit-Based Scholarship
#  10.2.1   Synthetic data generation (N=4,000; seed 20260510)
#  10.2.2   Density continuity test (rddensity) & covariate balance
#  10.2.3   Binned scatterplots
#  10.2.4   Sharp RD: OLS benchmark, manual local linear, rdrobust
#  10.2.5   Bandwidth sensitivity (9-point grid)
#  10.2.6   Polynomial order sensitivity (p = 1, 2, 3)
#  10.2.7   Fuzzy RD: first stage, reduced form, Wald/2SLS
#  10.2.8   Validity checks: placebo cutoffs, donut RD, augmented, subgroup
#  10.2.9   Publication-quality RD plots (rdplot)
#  10.2.10  Summary table; save Example_10_2_1.rds / .csv
#
#  10.3   Difference-in-Differences -- Georgia Consolidation
#  10.3.1   Data structure and variable construction (Example_10_3_1.csv)
#  10.3.2   TWFE DiD estimation (feols)
#  10.3.3   Parallel trends assessment
#  10.3.4   Robustness checks (alternative timing, border states, weighted)
#
#  10.4   LASSO-Residualized DiD
#  10.4.2-10.4.3  Double-selection LASSO DiD (hdm::rlasso)
#
#  10.5   Synthetic Control Method (SCM)
#  10.5.3   SCM application to Georgia consolidation (Synth)
#
#  10.6   Synthetic Difference-in-Differences (SDID)
#  10.6.3   SDID -- single treated unit (synthdid / augsynth)
#
#  10.7   Event Study and Callaway-Sant'Anna DiD
#  10.7.1   Event study specification
#  10.7.2   Callaway-Sant'Anna DiD (did::att_gt)
#  10.7.3-10.7.5  Multi-state staggered adoption analysis
#  10.7.4   Extended TWFE (Wooldridge) via etwfe
#            Unconditional (4a) vs covariate-adjusted (4b);
#            never-treated controls; tab10_7_etwfe, fig10_7_4_etwfe_event_study_R
#
#  10.8   Sensitivity Analysis
#  10.8.2   Permutation inference
#  10.8.3   Leave-one-out sensitivity analysis
#
#  10.9   Results Summary
#
#  Data files saved (in <out_root>/data):
#    Example_10_2_1.rds / .csv
