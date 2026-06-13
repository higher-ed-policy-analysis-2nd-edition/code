# ============================================================================
# R_code10.R  —  Chapter 10: Causal Inference and Marginal Treatment Effects
# R translation of Stata_code10.do
# Higher Education Policy Analysis Using Quantitative Techniques (2nd ed.)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch10
# Author: Marvin A. Titus
# Date: November 2025 (revised May 2026)
# NOTE: Code development was assisted by Claude (Anthropic). The author
#       provided specifications and reviewed, tested, and validated all code.
# ============================================================================
# Script tested in R 4.4+
#
# HOW TO RUN
# ----------
# In RStudio: open this file and click Source (or Ctrl+Shift+S).
# To save a log: RStudio menu -> Tools -> Global Options -> Console ->
#   check "Save .Rhistory" and use File -> Save Console Output.
# Alternatively, run from the Terminal tab:
#   Rscript R_code10.R > Chapter10_R_output.log 2>&1
#
# Sub-scripts (all in syntax_dir = Syntax/R/):
#   R_code10_RDD.R          Section 10.2         -- Sharp/Fuzzy RD, merit scholarship
#   R_code10_Georgia_DiD.R  Sections 10.3-10.9   -- DiD, SCM, SDID, CS-DiD
#   R_code10_ETWFE.R        Section 10.7.4       -- Extended TWFE (Wooldridge)
#   R_code10_MTE_MPRTE.R    Sections 10.10-10.16 -- MTE/MPRTE, CBA
#   R_code10_CATE.R                 Section 10.10.3      -- Conditional Average Treatment Effects
# ============================================================================

# ============================================================================
# 0. OUTPUT DIRECTORIES
#    Paths switch automatically on username. Mirrors Stata_code10.do logic.
# ============================================================================

username <- Sys.info()[["user"]]

if (username == "marvi") {
  graphs_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/graphs"
  tables_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/tables"
  log_dir    <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/logs"
  syntax_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Syntax/R"
} else {
  graphs_dir <- "Output/graphs"
  tables_dir <- "Output/tables"
  log_dir    <- "Output/logs"
  syntax_dir <- "Syntax/R"
}

dir.create(graphs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir,    recursive = TRUE, showWarnings = FALSE)

cat("Chapter 10 started:", format(Sys.time()), "\n")
cat("Graphs :", graphs_dir, "\n")
cat("Tables :", tables_dir, "\n\n")

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
#    Run once; comment out thereafter.
#    Mirrors Stata's: capture ssc install <package>
# ============================================================================

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
  "synthdid",  # synthdid()   -- Synthetic DiD (replaces sdid)
  "flextable", # Word tables  -- replaces estout .rtf output
  "officer",   # save_as_docx()
  "dplyr", "tidyr", "purrr"
))

# augsynth is GitHub-only (not on CRAN)
if (!requireNamespace("augsynth", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("ebenmichael/augsynth")
}

# Synth: mirrors Stata's ssc install synth
if (!requireNamespace("Synth", quietly = TRUE)) {
  tryCatch(
    install.packages("Synth"),
    error = function(e)
      cat("WARNING: Synth install failed. Run install.packages('Synth') manually.\n")
  )
}

# -- MTE / Part B -------------------------------------------------------------
# Stata: mtefe / moremata / fwildclusterboot
# Note: fwildclusterboot requires R >= 4.4.3; install separately when needed.
install_if_missing(c(
  "ivreg",     # ivreg()    -- replaces ivregress 2sls
  "lmtest",    # coeftest() -- robust SEs
  "sandwich",  # vcovHC(), vcovCL()
  "car",       # linearHypothesis() -- replaces test
  "haven"      # read_dta()
))

# -- Confirm all key packages loaded ------------------------------------------
required_pkgs <- c(
  "rdrobust", "rddensity",
  "fixest", "glmnet", "hdm", "did", "etwfe", "staggered", "synthdid",
  "ivreg", "lmtest", "sandwich", "car",
  "haven", "ggplot2", "flextable", "dplyr"
)
pkg_missing <- required_pkgs[
  !sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(pkg_missing) > 0)
  stop("Packages not found: ", paste(pkg_missing, collapse = ", "),
       "\nInstall manually with install.packages().")
cat("All required packages confirmed.\n\n")

# ============================================================================
# ============================================================================
#
#    PART A: CAUSAL INFERENCE  (Sections 10.2 - 10.9)
#
# ============================================================================
# ============================================================================

# ----------------------------------------------------------------------------
# SECTION 10.2: REGRESSION DISCONTINUITY DESIGN
#   Sharp and Fuzzy RD -- merit-based scholarship, HS GPA cutoff c = 3.25
#   Script:   RDD.R
#   Produces: fig10_2_1.png - fig10_2_9.png
#             ch10_rdd_hsls09_synthetic.rds
# ----------------------------------------------------------------------------

cat(strrep("=", 70), "\n")
cat(" SECTION 10.2: Regression Discontinuity Design\n")
cat(strrep("=", 70), "\n")

source(file.path(syntax_dir, "R_code10_RDD.R"))

# ----------------------------------------------------------------------------
# SECTIONS 10.3-10.9: DIFFERENCE-IN-DIFFERENCES
#   Georgia consolidation -- TWFE, LASSO, SCM, SDID, CS-DiD,
#   permutation, leave-one-out
#   Script:   Georgia_DiD.R
#   Data:     Example_10_3_1.csv, Example_10_7_3.csv
#   Produces: fig10_3.png - fig10_9_1.png
#             results.csv, results_lasso.csv, results_combined.csv
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
#   Produces: tab10_7_etwfe.docx, fig10_7_2.png
# ----------------------------------------------------------------------------

cat(strrep("=", 70), "\n")
cat(" SECTION 10.7.4: Extended TWFE\n")
cat(strrep("=", 70), "\n")

source(file.path(syntax_dir, "R_code10_ETWFE.R"))

# ============================================================================
# ============================================================================
#
#    PART B: MARGINAL TREATMENT EFFECTS  (Sections 10.10 - 10.16)
#
# ============================================================================
# ============================================================================

# ----------------------------------------------------------------------------
# SECTIONS 10.10-10.16: MARGINAL TREATMENT EFFECTS
#   OLS -> IV/2SLS -> MTE/MPRTE -> CBA -- returns to master's degree
#   Script:   MTE_MPRTE.R
#   Data:     Example_7_5_3_updated.dta (or Example_7_5_3.dta fallback)
#   Produces: bb_mte_analysis.rds
#             mte_summary_by_field.csv, mte_summary_by_program_area.csv
#             fig10_8.png, fig10_9.png, fig10_10.png, fig10_11.png
#             mte_by_decile.png, mprte_intensity.png
# ----------------------------------------------------------------------------

cat(strrep("=", 70), "\n")
cat(" SECTIONS 10.10-10.16: Marginal Treatment Effects\n")
cat(strrep("=", 70), "\n")

source(file.path(syntax_dir, "R_code10_MTE_MPRTE.R"))

# ----------------------------------------------------------------------------
# SECTION 10.10.3: CONDITIONAL AVERAGE TREATMENT EFFECTS
#   Script:   CATE.R
#   Data:     Example_7_5_3_updated.dta (reloaded inside CATE.R)
#   Produces: fig10_cate_forest.png
#             fig10_cate_interact.png
#             tab10_cate_subgroup.docx
# ----------------------------------------------------------------------------

cat(strrep("=", 70), "\n")
cat(" SECTION 10.10.3: Conditional Average Treatment Effects\n")
cat(strrep("=", 70), "\n")

source(file.path(syntax_dir, "R_code10_CATE.R"))

# ============================================================================
# CLOSING SUMMARY
# Mirrors Stata's final graph-save loop
# ============================================================================

cat(strrep("=", 70), "\n")
cat("All figures saved as .png to:", graphs_dir, "\n\n")
cat("Figure inventory:\n")
cat("  RDD:         fig10_2_1 through fig10_2_9\n")
cat("  Georgia DiD: fig10_3  fig10_6  fig10_3_2  fig10_4_1\n")
cat("               fig10_4  fig10_5_1  fig10_8_1  fig10_8_2  fig10_9_1\n")
cat("  ETWFE:       fig10_7_2\n")
cat("  MTE/MPRTE:   fig10_8  mte_by_decile  fig10_10\n")
cat("               mprte_intensity  fig10_11  fig10_9\n")
cat("  CATE:        fig10_cate_forest  fig10_cate_interact\n")
cat(strrep("=", 70), "\n")
cat("Chapter 10 complete:", format(Sys.time()), "\n")
cat(strrep("=", 70), "\n")

# ============================================================================
# END OF R_code10.R
# ============================================================================

# ============================================================================
# Chapter 10 Section Map
# ============================================================================
#
#  PART A -- Causal Inference
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
#  10.2.10  Summary table; save ch10_rdd_hsls09_synthetic.rds
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
#            never-treated controls; tab10_7_etwfe, fig10_7_2
#
#  10.8   Sensitivity Analysis
#  10.8.2   Permutation inference
#  10.8.3   Leave-one-out sensitivity analysis
#
#  10.9   Results Summary: Part A
#
#  PART B -- Marginal Treatment Effects
#
#  10.10  Instrumental Variables and the LATE
#    Sections 1-4: Data loading, summary statistics, first-stage, OLS
#    Section  5:   IV/2SLS (LATE) via ivreg
#
#  10.10.3  Conditional Average Treatment Effects (CATE.R)
#    Section 5b: Subgroup CATEs via IV/2SLS
#    Section 5c: Interaction IV -- test for differential subgroup effects
#    Section 5d: Forest-plot visualization
#    Section 5e: Comparison table (OLS / LATE / CATE by subgroup)
#
#  10.11  Marginal Treatment Effects
#    Section 6:  Manual polynomial MTE (quadratic and cubic)
#    Section 6b: Area-specific MTE by graduate program field
#    Section 6c: Cluster bootstrap SEs (fwildclusterboot::boottest)
#    Section 7:  Treatment effect comparison (ATE/ATT/ATU/LATE/CATE)
#
#  10.11 (Visualization)
#    Section 8: MTE curve, decile plot, by-area curves
#
#  10.12  Cost-Benefit Analysis
#    Sections 9-11:  PRTE and MPRTE policy simulations (Scenarios 1-8)
#    Sections 12-13: Parameter comparison table, MPRTE visualization
#    Section  14:    Cost-benefit analysis (B/C ratios)
#
#  10.13  Summary
#    Sections 15-16: Save results, final summary
#
#  Data files saved:
#    ch10_rdd_hsls09_synthetic.rds
#    bb_mte_analysis.rds
#    mte_summary_by_field.csv
#    mte_summary_by_program_area.csv
