#=========================================================================
# Chapter 11 - Instrumental Variables and Marginal Treatment Effects:
#              Returns to a Master's Degree
# Complete R Code
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch11
# Author: Marvin A. Titus
# Date: November 2026 (R translation of Stata_code11.do)
# NOTE: Code development was assisted by Claude (Anthropic). The author
# provided specifications and reviewed, tested, and validated all code.
#=========================================================================
# R translation of Stata_code11.do (Stata 19.5). Requires R 4.4 or later.
#
# This driver only sets up shared paths and the master log, then calls
# the two sub-scripts with source(). It contains no estimation code.
#
# Sub-scripts (all in syntax_dir = Syntax/R/):
#   CATE_R.R                Section 11.1.3 -- Conditional Average Treatment
#                           Effects (translation of CATE.do)
#   MTE_MPRTE.R             Section 11.2   -- MTE, PRTE/MPRTE, CBA
#                           (translation of MTE_MPRTE.do)
#
#   Section 11.1: Instrumental Variables and the LATE
#     OLS -> IV/2SLS (LATE) -- returns to master's degree.
#     Instrument: state-funded graduate assistantship (GA) amount.
#     Data: synthetic B&B panel (Example_7_5_3_updated.dta)
#
#   Section 11.1.3: Conditional Average Treatment Effects  ->  CATE_R.R
#     Heterogeneous IV returns to master's degree by observed subgroups
#     (field, income quintile, first-generation status).
#
#   Section 11.2.1: Polynomial Control Function Approach
#                                         ->  MTE_MPRTE.R
#     Pooled MTE (quadratic) and area-specific MTE (linear) estimated with
#     mte_poly(), an R re-implementation of Stata's mtefe; state-clustered
#     bootstrap SEs.
#
#   Section 11.2.2: MTE Results  ->  MTE_MPRTE.R
#     ATE/ATT/ATU/LATE/MPRTE (Tables 11.2-11.3); PRTE for eight GA
#     funding scenarios (Tables 11.4-11.5); benefit-cost ratios
#     (Table 11.6); MTE, policy-weight, and PRTE-intensity figures.
#
# RUN TIME: about 5 minutes for the whole chapter (CATE_R.R takes a few
# seconds; MTE_MPRTE.R about 4 minutes including its own
# 500-replication bootstrap). Unlike the Stata version, the R script
# runs its bootstrap by default because it is fast. See the RUN TIME
# note at the top of MTE_MPRTE.R.
#
# R vs. STATA: point estimates match the Stata log. Anything that depends
# on random draws (bootstrap standard errors and bands) differs between
# the two languages, because R and Stata use different random-number
# generators. The book reports the Stata standard errors.
#
# Required packages (installed below if missing):
#   haven, dplyr, tidyr, ivreg, sandwich, lmtest, car, ggplot2, stringr,
#   AER
#
# NOTE: This chapter continues from Chapter 10, which demonstrated
# causal inference methods for state- and institution-level policies
# (RDD, DiD, SCM, SDID, ETWFE). This script covers only the IV/CATE/MTE/
# PRTE/CBA content described above (Sections 11.1-11.2); see Chapter
# 10's master script (R_code10.R) for the earlier material.
#=========================================================================

#=========================================================================
# IMPORTANT: Set working directory (customize this for your system)
# Readers: the data are downloaded from GitHub into the working directory
# if they are not already there.
#=========================================================================

# setwd("C:/Users/YourName/Documents/book-materials/ch11")

#=========================================================================
# OUTPUT DIRECTORIES AND LOG FILE
# Paths switch automatically based on the OS username (Sys.info()[["user"]]).
# The instructor's personal paths are used when user == "marvi";
# all other users get the generic relative paths.
#=========================================================================

# Close any stale log sink silently, then open a fresh one
while (sink.number() > 0) sink()

if (Sys.info()[["user"]] == "marvi") {
  out_root   <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output"
  syntax_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Syntax/R"
} else {
  out_root   <- "Output"
  syntax_dir <- "Syntax/R"
}
graphs_dir <- file.path(out_root, "graphs")
tables_dir <- file.path(out_root, "tables")
log_dir    <- file.path(out_root, "logs")
logdir     <- log_dir     # CATE_R.R uses this name; MTE_MPRTE.R uses log_dir
for (p in c(out_root, graphs_dir, tables_dir, log_dir)) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

# Absolute paths, so later setwd() calls in the sub-scripts cannot break them
out_root   <- normalizePath(out_root,   winslash = "/")
graphs_dir <- normalizePath(graphs_dir, winslash = "/")
tables_dir <- normalizePath(tables_dir, winslash = "/")
log_dir    <- normalizePath(log_dir,    winslash = "/")
logdir     <- log_dir
syntax_dir <- normalizePath(syntax_dir, winslash = "/", mustWork = FALSE)

# Master log. Only ordinary output is logged; the message stream is not
# diverted, so errors still reach the console if a sub-script stops.
master_log_path <- file.path(log_dir, "Chapter11_R_output.log")
master_log_con  <- file(master_log_path, open = "wt")
sink(master_log_con, split = TRUE)

cat("Chapter 11 log opened:", format(Sys.time()), "\n")
cat("Graphs directory:", graphs_dir, "\n")

rm(list = setdiff(ls(), c("out_root", "graphs_dir", "tables_dir", "log_dir",
                          "logdir", "syntax_dir",
                          "master_log_con", "master_log_path")))

#=========================================================================
# PACKAGE INSTALLATIONS (installs only what is missing)
#=========================================================================

required_pkgs <- c("haven", "dplyr", "tidyr", "ivreg", "sandwich", "lmtest",
                   "car", "ggplot2", "stringr", "AER")
missing_required <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_required) > 0) install.packages(missing_required)
cat("MTE/IV packages confirmed.\n")
# The Heckman models and wild cluster bootstraps are coded directly in
# MTE_MPRTE.R, so sampleSelection and fwildclusterboot are not
# needed.

#=========================================================================
#=========================================================================
#
#    INSTRUMENTAL VARIABLES AND MARGINAL TREATMENT EFFECTS
#    (Sections 11.1 - 11.2)
#
#=========================================================================
#=========================================================================

#=========================================================================
# SECTION 11.1.3: CONDITIONAL AVERAGE TREATMENT EFFECTS (CATE)
#   Heterogeneous IV returns to master's degree by observed subgroups.
#   Strategy:
#     (a) Subgroup IV/2SLS -- run the baseline IV model separately within
#         cells defined by field, income quintile, and first-generation
#         status; collect point estimates and SEs for a forest plot.
#     (b) Interaction IV -- include field x treatment and
#         income_q1 x treatment interactions in the full-sample IV model
#         to test whether subgroup CATEs differ significantly; source of
#         the CATE values reported in the chapter's Table 11.1.
#     (c) Forest plot (Fig. 11.1) and interaction plot (Fig. 11.2).
#     (d) Supplementary comparison table: OLS vs. full-sample IV/LATE
#         vs. separately-estimated subgroup IV models from (a). It is
#         NOT reproduced in the chapter (Tables 11.2-11.6 in the book
#         are the MTE tables); the file keeps its original name,
#         tab11_2_cate_subgroup.rtf.
#
#   Script:   file.path(syntax_dir, "CATE_R.R")
#             (opens its own log, log_dir/CATE_output.log)
#   Inherits: graphs_dir, tables_dir, logdir
#   Data:     Example_7_5_3_updated.dta (CATE_R.R runs first, so this is
#             the chapter run's first data load; downloaded from GitHub
#             if not found locally)
#   Produces: fig11_1_cate_forest_R.png   (CATE forest plot, by subgroup)
#             fig11_2_cate_interact_R.png (CATE interaction plot,
#                                          STEM x income)
#             tab11_2_cate_subgroup.rtf   (supplementary; not a numbered
#                                          table in the chapter)
#
#   Placement in chapter: Section 11.1.3, immediately after the
#   OLS/IV/LATE comparison in Section 11.1.2 and before MTE (11.2).
#   CATE bridges LATE (a single complier average) and MTE (continuous
#   heterogeneity along unobserved resistance) by characterising
#   heterogeneity along observed dimensions (field, income, generation).
#=========================================================================

source(file.path(syntax_dir, "CATE_R.R"))

#-------------------------------------------------------------------------
# Display CATE figures (mirrors Stata's `capture graph display`)
#-------------------------------------------------------------------------
if (exists("fig11_1")) print(fig11_1)    # forest plot: CATE by subgroup
if (exists("fig11_2")) print(fig11_2)    # interaction-term CATE plot

#=========================================================================
# SECTION 11.2: MARGINAL TREATMENT EFFECTS (external script)
#   OLS -> IV/2SLS -> MTE -> PRTE/MPRTE -> CBA -- returns to master's degree
#   Script:   file.path(syntax_dir, "MTE_MPRTE.R")
#             (opens its own log, log_dir/MTE_MPRTE_R.log; the R
#             counterpart of MTE_MPRTE_output.log)
#   Inherits: graphs_dir, log_dir
#   Data:     Example_7_5_3_updated.dta
#
#   Key sections (same numbering as MTE_MPRTE.do):
#     Sec 1-5    Data, summary statistics, first stage, OLS, IV/2SLS
#     Sec 6a     Probit propensity score P(Z,X)
#     Sec 6b     Manual polynomial MTE (teaching illustration only)
#     Sec 6c     Pooled MTE via mte_poly(), quadratic: ATE, ATT, ATU,
#                  LATE, MPRTE1-3; Heckman comparison     -> Table 11.2
#     Sec 6d     Area-specific MTE via mte_poly(), linear, one model per
#                  program area; non-completers assigned a prospective
#                  area (read from Example_7_5_3_updated.dta) -> Table 11.3
#     Sec 6e     State-clustered bootstrap (G=50, R=500) for pooled and
#                  area-specific SEs; wild cluster bootstrap (WCR/WRE,
#                  Webb weights) for the OLS and IV estimates
#     Sec 9-10b  PRTE for Scenarios 1-8                   -> Tables 11.4-11.5
#     Sec 11     PRTE by policy intensity
#     Sec 13     Policy weights and propensity-score support
#     Sec 14     Cost-benefit analysis (B/C ratios)     -> Table 11.6
#
#   Produces: bb_mte_analysis_R.dta, mte_summary_by_field_R.csv,
#             mte_summary_by_program_area_R.csv, mte_bootstrap_draws_R.rds
#             fig11_3_mte_curve_R.png         (pooled MTE curve with
#                                              bootstrap band)
#             fig11_4_mte_by_propensity_R.png (propensity-score histogram
#                                              with the MTE curve)
#             fig11_5_mte_byarea_curve_R.png  (MTE curves by graduate
#                                              program area)
#             fig11_6_mte_policy_regions_R.png (MTE curve with PRTE
#                                              weights: uniform +$1k vs.
#                                              low-income +$2k)
#             fig11_8_mprte_by_intensity_R.png (PRTE by policy intensity)
#   (Fig. 11.7, MTE by propensity-score decile, is no longer produced.)
#=========================================================================

mte_script <- file.path(syntax_dir, "MTE_MPRTE.R")
if (!file.exists(mte_script)) stop("MTE_MPRTE.R not found in ", syntax_dir)

# MTE_MPRTE.R closes every open sink before opening its own log,
# so pause the master log here and resume it (appending) afterwards.
cat("\nMaster log paused while", basename(mte_script), "runs;",
    "see MTE_MPRTE_R.log for its output.\n")
sink()
source(mte_script)
while (sink.number() > 0) sink()
sink(master_log_con, split = TRUE)
cat("\nMaster log resumed:", format(Sys.time()), "\n")

#-------------------------------------------------------------------------
# Display MTE/MPRTE figures
# MTE_MPRTE.R saves each figure as a PNG (with the _R suffix).
# The block below provides a single consolidated recall of the plot
# objects (g3 = Fig. 11.3, g4 = Fig. 11.4, ...).
#-------------------------------------------------------------------------
for (g in c("g3", "g4", "g5", "g6", "g8")) {
  if (exists(g)) print(get(g))
}

#=========================================================================
# Close log and exit
# (Each sub-script saves its figures as PNG files in graphs_dir.)
#=========================================================================

cat("\nChapter 11 log closed:", format(Sys.time()), "\n")
sink()
close(master_log_con)

#=========================================================================
# END OF CHAPTER 11 CODE
#=========================================================================

# Chapter 11 Section Map
# ======================================================================
#
#   11.0  Introduction
#
#   11.1  Instrumental Variables and the LATE
#     11.1.1  First Stage and Instrument Relevance
#     11.1.2  OLS and IV/LATE Estimation
#             (MTE_MPRTE.R Sections 1-5: data, summary
#              statistics, first stage, OLS, IV/2SLS)
#     11.1.3  Conditional Average Treatment Effects (CATE)  -> CATE_R.R
#             Subgroup IV, interaction IV, and forest-plot visualization
#                 -> Table 11.1, Fig. 11.1 (forest plot),
#                    Fig. 11.2 (interaction plot)
#
#   11.2  Marginal Treatment Effects (MTE)         -> MTE_MPRTE.R
#     11.2.1  Polynomial Control Function Approach
#             Section 6a: probit propensity score
#             Section 6b: manual polynomial MTE (teaching illustration)
#             Section 6c: pooled mte_poly(), quadratic; Heckman comparison
#             Section 6d: area-specific mte_poly(), linear
#             Section 6e: state-clustered bootstrap; wild cluster bootstrap
#     11.2.2  MTE Results
#             Sections 6c, 6e, 7  -> Table 11.2 (pooled parameters)
#             Sections 6d, 6e     -> Table 11.3 (parameters by area)
#             Sections 9-10b      -> Tables 11.4-11.5 (PRTE, Scenarios 1-8)
#             Section 14          -> Table 11.6 (cost-benefit analysis)
#             Section 8           -> Fig. 11.3, Fig. 11.5
#             Section 11          -> Fig. 11.8
#             Section 13          -> Fig. 11.4, Fig. 11.6
#             Sections 15-16: save results, final summary
#
#   11.3  Summary
#
#   11.4  Appendix
#     11.4.1  Data
#     11.4.2  Code, log files, and figures
#
#   NOTE: This chapter continues from Chapter 10 (state- and institution-
#   level causal inference: RDD, DiD, SCM, SDID, ETWFE). See Chapter 10's
#   own master script and section map for that earlier material.
#
#   Figure inventory (script names; final numbering set in the chapter):
#     Fig. 11.1  CATE Forest Plot by Subgroup                  (CATE_R.R)
#     Fig. 11.2  CATE Interaction Plot (STEM x Income)         (CATE_R.R)
#     Fig. 11.3  Estimated MTE Curve, pooled (quadratic), with
#                state-clustered bootstrap band
#     Fig. 11.4  Propensity-Score Distribution with the MTE Curve
#     Fig. 11.5  MTE Curves by Graduate Program Area (linear)
#     Fig. 11.6  MTE Curve with PRTE Weights (uniform +$1k vs.
#                low-income +$2k)
#     Fig. 11.8  PRTE by Policy Intensity
#     (Fig. 11.7, MTE by propensity-score decile, is no longer produced.)
#     All R figure files end in _R.png so they never overwrite the
#     Stata exports (_Stata.png) in the same folder.
#
#   Table inventory:
#     Table 11.1  CATE of Master's Degree Completion on Log Annual Salary
#                  (interaction IV model, CATE_R.R Strategy (b); reported
#                  in the chapter text, not exported as a standalone .rtf)
#     Table 11.2  MTE-Based Treatment Parameters for Master's Degree
#                  Completion (MTE_MPRTE.R Sections 6c, 6e, 7)
#     Table 11.3  MTE-Based Treatment Parameters by Graduate Program Area
#                  (Sections 6d, 6e)
#     Table 11.4  PRTE for Four GA Funding Targeting Scenarios
#                  (Sections 9-10b)
#     Table 11.5  PRTE for Four Graduate Program Area Scenarios
#                  (Sections 9-10b)
#     Table 11.6  Cost-Benefit Analysis for Eight GA Funding Scenarios
#                  (Section 14)
#     The book's Tables 11.2-11.6 are assembled from the Stata log
#     (MTE_MPRTE_output.log); MTE_MPRTE_R.log holds the R counterparts.
#     Supplementary (not in the chapter): tab11_2_cate_subgroup.rtf,
#                  OLS vs. LATE vs. separate-model subgroup IV (CATE_R.R).
#                  The "tab11_2" prefix is a holdover from an earlier
#                  draft and does not correspond to Table 11.2.
#
#   Data files saved (by MTE_MPRTE.R, in the data directory):
#     bb_mte_analysis_R.dta           (MTE analysis; includes ma_*_pro vars)
#     mte_summary_by_field_R.csv
#     mte_summary_by_program_area_R.csv
#     mte_bootstrap_draws_R.rds       (bootstrap draws)
