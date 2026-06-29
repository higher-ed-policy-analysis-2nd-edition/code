#=========================================================================
# Chapter 11 - Instrumental Variables and Marginal Treatment Effects:
#              Returns to a Master's Degree
# Complete R Code
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch11
# Author: Marvin A. Titus
# Date: June 2026
# NOTE: Code development was assisted by Claude (Anthropic). The author
# provided specifications and reviewed, tested, and validated all code.
#=========================================================================
# R translation of Stata_code11.do (tested in Stata 19.5).
# CATE_R.R and MTE_MPRTE_R.R have each been run end-to-end against real data
# and debugged through several rounds of fixes (see each sub-script's own
# header for specifics). This driver script (R_code11.R) itself --
# which only sets up shared paths/log and calls source() on each
# sub-script in turn -- has not yet been run as a single top-to-bottom
# call; if something goes wrong here specifically (as opposed to inside
# CATE_R.R or MTE_MPRTE_R.R), the most likely causes are path/environment
# setup (graphs_dir/tables_dir/logdir/syntax_dir) or the rm() call at the
# top of this file, not the underlying statistical code. Cross-validate
# numeric output against Stata (Chapter11_Stata_output.log) regardless,
# since some differences will be expected (see CATE_output.log /
# MTE_MPRTE_R_output.log discussion of HC1-vs-Stata SE conventions, and
# the mtefe note below) rather than translation errors.
# See the per-section NOTES below for places where R has no exact
# package equivalent of a Stata command (`mtefe`, `boottest`) and a
# substitute or omission strategy was used instead.
#
# Required packages (install once):
#   install.packages(c("haven", "dplyr", "tidyr", "ivreg", "sandwich",
#                       "lmtest", "car", "ggplot2", "stringr"))
#   # Optional, for full feature parity:
#   install.packages(c("sampleSelection", "fwildclusterboot"))
#
# Sub-scripts (sourced from syntax_dir, default "Syntax/R/"):
#   CATE_R.R         Section 11.1.3     -- Conditional Average Treatment Effects
#   MTE_MPRTE_R.R    Sections 11.2-11.3 -- MTE/MPRTE, CBA
#
#   Section 11.1: Instrumental Variables and the LATE  ->  CATE_R.R
#     OLS -> IV/2SLS (LATE) -- returns to master's degree.
#     Instrument: state-funded graduate assistantship (GA) amount.
#     Data: synthetic B&B panel (Example_7_5_3_updated.dta)
#
#   Section 11.1.3: Conditional Average Treatment Effects  ->  CATE_R.R
#     Heterogeneous IV returns to master's degree by observed subgroups
#     (field, income quintile, first-generation status).
#     Data: Example_7_5_3_updated.dta (CATE_R.R now runs first, so this
#     is the script's first data load for the chapter run)
#
#   Section 11.2: Marginal Treatment Effects (MTE)  ->  MTE_MPRTE_R.R
#     Pooled and area-specific MTE via polynomial control function;
#     cluster bootstrap SEs; treatment effect comparison (ATE/ATT/ATU).
#
#   Section 11.2.3: MTE Visualization  ->  MTE_MPRTE_R.R
#     MTE curve, propensity-score distribution, by-area curves,
#     policy-relevant margins.
#
#   Section 11.3: Cost-Benefit Analysis  ->  MTE_MPRTE_R.R
#     PRTE and MPRTE policy simulations (Scenarios 1-8); benefit-cost
#     ratios translating MPRTE estimates into policy-relevant terms.
#
# NOTE: This chapter continues from Chapter 10, which demonstrated
# causal inference methods for state- and institution-level policies
# (RDD, DiD, SCM, SDID, ETWFE). This script covers only the IV/CATE/MTE/
# MPRTE/CBA content described above (Sections 11.1-11.3); see Chapter
# 10's master script (Stata_code10.R) for the earlier material.
#=========================================================================

#=========================================================================
# IMPORTANT: Set working directory (customize this for your system)
#=========================================================================

# setwd("C:/Users/YourName/Documents/book-materials/ch11/data")

#=========================================================================
# OUTPUT DIRECTORIES AND LOG FILE
# Paths switch automatically based on the OS username (Sys.info()[["user"]]).
# The instructor's personal paths are used when user == "marvi";
# all other users get the generic relative paths.
#=========================================================================

# Close any stale log sink silently, then open a fresh one
while (sink.number() > 0) sink()
# NOTE: sink.number(type = "message") does NOT return a count of active
# message diversions the way sink.number() does for output. Per R's own
# documentation, it returns the CONNECTION NUMBER currently used for
# messages, which defaults to 2 (stderr) when no diversion is active.
# A `while (sink.number(type = "message") > 0) ...` loop therefore never
# terminates when nothing is diverted, since 2 > 0 is always TRUE -- this
# is an infinite loop, not a slow operation. The correct check is whether
# the message connection differs from the default (2); only then is
# there an actual diversion to close. Wrapped in tryCatch defensively
# since calling sink(type="message") with nothing to undo can itself
# raise an error on some R versions.
if (sink.number(type = "message") != 2) {
  tryCatch(sink(type = "message"), error = function(e) NULL)
}

if (Sys.info()[["user"]] == "marvi") {
  graphs_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/graphs"
  tables_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/tables"
  logdir     <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/logs"
  syntax_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Syntax/R"
  dir.create("C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output",
             recursive = TRUE, showWarnings = FALSE)
  dir.create(graphs_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(logdir, recursive = TRUE, showWarnings = FALSE)
  master_log_path <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/logs/Chapter11_R_output.log"
} else {
  graphs_dir <- "Output/graphs"
  tables_dir <- "Output/tables"
  logdir     <- "Output/logs"
  syntax_dir <- "Syntax/R"
  dir.create("Output", showWarnings = FALSE)
  dir.create(graphs_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(logdir, recursive = TRUE, showWarnings = FALSE)
  master_log_path <- "Output/logs/Chapter11_R_output.log"
}

master_log_con <- file(master_log_path, open = "wt")
sink(master_log_con, split = TRUE)
sink(master_log_con, type = "message", append = TRUE)

cat("Chapter 11 log opened:", format(Sys.time()), "\n")
cat("Graphs directory:", graphs_dir, "\n")

rm(list = setdiff(ls(), c("graphs_dir", "tables_dir", "logdir", "syntax_dir",
                           "master_log_con", "master_log_path")))

#=========================================================================
# PACKAGE INSTALLATIONS (run once; comment out thereafter)
#=========================================================================

required_pkgs <- c("haven", "dplyr", "tidyr", "ivreg", "sandwich", "lmtest",
                    "car", "ggplot2", "stringr")
optional_pkgs <- c("sampleSelection", "fwildclusterboot")

missing_required <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_required) > 0) {
  install.packages(missing_required)
}
missing_optional <- setdiff(optional_pkgs, rownames(installed.packages()))
if (length(missing_optional) > 0) {
  cat("NOTE: optional packages not installed:", paste(missing_optional, collapse = ", "), "\n")
  cat("      Install for full feature parity (Heckman ML, wild cluster bootstrap):\n")
  cat("      install.packages(c(", paste0('"', missing_optional, '"', collapse = ", "), "))\n")
}
cat("MTE/IV package check complete.\n")

#=========================================================================
#=========================================================================
#
#    INSTRUMENTAL VARIABLES AND MARGINAL TREATMENT EFFECTS
#    (Sections 11.1 - 11.3)
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
#         to test whether subgroup CATEs differ significantly.
#     (c) Forest-plot visualization (Fig. 11.7) and interaction-margins
#         plot (Fig. 11.8), both from the interaction IV model in (b);
#         these are also the source of the CATE values reported in the
#         chapter's Table 11.1.
#     (d) Comparison table (Table 11.2): OLS vs. full-sample IV/LATE vs.
#         separately-estimated subgroup IV models from (a). This is a
#         distinct table from Table 11.1 -- it compares estimators rather
#         than reporting interaction-model subgroup CATEs.
#
#   Script: file.path(syntax_dir, "CATE_R.R")
#   Inherits: graphs_dir, tables_dir, log, theme_springer()
#   Data:     Example_7_5_3_updated.dta (CATE_R.R now runs first, so this
#             is the script's first data load; falls back to
#             Example_7_5_3.dta if the updated file is unavailable, per
#             CATE_R.R's own logic).
#   Produces: fig11_7_cate_forest.png (CATE forest plot, by subgroup)
#             fig11_8_cate_interact.png (CATE interaction plot, STEM x income)
#             tab11_2_cate_subgroup.rtf (Table 11.2)
#
#   Placement in chapter: Section 11.1.3, immediately after the
#   OLS/IV/LATE comparison in Section 11.1.2 and before MTE (11.2).
#   CATE bridges LATE (a single complier average) and MTE (continuous
#   heterogeneity along unobserved resistance) by characterising
#   heterogeneity along observed dimensions (field, income, generation).
#=========================================================================

source(file.path(syntax_dir, "CATE_R.R"))

#-------------------------------------------------------------------------
# Display CATE figures
# (CATE_R.R already prints fig11_7 / fig11_8 at its own end via print();
#  re-displaying here mirrors Stata's `capture graph display` recall.)
#-------------------------------------------------------------------------
tryCatch(print(fig11_7), error = function(e) NULL)
tryCatch(print(fig11_8), error = function(e) NULL)

#=========================================================================
# SECTIONS 11.2-11.3: MARGINAL TREATMENT EFFECTS (external script)
#   OLS -> IV/2SLS -> MTE/MPRTE -> CBA -- returns to master's degree
#   Script: file.path(syntax_dir, "MTE_MPRTE_R.R")
#   Inherits: graphs_dir, log, theme_springer()
#   Data:     Example_7_5_3_updated.dta (or Example_7_5_3.dta fallback)
#
#   Key sections:
#     Sec 6    Pooled cubic polynomial MTE (ATE, ATT, ATU); Heckman; mtefe (local-IV) not replicated -- see Stata output
#     Sec 6b   Area-specific MTE by graduate program area (fully interacted)
#     Sec 6b-ATU  Prospective program area assignment for untreated obs;
#                  area-specific ATU via counterfactual assignment
#                  (seed 20260102; mirrors treated assignment in Sec 1b)
#     Sec 6c   Cluster bootstrap (G=50, R=500): SEs for ATE, ATT, ATU
#                  -- pooled and area-specific, with 95% CIs
#     Sec 9-11 PRTE and MPRTE policy simulations (Scenarios 1-8)
#     Sec 14   Cost-benefit analysis (B/C ratios)
#
#   Produces: bb_mte_analysis.dta, mte_summary_by_field.csv
#             mte_summary_by_program_area.csv
#             fig11_3 (pooled MTE curve)
#             fig11_4 (MTE by propensity score)
#             fig11_5 (MTE curves by graduate program area)
#             fig11_6 (MTE curve with policy-relevant margins)
#             fig11_7 (MTE by propensity score decile)
#             fig11_8 (MPRTE by policy intensity)
#=========================================================================

source(file.path(syntax_dir, "MTE_MPRTE_R.R"))

#-------------------------------------------------------------------------
# Display MTE/MPRTE figures
# MTE_MPRTE_R.R already calls print() for fig11_3 - fig11_8 at its own end.
# The block below provides a single consolidated recall.
#-------------------------------------------------------------------------
tryCatch(print(fig11_3), error = function(e) NULL)
tryCatch(print(fig11_4), error = function(e) NULL)
tryCatch(print(fig11_5), error = function(e) NULL)
tryCatch(print(fig11_6), error = function(e) NULL)
tryCatch(print(fig11_7), error = function(e) NULL)
tryCatch(print(fig11_8), error = function(e) NULL)

#=========================================================================
# Close log and exit
#=========================================================================

cat("\nChapter 11 log closed:", format(Sys.time()), "\n")
sink(type = "message")
sink()
close(master_log_con)

#=========================================================================
# END OF CHAPTER 11 CODE
#=========================================================================

# Chapter 11 Section Map
# ======================================================================
#
#   11.1  Instrumental Variables and the LATE
#     Sections 1-4: Data loading, summary statistics, first-stage, OLS
#     Section 5: IV/2SLS (LATE)
#
#   11.1.3  Conditional Average Treatment Effects (CATE)
#     Section 5b: Subgroup CATEs via IV/2SLS with interaction terms
#     Section 5d: Forest-plot visualization of CATEs by field, income, and generation
#                  -> Fig. 11.7 (forest plot), Fig. 11.8 (interaction plot)
#     Section 5e: CATE comparison table (OLS vs. LATE vs. separate-model
#                 subgroup IV) -> Table 11.2
#
#   11.2  Marginal Treatment Effects
#     Section 6: Manual polynomial MTE (quadratic and cubic), Heckman.
#                mtefe (local-IV) not replicated in R -- see Stata output.
#     Section 6b: Area-specific MTE by graduate program field
#     Section 6c: Cluster bootstrap SEs, wild cluster bootstrap (if
#                 fwildclusterboot is installed)
#     Section 7: Treatment effect comparison (ATE/ATT/ATU/LATE/CATE)
#
#   11.2.3  Marginal Treatment Effects: Visualization
#     Section 8: MTE curve, decile plot, by-area curves, policy margins
#                 -> Fig. 11.3 (pooled MTE curve)
#                 -> Fig. 11.4 (MTE by propensity score)
#                 -> Fig. 11.5 (MTE curves by graduate program area;
#                    each curve truncated to its area's empirical
#                    propensity-score support)
#                 -> Fig. 11.6 (MTE curve with policy-relevant margins)
#                 -> Fig. 11.7 (MTE by propensity score decile)
#                 -> Fig. 11.8 (MPRTE by policy intensity; produced in
#                    Section 11 of MTE_MPRTE_R.R, listed here for the
#                    complete figure inventory)
#
#   11.3  Cost-Benefit Analysis
#     Sections 9-11: PRTE and MPRTE policy simulations (Scenarios 1-8)
#     Sections 12-13: Parameter comparison table, MPRTE visualization
#     Section 14: Cost-benefit analysis (B/C ratios)
#
#   11.4  Summary
#     Sections 15-16: Save results, final summary
#
#   11.5  Appendix
#     11.5.1  Data
#     11.5.2  Code, log files, and figures
#
#   NOTE: This chapter continues from Chapter 10 (state- and institution-
#   level causal inference: RDD, DiD, SCM, SDID, ETWFE). See Chapter 10's
#   own master script and section map for that earlier material.
#
#   NOTE ON FIG. 11.7 / FIG. 11.8: these two figures (MTE by propensity
#   score decile; MPRTE by policy intensity) are produced by the script
#   but are not currently discussed by name in the chapter's prose. They
#   are numbered here for consistency with everything else this chapter
#   produces, not because the text references them yet.
#
#   Figure inventory (in order produced):
#     Fig. 11.7  CATE Forest Plot by Subgroup           (from CATE_R.R)
#     Fig. 11.8  CATE Interaction Plot (STEM x Income)  (from CATE_R.R)
#     Fig. 11.3  Estimated MTE Curve (pooled, cubic polynomial)
#     Fig. 11.4  MTE by Propensity Score (diamond markers + frequency bars)
#     Fig. 11.5  MTE Curves by Graduate Program Area (support-truncated)
#     Fig. 11.6  MTE Curve with Policy-Relevant Margins
#     Fig. 11.7  MTE by Propensity Score Decile          (from MTE_MPRTE_R.R --
#                NOTE: this duplicates the fig11_7 object name used by
#                CATE_R.R's forest plot. In the Stata original, fig11_7 is
#                reused by name within Stata's graph manager across two
#                different sub-scripts; in R, the two objects are kept
#                separate as `fig11_7` (CATE_R.R, forest plot) and a second
#                MTE_MPRTE_R.R `fig11_7` (decile plot), since each script's
#                source() call creates objects in the same global
#                environment and the second will overwrite the first by
#                the time the whole driver finishes. If you need both
#                simultaneously, rename one before sourcing the next
#                script, or save each to disk immediately after creation
#                (already done via ggsave() in both sub-scripts) and
#                reload from file rather than relying on the in-memory
#                object.
#     Fig. 11.8  MPRTE by Policy Intensity                (from MTE_MPRTE_R.R --
#                same in-memory name collision note as fig11_7 above;
#                CATE_R.R's interaction plot and MTE_MPRTE_R.R's intensity
#                plot are both named fig11_8 as ggplot objects in this
#                session. Both are saved to distinct PNG files on disk,
#                so no output is lost -- only the LAST in-memory `fig11_8`
#                survives if you inspect the R environment after the
#                full driver has run.)
#
#   Table inventory:
#     Table 11.1  CATE of Master's Degree Completion on Log Annual Salary
#                  (interaction-model subgroup CATEs, reported in the
#                  chapter text; values come from the interaction IV
#                  model in CATE_R.R Strategy (b) -- not directly exported
#                  as a standalone .rtf by this script)
#     Table 11.2  OLS, LATE, and Conditional Average Treatment Effects
#                  (separate-model subgroup IV comparison, produced by
#                  CATE_R.R; tab11_2_cate_subgroup.rtf)
#
#   Data files saved:
#     bb_mte_analysis.dta             (MTE analysis; includes ma_*_pro vars)
#     mte_summary_by_field.csv
#     mte_summary_by_program_area.csv

#=========================================================================
# SAVE ALL NAMED GRAPHS AS .RDS
# Sub-scripts save .png files alongside each export; this block is a
# final safety net, saving the ggplot OBJECTS themselves (not just their
# rendered images) so they can be reloaded and further edited.
# Reload after R closes:
#   fig <- readRDS(file.path(graphs_dir, "fig11_x.rds"))
#   print(fig)
#=========================================================================

cat("\nSaving all named graphs as .rds files...\n")

graph_names <- c("fig11_3", "fig11_4", "fig11_5", "fig11_6", "fig11_7", "fig11_8")
for (gname in graph_names) {
  if (exists(gname)) {
    tryCatch(
      saveRDS(get(gname), file.path(graphs_dir, paste0(gname, ".rds"))),
      error = function(e) NULL
    )
  }
}

cat("\n========================================================\n")
cat("All graphs saved as .rds to:\n")
cat(" ", graphs_dir, "\n")
cat("\nReopen any graph after closing R with:\n")
cat('  fig <- readRDS(file.path(graphs_dir, "graphname.rds")); print(fig)\n')
cat("\nAll graph names:\n")
cat("  CATE:        fig11_7 (forest plot) fig11_8 (interaction plot)\n")
cat("  MTE/MPRTE:  fig11_3 (MTE curve) fig11_4 (by propensity score)\n")
cat("               fig11_5 (by program area) fig11_6 (policy margins)\n")
cat("               fig11_7 (by decile) fig11_8 (by policy intensity)\n")
cat("  NOTE: fig11_7 and fig11_8 are produced by BOTH CATE_R.R and\n")
cat("        MTE_MPRTE_R.R under the same R object names; only the PNG\n")
cat("        files on disk (distinct filenames) preserve both versions.\n")
cat("  (ATU):      area-specific ATU via prospective assignment (Sec 6b-ATU)\n")
cat("  Table:       tab11_2_cate_subgroup.rtf\n")
cat("========================================================\n")

#=========================================================================
# END OF R_code11.R (TRANSLATED FROM Stata_code11.do)
#=========================================================================
