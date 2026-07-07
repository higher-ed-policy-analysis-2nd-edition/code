#=========================================================================
# R_code13.R
# Chapter 13: Presenting Analyses to Policymakers
# Higher Education Policy Analysis Using Quantitative Techniques, 2nd Ed.
# Marvin A. Titus
#
# R translation of Stata_code13.do and its eight sub-scripts.
#
# Source (code): https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch13
# Source (data): https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch13
#
# Code development assisted by Claude (Anthropic). The author provided
# specifications and reviewed, tested, and validated all code.
#
# IMPORTANT -- PowerPoint export subroutine intentionally NOT included
# yet, per author's instruction: this driver and its sub-scripts must
# run error-free first. Once validated end to end, a PolicymakerDeck13.R
# sub-script (officer + rvg) can be added as its own optional section.
#=========================================================================

#-------------------------------------------------------------------------
# SYSTEM REQUIREMENTS
#-------------------------------------------------------------------------
# Tested and confirmed to run error-free under R 4.5.2.
#
# If more than one R version is installed (common on Windows, where
# each release installs to its own folder under C:/Program Files/R/),
# confirm which version is ACTIVE before running this script:
#   R.version.string
# In RStudio: Tools > Global Options > General > R version.
#
# Each R version keeps its OWN separate package library. Installing a
# package under one version does not make it available when this
# script is later run under a different version (e.g. via double-click,
# Rscript, or a batch file pointing at another installation).
#
# Required package versions:
#   svglite      -- requires systemfonts >= 1.3.0
#   systemfonts  -- must be >= 1.3.0, or svglite fails to load
#   gdtools, rvg -- optional; see PACKAGE CHECKS below for rvg's
#                   warn-only rationale
#
# If you see:
#   Error in loadNamespace(...) :
#     namespace 'systemfonts' 1.2.2 is already loaded, but >= 1.3.0
#     is required
# an outdated systemfonts is installed under the R version currently
# running this script. Fix:
#   1. Close ALL R/RStudio windows and processes completely.
#   2. Open one fresh R session under the intended R version.
#   3. install.packages(c("systemfonts", "svglite", "gdtools", "rvg"),
#                        type = "binary")
#   4. Restart R again, then confirm packageVersion("systemfonts")
#      reports >= 1.3.0.
#   5. Re-run this script from that fresh session.
#
# Harmless and safe to ignore: warnings like "package 'officer'/'rvg'
# was built under R version 4.5.3" -- this only means the package was
# compiled under a slightly newer R than the one in use here.
#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
# PREAMBLE / PATH SETUP
#-------------------------------------------------------------------------
# Mirrors Stata_code13.do's own username-conditional path logic
# (`if c(username) == "marvi"`).
if (Sys.getenv("USERNAME") == "marvi") {
  root_dir        <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 13"
  data_dir        <- file.path(root_dir, "Data", "Stata")
  ch12_tables_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 12/Output/tables"
} else {
  root_dir        <- getwd()
  data_dir        <- file.path(root_dir, "Data")
  ch12_tables_dir <- data_dir
}
graphs_dir <- file.path(root_dir, "Output", "figures")
tables_dir <- file.path(root_dir, "Output", "tables")
logdir     <- file.path(root_dir, "Output", "logs")
syntax_dir <- file.path(root_dir, "Syntax", "R")

dir.create(graphs_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(logdir,     showWarnings = FALSE, recursive = TRUE)

#-------------------------------------------------------------------------
# LOG
#-------------------------------------------------------------------------
log_file <- file.path(logdir, "Chapter13_R_output.log")
log_con  <- file(log_file, open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message")

cat("Chapter 13 R log opened:", format(Sys.time()), "\n")
cat("Graphs directory:", graphs_dir, "\n")
cat("Tables directory:", tables_dir, "\n")

#-------------------------------------------------------------------------
# FIGURE REGISTRY
#-------------------------------------------------------------------------
# Each sub-script's pubexport() helper also registers its live ggplot
# object here, keyed by canonical export name (e.g. "fig13_15_cate_illus")
# rather than R variable name -- avoids a real collision found during
# testing (fig13_15 is reused as a bare variable name in two different
# sub-scripts for two different figures). PolicymakerDeck13.R reads from
# this registry to build editable-vector slides without needing to
# re-derive or re-plot anything.
fig13_registry <- new.env()

#-------------------------------------------------------------------------
# PACKAGE CHECKS
#-------------------------------------------------------------------------
# Warn-only for analytical packages (mirrors Stata_code13.do's
# required_pkgs loop) -- these carry real estimation-method choices, so
# auto-installing without the author's awareness isn't appropriate.
required_pkgs <- c("haven", "dplyr", "tidyr", "ggplot2", "plm", "sandwich",
                    "lmtest", "quadprog", "sf", "maps", "officer")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("WARNING: package", pkg, "not found -- install with install.packages('", pkg, "')\n")
  }
}

# NOTE for rvg (editable-vector figures in the PowerPoint deck): this
# is intentionally warn-only, not auto-install. An earlier auto-install
# attempt here caused real problems on Windows (a locked systemfonts.dll
# from a prior R session blocking reinstall, and the install call itself
# occasionally stalling) -- since rvg only affects HOW figures are
# embedded (editable vs. static PNG, with a working fallback either
# way), it isn't worth the risk of blocking or slowing down every run.
# Install it once, by hand, in a completely fresh R session:
#   install.packages(c("systemfonts", "gdtools", "rvg"), type = "binary")
if (!requireNamespace("rvg", quietly = TRUE)) {
  cat("NOTE: package 'rvg' not found -- PolicymakerDeck13.R will fall back\n")
  cat("to static PNG figures (fully functional, just not editable vectors).\n")
  cat("To enable editable vectors, in a FRESH R session run:\n")
  cat("  install.packages(c('systemfonts', 'gdtools', 'rvg'), type = 'binary')\n")
}

#=========================================================================
# SECTION 13.2 -- PRESENTING DESCRIPTIVE STATISTICS
#=========================================================================
source(file.path(syntax_dir, "DescriptiveTables13.R"))

#=========================================================================
# SECTION 13.2.2 / 13.7.4 -- ESTIMATION RESULTS & MARGINAL EFFECTS TABLES
#=========================================================================
source(file.path(syntax_dir, "EstimationTables13.R"))

#=========================================================================
# SECTION 13.3 -- CHOROPLETH MAPS
#=========================================================================
source(file.path(syntax_dir, "Maps13.R"))

#=========================================================================
# SECTION 13.4 -- TREND GRAPHS AND SIMPLE COMPARISONS
#=========================================================================
source(file.path(syntax_dir, "TrendGraphs13.R"))

#=========================================================================
# SECTION 13.5 -- PRESENTING MULTIVARIATE PANEL REGRESSION RESULTS
#=========================================================================
source(file.path(syntax_dir, "RegressionPlots13.R"))

#=========================================================================
# SECTION 13.6 -- PRESENTING CAUSAL INFERENCE RESULTS
#=========================================================================
source(file.path(syntax_dir, "CausalPlots13.R"))

#=========================================================================
# SECTION 13.7 -- PRESENTING IV, CATE, AND MTE RESULTS
#=========================================================================
source(file.path(syntax_dir, "MTE_CATE_Plots13.R"))

#=========================================================================
# SECTION 13.8 -- PRESENTING BAYESIAN MICROSIMULATION AND CBA RESULTS
#=========================================================================
source(file.path(syntax_dir, "BayesianPlots13.R"))

#=========================================================================
# SECTION 13.9 -- TAILORING PRESENTATION TO POLICYMAKER AUDIENCES
#=========================================================================
# NOTE: primarily prose in the chapter text (matches Stata original).

#=========================================================================
# SUPPLEMENTARY -- EXAMPLE POWERPOINT DECK FOR POLICYMAKER AUDIENCES
#=========================================================================
# Wired in at the end, per author's instruction, only after every
# preceding sub-script has been confirmed to run error-free. Uses
# fig13_registry (populated by each sub-script's pubexport() calls
# above) rather than re-reading exported files, so the deck reflects
# exactly what this run just produced.
source(file.path(syntax_dir, "PolicymakerDeck13.R"))

cat("\nChapter 13 R run completed:", format(Sys.time()), "\n")
cat("All figures written to:", graphs_dir, "\n")
cat("All tables written to:", tables_dir, "\n")

sink(type = "message")
sink()
close(log_con)

cat("Log saved to:", log_file, "\n")
