*========================================================================
* Chapter 10 - Causal Inference
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative Techniques
* (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch10
* Author: Marvin A. Titus
* Date: November 2025 (revised June 2026)
* NOTE: Code development was assisted by Claude (Anthropic). The author
* provided specifications and reviewed, tested, and validated all code.
*========================================================================
* Script tested in Stata 19.5
* Compatible with Stata version 19 or later
*
* Sub-scripts (all in $syntax_dir = Syntax/Stata/):
*   RDD.do          Section 10.2   — Sharp/Fuzzy RD, merit scholarship
*   Georgia_DiD.do  Sections 10.3–10.9  — DiD, SCM, SDID, CS-DiD
*   ETWFE.do        Section 10.7.4 — Extended TWFE (Wooldridge) via jwdid
*
* NOTE: The instrumental variables, CATE, and marginal treatment effect
* (MTE/MPRTE) material previously developed as "Part B" of this chapter
* now constitutes Chapter 11 and is covered by Stata_code11.do and its
* sub-scripts (CATE.do, MTE_MPRTE.do). This chapter (10.2-10.9) covers
* only the institutional/state-level quasi-experimental methods below.
*
*   Section 10.2: Regression Discontinuity Design  →  RDD.do
*     Sharp and Fuzzy RD applied to a merit-based scholarship cutoff.
*     Running variable: HS GPA (cutoff c = 3.25). Outcomes: second-year
*     persistence, Year-1 credits earned, Year-1 college GPA.
*     Synthetic data (N = 4,000) calibrated to HSLS:09.
*     Packages: rdrobust, rddensity, lpdensity, cmogram.
*
*   Sections 10.3-10.9: Georgia Consolidation  →  Georgia_DiD.do
*     TWFE DiD, LASSO DiD, Synthetic Control, Synthetic DiD,
*     Event Study / Callaway-Sant'Anna, Staggered Adoption,
*     Permutation and Leave-One-Out sensitivity tests.
*     Data: SHEEO state-level finance panel (Example_10_3_1.csv)
*
*   Section 10.7.4 (extension): Extended TWFE  →  ETWFE.do
*     Wooldridge (2021, 2023) ETWFE via the jwdid command, applied to the
*     three-state staggered adoption design. Unconditional vs covariate-
*     adjusted specifications; never-treated controls. Data: Example_10_7_3.csv
*========================================================================

*========================================================================
* IMPORTANT: Set working directory (customize this for your system)
*========================================================================

* Use a global path to make it easy to update in one place
* global ch10data "C:/Users/YourName/Documents/book-materials/ch10/data"
* cd "$ch10data"

*========================================================================
* OUTPUT DIRECTORIES AND LOG FILE
* Paths switch automatically based on the OS username (c(username)).
* The instructor's personal paths are used when username == "marvi";
* all other users get the generic relative paths.
*========================================================================

* Close any stale log silently, then open a fresh one
capture log close

if c(username) == "marvi" {
    global graphs_dir  "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/graphs"
    global tables_dir  "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/tables"
    global logdir      "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/logs"
    global syntax_dir  "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Syntax/Stata"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/graphs"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/tables"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/logs"
    log using ///
        "C:\\Users\\marvi\\Dropbox\\Book\\2nd Edition\\Chapter 10\\Output\\logs\\Chapter10_Stata_output.log", ///
        replace text
}
else {
    global graphs_dir  "Output/graphs"
    global tables_dir  "Output/tables"
    global logdir      "Output/logs"
    global syntax_dir  "Syntax/Stata"
    capture mkdir "Output"
    capture mkdir "Output/graphs"
    capture mkdir "Output/tables"
    capture mkdir "Output/logs"
    log using "Output/logs/Chapter10_Stata_output.log", replace text
}

di "Chapter 10 log opened: " c(current_date) " " c(current_time)
di "Graphs directory: $graphs_dir"

clear all
set more off
version 19
set scheme s2mono        // Monochrome scheme for Springer B&W print
set graphics on          // Ensure graph window is active throughout

*========================================================================
* PACKAGE INSTALLATIONS (run once; comment out thereafter)
*========================================================================
* RDD packages (Section 10.2)
ssc install rdrobust,  replace   // also installs rdplot
ssc install rddensity, replace   // Cattaneo, Jansson & Ma (2020)
ssc install cmogram,   replace

* lpdensity is required for rddensity's plot option.
* Hosted on GitHub, not SSC; install once via net install.
net install lpdensity, ///
    from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata) ///
    replace

* Confirm RDD packages are available before proceeding
foreach pkg in rdrobust rddensity lpdensity cmogram {
    capture which `pkg'
    if _rc != 0 {
        di as error "Package '`pkg'' not found. Run:  ssc install `pkg', replace"
        exit 198
    }
}
di as text "RDD packages confirmed."

* DiD / causal inference packages (Sections 10.3-10.9)
ssc install reghdfe,   replace
ssc install lassopack, replace
capture ssc install synth, replace
if _rc != 0 {
    di as text "WARNING: ssc install synth failed (r(" _rc ")). synth may already be installed."
    di as text "  If not, run manually once the SSC mirror recovers: ssc install synth, replace"
}
ssc install sdid,      replace
ssc install csdid,     replace
ssc install drdid,     replace
ssc install jwdid,     replace   // Extended TWFE (Wooldridge) — Section 10.7
ssc install eventstudyinteract, replace
capture ssc install estout, replace

*========================================================================
*========================================================================
*
*    CAUSAL INFERENCE
*            (Sections 10.2 – 10.9)
*
*========================================================================
*========================================================================

*========================================================================
* SECTION 10.2: REGRESSION DISCONTINUITY DESIGN (external script)
*   Sharp and Fuzzy RD -- merit-based scholarship, HS GPA cutoff c = 3.25
*   Script: $syntax_dir/RDD.do
*   Inherits: $graphs_dir, log, set scheme s2mono
*   Produces: ch10_rdd_hsls09_synthetic.dta, fig10_2_1 - fig10_2_7
*========================================================================

do "$syntax_dir/RDD.do"

*------------------------------------------------------------------------
* Display RDD figures in the Stata graph window
*
* fig10_2_9 is already named (name(fig10_2_9, replace)) and always recallable.
* fig10_2_1 – fig10_2_8 are shown as they are created but are NOT named,
* so they cannot be recalled after the script moves on.
*
* To make all RDD figures recallable, add the following name() options
* to RDD.do (each appears immediately before the closing parenthesis of
* the relevant graph command):
*
*   rddensity x, c(0) plot graph_opt(... name(fig10_2_1, replace))
*
*   cmogram loop → add to graphopts():
*       name("fig10_2_2_`outcome'", replace)
*
*   Bandwidth sensitivity twoway:
*       name(fig10_2_3, replace)
*
*   rdplot persist_sharp ... graph_options(... name(fig10_2_4, replace))
*   rdplot credits_y1   ... graph_options(... name(fig10_2_5, replace))
*   rdplot cgpa_y1      ... graph_options(... name(fig10_2_6, replace))
*   rdplot D_fuzzy      ... graph_options(... name(fig10_2_7, replace))
*   rdplot persist_fuzzy... graph_options(... name(fig10_2_8, replace))
*------------------------------------------------------------------------
capture graph display fig10_2_1               // density test
capture graph display fig10_2_2_persist_sharp // binscatter persistence
capture graph display fig10_2_2_credits_y1    // binscatter credits
capture graph display fig10_2_2_cgpa_y1       // binscatter GPA
capture graph display fig10_2_3               // bandwidth sensitivity
capture graph display fig10_2_4               // rdplot persistence
capture graph display fig10_2_5               // rdplot credits
capture graph display fig10_2_6               // rdplot GPA
capture graph display fig10_2_7               // rdplot first stage
capture graph display fig10_2_8               // rdplot persist_fuzzy
capture graph display fig10_2_9               // fuzzy LATE comparison

*========================================================================
*========================================================================
*
*    CAUSAL INFERENCE (continued)
*    SECTIONS 10.3 – 10.9: GEORGIA HIGHER EDUCATION CONSOLIDATION
*
*========================================================================
*========================================================================

*========================================================================
* SECTION 10.3–10.9: DIFFERENCE-IN-DIFFERENCES (external script)
*   Georgia higher education consolidation — TWFE, LASSO, SCM, SDID,
*   Callaway-Sant'Anna, staggered adoption, permutation, leave-one-out
*   Script: $syntax_dir/Georgia_DiD.do
*   Inherits: $graphs_dir, log, set scheme s2mono
*   Data:     Example_10_3_1.csv, Example_10_7_3.csv (downloaded)
*   Produces: results.csv, results_lasso.csv, results_combined.csv
*             fig10_3 – fig10_7
*========================================================================

do "$syntax_dir/Georgia_DiD.do"

*------------------------------------------------------------------------
* Display Georgia DiD figures in the Stata graph window
* All Georgia DiD figures already use name() and are fully recallable.
*------------------------------------------------------------------------
capture graph display fig10_3      // parallel trends
capture graph display fig10_6      // event study
capture graph display fig10_3_2    // robustness checks
capture graph display fig10_4_1    // LASSO DiD comparison
capture graph display fig10_4      // SCM actual vs. synthetic
capture graph display fig10_5_1    // SCM gap plot
capture graph display fig10_8_1    // permutation distribution
capture graph display fig10_8_2    // leave-one-out sensitivity
capture graph display fig10_9_1    // estimator comparison

*========================================================================
* SECTION 10.7.4: EXTENDED TWO-WAY FIXED EFFECTS (external script)
*   Wooldridge (2021, 2023) ETWFE estimator via the jwdid command, applied
*   to the three-state staggered adoption design (48-state panel).
*   Unconditional (no covariates) vs covariate-adjusted (hettype(cohort))
*   specifications, both using never-treated states as controls.
*   Script:   $syntax_dir/ETWFE.do
*   Inherits: $graphs_dir, $tables_dir, log, set scheme s2mono
*   Data:     Example_10_7_3.csv (downloaded)
*   Produces: tab10_7_etwfe.rtf, fig10_7_3
*------------------------------------------------------------------------
* NOTE: ETWFE and Callaway-Sant'Anna target the same ATT(g,t) estimands;
* the unconditional ETWFE overall ATT is the figure directly comparable to
* the csdid staggered result from Georgia_DiD.do. The covariate-adjusted
* run illustrates the result's sensitivity to differential pre-trends.
*========================================================================

do "$syntax_dir/ETWFE.do"

*------------------------------------------------------------------------
* Display ETWFE figure in the Stata graph window
*------------------------------------------------------------------------
capture graph display fig10_7_3    // ETWFE event study (staggered adoption)

*========================================================================
* Close log and exit
*========================================================================

clear all

log close

*========================================================================
* END OF CHAPTER 10 CODE
*========================================================================

/* Chapter 10 Section Map
   ======================================================================

   Chapter 10 Sections

   10.2   Regression Discontinuity Design — Merit-Based Scholarship
   10.2.1   Synthetic data generation (N=4,000; seed 20260510)
   10.2.2   Density continuity test (rddensity) & covariate balance
   10.2.3   Binned scatterplots (cmogram)
   10.2.4   Sharp RD: OLS benchmark, manual local linear, rdrobust
   10.2.5   Bandwidth sensitivity (9-point grid)
   10.2.6   Polynomial order sensitivity (p = 1, 2, 3)
   10.2.7   Fuzzy RD: first stage, reduced form, Wald/2SLS
   10.2.8   Validity checks: placebo cutoffs, donut RD, augmented, subgroup
   10.2.9   Publication-quality RD plots (rdplot)
   10.2.10  Summary table; save ch10_rdd_hsls09_synthetic.dta

   10.3   Difference-in-Differences — Georgia Consolidation
   10.3.1   Data structure and variable construction (Example_10_3_1.csv)
   10.3.2   TWFE DiD estimation
   10.3.3   Parallel trends assessment
   10.3.4   Robustness checks (alternative timing, border states, weighted)

   10.4   LASSO-Residualized DiD
   10.4.2-10.4.3  Double-selection LASSO DiD

   10.5   Synthetic Control Method (SCM)
   10.5.3   SCM application to Georgia consolidation

   10.6   Synthetic Difference-in-Differences (SDID)
   10.6.3   SDID — single treated unit

   10.7   Event Study and Callaway-Sant'Anna DiD
   10.7.1   Event study specification
   10.7.2   Callaway-Sant'Anna DiD
   10.7.3-10.7.5  Multi-state staggered adoption analysis
   10.7.4   Extended TWFE (Wooldridge) via jwdid  →  ETWFE.do
            Unconditional (4a) vs covariate-adjusted (4b); never-treated
            controls; comparison table tab10_7_etwfe and figure fig10_7_3

   10.8   Sensitivity Analysis
   10.8.2   Permutation inference
   10.8.3   Leave-one-out sensitivity analysis

   10.9   Results Summary

   Data files saved:
     ch10_rdd_hsls09_synthetic.dta   (Section 10.2 RDD synthetic data)
*/

*========================================================================
* SAVE ALL NAMED GRAPHS AS .GPH
* Sub-scripts save .gph files alongside each export; this block is a
* final safety net. Reload after Stata closes:
*   graph use "$graphs_dir/graphname.gph"
*========================================================================
di _n as text "Saving all named graphs as .gph files..."

foreach gname in                                                        ///
    fig10_2_1  fig10_2_2_persist_sharp  fig10_2_2_credits_y1           ///
    fig10_2_2_cgpa_y1  fig10_2_3  fig10_2_4  fig10_2_5  fig10_2_6     ///
    fig10_2_7  fig10_2_8  fig10_2_9                                     ///
    fig10_3    fig10_6    fig10_7_3  fig10_3_2  fig10_4_1                 ///
    fig10_4    fig10_5_1  fig10_8_1  fig10_8_2  fig10_9_1 {
    capture graph save "$graphs_dir/`gname'.gph", replace
}

di as text _n "========================================================"
di as text "All graphs saved as .gph to:"
di as text "  $graphs_dir"
di as text _n "Reopen any graph after closing Stata with:"
di as text `"  graph use "$graphs_dir/graphname.gph""'
di as text _n "All graph names:"
di as text "  RDD:         fig10_2_1 through fig10_2_9"
di as text "  Georgia DiD: fig10_3 fig10_6 fig10_3_2 fig10_4_1"
di as text "               fig10_4    fig10_5_1 fig10_8_1 fig10_8_2 fig10_9_1"
di as text "  ETWFE:       fig10_7_3"
di as text "========================================================"
