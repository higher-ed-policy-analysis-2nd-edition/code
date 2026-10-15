*========================================================================
* Chapter 10 - Causal Inference
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative Techniques
* (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch10
* Author: Marvin A. Titus
* Date: November 2025 (revised September 2026)
* NOTE: Code development was assisted by Claude (Anthropic). The author
* provided specifications and reviewed, tested, and validated all code.
*========================================================================
* Script tested in Stata 19.5
* Compatible with Stata version 19 or later
*
* BEFORE YOU RUN
*   1. Working directory.  Run this file from the ch10 folder of the
*      repository.  Paths are relative to that folder: sub-scripts are read
*      from Syntax/Stata/, output is written to Output/ (graphs, tables,
*      logs), and the data files are downloaded to the working directory.
*      Set it first, e.g.   cd "C:/.../code/ch10"
*   2. User-written packages.  The PACKAGE INSTALLATIONS block installs:
*        RDD (10.2):     rdrobust, rddensity, lpdensity, cmogram
*        DiD (10.3-10.9): ftools, reghdfe, lassopack, synth, sdid, csdid,
*                        drdid, jwdid, eventstudyinteract, estout
*      The block reinstalls with -replace- on every run.  After the first
*      successful run you can comment it out.
*   3. Install order.  ftools must be installed before reghdfe, because
*      reghdfe depends on the Factor class that ftools provides.  Installing
*      them in the reverse order produces r(3000) when reghdfe is first
*      called.  The package block below already installs them in this order.
*
* Sub-scripts (all in $syntax_dir = Syntax/Stata/):
*   RDD.do          Section 10.2   — Sharp/Fuzzy RD, merit scholarship
*   Georgia_DiD.do  Sections 10.3–10.9 (except 10.7.4) — DiD, SCM, SDID, D-R DiD
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
*   The working directory must be the ch10 folder of the repository, i.e.
*   the folder that contains Syntax/Stata/.  See BEFORE YOU RUN, above.
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

* ------------------------------------------------------------------------
* STEP 0.  Clear stale path globals left by an earlier run in this session.
* ------------------------------------------------------------------------
* Stata globals survive across do-files within a session.  A relative value
* left behind by a standalone run of a sub-script used to override the paths
* set below and silently redirect output to the working directory -- this is
* what sent tab10_7_etwfe.rtf astray on 13-14 Aug 2026.  Dropping them here
* makes the master authoritative every time it runs.
foreach M in out_root graphs_dir tables_dir log_dir logdir syntax_dir {
    capture macro drop `M'
}

* Close any stale log silently, then open a fresh one
capture log close

* ------------------------------------------------------------------------
* STEP 1.  One root; every output directory derives from it.
* ------------------------------------------------------------------------
if c(username) == "marvi" {
    global ch10_root  "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10"
    global out_root   "$ch10_root/Output"
    global syntax_dir "$ch10_root/Syntax/Stata"
}
else {
    global out_root   "Output"
    global syntax_dir "Syntax/Stata"
}

global graphs_dir "$out_root/graphs"
global tables_dir "$out_root/tables"
global log_dir    "$out_root/logs"

* $logdir (no underscore) is retained as an alias: earlier versions of this
* file used that name, and anything still reading it keeps working.  New code
* should use $log_dir, which is what the sub-scripts expect.
global logdir     "$log_dir"

capture mkdir "$out_root"
foreach D in "$graphs_dir" "$tables_dir" "$log_dir" {
    capture mkdir "`D'"
}

log using "$log_dir/Chapter10_Stata_output.log", replace text

* ------------------------------------------------------------------------
* STEP 2.  Echo the resolved paths INTO the log, so any later confusion about
*          where output went can be settled by reading this block.
* ------------------------------------------------------------------------
di as text "Chapter 10 log opened: " c(current_date) " " c(current_time)
di as text "  working directory : " c(pwd)
di as text "  out_root          : $out_root"
di as text "  graphs_dir        : $graphs_dir"
di as text "  tables_dir        : $tables_dir"
di as text "  log_dir           : $log_dir"
di as text "  syntax_dir        : $syntax_dir"

* ------------------------------------------------------------------------
* STEP 3.  Confirm every sub-script exists before running anything, so a
*          missing or misplaced file fails loudly here rather than halfway
*          through the chapter.
* ------------------------------------------------------------------------
* -exit- does NOT halt a do-file run with the -nostop- option, which is how
* this master is usually launched: on 14 Aug 2026 this block correctly
* reported Georgia_DiD.do missing, then execution carried on and printed
* "All three sub-scripts found" two lines later.  So the check now records a
* flag per script, and each -do- call below is guarded by that flag instead
* of relying on -exit- to stop anything.
global ch10_missing 0
foreach S in RDD.do Georgia_DiD.do ETWFE.do {
    capture confirm file "$syntax_dir/`S'"
    if _rc {
        di as error "MISSING: $syntax_dir/`S'"
        global ch10_missing = $ch10_missing + 1
    }
}
if $ch10_missing > 0 {
    di as error "$ch10_missing sub-script(s) missing from $syntax_dir."
    di as error "Copy them there, then re-run.  Missing scripts are SKIPPED below."
}
else {
    di as text "  All three sub-scripts found in $syntax_dir"
}

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
* rddensity: installed from rdpackages, NOT SSC.  The SSC copy does not
* reliably deliver the ancillary file rddensity_fun.do, which rddensity
* runs internally -- its absence aborted RDD.do with r(601) on 14 Aug 2026
* ("file rddensity_fun.do not found") even though rddensity.ado was present.
net install rddensity, ///
    from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) ///
    replace
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

* -which- only confirms the .ado file.  rddensity also needs the ancillary
* rddensity_fun.do, which a partial install can leave behind -- that gap is
* what produced the r(601) abort on 14 Aug 2026.  -findfile- searches the
* adopath and catches it here rather than mid-run.
* NOTE: -findfile- does NOT locate rddensity_fun.do even when rddensity is
* working correctly (confirmed 14 Aug 2026: RDD.do completed cleanly while
* this check still reported the file missing).  The file lives inside the
* package directory rather than on the searched adopath, so this is a
* WARNING only -- never an exit.  If rddensity itself fails with r(601),
* reinstall from rdpackages using the command printed below.
capture findfile rddensity_fun.do
if _rc != 0 {
    di as text "NOTE: rddensity_fun.do not on the adopath (usually harmless)."
    di as text "      If rddensity errors with r(601), reinstall with:"
    di as text "      net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace"
}
di as text "RDD packages confirmed."

* DiD / causal inference packages (Sections 10.3-10.9)
* ftools MUST precede reghdfe: reghdfe uses the Factor class that ftools
* provides, and the reverse order produces r(3000) at the first reghdfe call.
ssc install ftools,    replace
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
*   Produces: Example_10_2_1.dta, fig10_1 - fig10_4 plus diagnostics
*========================================================================

capture confirm file "$syntax_dir/RDD.do"
if _rc {
    di as error "SKIPPED: $syntax_dir/RDD.do not found"
}
else {
    do "$syntax_dir/RDD.do"
}

*------------------------------------------------------------------------
* Display RDD figures in the Stata graph window
*------------------------------------------------------------------------
* RDD.do sets name() on only two graphs; the rest are exported straight to
* PNG without a name, so -graph display- cannot recall them. The long list
* that used to sit here referenced names that never existed, and every call
* failed silently under -capture-.
capture graph display fig10_2      // density continuity test (Fig. 10.2)
capture graph display fig10_4      // fuzzy RDD LATE (Fig. 10.4)
capture graph save fig10_2 "$graphs_dir/fig10_2.gph", replace
capture graph save fig10_4 "$graphs_dir/fig10_4.gph", replace
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
*             fig10_5 - fig10_9 plus diagnostics
*========================================================================

capture confirm file "$syntax_dir/Georgia_DiD.do"
if _rc {
    di as error "SKIPPED: $syntax_dir/Georgia_DiD.do not found"
}
else {
    do "$syntax_dir/Georgia_DiD.do"
}

*------------------------------------------------------------------------
* Display Georgia DiD figures in the Stata graph window
* All Georgia DiD figures already use name() and are fully recallable.
*------------------------------------------------------------------------
capture graph display fig10_5      // parallel trends        (Fig. 10.5)
capture graph display fig10_6      // SCM actual vs synthetic (Fig. 10.6)
capture graph display fig10_8      // event study             (Fig. 10.8)
capture graph display fig10_9      // staggered adoption      (Fig. 10.9)
capture graph display fig10_3_2    // robustness checks       (diagnostic)
capture graph display fig10_4_1    // LASSO DiD comparison    (diagnostic)
capture graph display fig10_5_1    // SCM gap plot            (diagnostic)
capture graph display fig10_7_2    // D-R DiD event study      (diagnostic)
capture graph display fig10_8_1    // permutation             (diagnostic)
capture graph display fig10_8_2    // leave-one-out           (diagnostic)
capture graph display fig10_9_1    // estimator comparison    (diagnostic)

*========================================================================
* SECTION 10.7.4: EXTENDED TWO-WAY FIXED EFFECTS (external script)
*   Wooldridge (2021, 2023) ETWFE estimator via the jwdid command, applied
*   to the three-state staggered adoption design (48-state panel).
*   Unconditional (no covariates) vs covariate-adjusted (hettype(cohort))
*   specifications, both using never-treated states as controls.
*   Script:   $syntax_dir/ETWFE.do
*   Inherits: $graphs_dir, $tables_dir, log, set scheme s2mono
*   Data:     Example_10_7_3.csv (downloaded)
*   Produces: tab10_7_etwfe.rtf, fig10_7_4
*------------------------------------------------------------------------
* NOTE: ETWFE and Callaway-Sant'Anna target the same ATT(g,t) estimands;
* the unconditional ETWFE overall ATT is the figure directly comparable to
* the csdid staggered result from Georgia_DiD.do. The covariate-adjusted
* run illustrates the result's sensitivity to differential pre-trends.
*========================================================================

capture confirm file "$syntax_dir/ETWFE.do"
if _rc {
    di as error "SKIPPED: $syntax_dir/ETWFE.do not found"
}
else {
    do "$syntax_dir/ETWFE.do"
}

*------------------------------------------------------------------------
* Display ETWFE figure in the Stata graph window
*------------------------------------------------------------------------
capture graph display fig10_7_4    // ETWFE event study (diagnostic, no manuscript number)

*========================================================================
* SAVE ALL NAMED GRAPHS AS .GPH
* Sub-scripts save .gph files alongside each export; this block is a
* final safety net.  It must run BEFORE -clear all-, which clears graphs
* from memory (every save below would otherwise fail silently under
* -capture-), and before -log close-, so its output is logged.
* Reload after Stata closes:
*   graph use "$graphs_dir/graphname.gph"
*========================================================================
di _n as text "Saving all named graphs as .gph files..."

* Only names that the sub-scripts actually set can be saved here.
foreach gname in                                                        ///
    fig10_2    fig10_4                                                  ///
    fig10_5    fig10_6    fig10_8    fig10_9                            ///
    fig10_3_2  fig10_4_1  fig10_5_1  fig10_7_2                          ///
    fig10_8_1  fig10_8_2  fig10_9_1                                     ///
    fig10_7_4 {
    capture graph save "$graphs_dir/`gname'.gph", replace
}

di as text _n "========================================================"
di as text "All graphs saved as .gph to:"
di as text "  $graphs_dir"
di as text _n "Reopen any graph after closing Stata with:"
di as text `"  graph use "$graphs_dir/graphname.gph""'
di as text _n "All graph names:"
di as text "  Manuscript:  fig10_2 fig10_4 (RDD)"
di as text "               fig10_5 fig10_6 fig10_8 fig10_9 (Georgia DiD)"
di as text "  Diagnostics: fig10_3_2 fig10_4_1 fig10_5_1 fig10_7_2"
di as text "               fig10_8_1 fig10_8_2 fig10_9_1"
di as text "  ETWFE:       fig10_7_4"
di as text "========================================================"

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
   10.2.10  Summary table; save Example_10_2_1.dta

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
            controls; comparison table tab10_7_etwfe and figure fig10_7_4

   10.8   Sensitivity Analysis
   10.8.2   Permutation inference
   10.8.3   Leave-one-out sensitivity analysis

   10.9   Results Summary

   Data files saved:
     Example_10_2_1.dta   (Section 10.2 RDD synthetic data)
*/
