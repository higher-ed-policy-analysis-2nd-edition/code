*========================================================================
* MTE_CATE_Plots13.do
* Section 13.7: Presenting IV, CATE, and MTE Results
*
* Called by:  Stata_code13.do
* Inherits:   $graphs_dir, $tables_dir, $logdir, $data_dir, $syntax_dir
*             globals from master (re-derived below if run standalone)
*
* Code development assisted by Claude (Anthropic). The author provided
* specifications and reviewed, tested, and validated all code.
*========================================================================

capture confirm global root_dir
if _rc {
    if c(username) == "marvi" {
        global root_dir "C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 13"
    }
    else {
        global root_dir "`c(pwd)'"
    }
    global graphs_dir "$root_dir/Output/graphs"
    global tables_dir "$root_dir/Output/tables"
    global logdir     "$root_dir/Output/logs"
    global syntax_dir "$root_dir/Syntax/Stata"
    global data_dir   "$root_dir/Data/Stata"
    capture mkdir "$graphs_dir"
    capture mkdir "$tables_dir"
    capture mkdir "$logdir"
}

* Open a dedicated log for this sub-script UNLESS a log is already open
* (i.e., the master's, if called via Stata_code13.do). Checked via
* "log query" -- Stata's own live status check -- rather than a custom
* global flag. A custom flag (whether $root_dir or $_master_run) can
* persist in memory from an earlier session run long after the relevant
* log has actually closed, causing this block to be skipped incorrectly.
* "log query" asks Stata directly, so it can never go stale.
global standalone_log_open 0
quietly log query
if "`r(status)'" != "on" {
    global standalone_log_open 1
    log using "$logdir/MTE_CATE_Plots13_output.log", replace text
    di "MTE_CATE_Plots13.do log opened: " c(current_date) " " c(current_time)
    di "Log location: $logdir/MTE_CATE_Plots13_output.log"
}

* NOTE -- ENTIRE FILE IS NEW. No precedent exists in the 1st-ed. Chapter
* 10 script. All sections below are stubs pending the finalized Ch. 11/12
* model objects -- fill in with the actual estimation results once
* Stata_code11.do / Stata_code12.do are finalized.

*========================================================================
* 13.7.1 IV/2SLS: First-Stage Strength and Reduced-Form Visuals
*========================================================================
* NOTE -- NEW CODE NEEDED. Pull from Ch. 11 Stata_code11.do
* (ivregress 2sls model on the B&B synthetic data). Add a first-stage
* F-statistic display alongside a simple coefplot of the reduced-form
* estimate -- the point of this figure is to show instrument strength is
* not being asserted, it is being shown.
*
*   * ivregress 2sls completion (treatment_var = state_ga_funding) $controls
*   * estat firststage
*   * coefplot, keep(treatment_var) name(fig13_14_iv_firststage, replace)
*   * graph export "$graphs_dir/fig13_14_iv_firststage_Stata.png", replace

*========================================================================
* 13.7.2 CATE by Subgroup (margins-based)
*========================================================================
* NOTE -- NEW CODE NEEDED. Pull from Ch. 11 CATE.do. Forest-plot style
* coefplot of subgroup treatment effects, reusing the master's-completion
* CATE subgroup results already estimated there.
*
*   * coefplot subgroup1 subgroup2 subgroup3 ..., ///
*   *    name(fig13_15_cate_forest, replace)
*   * graph export "$graphs_dir/fig13_15_cate_forest_Stata.png", replace

*========================================================================
* 13.7.2a Official cate/categraph Workflow (Stata 19)
*========================================================================
* NOTE -- NEW CODE NEEDED. Apply to the same B&B example as 13.7.2 for
* direct comparison. Sketch:
*
*   * cate requires Stata 19
*   global catecovars "[covariate list from Ch. 11 B&B model]"
*   cate po (completion $catecovars) (treatment_var)
*   categraph histogram
*   graph export "$graphs_dir/fig13_16_cate_histogram_Stata.png", replace
*   estat heterogeneity
*   estat gatetest
*   * dose-response over a continuous moderator, if applicable:
*   estat series [continuous_var]
*
* Confirm which nuisance-model options (lasso vs. random forest) best
* match the assumptions already documented for the Ch. 11 MTE model, so
* the two CATE approaches are presented as complementary rather than
* contradictory.

*========================================================================
* 13.7.3 MTE and MPRTE Curves
*========================================================================
* NOTE -- NEW CODE NEEDED. Pull from Ch. 11 MTE_MPRTE.do (mtefe output).
* Plot the MTE curve over the unobserved resistance-to-treatment margin
* (uniform [0,1] support). Confirm whether mtefe has a native graphing
* postestimation command or whether this needs a manual twoway build from
* exported MTE point estimates + CIs across the support.
*
*   * twoway (rarea mte_lo mte_hi u_support, color(gs12)) ///
*   *        (line mte_hat u_support, lcolor(black)), ///
*   *    xtitle("Unobserved Resistance to Treatment (u)") ///
*   *    ytitle("Marginal Treatment Effect") ///
*   *    name(fig13_17_mte_curve, replace)
*   * graph export "$graphs_dir/fig13_17_mte_curve_Stata.png", replace

*========================================================================
* 13.7.4 Policy-Relevant Treatment Effect (PRTE) Summary Table
*========================================================================
* NOTE -- table content lives in EstimationTables13.do (etable/collect
* path); this file supplies the underlying PRTE point estimates from the
* Ch. 12 Grad PLUS cap application that EstimationTables13.do formats.

*------------------------------------------------------------------------
* Close the standalone log opened above, if this script opened one.
* When called from Stata_code13.do, this is skipped and the master log
* remains open for the rest of the chapter's sub-scripts.
*------------------------------------------------------------------------
if $standalone_log_open == 1 {
    log close
}
