*========================================================================
* EstimationTables13.do
* Section 13.2.2: Tables of Estimation Results
* Section 13.7.4: Policy-Relevant Treatment Effect (PRTE) Summary Table
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
    log using "$logdir/EstimationTables13_output.log", replace text
    di "EstimationTables13.do log opened: " c(current_date) " " c(current_time)
    di "Log location: $logdir/EstimationTables13_output.log"
}

*------------------------------------------------------------------------
* 13.2.2 Tables of Estimation Results -- etable/collect (Stata 17+)
*------------------------------------------------------------------------
* NOTE -- NEW CODE NEEDED: standalone example introducing etable as the
* modern alternative to esttab/estout, using the Ch. 13.2 descriptive
* data already in memory from DescriptiveTables13.do.
*
*   reg y_pop x1fte x3_pop x4_pop x5_pop
*   etable, title("Table 13.3 Regression Results") ///
*      export("$tables_dir/Table13_3_etable.docx", replace)

*------------------------------------------------------------------------
* Marginal effects Word tables -- legacy path (esttab/estout)
*------------------------------------------------------------------------
* DATA PROVENANCE: Example_13_4.dta is the same file as Chapter 10's
* "Example 10.dta" (net tuition/administrative staffing panel),
* confirmed in the data repository's own README -- a DIFFERENT Chapter
* 10 dataset than Example_13_1.dta, which feeds Tables 13.1/13.2,
* Maps13.do's Figure 13.1, and RegressionPlots13.do's Figures 13.7-13.9.
* This same Example_13_4.dta file also feeds RegressionPlots13.do's
* Figures 13.10-13.11 (Sections 13.5.2/13.5.3).
*------------------------------------------------------------------------
use "$data_dir/Example_13_4.dta", clear

global y  "lnadminstaff"
global x1 "lnnet_tuition_rev_adj"
global x2 "lnstate_appro_adj"
global x3 "lnfedrev_r"
global x4 "lnFTE_enroll"

qui xtscc $y L1.$x1 L1.$x2 L1.$x3 L1.$x4
qui margins, eyex(*) at((p25) _all) cont post
eststo marginalp25

qui xtscc $y L1.$x1 L1.$x2 L1.$x3 L1.$x4
qui margins, eyex(*) at((p50) _all) cont post
eststo marginalmed

qui xtscc $y L1.$x1 L1.$x2 L1.$x3 L1.$x4
qui margins, eyex(*) at((p75) _all) cont post
eststo marginalp75

esttab marginalp25 marginalmed marginalp75 using "$tables_dir/Table13_Appendix_esttab", ///
    label se(3) ///
    title("Percent Change in Administrators" "Due to a One Percent Change" ///
    "in Net Tuition Revenue, Controlling for Other Factors" ///
    "(State Appropriations, Federal Revenue, and FTE Enrollment)") ///
    mtitle("25th Percentile" "Median" "75th Percentile") ///
    nonumbers rtf replace

*------------------------------------------------------------------------
* Marginal effects Word table -- modern path (etable/collect)
*------------------------------------------------------------------------
* NOTE -- NEW CODE NEEDED: confirm collect's syntax for combining three
* separately-estimated margins calls into one table -- likely needs
* collect get / collect combine rather than a single collect: prefix call.
*
*   collect clear
*   collect: margins, eyex(*) at((p25) _all)
*   * ... repeat at p50, p75, tagging each collection ...
*   collect layout (colname) (result)
*   collect export "$tables_dir/Table13_Appendix_etable.docx", replace

*------------------------------------------------------------------------
* 13.7.4 Policy-Relevant Treatment Effect (PRTE) Summary Table
*------------------------------------------------------------------------
* NOTE -- NEW CODE NEEDED: ties to the Ch. 12 Grad PLUS cap application.
* Use etable/collect (per the modernized workflow above) rather than
* esttab, to keep the chapter's table-building approach consistent.
* Depends on PRTE point estimates produced in MTE_CATE_Plots13.do.

*------------------------------------------------------------------------
* Close the standalone log opened above, if this script opened one.
* When called from Stata_code13.do, this is skipped and the master log
* remains open for the rest of the chapter's sub-scripts.
*------------------------------------------------------------------------
if $standalone_log_open == 1 {
    log close
}
