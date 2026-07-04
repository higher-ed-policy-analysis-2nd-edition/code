*========================================================================
* Maps13.do
* Section 13.3: Choropleth Maps
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
    log using "$logdir/Maps13_output.log", replace text
    di "Maps13.do log opened: " c(current_date) " " c(current_time)
    di "Log location: $logdir/Maps13_output.log"
}

* Retained from 1st ed. Ch. 10 -- no new Stata routines required.
* R equivalent (tigris + ggplot2, or usmap) belongs in R_code13.R,
* rebuilding these two maps on the same data for direct comparison.
*
* NOTE -- BUG FIX: this section previously had no "use" statement of its
* own and silently relied on whichever dataset happened to still be in
* memory from the previous sub-script in the master's call sequence.
* That broke the moment EstimationTables13.do's last "use" (Example_
* 13_4.dta) ran immediately before Maps13.do -- "x2 not found," since x2
* only exists in Example_13_1.dta. Loading it explicitly here removes
* the implicit dependency on execution order.
use "$data_dir/Example_13_1.dta", clear

* NOTE -- BUG FIX (final): this had gone through three failed attempts
* using statastates (state_name collision, then name() matching zero
* observations, then a further collision on state_abbrev). The root
* cause each time is the same: Example_13_1.dta already ships with
* fips, state_abbrev, and state_name as complete, pre-built columns --
* statastates was never actually needed. Using the existing fips
* variable directly sidesteps the whole matching problem.
gen statefips = fips
gen statename = state
gen x2_1000 = x2/1000

*------------------------------------------------------------------------
* Figure 13.1: State Appropriations per Capita by State, FY 2017
*------------------------------------------------------------------------
maptile y_pop if fy == 2017, geo(state) geoid(statefips) nquantiles(5) ///
    rangecolor(gray*0.075 gray*1.0) legd(0) ///
    twopt(title("State Appropriations per Capita, 2017" "(in dollars)") ///
    name(fig13_1_appro_percapita_2017, replace))

graph export "$graphs_dir/fig13_1_appro_percapita_2017_Stata.png", replace

*------------------------------------------------------------------------
* Figure 13.2: Percent Change in State Appropriations per FTE Enrollment,
* FY 2009-FY 2017
*------------------------------------------------------------------------
use "$data_dir/Example_13_2.dta", clear

maptile pctchnge, geo(state) geoid(statefips) ///
    rangecolor(gray*0.01 gray*1.2) nq(7) legd(0) ///
    twopt(title("Percent Change in State Appropriations per FTE Enrollment" ///
    "Between FY 2009 & FY 2017") name(fig13_2_pctchange_appro_fte, replace))

graph export "$graphs_dir/fig13_2_pctchange_appro_fte_Stata.png", replace

*------------------------------------------------------------------------
* Close the standalone log opened above, if this script opened one.
* When called from Stata_code13.do, this is skipped and the master log
* remains open for the rest of the chapter's sub-scripts.
*------------------------------------------------------------------------
if $standalone_log_open == 1 {
    log close
}
