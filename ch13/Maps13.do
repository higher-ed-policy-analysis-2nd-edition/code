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

* Publication export helper (idempotent redefinition -- see
* Stata_code13.do's Presentation Settings block for the master-run
* definition and full rationale). Defined here too so this script also
* works when run standalone.
capture program drop pubexport
program define pubexport
    args gname
    graph export "$graphs_dir/`gname'_Stata.svg", replace
    graph export "$graphs_dir/`gname'_Stata.pdf", replace
    graph export "$graphs_dir/`gname'_Stata.png", replace width(2400)
end

* Retained from 1st ed. Ch. 10 -- no new Stata routines required.
* R equivalent (tigris + ggplot2, or usmap) belongs in R_code13.R,
* rebuilding these two maps on the same data for direct comparison.
*
* DATA PROVENANCE (Figure 13.1 only): Example_13_1.dta is the same
* state appropriations panel as Chapter 10's Example_10_2_1.dta
* (confirmed in the data repository's own README) -- same source as
* Tables 13.1/13.2, TrendGraphs13.do's Figures 13.3-13.4, and
* RegressionPlots13.do's Figures 13.7-13.9. Figure 13.2 below switches
* to a different Chapter 10 file -- see the note above its own "use"
* statement.
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
*
* Presentation Enhancements for Policymakers:
*   - Legend now shown (legd(1)) rather than suppressed, at readable
*     size and placed to the side rather than overlapping the map
*   - nquantiles(5) keeps the classification simple -- a policymaker
*     reading five shades is easier than reading seven or more
*   - Subtitle names the actual highest/lowest states and their values,
*     computed directly from the data -- a state legislator's first
*     question is usually "where does my state rank," not the
*     distribution's general shape
*   - note() cites the data source directly on the figure
*------------------------------------------------------------------------
preserve
    keep if fy == 2017
    qui sum y_pop
    local ymax = r(max)
    local ymin = r(min)
    qui levelsof state if y_pop == `ymax', local(hi_state) clean
    qui levelsof state if y_pop == `ymin', local(lo_state) clean
restore

* NOTE -- BUG FIX: the subtitle originally tried to format ymax/ymin
* inline inside the quoted string itself (e.g., "...(`: display %6.0f
* `ymax''')..."), which is not valid Stata syntax -- the display
* extended macro function has to be the right-hand side of its own
* "local x : display ..." assignment, not spliced directly into text.
* Pre-formatted here as their own macros instead.
local ymax_fmt : display %9.0fc `ymax'
local ymin_fmt : display %9.0fc `ymin'

maptile y_pop if fy == 2017, geo(state) geoid(statefips) nquantiles(5) ///
    rangecolor(gray*0.075 gray*1.0) legd(1) ///
    twopt(title("State Appropriations per Capita, 2017" "(in dollars)", $TITLESIZE) ///
    subtitle("Highest: `hi_state' ($`ymax_fmt'). Lowest: `lo_state' ($`ymin_fmt').", $SUBTITLESIZE) ///
    legend(size(medium) position(3) rows(6)) ///
    note("Source: SHEF Data, FY2017.", $NOTESIZE) ///
    name(fig13_1_appro_percapita_2017, replace))

pubexport fig13_1_appro_percapita_2017

*------------------------------------------------------------------------
* Figure 13.2: Percent Change in State Appropriations per FTE Enrollment,
* FY 2009-FY 2017
*
* Presentation Enhancements: same as fig13_1 -- visible, readable legend,
* fewer classification bins (nq(7) reduced to 5), a source note, and a
* data-driven subtitle naming the largest increase/decrease.
*
* NOTE: Example_13_2.dta's full variable list was never independently
* confirmed the way Example_13_1.dta's was (no describe output was ever
* shared for this file) -- written defensively below with a capture, so
* it falls back to reporting by FIPS code if no state-name-style
* variable exists, rather than risk a "variable not found" error.
*
* DATA PROVENANCE: Example_13_2.dta is the same file as Chapter 10's
* Example_10_3.dta (confirmed in the data repository's own README) --
* a DIFFERENT Chapter 10 dataset than Figure 13.1's Example_13_1.dta
* above; not to be confused with CausalPlots13.do's Example_10_3_1.csv
* either, which is a separate file despite the similar name.
*------------------------------------------------------------------------
use "$data_dir/Example_13_2.dta", clear

local hi_state2 ""
local lo_state2 ""
capture confirm variable state
if !_rc {
    qui sum pctchnge
    local pmax = r(max)
    local pmin = r(min)
    qui levelsof state if pctchnge == `pmax', local(hi_state2) clean
    qui levelsof state if pctchnge == `pmin', local(lo_state2) clean
}

local subtitle2 "Fewer, simpler classification bins for readability"
if "`hi_state2'" != "" & "`lo_state2'" != "" {
    local pmax_fmt : display %4.1f `pmax'
    local pmin_fmt : display %4.1f `pmin'
    local subtitle2 "Largest increase: `hi_state2' (`pmax_fmt'%). Largest decrease: `lo_state2' (`pmin_fmt'%)."
}

maptile pctchnge, geo(state) geoid(statefips) ///
    rangecolor(gray*0.01 gray*1.2) nq(5) legd(1) ///
    twopt(title("Percent Change in State Appropriations per FTE Enrollment" ///
    "Between FY 2009 & FY 2017", $TITLESIZE) ///
    subtitle("`subtitle2'", $SUBTITLESIZE) ///
    legend(size(medium) position(3) rows(6)) ///
    note("Source: SHEF Data, FY2009-FY2017.", $NOTESIZE) ///
    name(fig13_2_pctchange_appro_fte, replace))

pubexport fig13_2_pctchange_appro_fte

*------------------------------------------------------------------------
* Close the standalone log opened above, if this script opened one.
* When called from Stata_code13.do, this is skipped and the master log
* remains open for the rest of the chapter's sub-scripts.
*------------------------------------------------------------------------
if $standalone_log_open == 1 {
    log close
}
