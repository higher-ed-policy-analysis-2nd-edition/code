*========================================================================
* TrendGraphs13.do
* Section 13.4: Trend Graphs and Simple Comparisons
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
    log using "$logdir/TrendGraphs13_output.log", replace text
    di "TrendGraphs13.do log opened: " c(current_date) " " c(current_time)
    di "Log location: $logdir/TrendGraphs13_output.log"
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
* R equivalent (ggplot2 + theme_springer()) belongs in R_code13.R,
* translating each example below for direct comparison.

use "$data_dir/Example_13_1.dta", clear

*------------------------------------------------------------------------
* Figure 13.3: State Appropriations per Population, FY 1980-2018
* (Maryland vs. all other states)
*
* Presentation Enhancements: bw (grayscale-safe line patterns) and a
* 12-o'clock legend position keep this print-ready without color, per
* Springer's B&W requirement noted throughout the book's other chapters.
*------------------------------------------------------------------------
lgraph y_pop fy, nom by(MD) xlabel(1980(3)2018) bw ///
    title("State Appropriations Per Population" "FY 1980-2018", $TITLESIZE) ///
    ytitle(Dollars) legend(pos(12) col(2)) name(fig13_3_md_v_nation, replace)

pubexport fig13_3_md_v_nation

*------------------------------------------------------------------------
* Figure 13.4: State Appropriations per FTE, Maryland vs. All Other
* SREB States, FY 1980-2018
*------------------------------------------------------------------------
* NOTE: Example_13_1.dta already contains MDSREB (with label MDSREB1) --
* this collided with "gen MDSREB = 0 if region_compact==1" the same way
* MD/decade did in DescriptiveTables13.do. Made idempotent below.
capture confirm variable MDSREB
if _rc {
    gen MDSREB = 0 if region_compact == 1
    replace MDSREB = 1 if fips == 24
}
capture label list MDSREB1
if _rc {
    label define MDSREB1 0 "All Other SREB States" 1 "Maryland"
}
label values MDSREB MDSREB1

lgraph yfte fy if region_compact == 1, nom by(MDSREB) xlabel(1980(2)2018, ///
    labsize(vsmall)) bw title("State Appropriations Per FTE" "FY 1980-2018", $TITLESIZE) ///
    ytitle(Dollars) legend(pos(12) col(2)) name(fig13_4_md_v_sreb, replace)

pubexport fig13_4_md_v_sreb

*------------------------------------------------------------------------
* Figure 13.5: Colorado Net Tuition Revenue per FTE Before and After
* Senate Bill 189, vs. All Other States
*------------------------------------------------------------------------
use "$data_dir/Example_13_3.dta", clear

* NOTE: Example_13_3.dta already contains netuit_fte (and likely T, fy)
* pre-built, matching the same pattern seen in Example_13_1.dta (MD,
* decade) and Example_13_1.dta's MDSREB. Made idempotent below.
capture confirm variable netuit_fte
if _rc {
    gen netuit_fte = netuit / fte
}
capture confirm variable T
if _rc {
    gen T = 0
    replace T = 1 if state == "CO"
}
capture confirm variable fy
if _rc {
    gen fy = year
}

global y "netuit_fte"
* NOTE -- BUG FIX: "fy & fy > 1999" was missing the "if" keyword --
* lgraph parsed "&" as part of the variable list ("& invalid name"),
* since a condition needs "if" before it, not a bare "&" after the
* varlist. Compare the correctly-formed "if region_compact==2 & fy>1999"
* a few lines below, which already has "if" and uses "&" properly inside
* the expression.
lgraph $y fy if fy > 1999, by(T) stat(mean) xline(2005) xlabel(2000(2)2016, ///
    labsize(small)) ylab(, nogrid) scheme(s2mono) bw ///
    title("Colorado's Net Tuition Revenue Per FTE" ///
    "Before and After Colorado Senate Bill 189", $TITLESIZE) ytitle(Dollars) ///
    legend(pos(12) col(2)) name(fig13_5_co_sb189_v_all, replace)

pubexport fig13_5_co_sb189_v_all

*------------------------------------------------------------------------
* Figure 13.6: Colorado Net Tuition Revenue per FTE Before and After
* Senate Bill 189, vs. All Other WICHE States
*------------------------------------------------------------------------
* NOTE: COWICHE was not seen in the Example_13_1.dta variable list from
* the earlier describe output, but made idempotent here too for
* consistency with the MDSREB fix above and safety on re-runs.
*
* NOTE -- BUG FIX: "replace COWICHE = 1 if fips == 8" threw "fips not
* found" -- Example_13_3.dta (loaded above for Figures 13.5/13.6) does
* not have a fips variable, only "state" (str2 abbreviation, already
* used successfully a few lines above for the T/Colorado indicator).
* Switched to the same "state" variable for consistency within this file.
capture confirm variable COWICHE
if _rc {
    gen COWICHE = 0 if region_compact == 2
    replace COWICHE = 1 if state == "CO"
}
capture label list COWICHE1
if _rc {
    label define COWICHE1 0 "All Other WICHE States" 1 "Colorado"
}
label values COWICHE COWICHE1

global y "netuit_fte"
lgraph $y fy if region_compact == 2 & fy > 1999, nom by(COWICHE) ///
    stat(mean) xline(2005) xlabel(2000(2)2016, labsize(small)) ///
    ylab(, nogrid) scheme(s2mono) bw ///
    title("Colorado's Net Tuition Revenue Per FTE" ///
    "Before and After Colorado Senate Bill 189", $TITLESIZE) ytitle(Dollars) ///
    legend(pos(12) col(2)) name(fig13_6_co_sb189_v_wiche, replace)

pubexport fig13_6_co_sb189_v_wiche

*------------------------------------------------------------------------
* Close the standalone log opened above, if this script opened one.
* When called from Stata_code13.do, this is skipped and the master log
* remains open for the rest of the chapter's sub-scripts.
*------------------------------------------------------------------------
if $standalone_log_open == 1 {
    log close
}
