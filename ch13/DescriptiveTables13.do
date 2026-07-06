*========================================================================
* DescriptiveTables13.do
* Section 13.2: Presenting Descriptive Statistics
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
    log using "$logdir/DescriptiveTables13_output.log", replace text
    di "DescriptiveTables13.do log opened: " c(current_date) " " c(current_time)
    di "Log location: $logdir/DescriptiveTables13_output.log"
}

*------------------------------------------------------------------------
* 13.2.1 Descriptive Statistics in Microsoft Word Tables
*------------------------------------------------------------------------
* Primary path: dtable (Stata 18+). asdoc/tabstat retained as a
* version-gated fallback for Stata <18 below.
*------------------------------------------------------------------------
use "$data_dir/Example_13_1.dta", clear

sum y x1 x2 x3 x4 x5

rescale y, millions
rescale x1, millions
rescale x3, millions
rescale x4, millions
rescale x5, millions

sum y x1 x2 x3 x4 x5

*========================================================================
* Derived per-capita / per-FTE variables
* Confirmed via "gen check_ypop = y/x5; gen diff = check_ypop - y_pop;
* sum diff" -- diff ~ 0 (floating-point rounding only), so y_pop = y/x5
* (x5 = Population, x2 = Net FTE). y_pop already exists in the data;
* the remaining four are generated below on the same pattern.
*
* x5_pop is Population itself under the _pop naming convention (not a
* ratio, since x5/x5 = 1 would be meaningless) -- included as a row in
* Table 13.1 alongside the other _pop/fte variables for reference.
*========================================================================
capture confirm variable x1fte
if _rc {
    gen x1fte  = x1 / x2
}
capture confirm variable x3_pop
if _rc {
    gen x3_pop = x3 / x5
}
capture confirm variable x4_pop
if _rc {
    gen x4_pop = x4 / x5
}
capture confirm variable x5_pop
if _rc {
    gen x5_pop = x5
}

tabstat y_pop x1fte x3_pop x4_pop x5_pop, statistics(mean median) ///
    column(statistics) format(%9.0fc)

*------------------------------------------------------------------------
* Modern path (Stata 18+): dtable -- official StataCorp command,
* purpose-built for descriptive/"Table 1" style tables. Replaces the
* asdoc/tabstat workaround with no third-party dependency.
*
* Presentation Enhancements: dtable builds and exports its own
* collection internally, so collect style commands need to be set
* BEFORE calling dtable (not after) for them to take effect on its
* export -- this refines number formatting and hides redundant header
* rows without touching the underlying statistical content at all.
*------------------------------------------------------------------------
if c(stata_version) >= 18 {
    collect style cell result, nformat(%9.2f)
    collect style header result, level(hide)
    * NOTE: a third style command ("collect style putdocx") was also
    * suggested but isn't complete, valid syntax on its own -- collect's
    * putdocx-related styling needs specific sub-options (e.g., layout()
    * or a named style). Left out until confirmed rather than risk a
    * broken command; the two lines above already cover the main ask
    * (cleaner number formatting, no redundant header rows).

    dtable y_pop x1fte x3_pop x4_pop x5_pop, ///
        continuous(y_pop x1fte x3_pop x4_pop x5_pop, statistics(mean median)) ///
        title("Table 13.1 Descriptive Statistics") ///
        export("$tables_dir/Table13_1_dtable.docx", replace)
}
else {
    di as error "Stata 18+ required for dtable -- falling back to asdoc/tabstat."

    * Legacy path (Stata < 18)
    asdoc tabstat y_pop x1fte x3_pop x4_pop x5_pop, statistics(mean median) ///
        column(statistics) format(%9.0fc) dec(0) long ///
        title("Table 13.1 Descriptive Statistics -- Legacy asdoc/tabstat") ///
        save("$tables_dir/Table13_1_asdoc.doc") replace label abb(0) replace
}

*------------------------------------------------------------------------
* Grouped comparison table (Maryland vs. all other states, by decade)
*------------------------------------------------------------------------
* NOTE: Example_13_1.dta already contains MD and decade (with value labels
* MD1/decade1) -- the block below is now idempotent so it works whether
* the variables/labels pre-exist in the data or need to be built fresh.
*------------------------------------------------------------------------
capture confirm variable MD
if _rc {
    gen MD = 0
    lab var MD "Comparisons"
    replace MD = 1 if fips == 24
}

capture label list MD1
if _rc {
    label define MD1 1 "Maryland" 0 "All Other States"
}
label values MD MD1

capture confirm variable decade
if _rc {
    gen decade = 0
    lab var decade "Decades"
    replace decade = 1 if fy >= 1980 & fy <= 1989
    replace decade = 2 if fy >= 1990 & fy <= 1999
    replace decade = 3 if fy >= 2000 & fy <= 2009
    replace decade = 4 if fy >= 2010 & fy <= 2018
}

capture label list decade1
if _rc {
    label define decade1 1 "1980 to 1989" 2 "1990 to 1999" 3 "2000 to 2009" ///
        4 "2010 to 2018"
}
label values decade decade1

*------------------------------------------------------------------------
* Modern path (Stata 17+): table + collect -- official StataCorp command,
* redesigned in Stata 17. Replaces the asdoc/table workaround with no
* third-party dependency.
*------------------------------------------------------------------------
if c(stata_version) >= 17 {
    collect clear
    table decade MD, statistic(mean y_pop) nformat(%9.0fc)
    collect title "Table 13.2 Average State Appropriations per Population"
    collect export "$tables_dir/Table13_2_table.docx", replace
}
else {
    di as error "Stata 17+ required for table/collect -- falling back to asdoc/table."

    * Legacy path (Stata < 17)
    asdoc table decade MD, contents(mean y_pop) format(%9.0fc) dec(0) ///
        title("Table 13.2 Average State Appropriations per Population -- Legacy") ///
        save("$tables_dir/Table13_2_asdoc.doc") replace label abb(0) replace
}

* NOTE -- for a group-comparison version with significance tests (rather
* than a plain mean-by-decade-by-MD crosstab), dtable's by()/tests option
* is the better fit than table/collect -- e.g.:
*   dtable y_pop, by(MD, tests) over(decade) ///
*      title("Table 13.2 Average State Appropriations per Population") ///
*      export("$tables_dir/Table13_2_dtable.docx", replace)
* Confirm with the publisher/reviewer which presentation (plain crosstab
* vs. tested group comparison) is preferred before finalizing.

*------------------------------------------------------------------------
* 13.2.3 Descriptive Statistics in Word/HTML Tables (R equivalent)
*------------------------------------------------------------------------
* NOTE: R equivalent (modelsummary/gtsummary) belongs in R_code13.R,
* rebuilding Table 13.1/13.2 on the same data for direct comparison.
* No further Stata code required here.

*------------------------------------------------------------------------
* Close the standalone log opened above, if this script opened one.
* When called from Stata_code13.do, this is skipped and the master log
* remains open for the rest of the chapter's sub-scripts.
*------------------------------------------------------------------------
if $standalone_log_open == 1 {
    log close
}
