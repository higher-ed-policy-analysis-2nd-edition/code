*========================================================================
* RegressionPlots13.do
* Section 13.5: Presenting Multivariate Panel Regression Results
*
* Called by:  Stata_code13.do
* Inherits:   $graphs_dir, $tables_dir, $logdir, $data_dir, $syntax_dir
*             globals from master (re-derived below if run standalone)
*
* Code development assisted by Claude (Anthropic). The author provided
* specifications and reviewed, tested, and validated all code.
*========================================================================
* NOTE: This section was split out of what was originally drafted as
* "13.5.1" inside CausalPlots13.do. The pooled OLS, CCEMG, DCCE-MG/ARDL,
* and CGB-subgroup models below are associational panel regressions --
* none involve an identification strategy (no IV, DiD, RDD, or synthetic
* control), so they do not belong under "Presenting Causal Inference
* Results." They are retained because coefplot-based presentation is
* still a core technique worth demonstrating, just under its own,
* correctly-labeled section ahead of the causal inference material.
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
    log using "$logdir/RegressionPlots13_output.log", replace text
    di "RegressionPlots13.do log opened: " c(current_date) " " c(current_time)
    di "Log location: $logdir/RegressionPlots13_output.log"
}

*========================================================================
* 13.5.1 Regression Coefficient Plots
*========================================================================
* Retained from 1st ed. Ch. 10. coefplot remains the primary tool for
* complex multi-model/multi-color plots such as the ones below.
*
* NOTE -- NEW CODE NEEDED: add a short standalone example using Stata 19's
* native graph bar/dot/box with the meanci statistic as a lighter-weight
* alternative for the simple single-model case, e.g.:
*
*   reg D1.lnnetut L1.D1.lnstateap L1.D1.lnfte L1.D1.lnperinc
*   margins, dydx(*)
*   marginsplot
*
* Frame in the text as "coefplot for anything with grouping/color logic;
* native meanci/heatmap/rpspike for a quick single-series CI display."
*========================================================================

use "$data_dir/Example_13_1.dta", clear

*------------------------------------------------------------------------
* Figure 13.7: Pct. Change in Appropriations, FTE, and Personal Income
* Due to a Pct. Change in Net Tuition Revenue (pooled OLS)
*------------------------------------------------------------------------
reg D1.lnnetut L1.D1.lnstateap L1.D1.lnfte L1.D1.lnperinc

mata: st_matrix("e(box)", (st_matrix("e(b)") :- 2 \ st_matrix("e(b)") :+ 2))

coefplot, xline(0) drop(_cons) mlabel format(%9.2g) mlabposition(0) ///
    msymbol(i) ciopts(recast(. rbar) barwidt(. 0.35) fcolor(. white) ///
    lwidth(. medium)) rescale(10) levels(95 99) ///
    coeflabels(LD.lnstateap = "State Appropriations" ///
    LD.lnfte = "FTE Enrollment" LD.lnperinc = "State Personal Income") ///
    ytitle(10 Percent Change in . . .) xtitle(Change in Net Tuition Revenue) ///
    name(fig13_7_ols_coefplot, replace)

graph export "$graphs_dir/fig13_7_ols_coefplot_Stata.png", replace

*------------------------------------------------------------------------
* Figure 13.8: Same, with CCEMG Estimator (cross-sectional dependence)
*------------------------------------------------------------------------
gen Dlnnetut   = D1.lnnetut
gen LDlnstateap = LD1.lnstateap
gen LDlnfte     = LD1.lnfte
gen LDlnperinc  = LD1.lnperinc

xtmg Dlnnetut LDlnstateap LDlnfte LDlnperinc, cce

mata: st_matrix("e(box)", (st_matrix("e(b)") :- 2 \ st_matrix("e(b)") :+ 2))

coefplot, xline(0) keep(LDlnstateap LDlnfte LDlnperinc) ///
    mlabel format(%9.2g) mlabposition(0) msymbol(i) ///
    ciopts(recast(. rbar) barwidt(. 0.35) fcolor(. white) lwidth(. medium)) ///
    rescale(10) levels(95 99) ///
    coeflabels(LDlnstateap = "{bf:State Appropriations}" ///
    LDlnfte = "FTE Enrollment" LDlnperinc = "State Personal Income", ///
    labsize(medium)) vertical ///
    title("Short-Run Change in {bf:Net Tuition Revenue} Due to a 10% Change in" ///
    "{bf:State Appropriations} (controlling for other factors)", ///
    size(medium) margin(small) justification(center)) ///
    name(fig13_8_ccemg_coefplot, replace)

graph export "$graphs_dir/fig13_8_ccemg_coefplot_Stata.png", replace

*------------------------------------------------------------------------
* Figure 13.9: HCR Model with DCCE-MG Estimator and ARDL
*------------------------------------------------------------------------
qui xtdcce2 Dlnnetut L1.Dlnnetut LDlnstateap LDlnfte LDlnperinc, ///
    reportc cr(_all) cr_lags(3 3 3 3) lr(L1.Dlnnetut LDlnstateap LDlnfte ///
    LDlnperinc) lr_options(ardl)

* NOTE: original 1st-ed. script did not carry a complete coefplot call
* through for this model -- confirm final coefplot spec against the
* Ch. 10 (2nd ed.) DCCE-MG results before finalizing Figure 13.9.

*========================================================================
* 13.5.2 Marginal Effects (Continuous Variables) and Graphs
*========================================================================
* Retained from 1st ed. Ch. 10 -- foundational material feeding into
* 13.5.1's coefplot building block.
*========================================================================

use "$data_dir/Example_13_4.dta", clear

global y  "lnadminstaff"
global x1 "lnnet_tuition_rev_adj"
global x2 "lnstate_appro_adj"
global x3 "lnfedrev_r"
global x4 "lnFTE_enroll"

xtscc $y L1.$x1 L1.$x2 L1.$x3 L1.$x4
margins, dydx(L1.$x1 L1.$x2 L1.$x3 L1.$x4)
margins, eyex(L1.$x1 L1.$x2 L1.$x3 L1.$x4) at((median) _all)

*------------------------------------------------------------------------
* Figures 13.10.1-13.10.3: Elasticities at the Median, 25th, and 75th
* Percentiles
*------------------------------------------------------------------------
foreach pctl in median p25 p75 {
    local at_opt "(median) _all"
    if "`pctl'" == "p25" local at_opt "(p25) _all"
    if "`pctl'" == "p75" local at_opt "(p75) _all"

    xtscc $y L1.$x1 L1.$x2 L1.$x3 L1.$x4
    margins, eyex(L1.$x1 L1.$x2 L1.$x3 L1.$x4) at(`at_opt') post
    mata: st_matrix("e(box)", (st_matrix("e(b)") :- 1 \ st_matrix("e(b)") :+ 1))

    coefplot (., keep(L.net_tuition_rev_adj) color(black)) ///
        (., keep(L.state_appro_adj) color(gray)) (., keep(L.fedrev_r) color(gray)) ///
        (., keep(L.FTE_enroll) color(gray)), legend(on) xline(0) ///
        nooffsets pstyle(p1) recast(bar) barwidth(0.4) fcolor(*.8) ///
        coeflabels(L.net_tuition_rev_adj = "{bf:Net Tuition Revenue}" ///
        L.state_appro_adj = "State Appropriations" L.fedrev_r = "Federal Revenue" ///
        L.FTE_enroll = "FTE Enrollment", labsize(small)) ///
        title("Percent Change in {bf:Administrators} Due to a 10% Change in" ///
        "{bf:Net Tuition Revenue} (controlling for other factors)", ///
        size(medium) margin(small) justification(center)) ///
        addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(1) mlabcolor(black)) ///
        vertical noci format(%9.1f) rescale(10) p2(nokey) p3(nokey) ///
        p1(label("Different from Zero")) p4(label("Ignore - not different zero")) ///
        ytitle(Percent) xtitle("At the `pctl'", size(small)) ///
        name(fig13_10_`pctl', replace)

    graph export "$graphs_dir/fig13_10_`pctl'_Stata.png", replace
}

*========================================================================
* 13.5.3 Marginal Effects (Categorical Variables) and Graphs
*========================================================================
use "$data_dir/Example_13_4.dta", clear

global y "adminstaff"
global x "L1.net_tuition_rev_adj L1.state_appro_adj L1.fedrev_r L1.FTE_enroll"

qui xtscc y $x if CGB == 0
qui margins, eyex(*) post
mata: st_matrix("e(box)", (st_matrix("e(b)") :- 1 \ st_matrix("e(b)") :+ 1))
eststo NoCGB

qui xtscc y $x if CGB == 1
qui margins, eyex(*) post
mata: st_matrix("e(box)", (st_matrix("e(b)") :- 1 \ st_matrix("e(b)") :+ 1))
eststo CGB

*------------------------------------------------------------------------
* Figure 13.11: Pct. Change in Administrators, by Consolidated Governing
* Board (CGB) Status
*------------------------------------------------------------------------
coefplot NoCGB CGB, xline(0) format(%9.0f) rescale(10) recast(bar) ///
    barwidth(0.3) fcolor(*.5) ///
    coeflabels(L.net_tuition_rev_adj = "{bf:Net Tuition Revenue}" ///
    L.state_appro_adj = "State Appropriations" L.fedrev_r = "Federal Revenues" ///
    L.FTE_enroll = "FTE Enrollment", labsize(small)) vertical ///
    p1(label("No CGB") color(gray)) p4(label("CGB") color(black)) ///
    ytitle(Percent) ylabel(-4(2)10) ///
    title("Percent Change in {bf:Administrators} Due to a 10% Change in" ///
    "{bf:Net Tuition Revenue} (controlling for other factors)", ///
    size(medium) margin(small) justification(center)) ///
    name(fig13_11_cgb_coefplot, replace)

graph export "$graphs_dir/fig13_11_cgb_coefplot_Stata.png", replace

*------------------------------------------------------------------------
* Close the standalone log opened above, if this script opened one.
* When called from Stata_code13.do, this is skipped and the master log
* remains open for the rest of the chapter's sub-scripts.
*------------------------------------------------------------------------
if $standalone_log_open == 1 {
    log close
}
