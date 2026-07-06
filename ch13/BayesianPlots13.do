*========================================================================
* BayesianPlots13.do
* Section 13.8: Presenting Bayesian Microsimulation and CBA Results
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
    log using "$logdir/BayesianPlots13_output.log", replace text
    di "BayesianPlots13.do log opened: " c(current_date) " " c(current_time)
    di "Log location: $logdir/BayesianPlots13_output.log"
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

* Adapted directly from Stata_code12.do (Sections 7-10: Bayesian
* Microsimulation, Cost-Benefit Decomposition, Posterior Summaries, and
* Figures). Chapter 12's script saves its S=1000 posterior draws to a
* known, deterministic path:
*   C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 12\Output\tables\
*     sim_results_ch12.dta
* (built from Chapter 12's own $tables_dir = "Output/tables" relative to
* its "cd" to the Chapter 12 folder -- unlike the RDD dataset earlier in
* this chapter, this path is fully deterministic, so it's used directly
* rather than regenerated here.)
*
* Ch. 12's actual posterior finding (Chapter12_Stata_output.log): the
* $100k Grad PLUS cap is net-NEGATIVE. Posterior mean net benefit =
* -$77,648 (000s), 95% CI [-95,872, -62,471], entirely below zero;
* P(Net Benefit > 0) = 0.000. The cap displaces students with positive
* marginal returns (MPRTE > ATT), so human capital loss dominates fiscal
* savings. This is Scenario B in Ch. 12's own classification. The figures
* below reflect this actual result rather than a generic/optimistic
* illustration.
global ch12_tables_dir "C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 12\Output\tables"

capture confirm file "$ch12_tables_dir/sim_results_ch12.dta"
if _rc {
    di as error "sim_results_ch12.dta not found at $ch12_tables_dir -- confirm Chapter 12 has been run and this path is still correct."
}

*========================================================================
* 13.8.1 Posterior Distributions and Credible Intervals
*========================================================================
* Adapted from Stata_code12.do's fig12_1 (posterior histogram of net
* social benefit). Contrast against the classical CI plots in 13.6-13.7:
* this is a credible interval (a probability statement about the
* parameter given the data and the posterior), not a confidence interval
* (a statement about the long-run behavior of the estimator) -- flag
* that distinction explicitly in the chapter prose next to this figure,
* not just in the note() text below.
*------------------------------------------------------------------------
use "$ch12_tables_dir/sim_results_ch12.dta", clear

qui sum net_benefit_s
local nb_mean_plot = r(mean)

*------------------------------------------------------------------------
* Presentation Enhancements for Policymakers (fig13_18)
*   - Subtitle now carries the VERDICT in plain language ("this policy
*     would reduce net value"), not a technical description of what the
*     chart shows -- a legislative staffer's eyes go title -> subtitle
*     first; that's where the bottom line needs to live, not in a note()
*     that may go unread.
*   - Technical detail (credible interval, Scenario B mechanism) moved
*     to note(), shortened, for readers who want the "why"
*   - Posterior mean marked with its own reference line, not just zero
*------------------------------------------------------------------------
histogram net_benefit_s, ///
    percent ///
    fcolor(gs8) lcolor(gs0) lwidth(thin) ///
    xline(0, lpattern(dash) lcolor(gs0) lwidth(medthick)) ///
    xline(`nb_mean_plot', lpattern(solid) lcolor(gs0) lwidth(medthick)) ///
    ytitle("Percent of Posterior Draws") ///
    xtitle("Net Social Benefit ($000s)") ///
    title("$100k Grad PLUS Lifetime Cap: Net Social Benefit", $TITLESIZE) ///
    subtitle("Bottom line: this policy would REDUCE net social value", $SUBTITLESIZE) ///
    note("95% of simulated outcomes fall below zero (posterior mean:" ///
         %6.1f `nb_mean_plot' " thousand). Human capital loss from" ///
         "displaced students outweighs the fiscal savings.", $NOTESIZE) ///
    name(fig13_18_posterior_density, replace)

pubexport fig13_18_posterior_density

*========================================================================
* 13.8.2 CBA Component Breakdown (Waterfall Chart)
*========================================================================
* Ch. 12's own fig12_7 ("Full Social Cost Stack") uses a plain graph bar
* with signed values, not a true cascading waterfall (no running
* cumulative total) -- confirmed by reading Stata_code12.do directly.
* This section upgrades that into an actual cascading waterfall using
* the same five posterior-mean components Ch. 12 already computed, as a
* genuine presentation improvement over the source chapter rather than
* a like-for-like copy. Manual construction via rbar with a running
* base, since no official Stata waterfall command exists (confirmed via
* search).
*------------------------------------------------------------------------
use "$ch12_tables_dir/sim_results_ch12.dta", clear

qui sum fiscal_s
scalar sc_fs = r(mean)
qui sum effic_gain_s
scalar sc_eg = r(mean)
qui sum behav_cost_s
scalar sc_bc = r(mean)
qui sum inst_rev_loss_s
scalar sc_ir = r(mean)
qui sum cross_sub_loss_s
scalar sc_cs = r(mean)

clear
set obs 5
gen step  = _n
gen str28 complabel = ""
gen delta = .

* Order chosen to tell the CBA story left to right: benefits first
* (fiscal savings, efficiency gain), then costs (human capital loss,
* institutional revenue loss, cross-subsidy disruption).
replace complabel = "Fiscal Savings"              in 1
replace complabel = "Efficiency Gain"             in 2
replace complabel = "Human Capital Loss"          in 3
replace complabel = "Institutional Rev. Loss"     in 4
replace complabel = "Cross-Subsidy Disruption"    in 5

replace delta =  sc_fs  in 1
replace delta =  sc_eg  in 2
replace delta = -sc_bc  in 3
replace delta = -sc_ir  in 4
replace delta = -sc_cs  in 5

* Running cumulative total -- the defining feature of a waterfall chart
* that Ch. 12's fig12_7 does not have.
gen running_end   = sum(delta)
gen running_start = running_end - delta
* Each bar spans [min(start,end), max(start,end)] so rbar draws the
* correct direction regardless of whether the step is a gain or a loss.
gen bar_lo = min(running_start, running_end)
gen bar_hi = max(running_start, running_end)
gen mid    = (bar_lo + bar_hi) / 2

* Dollar-value label for each bar, printed directly on the chart --
* a policymaker shouldn't have to eyeball bar height against the axis
* to know a component's actual dollar contribution.
* NOTE -- BUG FIX: "%+9.0f" is not a valid Stata display format -- Stata's
* numeric formats support a leading "-" for left-alignment but have no "+"
* flag to force an explicit plus sign on positive values (unlike C's
* printf). This previously aborted the do-file with "invalid %format"
* (r(120)) before the waterfall chart (fig13_19) was ever built. Fixed by
* formatting the magnitude with a plain %9.0f and prepending "+" by hand
* for benefit bars (delta > 0); cost bars already display their own "-"
* sign from the negated delta values set above, so no change needed there.
gen str16 dlabel = ""
forvalues i = 1/5 {
    local dv : display %9.0f delta[`i']
    local dv = trim("`dv'")
    if delta[`i'] > 0 {
        local dv = "+" + "`dv'"
    }
    quietly replace dlabel = "`dv'" + "K" in `i'
}

*------------------------------------------------------------------------
* Presentation Enhancements for Policymakers (fig13_19)
*   - Cascading running-total construction (see the block above) is
*     itself the main enhancement over Ch. 12's own fig12_7, which uses
*     a plain signed bar chart with no cumulative total at all
*   - Each bar's dollar value is printed directly on the chart (mlabel
*     layer below) -- no need to cross-reference the y-axis to know
*     what a component is actually worth
*   - Legend distinguishes benefit vs. cost bars explicitly, in words
*   - Subtitle states which direction the running total ends up moving
*------------------------------------------------------------------------
* NOTE -- BUG FIX: the subtitle below originally referenced `nb_mean',
* a local macro that isn't actually defined until Section 13.8.4,
* several hundred lines later in this same file -- at this point it
* would have been blank. Fixed by reading the final cumulative total
* (running_end in row 5) directly from data already built in this
* section, rather than depending on a macro defined elsewhere.
local nb_final = running_end[5]

twoway ///
    (rbar bar_lo bar_hi step if delta >= 0, ///
        barwidth(0.6) fcolor(gs11) lcolor(gs0)) ///
    (rbar bar_lo bar_hi step if delta <  0, ///
        barwidth(0.6) fcolor(gs4)  lcolor(gs0)) ///
    (scatter mid step, mcolor(none) msymbol(none) ///
        mlabel(dlabel) mlabpos(0) mlabsize(medsmall) mlabcolor(gs0)), ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    xlabel(1 "Fiscal" 2 "Efficiency" 3 "Human Capital" ///
           4 "Inst. Revenue" 5 "Cross-Subsidy", angle(30) labsize(small)) ///
    ytitle("Cumulative Net Benefit ($000s)") ///
    xtitle("") ///
    legend(order(1 "Benefit" 2 "Cost") cols(2)) ///
    title("Cost-Benefit Decomposition: $100k Grad PLUS Cap", $TITLESIZE) ///
    subtitle("Bottom line: costs exceed savings by " ///
             %6.0f -`nb_final' " thousand per student", $SUBTITLESIZE) ///
    note("Light bars = benefits; dark bars = costs. Values on each bar are" ///
         "posterior-mean dollar amounts ($000s); final bar = net social benefit" ///
         "(matches fig13_18).", $NOTESIZE) ///
    name(fig13_19_cba_waterfall, replace)

pubexport fig13_19_cba_waterfall

*========================================================================
* 13.8.3 Sensitivity Analysis (Tornado Diagram)
*========================================================================
* Built from the 95% credible intervals Ch. 12 already computes for each
* CBA component (Stata_code12.do Section 8/9) -- reframed as a tornado-
* style range plot rather than a one-at-a-time deterministic sensitivity
* sweep (Ch. 12 does not run one; its uncertainty is already fully
* propagated through the S=1000 posterior draws). Confirmed via search:
* no official Stata tornado-diagram command exists; twoway rbar
* (horizontal) is the standard manual approach, sorted by interval width
* so the component with the most posterior uncertainty appears first.
*------------------------------------------------------------------------
use "$ch12_tables_dir/sim_results_ch12.dta", clear

local complist fiscal_s effic_gain_s behav_cost_s inst_rev_loss_s cross_sub_loss_s
local nrow : word count `complist'

matrix TORN = J(`nrow', 3, .)
local r = 0
foreach v of local complist {
    local ++r
    qui sum `v'
    matrix TORN[`r', 1] = r(mean)
    _pctile `v', p(2.5 97.5)
    matrix TORN[`r', 2] = r(r1)
    matrix TORN[`r', 3] = r(r2)
}
matrix colnames TORN = mean lo hi

preserve
    clear
    svmat TORN, names(col)
    gen str28 complabel = ""
    replace complabel = "Fiscal Savings"           in 1
    replace complabel = "Efficiency Gain"          in 2
    replace complabel = "Human Capital Loss"       in 3
    replace complabel = "Institutional Rev. Loss"  in 4
    replace complabel = "Cross-Subsidy Disruption" in 5

    gen sens = hi - lo
    gsort -sens
    gen plotrow = _n

    * Build value labels for plotrow dynamically, since the label-to-row
    * mapping depends on the runtime sort order above (widest interval
    * first) -- a fixed ylabel() text list would mislabel rows whenever
    * the sort order changes with new data.
    local lbldef ""
    forvalues i = 1/5 {
        local thislbl = complabel[`i']
        local lbldef `lbldef' `i' "`thislbl'"
    }
    label define tornado_lbl `lbldef'
    label values plotrow tornado_lbl

    local most_uncertain = complabel[1]

    * Presentation Enhancements for Policymakers (fig13_20)
    *   - Sorted by interval width so the most uncertain component reads
    *     first, top to bottom -- not alphabetical or estimation order
    *   - Subtitle names that component directly rather than making the
    *     reader infer "most uncertain" from bar length alone
    *   - Horizontal orientation keeps long component labels readable
    *     without rotating text
    twoway rbar lo hi plotrow, horizontal barwidth(0.5) ///
        fcolor(gs8) lcolor(gs0) ///
        ylabel(1/5, valuelabel angle(0) labsize(small)) ///
        yscale(reverse) ///
        xtitle("95% Credible Interval ($000s)") ///
        ytitle("") ///
        title("Posterior Uncertainty by CBA Component", $TITLESIZE) ///
        subtitle("Most uncertain: `most_uncertain'", $SUBTITLESIZE) ///
        name(fig13_20_tornado, replace)

    pubexport fig13_20_tornado
restore

*========================================================================
* 13.8.4 Presenting a Single Policy Number
*========================================================================
* Primarily a prose section in the chapter text, contrasting a single
* headline number (for a legislator with 30 seconds) against the full
* posterior distribution (fig13_18, for a technical audience). The
* figure below is that headline compression: net social benefit as one
* point estimate with its 95% credible interval, deliberately stripped
* of the distributional detail in fig13_18.
*------------------------------------------------------------------------
use "$ch12_tables_dir/sim_results_ch12.dta", clear

qui sum net_benefit_s
local nb_mean = r(mean)
_pctile net_benefit_s, p(2.5 97.5)
local nb_lo = r(r1)
local nb_hi = r(r2)

preserve
    clear
    set obs 1
    gen j  = 1
    gen b  = `nb_mean'
    gen lo = `nb_lo'
    gen hi = `nb_hi'

    * Presentation Enhancements (fig13_21)
    *   - Deliberately stripped down relative to fig13_18: one point, one
    *     interval, nothing else -- this IS the enhancement, the point of
    *     the figure is what it leaves out for a time-constrained audience
    *   - Diamond marker + horizontal cap makes it read at a glance
    twoway ///
        (rcap lo hi j, horizontal lcolor(gs0) lwidth(medium)) ///
        (scatter j b, mcolor(gs0) msymbol(D) msize(large)), ///
        xline(0, lpattern(dash) lcolor(gs8)) ///
        ylabel(1 "Net Social Benefit", angle(0) labsize(medium)) ///
        ytitle("") xtitle("$000s") legend(off) ///
        title("$100k Grad PLUS Cap: Net Social Benefit", $TITLESIZE) ///
        subtitle("Recommendation: does not pay for itself", $SUBTITLESIZE) ///
        note("Point estimate = " %6.0f `nb_mean' " thousand per student" ///
             "(95% credible interval entirely below zero). Full" ///
             "distribution in fig13_18; see Section 13.9 for audience tailoring.", $NOTESIZE) ///
        name(fig13_21_single_policy_number, replace)

    pubexport fig13_21_single_policy_number
restore

*------------------------------------------------------------------------
* Close the standalone log opened above, if this script opened one.
* When called from Stata_code13.do, this is skipped and the master log
* remains open for the rest of the chapter's sub-scripts.
*------------------------------------------------------------------------
if $standalone_log_open == 1 {
    log close
}
