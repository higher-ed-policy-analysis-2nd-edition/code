*========================================================================
* CausalPlots13.do
* Section 13.6: Presenting Causal Inference Results
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
    log using "$logdir/CausalPlots13_output.log", replace text
    di "CausalPlots13.do log opened: " c(current_date) " " c(current_time)
    di "Log location: $logdir/CausalPlots13_output.log"
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

*========================================================================
* 13.6.1 Event-Study / DiD Plots
*========================================================================
* Adapted directly from the Ch. 10 (2nd ed.) scripts Georgia_DiD.do
* (Sections 10.3.2-10.3.3) and ETWFE.do (Section 10.7.4.1). Chapter 10
* does NOT use the event_plot package -- both of its event-study figures
* (fig10_6, fig10_7_3) are built by hand via twoway, pulling coefficients
* out of xtreg/jwdid into a small plotting dataset. This section
* reproduces that same approach under Chapter 13's own figure numbers,
* using freshly re-downloaded copies of the same GitHub data so the
* section is fully self-contained (no dependency on Ch. 10's local
* intermediate files).
*
* Two event studies are shown side by side -- the comparison IS the
* presentation technique here: a single-treatment TWFE design (Georgia
* only) next to a staggered-adoption ETWFE design (Georgia/Wisconsin/
* Pennsylvania cohorts) -- rather than overlaying multiple estimators on
* one axis via event_plot, which Ch. 10 never actually tested.
*========================================================================

capture which jwdid
if _rc {
    di as error "Package jwdid not found -- install with: ssc install jwdid, replace"
}
capture which reghdfe
if _rc {
    di as error "Package reghdfe not found -- install with: ssc install reghdfe, replace"
}

*------------------------------------------------------------------------
* Figure 13.12: TWFE Event Study -- Georgia Consolidation (single treatment)
* Adapted from Georgia_DiD.do Sections 10.3.1-10.3.3.
*------------------------------------------------------------------------
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch10/Example_10_3_1.csv" ///
     "Example_10_3_1.csv", replace
import delimited "Example_10_3_1.csv", clear

replace state = strtrim(state)

gen sreb = inlist(state, "Alabama","Arkansas","Delaware","Florida",        ///
                         "Georgia","Kentucky","Louisiana","Maryland","Mississippi") ///
         | inlist(state, "North Carolina","Oklahoma","South Carolina",     ///
                         "Tennessee","Texas","Virginia","West Virginia")
keep if sreb == 1

gen fips = .
replace fips = 1  if state == "Alabama"
replace fips = 5  if state == "Arkansas"
replace fips = 10 if state == "Delaware"
replace fips = 12 if state == "Florida"
replace fips = 13 if state == "Georgia"
replace fips = 21 if state == "Kentucky"
replace fips = 22 if state == "Louisiana"
replace fips = 24 if state == "Maryland"
replace fips = 28 if state == "Mississippi"
replace fips = 37 if state == "North Carolina"
replace fips = 40 if state == "Oklahoma"
replace fips = 45 if state == "South Carolina"
replace fips = 47 if state == "Tennessee"
replace fips = 48 if state == "Texas"
replace fips = 51 if state == "Virginia"
replace fips = 54 if state == "West Virginia"

gen byte treat_state = (state == "Georgia")
gen byte post        = (fy >= 2018)
gen byte did         = treat_state * post

gen lngenop  = log(general_public_operations)
gen lntotsup = log(total_state_support)
gen lnfinaid = log(total_financial_aid)
gen lntuifee = log(net_tuition_and_fee_revenue)
gen lnfte    = log(net_fte_enrollment)

global controls "lntotsup lnfinaid lntuifee lnfte"
xtset fips fy

* Event-time dummies: leads F2-F16 (F16 bins all ry <= -16), lags L0-L3.
* Reference period is ry = -1 (FY 2017), omitted by construction.
gen int rel_year = fy - 2018

local kpre  16
local kpost  3

forvalues k = 2/`kpre' {
    gen byte F`k'_ga = (treat_state == 1 & rel_year == -`k')
}
replace F`kpre'_ga = (treat_state == 1 & rel_year <= -`kpre')

forvalues k = 0/`kpost' {
    gen byte L`k'_ga = (treat_state == 1 & rel_year == `k')
}

local evars ""
forvalues k = `kpre'(-1)2 {
    local evars "`evars' F`k'_ga"
}
forvalues k = 0/`kpost' {
    local evars "`evars' L`k'_ga"
}

qui xtreg lngenop `evars' $controls i.fy, fe vce(cluster fips)

preserve
    local nobs = (`kpre' - 1) + 1 + (`kpost' + 1)
    clear
    set obs `nobs'
    gen int t = .
    gen b     = .
    gen lo    = .
    gen hi    = .

    local i = 0
    forvalues k = `kpre'(-1)2 {
        local ++i
        qui replace t  = -`k'                            in `i'
        qui replace b  = _b[F`k'_ga]                     in `i'
        qui replace lo = _b[F`k'_ga] - 1.96*_se[F`k'_ga]  in `i'
        qui replace hi = _b[F`k'_ga] + 1.96*_se[F`k'_ga]  in `i'
    }
    local ++i
    qui replace t = -1 in `i'
    qui replace b = 0  in `i'
    qui replace lo = 0 in `i'
    qui replace hi = 0 in `i'
    forvalues k = 0/`kpost' {
        local ++i
        qui replace t  = `k'                             in `i'
        qui replace b  = _b[L`k'_ga]                     in `i'
        qui replace lo = _b[L`k'_ga] - 1.96*_se[L`k'_ga]  in `i'
        qui replace hi = _b[L`k'_ga] + 1.96*_se[L`k'_ga]  in `i'
    }
    sort t

    * Presentation Enhancements for Policymakers (fig13_12)
    *   - Shaded rarea bands (not just error bars) make the pre/post
    *     periods visually distinct at a glance
    *   - Reference period marked with an X marker, not just omitted
    *   - "Policy Adopted" printed directly on the chart at the break
    *     point -- a thin dotted line alone is easy to miss on a skim;
    *     labeling it removes any need to cross-reference the caption
    qui sum hi
    local ytxt = r(max) * 0.92

    twoway ///
        (rarea lo hi t if t < 0, fcolor(gs14) lwidth(none))                   ///
        (rarea lo hi t if t >= 0, fcolor(gs10) lwidth(none))                  ///
        (line  b  t,  lcolor(gs0) lwidth(medthick) lpattern(solid))           ///
        (scatter b t if t == -1, mcolor(gs0) msymbol(X) msize(large)),        ///
        yline(0, lpattern(dash) lcolor(gs8))                                  ///
        xline(-0.5, lpattern(dot) lcolor(gs6))                                ///
        text(`ytxt' -0.5 "Policy Adopted", orientation(vertical) place(w) size(small)) ///
        xtitle("Years Relative to Consolidation (FY 2018 = 0)")               ///
        ytitle("Coefficient (log operating expenses)")                       ///
        title("Event Study: Georgia Higher Education Consolidation", $TITLESIZE) ///
        subtitle("TWFE, single treated unit. Reference period: t = -1.", $SUBTITLESIZE) ///
        legend(off) name(fig13_12_twfe_event_study, replace)

    pubexport fig13_12_twfe_event_study
restore

*------------------------------------------------------------------------
* Figure 13.13: ETWFE Event Study -- Staggered Adoption (Georgia/
* Wisconsin/Pennsylvania cohorts). Adapted from ETWFE.do Section 10.7.4.1.
*------------------------------------------------------------------------
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch10/Example_10_7_3.csv" ///
     "Example_10_7_3.csv", replace
import delimited "Example_10_7_3.csv", clear

gen lngenop  = log(generalpublicoperations)
gen lntotsup = log(totalstatesupport)
gen lnfinaid = log(totalfinancialaid)
gen lntuifee = log(nettuitionandfeerevenue)
gen lnfte    = log(netfteenrollment)

drop if missing(lngenop, lntotsup, lnfinaid, lntuifee, lnfte)

global controls "lntotsup lnfinaid lntuifee lnfte"

gen gyear = 0
replace gyear = 2013 if fips == 13     // Georgia
replace gyear = 2018 if fips == 55     // Wisconsin
replace gyear = 2022 if fips == 42     // Pennsylvania

xtset fips fy

* Covariate-adjusted specification, never-treated controls, cohort-level
* heterogeneity (hettype(cohort) avoids the rank-deficient fully-saturated
* model with only 3 cohorts and 4 time-varying covariates -- see ETWFE.do
* for the full explanation).
jwdid lngenop $controls, ivar(fips) tvar(fy) gvar(gyear) ///
    never hettype(cohort) cluster(fips)

*------------------------------------------------------------------------
* estat event, plot does not accept title()/ytitle()/xtitle(), so the
* figure is built manually from r(b)/r(V) -- identical approach to
* ETWFE.do. The omitted reference period is located by its zero/missing
* variance on the r(V) diagonal (format-proof across jwdid versions,
* unlike parsing event time from r(b) column names).
*------------------------------------------------------------------------
estat event
matrix Rb = r(b)
matrix RV = r(V)
local nE  = colsof(Rb)

local refpos = 0
forvalues i = 1/`nE' {
    if (RV[`i',`i'] == 0 | missing(RV[`i',`i']))  local refpos = `i'
}
if `refpos' == 0  local refpos = `=ceil(`nE'/2)'

preserve
    clear
    set obs `nE'
    gen long   _ord = _n
    gen double att  = .
    gen double se   = .
    forvalues i = 1/`nE' {
        replace att = Rb[1,`i']         in `i'
        replace se  = sqrt(RV[`i',`i']) in `i'
    }
    gen double event = _ord - `refpos' - 1
    drop _ord

    gen double lo = att - 1.96*se
    gen double hi = att + 1.96*se
    sort event

    * Presentation Enhancements for Policymakers (fig13_13)
    *   - Connected line + markers (not bare points) makes the event-time
    *     path readable; rcap intervals in a muted gray so they recede
    *     behind the point estimates rather than competing with them
    *   - "Treatment Begins" printed on the chart at the break point, same
    *     rationale as fig13_12 -- don't make the reader infer it from an
    *     axis label alone. Deliberately does NOT smooth over the kink at
    *     t = 0 -- see the caution box immediately below for why.
    qui sum hi
    local ytxt13 = r(max) * 0.90

    twoway (rcap hi lo event, lcolor(gs8))                                  ///
           (connected att event, lcolor(black) mcolor(black) msymbol(O)) , ///
        yline(0, lpattern(dash) lcolor(gs10))                              ///
        xline(-0.5, lpattern(dot) lcolor(gs8))                             ///
        text(`ytxt13' -0.5 "Treatment Begins", orientation(vertical) place(w) size(small)) ///
        title("ETWFE Event Study: Staggered Adoption", $TITLESIZE)         ///
        subtitle("Covariate-adjusted, never-treated controls", $SUBTITLESIZE) ///
        ytitle("ATT on log operations")                                   ///
        xtitle("Years relative to treatment")                             ///
        legend(off) name(fig13_13_etwfe_event_study, replace)

    pubexport fig13_13_etwfe_event_study
restore

*------------------------------------------------------------------------
* CAUTION BOX (for the chapter prose, next to fig13_13 specifically --
* this is the staggered/cohort case where it applies, not fig13_12):
* Cohort-based estimators built on Callaway-Sant'Anna/Sun-Abraham-style
* logic construct pre- and post-treatment event-time coefficients
* asymmetrically. A visual "kink" right at t = 0 does not by itself mean
* parallel trends is violated -- the pre-period and post-period
* coefficients are not always estimated the same way under the hood.
* This is a genuinely new addition for Chapter 13 -- Chapter 10's own
* text does not currently discuss this (Roth, 2024); flag it explicitly
* in the caption/prose so a policymaker reader isn't misled by comparing
* fig13_12 and fig13_13's pre-period flatness on the same visual terms.
*------------------------------------------------------------------------

*------------------------------------------------------------------------
* Figure 13.13b: xthdidregress -- official Stata 18 (2022) heterogeneous
* DiD command. This is genuinely new since Ch. 10 was written and not
* used anywhere in the Ch. 10 scripts -- StataCorp's own built-in
* alternative to community-contributed csdid, estimating the same
* Callaway-Sant'Anna (2021) RA/IPW/AIPW estimands natively, with no SSC
* install required. Its postestimation command estat atetplot produces
* a publication-quality event-study plot directly -- no manual twoway
* construction needed at all, unlike fig13_12/fig13_13 above. Reuses the
* same staggered-cohort data already loaded for the ETWFE figure.
*
* NOTE: estat atetplot only supports the ra/ipw/aipw estimators, not
* twfe -- twfe produces no pre-treatment coefficients to plot (confirmed
* against Stata's own documentation/user reports). Use ra here for that
* reason, matching the RA emphasis already used in Ch. 11/12's MTE work.
*
* NOTE: unlike coefplot/twoway, estat atetplot's title()/subtitle() text
* option support is unconfirmed (estat event's own plot option notably
* did NOT support them, per ETWFE.do's comments) -- only name() is used
* below until that's verified directly in Stata.
*
* Companion package worth knowing about: eventbaseline (Koren, SSC 2024)
* re-normalizes the xthdidregress event study to a chosen baseline
* period -- useful if the automatic reference-period choice doesn't
* match the convention used elsewhere in the chapter.
*------------------------------------------------------------------------
if c(stata_version) >= 18 {
    capture confirm variable treated_bin
    if _rc {
        gen byte treated_bin = (fy >= gyear) & (gyear > 0)
    }

    xthdidregress ra (lngenop $controls) (treated_bin), group(fips) ///
        controlgroup(never) vce(cluster fips)

    * NOTE -- BUG FIX: the original name here, "fig13_13b_xthdidregress_
    * event_study" (35 chars), exceeded Stata's 32-character limit for
    * name-class identifiers, throwing "invalid name" -- confirmed this
    * wasn't an estat atetplot-specific quirk, since a same-length-issue
    * name on a plain twoway command elsewhere failed identically. The
    * pubexport call below also previously referenced a different,
    * mismatched name that was never the graph's actual assigned name --
    * both are now the same, shortened, consistent name.
    estat atetplot, name(fig13_13b_xthdid_es, replace)

    pubexport fig13_13b_xthdid_es
}
else {
    di as error "Stata 18+ required for xthdidregress -- skipping this demonstration."
}

*========================================================================
* 13.6.2 RD Plots
*========================================================================
* Adapted from RDD.do Section 10.2.9 (rdplot) and 10.2.5 (bandwidth
* sensitivity). Section 10.2.1's synthetic data generation is also
* reproduced below, self-contained, rather than depending on RDD.do's
* saved ch10_rdd_hsls09_synthetic.dta -- that file is saved to Stata's
* working directory with no explicit path when Chapter 10 runs, and
* turned out not to exist anywhere findable. Same fixed seed (20260510)
* as RDD.do, so this reproduces the identical synthetic sample.
*========================================================================

capture which rdrobust
if _rc {
    di as error "Package rdrobust not found -- install with: ssc install rdrobust, replace"
}

*------------------------------------------------------------------------
* Synthetic HSLS:09-calibrated data (adapted from RDD.do Section 10.2.1)
* N = 4,000; running variable = HS GPA centered at cutoff c = 3.25;
* true LATE on the latent persistence index = 0.10.
*------------------------------------------------------------------------
set seed 20260510

local N         4000
local cutoff    3.25
local true_late 0.10

clear
set obs `N'
gen id = _n

local mu_gpa   3.15
local sd_gpa   0.72
local lo_gpa   1.00
local hi_gpa   4.00
local Fa_gpa = normal((`lo_gpa' - `mu_gpa') / `sd_gpa')
local Fb_gpa = normal((`hi_gpa' - `mu_gpa') / `sd_gpa')

gen double u_gpa  = `Fa_gpa' + runiform() * (`Fb_gpa' - `Fa_gpa')
gen double hs_gpa = invnormal(u_gpa) * `sd_gpa' + `mu_gpa'
drop u_gpa

gen double x = hs_gpa - `cutoff'

label variable hs_gpa "High-school GPA (0-4.0 scale)"
label variable x      "HS GPA centered at cutoff (c = 3.25)"

gen byte D_sharp = (x >= 0)

gen double pr_take_up = .
    replace pr_take_up = 0.05 + 0.09 * (x + 2.25) / 2.25   if x <  0
    replace pr_take_up = 0.70 + 0.15 * min(x / 0.75, 1.0)  if x >= 0
    replace pr_take_up = min(pr_take_up, 0.92)
    replace pr_take_up = max(pr_take_up, 0.02)

gen byte D_fuzzy = (runiform() < pr_take_up)

label variable D_sharp "Scholarship received (sharp design)"
label variable D_fuzzy "Scholarship received (fuzzy design)"

gen byte   female     = (runiform() < 0.54)
gen byte   firstgen   = (runiform() < 0.32)
gen byte   urm        = (runiform() < 0.28)
gen int    act_score  = 18 + int(12 * runiform())
gen byte   income_cat = 1 + int(3 * runiform())
    replace income_cat = min(income_cat, 3)

label variable female     "Female"
label variable firstgen   "First-generation student"
label variable urm        "Underrepresented minority"
label variable act_score  "ACT composite score"
label variable income_cat "Income category (1=low, 3=high)"

gen double mu0 = 0.58 + 0.20 * x - 0.08 * x^2          ///
               + 0.03 * female  - 0.05 * firstgen        ///
               + 0.01 * (income_cat - 2)                 ///
               + rnormal(0, 0.18)

gen double Y1_s      = mu0 + `true_late' * D_sharp + rnormal(0, 0.12)
    replace Y1_s     = max(min(Y1_s, 1), 0)
gen byte   persist_sharp = (Y1_s > 0.50)

gen double Y1_f      = mu0 + `true_late' * D_fuzzy + rnormal(0, 0.12)
    replace Y1_f     = max(min(Y1_f, 1), 0)
gen byte   persist_fuzzy = (Y1_f > 0.50)

gen double credits_y1 = 28 + 5.0 * x - 1.5 * x^2 + 4 * D_sharp ///
                       + rnormal(0, 5)
    replace credits_y1 = max(credits_y1, 0)

gen double cgpa_y1   = 2.80 + 0.35 * x - 0.08 * x^2    ///
                      + 0.08 * D_sharp + rnormal(0, 0.40)
    replace cgpa_y1  = min(max(cgpa_y1, 0), 4.0)

label variable persist_sharp "Second-year persistence (sharp)"
label variable persist_fuzzy "Second-year persistence (fuzzy)"
label variable credits_y1    "Year-1 credits earned"
label variable cgpa_y1       "Year-1 college GPA"

drop Y1_s Y1_f mu0 pr_take_up

*------------------------------------------------------------------------
* Figure 13.14: Publication-quality RD plot -- Second-Year Persistence
* Adapted from RDD.do Section 10.2.9 (fig10_2_4).
*
* Presentation Enhancements for Policymakers:
*   - IMSE-optimal binned scatter (nbins()) rather than a raw scatter of
*     4,000 points, which would just look like noise to a policymaker
*   - Cutoff marked with a vertical line, not left implicit in the axis
*   - Subtitle states the actual estimated effect size in plain language
*     (percentage points), not just "there's a jump at the cutoff"
*   - note() explains bin selection so the figure is self-documenting
*------------------------------------------------------------------------
qui rdrobust persist_sharp x, c(0)
local late_pp : display %4.1f (e(tau_cl) * 100)

rdplot persist_sharp x, c(0) nbins(30 30)                              ///
    graph_options(                                                      ///
        title("Effect of Institutional Merit Scholarship on"           ///
              "Second-Year Persistence", $TITLESIZE)                   ///
        subtitle("Scholarship recipients persist at a `late_pp' percentage-point" ///
                 " higher rate", $SUBTITLESIZE) ///
        xtitle("High-School GPA (centered at cutoff)")                 ///
        ytitle("Second-Year Persistence Rate")                         ///
        xline(0, lcolor(gs0) lpattern(dash) lwidth(medthin))          ///
        legend(off) scheme(s2mono)                                     ///
        name(fig13_14_rdplot_persistence, replace)                     ///
        note("Sharp RD, HS GPA cutoff = 3.25. Circles = bin means; lines =" ///
             "local polynomial fit, bins IMSE-optimal (Calonico et al., 2015).", $NOTESIZE))

pubexport fig13_14_rdplot_persistence

*------------------------------------------------------------------------
* Figure 13.15: Bandwidth Sensitivity -- the visual analog of a
* robustness table. Adapted from RDD.do Section 10.2.5 (fig10_2_3).
*------------------------------------------------------------------------
local bw_list "0.15 0.20 0.25 0.30 0.40 0.50 0.60 0.75 1.00"
local nrow    = wordcount("`bw_list'")

matrix BW = J(`nrow', 4, .)
local r = 0

foreach h of local bw_list {
    local ++r
    qui rdrobust persist_sharp x, c(0) h(`h') kernel(triangular)
    matrix BW[`r', 1] = `h'
    matrix BW[`r', 2] = e(tau_cl)
    matrix BW[`r', 3] = e(ci_l_rb)
    matrix BW[`r', 4] = e(ci_r_rb)
}

matrix colnames BW = h LATE CI_lo CI_hi

preserve
    clear
    svmat BW, names(col)

    * Presentation Enhancements for Policymakers (fig13_15)
    *   - This IS the "visual robustness table" -- a reference zero-line
    *     shows at a glance which bandwidths keep the LATE distinguishable
    *     from zero, rather than making a reader scan a table of p-values
    *   - Subtitle states the robustness verdict directly, computed from
    *     whether any bandwidth's CI actually crosses zero
    qui count if CI_lo <= 0 & CI_hi >= 0
    local crosses_zero = r(N)
    if `crosses_zero' == 0 local robust_txt "Robust: positive at every bandwidth tested"
    else local robust_txt "Caution: not distinguishable from zero at `crosses_zero' of `nrow' bandwidths"

    twoway                                                          ///
        (rcap CI_lo CI_hi h, lcolor(gs10) lwidth(medthin))         ///
        (scatter LATE h, mcolor(gs0) msize(medlarge) msymbol(D))   ///
        (function y = 0, range(0.10 1.05)                          ///
             lcolor(gs0) lpattern(dash) lwidth(medthin)),           ///
        legend(off)                                                 ///
        xtitle("Bandwidth (HS GPA units)")                         ///
        ytitle("Estimated LATE (pp)")                              ///
        title("Bandwidth Sensitivity -- Sharp RD", $TITLESIZE)     ///
        subtitle("`robust_txt'", $SUBTITLESIZE) ///
        note("Outcome: Second-Year Persistence, cutoff = 3.25 HS GPA.", $NOTESIZE) ///
        scheme(s2mono) name(fig13_15_rdd_bw_sensitivity, replace)

    pubexport fig13_15_rdd_bw_sensitivity
restore

*========================================================================
* 13.6.3 Synthetic Control / SDiD Plots
*========================================================================
* Adapted from Georgia_DiD.do Section 10.5 (synth). Re-estimates synth
* directly (Ch. 10's own comment notes it takes about 30 seconds) rather
* than depending on synth's intermediate keep() dataset, whose exact
* location has the same local-working-directory ambiguity flagged above
* for the RDD dataset.
*
* SDID NOTE -- this is deliberately NOT included as a trajectory/gap
* figure here: Chapter 10's own development log (Georgia_DiD.do, Section
* 10.6) records that sdid's native graph option (g1on/g2on) failed with
* r(198) in every tested configuration. Ch. 10 works around this by
* reporting the SDID point estimate only in its estimator-comparison
* forest plot (fig10_9_1), not as a standalone SCM-style trajectory plot.
* The sdid_event package mentioned in earlier drafts of this outline was
* never actually tested against this failure -- treat it as unconfirmed,
* not a validated fix, before relying on it here.
*========================================================================

capture which synth
if _rc {
    di as error "Package synth not found -- install with: ssc install synth, replace"
}

import delimited "Example_10_3_1.csv", clear

replace state = strtrim(state)
gen sreb = inlist(state, "Alabama","Arkansas","Delaware","Florida",        ///
                         "Georgia","Kentucky","Louisiana","Maryland","Mississippi") ///
         | inlist(state, "North Carolina","Oklahoma","South Carolina",     ///
                         "Tennessee","Texas","Virginia","West Virginia")
keep if sreb == 1

gen fips = .
replace fips = 1  if state == "Alabama"
replace fips = 5  if state == "Arkansas"
replace fips = 10 if state == "Delaware"
replace fips = 12 if state == "Florida"
replace fips = 13 if state == "Georgia"
replace fips = 21 if state == "Kentucky"
replace fips = 22 if state == "Louisiana"
replace fips = 24 if state == "Maryland"
replace fips = 28 if state == "Mississippi"
replace fips = 37 if state == "North Carolina"
replace fips = 40 if state == "Oklahoma"
replace fips = 45 if state == "South Carolina"
replace fips = 47 if state == "Tennessee"
replace fips = 48 if state == "Texas"
replace fips = 51 if state == "Virginia"
replace fips = 54 if state == "West Virginia"

gen lngenop  = log(general_public_operations)
gen lntotsup = log(total_state_support)
gen lnfinaid = log(total_financial_aid)
gen lntuifee = log(net_tuition_and_fee_revenue)
gen lnfte    = log(net_fte_enrollment)

xtset fips fy

capture {
    synth lngenop                                                              ///
        lngenop(2001) lngenop(2005) lngenop(2008) lngenop(2010)               ///
        lngenop(2013) lngenop(2015) lngenop(2017)                             ///
        lntotsup lnfinaid lntuifee lnfte,                                     ///
        trunit(13) trperiod(2018)                                             ///
        xperiod(2001(1)2017) resultsperiod(2001(1)2021)                       ///
        keep(synth_results_ch13, replace)
}

if _rc == 0 {
    *--------------------------------------------------------------------
    * Figure 13.16: Actual vs. Synthetic Georgia Trend
    * Adapted from Georgia_DiD.do Section 10.5 (fig10_4).
    *
    * Presentation Enhancements:
    *   - Solid vs. dashed lines (not color alone) distinguish the two
    *     series, so the figure still reads correctly in grayscale print
    *   - Legend spelled out in plain language, not "treated"/"synthetic"
    *--------------------------------------------------------------------
    preserve
        use synth_results_ch13, clear
        rename _Y_treated Y_ga
        rename _Y_synthetic Y_synth
        rename _time fy

        qui sum Y_ga if fy >= 2014 & fy < 2018
        local ytxt16 = r(max) * 1.01

        * Presentation Enhancements for Policymakers (fig13_16)
        *   - "Consolidation" printed on the chart at 2018, matching the
        *     fig13_12/13_13 pattern -- consistent visual language for
        *     "the policy happens here" across every event-style figure
        *     in the chapter, not just a dotted line the reader has to
        *     notice on their own
        twoway ///
            (line Y_ga    fy, lcolor(gs0)  lwidth(medthick) lpattern(solid)) ///
            (line Y_synth fy, lcolor(gs0)  lwidth(medthick) lpattern(dash)), ///
            xline(2018, lpattern(dot) lcolor(gs6))                           ///
            text(`ytxt16' 2018 "Consolidation", place(w) size(small))       ///
            legend(label(1 "Georgia") label(2 "Synthetic Georgia") rows(1))  ///
            ytitle("Log Operating Expenses") xtitle("Fiscal Year")           ///
            title("SCM: Georgia vs. Synthetic Control", $TITLESIZE)         ///
            subtitle("Dashed = synthetic Georgia (the counterfactual)", $SUBTITLESIZE) ///
            name(fig13_16_scm_trends, replace)
        pubexport fig13_16_scm_trends

        *----------------------------------------------------------------
        * Figure 13.17: SCM Gap Plot (actual minus synthetic)
        * Adapted from Georgia_DiD.do Section 10.5 (fig10_5_1).
        *
        * Presentation Enhancements for Policymakers:
        *   - Companion figure to fig13_16 -- isolates the effect itself
        *     rather than making the reader eyeball the gap between two
        *     overlapping trend lines
        *   - Subtitle states the average post-policy gap directly,
        *     computed from the data, not just the sign convention
        *----------------------------------------------------------------
        gen gap = Y_ga - Y_synth
        qui sum gap if fy >= 2018
        local avg_gap : display %5.3f r(mean)

        twoway ///
            (line gap fy, lcolor(gs0) lwidth(medthick) lpattern(solid)), ///
            yline(0, lpattern(dash) lcolor(gs8))                          ///
            xline(2018, lpattern(dot) lcolor(gs6))                        ///
            ytitle("Gap: Log Expenses (Georgia - Synthetic)") xtitle("Fiscal Year") ///
            title("SCM Gap: Effect of Georgia Consolidation", $TITLESIZE) ///
            subtitle("Average post-consolidation gap: `avg_gap' log points", $SUBTITLESIZE) ///
            note("Above zero = Georgia's actual spending exceeded its synthetic counterfactual.", $NOTESIZE) ///
            name(fig13_17_scm_gap, replace)
        pubexport fig13_17_scm_gap
    restore
}
else {
    di as error "synth failed (r(" _rc ")). Check synth installation."
}
*========================================================================

*------------------------------------------------------------------------
* Close the standalone log opened above, if this script opened one.
* When called from Stata_code13.do, this is skipped and the master log
* remains open for the rest of the chapter's sub-scripts.
*------------------------------------------------------------------------
if $standalone_log_open == 1 {
    log close
}
