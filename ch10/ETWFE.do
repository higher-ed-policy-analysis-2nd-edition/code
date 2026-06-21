*========================================================================
* ETWFE.do
* Section 10.7.4: Staggered Adoption Robustness — Two Estimator Families
*   10.7.4.1  Extended Two-Way Fixed Effects (ETWFE) via jwdid
*             (Rios-Avila, Nagengast & Yotov; estimator: Wooldridge 2021, 2023)
*   10.7.4.2  Rolling Difference-in-Differences via lwdid
*             (Lee & Wooldridge 2026a, 2026b; command: Lee & Wooldridge)
* Higher Education Policy Analysis Using Quantitative Techniques
* (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch10
* Author: Marvin A. Titus
* Date: June 2026
* NOTE: Code development was assisted by Claude (Anthropic). The author
* provided specifications and reviewed, tested, and validated all code.
*========================================================================
* Called by Stata_code10.do AFTER Georgia_DiD.do.
* Inherits from the master script: $graphs_dir, $tables_dir, $syntax_dir,
* the open log, version 19, and set scheme s2mono. Does NOT clear/reset
* those settings. Loads its own dataset (Example_10_7_3.csv).
*
* PURPOSE
*   Demonstrate TWO estimators for the same three-state staggered adoption
*   design (48-state panel, FY 2001-2024), both estimated on the identical
*   panel, cohort variable, outcome, and never-treated comparison group --
*   so that any difference in results reflects the estimator, not the data.
*
*   10.7.4.1 (jwdid/ETWFE): fits a single FE regression saturated with
*     cohort x time interactions, recovering heterogeneity-robust ATT(g,t)
*     that align with the Callaway-Sant'Anna (2021) doubly-robust estimates
*     from Section 10.7.
*
*   10.7.4.2 (lwdid): residualizes each unit's outcome against its own
*     pre-treatment mean (rolling=demean) or linear trend (rolling=detrend),
*     then estimates ATT(g,t) via cross-sectional regressions in each
*     post-treatment period. The detrend variant speaks directly to the
*     pre-trend drift documented in the Section 10.7.1 event study.
*
*   Treated states (gyear = treatment cohort):
*     Georgia      (FIPS 13)  -> 2013
*     Wisconsin    (FIPS 55)  -> 2018
*     Pennsylvania (FIPS 42)  -> 2022
*   Never-treated comparison units: remaining states (gyear = 0).
*
*   Outcome:  lngenop  (log general public higher education operations)
*   Controls: $controls = lntotsup lnfinaid lntuifee lnfte
*
* SPECIFICATION NOTE (important, 10.7.4.1 / jwdid)
*   With only THREE treated cohorts and FOUR time-varying covariates, the
*   FULLY saturated ETWFE (covariates interacted with every cohort x year
*   cell) is rank-deficient: the covariate-interaction block is dropped for
*   collinearity, the variance matrix becomes singular, and standard errors
*   are missing. We therefore use:
*     (a) the  never  option  -> never-treated as the clean control group
*         (matches xthdidregress with a balanced/time-constant-control panel);
*     (b)  hettype(cohort)    -> heterogeneity restricted to cohort level,
*         so covariates enter as additive Mundlak-style corrections rather
*         than fully interacting; the model is identified and SEs estimable.
*   A bare (no-covariate) specification is run first as a design check.
*
* SPECIFICATION NOTE (10.7.4.2 / lwdid)
*   lwdid's small-N exact-inference mode (the "small" option) does not
*   currently support covariates under staggered adoption, so no covariate-
*   adjusted lwdid analog to the jwdid 4b specification is run. Both lwdid
*   specifications below (demean, detrend) are unconditional, matching the
*   jwdid 4a specification as the fair comparison point.
*========================================================================

*------------------------------------------------------------------------
* Defensive fallbacks so the script also runs standalone.
* When called by the master these globals already exist and are unchanged.
*------------------------------------------------------------------------
if "$graphs_dir" == "" {
    global graphs_dir "Output/graphs"
    capture mkdir "Output"
    capture mkdir "Output/graphs"
}
if "$tables_dir" == "" {
    global tables_dir "Output/tables"
    capture mkdir "Output/tables"
}

* Confirm jwdid is available (the master installs it; this is a safety net).
capture which jwdid
if _rc {
    di as error "jwdid not found. Run:  ssc install jwdid, replace"
    exit 198
}

* Confirm lwdid is available AND current (used in Section 10.7.4.2 below).
* NOTE: "capture which lwdid" only confirms SOME version is installed, not
* the current one. An outdated cached copy can lack options (e.g. title())
* added in later releases and will fail with "option X not allowed" even
* though X is documented. Force a fresh install to avoid this.
capture which lwdid
if _rc {
    di as text "lwdid not found. Installing..."
    ssc install lwdid, replace
}
else {
    di as text "lwdid found; reinstalling to ensure current version (>= 2.4)..."
    ssc install lwdid, replace
}
capture which lwdid
if _rc {
    di as error "lwdid install failed. Run manually:  ssc install lwdid, replace"
    exit 198
}

*========================================================================
* 1. IMPORT EXPANDED 48-STATE PANEL
*========================================================================
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch10/Example_10_7_3.csv" ///
     "Example_10_7_3.csv", replace
import delimited "Example_10_7_3.csv", clear

*========================================================================
* 2. CONSTRUCT LOGGED OUTCOME AND CONTROLS
*========================================================================
gen lngenop  = log(generalpublicoperations)
gen lntotsup = log(totalstatesupport)
gen lnfinaid = log(totalfinancialaid)
gen lntuifee = log(nettuitionandfeerevenue)
gen lnfte    = log(netfteenrollment)

drop if missing(lngenop, lntotsup, lnfinaid, lntuifee, lnfte)

global controls "lntotsup lnfinaid lntuifee lnfte"

*========================================================================
* 3. STAGGERED TREATMENT-COHORT VARIABLE
*   gvar = first treatment period; 0 for never-treated.
*========================================================================
gen gyear = 0
replace gyear = 2013 if fips == 13     // Georgia
replace gyear = 2018 if fips == 55     // Wisconsin
replace gyear = 2022 if fips == 42     // Pennsylvania

xtset fips fy

*========================================================================
* SECTION 10.7.4.1 — EXTENDED TWO-WAY FIXED EFFECTS (ETWFE) via jwdid
*========================================================================

*========================================================================
* 4a. ETWFE — BASELINE, NO COVARIATES (design check)
*   The "never" option uses never-treated units as the control group.
*   This run is fully identified with valid SEs; it confirms the
*   panel/cohort structure is sound before covariates are introduced.
*========================================================================
di as text _n "=== ETWFE (no covariates, never-treated controls) ==="
jwdid lngenop, ivar(fips) tvar(fy) gvar(gyear) never

estat simple
estat group
estat event

*------------------------------------------------------------------------
* Store Model 4a aggregates for the side-by-side table (Section 6b).
* estat leaves results in r(b)/r(V); we capture overall + group ATTs.
*------------------------------------------------------------------------
estat simple
scalar a_simple_b  = r(b)[1,1]
scalar a_simple_se = sqrt(r(V)[1,1])
estat group
matrix A_group_b  = r(b)
matrix A_group_V  = r(V)

*========================================================================
* 4b. ETWFE — WITH COVARIATES, COHORT-LEVEL HETEROGENEITY
*   hettype(cohort) restricts ATT heterogeneity to the cohort dimension,
*   so the four controls enter as additive corrections rather than being
*   interacted with every cohort x year cell. This avoids the rank
*   deficiency / singular VCE seen under the fully saturated default and
*   yields estimable, state-clustered standard errors.
*========================================================================
di as text _n "=== ETWFE (covariates, never-treated controls, hettype(cohort)) ==="
jwdid lngenop $controls, ivar(fips) tvar(fy) gvar(gyear) ///
    never hettype(cohort) cluster(fips)

*========================================================================
* 5. POST-ESTIMATION AGGREGATIONS
*   Directly comparable to the csdid estat output in Section 10.7.
*========================================================================
* Overall ATT (simple average across post-treatment ATT(g,t))
estat simple

* Group-specific ATT (one per treatment cohort: 2013, 2018, 2022)
estat group

* Calendar-time ATT (one per calendar year)
estat calendar

* Event-study (dynamic effects relative to treatment onset)
estat event

*------------------------------------------------------------------------
* Store Model 4b aggregates for the side-by-side table (Section 6b).
*------------------------------------------------------------------------
estat simple
scalar b_simple_b  = r(b)[1,1]
scalar b_simple_se = sqrt(r(V)[1,1])
estat group
matrix B_group_b  = r(b)
matrix B_group_V  = r(V)

*========================================================================
* 6. EVENT-STUDY PLOT (built manually for full Springer B&W styling)
*   "estat event, plot" creates a bare graph but its plot option does NOT
*   accept title()/ytitle()/xtitle(), and graph display rejects them too.
*   We therefore pull the event-time coefficients from r(table) and draw
*   the figure ourselves with twoway. Built on the Model 4b (covariate-
*   adjusted) results, which are the preferred specification.
*
*   ROBUST AXIS: an earlier version parsed event time from r(b) column
*   NAMES, but estat event stores them as equation-style strings (not bare
*   integers), so real("`name'") returned missing and every row was dropped
*   -> an empty plot. We instead read the ATT/SE values from r(table) by
*   POSITION and reconstruct the event axis deterministically: estat event
*   returns periods in ascending order, and the omitted reference period is
*   stored as MISSING in r(table) (displayed as "0 (omitted)"). That row is
*   event time -1. This is format-proof across jwdid/Stata versions.
*------------------------------------------------------------------------
estat event                       // populate r(b)/r(V)/r(table)
matrix Rb = r(b)                  // 1 x nE row vector of event-time ATTs
matrix RV = r(V)                  // nE x nE variance matrix
local nE  = colsof(Rb)

* The event coefficients are returned in ascending order of event time.
* The omitted reference period is the unique column with zero (or missing)
* variance on the r(V) diagonal -- an omitted coefficient is structurally
* non-estimated, so its variance is not a small float but exactly 0/missing.
* We locate it, set its event time to -1, and number the rest by offset.
local refpos = 0
forvalues i = 1/`nE' {
    if (RV[`i',`i'] == 0 | missing(RV[`i',`i']))  local refpos = `i'
}
* Fallback only if detection fails: assume reference precedes the first
* at-treatment period at the midpoint of the returned sequence.
if `refpos' == 0  local refpos = `=ceil(`nE'/2)'
di as text "Event-study reference period located at position `refpos' of `nE'."

preserve
    clear
    set obs `nE'
    gen long   _ord  = _n
    gen double att   = .
    gen double se    = .
    forvalues i = 1/`nE' {
        replace att = Rb[1,`i']            in `i'
        replace se  = sqrt(RV[`i',`i'])    in `i'
    }
    * Place the reference column at event time -1. A single uniform offset
    * works for every row: the reference row (refpos) maps to -1, the row
    * after it to 0 (the at-treatment period), and so on. Verified against
    * the estat event display (-21..-1 omitted..0..+11).
    gen double event = _ord - `refpos' - 1
    drop _ord

    gen double lo = att - 1.96*se
    gen double hi = att + 1.96*se
    sort event

    twoway (rcap hi lo event, lcolor(gs8))                                  ///
           (connected att event, lcolor(black) mcolor(black) msymbol(O)) , ///
        yline(0, lpattern(dash) lcolor(gs10))                              ///
        xline(-0.5, lpattern(dot) lcolor(gs8))                            ///
        title("ETWFE Event Study: Staggered Adoption")                    ///
        subtitle("Covariate-adjusted, never-treated controls")            ///
        ytitle("ATT on log operations")                                   ///
        xtitle("Years relative to treatment")                             ///
        legend(off) name(fig10_6, replace) scheme(s2mono)

    capture graph export "$graphs_dir/fig10_6.png", replace width(2000)
    capture graph save   "$graphs_dir/fig10_6.gph", replace
restore

*========================================================================
* 6b. COMPARISON TABLE: ETWFE WITHOUT vs WITH COVARIATES
*   Springer-formatted table contrasting the unconditional (4a) and
*   covariate-adjusted (4b) ETWFE overall and cohort-specific ATTs.
*------------------------------------------------------------------------
* Assemble a results matrix: rows = {Overall, G2013, G2018, G2022},
* columns = {b(4a), se(4a), b(4b), se(4b)}.
matrix T = J(4, 4, .)
matrix T[1,1] = a_simple_b
matrix T[1,2] = a_simple_se
matrix T[1,3] = b_simple_b
matrix T[1,4] = b_simple_se
forvalues g = 1/3 {
    matrix T[`=`g'+1', 1] = A_group_b[1,`g']
    matrix T[`=`g'+1', 2] = sqrt(A_group_V[`g',`g'])
    matrix T[`=`g'+1', 3] = B_group_b[1,`g']
    matrix T[`=`g'+1', 4] = sqrt(B_group_V[`g',`g'])
}
matrix rownames T = Overall G2013_Georgia G2018_Wisconsin G2022_Pennsylvania
matrix colnames T = ATT_nocov SE_nocov ATT_cov SE_cov

di _n as text "{hline 72}"
di as text "ETWFE ATT estimates: unconditional (4a) vs covariate-adjusted (4b)"
di as text "{hline 72}"
matlist T, format(%9.4f)

* Export to a Springer-style RTF table (requires estout; master installs it).
capture which esttab
if !_rc {
    capture esttab matrix(T, fmt(%9.4f)) using "$tables_dir/tab10_7_etwfe.rtf", ///
        replace title("ETWFE ATT: Unconditional (4a) vs Covariate-Adjusted (4b)") ///
        addnotes("Never-treated controls; SEs clustered by state (fips)." ///
                 "Cols 1-2: no covariates. Cols 3-4: hettype(cohort) adjusted.")
}

di as text _n "ETWFE / jwdid section complete."
di as text "Figure: $graphs_dir/fig10_6.(png|gph)"
di as text "Table:  $tables_dir/tab10_7_etwfe.rtf"

*========================================================================
* INTERPRETATION (for chapter prose) — Section 10.7.4.1 (jwdid/ETWFE)
*------------------------------------------------------------------------
* - The unconditional ETWFE (4a) reproduces the CS-DiD pattern: overall
*   ATT ~0.051 (p=0.001), driven by G2013/Georgia (~0.114, p<0.001), with
*   G2018 null and G2022 negative. Its event study shows large, significant
*   PRE-treatment leads, so the estimate rests on a questionable parallel-
*   trends assumption.
* - The covariate-adjusted ETWFE (4b) absorbs those differential trends and
*   collapses the overall ATT to a precise null (~ -0.002, p=0.90).
* - Report BOTH: 4a as the unconditional benchmark comparable to csdid, 4b
*   as the adjusted specification showing sensitivity to confounders.
* - The fully saturated default ETWFE is NOT appropriate here: with three
*   treated cohorts and four time-varying covariates it is rank-deficient.
*   Note this as a practical caution on ETWFE in small-T, few-cohort panels.
*========================================================================

*========================================================================
* SECTION 10.7.4.2 — ROLLING DIFFERENCE-IN-DIFFERENCES (lwdid)
*   We next estimate the same design using lwdid, a different
*   transformation-based estimator. Unlike jwdid's saturated regression
*   on cohort-by-period indicators, lwdid residualizes each unit's outcome
*   against its own pre-treatment path (mean or trend) and then estimates
*   ATT(g,t) via cross-sectional regressions in each post-treatment period.
*   We use the SAME panel, cohort variable, outcome, and never-treated
*   comparison-group choice as the jwdid specification above (10.7.4.1),
*   so any difference in results reflects the estimator, not the data.
*========================================================================

*========================================================================
* 7a. lwdid — UNCONDITIONAL, ROLLING(DEMEAN)
*   Residualizes each unit's outcome against the average of ALL its
*   pre-treatment periods ("lags-only" reference point), in contrast to
*   csdid's single-pre-period anchor. method(ra) = regression adjustment,
*   the large-N unconditional analog to jwdid's 4a specification.
*   NOTE 1: lwdid's graph option sets ytitle("WATT(r)") and
*   xtitle("Time to Treatment (r)") internally and does not expose them
*   as separate top-level options; gopts() is reserved for subtitle/name
*   only to avoid an "option repeated" error from Stata's twoway.
*   NOTE 2 (lwdid v2.4 bug): the TOP-LEVEL "lwdid" dispatcher program's
*   syntax line omits TITLE(string), even though the lwdid_large
*   subroutine it calls accepts and uses it. Because Stata validates
*   options against the dispatcher's syntax line BEFORE the subroutine
*   ever runs, passing title() fails with "option title() not allowed"
*   regardless of version/cache state. There is no scriptable way to
*   retitle a twoway graph after the fact (Stata graphs are immutable
*   once drawn), so we omit title() and accept lwdid's own internal
*   default, built from method()/rolling(): "lwdid: ra (demean)".
*========================================================================
di as text _n "=== lwdid (rolling=demean, method=ra, never-treated controls) ==="
lwdid lngenop, ivar(fips) tvar(fy) gvar(gyear) ///
    rolling(demean) method(ra) never attgt graph ///
    scheme(s2mono) save("$tables_dir/lwdid_demean_watt.dta") ///
    gopts(subtitle("Unconditional, never-treated controls") name(fig10_12, replace))

capture graph export "$graphs_dir/fig10_12.png", replace width(2000)
capture graph save   "$graphs_dir/fig10_12.gph", replace

*------------------------------------------------------------------------
* Store demean aggregates for the side-by-side table (Section 7c).
* NOTE: lwdid_large (unlike the small-N paths) does NOT post e(att)/
* e(se_att) -- the large-N path ends without an ereturn. The overall
* post-treatment effect is the "Post_avg" row of the saved WATT(r) table,
* so we reload save() output and pull it from there instead.
*------------------------------------------------------------------------
preserve
    use "$tables_dir/lwdid_demean_watt.dta", clear
    qui su watt if effect == "Post_avg", meanonly
    scalar l_demean_b  = r(mean)
    qui su se if effect == "Post_avg", meanonly
    scalar l_demean_se = r(mean)
restore

*========================================================================
* 7b. lwdid — UNCONDITIONAL, ROLLING(DETREND)
*   Removes each unit's pre-treatment LINEAR TREND (not just its mean)
*   before estimation. This speaks directly to the pre-trend drift
*   documented in the Section 10.7.1 event study and in the significant
*   pre-treatment leads noted under jwdid 4a above: if that drift is
*   driving the unconditional ATT, detrending should move the estimate
*   toward the null, the same direction as jwdid's covariate adjustment.
*   NOTE (lwdid v2.4 bug): see the note under 7a above -- title() is
*   omitted here for the same reason (rejected by the dispatcher's
*   syntax line before lwdid_large ever sees it), and there is no
*   scriptable way to retitle a twoway graph after the fact. We accept
*   lwdid's own internal default title: "lwdid: ra (detrend)".
*========================================================================
di as text _n "=== lwdid (rolling=detrend, method=ra, never-treated controls) ==="
lwdid lngenop, ivar(fips) tvar(fy) gvar(gyear) ///
    rolling(detrend) method(ra) never attgt graph ///
    scheme(s2mono) save("$tables_dir/lwdid_detrend_watt.dta") ///
    gopts(subtitle("Unconditional, never-treated controls") name(fig10_13, replace))

capture graph export "$graphs_dir/fig10_13.png", replace width(2000)
capture graph save   "$graphs_dir/fig10_13.gph", replace

*------------------------------------------------------------------------
* Store detrend aggregates for the side-by-side table (Section 7c).
* Same caveat as the demean block above: pull "Post_avg" from save().
*------------------------------------------------------------------------
preserve
    use "$tables_dir/lwdid_detrend_watt.dta", clear
    qui su watt if effect == "Post_avg", meanonly
    scalar l_detrend_b  = r(mean)
    qui su se if effect == "Post_avg", meanonly
    scalar l_detrend_se = r(mean)
restore

* NOTE: lwdid's small-N exact-inference mode ("small") does not currently
* support covariates under staggered adoption, so a covariate-adjusted
* lwdid analog to jwdid's 4b specification is not run here.

*========================================================================
* 7c. COMPARISON TABLE: jwdid (4a) vs lwdid (demean, detrend)
*   Springer-formatted table contrasting the unconditional jwdid overall
*   ATT against the two unconditional lwdid specifications. All three
*   rows share the same panel, cohort variable, outcome, and never-
*   treated comparison group; only the estimator differs.
*------------------------------------------------------------------------
matrix T2 = J(3, 2, .)
matrix T2[1,1] = a_simple_b
matrix T2[1,2] = a_simple_se
matrix T2[2,1] = l_demean_b
matrix T2[2,2] = l_demean_se
matrix T2[3,1] = l_detrend_b
matrix T2[3,2] = l_detrend_se
matrix rownames T2 = jwdid_4a_nocov lwdid_demean lwdid_detrend
matrix colnames T2 = ATT SE

di _n as text "{hline 72}"
di as text "Overall ATT, staggered consolidation: jwdid vs lwdid (unconditional)"
di as text "{hline 72}"
matlist T2, format(%9.4f)

capture which esttab
if !_rc {
    capture esttab matrix(T2, fmt(%9.4f)) using "$tables_dir/tab10_7_lwdid.rtf", ///
        replace title("Overall ATT: jwdid (unconditional) vs lwdid (demean, detrend)") ///
        addnotes("Never-treated controls throughout; lwdid uses method(ra)." ///
                 "Same panel, cohort variable, and outcome as jwdid 4a above.")
}

di as text _n "lwdid section complete."
di as text "Figures: $graphs_dir/fig10_12.(png|gph), $graphs_dir/fig10_13.(png|gph)"
di as text "Table:   $tables_dir/tab10_7_lwdid.rtf"

*========================================================================
* INTERPRETATION (for chapter prose) — Section 10.7.4.2 (lwdid)
*------------------------------------------------------------------------
* RESULTS:
*   jwdid 4a (unconditional ETWFE):    Overall ATT =  0.0510 (SE .0160, p=.001)
*   jwdid 4b (covariate-adjusted):     Overall ATT = -0.0016 (SE .0124, p=.897)
*   lwdid demean (unconditional):      Overall ATT = -0.0624 (SE .0228, p=.006)
*   lwdid detrend (unconditional):     Overall ATT =  0.1161 (SE .0271, p<.001)
*
* - The two unconditional, never-treated-controls specifications -- jwdid 4a
*   and lwdid(demean) -- should be the closest comparison in this table, since
*   neither uses covariates. They do NOT agree, not even on sign (+0.051 vs.
*   -0.062). Because both share the identical panel, cohort variable, outcome,
*   and comparison group, this disagreement is attributable to the estimator's
*   mechanics (saturated cohort x period regression vs. residualizing against
*   each unit's own pre-treatment mean), not to the data or sample.
*
* - lwdid(detrend) does NOT move toward jwdid 4b's near-zero estimate the way
*   we hypothesized going in. Instead it produces the LARGEST and most
*   precisely estimated effect in the table (0.116, p<.001) -- the opposite
*   of "detrending corroborates the covariate-adjustment null via a different
*   mechanism." Both adjustments (Mundlak-style covariates in 4b; unit-
*   specific linear detrending in lwdid) move the estimate, but in opposite
*   directions, which is itself the finding: they are NOT correcting for the
*   same source of pre-trend bias.
*
* - Cohort-level detail (post-period ATT(g,t) cells; see attgt output) shows
*   WHY: Georgia (g=2013) and Pennsylvania (g=2022) account for nearly all of
*   the movement across specifications.
*     - Georgia is positive everywhere but the SIZE varies enormously: 0.114
*       (jwdid 4a) vs. a post-period range of -0.058 (2013) rising to +0.150
*       (2023) under demean, vs. a climbing 0.014 (2013, n.s.) -> 0.337 (2023,
*       p<.001) under detrend. The detrend run's drift -- not significant in
*       the first post-treatment year but growing steadily significant
*       thereafter -- suggests Georgia had a declining pre-treatment linear
*       trajectory that demeaning alone does not fully net out, and that
*       removing it mechanically widens the post-treatment gap over time.
*     - Pennsylvania flips sign across specifications: negative in jwdid 4a
*       (-0.074) and jwdid 4b (-0.070, n.s.), negative under lwdid demean
*       (-0.23 to -0.27), but POSITIVE and significant under lwdid detrend
*       (0.08 to 0.14). This is the single largest point of disagreement
*       among all cohort-by-estimator combinations.
*     - Wisconsin (g=2018) is the only cohort that is consistently near null
*       under THREE of the four specifications (jwdid 4a: -0.004; jwdid 4b:
*       0.001; lwdid detrend: -0.04 to +0.04, none significant) but is sharply
*       NEGATIVE under lwdid demean (-0.13 to -0.25, all significant). Even
*       the "stable" cohort is not actually stable across every estimator.
*
* - The lwdid Pre_avg row provides an additional diagnostic neither jwdid
*   specification offers directly: under demean, Pre_avg = 0.0087 (p<.001) --
*   small but statistically distinguishable from zero, indicating some
*   residual pre-trend survives demeaning. Under detrend, Pre_avg = -0.0027
*   (p=.007) -- also significant, and of the OPPOSITE sign. Neither
*   transformation fully eliminates a detectable pre-trend; they simply trade
*   one small, signed residual for another, and the post-period estimate
*   appears sensitive to which residual remains.
*
* - TAKEAWAY FOR THE CHAPTER: this is not a robustness check that confirms
*   the Section 10.7 consolidation result. It is evidence that the staggered-
*   adoption ATT for this three-state design is highly sensitive to how the
*   pre-treatment counterfactual is constructed -- more sensitive than the
*   TWFE-vs-LASSO-DiD contrast already documented in Sections 10.3-10.4.
*   Readers should treat the overall ATT from ANY single staggered-adoption
*   estimator in this design with caution, and the chapter should present
*   jwdid and lwdid side by side as competing answers, not as one estimator
*   validating another.
*========================================================================
* END OF ETWFE.do
*========================================================================
