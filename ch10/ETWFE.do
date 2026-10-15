*========================================================================
* ETWFE.do
* Section 10.7.4: Extended Two-Way Fixed Effects (ETWFE) via jwdid
*   (Rios-Avila, Nagengast & Yotov; estimator: Wooldridge 2021, 2023)
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
* version 19, and set scheme s2mono. Does NOT clear/reset those settings.
* Loads its own dataset (Example_10_7_3.csv).
* Logging: opens its OWN named log (etwfe_log) rather than relying on the
* master's, so a standalone run is recorded too. Named logs coexist with
* the master's log and closing this one does not close it.
*
* PURPOSE
*   Estimate ETWFE for the three-state staggered adoption design
*   (48-state panel, FY 2001-2024), recovering heterogeneity-robust
*   ATT(g,t) that align with the Callaway-Sant'Anna (2021) doubly-robust
*   estimates from Section 10.7.
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
* REVISION (Aug 2026, after the 13 Aug run)
*   - Aug 2026 (19th): $graphs_dir, $tables_dir and $log_dir are now DERIVED
*     from $out_root unconditionally rather than accepted when absolute.  A
*     stale ABSOLUTE global survives an "is it absolute" guard -- that is how
*     a standalone RDD.do run wrote its log to .../Output/log.  An inherited
*     value that differs is announced before being replaced.  No estimation,
*     sample, or figure changed.  Version stamp: 2026-08-19a.
*   - $graphs_dir and $tables_dir now derive from a single username-branched
*     $out_root, and a stale RELATIVE value inherited from an earlier run in
*     the same Stata session is discarded rather than trusted.  That stale
*     global is what kept sending tab10_7_etwfe.rtf to the working directory.
*   - The log is now opened BEFORE the directories are resolved, so the
*     resolution is recorded; previously those diagnostics ran unlogged.
*   - -capture- dropped from the two graph export/save lines so the log
*     records whether the figure was written.
*   - Named log etwfe_log added (see below).
*
* FIGURE NAMING (Aug 2026)
*   Chapter 10 gained Figs. 10.2 and 10.3 in Section 10.2, so manuscript
*   figure numbers now run 10.1-10.9 and end with Fig. 10.9 (staggered
*   adoption, Section 10.7.3). Section 10.7.4 has NO numbered manuscript
*   figure: the event study exported here is a diagnostic. Under the
*   convention shared with RDD.do and Georgia_DiD.do,
*     fig10_<N>_<name>      = MANUSCRIPT FIGURE number
*     fig10_<N>_<M>_<name>  = SECTION number, diagnostic only
*   this graph was renamed fig10_7_3 -> fig10_7_4_etwfe_event_study_Stata:
*   it belongs to Section 10.7.4, not 10.7.3, and the _Stata suffix matches
*   the sibling scripts so the R export never overwrites it.
*   tab10_7_etwfe.rtf is likewise a diagnostic; Chapter 10 has one numbered
*   table (Table 10.1, the Section 10.3.4 robustness table).
*========================================================================

*------------------------------------------------------------------------
* Output root, log, then output directories -- IN THAT ORDER
*------------------------------------------------------------------------
* ORDERING MATTERS.  In the 14 Aug 2026 run the directory diagnostics ran
* BEFORE the log was opened, so nothing about $graphs_dir / $tables_dir was
* recorded and the run looked silent.  The log is now opened first and the
* directory resolution is echoed into it.
*
* One out_root branching on username feeds every output directory.  It is a
* GLOBAL, not a local: locals do not survive across the -log using- call in
* some execution contexts, and the value is needed further down.
global out_root "Output"
if c(username) == "marvi" {
    global out_root "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output"
}
capture mkdir "$out_root"

* DERIVED from $out_root unconditionally, not merely set when empty.  Stata
* globals persist across do-files within a session, so an "is it empty" test
* lets a stale value through -- and so does an "is it absolute" test, which
* is how a standalone RDD.do run wrote its log to .../Output/log on
* 19 Aug 2026.  Derivation is the only rule nothing stale survives.
local cur "$log_dir"
global log_dir "$out_root/logs"
if "`cur'" != "" & "`cur'" != "$log_dir" {
    di as text "ETWFE.do: replacing inherited log_dir (`cur')"
}
capture mkdir "$log_dir"

* -capture- so an aborted earlier run that left etwfe_log open is harmless.
* A NAMED log coexists with any log the master driver already has open, and
* closing this one does not close the master's.  Opened before the jwdid check
* so a missing-package exit is still recorded.
capture log close etwfe_log
log using "$log_dir/ETWFE.log", replace text name(etwfe_log)

di as text "ETWFE.do: logging to $log_dir/ETWFE.log"

* ------------------------------------------------------------------------
* VERSION STAMP -- printed INSIDE the log on purpose.
* If a log does not show this line, an older copy of the script ran.  That
* is the fastest way to settle "did my edit take effect?" without diffing
* files.  Bump the date when the script changes.
* ------------------------------------------------------------------------
di as text "ETWFE.do: version 2026-08-19a"
di as text "ETWFE.do: working directory = " c(pwd)
di as text "ETWFE.do: out_root   = $out_root"
di as text "ETWFE.do: graphs_dir = $graphs_dir"
di as text "ETWFE.do: tables_dir = $tables_dir"
di as text "ETWFE.do: log_dir    = $log_dir"

*------------------------------------------------------------------------
* Graph and table directories (resolved INSIDE the log)
*------------------------------------------------------------------------
* Stata globals persist across do-files within a session, so a value set by
* an earlier run survives into this one.  Both earlier rules leaked:
*   "set it if it is empty"       -- a stale RELATIVE $tables_dir survived,
*                                    which is why tab10_7_etwfe.rtf kept
*                                    landing in "Output/tables" relative to
*                                    the working directory.
*   "accept it if it is ABSOLUTE" -- a stale ABSOLUTE value survives too.  A
*                                    standalone RDD.do run on 19 Aug 2026
*                                    inherited $log_dir = .../Output/log and
*                                    wrote its log there, because the value
*                                    passed the absolute test.
*
* Both directories are therefore DERIVED from $out_root unconditionally.
* Nothing inherited survives, so the paths cannot drift.  This costs nothing
* under the master, which derives the same directories from the same
* $out_root.  To redirect output, change $out_root, not these globals.
foreach D in graphs tables {
    local cur "${`D'_dir}"
    global `D'_dir "$out_root/`D'"
    if "`cur'" != "" & "`cur'" != "${`D'_dir}" {
        di as text "ETWFE.do: replacing inherited `D'_dir (`cur')"
    }
    capture mkdir "${`D'_dir}"
}

di as text "ETWFE.do: out_root    = $out_root"
di as text "ETWFE.do: graphs_dir  = $graphs_dir"
di as text "ETWFE.do: tables_dir  = $tables_dir"

* Confirm jwdid is available (the master installs it; this is a safety net).
capture which jwdid
if _rc {
    di as error "jwdid not found. Run:  ssc install jwdid, replace"
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
        legend(off) name(fig10_7_4, replace) scheme(s2mono)

    * Diagnostic figure - Section 10.7.4 has no numbered manuscript figure.
    * NOT -capture-: capture suppresses the "file ... saved as PNG format"
    * confirmation, leaving the log with no evidence the figure was written.
    graph export "$graphs_dir/fig10_7_4_etwfe_event_study_Stata.png", ///
        replace width(2000)
    graph save   "$graphs_dir/fig10_7_4_etwfe_event_study_Stata.gph", ///
        replace
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
* NOT -capture- on the esttab call itself: a captured failure produced a run
* that echoed a Table: path while writing no file. A real failure should stop
* the script. The -which- guard is kept but now reports when it fires.
capture which esttab
if _rc {
    di as error "esttab not found -- table NOT written. Run: ssc install estout, replace"
}
else {
    esttab matrix(T, fmt(%9.4f)) using "$tables_dir/tab10_7_etwfe.rtf", ///
        replace title("ETWFE ATT: Unconditional (4a) vs Covariate-Adjusted (4b)") ///
        addnotes("Never-treated controls; SEs clustered by state (fips)." ///
                 "Cols 1-2: no covariates. Cols 3-4: hettype(cohort) adjusted.")

    * Confirm the file exists rather than trusting the command's silence.
    capture confirm file "$tables_dir/tab10_7_etwfe.rtf"
    if _rc {
        di as error "tab10_7_etwfe.rtf was NOT created in $tables_dir"
    }
    else {
        di as text "   tab10_7_etwfe.rtf written to $tables_dir"
    }
}

di as text _n "ETWFE / jwdid section complete."
di as text "Figure: $graphs_dir/fig10_7_4_etwfe_event_study_Stata.(png|gph)"
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

* Close the named log opened at the top. -capture- so that re-running the
* script, or a master driver that has already closed it, is harmless.
capture log close etwfe_log

* END OF ETWFE.do
*========================================================================
