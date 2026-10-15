*========================================================================
* Chapter 10 – Section 10.2: Regression Discontinuity Design
*             The Merit-Based Scholarship
* Higher Education Policy Analysis Using Quantitative Techniques
* (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch10
* Author: Marvin A. Titus
* Date: May 2026
* NOTE: Code development was assisted by Claude (Anthropic). The author
* provided specifications and reviewed, tested, and validated all code.
*========================================================================
* Called by: Stata_code10.do  (inherits $graphs_dir, set scheme)
* Logging:   opens its OWN named log (rdd_log), so it records a complete
*            run either way and never collides with the master's log.
* Standalone: can also be run directly; uses fallback paths if needed.
*
* Required packages: rdrobust, rddensity, lpdensity, cmogram
*   ssc install rdrobust,  replace
*   net install rddensity, ///
*       from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace
*       (NOT ssc install -- the SSC copy can omit the ancillary
*        rddensity_fun.do, which rddensity runs internally)
*   ssc install cmogram,   replace
*   net install lpdensity, ///
*       from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata) replace
*
* Sections:
*   10.2.1   Synthetic data generation (N = 4,000; seed 20260510)
*   10.2.2   Density continuity test (rddensity) & covariate balance
*   10.2.3   Binned scatterplots (cmogram)
*   10.2.4   Sharp RD: OLS benchmark, manual local linear, rdrobust,
*            covariate-adjusted rdrobust (female firstgen urm act_score income_cat)
*   10.2.5   Bandwidth sensitivity (9-point grid)
*   10.2.6   Polynomial order sensitivity (p = 1, 2, 3)
*   10.2.7   Fuzzy RD: first stage, reduced form, Wald/2SLS for all outcomes
*            (persist_fuzzy, credits_y1, cgpa_y1); covariate-adjusted fuzzy LATE;
*            consolidated summary table
*   10.2.8   Validity checks: placebo cutoffs, donut RD, augmented, subgroup
*   10.2.9   Publication-quality RD plots (rdplot); persist_fuzzy rdplot +
*            fuzzy LATE comparison figure
*   10.2.10  Summary table; save Example_10_2_1.dta
*
* NOTE ON NUMBERING: the section numbers above are INTERNAL to this
* script and do NOT correspond to the manuscript's subsection numbers.
* The manuscript's Section 10.2 has three subsections: 10.2.1 sharp RDD,
* 10.2.2 fuzzy RDD, 10.2.3 covariate adjustment.
*
* MANUSCRIPT CROSS-REFERENCES (Chapter 10, Section 10.2)
* ------------------------------------------------------
* Four graphs from this script appear as numbered figures in the
* manuscript. They carry fig10_<manuscript figure number>_ prefixes.
* Every other graph exported here is a script-only diagnostic and keeps
* its fig10_2_<n>_ prefix, which encodes this script's section number,
* not a manuscript figure number.
*
*   Manuscript      Script export                        Script section
*   ----------      -------------                        --------------
*   Fig. 10.1       fig10_1_rdd_plot_credits.png         10.2.9
*   Fig. 10.2       fig10_2_rdd_density_test.png         10.2.2
*   Fig. 10.3       fig10_3_rdd_plot_firststage.png      10.2.9
*   Fig. 10.4       fig10_4_fuzzy_late_credits.png       10.2.9
*
* Manuscript results reported from this script:
*   Sharp LATE (credits_y1), unadjusted   3.2999, h* = 0.224   (10.2.4)
*   Sharp LATE (credits_y1), covs         3.3710, h* = 0.225   (10.2.4)
*   Density test (robust)                 T = -0.8925, p = 0.37 (10.2.2)
*   First stage jump                      0.5051, h* = 0.229   (10.2.7)
*   Fuzzy LATE (credits_y1), rdrobust     6.4145, h* = 0.213   (10.2.7)
*   Fuzzy LATE (credits_y1), 2SLS h=0.50  6.2978, N = 2,343    (10.2.7)
*   Fuzzy LATE (credits_y1), covs         6.4624, h* = 0.213   (10.2.7)
*
* CAVEAT carried into the manuscript text: credits_y1 and cgpa_y1 are
* generated from D_sharp, so their fuzzy LATE is a demonstration of the
* Wald mechanics, not a distinct complier effect. Only persist_fuzzy has
* a genuinely fuzzy DGP. Section 10.2.3 of the manuscript now says so.
*
* EQUATION CROSS-REFERENCES (manuscript Section 10.2)
* ---------------------------------------------------
*   Eq. 10.1   Sharp RD estimand (ATE at the cutoff)
*                -> rdrobust <outcome> x, c(0) kernel(triangular)
*                   bwselect(mserd) all                        (10.2.4)
*   Eq. 10.2   Sharp estimand written as an expectation        (no command)
*   Eq. 10.3   Assignment rule D_i = 1(X_i >= c)
*                -> gen byte D_sharp = (x >= 0)                (10.2.1)
*   Eq. 10.4   Two-regression-line / interacted OLS form; printed in the
*              manuscript to make the logic visible, NOT the estimator
*              used for any reported number                    (10.2.4)
*   Eq. 10.5   Fuzzy RD (Wald) estimand
*                -> rdrobust <outcome> x, c(0) fuzzy(D_fuzzy)  (10.2.7)
*                -> manual 2SLS with in_bw2 / tri_wt2 / x_Ds2  (10.2.7)
*
* FIGURE FORMAT: Springer requires GRAYSCALE figures.  -set scheme s2mono-
* covers every graph drawn by twoway, rdplot and cmogram.  It does NOT
* cover rddensity, which hardcodes red (left of cutoff) and blue (right)
* for its fitted lines, confidence regions and histogram bars.  Fig. 10.2
* therefore sets those explicitly through rddensity's own plot options;
* see Section 10.2.2.
*
* KNOWN DIVERGENCES from the printed chapter listings (deliberate)
* ---------------------------------------------------------------
*   1. The chapter prints -rddensity x, c(0) plot- bare.  This script
*      adds graph_opt() and the grayscale plot options above.  The
*      simplification is intentional: the listing shows the command, the
*      script produces the publication figure.
*   2. The chapter's install listing prints -ssc install rddensity,
*      replace-.  That is a CHAPTER DEFECT, not a script one: the SSC
*      copy can omit the ancillary rddensity_fun.do and aborts with
*      r(601).  Use the net install line below.  The chapter listing
*      needs correcting; the script is already right.
*
* CHANGE LOG
* ----------
* v1  May 2026 — initial draft
* v2  May 2026 — Fuzzy RD extended per user specification:
*                all three outcomes (persist_fuzzy, credits_y1, cgpa_y1)
*                looped through reduced form, rdrobust fuzzy LATE, and
*                manual 2SLS using specified variable names (in_bw2,
*                tri_wt2, x_Ds2); consolidated fuzzy summary table;
*                fig10_2_8 (persist_fuzzy rdplot) and fig10_2_9
*                (fuzzy LATE comparison) added to Section 10.2.9.
* v3  May 2026 — Covariate-adjusted models added per user specification:
*                rdrobust with covs(female firstgen urm act_score income_cat)
*                added to Section 10.2.4 (sharp) and Section 10.2.7
*                (fuzzy LATE); both loop over all three outcomes.
* v4  Aug 2026 — Figure filenames synchronised with the manuscript after
*                Fig. 10.2 (density test) and Fig. 10.3 (first stage)
*                were added to Section 10.2 and everything downstream
*                was renumbered:
*                  fig10_2_1_rdd_density_test    -> fig10_2_rdd_density_test
*                  fig10_2_7_rdd_plot_firststage -> fig10_3_rdd_plot_firststage
*                  fig10_2_fuzzy_late_credits    -> fig10_4_fuzzy_late_credits
*                  name(fig10_2)                 -> name(fig10_4)
*                Manuscript cross-reference block added above. Script-only
*                diagnostic figures are unchanged. No executable logic
*                changed — only export filenames, one graph name, and
*                comments.
* v5  Aug 2026 - Added a log file, matching Georgia_DiD.do v13.  A NAMED
*                log (rdd_log) is opened after -set scheme- and closed at
*                the end of the script, with $log_dir resolved by the same
*                standalone-fallback pattern already used for $graphs_dir.
*                Named logs coexist with the master driver's log, so
*                running through Stata_code10.do and running standalone
*                both produce a complete RDD.log.  No estimation, sample,
*                or figure changed.
* v6  Aug 2026 - $graphs_dir and $log_dir now resolve through a shared
*                $out_root and are accepted from the environment only if
*                ABSOLUTE.  A stale relative global left by an earlier run in
*                the same session used to survive the "is it empty" test and
*                redirect output to the working directory.  Directories are
*                created unconditionally.  No estimation or figure changed.
* v7  Aug 2026 - Synchronised with Chapter10_version_48.docx:
*                (a) Fig. 10.2 (rddensity) now drawn in GRAYSCALE.
*                    rddensity ignores -set scheme s2mono- for its own
*                    plot elements, so esll_opt/eslr_opt, cirl_opt/
*                    cirr_opt and histl_opt/histr_opt now set gray levels
*                    explicitly, and legend(off) suppresses rddensity's
*                    internal series names (__00000G, __000004, ...),
*                    which are unreadable and were printed in the figure.
*                (b) Density-test and covariate-balance comments rewritten
*                    to match the hedged language adopted in the chapter:
*                    the data were generated without sorting, so the test
*                    demonstrates the procedure rather than certifying the
*                    design, and the balance checks are local and not
*                    highly powered.  The "c = 3.25 is non-salient"
*                    rationalization was dropped from the chapter and is
*                    dropped here too.
*                (c) The four instrument-validity assumptions the chapter
*                    now states in Section 10.2.2 added as a comment block
*                    above the fuzzy estimation.
*                (d) "Equation 10.x" placeholders resolved to Eq. 10.5.
*                (e) Equation cross-reference block added to this header.
*                (f) Saved dataset renamed ch10_rdd_hsls09_synthetic.dta
*                    -> Example_10_2_1.dta, the filename the chapter's
*                    data-availability section gives readers.
*                (h) $graphs_dir and $log_dir are now DERIVED from
*                    $out_root unconditionally rather than inherited when
*                    absolute.  A stale ABSOLUTE global survives an
*                    "is it absolute" guard, which is how a 19 Aug 2026
*                    standalone run still wrote to .../Output/log after
*                    (g) below was applied.  An inherited value that
*                    differs is announced before being replaced.
*                (g) Standalone fallback for $log_dir corrected from
*                    "$out_root/log" to "$out_root/logs".  The master
*                    passes an absolute .../Output/logs, so the wrong
*                    value only surfaced when RDD.do was run on its own,
*                    where it silently created a second directory beside
*                    the real one.  Georgia_DiD.do and ETWFE.do already
*                    used "logs".
*                (i) Fig. 10.4 rebuilt.  Three defects: (1) the x axis
*                    ran from ~3.6 to ~9.2, so ZERO WAS NOT ON THE PLOT
*                    even though the chapter's prose says the interval
*                    does not cross zero, and the -xline(0)- drew nothing
*                    because -rcap ..., horizontal- swaps the axes; the
*                    axis is now forced to start at 0 and the reference
*                    line is drawn with -pci-.  (2) The interval was
*                    b +/- 1.96*SE(conventional) while every p-value in
*                    Section 10.2 is e(pv_rb); a -ci_type- switch now
*                    selects the robust bias-corrected bounds (default)
*                    or the conventional ones, and the subtitle names
*                    which is shown.  (3) The note read "Complier LATE =
*                    RF / FS", contradicting the Section 10.2.3 caveat
*                    that credits_y1 comes from the sharp DGP; rewritten.
*                    e(ci_l_rb) / e(ci_r_rb) are now stored in the fuzzy
*                    loop, which is -qui- and previously discarded them.
*                (j) Added -chkfig-, which re-opens each manuscript figure
*                    after export and reports its size, plus an end-of-run
*                    roll call and a -dir- listing of $graphs_dir.  Stata's
*                    own "saved as PNG format" only means the command
*                    returned cleanly; it is not evidence the file is
*                    readable at that path afterwards.
*                (k) Fig. 10.2's note() collapsed to a SINGLE string.  A
*                    two-string note passed through rddensity's graph_opt()
*                    was tokenised one word per line in the export.
*                Version stamp: 2026-08-19f.
*                No estimation, sample, bandwidth or reported number
*                changed.
*========================================================================

*------------------------------------------------------------------------
* Fallback paths when running standalone (not called from Stata_code10.do)
*------------------------------------------------------------------------
* If $graphs_dir is already defined by the parent script, these lines
* have no effect. If running RDD.do directly, they set sensible defaults.

* Resolve $out_root once, then DERIVE every output directory from it.
*
* Two earlier rules both failed, for the same underlying reason -- Stata
* globals persist across do-files within a session, so a value set by an
* earlier run survives into this one:
*   "set it if it is empty"    -- a stale RELATIVE value survived and sent
*                                 output to the working directory (the bug
*                                 that misplaced tab10_7_etwfe.rtf on
*                                 14 Aug 2026).
*   "accept it if it is ABSOLUTE" -- a stale ABSOLUTE value survived too.
*                                 On 19 Aug 2026 a standalone run inherited
*                                 $log_dir = .../Output/log, left by an
*                                 earlier run of RDD.do v6 whose fallback
*                                 used the singular name.  It was absolute,
*                                 so the guard accepted it, and RDD.log was
*                                 written to .../Output/log even though the
*                                 v7 fallback said "logs".
*
* Derivation removes the failure mode entirely: nothing inherited can
* survive, so the paths cannot drift from $out_root.  This costs nothing
* when the master calls the script, because Stata_code10.do derives the
* same directories from the same $out_root -- the values are identical.
* To redirect output, change $out_root, not the individual directories.
global out_root "Output"
if c(username) == "marvi" {
    global out_root "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output"
}
capture mkdir "$out_root"

* $graphs_dir and $log_dir are DERIVED FROM $out_root unconditionally.
* An inherited value is announced and then replaced; see the note above
* on why "accept it if it is absolute" was not enough.
local cur "$graphs_dir"
global graphs_dir "$out_root/graphs"
if "`cur'" != "" & "`cur'" != "$graphs_dir" {
    di as text "RDD.do: replacing inherited graphs_dir (`cur')"
}
di as text "RDD.do: graphs_dir = $graphs_dir"
capture mkdir "$graphs_dir"

* ------------------------------------------------------------------------
* -chkfig- : confirm a manuscript figure actually landed on disk.
* Stata's "saved as PNG format" message means the export command returned
* without error, which is not the same as the file being readable at that
* path afterwards (sync clients, permissions, and antivirus can all move
* or quarantine it).  This re-opens the file and reports its size, so the
* log carries independent evidence rather than the exporter's own word.
* ------------------------------------------------------------------------
capture program drop chkfig
program define chkfig
    args f
    capture confirm file "`f'"
    if _rc {
        di as error "   MISSING: `f'  (confirm returned " _rc ")"
    }
    else {
        qui checksum "`f'"
        di as text "   OK: `f'  (" r(filelen) " bytes)"
    }
end

set scheme s2mono    // Springer B&W print (harmless if already set)

*------------------------------------------------------------------------
* Log file
*------------------------------------------------------------------------
* A NAMED log (rdd_log) is used rather than the default one.  Stata allows
* several named logs open at once, and a named log never collides with the
* default log, so this works whether the script is run standalone or called
* by Stata_code10.do -- and closing this one does not close the master's.
* Closed at the end of the script; see the close-out below.

* Derived from $out_root unconditionally, for the reason given above.
local cur "$log_dir"
global log_dir "$out_root/logs"
if "`cur'" != "" & "`cur'" != "$log_dir" {
    di as text "RDD.do: replacing inherited log_dir (`cur')"
}
di as text "RDD.do: log_dir = $log_dir"
capture mkdir "$log_dir"

* -capture- so an aborted earlier run that left rdd_log open is harmless.
capture log close rdd_log
log using "$log_dir/RDD.log", replace text name(rdd_log)

di as text "RDD.do: logging to $log_dir/RDD.log"

* ------------------------------------------------------------------------
* VERSION STAMP -- printed INSIDE the log on purpose.
* If a log does not show this line, an older copy of the script ran.  That
* is the fastest way to settle "did my edit take effect?" without diffing
* files.  Bump the date when the script changes.
* ------------------------------------------------------------------------
di as text "RDD.do: version 2026-08-19f"
di as text "RDD.do: working directory = " c(pwd)
di as text "RDD.do: out_root   = $out_root"
di as text "RDD.do: graphs_dir = $graphs_dir"
di as text "RDD.do: tables_dir = $tables_dir"
di as text "RDD.do: log_dir    = $log_dir"

di as text _n "========================================================"
di as text    "SECTION 10.2: REGRESSION DISCONTINUITY DESIGN"
di as text    "========================================================"

*--------------------------------------------------------------------
* 10.2.1  Synthetic Data Generation
*--------------------------------------------------------------------
* Design notes
* ─────────────
*  N    = 4,000 first-time, full-time freshmen at a flagship
*           public university
*  X    = high-school GPA, 0–4.0 scale (running variable)
*           Exact truncated normal via CDF inversion: N(3.15, 0.72)
*           restricted to [1.0, 4.0]; yields mean ≈ 3.0, ~37% at or
*           above the 3.25 cutoff, no mass points at boundaries.
*  c    = 3.25  (institutional merit scholarship cutoff)
*  D    = scholarship receipt
*         Sharp:  D = 1 iff X >= 3.25
*         Fuzzy:  take-up probability jumps at c but is < 100%
*  True LATE on latent persistence index = 0.10
*  (Implied effect on binary persistence ≈ 0.13–0.16 pp, as
*   the probit-like transformation maps the latent effect onto
*   the binary outcome scale.)
*
*  Postsecondary baseline rates near c = 3.25 (HSLS:09-like):
*    Second-year persistence ≈ 62–65%
*    Year-1 credits earned   ≈ 28–30
*    Year-1 college GPA      ≈ 2.75–2.85
*
* IMPORTANT: After any DGP change, re-run Section 10.2.8 (placebo
* cutoffs) to confirm all six placebos are non-significant before
* finalizing for publication.

set seed 20260510

local N         4000
local cutoff    3.25
local true_late 0.10     // true LATE on latent persistence index

* -- Running variable ------------------------------------------------
* HS GPA: exact truncated normal via CDF inversion.
* Parameters: N(mu=3.15, sd=0.72) restricted to [lo=1.0, hi=4.0].
* This yields mean ≈ 3.0, SD ≈ 0.60, and approximately 37% of
* students at or above the 3.25 cutoff — consistent with HSLS:09.
*
* Why CDF inversion rather than draw-and-clip?
*   Drawing from rnormal() and clipping at 4.0 creates a mass point:
*   ~8% of draws from N(3.0,0.72) exceed 4.0 and pile up at exactly
*   x = 4.0. A single mass point carrying 8% of the above-cutoff
*   sample distorts the density test, placebo checks, and rdrobust
*   bandwidth selection. CDF inversion generates from the exact
*   truncated distribution with no mass points at either boundary.
*
* Method: u ~ Uniform[F(lo), F(hi)] -> x = F^{-1}(u)

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

* Centered running variable (x = 0 at the eligibility threshold)
gen double x = hs_gpa - `cutoff'

label variable hs_gpa "High-school GPA (0-4.0 scale)"
label variable x      "HS GPA centered at cutoff (c = 3.25)"

* -- Treatment assignment --------------------------------------------
gen byte D_sharp = (x >= 0)

* Fuzzy: imperfect compliance on both sides of the cutoff.
* Below cutoff: small baseline take-up (~5–14%).
* Above cutoff: high but incomplete take-up (~70–85%).
gen double pr_take_up = .
    replace pr_take_up = 0.05 + 0.09 * (x + 2.25) / 2.25   if x <  0
    replace pr_take_up = 0.70 + 0.15 * min(x / 0.75, 1.0)  if x >= 0
    replace pr_take_up = min(pr_take_up, 0.92)
    replace pr_take_up = max(pr_take_up, 0.02)

gen byte D_fuzzy = (runiform() < pr_take_up)

label variable D_sharp "Scholarship received (sharp design)"
label variable D_fuzzy "Scholarship received (fuzzy design)"

* -- Pre-determined covariates (HSLS:09-calibrated) ------------------
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

* -- Outcomes --------------------------------------------------------
* mu0: latent persistence index, HSLS:09-calibrated.
* Intercept 0.58 reflects ~62% baseline persistence near c = 3.25.

gen double mu0 = 0.58 + 0.20 * x - 0.08 * x^2          ///
               + 0.03 * female  - 0.05 * firstgen        ///
               + 0.01 * (income_cat - 2)                 ///
               + rnormal(0, 0.18)

* (a) Sharp persistence (binary)
gen double Y1_s      = mu0 + `true_late' * D_sharp + rnormal(0, 0.12)
    replace Y1_s     = max(min(Y1_s, 1), 0)
gen byte   persist_sharp = (Y1_s > 0.50)

* (b) Fuzzy persistence (binary)
gen double Y1_f      = mu0 + `true_late' * D_fuzzy + rnormal(0, 0.12)
    replace Y1_f     = max(min(Y1_f, 1), 0)
gen byte   persist_fuzzy = (Y1_f > 0.50)

* (c) Year-1 credits earned (continuous)
gen double credits_y1 = 28 + 5.0 * x - 1.5 * x^2 + 4 * D_sharp ///
                       + rnormal(0, 5)
    replace credits_y1 = max(credits_y1, 0)

* (d) Year-1 college GPA (continuous)
gen double cgpa_y1   = 2.80 + 0.35 * x - 0.08 * x^2    ///
                      + 0.08 * D_sharp + rnormal(0, 0.40)
    replace cgpa_y1  = min(max(cgpa_y1, 0), 4.0)

label variable persist_sharp "Second-year persistence (sharp)"
label variable persist_fuzzy "Second-year persistence (fuzzy)"
label variable credits_y1    "Year-1 credits earned"
label variable cgpa_y1       "Year-1 college GPA"

drop Y1_s Y1_f mu0 pr_take_up

di as text _n "=== Data generation complete. N = " _N " ==="
summarize x D_sharp D_fuzzy persist_sharp credits_y1 cgpa_y1

*--------------------------------------------------------------------
* 10.2.2  Density Continuity Test and Covariate Balance
*--------------------------------------------------------------------

di as text _n "--- 10.2.2  Density Continuity Test (Cattaneo, Jansson & Ma 2020) ---"
*
* H0: density of x is continuous at c = 0 (no manipulation).
* Method: Cattaneo, Jansson & Ma (2020) local polynomial density
* estimator with bias-corrected robust inference (JASA, 115, 1393-1407).
*
* Preferred over McCrary (2008) here: rddensity selects a data-driven
* local bandwidth on each side of c so curvature away from the cutoff
* does not contaminate the test.
*
* Interpretation: look at the Robust row.
*   p > 0.05  → no statistical evidence of a discontinuity in the density
*               of the running variable at the cutoff.  That is CONSISTENT
*               WITH, but does not establish, the absence of precise
*               manipulation.  Here the data were generated without
*               sorting, so the test demonstrates the procedure rather
*               than certifying the design (chapter Section 10.2.1).
*   p ≤ 0.05  → evidence of sorting; investigate before proceeding.

* GRAYSCALE (Springer).  rddensity does NOT inherit -set scheme s2mono-
* for its own plot elements: it hardcodes red below the cutoff and blue
* above it for the fitted density, the confidence region and the
* histogram bars.  Those are set explicitly here through rddensity's own
* plot options, and the two sides are distinguished by LINE PATTERN
* (solid below, dashed above) rather than by color, so the figure still
* reads correctly in print.  legend(off) suppresses rddensity's internal
* series names (__00000G, __000005/__000006, __000004), which carry no
* information for the reader.
*
* NOTE() MUST BE A SINGLE STRING HERE.  A multi-line note -- note("line
* one" "line two") -- renders correctly in an ordinary twoway call, but
* rddensity passes graph_opt() through its own parser, which breaks every
* argument after the first into separate tokens: the 19 Aug 2026 export
* printed "Shaded / bands / are / 95% / confidence / regions." one word
* per line.  Keep it to one quoted string.
*
* Option names follow rddensity's help file:
*   esll_opt / eslr_opt  twoway line  options for the point estimates
*   cirl_opt / cirr_opt  twoway rarea options for the confidence regions
*   histl_opt / histr_opt  twoway bar options for the background histogram
* The l / r in each name is left / right of the cutoff.

rddensity x, c(0) plot                                                   ///
    esll_opt(lcolor(gs0) lpattern(solid) lwidth(medthick))               ///
    eslr_opt(lcolor(gs0) lpattern(dash)  lwidth(medthick))               ///
    cirl_opt(color(gs12))                                                ///
    cirr_opt(color(gs9))                                                 ///
    histl_opt(color(gs14) lcolor(gs8) lwidth(vthin))                     ///
    histr_opt(color(gs11) lcolor(gs8) lwidth(vthin))                     ///
    graph_opt(                                                            ///
        scheme(s2mono)                                                    ///
        title("Density Continuity Test (rddensity)")                     ///
        subtitle("Running variable: HS GPA centered at c = 3.25")        ///
        xtitle("HS GPA (centered at cutoff)") ytitle("Density")          ///
        xline(0, lcolor(gs0) lpattern(dash) lwidth(medthin))             ///
        legend(off)                                                       ///
        note("Solid line: below the cutoff; dashed line: above. Shaded bands are 95% confidence regions."))

* MANUSCRIPT Fig. 10.2 — density continuity test.
graph export "$graphs_dir/fig10_2_rdd_density_test.png", replace width(1400)
chkfig "$graphs_dir/fig10_2_rdd_density_test.png"
di as text "   Density test exported -> fig10_2_rdd_density_test.png"
di as text "   See the Robust row above for the preferred test statistic."

* -- Covariate balance via rdrobust ----------------------------------
* All five covariates are pre-determined; none should show a jump at c = 3.25.

di as text _n "--- Covariate balance ---"
di as text    "   Variable        Coef(conv)   p(robust)   h*"
di as text    "   {hline 52}"

foreach v in female firstgen urm act_score income_cat {
    qui rdrobust `v' x, c(0) kernel(triangular) bwselect(mserd)
    di as text "   `v'" _col(20) %8.4f e(tau_cl) _col(32) %8.4f e(pv_rb) ///
               _col(44) %6.3f e(h_l)
}

di as text    "   {hline 52}"
di as text    "   None should be statistically significant under a valid RD."
di as text    "   Failing to detect a jump is not the same as establishing that"
di as text    "   these characteristics are identical across the threshold: the"
di as text    "   tests are local and therefore not highly powered."

*--------------------------------------------------------------------
* 10.2.3  Binned Scatterplots (cmogram)
*--------------------------------------------------------------------
* NOTE: cmogram does not support scheme() inside graphopts();
*       apply via set scheme before the loop.

di as text _n "--- 10.2.3  Binned scatterplots ---"

set scheme s2mono

foreach outcome in persist_sharp credits_y1 cgpa_y1 {
    qui cmogram `outcome' x, cut(0) scatter lineat(0)                    ///
        graphopts(                                                        ///
            title("RD Binscatter: `outcome'")                            ///
            xtitle("HS GPA centered at c = 3.25") ytitle("`outcome'")   ///
            xline(0, lcolor(gs0) lpattern(dash)))
    graph export "$graphs_dir/fig10_2_2_rdd_binscatter_`outcome'.png",  ///
        replace width(1400)
}

di as text _n "Binned scatterplots exported."

*--------------------------------------------------------------------
* 10.2.4  Sharp RD Estimates
*--------------------------------------------------------------------

di as text _n "=== 10.2.4: Sharp RD Estimates ==="

* -- Naive OLS (biased benchmark) -----------------------------------
* OLS on the full sample conflates the GPA-persistence gradient with
* the scholarship effect; illustrates why bandwidth restriction matters.

di as text _n "--- OLS full sample (biased benchmark) ---"

reg persist_sharp D_sharp x, robust
reg credits_y1    D_sharp x, robust

* -- Manual local linear with triangular kernel ---------------------
* Kernel weight: w_i = (1 - |x_i|/h) for |x_i| <= h, else 0.
* Separate slopes each side via the interaction term x_D.
* h = 0.50 used here for exposition; Section 10.2.5 examines
* sensitivity across the full bandwidth grid.

local h_manual = 0.50

gen byte   in_bw  = (abs(x) <= `h_manual')
gen double tri_wt = (1 - abs(x) / `h_manual') * in_bw
gen double x_D    = x * D_sharp

di as text _n "--- Manual local linear (h = `h_manual', triangular kernel) ---"

foreach outcome in persist_sharp credits_y1 cgpa_y1 {
    reg `outcome' D_sharp x x_D [aw = tri_wt] if in_bw, robust
    di as text "   `outcome':  LATE = " %7.4f _b[D_sharp]  ///
               "  SE = " %7.4f _se[D_sharp]
}

drop in_bw tri_wt x_D

* -- rdrobust: CCT MSE-optimal bandwidth ----------------------------
* rdrobust selects the MSE-optimal bandwidth (MSERD) and computes
* bias-corrected robust CIs following Calonico, Cattaneo & Titiunik
* (2014, Econometrica).

di as text _n "--- rdrobust (CCT optimal bandwidth) ---"
di as text    "   Outcome           LATE     SE(conv)  p(robust)  h*"
di as text    "   {hline 58}"

foreach outcome in persist_sharp credits_y1 cgpa_y1 {
    qui rdrobust `outcome' x, c(0) kernel(triangular) bwselect(mserd) all
    di as text "   `outcome'" _col(22) %7.4f e(tau_cl)       ///
               _col(31) %8.4f e(se_tau_cl)                   ///
               _col(41) %9.4f e(pv_rb)                       ///
               _col(53) %6.3f e(h_l)
}

di as text    "   {hline 58}"

* -- Covariate-adjusted rdrobust (Sharp RD) -------------------------
* Under local randomisation, adding pre-determined covariates via
* covs() leaves the point estimate unchanged but reduces residual
* variance and tightens the confidence interval. Compare with the
* unadjusted estimates above to confirm balance (Section 10.2.2).
* Covariates: female firstgen urm act_score income_cat.

di as text _n "--- rdrobust with covariates (Sharp RD) ---"
di as text    "   Covariates: female firstgen urm act_score income_cat"
di as text    "   Outcome           LATE     SE(conv)  p(robust)  h*"
di as text    "   {hline 58}"

foreach outcome in persist_sharp credits_y1 cgpa_y1 {
    qui rdrobust `outcome' x, c(0) kernel(triangular) bwselect(mserd) ///
        covs(female firstgen urm act_score income_cat) all
    di as text "   `outcome'" _col(22) %7.4f e(tau_cl)       ///
               _col(31) %8.4f e(se_tau_cl)                   ///
               _col(41) %9.4f e(pv_rb)                       ///
               _col(53) %6.3f e(h_l)
}

di as text    "   {hline 58}"
di as text    "   Point estimates should be stable relative to unadjusted rdrobust."
di as text    "   SE reduction reflects lower residual variance with covariates."

*--------------------------------------------------------------------
* 10.2.5  Bandwidth Sensitivity
*--------------------------------------------------------------------

di as text _n "=== 10.2.5: Bandwidth Sensitivity (persistence) ==="

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
matlist BW, format(%8.4f) title("Bandwidth sensitivity -- persistence")

preserve
    clear
    svmat BW, names(col)

    twoway                                                          ///
        (rcap CI_lo CI_hi h, lcolor(gs10) lwidth(medthin))         ///
        (scatter LATE h, mcolor(gs0) msize(medlarge) msymbol(D))   ///
        (function y = 0, range(0.10 1.05)                          ///
             lcolor(gs0) lpattern(dash) lwidth(medthin)),           ///
        legend(off)                                                 ///
        xtitle("Bandwidth (HS GPA units)")                         ///
        ytitle("Estimated LATE (pp)")                              ///
        title("Bandwidth Sensitivity -- Sharp RD")                 ///
        subtitle("Outcome: Second-Year Persistence, c = 3.25")     ///
        scheme(s2mono)

    graph export "$graphs_dir/fig10_2_3_rdd_bw_sensitivity.png", replace width(1400)
restore

di as text "Bandwidth sensitivity plot exported -> fig10_2_3_rdd_bw_sensitivity.png"

*--------------------------------------------------------------------
* 10.2.6  Polynomial Order Sensitivity
*--------------------------------------------------------------------

di as text _n "=== 10.2.6: Polynomial Order Sensitivity ==="
di as text    "   p   LATE      SE(conv)  p(robust)   h*"
di as text    "   {hline 48}"

forvalues p = 1/3 {
    qui rdrobust persist_sharp x, c(0) p(`p') kernel(triangular) bwselect(mserd)
    di as text "   `p'   " %7.4f e(tau_cl)   ///
               "   " %7.4f e(se_tau_cl)       ///
               "   " %7.4f e(pv_rb)           ///
               "   " %6.3f e(h_l)
}

di as text    "   {hline 48}"
di as text    "   p = 1 (local linear) is the default and typically preferred."

*--------------------------------------------------------------------
* 10.2.7  Fuzzy RD
*--------------------------------------------------------------------
* Three-step procedure for each outcome:
*   1. First stage    D_sharp → D_fuzzy (common to all outcomes)
*   2. Reduced forms  ITT jump at cutoff for each outcome
*   3. Fuzzy LATE     rdrobust (Wald/2SLS via fuzzy option)
*                     Manual 2SLS  (h = 0.50, triangular kernel)
*
* Outcomes examined
*   persist_fuzzy   Second-year persistence (binary, fuzzy DGP)
*   credits_y1      Year-1 credits earned   (continuous, ~28–32 cr)
*   cgpa_y1         Year-1 college GPA      (continuous, 0–4 scale)
*
* Manual 2SLS variable names operationalise Eq. 10.5 (fuzzy Wald
* estimand) in the manuscript:
*   in_bw2  = bandwidth indicator  (|x| ≤ 0.50)
*   tri_wt2 = triangular kernel weight
*   x_Ds2   = slope interaction     (x × D_sharp)
*
* v2: all three outcomes looped through every step; consolidated
*     summary table added.

di as text _n "=== 10.2.7: Fuzzy RD (Imperfect Compliance) ==="

* INSTRUMENT-VALIDITY ASSUMPTIONS (manuscript Section 10.2.2 states these
* four; reading the cutoff as an instrument requires all of them)
*   1. Continuity of potential outcomes in x at c, and no precise sorting
*      across c.  Probed by the density test in Section 10.2.2 above.
*   2. A discontinuous first stage: Pr(D = 1) must jump at c.  Estimated
*      immediately below; it is the denominator of Eq. 10.5.
*   3. Exclusion: crossing c affects the outcome ONLY through take-up.
*      VIOLATED BY CONSTRUCTION here for credits_y1 and cgpa_y1, which
*      are generated from D_sharp — see the caveat in the header.
*   4. Monotonicity: crossing c moves students into treatment, never out
*      of it (no defiers).  The complier reading of the LATE rests on it.
*
* ── First stage: jump in scholarship take-up at cutoff ───────────
* D_sharp (eligibility) is a strong, binary predictor of D_fuzzy
* (actual take-up). One first stage serves all three outcomes since
* the instrument and first-stage equation are the same throughout.

di as text _n "--- First stage: jump in take-up at cutoff ---"

rdrobust D_fuzzy x, c(0) kernel(triangular) bwselect(mserd) all

local fs_late = e(tau_cl)
local fs_se   = e(se_tau_cl)
local fs_p    = e(pv_rb)
local fs_h    = e(h_l)

di as text "   FS jump  = " %7.4f `fs_late' ///
           "   SE = " %7.4f `fs_se' ///
           "   p(robust) = " %6.4f `fs_p'
di as text "   h* = " %6.3f `fs_h'
di as text "   Strong first stage: cutoff strongly predicts scholarship take-up."

* ── Reduced forms: ITT effect at cutoff for each outcome ─────────
* The reduced form is the intent-to-treat (ITT) effect of eligibility
* (crossing c = 3.25) on each outcome, estimated via rdrobust on the
* full sharp running-variable design. LATE = RF / FS.

di as text _n "--- Reduced forms: cutoff → outcome (ITT) ---"
di as text    "   Outcome            ITT      SE(conv)  p(robust)   h*"
di as text    "   {hline 60}"

foreach outcome in persist_fuzzy credits_y1 cgpa_y1 {
    qui rdrobust `outcome' x, c(0) kernel(triangular) bwselect(mserd) all
    local rf_`outcome'_b  = e(tau_cl)
    local rf_`outcome'_se = e(se_tau_cl)
    local rf_`outcome'_p  = e(pv_rb)
    local rf_`outcome'_h  = e(h_l)
    di as text "   `outcome'" _col(23) %7.4f e(tau_cl)   ///
               _col(32) %8.4f e(se_tau_cl)               ///
               _col(43) %9.4f e(pv_rb)                   ///
               _col(55) %6.3f e(h_l)
}

di as text    "   {hline 60}"
di as text    "   ITT: local average effect of eligibility (not take-up) on outcome."

* ── Fuzzy LATE via rdrobust ──────────────────────────────────────
* Wald estimator: LATE = RF / FS  (CCT bias-corrected, robust CI).
* Identifies the causal effect for compliers — students induced to
* take up the scholarship by crossing the GPA cutoff.

di as text _n "--- Fuzzy LATE (rdrobust, fuzzy option) ---"
di as text    "   Outcome            LATE     SE(conv)  p(robust)   h*"
di as text    "   {hline 60}"

foreach outcome in persist_fuzzy credits_y1 cgpa_y1 {
    qui rdrobust `outcome' x, c(0) fuzzy(D_fuzzy) ///
        kernel(triangular) bwselect(mserd) all
    local fl_`outcome'_b  = e(tau_cl)
    local fl_`outcome'_se = e(se_tau_cl)
    local fl_`outcome'_p  = e(pv_rb)
    local fl_`outcome'_h  = e(h_l)
    * Robust bias-corrected CI bounds.  Stored because Fig. 10.4 plots them;
    * the loop is -qui-, so they are otherwise unrecoverable from the log.
    local fl_`outcome'_lo = e(ci_l_rb)
    local fl_`outcome'_hi = e(ci_r_rb)
    di as text "   `outcome'" _col(23) %7.4f e(tau_cl)   ///
               _col(32) %8.4f e(se_tau_cl)               ///
               _col(43) %9.4f e(pv_rb)                   ///
               _col(55) %6.3f e(h_l)
}

di as text    "   {hline 60}"
di as text    "   LATE: causal effect for compliers at the cutoff."
di as text    "   Compliers are a LATENT group: the first stage identifies their"
di as text    "   share at the margin, not which students belong to it."
di as text    "   For credits_y1 / cgpa_y1 the exclusion restriction fails by"
di as text    "   construction; read those as Wald arithmetic, not a complier effect."
di as text    "   Robust CIs: Calonico, Cattaneo & Titiunik (2014, Econometrica)."
di as text    "   h* selected by CCT MSERD criterion separately for each outcome."

* ── Covariate-adjusted Fuzzy LATE ────────────────────────────────
* Same five covariates as the sharp adjusted model (Section 10.2.4).
* covs() tightens SEs without shifting the Wald estimator under
* local randomisation. Compare with unadjusted fuzzy LATE above
* to verify stability of the point estimates.

di as text _n "--- Covariate-adjusted Fuzzy LATE (rdrobust, covs) ---"
di as text    "   Covariates: female firstgen urm act_score income_cat"
di as text    "   Outcome            LATE     SE(conv)  p(robust)   h*"
di as text    "   {hline 60}"

foreach outcome in persist_fuzzy credits_y1 cgpa_y1 {
    qui rdrobust `outcome' x, c(0) fuzzy(D_fuzzy)              ///
        kernel(triangular) bwselect(mserd)                     ///
        covs(female firstgen urm act_score income_cat) all
    local flc_`outcome'_b  = e(tau_cl)
    local flc_`outcome'_se = e(se_tau_cl)
    local flc_`outcome'_p  = e(pv_rb)
    local flc_`outcome'_h  = e(h_l)
    di as text "   `outcome'" _col(23) %7.4f e(tau_cl)   ///
               _col(32) %8.4f e(se_tau_cl)               ///
               _col(43) %9.4f e(pv_rb)                   ///
               _col(55) %6.3f e(h_l)
}

di as text    "   {hline 60}"
di as text    "   Wald estimator = RF / FS; covs() reduces outcome residual variance."
di as text    "   Point estimates stable relative to unadjusted fuzzy LATE above."

* ── Manual 2SLS (h = 0.50, triangular kernel) ────────────────────
* Pedagogical bridge from IV/2SLS (Chapters 6–7) to rdrobust fuzzy.
* Variable names operationalise Eq. 10.5 (fuzzy Wald estimand):
*
*   gen byte   in_bw2  = (abs(x) <= 0.50)
*   gen double tri_wt2 = (1 - abs(x) / 0.50) * in_bw2
*   gen double x_Ds2   = x * D_sharp
*
*   ivregress 2sls Y (D_fuzzy = D_sharp) x x_Ds2 [aw=tri_wt2] if in_bw2, robust
*
* Example for credits_y1:
*   ivregress 2sls credits_y1 (D_fuzzy = D_sharp) x x_Ds2 ///
*       [aw = tri_wt2] if in_bw2, robust
*
* Instruments D_fuzzy with D_sharp; separate slopes on each side of
* the cutoff via the x_Ds2 interaction. Constant h = 0.50 GPA units.

di as text _n "--- Manual 2SLS (h = 0.50, triangular kernel) ---"
di as text    "   Variables: in_bw2  tri_wt2  x_Ds2  (as in text Eq. 10.5)"
di as text    "   Outcome            LATE     SE       p-value"
di as text    "   {hline 52}"

local h2 = 0.50

gen byte   in_bw2  = (abs(x) <= `h2')
gen double tri_wt2 = (1 - abs(x) / `h2') * in_bw2
gen double x_Ds2   = x * D_sharp

foreach outcome in persist_fuzzy credits_y1 cgpa_y1 {
    ivregress 2sls `outcome' (D_fuzzy = D_sharp) x x_Ds2 ///
        [aw = tri_wt2] if in_bw2, robust
    local iv_`outcome'_b  = _b[D_fuzzy]
    local iv_`outcome'_se = _se[D_fuzzy]
    local iv_`outcome'_p  = 2 * normal(-abs(`iv_`outcome'_b' / `iv_`outcome'_se'))
    di as text "   `outcome'" _col(23) %7.4f `iv_`outcome'_b' ///
               _col(32) %8.4f `iv_`outcome'_se'               ///
               _col(42) %7.4f `iv_`outcome'_p'
}

drop in_bw2 tri_wt2 x_Ds2

di as text    "   {hline 52}"
di as text    "   2SLS uses fixed h = 0.50; rdrobust above uses MSE-optimal h*."
di as text    "   For publication use rdrobust fuzzy estimates. 2SLS is pedagogical."

* ── Consolidated fuzzy summary table ─────────────────────────────
di as text _n "--- Consolidated Fuzzy RD Summary ---"
di as text    "   {hline 76}"
di as text    "   Outcome           FS jump    RF (ITT)   LATE(rdrobust)  p(LATE)"
di as text    "   {hline 76}"

foreach outcome in persist_fuzzy credits_y1 cgpa_y1 {
    di as text "   `outcome'" _col(23) %6.4f `fs_late'          ///
               _col(32)       %7.4f   `rf_`outcome'_b'          ///
               _col(42)       %7.4f   `fl_`outcome'_b'          ///
               _col(53)       %8.4f   `fl_`outcome'_p'
}

di as text    "   {hline 76}"
di as text    "   FS: first-stage jump in D_fuzzy at the cutoff (same for all outcomes)."
di as text    "   RF: rdrobust ITT at CCT MSE-optimal h* for each outcome."
di as text    "   LATE: Wald estimator = RF / FS via rdrobust fuzzy option."
di as text    "   NOTE: credits_y1 and cgpa_y1 use sharp DGP; fuzzy LATE shown"
di as text    "         for pedagogical comparison. Prefer sharp estimates (§10.2.4)."

*--------------------------------------------------------------------
* 10.2.8  Validity Checks
*--------------------------------------------------------------------

di as text _n "=== 10.2.8: Validity Checks ==="

* -- Placebo cutoffs -------------------------------------------------
* Under a valid RD, rdrobust should detect no discontinuity at
* artificially imposed cutoffs away from c = 3.25.
* Three below, three above the true cutoff.
*
* With alpha = 0.05 and six simultaneous tests, one false positive is
* consistent with the null (Bonferroni threshold = 0.008).
* Flag p < 0.05 for inspection; flag p < 0.008 as a concern.

di as text _n "--- Placebo cutoffs ---"
di as text    "   Cutoff    LATE      p(robust)   Side"
di as text    "   {hline 44}"

foreach c in -0.60 -0.40 -0.20 {
    qui rdrobust persist_sharp x if x < 0, c(`c') ///
        kernel(triangular) bwselect(mserd)
    di as text "    `c'     " %7.4f e(tau_cl) "   " %6.4f e(pv_rb) "     below"
}
foreach c in 0.20 0.40 0.60 {
    qui rdrobust persist_sharp x if x > 0, c(`c') ///
        kernel(triangular) bwselect(mserd)
    di as text "    +`c'    " %7.4f e(tau_cl) "   " %6.4f e(pv_rb) "     above"
}

di as text    "   {hline 44}"

* -- Donut RD --------------------------------------------------------
* Excludes a narrow band at the cutoff; stable estimates corroborate
* the baseline. Capped at d = 0.10 — at d = 0.15 the above-cutoff
* range shrinks to 0.60 GPA units and MSE-optimal BW extrapolates
* through the gap, producing unstable estimates for a binary outcome.

di as text _n "--- Donut RD (exclude narrow band at cutoff) ---"
di as text    "   Donut   LATE      p(robust)"
di as text    "   {hline 30}"

foreach d in 0.05 0.10 {
    qui rdrobust persist_sharp x if abs(x) > `d', c(0) ///
        kernel(triangular) bwselect(mserd)
    di as text "    `d'    " %7.4f e(tau_cl) "   " %6.4f e(pv_rb)
}

di as text    "   {hline 30}"
di as text    "   Note: interpret alongside bandwidth sensitivity (Section 10.2.5)."
di as text    "   Precision loss at d = 0.10 does not imply a zero treatment effect."

* -- Covariate-augmented rdrobust -----------------------------------
* Adding pre-determined covariates should not shift the RD estimate
* materially if balance holds (Section 10.2.2).

di as text _n "--- Covariate-augmented estimate ---"

rdrobust persist_sharp x, c(0) kernel(triangular) bwselect(mserd) ///
    covs(female firstgen act_score income_cat urm) all

di as text "   Augmented LATE = " %7.4f e(tau_cl) ///
           "  p(robust) = " %6.4f e(pv_rb)

* -- Subgroup heterogeneity -----------------------------------------
* Interpret with caution: subgroup samples are smaller and less precise.

di as text _n "--- Subgroup effects ---"
di as text    "   Subgroup            LATE      p(robust)   n"
di as text    "   {hline 52}"

foreach fg in 1 0 {
    local lbl = cond(`fg' == 1, "First-gen       ", "Continuing-gen  ")
    qui count if firstgen == `fg'
    local n_sub = r(N)
    qui rdrobust persist_sharp x if firstgen == `fg', ///
        c(0) kernel(triangular) bwselect(mserd)
    di as text "   `lbl'" %7.4f e(tau_cl) "   " %6.4f e(pv_rb) "   " `n_sub'
}

foreach f in 1 0 {
    local lbl = cond(`f' == 1, "Female          ", "Male            ")
    qui count if female == `f'
    local n_sub = r(N)
    qui rdrobust persist_sharp x if female == `f', ///
        c(0) kernel(triangular) bwselect(mserd)
    di as text "   `lbl'" %7.4f e(tau_cl) "   " %6.4f e(pv_rb) "   " `n_sub'
}

foreach r_grp in 1 0 {
    local lbl = cond(`r_grp' == 1, "URM             ", "Non-URM         ")
    qui count if urm == `r_grp'
    local n_sub = r(N)
    qui rdrobust persist_sharp x if urm == `r_grp', ///
        c(0) kernel(triangular) bwselect(mserd)
    di as text "   `lbl'" %7.4f e(tau_cl) "   " %6.4f e(pv_rb) "   " `n_sub'
}

di as text    "   {hline 52}"

*--------------------------------------------------------------------
* 10.2.9  Publication-Quality RD Plots (rdplot)
*--------------------------------------------------------------------

di as text _n "=== 10.2.9: Publication-Quality RD Plots ==="

rdplot persist_sharp x, c(0) nbins(30 30)                              ///
    graph_options(                                                      ///
        title("Effect of Institutional Merit Scholarship on"           ///
              "Second-Year Persistence")                               ///
        subtitle("Sharp RD -- HS GPA cutoff c = 3.25")                ///
        xtitle("High-School GPA (centered at cutoff)")                 ///
        ytitle("Second-Year Persistence Rate")                         ///
        xline(0, lcolor(gs0) lpattern(dash) lwidth(medthin))          ///
        legend(off) scheme(s2mono)                                     ///
        note("Circles = bin means; lines = local polynomial fit."     ///
             "Bins selected by IMSE-minimizing method (Calonico et al., 2015)."))

graph export "$graphs_dir/fig10_2_4_rdd_plot_persistence.png", replace width(1400)

rdplot credits_y1 x, c(0) nbins(30 30)                                 ///
    graph_options(                                                      ///
        title("Effect of Institutional Merit Scholarship on"           ///
              "Year-1 Credits Earned")                                 ///
        subtitle("Sharp RD -- HS GPA cutoff c = 3.25")                ///
        xtitle("High-School GPA (centered at cutoff)")                 ///
        ytitle("Year-1 Credits Earned")                                ///
        xline(0, lcolor(gs0) lpattern(dash) lwidth(medthin))          ///
        legend(off) scheme(s2mono))

* MANUSCRIPT Fig. 10.1 — sharp RDD effect on credits earned.
graph export "$graphs_dir/fig10_1_rdd_plot_credits.png", replace width(1400)
chkfig "$graphs_dir/fig10_1_rdd_plot_credits.png"

rdplot cgpa_y1 x, c(0) nbins(30 30)                                    ///
    graph_options(                                                      ///
        title("Effect of Institutional Merit Scholarship on"           ///
              "Year-1 College GPA")                                    ///
        subtitle("Sharp RD -- HS GPA cutoff c = 3.25")                ///
        xtitle("High-School GPA (centered at cutoff)")                 ///
        ytitle("Year-1 College GPA")                                   ///
        xline(0, lcolor(gs0) lpattern(dash) lwidth(medthin))          ///
        legend(off) scheme(s2mono))

graph export "$graphs_dir/fig10_2_6_rdd_plot_cgpa.png", replace width(1400)

rdplot D_fuzzy x, c(0) nbins(30 30)                                    ///
    graph_options(                                                      ///
        title("First Stage: Scholarship Take-Up at GPA Cutoff")        ///
        subtitle("Fuzzy RD -- jump in take-up probability at c = 3.25") ///
        xtitle("High-School GPA (centered at cutoff)")                 ///
        ytitle("P(Scholarship Received)")                              ///
        xline(0, lcolor(gs0) lpattern(dash) lwidth(medthin))          ///
        legend(off) scheme(s2mono))

* MANUSCRIPT Fig. 10.3 — first stage, jump in take-up at the cutoff.
graph export "$graphs_dir/fig10_3_rdd_plot_firststage.png", replace width(1400)
chkfig "$graphs_dir/fig10_3_rdd_plot_firststage.png"

* -- fig10_2_8: Fuzzy outcome (persist_fuzzy) rdplot ---------------
* Shows the discontinuity in the fuzzy persistence outcome; compare
* with fig10_2_4 (sharp) to see how imperfect compliance attenuates
* the visible jump.

rdplot persist_fuzzy x, c(0) nbins(30 30)                             ///
    graph_options(                                                     ///
        title("Fuzzy RD: Second-Year Persistence (Fuzzy Outcome)")    ///
        subtitle("Fuzzy RD — imperfect compliance around c = 3.25")  ///
        xtitle("High-School GPA (centered at cutoff)")                ///
        ytitle("P(Second-Year Persistence)")                          ///
        xline(0, lcolor(gs0) lpattern(dash) lwidth(medthin))         ///
        legend(off) scheme(s2mono)                                    ///
        note("Compare fig10_2_4 (sharp): attenuated jump reflects"   ///
             "imperfect compliance. Fuzzy LATE = RF/FS recovers LATE."))

graph export "$graphs_dir/fig10_2_8_rdd_plot_persist_fuzzy.png", replace width(1400)

* -- fig10_4: Fuzzy LATE for credits_y1 (rdrobust) ------------------
* MANUSCRIPT Fig. 10.4. Forest-plot style: point estimate and 95% CI
* for the fuzzy LATE for credits earned in year 1. Plots the rdrobust
* fuzzy estimate at the MSE-optimal bandwidth (6.4145, h* = 0.213),
* NOT the fixed-bandwidth 2SLS estimate (6.2978, h = 0.50) that the
* manuscript reports alongside it.
*
* WHICH INTERVAL: set -ci_type- to robust or conventional.
*   robust        e(ci_l_rb), e(ci_r_rb) -- rdrobust's bias-corrected
*                 interval.  This is what the chapter's inference rests
*                 on (p-values throughout Section 10.2 are e(pv_rb)), and
*                 what the Section 10.2.10 summary table prints.  It is
*                 NOT centered on the conventional point estimate,
*                 because it centers on the bias-corrected one -- so the
*                 diamond will sit off-centre in the interval.  That is
*                 correct, not a plotting error.
*   conventional  b +/- 1.96*SE.  Symmetric and easier to read, but it is
*                 not the interval any reported p-value comes from.
local ci_type robust

* ZERO ON THE AXIS: the chapter's prose for this figure says the interval
* "does not cross zero", so zero must be visible.  Two things are needed.
* -xscale(range(0 ...))- forces the axis to include it; and the reference
* line is drawn with -pci-, NOT -xline()-.  With -rcap ..., horizontal-
* the axes are swapped, and the -xline(0)- that used to sit here silently
* drew nothing in every run through 19 Aug 2026.  -pci- takes explicit
* (y1 x1 y2 x2) coordinates and is immune to the swap.

preserve
    clear
    set obs 1
    gen j       = _n
    gen b       = .
    gen lo      = .
    gen hi      = .

    replace b  = `fl_credits_y1_b'                                    in 1
    if "`ci_type'" == "robust" {
        replace lo = `fl_credits_y1_lo'                               in 1
        replace hi = `fl_credits_y1_hi'                               in 1
        local ci_label "robust bias-corrected 95% CI"
    }
    else {
        replace lo = `fl_credits_y1_b' - 1.96 * `fl_credits_y1_se'  in 1
        replace hi = `fl_credits_y1_b' + 1.96 * `fl_credits_y1_se'  in 1
        local ci_label "conventional 95% CI"
    }

    * Upper end of the x axis: round the interval's top up to the next
    * whole unit, with a minimum of 10 so the figure keeps a stable shape.
    * The lower end is -0.4 rather than 0 so the dashed zero line stands
    * clear of the plot frame instead of sitting on top of it; the first
    * x label is still 0.
    local xmax = max(10, ceil(hi[1]) + 1)

    twoway ///
        (pci 0.6 0 1.4 0, lpattern(dash) lcolor(gs8) lwidth(medthin))  ///
        (rcap lo hi j, horizontal lcolor(gs0) lwidth(medium))          ///
        (scatter j b,  mcolor(gs0) msymbol(D) msize(large)),           ///
        xscale(range(-0.4 `xmax'))                                     ///
        xlabel(0(2)`xmax')                                             ///
        yscale(range(0.5 1.5))                                         ///
        ylabel(1 "Credits Y1 (cr-hrs)",                                ///
               angle(0) labsize(small))                                ///
        xtitle("Fuzzy LATE (rdrobust, CCT MSE-optimal h*)")            ///
        ytitle("") legend(off) scheme(s2mono)                          ///
        title("Fuzzy RD: LATE for First-Year Credits")                 ///
        subtitle("Point estimate with `ci_label'")                     ///
        note("Wald ratio: reduced form / first stage."                 ///
             "credits_y1 is generated from D_sharp, so this is Wald"   ///
             "arithmetic rather than a complier effect (Sec. 10.2.3)." ///
             "rdrobust fuzzy option, kernel(triangular), bwselect(mserd).") ///
        name(fig10_4, replace)

    graph export "$graphs_dir/fig10_4_fuzzy_late_credits.png", replace width(1400)
    chkfig "$graphs_dir/fig10_4_fuzzy_late_credits.png"

    di as text "   Fig. 10.4: LATE = " %6.3f b[1] ///
               "   `ci_label' = [" %6.3f lo[1] ", " %6.3f hi[1] "]"
restore

di as text "All RD plots exported."

*--------------------------------------------------------------------
* 10.2.10  Summary Table and Dataset Save
*--------------------------------------------------------------------

di as text _n "=== 10.2.10: Summary of Sharp RD Estimates ==="
di as text    "   {hline 72}"
di as text    "   Outcome           LATE     SE(conv)  95% CI (robust)     h*"
di as text    "   {hline 72}"

foreach outcome in persist_sharp credits_y1 cgpa_y1 {
    qui rdrobust `outcome' x, c(0) kernel(triangular) bwselect(mserd) all
    local lo = string(round(e(ci_l_rb), 0.001), "%7.3f")
    local hi = string(round(e(ci_r_rb), 0.001), "%7.3f")
    di as text "   `outcome'" _col(22) %7.4f e(tau_cl)       ///
               _col(32) %8.4f e(se_tau_cl)                   ///
               _col(44) "[" "`lo'" ", " "`hi'" "]"           ///
               _col(66) %5.3f e(h_l)
}

di as text    "   {hline 72}"
di as text    "   Robust CIs: Calonico, Cattaneo & Titiunik (2014, Econometrica)."
di as text    "   Kernel: triangular. Bandwidth selector: MSERD (MSE-optimal)."
di as text    "   True simulated LATE (latent scale) = `true_late'."
di as text    "   Implied binary persistence effect approx. 0.13-0.16 pp."

compress
* Synthetic dataset goes to $out_root/data, not the current directory.
* Filename matches the chapter's data-availability section, which tells
* readers the repository file for Section 10.2 is Example_10_2_1.dta.
capture mkdir "$out_root/data"
save "$out_root/data/Example_10_2_1.dta", replace

* Roll call: re-confirm the four manuscript figures at the end of the run,
* so one block in the log answers "did Section 10.2 produce its figures?"
di as text _n "=== Manuscript figures for Section 10.2 ==="
foreach f in fig10_1_rdd_plot_credits fig10_2_rdd_density_test ///
             fig10_3_rdd_plot_firststage fig10_4_fuzzy_late_credits {
    chkfig "$graphs_dir/`f'.png"
}
di as text "   Directory listed below is the one they were written to:"
di as text "   $graphs_dir"
capture noisily dir "$graphs_dir/fig10_*.png"

di as text _n "=== RDD dataset saved -> $out_root/data/Example_10_2_1.dta ==="
di as text    "=== Section 10.2 complete.                                 ==="

* Close the named log opened at the top.  -capture- so that re-running the
* script, or a master driver that has already closed it, is harmless.
capture log close rdd_log

*========================================================================
* END OF RDD.do
*========================================================================
