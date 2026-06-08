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
* Called by: Stata_code10.do  (inherits $graphs_dir, log, set scheme)
* Standalone: can also be run directly; uses fallback paths if needed.
*
* Required packages: rdrobust, rddensity, lpdensity, cmogram
*   ssc install rdrobust,  replace
*   ssc install rddensity, replace
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
*   10.2.10  Summary table; save ch10_rdd_hsls09_synthetic.dta
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
*========================================================================

*------------------------------------------------------------------------
* Fallback paths when running standalone (not called from Stata_code10.do)
*------------------------------------------------------------------------
* If $graphs_dir is already defined by the parent script, these lines
* have no effect. If running RDD.do directly, they set sensible defaults.

capture confirm global graphs_dir
if _rc != 0 {
    if c(username) == "marvi" {
        global graphs_dir ///
            "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/graphs"
        capture mkdir ///
            "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output"
        capture mkdir ///
            "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/graphs"
    }
    else {
        global graphs_dir "Output/graphs"
        capture mkdir "Output"
        capture mkdir "Output/graphs"
    }
    di as text "RDD.do (standalone): graphs_dir set to $graphs_dir"
}

set scheme s2mono    // Springer B&W print (harmless if already set)

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
*   p > 0.05  → no evidence of density manipulation.
*   p ≤ 0.05  → investigate; note that c = 3.25 is non-salient.

rddensity x, c(0) plot                                                   ///
    graph_opt(                                                            ///
        scheme(s2mono)                                                    ///
        title("Density Continuity Test (rddensity)")                     ///
        subtitle("Running variable: HS GPA centered at c = 3.25")        ///
        xtitle("HS GPA (centered at cutoff)") ytitle("Density")          ///
        xline(0, lcolor(gs0) lpattern(dash) lwidth(medthin)))

graph export "$graphs_dir/fig10_2_1_rdd_density_test.png", replace width(1400)
di as text "   Density test exported -> fig10_2_1_rdd_density_test.png"
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
* Manual 2SLS variable names match Equation 10.x in text:
*   in_bw2  = bandwidth indicator  (|x| ≤ 0.50)
*   tri_wt2 = triangular kernel weight
*   x_Ds2   = slope interaction     (x × D_sharp)
*
* v2: all three outcomes looped through every step; consolidated
*     summary table added.

di as text _n "=== 10.2.7: Fuzzy RD (Imperfect Compliance) ==="

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
    di as text "   `outcome'" _col(23) %7.4f e(tau_cl)   ///
               _col(32) %8.4f e(se_tau_cl)               ///
               _col(43) %9.4f e(pv_rb)                   ///
               _col(55) %6.3f e(h_l)
}

di as text    "   {hline 60}"
di as text    "   LATE: causal effect for compliers at the cutoff."
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
* Variable names match Equation 10.x in text:
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
di as text    "   Variables: in_bw2  tri_wt2  x_Ds2  (as in text Eq. 10.x)"
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

graph export "$graphs_dir/fig10_1_rdd_plot_credits.png", replace width(1400)

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

graph export "$graphs_dir/fig10_2_7_rdd_plot_firststage.png", replace width(1400)

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

* -- fig10_2: Fuzzy LATE for credits_y1 (rdrobust) ------------------
* Forest-plot style: point estimate and 95% CI for the fuzzy LATE
* for credits earned in year 1.

preserve
    clear
    set obs 1
    gen j       = _n
    gen b       = .
    gen lo      = .
    gen hi      = .

    replace b  = `fl_credits_y1_b'                                  in 1
    replace lo = `fl_credits_y1_b'  - 1.96 * `fl_credits_y1_se'   in 1
    replace hi = `fl_credits_y1_b'  + 1.96 * `fl_credits_y1_se'   in 1

    twoway ///
        (rcap lo hi j, horizontal lcolor(gs0) lwidth(medium))         ///
        (scatter j b,  mcolor(gs0) msymbol(D) msize(large)),          ///
        xline(0, lpattern(dash) lcolor(gs8))                          ///
        ylabel(1 "Credits Y1 (cr-hrs)",                               ///
               angle(0) labsize(small))                               ///
        xtitle("Fuzzy LATE (rdrobust, CCT MSE-optimal h*)")           ///
        ytitle("") legend(off) scheme(s2mono)                         ///
        title("Fuzzy RD: LATE for First-Year Credits")                ///
        subtitle("Point estimate with 95% CI")                        ///
        note("Complier LATE = RF / FS."                               ///
             "rdrobust fuzzy option, kernel(triangular), bwselect(mserd).") ///
        name(fig10_2, replace)

    graph export "$graphs_dir/fig10_2_fuzzy_late_credits.png", replace width(1400)
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
save "ch10_rdd_hsls09_synthetic.dta", replace

di as text _n "=== RDD dataset saved  ->  ch10_rdd_hsls09_synthetic.dta ==="
di as text    "=== Section 10.2 complete.                                 ==="

*========================================================================
* END OF RDD.do
*========================================================================
