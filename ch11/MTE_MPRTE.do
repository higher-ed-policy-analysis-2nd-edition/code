*========================================================================
* Chapter 11 – Sections 11.1–11.3: Marginal Treatment Effects
*             Returns to Master's Degree Completion
* Higher Education Policy Analysis Using Quantitative Techniques
* (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch11
* Author: Marvin A. Titus
*------------------------------------------------------------------------
* RUN TIME — PLEASE READ BEFORE RUNNING
*   By default this script REUSES the author's published bootstrap draws
*   (mte_bootstrap_draws.dta, downloaded from GitHub if not found
*   locally) and runs in about 5-10 minutes (Stata MP) or 10-15 minutes
*   (Stata SE). Every estimate, standard error, and figure then matches
*   the book.
*
*   To run the state-clustered bootstrap yourself, set
*   use_saved_boot = 0 in Section 6e. Approximate run times:
*
*                                         Stata MP (2 cores)   Stata SE
*   Full bootstrap, area models on        about 7 hours        9-12 hours
*   Full bootstrap, area models off       about 1.5 hours      2-2.5 hours
*     (run_areas = 0)
*   Default (published draws)             5-10 minutes         10-15 minutes
*
*   CAUTION — WHEN THE PUBLISHED DRAWS ARE VALID:
*   The saved draws apply ONLY to the specification in the book: the same
*   data (Example_7_5_3_updated.dta), the same models (quadratic pooled
*   MTE, linear area MTEs), the same controls, R = 500, and seed 20260101.
*   If you change ANY of these (e.g., add a control, change pol(), drop
*   observations, or use your own data), set use_saved_boot = 0 and run the
*   bootstrap yourself. The script cannot detect such changes; reusing the
*   draws after a change gives standard errors and confidence bands that
*   do not belong to your estimates.
*
*   - Times are from the author's machine; yours may differ. When the
*     bootstrap runs, the script prints a projected total after the
*     first replication. Lower R in Section 6e for quick draft runs.
*   - Switches: run_areas (search "OPTIONAL ANALYSIS SWITCH", just
*     before Section 1); R and use_saved_boot (Section 6e).
*------------------------------------------------------------------------
* Date: May 2026 (revised October 2026)
* REVISION (Oct 2026): mtefe (Andresen 2018) is now the main estimator
*   for the MTE curve and for the ATE, ATT, ATU, LATE, MPRTE, and PRTE.
*   The earlier manual polynomial read the MTE directly from the
*   coefficients on masters and its interactions with phat; those
*   coefficients still contain selection, so they are not the MTE. The
*   manual model is kept in Section 6b as a teaching illustration with
*   the corrected conversion and Eq. 11.3 weights, and is not used later.
*   Area-specific estimates use a separate linear (pol(1)) mtefe model per
*   area; the pooled model is quadratic (pol(2)). Standard
*   errors come from a state-clustered bootstrap that re-estimates mtefe.
*   Program areas (ma_*) and prospective areas (ma_*_pro) are read from
*   Example_7_5_3_updated.dta when present (make_Example_7_5_3_updated.do).
* NOTE: Code development was assisted by Claude (Anthropic). The author
* provided specifications and reviewed, tested, and validated all code.
*========================================================================
* Called by: Stata_code11.do  (inherits $graphs_dir, log, set scheme)
* Standalone: can also be run directly; uses fallback paths if needed.
*
* Data: synthetic B&B panel
*   Example_7_5_3_updated.dta  (primary; contains pre-generated ma_* vars)
*   Example_7_5_3.dta          (fallback; ma_* generated in Section 1b)
*
* Required packages: mtefe, moremata, fwildclusterboot
*
* Sections (Chapter 11 numbering):
*   1     Load dataset (Example_7_5_3_updated.dta / Example_7_5_3.dta)        [11.1]
*   1b    Verify / generate master's program area indicators (ma_*)          [11.1]
*   2     Summary statistics                                                  [11.1]
*   3     First stage and instrument relevance                                [11.1]
*   4     Naive OLS estimation                                                [11.1]
*   5     IV/2SLS estimation (LATE)                                          [11.1]
*   6     MTE estimation: probit, manual illustration, mtefe (main)      [11.2]
*   6d    MTE by graduate program area (separate mtefe per area)         [11.2]
*   6e    State-clustered bootstrap (mtefe) + wild cluster bootstrap     [11.2]
*   7     Results comparison (ATE / ATT / ATU / LATE / MPRTE)            [11.2]
*   8     MTE visualization (Figs 11.3, 11.5)                            [11.2.3]
*   9-10b Policy-relevant treatment effects via mtefe prte(), Scen. 1-8  [11.3]
*   11    PRTE by policy intensity (Fig 11.8)                            [11.3]
*   12    Comparing treatment effect parameters                          [11.3]
*   13    Policy weights and propensity support (Figs 11.6, 11.4)        [11.3]
*   14    Policy cost-benefit analysis (PRTE-based)                      [11.3]
*   15    Save results                                                        [11.4]
*   16    Final summary                                                       [11.4]
*========================================================================

*------------------------------------------------------------------------
* Fallback paths when running standalone (not called from Stata_code11.do)
*------------------------------------------------------------------------
capture confirm global graphs_dir
if _rc != 0 {
    if c(username) == "marvi" {
        global graphs_dir ///
            "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/graphs"
        capture mkdir ///
            "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output"
        capture mkdir ///
            "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/graphs"
    }
    else {
        global graphs_dir "Output/graphs"
        capture mkdir "Output"
        capture mkdir "Output/graphs"
    }
    di as text "MTE_MPRTE.do (standalone): graphs_dir set to $graphs_dir"
}

*------------------------------------------------------------------------
* Dedicated log for this sub-script
* Opens its own log file, separate from Stata_code11.do's master log,
* so that Sections 11.1-11.3 output can be reviewed independently.
* Fallback $logdir is defined here if not already set by the caller.
*------------------------------------------------------------------------
capture confirm global logdir
if _rc != 0 {
    if c(username) == "marvi" {
        global logdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/logs"
        capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/logs"
    }
    else {
        global logdir "Output/logs"
        capture mkdir "Output/logs"
    }
}

capture log close mte_mprte
log using "$logdir/MTE_MPRTE_output.log", name(mte_mprte) replace text

di as text "MTE_MPRTE.do log opened: " c(current_date) " " c(current_time)
di as text "Log file: $logdir/MTE_MPRTE_output.log"

set scheme s2mono    // Springer B&W print (harmless if already set)

*------------------------------------------------------------------------
* OPTIONAL ANALYSIS SWITCH
*   run_areas = 1 (default): run the area-specific mtefe models
*     (Section 6d), their bootstrap replications, Fig 11.5, and the area
*     tables reported in the book.
*   run_areas = 0: skip them.
*   See RUN TIME in the header for how this affects run time.
*------------------------------------------------------------------------
local run_areas = 1

*------------------------------------------------------------------------
* Data location: the author's Dropbox data folder; readers use the
* current working directory (data are downloaded from GitHub if absent).
*------------------------------------------------------------------------
if c(username) == "marvi" {
    cd "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Data/Stata"
}
di as text "Data directory: `c(pwd)'"

********************************************************************************
* SECTION 1: Load Dataset
********************************************************************************

di _n "=============================================="
di "LOADING SYNTHETIC B&B DATASET"
di "=============================================="

* Download Part B dataset from GitHub repository.
* Try the updated file (contains pre-generated ma_* variables) first;
* fall back to the base file if the updated version is unavailable.
capture confirm file "Example_7_5_3_updated.dta"
if _rc != 0 {
    di as text "Attempting to download Example_7_5_3_updated.dta from GitHub..."
    capture copy ///
        "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch11/Example_7_5_3_updated.dta" ///
        "Example_7_5_3_updated.dta", replace
    if _rc != 0 di as text "Download failed — will try local file or base version."
}

capture use "Example_7_5_3_updated.dta", clear
if _rc != 0 {
    di as text "Note: Example_7_5_3_updated.dta not found."
    di as text "Attempting to download Example_7_5_3.dta from GitHub (ch11 repository)..."
    capture copy ///
        "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch11/Example_7_5_3.dta" ///
        "Example_7_5_3.dta", replace
    if _rc != 0 {
        di as error "ERROR: Download of Example_7_5_3.dta failed."
        di as error "Please download the file manually from:"
        di as error "https://github.com/higher-ed-policy-analysis-2nd-edition/data/blob/main/ch11/Example_7_5_3.dta"
        di as error "and place it in the working directory before running Part B."
        exit 601
    }
    di as text "Loading Example_7_5_3.dta; ma_* will be generated in Section 1b."
    use "Example_7_5_3.dta", clear
}

capture confirm variable id
if _rc != 0 gen id = _n

describe
di _n "Sample size: " _N

********************************************************************************
* SECTION 1b: Verify Master's Degree Program Area Indicators
*
* FIX B: Replaced if-else-foreach block with a simple confirm check.
*
* The dataset Example_7_5_3_updated.dta was produced by the revised
* data-generation script and already contains ma_stem, ma_business,
* ma_education, ma_health, and ma_other.  If running against the
* original dataset without these variables, the script stops with a
* clear message rather than attempting to generate them, which avoids
* the nested if-else-foreach brace structure that contributed to r(198).
*
* To generate ma_* from scratch, run Generate_Example_7_5_3_Synthetic_Data.do
* (revised version) on the original dataset first.
*
* Five mutually exclusive categories (IPEDS CIP-based):
*   ma_business   Business, Management, Marketing (CIP 52)
*   ma_education  Education (CIP 13)
*   ma_health     Health Professions & Related (CIP 51)
*   ma_stem       STEM fields (CIPs 11, 14, 15, 26, 27, 40, 41)
*   ma_other      All remaining fields
*
* ma_* = 0 for all untreated observations (masters == 0).
********************************************************************************

di _n "=============================================="
di "MASTER'S PROGRAM AREA INDICATORS"
di "=============================================="

capture confirm variable ma_stem
if _rc != 0 {
    * FIX G (v14): generate ma_* from undergraduate major fields when
    * the updated dataset is unavailable.  Uses a seeded random draw so
    * results are reproducible.  Transition probabilities match the
    * pipeline definitions in Sections 10b and the data-generation script.
    *
    * All untreated obs (masters==0) receive ma_* = 0 throughout.
    di as text "Generating ma_* variables from undergraduate major fields..."

    gen ma_stem      = 0
    gen ma_business  = 0
    gen ma_education = 0
    gen ma_health    = 0
    gen ma_other     = 0

    set seed 20251130
    gen _rma = runiform() if masters == 1
    replace _rma = . if masters == 0

    replace ma_stem      = 1 if masters==1 & stem_major==1 & _rma <= 0.55
    replace ma_business  = 1 if masters==1 & bus_major==1  & _rma <= 0.65 & ma_stem==0
    replace ma_education = 1 if masters==1 & ed_major==1   & _rma <= 0.70 & ma_stem==0 & ma_business==0
    replace ma_health    = 1 if masters==1 & socsci_major==1 & _rma <= 0.40 & ma_stem==0 & ma_business==0 & ma_education==0
    replace ma_health    = 1 if masters==1 & stem_major==1   & _rma >  0.55 & _rma <= 0.75 & ma_stem==0
    replace ma_other     = 1 if masters==1 & ma_stem==0 & ma_business==0 & ma_education==0 & ma_health==0

    drop _rma

    label var ma_stem      "Master's in STEM (CIPs 11 14 15 26 27 40 41)"
    label var ma_business  "Master's in Business/Management (CIP 52)"
    label var ma_education "Master's in Education (CIP 13)"
    label var ma_health    "Master's in Health Professions (CIP 51)"
    label var ma_other     "Master's in other field"

    di as text "ma_* variables generated successfully."
}
di "ma_* variables confirmed present in dataset."

* Verification
qui count if masters == 1
local n_treated = r(N)
di _n "--- Program Area Distribution (Treated Only) ---"
di "Total treated: " `n_treated'
foreach a in stem business education health other {
    qui count if ma_`a' == 1
    di "  ma_`a': " r(N) "  (" %5.1f r(N)/`n_treated'*100 "%)"
}

gen ma_check = ma_business + ma_education + ma_health + ma_stem + ma_other
qui count if masters == 1 & ma_check != 1
if r(N) > 0 di "WARNING: " r(N) " treated obs with != 1 program area flag"
else         di "CHECK PASSED: all treated obs have exactly 1 program area"
qui count if masters == 0 & ma_check != 0
if r(N) > 0 di "WARNING: " r(N) " untreated obs with non-zero program area flag"
else         di "CHECK PASSED: all untreated obs have zero program area"
drop ma_check

********************************************************************************
* SECTION 2: Summary Statistics
********************************************************************************

di _n "=============================================="
di "SUMMARY STATISTICS"
di "=============================================="

tab masters
qui sum masters
local treat_rate = r(mean)
di "Treatment rate: " %5.3f `treat_rate'

sum ln_salary salary masters ga_funding_adj
tabstat salary ln_salary, by(masters) stats(mean sd min max n)
sum ga_funding_adj, detail

sum female black hispanic asian age_ba firstgen parent_income_q parent_grad ///
    ugpa stem_major bus_major ed_major selective_inst public_ug state_unemp metro

di _n "--- Program Area by Undergraduate Major (Treated Only) ---"
di "  STEM undergrads:"
tabstat ma_stem ma_health ma_business ma_education ma_other ///
    if masters==1 & stem_major==1, stats(mean n)
di "  Business undergrads:"
tabstat ma_stem ma_health ma_business ma_education ma_other ///
    if masters==1 & bus_major==1, stats(mean n)
di "  Education undergrads:"
tabstat ma_stem ma_health ma_business ma_education ma_other ///
    if masters==1 & ed_major==1, stats(mean n)
di "  Social sci / other undergrads:"
tabstat ma_stem ma_health ma_business ma_education ma_other ///
    if masters==1 & socsci_major==1, stats(mean n)

di _n "--- Mean Log Salary by Program Area (Treated Only) ---"
tabstat ln_salary salary, by(ma_stem) stats(mean n)
foreach a in business education health other {
    qui sum ln_salary if ma_`a' == 1
    di "  ma_`a': mean ln_salary = " %6.4f r(mean) "  (N = " r(N) ")"
}

********************************************************************************
* SECTION 3: First-Stage and Instrument Relevance
********************************************************************************

di _n "=============================================="
di "INSTRUMENT RELEVANCE CHECK"
di "=============================================="

global X_controls "female black hispanic asian age_ba firstgen parent_income_q parent_grad ugpa stem_major bus_major ed_major selective_inst public_ug state_unemp metro"
global X "$X_controls"
global Z "ga_funding_adj"

reg masters ga_funding_adj $X_controls, robust
est store first_stage

test ga_funding_adj
local first_stage_F = r(F)
di _n "First-stage F: " %6.2f `first_stage_F'
if `first_stage_F' > 10  di "RESULT: Strong instrument (F > 10)"
else                      di "WARNING: Potentially weak instrument"

********************************************************************************
* SECTION 4: Naive OLS Estimation
********************************************************************************

di _n "=============================================="
di "NAIVE OLS ESTIMATION"
di "=============================================="

reg ln_salary masters $X_controls, robust
est store ols_naive
local ols_est = _b[masters]
local ols_se  = _se[masters]
di "OLS estimate: " %6.4f `ols_est' " (SE = " %6.4f `ols_se' ")"

********************************************************************************
* SECTION 5: IV/2SLS Estimation (LATE)
********************************************************************************

di _n "=============================================="
di "IV/2SLS ESTIMATION (LATE)"
di "=============================================="

ivregress 2sls ln_salary $X_controls (masters = ga_funding_adj), robust first
est store iv_2sls
local iv_est = _b[masters]
local iv_se  = _se[masters]
di _n "IV/LATE estimate: " %6.4f `iv_est' " (SE = " %6.4f `iv_se' ")"
estat firststage
estat endogenous

********************************************************************************
* SECTION 6: MTE ESTIMATION
*
* 6a  Propensity score (probit) — used for policy scenarios and figures
* 6b  Manual polynomial control function — TEACHING ILLUSTRATION ONLY
* 6c  mtefe (Andresen 2018) — MAIN ESTIMATOR for the MTE curve and for
*     the ATE, ATT, ATU, LATE, and MPRTE. Everything after Section 6b uses
*     the mtefe estimates.
********************************************************************************

di _n "=============================================="
di "MTE ESTIMATION"
di "=============================================="

* --- 6a. Propensity score ------------------------------------------------
probit masters $Z $X
predict phat, pr
local ga_coef = _b[ga_funding_adj]
di "GA funding probit coefficient: " %7.5f `ga_coef'
predict z_index, xb

gen phat2 = phat^2
gen phat3 = phat^3

* --- 6b. Manual polynomial control function (teaching illustration) -----
* The regression below includes the treatment indicator, its interactions
* with the propensity-score polynomial, and the polynomial itself (the
* control function). The coefficients on masters and its interactions
* (theta) are NOT the MTE: they measure the gap between completers' and
* non-completers' expected outcomes at a given p, which still contains
* selection. The MTE is recovered by the separate-approach conversion
*   MTE(u) = d/du[ u*theta(u) ] + gamma'(u)
* where gamma(p) is the control-function polynomial. For a cubic:
*   MTE(u) = (th0 + g1) + (2*th1 + 2*g2)u + (3*th2 + 3*g3)u^2 + 4*th3*u^3
* Polynomial MTEs recovered this way are sensitive to the polynomial
* degree and to extrapolation near u = 0 and u = 1. They are reported here
* to show the mechanics only; Section 6c (mtefe) provides the estimates
* used in the rest of the chapter.

di _n "--- 6b. Manual polynomial (illustration only) ---"

di _n "--- Quadratic specification ---"
reg ln_salary masters c.masters#(c.phat c.phat2) $X phat phat2, robust
local qc0 = _b[masters] + _b[phat]
local qc1 = 2*_b[c.masters#c.phat] + 2*_b[phat2]
local qc2 = 3*_b[c.masters#c.phat2]
local ate_man_quad = `qc0' + `qc1'/2 + `qc2'/3
di "Quadratic MTE(u) = " %7.4f `qc0' " + " %7.4f `qc1' "*u + " %7.4f `qc2' "*u^2"
di "ATE (manual quadratic): " %6.4f `ate_man_quad'

di _n "--- Cubic specification ---"
reg ln_salary masters c.masters#(c.phat c.phat2 c.phat3) $X phat phat2 phat3, robust
local mc0 = _b[masters] + _b[phat]
local mc1 = 2*_b[c.masters#c.phat]  + 2*_b[phat2]
local mc2 = 3*_b[c.masters#c.phat2] + 3*_b[phat3]
local mc3 = 4*_b[c.masters#c.phat3]
local ate_man_cubic = `mc0' + `mc1'/2 + `mc2'/3 + `mc3'/4
di "Cubic MTE(u) = " %7.4f `mc0' " + " %7.4f `mc1' "*u + " %7.4f `mc2' "*u^2 + " %7.4f `mc3' "*u^3"
di "ATE (manual cubic): " %6.4f `ate_man_cubic'

* ATT and ATU with the Eq. 11.3 weights:
*   ATT = E[ integral_0^P MTE(u) du ] / E[P]
*   ATU = E[ integral_P^1 MTE(u) du ] / E[1 - P]
gen double _area0P = `mc0'*phat + `mc1'*phat^2/2 + `mc2'*phat^3/3 + `mc3'*phat^4/4
qui sum phat
local Ep = r(mean)
qui sum _area0P
local att_man = r(mean) / `Ep'
local atu_man = (`ate_man_cubic' - r(mean)) / (1 - `Ep')
drop _area0P
di "ATT (manual cubic): " %6.4f `att_man'
di "ATU (manual cubic): " %6.4f `atu_man'
di "Note: illustration only — compare with mtefe estimates in Section 6c."

* --- 6c. mtefe: main estimator -------------------------------------------
* NOTE: bootreps(0) — standard errors come from the state-clustered
*   bootstrap in Section 6e, which re-estimates the full model each rep.
di _n "--- 6c. mtefe (main estimator) ---"

* Sensitivity: linear MTE (pol=1)
mtefe ln_salary $X_controls (masters = ga_funding_adj), pol(1) bootreps(0) noplot
est store mtefe_linear
local ate_lin  = _b[effects:ate]
local att_lin  = _b[effects:att]
local atu_lin  = _b[effects:atut]
local late_lin = _b[effects:late]

* Main specification: quadratic MTE (pol=2)
mtefe ln_salary $X_controls (masters = ga_funding_adj), ///
    pol(2) bootreps(0) noplot savepropensity(p_mtefe)
est store mtefe_main
local ate_est    = _b[effects:ate]
local att_est    = _b[effects:att]
local atu_est    = _b[effects:atut]
local late_est   = _b[effects:late]
local mprte1_est = _b[effects:mprte1]
local mprte2_est = _b[effects:mprte2]
local mprte3_est = _b[effects:mprte3]
local p_obs_het  = e(p_X)
local p_ess_het  = e(p_U)

* MTE curve at mean X (99 points), support grid, and k(u)
matrix EB_main     = e(b)
matrix MTE_pooled  = EB_main[1, "mte:"]
matrix SUP         = e(support)
matrix KD          = e(dkdp)

di _n "mtefe results (pol=2):"
di "  ATE:    " %7.4f `ate_est'
di "  ATT:    " %7.4f `att_est'
di "  ATU:    " %7.4f `atu_est'
di "  LATE:   " %7.4f `late_est'   "   (2SLS = " %7.4f `iv_est' ")"
di "  MPRTE1: " %7.4f `mprte1_est'
di "  MPRTE2: " %7.4f `mprte2_est'
di "  MPRTE3: " %7.4f `mprte3_est'
di "  Test of essential heterogeneity, p = " %6.4f `p_ess_het'
di "  Test of observable heterogeneity, p = " %6.4f `p_obs_het'
di "  MTE at u = 0.10, 0.50, 0.90: " %7.4f MTE_pooled[1,10] ", " ///
   %7.4f MTE_pooled[1,50] ", " %7.4f MTE_pooled[1,90]

* Checks: mtefe's propensity score matches phat; support grid is j/100
gen double _pdiff = abs(p_mtefe - phat)
qui sum _pdiff
di "Check: max |p_mtefe - phat| = " %9.2e r(max)
drop _pdiff
local maxgrid = 0
forvalues j = 1/99 {
    local maxgrid = max(`maxgrid', abs(SUP[`j',1] - `j'/100))
}
di "Check: max |support grid - j/100| = " %9.2e `maxgrid'

* Individual-level MTE at each student's own X and propensity score:
*   MTE(x_i, p_i) = x_i(b1 - b0) + k(p_i), with k(u) from e(dkdp)
* x_i(b1 - b0) is built from the "beta1-beta0" equation of e(b), located
* by equation name so the code does not depend on column order.
local eqs : coleq EB_main
local nms : colnames EB_main
gen double xbd = 0
local k = 0
foreach eq of local eqs {
    local ++k
    if "`eq'" == "beta1-beta0" {
        local v : word `k' of `nms'
        if "`v'" == "_cons" qui replace xbd = xbd + EB_main[1,`k']
        else                qui replace xbd = xbd + EB_main[1,`k']*`v'
    }
}
gen int _gidx = max(1, min(99, round(phat*100)))
gen double _kp = .
forvalues j = 1/99 {
    qui replace _kp = KD[1,`j'] if _gidx == `j'
}
gen double mte_hat = xbd + _kp
label var mte_hat "MTE at own X and propensity score (mtefe, pol=2)"
drop _gidx _kp

* Check: mean of x_i(b1-b0) + k(0.5) should equal the MTE at mean X, u=0.5
qui sum xbd
di "Check: mean x(b1-b0) + k(0.50) = " %7.4f (r(mean) + KD[1,50]) ///
   "  vs  MTE(xbar, 0.50) = " %7.4f MTE_pooled[1,50]

di _n "--- Heckman Selection Model ---"
heckman ln_salary $X, select(masters = $Z $X) twostep
local heck2_lambda = e(mills)
local heck2_rho    = e(rho)
local heck2_sigma  = e(sigma)
est store heck_2step

heckman ln_salary $X, select(masters = $Z $X)
local heck_ml_rho    = tanh(_b[/athrho])
local heck_ml_sigma  = exp(_b[/lnsigma])
local heck_ml_lambda = `heck_ml_rho' * `heck_ml_sigma'
local heck_ml_chi2   = e(chi2_c)
local heck_ml_p      = e(p_c)
est store heck_ml
di "Heckman ML: lambda = " %6.4f `heck_ml_lambda' ///
   "  rho = " %6.4f `heck_ml_rho' ///
   "  LR chi2 = " %6.2f `heck_ml_chi2' " (p = " %5.4f `heck_ml_p' ")"

********************************************************************************
* SECTION 6d: MTE BY GRADUATE PROGRAM AREA — SEPARATE mtefe PER AREA
*
* Group a = completers in area a (ma_a==1) plus non-completers
* prospectively assigned to area a (ma_a_pro==1). mtefe is fitted
* separately within each group, so each area has its own selection model,
* MTE curve, and ATE / ATT / ATU / LATE.
*
* The area models use a LINEAR MTE (pol(1)); the pooled model uses a
* quadratic (pol(2)). With 500-2,000 students per group, the quadratic
* produced unstable, implausible curves (e.g., MTE below -4 at high u and
* a negative STEM ATE); the linear form has one fewer k(u) parameter.
*
* Prospective assignment of non-completers: because ma_* = 0 for all
* untreated observations by construction, untreated students are assigned
* to the area they would most likely have entered, using their
* undergraduate major and the same transition probabilities as Section 1b.
* This is a counterfactual assignment, not an observed classification.
*
* Transition probabilities (mirroring Section 1b):
*   STEM:      stem_major,    p <= 0.55
*   Business:  bus_major,     p <= 0.65 (conditional on not STEM)
*   Education: ed_major,      p <= 0.70 (conditional on not STEM/Bus)
*   Health:    socsci_major   p <= 0.40 OR stem_major p in (0.55, 0.75]
*   Other:     residual
* Seed 20260102 (distinct from treated seed 20251130).
*
* Caution: groups are much smaller than the pooled sample (Business has
* about 179 completers), so area estimates are imprecise.
********************************************************************************

if `run_areas' == 1 {
    di _n "=============================================="
    di "MTE BY GRADUATE PROGRAM AREA (separate mtefe per area)"
    di "=============================================="

    * Use prospective areas from the dataset when present
    * (Example_7_5_3_updated.dta), so Stata and R share the same draws.
    capture confirm variable ma_stem_pro
    if _rc == 0 {
        di "Prospective areas (ma_*_pro) found in dataset; no new draws."
    }
    else {
        gen ma_stem_pro      = 0
        gen ma_business_pro  = 0
        gen ma_education_pro = 0
        gen ma_health_pro    = 0
        gen ma_other_pro     = 0

        label var ma_stem_pro      "Prospective STEM master's (untreated)"
        label var ma_business_pro  "Prospective Business master's (untreated)"
        label var ma_education_pro "Prospective Education master's (untreated)"
        label var ma_health_pro    "Prospective Health master's (untreated)"
        label var ma_other_pro     "Prospective Other master's (untreated)"

        set seed 20260102
        gen _rma_u = runiform() if masters == 0
        replace _rma_u = . if masters == 1

        replace ma_stem_pro      = 1 if masters==0 & stem_major==1   & _rma_u <= 0.55
        replace ma_business_pro  = 1 if masters==0 & bus_major==1    & _rma_u <= 0.65 ///
                                                   & ma_stem_pro==0
        replace ma_education_pro = 1 if masters==0 & ed_major==1     & _rma_u <= 0.70 ///
                                                   & ma_stem_pro==0 & ma_business_pro==0
        replace ma_health_pro    = 1 if masters==0 & socsci_major==1 & _rma_u <= 0.40 ///
                                                   & ma_stem_pro==0 & ma_business_pro==0 ///
                                                   & ma_education_pro==0
        replace ma_health_pro    = 1 if masters==0 & stem_major==1   & _rma_u >  0.55 ///
                                                   & _rma_u <= 0.75  & ma_stem_pro==0
        replace ma_other_pro     = 1 if masters==0 & ma_stem_pro==0 & ma_business_pro==0 ///
                                                   & ma_education_pro==0 & ma_health_pro==0
        drop _rma_u
    }

    gen _pro_check = ma_stem_pro + ma_business_pro + ma_education_pro ///
                   + ma_health_pro + ma_other_pro if masters == 0
    qui count if masters==0 & _pro_check != 1
    if r(N) > 0 di "WARNING: " r(N) " untreated obs with != 1 prospective area flag"
    else         di "CHECK PASSED: all untreated obs have exactly 1 prospective area"
    drop _pro_check

    * Area groups and area-specific mtefe
    foreach a in stem business education health other {
        gen byte grp_`a' = (ma_`a' == 1 | ma_`a'_pro == 1)
        label var grp_`a' "Area group `a': completers in area + prospective non-completers"
    }

    di _n "Area      N     Completers   ATE       ATT       ATU       LATE"
    di "-------------------------------------------------------------------"
    foreach a in stem business education health other {
        qui count if grp_`a' == 1
        local n_`a' = r(N)
        qui count if grp_`a' == 1 & masters == 1
        local ntr_`a' = r(N)

        capture noisily mtefe ln_salary $X_controls (masters = ga_funding_adj) ///
            if grp_`a' == 1, pol(1) bootreps(0) noplot savepropensity(p_area_`a')
        if _rc == 0 {
            est store mtefe_`a'
            local ate_`a'  = _b[effects:ate]
            local att_`a'  = _b[effects:att]
            local atu_`a'  = _b[effects:atut]
            local late_`a' = _b[effects:late]
            matrix EB_`a'  = e(b)
            matrix MTE_`a' = EB_`a'[1, "mte:"]
            matrix SUP_`a' = e(support)
            * Common support of the area's propensity score (treated vs untreated)
            qui sum p_area_`a' if grp_`a' == 1 & masters == 1
            local lo1 = r(min)
            local hi1 = r(max)
            qui sum p_area_`a' if grp_`a' == 1 & masters == 0
            local lo_`a' = max(`lo1', r(min))
            local hi_`a' = min(`hi1', r(max))
        }
        else {
            di as error "mtefe failed for area `a' (rc = " _rc ")"
            foreach s in ate att atu late {
                local `s'_`a' = .
            }
            local lo_`a' = .
            local hi_`a' = .
        }
    }

    foreach a in stem business education health other {
        di "  `a'" _col(11) %5.0f `n_`a'' "   " %5.0f `ntr_`a'' "     " ///
           %7.4f `ate_`a'' "   " %7.4f `att_`a'' "   " %7.4f `atu_`a'' "   " %7.4f `late_`a''
    }
    di _n "Common support of each area's propensity score:"
    foreach a in stem business education health other {
        di "  `a': [" %5.3f `lo_`a'' ", " %5.3f `hi_`a'' "]"
    }
}
else {
    di _n "Section 6d skipped (run_areas = 0). Set run_areas = 1 at the top of the script to run the area-specific models."
}

********************************************************************************
* SECTION 6e: STATE-CLUSTERED BOOTSTRAP (mtefe re-estimated each rep)
*
* Each replication resamples the 50 states with replacement and
* re-estimates the pooled mtefe model (probit + MTE) and the five area
* models. Standard errors are the standard deviations of the replicates;
* the MTE confidence band in Fig 11.3 uses pointwise 2.5th and 97.5th
* percentiles.
*
* RUN TIME: see the table in the header. Each mtefe call takes about
* 8 seconds on the author's machine (Stata MP, 2 cores), largely
* regardless of sample size, so a replication with the pooled model plus
* five area models takes about 50 seconds (about 7 hours for R = 500).
* The projected total is printed after the first replication.
*
* PUBLISHED DRAWS (default, use_saved_boot = 1): the script reuses the
* author's 500 replications in mte_bootstrap_draws.dta. If the file is
* not in the working directory, it is downloaded from the book's data
* repository; if the download fails, the full bootstrap runs instead.
* Set use_saved_boot = 0 to run the bootstrap yourself. Each full run
* overwrites mte_bootstrap_draws.dta with the new draws.
*
* CAUTION: the published draws are valid ONLY for the specification in
* the book (same data, models, controls, R = 500, seed 20260101). After
* ANY change to the data or models, set use_saved_boot = 0. See the
* RUN TIME note in the header.
*
* Implementation: postfile/forvalues loop with a local "ok" flag and NO
* continue statements (avoids the r(198) issue documented earlier).
********************************************************************************

local R              = 500
local boot_areas     = `run_areas'   // follows the switch at the top of the script
local use_saved_boot = 1             // 1 = reuse published draws; 0 = run bootstrap
local bootfile       "mte_bootstrap_draws.dta"
local booturl        "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch11/mte_bootstrap_draws.dta"

if `use_saved_boot' == 1 {
    capture confirm file "`bootfile'"
    if _rc != 0 {
        di as text "Bootstrap draws not found locally; downloading published draws from GitHub..."
        capture copy "`booturl'" "`bootfile'", replace
        if _rc != 0 {
            di as error "Download of `bootfile' failed; running the full bootstrap instead."
            di as error "(See the RUN TIME note in the header: this takes several hours.)"
            local use_saved_boot = 0
        }
    }
}

* Basic consistency checks on the saved draws (they cannot detect every
* change to the specification; see the CAUTION note in the header)
if `use_saved_boot' == 1 {
    preserve
        quietly use "`bootfile'", clear
        quietly count
        local n_saved = r(N)
        capture confirm variable b_ate_stem m99
        local has_vars = (_rc == 0)
    restore
    if `n_saved' != `R' {
        di as error "WARNING: `bootfile' has `n_saved' replications but R = `R'."
    }
    if `run_areas' == 1 & `has_vars' == 0 {
        di as error "WARNING: `bootfile' lacks the area-model draws; running the full bootstrap."
        local use_saved_boot = 0
    }
}

if `use_saved_boot' == 0 {
    local mvars ""
    forvalues j = 1/99 {
        local mvars `mvars' m`j'
    }
    local avars ""
    foreach a in stem business education health other {
        local avars `avars' b_ate_`a' b_att_`a' b_atu_`a' b_late_`a'
    }

    tempname bsh
    postfile `bsh' rep b_ate b_att b_atu b_late b_mprte1 `avars' `mvars' ///
        using "`bootfile'", replace

    set seed 20260101
    local n_ok = 0

    di _n "Running state-clustered bootstrap (G=50, R=`R' reps)..."
    di "Each dot = 10 reps completed. See the projected run time below."

    timer clear 2
    timer on 2

    forvalues b = 1/`R' {

        preserve
        local ok = 1

        capture quietly bsample, cluster(state) idcluster(newstate)
        if _rc != 0 local ok = 0

        * --- Pooled model ---
        if `ok' == 1 {
            capture quietly mtefe ln_salary $X_controls ///
                (masters = ga_funding_adj), pol(2) bootreps(0) noplot
            if _rc != 0 local ok = 0
        }

        if `ok' == 1 {
            local r_ate    = _b[effects:ate]
            local r_att    = _b[effects:att]
            local r_atu    = _b[effects:atut]
            local r_late   = _b[effects:late]
            local r_mprte1 = _b[effects:mprte1]
            matrix _EBr = e(b)
            matrix _Mr  = _EBr[1, "mte:"]
            local mpost ""
            forvalues j = 1/99 {
                local mpost `mpost' (`=_Mr[1,`j']')
            }

            * --- Area models ---
            local apost ""
            foreach a in stem business education health other {
                local arc = 1
                if `boot_areas' == 1 {
                    capture quietly mtefe ln_salary $X_controls ///
                        (masters = ga_funding_adj) if grp_`a' == 1, ///
                        pol(1) bootreps(0) noplot
                    local arc = _rc
                }
                if `arc' == 0 {
                    local apost `apost' (`=_b[effects:ate]') (`=_b[effects:att]') ///
                                        (`=_b[effects:atut]') (`=_b[effects:late]')
                }
                else {
                    local apost `apost' (.) (.) (.) (.)
                }
            }

            post `bsh' (`b') (`r_ate') (`r_att') (`r_atu') (`r_late') ///
                (`r_mprte1') `apost' `mpost'
            local ++n_ok
        }

        restore

        if `b' == 1 {
            timer off 2
            qui timer list 2
            local secs1 = r(t2)
            di _n "First replication took " %5.1f `secs1' " seconds." ///
               " Projected total: about " %5.0f (`secs1'*`R'/60) " minutes."
        }
        if mod(`b', 10) == 0 di "." _continue
    }

    postclose `bsh'
    di _n "Bootstrap complete: `n_ok' of `R' reps successful"
}
else {
    di _n "Using saved bootstrap draws from `bootfile' (`n_saved' replications)."
    di as text "NOTE: these are the published draws for the book's specification."
    di as text "If you changed the data, controls, or models, set use_saved_boot = 0"
    di as text "in Section 6e and rerun; otherwise the standard errors and"
    di as text "confidence bands below do not correspond to your estimates."
}

* Standard errors and MTE confidence band from the bootstrap draws
matrix MLO = J(1, 99, .)
matrix MHI = J(1, 99, .)
preserve
    use "`bootfile'", clear
    qui count
    local n_boot = r(N)
    foreach s in ate att atu late mprte1 {
        qui sum b_`s'
        local `s'_se = r(sd)
    }
    if `run_areas' == 1 {
        foreach a in stem business education health other {
            foreach s in ate att atu late {
                qui sum b_`s'_`a'
                local `s'_se_`a' = r(sd)
            }
            qui count if !missing(b_ate_`a')
            local nb_`a' = r(N)
        }
    }
    forvalues j = 1/99 {
        _pctile m`j', p(2.5 97.5)
        matrix MLO[1,`j'] = r(r1)
        matrix MHI[1,`j'] = r(r2)
    }
restore

di _n "--- Bootstrap SEs: Pooled mtefe Parameters (`n_boot' reps) ---"
di "  ATE    = " %7.4f `ate_est'    "  (BS SE = " %6.4f `ate_se'    ")"
di "  ATT    = " %7.4f `att_est'    "  (BS SE = " %6.4f `att_se'    ")"
di "  ATU    = " %7.4f `atu_est'    "  (BS SE = " %6.4f `atu_se'    ")"
di "  LATE   = " %7.4f `late_est'   "  (BS SE = " %6.4f `late_se'   ")"
di "  MPRTE1 = " %7.4f `mprte1_est' "  (BS SE = " %6.4f `mprte1_se' ")"

if `run_areas' == 1 {
    foreach s in ate att atu {
        di _n "--- Bootstrap SEs: Area-Specific " upper("`s'") " ---"
        di "Area          Estimate   BS SE    95% CI                  Sig   Reps"
        di "-------------------------------------------------------------------"
        foreach a in other stem business education health {
            local lo = ``s'_`a'' - 1.96 * ``s'_se_`a''
            local hi = ``s'_`a'' + 1.96 * ``s'_se_`a''
            local sig = cond(`lo' > 0, "***", cond(`hi' < 0, "***", "   "))
            di "  `a'" _col(14) %7.4f ``s'_`a'' "    " %6.4f ``s'_se_`a'' ///
               "   [" %7.4f `lo' ", " %7.4f `hi' "]   `sig'   " `nb_`a''
        }
    }
    di "  *** = 95% CI excludes zero (p < 0.05, two-tailed)"
    di "  Note: ATU based on prospective program area assignment (seed 20260102)."
}

* -----------------------------------------------------------------------
* Wild cluster bootstrap for OLS and IV stages
*   Webb weights, 9999 reps: runs in seconds, valid for G=50.
*   Requires: ssc install fwildclusterboot
* -----------------------------------------------------------------------

di _n "--- Wild Cluster Bootstrap: OLS ---"
qui reg ln_salary masters $X_controls
boottest masters, cluster(state) reps(9999) nograph
di "  p-value (wild cluster): " %6.4f r(p)

di _n "--- Wild Cluster Bootstrap: IV ---"
qui ivregress 2sls ln_salary $X_controls (masters = ga_funding_adj)
boottest masters, cluster(state) reps(9999) nograph
di "  p-value (wild cluster): " %6.4f r(p)

********************************************************************************
* SECTION 7: Results Comparison
********************************************************************************

di _n "=============================================="
di "RESULTS COMPARISON"
di "=============================================="

di "  Naive OLS:                 " %7.4f `ols_est'  " (SE = " %6.4f `ols_se' " — likely biased)"
di "  IV/2SLS:                   " %7.4f `iv_est'   " (SE = " %6.4f `iv_se'  " — complier effect)"
di "  mtefe LATE:                " %7.4f `late_est' " (BS SE = " %6.4f `late_se' ")"
di "  mtefe ATE:                 " %7.4f `ate_est'  " (BS SE = " %6.4f `ate_se'  ")"
di "  mtefe ATT:                 " %7.4f `att_est'  " (BS SE = " %6.4f `att_se'  ")"
di "  mtefe ATU:                 " %7.4f `atu_est'  " (BS SE = " %6.4f `atu_se'  ")"
di "  mtefe MPRTE1:              " %7.4f `mprte1_est' " (BS SE = " %6.4f `mprte1_se' ")"
di _n "  Sensitivity — mtefe linear MTE (pol=1):"
di "    ATE = " %7.4f `ate_lin' "  ATT = " %7.4f `att_lin' ///
   "  ATU = " %7.4f `atu_lin' "  LATE = " %7.4f `late_lin'
di _n "  Illustration — manual cubic (Section 6b, not used below):"
di "    ATE = " %7.4f `ate_man_cubic' "  ATT = " %7.4f `att_man' "  ATU = " %7.4f `atu_man'

if `att_est' > `ate_est' & `ate_est' > `atu_est' {
    di _n "  ATT > ATE > ATU: POSITIVE SELECTION on gains"
}
else if `att_est' < `ate_est' & `ate_est' < `atu_est' {
    di _n "  ATT < ATE < ATU: NEGATIVE (REVERSE) SELECTION on gains"
}
else {
    di _n "  Mixed selection pattern"
}

local ols_bias = (`ols_est' - `ate_est') / `ate_est' * 100
di "OLS BIAS: " %6.1f `ols_bias' "% relative to mtefe ATE"

estimates table ols_naive iv_2sls, ///
    stats(N r2) b(%7.4f) se(%7.4f) keep(masters) ///
    title("OLS vs. IV Estimates of Master's Degree Effect")

********************************************************************************
* SECTION 8: MTE Visualization
*   Fig 11.3  Pooled MTE curve (mtefe) with 95% bootstrap band
*   Fig 11.5  MTE curves by graduate program area (separate mtefe)
*   (Fig 11.7, MTE by propensity-score decile, has been removed: it
*    repeated Fig 11.3.)
********************************************************************************

di _n "=============================================="
di "MTE VISUALIZATION"
di "=============================================="

* Fig 11.3: pooled MTE with bootstrap band
preserve
    clear
    set obs 99
    gen u       = .
    gen mte_est = .
    gen mte_lo  = .
    gen mte_hi  = .
    forvalues j = 1/99 {
        qui replace u       = SUP[`j',1]        in `j'
        qui replace mte_est = MTE_pooled[1,`j'] in `j'
        qui replace mte_lo  = MLO[1,`j']        in `j'
        qui replace mte_hi  = MHI[1,`j']        in `j'
    }
    label var u "Unobserved Resistance to Treatment"
    label var mte_est "Marginal Treatment Effect"
    twoway (rarea mte_lo mte_hi u, fcolor(gs13) lwidth(none)) ///
           (line mte_est u, lcolor(gs0) lwidth(medthick) lpattern(solid)), ///
           ytitle("Marginal Treatment Effect") ///
           xtitle("u (Unobserved Resistance to Treatment)") ///
           title("Estimated MTE Curve - Pooled") ///
           subtitle("Master's Degree Effect on Log Salary") ///
           note("MTE at mean X (mtefe, quadratic). Shaded: 95% state-clustered bootstrap band." ///
                "Declining MTE indicates positive selection on gains.") ///
           yline(0, lpattern(dash) lcolor(gs8)) ///
           legend(order(2 "Estimated MTE" 1 "95% bootstrap band") size(small)) ///
           name(fig11_3, replace)
    graph save "$graphs_dir/fig11_3.gph", replace
    graph export "$graphs_dir/fig11_3_mte_curve_Stata.png", replace width(1200)
restore

if `run_areas' == 1 {
    * Fig 11.5: area-specific MTE curves, each restricted to its common support
    preserve
        clear
        set obs 99
        gen u = _n/100
        foreach a in stem business education health other {
            gen mte_`a' = .
            capture confirm matrix MTE_`a'
            if _rc == 0 {
                forvalues j = 1/99 {
                    qui replace mte_`a' = MTE_`a'[1,`j'] in `j'
                }
                qui replace mte_`a' = . if u < `lo_`a'' | u > `hi_`a''
            }
        }
        rename mte_education mte_educ
        twoway (line mte_health   u, lcolor(gs0) lwidth(medthick) lpattern(solid))     ///
               (line mte_stem     u, lcolor(gs0) lwidth(medthick) lpattern(dash))       ///
               (line mte_business u, lcolor(gs0) lwidth(medthick) lpattern(longdash))   ///
               (line mte_educ     u, lcolor(gs8) lwidth(medthick) lpattern(solid))      ///
               (line mte_other    u, lcolor(gs8) lwidth(medthick) lpattern(dash)),      ///
            ytitle("Marginal Treatment Effect") ///
            xtitle("u (Unobserved Resistance to Treatment)") ///
            title("MTE Curves by Graduate Program Area") ///
            subtitle("Field-specific returns to master's degree") ///
            yline(0, lpattern(shortdash) lcolor(gs10)) ///
            note("Separate mtefe model per area (linear MTE), at the area's mean X." ///
                 "Curves restricted to each area's common propensity-score support.") ///
            legend(order(1 "Health & Related" 2 "STEM" 3 "Business" ///
                         4 "Education" 5 "Other") ///
                   cols(3) size(small)) ///
            name(fig11_5, replace)
        graph save "$graphs_dir/fig11_5.gph", replace
        graph export "$graphs_dir/fig11_5_mte_byarea_curve_Stata.png", replace width(1200)
    restore
}

********************************************************************************
* SECTIONS 9-10b: POLICY-RELEVANT TREATMENT EFFECTS (PRTE) VIA mtefe
*
* Each scenario defines a counterfactual propensity score from the same
* probit used by mtefe (z_index, ga_coef). mtefe's prte() option then
* weights the MTE by the students whose completion the policy changes.
* PRTE standard errors shown are mtefe's analytic SEs (they ignore
* first-stage estimation error); see Section 6e for bootstrapped
* pooled parameters.
*
* Scenarios 5-8 target the undergraduate pipelines that feed each
* graduate area and use the pooled mtefe model, whose MTE varies with
* each student's X (including undergraduate major).
********************************************************************************

di _n "=============================================="
di "POLICY-RELEVANT TREATMENT EFFECTS (mtefe prte)"
di "=============================================="

qui sum ga_funding_adj
local ga_current = r(mean)
local ga_new = `ga_current' * 1.2
di "Current mean GA: $" %5.2f `ga_current' "k  Proposed (20% above mean): $" %5.2f `ga_new' "k"

* Section 9 scenario: every student's GA set to 120% of the current mean
gen double p_pol_ga20   = normal(z_index + `ga_coef'*(`ga_new' - ga_funding_adj))
* Section 10 scenarios 1-4
gen double p_pol_unif   = normal(z_index + `ga_coef'*1)
gen byte   targeted_lowinc = (parent_income_q <= 2)
gen double p_pol_lowinc = normal(z_index + `ga_coef'*2*targeted_lowinc)
gen double p_pol_stem   = normal(z_index + `ga_coef'*3*stem_major)
gen double p_pol_ed     = normal(z_index + `ga_coef'*2.5*ed_major)
* Section 10b scenarios 5-8 ($2.5k to each pipeline)
gen byte   target_health = (stem_major == 1 | socsci_major == 1)
gen double p_pol_pstem  = normal(z_index + `ga_coef'*2.5*stem_major)
gen double p_pol_pbus   = normal(z_index + `ga_coef'*2.5*bus_major)
gen double p_pol_ped    = normal(z_index + `ga_coef'*2.5*ed_major)
gen double p_pol_phlth  = normal(z_index + `ga_coef'*2.5*target_health)

local lbl_ga20   "GA set to 120% of mean (Sec. 9)"
local lbl_unif   "Uniform +$1k (Scen. 1)"
local lbl_lowinc "Low-income +$2k (Scen. 2)"
local lbl_stem   "STEM ug +$3k (Scen. 3)"
local lbl_ed     "Education ug +$2.5k (Scen. 4)"
local lbl_pstem  "STEM pipeline +$2.5k (Scen. 5)"
local lbl_pbus   "Business pipeline +$2.5k (Scen. 6)"
local lbl_ped    "Education pipeline +$2.5k (Scen. 7)"
local lbl_phlth  "Health pipeline +$2.5k (Scen. 8)"

di _n "Scenario                              Mean dP     PRTE    (SE, analytic)"
di "-------------------------------------------------------------------------"
foreach s in ga20 unif lowinc stem ed pstem pbus ped phlth {
    gen double _dp = p_pol_`s' - phat
    qui sum _dp
    local dp_`s' = r(mean)
    drop _dp
    quietly mtefe ln_salary $X_controls (masters = ga_funding_adj), ///
        pol(2) bootreps(0) noplot prte(p_pol_`s')
    local prte_`s'    = _b[effects:prte]
    local prte_se_`s' = _se[effects:prte]
    matrix W_`s' = e(weightsprte)
    di "  `lbl_`s''" _col(39) %7.4f `dp_`s'' "   " %7.4f `prte_`s'' "   (" %6.4f `prte_se_`s'' ")"
}
di _n "  Marginal versions (pooled mtefe): MPRTE1 = " %6.4f `mprte1_est' ///
   "  MPRTE2 = " %6.4f `mprte2_est' "  MPRTE3 = " %6.4f `mprte3_est'

********************************************************************************
* SECTION 11: PRTE BY POLICY INTENSITY (uniform GA increase)
********************************************************************************

di _n "=============================================="
di "PRTE BY POLICY INTENSITY"
di "=============================================="

matrix INT = J(20, 2, .)
forvalues i = 1/20 {
    local inc = `i' * 0.5
    capture drop _p_int
    gen double _p_int = normal(z_index + `ga_coef'*`inc')
    quietly mtefe ln_salary $X_controls (masters = ga_funding_adj), ///
        pol(2) bootreps(0) noplot prte(_p_int)
    matrix INT[`i',1] = `inc'
    matrix INT[`i',2] = _b[effects:prte]
    drop _p_int
}

preserve
    clear
    svmat double INT, names(col)
    rename c1 ga_increase
    rename c2 prte_int
    list ga_increase prte_int, noobs clean
    twoway (line prte_int ga_increase, lcolor(gs0) lwidth(medthick) lpattern(solid)), ///
        ytitle("PRTE") xtitle("GA Funding Increase ($1000s)") ///
        title("PRTE by Policy Intensity") ///
        subtitle("Average return for students induced by a uniform GA increase") ///
        yline(0, lpattern(dash) lcolor(gs8)) yscale(range(0)) ///
        name(fig11_8, replace)
    graph save "$graphs_dir/fig11_8.gph", replace
    graph export "$graphs_dir/fig11_8_mprte_by_intensity_Stata.png", replace width(1200)
restore

********************************************************************************
* SECTION 12: COMPARING TREATMENT EFFECT PARAMETERS
********************************************************************************

di _n "=============================================="
di "COMPARISON OF TREATMENT EFFECT PARAMETERS"
di "=============================================="

di "Parameter     mtefe (pol=2)   BS SE     mtefe (pol=1)"
di "======================================================"
di "ATE           " %7.4f `ate_est'  "      (" %6.4f `ate_se'  ")   " %7.4f `ate_lin'
di "ATT           " %7.4f `att_est'  "      (" %6.4f `att_se'  ")   " %7.4f `att_lin'
di "ATU           " %7.4f `atu_est'  "      (" %6.4f `atu_se'  ")   " %7.4f `atu_lin'
di "LATE          " %7.4f `late_est' "      (" %6.4f `late_se' ")   " %7.4f `late_lin'
di "2SLS          " %7.4f `iv_est'   "      (" %6.4f `iv_se'   ")"
di "MPRTE1        " %7.4f `mprte1_est' "      (" %6.4f `mprte1_se' ")"
di "------------------------------------------------------"
di "PRTE by scenario (analytic SEs):"
foreach s in unif lowinc stem ed pstem pbus ped phlth {
    di "  `lbl_`s''" _col(39) %7.4f `prte_`s'' "   (" %6.4f `prte_se_`s'' ")"
}

********************************************************************************
* SECTION 13: POLICY WEIGHTS AND PROPENSITY-SCORE SUPPORT
*   Fig 11.6  MTE curve with PRTE weights for two policies
*   Fig 11.4  Propensity-score distribution with the MTE curve
********************************************************************************

* Fig 11.6: MTE with policy weights (uniform +$1k vs low-income +$2k)
preserve
    clear
    set obs 99
    gen u      = .
    gen mte    = .
    gen w_unif = .
    gen w_low  = .
    forvalues j = 1/99 {
        qui replace u      = SUP[`j',1]        in `j'
        qui replace mte    = MTE_pooled[1,`j'] in `j'
        qui replace w_unif = W_unif[`j',1]     in `j'
        qui replace w_low  = W_lowinc[`j',1]   in `j'
    }
    twoway (area w_unif u, yaxis(2) fcolor(gs13) lcolor(gs10))        ///
           (line w_low  u, yaxis(2) lcolor(gs6) lwidth(medthick) lpattern(dash)) ///
           (line mte    u, yaxis(1) lcolor(gs0) lwidth(medthick) lpattern(solid)), ///
        yline(0, lpattern(shortdash) lcolor(gs8) axis(1)) ///
        ytitle("Marginal Treatment Effect", axis(1)) ///
        ytitle("Policy weight", axis(2)) ///
        xtitle("u (Unobserved Resistance to Treatment)") ///
        title("MTE Curve and Policy-Relevant Weights") ///
        note("Weights show where each policy's induced completers lie on the resistance scale.") ///
        legend(order(3 "Estimated MTE" 1 "Uniform +$1k weights" ///
                     2 "Low-income +$2k weights") cols(2) size(small)) ///
        name(fig11_6, replace)
    graph save   "$graphs_dir/fig11_6.gph", replace
    graph export "$graphs_dir/fig11_6_mte_policy_regions_Stata.png", replace width(1200)
restore

* Fig 11.4: propensity-score histogram with the MTE curve
preserve
    gen p_bin = floor(phat * 20) / 20 + 0.025
    collapse (count) n_bin = id, by(p_bin)
    local N0 = _N
    set obs `=`N0' + 99'
    gen u   = .
    gen mte = .
    forvalues j = 1/99 {
        local r = `N0' + `j'
        qui replace u   = SUP[`j',1]        in `r'
        qui replace mte = MTE_pooled[1,`j'] in `r'
    }
    twoway (bar n_bin p_bin, barwidth(0.045) fcolor(gs12) lcolor(gs8) yaxis(2)) ///
           (line mte u, lcolor(gs0) lwidth(medthick) lpattern(solid) yaxis(1)), ///
        yline(0, lpattern(dash) lcolor(gs8) axis(1)) ///
        ytitle("Marginal Treatment Effect", axis(1)) ///
        ytitle("Number of students", axis(2)) ///
        xtitle("Propensity score / u") ///
        title("MTE and the Distribution of Propensity Scores") ///
        note("Bars: students by propensity score. Line: MTE at mean X (mtefe)." ///
             "Where bars are short, the curve relies on the polynomial form.") ///
        legend(order(2 "Estimated MTE" 1 "Students (count)") size(small)) ///
        name(fig11_4, replace)
    graph save "$graphs_dir/fig11_4.gph", replace
    graph export "$graphs_dir/fig11_4_mte_by_propensity_Stata.png", replace width(1200)
restore

********************************************************************************
* SECTION 14: POLICY COST-BENEFIT ANALYSIS (PRTE-based)
********************************************************************************

local cost_per_degree = 50000
local career_years    = 30
local discount_rate   = 0.03
local base_salary     = 47000
local pv_factor = (1 - (1 + `discount_rate')^(-`career_years')) / `discount_rate'
di "Present value factor (30 years, 3%): " %6.2f `pv_factor'

di _n "--- Scenarios 1-4: PRTE-based CBA ---"
di "Policy                              PRTE    Annual Gain   PV Gain    B/C Ratio"
di "================================================================================"
foreach s in unif lowinc stem ed {
    local ag  = `base_salary' * (exp(`prte_`s'') - 1)
    local pvg = `ag' * `pv_factor'
    di "  `lbl_`s''" _col(37) %6.4f `prte_`s'' "   $" %7.0f `ag' "   $" %9.0f `pvg' "   " %5.2f `pvg'/`cost_per_degree'
}

di _n "--- Scenarios 5-8: Graduate Pipeline PRTE-based CBA ---"
local base_pstem = 65000
local base_pbus  = 60000
local base_ped   = 42000
local base_phlth = 68000
di "  Base salaries: STEM=$" %6.0f `base_pstem' " Business=$" %6.0f `base_pbus' ///
   " Ed=$" %6.0f `base_ped' " Health=$" %6.0f `base_phlth'
di "Pipeline                            PRTE    Annual Gain   PV Gain    B/C Ratio"
di "================================================================================"
foreach s in pstem pbus ped phlth {
    local ag  = `base_`s'' * (exp(`prte_`s'') - 1)
    local pvg = `ag' * `pv_factor'
    di "  `lbl_`s''" _col(37) %6.4f `prte_`s'' "   $" %7.0f `ag' "   $" %9.0f `pvg' "   " %5.2f `pvg'/`cost_per_degree'
}
di "Note: B/C > 1 suggests policy expansion is beneficial (synthetic data only)."

********************************************************************************
* SECTION 15: Save Results
********************************************************************************

drop p_pol_* targeted_lowinc target_health

label var phat    "Estimated propensity score"
label var z_index "Probit linear index"

save "bb_mte_analysis.dta", replace

preserve
    collapse (mean) masters ln_salary phat mte_hat ///
             ma_stem ma_business ma_education ma_health ma_other ///
             (sd) sd_mte=mte_hat (count) n=id, by(stem_major ed_major)
    export delimited "mte_summary_by_field.csv", replace
restore

preserve
    keep if masters == 1
    collapse (mean) ln_salary salary phat mte_hat ///
             (count) n=id,                        ///
             by(ma_stem ma_business ma_education ma_health ma_other)
    export delimited "mte_summary_by_program_area.csv", replace
restore

********************************************************************************
* SECTION 16: FINAL SUMMARY
********************************************************************************

di _n "=============================================="
di "ANALYSIS COMPLETE"
di "=============================================="
di "  1.  Treatment rate:          " %5.3f `treat_rate'
di "  2.  First-stage F:           " %6.1f `first_stage_F'
di "  3.  OLS estimate (biased):   " %7.4f `ols_est'
di "  4.  IV/2SLS (LATE):          " %7.4f `iv_est'
di "  5.  mtefe ATE:               " %7.4f `ate_est'  " (BS SE = " %6.4f `ate_se'  ")"
di "  6.  mtefe ATT:               " %7.4f `att_est'  " (BS SE = " %6.4f `att_se'  ")"
di "  7.  mtefe ATU:               " %7.4f `atu_est'  " (BS SE = " %6.4f `atu_se'  ")"
di "  8.  mtefe LATE:              " %7.4f `late_est' " (BS SE = " %6.4f `late_se' ")"
di "  9.  mtefe MPRTE1:            " %7.4f `mprte1_est' " (BS SE = " %6.4f `mprte1_se' ")"
di " 10.  Essential heterogeneity: p = " %6.4f `p_ess_het'

if `run_areas' == 1 {
    di _n "AREA-SPECIFIC ATE, ATT, AND ATU (separate mtefe per area)"
    di "(State-clustered bootstrap, G=50, seed 20260101)"
    di "ATU based on prospective program area assignment (seed 20260102)"
    di "=================================================================="
    foreach s in ate att atu {
        di _n upper("`s'") " estimates:"
        di "Area          Estimate   (BS SE)    95% CI                  Sig"
        di "------------------------------------------------------------------"
        foreach a in other stem business education health {
            local lo = ``s'_`a'' - 1.96 * ``s'_se_`a''
            local hi = ``s'_`a'' + 1.96 * ``s'_se_`a''
            local sig = cond(`lo' > 0, "***", cond(`hi' < 0, "***", "   "))
            di "  `a'" _col(14) %7.4f ``s'_`a'' "   (" %6.4f ``s'_se_`a'' ")   [" ///
               %7.4f `lo' ", " %7.4f `hi' "]   `sig'"
        }
    }
    di "  *** = 95% CI excludes zero (p < 0.05, two-tailed)"
    di "  Note: area groups are small (Business ~179 completers); interpret with caution."
}

di _n "PRTE SUMMARY (mtefe prte):"
foreach s in unif lowinc stem ed pstem pbus ped phlth {
    di "  `lbl_`s''" _col(39) %7.4f `prte_`s''
}

di _n "Bootstrap: G=50 state clusters, `n_boot' reps, seed(20260101)"
di "Files saved: bb_mte_analysis.dta, mte_summary_by_field.csv,"
di "             mte_summary_by_program_area.csv, `bootfile'"

di _n "IMPORTANT NOTE: Synthetic data — results illustrate methods only."
di "=============================================="
di "END OF MTE/MPRTE ANALYSIS"
di "=============================================="

capture graph display fig11_3
capture graph display fig11_4
capture graph display fig11_5
capture graph display fig11_6
capture graph display fig11_8

*------------------------------------------------------------------------
* Close this sub-script's dedicated log
* (Stata_code11.do's own master log, if any, remains open.)
*------------------------------------------------------------------------
di as text _n "MTE_MPRTE.do log closed: " c(current_date) " " c(current_time)
capture log close mte_mprte

*========================================================================
* END OF MTE_MPRTE.do
*========================================================================
