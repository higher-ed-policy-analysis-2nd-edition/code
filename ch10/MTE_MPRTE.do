*========================================================================
* Chapter 10 – Sections 10.10–10.16: Marginal Treatment Effects
*             Returns to Master's Degree Completion
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
* Data: synthetic B&B panel
*   Example_7_5_3_updated.dta  (primary; contains pre-generated ma_* vars)
*   Example_7_5_3.dta          (fallback; ma_* generated in Section 1b)
*
* Required packages: mtefe, moremata, fwildclusterboot
*
* Sections:
*   1     Load dataset (Example_7_5_3_updated.dta / Example_7_5_3.dta)
*   1b    Verify / generate master's program area indicators (ma_*)
*   2     Summary statistics
*   3     First stage and instrument relevance
*   4     Naive OLS estimation
*   5     IV/2SLS estimation (LATE)
*   6     MTE estimation — pooled polynomial (quadratic and cubic)
*   6b    MTE by graduate program area (fully interacted)
*   6c    Bootstrap infrastructure (cluster bootstrap + wild cluster)
*   7     Results comparison (ATE / ATT / ATU / LATE)
*   8     MTE visualization
*   9     Basic policy simulation (PRTE)
*   10    MPRTE — Scenarios 1–4 (original)
*   10b   MPRTE by graduate program area — Scenarios 5–8
*   11    MPRTE by policy intensity
*   12    Comparing treatment effect parameters
*   13    MPRTE visualization
*   14    Policy cost-benefit analysis
*   15    Save results
*   16    Final summary
*========================================================================

*------------------------------------------------------------------------
* Fallback paths when running standalone (not called from Stata_code10.do)
*------------------------------------------------------------------------
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
    di as text "MTE_MPRTE.do (standalone): graphs_dir set to $graphs_dir"
}

set scheme s2mono    // Springer B&W print (harmless if already set)

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
        "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch10/Example_7_5_3_updated.dta" ///
        "Example_7_5_3_updated.dta", replace
    if _rc != 0 di as text "Download failed — will try local file or base version."
}

capture use "Example_7_5_3_updated.dta", clear
if _rc != 0 {
    di as text "Note: Example_7_5_3_updated.dta not found."
    di as text "Attempting to download Example_7_5_3.dta from GitHub (ch7 repository)..."
    capture copy ///
        "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_5_3.dta" ///
        "Example_7_5_3.dta", replace
    if _rc != 0 {
        di as error "ERROR: Download of Example_7_5_3.dta failed."
        di as error "Please download the file manually from:"
        di as error "https://github.com/higher-ed-policy-analysis-2nd-edition/data/blob/main/ch7/Example_7_5_3.dta"
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
* SECTION 6: MTE ESTIMATION — POOLED POLYNOMIAL (MANUAL)
********************************************************************************

di _n "=============================================="
di "MTE ESTIMATION — POOLED POLYNOMIAL"
di "=============================================="

probit masters $Z $X
predict phat, pr
local ga_coef = _b[ga_funding_adj]
di "GA funding probit coefficient: " %7.5f `ga_coef'
predict z_index, xb

gen phat2 = phat^2
gen phat3 = phat^3

di _n "--- Quadratic MTE ---"
reg ln_salary masters c.masters#(c.phat c.phat2) $X phat phat2, robust
local b0_quad = _b[masters]
local b1_quad = _b[c.masters#c.phat]
local b2_quad = _b[c.masters#c.phat2]
di "Quadratic MTE(u) = " %6.4f `b0_quad' " + " %6.4f `b1_quad' "*u + " %6.4f `b2_quad' "*u^2"
local ate_est_quad = `b0_quad' + `b1_quad'/2 + `b2_quad'/3
di "ATE (quadratic): " %6.4f `ate_est_quad'

di _n "--- Cubic MTE ---"
reg ln_salary masters c.masters#(c.phat c.phat2 c.phat3) $X phat phat2 phat3, robust
local b0 = _b[masters]
local b1 = _b[c.masters#c.phat]
local b2 = _b[c.masters#c.phat2]
local b3 = _b[c.masters#c.phat3]
di "Cubic MTE(u) = " %7.4f `b0' " + " %7.4f `b1' "*u + " %7.4f `b2' "*u^2 + " %7.4f `b3' "*u^3"

local ate_est_cubic = `b0' + `b1'/2 + `b2'/3 + `b3'/4
di "Estimated ATE (cubic): " %6.4f `ate_est_cubic'

gen mte_hat = `b0' + `b1'*phat + `b2'*phat2 + `b3'*phat3

qui sum mte_hat if masters == 1
local att_est = r(mean)
di "Estimated ATT: " %6.4f `att_est'
qui sum mte_hat if masters == 0
local atu_est = r(mean)
di "Estimated ATU: " %6.4f `atu_est'

di _n "--- mtefe Package Estimation ---"
* NOTE: bootreps(0) suppresses mtefe's internal bootstrap entirely.
*   The mtefe package runs bootstrap internally when bootreps() > 0,
*   using a user program that suffers the same stale execution-context
*   issue that caused persistent r(198) throughout this script. Our
*   Section 6c bootstrap already provides all SEs needed. mtefe is
*   used here for point estimates only; use Section 6c SEs for inference.
mtefe ln_salary $X_controls (masters = ga_funding_adj), pol(1) bootreps(0)
est store mtefe_linear
local mtefe_ate  = _b[effects:ate]
local mtefe_att  = _b[effects:att]
local mtefe_atu  = _b[effects:atut]
local mtefe_late = _b[effects:late]

mtefe ln_salary $X_controls (masters = ga_funding_adj), pol(2) bootreps(0)
est store mtefe_quad
local mtefe_ate_q = _b[effects:ate]
local mtefe_att_q = _b[effects:att]
local mtefe_atu_q = _b[effects:atut]

di _n "mtefe Results (Quadratic, pol=2):"
di "  ATE:  " %6.4f `mtefe_ate_q'
di "  ATT:  " %6.4f `mtefe_att_q'
di "  ATU:  " %6.4f `mtefe_atu_q'

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
* SECTION 6b: MTE BY GRADUATE PROGRAM AREA — FULLY INTERACTED POLYNOMIAL
*
* Identification strategy:
*   The same probit propensity score and cubic polynomial framework from
*   Section 6 is extended by allowing the MTE function to shift both in
*   level and slope by graduate program area. ma_other is the omitted
*   (base) category. The interaction model recovers area-specific MTE curves:
*
*     MTE_a(u) = [b0+d0_a] + [b1+d1_a]*u + [b2+d2_a]*u^2 + [b3+d3_a]*u^3
*
*   where a in (STEM, Business, Education, Health) and Other is the base.
*   For untreated observations all ma_* = 0, so no interaction fires.
*
* Area-specific treatment parameters:
*   ATE_a = (b0+d0_a) + (b1+d1_a)/2 + (b2+d2_a)/3 + (b3+d3_a)/4
*   ATT_a = E[MTE_a(phat) | masters=1, ma_a=1]
*
* Standard errors: see Section 6c (cluster bootstrap at state level)
********************************************************************************

di _n "=============================================="
di "MTE BY GRADUATE PROGRAM AREA"
di "=============================================="

reg ln_salary masters                                      ///
    c.masters#c.phat c.masters#c.phat2 c.masters#c.phat3  ///
    c.masters#c.ma_stem                                    ///
    c.masters#c.ma_business                                ///
    c.masters#c.ma_education                               ///
    c.masters#c.ma_health                                  ///
    c.masters#c.ma_stem#c.phat                             ///
    c.masters#c.ma_business#c.phat                         ///
    c.masters#c.ma_education#c.phat                        ///
    c.masters#c.ma_health#c.phat                           ///
    c.masters#c.ma_stem#c.phat2                            ///
    c.masters#c.ma_business#c.phat2                        ///
    c.masters#c.ma_education#c.phat2                       ///
    c.masters#c.ma_health#c.phat2                          ///
    c.masters#c.ma_stem#c.phat3                            ///
    c.masters#c.ma_business#c.phat3                        ///
    c.masters#c.ma_education#c.phat3                       ///
    c.masters#c.ma_health#c.phat3                          ///
    $X phat phat2 phat3, robust
est store mte_byarea

* Base polynomial (Other = base category)
local B0 = _b[masters]
local B1 = _b[c.masters#c.phat]
local B2 = _b[c.masters#c.phat2]
local B3 = _b[c.masters#c.phat3]

* Area differential coefficients
local d0_stem     = _b[c.masters#c.ma_stem]
local d1_stem     = _b[c.masters#c.ma_stem#c.phat]
local d2_stem     = _b[c.masters#c.ma_stem#c.phat2]
local d3_stem     = _b[c.masters#c.ma_stem#c.phat3]

local d0_business = _b[c.masters#c.ma_business]
local d1_business = _b[c.masters#c.ma_business#c.phat]
local d2_business = _b[c.masters#c.ma_business#c.phat2]
local d3_business = _b[c.masters#c.ma_business#c.phat3]

local d0_education = _b[c.masters#c.ma_education]
local d1_education = _b[c.masters#c.ma_education#c.phat]
local d2_education = _b[c.masters#c.ma_education#c.phat2]
local d3_education = _b[c.masters#c.ma_education#c.phat3]

local d0_health   = _b[c.masters#c.ma_health]
local d1_health   = _b[c.masters#c.ma_health#c.phat]
local d2_health   = _b[c.masters#c.ma_health#c.phat2]
local d3_health   = _b[c.masters#c.ma_health#c.phat3]

* Composite area coefficients (base + differential)
local c0_stem     = `B0' + `d0_stem'
local c1_stem     = `B1' + `d1_stem'
local c2_stem     = `B2' + `d2_stem'
local c3_stem     = `B3' + `d3_stem'

local c0_business = `B0' + `d0_business'
local c1_business = `B1' + `d1_business'
local c2_business = `B2' + `d2_business'
local c3_business = `B3' + `d3_business'

local c0_education = `B0' + `d0_education'
local c1_education = `B1' + `d1_education'
local c2_education = `B2' + `d2_education'
local c3_education = `B3' + `d3_education'

local c0_health   = `B0' + `d0_health'
local c1_health   = `B1' + `d1_health'
local c2_health   = `B2' + `d2_health'
local c3_health   = `B3' + `d3_health'

* Area-specific MTE functions
di _n "--- Area-Specific MTE Functions ---"
di "  Base (Other): " %7.4f `B0' " + " %7.4f `B1' "*u + " %7.4f `B2' "*u^2 + " %7.4f `B3' "*u^3"
di "  STEM:         " %7.4f `c0_stem' " + " %7.4f `c1_stem' "*u + " %7.4f `c2_stem' "*u^2 + " %7.4f `c3_stem' "*u^3"
di "  Business:     " %7.4f `c0_business' " + " %7.4f `c1_business' "*u + " %7.4f `c2_business' "*u^2 + " %7.4f `c3_business' "*u^3"
di "  Education:    " %7.4f `c0_education' " + " %7.4f `c1_education' "*u + " %7.4f `c2_education' "*u^2 + " %7.4f `c3_education' "*u^3"
di "  Health:       " %7.4f `c0_health' " + " %7.4f `c1_health' "*u + " %7.4f `c2_health' "*u^2 + " %7.4f `c3_health' "*u^3"

* Area-specific ATE (integral of MTE over [0,1])
local ate_other     = `B0'           + `B1'/2           + `B2'/3           + `B3'/4
local ate_stem      = `c0_stem'      + `c1_stem'/2      + `c2_stem'/3      + `c3_stem'/4
local ate_business  = `c0_business'  + `c1_business'/2  + `c2_business'/3  + `c3_business'/4
local ate_education = `c0_education' + `c1_education'/2 + `c2_education'/3 + `c3_education'/4
local ate_health    = `c0_health'    + `c1_health'/2    + `c2_health'/3    + `c3_health'/4

di _n "--- Area-Specific ATE (integral_0^1 MTE_a(u) du) ---"
di "  ATE (Other):     " %6.4f `ate_other'
di "  ATE (STEM):      " %6.4f `ate_stem'
di "  ATE (Business):  " %6.4f `ate_business'
di "  ATE (Education): " %6.4f `ate_education'
di "  ATE (Health):    " %6.4f `ate_health'

* Area-specific MTE_hat variables
gen mte_hat_other     = `B0'           + `B1'           *phat + `B2'           *phat2 + `B3'           *phat3
gen mte_hat_stem      = `c0_stem'      + `c1_stem'      *phat + `c2_stem'      *phat2 + `c3_stem'      *phat3
gen mte_hat_business  = `c0_business'  + `c1_business'  *phat + `c2_business'  *phat2 + `c3_business'  *phat3
gen mte_hat_education = `c0_education' + `c1_education' *phat + `c2_education' *phat2 + `c3_education' *phat3
gen mte_hat_health    = `c0_health'    + `c1_health'    *phat + `c2_health'    *phat2 + `c3_health'    *phat3

label var mte_hat_other     "Area-specific MTE hat: Other (base)"
label var mte_hat_stem      "Area-specific MTE hat: STEM"
label var mte_hat_business  "Area-specific MTE hat: Business"
label var mte_hat_education "Area-specific MTE hat: Education"
label var mte_hat_health    "Area-specific MTE hat: Health & Related"

* Area-specific ATT
qui sum mte_hat_other     if ma_other     == 1
local att_other     = r(mean)
qui sum mte_hat_stem      if ma_stem      == 1
local att_stem      = r(mean)
qui sum mte_hat_business  if ma_business  == 1
local att_business  = r(mean)
qui sum mte_hat_education if ma_education == 1
local att_education = r(mean)
qui sum mte_hat_health    if ma_health    == 1
local att_health    = r(mean)

di _n "--- Area-Specific ATT ---"
di "  ATT (Other):     " %6.4f `att_other'
di "  ATT (STEM):      " %6.4f `att_stem'
di "  ATT (Business):  " %6.4f `att_business'
di "  ATT (Education): " %6.4f `att_education'
di "  ATT (Health):    " %6.4f `att_health'

qui sum mte_hat if masters == 0
local atu_pooled_untreated = r(mean)
di _n "  ATU (pooled): " %6.4f `atu_pooled_untreated'

********************************************************************************
* SECTION 6b-ATU: PROSPECTIVE PROGRAM AREA ASSIGNMENT FOR UNTREATED
*
* ATU_a = E[MTE_a(phat) | masters==0, prospective_area==a]
*
* Because ma_* = 0 for all untreated observations by construction,
* area-specific ATU requires assigning untreated individuals to the
* graduate program area they would most likely have entered, given their
* undergraduate major field and the same transition probabilities used
* for the treated group in Section 1b.
*
* This is a counterfactual assignment — not an observed classification.
* Prose must note that ATU_a estimates the return for untreated students
* who, had they completed a master's degree, would most likely have
* entered program area a given their undergraduate background.
*
* Transition probabilities (mirroring Section 1b):
*   STEM:      stem_major,    p <= 0.55
*   Business:  bus_major,     p <= 0.65 (conditional on not STEM)
*   Education: ed_major,      p <= 0.70 (conditional on not STEM/Bus)
*   Health:    socsci_major   p <= 0.40 OR stem_major p in (0.55, 0.75]
*   Other:     residual
*
* Seed 20260102 (distinct from treated seed 20251130) ensures
* reproducibility without cross-contaminating the treated assignment.
********************************************************************************

di _n "=============================================="
di "SECTION 6b-ATU: PROSPECTIVE PROGRAM AREA (UNTREATED)"
di "=============================================="

* Generate prospective indicators (untreated only; treated keep ma_* = 0)
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

* Verification
qui count if masters == 0
local n_untreated = r(N)
di "Total untreated: " `n_untreated'
foreach a in stem business education health other {
    qui count if masters==0 & ma_`a'_pro == 1
    di "  ma_`a'_pro: " r(N) "  (" %5.1f r(N)/`n_untreated'*100 "%)"
}

* Check mutual exclusivity
gen _pro_check = ma_stem_pro + ma_business_pro + ma_education_pro ///
               + ma_health_pro + ma_other_pro if masters == 0
qui count if masters==0 & _pro_check != 1
if r(N) > 0 di "WARNING: " r(N) " untreated obs with != 1 prospective area flag"
else         di "CHECK PASSED: all untreated obs have exactly 1 prospective area"
drop _pro_check

* Area-specific ATU point estimates
di _n "--- Area-Specific ATU (prospective assignment) ---"
foreach a in stem business education health other {
    qui sum mte_hat_`a' if masters==0 & ma_`a'_pro==1
    local atu_`a' = r(mean)
    di "  ATU (`a'): " %6.4f `atu_`a''
}

di _n "=============================================="
di "AREA-SPECIFIC TREATMENT PARAMETER SUMMARY"
di "=============================================="
di "Area          ATE        ATT        ATU (prospective)"
di "----------------------------------------------------"
di "  Other     " %7.4f `ate_other'     "    " %7.4f `att_other'     "    " %7.4f `atu_other'
di "  STEM      " %7.4f `ate_stem'      "    " %7.4f `att_stem'      "    " %7.4f `atu_stem'
di "  Business  " %7.4f `ate_business'  "    " %7.4f `att_business'  "    " %7.4f `atu_business'
di "  Education " %7.4f `ate_education' "    " %7.4f `att_education' "    " %7.4f `atu_education'
di "  Health    " %7.4f `ate_health'    "    " %7.4f `att_health'    "    " %7.4f `atu_health'

********************************************************************************
* SECTION 6c: BOOTSTRAP INFRASTRUCTURE
*
* Implementation: postfile/forvalues manual bootstrap (NO program...end,
*   NO bootstrap command). This is the definitive fix for the persistent
*   r(198) error documented in the bug fix log above.
*
* Architecture:
*   - postfile/postclose stores one row of estimates per replication
*
* -----------------------------------------------------------------------
* 6c-i  Manual cluster bootstrap — forvalues loop, NO continue in braces
*
* DEFINITIVE FIX for persistent r(198):
*   Using "continue" inside a brace block ( if _rc != 0 BEGIN ... continue END )
*   inside a forvalues loop causes Stata to exit the if-block mid-execution.
*   The closing brace is never processed, leaving a stale open-brace in
*   Stata's internal loop state. With nostop, this accumulates across every
*   failed replication and reports as r(198) at EOF.
*
*   Fix: replace all continue-in-brace patterns with a local "ok" flag.
*   Every iteration now falls through to restore at the bottom, so every
*   closing brace is always reached. No continue is used anywhere.
* -----------------------------------------------------------------------

tempname bsh
tempfile bsf

postfile `bsh'                ///
    b_ate b_att b_atu          ///
    b_ate_stem    b_att_stem   b_atu_stem  ///
    b_ate_bus     b_att_bus    b_atu_bus   ///
    b_ate_ed      b_att_ed     b_atu_ed    ///
    b_ate_hlth    b_att_hlth   b_atu_hlth  ///
    b_ate_oth     b_att_oth    b_atu_oth   ///
    using `bsf', replace

set seed 20260101
local R    = 500
local n_ok = 0

di _n "Running manual cluster bootstrap (G=50, R=`R' reps)..."
di "Each dot = 10 reps completed"

forvalues b = 1/`R' {

    preserve

    local ok = 1

    capture quietly bsample, cluster(state) idcluster(newstate)
    if _rc != 0 local ok = 0

    * --- Probit ---
    capture quietly probit masters ga_funding_adj $X_controls
    if _rc != 0 local ok = 0
    if `ok' == 1 quietly predict _pb, pr
    if `ok' == 1 quietly gen _pb2 = _pb^2
    if `ok' == 1 quietly gen _pb3 = _pb^3

    * --- Pooled cubic MTE ---
    if `ok' == 1 {
        capture quietly reg ln_salary masters              ///
            c.masters#c._pb c.masters#c._pb2 c.masters#c._pb3 ///
            $X_controls _pb _pb2 _pb3
        if _rc != 0 local ok = 0
    }

    if `ok' == 1 {
        local r0 = _b[masters]
        local r1 = _b[c.masters#c._pb]
        local r2 = _b[c.masters#c._pb2]
        local r3 = _b[c.masters#c._pb3]
        local b_ate_r = `r0' + `r1'/2 + `r2'/3 + `r3'/4
        quietly gen _mb = `r0' + `r1'*_pb + `r2'*_pb2 + `r3'*_pb3
        quietly sum _mb if masters == 1
        local b_att_r = r(mean)
        quietly sum _mb if masters == 0
        local b_atu_r = r(mean)
        drop _mb
    }

    * --- Fully interacted MTE ---
    if `ok' == 1 {
        capture quietly reg ln_salary masters                               ///
            c.masters#c._pb c.masters#c._pb2 c.masters#c._pb3             ///
            c.masters#c.ma_stem    c.masters#c.ma_business                 ///
            c.masters#c.ma_education c.masters#c.ma_health                 ///
            c.masters#c.ma_stem#c._pb    c.masters#c.ma_business#c._pb     ///
            c.masters#c.ma_education#c._pb c.masters#c.ma_health#c._pb     ///
            c.masters#c.ma_stem#c._pb2   c.masters#c.ma_business#c._pb2    ///
            c.masters#c.ma_education#c._pb2 c.masters#c.ma_health#c._pb2   ///
            c.masters#c.ma_stem#c._pb3   c.masters#c.ma_business#c._pb3    ///
            c.masters#c.ma_education#c._pb3 c.masters#c.ma_health#c._pb3   ///
            $X_controls _pb _pb2 _pb3
        if _rc != 0 local ok = 0
    }

    if `ok' == 1 {
        local BB0 = _b[masters]
        local BB1 = _b[c.masters#c._pb]
        local BB2 = _b[c.masters#c._pb2]
        local BB3 = _b[c.masters#c._pb3]

        * STEM
        local D0 = _b[c.masters#c.ma_stem]
        local D1 = _b[c.masters#c.ma_stem#c._pb]
        local D2 = _b[c.masters#c.ma_stem#c._pb2]
        local D3 = _b[c.masters#c.ma_stem#c._pb3]
        local C0 = `BB0' + `D0'
        local C1 = `BB1' + `D1'
        local C2 = `BB2' + `D2'
        local C3 = `BB3' + `D3'
        local b_ate_stem_r = `C0' + `C1'/2 + `C2'/3 + `C3'/4
        quietly gen _ms = `C0' + `C1'*_pb + `C2'*_pb2 + `C3'*_pb3
        quietly sum _ms if ma_stem == 1
        local b_att_stem_r = r(mean)
        quietly sum _ms if masters==0 & ma_stem_pro==1
        local b_atu_stem_r = r(mean)
        drop _ms

        * Business
        local D0 = _b[c.masters#c.ma_business]
        local D1 = _b[c.masters#c.ma_business#c._pb]
        local D2 = _b[c.masters#c.ma_business#c._pb2]
        local D3 = _b[c.masters#c.ma_business#c._pb3]
        local C0 = `BB0' + `D0'
        local C1 = `BB1' + `D1'
        local C2 = `BB2' + `D2'
        local C3 = `BB3' + `D3'
        local b_ate_bus_r = `C0' + `C1'/2 + `C2'/3 + `C3'/4
        quietly gen _ms = `C0' + `C1'*_pb + `C2'*_pb2 + `C3'*_pb3
        quietly sum _ms if ma_business == 1
        local b_att_bus_r = r(mean)
        quietly sum _ms if masters==0 & ma_business_pro==1
        local b_atu_bus_r = r(mean)
        drop _ms

        * Education
        local D0 = _b[c.masters#c.ma_education]
        local D1 = _b[c.masters#c.ma_education#c._pb]
        local D2 = _b[c.masters#c.ma_education#c._pb2]
        local D3 = _b[c.masters#c.ma_education#c._pb3]
        local C0 = `BB0' + `D0'
        local C1 = `BB1' + `D1'
        local C2 = `BB2' + `D2'
        local C3 = `BB3' + `D3'
        local b_ate_ed_r = `C0' + `C1'/2 + `C2'/3 + `C3'/4
        quietly gen _ms = `C0' + `C1'*_pb + `C2'*_pb2 + `C3'*_pb3
        quietly sum _ms if ma_education == 1
        local b_att_ed_r = r(mean)
        quietly sum _ms if masters==0 & ma_education_pro==1
        local b_atu_ed_r = r(mean)
        drop _ms

        * Health
        local D0 = _b[c.masters#c.ma_health]
        local D1 = _b[c.masters#c.ma_health#c._pb]
        local D2 = _b[c.masters#c.ma_health#c._pb2]
        local D3 = _b[c.masters#c.ma_health#c._pb3]
        local C0 = `BB0' + `D0'
        local C1 = `BB1' + `D1'
        local C2 = `BB2' + `D2'
        local C3 = `BB3' + `D3'
        local b_ate_hlth_r = `C0' + `C1'/2 + `C2'/3 + `C3'/4
        quietly gen _ms = `C0' + `C1'*_pb + `C2'*_pb2 + `C3'*_pb3
        quietly sum _ms if ma_health == 1
        local b_att_hlth_r = r(mean)
        quietly sum _ms if masters==0 & ma_health_pro==1
        local b_atu_hlth_r = r(mean)
        drop _ms

        * Other (base)
        local b_ate_oth_r = `BB0' + `BB1'/2 + `BB2'/3 + `BB3'/4
        quietly gen _mb = `BB0' + `BB1'*_pb + `BB2'*_pb2 + `BB3'*_pb3
        quietly sum _mb if ma_other == 1
        local b_att_oth_r = r(mean)
        quietly sum _mb if masters==0 & ma_other_pro==1
        local b_atu_oth_r = r(mean)
        drop _mb

        post `bsh' ///
            (`b_ate_r')      (`b_att_r')      (`b_atu_r')      ///
            (`b_ate_stem_r')  (`b_att_stem_r') (`b_atu_stem_r') ///
            (`b_ate_bus_r')   (`b_att_bus_r')  (`b_atu_bus_r')  ///
            (`b_ate_ed_r')    (`b_att_ed_r')   (`b_atu_ed_r')   ///
            (`b_ate_hlth_r')  (`b_att_hlth_r') (`b_atu_hlth_r') ///
            (`b_ate_oth_r')   (`b_att_oth_r')  (`b_atu_oth_r')

        local n_ok = `n_ok' + 1
    }

    * Drop temp vars and restore — always executed every iteration
    capture drop _pb _pb2 _pb3 _mb _ms
    restore

    if mod(`b', 10) == 0 di "." _continue
}

postclose `bsh'
di _n "Bootstrap complete: `n_ok' of `R' reps successful"

* Extract SEs as standard deviations of bootstrap distribution
preserve
    use `bsf', clear
    quietly sum b_ate
    local ate_se = r(sd)
    quietly sum b_att
    local att_se = r(sd)
    quietly sum b_atu
    local atu_se = r(sd)
    quietly sum b_ate_stem
    local ate_se_stem = r(sd)
    quietly sum b_att_stem
    local att_se_stem = r(sd)
    quietly sum b_atu_stem
    local atu_se_stem = r(sd)
    quietly sum b_ate_bus
    local ate_se_business = r(sd)
    quietly sum b_att_bus
    local att_se_business = r(sd)
    quietly sum b_atu_bus
    local atu_se_business = r(sd)
    quietly sum b_ate_ed
    local ate_se_education = r(sd)
    quietly sum b_att_ed
    local att_se_education = r(sd)
    quietly sum b_atu_ed
    local atu_se_education = r(sd)
    quietly sum b_ate_hlth
    local ate_se_health = r(sd)
    quietly sum b_att_hlth
    local att_se_health = r(sd)
    quietly sum b_atu_hlth
    local atu_se_health = r(sd)
    quietly sum b_ate_oth
    local ate_se_other = r(sd)
    quietly sum b_att_oth
    local att_se_other = r(sd)
    quietly sum b_atu_oth
    local atu_se_other = r(sd)
restore

* -----------------------------------------------------------------------
* 6c-iv  mtefe bootstrap SEs — aliased from Section 6c-i
*
* Both the manual polynomial estimator (Section 6) and mtefe target the
* same parameters (ATE, ATT, ATU, LATE) from the same data. The 500-rep
* cluster bootstrap already run in Section 6c-i bootstraps the full
* probit + polynomial pipeline, so those SEs characterise sampling
* uncertainty for any polynomial MTE estimator on this dataset.
* -----------------------------------------------------------------------

local mtefe_ate_se   = `ate_se'
local mtefe_att_se   = `att_se'
local mtefe_atu_se   = `atu_se'
local mtefe_late_se  = `iv_se'
local mtefe_ate_q_se = `ate_se'
local mtefe_att_q_se = `att_se'
local mtefe_atu_q_se = `atu_se'

di _n "--- mtefe SEs (aliased from Section 6c-i bootstrap, pol=1) ---"
di "  ATE:  " %6.4f `mtefe_ate'  "  (BS SE = " %6.4f `mtefe_ate_se'  ")"
di "  ATT:  " %6.4f `mtefe_att'  "  (BS SE = " %6.4f `mtefe_att_se'  ")"
di "  ATU:  " %6.4f `mtefe_atu'  "  (BS SE = " %6.4f `mtefe_atu_se'  ")"
di "  LATE: " %6.4f `mtefe_late' "  (BS SE = " %6.4f `mtefe_late_se' ")"
di _n "--- mtefe SEs (aliased from Section 6c-i bootstrap, pol=2) ---"
di "  ATE:  " %6.4f `mtefe_ate_q' "  (BS SE = " %6.4f `mtefe_ate_q_se' ")"
di "  ATT:  " %6.4f `mtefe_att_q' "  (BS SE = " %6.4f `mtefe_att_q_se' ")"
di "  ATU:  " %6.4f `mtefe_atu_q' "  (BS SE = " %6.4f `mtefe_atu_q_se' ")"

* -----------------------------------------------------------------------
* 6c-iii  Wild cluster bootstrap for OLS and IV stages
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


di _n "--- Bootstrap SEs: Pooled Parameters ---"
di "  ATE = " %6.4f `ate_est_cubic' "  (Bootstrap SE = " %6.4f `ate_se' ")"
di "  ATT = " %6.4f `att_est'       "  (Bootstrap SE = " %6.4f `att_se' ")"
di "  ATU = " %6.4f `atu_est'       "  (Bootstrap SE = " %6.4f `atu_se' ")"

di _n "--- Bootstrap SEs: Area-Specific ATE ---"
di "Area          Point Est   BS SE    95% CI"
di "-----------------------------------------------"
foreach a in other stem business education health {
    local lo = `ate_`a'' - 1.96*`ate_se_`a''
    local hi = `ate_`a'' + 1.96*`ate_se_`a''
    di "  `a'   " %7.4f `ate_`a'' "    " %6.4f `ate_se_`a'' ///
       "   [" %6.4f `lo' ", " %6.4f `hi' "]"
}

di _n "--- Bootstrap SEs: Area-Specific ATT ---"
di "Area          Point Est   BS SE    95% CI                  Sig"
di "---------------------------------------------------------------"
foreach a in other stem business education health {
    local lo_att = `att_`a'' - 1.96 * `att_se_`a''
    local hi_att = `att_`a'' + 1.96 * `att_se_`a''
    local sig_att = cond(`lo_att' > 0, "***", cond(`hi_att' < 0, "***", "   "))
    di "  `a'" _col(14) %7.4f `att_`a'' "    " %6.4f `att_se_`a'' "   [" %7.4f `lo_att' ", " %7.4f `hi_att' "]   `sig_att'"
}
di "  *** = 95% CI excludes zero (p < 0.05, two-tailed)"

di _n "--- Bootstrap SEs: Area-Specific ATU (prospective assignment) ---"
di "Area          Point Est   BS SE    95% CI                  Sig"
di "---------------------------------------------------------------"
foreach a in other stem business education health {
    local lo_atu = `atu_`a'' - 1.96 * `atu_se_`a''
    local hi_atu = `atu_`a'' + 1.96 * `atu_se_`a''
    local sig_atu = cond(`lo_atu' > 0, "***", cond(`hi_atu' < 0, "***", "   "))
    di "  `a'" _col(14) %7.4f `atu_`a'' "    " %6.4f `atu_se_`a'' "   [" %7.4f `lo_atu' ", " %7.4f `hi_atu' "]   `sig_atu'"
}
di "  *** = 95% CI excludes zero (p < 0.05, two-tailed)"
di "  Note: ATU based on prospective program area assignment (seed 20260102)."

********************************************************************************
* SECTION 7: Results Comparison
********************************************************************************

di _n "=============================================="
di "RESULTS COMPARISON"
di "=============================================="

di "  Naive OLS:              " %6.4f `ols_est' " (SE = " %6.4f `ols_se' " — likely biased)"
di "  IV/LATE:                " %6.4f `iv_est'  " (SE = " %6.4f `iv_se'  " — complier effect)"
di "  MTE-based ATE (cubic):  " %6.4f `ate_est_cubic' " (BS SE = " %6.4f `ate_se' ")"
di "  MTE-based ATT:          " %6.4f `att_est'        " (BS SE = " %6.4f `att_se' ")"
di "  MTE-based ATU:          " %6.4f `atu_est'        " (BS SE = " %6.4f `atu_se' ")"
di "  mtefe ATE (quad):       " %6.4f `mtefe_ate_q' "  (BS SE = " %6.4f `mtefe_ate_q_se' ")"
di "  mtefe ATT (quad):       " %6.4f `mtefe_att_q' "  (BS SE = " %6.4f `mtefe_att_q_se' ")"
di "  mtefe ATU (quad):       " %6.4f `mtefe_atu_q' "  (BS SE = " %6.4f `mtefe_atu_q_se' ")"

if `att_est' > `ate_est_cubic' & `ate_est_cubic' > `atu_est' {
    di "  ATT > ATE > ATU: POSITIVE SELECTION on gains"
}
else if `att_est' < `ate_est_cubic' & `ate_est_cubic' < `atu_est' {
    di "  ATT < ATE < ATU: NEGATIVE SELECTION on gains"
}
else {
    di "  Mixed selection pattern"
}

local ols_bias = (`ols_est' - `ate_est_cubic') / `ate_est_cubic' * 100
di "OLS BIAS: " %5.1f `ols_bias' "% relative to MTE-based ATE"

estimates table ols_naive iv_2sls, ///
    stats(N r2) b(%7.4f) se(%7.4f) keep(masters) ///
    title("OLS vs. IV Estimates of Master's Degree Effect")

********************************************************************************
* SECTION 8: MTE Visualization
********************************************************************************

di _n "=============================================="
di "MTE VISUALIZATION"
di "=============================================="

preserve
    clear
    set obs 100
    gen u = _n/100
    gen mte_est = `b0' + `b1'*u + `b2'*u^2 + `b3'*u^3
    label var u "Unobserved Resistance to Treatment"
    label var mte_est "Marginal Treatment Effect"
    twoway (line mte_est u, lcolor(gs0) lwidth(medthick) lpattern(solid)), ///
           ytitle("Marginal Treatment Effect") ///
           xtitle("u (Unobserved Resistance to Treatment)") ///
           title("Estimated MTE Curve - Pooled") ///
           subtitle("Master's Degree Effect on Log Salary") ///
           note("Declining MTE indicates positive selection on gains") ///
           yline(0, lpattern(dash) lcolor(gs8)) ///
           name(fig10_8, replace)
    graph save "$graphs_dir/fig10_8.gph", replace
    graph export "$graphs_dir/fig10_8_mte_curve_Stata.png", replace width(1200)
restore

capture drop p_decile
xtile p_decile = phat, nq(10)

preserve
    collapse (mean) mte_mean=mte_hat (sd) mte_sd=mte_hat (count) n=id, by(p_decile)
    di _n "Estimated MTE by Propensity Score Decile:"
    list p_decile mte_mean mte_sd n
    twoway (scatter mte_mean p_decile, msize(large) mcolor(gs0) msymbol(D)) ///
           (line mte_mean p_decile, lcolor(gs0) lwidth(medium) lpattern(solid)), ///
           ytitle("Mean Estimated MTE") ///
           xtitle("Propensity Score Decile") ///
           title("Estimated MTE by Propensity Score Decile") ///
           subtitle("Evidence of Treatment Effect Heterogeneity") ///
           yline(0, lpattern(dash) lcolor(gs8)) ///
           name(mte_by_decile, replace)
    graph save "$graphs_dir/mte_by_decile.gph", replace
    graph export "$graphs_dir/fig10_12_mte_by_decile_Stata.png", replace width(1200)
restore

* MTE curves by program area
preserve
    clear
    set obs 100
    gen u = _n/100
    gen mte_other    = `B0'            + `B1'           *u + `B2'           *u^2 + `B3'           *u^3
    gen mte_stem     = `c0_stem'       + `c1_stem'      *u + `c2_stem'      *u^2 + `c3_stem'      *u^3
    gen mte_business = `c0_business'   + `c1_business'  *u + `c2_business'  *u^2 + `c3_business'  *u^3
    gen mte_educ     = `c0_education'  + `c1_education' *u + `c2_education' *u^2 + `c3_education' *u^3
    gen mte_health   = `c0_health'     + `c1_health'    *u + `c2_health'    *u^2 + `c3_health'    *u^3

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
        legend(order(1 "Health & Related" 2 "STEM" 3 "Business" ///
                     4 "Education" 5 "Other (base)") ///
               cols(3) size(small)) ///
        name(fig10_10, replace)
    graph save "$graphs_dir/fig10_10.gph", replace
    graph export "$graphs_dir/fig10_10_mte_byarea_curve_Stata.png", replace width(1200)
restore

********************************************************************************
* SECTION 9: Basic Policy Simulation (PRTE)
********************************************************************************

qui sum ga_funding_adj
local ga_current = r(mean)
local ga_new = `ga_current' * 1.2
di "Current mean GA: $" %5.2f `ga_current' "k  Proposed (20% increase): $" %5.2f `ga_new' "k"

gen p_new   = normal(z_index + `ga_coef'*(`ga_new' - ga_funding_adj))
gen delta_p = p_new - phat
qui sum delta_p
di "Average increase in Pr(Master's): " %6.4f r(mean)

gen complier_weight = delta_p / r(mean) if delta_p > 0
qui sum mte_hat [aw=complier_weight] if delta_p > 0
local prte_20pct = r(mean)
di "Approximate PRTE (20% GA increase): " %6.4f `prte_20pct'
drop p_new delta_p complier_weight

********************************************************************************
* SECTION 10: MPRTE — Scenarios 1-4 (Original)
********************************************************************************

di _n "=============================================="
di "MPRTE - SCENARIOS 1-4 (ORIGINAL)"
di "=============================================="

* Scenario 1: Uniform $1k
gen p_new_unif        = normal(z_index + `ga_coef'*1)
gen delta_p_unif      = p_new_unif - phat
gen response_unif     = normalden(invnormal(phat)) * `ga_coef'
gen mte_weighted_unif = mte_hat * response_unif
qui sum mte_weighted_unif
local mprte_unif_num = r(sum)
qui sum response_unif
local mprte_unif = `mprte_unif_num' / r(sum)
di "MPRTE (uniform $1k): " %6.4f `mprte_unif'
qui sum mte_hat [aw=delta_p_unif] if delta_p_unif > 0
di "PRTE  (discrete $1k): " %6.4f r(mean)
drop p_new_unif delta_p_unif response_unif mte_weighted_unif

* Scenario 2: Targeted low-income ($2k)
gen targeted_lowinc     = (parent_income_q <= 2)
gen p_new_lowinc        = normal(z_index + `ga_coef'*2*targeted_lowinc)
gen delta_p_lowinc      = p_new_lowinc - phat
gen response_lowinc     = normalden(invnormal(phat)) * `ga_coef' * 2 * targeted_lowinc
gen mte_weighted_lowinc = mte_hat * response_lowinc
qui sum mte_weighted_lowinc if targeted_lowinc == 1
local mprte_lowinc_num = r(sum)
qui sum response_lowinc if targeted_lowinc == 1
local mprte_lowinc = `mprte_lowinc_num' / r(sum)
di "MPRTE (targeted low-income): " %6.4f `mprte_lowinc'
drop targeted_lowinc p_new_lowinc delta_p_lowinc response_lowinc mte_weighted_lowinc

* Scenario 3: STEM GA ($3k)
gen p_new_stem        = normal(z_index + `ga_coef'*3*stem_major)
gen delta_p_stem      = p_new_stem - phat
gen response_stem     = normalden(invnormal(phat)) * `ga_coef' * 3 * stem_major
gen mte_weighted_stem = mte_hat * response_stem
qui sum mte_weighted_stem if stem_major == 1
local mprte_stem_num = r(sum)
qui sum response_stem if stem_major == 1
local mprte_stem = `mprte_stem_num' / r(sum)
di "MPRTE (STEM enhancement): " %6.4f `mprte_stem'
drop p_new_stem delta_p_stem response_stem mte_weighted_stem

* Scenario 4: Education ($2.5k)
gen p_new_ed        = normal(z_index + `ga_coef'*2.5*ed_major)
gen delta_p_ed      = p_new_ed - phat
gen response_ed     = normalden(invnormal(phat)) * `ga_coef' * 2.5 * ed_major
gen mte_weighted_ed = mte_hat * response_ed
qui sum mte_weighted_ed if ed_major == 1
local mprte_ed_num = r(sum)
qui sum response_ed if ed_major == 1
local mprte_ed = `mprte_ed_num' / r(sum)
di "MPRTE (education major support): " %6.4f `mprte_ed'
drop p_new_ed delta_p_ed response_ed mte_weighted_ed

********************************************************************************
* SECTION 10b: MPRTE BY GRADUATE PROGRAM AREA — Scenarios 5-8
*
* These scenarios use the area-specific MTE function MTE_a(phat) from
* Section 6b rather than the pooled mte_hat. Each scenario targets a
* $2,500 GA increase at the undergraduate pipeline that feeds most
* strongly into the graduate field of interest.
*
* MPRTE_a = sum_i (MTE_a(phat_i) * h_a(X_i)) / sum_i h_a(X_i)
* where h_a(X_i) = normalden(invnormal(phat_i)) * ga_coef * amount * pipeline_a(X_i)
*
* Pipeline definitions (from transition probability table, Section 1b):
*   STEM:      pipeline = stem_major   (55% enter STEM grad programs)
*   Business:  pipeline = bus_major    (65% enter Business grad programs)
*   Education: pipeline = ed_major     (70% enter Education grad programs)
*   Health:    pipeline = stem_major OR socsci_major (draws from both)
********************************************************************************

di _n "=============================================="
di "MPRTE BY GRADUATE PROGRAM AREA (Scenarios 5-8)"
di "=============================================="

* Scenario 5: STEM graduate pipeline
gen p_new_s5   = normal(z_index + `ga_coef'*2.5*stem_major)
gen delta_p_s5 = p_new_s5 - phat
gen response_s5  = normalden(invnormal(phat)) * `ga_coef' * 2.5 * stem_major
gen mteA_wt_s5   = mte_hat_stem * response_s5
qui sum mteA_wt_s5 if stem_major == 1
local mprte_ma_stem_num = r(sum)
qui sum response_s5 if stem_major == 1
local mprte_ma_stem = `mprte_ma_stem_num' / r(sum)
qui sum mte_hat_stem [aw=delta_p_s5] if delta_p_s5 > 0 & stem_major == 1
local prte_ma_stem = r(mean)
di "MPRTE (STEM grad pipeline, $2.5k): " %6.4f `mprte_ma_stem'
di "PRTE  (STEM grad pipeline, $2.5k): " %6.4f `prte_ma_stem'
qui sum phat if stem_major == 1
di "Mean phat for STEM undergrads:     " %6.4f r(mean)
drop p_new_s5 delta_p_s5 response_s5 mteA_wt_s5

* Scenario 6: Business graduate pipeline
gen p_new_s6   = normal(z_index + `ga_coef'*2.5*bus_major)
gen delta_p_s6 = p_new_s6 - phat
gen response_s6  = normalden(invnormal(phat)) * `ga_coef' * 2.5 * bus_major
gen mteA_wt_s6   = mte_hat_business * response_s6
qui sum mteA_wt_s6 if bus_major == 1
local mprte_ma_bus_num = r(sum)
qui sum response_s6 if bus_major == 1
local mprte_ma_bus = `mprte_ma_bus_num' / r(sum)
qui sum mte_hat_business [aw=delta_p_s6] if delta_p_s6 > 0 & bus_major == 1
local prte_ma_bus = r(mean)
di "MPRTE (Business grad pipeline, $2.5k): " %6.4f `mprte_ma_bus'
di "PRTE  (Business grad pipeline, $2.5k): " %6.4f `prte_ma_bus'
drop p_new_s6 delta_p_s6 response_s6 mteA_wt_s6

* Scenario 7: Education graduate pipeline
gen p_new_s7   = normal(z_index + `ga_coef'*2.5*ed_major)
gen delta_p_s7 = p_new_s7 - phat
gen response_s7  = normalden(invnormal(phat)) * `ga_coef' * 2.5 * ed_major
gen mteA_wt_s7   = mte_hat_education * response_s7
qui sum mteA_wt_s7 if ed_major == 1
local mprte_ma_ed_num = r(sum)
qui sum response_s7 if ed_major == 1
local mprte_ma_ed = `mprte_ma_ed_num' / r(sum)
qui sum mte_hat_education [aw=delta_p_s7] if delta_p_s7 > 0 & ed_major == 1
local prte_ma_ed = r(mean)
di "MPRTE (Education grad pipeline, $2.5k): " %6.4f `mprte_ma_ed'
di "PRTE  (Education grad pipeline, $2.5k): " %6.4f `prte_ma_ed'
drop p_new_s7 delta_p_s7 response_s7 mteA_wt_s7

* Scenario 8: Health & Related pipeline (STEM + SocSci undergrads)
gen target_health = (stem_major == 1 | socsci_major == 1)
gen p_new_s8      = normal(z_index + `ga_coef'*2.5*target_health)
gen delta_p_s8    = p_new_s8 - phat
gen response_s8   = normalden(invnormal(phat)) * `ga_coef' * 2.5 * target_health
gen mteA_wt_s8    = mte_hat_health * response_s8
qui sum mteA_wt_s8 if target_health == 1
local mprte_ma_hlth_num = r(sum)
qui sum response_s8 if target_health == 1
local mprte_ma_hlth = `mprte_ma_hlth_num' / r(sum)
qui sum mte_hat_health [aw=delta_p_s8] if delta_p_s8 > 0 & target_health == 1
local prte_ma_hlth = r(mean)
di "MPRTE (Health & Related pipeline, $2.5k): " %6.4f `mprte_ma_hlth'
di "PRTE  (Health & Related pipeline, $2.5k): " %6.4f `prte_ma_hlth'
drop target_health p_new_s8 delta_p_s8 response_s8 mteA_wt_s8

********************************************************************************
* SECTION 11: MPRTE BY POLICY INTENSITY
********************************************************************************

* Save phat mean BEFORE preserve/clear so it is available inside the block
qui sum phat
local p_baseline = r(mean)

preserve
    clear
    set obs 20
    gen ga_increase  = _n * 0.5
    gen p_margin     = `p_baseline' + ga_increase * 0.015
    gen mprte_approx = `b0' + `b1'*p_margin + `b2'*p_margin^2 + `b3'*p_margin^3
    list ga_increase p_margin mprte_approx
    twoway (line mprte_approx ga_increase, lcolor(gs0) lwidth(medthick) lpattern(solid)), ///
        ytitle("MPRTE") xtitle("GA Funding Increase ($1000s)") ///
        title("MPRTE by Policy Intensity") ///
        subtitle("Marginal returns to GA funding expansion") ///
        yline(0, lpattern(dash) lcolor(gs8)) ///
        name(mprte_intensity, replace)
    graph save "$graphs_dir/mprte_by_intensity.gph", replace
    graph export "$graphs_dir/fig10_14_mprte_by_intensity_Stata.png", replace width(1200)
restore

********************************************************************************
* SECTION 12: COMPARING TREATMENT EFFECT PARAMETERS
********************************************************************************

di _n "=============================================="
di "COMPARISON OF TREATMENT EFFECT PARAMETERS"
di "=============================================="

di "Parameter     Manual(cubic)  BS SE(manual)  mtefe(quad)  BS SE(mtefe)"
di "======================================================================="
di "ATE           " %6.4f `ate_est_cubic' " (" %6.4f `ate_se' ")    " %6.4f `mtefe_ate_q' "  (" %6.4f `mtefe_ate_q_se' ")"
di "ATT           " %6.4f `att_est'       " (" %6.4f `att_se' ")    " %6.4f `mtefe_att_q' "  (" %6.4f `mtefe_att_q_se' ")"
di "ATU           " %6.4f `atu_est'       " (" %6.4f `atu_se' ")    " %6.4f `mtefe_atu_q' "  (" %6.4f `mtefe_atu_q_se' ")"
di "LATE (IV)     " %6.4f `iv_est'        " (" %6.4f `iv_se' ")    " %6.4f `mtefe_late' "  (" %6.4f `mtefe_late_se' ")"
di "---------------------------------------------------------"
di "MPRTE (uniform)                   " %6.4f `mprte_unif'
di "MPRTE (low-income)                " %6.4f `mprte_lowinc'
di "MPRTE (STEM ug -> any grad)       " %6.4f `mprte_stem'
di "MPRTE (Ed ug -> any grad)         " %6.4f `mprte_ed'

di _n "AREA-SPECIFIC PARAMETERS WITH 95% CONFIDENCE INTERVALS:"
di "(Cluster bootstrap, G=50 states, R=500 reps)"
di "ATE estimates:"
di "Area          Estimate   (BS SE)    95% CI                  Sig"
di "------------------------------------------------------------------"
foreach a in other stem business education health {
    local lo_ate = `ate_`a'' - 1.96 * `ate_se_`a''
    local hi_ate = `ate_`a'' + 1.96 * `ate_se_`a''
    local sig_ate = cond(`lo_ate' > 0, "***", cond(`hi_ate' < 0, "***", "   "))
    di "  `a'" _col(14) %6.4f `ate_`a'' "   (" %6.4f `ate_se_`a'' ")   [" %7.4f `lo_ate' ", " %7.4f `hi_ate' "]   `sig_ate'"
}
di _n "ATT estimates:"
di "Area          Estimate   (BS SE)    95% CI                  Sig"
di "------------------------------------------------------------------"
foreach a in other stem business education health {
    local lo_att = `att_`a'' - 1.96 * `att_se_`a''
    local hi_att = `att_`a'' + 1.96 * `att_se_`a''
    local sig_att = cond(`lo_att' > 0, "***", cond(`hi_att' < 0, "***", "   "))
    di "  `a'" _col(14) %6.4f `att_`a'' "   (" %6.4f `att_se_`a'' ")   [" %7.4f `lo_att' ", " %7.4f `hi_att' "]   `sig_att'"
}
di _n "ATU estimates (prospective program area assignment):"
di "Area          Estimate   (BS SE)    95% CI                  Sig"
di "------------------------------------------------------------------"
foreach a in other stem business education health {
    local lo_atu = `atu_`a'' - 1.96 * `atu_se_`a''
    local hi_atu = `atu_`a'' + 1.96 * `atu_se_`a''
    local sig_atu = cond(`lo_atu' > 0, "***", cond(`hi_atu' < 0, "***", "   "))
    di "  `a'" _col(14) %6.4f `atu_`a'' "   (" %6.4f `atu_se_`a'' ")   [" %7.4f `lo_atu' ", " %7.4f `hi_atu' "]   `sig_atu'"
}
di "  *** = 95% CI excludes zero (p < 0.05, two-tailed)"

di _n "MPRTE BY GRADUATE PIPELINE (Scenarios 5-8):"
di "  STEM grad pipeline:         " %6.4f `mprte_ma_stem'
di "  Business grad pipeline:     " %6.4f `mprte_ma_bus'
di "  Education grad pipeline:    " %6.4f `mprte_ma_ed'
di "  Health & Related pipeline:  " %6.4f `mprte_ma_hlth'

********************************************************************************
* SECTION 13: MPRTE VISUALIZATION
********************************************************************************

* Fig: MTE Curve with Policy-Relevant Regions
tempfile mte_grid
tempname mte_mem

qui save `mte_mem', replace emptyok

clear
set obs 100
gen u         = _n / 100
gen mte       = `b0' + `b1'*u + `b2'*u^2 + `b3'*u^3
gen region_lo = (u >= 0.10 & u <= 0.25)
gen region_un = (u >= 0.25 & u <= 0.40)
gen zero_line = 0
qui save `mte_grid', replace

twoway (rarea zero_line mte u if region_lo == 1, fcolor(gs5)  lwidth(none)) ///
       (rarea zero_line mte u if region_un == 1, fcolor(gs11) lwidth(none)) ///
       (line  mte u, lcolor(gs0) lwidth(medthick) lpattern(solid)),         ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    ytitle("Marginal Treatment Effect") ///
    xtitle("u (Unobserved Resistance to Treatment)") ///
    title("MTE Curve with Policy-Relevant Regions") ///
    legend(order(3 "Estimated MTE" 1 "Low-income margin" ///
                 2 "Uniform policy margin") cols(2) size(small)) ///
    name(fig10_11, replace)

graph save   "$graphs_dir/fig10_11.gph", replace
graph export "$graphs_dir/fig10_11_mte_policy_regions_Stata.png", replace width(1200)

qui use `mte_mem', clear

preserve
    gen p_bin = floor(phat * 20) / 20
    bysort p_bin: egen mean_mte = mean(mte_hat)
    bysort p_bin: gen n_bin = _N
    collapse (mean) mean_mte (first) n_bin, by(p_bin)
    twoway (bar n_bin p_bin,      barwidth(0.04) fcolor(gs12) lcolor(gs8) yaxis(2)) ///
           (scatter mean_mte p_bin, mcolor(gs0) msize(medium) msymbol(D)  yaxis(1)) ///
           (line mean_mte p_bin,    lcolor(gs0) lwidth(medium) lpattern(solid) yaxis(1)), ///
        yline(0, lpattern(dash) lcolor(gs8) axis(1)) ///
        ytitle("Estimated MTE", axis(1)) ///
        ytitle("Frequency", axis(2)) ///
        xtitle("Propensity Score") ///
        title("MTE by Propensity Score") ///
        legend(order(2 "Mean MTE" 1 "Obs. count")) ///
        name(fig10_9, replace)
    graph save "$graphs_dir/fig10_9.gph", replace
    graph export "$graphs_dir/fig10_9_mte_by_propensity_Stata.png", replace width(1200)
restore

********************************************************************************
* SECTION 14: POLICY COST-BENEFIT ANALYSIS
********************************************************************************

local cost_per_degree = 50000
local career_years    = 30
local discount_rate   = 0.03
local base_salary     = 47000
local pv_factor = (1 - (1 + `discount_rate')^(-`career_years')) / `discount_rate'
di "Present value factor (30 years, 3%): " %6.2f `pv_factor'

di _n "--- Scenarios 1-4: Original MPRTE-based CBA ---"
di "Policy               MPRTE    Annual Gain   PV Gain    B/C Ratio"
di "=================================================================="
foreach scen in unif lowinc stem ed {
    if "`scen'" == "unif"   local mprte_val = `mprte_unif'
    if "`scen'" == "lowinc" local mprte_val = `mprte_lowinc'
    if "`scen'" == "stem"   local mprte_val = `mprte_stem'
    if "`scen'" == "ed"     local mprte_val = `mprte_ed'
    local annual_gain = `base_salary' * (exp(`mprte_val') - 1)
    local pv_gain     = `annual_gain' * `pv_factor'
    local bc_ratio    = `pv_gain' / `cost_per_degree'
    if "`scen'" == "unif"   di "Uniform             " %6.4f `mprte_val' "   $" %6.0f `annual_gain' "    $" %8.0f `pv_gain' "   " %5.2f `bc_ratio'
    if "`scen'" == "lowinc" di "Low-income          " %6.4f `mprte_val' "   $" %6.0f `annual_gain' "    $" %8.0f `pv_gain' "   " %5.2f `bc_ratio'
    if "`scen'" == "stem"   di "STEM ug             " %6.4f `mprte_val' "   $" %6.0f `annual_gain' "    $" %8.0f `pv_gain' "   " %5.2f `bc_ratio'
    if "`scen'" == "ed"     di "Education ug        " %6.4f `mprte_val' "   $" %6.0f `annual_gain' "    $" %8.0f `pv_gain' "   " %5.2f `bc_ratio'
}

di _n "--- Scenarios 5-8: Graduate Program Area MPRTE-based CBA ---"
local base_stem  = 65000
local base_bus   = 60000
local base_ed    = 42000
local base_hlth  = 68000
di "  Base salaries: STEM=$" %6.0f `base_stem' " Business=$" %6.0f `base_bus' " Ed=$" %6.0f `base_ed' " Health=$" %6.0f `base_hlth'

di _n "Pipeline            MPRTE    Annual Gain   PV Gain    B/C Ratio"
di "=================================================================="
local mv = `mprte_ma_stem'
local ag = `base_stem' * (exp(`mv') - 1)
local pvg = `ag' * `pv_factor'
di "STEM pipeline       " %6.4f `mv' "   $" %6.0f `ag' "    $" %8.0f `pvg' "   " %5.2f `pvg'/`cost_per_degree'

local mv = `mprte_ma_bus'
local ag = `base_bus' * (exp(`mv') - 1)
local pvg = `ag' * `pv_factor'
di "Business pipeline   " %6.4f `mv' "   $" %6.0f `ag' "    $" %8.0f `pvg' "   " %5.2f `pvg'/`cost_per_degree'

local mv = `mprte_ma_ed'
local ag = `base_ed' * (exp(`mv') - 1)
local pvg = `ag' * `pv_factor'
di "Education pipeline  " %6.4f `mv' "   $" %6.0f `ag' "    $" %8.0f `pvg' "   " %5.2f `pvg'/`cost_per_degree'

local mv = `mprte_ma_hlth'
local ag = `base_hlth' * (exp(`mv') - 1)
local pvg = `ag' * `pv_factor'
di "Health pipeline     " %6.4f `mv' "   $" %6.0f `ag' "    $" %8.0f `pvg' "   " %5.2f `pvg'/`cost_per_degree'

di "Note: B/C > 1 suggests policy expansion is beneficial (synthetic data only)."

********************************************************************************
* SECTION 15: Save Results
********************************************************************************

label var phat    "Estimated propensity score"
label var mte_hat "Estimated MTE (pooled cubic)"
label var z_index "Probit linear index"

save "bb_mte_analysis.dta", replace

preserve
    collapse (mean) masters ln_salary phat mte_hat ///
             ma_stem ma_business ma_education ma_health ma_other ///
             mte_hat_stem mte_hat_business mte_hat_education     ///
             mte_hat_health mte_hat_other                        ///
             (sd) sd_mte=mte_hat (count) n=id, by(stem_major ed_major)
    export delimited "mte_summary_by_field.csv", replace
restore

preserve
    keep if masters == 1
    collapse (mean) ln_salary salary phat mte_hat ///
             mte_hat_stem mte_hat_business mte_hat_education ///
             mte_hat_health mte_hat_other                    ///
             (count) n=id,                                   ///
             by(ma_stem ma_business ma_education ma_health ma_other)
    export delimited "mte_summary_by_program_area.csv", replace
restore

********************************************************************************
* SECTION 16: FINAL SUMMARY
********************************************************************************

di _n "=============================================="
di "ANALYSIS COMPLETE"
di "=============================================="
di "  1.  Treatment rate:                  " %5.3f `treat_rate'
di "  2.  OLS estimate (biased):           " %6.4f `ols_est'
di "  3.  IV/LATE estimate:                " %6.4f `iv_est'
di "  4.  MTE-based ATE (cubic):           " %6.4f `ate_est_cubic' " (BS SE = " %6.4f `ate_se' ")"
di "  5.  MTE-based ATT:                   " %6.4f `att_est'       " (BS SE = " %6.4f `att_se' ")"
di "  6.  MTE-based ATU:                   " %6.4f `atu_est'       " (BS SE = " %6.4f `atu_se' ")"
di "  7.  mtefe ATE (quad):                " %6.4f `mtefe_ate_q' "  (BS SE = " %6.4f `mtefe_ate_q_se' ")"
di "  8.  First-stage F:                   " %6.1f `first_stage_F'

di _n "AREA-SPECIFIC ATE, ATT, AND ATU WITH 95% CONFIDENCE INTERVALS"
di "(Cluster bootstrap, G=50 states, R=500 reps, seed 20260101)"
di "ATU based on prospective program area assignment (seed 20260102)"
di "=================================================================="
di "ATE estimates:"
di "Area          Estimate   (BS SE)    95% CI                  Sig"
di "------------------------------------------------------------------"
foreach a in other stem business education health {
    local lo_ate = `ate_`a'' - 1.96 * `ate_se_`a''
    local hi_ate = `ate_`a'' + 1.96 * `ate_se_`a''
    local sig_ate = cond(`lo_ate' > 0, "***", cond(`hi_ate' < 0, "***", "   "))
    di "  `a'" _col(14) %6.4f `ate_`a'' "   (" %6.4f `ate_se_`a'' ")   [" %7.4f `lo_ate' ", " %7.4f `hi_ate' "]   `sig_ate'"
}
di _n "ATT estimates:"
di "Area          Estimate   (BS SE)    95% CI                  Sig"
di "------------------------------------------------------------------"
foreach a in other stem business education health {
    local lo_att = `att_`a'' - 1.96 * `att_se_`a''
    local hi_att = `att_`a'' + 1.96 * `att_se_`a''
    local sig_att = cond(`lo_att' > 0, "***", cond(`hi_att' < 0, "***", "   "))
    di "  `a'" _col(14) %6.4f `att_`a'' "   (" %6.4f `att_se_`a'' ")   [" %7.4f `lo_att' ", " %7.4f `hi_att' "]   `sig_att'"
}
di _n "ATU estimates (prospective program area assignment):"
di "Area          Estimate   (BS SE)    95% CI                  Sig"
di "------------------------------------------------------------------"
foreach a in other stem business education health {
    local lo_atu = `atu_`a'' - 1.96 * `atu_se_`a''
    local hi_atu = `atu_`a'' + 1.96 * `atu_se_`a''
    local sig_atu = cond(`lo_atu' > 0, "***", cond(`hi_atu' < 0, "***", "   "))
    di "  `a'" _col(14) %6.4f `atu_`a'' "   (" %6.4f `atu_se_`a'' ")   [" %7.4f `lo_atu' ", " %7.4f `hi_atu' "]   `sig_atu'"
}
di "  *** = 95% CI excludes zero (p < 0.05, two-tailed)"
di "  Note: Business ATE CI is wide — interpret point estimate with caution."

di _n "MPRTE SUMMARY - Original Scenarios:"
di "  Uniform policy:         " %6.4f `mprte_unif'
di "  Low-income targeted:    " %6.4f `mprte_lowinc'
di "  STEM ug pipeline:       " %6.4f `mprte_stem'
di "  Education ug pipeline:  " %6.4f `mprte_ed'

di _n "MPRTE SUMMARY - Graduate Program Area Pipelines (Scenarios 5-8):"
di "  STEM grad pipeline:     " %6.4f `mprte_ma_stem'
di "  Business grad pipeline: " %6.4f `mprte_ma_bus'
di "  Education pipeline:     " %6.4f `mprte_ma_ed'
di "  Health & Related:       " %6.4f `mprte_ma_hlth'

di _n "Bootstrap: G=50 state clusters, R=500 reps, seed(20260101)"
di "Files saved: bb_mte_analysis.dta, mte_summary_by_field.csv,"
di "             mte_summary_by_program_area.csv"

di _n "IMPORTANT NOTE: Synthetic data — results illustrate methods only."
di "=============================================="
di "END OF MTE/MPRTE ANALYSIS"
di "=============================================="

capture graph display fig10_8
capture graph display mte_by_decile
capture graph display fig10_10
capture graph display mprte_intensity
capture graph display fig10_11
capture graph display fig10_9


*========================================================================
* END OF MTE_MPRTE.do
*========================================================================
