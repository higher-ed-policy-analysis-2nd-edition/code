********************************************************************************
* MTE and MPRTE Analysis: Effect of Master's Degree on Salary Outcomes
* Chapter 10 - Marginal Treatment Effects
*
* Instrument: State-Funded Graduate Assistantship (GA) Dollar Amount
* 
* Based on synthetic data mirroring NCES B&B Longitudinal Study characteristics
* and higher education finance literature (Titus 2007; Bound, Lovenheim & 
* Turner 2010; Zhang 2005; Ehrenberg et al. 2007)
*
* Author: Marvin A. Titus
* Date: December 2025
* Purpose: Demonstrate MTE/MPRTE framework for textbook Chapter 10
*
* Data Source: Synthetic dataset mirroring NCES B&B
* https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_5_3.dta
*
* Note: Because a synthetic dataset is used in this application, the results
* are intended to illustrate MTE/MPRTE estimation methods and should not be
* viewed as having policy implications.
********************************************************************************

clear all
set more off
set seed 20251130

********************************************************************************
* SECTION 1: Load Synthetic Dataset
********************************************************************************

di _n "=============================================="
di "LOADING SYNTHETIC B&B DATASET"
di "=============================================="

* Download synthetic dataset mirroring NCES B&B Longitudinal Study
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_5_3.dta" ///
     "Example_7_5_3.dta", replace

use "Example_7_5_3.dta", clear

* Generate observation ID if not present
capture confirm variable id
if _rc != 0 {
    gen id = _n
}

* Display dataset information
describe
di _n "Sample size: " _N

********************************************************************************
* SECTION 2: Summary Statistics
********************************************************************************

di _n "=============================================="
di "SUMMARY STATISTICS"
di "=============================================="

* Treatment variable
di _n "--- Treatment: Master's Degree Completion ---"
tab masters
sum masters

* Store treatment rate for later use
qui sum masters
local treat_rate = r(mean)
di "Treatment rate: " %5.3f `treat_rate'

* Key variables
di _n "--- Key Variables ---"
sum ln_salary salary masters ga_funding_adj

* Outcome by treatment status
di _n "--- Salary by Master's Degree Status ---"
tabstat salary ln_salary, by(masters) stats(mean sd min max n)

* Instrument summary
di _n "--- Instrument: GA Funding ---"
sum ga_funding_adj, detail

* Control variables
di _n "--- Control Variables ---"
sum female black hispanic asian age_ba firstgen parent_income_q parent_grad ///
    ugpa stem_major bus_major ed_major selective_inst public_ug state_unemp metro

********************************************************************************
* SECTION 3: First-Stage and Instrument Relevance
********************************************************************************

di _n "=============================================="
di "INSTRUMENT RELEVANCE CHECK"
di "=============================================="

* Define control variables
global X_controls "female black hispanic asian age_ba firstgen parent_income_q parent_grad ugpa stem_major bus_major ed_major selective_inst public_ug state_unemp metro"

* First-stage regression
di _n "--- First-Stage Regression ---"
reg masters ga_funding_adj $X_controls, robust
est store first_stage

* Test instrument strength
test ga_funding_adj
local first_stage_F = r(F)
di _n "First-stage F-statistic: " %6.2f `first_stage_F' " (should be >> 10)"

if `first_stage_F' > 10 {
    di "RESULT: Strong instrument (F > 10)"
}
else {
    di "WARNING: Potentially weak instrument"
}

********************************************************************************
* SECTION 4: Naive OLS Estimation
********************************************************************************

di _n "=============================================="
di "NAIVE OLS ESTIMATION"
di "=============================================="

reg ln_salary masters $X_controls, robust
est store ols_naive
local ols_est = _b[masters]
local ols_se = _se[masters]

di "OLS estimate: " %6.4f `ols_est' " (SE = " %6.4f `ols_se' ")"
di "Note: Likely biased upward due to positive selection on unobservables"

********************************************************************************
* SECTION 5: IV/2SLS Estimation (LATE)
********************************************************************************

di _n "=============================================="
di "IV/2SLS ESTIMATION (LATE)"
di "=============================================="

ivregress 2sls ln_salary $X_controls (masters = ga_funding_adj), robust first
est store iv_2sls
local iv_est = _b[masters]
local iv_se = _se[masters]

di _n "IV/LATE estimate: " %6.4f `iv_est' " (SE = " %6.4f `iv_se' ")"
di "Interpretation: Effect for compliers induced by GA funding variation"

* Diagnostic tests
di _n "--- First-Stage Diagnostics ---"
estat firststage

di _n "--- Endogeneity Test ---"
estat endogenous

********************************************************************************
* SECTION 6: MTE ESTIMATION
********************************************************************************

di _n "=============================================="
di "MTE ESTIMATION"
di "=============================================="

* Check/install required packages
capture which mtefe
if _rc != 0 {
    di "Installing mtefe package..."
    ssc install mtefe, replace
}
capture which moremata
if _rc != 0 {
    di "Installing moremata package..."
    ssc install moremata, replace
}

* Define variable lists
global X "$X_controls"
global Z "ga_funding_adj"

*------------------------------------------------------------------------------
* Approach 1: Parametric MTE via Polynomial in Propensity Score
*------------------------------------------------------------------------------

di _n "--- Approach 1: Polynomial MTE (Manual) ---"

* Step 1: Estimate propensity score via probit
probit masters $Z $X
predict phat, pr

* Store probit coefficients for later policy simulations
local ga_coef = _b[ga_funding_adj]
di "GA funding coefficient in probit: " %7.5f `ga_coef'

* Also store the linear index for policy simulations
predict z_index, xb

* Step 2: Generate polynomial terms
gen phat2 = phat^2
gen phat3 = phat^3

* Step 3: Estimate switching regression with quadratic interactions
di _n "--- Quadratic MTE Specification ---"
reg ln_salary masters c.masters#(c.phat c.phat2) $X phat phat2, robust

* Extract MTE function parameters (quadratic)
local b0_quad = _b[masters]
local b1_quad = _b[c.masters#c.phat]
local b2_quad = _b[c.masters#c.phat2]

di _n "Quadratic MTE(u) = " %6.4f `b0_quad' " + " %6.4f `b1_quad' "*u + " %6.4f `b2_quad' "*u^2"

* Calculate treatment effect parameters by integration
* ATE = integral_0^1 MTE(u) du = b0 + b1/2 + b2/3
local ate_est_quad = `b0_quad' + `b1_quad'/2 + `b2_quad'/3
di "Estimated ATE (quadratic): " %6.4f `ate_est_quad'

* Step 4: Estimate cubic specification for more flexibility
di _n "--- Cubic MTE Specification ---"
reg ln_salary masters c.masters#(c.phat c.phat2 c.phat3) $X phat phat2 phat3, robust

* Extract cubic coefficients
local b0 = _b[masters]
local b1 = _b[c.masters#c.phat]
local b2 = _b[c.masters#c.phat2]
local b3 = _b[c.masters#c.phat3]

di _n "Cubic MTE(u) = " %7.4f `b0' " + " %7.4f `b1' "*u + " %7.4f `b2' "*u² + " %7.4f `b3' "*u³"

* ATE from cubic: b0 + b1/2 + b2/3 + b3/4
local ate_est_cubic = `b0' + `b1'/2 + `b2'/3 + `b3'/4
di "Estimated ATE (cubic): " %6.4f `ate_est_cubic'

* Create MTE function variable
gen mte_hat = `b0' + `b1'*phat + `b2'*phat2 + `b3'*phat3

* ATT and ATU via numerical approximation
qui sum mte_hat if masters == 1
local att_est = r(mean)
di "Estimated ATT: " %6.4f `att_est'

qui sum mte_hat if masters == 0  
local atu_est = r(mean)
di "Estimated ATU: " %6.4f `atu_est'

*------------------------------------------------------------------------------
* Approach 2: MTE Estimation Using mtefe Package
*------------------------------------------------------------------------------

di _n "--- Approach 2: mtefe Package Estimation ---"

/*
The mtefe package (Andresen, 2018) provides a comprehensive framework for 
estimating Marginal Treatment Effects following Heckman & Vytlacil (2005).

Syntax: mtefe depvar xvars (treatment = instruments), options

Key options:
  - pol(#): Polynomial degree for MTE approximation (default=2)
  - separate: Allow different X coefficients for treated/untreated
  - link(): Probit (default) or logit for selection equation
  - bootreps(): Bootstrap replications for inference on ATE/ATT/ATU

The command estimates:
  - MTE as a function of the unobserved resistance U
  - Treatment effect parameters: ATE, ATT, ATU, LATE
  - Standard errors via bootstrap
*/

* Estimate MTE using mtefe with linear polynomial and bootstrap SEs
di _n "Running mtefe estimation (this may take a few minutes with bootstrap)..."

mtefe ln_salary $X_controls (masters = ga_funding_adj), pol(1) bootreps(50)
est store mtefe_linear

* Store mtefe results - access via _b[effects:] notation
local mtefe_ate = _b[effects:ate]
local mtefe_att = _b[effects:att]
local mtefe_atu = _b[effects:atut]
local mtefe_late = _b[effects:late]

di _n "mtefe Results (Linear MTE, pol=1):"
di "  ATE:  " %6.4f `mtefe_ate'
di "  ATT:  " %6.4f `mtefe_att'
di "  ATU:  " %6.4f `mtefe_atu'
di "  LATE: " %6.4f `mtefe_late'

* Estimate with quadratic polynomial for comparison
di _n "Running mtefe with quadratic polynomial..."

mtefe ln_salary $X_controls (masters = ga_funding_adj), pol(2) bootreps(50)
est store mtefe_quad

local mtefe_ate_q = _b[effects:ate]
local mtefe_att_q = _b[effects:att]
local mtefe_atu_q = _b[effects:atut]

di _n "mtefe Results (Quadratic MTE, pol=2):"
di "  ATE:  " %6.4f `mtefe_ate_q'
di "  ATT:  " %6.4f `mtefe_att_q'
di "  ATU:  " %6.4f `mtefe_atu_q'

* Generate mtefe MTE curve plot
di _n "Generating mtefe MTE plot..."
mtefe ln_salary $X_controls (masters = ga_funding_adj), pol(2) mte

* Note: mtefe creates a graph named "mtePlot" by default

* Display comparison of manual vs mtefe estimates
di _n "--- Comparison: Manual Polynomial vs. mtefe ---"
di "Parameter     Manual (Cubic)    mtefe (Quad)"
di "================================================"
di "ATE           " %6.4f `ate_est_cubic' "           " %6.4f `mtefe_ate_q'
di "ATT           " %6.4f `att_est' "           " %6.4f `mtefe_att_q'
di "ATU           " %6.4f `atu_est' "           " %6.4f `mtefe_atu_q'

di _n "Note: Small differences are expected due to different polynomial"
di "      specifications and estimation approaches."

*------------------------------------------------------------------------------
* Approach 3: Heckman Selection Model
*------------------------------------------------------------------------------

di _n "--- Approach 3: Heckman Selection Model ---"

* Two-step estimation
heckman ln_salary $X, select(masters = $Z $X) twostep
local heck2_lambda = e(mills)
local heck2_rho = e(rho)
local heck2_sigma = e(sigma)
est store heck_2step

di _n "Heckman 2-step results:"
di "  lambda (selection correction): " %6.4f `heck2_lambda'
di "  rho (correlation):             " %6.4f `heck2_rho'
di "  sigma:                         " %6.4f `heck2_sigma'

* Maximum likelihood estimation
heckman ln_salary $X, select(masters = $Z $X) 
local heck_ml_rho = tanh(_b[/athrho])
local heck_ml_sigma = exp(_b[/lnsigma])
local heck_ml_lambda = `heck_ml_rho' * `heck_ml_sigma'
local heck_ml_chi2 = e(chi2_c)
local heck_ml_p = e(p_c)
est store heck_ml

di _n "Heckman ML results:"
di "  lambda (selection correction): " %6.4f `heck_ml_lambda'
di "  rho (correlation):             " %6.4f `heck_ml_rho'
di "  sigma:                         " %6.4f `heck_ml_sigma'
di "  LR test of indep. eqns:        chi2 = " %6.2f `heck_ml_chi2' ", p = " %5.4f `heck_ml_p'

di _n "IMPORTANT: In Heckman selection models:"
di "  - 'masters' appears in the SELECTION equation, not outcome equation"
di "  - Lambda (inverse Mills ratio) corrects for selection bias"
di "  - Lambda is NOT the treatment effect"
di "  - To get treatment effects, use MTE framework"

********************************************************************************
* SECTION 7: Results Comparison
********************************************************************************

di _n "=============================================="
di "RESULTS COMPARISON"
di "=============================================="

di _n "ESTIMATED PARAMETERS:"
di "  Naive OLS:           " %6.4f `ols_est' " (likely biased by selection)"
di "  IV/LATE:             " %6.4f `iv_est' " (complier effect)"
di "  MTE-based ATE:       " %6.4f `ate_est_cubic' " (manual polynomial)"
di "  MTE-based ATT:       " %6.4f `att_est' " (manual polynomial)"
di "  MTE-based ATU:       " %6.4f `atu_est' " (manual polynomial)"
di "  mtefe ATE:           " %6.4f `mtefe_ate_q' " (mtefe package)"
di "  mtefe ATT:           " %6.4f `mtefe_att_q' " (mtefe package)"
di "  mtefe ATU:           " %6.4f `mtefe_atu_q' " (mtefe package)"

di _n "Selection Pattern Check:"
if `att_est' > `ate_est_cubic' & `ate_est_cubic' > `atu_est' {
    di "  ATT (" %6.4f `att_est' ") > ATE (" %6.4f `ate_est_cubic' ") > ATU (" %6.4f `atu_est' ")"
    di "  Confirms POSITIVE SELECTION on gains"
}
else if `att_est' < `ate_est_cubic' & `ate_est_cubic' < `atu_est' {
    di "  ATT (" %6.4f `att_est' ") < ATE (" %6.4f `ate_est_cubic' ") < ATU (" %6.4f `atu_est' ")"
    di "  Indicates NEGATIVE SELECTION on gains"
}
else {
    di "  ATT = " %6.4f `att_est' ", ATE = " %6.4f `ate_est_cubic' ", ATU = " %6.4f `atu_est'
    di "  Mixed selection pattern"
}

* Bias analysis
local ols_bias = (`ols_est' - `ate_est_cubic') / `ate_est_cubic' * 100

di _n "OLS BIAS ANALYSIS:"
di "  OLS coefficient:     " %6.4f `ols_est'
di "  MTE-based ATE:       " %6.4f `ate_est_cubic'
di "  Percent difference:  " %5.1f `ols_bias' "%"

* Comparison table
di _n "--- Comparison Table: Direct Treatment Effect Estimates ---"
estimates table ols_naive iv_2sls, ///
    stats(N r2) b(%7.4f) se(%7.4f) keep(masters) ///
    title("OLS vs. IV Estimates of Master's Degree Effect")

di _n "Note: Heckman models not included in table because 'masters'"
di "      is in selection equation. See lambda values above."

********************************************************************************
* SECTION 8: MTE Visualization
********************************************************************************

di _n "=============================================="
di "MTE VISUALIZATION"
di "=============================================="

* Calculate MTE at grid of u values
preserve
    clear
    set obs 100
    gen u = _n/100
    gen mte_est = `b0' + `b1'*u + `b2'*u^2 + `b3'*u^3
    
    * Label for interpretation
    label var u "Unobserved Resistance to Treatment"
    label var mte_est "Marginal Treatment Effect"
    
    twoway (line mte_est u, lcolor(navy) lwidth(medthick)), ///
           ytitle("Marginal Treatment Effect") ///
           xtitle("u (Unobserved Resistance to Treatment)") ///
           title("Estimated MTE Curve") ///
           subtitle("Master's Degree Effect on Log Salary") ///
           note("Declining MTE indicates positive selection on gains" ///
                "ATT > ATE > ATU when MTE is decreasing in u") ///
           name(mte_curve, replace)
    graph save "mte_curve.gph", replace
    graph export "mte_curve.png", replace width(1200)
restore

* MTE by propensity score decile
capture drop p_decile
xtile p_decile = phat, nq(10)

preserve
    collapse (mean) mte_mean=mte_hat (sd) mte_sd=mte_hat (count) n=id, by(p_decile)
    
    di _n "Estimated MTE by Propensity Score Decile:"
    list p_decile mte_mean mte_sd n
    
    twoway (scatter mte_mean p_decile, msize(large) mcolor(navy)) ///
           (line mte_mean p_decile, lcolor(navy) lwidth(medium)), ///
           ytitle("Mean Estimated MTE") ///
           xtitle("Propensity Score Decile") ///
           title("Estimated MTE by Propensity Score Decile") ///
           subtitle("Evidence of Treatment Effect Heterogeneity") ///
           note("Increasing MTE with lower decile indicates positive selection") ///
           name(mte_by_decile, replace)
    graph save "mte_by_decile.gph", replace
    graph export "mte_by_decile.png", replace width(1200)
restore

********************************************************************************
* SECTION 9: Basic Policy Simulation (PRTE)
********************************************************************************

di _n "=============================================="
di "POLICY SIMULATION: INCREASE GA FUNDING"
di "=============================================="

qui sum ga_funding_adj
local ga_current = r(mean)
local ga_new = `ga_current' * 1.2

di "Current mean GA funding: $" %5.2f `ga_current' "k"
di "Proposed GA funding (20% increase): $" %5.2f `ga_new' "k"

* Calculate new propensity scores under policy
gen p_new = normal(z_index + `ga_coef'*(`ga_new' - ga_funding_adj))
gen delta_p = p_new - phat

qui sum delta_p
di "Average increase in Pr(Master's): " %6.4f r(mean)

* PRTE calculation
gen complier_weight = delta_p / r(mean) if delta_p > 0
qui sum mte_hat [aw=complier_weight] if delta_p > 0
local prte_20pct = r(mean)
di "Approximate PRTE (20% GA increase): " %6.4f `prte_20pct'

drop p_new delta_p complier_weight

********************************************************************************
* SECTION 10: MARGINAL POLICY-RELEVANT TREATMENT EFFECTS (MPRTE)
********************************************************************************

di _n "=============================================="
di "MARGINAL POLICY-RELEVANT TREATMENT EFFECTS"
di "=============================================="

/*
The MPRTE answers: "What is the treatment effect for individuals at the
margin of being induced into treatment by a small policy change?"

Key distinction:
  - PRTE: Average effect for ALL compliers induced by a discrete policy change
  - MPRTE: Effect at the MARGIN - the infinitesimal policy change

References:
  - Carneiro, Heckman & Vytlacil (2010, 2011)
  - Heckman & Vytlacil (2005, 2007)
  - Mogstad, Santos & Torgovitsky (2018)
*/

*------------------------------------------------------------------------------
* Scenario 1: Uniform GA Funding Increase
*------------------------------------------------------------------------------

di _n "--- Scenario 1: Uniform $1,000 GA Funding Increase ---"

gen p_new_unif = normal(z_index + `ga_coef'*1)
gen delta_p_unif = p_new_unif - phat
gen response_unif = normalden(invnormal(phat)) * `ga_coef'
gen mte_weighted_unif = mte_hat * response_unif

qui sum mte_weighted_unif
local mprte_unif_num = r(sum)
qui sum response_unif
local mprte_unif_den = r(sum)
local mprte_unif = `mprte_unif_num' / `mprte_unif_den'

di "MPRTE (uniform $1k increase): " %6.4f `mprte_unif'

qui sum mte_hat if phat > 0.25 & phat < 0.40
di "Average MTE for marginal region (p=0.25-0.40): " %6.4f r(mean)

qui sum mte_hat [aw=delta_p_unif] if delta_p_unif > 0
di "PRTE (discrete $1k increase): " %6.4f r(mean)

drop p_new_unif delta_p_unif response_unif mte_weighted_unif

*------------------------------------------------------------------------------
* Scenario 2: Targeted Funding for Low-Income Students
*------------------------------------------------------------------------------

di _n "--- Scenario 2: Targeted $2,000 Increase for Low-Income (Q1-Q2) ---"

gen targeted_lowinc = (parent_income_q <= 2)
gen p_new_lowinc = normal(z_index + `ga_coef'*2*targeted_lowinc)
gen delta_p_lowinc = p_new_lowinc - phat
gen response_lowinc = normalden(invnormal(phat)) * `ga_coef' * 2 * targeted_lowinc
gen mte_weighted_lowinc = mte_hat * response_lowinc

qui sum mte_weighted_lowinc if targeted_lowinc == 1
local mprte_lowinc_num = r(sum)
qui sum response_lowinc if targeted_lowinc == 1
local mprte_lowinc_den = r(sum)
local mprte_lowinc = `mprte_lowinc_num' / `mprte_lowinc_den'

di "MPRTE (targeted low-income): " %6.4f `mprte_lowinc'

drop targeted_lowinc p_new_lowinc delta_p_lowinc response_lowinc mte_weighted_lowinc

*------------------------------------------------------------------------------
* Scenario 3: Enhanced STEM GA Funding
*------------------------------------------------------------------------------

di _n "--- Scenario 3: Enhanced $3,000 STEM GA Funding ---"

gen p_new_stem = normal(z_index + `ga_coef'*3*stem_major)
gen delta_p_stem = p_new_stem - phat
gen response_stem = normalden(invnormal(phat)) * `ga_coef' * 3 * stem_major
gen mte_weighted_stem = mte_hat * response_stem

qui sum mte_weighted_stem if stem_major == 1
local mprte_stem_num = r(sum)
qui sum response_stem if stem_major == 1
local mprte_stem_den = r(sum)
local mprte_stem = `mprte_stem_num' / `mprte_stem_den'

di "MPRTE (STEM enhancement): " %6.4f `mprte_stem'

drop p_new_stem delta_p_stem response_stem mte_weighted_stem

*------------------------------------------------------------------------------
* Scenario 4: Education Major Support (Teacher Pipeline)
*------------------------------------------------------------------------------

di _n "--- Scenario 4: Education Major GA Support ($2,500) ---"

gen p_new_ed = normal(z_index + `ga_coef'*2.5*ed_major)
gen delta_p_ed = p_new_ed - phat
gen response_ed = normalden(invnormal(phat)) * `ga_coef' * 2.5 * ed_major
gen mte_weighted_ed = mte_hat * response_ed

qui sum mte_weighted_ed if ed_major == 1
local mprte_ed_num = r(sum)
qui sum response_ed if ed_major == 1
local mprte_ed_den = r(sum)
local mprte_ed = `mprte_ed_num' / `mprte_ed_den'

di "MPRTE (education major support): " %6.4f `mprte_ed'

qui sum phat if ed_major == 1
di "Mean propensity for ed majors: " %6.4f r(mean)

drop p_new_ed delta_p_ed response_ed mte_weighted_ed

********************************************************************************
* SECTION 11: MPRTE BY POLICY INTENSITY
********************************************************************************

di _n "=============================================="
di "MPRTE BY POLICY INTENSITY"
di "=============================================="

preserve
    clear
    set obs 20
    gen ga_increase = _n * 0.5
    
    * Approximate margin location as function of policy intensity
    qui sum phat
    local p_baseline = r(mean)
    gen p_margin = `p_baseline' + ga_increase * 0.015
    
    * Calculate MPRTE at each margin
    gen mprte_approx = `b0' + `b1'*p_margin + `b2'*p_margin^2 + `b3'*p_margin^3
    
    list ga_increase p_margin mprte_approx
    
    twoway (line mprte_approx ga_increase, lcolor(navy) lwidth(medthick)), ///
        ytitle("MPRTE") ///
        xtitle("GA Funding Increase ($1000s)") ///
        title("MPRTE by Policy Intensity") ///
        subtitle("Marginal returns to GA funding expansion") ///
        note("MPRTE pattern depends on selection mechanism" ///
             "and where policy operates on MTE curve") ///
        name(mprte_intensity, replace)
    graph save "mprte_by_intensity.gph", replace
    graph export "mprte_by_intensity.png", replace width(1200)
restore

********************************************************************************
* SECTION 12: COMPARING TREATMENT EFFECT PARAMETERS
********************************************************************************

di _n "=============================================="
di "COMPARISON OF TREATMENT EFFECT PARAMETERS"
di "=============================================="

di _n "Parameter          Manual       mtefe"
di "==========================================="
di "ATE                " %6.4f `ate_est_cubic' "      " %6.4f `mtefe_ate_q'
di "ATT                " %6.4f `att_est' "      " %6.4f `mtefe_att_q'
di "ATU                " %6.4f `atu_est' "      " %6.4f `mtefe_atu_q'
di "LATE (IV)          " %6.4f `iv_est' "      " %6.4f `mtefe_late'
di "-------------------------------------------"
di "MPRTE (uniform)              " %6.4f `mprte_unif'
di "MPRTE (low-income)           " %6.4f `mprte_lowinc'
di "MPRTE (STEM)                 " %6.4f `mprte_stem'
di "MPRTE (education)            " %6.4f `mprte_ed'

di _n "Key insight: Different parameters answer different policy questions"
di "  - ATE: Effect of universal mandatory policy"
di "  - ATT: Effect for current participants (selection already occurred)"
di "  - ATU: Effect if we could induce ALL non-participants"
di "  - LATE: Effect for those induced by instrument variation"
di "  - MPRTE: Effect for those at the MARGIN of a specific policy"

********************************************************************************
* SECTION 13: MPRTE VISUALIZATION
********************************************************************************

di _n "--- Generating MPRTE Visualizations ---"

preserve
    clear
    set obs 100
    gen u = _n/100
    gen mte = `b0' + `b1'*u + `b2'*u^2 + `b3'*u^3
    
    * Define policy-relevant regions
    gen region_lowinc = (u >= 0.10 & u <= 0.25)
    gen region_uniform = (u >= 0.25 & u <= 0.40)
    
    twoway (area mte u if region_lowinc, color(cranberry%30)) ///
           (area mte u if region_uniform, color(navy%30)) ///
           (line mte u, lcolor(navy) lwidth(medthick)), ///
        ytitle("Marginal Treatment Effect") ///
        xtitle("u (Unobserved Resistance to Treatment)") ///
        title("MTE Curve with Policy-Relevant Regions") ///
        subtitle("Different policies target different margins") ///
        legend(order(3 "Estimated MTE" ///
                     1 "Low-income margin" 2 "Uniform policy margin") ///
               cols(2) size(small)) ///
        note("MPRTE = MTE evaluated at the policy-specific margin") ///
        name(mte_policy_regions, replace)
    graph save "mte_policy_regions.gph", replace
    graph export "mte_policy_regions.png", replace width(1200)
restore

* MTE by propensity score with distribution
preserve
    gen p_bin = floor(phat * 20) / 20
    bysort p_bin: egen mean_mte = mean(mte_hat)
    bysort p_bin: gen n_bin = _N
    
    collapse (mean) mean_mte (first) n_bin, by(p_bin)
    
    twoway (bar n_bin p_bin, barwidth(0.04) color(gray%50) yaxis(2)) ///
           (scatter mean_mte p_bin, mcolor(navy) msize(medium) msymbol(D) yaxis(1)) ///
           (line mean_mte p_bin, lcolor(navy) lwidth(medium) yaxis(1)), ///
        ytitle("Estimated MTE", axis(1)) ///
        ytitle("Frequency", axis(2)) ///
        xtitle("Propensity Score") ///
        title("MTE by Propensity Score") ///
        subtitle("MPRTE depends on where policy operates") ///
        legend(order(1 "N per bin" 2 "Estimated MTE") size(small)) ///
        name(mte_by_propensity, replace)
    graph save "mte_by_propensity.gph", replace
    graph export "mte_by_propensity.png", replace width(1200)
restore

********************************************************************************
* SECTION 14: POLICY COST-BENEFIT ANALYSIS
********************************************************************************

di _n "=============================================="
di "POLICY COST-BENEFIT ILLUSTRATION"
di "=============================================="

local cost_per_degree = 50000
local career_years = 30
local discount_rate = 0.03
local base_salary = 47000

local pv_factor = (1 - (1 + `discount_rate')^(-`career_years')) / `discount_rate'
di "Present value factor (30 years, 3%): " %6.2f `pv_factor'

di _n "Policy               MPRTE    Annual Gain   PV Gain    B/C Ratio"
di "=================================================================="

foreach scen in unif lowinc stem ed {
    if "`scen'" == "unif" local mprte_val = `mprte_unif'
    if "`scen'" == "lowinc" local mprte_val = `mprte_lowinc'
    if "`scen'" == "stem" local mprte_val = `mprte_stem'
    if "`scen'" == "ed" local mprte_val = `mprte_ed'
    
    local annual_gain = `base_salary' * (exp(`mprte_val') - 1)
    local pv_gain = `annual_gain' * `pv_factor'
    local bc_ratio = `pv_gain' / `cost_per_degree'
    
    if "`scen'" == "unif" di "Uniform             " %6.4f `mprte_val' "   $" %6.0f `annual_gain' "    $" %8.0f `pv_gain' "   " %5.2f `bc_ratio'
    if "`scen'" == "lowinc" di "Low-income          " %6.4f `mprte_val' "   $" %6.0f `annual_gain' "    $" %8.0f `pv_gain' "   " %5.2f `bc_ratio'
    if "`scen'" == "stem" di "STEM                " %6.4f `mprte_val' "   $" %6.0f `annual_gain' "    $" %8.0f `pv_gain' "   " %5.2f `bc_ratio'
    if "`scen'" == "ed" di "Education           " %6.4f `mprte_val' "   $" %6.0f `annual_gain' "    $" %8.0f `pv_gain' "   " %5.2f `bc_ratio'
}

di _n "Note: B/C ratio > 1 suggests policy expansion is beneficial"
di "      These calculations are illustrative only (synthetic data)"
di "      Real analysis would require actual cost and outcome data"

********************************************************************************
* SECTION 15: Save Results
********************************************************************************

* Label key variables
label var phat "Estimated propensity score"
label var mte_hat "Estimated MTE at individual's propensity"
label var z_index "Probit linear index"

* Save analysis dataset
save "bb_mte_analysis.dta", replace

* Export summary statistics by field
preserve
    collapse (mean) masters ln_salary phat mte_hat ///
             (sd) sd_mte=mte_hat (count) n=id, by(stem_major ed_major)
    export delimited "mte_summary_by_field.csv", replace
restore

********************************************************************************
* SECTION 16: FINAL SUMMARY
********************************************************************************

di _n "=============================================="
di "ANALYSIS COMPLETE"
di "=============================================="
di "Key findings:"
di "  1. Treatment rate: " %5.3f `treat_rate'
di "  2. OLS estimate: " %6.4f `ols_est' " (likely biased)"
di "  3. IV/LATE estimate: " %6.4f `iv_est'
di "  4. MTE-based ATE: " %6.4f `ate_est_cubic' " (manual polynomial)"
di "  5. MTE-based ATT: " %6.4f `att_est' " (manual polynomial)"
di "  6. MTE-based ATU: " %6.4f `atu_est' " (manual polynomial)"
di "  7. mtefe ATE: " %6.4f `mtefe_ate_q' " (mtefe package)"
di "  8. mtefe ATT: " %6.4f `mtefe_att_q' " (mtefe package)"
di "  9. mtefe ATU: " %6.4f `mtefe_atu_q' " (mtefe package)"
di "  10. First-stage F = " %6.1f `first_stage_F' " (strong instrument)"
di "  11. MPRTE varies by policy scenario"

di _n "MPRTE SUMMARY:"
di "  - Uniform policy:    " %6.4f `mprte_unif'
di "  - Low-income target: " %6.4f `mprte_lowinc'
di "  - STEM enhancement:  " %6.4f `mprte_stem'
di "  - Education support: " %6.4f `mprte_ed'

di _n "KEY INSIGHTS:"
di "  - PRTE averages over all compliers induced by policy change"
di "  - MPRTE is the effect at the margin (infinitesimal change)"
di "  - Selection pattern determines how returns change with expansion"
di "  - Cost-effectiveness depends on WHERE policy operates on MTE curve"

di _n "Files saved:"
di "  - bb_mte_analysis.dta (analysis dataset)"
di "  - mte_summary_by_field.csv (summary statistics)"
di "  - mte_curve.gph/.png (MTE curve - manual polynomial)"
di "  - mte_by_decile.gph/.png (MTE by propensity decile)"
di "  - mprte_by_intensity.gph/.png (MPRTE by policy intensity)"
di "  - mte_policy_regions.gph/.png (MTE with policy regions)"
di "  - mte_by_propensity.gph/.png (MTE by propensity score)"

di _n "=============================================="
di "DISPLAYING GRAPHS"
di "=============================================="

* List all graphs in memory
graph dir

* Display each graph
graph display mte_curve
graph display mte_by_decile
graph display mprte_intensity
graph display mte_policy_regions
graph display mte_by_propensity
graph display mtePlot

di _n "All graphs displayed. Use 'graph display [name]' to switch between them."
di "Graph names: mte_curve, mte_by_decile, mprte_intensity, mte_policy_regions,"
di "             mte_by_propensity, mtePlot"

di _n "=============================================="
di "END OF MTE/MPRTE ANALYSIS"
di "=============================================="

di _n "IMPORTANT NOTE:"
di "Because a synthetic dataset is used in this application, the results"
di "are intended to illustrate MTE/MPRTE estimation methods and should"
di "not be viewed as having policy implications."

********************************************************************************
* END
********************************************************************************
