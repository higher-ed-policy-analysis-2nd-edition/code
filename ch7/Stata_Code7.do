* Stata Code for Chapter 7
* is available in the book's code repository * on GitHub at:
* https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch7
* README.md - Detailed instructions and documentation
* Important Note: Before running any code, you must:
*	Download data files from the data repository
*	Save them to your local working directory
*	Change all file paths in the code to match your directory structure
*================================================================
* Chapter 7 - Introduction to Intermediate Statistical Techniques
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative Techniques 
* (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-
* edition/tree/main/code/ch7
* Author: Marvin A. Titus
* Date: November 15, 2025
*================================================================

* Script tested in Stata 19.5
* Compatible with Stata version 19 or later

*========================================================================
* IMPORTANT: Set working directory (customize this for your system)
*========================================================================

* Use a global path to make it easy to update in one place
* global ch7data "C:/Users/YourName/Documents/book-materials/ch7/data"
* cd "$ch7data"

*========================================================================
* REQUIRED USER-WRITTEN PACKAGE
*========================================================================

/* The rhausman command is needed for Section 7.5.1 (cluster-robust Hausman test)
   Install once with: ssc install rhausman, replace
   If already installed, you can skip this step */

*========================================================================
* Section 7.2: Review of OLS Regression
* Section 7.22: Bivariate OLS Regression
*========================================================================
*/
/* Download state-level panel dataset (50 states × 27 years, 1990-2016) */
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_2_2.dta" ///
     "Example_7_2_2.dta", replace

use "Example_7_2_2.dta", clear

/* Create per FTE variables (dividing aggregate amounts by enrollment) */
gen netuit_fte = netuit/fte
gen stapr_fte = stapr/fte

/* Bivariate regression for single year (2016)
   Tests relationship between state appropriations and net tuition per FTE */
regress netuit_fte stapr_fte if year==2016

/* Expected results: Negative coefficient (~-0.35), R² ≈ 0.13, F ≈ 7.19 */

*========================================================================
* Section 7.23: Multivariate OLS Regression
*========================================================================

/* Create squared term to test for non-linear (quadratic) relationship */
gen stapr_fte2 = stapr_fte*stapr_fte

/* Add polynomial term and additional control variable (per capita income) */
regress netuit_fte stapr_fte stapr_fte2 pc_income if year==2016

/* Expected results: R² increases to ~0.28 with additional variables */

*========================================================================
* Section 7.24: Multivariate Pooled OLS Regression
*========================================================================

/* Pooled OLS uses all years of data (1990-2016) not just 2016
   This increases N from 50 to 1,350 observations */
reg netuit_fte stapr_fte stapr_fte2 pc_income

/* Add categorical control for regional compact membership
   i. prefix creates dummy variables for each category */
reg netuit_fte stapr_fte stapr_fte2 pc_income i.region_compact

*========================================================================
* Section 7.24.1: Multivariate Pooled OLS Regression with Interaction Terms
*========================================================================

/* EXAMPLE 1: Categorical × Categorical Interaction
   ## operator creates all combinations of region_compact and ugradmerit
   Tests whether merit aid effects differ by region */
reg netuit_fte stapr_fte i.region_compact##i.ugradmerit, allbaselevels

/* Store models to test whether adding interactions improves fit */
quietly: reg netuit_fte stapr_fte i.region_compact
est sto model1

quietly: reg netuit_fte stapr_fte i.region_compact##i.ugradmerit
est sto model2

/* Likelihood ratio test: Are interaction terms jointly significant? */
lrtest model1 model2

/* Alternative test of interaction terms only */
testparm i.region_compact#i.ugradmerit

/* EXAMPLE 2: Categorical × Continuous Interaction
   c. prefix indicates continuous variable
   Tests whether appropriations effect varies by tuition-setting authority */
reg netuit_fte i.ugradmerit i.region_compact c.stapr_fte##i.tuitset

/* Test interaction terms */
testparm c.stapr_fte#i.tuitset

/* EXAMPLE 3: Continuous × Continuous Interaction
   Tests whether appropriations effect varies by level of need-based aid */
reg netuit_fte i.region_compact c.stapr_fte##c.state_needFTE

/* Calculate marginal effect of appropriations at different aid levels
   at() specifies values: 0, 3000, 6000, 9000, 10000
   dydx() calculates derivative (marginal effect) */
margins, dydx(stapr_fte) at(state_needFTE=(0(3000)10000)) vsquish

/* Generate predicted values for graphing
   quietly suppresses output (graph is what matters) */
quietly: margins, at(stapr_fte=(0 10000) state_needFTE=(0(3000)10000)) vsquish

/* Create visualization showing how relationship changes */
marginsplot, noci x(stapr_fte) recast(line) xlabel(0(3000)10000)

*========================================================================
* Section 7.24: Testing Regression Assumptions
*========================================================================

/* Create residual-versus-fitted plot to check for heteroscedasticity
   Funnel shape indicates violation of constant variance assumption */
quietly: reg netuit_fte stapr_fte stapr_fte2 pc_income i.region_compact
rvfplot

/* Information matrix test for heteroscedasticity, skewness, and kurtosis */
estat imtest

/* Robust standard errors correct for heteroscedasticity
   Results are valid even if constant variance assumption is violated */
reg netuit_fte stapr_fte stapr_fte2 pc_income i.region_compact, robust

/* Test for heteroscedasticity across states
   Generates residuals, then tests if variance differs by state */
quietly: reg netuit_fte stapr_fte stapr_fte2 pc_income i.region_compact
predict double eps, residual
robvar eps, by(state)

/* Cluster-robust standard errors account for within-state correlation
   Use when observations within same state are not independent */
reg netuit_fte stapr_fte stapr_fte2 pc_income i.region_compact, cluster(state)

*========================================================================
* Section 7.4: Fixed-Effects Regression
* Section 7.4.2: Estimating FEDV Multivariate POLS Regression Models
*========================================================================

/* Download institutional-level panel dataset
   Different dataset: 220 institutions observed over ~9 years each */
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_4_2.dta" ///
     "Example_7_4_2.dta", replace

use "Example_7_4_2.dta", clear

/* Fixed-effects with state dummy variables (FEDV approach)
   i.stateid creates dummy for each state, controlling for state differences */
reg netuit_fte stapr_fte stapr_fte2 pc_income i.stateid, cluster(state)

/* Test whether state dummies are jointly significant
   If significant, state fixed effects are needed */
testparm i.stateid

/* Alternative: areg command "absorbs" fixed effects (more efficient)
   Produces same coefficients but doesn't display all dummy variables */
areg netuit_fte stapr_fte stapr_fte2 pc_income, ///
     cluster(stateid) absorb(stateid)

/* Institutional-level fixed effects example
   Controls for time-invariant institution characteristics
   eg = education & general expenditures (dependent variable) */
areg eg statea tuition totfteiarep ftfac ptfac D, ///
     cluster(opeid5_new) absorb(opeid5_new)

*========================================================================
* Section 7.4.2.1: Within-Group Estimator Fixed-Effects Regression
*========================================================================

/* xtreg with fe option uses "within" transformation
   Automatically removes time-invariant characteristics
   More information provided than areg (rho, within/between R²) */
xtreg eg statea tuition totfteiarep ftfac ptfac, fe cluster(opeid5_new)

/* Output includes:
     - rho: fraction of variance due to fixed effects
     - within R²: variation explained within units
     - between R²: variation explained between units */

*========================================================================
* Section 7.5: Random-Effects Regression
*========================================================================

/* Return to state-level panel data */
use "Example_7_2_2.dta", clear

/* Recreate per FTE variables */
capture drop netuit_fte stapr_fte stapr_fte2
gen netuit_fte = netuit/fte
gen stapr_fte = stapr/fte
gen stapr_fte2 = stapr_fte*stapr_fte

/* Random-effects assumes unit effects uncorrelated with predictors
   GLS estimation is more efficient than FE if assumption holds */
xtreg netuit_fte stapr_fte stapr_fte2 pc_income i.region_compact, ///
      re cluster(stateid)

/* Breusch-Pagan test: Is random effects better than pooled OLS?
   Null hypothesis: variance of random effects = 0
   If rejected, use random effects instead of pooled OLS */
xttest0

*========================================================================
* Section 7.5.1: Hausman Test
*========================================================================

/* Hausman test: Should we use fixed or random effects?
   Null hypothesis: Random effects estimates are consistent
   If rejected, use fixed effects */

/* For institutional-level data */
use "Example_7_4_2.dta", clear

/* Estimate both models and store results */
quietly: xtreg eg statea tuition totfteiarep ftfac ptfac, fe
est sto fixed

quietly: xtreg eg statea tuition totfteiarep ftfac ptfac, re
est sto random

/* Hausman test compares the two estimators
   Rejection suggests fixed effects is appropriate */
hausman fixed random

/* Log-transformed variables often work better for Hausman test
   Reduces influence of outliers and improves test properties */
quietly: xtreg lneg lnstatea lntuition lntotfteiarep lnftfac ptfac, fe
est sto fixed

quietly: xtreg lneg lnstatea lntuition lntotfteiarep lnftfac ptfac, re
est sto random

hausman fixed random

/* Cluster-robust Hausman test (more reliable with clustered errors)
   Requires user-written rhausman package
   Install with: ssc install rhausman, replace */
quietly: xtreg lneg lnstatea lntuition lntotfteiarep lnftfac ptfac, ///
         cluster(opeid5_new) fe
est sto fixed

quietly: xtreg lneg lnstatea lntuition lntotfteiarep lnftfac ptfac, ///
         cluster(opeid5_new) re
est sto random

/* The bootstrap-based test with 400 replications
   is more computationally intensive but more robust. It will take a while to
   run, depending on the speed of our computer. */
rhausman fixed random, reps(400) cluster

********************************************************************************
********************************************************************************
*
* SECTION 7.6: INSTRUMENTAL VARIABLES AND TWO-STAGE LEAST SQUARES
*
* This section introduces IV/2SLS estimation using a simulation based on
* the Baccalaureate and Beyond Longitudinal Study (B&B) characteristics.
*
* Application: Effect of Master's Degree on Salary Outcomes
* Instrument: State Graduate Assistantship (GA) Funding
*
* The same synthetic dataset is used in Chapter 10 for Marginal Treatment
* Effects (MTE) analysis, providing pedagogical continuity.
*
********************************************************************************
********************************************************************************

clear all
set more off
set seed 20251130

*========================================================================
* Section 7.6.1-7.6.5: Synthetic Data Generation
* (B&B-Style Simulation for IV/2SLS Demonstration)
*========================================================================

/*
NOTE ON SYNTHETIC DATA:
-----------------------
This application uses synthetic data calibrated to mirror the Baccalaureate
and Beyond Longitudinal Study (B&B). We use synthetic rather than actual
B&B data for several reasons:

1. ACCESS RESTRICTIONS: B&B restricted-use data requires NCES license
2. PEDAGOGICAL TRANSPARENCY: Known true parameters allow validation
3. REPRODUCIBILITY: Readers can generate identical datasets
4. CONTINUITY: Same dataset used in Chapter 10 for MTE analysis

NOTE ON AI-ASSISTED CODE DEVELOPMENT:
-------------------------------------
The simulation code was developed with assistance from Claude (Anthropic).
The author provided specifications based on B&B characteristics and higher
education finance literature. Claude assisted in translating specifications
to executable code. The author reviewed, tested, and validated all code.
*/

* Set sample size
local N = 8000
set obs `N'
gen id = _n

*--- Section 1: Demographics ---*

gen female = rbinomial(1, 0.57)
label var female "Female (1=Yes)"

gen race_rand = runiform()
gen byte white = (race_rand < 0.62)
gen byte black = (race_rand >= 0.62 & race_rand < 0.72)
gen byte hispanic = (race_rand >= 0.72 & race_rand < 0.84)
gen byte asian = (race_rand >= 0.84 & race_rand < 0.92)
gen byte other_race = (race_rand >= 0.92)
drop race_rand

gen age_ba = 22 + rpoisson(1.5)
replace age_ba = 22 if age_ba < 20
replace age_ba = 35 if age_ba > 35

*--- Section 2: Family Background ---*

gen firstgen = rbinomial(1, 0.35)
gen parent_income_q = 1 + rbinomial(4, 0.55)
gen parent_grad = rbinomial(1, 0.25)

*--- Section 3: Academic Background ---*

gen ugpa = 2.0 + 1.2*rbeta(5, 3)
replace ugpa = 4.0 if ugpa > 4.0
replace ugpa = 2.0 if ugpa < 2.0

gen stem_major = rbinomial(1, 0.25)
gen bus_major = rbinomial(1, 0.20) if stem_major == 0
replace bus_major = 0 if stem_major == 1
gen ed_major = rbinomial(1, 0.15) if stem_major == 0 & bus_major == 0
replace ed_major = 0 if stem_major == 1 | bus_major == 1
gen socsci_major = (stem_major == 0 & bus_major == 0 & ed_major == 0)

gen selective_inst = rbinomial(1, 0.30)
gen public_ug = rbinomial(1, 0.65)

*--- Section 4: Labor Market ---*

gen state_unemp = 4 + 6*rbeta(2, 3)
gen metro = rbinomial(1, 0.75)

*--- Section 5: Generate Instrument - State GA Funding ---*

gen state = ceil(50*runiform())

bysort state: gen state_effect = rnormal(0, 4) if _n == 1
bysort state: replace state_effect = state_effect[1]

gen ga_funding = 18 + state_effect + rnormal(0, 2)
replace ga_funding = 8 if ga_funding < 8
replace ga_funding = 35 if ga_funding > 35

gen ga_field_mult = 1.3 if stem_major == 1
replace ga_field_mult = 0.9 if bus_major == 1
replace ga_field_mult = 1.1 if ed_major == 1
replace ga_field_mult = 1.0 if socsci_major == 1

gen ga_funding_adj = ga_funding * ga_field_mult
drop state_effect ga_field_mult

label var ga_funding_adj "State GA Funding (field-adjusted, $1000s)"

*--- Section 6: Generate Latent Factors (Unobserved) ---*

gen eta_ability = rnormal(0, 1)
gen eta_taste = 0.3*eta_ability + rnormal(0, 0.9)
gen eta_prod = 0.5*eta_ability + rnormal(0, 0.85)

*--- Section 7: Generate Treatment (Master's Degree) ---*

gen z_masters = ///
    -0.9 + ///                              /* Baseline */
    0.15*female + ///
    0.10*black + ///
    0.05*hispanic + ///
    0.20*asian + ///
    -0.03*(age_ba - 22) + ///
    -0.25*firstgen + ///
    0.08*parent_income_q + ///
    0.35*parent_grad + ///
    0.60*(ugpa - 3.0) + ///
    0.20*stem_major + ///
    -0.15*bus_major + ///
    0.45*ed_major + ///
    0.30*selective_inst + ///
    -0.02*state_unemp + ///
    0.15*metro + ///
    0.06*(ga_funding_adj - 18) + ///       /* INSTRUMENT EFFECT */
    0.40*eta_taste + ///                   /* Unobserved taste for education */
    0.25*eta_ability                       /* Unobserved ability */

gen p_masters = normal(z_masters)
gen u_d = runiform()
gen masters = (p_masters > u_d)

label var masters "Completed Master's Degree (1=Yes)"
label var p_masters "Propensity Score (true)"

*--- Section 8: Generate Outcome (Salary) ---*

* Potential outcome without treatment (Y0)
gen ln_salary_0 = ///
    10.50 + ///
    -0.08*female + ///
    -0.05*black + ///
    -0.03*hispanic + ///
    0.06*asian + ///
    0.02*(age_ba - 22) + ///
    -0.03*firstgen + ///
    0.03*parent_income_q + ///
    0.04*parent_grad + ///
    0.10*(ugpa - 3.0) + ///
    0.25*stem_major + ///
    0.15*bus_major + ///
    -0.12*ed_major + ///
    0.08*selective_inst + ///
    -0.01*state_unemp + ///
    0.10*metro + ///
    0.20*eta_prod + ///                    /* Unobserved productivity */
    rnormal(0, 0.25)

* Heterogeneous treatment effect (essential heterogeneity)
gen te_masters = ///
    0.12 + ///                             /* Base effect */
    0.08*stem_major + ///
    0.05*bus_major + ///
    0.10*ed_major + ///
    0.03*selective_inst + ///
    0.05*(ugpa - 3.0) + ///
    0.08*eta_ability + ///                 /* Ability-education complementarity */
    -0.10*(p_masters - 0.5) + ///          /* Essential heterogeneity */
    rnormal(0, 0.05)

label var te_masters "True Individual Treatment Effect"

* Potential outcome with treatment (Y1)
gen ln_salary_1 = ln_salary_0 + te_masters

* Observed outcome (switching regression)
gen ln_salary = masters*ln_salary_1 + (1-masters)*ln_salary_0
gen salary = exp(ln_salary)

label var ln_salary "Log Annual Salary"
label var salary "Annual Salary ($)"

*========================================================================
* Section 7.6.6: Summary Statistics and True Parameters
*========================================================================

di _n "=============================================="
di "SECTION 7.6: IV/2SLS DEMONSTRATION"
di "Effect of Master's Degree on Salary"
di "=============================================="

di _n "--- Sample Characteristics ---"
tab masters
sum salary ln_salary masters female ugpa ga_funding_adj p_masters

di _n "--- True Treatment Effects (from DGP) ---"
sum te_masters if masters == 1
local true_att = r(mean)
di "True ATT (treated): " %6.4f `true_att'

sum te_masters if masters == 0
local true_atu = r(mean)
di "True ATU (untreated): " %6.4f `true_atu'

sum te_masters
local true_ate = r(mean)
di "True ATE (population): " %6.4f `true_ate'

di _n "Selection Pattern: ATT > ATE > ATU"
di "This confirms POSITIVE SELECTION on gains"
di "(Those who select into treatment benefit more)"

*========================================================================
* Section 7.6.7: Naive OLS Estimation (Biased)
*========================================================================

di _n "=============================================="
di "NAIVE OLS ESTIMATION"
di "=============================================="

* Define control variables
global X_controls "female black hispanic asian age_ba firstgen parent_income_q parent_grad ugpa stem_major bus_major ed_major selective_inst public_ug state_unemp metro"

* OLS regression (biased due to selection on unobservables)
regress ln_salary masters $X_controls, robust

local ols_est = _b[masters]
local ols_se = _se[masters]

di _n "OLS Estimate: " %6.4f `ols_est' " (SE = " %6.4f `ols_se' ")"
di "True ATE:     " %6.4f `true_ate'

local ols_bias = (`ols_est' - `true_ate') / `true_ate' * 100
di "OLS Bias:     " %5.1f `ols_bias' "% (upward bias due to positive selection)"

est sto ols_model

*========================================================================
* Section 7.6.8: First-Stage Regression (Instrument Relevance)
*========================================================================

di _n "=============================================="
di "FIRST-STAGE REGRESSION"
di "(Testing Instrument Relevance)"
di "=============================================="

* First stage: Regress endogenous variable on instrument and controls
regress masters ga_funding_adj $X_controls, robust

* Store first-stage results
local fs_coef = _b[ga_funding_adj]
local fs_se = _se[ga_funding_adj]
local fs_t = `fs_coef' / `fs_se'
local fs_F = `fs_t'^2

di _n "First-Stage Results:"
di "  GA Funding coefficient: " %7.4f `fs_coef'
di "  Standard error:         " %7.4f `fs_se'
di "  t-statistic:            " %7.2f `fs_t'
di "  Partial F-statistic:    " %7.1f `fs_F'
di _n "  Stock-Yogo threshold:   F > 10 for weak instrument test"

if `fs_F' > 10 {
    di "  RESULT: Strong instrument (F = " %5.1f `fs_F' " >> 10)"
}
else {
    di "  WARNING: Potentially weak instrument (F = " %5.1f `fs_F' ")"
}

est sto first_stage

*========================================================================
* Section 7.6.9: IV/2SLS Estimation (LATE)
*========================================================================

di _n "=============================================="
di "IV/2SLS ESTIMATION"
di "(Local Average Treatment Effect)"
di "=============================================="

* IV/2SLS using ivregress command
ivregress 2sls ln_salary (masters = ga_funding_adj) $X_controls, ///
    first robust

local iv_est = _b[masters]
local iv_se = _se[masters]

di _n "IV/2SLS Results:"
di "  LATE Estimate:    " %6.4f `iv_est'
di "  Standard Error:   " %6.4f `iv_se'
di "  95% CI:          [" %6.4f (`iv_est' - 1.96*`iv_se') ", " %6.4f (`iv_est' + 1.96*`iv_se') "]"

est sto iv_model

* First-stage diagnostics
estat firststage

* Test for endogeneity (Durbin-Wu-Hausman)
estat endogenous

di _n "Interpretation:"
di "  The IV estimate identifies the Local Average Treatment Effect (LATE)"
di "  for COMPLIERS - those whose master's degree completion is affected"
di "  by variation in state GA funding."

*========================================================================
* Section 7.6.10: Comparison of Estimates
*========================================================================

di _n "=============================================="
di "COMPARISON OF ESTIMATES"
di "=============================================="

di _n "Method               Estimate    Std.Err.    Interpretation"
di "================================================================"
di "True ATE             " %7.4f `true_ate' "       —       Population average effect"
di "True ATT             " %7.4f `true_att' "       —       Effect for treated"
di "True ATU             " %7.4f `true_atu' "       —       Effect for untreated"
di "----------------------------------------------------------------"
di "OLS (biased)         " %7.4f `ols_est' "    " %6.4f `ols_se' "    Confounded by selection"
di "IV/2SLS (LATE)       " %7.4f `iv_est' "    " %6.4f `iv_se' "    Effect for compliers"
di "================================================================"

di _n "Key Insights:"
di "  1. OLS is biased upward (" %4.1f `ols_bias' "%) due to positive selection"
di "  2. IV provides consistent estimate of LATE for compliers"
di "  3. LATE ≠ ATE when treatment effects are heterogeneous"
di "  4. First-stage F = " %5.1f `fs_F' " confirms strong instrument"

* Create comparison table
estimates table ols_model iv_model, ///
    keep(masters) b(%9.4f) se(%9.4f) ///
    stats(N r2) ///
    title("OLS vs. IV/2SLS: Effect of Master's Degree on Log Salary")

*========================================================================
* Section 7.6.11: Manual 2SLS (Pedagogical Demonstration)
*========================================================================

di _n "=============================================="
di "MANUAL 2SLS (for understanding)"
di "=============================================="

di "NOTE: This manual approach is for pedagogical purposes only."
di "      Standard errors are INCORRECT with manual 2SLS."
di "      Always use ivregress for proper inference."

* Stage 1: Predict treatment using instrument
quietly regress masters ga_funding_adj $X_controls
predict masters_hat, xb

* Stage 2: Regress outcome on predicted treatment
regress ln_salary masters_hat $X_controls

local manual_iv = _b[masters_hat]
di _n "Manual 2SLS estimate: " %6.4f `manual_iv'
di "ivregress estimate:   " %6.4f `iv_est'
di "(Should be identical)"

drop masters_hat

*========================================================================
* Section 7.6.12: Preview of Chapter 10 (MTE Framework)
*========================================================================

di _n "=============================================="
di "PREVIEW: CHAPTER 10 - MARGINAL TREATMENT EFFECTS"
di "=============================================="

di _n "The IV/LATE framework has an important limitation:"
di "  - LATE identifies the effect only for COMPLIERS"
di "  - Different instruments yield different LATEs"
di "  - We cannot recover ATE, ATT, or ATU directly"

di _n "In Chapter 10, we extend this analysis using:"
di "  - Marginal Treatment Effects (MTE) framework"
di "  - Recovers full distribution of treatment effects"
di "  - Allows calculation of ATE, ATT, ATU, and policy-specific effects"
di "  - Uses the same synthetic B&B dataset for continuity"

di _n "The MTE framework reveals:"
di "  - How treatment effects vary with propensity to select"
di "  - Selection patterns (positive vs. negative selection on gains)"
di "  - Policy-relevant treatment effects (PRTE, MPRTE)"

*========================================================================
* Save Dataset for Chapter 10
*========================================================================

save "bb_iv_simulation.dta", replace
di _n "Dataset saved: bb_iv_simulation.dta"
di "This dataset will be used in Chapter 10 for MTE analysis."

*================================================================
* END OF CHAPTER 7 CODE
*================================================================

clear all
exit
