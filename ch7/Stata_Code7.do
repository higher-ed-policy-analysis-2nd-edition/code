*================================================================
* Chapter 7 - Introduction to Intermediate Statistical Techniques
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative Techniques 
* (2nd Edition)
*================================================================
* Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch7
* README.md - Detailed instructions and documentation
*
* Important Note: Before running any code, you must:
*   1. Download data files from the data repository
*   2. Save them to your local working directory
*   3. Change all file paths in the code to match your directory structure
*
* Author: Marvin A. Titus
* Date: December 2025
*================================================================

* Script tested in Stata 19.5
* Compatible with Stata version 15 or later

*========================================================================
* IMPORTANT: Set working directory (customize this for your system)
*========================================================================

* Use a global path to make it easy to update in one place
* global ch7data "C:/Users/YourName/Documents/book-materials/ch7/data"
* cd "$ch7data"

*========================================================================
* OUTPUT DIRECTORIES AND LOG FILE
* Paths switch automatically based on the OS username (c(username)).
* The instructor's personal paths are used when username == "marvi";
* all other users get the generic relative paths.
*========================================================================

* Close any stale log silently, then open a fresh one
capture log close

if c(username) == "marvi" {
    global graphs_dir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 7/Output/graphs"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 7/Output"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 7/Output/graphs"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 7/Output/logs"
    log using ///
        "C:\\Users\\marvi\\Dropbox\\Book\\2nd Edition\\Chapter 7\\Output\\logs\\Chapter7_Stata_output.log", ///
        replace text
}
else {
    global graphs_dir "Output/graphs"
    capture mkdir "Output"
    capture mkdir "Output/graphs"
    capture mkdir "Output/logs"
    log using "Output/logs/Chapter7_Stata_output.log", replace text
}

di "Chapter 7 log opened: " c(current_date) " " c(current_time)
di "Graphs directory: $graphs_dir"

*========================================================================
* REQUIRED USER-WRITTEN PACKAGE
*========================================================================

/* The rhausman command is needed for Section 7.4.1 (cluster-robust Hausman test)
   Install once with: ssc install rhausman, replace
   If already installed, you can skip this step */

*========================================================================
*========================================================================
*
*                    SECTION 7.2: REVIEW OF OLS REGRESSION
*
*========================================================================
*========================================================================

*========================================================================
* Section 7.2.2: Bivariate and Multivariate OLS Regression
*========================================================================

/* Download state-level panel dataset (50 states × 27 years, 1990-2016) */
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_2_2.dta" ///
     "Example_7_2_2.dta", replace

use "Example_7_2_2.dta", clear

/* Create per FTE variables (dividing aggregate amounts by enrollment) */
gen netuit_fte = netuit/fte
gen stapr_fte = stapr/fte

*------------------------------------------------------------------------
* Bivariate OLS Regression
*------------------------------------------------------------------------

/* Bivariate regression for single year (2016)
   Tests relationship between state appropriations and net tuition per FTE */
regress netuit_fte stapr_fte if year==2016

/* Expected results: Negative coefficient (~-0.35), R² ≈ 0.13, F ≈ 7.19 */

*------------------------------------------------------------------------
* Multivariate OLS Regression
*------------------------------------------------------------------------

/* Create squared term to test for non-linear (quadratic) relationship */
gen stapr_fte2 = stapr_fte*stapr_fte

/* Add polynomial term and additional control variable (per capita income) */
regress netuit_fte stapr_fte stapr_fte2 pc_income if year==2016

/* Expected results: R² increases to ~0.28 with additional variables */

*========================================================================
* Section 7.2.3: Pooled OLS Regression
*========================================================================

/* Pooled OLS uses all years of data (1990-2016) not just 2016
   This increases N from 50 to 1,350 observations */
reg netuit_fte stapr_fte stapr_fte2 pc_income

/* Add categorical control for regional compact membership
   i. prefix creates dummy variables for each category */
reg netuit_fte stapr_fte stapr_fte2 pc_income i.region_compact

*------------------------------------------------------------------------
* Interaction Terms in Pooled OLS
*------------------------------------------------------------------------

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
graph export "$graphs_dir/fig7_1_marginsplot_Stata.png", replace

*------------------------------------------------------------------------
* Testing Regression Assumptions
*------------------------------------------------------------------------

/* Create residual-versus-fitted plot to check for heteroscedasticity
   Funnel shape indicates violation of constant variance assumption */
quietly: reg netuit_fte stapr_fte stapr_fte2 pc_income i.region_compact
rvfplot
graph export "$graphs_dir/fig7_2_rvfplot_Stata.png", replace

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
*========================================================================
*
*                 SECTION 7.3: FIXED-EFFECTS REGRESSION
*
*========================================================================
*========================================================================

*========================================================================
* Section 7.3.1: Fixed-Effects Dummy Variable (FEDV) Estimation
*========================================================================

/* Continue using state-level panel data (Example_7_2_2.dta) for state FE models */

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

/* Now switch to institutional-level panel dataset for institution FE models
   Different dataset: 220 institutions observed over ~9 years each */
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_3_1.dta" ///
     "Example_7_3_1.dta", replace

use "Example_7_3_1.dta", clear

/* Institutional-level fixed effects example
   Controls for time-invariant institution characteristics
   eg = education & general expenditures (dependent variable) */
areg eg statea tuition totfteiarep ftfac ptfac D, ///
     cluster(opeid5_new) absorb(opeid5_new)

*========================================================================
* Section 7.3.2: Within-Group Estimator
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
*========================================================================
*
*                SECTION 7.4: RANDOM-EFFECTS REGRESSION
*
*========================================================================
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
* Section 7.4.1: The Hausman Test
*========================================================================

/* Hausman test: Should we use fixed or random effects?
   Null hypothesis: Random effects estimates are consistent
   If rejected, use fixed effects */

/* For institutional-level data */
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_3_1.dta" ///
     "Example_7_3_1.dta", replace

use "Example_7_3_1.dta", clear

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
gen lneg = log(eg)
gen lnstatea = log(statea)
gen lntuition = log(tuition)
gen lntotfteiarep = log(totfteiarep)
gen lnftfac = log(ftfac)
gen lnptfac = log(ptfac)

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
   run, depending on the speed of your computer. */
rhausman fixed random, reps(400) cluster

*========================================================================
* Section 7.4.2: Correlated Random-Effects (CRE) Regression
*========================================================================

/* The standard Hausman test (Section 7.4.1) rests on the assumption that
   the random effects (u_i) are uncorrelated with the regressors. When that
   assumption is violated, the RE estimator is inconsistent. The
   correlated random-effects (CRE) estimator -- following Mundlak (1978) --
   addresses this by augmenting the RE model with group (institution) means
   of all time-varying regressors. The group-mean terms absorb the
   correlation between u_i and X, yielding estimates of the time-varying
   coefficients that are numerically identical to the within (FE) estimator.
   The key advantage of CRE over FE is that it:
     (1) allows time-invariant regressors to enter the model directly, and
     (2) provides a clean Wald test of the RE vs. FE choice by testing
         the joint significance of the group-mean terms.

   Stata implements CRE via xtreg with the cre option (Stata 15+).
   The syntax automatically constructs the group means and appends them.

   We continue with the institutional-level log-transformed data from
   the Hausman test above (Example_7_3_1.dta). */

*------------------------------------------------------------------------
* Step 1: Estimate the CRE model
*------------------------------------------------------------------------

/* xtreg, cre re adds the within-group means of all time-varying
   covariates to the RE specification. The prefix "mean(" is applied
   to the generated mean variables in the output. */
xtreg lneg lnstatea lntuition lntotfteiarep lnftfac lnptfac, ///
      cre cluster(opeid5_new)

/* Key output to examine:
     - Coefficients on main regressors: equivalent to FE estimates
     - Coefficients on mean() terms: test whether group means matter
     - rho: fraction of variance attributable to institutional effects
   
   If the group-mean terms are jointly insignificant, RE is preferred
   because it is more efficient. If they are significant, CRE/FE is
   required for consistent estimation. */

*------------------------------------------------------------------------
* Step 2: Joint significance test of the group-mean (Mundlak) terms
*------------------------------------------------------------------------

/* Stata reports the Mundlak test automatically at the bottom of the
   xtreg, cre output as: "Mundlak test (xt_means = 0): chi2(df) = ..."
   No separate post-estimation command is required.

   H0: All xt_means coefficients are jointly zero (RE is consistent)
   Ha: At least one xt_means coefficient is nonzero (FE/CRE required)

   Rejection (p < 0.05): The institutional effects are correlated with
   the regressors, so RE is inconsistent; use CRE or FE.
   Failure to reject: RE is consistent and more efficient than FE;
   the CRE model reduces to the standard RE specification.

   Unlike the standard Hausman test, the Mundlak test:
     (a) is valid under cluster-robust variance estimation, and
     (b) does not require the homoscedasticity assumptions that can
         make the standard Hausman test unreliable in finite samples
         (as flagged in Section 7.4.1 above). */

*========================================================================
*========================================================================
*
*     SECTION 7.5: INSTRUMENTAL VARIABLES AND TWO-STAGE LEAST SQUARES
*
*========================================================================
*========================================================================

clear all
set more off

*========================================================================
* Section 7.5.3: Application - Master's Degree Completion and Salary
*========================================================================

/* This section demonstrates IV/2SLS estimation using the relationship
   between master's degree completion and salary outcomes.
   
   Endogenous Variable: Master's degree completion (masters)
   Instrument: State Graduate Assistantship (GA) Funding (ga_funding_adj)
   Outcome: Log salary (ln_salary)
   
   Note: This uses a synthetic dataset for pedagogical purposes.
   Results should not be interpreted as having policy implications. */

*------------------------------------------------------------------------
* Load Data
*------------------------------------------------------------------------

/* Download synthetic dataset for IV/2SLS demonstration */
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_5_3.dta" ///
     "Example_7_5_3.dta", replace

use "Example_7_5_3.dta", clear

/* Alternative: Load CSV version
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_5_3.csv" ///
     "Example_7_5_3.csv", replace
import delimited "Example_7_5_3.csv", clear
*/

*------------------------------------------------------------------------
* Summary Statistics
*------------------------------------------------------------------------

di _n "--- Sample Characteristics ---"
tab masters

di _n "--- Key Variables ---"
sum ln_salary masters ga_funding_adj p_masters

di _n "--- Salary by Master's Degree Status ---"
tabstat salary ln_salary, by(masters) stats(mean sd n)

*------------------------------------------------------------------------
* OLS Estimation (Potentially Biased)
*------------------------------------------------------------------------

di _n "=============================================="
di "OLS ESTIMATION"
di "(Potentially biased due to endogeneity)"
di "=============================================="

* Define control variables
global X_controls "female black hispanic asian age_ba firstgen parent_income_q parent_grad ugpa stem_major bus_major ed_major selective_inst public_ug state_unemp metro"

* OLS regression
regress ln_salary masters $X_controls, robust

local ols_est = _b[masters]
local ols_se = _se[masters]

di _n "OLS Estimate: " %6.4f `ols_est' " (SE = " %6.4f `ols_se' ")"

estimates store ols_model

*------------------------------------------------------------------------
* First-Stage Regression
*------------------------------------------------------------------------

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
di _n "  Stock-Yogo threshold:   F > 10"

if `fs_F' > 10 {
    di "  RESULT: Strong instrument (F = " %5.1f `fs_F' " > 10)"
}
else {
    di "  WARNING: Potentially weak instrument (F = " %5.1f `fs_F' ")"
}

estimates store first_stage

*------------------------------------------------------------------------
* IV/2SLS Estimation
*------------------------------------------------------------------------

di _n "=============================================="
di "IV/2SLS ESTIMATION"
di "=============================================="

* IV/2SLS using ivregress command
ivregress 2sls ln_salary (masters = ga_funding_adj) $X_controls, ///
    first robust

local iv_est = _b[masters]
local iv_se = _se[masters]

di _n "IV/2SLS Results:"
di "  Coefficient on masters: " %6.4f `iv_est'
di "  Standard error:         " %6.4f `iv_se'
di "  95% CI: [" %6.4f (`iv_est' - 1.96*`iv_se') ", " %6.4f (`iv_est' + 1.96*`iv_se') "]"

estimates store iv_model

*========================================================================
* Section 7.5.4: Assessing Instrument Validity
*========================================================================

di _n "=============================================="
di "ASSESSING INSTRUMENT VALIDITY"
di "=============================================="

*------------------------------------------------------------------------
* First-Stage F-Statistic
*------------------------------------------------------------------------

di _n "--- First-Stage Diagnostics ---"
di "Tests whether instrument is sufficiently strong"
estat firststage

/* The first-stage F-statistic should exceed the Stock-Yogo threshold of 10
   to avoid weak instrument bias */

*------------------------------------------------------------------------
* Weak-Instrument-Robust Inference
*------------------------------------------------------------------------

/* The first-stage F-statistic is a pre-test for instrument strength, but
   it does not protect inference on the structural parameter itself. Even
   when F > 10, confidence intervals and p-values from standard IV/2SLS
   can be misleading if the instrument is only moderately strong. The
   Anderson-Rubin (AR) test and the conditional likelihood ratio (CLR)
   test remain valid regardless of instrument strength because they do not
   condition on a particular first-stage estimate.

   estat weakrobust (Stata 17+) reports these weak-instrument-robust
   statistics after ivregress. It should be run routinely alongside
   estat firststage as part of every IV diagnostic workflow -- not only
   when the F-statistic raises concern, but as a standard validity check.

   H0 (for each test): The structural parameter equals zero
   The tests remain correctly sized even when the instrument is weak. */

estat weakrobust

/* Output includes:
     - Anderson-Rubin Wald statistic: robust to weak instruments;
       valid under homoscedasticity.
     - Anderson-Rubin F-statistic: finite-sample version of the AR test.
     - Stock-Wright S statistic: weak-instrument-robust score test
       for joint significance of all endogenous regressors.
   
   When the instrument is strong (as confirmed by estat firststage above),
   the robust p-values will closely match those from standard IV/2SLS.
   A large divergence between the two would signal that the F-statistic
   overstated instrument strength and that the robust results should be
   preferred for inference. */

*------------------------------------------------------------------------
* Endogeneity Test (Durbin-Wu-Hausman)
*------------------------------------------------------------------------

di _n "--- Endogeneity Test (Durbin-Wu-Hausman) ---"
di "H0: Variable is exogenous (OLS is consistent)"
di "Ha: Variable is endogenous (IV is needed)"
estat endogenous

/* If we reject the null hypothesis (p < 0.05), this confirms that
   the variable is endogenous and IV estimation is warranted */

*------------------------------------------------------------------------
* Comparison of Estimates
*------------------------------------------------------------------------

di _n "=============================================="
di "COMPARISON OF ESTIMATES"
di "=============================================="

* Create comparison table
estimates table ols_model iv_model, ///
    keep(masters) b(%9.4f) se(%9.4f) ///
    stats(N r2) ///
    title("OLS vs. IV/2SLS: Effect of Master's Degree on Log Salary")

di _n "Summary:"
di "  OLS estimate:  " %7.4f `ols_est' " (SE = " %6.4f `ols_se' ")"
di "  IV estimate:   " %7.4f `iv_est' " (SE = " %6.4f `iv_se' ")"

local diff = `ols_est' - `iv_est'
di "  Difference:    " %7.4f `diff'

di _n "Interpretation:"
di "  The OLS estimate exceeds the IV estimate, indicating upward bias"
di "  due to positive selection on unobservables. Students who complete"
di "  master's degrees have higher unobserved ability and motivation,"
di "  which independently increases salary."

*------------------------------------------------------------------------
* Manual 2SLS (Pedagogical Demonstration)
*------------------------------------------------------------------------

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
di "(Coefficients should be identical)"

drop masters_hat

*================================================================
* END OF CHAPTER 7 CODE
*================================================================

clear all

* Close log
capture log close

exit
