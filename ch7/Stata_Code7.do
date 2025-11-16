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

*========================================================================
* Section 7.4.3: Fixed-Effects Regression and Difference-in-Differences
* Section 7.4.3.2: Fixed-Effects Regression-Based DiD Example
*========================================================================

/* Difference-in-differences evaluates policy impact
   Example: Colorado's College Opportunity Fund (enacted 2004) */

/* Return to state-level data */
use "Example_7_2_2.dta", clear

/* Create per FTE variables */
gen netuit_fte = netuit/fte
gen stapr_fte = stapr/fte

/* Create treatment indicator: Colorado = 1, all other states = 0 */
gen T = 0
replace T = 1 if state=="CO"

/* Create post-treatment indicator: 1 for years 2004 and after */
gen P = 0
replace P = 1 if year>=2004

/* Define control groups
   C1: All states except Colorado (broad control group) */
gen C1 = 0
replace C1 = 1 if state !="CO"

/* C2: Only WICHE states except Colorado (regional control group)
   region_compact==2 identifies WICHE member states */
gen C2 = 0
replace C2 = 1 if state !="CO" & region_compact==2

/* Create global macros for convenience
   Dependent variable */
global y "netuit_fte"

/* Control variables (covariates) */
global controls "stapr_fte pc_income"

/* DiD regression with state and year fixed effects
   T#P is the DiD estimator (treatment effect)
   i.year controls for common time trends
   i.fips creates state fixed effects
   robust option provides heteroscedasticity-robust standard errors */
reg $y i.T i.P T#P $controls i.year i.fips ///
    if year>=2000 & (C1==1 | T==1), robust

/* Within-group fixed-effects DiD (alternative specification)
   xtreg with fe automatically includes state fixed effects
   T##P creates T, P, and T×P (same as i.T i.P T#P) */
xtreg $y T##P $controls i.year ///
      if year>=2000 & (C1==1 | T==1), fe robust

/* DiD with regional control group (WICHE states only)
   Provides more comparable comparison states
   Trade-off: Fewer control states but better similarity */
xtreg $y T##P $controls i.year ///
      if year>=2000 & (C2==1 | T==1), fe robust

*========================================================================
* Section 7.4.3.3: DiD Placebo Tests
*========================================================================

/* Placebo tests check for spurious treatment effects
   Falsely assign treatment to pre-treatment period
   If "effect" found, original DiD may be invalid */

/* Create placebo treatment: Falsely set treatment year to 2000 */
gen placebo_2000 = 1 if year>=2000
recode placebo_2000 (.=0)

/* Test for "treatment effect" in pre-treatment period (1996-2004)
   If placebo is significant, parallel trends assumption likely violated */
xtreg $y T##placebo_2000 $controls ///
      if (year>1995 | year<2005) & (C2==1 | T==1), fe robust

/* Expected result: Placebo should NOT be statistically significant
   Significant placebo suggests pre-existing trends, not policy effect */

clear all
exit

*================================================================
* END OF CHAPTER 7 CODE
*================================================================


