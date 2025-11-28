


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
