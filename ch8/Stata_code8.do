*================================================================
* Chapter 8 - Advanced Statistical Techniques: I
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative Techniques
* (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-
* edition/tree/main/code/ch8
* Author: Marvin A. Titus
* Date: November 19, 2025
*================================================================

* Script tested in Stata 19.5
* Compatible with Stata version 19 or later
*================================================================
* IMPORTANT: Set working directory (customize this for your system)
*================================================================

/* Use a global path to make it easy to update in one place
global ch8data "C:/Users/YourName/Documents/book-materials/ch8/data"
cd "$ch8data"
*/
*================================================================
* Section 8.2: Time Series Data and Autocorrelation
*================================================================

clear all

* Access the time series dataset using the copy and use commands
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch8/Example_8_2.dta" ///
     "Example_8_2.dta", replace
use "Example_8_2.dta", clear

* Create log-transformed variables
gen lntupub2yr = log(tupub2yr)
gen lnenpub2yr = log(enpub2yr)
gen lnunemprate = log(unemprate)

* lablel variables
lab var lntupub2yr "Tuition at 2-yr colleges"
lab var lnenpub2yr "Enrollment at 2-yr colleges"
lab var lnunemprate "State-wide unemployment rate"


* Set data as time series
tsset year

* Create line graph of time series variables
twoway (line lnenpub2yr year, lcolor(black) lpattern(solid)) ///
	(line lntupub2yr year, lcolor(black) lpattern(dash)) ///
	(line lnunemprate year, lcolor(black) lpattern(dot)), ///
	xlabel(1970 (6) 2017, labsize(small)) ytitle(Logs) ///
	title("Trends in Enrollment in 2 YR, Tuition at 2 YR, and Unemployment Rates" "1970 to 2017", size(medium))

* DF-GLS unit root tests for stationarity
dfgls lnenpub2yr
dfgls lntupub2yr
dfgls lnunemprate

* Graph first-differenced time series
twoway (line D1.lnenpub2yr year, lcolor(black) lpattern(solid)) ///
	(line D1.lntupub2yr year, lcolor(black) lpattern(dash)) ///
	(line D1.lnunemprate year, lcolor(black) lpattern(dot)), ///
	xlabel(1971 (5) 2017, labsize(small)) ytitle(Change in Logs) ///
	title("First-Differenced Enrollment in 2 YR, Tuition at 2 YR, and Unemployment Rates" "1971 to 2017", size(small))

* Regression with first-differenced variables
reg D1.lnenpub2yr D1.lntupub2yr D1.lnunemprate

* Autocorrelation function (correlogram) of residuals
predict residuals, resid
ac residuals

* Partial autocorrelation function
pac residuals, yw

*================================================================
* Section 8.3: Testing for Autocorrelations
* Section 8.3.1: Examples of Autocorrelation Tests—Time Series Data
*================================================================

* Durbin-Watson test for autocorrelation
estat dwatson

* Alternative Durbin-Watson test (with robust standard errors)
quietly: reg D1.lnenpub2yr D1.lntupub2yr D1.lnunemprate, rob
estat durbinalt, force

*================================================================
* Section 8.4: Time Series Regression Models with AR terms
*================================================================
* Prais-Winsten regression with AR(1) term
prais D1.lnenpub2yr D1.lntupub2yr D1.lnunemprate, rob

*================================================================
* Section 8.4.1: Autocorrelation of the Residuals from the P-W Regression
*================================================================

* Generate residuals from Prais-Winsten regression
predict residuals_PW, resid

* Autocorrelation function of P-W residuals
ac residuals_PW

* Partial autocorrelation function of P-W residuals
pac residuals_PW, yw

* Install and run Cumby-Huizinga test (if not already installed)
* ssc install actest, replace
actest residuals_PW, lag(4) q0 rob

* ARMAX model with AR(1) and AR(2) terms
arima D1.lnenpub2yr D1.lntupub2yr D1.lnunemprate, ar(1/2) vce(robust)

* Generate residuals from ARMAX model
predict residuals_armax, residuals

* Test residuals for autocorrelation
actest residuals_armax, lag(4) q0 rob

*================================================================
* Section 8.6: Examples of Autocorrelation Tests—Panel Data
*================================================================

* Download panel dataset using the copy and use commands
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch8/Example_8_6.dta" ///
     "Example_8_6.dta", replace

use "Example_8_6.dta", clear

* We log transform the following variables: stapr; netuit; fte; and pc_income. 
gen lnstapr = log(stapr)
gen lnnetuit = log(netuit)
gen lnfte = log(fte)
gen lnpc_income = log(pc_income)

* Then we invoke the xtserial command with respect to those variables.
xtserial lnnetuit lnstapr lnfte lnpc_income, output

*================================================================
* Section 8.7: Panel-Data Regression Models with AR Terms
*================================================================

* Panel regression with AR(1) error structure, xtregar command
xtregar lnnetuit lnstapr lnfte lnpc_income, fe

* Stata user-written routine xtpurt; unit root tests
xtpurt lnnetuit
xtpurt lnstapr
xtpurt lnfte
xtpurt lnpc_income

* include first-differenced variables in our final regression fixed- or 
* random-effects model with an AR1 disturbance term

. qui xtregar D1.lnnetuit D1.lnstapr D1.lnfte D1.lnpc_income, re
/* However, we conduct a test to see if there is any remaining autocorrelation 
   in the residuals. We do this by using the Cumby-Huizinga (C-H) general test
   for autocorrelation. First, we generate residuals from the model. */
predict ar_residuals_re, ue

* Then we conduct the C-H autocorrelation general test of the residuals.
actest ar_residuals_re, lags(10) q0 robust

*================================================================
* Section 8.8: Cross-Sectional Dependence
* Section 8.8.2: Tests to Detect Cross-Sectional Dependence
*================================================================
/* We change to our working directory and access our dataset from our GitHub
 directory using the copy and use commands. */
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch8/Example_8_8_2.dta" "Example_8_8_2.dta", replace
use "Example_8_8_2.dta", clear

/* We use the xtdescribe (or the shortened version, xtdes) command to get a 
   sense of the distribution of observations per unit (i.e., institution) in the
   panel dataset. */
xtdes

/* We log transform the following variables: eg (education and general expenses)
   ; statea (state higher education appropriations); tuition (tuition and fee 
   revenue); totfteiarep (total FTE enrollment); ftfac (full-time faculty); and
   ptfac (part-time faculty). */
gen lneg = log(eg)
gen lnstatea = log(statea)
gen lntuition = log(tuition)
gen lnfte = log(totfteiarep)
gen lnftfac = log(ftfac)
gen lnptfac = log(ptfac)

/*If not already done so, we install the Stata user-written routine, xtcsd
 (De Hoyos and Sarafidis 2006) */
ssc install xtcsd, replace
 
* Breusch-Pagan LM test for cross-sectional dependence

/* Next, we "quietly" run our fixed-effects regression model using the within 
   regression estimator (xtreg, with the fe option). */
qui: xtreg lneg lnstatea lntuition lnfte lnftfac ptfac, fe

* We run the Pesaran, Friedman, and Frees tests of cross sectional independence.
xtcsd, pesaran
xtcsd, friedman // This takes a while. 
xtcsd, frees    // This also takes a while. 

* If needed, we download the most recent version of xtcd (Eberhardt 2011). 
ssc install xtcd, replace

* Then we run the test on variables of interest from the same panel dataset.
xtcd lneg lntuition lnftfac lnptfac // This may take few seconds.

/* Using the variables that we included in a fixed-effects model above, we 
   employ a random-effects regression model and apply the test to the 
   residuals. */
qui xtreg lneg lnstatea lntuition lnfte lnftfac lnptfac, re
predict ue_residuals_re, ue
xtcd ue_residuals_re

/* If the correlation approaches zero as the units approach infinity, then we
   have what is called a "weak" correlation (Pesaran 2015). Written by Pesaran,
   the Stata routine xtcd2 allows us to test for weak cross-sectional dependence.
*/
* install xtcd2 if needed. 
ssc install xtcd2, replace

* quietly run fixed-effect, then xtcd2 
qui: xtreg lneg lnstatea lntuition lnfte lnftfac lnptfac, fe
xtcd2

/* Finally, another Stata user-written program xtcdf (Wursten 2017) allows for 
   a much faster estimation of the Pesaran cross-sectional dependence test and 
   provides additional statistics. The xtcdf routine also enables us to conduct
   a test on several variables as well as the residuals from a regression model.
   As customary, we first install the most recent version of Wursten-written 
   Stata routine. */
ssc install xtcdf, replace

* Then we "quietly" run our fixed-effect regression model and generate the residuals.
qui xtreg lneg lnstatea lntuition lnfte lnftfac lnptfac, fe
predict ue_residuals_fe, ue

/* We conduct the test, which includes the variables as well as the residuals 
   from the fixed-effects regression. */
xtcdf lneg lnstatea lntuition lnfte lnftfac lnptfac ue_residuals_fe

*================================================================
* Section 8.9: Panel Regression Models That Take Cross-Sectional
*              Dependency into Account
*================================================================
/* * Download panel dataset using the copy and use commands */
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch8/Example_8_8_2.dta" "Example_8_8_2.dta", replace

use "Example_8_8_2.dta", clear

/* We log transform the following variables: eg (education and general expenses)
   ; statea (state higher education appropriations); tuition (tuition and fee 
   revenue); totfteiarep (total FTE enrollment); ftfac (full-time faculty); and
   ptfac (part-time faculty). */

* Create log-transformed variables
gen lneg = log(eg)
gen lnstatea = log(statea)
gen lntuition = log(tuition)
gen lnfte = log(totfteiarep)
gen lnftfac = log(ftfac)
gen lnptfac = log(ptfac)

* We run the fixed-effects regression model with Driscoll-Kraay (D-K) standard errors
xtscc lneg lnstatea lntuition lnfte lnftfac lnptfac, fe lag(2)

/* We include year-fixed effects in a fixed-effects model with D-K standard
 errors */
qui xtscc lneg lnstatea lntuition lnfte lnftfac lnptfac i.endyear, fe lag(2)
predict xtscc_residuals_fe2y, resid
xtcdf xtscc_residuals_fe2y

clear all

exit

*================================================================
* END OF CHAPTER 8 CODE
*================================================================

