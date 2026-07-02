*========================================================================
* Chapter 8 - Advanced Statistical Techniques: I
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative Techniques
* (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch8
* Author: Marvin A. Titus
* Date: November 19, 2025
* NOTE: Code development was assisted by Claude (Anthropic). The author
* provided specifications and reviewed, tested, and validated all code.
*========================================================================
* Script tested in Stata 19.5
* Compatible with Stata version 19 or later

*========================================================================
* IMPORTANT: Set working directory (customize this for your system)
*========================================================================

* Use a global path to make it easy to update in one place
* global ch8data "C:/Users/YourName/Documents/book-materials/ch8/data"
* cd "$ch8data"

*========================================================================
* OUTPUT DIRECTORIES AND LOG FILE
* Paths switch automatically based on the OS username (c(username)).
* The instructor's personal paths are used when username == "marvi";
* all other users get the generic relative paths.
*========================================================================

* Close any stale log silently, then open a fresh one
capture log close

if c(username) == "marvi" {
    global graphs_dir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 8/Output/graphs"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 8/Output"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 8/Output/graphs"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 8/Output/logs"
    log using ///
        "C:\\Users\\marvi\\Dropbox\\Book\\2nd Edition\\Chapter 8\\Output\\logs\\Chapter8_Stata_output.log", ///
        replace text
}
else {
    global graphs_dir "Output/graphs"
    capture mkdir "Output"
    capture mkdir "Output/graphs"
    capture mkdir "Output/logs"
    log using "Output/logs/Chapter8_Stata_output.log", replace text
}

di "Chapter 8 log opened: " c(current_date) " " c(current_time)
di "Graphs directory: $graphs_dir"

clear all
set more off
version 19
set scheme s2mono        // Monochrome scheme for Springer B&W print
set graphics on          // Ensure graph window is active throughout

*========================================================================
* PACKAGE INSTALLATIONS (run once; comment out thereafter)
*========================================================================
* actest is optional; uncomment if needed for autocorrelation diagnostics
* ssc install actest, replace
ssc install xtcsd, replace   // Pesaran CD test for cross-sectional dependence
ssc install xtcd,  replace   // Cross-sectional dependence test
ssc install xtcd2, replace   // Cross-sectional dependence test (alternative)
ssc install xtcdf, replace   // Cross-sectional dependence F-test

*========================================================================
* Section 8.2: Time Series Data and Autocorrelation
*========================================================================

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
graph export "$graphs_dir/fig8_1_ts_levels_Stata.png", replace width(1200)

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
graph export "$graphs_dir/fig8_2_ts_firstdiff_Stata.png", replace width(1200)

* Regression with first-differenced variables
reg D1.lnenpub2yr D1.lntupub2yr D1.lnunemprate

* Autocorrelation function (correlogram) of residuals
predict residuals, resid
ac residuals
graph export "$graphs_dir/fig8_3_ac_residuals_Stata.png", replace width(1200)

* Partial autocorrelation function
pac residuals, yw
graph export "$graphs_dir/fig8_4_pac_residuals_Stata.png", replace width(1200)

*========================================================================
* Section 8.3: Testing for Autocorrelations
* Section 8.3.1: Examples of Autocorrelation Tests—Time Series Data
*========================================================================

* Durbin-Watson test for autocorrelation
estat dwatson

* Alternative Durbin-Watson test (with robust standard errors)
quietly: reg D1.lnenpub2yr D1.lntupub2yr D1.lnunemprate, rob
estat durbinalt, force

*========================================================================
* Section 8.4: Time Series Regression Models with AR terms
*========================================================================
* Prais-Winsten regression with AR(1) term
prais D1.lnenpub2yr D1.lntupub2yr D1.lnunemprate, rob

*========================================================================
* Section 8.4.1: Autocorrelation of the Residuals from the P-W Regression
*========================================================================

* Generate residuals from Prais-Winsten regression
predict residuals_PW, resid

* Autocorrelation function of P-W residuals
ac residuals_PW
graph export "$graphs_dir/fig8_5_ac_residuals_PW_Stata.png", replace width(1200)

* Partial autocorrelation function of P-W residuals
pac residuals_PW, yw
graph export "$graphs_dir/fig8_6_pac_residuals_PW_Stata.png", replace width(1200)

* Install and run Cumby-Huizinga test (if not already installed)
* ssc install actest, replace
actest residuals_PW, lag(4) q0 rob

* ARMAX model with AR(1) and AR(2) terms
arima D1.lnenpub2yr D1.lntupub2yr D1.lnunemprate, ar(1/2) vce(robust)

* Generate residuals from ARMAX model
predict residuals_armax, residuals

* Test residuals for autocorrelation
actest residuals_armax, lag(4) q0 rob

*========================================================================
* Section 8.6: Examples of Autocorrelation Tests—Panel Data
*========================================================================

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

*========================================================================
* Section 8.7: Panel-Data Regression Models with AR Terms
*========================================================================

* Panel regression with AR(1) error structure, xtregar command
xtregar lnnetuit lnstapr lnfte lnpc_income, fe

* Stata user-written routine xtpurt; unit root tests
xtpurt lnnetuit
xtpurt lnstapr
xtpurt lnfte
xtpurt lnpc_income

* include first-differenced variables in our final regression fixed- or 
* random-effects model with an AR1 disturbance term

qui xtregar D1.lnnetuit D1.lnstapr D1.lnfte D1.lnpc_income, re
/* However, we conduct a test to see if there is any remaining autocorrelation 
   in the residuals. We do this by using the Cumby-Huizinga (C-H) general test
   for autocorrelation. First, we generate residuals from the model. */
predict ar_residuals_re, ue

* Then we conduct the C-H autocorrelation general test of the residuals.
actest ar_residuals_re, lags(10) q0 robust

*========================================================================
* Section 8.8: Cross-Sectional Dependence
* Section 8.8.2: Tests to Detect Cross-Sectional Dependence
*========================================================================
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

* xtcsd (De Hoyos and Sarafidis 2006) installed in the package block above
* Breusch-Pagan LM test for cross-sectional dependence

/* Next, we "quietly" run our fixed-effects regression model using the within 
   regression estimator (xtreg, with the fe option). */
qui: xtreg lneg lnstatea lntuition lnfte lnftfac ptfac, fe

* We run the Pesaran, Friedman, and Frees tests of cross sectional independence.
xtcsd, pesaran
xtcsd, friedman // This takes a while. 
xtcsd, frees    // This also takes a while. 

* xtcd (Eberhardt 2011) installed in the package block above

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
* xtcd2 installed in the package block above

* quietly run fixed-effect, then xtcd2 
qui: xtreg lneg lnstatea lntuition lnfte lnftfac lnptfac, fe
xtcd2

/* Finally, another Stata user-written program xtcdf (Wursten 2017) allows for 
   a much faster estimation of the Pesaran cross-sectional dependence test and 
   provides additional statistics. The xtcdf routine also enables us to conduct
   a test on several variables as well as the residuals from a regression model.
   As customary, we first install the most recent version of Wursten-written 
   Stata routine. */
* xtcdf installed in the package block above

* Then we "quietly" run our fixed-effect regression model and generate the residuals.
qui xtreg lneg lnstatea lntuition lnfte lnftfac lnptfac, fe
predict ue_residuals_fe, ue

/* We conduct the test, which includes the variables as well as the residuals 
   from the fixed-effects regression. */
xtcdf lneg lnstatea lntuition lnfte lnftfac lnptfac ue_residuals_fe

*========================================================================
* Section 8.9: Panel Regression Models That Take Cross-Sectional
*              Dependency into Account
*========================================================================
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

*========================================================================
* END OF CHAPTER 8 CODE
*========================================================================

clear all

log close

exit

