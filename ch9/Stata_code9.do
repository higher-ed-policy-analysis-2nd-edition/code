*=======================================================
* Chapter 9 - Advanced Statistical Techniques: II
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative
* Techniques (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis- * 2nd-edition/tree/main/code/ch9
* Author: Marvin A. Titus
* Date: November 20, 2025
*=======================================================
* Script tested in Stata 19.5
* Compatible with Stata version 19 or later
*=======================================================
* IMPORTANT: Set working directory (customize this for 8 * your system)
*=======================================================
/* Use a global path to make it easy to update in one place
global ch9data "C:/Users/YourName/Documents/book-materials/ch9/data"
cd "$ch9data"
*/
*=======================================================
* Heterogeneous Coefficient Regression with DCCE and MG
* Estimators Using Macro Panel Data
*=======================================================

clear all

* Access the time series dataset using the copy and use
* commands
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch9/Example_9_3_1.dta" ///
     "Example_9_3_1.dta", replace
use "Example_9_3_1.dta", clear

*=======================================================
* Section 9.6: Demonstration of HCR with DCCE and MG
* Estimators
*=======================================================

* Variables in the dataset:
* lny1 = log of state appropriations to higher education
* lnx1 = log of net tuition revenue
* lnx2 = log of full-time equivalent students
* lnx3 = log of per capita income

* label variables
lab var lny1 "Log of State Appropriations"
lab var lnx1 "Log of Tuition & Fee Revenue"
lab var lnx2 "Log of FTE Enrollment"
lab var lnx3 "Log of State Per Capita Income"

*=======================================================
* Section 9.6.1: Macroeconomic Panel Data
* Create Figure 9.1: Trends in Log of Appropriations by
* State
*=======================================================
twoway (line lny1 FY), by(state) ///
   xlabel(1980 (12) 2024,labsize(small))///
   ytitle(Log of State Appropriations)///
   xtitle(Fiscal Year)
*=======================================================
* Create Figure 9.2: Trends in Log of Per Capita Income * by State
*=======================================================
twoway (line lnx3 FY), by(state) ///
  xlabel(1980 (12) 2024, labsize(small)) ///
  ytitle(Log of Per Capita Income) xtitle(Fiscal Year)

*=======================================================
* Section 9.3.2: Tests for Nonstationary Data
*=======================================================
* OLS regression
reg lny1 lnx1 lnx2 lnx3 
* 
*=======================================================
* Section 9.6.2: Tests for Nonstationary Data
*=======================================================
/* Use the Stata routine xtpurt, with test options
 proposed by Herwartz and
   Siedenburg (2008), Demetrescu and Hanck (2012), and
   Herwartz et al. (2019). In the three test options,the
 null hypothesis is that the panels (i.e., states)
 contain non-stationary data
  or unit roots.
*/
* xtpurt with test options proposed by Herwartz and Siedenburg (hs)
xtpurt lny1, test(hs)
xtpurt lnx1, test(hs)
xtpurt lnx2, test(hs)
xtpurt lnx3, test(hs)

* xtpurt with test options proposed by Demetrescu and Hanck (dh)
xtpurt lny1, test(dh)
xtpurt lnx1, test(dh)
xtpurt lnx2, test(dh)
xtpurt lnx3, test(dh)

* xtpurt with test options proposed by Herwartz, Maxand, and Walle (hmw)
xtpurt lny1, test(hmw) trend
xtpurt lnx1, test(hmw) trend
xtpurt lnx2, test(hmw) trend
xtpurt lnx3, test(hmw) trend

* xtpurt with all test options with first-differences (d)

* create first-differences
gen dlny1 = D.lny1
gen dlnx1 = D.lnx1
gen dlnx2 = D.lnx2
gen dlnx3 = D.lnx3

xtpurt dlny1, test(all)
xtpurt dlnx1, test(all)
xtpurt dlnx2, test(all)
xtpurt dlnx3, test(all)

*=======================================================
* Section 9.6.3: Tests for Cointegration
*=======================================================

/* Test for no cointegration with and without demeaning 
   (first subtracting the cross-sectional averages from the series) the data
*/
* Kao test for cointegration
xtcointtest kao lny1 lnx1 lnx2 lnx3
xtcointtest kao lny1 lnx1 lnx2 lnx3, demean

* Pedroni test for cointegration
xtcointtest pedroni lny1 lnx1 lnx2 lnx3
xtcointtest pedroni lny1 lnx1 lnx2 lnx3, demean

* Westerlund test for cointegration
xtcointtest westerlund lny1 lnx1 lnx2 lnx3
xtcointtest westerlund lny1 lnx1 lnx2 lnx3, demean

/* ECM-based cointegration test, developed by Westerlund 2007), that is robust
   to structural breaks in the intercept and slope of the cointegrated
   regression, serial correlation, and heteroscedasticity.
*/
xtwest lny1 lnx1 lnx2 lnx3, constant lags(0 3)

*=======================================================
* Section 9.6.4: Tests for Cross-Sectional Independence
*=======================================================

/* Tests using Stata user-written routine xtcdf
   (Wursten 2017) for cross-sectional independence,
   using updated version
*/
ssc install xtcdf, replace
xtcdf lny1 lnx1 lnx2 lnx3

*=======================================================
* Section 9.6.5: Test of Homogeneous Coefficients
*=======================================================

/* Test of homogeneous coefficients utilizing the Stata user-written xthst
   (Ditzen and Bersvendsen 2020) routine
*/
ssc install xthst, replace

* Test with first-differenced variables
xthst D1.lny1 D1.L1.lny1 D1.lnx1 D1.lnx2 D1.lnx3, ///
  hac whitening

* Test with levels variables
xthst lny1 L1.lny1 lnx1 lnx2 lnx3, hac whitening

*=======================================================
* Section 9.6.6: Results of the HCR with DCCE and MG
* Estimators
*=======================================================

/* HCR with DCCE and MG estimators using the Stata-user written xtdcce2
  (Ditzen 2018b)
* search xtdcce2, all
* click on st0536, then install or type:
* net install st0536.pkg, replace
* Run an autoregressive model with distributed lags (ARDLs) of (1 1 1) and
* cross-sectional lags (3 3 3 3) within an ECM framework
*/
xtdcce2 D1.lny1 L1.D1.lny1 L1.D1.lnx1 L1.D1.lnx2 ///
  L1.D1.lnx3, reportc ///
  cr(_all) cr_lags(3 3 3 3) lr(L1.lny1 lnx1 lnx2 ///
  lnx3) lr_options(ardl)

* Pesaran (2015) test for weak cross-sectional dependence
xtcd2

* Run xtdcce2 with the options lr(xtpmg) and exponent
xtdcce2 D1.lny1 L1.D1.lny1 L1.D1.lnx1 L1.D1.lnx2 ///
  L1.D1.lnx3, reportc ///
  cr(_all) cr_lags(3 3 3 3) ///
  lr(L1.lny1 lnx1 lnx2 lnx3) ///
  lr_options(xtpmg) exponent

/* If we want to see the estimates for the individual states, then we include
   the option showindividual.
*/
xtdcce2 D1.lny1 L1.D1.lny1 L1.D1.lnx1 L1.D1.lnx2 ///
   L1.D1.lnx3, reportc cr(_all) cr_lags(1 3 3 3) ///
   lr(L1.lny1 lnx1 lnx2 lnx3) lr_options(ardl)///
   exponent showin

/*  
========================================================
* Additional Analysis Options
========================================================

/* Alternative specifications with different cross-sectional lags 2 cross-sectional lags
xtdcce2 D1.lny1 L1.D1.lny1 L1.D1.lnx1 L1.D1.lnx2 ///
   L1.D1.lnx3, reportc cr(_all) cr_lags(2 2 2 2) ///
   lr(L1.lny1 lnx1 lnx2 lnx3) lr_options(ardl)

4 cross-sectional lags
xtdcce2 D1.lny1 L1.D1.lny1 L1.D1.lnx1 L1.D1.lnx2 ///
   L1.D1.lnx3, reportc cr(_all) cr_lags(4 4 4 4)///
   lr(L1.lny1 lnx1 lnx2 lnx3) lr_options(ardl)
*/
    
clear all

exit

*=======================================================
* END OF CHAPTER 9 CODE
*=======================================================

/* Notes:
 - The script follows the organization and structure of Chapter 8
 - Cross-sectional lags of (3 3 3 3) are used to account for 
   cross-sectional dependence
 - ARDL specification (1 1 1) is used for short-run dynamics
 - ECM framework allows for estimation of both short-run and 
   long-run coefficients
 - MG estimator provides average coefficients across states
 - DCCE estimator accounts for cross-sectional dependence
 - The showindividual option provides state-specific estimates
*/
