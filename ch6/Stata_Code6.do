*================================================================
* Chapter 6 - Using Descriptive Statistics and Graphs
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative Techniques 
* (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-
* edition/tree/main/code/ch6
* Author: Marvin A. Titus
* Date: November 14, 2025
*================================================================

* Script tested in Stata 19.5
* Compatible with Stata version 19 or later

*========================================================================
* IMPORTANT: Set working directory (customize this for your system)
*========================================================================

* Use a global path to make it easy to update in one place
* global ch6data "C:/Users/YourName/Documents/book-materials/ch6/data"
* cd "$ch6data"

*========================================================================
* Section 6.2.1: Measures of Central Tendency
*========================================================================

* Method 1: Import from local directory (if previously downloaded)
* import excel "tabn302_50.xlsx", sheet("reformatted") firstrow clear

* Method 2: Download from GitHub and import
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/tabn302_50.xlsx" ///
     "tabn302_50.xlsx", replace

import excel "tabn302_50.xlsx", sheet("reformatted") firstrow clear

* Calculate arithmetic, geometric, and harmonic means
ameans Public Private

* Calculate arithmetic mean only
mean Public Private

* Generate detailed summary statistics including median
sum, detail

*========================================================================
* Section 6.2.2: Measures of Dispersion
*========================================================================

* Download SHEEO finance dataset
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch6/Example_6_2_2.dta" ///
     "Example_6_2_2.dta", replace

use "Example_6_2_2.dta", clear

* Calculate coefficient of variation (CV)
tabstat NetTuition FTEStudents, stat(cv)

* Calculate descriptive statistics by state
tabstat NetTuition FTEStudents, stat(mean median sd min max ///
        cv) labelwidth(30) long format by(State) col(stat) nototal

* Calculate descriptive statistics by year
tabstat NetTuition FTEStudents, stat(mean median sd min max ///
        cv) labelwidth(30) long format by(FY) col(stat) nototal

*========================================================================
* Section 6.2.3: Distributions
*========================================================================

* Download HSLS:09 condensed dataset with earnings variable
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch6/Example_6_2_3.dta" ///
     "Example_6_2_3.dta", replace

use "Example_6_2_3.dta", clear

* Examine race/ethnicity variable codebook
codebook X1RACE

* Create recoded race/ethnicity variable
gen RaceEthnic = 0
replace RaceEthnic = 1 if X1RACE==2
replace RaceEthnic = 2 if X1RACE==3
replace RaceEthnic = 3 if X1RACE==4 | X1RACE==5
replace RaceEthnic = 4 if X1RACE==6
replace RaceEthnic = 5 if X1RACE==1 | X1RACE==7
replace RaceEthnic = 6 if X1RACE==8

* Label variable and values
lab var RaceEthnic "Race/Ethnicity"
label define RaceEthnic1 1 "Asian" 2 "Black" 3 "Hispanic" ///
                         4 "Multiracial" 5 "Other" 6 "White"
label values RaceEthnic RaceEthnic1

* Frequency distribution using original variable
prop X1RACE

* Tabulate with frequencies and percentages (sorted)
tab X1RACE, sort

* One-way table with summary statistics
tab X1RACE, summarize(EarnHr)

* Two-way table showing means by race/ethnicity and sex
tab X1RACE X1SEX, sum(EarnHr) means

* Alternative: Two-way table using recoded variable
tabulate RaceEthnic X1SEX, sum(EarnHr) means

* Panel data: Download state-level panel dataset
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch6/Example_6_3.dta" ///
     "Example_6_3.dta", replace

use "Example_6_3.dta", clear

* declare a panel dataset
xtset fips year

* Check panel structure
xtdescribe 

* Cross-tabulation for panel data with time-invariant categorical variable
xttab region_compact

*========================================================================
* Section 6.2.4: Testing Differences in Means Across Groups (ANOVA)
*========================================================================

* Using the HSLS:09 dataset with earnings variable (loaded above)
* If not already loaded:
use "Example_6_2_3.dta", clear

* Create recoded race/ethnicity variable
gen RaceEthnic = 0
replace RaceEthnic = 1 if X1RACE==2
replace RaceEthnic = 2 if X1RACE==3
replace RaceEthnic = 3 if X1RACE==4 | X1RACE==5
replace RaceEthnic = 4 if X1RACE==6
replace RaceEthnic = 5 if X1RACE==1 | X1RACE==7
replace RaceEthnic = 6 if X1RACE==8

* Label variable and values
lab var RaceEthnic "Race/Ethnicity"
label define RaceEthnic1 1 "Asian" 2 "Black" 3 "Hispanic" ///
                         4 "Multiracial" 5 "Other" 6 "White"
label values RaceEthnic RaceEthnic1

* One-way ANOVA: Hourly earnings by race/ethnicity
anova EarnHr RaceEthnic

* Alternative: Using oneway command for detailed output
oneway EarnHr RaceEthnic, tabulate

* Post-hoc pairwise comparisons with Bonferroni correction
pwmean EarnHr, over(RaceEthnic) mcompare(bonferroni) effects

* Two-way ANOVA: Hourly earnings by race/ethnicity and sex
anova EarnHr RaceEthnic##X1SEX

* Test for interaction effect
anova EarnHr RaceEthnic##X1SEX
testparm RaceEthnic#X1SEX

*========================================================================
* Section 6.3.1: Graphs—Exploratory Data Analysis (EDA)
*========================================================================

* Using panel dataset (if not already loaded)
use "Example_6_3.dta", clear

* Create state appropriations per FTE variable
gen stapr_fte = stapr/fte

* Histogram with normal curve overlay
histogram stapr_fte, normal

* Box chart
graph box stapr_fte

* Histogram of categorical variable (regional compact)
histogram region_compact, discrete addlabels ylabel(,grid) ///
          xlabel(0 1 2 3 4, valuelabel) percent

* Histogram by categories
histogram stapr_fte, by(region_compact)

* Box chart by categories
graph box stapr_fte, by(region_compact)

* Create net tuition per FTE variable
gen netuit_fte = netuit/fte

* Scatter plot for a specific year
graph twoway scatter stapr_fte netuit_fte if year==2016

* Scatter plot with fitted regression line (Method 1)
twoway (scatter stapr_fte netuit_fte) (lfit stapr_fte netuit_fte) ///
       if year==2016

* Scatter plot with fitted line and state labels (Method 2)
twoway scatter stapr_fte netuit_fte, mlabel(state) ///
       || lfit stapr_fte netuit_fte || if year==2016

* Install user-written aaplot command (run once)
ssc install aaplot, replace

* Scatter plot with regression line for 1990
aaplot netuit_fte stapr_fte if year==1990

* Scatter plot with regression line for 2016
aaplot netuit_fte stapr_fte if year==2016

clear all

exit

*================================================================
* END OF CHAPTER 6 CODE
*================================================================

