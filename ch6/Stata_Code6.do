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
* OUTPUT DIRECTORIES AND LOG FILE
* Paths switch automatically based on the OS username (c(username)).
* The instructor's personal paths are used when username == "marvi";
* all other users get the generic relative paths.
*========================================================================

* Close any stale log silently, then open a fresh one
capture log close

if c(username) == "marvi" {
    global graphs_dir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 6/Output/graphs"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 6/Output"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 6/Output/graphs"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 6/Output/logs"
    log using ///
        "C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 6\Output\logs\Chapter6_Stata_output.log", ///
        replace text
}
else {
    global graphs_dir "Output/graphs"
    capture mkdir "Output"
    capture mkdir "Output/graphs"
    capture mkdir "Output/logs"
    log using "Output/logs/Chapter6_Stata_output.log", replace text
}

di "Chapter 6 log opened: " c(current_date) " " c(current_time)
di "Graphs directory: $graphs_dir"

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
* Note: the chapter appendix shows 'ameans public private' (lowercase), but
* Stata variable names are case-sensitive. The Excel column headers are
* 'Public' and 'Private' (capitalised), so uppercase is required here.
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

* Calculate descriptive statistics by state (Fig. 6.1)
tabstat NetTuition FTEStudents, stat(mean median sd min max ///
        cv) labelwidth(30) long format by(State) col(stat) nototal

* Calculate descriptive statistics by fiscal year (Fig. 6.2)
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

* Frequency distribution using original variable (Fig. 6.3)
prop X1RACE

* Tabulate with frequencies and percentages (sorted) (Fig. 6.4)
tab X1RACE, sort

* One-way table with summary statistics (Fig. 6.5)
tab X1RACE, summarize(EarnHr)

* Two-way table showing means by race/ethnicity and sex (Fig. 6.6)
tab X1RACE X1SEX, sum(EarnHr) means

* Alternative: Two-way table using recoded variable (Fig. 6.7)
tabulate RaceEthnic X1SEX, sum(EarnHr) means

* Panel data: Download state-level panel dataset
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch6/Example_6_3.dta" ///
     "Example_6_3.dta", replace

use "Example_6_3.dta", clear

* Declare panel dataset — fips = panel variable; year = time variable
xtset fips year, yearly

* Check panel structure
xtdescribe

* Cross-tabulation for panel data with time-invariant categorical variable
xttab region_compact

* Transition probabilities for time-variant categorical variable
* Shows probability of states changing merit aid policy year-to-year
xttrans ugradmerit

*========================================================================
* Section 6.2.4: Testing Differences in Means Across Groups (ANOVA)
*========================================================================

* Reload HSLS:09 dataset — necessary because Example_6_3.dta was loaded
* above for the panel data section (xttab/xttrans)
use "Example_6_2_3.dta", clear

* Recreate RaceEthnic variable (required after reloading)
gen RaceEthnic = 0
replace RaceEthnic = 1 if X1RACE==2
replace RaceEthnic = 2 if X1RACE==3
replace RaceEthnic = 3 if X1RACE==4 | X1RACE==5
replace RaceEthnic = 4 if X1RACE==6
replace RaceEthnic = 5 if X1RACE==1 | X1RACE==7
replace RaceEthnic = 6 if X1RACE==8

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
* Note: anova is re-run here so that testparm uses the correct stored results
anova EarnHr RaceEthnic##X1SEX
testparm RaceEthnic#X1SEX

*========================================================================
* Section 6.3.1: Graphs—Exploratory Data Analysis (EDA)
*========================================================================

* Load panel dataset
use "Example_6_3.dta", clear

* Create state appropriations per FTE variable
gen stapr_fte = stapr/fte

* --- Fig. 6.8: Histogram of State Appropriations per FTE Student ---
histogram stapr_fte, normal
graph export "$graphs_dir/fig6_8_histogram_stapr_fte_Stata.png", replace width(1200)

* --- Fig. 6.9: Box Chart of State Appropriations per FTE Student ---
graph box stapr_fte
graph export "$graphs_dir/fig6_9_box_stapr_fte_Stata.png", replace width(1200)

* --- Fig. 6.10: Histogram of Membership in Regional Compacts ---
histogram region_compact, discrete addlabels ylabel(,grid) ///
          xlabel(0 1 2 3 4, valuelabel) percent
graph export "$graphs_dir/fig6_10_histogram_region_compact_Stata.png", replace width(1200)

* --- Fig. 6.11: State Appropriations per FTE Student by Regional Compact ---
histogram stapr_fte, by(region_compact)
graph export "$graphs_dir/fig6_11_histogram_stapr_fte_by_region_Stata.png", replace width(1200)

* --- Fig. 6.12: Box Chart of State Appropriations per FTE Student by Regional Compact ---
graph box stapr_fte, by(region_compact)
graph export "$graphs_dir/fig6_12_box_stapr_fte_by_region_Stata.png", replace width(1200)

* Create net tuition per FTE variable
gen netuit_fte = netuit/fte

* --- Fig. 6.13: Scatter Plot of State Appropriations and Net Tuition Revenue per FTE Student ---
graph twoway scatter stapr_fte netuit_fte if year==2016
graph export "$graphs_dir/fig6_13_scatter_2016_Stata.png", replace width(1200)

* --- Fig. 6.14: Scatter Plot with Fitted Regression Line (Method 1) ---
twoway (scatter stapr_fte netuit_fte) (lfit stapr_fte netuit_fte) ///
       if year==2016
graph export "$graphs_dir/fig6_14_scatter_fitted_2016_Stata.png", replace width(1200)

* --- Fig. 6.15: Scatter Plot with Fitted Line and State Labels (Method 2) ---
twoway scatter stapr_fte netuit_fte, mlabel(state) ///
       || lfit stapr_fte netuit_fte || if year==2016
graph export "$graphs_dir/fig6_15_scatter_labels_2016_Stata.png", replace width(1200)

* Install user-written aaplot command (run once)
ssc install aaplot, replace

* --- Fig. 6.16: State Appropriations and Net Tuition per FTE Student, FY 1990 ---
aaplot netuit_fte stapr_fte if year==1990
graph export "$graphs_dir/fig6_16_aaplot_1990_Stata.png", replace width(1200)

* --- Fig. 6.17: State Appropriations and Net Tuition per FTE Student, FY 2016 ---
aaplot netuit_fte stapr_fte if year==2016
graph export "$graphs_dir/fig6_17_aaplot_2016_Stata.png", replace width(1200)

di "All graphs saved to: $graphs_dir"

clear all

* Close log
capture log close

exit

*================================================================
* END OF CHAPTER 6 CODE
*================================================================
