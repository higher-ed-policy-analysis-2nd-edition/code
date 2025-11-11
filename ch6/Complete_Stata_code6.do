*================================================================
* Chapter 6 - Using Descriptive Statistics and Graphs
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-edition/tree/main/code/ch6
* Author: Marvin A. Titus
* Date: November 11, 2025
*================================================================

* Script tested in Stata 19.5
* Compatible with Stata version 19 or later

*===============================================================================
* 📂 IMPORTANT: Set working directory (customize this for your system)
*===============================================================================

* Use a global path to make it easy to update in one place
global ch6data "C:/Users/YourName/Documents/book-materials/ch6/data"
cd "$ch6data"

*===============================================================================
* Section 6.2: Descriptive Statistics
*===============================================================================

*-------------------------------------------------------------------------------
* Section 6.2.1: Measures of Central Tendency
*-------------------------------------------------------------------------------

/* Download or use the copy and import commands to open the dataset
 Example_6_1.dta from the /data/ch6/ repository on GitHub. */ 
 
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch6/Example_6_1.dta" "Example_6_1.dta", replace
use "Example_6_1.dta", clear

* 🔹 Basic descriptive statistics
summarize

* 🔹 Detailed summary statistics
summarize, detail

* 🔹 Mean, median, and mode for specific variables
tabstat enrollment tuition_fees, statistics(mean median) columns(statistics)

* 🔹 Calculate mode for categorical variable
tabulate institution_type, sort

*-------------------------------------------------------------------------------
* Section 6.2.2: Measures of Dispersion
*-------------------------------------------------------------------------------

* 🔹 Standard deviation, variance, range
summarize enrollment tuition_fees, detail

* 🔹 Coefficient of variation
tabstat enrollment tuition_fees, statistics(mean sd cv) columns(statistics)

* 🔹 Interquartile range
summarize enrollment, detail
display "IQR = " r(p75) - r(p25)

*-------------------------------------------------------------------------------
* Section 6.2.3: Distributions
*-------------------------------------------------------------------------------

/* Download the public-use HSLS:09 subset data from GitHub or use the copy 
   command. This dataset contains student demographic and achievement data. */

copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch6/Example_6_2_3.dta" "Example_6_2_3.dta", replace
use "Example_6_2_3.dta", clear

* 🔹 Frequency distribution
tabulate X1RACE, missing

* 🔹 Cross-tabulation by SES quintile
tabulate X1RACE X1SESQ5, row column chi2

* 🔹 Distribution of continuous variable
histogram X1SES, normal

* 🔹 Skewness and kurtosis
summarize X1SES, detail

* Check normality
sktest X1SES

clear all

/* create a new categorical variable (with fewer categories), RaceEthnic, and
   value labels */
   
codebook X1RACE // to look at the variable names, labels, and data 
gen RaceEthnic = 0 // create new variable

* create categories  
replace RaceEthnic = 1 if X1RACE==2
replace RaceEthnic = 2 if X1RACE==3
replace RaceEthnic = 3 if X1RACE==4 | X1RACE==5
replace RaceEthnic = 4 if X1RACE==6
replace RaceEthnic = 5 if X1RACE==1 | X1RACE==7
replace RaceEthnic = 6 if X1RACE==8
lab var RaceEthnic "Race/Ethnicity" // create a label variable

* create a value label
label define RaceEthnic1 1 Asian 2 Black 3 Hispanic 4 Multiracial 5 Other 6 White
label values RaceEthnic RaceEthnic1 // link the new variable to the value labels

/* two-way table to generate a summary statistic (e.g., means) across two 
   categories - EarnHr across RaceEthnic and X1SEX */
tabulate RaceEthnic X1SEX, sum(EarnHr) means //

*-------------------------------------------------------------------------------
* Exploratory Data Analysis (EDA) of Panel Data
*-------------------------------------------------------------------------------

/* We can download or access this data (Example_6_3.dta) from our GitHub 
   repository /data/ch6 or use the copy and import commands. */
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch6/Example_6_3.dta" "Example_6_3.dta", replace

use "Example_6_3.dta", clear

/* Use the xtset commmand with year and time variable and fips 
  (Federal Information Processing Standards) code as the panel variable for the 
   state, we specify that the dataset is structured as a panel. */
  
xtset fips year, yearly  

/* use the xttab command to examine the distribution of data across higher 
  categories (e.g., education academic common markets or regional compacts) */

xttab region_compact  

/* use the xttans command to examine the distribution of time-variant  
  categories (e.g., state-level undergraduate merit aid)  */
  
xttrans ugradmerit
  
*===============================================================================
* Section 6.3: Graphs
*===============================================================================

*-------------------------------------------------------------------------------
* Section 6.3.1: Graphs for Exploratory Data Analysis (EDA)
*-------------------------------------------------------------------------------

/* Load the HSLS:09 dataset for graphical analysis */
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch6/Example_6_3.dta" "Example_6_3.dta", replace
use "Example_6_3.dta", clear

* 🔹 Histogram with normal curve overlay
histogram X1SES, normal ///
    title("Distribution of Socioeconomic Status") ///
    xtitle("SES Composite Score") ///
    ytitle("Density") ///
    name(hist_ses, replace)

* 🔹 Box plot by group
graph box X1SES, over(X1RACE) ///
    title("SES Distribution by Race/Ethnicity") ///
    ytitle("SES Composite Score") ///
    name(box_race, replace)

* 🔹 Scatter plot with fitted line
twoway (scatter X4ATPRLVLA X1SES) ///
       (lfit X4ATPRLVLA X1SES), ///
    title("Achievement vs. Socioeconomic Status") ///
    xtitle("SES Composite Score") ///
    ytitle("Achievement Percentile Level") ///
    legend(order(1 "Observed" 2 "Linear Fit")) ///
    name(scatter_achieve, replace)

* 🔹 Bar graph with error bars
graph bar (mean) X1SES, over(X1RACE) ///
    title("Mean SES by Race/Ethnicity") ///
    ytitle("Mean SES Score") ///
    name(bar_ses, replace)

* 🔹 Multiple histograms by group
histogram X1SES, by(X1SESQ5, total) ///
    normal ///
    title("SES Distribution by Quintile") ///
    name(hist_quintile, replace)

*-------------------------------------------------------------------------------
* Time Series Graphs
*-------------------------------------------------------------------------------

/* Download time series dataset from Chapter 4 repository */
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/Example_4_2_2_TS.dta" "Example_4_2_2_TS.dta", replace
use "Example_4_2_2_TS.dta", clear

* 🔹 Line graph for time series
twoway line pctpse year, ///
    title("Percent of High School Graduates Enrolled in PSE") ///
    xtitle("Year") ///
    ytitle("Percent") ///
    name(line_pse, replace)

* 🔹 Connected scatter plot
twoway connected pctpse year, ///
    title("PSE Enrollment Trend, 1960-2016") ///
    xtitle("Year") ///
    ytitle("Percent Enrolled") ///
    name(connected_pse, replace)

clear all

*-------------------------------------------------------------------------------
* Panel Data Graphs
*-------------------------------------------------------------------------------

/* Load SHEEO state finance panel data from Chapter 5 repository */
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_2.dta" "Example_5_2.dta", replace
use "Example_5_2.dta", clear

* 🔹 Panel line graph for selected states
preserve
keep if inlist(State, "California", "Texas", "New York", "Florida")

xtline state_support, overlay ///
    title("State Support for Higher Education") ///
    subtitle("Selected States, 2010-2020") ///
    ytitle("Support (in millions)") ///
    xtitle("Fiscal Year") ///
    legend(label(1 "CA") label(2 "TX") label(3 "NY") label(4 "FL")) ///
    name(panel_states, replace)
restore

* 🔹 Scatter plot with panel structure
xtscatter state_support FTE_enrollment, ///
    title("State Support vs. Enrollment by State") ///
    name(panel_scatter, replace)

*-------------------------------------------------------------------------------
* Advanced Visualization
*-------------------------------------------------------------------------------

/* Create publication-quality graphs with customization */
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch6/Example_6_4.dta" "Example_6_4.dta", replace
use "Example_6_4.dta", clear

* 🔹 Kernel density plot
kdensity tuition_fees, ///
    normal ///
    title("Distribution of Tuition and Fees") ///
    xtitle("Tuition and Fees (in thousands)") ///
    ytitle("Density") ///
    note("Source: IPEDS 2020-21") ///
    name(kdens_tuition, replace)

* 🔹 Multiple density plots by group
twoway (kdensity tuition_fees if sector==1) ///
       (kdensity tuition_fees if sector==2), ///
    title("Tuition Distribution by Sector") ///
    xtitle("Tuition and Fees") ///
    ytitle("Density") ///
    legend(order(1 "Public" 2 "Private")) ///
    name(kdens_sector, replace)

* 🔹 Q-Q plot for normality check
qnorm tuition_fees, ///
    title("Normal Q-Q Plot: Tuition and Fees") ///
    name(qq_tuition, replace)

* 🔹 Standardized Q-Q plot
pnorm tuition_fees, ///
    title("Standardized Normal Probability Plot") ///
    name(pnorm_tuition, replace)

*===============================================================================
* Exporting Graphs
*===============================================================================

* 🔹 Export graphs to various formats

* Export as PNG (high resolution)
graph export "hist_ses.png", name(hist_ses) replace width(1200) height(800)

* Export as PDF (vector graphics)
graph export "scatter_achieve.pdf", name(scatter_achieve) replace

* Export as EPS (for LaTeX)
graph export "line_pse.eps", name(line_pse) replace

* Export all active graphs
graph dir
foreach g in hist_ses box_race scatter_achieve {
    graph export "`g'.png", name(`g') replace width(1200)
}

*===============================================================================
* Combining Graphs
*===============================================================================

* 🔹 Combine multiple graphs into one figure
graph combine hist_ses box_race scatter_achieve bar_ses, ///
    title("Summary of HSLS:09 Student Characteristics") ///
    rows(2) ///
    name(combined, replace)

graph export "combined_analysis.png", name(combined) replace width(1600)

*===============================================================================
* Graph Schemes
*===============================================================================

* 🔹 Apply different Stata schemes
set scheme s2color  // Default color scheme
set scheme s1mono   // Monochrome scheme
set scheme economist // Economist magazine style

* Reset to default
set scheme s2color

*===============================================================================
* Custom Graph Options
*===============================================================================

* 🔹 Example with extensive customization
histogram enrollment, ///
    width(500) ///
    frequency ///
    addlabels ///
    title("Student Enrollment Distribution", size(medium) color(navy)) ///
    subtitle("Fall 2020", size(small)) ///
    xtitle("Total Enrollment", size(medsmall)) ///
    ytitle("Number of Institutions", size(medsmall)) ///
    note("Data source: IPEDS 2020-21" "Analysis date: 2025", ///
         size(vsmall) position(7)) ///
    graphregion(color(white)) ///
    plotregion(margin(medsmall)) ///
    name(custom_hist, replace)

clear all
	
exit

*================================================================
* END OF CHAPTER 6 CODE
*==============================================================================
