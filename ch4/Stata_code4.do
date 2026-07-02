*========================================================================
* Chapter 4 - Creating Datasets and Managing Data
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative Techniques
* (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch4
* Author: Marvin A. Titus
* Date: November 16, 2025
* NOTE: Code development was assisted by Claude (Anthropic). The author
* provided specifications and reviewed, tested, and validated all code.
*========================================================================
* Script tested in Stata 19.5
* Compatible with Stata version 19 or later

*========================================================================
* IMPORTANT: Set working directory (customize this for your system)
*========================================================================

* Use a global path to make it easy to update in one place
* global ch4data "C:/Users/YourName/Documents/book-materials/ch4/data"
* cd "$ch4data"

*========================================================================
* OUTPUT DIRECTORIES AND LOG FILE
* Paths switch automatically based on the OS username (c(username)).
* The instructor's personal paths are used when username == "marvi";
* all other users get the generic relative paths.
*========================================================================

* Close any stale log silently, then open a fresh one
capture log close

if c(username) == "marvi" {
    global graphs_dir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 4/Output/graphs"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 4/Output"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 4/Output/graphs"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 4/Output/logs"
    log using ///
        "C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 4\Output\logs\Chapter4_Stata_output.log", ///
        replace text
}
else {
    global graphs_dir "Output/graphs"
    capture mkdir "Output"
    capture mkdir "Output/graphs"
    capture mkdir "Output/logs"
    log using "Output/logs/Chapter4_Stata_output.log", replace text
}

di "Chapter 4 log opened: " c(current_date) " " c(current_time)
di "Graphs directory: $graphs_dir"

clear all
set more off
version 19
set scheme s2mono        // Monochrome scheme for Springer B&W print
set graphics on          // Ensure graph window is active throughout

*========================================================================
* PACKAGE INSTALLATIONS (run once; comment out thereafter)
*========================================================================
ssc install statastates,  replace   // FIPS codes / state abbreviations (4.2.2)
* ssc install iefieldkit, replace   // Optional: primary data collection (CAPI)
* Reshape long uses the built-in `sreshape`; if unavailable on your system,
* run: search sreshape, all  -- then install package dm0090.

*========================================================================
* Section 4.2.1: Primary Data Entry
*========================================================================

*----------------------------------------------------------------
* Simple data entry using input command
*----------------------------------------------------------------

clear all

input variable_x variable_y variable_z
31 57 18
25 68 12
35 60 13
38 59 17
30 59 15
end

* Display the data
list

* Save the dataset
save "Example_1_0.dta", replace

* To enter additional data using Stata editor
* edit

* Export to CSV format
export delimited using "Example_1.csv", replace

*----------------------------------------------------------------
* Importing data from CSV file
*----------------------------------------------------------------

clear all

insheet using "Example_1.csv", comma

*----------------------------------------------------------------
* 🔹 Installing iefieldkit for primary data collection (CAPI)
*----------------------------------------------------------------

* Install the package (run once)
* ssc install iefieldkit, replace

* For information on iefieldkit, visit:
* https://github.com/worldbank/iefieldkit
* https://dimewiki.worldbank.org/wiki/Iefieldkit

*========================================================================
* Section 4.2.2: Secondary Data - Cross-Sectional Data
*========================================================================

*----------------------------------------------------------------
* Importing data from NCES Digest Table 302.50
* High school graduates enrolled in postsecondary education by state, 2012
*----------------------------------------------------------------

clear all

* Download from GitHub and import
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/tabn302_50.xlsx" ///
     "tabn302_50.xlsx", replace

import excel "tabn302_50.xlsx", sheet("reformatted") firstrow clear

* View the data in browse mode
browse

*----------------------------------------------------------------
* 🔹 Adding FIPS codes and state abbreviations
*----------------------------------------------------------------

* statastates installed in the package block above
* Add FIPS codes and state abbreviations
statastates, name(State)

* Drop the merge variable created by statastates
drop _merge

* Reorder variables to put state identifiers at the beginning
order state_abbrev state_fips, before(State)

*----------------------------------------------------------------
* Saving with descriptive name and viewing structure
*----------------------------------------------------------------

* Save the dataset
save "US high school graduates in 2012 enrolled in PSE, by state.dta", replace

* View dataset structure
describe

*----------------------------------------------------------------
* 🔹 Adding variable labels
*----------------------------------------------------------------

lab var Stateid "State ID number"
lab var state_abbrev "State abbreviation"
lab var state_fips "FIPS code"
lab var State "State name"
lab var total "Total number of graduates from HS located in the state"
lab var public "Number of graduates from public HS located in the state"
lab var private "Number of graduates from private HS located in the state"

* Note: Labels cannot exceed 80 characters, so we shorten as needed
lab var anystate "Number of 1st-time freshmen graduating from HS enrolled in any state"
lab var homestate "Number of 1st-time freshmen graduating from HS enrolled in home state"
lab var anyrate "Estimated rate of HS graduates going to college in any state"
lab var homerate "Estimated rate of HS graduates going to college in home state"

* View updated structure with labels
describe

* Re-save with labels
save "US high school graduates in 2012 enrolled in PSE, by state.dta", replace

clear all

*========================================================================
* Section 4.2.2: Secondary Data - Time Series Data
*========================================================================

*----------------------------------------------------------------
* Creating time series dataset from NCES Digest Table 302.10
* Percent of HS graduates enrolled in college, 1960-2016
*----------------------------------------------------------------

clear all

* Download reformatted data from GitHub
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/tabn302_10.xlsx" ///
     "tabn302_10.xlsx", replace

import excel "tabn302_10.xlsx", sheet("reformatted") firstrow clear

* 🔹 Declare as time series
tsset year, yearly

* View time series structure
tsdes

* Save the time series dataset
save "Percent of US high school graduates in PSE, 1960 to 2016.dta", replace

* Note: On GitHub, this file is named Example_4_2_2_TS.dta and found in 
* the /data/ch4/ repository

clear all

*========================================================================
* Section 4.2.2: Secondary Data - Panel Data (Wide Format)
*========================================================================

*----------------------------------------------------------------
* Creating panel dataset from NCES Digest Table 304.70
* Undergraduate enrollment by state, selected years 2000-2017
*----------------------------------------------------------------

clear all

* Download from GitHub
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/tabn304_70.xlsx" ///
     "tabn304_70.xlsx", replace

import excel "tabn304_70.xlsx", sheet("Undergrads") firstrow clear

* View the wide format data
describe
browse

* Save in wide format
save "Undergraduate enrollment data - Wide.dta", replace

*----------------------------------------------------------------
* 🔹 Converting panel data from wide to long format
*----------------------------------------------------------------

* Note: Install sreshape if not already installed
* To install: search sreshape, all
* Click on dm0090 and install

* Reshape from wide to long
sreshape long Ugrad, i(id) j(year)

* View the results
describe

* 🔹 Declare as panel dataset
xtset id year, yearly

* View panel structure
xtdes

* Save in long format
save "Undergraduate enrollment data - Long.dta", replace

clear all

*========================================================================
* Section 4.2.2: Creating Additional Panel Variables
*========================================================================

*----------------------------------------------------------------
* Creating and reshaping high school graduation dataset
* Data from NCES Digest Table 219.20
*----------------------------------------------------------------

clear all

* Download high school graduation data from GitHub
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/tabn219_20.xlsx" ///
     "tabn219_20.xlsx", replace

import excel "tabn219_20.xlsx", sheet("HSGrad") firstrow clear

* Save in wide format
save "HSGrad - Wide.dta", replace

* Reshape to long format
sreshape long HSGrad, i(id) j(year)

* Declare as panel
xtset id year, yearly

* Save in long format
save "HSGrad - Long.dta", replace

clear all

*========================================================================
* Section 4.2.2: Joining Panel Datasets
*========================================================================

*----------------------------------------------------------------
* 🔹 Joining two panel datasets
*----------------------------------------------------------------

* Load the high school graduation dataset
use "HSGrad - Long.dta", clear

* Join with first-time enrollment data (if available)
* Note: This assumes "First-Time - Long.dta" exists in your directory
* If the file exists, uncomment the line below:
* joinby id year using "First-Time - Long.dta", unmatched(none)

* View panel structure
xtdes

* Save joined dataset
* save "HSGrad and FirstTime - Long.dta", replace

*----------------------------------------------------------------
* 🔹 Joining multiple panel datasets
*----------------------------------------------------------------

* Load undergraduate enrollment data
use "Undergraduate enrollment data - Long.dta", clear

* Join with need-based financial aid data
* Note: These files must exist in your working directory
* Download from GitHub repository /data/ch4/ if needed

joinby id year using "Undergraduate state financial aid - need"

* Join with merit-based financial aid data
joinby id year using "Undergraduate state financial aid - merit"

* Declare as panel
xtset id year, yearly

* View panel structure
xtdes

* Save the combined dataset
save "Example_4_2_2_Panel.dta", replace

* Browse the final dataset
browse

clear all

*========================================================================
* Additional Data Management and Panel Data Commands
*========================================================================

*----------------------------------------------------------------
* Loading and examining the panel dataset
*----------------------------------------------------------------

use "Example_4_2_2_Panel.dta", clear

* View dataset structure
describe

* Summarize all variables
summarize

* Detailed summary statistics
summarize, detail

* Check for missing values
misstable summarize

* Display first 10 observations
list in 1/10

*----------------------------------------------------------------
* Panel data diagnostics
*----------------------------------------------------------------

* Check panel structure
xtdes

* Panel summary statistics
xtsum

* Check balance
* The dataset should be strongly balanced with gaps in years

*----------------------------------------------------------------
* Creating lagged and differenced variables
*----------------------------------------------------------------

* Create lagged variables (requires panel to be declared)
gen Ugrad_lag1 = L.Ugrad
lab var Ugrad_lag1 "Undergraduate enrollment (t-1)"

* Create first differences
gen Ugrad_diff = D.Ugrad
lab var Ugrad_diff "Change in undergraduate enrollment"

* View the new variables
list id year Ugrad Ugrad_lag1 Ugrad_diff in 1/20

*----------------------------------------------------------------
* Creating per-student variables
*----------------------------------------------------------------

* Create need-based aid per student
gen need_per_student = need / Ugrad
lab var need_per_student "Need-based aid per undergraduate student"

* Create merit-based aid per student
gen merit_per_student = merit / Ugrad
lab var merit_per_student "Merit-based aid per undergraduate student"

* Summarize new variables
summarize need_per_student merit_per_student

*----------------------------------------------------------------
* Subsetting data
*----------------------------------------------------------------

* Keep only specific years
* keep if year >= 2010

* Keep only specific states (example: Northeast states)
* keep if inlist(state_fips, 9, 23, 25, 33, 44, 50, 34, 36, 42)

*----------------------------------------------------------------
* Exporting data
*----------------------------------------------------------------

* Export to CSV
export delimited using "Example_4_2_2_Panel.csv", replace

* Export to Excel
export excel using "Example_4_2_2_Panel.xlsx", replace firstrow(variables)

*----------------------------------------------------------------
* Saving final dataset with all modifications
*----------------------------------------------------------------

save "Example_4_2_2_Panel.dta", replace

clear all

*========================================================================
* Additional Useful Panel Data Examples
*========================================================================

*----------------------------------------------------------------
* Working with the time series dataset
*----------------------------------------------------------------

use "Percent of US high school graduates in PSE, 1960 to 2016.dta", clear

* Note: On GitHub, this file is named Example_4_2_2_TS.dta

* Verify time series declaration
tsdes

* Create time trend variable
gen trend = _n
lab var trend "Time trend (1 to 57)"

* Create lagged variable
gen totalpct_lag1 = L.totalpct
lab var totalpct_lag1 "Total percent enrolled (t-1)"

* Create first difference
gen totalpct_diff = D.totalpct
lab var totalpct_diff "Change in total percent enrolled"

* Summary statistics
summarize

* Save with new variables
save "Example_4_2_2_TS.dta", replace

clear all

*========================================================================
* Best Practices Summary
*========================================================================

/*
KEY RECOMMENDATIONS FOR DATASET CREATION AND MANAGEMENT:

1. FILE NAMING:
   - Use descriptive names that indicate content and format
   - Include "Wide" or "Long" suffix for panel data
   - Example: "Undergraduate enrollment data - Long.dta"

2. VARIABLE LABELS:
   - Always add labels using lab var command
   - Keep labels under 80 characters
   - Be descriptive but concise

3. PANEL DATA:
   - Convert to long format before analysis (use sreshape)
   - Always declare panel structure with xtset
   - Check balance with xtdes
   - Aim for strongly balanced panels

4. TIME SERIES:
   - Declare with tsset command
   - Check for gaps with tsdes
   - Use tsfill if gaps need to be filled

5. FILE PATHS:
   - Use forward slashes (/) for cross-platform compatibility
   - Set working directory at start of do-file
   - Use global macros for frequently used paths

6. DATA CLEANING:
   - Check for missing values with misstable
   - Remove non-numeric characters from Excel before importing
   - Verify variable types after import with describe
   - Use compress to optimize storage

7. GITHUB INTEGRATION:
   - Use copy command to download directly from GitHub
   - Ensures reproducibility
   - Others can run your code without manual downloads

8. DOCUMENTATION:
   - Comment your code extensively
   - Save intermediate datasets with version indicators
   - Keep original data files unchanged
   - Document all transformations

9. VERIFICATION:
   - Always use describe, summarize, and browse after import
   - Check panel structure with xtdes
   - Verify time series with tsdes
   - List first few observations to confirm correctness

10. JOINING DATASETS:
    - Use joinby for panel data with matching id and year
    - Specify unmatched(none) if you want only matched obs
    - Always re-declare panel structure after joining
    - Use xtdes to verify the merged panel structure
*/

*========================================================================
* END OF CHAPTER 4 CODE
*========================================================================

clear all

capture window manage close editor

log close

exit
