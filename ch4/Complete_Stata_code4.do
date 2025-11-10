*================================================================
* Chapter 4 - Creating Datasets and Managing Data
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative Techniques *(2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-
* edition/tree/main/code/ch4
* Author: Marvin A. Titus
* Date: November 10, 2025
*================================================================

* Script tested in Stata 19.5
* Compatible with Stata version 19 or later

*===============================================================================
* 📂 IMPORTANT: Set working directory (customize this for your system)
*===============================================================================
* for example:
cd "C:/Users/YourName/Documents/book-materials/ch4/data"

* ✅ or use a global path to make it easy to update in one place
global ch5data "C:/Users/YourName/Documents/book-materials/ch4/data"
cd "$ch4data"

* Verify working directory
pwd

*================================================================
* SECTION 4.2.1: PRIMARY DATA ENTRY
*================================================================

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

* Save the dataset in Stata 
save "Example 1.0.dta", replace

* To enter additional data using Stata editor
edit

* Save (export) the dataset to a CSV format

export delimited using "Example 1.csv", replace

*----------------------------------------------------------------
* Importing data from CSV file
*----------------------------------------------------------------
clear all
insheet using "Example 1.csv", comma

*----------------------------------------------------------------
* Installing iefieldkit for primary data collection (CAPI)
*----------------------------------------------------------------
* Install the package
cap ssc install iefieldkit, replace

* For information on iefieldkit, visit:
* https://dimewiki.worldbank.org/wiki/Iefieldkit

*================================================================
* SECTION 4.2.2: SECONDARY DATA - CROSS-SECTIONAL DATA
*================================================================

*----------------------------------------------------------------
* Importing data from NCES Digest Table 302.50
* High school graduates enrolled in postsecondary education by
* state, 2012
*----------------------------------------------------------------
clear all

* Method 1: Import from local file (if you downloaded it)
import excel "tabn302_50.xlsx", sheet("reformatted") firstrow clear

* Method 2: Download from GitHub and import
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/tabn302_50.xlsx" ///
     "tabn302_50.xlsx", replace
import excel "tabn302_50.xlsx", sheet("reformatted") firstrow clear

* View the data in the editor
browse

*----------------------------------------------------------------
* Adding FIPS codes and state abbreviations
*----------------------------------------------------------------
* Install statastates package
cap ssc install statastates, replace

* Add FIPS codes and state abbreviations
* statastates, name(State)

* Drop the merge variable
drop _merge

* Reorder variables to put state identifiers at the beginning
* order state_abbrev state_fips, before(State)

*----------------------------------------------------------------
* Saving with descriptive name and viewing structure
*----------------------------------------------------------------
* Save the dataset
save "US high school graduates in 2012 enrolled in PSE, by state.dta", replace

* View dataset structure
describe

*----------------------------------------------------------------
* Adding variable labels
*----------------------------------------------------------------
lab var Stateid "Stateid"
lab var state_abbrev "State abbreviation"
lab var state_fips "FIPS code"
lab var State "State name"
lab var total "Total number of graduates from HS located in the state"
lab var public "Number of graduates from public HS located in the state"
lab var private "Number of graduates from private HS located in the state"

* Note: Labels cannot exceed 80 characters, so we shorten as
* needed
lab var anystate "Number of 1st-time freshmen graduating from HS enrolled in any state"
lab var homestate "Number of 1st-time freshmen graduating from HS enrolled in home state"
lab var anyrate "Estimated rate of HS graduates going to college in any state"
lab var homerate "Estimated rate of HS graduates going to college in home state"

* View updated structure with labels
describe

* Re-save with labels
save "US high school graduates in 2012 enrolled in PSE, by state.dta", replace

*================================================================
* SECTION 4.2.2: SECONDARY DATA - TIME SERIES DATA
*================================================================

*----------------------------------------------------------------
* Creating time series dataset from NCES Digest Table * 302.10
* Percent of HS graduates enrolled in college, 1960-2016
*----------------------------------------------------------------
clear all

* Import data (assuming column H data was copied to Stata)
* For this example, assume var1 contains the percentage data

* Drop missing observations (from blank rows in Excel)
drop if var1==.

* Rename the variable
rename var1 totalpct

* Create year variable (1960-2016)
gen year = 1959 + _n

* Move year to beginning of dataset
order year, first

* Declare as time series
tsset year, yearly

* Save the time series dataset
save "Percent of US high school graduates in PSE, 1960 to 2016.dta"
* Note: On GitHub, this file is named Example_4_2_2_TS.dta and found in the 
* /data/ch4/ repository. 

*----------------------------------------------------------------
* Alternative: Download reformatted data from GitHub
*----------------------------------------------------------------
clear all
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/tabn302_10.xlsx" ///
     "tabn302_10.xlsx", replace
import excel "tabn302_10.xlsx", sheet("reformatted") firstrow clear

* Declare as time series
tsset year, yearly

* Save the time series dataset
save "Percent of US high school graduates in PSE, 1960 to 2016.dta"
* Note: On GitHub, this file is named Example_4_2_2_TS.dta and found in the 
* /data/ch4/ repository. 

*===============================================================================
* SECTION 4.2.2: SECONDARY DATA - PANEL DATA
*===============================================================================

*----------------------------------------------------------------
* Creating panel dataset from NCES Digest Table 304.70
* Undergraduate enrollment by state, 2000-2017
*----------------------------------------------------------------
clear all

* Method 1: Import from local file
cd "C:/Users/YourName/Documents/book-materials/ch4/data"
import excel "College enrollment data.xls", firstrow

* Method 2: Download from GitHub
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/tabn304_70.xlsx" ///
     "tabn304_70.xlsx", replace
import excel "tabn304_70.xlsx", sheet("Undergrads") firstrow clear

* Save in wide format
save "Undergraduate enrollment data - Wide.dta", replace

*----------------------------------------------------------------
* Converting panel data from wide to long format
*----------------------------------------------------------------
* Install sreshape if not already installed
* Type: search sreshape, all
* Click on dm0090 and install

* Reshape from wide to long
sreshape long Ugrad, i(id) j(year)

* Declare as panel dataset
xtset id year, yearly

* View panel structure
xtdes

* Save in long format
save "Undergraduate enrollment data - Long.dta", replace

*----------------------------------------------------------------
* Creating and reshaping high school graduation
* dataset
*----------------------------------------------------------------
clear all
cd "C:/Users/YourName/Documents/book-materials/ch4/data"
import excel "Example 4.xls", sheet("HSGrad") firstrow

* Save in wide format
save "HSGrad - Wide.dta", replace

* Reshape to long format
sreshape long HSGrad, i(id) j(year)

* Declare as panel
xtset id year, yearly

* Save in long format
save "HSGrad - Long.dta", replace

*================================================================
* SECTION 4.2.2: JOINING PANEL DATASETS
*================================================================

*----------------------------------------------------------------
* Joining two panel datasets
*----------------------------------------------------------------
* Load the first dataset
use "HSGrad - Long.dta", clear

* Join with another dataset (assuming First-Time data exists)
* joinby id year using "First-Time - Long.dta", unmatched(none)

* Browse the merged data
browse

*----------------------------------------------------------------
* Joining multiple panel datasets
*----------------------------------------------------------------
* Load undergraduate enrollment data
use "Undergraduate enrollment data - Long.dta", clear

* Join with need-based financial aid data
joinby id year using "Undergraduate state financial aid - need"

* Join with merit-based financial aid data
joinby id year using "Undergraduate state financial aid - merit"

* Declare as panel
xtset id year, yearly

* View panel structure
xtdes

* Save the combined dataset
save "Example4_2_2_Panel", replace

* Browse the final dataset
browse

*================================================================
* ADDITIONAL USEFUL COMMANDS
*================================================================

*----------------------------------------------------------------
* Data inspection and verification
*----------------------------------------------------------------
* Summarize all variables
summarize

* Check for missing values
misstable summarize

* List first 10 observations
list in 1/10

* Get detailed variable information
codebook

* Display variable labels
describe, short

*----------------------------------------------------------------
* Panel data diagnostics
*----------------------------------------------------------------
* Check if panel is balanced
xtdes

* Summarize panel characteristics
xtsum

* Display panel data structure
xtline Ugrad if id<=5, overlay legend(off)

*----------------------------------------------------------------
* Data management
*----------------------------------------------------------------
* Keep only specific variables
* keep id year State Ugrad HSGrad

* Keep only specific observations
* keep if year >= 2010

* Create lagged variables (for time series or panel data)
* gen Ugrad_lag1 = L.Ugrad

* Create first differences
* gen Ugrad_diff = D.Ugrad

*----------------------------------------------------------------
* Exporting data
*----------------------------------------------------------------
* Export to CSV
* export delimited using "output_data.csv", replace

* Export to Excel
export excel using "output_data.xlsx", replace firstrow(variables)

*================================================================
* NOTES ON BEST PRACTICES
*================================================================
/*
1. ALWAYS save your data with descriptive file names that include:
   - Content description
   - Time period (if applicable)
   - Format indicator (Wide/Long for panel data)
   
2. ALWAYS add variable labels to make your dataset self-documenting:
   - Use lab var command for each variable
   - Keep labels under 80 characters
   - Be descriptive but concise
   
3. For PANEL DATA:
   - Convert to long format before analysis
   - Declare panel structure with xtset
   - Check balance with xtdes
   - Aim for strongly balanced panels
   
4. For TIME SERIES:
   - Declare time variable with tsset
   - Check for gaps in time periods
   - Fill gaps if needed with tsfill
   
5. FILE PATHS:
   - Use forward slashes (/) for cross-platform compatibility
   - Or use compound quotes: `"C:\Users\..."'
   - Set working directory at start of do-file
   - Use relative paths when possible
   
6. DATA CLEANING:
   - Always check for missing values
   - Remove non-numeric characters from Excel before importing
   - Verify variable types after import
   - Drop unnecessary variables to save memory
   
7. VERSION CONTROL:
   - Save intermediate datasets with version numbers
   - Keep original data files unchanged
   - Document all data transformations in do-files
   
8. GITHUB INTEGRATION:
   - Use copy command to download data directly from GitHub
   - This ensures reproducibility
   - Others can run your code without manually downloading files
*/

*================================================================
* END OF CHAPTER 4 CODE
*==============================================================================

