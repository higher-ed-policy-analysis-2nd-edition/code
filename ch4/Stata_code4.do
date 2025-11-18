*================================================================
* Chapter 4 - Creating Datasets and Managing Data
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative Techniques
* (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-
* edition/tree/main/code/ch4
* Author: Marvin A. Titus
* Date: November 17, 2025
*================================================================
* Script tested in Stata 19.5
* Compatible with Stata version 19 or later
*===============================================================================
* IMPORTANT: Set working directory (customize this for your system)
*===============================================================================
/* Use a global path to make it easy to update in one place
   global ch4data "C:/Users/YourName/Documents/book-materials/ch4/data"
   cd "$ch4data" */
*===============================================================================
* Section 4.2.1: Primary Data
*===============================================================================
* Example 4.2.1: Creating a dataset using the input command
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
* To edit data using Stata's data editor
edit
* Save the dataset or export to a csv file
* save "Example_4_2_1.dta", replace
* export delimited using Example_4_2_1.csv

* Alternative: Import data from CSV file
* insheet using "Example_4_2_1.csv", comma
*===============================================================================
* Section 4.2.2: Secondary Data - Cross-Sectional Dataset
*===============================================================================
* Example: Creating a cross-sectional dataset from NCES Digest Table
* Data source: NCES Digest of Education Statistics, Table 302.50
clear all
* Download the reformatted Excel file from GitHub
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/tabn302_50.xlsx" ///
     "tabn302_50.xlsx", replace
* Import the Excel file
import excel "tabn302_50.xlsx", sheet("reformatted") firstrow clear
* View the data structure
describe
* Add FIPS codes and state abbreviations using statastates
* Install statastates if needed (run only once):
ssc install statastates, replace
statastates, name(State)
* Drop the merge variable
drop _merge
* Reorder variables to place state identifiers at the front
order state_abbrev state_fips, before(State)
* Add variable labels
lab var stateid "State id"
lab var state_abbrev "State abbreviation"
lab var state_fips "FIPS code"
lab var State "State name"
lab var Total "Total graduates from HS located in the state"
lab var Public "Public graduates from HS located in the state"
lab var Private "Private graduates from HS located in the state"
lab var anystate "1st-time freshmen graduating from HS enrolled in any state"
lab var homestate "1st-time freshmen graduating from HS enrolled in home state"
lab var anyrate "Percent of HS completers enrolled in PSE in any state"
lab var homerate "Percent of HS completers enrolled in PSE in home state"
* Verify the labels
describe
* Save the dataset with a descriptive name
* save "US high school graduates in 2012 enrolled in PSE, by state.dta", replace
*===============================================================================
* Section 4.2.2 (continued): Time-Series Dataset
*===============================================================================
* Example: Creating a time-series dataset (1960-2016)
* Data source: NCES Digest of Education Statistics, Table 302.10
clear all

/* If the Excel file has been downloaded from NCES, then it has to be refomatted.
   That involves creating and naming a worksheet within the Excel workbook with
   reformatted data that includes the following: 
   1. deleting rows with missing values. 
   2. renaming variables using one-word names
   3. creating a year variable (1960-2016)
   4. creating an id variable
   5. making sure there are no trailing dots in the column with state names
   (See Chapter 4 for a full expanation). */

* If the Excel file is accessed from the GitHub /data/ch4/ repository, then we
* have to 
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/tabn302_10.xlsx" ///
     "tabn302_10.xlsx", replace
* Import the reformatted worksheet
import excel "tabn302_10.xlsx", sheet("reformatted") firstrow clear

* Declare the dataset to be a time-series (TS)
tsset year

* Add variable labels
lab var year "Year"
lab var totalpct "Percent of HS graduates enrolled in PSE"
* Save the time-series dataset
* save "Percent of US high school graduates in PSE, 1960 to 2016.dta", replace
* Alternative: Save with abbreviated name from GitHub
* save "Example_4_2_2_TS.dta", replace // save in working directory
*===============================================================================
* Section 4.2.2 (continued): Panel Dataset (Cross-Sectional Time-Series)
*===============================================================================
* Example: Creating a panel dataset of undergraduate enrollment by state
* Data source: NCES Digest of Education Statistics, Table 304.70
clear all
* Download the Excel file from GitHub
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/tabn304_70.xlsx" ///
     "tabn304_70.xlsx", replace
* Import the Undergrads worksheet
import excel "tabn304_70.xlsx", sheet("HSGrad") firstrow clear
* Save the dataset in wide format
* save "Undergraduate enrollment data - Wide.dta", replace // save to working directory

* Reshape from wide to long format
reshape long HSGrad, i(id) j(year)
* Declare the dataset as panel data
xtset id year, yearly
* Save the dataset in long format
* save "Undergraduate enrollment data - Long.dta", replace // save to working directory
* View the panel data structure
xtdescribe
*===============================================================================
* Section 4.2.2 (continued): Merging Multiple Variables into Panel Dataset
*===============================================================================
* Example: Adding high school graduates data to the panel
clear all
* Download and import the HSGrad worksheet
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/tabn304_70.xlsx" ///
     "tabn304_70.xlsx", replace
import excel "tabn304_70.xlsx", sheet("HSGrad") firstrow clear
* Save in wide format
* save "HSGrad - Wide.dta", replace // save to working directory
* Reshape to long format and declare as panel
reshape long HSGrad, i(State) j(year)
xtset id year, yearly
* save "HSGrad - Long.dta", replace // save to working directory
* Merge multiple variables using joinby
/* Download the following datasets from the GitHub /data/ch4/ repository to
   to working directory:
   "Undergraduate state financial aid - need.dta"
   "Undergraduate state financial aid - need.dta" 
   "Undergraduate enrollment data - Long.dta" 
   */
use "Undergraduate enrollment data - Long.dta", clear
* Join high school graduates data
joinby id year using "HSGrad - Long"
* Join need-based financial aid data
* download "Undergraduate state financial aid - need".dta" in working directory 
joinby id year using "Undergraduate state financial aid - need"
* Join merit-based financial aid data
* download "Undergraduate state financial aid - merit" in working directory
joinby id year using "Undergraduate state financial aid - merit"
* Examine the merged panel dataset structure
xtdescribe
* View the first few observations
list in 1/10
* Save the complete panel dataset
* save "Complete_Panel_Dataset.dta", replace // save to working directory
*===============================================================================
* End of Chapter 4 Code
*===============================================================================
/*
Note: All datasets referenced in this code are available in the book's data repository at: https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch4
For detailed documentation and additional examples, please refer to the README.md file in the code repository. */
