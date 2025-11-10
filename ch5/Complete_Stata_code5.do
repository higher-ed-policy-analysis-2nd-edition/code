*================================================================
* Chapter 5 - Getting to Know Thy Data
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative Techniques *(2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-
* edition/tree/main/code/ch5
*================================================================

* Script tested in Stata 19.5
* Compatible with Stata version 19 or later

*===============================================================================
* 📂 IMPORTANT: Set working directory (customize this for your system)
*===============================================================================

* ✅ Use a global path to make it easy to update in one place
global ch5data "C:/Users/YourName/Documents/book-materials/ch5/data"
cd "$ch5data"

*===============================================================================
* 📊 Section 5.2: Getting to Know the Structure of Our Datasets
*===============================================================================

* 🔹 Use time series data from Chapter 4

use "Percent of US high school graduates in PSE, 1960 to 2016.dta", clear
describe
compress
describe

* 🔹 Open panel dataset
use "Example_5_0.dta", clear
compress
describe

* 🔹 Recast id variable to integer
recast int id
describe
save "Example_5_0.dta", replace

* 🔹 Load a large dataset (HSLS:09)
set maxvar 60000
use "Public_use_HSLS.dta", clear
describe, short
memory
compress

clear all

*===============================================================================
* 📈 Section 5.2 (continued): SHEEO Finance Data Example
*===============================================================================

* 🔹 Download and import Excel file from GitHub
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_1.xlsx" ///
     "Example_5_1.xlsx", replace

import excel "Example_5_1.xlsx", sheet("reformatted") firstrow clear

drop if FY < 2010
list if FY == 2010
drop if State == "U.S."
drop if State == "D.C."

* 🔹 Create state identifiers
* Install statastates if needed:
cap ssc install statastates
statastates, name(State) nogenerate

* Alternative method
* egen stateid = group(State)

compress
xtset state_fips FY, yearly
save "Example_5_2.dta", replace

clear all

*===============================================================================
* 🔎 Section 5.3: Getting to Know Our Data
*===============================================================================

* 📥 Download truncated HSLS:09 dataset
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_3.dta" ///
     "Example_5_3.dta", replace

use "Example_5_3.dta", clear
codebook S3CLGPELL

* 🔄 Recode special missing values
mvdecode _all, mv(-9=.)
save "Example_5_3.dta", replace

*===============================================================================
* ❓ Section 5.4: Missing Data Analysis
*===============================================================================

* 📥 Load dataset with missing data already recoded
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_4_1.dta" ///
     "Example_5_4_1.dta", replace

use "Example_5_4_1.dta", clear

* 🔹 Install and run missing value summaries
cap ssc install mdesc, replace
mdesc

misstable tree
misstable patterns
misstable tree, frequency

*===============================================================================
* 🔄 Section 5.4 (continued): Panel Missing Analysis with xtmis
*===============================================================================

* 📥 Download IPEDS panel dataset
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_4.dta" ///
     "Example_5_4.dta", replace

use "Example_5_4.dta", clear

* 🔹 Install and use xtmis
cap ssc install tomata
cap ssc install xtmis

tostring unitid, generate(unitid_s)
xtmis grantlow, id(unitid_s)

*===============================================================================
* 📊 Section 5.4 (continued): Missing Data by Categorical Variables
*===============================================================================

use "Example_5_4_1.dta", clear

* 🔹 Install missings suite
cap net install dm0085_1.pkg, replace

bysort X1SESQ5 : missings table
bysort X1RACE : missings table

*===============================================================================
* 🧪 Section 5.4.1: Testing for MCAR (Missing Completely at Random)
*===============================================================================

/* 📥 Use full public_use HSLS:09 dataset (2017 Student File), which can be 
 downloaded directly from NCES at 
 https://nces.ed.gov/datalab/onlinecodebook and rename HSLS09.dta.
 Be aware, this is a hugh file. If you don't have Stata/MP or Stata/SE, you may 
 not be able to download the entire file. 
 If you have Stata/MP, set maxvar 60000. If you have Stata/SE, set maxvar 32000.
 Then keep the these variables: STU_ID X1SEX X1RACE X1SES X1SESQ5 X4ATPRLVLA
 S3CLGPELL P1TUITION*/

 keep STU_ID X1SEX X1RACE X1SES X1SESQ5 X4ATPRLVLA S3CLGPELL P1TUITION

/* If you have neither Stata/MP or Stata/SE, download the a truncated version of
 of public_use HSLS:09 dataset (Public_use_HSLS_09_truncated.dta) on GitHub in 
 the repository /ch5/data, or use the copy and import commands.
 


mvdecode _all, mv(-9=.)

* 🔹 Run MCAR tests (using mcartest, if installed)
* cap net install st0318.pkg, replace

* Equal variances
mcartest S3CLGPELL P1TUITION

* Unequal variances
mcartest S3CLGPELL P1TUITION, unequal

* Covariate-dependent MCAR test (CDM)
mcartest S3CLGPELL P1TUITION = i.X1RACE if X1RACE != ., ///
    unequal emoutput nolog

exit
