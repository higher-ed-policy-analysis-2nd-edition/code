*================================================================
* Chapter 5 - Getting to Know Thy Data
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-
* edition/tree/main/code/ch5
* Author: Marvin A. Titus
* Date: November 10, 2025
*================================================================

* Script tested in Stata 19.5
* Compatible with Stata version 19 or later

*===============================================================================
* IMPORTANT: Set working directory (customize this for your system)
*===============================================================================

* Use a global path to make it easy to update in one place
* global ch5data "C:/Users/YourName/Documents/book-materials/ch5/data"
* cd "$ch5data"

* Or use the cd command directly:
* cd "C:/Users/YourName/Documents/book-materials/ch5/data"

* Verify your working directory
* pwd

*===============================================================================
* OUTPUT DIRECTORIES AND LOG FILE
*===============================================================================

* Create output directories if they do not already exist
* Adjust paths to match your working directory structure
capture mkdir "output"
capture mkdir "output/logs"
capture mkdir "output/graphs"

* Open log — captures all Results-window output as plain text
* Replace overwrites any previous run; text avoids Stata-only .smcl format
log using "output/logs/Chapter5_output.log", replace text

di "Chapter 5 log opened: " c(current_date) " " c(current_time)

*===============================================================================
* Section 5.2: Getting to Know the Structure of Our Datasets
*===============================================================================

*----------------------------------------------------------------
* Time series dataset: describe and compress
*----------------------------------------------------------------

clear all

/* Download or use the copy and import commands to open the time series
   dataset of the percent of US high school graduates in PSE, 1960 to 2016
   from Chapter 4. On GitHub, this file is named Example_4_2_2_TS.dta
   and is found in the /data/ch4/ repository. */

copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/Example_4_2_2_TS.dta" ///
     "Example_4_2_2_TS.dta", replace

use "Example_4_2_2_TS.dta", clear

* View storage types for all variables
describe

* 🔹 Compress to save memory (converts year from float to int)
compress

describe

* save "Example_4_2_2_TS.dta", replace

*----------------------------------------------------------------
* 🔹 Panel dataset: describe, compress, and recast
*----------------------------------------------------------------

/* Download or use the copy and import commands to open Example_5_0.dta,
   found in the /data/ch5/ repository. */

copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_0.dta" ///
     "Example_5_0.dta", replace

use "Example_5_0.dta", clear

* View storage types — id is stored as float but should be integer
describe

compress

describe

* 🔹 Recast id variable to integer (compress only went to byte; recast forces int)
recast int id

describe

* save "Example_5_0.dta", replace

clear all

*===============================================================================
* Section 5.2 (continued): SHEEO Finance Data Example
*===============================================================================

*----------------------------------------------------------------
* Download and import SHEEO state higher education finance data
*----------------------------------------------------------------

/* The reformatted SHEEO Excel file is available on GitHub in the
   data/ch5/ repository as Example_5_1.xlsx. See footnote 4 in the
   chapter for details on the variables included. */

copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_1.xlsx" ///
     "Example_5_1.xlsx", replace

import excel "Example_5_1.xlsx", sheet("reformatted") firstrow clear

*----------------------------------------------------------------
* Drop pre-recession years and non-state observations
*----------------------------------------------------------------

* Keep only post-Great Recession data (FY 2010 onward)
drop if FY < 2010

* Inspect FY 2010 observations
list if FY == 2010

* Drop U.S. aggregate row and D.C.
drop if State == "U.S."
drop if State == "D.C."

*----------------------------------------------------------------
* 🔹 Create state identifiers using statastates
*----------------------------------------------------------------

* Install statastates if needed (run once)
* ssc install statastates, replace

statastates, name(State) nogenerate

* Alternative: create a numeric state id from state names
* egen stateid = group(State)

compress

*----------------------------------------------------------------
* Declare as panel dataset and save
*----------------------------------------------------------------

* Use state FIPS code as panel identifier; FY as time variable
xtset state_fips FY, yearly

* save "Example_5_2.dta", replace

clear all

*===============================================================================
* Section 5.3: Getting to Know Our Data
*===============================================================================

*----------------------------------------------------------------
* Loading the HSLS:09 public-use student dataset
*----------------------------------------------------------------

/* Use the public-use HSLS:09 dataset (2017 Student File), which can be
   downloaded directly from NCES at https://nces.ed.gov/datalab/onlinecodebook.
   Note: this is a very large file.
     - Stata/MP: set maxvar 60000
     - Stata/SE: set maxvar 32000
   Then keep: STU_ID X1SEX X1RACE X1SES X1SESQ5 X4ATPRLVLA S3CLGPELL P1TUITION
   Alternatively, copy and import the truncated version from GitHub (below). */

clear all

copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Public_use_HSLS_09_truncated.dta" ///
     "Public_use_HSLS_09_truncated.dta", replace

use "Public_use_HSLS_09_truncated.dta", clear

keep STU_ID X1SEX X1RACE X1SES X1SESQ5 X4ATPRLVLA S3CLGPELL P1TUITION

/* If you have neither Stata/MP nor Stata/SE, download the pre-truncated
   version (Example_5_3.dta) from GitHub using the copy and import commands. */

copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_3.dta" ///
     "Example_5_3.dta", replace

use "Example_5_3.dta", clear

*----------------------------------------------------------------
* 🔹 Inspect missing data coding with codebook
*----------------------------------------------------------------

* Examine how NCES codes missing values in S3CLGPELL (-9 = Missing)
codebook S3CLGPELL

*----------------------------------------------------------------
* Recode NCES missing codes to Stata system missing (.)
*----------------------------------------------------------------

* mvdecode replaces -9 (and other special codes) with Stata's system missing
mvdecode _all, mv(-9=.)

* Save recoded file
* save "Example_5_4.dta", replace

*===============================================================================
* Section 5.4: Missing Data Analysis
*===============================================================================

*----------------------------------------------------------------
* 🔹 Tabulate missing values with mdesc
*----------------------------------------------------------------

clear all

/* Load dataset with missing values already recoded from Section 5.3.
   Available on GitHub in the /data/ch5/ repository. */

copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_4_1.dta" ///
     "Example_5_4_1.dta", replace

use "Example_5_4_1.dta", clear

* Install mdesc from SSC (run once)
cap ssc install mdesc, replace

* Tabulate number, total, and percent missing for each variable
mdesc

*----------------------------------------------------------------
* Explore missingness patterns with misstable
*----------------------------------------------------------------

* Nested pattern of missing values (percent)
misstable tree

* Cross-tabulated missing-value patterns (1 = complete)
misstable patterns

* Nested pattern of missing values (frequency counts)
misstable tree, frequency

*===============================================================================
* Section 5.4 (continued): Missing Data by Categorical Variables
*===============================================================================

*----------------------------------------------------------------
* 🔹 Examine missing data patterns by subgroup using missings
*----------------------------------------------------------------

use "Example_5_4_1.dta", clear

/* Install the most recent version of missings from the Stata Journal.
   Run this line only once. */
* net install dm0085_1.pkg, replace

* Missingness by SES quintile
bysort X1SESQ5 : missings table

* Missingness by race-ethnicity
bysort X1RACE : missings table

*===============================================================================
* Section 5.4 (continued): Panel Missing Analysis with xtmis (Legacy)
*===============================================================================

*----------------------------------------------------------------
* xtmis: frequency of missing values by panel unit
*----------------------------------------------------------------

/* Note: xtmis (Nguyen 2008) reports missing observations by group for
   xt data. It has been superseded by xtmispanel (Roudane 2026), demonstrated
   in Section 5.4.2 below, which provides more comprehensive panel-aware
   detection, mechanism testing, and visualization. xtmis is retained here
   for backward compatibility and comparison purposes. */

* Download IPEDS panel dataset
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_4.dta" ///
     "Example_5_4.dta", replace

use "Example_5_4.dta", clear

* Install dependencies (run once)
cap ssc install tomata
cap ssc install xtmis

* xtmis requires a string ID variable; create one from the numeric IPEDS code
tostring unitid, generate(unitid_s)

* Report missing observations for grantlow by institution
xtmis grantlow, id(unitid_s)

*===============================================================================
* Section 5.4.1: Testing for Missing Completely at Random (MCAR)
*===============================================================================

*----------------------------------------------------------------
* 🔹 Little's MCAR test using mcartest
*----------------------------------------------------------------

/* Use the full public-use HSLS:09 dataset (2017 Student File), downloadable
   from NCES at https://nces.ed.gov/datalab/onlinecodebook (rename to HSLS09.dta).
   Stata/MP: set maxvar 60000 | Stata/SE: set maxvar 32000
   Then keep: STU_ID X1SEX X1RACE X1SES X1SESQ5 X4ATPRLVLA S3CLGPELL P1TUITION
   If unavailable, use the truncated version below. */

copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example_5_3.dta" ///
     "Example_5_3.dta", replace

use "Example_5_3.dta", clear

* Recode NCES missing codes to Stata system missing
mvdecode _all, mv(-9=.)

* Install mcartest (run once): search mcartest, all → click st0318 → install
* cap net install st0318.pkg, replace

* Test 1: MCAR with equal variances
mcartest S3CLGPELL P1TUITION

* Test 2: MCAR with unequal variances
mcartest S3CLGPELL P1TUITION, unequal

* Test 3: Covariate-dependent missingness (CDM) — controlling for race-ethnicity
mcartest S3CLGPELL P1TUITION = i.X1RACE if X1RACE != ., ///
         unequal emoutput nolog

*===============================================================================
* Section 5.4.2: Panel-Specific Missing Data Analysis with xtmispanel
*===============================================================================

*----------------------------------------------------------------
* Setup: load SHEEO panel and install xtmispanel
*----------------------------------------------------------------

/* xtmispanel (Roudane 2026) is a comprehensive, all-in-one command for
   handling missing values in panel (time-series cross-sectional) data.
   Released: SSC, March 2026. SSC handle: S459624.
   Note: The GitHub version of Example5_2.dta spans FY 1980–2024 and includes
   pre-2001 years where Appropriations is missing. A locally generated
   Example_5_2.dta (from Section 5.2's drop if FY < 2010) covers FY 2010–2024
   only. Both files work with xtmispanel; the full-range file provides a richer
   missing-data demonstration. If you have not previously saved Example_5_2.dta,
   download the GitHub version below. */
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/Example5_2.dta" ///  
"Example_5_2.dta", replace

use "Example_5_2.dta", clear

* Declare it a panel dataset.
xtset state_fips FY, yearly

* Install xtmispanel from SSC (run once)
cap ssc install xtmispanel, replace

*----------------------------------------------------------------
* 🔹 Module 1: Detection — missingness by variable, state, and fiscal year
*----------------------------------------------------------------

/* The detect module generates three simultaneous tables:
   (1) Variable-level: count, percent, and pattern of missing values
   (2) Panel-unit: which states have the highest missingness rates
   (3) Time-period: whether missingness clusters in specific fiscal years
   Unlike mdesc or misstable, xtmispanel never conflates cross-sectional
   and temporal sources of missingness. */

xtmispanel NetTuition Appropriations, detect

*----------------------------------------------------------------
* 🔹 Module 2: Mechanism test (panel-aware MCAR test with stored results)
*----------------------------------------------------------------

/* The test module runs Little's MCAR test in a panel-aware manner and stores
   the diagnosed mechanism in r(mechanism), the chi-squared statistic in
   r(mcar_chi2), and the p-value in r(mcar_pval). */

xtmispanel NetTuition Appropriations, test

di "Mechanism: " r(mechanism)
di "MCAR chi2 = " r(mcar_chi2) " p = " r(mcar_pval)

*----------------------------------------------------------------
* 🔹 Module 5: Visualization — missingness heatmap and dashboard
*----------------------------------------------------------------

/* The graph module produces up to eight named graphs.
   Key outputs displayed here:
     xtmis_heatmap  — panel unit (y-axis) × time period (x-axis) grid,
                      cells shaded by proportion of variables missing
     xtmis_combined — dashboard assembling heatmap + bar charts by
                      variable, panel unit, and time period
   The heatmap makes it immediately apparent whether missingness is random,
   clustered in specific years, or confined to particular states. */

xtmispanel NetTuition Appropriations, graph

graph display xtmis_heatmap
graph display xtmis_combined

* Export all xtmispanel graphs to the graphs output directory
* width(1200) produces 1200-pixel-wide PNGs suitable for publication
foreach gname in heatmap barvar barpanel bartime pattern timeline combined {
    capture graph export "output/graphs/xtmis_`gname'.png", ///
        name(xtmis_`gname') replace width(1200)
}

*===============================================================================
* END OF CHAPTER 5 CODE
*===============================================================================

/*
KEY RECOMMENDATIONS FOR GETTING TO KNOW THY DATA:

1. STORAGE TYPES AND MEMORY:
   - Always run compress after importing data to reduce memory consumption
   - Use describe before and after compress to verify changes
   - Use recast int varname if compress stops at byte but int is needed
   - Run describe, short for very large datasets (e.g., HSLS:09)

2. MISSING DATA CODING IN SECONDARY DATA:
   - Use codebook to identify how missings are coded in NCES datasets (-9, -8, etc.)
   - Use mvdecode _all, mv(-9=.) to convert special codes to Stata system missing
   - Never assume missings are already coded as "." when importing from NCES

3. MISSING DATA ANALYSIS TOOLS — CROSS-SECTIONAL / SURVEY DATA:
   - mdesc: simplest summary — count, total, and percent missing per variable
   - misstable tree: nested pattern of missingness (percent or frequency)
   - misstable patterns: cross-tabulated missing-value patterns across variables
   - missings (with bysort): examine missingness patterns within subgroups
   - Install: cap ssc install mdesc; net install dm0085_1.pkg

4. MISSING DATA ANALYSIS TOOLS — PANEL DATA:
   - xtmis (legacy): frequency of missing by panel unit; install: ssc install xtmis
   - xtmispanel (current, Roudane 2026): comprehensive detection + testing +
     visualization in a single panel-aware command
     Recommended workflow:
       Step 1  xtmispanel varlist, detect    (who/when is missing)
       Step 2  xtmispanel varlist, test      (what mechanism: MCAR/MAR/MNAR)
       Step 3  xtmispanel varlist, graph     (visualize spatial/temporal pattern)
   - Imputation (Modules 3-4) is deferred to later chapters on panel estimation
   - Install: cap ssc install xtmispanel, replace

5. MCAR TESTING:
   - Use mcartest (Li 2013) for cross-sectional / survey data
     Install: net install st0318.pkg, replace
     Options: equal variances (default), unequal, CDM with covariates
   - Use xtmispanel, test for panel data (panel-aware MCAR test)
   - Stored results: r(mechanism), r(mcar_chi2), r(mcar_pval) enable
     conditional branching (e.g., trigger MI if mechanism is MAR or MNAR)

6. PANEL STRUCTURE BEST PRACTICES:
   - Always re-run xtset after loading or merging panel data
   - Use state_fips (not stateid) as the panel variable for state-level data
     to enable merges with other FIPS-coded files
   - Check balance: xtdescribe after xtset
   - Strongly balanced panels require no gaps in the time variable

7. DATA SOURCES IN THIS CHAPTER:
   - SHEEO SHEF data: https://shef.sheeo.org/data-downloads/
   - HSLS:09 public-use file: https://nces.ed.gov/datalab/onlinecodebook
   - All example datasets: https://github.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/

8. USER-WRITTEN PACKAGES USED IN THIS CHAPTER:
   - statastates  (Schpero 2018):  ssc install statastates
   - mdesc        (Medeiros & Blanchette 2011): ssc install mdesc
   - missings     (Cox 2015):      net install dm0085_1.pkg, replace
   - xtmis        (Nguyen 2008):   ssc install xtmis  [+ ssc install tomata]
   - mcartest     (Li 2013):       net install st0318.pkg, replace
   - xtmispanel   (Roudane 2026):  ssc install xtmispanel [released March 2026]
*/

* Close log
log close

exit
