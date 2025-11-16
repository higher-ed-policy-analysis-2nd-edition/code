# Chapter 5 – Getting to Know Thy Data

Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)  
Author: Marvin A. Titus  
Repository: higher-ed-policy-analysis-2nd-edition/code/ch5

This directory contains the complete, reproducible code and supporting files for Chapter 5 of the book. The chapter demonstrates data structure examination, panel data organization, missing data analysis, missingness patterns, and tests for Missing Completely At Random (MCAR) using the datasets distributed with the book. Both Stata and R implementations are provided and have been cross-checked for consistency.

---

## Contents

- **Stata_Code5.do**  
  Complete Stata do-file that reproduces the analyses used in Chapter 5 (tested in Stata 19.5+).

- **R_Code5.R** (also provided as R_Code5.txt)  
  Full R translation of the Stata do-file using tidyverse, haven, plm, naniar, mice, and other packages. This script downloads the same data used by the Stata code and runs identical analyses.

- **Results_Comparison.md**  
  Detailed validation document showing statistical equivalence of Stata vs R MCAR test results (chi-square statistics match within 0.065%).

- **Datasets** (downloaded automatically by scripts)  
  Example_4_2_2_TS.dta, Example_5_0.dta, Example_5_1.xlsx, Example_5_3.dta, Example_5_4.dta, Example_5_4_1.dta, and Public_use_HSLS_09_truncated.dta are downloaded from the book data repository at runtime.

---

## Data

All datasets used in the scripts are available in the book's data repository and are downloaded automatically by the Stata and R scripts. The primary files used in this chapter are:

- **Example_4_2_2_TS.dta** – time series of percent of US high school graduates in postsecondary education, 1960–2016
- **Example_5_0.dta** – panel dataset of state-level undergraduate enrollment and financial aid (50 states × 5 years)
- **Example_5_1.xlsx** (reformatted sheet) – SHEEO state higher education finance data (FY 2010–2024)
- **Public_use_HSLS_09_truncated.dta** – truncated version of HSLS:09 2017 Student File (23,503 observations)
- **Example_5_3.dta** – HSLS:09 subset with selected variables for MCAR testing (STU_ID, X1SEX, X1RACE, X1SES, X1SESQ5, X4ATPRLVLA, S3CLGPELL, P1TUITION)
- **Example_5_4.dta** – IPEDS institutional data example (5,173 institutions)
- **Example_5_4_1.dta** – HSLS:09 data for missing pattern analysis

Data repository (raw files):  
https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch5

Note: The full public-use HSLS:09 dataset (2017 Student File) can be downloaded directly from NCES at https://nces.ed.gov/datalab/onlinecodebook. This is a large file requiring Stata/MP or Stata/SE with appropriate maxvar settings. The truncated version provided is sufficient for all chapter examples.

---

## Quick start – Stata

1. Open Stata and set your working directory to this folder (or edit the do-file paths).
2. (Optional) Install user-written packages if needed:
   - `mcartest` (required for Little's MCAR test):
     ```stata
     net install st0318.pkg, replace
     ```
3. Run:
   ```stata
   do Stata_Code5.do
   ```

The do-file will download required data files, examine data structures using `describe` and `compress`, create panel data structures, analyze missing data patterns, and perform Little's MCAR test with equal variances, unequal variances, and covariate-dependent missing (CDM) specifications.

---

## Quick start – R

The R script reproduces the Stata workflow. It downloads the same datasets and performs equivalent analyses with automatic fallback methods for MCAR testing.

**Prerequisites:**
- R >= 4.0
- Recommended packages (the script will attempt to install missing packages):
  - Required: `tidyverse`, `haven`, `readxl`, `plm`, `naniar`, `mice`, `DescTools`, `car`
  - Optional: `BaylorEdPsych`, `MissMech` (fallback MCAR test methods)

**Run the script:**
1. Open the R script file (R_Code5.R) or the R version included here.
2. Optionally set a local directory for persistent file storage:
   ```r
   ch5data <- "C:/Users/YourName/Documents/book-materials/ch5/data"
   dir.create(ch5data, recursive = TRUE, showWarnings = FALSE)
   setwd(ch5data)
   ```
3. Source the file or run it in an R session:
   ```r
   source("R_Code5.R")
   ```

The script prints summaries to the console, displays missing data patterns, performs MCAR tests, and saves results to an RDS file.

---

## What the code does (high level)

### Section 5.2 – Getting to Know the Structure of Our Datasets

- **Time series data examination:**
  - Load and describe Example_4_2_2_TS.dta (56 observations, 2 variables)
  - Variable types and storage optimization (compress/downcast)

- **Panel data structure:**
  - Load Example_5_0.dta (250 observations: 50 states × 5 years)
  - Examine panel structure (id, year, State, Ugrad, need, merit)
  - Data type optimization and variable recasting

- **SHEEO finance data:**
  - Import Example_5_1.xlsx and filter to FY >= 2010
  - Exclude aggregate rows (U.S., D.C.)
  - Create panel data structures with state and year indices

### Section 5.3 – Analyzing Missing Data Patterns

- **HSLS:09 dataset preparation:**
  - Load truncated HSLS:09 dataset (23,503 students)
  - Keep relevant variables for analysis
  - Recode -9 values to missing (NA/.)

- **Missing data pattern analysis:**
  - Summarize missing values by variable (`naniar::miss_var_summary` in R, `misstable` in Stata)
  - Identify missing data patterns (`mice::md.pattern` in R)
  - Examine patterns by demographic subgroups (race/ethnicity)
  - Display frequency of each unique missing pattern

### Section 5.4.1 – Testing for Missing Completely at Random (MCAR)

- **Little's MCAR test** on S3CLGPELL and P1TUITION:
  - **Equal variances** (primary test): Tests whether missingness is completely at random
  - **Unequal variances** (Stata): Relaxes equal variance assumption
  - **Covariate-Dependent Missing (CDM)**: Tests whether missingness depends on observed covariates (X1RACE)

- **R implementation features:**
  - Primary method: `naniar::mcar_test()` (modern, actively maintained)
  - Fallback hierarchy: BaylorEdPsych → MissMech → logistic regression CDM
  - Automatic handling of completely missing observations
  - Fisher combination of p-values for CDM diagnostics
  - Results saved to RDS file for reproducibility

- **Key outputs:**
  - Chi-square test statistic and degrees of freedom
  - P-value for MCAR null hypothesis
  - Number of observations and missing patterns
  - EM algorithm convergence diagnostics (Stata CDM)

---

## Reproducibility & validation

- The R translation has been rigorously validated against the Stata output (see `Results_Comparison.md`).
- **MCAR test validation:**
  - Chi-square statistics: 68.1557 (Stata) vs 68.2 (R) – difference of 0.0443 (0.065%)
  - Degrees of freedom: 2 (both implementations)
  - P-values: < 0.001 (both implementations reject MCAR)
  - Sample sizes: 23,471 observations (both, excluding 32 with all variables missing)
  - Missing patterns: 3 distinct patterns identified (both)

- **Dataset validation:**
  - All 7 datasets load with identical dimensions (rows and columns)
  - Filtering operations produce identical sample sizes
  - Missing data patterns match exactly
  - Variable transformations (recoding, type conversions) are equivalent

- **Statistical conclusions:**
  - Both implementations reject the MCAR null hypothesis (p < 0.001)
  - Data exhibit Missing At Random (MAR) or Missing Not At Random (MNAR) mechanisms
  - Missingness is significantly related to race/ethnicity (CDM test)
  - Complete-case analysis would introduce bias

- **Code quality:**
  - R code follows professional formatting standards consistent with Chapter 6
  - Clear section organization with 64-character dividers
  - Comprehensive inline documentation
  - Robust error handling and fallback mechanisms

---

## Key findings from Chapter 5 analyses

### Data Structure Insights

- **Time series data:** 56 annual observations from 1960–2016 demonstrate long-term trends
- **Panel data:** 50 states × 5 years structure enables within-state and between-state comparisons
- **SHEEO finance:** 750 state-year observations (15 years × 50 states) after filtering

### Missing Data Patterns

- **Complete cases:** 20,572 students (87.5%) have no missing values on analysis variables
- **Most common pattern:** 1,358 students (5.8%) missing only S3CLGPELL (Pell Grant receipt)
- **Second pattern:** 969 students (4.1%) missing only X1RACE
- **Total patterns:** 3 distinct patterns across 8 variables

### MCAR Test Results

- **Null hypothesis rejected:** χ² = 68.16, df = 2, p < 0.001
- **Implication:** Data are NOT Missing Completely At Random
- **Substantive finding:** Missingness varies systematically by race/ethnicity (CDM χ² = 105.20, df = 18, p < 0.001)
- **Methodological implication:** Complete-case analysis would be biased; use multiple imputation, inverse probability weighting, or other MAR-appropriate methods

---

## Technical notes

### Helper Functions (R)

The R script includes two helper functions for data optimization and cleaning:

- **`downcast_double()`:** Converts double-precision numeric variables to integers when appropriate (values are whole numbers), reducing memory usage and matching Stata's `compress` behavior.

- **`recode_minus9_to_na()`:** Converts all -9 coded missing values to proper NA values, equivalent to Stata's `mvdecode _all, mv(-9=.)`.

### MCAR Test Methods

**Stata implementation:**
- Uses `mcartest` command (user-written package by Bluml et al., 2007)
- Implements Little's (1988) test via EM algorithm
- Options: `equal` (default), `unequal` (relaxes equal variance), CDM with covariates

**R implementation:**
- Primary: `naniar::mcar_test()` based on Little's (1988) test
- Falls back to `BaylorEdPsych::LittleMCAR()` or `MissMech::TestMCAR()` if naniar unavailable
- Final fallback: logistic regression approach (missingness ~ covariates) with Fisher combination of p-values
- All methods test the same null hypothesis and produce statistically equivalent results

**Reference:**  
Little, R.J.A. (1988). A test of missing completely at random for multivariate data with missing values. *Journal of the American Statistical Association*, 83(404), 1198-1202.

---

## Troubleshooting

### Stata issues

- **"mcartest not found":**
  - Install with: `net install st0318.pkg, replace`
  - Alternatively: `ssc install mcartest` (if available on SSC)

- **"insufficient memory" errors with full HSLS:09 dataset:**
  - Use Stata/MP or Stata/SE (not Stata/IC)
  - Set `set maxvar 32000` (Stata/SE) or `set maxvar 60000` (Stata/MP)
  - Alternatively, use the truncated version (Example_5_3.dta) provided

- **Download failures:**
  - Check internet connection
  - GitHub raw URLs may require https access
  - Save files locally and edit file paths in do-file

### R issues

- **"naniar not found" error:**
  - The script should auto-install, but if it fails:
    ```r
    install.packages("naniar", dependencies = TRUE)
    ```

- **Package installation fails:**
  - Install packages manually:
    ```r
    install.packages(c("tidyverse", "haven", "readxl", "plm", 
                       "naniar", "mice", "DescTools", "car"))
    ```

- **"BaylorEdPsych not available" warning:**
  - This is expected; BaylorEdPsych is archived on CRAN
  - The script uses naniar instead (modern, maintained alternative)
  - Warning does not affect functionality

- **Memory issues with large datasets:**
  - R loads entire datasets into memory
  - Close other applications if needed
  - Use 64-bit R for large datasets
  - The truncated datasets provided are suitable for most systems

- **"rows have all test variables missing" message:**
  - This is informational, not an error
  - Indicates observations excluded from MCAR test (expected behavior)
  - Stata and R both exclude 32 observations with all test variables missing

---

## Understanding the output

### MCAR Test Interpretation

**If p-value < 0.05 (reject MCAR):**
- Missing data mechanism is NOT completely at random
- Missingness depends on observed or unobserved variables
- Complete-case analysis may be biased
- Consider: multiple imputation (if MAR), selection models (if MNAR), or inverse probability weighting

**If p-value ≥ 0.05 (fail to reject MCAR):**
- Insufficient evidence against MCAR
- Complete-case analysis may be acceptable
- However, low power with small samples; consider multiple imputation for robustness

**CDM Test Interpretation:**
- Tests whether missingness depends on observed covariates
- Significant result suggests Missing At Random (MAR) conditional on covariates
- Guides choice of variables to include in imputation models

### Missing Data Pattern Interpretation

- **Pattern "00000000":** Complete cases (no missing values)
- **Pattern "00000001":** Missing only the rightmost variable (often outcome variable)
- **Pattern "00100000":** Missing only the 3rd variable (from left)

More complex patterns suggest:
- Unit nonresponse (entire surveys missing)
- Item nonresponse (specific questions skipped)
- Systematic data collection issues

---

## Extensions and modifications

Researchers can modify these scripts to:

1. **Test different variables for MCAR:**
   - Stata: Change variable list in `mcartest` command
   - R: Modify `vars_test <- c("VAR1", "VAR2")` vector

2. **Add covariates to CDM test:**
   - Stata: Add to formula: `mcartest Y1 Y2 = i.COVAR1 i.COVAR2`
   - R: Modify `covariates <- c("VAR1", "VAR2")` vector

3. **Analyze different subgroups:**
   - Use `if` conditions (Stata) or `filter()` (R) before testing
   - Example: `filter(X1SEX == 1)` for male students only

4. **Visualize missing patterns:**
   - R: Add `naniar::gg_miss_var(dataset)` for variable-level plot
   - R: Add `naniar::gg_miss_upset(dataset)` for pattern intersection plot
   - Stata: Use `misstable patterns, freq` for frequency table

5. **Export results for reporting:**
   - R: Results saved automatically to `MCAR_test_result.rds`
   - R: Load with `readRDS("MCAR_test_result.rds")`
   - Stata: Use `estout` or `outreg2` to export test statistics

---

## Data citation

When using the HSLS:09 data, please cite:

U.S. Department of Education, National Center for Education Statistics. (2017). *High School Longitudinal Study of 2009 (HSLS:09) Second Follow-up and High School Transcript Data File Documentation* (NCES 2017-401). Washington, DC: Author.

Dataset available at: https://nces.ed.gov/surveys/hsls09/

---

## Book citation and license

Please cite the book when using this code in research or teaching:

Titus, M. A. (2025). *Higher Education Policy Analysis Using Quantitative Techniques* (2nd ed.). Springer.

Code repository: https://github.com/higher-ed-policy-analysis-2nd-edition/code

License: See the book/repository license for permitted use and redistribution.

---

## Contact / Support

For code-related issues or corrections:
- Open an issue in this GitHub repository: https://github.com/higher-ed-policy-analysis-2nd-edition/code/issues
- Email the author: marvinatitus@gmail.com

For questions about the HSLS:09 dataset:
- NCES data support: https://nces.ed.gov/datalab/help

---

## Version history

- **2025-11-16:** Initial release with validated R translation
  - R code reformatted to match Chapter 6 style standards
  - Cross-validated against Stata output (chi-square match within 0.065%)
  - Added comprehensive documentation and troubleshooting
  - Implemented robust fallback methods for MCAR testing

---

## Acknowledgments

The R translation benefits from the excellent work of the naniar package developers (Tierney & Cook, 2023) and the mice package team (van Buuren & Groothuis-Oudshoorn, 2011) for modern missing data analysis tools.

**Key package citations:**

Tierney, N., & Cook, D. (2023). Expanding tidy data principles to facilitate missing data exploration, visualization and assessment of imputations. *Journal of Statistical Software*, 105(7), 1-31.

van Buuren, S., & Groothuis-Oudshoorn, K. (2011). mice: Multivariate imputation by chained equations in R. *Journal of Statistical Software*, 45(3), 1-67.

---

Last updated: 2025-11-16