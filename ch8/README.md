# Chapter 8: Advanced Statistical Techniques I

## Overview

This repository contains code and validation materials for **Chapter 8** of *Higher Education Policy Analysis Using Quantitative Techniques* (2nd Edition) by Marvin A. Titus, published by Springer.

Chapter 8 focuses on advanced statistical techniques for handling autocorrelation and cross-sectional dependence in time series and panel data analysis, with applications to higher education policy research.

## Repository Contents

### Code Files

- **`Stata_code8.do`** - Complete Stata implementation (tested in Stata 19.5)
- **`R_code8.txt`** - Full R translation with identical functionality

### Documentation

- **`Comparison_of_R_code8_results_and_Stata_code8_results.txt`** - Detailed validation showing perfect correspondence between Stata and R results across all analyses
- **`README.md`** - This file

## Topics Covered

### Section 8.2: Time Series Data and Autocorrelation
- Log transformations of enrollment, tuition, and unemployment rate data
- Time series visualization and trends (1970-2017)
- DF-GLS unit root tests for stationarity
- First-differencing to achieve stationarity
- OLS regression with differenced variables

### Section 8.3: Testing for Autocorrelations
- Durbin-Watson test
- Breusch-Godfrey test
- ACF and PACF diagnostics
- Time series autocorrelation diagnostics

### Section 8.4: Time Series Regression Models with AR Terms
- Prais-Winsten regression with AR(1) terms
- ARIMA models with AR(1) and AR(2) terms
- Cumby-Huizinga test for residual autocorrelation
- Post-estimation diagnostics

### Section 8.6: Autocorrelation Tests for Panel Data
- Wooldridge test for serial correlation in panel data
- Panel data structure: 50 states × multiple years

### Section 8.7: Panel-Data Regression Models with AR Terms
- Fixed-effects panel regression with AR(1) error structure
- Panel unit root tests (Hadri LM in R, Herwartz-Siedenburg in Stata)
- First-differencing in panel context
- Random-effects models with AR(1) disturbances

### Section 8.8: Cross-Sectional Dependence
- Pesaran CD test
- Friedman test
- Frees test
- Tests on regression residuals
- Multiple institution panel dataset (1,159 institutions)

### Section 8.9: Panel Regression with Cross-Sectional Dependency
- Driscoll-Kraay standard errors
- Fixed-effects models with D-K robust standard errors
- Year fixed effects specification
- Post-estimation diagnostics

## Data Sources

All datasets are publicly available from the book's GitHub repository:

- **Example_8_2.dta** - Time series data (1970-2017) for 2-year college enrollment, tuition, and unemployment rates
- **Example_8_6.dta** - State-level panel data for appropriations, net tuition, FTE enrollment, and per capita income
- **Example_8_8_2.dta** - Institutional panel data with 1,159 public institutions across multiple years

Data URL: `https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch8/`

## Software Requirements

### Stata
- **Version:** Stata 19.5 or later
- **Required user-written commands:**
  - `xtserial` - Serial correlation tests for panel data
  - `xtregar` - Panel regression with AR terms
  - `xtpurt` - Panel unit root tests
  - `actest` - Cumby-Huizinga autocorrelation test
  - `xtcsd` - Cross-sectional dependence tests (De Hoyos & Sarafidis)
  - `xtcd` - Cross-sectional dependence test (Eberhardt)
  - `xtcd2` - Weak cross-sectional dependence test (Pesaran)
  - `xtcdf` - Fast Pesaran CD test (Wursten)
  - `xtscc` - Driscoll-Kraay standard errors

### R
- **Version:** R 4.0 or later
- **Required packages:**
  - `haven` - Reading Stata files
  - `dplyr`, `tidyr` - Data manipulation
  - `ggplot2` - Visualization
  - `tseries`, `urca` - Time series analysis
  - `lmtest` - Diagnostic tests
  - `plm` - Panel data models
  - `sandwich` - Robust covariance matrices
  - `car` - Additional diagnostics
  - `forecast` - ARIMA modeling
  - `broom` - Model tidying
  - `prais` - Prais-Winsten regression
  - `clubSandwich` - Driscoll-Kraay standard errors
  - `data.table` - Efficient data handling

## Cross-Platform Validation

The R code has been rigorously validated against Stata results. **All estimates match to at least 5 decimal places** across:

✅ **Time Series Models**
- DF-GLS unit root statistics: Exact match
- OLS coefficients and R²: Perfect correspondence
- Prais-Winsten AR(1) estimates: Identical (including ρ = 0.61495)
- ARIMA AR(1) and AR(2) coefficients: Exact match

✅ **Panel Data Models**
- Wooldridge test statistic: F = 83.583 (both platforms)
- Fixed-effects with AR(1): All coefficients match exactly
- Driscoll-Kraay standard errors: Perfect correspondence

✅ **Diagnostic Tests**
- Cross-sectional dependence tests (Pesaran, Friedman, Frees): Identical
- Panel unit root tests: Both platforms confirm stationarity
- Serial correlation diagnostics: Consistent results

See `Comparison_of_R_code8_results_and_Stata_code8_results.txt` for detailed validation tables.

## Usage Instructions

### Stata

1. Set your working directory or define a global path:
   ```stata
   global ch8data "C:/your/path/here"
   cd "$ch8data"
   ```

2. Run the complete do-file:
   ```stata
   do Stata_code8.do
   ```

The script automatically downloads required datasets from GitHub.

### R

1. Install required packages (done automatically by script):
   ```r
   source("R_code8.txt")
   ```

2. Or run sections individually by copying relevant code blocks

The R script automatically:
- Installs missing packages
- Downloads datasets from GitHub
- Creates all necessary variables
- Runs all analyses

## Key Variables

### Time Series Variables (Example_8_2.dta)
- `tupub2yr` - Average tuition at public 2-year colleges
- `enpub2yr` - Total enrollment at public 2-year colleges
- `unemprate` - State unemployment rate
- `year` - Time index (1970-2017)

### Panel Variables (Example_8_6.dta)
- `stateid` - State identifier
- `stapr` - State appropriations per FTE
- `netuit` - Net tuition revenue per FTE
- `fte` - Full-time equivalent enrollment
- `pc_income` - Per capita income
- `year` - Time index

### Institutional Panel Variables (Example_8_8_2.dta)
- `opeid5_new` - Institution identifier
- `eg` - Education and general expenses
- `statea` - State appropriations
- `tuition` - Tuition and fee revenue
- `totfteiarep` - Total FTE enrollment
- `ftfac` - Full-time faculty count
- `ptfac` - Part-time faculty count
- `endyear` - Academic year end

## Citation

If you use this code in your research, please cite:

> Titus, M. A. (2026). *Higher Education Policy Analysis Using Quantitative Techniques* (2nd ed.). Springer.

## Author

**Marvin A. Titus, Ph.D.**  
Professor and Author  
Last Updated: November 19, 2025

## GitHub Repository

Complete materials available at:  
`https://github.com/higher-ed-policy-analysis-2nd-edition`

## License

Code is provided for educational and research purposes in conjunction with the textbook.

## Notes

- The R translation maintains identical variable naming conventions where possible
- All log transformations use natural logarithms
- Panel data structures are explicitly defined using `pdata.frame()` in R
- Driscoll-Kraay standard errors use `vcovSCC()` from the `plm` package
- Missing values are handled consistently across platforms

## Support

For questions or issues:
1. Consult the textbook for theoretical background
2. Check the comparison file for validation details
3. Review inline comments in code files
4. Ensure all required packages/commands are properly installed

---

**Reproducible Research:** All code is designed for complete reproducibility. Simply run the scripts to replicate all analyses from Chapter 8.
