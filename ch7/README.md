# Chapter 7: Introduction to Intermediate Statistical Techniques

## Overview

This directory contains code and validation materials for **Chapter 7** of *Higher Education Policy Analysis Using Quantitative Techniques* (2nd Edition) by Marvin A. Titus, published by Springer.

Chapter 7 introduces intermediate regression methods and applied panel-data techniques frequently used in higher education policy research. Analyses and examples include ordinary least squares (OLS), pooled OLS, fixed-effects (FE) and random-effects (RE) panel models, interaction terms, regression diagnostics, and difference-in-differences (DiD) designs — demonstrated in both Stata and R.

## Repository Contents

### Code Files

- **`Stata_code7.do`** - Complete Stata implementation (tested in Stata 19.5)
- **`R_code7.txt`** - Full R translation (tested in R 4.3.0+)
- **`Comparison of R script and Stata script results.txt`** - Validation comparing Stata and R results
- **`README.md.txt`** - Original README content for reference

### Documentation & Results

- Comparison file documents side-by-side verification of estimates, standard errors, and test statistics across platforms.

## Topics Covered

- Section 7.2: OLS regression review (bivariate and multivariate)
- Section 7.2.4: Pooled OLS for panel data and interactions (categorical × categorical, categorical × continuous, continuous × continuous)
- Regression diagnostics and robust/clustered standard errors
- Section 7.4: Fixed-effects estimation (FEDV, within estimator), institutional-level examples
- Section 7.5: Random-effects models, Breusch-Pagan test, Hausman test (including log-transformed specifications)

## Data Sources

All datasets used by Chapter 7 are available from the book's public data repository and are downloaded automatically by the scripts:

- **Example_7_2_2.dta** — State-level panel (50 states × 27 years, 1990–2016): appropriations, net tuition, FTE enrollment, per capita income, region membership, etc.
- **Example_7_4_2.dta** — Institutional-level panel (≈220 institutions × ~9 years): education & general expenditures, state appropriations, tuition, enrollment, faculty measures, etc.

Data URL root: `https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/`

## Software Requirements

### Stata

- Version: Stata 19.0+ (tested in Stata 19.5)
- Recommended user-written packages (used in some diagnostics):
  - `rhausman` (for cluster-robust Hausman test) — install with:
    ```
    ssc install rhausman, replace
    ```

The do-file includes comments describing required packages and where to install them.

### R

- Version: R 4.3.0 or later
- Required packages (the R script installs missing packages automatically):
  - haven, dplyr, lmtest, sandwich, car, plm, margins, ggplot2, multiwayvcov, stargazer, broom, tidyr

If automatic installation fails, install manually:
```r
install.packages(c("haven", "dplyr", "lmtest", "sandwich", "car",
                   "plm", "margins", "ggplot2", "multiwayvcov",
                   "stargazer", "broom", "tidyr"))
```

## Cross-Platform Validation

The R translation has been validated against the Stata implementation. Key points from the comparison:

- Coefficient estimates and non-clustered standard errors match exactly across platforms for the reported models.
- Cluster-robust standard errors may differ slightly due to implementation differences (typical small percentage differences) but do not change substantive conclusions.
- Representative matching results:
  - Bivariate OLS (2016): stapr_fte ≈ -0.354, R² ≈ 0.1303, F ≈ 7.19
  - Pooled OLS (1990–2016): identical coefficients and N = 1,350

See `Comparison of R script and Stata script results.txt` for a detailed side-by-side comparison.

## Usage Instructions

### Stata

1. Set your working directory or define a global path in `Stata_code7.do`:
   ```stata
   global ch7data "C:/your/path/here"
   cd "$ch7data"
   ```

2. Run the do-file:
   ```stata
   do Stata_code7.do
   ```

The do-file downloads the required datasets from the book's data repository when run.

### R

1. Open `R_code7.txt` (or copy contents to `R_code7.R`) and set your working directory as needed at the top of the script.

2. Source/run the file in R or RStudio:
   ```r
   source("R_code7.txt")
   ```
   or run sections interactively.

The R script will install missing packages, download datasets from GitHub, create variables, run regressions, and produce diagnostic output and plots.

## Key Variables (examples)

- state-level panel (Example_7_2_2.dta)
  - netuit — net tuition revenue
  - stapr — state appropriations
  - fte — full-time equivalent enrollment
  - pc_income — per capita income
  - region_compact — regional compact membership indicator
  - stateid, fips, year — identifiers and time index

- institutional panel (Example_7_4_2.dta)
  - eg — education & general expenditures (dependent variable in institutional models)
  - statea — state appropriations
  - tuition — tuition revenue
  - totfteiarep — total FTE enrollment
  - ftfac, ptfac — full- and part-time faculty counts
  - opeid5_new, endyear — institution identifier and year

## Notes on Implementation Differences

1. Cluster-Robust Standard Errors:
   - Stata and R use different underlying implementations for clustered covariances; small numerical differences in SEs are expected. Coefficients are identical and substantive conclusions remain the same.

2. Random Effects & Numerical Stability:
   - R’s `plm` sometimes encounters singularities for certain RE specifications (particularly with time-invariant factors or untransformed level variables). The R code uses log-transformed specifications when necessary and documents these choices.

## Troubleshooting

- Data download fails: check internet access or download dataset files manually from the data repository and place them in your working directory.
- R package installation fails: install packages manually and restart R.
- "object not found" errors in R: ensure the dataset downloaded correctly and that the per-FTE variables are created before model estimation.
- Numerical singularity in `plm` RE models: try log-transforming variables or use FE models which are more robust for clustered inference in R.

## Citation

If you use this code in your research or teaching, please cite:

Titus, M. A. (2025). Higher Education Policy Analysis Using Quantitative Techniques (2nd ed.). Springer.

GitHub repository: https://github.com/higher-ed-policy-analysis-2nd-edition/

## Author & Contact

Marvin A. Titus, Ph.D.  
Email: marvinatitus@gmail.com

## License

Code is provided for educational and research purposes. Refer to the repository license for terms of use.

## Last Updated

November 16, 2025
