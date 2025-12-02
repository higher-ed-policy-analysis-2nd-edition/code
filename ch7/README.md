# Chapter 7: Introduction to Intermediate Statistical Techniques

## Overview

This directory contains code and validation materials for **Chapter 7** of *Higher Education Policy Analysis Using Quantitative Techniques* (2nd Edition) by Marvin A. Titus, published by Springer.

Chapter 7 introduces intermediate regression methods and applied panel-data techniques frequently used in higher education policy research. Analyses and examples include ordinary least squares (OLS), pooled OLS, fixed-effects (FE) and random-effects (RE) panel models, interaction terms, regression diagnostics, and instrumental variables (IV) / two-stage least squares (2SLS) estimation — demonstrated in both Stata and R.

## Repository Contents

### Code Files

- **`Stata_code7.do`** — Complete Stata implementation (tested in Stata 19.5)
- **`R_code7.R`** — Full R translation (tested in R 4.3.0+)
- **`R_code7.txt`** — Same as R_code7.R (text file format for compatibility)

### Documentation

- **`README.md`** — This file

## Topics Covered

- **Section 7.2:** OLS regression review (bivariate and multivariate)
- **Section 7.2.4:** Pooled OLS for panel data and interactions (categorical × categorical, categorical × continuous, continuous × continuous)
- **Section 7.2.4 (cont.):** Regression diagnostics and robust/clustered standard errors
- **Section 7.4:** Fixed-effects estimation (FEDV, within estimator), institutional-level examples
- **Section 7.5:** Random-effects models, Breusch-Pagan test, Hausman test (including log-transformed specifications and cluster-robust Hausman test)
- **Section 7.6:** Instrumental variables and two-stage least squares (IV/2SLS)
  - Endogeneity and selection bias
  - IV assumptions (relevance, exclusion restriction, monotonicity)
  - First-stage regression and weak instrument diagnostics
  - LATE (Local Average Treatment Effect) interpretation
  - Application: Effect of master's degree on salary using state GA funding as instrument

## Data Sources

All datasets used by Chapter 7 are available from the book's public data repository and are downloaded automatically by the scripts:

| Dataset | Description |
|---------|-------------|
| **Example_7_2_2.dta** | State-level panel (50 states × 27 years, 1990–2016): appropriations, net tuition, FTE enrollment, per capita income, region membership, etc. |
| **Example_7_4_2.dta** | Institutional-level panel (≈220 institutions × ~9 years): education & general expenditures, state appropriations, tuition, enrollment, faculty measures, etc. |

Data URL root: `https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/`

### Synthetic Data (Section 7.6)

Section 7.6 uses **synthetic data** calibrated to mirror the Baccalaureate and Beyond Longitudinal Study (B&B). Synthetic data is used for several reasons:

1. **Access restrictions:** B&B restricted-use data requires NCES license
2. **Pedagogical transparency:** Known true parameters allow validation of estimators
3. **Reproducibility:** Readers can generate identical datasets using `set.seed(20251130)` (R) or `set seed 20251130` (Stata)
4. **Continuity:** The same dataset is used in Chapter 10 for Marginal Treatment Effects (MTE) analysis

The synthetic dataset (`bb_iv_simulation`) includes 8,000 observations with demographics, academic background, labor market characteristics, treatment assignment (master's degree completion), and salary outcomes with heterogeneous treatment effects.

**To save the synthetic dataset for use in Chapter 10**, uncomment and run the save commands at the end of each script. Instructions are provided in the commented section.

## Software Requirements

### Stata

- **Version:** Stata 19.0+ (tested in Stata 19.5)
- **User-written packages:**
  - `rhausman` (for cluster-robust Hausman test in Section 7.5.1) — install with:
    ```stata
    ssc install rhausman, replace
    ```

### R

- **Version:** R 4.3.0 or later
- **Required packages** (the R script installs missing packages automatically):

| Package | Purpose |
|---------|---------|
| haven | Read Stata .dta files |
| dplyr | Data manipulation |
| lmtest | Diagnostic tests (Breusch-Pagan, likelihood ratio) |
| sandwich | Robust standard errors (HC1, etc.) |
| car | Linear hypothesis tests (Wald tests) |
| plm | Panel data models (FE, RE, Hausman test) |
| ggplot2 | Visualization |
| multiwayvcov | Cluster-robust standard errors |
| stargazer | Regression tables |
| broom, tidyr | Tidy model output |
| **AER** | Instrumental variables regression (`ivreg`) |

If automatic installation fails, install manually:
```r
install.packages(c("haven", "dplyr", "lmtest", "sandwich", "car",
                   "plm", "ggplot2", "multiwayvcov",
                   "stargazer", "broom", "tidyr", "AER"))
```

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

1. Open `R_code7.R` and set your working directory as needed at the top of the script.

2. Source/run the file in R or RStudio:
   ```r
   source("R_code7.R")
   ```
   or run sections interactively.

The R script will install missing packages, download datasets from GitHub, create variables, run regressions, and produce diagnostic output and plots.

## Key Variables

### State-Level Panel (Example_7_2_2.dta)

| Variable | Description |
|----------|-------------|
| netuit | Net tuition revenue |
| stapr | State appropriations |
| fte | Full-time equivalent enrollment |
| pc_income | Per capita income |
| region_compact | Regional compact membership indicator |
| stateid, fips, year | Identifiers and time index |

### Institutional Panel (Example_7_4_2.dta)

| Variable | Description |
|----------|-------------|
| eg | Education & general expenditures (dependent variable) |
| statea | State appropriations |
| tuition | Tuition revenue |
| totfteiarep | Total FTE enrollment |
| ftfac, ptfac | Full- and part-time faculty counts |
| opeid5_new, endyear | Institution identifier and year |

### Synthetic B&B Data (Section 7.6)

| Variable | Description |
|----------|-------------|
| masters | Treatment: completed master's degree (1=Yes) |
| ln_salary | Log annual salary (outcome) |
| ga_funding_adj | State GA funding, field-adjusted (instrument) |
| te_masters | True individual treatment effect (from DGP) |
| p_masters | True propensity score (from DGP) |
| female, black, hispanic, asian | Demographics |
| ugpa, stem_major, bus_major, ed_major | Academic background |
| firstgen, parent_income_q, parent_grad | Family background |

## Notes on Implementation Differences

### Cluster-Robust Standard Errors

Stata and R use different underlying implementations for clustered covariances; small numerical differences in SEs are expected. Coefficients are identical and substantive conclusions remain the same.

### Random Effects & Numerical Stability

R's `plm` package sometimes encounters singularities for certain RE specifications, particularly with:
- Time-invariant factors included in the model
- Untransformed level variables with very different scales

The R code addresses this by:
- Using the Swamy-Arora method (`random.method = "swar"`) for better numerical stability
- Wrapping problematic models in `tryCatch()` with informative messages
- Using log-transformed variables for Hausman tests (as recommended in the Stata code)

### IV/2SLS Implementation

| Stata | R |
|-------|---|
| `ivregress 2sls` | `ivreg()` from AER package |
| `estat firststage` | Included in `summary(iv_model, diagnostics = TRUE)` |
| `estat endogenous` | Included in `summary(iv_model, diagnostics = TRUE)` |

## AI-Assisted Code Development

The simulation code in Section 7.6 was developed with assistance from Claude (Anthropic). The author provided specifications based on B&B characteristics and higher education finance literature. Claude assisted in translating specifications to executable code. The author reviewed, tested, and validated all code.

## Troubleshooting

| Issue | Solution |
|-------|----------|
| Data download fails | Check internet access or download dataset files manually from the data repository and place them in your working directory |
| R package installation fails | Install packages manually and restart R |
| "object not found" errors in R | Ensure the dataset downloaded correctly and that derived variables are created before model estimation |
| Numerical singularity in `plm` RE models | Try log-transforming variables or use FE models which are more robust |
| Weak instrument warning | Check first-stage F-statistic; values > 10 indicate a strong instrument |

## Citation

If you use this code in your research or teaching, please cite:

> Titus, M. A. (2025). *Higher Education Policy Analysis Using Quantitative Techniques* (2nd ed.). Springer.

GitHub repository: https://github.com/higher-ed-policy-analysis-2nd-edition/

## Author & Contact

**Marvin A. Titus, Ph.D.**  
Email: marvinatitus@gmail.com

## License

Code is provided for educational and research purposes. Refer to the repository license for terms of use.

## Last Updated

December 2025
