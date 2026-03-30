# Chapter 8 — Advanced Statistical Techniques: I

**Book:** Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)  
**Author:** Marvin A. Titus  
**Repository:** https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch8

## Overview

This directory contains complete, reproducible code for Chapter 8, which covers advanced statistical techniques for handling autocorrelation and cross-sectional dependence in time series and panel data, with applications to higher education policy research. Both Stata and R implementations are provided.

## Repository Contents

- `Stata_code8.do` — Complete Stata do-file (tested in Stata 19.5)
- `R_code8.R` — Full R translation (tested in R 4.4.x)
- `Comparison_of_R_code8_results_and_Stata_code8_results.txt` — Section-by-section validation showing correspondence between Stata and R outputs
- `README.md` — This file

## Datasets (downloaded by the scripts)

All datasets are fetched automatically at runtime from the companion data repository:

| File | Description | Used in |
|------|-------------|---------|
| `Example_8_2.dta` | Time series data (1970–2017): 2-year college enrollment, tuition, unemployment rate | §8.2–8.4 |
| `Example_8_6.dta` | State-level panel: appropriations, net tuition, FTE enrollment, per capita income (50 states) | §8.6–8.7 |
| `Example_8_8_2.dta` | Institutional panel: education & general expenses, state appropriations, tuition, FTE, faculty (public institutions) | §8.8–8.9 |

Data URL root: `https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch8/`

### Key Variables

**Example_8_2.dta** (time series)
- `tupub2yr` — average tuition at public 2-year colleges
- `enpub2yr` — total enrollment at public 2-year colleges
- `unemprate` — state unemployment rate
- `year` — time index (1970–2017)

**Example_8_6.dta** (state panel)
- `stateid` — state identifier; `year` — time index
- `stapr`, `netuit`, `fte`, `pc_income` — appropriations, net tuition, FTE enrollment, per capita income

**Example_8_8_2.dta** (institutional panel)
- `opeid5_new` — institution identifier; `endyear` — academic year end
- `eg`, `statea`, `tuition`, `totfteiarep`, `ftfac`, `ptfac` — expenditure, appropriations, tuition, FTE, faculty

## Output Files

### Graph Output Directory

Both scripts save all plots to:

```
C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 8\Output\graphs
```

Plots from each platform are distinguished by a `_R` or `_Stata` filename suffix:

| Figure | Description | R filename | Stata filename |
|--------|-------------|------------|----------------|
| Fig. 8.1 | Levels time series (enrollment, tuition, unemployment) | `fig8_1_ts_levels_R.png` | `fig8_1_ts_levels_Stata.png` |
| Fig. 8.2 | First-differenced time series | `fig8_2_ts_firstdiff_R.png` | `fig8_2_ts_firstdiff_Stata.png` |
| Fig. 8.3 | ACF of OLS residuals | `fig8_3_ac_residuals_R.png` | `fig8_3_ac_residuals_Stata.png` |
| Fig. 8.4 | PACF of OLS residuals (Yule-Walker) | `fig8_4_pac_residuals_R.png` | `fig8_4_pac_residuals_Stata.png` |
| Fig. 8.5 | ACF of Prais-Winsten residuals | `fig8_5_ac_residuals_PW_R.png` | `fig8_5_ac_residuals_PW_Stata.png` |
| Fig. 8.6 | PACF of Prais-Winsten residuals | `fig8_6_pac_residuals_PW_R.png` | `fig8_6_pac_residuals_PW_Stata.png` |

### Log Output Directory

```
C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 8\Output\logs\
```

- Stata: `Chapter8_Stata_output.log`
- R: `Chapter8_R_output.log`

Other users receive logs at `Output/logs/` relative to their working directory.

## What the Code Does (Section-by-Section)

**Section 8.2 — Time Series Data and Autocorrelation**
- Log transformations of enrollment, tuition, and unemployment rate
- Time series visualization (levels and first-differenced)
- DF-GLS unit root tests for stationarity
- OLS regression on first-differenced variables

**Section 8.3 — Testing for Autocorrelations**
- Durbin-Watson test
- Durbin's alternative test (Breusch-Godfrey)
- ACF and PACF plots of OLS residuals

**Section 8.4 — Time Series Regression Models with AR Terms**
- Prais-Winsten regression with AR(1) correction
- ACF and PACF plots of Prais-Winsten residuals
- Cumby-Huizinga test for residual autocorrelation
- ARMAX model with AR(1) and AR(2) terms

**Section 8.6 — Autocorrelation Tests for Panel Data**
- Wooldridge test for serial correlation in panel data

**Section 8.7 — Panel-Data Regression Models with AR Terms**
- Fixed-effects regression with AR(1) error structure (`xtregar, fe`)
- Panel unit root tests (`xtpurt` / Im-Pesaran-Shin)
- First-differenced random-effects model with AR(1) disturbance

**Section 8.8 — Cross-Sectional Dependence**
- Pesaran, Friedman, and Frees tests (`xtcsd`)
- Cross-sectional dependence tests on individual variables (`xtcd`)
- Weak cross-sectional dependence test (`xtcd2`)
- Wursten fast CD test on variables and residuals (`xtcdf`)

**Section 8.9 — Panel Regression with Cross-Sectional Dependency**
- Driscoll-Kraay standard errors (`xtscc, lag(2)`)
- FE model with year fixed effects and D-K SEs
- Post-estimation CD test on residuals

## Software Requirements

### Stata
- Version: Stata 19.0+ (tested in Stata 19.5)
- Required user-written commands (install via `ssc install`):
  - `actest` — Cumby-Huizinga autocorrelation test
  - `xtcsd` — cross-sectional dependence tests (De Hoyos & Sarafidis 2006)
  - `xtcd` — cross-sectional dependence test (Eberhardt 2011)
  - `xtcd2` — weak cross-sectional dependence test (Pesaran 2015)
  - `xtcdf` — fast Pesaran CD test (Wursten 2017)

  Standard Stata commands used (no installation required): `xtserial`, `xtregar`, `xtpurt`, `xtscc`

### R
- Version: R 4.0 or later (R 4.4.x recommended)
- Required packages (installed automatically by the script):
  - `haven`, `dplyr`, `tidyr` — data import and manipulation
  - `lmtest`, `sandwich` — diagnostic tests and robust covariance
  - `plm` — panel data models, unit root tests, CD tests, Driscoll-Kraay SEs
  - `urca` — DF-GLS unit root test
  - `forecast` — ACF/PACF plots, ARIMA
  - `ggplot2`, `scales` — graphics

Install manually if needed:
```r
install.packages(c("haven", "dplyr", "tidyr", "lmtest", "sandwich",
                   "plm", "urca", "forecast", "ggplot2", "scales"))
```

## Quick Start

### Clone the repository
```bash
git clone https://github.com/higher-ed-policy-analysis-2nd-edition/code.git
cd code/ch8
```

### Run with Stata
```stata
do Stata_code8.do
```
Output paths are set automatically via `c(username)`. For username `"marvi"` the full Dropbox paths are used; all other users get relative `Output/` paths.

### Run with R
```r
source("R_code8.R")
```
Output paths are hardcoded to the Dropbox directory. All plots are displayed in the RStudio Plots pane as they are generated and saved via a temp-file copy strategy that avoids Dropbox sync-lock conflicts.

## R Implementation Notes

Several Stata commands have no direct CRAN equivalent and are implemented manually in `R_code8.R`:

| Stata command | R approach |
|---|---|
| `dfgls` | `urca::ur.ers(type="DF-GLS")` |
| `estat dwatson` | Manual: `sum(diff(e)²)/sum(e²)` — avoids hat-matrix inversion |
| `estat durbinalt` | Manual: LM = n×R² from auxiliary regression of e_t on e_{t-1} |
| `prais` | Manual iterative Prais-Winsten using `lm()` — package implementations fail on first-differenced data |
| `arima ar(1/2)` | Manual CSS via `lm()` augmented with lagged residuals |
| `actest` | `Box.test(type="Ljung-Box")` |
| `xtserial` | `plm::pbgtest()` |
| `xtregar` | `plm(model="within")` — AR(1) GLS correction has no CRAN equivalent |
| `xtpurt` | `plm::purtest(test="ips")` — Im-Pesaran-Shin test |
| `xtcsd pesaran` | `plm::pcdtest(test="cd")` |
| `xtcsd friedman` | `plm::pcdtest(test="lm")` — Breusch-Pagan LM |
| `xtcsd frees` | `plm::pcdtest(test="rho")` — average pairwise correlation |
| `xtcd`, `xtcd2`, `xtcdf` | `plm::pcdtest(test="cd")` |
| `xtscc lag(2)` | `plm::vcovSCC(maxlag=2)` |

## Citation

If you use these materials in research or teaching, please cite:

Titus, M. A. (2026). *Higher Education Policy Analysis Using Quantitative Techniques* (2nd ed.). Springer.

GitHub repository: https://github.com/higher-ed-policy-analysis-2nd-edition/code

## Author & Contact

Marvin A. Titus, Ph.D.  
Email: marvinatitus@gmail.com

## License

Code is provided for educational and research purposes. See the repository top-level LICENSE for terms of use.

## Last Updated

March 30, 2026
