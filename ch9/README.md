# Chapter 9 — Advanced Statistical Techniques: II

**Book:** Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)  
**Author:** Marvin A. Titus  
**Repository:** https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch9

## Overview

This directory contains complete, reproducible code for Chapter 9, which demonstrates heterogeneous coefficient regression (HCR) with dynamic common correlated effects (DCCE) and mean group (MG) estimators for macro panel data. Both Stata and R implementations are provided.

## Repository Contents

- `Stata_code9.do` — Complete Stata do-file (tested in Stata 19.5)
- `R_code9.R` — Full R translation (tested in R 4.4.x)
- `README.md` — This file

## Dataset

Both scripts download the dataset automatically at runtime from the companion data repository:

| File | Description |
|------|-------------|
| `Example_9_3_1.dta` | State-level macro panel: log state appropriations (`lny1`), log net tuition revenue (`lnx1`), log FTE enrollment (`lnx2`), log per capita income (`lnx3`); 48 states × 45 fiscal years (FY 1980–2024) |

Data URL: `https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch9/Example_9_3_1.dta`

### Key Variables

| Variable | Description |
|----------|-------------|
| `lny1` | Log of state appropriations to higher education |
| `lnx1` | Log of net tuition revenue |
| `lnx2` | Log of full-time equivalent (FTE) enrollment |
| `lnx3` | Log of state per capita income |
| `FY` | Fiscal year (time index) |
| `state` | State identifier |

## Output Files

### Graph Output Directory

Both scripts save all plots to:

```
C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 9\Output\graphs
```

Plots from each platform carry a `_R` or `_Stata` filename suffix:

| Figure | Description | R filename | Stata filename |
|--------|-------------|------------|----------------|
| Fig. 9.1 | Trends in log appropriations by state | `fig9_1_lny1_by_state_R.png` | `fig9_1_lny1_by_state_Stata.png` |
| Fig. 9.2 | Trends in log per capita income by state | `fig9_2_lnx3_by_state_R.png` | `fig9_2_lnx3_by_state_Stata.png` |

### Log Output Directory

```
C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 9\Output\logs\
```

- Stata: `Chapter9_Stata_output.log`
- R: `Chapter9_R_output.log`

Other users receive logs at `Output/logs/` relative to their working directory.

## What the Code Does (Section-by-Section)

**Section 9.6.1 — Macroeconomic Panel Data**
- Describes the SHEEO/BEA macro panel dataset (48 states × 45 fiscal years)
- Produces Figs. 9.1 and 9.2: state-by-state trends in log appropriations and log per capita income

**Section 9.6.2 — Tests for Nonstationary Data**
- Panel unit root tests (`xtpurt`) for all four variables in levels, with three test variants:
  - Herwartz and Siedenburg (2008): `test(hs)`
  - Demetrescu and Hanck (2012): `test(dh)`
  - Herwartz, Maxand, and Walle (2019): `test(hmw) trend`
- Unit root tests on first-differenced variables: `test(all)`
- Confirms all variables are I(1)

**Section 9.6.3 — Tests for Cointegration**
- Kao (1999) panel cointegration test, with and without demeaning (`xtcointtest kao`)
- Pedroni (2000, 2004) panel cointegration test, with and without demeaning (`xtcointtest pedroni`)
- Westerlund (2005) variance ratio test, with and without demeaning (`xtcointtest westerlund`)
- Westerlund (2007) ECM-based panel cointegration test (`xtwest`): Gt, Ga, Pt, Pa statistics

**Section 9.6.4 — Tests for Cross-Sectional Independence**
- Pesaran CD test on all four variables (`xtcdf`)

**Section 9.6.5 — Test of Homogeneous Coefficients**
- Pesaran-Yamagata / Blomquist-Westerlund slope homogeneity test (`xthst`):
  - First-differenced specification
  - Levels specification

**Section 9.6.6 — Results of the HCR with DCCE and MG Estimators**
- Main DCCE-MG ARDL-ECM model with `cr_lags(3 3 3 3)` and `lr_options(ardl)`
- Post-estimation weak cross-sectional dependence test (`xtcd2`)
- Alternative specification with `lr_options(xtpmg)` and cross-sectional exponent estimation
- Individual-state EC coefficients with `cr_lags(1 3 3 3)` and `showindividual`

## Software Requirements

### Stata
- Version: Stata 19.0+ (tested in Stata 19.5)
- Required user-written commands (install via `ssc install` or `net install`):
  - `xtcdf` — Pesaran CD test (Wursten 2017): `ssc install xtcdf, replace`
  - `xthst` — slope homogeneity test (Ditzen & Bersvendsen 2020): `ssc install xthst, replace`
  - `xtdcce2` — DCCE/MG estimator (Ditzen 2018): `net install st0536.pkg, replace`
  - `xtwest` — Westerlund (2007) ECM cointegration test
  - `xtpurt` — panel unit root tests (Herwartz-Siedenburg, Demetrescu-Hanck, HMW variants)

### R
- Version: R 4.0 or later (R 4.4.x recommended)
- Required packages (installed automatically by the script):
  - `haven`, `dplyr`, `tidyr` — data import and manipulation
  - `ggplot2`, `scales` — graphics
  - `plm` — panel unit root tests, Pesaran CD test
  - `urca` — unit root tests
  - `tseries` — Phillips-Ouliaris cointegration test
  - `lmtest`, `sandwich` — inference and robust standard errors

Install manually if needed:
```r
install.packages(c("haven", "dplyr", "tidyr", "ggplot2", "scales",
                   "plm", "urca", "tseries", "lmtest", "sandwich"))
```

## Quick Start

### Run with Stata
```stata
do Stata_code9.do
```
Output paths switch automatically via `c(username)`. For username `"marvi"` the full Dropbox paths are used; all other users get relative `Output/` paths.

### Run with R
```r
source("R_code9.R")
```
Plots are displayed in the RStudio Plots pane as generated, and saved via a temp-file copy strategy to avoid Dropbox sync-lock conflicts.

## R Implementation Notes

Most of Chapter 9's Stata commands have no direct CRAN equivalent and are implemented manually in `R_code9.R`:

| Stata command | R approach |
|---|---|
| `xtpurt test(hs/dh/hmw)` | `plm::purtest(test="ips")` — Im-Pesaran-Shin (2003); hs/dh/hmw variants have no CRAN equivalent |
| `xtcointtest kao/pedroni` | Engle-Granger two-step: FE residuals → `plm::purtest()` |
| `xtcointtest westerlund` | Per-unit `tseries::po.test()` aggregated via Fisher combination |
| `xtwest` (Westerlund 2007) | Manual unit-specific ECM regressions → Gt/Ga statistics |
| `xtcdf` | `plm::pcdtest(test="cd")` |
| `xthst` (Pesaran-Yamagata 2008) | Manual Δ-tilde statistic via unit-specific OLS |
| `xtdcce2` (DCCE-MG ARDL-ECM) | Manual: cross-sectional averages + unit ECM regressions → MG averaging |
| `xtcd2` | `plm::pcdtest(test="cd")` on FE model residuals |

## Citation

If you use these materials in research or teaching, please cite:

Titus, M. A. (2026). *Higher Education Policy Analysis Using Quantitative Techniques* (2nd ed.). Springer.

GitHub repository: https://github.com/higher-ed-policy-analysis-2nd-edition/code

## Author & Contact

Marvin A. Titus, Ph.D.

## License

Code is provided for educational and research purposes. See the repository top-level LICENSE for terms of use.

## Last Updated

March 30, 2026
