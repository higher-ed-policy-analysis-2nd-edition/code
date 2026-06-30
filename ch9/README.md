# Chapter 9 — Heterogeneous Coefficient Regression with Dynamic Common Correlated Effects and Mean Group Estimators

This folder holds the Stata and R code for Chapter 9 of *Higher Education Policy Analysis Using Quantitative Techniques* (2nd Edition, Springer). The chapter introduces and demonstrates heterogeneous coefficient regression (HCR) with dynamic common correlated effects (DCCE) and mean group (MG) estimators for macro panel data. The analysis addresses cross-sectional dependence, nonstationarity, and slope heterogeneity in long state-level higher education finance panels.

## Empirical Setting

The chapter's running example uses a SHEEO/BEA state-level macro panel covering 48 states across 45 fiscal years (FY 1980–2024). The outcome variable is log state appropriations to higher education (`lny1`); predictors are log net tuition revenue (`lnx1`), log FTE enrollment (`lnx2`), and log state per capita income (`lnx3`). The analysis proceeds through a standard pre-estimation diagnostic sequence — unit root tests, cointegration tests, cross-sectional dependence, and slope homogeneity — before estimating the HCR with DCCE-MG ARDL-ECM model that recovers state-specific short-run dynamics and a common long-run relationship.

## File Structure

### Stata Script

| File | Description |
|---|---|
| `Stata_code9.do` | Single Stata script. Sets output paths and log, installs packages, runs all diagnostic tests (Sections 9.3.2–9.3.5), and estimates the main HCR with DCCE-MG model and alternatives (Section 9.3.6). |

### R Translation

| File | Stata Counterpart | Description |
|---|---|---|
| `R_code9.R` | `Stata_code9.do` | Complete R translation. Reproduces all diagnostics and estimation steps. Most Stata commands in this chapter have no direct CRAN equivalent and are implemented manually; see the note under Running the Code below. |

## Data

The dataset is in the [data repository](https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch9) and is downloaded automatically by both scripts at runtime.

| File | Description |
|---|---|
| `Example_9_3_1.dta` | State-level macro panel: log state appropriations (`lny1`), log net tuition revenue (`lnx1`), log FTE enrollment (`lnx2`), log per capita income (`lnx3`); 48 states × 45 fiscal years (FY 1980–2024) |

## Running the Code

**Stata** (requires version 19; tested in Stata 19.5):
```stata
do Stata_code9.do
```

**R** (requires R 4.0 or later; R 4.4.x recommended):
```r
source("R_code9.R")
```

> **Note:** Output paths switch automatically based on the OS username (`if c(username) == "marvi"` in Stata). All other users receive the default relative-path output (`Output/graphs/`, `Output/tables/`, `Output/logs/`), which is created automatically at runtime.

> **Note on R implementation:** Most of this chapter's Stata commands have no direct CRAN equivalent and are implemented manually in `R_code9.R`. `xtpurt` (Herwartz-Siedenburg, Demetrescu-Hanck, and HMW variants) is approximated with `plm::purtest(test="ips")`; the Kao and Pedroni cointegration tests are reproduced via Engle-Granger two-step on fixed-effects residuals; the Westerlund (2007) ECM-based test is built from unit-specific ECM regressions; and the DCCE-MG ARDL-ECM model is constructed manually via unit-specific regressions with cross-sectional averages, followed by mean-group averaging. Results are distributional equivalents of the Stata output rather than numerically identical replication.

## Methods Covered

- **Panel unit root tests (Section 9.3.2):** Herwartz and Siedenburg (2008), Demetrescu and Hanck (2012), and Herwartz-Maxand-Walle (2019) variants applied to levels and first-differenced variables via `xtpurt`
- **Panel cointegration tests (Section 9.3.3):** Kao (1999) and Pedroni (2000, 2004) residual-based tests (`xtcointtest`); Westerlund (2005) variance ratio test; Westerlund (2007) ECM-based panel cointegration test (`xtwest`) with Gt, Ga, Pt, and Pa statistics
- **Cross-sectional dependence test (Section 9.3.4):** Pesaran CD test on all four panel variables (`xtcdf`)
- **Slope homogeneity test (Section 9.3.5):** Pesaran-Yamagata / Blomquist-Westerlund Δ-tilde statistic (`xthst`) in first-differenced and levels specifications
- **HCR with DCCE-MG ARDL-ECM (Section 9.3.6):** main model with `cr_lags(3 3 3 3)` and `lr_options(ardl)`; post-estimation weak cross-sectional dependence check (`xtcd2`); alternative `lr_options(xtpmg)` specification with cross-sectional exponent estimation; individual state error-correction coefficients via `showindividual`

## Output

Running the scripts produces Figs. 9.1 and 9.2 (state-by-state trends in log appropriations and log per capita income) and the estimation output tables for all diagnostic tests and the DCCE-MG model, written to `Output/graphs/` and `Output/logs/` respectively. Stata figures carry a `_Stata.png` suffix and R figures a `_R.png` suffix. All figures are produced in grayscale for Springer monochrome print compatibility.

## Related Chapters

This chapter builds directly on Chapter 8, which introduces the ARDL-ECM framework in a single-equation time series and symmetric panel setting. Chapter 9 extends that foundation to the macro panel case, where T is large relative to N, slope heterogeneity across units must be allowed, and cross-sectional dependence invalidates standard panel estimators. Chapter 10 then shifts from long-run equilibrium relationships to causal identification in shorter panels, introducing regression discontinuity and difference-in-differences methods.
