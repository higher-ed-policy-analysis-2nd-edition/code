# Chapter 10 — Regression Discontinuity, Difference-in-Differences, and Related Quasi-Experimental Methods

This folder holds the Stata and R code for Chapter 10 of *Higher Education Policy Analysis Using Quantitative Techniques* (2nd Edition, Springer). The chapter covers the core quasi-experimental toolkit: regression discontinuity design (RDD), difference-in-differences (DiD) and its extensions (LASSO-DiD, SCM, SDID, CS-DiD), and the Wooldridge Extended TWFE estimator (ETWFE).

> **Note:** Instrumental variables, conditional average treatment effects (CATE), marginal treatment effects (MTE/MPRTE), and policy cost-benefit analysis are covered in Chapter 11. See the `ch11` folder and `Stata_code11.do` / `R_code11.R`.

## Empirical Settings

The chapter uses two distinct empirical applications, each in a separate sub-script:

**Merit-based scholarship (Section 10.2).** A synthetic, HSLS:09-calibrated dataset (N = 4,000) is generated in-script using a GPA cutoff at 3.25. This is the running example for sharp and fuzzy RDD, bandwidth and polynomial sensitivity analysis, and validity checks. No external data file is required; the script saves the generated dataset as `ch10_rdd_hsls09_synthetic.rds` / `.csv` at runtime.

**Georgia higher education consolidation (Sections 10.3–10.9).** A SHEEO state-level finance panel (16 SREB states, FY 2001–2021) and a 48-state staggered adoption panel are downloaded from the book's data repository at startup. These support the DiD family of methods and the ETWFE extension.

## File Structure

### Stata Scripts

| File | Description |
|---|---|
| `Stata_code10.do` | Master driver. Sets output paths and log, installs packages, then calls `RDD.do`, `Georgia_DiD.do`, and `ETWFE.do` in sequence. |
| `RDD.do` | Section 10.2. Sharp and fuzzy RDD applied to the merit scholarship cutoff (GPA = 3.25). Generates and saves the synthetic HSLS:09 dataset. |
| `Georgia_DiD.do` | Sections 10.3–10.9. TWFE DiD, LASSO-residualized DiD, synthetic control (SCM), synthetic DiD (SDID), Callaway–Sant'Anna CS-DiD (single cohort and staggered), permutation inference, and leave-one-out sensitivity. |
| `ETWFE.do` | Section 10.7.4. Wooldridge Extended TWFE via `jwdid`, applied to the three-state staggered adoption design with never-treated controls. Unconditional and covariate-adjusted specifications. |

### R Translations

| File | Stata Counterpart | Description |
|---|---|---|
| `R_code10.R` | `Stata_code10.do` | Master R script. Sets shared paths and logging, defines `theme_springer()`, and sources the section scripts in order. |
| `R_code10_RDD.R` | `RDD.do` | Section 10.2 RDD workflow in R. |
| `R_code10_Georgia_DiD.R` | `Georgia_DiD.do` | Sections 10.3–10.9 DiD family workflow in R. |
| `R_code10_ETWFE.R` | `ETWFE.do` | Section 10.7.4 Extended TWFE via the `etwfe` package and `marginaleffects::emfx()` aggregations. |

Each R section script can also be sourced independently after `R_code10.R` has been run once to establish the shared environment (paths, packages, plotting theme).

## Data

Datasets are in the [data repository](https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch10). Both implementations download data automatically at startup; no manual download is required. R reads CSV by default and falls back to `.dta` via `haven::read_dta()` when only the Stata file is available.

| File | Used in | Description |
|---|---|---|
| `Example_10_3_1.csv` | Sections 10.3–10.9 | SHEEO state-level finance panel, 16 SREB states, FY 2001–2021 |
| `Example_10_7_3.csv` | Section 10.7 | Multi-state staggered adoption panel (48 states); falls back to a synthetic staggered panel if unavailable |

The Section 10.2 RDD analysis generates its own synthetic dataset in-script and does not require an external data file.

## Running the Code

**Stata** (requires version 19; tested in Stata 19.5):
```stata
do Stata_code10.do
```
This calls `RDD.do`, then `Georgia_DiD.do`, then `ETWFE.do`.

**R** (requires R 4.4.x or later):
```r
source("R_code10.R")
```
This sources `R_code10_RDD.R`, `R_code10_Georgia_DiD.R`, and `R_code10_ETWFE.R` in order. The `etwfe` package requires `fixest >= 0.13.2`; if an older version is installed, update it in a fresh R session before running.

> **Note:** Output paths switch automatically based on the OS username (`if c(username) == "marvi"` in Stata). All other users receive the default relative-path output (`Output/graphs/`, `Output/tables/`, `Output/logs/`), which is created automatically at runtime.

## Methods Covered

- **Sharp and fuzzy regression discontinuity (RDD):** local linear estimation via `rdrobust`, bandwidth and polynomial sensitivity, density continuity test (`rddensity`), placebo cutoffs, donut RD, and augmented subgroup checks
- **Two-Way Fixed Effects DiD (TWFE):** baseline DiD with unit and time fixed effects; parallel trends assessment and robustness checks
- **LASSO-residualized DiD:** double-selection LASSO via `lassopack`/`rlasso` to residualize both outcome and treatment indicator before DiD estimation
- **Synthetic Control Method (SCM):** Georgia vs. synthetic Georgia using donor-pool weighting (`synth`)
- **Synthetic DiD (SDID):** `sdid` (Stata) / `synthdid` and `augsynth` (R)
- **Callaway–Sant'Anna CS-DiD:** doubly-robust ATT(g,t) for a single treated cohort and for staggered adoption across three treatment cohorts (`csdid`/`did`)
- **Extended TWFE (Wooldridge):** heterogeneity-robust ATT(g,t) via `jwdid` (Stata) and `etwfe`/`marginaleffects` (R); unconditional and covariate-adjusted specifications, never-treated controls
- **Permutation inference and leave-one-out sensitivity:** non-parametric tests of the DiD estimate's robustness to the choice of control units

## Output

Running the scripts produces the figures and tables referenced in Chapter 10 — RDD plots, parallel trends and event-study figures, SCM and SDID trend comparisons, CS-DiD aggregations, the ETWFE event-study panel, permutation distributions, and the cross-estimator comparison — rendered in Stata's `s2mono` scheme and R's `theme_springer()` for Springer monochrome print compatibility. Tables are written as `.rtf` (Stata) and `.csv` (R) to `Output/tables/`.

## Related Chapters

This chapter pairs with Chapter 11, which addresses individual-level selection into treatment using the same SHEEO-linked context: where Chapter 10 assigns treatment to institutions or states, Chapter 11 uses IV/2SLS to handle student-level endogenous selection and builds the MTE and MPRTE framework from the resulting LATE estimates. The ETWFE estimator in Section 10.7.4 complements the CS-DiD approach in Section 10.7 and is directly comparable to the Callaway–Sant'Anna staggered ATT(g,t) estimates produced in the same sub-section.
