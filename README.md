# Chapter 10: Causal Inference and Marginal Treatment Effects

**Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)**
Marvin A. Titus | Springer

---

## Overview

This directory contains all replication code for Chapter 10, which is organized into two substantive parts.

**Part A (Sections 10.3–10.9)** applies multiple causal inference methods to evaluate the impact of Georgia's higher education consolidation policy using a state-level finance panel. Methods covered include two-way fixed effects difference-in-differences (TWFE DiD), LASSO-residualized DiD, the Synthetic Control Method (SCM), Synthetic DiD, Callaway-Sant'Anna staggered adoption estimators, permutation inference, and leave-one-out sensitivity analysis.

**Part B (Sections 10.10–10.16)** develops a Marginal Treatment Effects (MTE) framework to estimate heterogeneous returns to master's degree completion. Using state-funded graduate assistantship (GA) amounts as an instrument, the analysis estimates the MTE curve, derives policy-relevant treatment effects (ATE, ATT, ATU, LATE), and computes Policy-Relevant Marginal Treatment Effects (MPRTE) for a range of simulated policy scenarios. Part B concludes with a cost-benefit analysis linking MPRTE estimates to net present value calculations.

---

## Files in This Directory

| File | Description |
|------|-------------|
| `Stata_code10.do` | Complete Stata replication script for Parts A and B (Stata 19+) |
| `R_code10.R` | R translation of the complete Stata script (R 4.4.x) |
| `Synthetic_truncated_BB.do` | Stata script to generate the synthetic B&B dataset (`Example_7_5_3.dta`) |

---

## Data

| File | Description | Source |
|------|-------------|--------|
| `Example_10_3_1.csv` | State-level higher education finance panel, SREB states | SHEEO |
| `Example_7_5_3.dta` | Synthetic B&B panel, master's degree completion and earnings (N = 8,000) | Synthetic — see note below |

`Example_10_3_1.csv` is downloaded automatically at the top of `Stata_code10.do` and `R_code10.R` from the chapter's GitHub data repository. `Example_7_5_3.dta` must be generated locally by running `Synthetic_truncated_BB.do` before executing the main analysis scripts, or downloaded separately from the data repository.

> **Note on synthetic data.** The B&B panel used in Part B is a synthetic dataset calibrated to mirror the characteristics of the NCES Baccalaureate and Beyond Longitudinal Study (B&B:08/18). Synthetic data are used in place of actual B&B restricted-use data for three reasons: (1) B&B restricted-use files require an NCES data license; (2) known true parameter values allow readers to validate their results; and (3) the same dataset is used continuously across Chapters 7 and 10, supporting cumulative learning. All results based on this dataset are illustrative and should not be interpreted as estimates from actual B&B data.

---

## Synthetic Dataset Structure (`Example_7_5_3.dta`)

The synthetic dataset contains 8,000 observations representing bachelor's degree recipients. Key variables are organized into the following groups:

| Group | Variables |
|-------|-----------|
| Demographics | `female`, `white`, `black`, `hispanic`, `asian`, `other_race`, `age_ba` |
| Family background | `firstgen`, `parent_income_q`, `parent_grad` |
| Academic background | `ugpa`, `stem_major`, `bus_major`, `ed_major`, `socsci_major`, `selective_inst`, `public_ug` |
| Labor market context | `state_unemp`, `metro`, `state` |
| Instrument | `ga_funding`, `ga_funding_adj` |
| Latent factors | `eta_ability`, `eta_taste`, `eta_prod` |
| Treatment selection | `z_masters`, `p_masters`, `u_d`, `masters` |
| Graduate program area | `ma_stem`, `ma_business`, `ma_education`, `ma_health`, `ma_other` |
| Potential outcomes | `ln_salary_0`, `te_masters`, `ln_salary_1`, `ln_salary`, `salary` |

Field-specific returns to the master's degree are calibrated to the wage-premium literature by graduate program area: Health & Related (+0.14 log points), STEM (+0.10), Business (+0.08), Education (+0.04), and Other (0.00 baseline).

---

## R Packages Required

The R script (`R_code10.R`) requires the following packages, which are installed automatically on first run if not already present:

**Part A:** `readr`, `dplyr`, `tidyr`, `ggplot2`, `scales`, `patchwork`, `janitor`, `plm`, `fixest`, `clubSandwich`, `lmtest`, `sandwich`, `hdm`, `Synth`, `synthdid`, `did`, `modelsummary`

**Part B:** `haven`, `ivreg`, `sampleSelection`

**Optional:** `fwildclusterboot` (wild cluster bootstrap; falls back to sandwich clustered standard errors if unavailable)

> **Note on R equivalents.** Two Stata commands used in Part B have no direct CRAN equivalents: `mtefe` (manual polynomial MTE is implemented directly) and `synth_runner` (noted inline). The R script is tested in R 4.4.x.

---

## Output

Running either `Stata_code10.do` or `R_code10.R` produces figures and a log file, saved to `Output/graphs/` and `Output/logs/` respectively.

### Figures

| File | Figure | Description |
|------|--------|-------------|
| `fig10_1_trends_Stata.png` | Fig. 10.1 | Parallel trends — Georgia vs. control states |
| `fig10_2_event_study_Stata.png` | Fig. 10.2 | Event study plot — TWFE coefficients |
| `fig10_3_scm_Stata.png` | Fig. 10.3 | Synthetic Control — actual vs. synthetic Georgia |
| `fig10_4_sdid_Stata.png` | Fig. 10.4 | Synthetic DiD weights and estimates |
| `fig10_5_cs_did_Stata.png` | Fig. 10.5 | Callaway-Sant'Anna group-time ATTs |
| `fig10_6_permutation_Stata.png` | Fig. 10.6 | Permutation inference distribution |
| `fig10_7_loo_Stata.png` | Fig. 10.7 | Leave-one-out sensitivity |
| `fig10_8_mte_curve_Stata.png` | Fig. 10.8 | Estimated MTE curve — pooled cubic polynomial |
| `fig10_9_mprte_by_intensity_Stata.png` | Fig. 10.9 | MPRTE by GA funding intensity |
| `fig10_10_mte_policy_regions_Stata.png` | Fig. 10.10 | MTE curve and policy-relevant margins |
| `fig10_11_mte_by_propensity_Stata.png` | Fig. 10.11 | MTE by propensity score decile |

R equivalents are saved with the suffix `_R.png` in the same directory.

### Logs

| File | Description |
|------|-------------|
| `Output/logs/Chapter10_Stata_output.log` | Full Stata output including all estimation results, MPRTE estimates, and CBA summary |
| `Output/logs/Chapter10_R_output.log` | Full R output, cross-validating key Part B estimates |

---

## Replication Notes

- `Stata_code10.do` was developed and tested in **Stata 19.5**. Stata 19 or later is required.
- `R_code10.R` was tested in **R 4.4.x**.
- Both scripts include automatic output path-switching based on the system username (`c(username)` in Stata; `Sys.info()[["user"]]` in R). Users should either add their own username block or set the output directory globals and variables manually at the top of each script.
- `set seed 20251130` is used throughout to ensure reproducibility. Results may differ across Stata and R due to differences in random number generators and numerical optimization routines, but key estimates should be substantively consistent.
- All results are based on synthetic data and are intended to illustrate methods only. They should not be interpreted as estimates from actual survey data.

---

## Correspondence

Questions regarding the code or data should be directed to the author via the book's GitHub organization: [https://github.com/higher-ed-policy-analysis-2nd-edition](https://github.com/higher-ed-policy-analysis-2nd-edition)
