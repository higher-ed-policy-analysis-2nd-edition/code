# Chapter 10 — Causal Inference and Marginal Treatment Effects

**Higher Education Policy Analysis Using Quantitative Techniques, 2nd Edition**
Marvin A. Titus | Springer

**Repository:** https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch10

---

## Overview

This folder contains the complete replication code for Chapter 10, which covers two methodological areas:

- **Part A (Sections 10.3–10.9):** Quasi-experimental causal inference methods applied to Georgia's higher education consolidation policy, using a SHEEO state-level finance panel.
- **Part B (Sections 10.10–10.16):** Marginal treatment effects (MTE) and policy-relevant treatment effects (MPRTE) applied to returns to master's degree completion, using a synthetic B&B-mirroring individual-level dataset.

---

## Files

| File | Description |
|------|-------------|
| `Stata_code10.do` | Complete Stata implementation (tested in Stata 19.5) |
| `R_code10.R` | Complete R translation (tested in R 4.4.x) |

---

## Data

Both scripts download their data automatically from the book's GitHub data repository at startup. No manual download is required.

| Dataset | Used in | Description |
|---------|---------|-------------|
| `Example_10_3_1.csv` | Part A | SHEEO state-level finance panel, 16 SREB states |
| `Example_10_7_3.csv` | Part A (Section 10.7) | Multi-state staggered adoption panel |
| `Example_7_5_3_updated.dta` | Part B | Synthetic B&B panel with pre-generated `ma_*` fields |
| `Example_7_5_3.dta` | Part B (fallback) | Base synthetic B&B panel; `ma_*` variables generated at runtime |

---

## Methods Covered

### Part A — Causal Inference (Georgia Consolidation)

| Section | Method | Stata command(s) | R package(s) |
|---------|--------|-----------------|--------------|
| 10.3 | Two-Way Fixed Effects DiD | `xtreg`, `reghdfe` | `fixest` |
| 10.4 | LASSO-Residualized DiD | `lasso2`, `reghdfe` | `hdm` |
| 10.5 | Synthetic Control Method (SCM) | `synth`, `synth_runner` | `Synth` |
| 10.6 | Synthetic DiD (SDiD) | `sdid` | `synthdid` |
| 10.7 | Callaway-Sant'Anna CS-DiD (staggered) | `csdid` | `did` |
| 10.8 | Permutation inference, leave-one-out | `permute` | base R |

### Part B — Marginal Treatment Effects

| Section | Method | Stata command(s) | R package(s) |
|---------|--------|-----------------|--------------|
| 10.10 | Probit first stage, propensity score | `probit`, `predict` | `glm` (binomial probit) |
| 10.11 | MTE polynomial estimation (quadratic, cubic) | `reg` with interactions | `lm` |
| 10.11 | MTE by graduate program area | `reg` with field interactions | `lm` |
| 10.12 | Cluster bootstrap SEs | manual `forvalues` loop | base R |
| 10.12 | Wild cluster bootstrap | `boottest` | `fwildclusterboot` |
| 10.13 | Heckman selection model | `heckman` | `sampleSelection` |
| 10.14 | PRTE / MPRTE (Scenarios 1–8) | manual local macros | base R |
| 10.15 | MTE visualization | `twoway`, `rarea` | `ggplot2` |
| 10.16 | Cost-benefit analysis | Stata matrix ops | base R |

---

## Output

Both scripts write figures to `Output/graphs/` and a log file to `Output/logs/`. These directories are created automatically at runtime.

### Figures

| Figure | Filename (Stata) | Filename (R) | Content |
|--------|-----------------|--------------|---------|
| Fig 10.1 | `fig10_1_parallel_trends_Stata.png` | `fig10_1_parallel_trends_R.png` | Parallel trends: treatment vs. control |
| Fig 10.2 | `fig10_2_synth_control_Stata.png` | `fig10_2_synth_control_R.png` | SCM: Georgia vs. synthetic Georgia |
| Fig 10.3 | `fig10_3_sdid_Stata.png` | `fig10_3_sdid_R.png` | Synthetic DiD estimate |
| Fig 10.4 | `fig10_4_event_study_Stata.png` | `fig10_4_event_study_R.png` | Event study coefficients |
| Fig 10.5 | `fig10_5_csdid_staggered_Stata.png` | `fig10_5_csdid_staggered_R.png` | CS-DiD staggered adoption event study |
| Fig 10.6 | `fig10_6_mte_curve_Stata.png` | `fig10_6_mte_curve_R.png` | Pooled MTE curve |
| Fig 10.7 | `fig10_7_mte_by_decile_Stata.png` | `fig10_7_mte_by_decile_R.png` | MTE by propensity score decile |
| Fig 10.8 | `fig10_8_mte_byarea_curve_Stata.png` | `fig10_8_mte_byarea_curve_R.png` | MTE curves by graduate program area |
| Fig 10.9 | `fig10_9_mprte_by_intensity_Stata.png` | `fig10_9_mprte_by_intensity_R.png` | MPRTE by policy intensity |
| Fig 10.10 | `fig10_10_mte_policy_regions_Stata.png` | `fig10_10_mte_policy_regions_R.png` | MTE curve with policy-relevant regions |
| Fig 10.11 | `fig10_11_mte_by_propensity_Stata.png` | `fig10_11_mte_by_propensity_R.png` | MTE by propensity score distribution |

All figures are produced in grayscale (`set scheme s2mono` in Stata; `theme_springer_bw()` in R) for compatibility with Springer's black-and-white print requirements.

### Saved Datasets

| File | Description |
|------|-------------|
| `bb_mte_analysis.dta` | Individual-level MTE estimates and program area flags |
| `mte_summary_by_field.csv` | Mean MTE by undergraduate field |
| `mte_summary_by_program_area.csv` | Mean MTE by graduate program area (treated only) |
| `results_combined.csv` | TWFE and LASSO DiD coefficient table |

---

## Software Requirements

### Stata
- Stata 19 or later (tested in Stata 19.5)
- Required user-written packages: `reghdfe`, `ftools`, `lasso2`, `synth`, `synth_runner`, `sdid`, `csdid`, `boottest`, `coefplot`
- Install with: `ssc install <package>` or `net install <package>`

### R
- R 4.4.x or later
- Required CRAN packages (installed automatically at startup):

```r
# Part A
readr, dplyr, tidyr, ggplot2, scales, patchwork, janitor,
plm, fixest, clubSandwich, lmtest, sandwich,
hdm, Synth, synthdid, did, modelsummary

# Part B
haven, ivreg, sampleSelection

# Optional (wild cluster bootstrap)
fwildclusterboot
```

---

## Reproducibility Notes

- Both scripts set `seed 20251130` (Stata) / `set.seed(20251130)` (R) at the start of Part A, and `seed 20260101` / `set.seed(20260101)` for the Part B cluster bootstrap, ensuring replicable results.
- The `u`-grid for MTE visualization uses `gen u = _n/100` (Stata) and `(1:100)/100` (R) to evaluate the MTE polynomial at identical grid points across platforms.
- Working directory paths switch automatically based on the OS username. Users other than the author should use the default relative-path output (`Output/graphs/`, `Output/logs/`).
- All results are based on synthetic data and are intended to illustrate methods only.

---

> **NOTE:** Code development was assisted by Claude (Anthropic). The author provided specifications and reviewed, tested, and validated all code.
