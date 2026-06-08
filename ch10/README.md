# Chapter 10 — Causal Inference and Marginal Treatment Effects

**Higher Education Policy Analysis Using Quantitative Techniques, 2nd Edition**
Marvin A. Titus | Springer

**Repository:** https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch10

---

## Overview

This folder contains the complete replication code for Chapter 10, which covers three methodological areas across two parts:

- **Part A (Sections 10.2–10.9):** Quasi-experimental causal inference. Section 10.2 introduces regression discontinuity design (RDD) using a synthetic, HSLS:09-calibrated merit-scholarship dataset. Sections 10.3–10.9 apply difference-in-differences family methods to Georgia's higher education consolidation policy, using a SHEEO state-level finance panel, and Section 10.7.4 adds the Wooldridge Extended TWFE estimator.
- **Part B (Sections 10.10–10.16):** Marginal treatment effects (MTE) and policy-relevant treatment effects (MPRTE) applied to returns to master's degree completion, using a synthetic B&B-mirroring individual-level dataset.

All results are based on synthetic data and are intended to illustrate methods only.

---

## Files

The chapter ships with a complete implementation in each language. The Stata implementation is modular (a master do-file that calls four section do-files); the R implementation is provided as a single self-contained script.

| File | Description |
|------|-------------|
| `Stata_code10.do` | Stata master script; sets paths, installs packages, and runs the four section do-files in order (tested in Stata 19.5) |
| `RDD.do` | Stata — Section 10.2, regression discontinuity design |
| `Georgia_DiD.do` | Stata — Sections 10.3–10.9, difference-in-differences family |
| `ETWFE.do` | Stata — Section 10.7.4, Extended TWFE |
| `MTE_MPRTE.do` | Stata — Sections 10.10–10.16, marginal treatment effects |
| `R_code10.R` | Complete, self-contained R translation of the entire chapter; all four sections inlined, runs top to bottom (tested in R 4.4.x) |

The R script reproduces the full chapter in one file: it sets up shared paths and logging, loads all packages once, defines the grayscale `theme_springer()` plotting theme, and then executes the RDD, DiD, ETWFE, and MTE/MPRTE sections in sequence.

---

## Data

Both implementations download their data automatically from the book's GitHub data repository at startup. No manual download is required. The R script reads CSV by default and falls back to `.dta` via `haven::read_dta()` when only the Stata file is available.

| Dataset | Used in | Description |
|---------|---------|-------------|
| `Example_10_3_1.csv` | Part A (10.3–10.9) | SHEEO state-level finance panel, 16 SREB states, FY 2001–2021 |
| `Example_10_7_3.csv` | Part A (10.7) | Multi-state staggered adoption panel (48 states); falls back to a synthetic staggered panel if unavailable |
| `Example_7_5_3_updated.csv` / `.dta` | Part B | Synthetic B&B panel with pre-generated `ma_*` program-area fields |
| `Example_7_5_3.csv` / `.dta` | Part B (fallback) | Base synthetic B&B panel; `ma_*` variables generated at runtime |

Section 10.2 (RDD) does not download a file: it generates a synthetic HSLS:09-calibrated dataset in-script (N = 4,000; merit cutoff at HS GPA = 3.25) and saves it as `ch10_rdd_hsls09_synthetic.rds` / `.csv`.

---

## Methods Covered

### Part A — Causal Inference

| Section | Method | Stata command(s) | R package / function |
|---------|--------|-----------------|----------------------|
| 10.2 | Sharp & fuzzy regression discontinuity | `rdrobust`, `rddensity`, `ivregress 2sls` | `rdrobust`, `rddensity`, `AER::ivreg` |
| 10.3 | Two-Way Fixed Effects DiD | `xtreg`, `reghdfe` | `fixest::feols` |
| 10.4 | LASSO-residualized DiD (double selection) | `lasso2`/`rlasso`, `reghdfe` | `hdm::rlasso`, `fixest` |
| 10.5 | Synthetic Control Method (SCM) | `synth` | `Synth::synth`, `dataprep` |
| 10.6 | Synthetic DiD (SDID) | `sdid` | `sdid::sdid` |
| 10.7 | Callaway–Sant'Anna CS-DiD (single + staggered) | `csdid`, `estat` | `did::att_gt`, `aggte` |
| 10.7.4 | Extended TWFE (Wooldridge) | `jwdid` | `etwfe::etwfe`, `marginaleffects::emfx` |
| 10.8 | Permutation inference, leave-one-out | `permute` | base R |

### Part B — Marginal Treatment Effects

| Section | Method | Stata command(s) | R package / function |
|---------|--------|-----------------|----------------------|
| 10.10 | Probit first stage, propensity score | `probit`, `predict` | `glm` (binomial probit) |
| 10.11 | MTE polynomial estimation (quadratic, cubic) | `reg` with interactions | `lm` |
| 10.11 | MTE by graduate program area | `reg` with field interactions | `lm` |
| 10.12 | Cluster bootstrap SEs | manual `forvalues` loop | base R loop over state clusters |
| 10.13 | Heckman selection model | `heckman` (two-step + ML) | `sampleSelection::heckit` |
| 10.14 | PRTE / MPRTE (Scenarios 1–8) | manual local macros | base R helper function |
| 10.15 | MTE visualization | `twoway`, `rarea` | `ggplot2` (`geom_ribbon`, etc.) |
| 10.16 | Cost-benefit analysis | Stata matrix ops | base R |

**Implementation notes.** The R `etwfe`/`marginaleffects` pair replicates Stata's `jwdid` plus its `estat` aggregations (simple, group, calendar, event). The MTE cluster bootstrap is implemented as an explicit base-R loop over state clusters (no separate package). Stata's `mtefe` has no direct R equivalent, so its ATE/ATT/ATU/LATE output is reproduced with the manual polynomial MTE estimator and labelled accordingly.

---

## Output

Both implementations write figures to `Output/graphs/`, tables to `Output/tables/`, and a log file to `Output/logs/`. These directories are created automatically at runtime. In R, each figure is both written to disk and printed to the active graphics device.

### Figures

Figure numbering follows the chapter text. Internal code names that differ from the published figure numbers (e.g. several diagnostic sub-figures) are listed by their on-disk filenames below.

| Chapter figure | Filename stem (Stata / R) | Content |
|----------------|---------------------------|---------|
| Fig 10.1 | `fig10_1_rdd_plot_credits` | Sharp RDD: scholarship effect on Year-1 credits |
| Fig 10.2 | `fig10_2_fuzzy_late_credits` | Fuzzy RDD LATE for Year-1 credits |
| Fig 10.3 | `fig10_3_parallel_trends` | Parallel trends: Georgia vs. SREB control states |
| Fig 10.4 | `fig10_4_scm_trends` | SCM: Georgia vs. synthetic Georgia |
| Fig 10.5 | `fig10_5_1_scm_gap` | SCM gap plot (Georgia − synthetic) |
| Fig 10.6 | `fig10_6_event_study` | Event study: Georgia consolidation |
| Fig 10.7 | `fig10_7_staggered_es` | CS-DiD staggered adoption event study |
| Fig 10.8 | `fig10_8_mte_curve` | Pooled MTE curve |
| Fig 10.9 | `fig10_9_mte_by_propensity` | MTE by propensity score |
| Fig 10.10 | `fig10_10_mte_byarea_curve` | MTE curves by graduate program area |
| Fig 10.11 | `fig10_11_mte_policy_regions` | MTE curve with policy-relevant regions |

Additional diagnostic figures are produced alongside the numbered chapter figures, including: RDD density-continuity test (`fig10_2_1`), RDD binned scatterplots (`fig10_2_2_rdd_binscatter_*`), RDD bandwidth sensitivity (`fig10_2_3`), RDD persistence/CGPA/first-stage plots (`fig10_2_4`, `fig10_2_6`, `fig10_2_7`, `fig10_2_8`); DiD robustness (`fig10_3_2`), LASSO comparison (`fig10_4_1`), CS-DiD single-cohort event study (`fig10_7_2`), permutation distribution (`fig10_8_1`), leave-one-out sensitivity (`fig10_8_2`), estimator comparison (`fig10_9_1`); and MTE by decile (`fig10_12`) and MPRTE by policy intensity (`fig10_14`).

Stata files carry a `_Stata.png` suffix and R files a `_R.png` suffix. All figures are produced in grayscale (`set scheme s2mono` in Stata; `theme_springer()` in R) for compatibility with Springer's black-and-white print requirements.

### Saved Datasets

| File | Description |
|------|-------------|
| `ch10_rdd_hsls09_synthetic.rds` / `.csv` | Synthetic RDD dataset generated in Section 10.2 |
| `results.csv` | DiD estimator summary (TWFE, placebo, robustness, LASSO, SCM) |
| `results_lasso.csv` | TWFE vs. LASSO-residualized DiD comparison |
| `results_combined.csv` | Combined TWFE / LASSO / SDID / CS-DiD coefficient table |
| `tab10_7_etwfe.csv` | ETWFE ATT estimates, unconditional vs. covariate-adjusted |
| `bb_mte_analysis.rds` / `.csv` | Individual-level MTE estimates and program-area flags |
| `mte_summary_by_field.csv` | Mean MTE by undergraduate field |
| `mte_summary_by_program_area.csv` | Mean MTE by graduate program area (treated only) |

---

## Software Requirements

### Stata
- Stata 19 or later (tested in Stata 19.5)
- Required user-written packages: `rdrobust`, `rddensity`, `reghdfe`, `ftools`, `lasso2`, `synth`, `sdid`, `csdid`, `drdid`, `jwdid`, `coefplot`
- Install with `ssc install <package>` or `net install <package>`

### R
- R 4.4.x or later
- The `etwfe` package requires `fixest >= 0.13.2`. If an older `fixest` is already installed, update it in a fresh R session (`install.packages("fixest")`) before running.
- Required CRAN packages:

```r
# RDD (Section 10.2)
rdrobust, rddensity

# DiD / causal inference (Sections 10.3–10.9)
fixest, did, Synth, sdid, hdm

# Extended TWFE (Section 10.7.4) — needs fixest >= 0.13.2
etwfe, marginaleffects

# MTE / MPRTE (Part B)
AER, sampleSelection, sandwich, lmtest, truncnorm

# Shared
dplyr, tidyr, ggplot2, haven
```

The `Synth`, `sdid`, and `hdm` packages are loaded with graceful fallback: if any is unavailable, the corresponding section is skipped (or, for `hdm`, falls back to the full control set) and the script continues.

---

## Reproducibility Notes

- Seeds are set at the start of each stochastic block for replicable results: `20260510` (RDD data generation), `20251130` (Part B program-area assignment), `20260101` (MTE cluster bootstrap), and `20260511` (staggered-panel and SDID placebo inference).
- The `u`-grid for MTE visualization evaluates the MTE polynomial at identical grid points across platforms: `gen u = _n/100` in Stata and `seq(0.01, 1.00, length.out = 100)` in R.
- Working-directory paths switch automatically based on the OS username. Users other than the author receive the default relative-path output (`Output/graphs/`, `Output/tables/`, `Output/logs/`).
- Because several financial variables are logged, the R script filters on `is.finite()` (not `complete.cases()`) so that any `log(0) = -Inf` rows are dropped before estimation, keeping the model frame and post-estimation aggregations aligned.
- All results are based on synthetic data and are intended to illustrate methods only.

---

> **NOTE:** Code development was assisted by Claude (Anthropic). The author provided specifications and reviewed, tested, and validated all code.
