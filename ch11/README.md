# Chapter 11 — Instrumental Variables, CATE, MTE/MPRTE, and Policy Cost-Benefit Analysis

This folder holds the Stata and R code for Chapter 11 of *Higher Education Policy Analysis Using Quantitative Techniques* (2nd Edition, Springer). The chapter works through instrumental variables (IV) estimation, conditional average treatment effects (CATE), marginal treatment effects (MTE), marginal policy-relevant treatment effects (MPRTE), and policy cost-benefit analysis (CBA), all applied to a synthetic panel dataset on master's degree completion.

## Empirical Setting

The running example draws on a synthetic dataset built to mirror the Baccalaureate and Beyond (B&B) longitudinal study, with master's degree completion as the treatment and labor market outcomes as the dependent variables. State-level graduate assistantship (GA) funding is the instrument, chosen because it creates exogenous variation in the likelihood of graduate enrollment without directly affecting individual earnings — a feature that motivates the heterogeneity and policy-relevance extensions taken up in the CATE and MTE/MPRTE sections.

## File Structure

### Stata Scripts

| File | Description |
|---|---|
| `Stata_code11.do` | Master driver script. Calls `CATE.do` and `MTE_MPRTE.do`, in that order. Does not construct the dataset itself — see `Synthetic_truncated_BB.do` below. |
| `CATE.do` | Runs first. Implements conditional average treatment effect estimation: subgroup IV, interaction IV, and visualization of heterogeneous treatment effects. Produces CATE figures. |
| `MTE_MPRTE.do` | Runs second. Implements marginal treatment effect and marginal policy-relevant treatment effect estimation, including the MTE curve and the policy cost-benefit analysis components. Produces MTE/MPRTE figures. |
| `Synthetic_truncated_BB.do` | Standalone, one-time data-generation script. Builds the synthetic, truncated B&B-style panel dataset (`Example_7_5_3.dta`) used throughout the chapter. **Not called by `Stata_code11.do`** — run separately, only when the dataset needs to be (re)generated. |

### R Translations

| File | Stata Counterpart | Description |
|---|---|---|
| `R_code11.R` | `Stata_code11.do` | Master R script. Sources `CATE_R.R` and `MTE_MPRTE_R.R`, in that order. Does not construct the dataset itself. |
| `CATE_R.R` | `CATE.do` | Runs first. R implementation of the CATE estimation and visualization workflow. |
| `MTE_MPRTE_R.R` | `MTE_MPRTE.do` | Runs second. R implementation of the MTE/MPRTE estimation and cost-benefit analysis workflow. |
| `R_Synthetic_truncated_BB.R` | `Synthetic_truncated_BB.do` | Standalone, one-time R version of the synthetic dataset construction. **Not sourced by `R_code11.R`** — run separately, only when the dataset needs to be (re)generated. |

## Data

Datasets for this chapter are in the [data repository](https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch11). The file the current scripts actually load:

| File | Used by | Description |
|---|---|---|
| `Example_7_5_3.dta` | `CATE.do`/`CATE_R.R`, `MTE_MPRTE.do`/`MTE_MPRTE_R.R`; produced by `Synthetic_truncated_BB.do`/`R_Synthetic_truncated_BB.R` | The dataset actually loaded by every script run today — synthetic B&B-style panel (N = 8,000) on master's degree completion. `ma_*` program-area variables are generated at runtime rather than pre-loaded. |

> **Note:** All four sub-scripts try `Example_7_5_3_updated.dta` first — a version with `ma_*` variables pre-generated — then fall back to `Example_7_5_3.dta` if it isn't found. Since `Example_7_5_3_updated.dta` isn't currently in `data/ch11`, the fallback is what every run actually uses. Consider either adding the updated file or simplifying the loading logic to target `Example_7_5_3.dta` directly.

## Running the Code

**Prerequisite:** `Example_7_5_3.dta` must be in the working directory before running either master script. If it isn't, `MTE_MPRTE.do` and `MTE_MPRTE_R.R` will attempt to download it from the data repository — the first attempt (for the `_updated` version) will fail since that file doesn't exist there yet, but the second attempt pulls `Example_7_5_3.dta` directly and succeeds. `CATE.do` and `CATE_R.R` do not have their own download logic, so they require the file to already be present locally. To build the dataset from scratch, run the standalone generator first:
```stata
do Synthetic_truncated_BB.do
```

**Stata** (target version 19; tested for compatibility with `version 19`):
```stata
do Stata_code11.do
```
This calls `CATE.do` followed by `MTE_MPRTE.do`.

**R**:
```r
source("R_code11.R")
```
This sources `CATE_R.R` followed by `MTE_MPRTE_R.R`. To regenerate the dataset instead, run `R_Synthetic_truncated_BB.R` first.

Required packages cover IV and heterogeneous treatment effect estimation; the book's shared `theme_springer()` plotting theme is also needed (see Chapter 1 setup or the repository root for shared utilities).

> **Note:** Path setup uses username-conditional branching (`if c(username) == "marvi"` in Stata) with a GitHub raw-URL fallback, following the same convention used across the rest of the 2nd edition codebase.

## Methods Covered

- **Instrumental variables (IV):** identification using state-level graduate assistantship (GA) funding as an instrument
- **Conditional average treatment effects (CATE):** subgroup IV, interaction IV, and heterogeneity visualization
- **Marginal treatment effects (MTE) and MPRTE:** local instrument variation, policy-relevant treatment parameters
- **Cost-benefit analysis (CBA):** policy evaluation built on the MTE/MPRTE framework

## Output

Running the scripts produces the figures and tables referenced in Chapter 11 — CATE plots and MTE/MPRTE curves — rendered in Stata's `s2mono` scheme for Springer monochrome print compatibility.

## Related Chapters

Chapter 7 introduced the IV/2SLS framework this chapter builds on; here that estimate is recast as a local average treatment effect (LATE), then extended into the MTE/MPRTE and CBA framework. Chapter 10 is the natural companion for state- and institution-level causal inference (DiD, SCM, SDID, CS-DiD, RDD) — the two chapters approach identification from different angles, with Chapter 10 handling treatment assigned across a small number of units and Chapter 11 focusing on individual-level selection.
