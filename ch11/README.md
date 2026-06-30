# Chapter 11 — Instrumental Variables, CATE, MTE/MPRTE, and Policy Cost-Benefit Analysis

This folder contains the Stata and R code supporting Chapter 11 of *Higher Education Policy Analysis Using Quantitative Techniques* (2nd Edition, Springer). The chapter applies instrumental variables (IV) estimation, conditional average treatment effects (CATE), marginal treatment effects (MTE) and marginal policy-relevant treatment effects (MPRTE), and policy cost-benefit analysis (CBA) to a synthetic panel dataset on master's degree completion.

## Empirical Setting

The chapter's running example uses a synthetic dataset modeled on the Baccalaureate and Beyond (B&B) longitudinal study, examining the effect of master's degree completion on labor market outcomes. State-level graduate assistant (GA) higher education funding serves as the instrument for IV identification, motivating the heterogeneity and policy-relevance extensions developed across the CATE and MTE/MPRTE sections.

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

Datasets for this chapter are available in the [data repository](https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch11). The file referenced by the chapter's current scripts:

| File | Used by | Description |
|---|---|---|
| `Example_7_5_3.dta` | `CATE.do`/`CATE_R.R`, `MTE_MPRTE.do`/`MTE_MPRTE_R.R`; produced by `Synthetic_truncated_BB.do`/`R_Synthetic_truncated_BB.R` | The dataset actually loaded by every script run today — synthetic B&B-style panel (N = 8,000) on master's degree completion. `ma_*` program-area variables are generated at runtime rather than pre-loaded. |

> **Note:** The scripts are written to try `Example_7_5_3_updated.dta` first (a version with pre-generated `ma_*` variables) and fall back to `Example_7_5_3.dta` only if that file is missing. As of this writing, `Example_7_5_3_updated.dta` is **not present** in `data/ch11`, so the fallback path is the one actually exercised on every run — confirm whether the updated file should be added, or whether the primary/fallback logic should be simplified to reflect `Example_7_5_3.dta` as the sole dataset.

## Running the Code

**Prerequisite:** the dataset must already exist in the working directory, or be reachable via the scripts' GitHub raw-URL fallback (see Data, above). As of this writing, the scripts' first download attempt (for `Example_7_5_3_updated.dta`) will fail since that file isn't in the data repository; they then automatically fall back to downloading and using `Example_7_5_3.dta`, which succeeds. To regenerate the dataset from scratch instead, run the standalone generator first:
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

Required packages typically include those used for IV/heterogeneous treatment effect estimation and the book's shared `theme_springer()` plotting theme (see Chapter 1 setup or the repository root for shared utilities).

> **Note:** Scripts use username-conditional path branching (`if c(username) == "marvi"` in Stata) with a GitHub raw-URL fallback for data loading, consistent with the conventions used throughout the 2nd edition codebase.

## Methods Covered

- **Instrumental variables (IV):** identification using GA state funding as an instrument
- **Conditional average treatment effects (CATE):** subgroup IV, interaction IV, and heterogeneity visualization
- **Marginal treatment effects (MTE) and MPRTE:** local instrument variation, policy-relevant treatment parameters
- **Cost-benefit analysis (CBA):** policy evaluation built on the MTE/MPRTE framework

## Output

Running the scripts generates the figures and tables referenced in Chapter 11 (CATE plots and MTE/MPRTE curves), using the `s2mono` scheme in Stata for Springer monochrome print compatibility.

## Related Chapters

This chapter extends the instrumental variables/2SLS approach introduced in Chapter 7, reinterpreting the IV/2SLS estimate as a local average treatment effect (LATE) and building out the MTE/MPRTE and CBA framework around it. It also complements Chapter 10's state- and institution-level causal inference toolkit (DiD, SCM, SDID, CS-DiD, RDD): where Chapter 10 addresses treatment assigned to a small number of states, Chapter 11 addresses individual-level selection into treatment.
