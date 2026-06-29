# Chapter 11 — Instrumental Variables, CATE, MTE/MPRTE, and Policy Cost-Benefit Analysis

This folder contains the Stata and R code supporting Chapter 11 of *Higher Education Policy Analysis Using Quantitative Techniques* (2nd Edition, Springer). The chapter applies instrumental variables (IV) estimation, conditional average treatment effects (CATE), marginal treatment effects (MTE) and marginal policy-relevant treatment effects (MPRTE), and policy cost-benefit analysis (CBA) to a synthetic panel dataset on master's degree completion.

## Empirical Setting

The chapter's running example uses a synthetic dataset modeled on the Baccalaureate and Beyond (B&B) longitudinal study, examining the effect of master's degree completion on labor market outcomes. State-level GA (Georgia) higher education funding serves as the instrument for IV identification, motivating the heterogeneity and policy-relevance extensions developed across the CATE and MTE/MPRTE sections.

## File Structure

### Stata Scripts

| File | Description |
|---|---|
| `Stata_code11.do` | Master driver script. Loads/builds the synthetic dataset and calls the CATE and MTE/MPRTE sub-scripts in sequence. |
| `Synthetic_truncated_BB.do` | Constructs the synthetic, truncated B&B-style panel dataset used throughout the chapter. |
| `CATE.do` | Implements conditional average treatment effect estimation: subgroup IV, interaction IV, and visualization of heterogeneous treatment effects. Produces CATE figures. |
| `MTE_MPRTE.do` | Implements marginal treatment effect and marginal policy-relevant treatment effect estimation, including the MTE curve and the policy cost-benefit analysis components. Produces MTE/MPRTE figures. |

### R Translations

| File | Stata Counterpart | Description |
|---|---|---|
| `R_code11.R` | `Stata_code11.do` | Master R script replicating the overall chapter pipeline. |
| `R_Synthetic_truncated_BB.R` | `Synthetic_truncated_BB.do` | R version of the synthetic dataset construction. |
| `CATE_R.R` | `CATE.do` | R implementation of the CATE estimation and visualization workflow. |
| `MTE_MPRTE_R.R` | `MTE_MPRTE.do` | R implementation of the MTE/MPRTE estimation and cost-benefit analysis workflow. |

## Running the Code

**Stata** (target version 19; tested for compatibility with `version 19`):
```stata
do Stata_code11.do
```
This will construct the synthetic dataset and call `CATE.do` and `MTE_MPRTE.do` in turn.

**R**:
```r
source("R_code11.R")
```
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

This chapter extends the instrumental variables/2SLS approach introduced in Chapter 7, reinterpreting the IV/2SLS estimate as a local average treatment effect (LATE) and building out the MTE/MPRTE and CBA framework around it. It also complements Chapter 10's state- and institution-level causal inference toolkit (DiD, SCM, SDID, CS-DiD, RDD): where Chapter 10 addresses treatment assigned to a small number of state/institutional units, Chapter 11 addresses individual-level selection into treatment.
