# Chapter 11 — Instrumental Variables, CATE, MTE/MPRTE, and Policy Cost-Benefit Analysis

This folder holds the Stata and R code for Chapter 11 of *Higher Education Policy Analysis Using Quantitative Techniques* (2nd Edition, Springer). The chapter works through these methods:

- instrumental variables (IV) estimation, reinterpreted as a local average treatment effect (LATE);
- conditional average treatment effects (CATE);
- marginal treatment effects (MTE);
- policy-relevant and marginal policy-relevant treatment effects (PRTE and MPRTE);
- policy cost-benefit analysis (CBA).

All of them are applied to a synthetic dataset on master's degree completion.

## Empirical Setting

The running example uses a synthetic dataset built to mirror the Baccalaureate and Beyond (B&B) longitudinal study. Master's degree completion is the treatment and log annual salary is the outcome. State graduate assistantship (GA) funding is the instrument: it shifts the likelihood of completing a master's degree without directly affecting individual earnings. That property motivates the heterogeneity and policy-relevance extensions in the CATE and MTE/MPRTE sections.

## File Structure

### Stata Scripts

| File | Description |
|---|---|
| `Stata_code11.do` | Master driver script. Sets paths and the master log, installs packages, and calls `CATE.do` and `MTE_MPRTE.do`, in that order. Contains no estimation code. |
| `CATE.do` | Runs first (Section 11.1.3). Subgroup IV, interaction IV, and supplementary CATE figures. Opens its own log. |
| `MTE_MPRTE.do` | Runs second (Sections 11.1.1–11.1.2 and 11.2). OLS and IV/2SLS; MTE estimation with `mtefe` (quadratic pooled model, linear area-specific models); state-clustered and wild cluster bootstraps; PRTE for eight GA funding scenarios; cost-benefit analysis; the chapter's Figs. 11.1–11.2 and supplementary MTE figures. Opens its own log, which is the source of Tables 11.2–11.6. |
| `Synthetic_truncated_BB.do` | Standalone, one-time data-generation script that builds `Example_7_5_3.dta`. **Not called by `Stata_code11.do`.** Run it separately, and only when the dataset needs to be regenerated. |

### R Translations

| File | Stata Counterpart | Description |
|---|---|---|
| `R_code11.R` | `Stata_code11.do` | Master R script. Sets paths and the master log, installs missing packages, and sources `CATE_R.R` and `MTE_MPRTE.R`, in that order. Contains no estimation code. |
| `CATE_R.R` | `CATE.do` | Runs first. R implementation of the CATE estimation and figures. |
| `MTE_MPRTE.R` | `MTE_MPRTE.do` | Runs second. R implementation of the MTE/PRTE/CBA workflow. `mtefe` has no R port, so `mte_poly()` re-implements its parametric polynomial local-IV estimator; on the chapter data it reproduces the Stata point estimates to four decimal places. The Heckman models and wild cluster bootstraps are coded directly. |
| `R_Synthetic_truncated_BB.R` | `Synthetic_truncated_BB.do` | Standalone, one-time R version of the dataset construction. **Not sourced by `R_code11.R`.** |

## Data

Datasets for this chapter are in the [data repository](https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch11).

| File | Used by | Description |
|---|---|---|
| `Example_7_5_3_updated.dta` | All four sub-scripts | Primary dataset: the synthetic B&B-style sample (N = 8,000). It adds two sets of pre-generated variables: graduate program areas for completers (`ma_*`) and prospective program areas for non-completers (`ma_*_pro`). Because every script reads the same area assignments, the Stata and R area-specific estimates are identical. |
| `Example_7_5_3.dta` | Fallback; the CATE listing printed in Section 11.1.3 | Base dataset produced by `Synthetic_truncated_BB.do` / `R_Synthetic_truncated_BB.R`; also in [`data/ch7`](https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch7), where Chapter 7 introduces it. The CATE and pooled MTE results are the same with either file. If a script falls back to it, the program areas are generated at run time, and the R and Stata area estimates then differ (each language uses its own random draws). |
| `mte_bootstrap_draws.dta` | `MTE_MPRTE.do` | The 500 state-clustered bootstrap replications behind the book's published standard errors. Reused by default (see Run Time). |

The MTE scripts and `CATE_R.R` download `Example_7_5_3_updated.dta` from the data repository if it is not in the working directory. `MTE_MPRTE.do` does the same for `mte_bootstrap_draws.dta`.

## Running the Code

**Stata** (version 19 or later; tested in Stata 19.5):
```stata
do Stata_code11.do
```

**R** (version 4.4 or later):
```r
source("R_code11.R")
```

Required packages:
- **Stata:** `mtefe`, `moremata`, `boottest` and `estout`, installed from SSC by `Stata_code11.do`.
- **R:** `haven`, `dplyr`, `tidyr`, `ivreg`, `sandwich`, `lmtest`, `car`, `ggplot2`, `stringr` and `AER`, installed by `R_code11.R` if missing. The Springer plotting theme (`theme_springer()`) is defined inside the scripts.

### Run Time

| Run | Stata MP (2 cores) | Stata SE | R |
|---|---|---|---|
| Default | 5–10 min | 10–15 min | about 5 min |
| Full state-clustered bootstrap | about 7 hours | 9–12 hours | (default) |

By default, `MTE_MPRTE.do` reuses the published bootstrap draws (`use_saved_boot = 1` in Section 6e), so every estimate, standard error and figure matches the book. To rerun the bootstrap, set `use_saved_boot = 0`. The R script runs its own 500-replication bootstrap by default because it takes only a few minutes.

> **Caution:** The published draws are valid **only** for the book's exact specification: the same data, controls, models (quadratic pooled, linear areas), R = 500 and seed 20260101. If you change any of these, set `use_saved_boot = 0`. The script cannot detect such changes.

### Stata vs. R

Point estimates are identical in the two languages: OLS, IV, CATE, MTE parameters, area-specific estimates, PRTEs and benefit-cost ratios. Bootstrap standard errors differ because Stata and R use different random-number generators, and CATE robust standard errors differ slightly because of small-sample conventions. The book reports the Stata results.

> **Note:** Paths use username-conditional branching (`if c(username) == "marvi"` in Stata, `Sys.info()[["user"]] == "marvi"` in R), following the convention used across the 2nd edition codebase. Other users get relative `Output/` paths and work from the current working directory.

## Methods Covered

- **Instrumental variables (IV):** identification with state GA funding as the instrument; the IV/2SLS estimate interpreted as a LATE
- **Conditional average treatment effects (CATE):** subgroup IV, interaction IV, and heterogeneity along observed characteristics
- **Marginal treatment effects (MTE):** heterogeneity along unobserved resistance to treatment, estimated with a polynomial local-IV model (`mtefe`; Andresen, 2018); ATE, ATT, ATU, LATE and MPRTE as weighted averages of the MTE curve
- **PRTE and MPRTE:** returns for the individuals induced to complete by specific GA funding policies (eight scenarios) and by marginal policy changes
- **Cost-benefit analysis (CBA):** PRTEs converted into present-value earnings gains and benefit-cost ratios

## Output

### Chapter figures

The scripts keep their original figure names. The two figures printed in the chapter map to script outputs as follows:

| Chapter | Content | Stata (`$graphs_dir`) | R (`graphs_dir`) | Produced in |
|---|---|---|---|---|
| Fig. 11.1 | Pooled MTE curve with 95% bootstrap band | `fig11_3_mte_curve_Stata.png` (`fig11_3.gph`) | `fig11_3_mte_curve_R.png` | `MTE_MPRTE.do` / `MTE_MPRTE.R`, Section 8 |
| Fig. 11.2 | PRTE by size of a uniform GA funding increase | `fig11_8_mprte_by_intensity_Stata.png` (`fig11_8.gph`) | `fig11_8_mprte_by_intensity_R.png` | `MTE_MPRTE.do` / `MTE_MPRTE.R`, Section 11 |

### Other outputs

| Output | Stata | R |
|---|---|---|
| Supplementary CATE figures (not in the chapter) | `fig11_1`, `fig11_2` | `fig11_1_cate_forest_R.png`, `fig11_2_cate_interact_R.png` |
| Supplementary MTE figures (not in the chapter) | `fig11_4` (propensity-score histogram), `fig11_5` (MTE by program area), `fig11_6` (PRTE weights) | same names, `*_R.png` |
| Logs | `Chapter11_Stata_output.log`, `CATE.do`'s own log, `MTE_MPRTE_output.log` | `Chapter11_R_output.log`, `CATE_output.log`, `MTE_MPRTE_R.log` |
| Supplementary table | `tab11_2_cate_subgroup.rtf` | `tab11_2_cate_subgroup.rtf` |

Figures use Stata's `s2mono` scheme (and a matching black-and-white `ggplot2` theme) for Springer print. The script names `fig11_1` and `fig11_2` refer to the supplementary CATE plots, not to the chapter's Figs. 11.1 and 11.2. The chapter's Tables 11.2–11.6 are compiled from `MTE_MPRTE_output.log`; the scripts do not export them as separate files. `tab11_2_cate_subgroup.rtf` is a supplementary comparison table that is not reproduced in the chapter; its `tab11_2` prefix is a holdover from an earlier draft. The scripts produce no `fig11_7`.

## Related Chapters

Chapter 7 introduced the IV/2SLS framework this chapter builds on; here that estimate is recast as a LATE and then extended into the MTE, PRTE/MPRTE and CBA framework. Chapter 10 is the natural companion for state- and institution-level causal inference (RDD, DiD, SCM, SDID, CS-DiD, ETWFE). The two chapters approach identification from different angles: Chapter 10 handles treatment assigned across a small number of units, and Chapter 11 focuses on individual-level selection.
