# Data — Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)

Companion data repository for the book's Stata and R code (see the [`code`](https://github.com/higher-ed-policy-analysis-2nd-edition/code) repository). One folder per chapter, `ch4`–`ch13`, each holding the datasets that chapter's scripts load or download.

Chapters 1–3 have no data folder (introductory/conceptual material only).

## Repository structure

```
data/
├── ch4/    Introduction to data management and description
├── ch5/    (see ch5 contents below)
├── ch6/    Simple regression / trend analysis
├── ch7/    Panel data foundations
├── ch8/    Diagnostics
├── ch9/    Advanced panel/time-series techniques
├── ch10/   Causal inference (RDD, DiD, SCM, SDiD, ETWFE)
├── ch11/   Instrumental variables, CATE, and MTE
├── ch12/   Bayesian MTE microsimulation and cost-benefit analysis
└── ch13/   Presenting analyses to policymakers
```

## Chapter-by-chapter contents

| Folder | Representative files | Chapter README |
|---|---|---|
| `ch4` | `Public_use_HSLS_09_truncated.dta`, `College_enrollment_data.xlsx`, first-time/HS-grad enrollment panels (long & wide), state financial aid (merit/need) data, several NCES `tabn*` tables | Placeholder — not yet written |
| `ch5` | `Example_5_0.dta`–`Example_5_4_1.dta`, `Example5_2.dta`, `Public_use_HSLS_09_truncated.dta` | Placeholder — not yet written |
| `ch6` | `Example_5_*`/`Example_6_*`/`Example_7_*` series, `Public_use_HSLS_09_truncated`, NCES `tabn*` tables, state PSE enrollment data | Placeholder — not yet written |
| `ch7` | `Example 7.1.dta`, `Example_7_2_2.dta`–`Example_7_5_3.dta/csv` | **Full README** (~10 KB) |
| `ch8` | `Example_8_2.dta`, `Example_8_6.dta`, `Example_8_8_2.dta` | **Full README** (~3.6 KB) |
| `ch9` | `Example_9_3_1.dta` | Placeholder — not yet written |
| `ch10` | `Example_10_2_1.dta`, `Example_10_3.dta`, `Example_10_3_1.csv`, `Example_10_7_3.csv`, `Example 10.dta`, `Example_7_5_3.dta` | Placeholder — not yet written |
| `ch11` | `Example_11_1.dta`/`.csv`, `Example_7_5_3.dta` | No README yet |
| `ch12` | `Example_12_1.dta`, `sim_results_ch12.dta` (posterior simulation draws) | **Full README** (~14 KB) |
| `ch13` | `Example_13_1.dta`–`Example_13_4.dta` | No README yet |

Chapters with a "Placeholder" or "No README yet" README are next in line for documentation — contributions welcome.

## A note on cross-chapter file reuse

Several files appear in more than one chapter folder because Chapter 13 (a presentation-focused chapter) and later chapters reuse earlier worked examples rather than generating new data. Notably:

- `ch13/Example_13_1.dta` is the same file as `ch10/Example_10_2_1.dta` (state appropriations panel)
- `ch13/Example_13_2.dta` is the same file as `ch10/Example_10_3.dta`
- `ch13/Example_13_3.dta` is the same file as `ch7/Example 7.1.dta`
- `ch13/Example_13_4.dta` is the same file as `ch10/Example 10.dta` (net tuition/administrative staffing panel)
- `ch10/Example_7_5_3.dta` and `ch11/Example_7_5_3.dta` both duplicate `ch7/Example_7_5_3.dta`

This is intentional — each chapter's folder is self-contained so its scripts can `use`/`import` directly without a cross-chapter path — but it means a fix to one copy (e.g., a data-cleaning correction) needs to be propagated to its duplicates elsewhere in the repository.

## Downloaded-at-runtime files

Some chapters' scripts download data directly from this repository via `copy`/`import delimited` at run time rather than assuming a local file — for example, Chapter 13's `CausalPlots13.do` fetches `ch10/Example_10_3_1.csv` and `ch10/Example_10_7_3.csv` directly from `raw.githubusercontent.com`. Chapter 13's `BayesianPlots13.do` currently reads `ch12/sim_results_ch12.dta` from a local path rather than downloading it the same way; that's a known inconsistency worth fixing so the chapter is reproducible from a clean clone.

## Format conventions

- Stata data: `.dta`
- Raw/intermediate data: `.csv`
- Excel source tables (mostly NCES digest tables): `.xlsx`

File names generally follow `Example_<chapter>_<section>[_<subsection>].dta`, though some early-chapter files predate this convention (e.g., `Example5_2.dta`, `Example 7.1.dta`, `Example 10.dta`).
