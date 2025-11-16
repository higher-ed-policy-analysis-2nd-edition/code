# Chapter 6 — Using Descriptive Statistics and Graphs

Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)  
Author: Marvin A. Titus  
Repository: higher-ed-policy-analysis-2nd-edition/code/ch6

This directory contains the complete, reproducible code and supporting files for Chapter 6 of the book. The chapter demonstrates descriptive statistics, distributional summaries, ANOVA, post-hoc tests, panel-data summaries, and exploratory graphics using the datasets distributed with the book. Both Stata and R implementations are provided and have been cross-checked for consistency.

---

## Contents

- Stata_Code6.do  
  Complete Stata do-file that reproduces the analyses and figures used in Chapter 6 (tested in Stata 19.5+).

- R_Code6.R (also provided as R_Code6.txt)  
  Full R translation of the Stata do-file using tidyverse, haven, plm, car, margins, ggplot2, and other packages. This script downloads the same data used by the Stata code and runs identical analyses.

- Example_6_2_2.dta, Example_6_2_3.dta, Example_6_3.dta (downloaded by scripts)  
  Datasets used by Chapter 6 code (downloaded from the book data repository at runtime).

- Comparison of Stata and R Results.txt  
  Detailed section-by-section comparison of Stata vs R output showing match of estimates, test statistics, and conclusions.

---

## Data

All datasets used in the scripts are available in the book's data repository and are downloaded automatically by the Stata and R scripts. The primary files used in this chapter are:

- tabn302_50.xlsx (reformatted) — public/private enrollment summary (used for measures of central tendency)
- Example_6_2_2.dta — SHEEO finance data (state-level panel finance series)
- Example_6_2_3.dta — HSLS:09 condensed dataset (earnings and demographics)
- Example_6_3.dta — state-level panel (1990–2016)

Data repository (raw files):
https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch6

---

## Quick start — Stata

1. Open Stata and set your working directory to this folder (or edit the do-file paths).
2. (Optional) Install user-written packages if needed:
   - rhausman (only used in other chapters, not required here)
   - aaplot (used for labeled scatter plots; Stata will prompt if missing)
3. Run:
   do Stata_Code6.do

The do-file will download required data files, generate variables (netuit_fte, stapr_fte, etc.), run descriptive statistics, ANOVA and post-hoc comparisons, and produce the simple exploratory graphs shown in the book.

---

## Quick start — R

The R script reproduces the Stata workflow. It downloads the same datasets and performs equivalent analyses and graphics.

Prerequisites:
- R >= 4.0
- Recommended packages (the script will attempt to install missing packages):
  tidyverse, haven, plm, car, margins, ggplot2, psych, readxl, lmtest, sandwich

Run the script:
1. Open the R script file (R_Code6.R) or the R version included here.
2. Optionally set `ch6data` to a local directory if you want to save downloaded .dta/.xlsx files persistently.
3. Source the file or run it in an R session:
   source("R_Code6.R")

The script prints summaries to the console and displays the ggplot2 charts.

---

## What the code does (high level)

- Section 6.2.1 — Measures of central tendency:
  - Arithmetic, geometric, and harmonic means for the public/private enrollment dataset.
  - Detailed summary statistics and standard errors.

- Section 6.2.2 — Measures of dispersion:
  - Coefficients of variation and descriptive statistics by state and by year for SHEEO finance data.

- Section 6.2.3 — Distributions:
  - Frequency tables and two-way summaries for HSLS:09 earnings (EarnHr).
  - Recoding of race/ethnicity and comparisons by sex.

- Section 6.2.4 — ANOVA and post-hoc tests:
  - One-way ANOVA of EarnHr by race/ethnicity.
  - Bonferroni-adjusted pairwise t-tests and Tukey HSD comparisons.
  - Two-way ANOVA (race × sex) and interaction testing.

- Section 6.3.1 — Graphs (EDA):
  - Histograms (with normal overlays), boxplots, faceted histograms, scatter plots with fitted regression lines and labels.

---

## Reproducibility & validation

- The R translation has been validated against the Stata output (see `Comparison of Stata and R Results.txt`). Estimates, standard errors, F-statistics, p-values, group means, and post-hoc comparisons match to machine precision in virtually all cases.
- Notable, documented differences:
  - Kurtosis: Stata reports excess kurtosis (kurtosis − 3); some R functions report regular kurtosis. This is a formula difference only (interpretation is equivalent after adjustment).
  - Minor numeric differences in certain ancillary statistics (e.g., some clustered SE computations) do not change substantive conclusions.

---

## Troubleshooting

- If a script errors with "object 'netuit_fte' not found":
  - Confirm the script successfully downloaded and read the dataset.
  - Ensure the per-FTE variables are created (scripts include code to generate `netuit_fte`, `stapr_fte`, and `stapr_fte2`).
  - In R, check `names()` and `glimpse()` of the loaded dataset; if variable names differ, adapt the mapping in the script.

- If package installation fails in R:
  - Install packages manually, then re-run the script:
    install.packages(c("haven","tidyverse","plm","car","margins","ggplot2","psych","readxl","lmtest","sandwich"))

- If plots do not display:
  - In RStudio, ensure Plots pane is visible. Use `ggsave()` in the script to save PNGs if needed.

---

## Citation and license

Please cite the book when using this code in research or teaching:

Titus, M. A. (2025). Higher Education Policy Analysis Using Quantitative Techniques (2nd ed.). Springer.

Code repository: https://github.com/higher-ed-policy-analysis-2nd-edition/code

License: See the book/repository license for permitted use and redistribution.

---

## Contact / Support

For code-related issues or corrections:
- Open an issue in this GitHub repository.
- Email the author: marvinatitus@gmail.com

---

Last updated: 2025-11-16
