# Chapter 6 — Using Descriptive Statistics and Graphs

Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)  
Author: Marvin A. Titus  
Repository: higher-ed-policy-analysis-2nd-edition/code/ch6

This folder contains the complete, reproducible code and supporting files for Chapter 6. The chapter demonstrates descriptive statistics, distributional summaries, ANOVA (one- and two-way), post-hoc comparisons, panel-data summaries, and exploratory graphics using the datasets distributed with the book. Both Stata and R implementations are provided and have been cross-checked for consistency.

---

## Contents

- `Stata_Code6.do`  
  Complete Stata do-file that reproduces the analyses and figures used in Chapter 6 (tested in Stata 19.5).

- `R_Code6.R` (also included as `R_Code6.txt`)  
  Full R translation following the organization and outputs of the Stata code. Uses tidyverse, haven, plm, car, psych, DescTools, ggplot2 and other packages.

- `Example_6_2_2.dta`, `Example_6_2_3.dta`, `Example_6_3.dta`, `tabn302_50.xlsx`  
  Data files used by the chapter. (The scripts download these from the book data repository at runtime; files are not required to be committed to this repo.)

- `Comparison of Stata and R Results.txt`  
  Detailed, section-by-section comparison showing the match between Stata and R outputs and notes on any small differences.

---

## Datasets (downloaded automatically by scripts)

All data used by the Chapter 6 scripts are available in the book's data repository:

- `tabn302_50.xlsx` — public/private enrollment summary (reformatted sheet). Used for measures of central tendency (Section 6.2.1).
- `Example_6_2_2.dta` — SHEEO finance data (state-level series). Used for measures of dispersion (Section 6.2.2).
- `Example_6_2_3.dta` — HSLS:09 condensed dataset (earnings and demographics). Used for distributions and ANOVA (Section 6.2.3 and 6.2.4).
- `Example_6_3.dta` — State-level panel (1990–2016). Used for panel summaries and exploratory graphics (Section 6.3.1).

Data repository (raw files):  
https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch6

---

## Quick start

### Requirements

For Stata users:
- Stata 19 or later (scripts tested in Stata 19.5).
- (Optional) user-written commands: `aaplot` (for labeled scatter plots) and `rhausman` (used elsewhere). Install via:
```stata
ssc install aaplot, replace
ssc install rhausman, replace
```

For R users:
- R >= 4.0 (tested with R 4.3.0+)
- The R script installs missing packages automatically; key packages used include:
```r
c("haven","readxl","tidyverse","psych","plm","car","DescTools","lmtest","sandwich","ggplot2","ggrepel")
```

### Run the analyses

1. Clone the repository and change to the chapter folder:
```bash
git clone https://github.com/higher-ed-policy-analysis-2nd-edition/code.git
cd code/ch6
```

2. Stata:
- Open Stata, set working directory to this folder (or edit the do-file), then:
```stata
do Stata_Code6.do
```
The do-file downloads required files (if needed), generates variables, runs descriptive analysis, ANOVAs, post-hoc tests, and creates plots.

3. R:
- Open the R script `R_Code6.R` (or `R_Code6.txt`) and run it in your R session / RStudio.  
- Optionally set `ch6data` at the top of the file to save downloaded data to a persistent folder.

The R script prints console output and shows ggplot2 charts that replicate the Stata figures and tables.

---

## What the code does (by section)

- Section 6.2.1 — Measures of central tendency:
  - Arithmetic, geometric, and harmonic means for public/private enrollments.
  - Detailed summary statistics and standard errors (equivalent to Stata's `ameans` and `mean`).

- Section 6.2.2 — Measures of dispersion:
  - Coefficients of variation and descriptive statistics by state and by fiscal year for the SHEEO finance dataset.

- Section 6.2.3 — Distributions:
  - Frequency distributions, two-way summary tables, recoding of race/ethnicity (HSLS:09), and panel-data structure checks.

- Section 6.2.4 — ANOVA and post-hoc tests:
  - One-way ANOVA of hourly earnings (EarnHr) by race/ethnicity.
  - Bonferroni-corrected pairwise comparisons and Tukey HSD.
  - Two-way ANOVA (race × sex) and tests of interaction.

- Section 6.3.1 — Graphs (EDA):
  - Histograms with normal overlays, box plots, faceted histograms by regional compact, scatter plots with fitted lines, and labeled scatter plots (ggrepel recommended).

---

## Validation: Stata vs. R

A comprehensive point-by-point comparison is available in `Comparison of Stata and R Results.txt`. Summary:

- Overall match: Excellent. R reproduces Stata results with high precision for means, standard deviations, test statistics, p-values, and ANOVA/Tukey comparisons.
- Differences:
  - Kurtosis: Stata reports excess kurtosis (kurtosis − 3); some R functions report regular kurtosis. Adjust by adding/subtracting 3 when comparing.
  - Minor numerical differences in some ancillary statistics (e.g., certain variance estimators, plotting defaults). These do not change substantive conclusions.
- Conclusion: R translation is publication-ready and suitable for replication and teaching.

---

## Implementation notes / tips

- Factor variables: R requires explicit factor conversion (e.g., `factor(region_compact)`) to reproduce categorical contrasts Stata creates with `i.var`. The R scripts do this where needed.
- Panel data: Stata's `xtset` is global; R uses `pdata.frame()` from `plm` — make sure the panel identifiers (e.g., `fips`, `year`) are present and consistently typed.
- Kurtosis: To compare Stata's reported kurtosis to R's `DescTools::Kurt()`, add or subtract 3 accordingly (Stata gives excess kurtosis).
- Graphics: Plots in R (ggplot2) differ visually from Stata but convey identical statistical content.
- Packages: If package installation in R fails (e.g., due to R version or permissions), install packages manually then re-run the script:
```r
install.packages(c("haven","readxl","tidyverse","psych","plm","car","DescTools","lmtest","sandwich","ggplot2","ggrepel"))
```

---

## Troubleshooting (common issues)

- "object 'netuit_fte' not found" (R): Ensure the panel dataset loaded correctly and that the per‑FTE variables are created. The R script includes `mutate(netuit_fte = netuit / fte)` where needed.
- Package installation warnings (R): Some packages (e.g., `DescTools`, `readxl`) may build under a different R minor version. If you see binary package warnings, try updating R or install from source.
- Plot labeling overlap: Install `ggrepel` to improve label placement in scatterplots.
- Stata user-written commands: If a Stata command (like `aaplot`) is missing, run `ssc install aaplot, replace` once.

---

## Reproducibility & provenance

- Scripts download data directly from the book's data repository when run, ensuring reproducibility.
- The R translation was validated against Stata output; see `Comparison of Stata and R Results.txt` for numeric tables and detailed notes.
- Use the provided scripts as-is for teaching, replication studies, or as templates for similar analyses.

---

## Citation

If you use the code or datasets in research or teaching, please cite:

Titus, M. A. (2025). Higher Education Policy Analysis Using Quantitative Techniques (2nd ed.). Springer.  
GitHub repository: https://github.com/higher-ed-policy-analysis-2nd-edition/

---

## Contributing / Support

Found an issue or suggestion?
1. Open an issue in this repository (preferred).
2. Submit a pull request with proposed changes.
3. Email Marvin A. Titus at marvinatitus@gmail.com for direct queries.

Please include a short description, reproduction steps, and any console output or screenshots that help diagnose problems.

---

**Last updated:** 2025-11-16  
**Maintained by:** Marvin A. Titus