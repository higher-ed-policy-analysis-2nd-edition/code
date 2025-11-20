```markdown
# Chapter 6 — Using Descriptive Statistics and Graphs

**Book:** Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)  
**Author:** Marvin A. Titus  
**Repository:** https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch6

## Overview

This directory contains complete, reproducible code and supporting files for Chapter 6. The chapter demonstrates descriptive statistics, distributional summaries, ANOVA and post-hoc tests, panel-data summaries, and exploratory graphics using datasets provided with the book. Both Stata and R implementations are provided and cross-checked for consistency.

## Repository Contents

- `Stata_code6.do` — Complete Stata implementation (tested in Stata 19.5)
- `R_code6.R` (also provided as `R_code6.txt`) — Full R translation (tested in R >= 4.0)
- `Comparison of Stata and R Results.txt` — Detailed section-by-section validation of Stata vs R output
- Example data files are downloaded by the scripts at runtime from the book data repository

## Datasets

The scripts download the datasets from the book's public data repository when run. Main data used in this chapter:

- `tabn302_50.xlsx` (reformatted sheet) — Public/private enrollment summary
- `Example_6_2_2.dta` — SHEEO finance dataset (state-level finance series)
- `Example_6_2_3.dta` — HSLS:09 condensed dataset (earnings and demographics)
- `Example_6_3.dta` — State-level panel (1990–2016)

Data URL root: `https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch6/`

## Software Requirements

### Stata
- Version: Stata 19.0+ (tested in Stata 19.5)
- The do-file includes instructions for any optional user-written commands used for plotting or specific diagnostics.

### R
- Version: R 4.0 or later (R 4.3.0+ recommended)
- Required R packages (the R script attempts to install missing packages automatically):
```r
c("tidyverse", "haven", "readxl", "psych", "plm",
  "car", "DescTools", "lmtest", "sandwich", "ggplot2")
```
If automatic installation fails, install packages manually:
```r
install.packages(c("tidyverse", "haven", "readxl", "psych",
                   "plm", "car", "DescTools", "lmtest",
                   "sandwich", "ggplot2"))
```

## Quick Start

### Clone the repository
```bash
git clone https://github.com/higher-ed-policy-analysis-2nd-edition/code.git
cd code/ch6
```

### Run with Stata
1. Open `Stata_code6.do`.
2. (Optional) Set your working directory at the top of the do-file.
3. Run:
```stata
do Stata_code6.do
```
The do-file downloads required datasets and runs all analyses and graphs.

### Run with R
1. Open `R_code6.R` (or `R_code6.txt`) in R or RStudio.
2. (Optional) Set `ch6data` at the top of the script if you want to persist downloaded files.
3. Source or run the script:
```r
source("R_code6.R")
```
The script installs missing packages (if needed), downloads datasets, creates necessary variables (`netuit_fte`, `stapr_fte`, etc.), performs analyses, and displays ggplot2 graphics.

## What the Code Does (High Level)

- Measures of central tendency (arithmetic, geometric, harmonic)
- Measures of dispersion (standard deviation, coefficient of variation) by state and year
- Frequency distributions, two-way tables, and recoding (HSLS:09 race/ethnicity example)
- ANOVA and post-hoc pairwise comparisons (Bonferroni and Tukey HSD)
- Panel-data declaration and summary (using `plm` in R; `xtset` in Stata)
- Exploratory graphics:
  - Histograms with normal overlays
  - Boxplots and faceted histograms
  - Scatter plots with fitted regression lines and optional labels

Section organization in the scripts mirrors Chapter 6 of the textbook (Section 6.2.x, 6.3.x).

## Cross-Platform Validation

A detailed comparison between Stata and R results is provided in `Comparison of Stata and R Results.txt`. Key points:

- The R translation reproduces Stata estimates and test statistics with high precision.
- Minor, documented differences may appear in ancillary statistics (e.g., definitions of kurtosis or specific clustered SE implementations) but do not affect substantive conclusions.

## Notes on Implementation Differences

- Kurtosis: Some R functions report regular kurtosis while Stata may report excess kurtosis. This is a formulaic difference; adjust interpretation accordingly.
- Clustered/robust SEs: Small numeric differences in clustered standard errors across platforms are expected due to different algorithms — coefficients remain the same and conclusions unchanged.
- Panel-package differences: R's `plm` and Stata's `xt` family sometimes use different default output formats; the scripts document implementation choices and any modifications made for numerical stability.

## Troubleshooting

- Data download failure: Check internet access or manually download datasets from the data repository and place them in your working directory.
- R package installation failure: Install packages manually and restart your R session.
- "object not found" errors: Ensure the dataset loaded correctly and that per-FTE variables are created before model estimation (scripts include code to generate these variables).
- Plots not visible: In RStudio, check the Plots pane or use `ggsave()` in the script to save figures.

## Citation

If you use these materials in research or teaching, please cite:

Titus, M. A. (2025). Higher Education Policy Analysis Using Quantitative Techniques (2nd ed.). Springer.

GitHub repository: https://github.com/higher-ed-policy-analysis-2nd-edition/

## Author & Contact

Marvin A. Titus, Ph.D.  
Email: marvinatitus@gmail.com

## License

Code is provided for educational and research purposes. See the repository license for terms of use.

## Last Updated

November 16, 2025
```