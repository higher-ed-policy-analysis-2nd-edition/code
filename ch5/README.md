# Chapter 5 — Getting to Know Thy Data

**Book:** Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)  
**Author:** Marvin A. Titus  
**Repository:** https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch5

## Overview

This directory contains complete, reproducible code and supporting files for Chapter 5. The chapter teaches practical data‑preparation and data‑discovery skills used throughout the book: examining dataset structures, declaring panel data, exploring missingness patterns, and performing formal missing‑data diagnostics (including Little's MCAR test and related diagnostics). Both Stata and R implementations are provided and validated for consistency.

## Repository Contents

- `Stata_code5.do` — Complete Stata do-file (tested in Stata 19.5)  
- `R_code5.R` (also provided as `R_code5.txt`) — Full R translation (tested in R >= 4.0)  
- `Public_use_HSLS_09_truncated.dta` (downloaded by scripts) — truncated HSLS:09 student data used for examples  
- `Example_5_0.dta`, `Example_5_1.xlsx`, `Example_5_3.dta`, `Example_5_4.dta`, `Example_5_4_1.dta` — data files downloaded by the scripts at runtime  
- `Comparison of R and Stata Results.txt` — Section-by-section comparison showing R and Stata outputs match for the Chapter 5 analyses

## Datasets (downloaded by the scripts)

All datasets are stored in the companion data repository and are downloaded automatically by the scripts when run:

- `Example_4_2_2_TS.dta` — time series example (1960–2016 high‑school-to‑PSE percentage)
- `Example_5_0.dta` — small panel example (id, year, state, enrollment, state aid)
- `Example_5_1.xlsx` — SHEEO finance worksheet (reformatted sheet used in the chapter)
- `Public_use_HSLS_09_truncated.dta` / `Example_5_3.dta` — HSLS:09 student data (truncated for examples)
- `Example_5_4.dta`, `Example_5_4_1.dta` — IPEDS / institutional panel data used for missingness diagnostics

Data URL root:
`https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch5/`

## Output Files

### Graph Output Directory

Both scripts save all plots to:

```
C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 5\Output\graphs
```

Plots from each platform are distinguished by a filename suffix to prevent overwrites when both scripts are run:

| Figure | R filename (`_R` suffix) | Stata filename (`_Stata` suffix) |
|--------|--------------------------|----------------------------------|
| Panel missingness heatmap | `xtmis_heatmap_R.png` | `xtmis_heatmap_Stata.png` |
| Bar chart: variable-level missingness | `xtmis_barvar_R.png` | `xtmis_barvar_Stata.png` |
| Bar chart: panel-level missingness | `xtmis_barpanel_R.png` | `xtmis_barpanel_Stata.png` |
| Bar chart: time-period missingness | `xtmis_bartime_R.png` | `xtmis_bartime_Stata.png` |
| Combined missingness dashboard | `xtmis_combined_R.png` | `xtmis_combined_Stata.png` |
| Missingness pattern plot | *(R: not produced)* | `xtmis_pattern_Stata.png` |
| Timeline plot | *(R: not produced)* | `xtmis_timeline_Stata.png` |

### Log Output Directory

Both scripts write a plain-text log to:

```
C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 5\Output\logs\
```

- Stata: `Chapter5_Stata_output.log`
- R: `Chapter5_R_output.log`

## Software Requirements

### Stata
- Version: Stata 19.0+ (code tested in Stata 19.5)
- Recommended / optional user-written commands:
  - `statastates` — convenient for matching state names / IDs (used in the script)
  - `mdesc`, `misstable` — missing data summaries (usually included in base Stata; `mdesc` may be user-written)
  - `xtmis`, `tomata` — panel missingness diagnostics (install via `ssc install xtmis, replace` / `ssc install tomata, replace`)
  - `mcartest` — Little's MCAR test (install if needed; e.g., `cap net install st0318.pkg, replace` as directed in the do-file)

The do-file includes inline comments describing where to install user-written packages if required.

### R
- Version: R 4.0 or later (R 4.3.0+ recommended)
- Required R packages (the R script attempts to install missing packages automatically):
  - tidyverse, haven, readxl, plm, naniar, mice, DescTools, car
- Optional MCAR packages (fallbacks used in R code if available):
  - BaylorEdPsych, MissMech

If automatic installation fails, install packages manually. Example:
```r
install.packages(c("tidyverse", "haven", "readxl", "plm", "naniar", "mice", "DescTools", "car"))
```

## Quick Start

### Clone the repository
```bash
git clone https://github.com/higher-ed-policy-analysis-2nd-edition/code.git
cd code/ch5
```

### Run with Stata
1. Open `Stata_code5.do`.
2. (Optional) Edit the working directory path at the top of the do-file (`global ch5data ...`) to a local folder.
3. If needed, install user-written packages referenced in the do-file:
   ```stata
   ssc install statastates, replace
   ssc install xtmis, replace
   cap net install st0318.pkg, replace   // if mcartest not available
   ```
4. Execute the do-file:
   ```stata
   do Stata_code5.do
   ```

The do-file downloads required datasets from the book's data repository, prepares variables, runs missingness summaries and patterns, and executes Little's MCAR and related diagnostics. All plots are exported to the graphs output directory with `_Stata` suffixes.

### Run with R
1. Open `R_code5.R` (or `R_code5.txt`) in RStudio.
2. Source or run the script:
   ```r
   source("R_code5.R")
   ```
The R script installs missing packages (if needed), downloads the datasets, creates necessary variables and ids, computes missingness summaries, displays missingness patterns, and runs Little's MCAR test (via `naniar::mcar_test()` with fallbacks to `BaylorEdPsych`/`MissMech` if available). It also includes a CDM-style logistic fallback (missingness ~ covariates) when formal MCAR functions are not present.

All plots are displayed in the RStudio Plots pane as they are generated and saved to the graphs output directory with `_R` suffixes. Output paths are hardcoded — no path configuration is required.

## What the Code Does (High Level)

- Section 5.2 — Inspecting data structures and declaring panels:
  - Load time series and panel datasets (describe, compress, recast id types)
  - Read and filter SHEEO Excel file (drop pre-2010 or aggregates such as U.S. and D.C.)
  - Create state identifiers and declare panels (`xtset` in Stata; `pdata.frame()` in R)

- Section 5.3 — Missingness summaries and patterns:
  - Variable-level missingness summaries (`mdesc`/`misstable` in Stata; `naniar::miss_var_summary`, `mice::md.pattern` in R)
  - Nested missing patterns and frequencies (Stata `misstable tree`; R `md.pattern()` and a custom pattern frequency table)

- Section 5.4 — Missing data diagnostics:
  - Unit-level panel missing summaries (Stata `xtmis`; R can reproduce via grouping/aggregation)
  - Tests for Missing Completely at Random (Little's MCAR):
    - Stata: `mcartest` (equal- and unequal-variance versions; CDM conditional test variant)
    - R: `naniar::mcar_test()` primary, with optional fallbacks to `BaylorEdPsych::LittleMCAR` or `MissMech` functions
  - When formal MCAR tests are unavailable, the R code runs CDM‑style logistic diagnostics (fit missingness indicators on covariates, report LR tests and a Fisher combination)

## Cross-Platform Validation

A dedicated comparison file (`Comparison of R and Stata Results.txt`) documents that the R translation reproduces Stata outputs for Chapter 5 analyses. Notable matches:

- Dataset loads and shapes:
  - Example_4_2_2_TS: 56 observations, 2 variables (matched)
  - Example_5_0: 250 observations, 6 variables (matched)
  - SHEEO (filtered FY >= 2010, excluding U.S./D.C.): 750 rows (matched)
  - HSLS truncated and Example_5_3 / Example_5_4_1: 23,503 rows (matched)
  - Example_5_4 (IPEDS panel): 9,596 rows (matched)

- Missingness summaries:
  - P1TUITION: 1,407 missing (5.99%)
  - X1RACE: 1,006 missing (4.28%)
  - S3CLGPELL: 459 missing (1.95%)
  - Dominant complete-case pattern (e.g., pattern `00000000` with 20,572 complete cases)
  - These counts and pattern frequencies match between Stata and R outputs.

- Little's MCAR test:
  - Stata: Chi-square ≈ 68.1557, df = 2, p < 0.0001 (reject MCAR)
  - R (`naniar::mcar_test`): statistic ≈ 68.2, df = 2, p ≈ 1.55e-15 (same conclusion)
  - CDM and unequal-variance variants produce highly significant results in both implementations.

Conclusion: The R translation reproduces Stata results with high precision. Minor differences are confined to formatting and numeric display rounding; substantive conclusions match.

## Troubleshooting & Notes

- Data download errors: Check internet connectivity or download datasets manually from the data repository and place them in your working directory.
- Stata package installation:
  - If `mcartest` is not available in your Stata installation, the do-file suggests `cap net install st0318.pkg, replace` or install the relevant user package.
  - Install `xtmis`, `statastates`, or `tomata` via `ssc install` if missing.
- R package issues:
  - If automatic installation fails, install packages manually and restart R.
  - The R script checks for optional MCAR packages (`BaylorEdPsych`, `MissMech`) — these are not required but provide alternative implementations.
- Plot saving (R): The R script saves plots via a temp-file copy approach to avoid Dropbox sync-lock conflicts. If a plot is not found in the output directory after running, check the temp path reported in the console for a fallback copy.
- Differences to expect:
  - Output formatting differs across platforms (Stata tables vs R tibbles). Numbers match to displayed precision.
  - Some specialized CDM tests available in Stata may not have direct equivalents in base R packages; the R script provides diagnostic fallbacks (CDM‑style logistic models) to approximate the same checks.
  - Stata's `xtmis` produces two additional graph types (`pattern`, `timeline`) that are not replicated in the R script; only the five shared graph types have `_R` counterparts.

## Reproducibility & Where to Look in the Code

- `Stata_code5.do` contains inline comments and step-by-step commands for:
  - Downloading data, recoding special missing values, summarizing missingness, and running Little's MCAR and CDM diagnostics.
  - Exporting all `xtmis` graphs with `_Stata` filename suffixes via a `foreach` loop.
- `R_code5.R` mirrors the Stata workflow and includes:
  - Helpers to recode -9 to NA, downcast numeric types, produce missingness patterns, run `naniar::mcar_test`, and fallback CDM logistic diagnostics.
  - A `save_fig()` helper that prints each plot to the RStudio Plots pane and saves it to the output directory using a temp-file copy strategy.

## Citation

If you use these materials in research or teaching, please cite:

Titus, M. A. (2025). Higher Education Policy Analysis Using Quantitative Techniques (2nd ed.). Springer.

GitHub repository: https://github.com/higher-ed-policy-analysis-2nd-edition/code

## Author & Contact

Marvin A. Titus, Ph.D.  
Email: marvinatitus@gmail.com

## License

Code is provided for educational and research purposes. See the repository license for terms of use.

## Last Updated

March 29, 2026
