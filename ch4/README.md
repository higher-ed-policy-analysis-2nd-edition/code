# Chapter 4 — Creating Datasets and Managing Data

**Book:** Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)  
**Author:** Marvin A. Titus  
**Repository:** https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch4

## Overview

This directory contains reproducible code used to create, reshape, and manage core datasets used throughout the book. Chapter 4 shows how to import reformatted source tables, reshape wide → long, create time-series and panel structures, add state identifiers (abbreviations and FIPS), merge multiple variables into a single long-panel dataset, and save outputs for analysis. Both Stata and R implementations are provided and have been validated to produce equivalent data objects for replication and classroom use.

## Repository Contents

- `Stata_code4.do` — Complete Stata do-file for Chapter 4 (tested in Stata 19.5)  
- `R_code4.R` (also provided as `R_code4.txt`) — Full R translation of the Stata workflow (tested in R >= 4.0)  
- Reformatted Excel inputs are fetched at runtime from the companion data repository (scripts download them automatically)
- This `README.md` — Overview, quick start, section summaries, validation, and troubleshooting

## Datasets (downloaded by the scripts)

The scripts fetch reformatted Excel/ Stata files from the companion data repository at runtime. Primary files used:

- `tabn302_50.xlsx` — reformatted NCES Table 302.50 (state-level HS graduates / enrollment)
- `tabn302_10.xlsx` — reformatted NCES Table 302.10 (time series: percent of HS graduates enrolled in PSE, 1960–2016)
- `tabn304_70.xlsx` — reformatted NCES Table 304.70 (undergraduate enrollment by state and year)

Data URL root:
`https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/`

## Software Requirements

### Stata
- Version: Stata 19.0+ (tested in Stata 19.5)
- Optional user-written commands (install via `ssc install` if needed):
  - `statastates` — useful for matching state names / FIPS
  - Standard Stata utilities (`reshape`, `tsset`, `xtset`, etc.) are used

The do-file contains comments indicating where to set a working directory and where optional packages may be installed.

### R
- Version: R 4.0 or later (R 4.3.0+ recommended)
- Required packages (the R script attempts to install missing packages automatically):
  - tidyverse, haven, readxl, plm, writexl

Install manually if needed:
```r
install.packages(c("tidyverse", "haven", "readxl", "plm", "writexl"))
```

## Quick Start

### Clone the repository
```bash
git clone https://github.com/higher-ed-policy-analysis-2nd-edition/code.git
cd code/ch4
```

### Run with Stata
1. Open `Stata_code4.do`.
2. (Optional) Edit the global working-path at the top of the do-file.
3. Execute:
```stata
do Stata_code4.do
```
The do-file downloads the reformatted Excel files from the data repository (if not already present), imports worksheets, reshapes data, declares panel/time-series structures, and demonstrates merges.

### Run with R
1. Open `R_code4.R` (or `R_code4.txt`) in R or RStudio.
2. (Optional) Set a persistent download directory at the top of the script:
```r
ch4data <- "C:/your/path/here"
dir.create(ch4data, recursive = TRUE, showWarnings = FALSE)
setwd(ch4data)
```
3. Source the script:
```r
source("R_code4.R")
```
The R script prints progress messages, creates the primary objects, and leaves optional save/write lines commented for exporting outputs.

## What the Code Does (section-by-section)

- Section 4.2.1 — Primary data
  - Demonstrates manual data creation (Stata `input`; R data.frame) and saving/exporting.

- Section 4.2.2 — Cross-sectional dataset (NCES Table 302.50)
  - Downloads reformatted `tabn302_50.xlsx`, imports the `reformatted` sheet, inspects structure, and adds state identifiers (abbreviation + FIPS).
  - Adds variable labels and downcasts integer-valued doubles where appropriate.

- Section 4.2.2 — Time-series dataset (NCES Table 302.10)
  - Downloads `tabn302_10.xlsx`, imports the `reformatted` sheet, ensures `year` is integer, and declares time-series (`tsset` in Stata; keep `year` integer in R).

- Section 4.2.2 — Panel dataset (NCES Table 304.70)
  - Downloads `tabn304_70.xlsx`, selects an appropriate sheet (script scans common sheet names and falls back to the first sheet), reads a wide table and reshapes to long using `reshape` (Stata) or `pivot_longer` (R).
  - Declares panel using `xtset` (Stata) or `pdata.frame()` (R) and reports panel dimensions.

- Section 4.2.2 — Merging multiple variables into the panel
  - Demonstrates merging HSGrad, and (optionally) need- and merit-based financial aid series into a single long-panel dataset (`joinby` in Stata; `left_join` in R).
  - Produces a `complete_panel` object and declares it as `pdata.frame` for downstream analysis.

## Implementation Notes (R)

- Helper functions in `R_code4.R`:
  - `downcast_double()` — safely converts integer-valued doubles to integer (analogous to Stata `compress`).
  - `add_state_identifiers(df, state_col = "State")` — adds `state_abbrev` and `state_fips` by matching state names; includes DC if present (replicates `statastates` functionality).
- Robust sheet selection — the R script checks for common sheet names and uses the first available sheet if none match exactly to increase tolerance to formatting differences.
- The R script downcasts numeric columns where appropriate to reduce memory and better match Stata storage types.

## Cross-Platform Validation

- The R translation was written to produce the same data objects (rows, columns, variable names and key types) as the Stata implementation given the same reformatted Excel inputs.
- The script prints row/column counts and panel dimensions after each major step to help verify results.
- The data files referenced by the scripts are available in the companion data repository linked above.

## Troubleshooting & Notes

- Missing or different sheet names:
  - Inspect the downloaded `.xlsx` files in your working directory; the R script reports which sheet it selected.
- Package installation problems (R):
  - If your environment prevents automatic installation, install packages manually and restart R.
- Missing `id` or `year` columns:
  - Panel creation requires `id` and `year`. If reformatted sheets use different names, rename columns or edit the script mapping.
- Large raw NCES files:
  - Use the reformatted versions available in the data repo for these examples. If loading full NCES raw releases, use a 64-bit R with adequate memory or Stata/SE or Stata/MP.

## Reproducibility & Where to Look in the Code

- `Stata_code4.do` provides step-by-step commands for downloading, importing, reshaping, and merging (with comments indicating where to save outputs).
- `R_code4.R` mirrors the Stata workflow and includes the helper functions noted above. Both scripts include commented save/export lines for producing `.dta` or `.csv` outputs if you wish to persist results.

## How to use the outputs

- `complete_panel` (R) / merged panel dataset (Stata): Inputs for regression and panel methods in later chapters (e.g., Chapters 6–7). The long-form `id`/`year` structure is ready for `plm`/`xtset`.
- Example save commands (uncomment in code if desired):
  - R: `write_dta(complete_panel, "Complete_Panel_Dataset.dta")` or `write_csv(complete_panel, "Complete_Panel_Dataset.csv")`
  - Stata: `save "Complete_Panel_Dataset.dta", replace`

## Citation

If you use these materials in research or teaching, please cite:

Titus, M. A. (2025). *Higher Education Policy Analysis Using Quantitative Techniques* (2nd ed.). Springer.

GitHub repository: https://github.com/higher-ed-policy-analysis-2nd-edition/code

## Author & Contact

Marvin A. Titus, Ph.D.  
Email: marvinatitus@gmail.com

## License

Code is provided for educational and research purposes. See the repository top-level LICENSE for terms of use.

## Last Updated

November 17, 2025