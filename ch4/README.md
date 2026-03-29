# Chapter 4 — Creating Datasets and Managing Data

**Book:** Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)  
**Author:** Marvin A. Titus  
**Repository:** https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch4

## Overview

This directory contains reproducible code used to create, reshape, and manage core datasets used throughout the book. Chapter 4 shows how to import reformatted source tables, reshape wide → long, create time-series and panel structures, add state identifiers (abbreviations and FIPS), merge multiple variables into a single long-panel dataset, and save outputs for analysis. Both Stata and R implementations are provided and have been validated to produce equivalent data objects for replication and classroom use.

## Repository Contents

- `Stata_code4.do` — Complete Stata do-file for Chapter 4 (tested in Stata 19.5)  
- `R_code4.R` (also provided as `R_code4.txt`) — Full R translation of the Stata workflow (tested in R 4.4.x)  
- Reformatted Excel inputs are fetched at runtime from the companion data repository (scripts download them automatically)
- This `README.md` — Overview, quick start, section summaries, validation, and troubleshooting

## Datasets (downloaded by the scripts)

The scripts fetch reformatted Excel/Stata files from the companion data repository at runtime. Primary files used:

- `tabn302_50.xlsx` — reformatted NCES Table 302.50 (state-level HS graduates / enrollment)
- `tabn302_10.xlsx` — reformatted NCES Table 302.10 (time series: percent of HS graduates enrolled in PSE, 1960–2016)
- `tabn304_70.xlsx` — reformatted NCES Table 304.70 (undergraduate enrollment by state and year)

Data URL root:
`https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/`

### Synthetic Data Fallbacks (R only)

If any Excel file cannot be downloaded (e.g., no internet connection), `R_code4.R` automatically substitutes a synthetic dataset of the same structure so the script can run to completion. A message such as `[Using synthetic data for tabn302_50]` is printed to the log when this occurs. Results from synthetic data are illustrative only and will not match Stata outputs.

## Output Files

### Log Output

Both scripts write a plain-text log capturing all console output:

| Platform | Log path |
|----------|----------|
| R        | `C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 4\Output\logs\Chapter4_R_output.log` |
| Stata    | `C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 4\Output\logs\Chapter4_Stata_output.log` |

Other users receive logs at `Output/logs/` relative to the working directory.

### Graphs

Chapter 4 produces no plots. A `graphs_dir` global is defined in `Stata_code4.do` for structural consistency with other chapters but is not used.

## Software Requirements

### Stata
- Version: Stata 19.0+ (tested in Stata 19.5)
- Optional user-written commands (install via `ssc install` if needed):
  - `statastates` — useful for matching state names / FIPS
  - Standard Stata utilities (`reshape`, `tsset`, `xtset`, etc.) are used

The do-file contains comments indicating where to set a working directory and where optional packages may be installed.

### R
- Version: R 4.0 or later (R 4.4.x recommended)
- Required packages (the R script attempts to install missing packages automatically):
  - `readxl`, `writexl`, `haven`, `dplyr`, `tidyr`, `psych`, `plm`

Install manually if needed:
```r
install.packages(c("readxl", "writexl", "haven", "dplyr", "tidyr", "psych", "plm"))
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
The do-file downloads the reformatted Excel files from the data repository (if not already present), imports worksheets, reshapes data, declares panel/time-series structures, and demonstrates merges. A log is written to the Output/logs directory.

### Run with R
1. Open `R_code4.R` (or `R_code4.txt`) in R or RStudio.
2. Source the script:
```r
source("R_code4.R")
```
The R script installs missing packages (if needed), downloads the datasets, creates necessary variables, computes summaries, and mirrors the Stata workflow section by section. If any data file cannot be fetched, synthetic fallback data is used automatically. A log is written to the Output/logs directory.

## What the Code Does (section-by-section)

- **Section 4.2.1 — Primary Data Entry**
  - Demonstrates manual data creation (Stata `input`; R `data.frame`) and saving/exporting to `.dta` and `.csv`.

- **Section 4.2.2 — Cross-sectional dataset (NCES Table 302.50)**
  - Downloads `tabn302_50.xlsx`, imports the reformatted sheet, inspects structure, and adds state identifiers (abbreviation + FIPS) via a built-in lookup table (replicating `statastates`).

- **Section 4.2.2 — Time-series dataset (NCES Table 302.10)**
  - Downloads `tabn302_10.xlsx`, imports the reformatted sheet, ensures `year` is integer, and declares time-series structure (`tsset` in Stata; integer `year` in R).

- **Section 4.2.2 — Panel dataset (NCES Table 304.70)**
  - Downloads `tabn304_70.xlsx`, reads a wide table, and reshapes to long using `reshape` (Stata) or `pivot_longer` (R).
  - Declares panel using `xtset` (Stata) or `pdata.frame()` (R) and reports panel dimensions.

- **Section 4.2.2 — Creating Additional Panel Variables**
  - Constructs additional per-state panel variables (e.g., need- and merit-based aid series) from supplementary NCES tables, with synthetic fallbacks in R if downloads fail.

- **Section 4.2.2 — Joining Panel Datasets**
  - Merges HSGrad and financial aid series into a single long-panel dataset (`joinby` in Stata; `left_join` in R).
  - Produces a `complete_panel` object and declares it as `pdata.frame` for downstream analysis.

## Implementation Notes (R)

- **`parse_nces_wide(file, sheet, prefix)`** — parses NCES multi-header Excel tables. NCES Digest tables have title rows, sub-header rows, and column-number rows before the data. This helper: (1) reads the raw sheet with no header assumptions; (2) detects the data-start row by searching for "United States" or state names; (3) scans up to 8 rows above the data start for four-digit year values; (4) builds clean `PREFIX_YYYY` column names; (5) deduplicates columns (NCES tables often have two side-by-side panels); and (6) returns a clean data frame with `State` + `PREFIX_YYYY` columns. This is the R equivalent of Stata's `import excel, cellrange()` approach.
- **State identifier lookup** — `R_code4.R` builds state abbreviation and FIPS lookups directly from R's built-in `state.name`/`state.abb` vectors plus a DC entry, replicating `statastates` without requiring an external package.
- **Synthetic fallbacks** — each download block checks `file.exists()` and falls back to a randomly generated dataset of the correct structure if the file is unavailable. Fallback use is reported in the log.
- **Path setup** — log paths are set automatically based on `Sys.info()[["user"]]`: `"marvi"` gets the full Dropbox path; all other users get `Output/logs/` relative to the working directory.

## Cross-Platform Validation

- The R translation was written to produce the same data objects (rows, columns, variable names and key types) as the Stata implementation given the same reformatted Excel inputs.
- The script prints row/column counts and panel dimensions after each major step to help verify results.
- Validation is against real downloaded data; synthetic fallback outputs will differ from Stata.

## Troubleshooting & Notes

- **Missing or different sheet names:** The R script checks for common sheet names and falls back to the first available sheet; it reports which sheet it selected. Inspect downloaded `.xlsx` files if results look unexpected.
- **Synthetic data in log:** If the log shows `[Using synthetic data for ...]`, the corresponding Excel file could not be downloaded. Check internet connectivity or download the file manually from the data repository and place it in the working directory.
- **Package installation problems (R):** If your environment prevents automatic installation, install packages manually and restart R.
- **Missing `id` or `year` columns:** Panel creation requires `id` and `year`. If reformatted sheets use different names, rename columns or edit the script mapping.
- **Large raw NCES files:** Use the reformatted versions available in the data repo for these examples. If loading full NCES raw releases, use a 64-bit R with adequate memory or Stata/SE or Stata/MP.

## Reproducibility & Where to Look in the Code

- `Stata_code4.do` provides step-by-step commands for downloading, importing, reshaping, and merging (with comments indicating where to save outputs).
- `R_code4.R` mirrors the Stata workflow and includes the `parse_nces_wide()` helper and synthetic fallback logic noted above. Both scripts include commented save/export lines for producing `.dta` or `.csv` outputs if you wish to persist results.

## How to Use the Outputs

- `complete_panel` (R) / merged panel dataset (Stata): inputs for regression and panel methods in later chapters (e.g., Chapters 6–7). The long-form `id`/`year` structure is ready for `plm`/`xtset`.
- Example save commands (uncomment in code if desired):
  - R: `write_dta(complete_panel, "Complete_Panel_Dataset.dta")` or `write.csv(complete_panel, "Complete_Panel_Dataset.csv")`
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

March 29, 2026
