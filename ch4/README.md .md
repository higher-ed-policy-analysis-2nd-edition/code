# Chapter 4 — Creating Datasets and Managing Data

Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)  
Author: Marvin A. Titus  
Repository: higher-ed-policy-analysis-2nd-edition/code/ch4

This directory contains the reproducible code used to create, reshape, and manage the core datasets used throughout the book. The chapter demonstrates how to import reformatted source tables, reshape wide → long, create time-series and panel structures, add state identifiers (abbreviations and FIPS), merge multiple variables into a single long-panel dataset, and save outputs for analysis. Both Stata and R implementations are provided and produce equivalent data objects suitable for replication and classroom use.

---

## Contents

- `Stata_code4.do`  
  Complete Stata do-file for Chapter 4 (tested in Stata 19.5). Shows downloading reformatted Excel files from the data repository, importing worksheets, reshaping with `reshape`, declaring `tsset`/`xtset`, and merging panel variables.

- `R_Code4.R` (also provided as `R_Code4.txt`)  
  Full R translation of the Stata workflow. Implements the same steps using tidyverse, haven, readxl, plm and writexl. Includes helper functions to:
  - safely downcast integer-valued doubles to integer (Stata `compress` analogue),
  - add U.S. state abbreviations and FIPS codes (replicating `statastates`),
  - reshape wide sheets to long panel form and create `pdata.frame` objects.

- This `README.md`  
  Overview, quick start, a section-by-section summary of what the code does, validation and troubleshooting notes, and citation information.

---

## Datasets (downloaded by the scripts)

The scripts download reformatted Excel files from the book data repository at runtime. You do not need to commit these large data files to the code repo — the code fetches the raw files when run.

Primary data sources used by Chapter 4:
- `tabn302_50.xlsx` — reformatted NCES Table 302.50 (state-level HS graduates / enrollment)
- `tabn302_10.xlsx` — reformatted NCES Table 302.10 (time series: percent of HS graduates enrolled in PSE, 1960–2016)
- `tabn304_70.xlsx` — reformatted NCES Table 304.70 (undergraduate enrollment by state and year)

Data repository (raw files):  
https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch4

---

## Quick start

### Requirements

For Stata users:
- Stata 19 or later (code tested in Stata 19.5).  
- Optional: `statastates` (user-written) may be used to add state identifiers in Stata:
  ```stata
  ssc install statastates, replace
  ```

For R users:
- R >= 4.0 (script tested in R 4.x).  
- Required packages (the R script installs any missing packages automatically if allowed):  
  tidyverse, haven, readxl, plm, writexl

To install manually:
```r
install.packages(c("tidyverse","haven","readxl","plm","writexl"), dependencies = TRUE)
```

### Running the code

1. Clone the repository and open the chapter folder:
   ```bash
   git clone https://github.com/higher-ed-policy-analysis-2nd-edition/code.git
   cd code/ch4
   ```

2. Stata:
   - Open Stata, set the working directory to this folder (or edit paths in the do-file), then run:
     ```stata
     do Stata_code4.do
     ```
   - The do-file downloads the reformatted Excel files (if not already present), imports sheets, reshapes data, and demonstrates panel declaration and merges.

3. R:
   - Open `R_Code4.R` (or `R_Code4.txt`) in RStudio. Optionally set a persistent download directory at the top of the script:
     ```r
     ch4data <- "C:/Users/YourName/Documents/book-materials/ch4/data"
     dir.create(ch4data, recursive = TRUE, showWarnings = FALSE)
     setwd(ch4data)
     ```
   - Source the script:
     ```r
     source("R_Code4.R")
     ```
   - The R script prints progress messages, creates the primary objects, and leaves the option to save data files (commented save/write_dta/write_csv lines are provided).

---

## What the code does (section-by-section)

- Section 4.2.1 — Primary data
  - Demonstrates simple manual data entry and saving (Stata `input` / R data.frame).

- Section 4.2.2 — Cross-sectional dataset (NCES Table 302.50)
  - Downloads `tabn302_50.xlsx` (reformatted) from the book data repository.
  - Imports the reformatted worksheet, inspects structure, and adds state identifiers (abbreviation + FIPS).
  - Adds descriptive variable labels and applies downcasting to integer types where appropriate.
  - Optional save/export lines are included for `.dta` or `.csv` output.

- Section 4.2.2 — Time-series dataset (NCES Table 302.10)
  - Downloads `tabn302_10.xlsx`, imports the reformatted `reformatted` sheet, and ensures `year` is integer.
  - Declares the dataset as time-series (`tsset` in Stata; keep `year` integer in R).
  - Optional save/export lines are provided.

- Section 4.2.2 — Panel dataset (NCES Table 304.70)
  - Downloads `tabn304_70.xlsx`, selects an appropriate sheet (script scans common sheet names and falls back to the first sheet).
  - Reads a wide table of state-year data and reshapes to long using `reshape` (Stata) or `pivot_longer` (R).
  - Ensures `id` and `year` exist, applies downcasting, and declares the dataset as panel data (`xtset` or `pdata.frame`).
  - Demonstrates checking pdim (panel dimensions) and balanced panel status in R.

- Section 4.2.2 — Merging multiple variables into the panel
  - Shows how to combine high-school graduates (HSGrad), need-based and merit-based financial aid, and other variables into a single long-panel dataset.
  - In Stata: uses `joinby` and `reshape`. In R: uses `left_join` and tidyverse joining idioms.
  - Produces a `complete_panel` object and declares it as `pdata.frame` for use with panel models.

---

## Implementation notes (R)

- Helper functions:
  - `downcast_double()` — Converts double-precision numeric columns that are integer-valued into integers, safely (replicates Stata’s `compress`).
  - `add_state_identifiers(df, state_col = "State")` — Adds `state_abbrev` and `state_fips` by matching state names; includes District of Columbia when present. This mirrors Stata’s `statastates`.

- Robust sheet selection:
  - The R script checks for common sheet names (`Undergrads`, `Undergrad`, `Undergraduate`, `HSGrad`, etc.) and uses the first available sheet if none match exactly. This increases tolerance to small formatting differences.

- Memory and types:
  - The R code downcasts numeric columns where appropriate to reduce memory and match Stata storage types. If you modify the script to read the full NCES raw files rather than reformatted versions, expect increased memory usage.

---

## Validation & reproducibility

- The R translation was written to produce the same data objects (rows, columns, variable names and key types) as the Stata implementation when given the same reformatted Excel inputs.
- The scripts print messages with row/column counts after each major step to make it easy to verify results.
- The code relies on reformatted worksheets prepared for the book — these are available in the data repository linked above.

---

## Troubleshooting

- Missing or different sheet names:
  - Inspect the `.xlsx` files downloaded in your working directory; the R script reports which sheet it used if a matching sheet name is not found.

- Package installation problems (R):
  - If your system prevents automatic installation, install required packages manually:
    ```r
    install.packages(c("tidyverse","haven","readxl","plm","writexl"))
    ```

- Missing `id` or `year` columns:
  - Panel creation requires `id` and `year`. If your reformatted sheet uses different names, rename columns or edit the script mapping.

- Large raw NCES files:
  - Use reformatted versions included in the data repo for these examples. If you want to load the full NCES releases, use 64-bit R with ample memory or Stata/SE or Stata/MP for very large tables.

---

## How to use the outputs

- `complete_panel` (R) / merged panel dataset (Stata): Use these as inputs to regression or panel methods in later chapters (e.g., Chapters 6–7). They are formatted in long form with `id` and `year` ready for `plm`/`xtset` usage.
- Optionally save outputs:
  - R: `write_dta(complete_panel, "Complete_Panel_Dataset.dta")` or `write_csv(...)`.
  - Stata: uncomment `save` lines in the do-file to export `.dta` files.

---

## Citation & license

Please cite the book when using these scripts:

Titus, M. A. (2025). *Higher Education Policy Analysis Using Quantitative Techniques* (2nd ed.). Springer.  
GitHub repository: https://github.com/higher-ed-policy-analysis-2nd-edition/

Refer to the repository top-level LICENSE for terms of use. The code is provided for educational and research purposes; please acknowledge the author and the book when reusing substantial portions.

---

## Support & contributions

- Found an issue or have an improvement?  
  1. Open an issue on GitHub: https://github.com/higher-ed-policy-analysis-2nd-edition/code/issues  
  2. Submit a pull request with a descriptive title and clear change description.

- Contact: marvinatitus@gmail.com

---

Last updated: 2025-11-17