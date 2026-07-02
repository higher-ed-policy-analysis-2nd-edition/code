# Chapter 5 — Getting to Know Thy Data

This folder holds the Stata and R code for Chapter 5 of *Higher Education Policy Analysis Using Quantitative Techniques* (2nd Edition, Springer). The chapter covers practical data-preparation and data-discovery skills used throughout the book: inspecting dataset structure and storage types, declaring panel data, exploring missingness patterns, and running formal missing-data diagnostics, including Little's MCAR test and a panel-aware missing-data workflow.

## Empirical Setting

Chapter 5 works through several previously introduced and new datasets rather than a single running example. Storage-type inspection uses the Chapter 4 time-series dataset and a small illustrative panel; panel declaration and preparation use a SHEEO state finance panel; and missingness exploration and diagnostics use a truncated HSLS:09 student extract and an IPEDS institution-level panel. The chapter closes by returning to the SHEEO panel for a fully panel-aware missing-data workflow (detection, mechanism testing, and visualization in one command).

## File Structure

### Stata Script

| File | Description |
|---|---|
| `Stata_code5.do` | Single Stata script. Sets output paths and log, then runs all analyses in Section 5.2 (dataset structure, panel declaration), Section 5.3 (loading HSLS:09), and Section 5.4 (missingness summaries, subgroup patterns, legacy `xtmis`, Little's MCAR test, and the `xtmispanel` detect/test/graph workflow), in order. |

### R Translation

| File | Stata Counterpart | Description |
|---|---|---|
| `R_code5.R` | `Stata_code5.do` | Complete R translation. Reproduces the structure inspection, panel declaration, and missing-data workflow using `naniar` and `mice` in place of Stata's missing-data commands. Where Stata's `mcartest` has a covariate-dependent (CDM) variant with no direct R equivalent, the script substitutes a logistic-regression fallback (missingness indicator regressed on the covariate, with an LR test) to approximate the same check. |

## Data

Datasets are in the [data repository](https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch5) and are downloaded automatically by both scripts at runtime.

| File | Used in | Description |
|---|---|---|
| `Example_4_2_2_TS.dta` (from `/data/ch4`) | Section 5.2 | Time series: percentage of U.S. high school graduates enrolling in postsecondary education, 1960–2016 |
| `Example_5_0.dta` | Section 5.2 | Small panel dataset used to demonstrate storage-type inspection, compression, and `recast` |
| `Example_5_1.xlsx` | Section 5.2 | Raw SHEEO state finance worksheet; filtered and merged with state identifiers to build the SHEEO panel used again in Section 5.4.2 |
| `Public_use_HSLS_09_truncated.dta` | Section 5.3 | Truncated public-use HSLS:09 (2017 Student File) extract |
| `Example_5_3.dta` | Sections 5.3, 5.4.1 | The same eight HSLS:09 variables as a stand-alone file for readers without Stata/MP or Stata/SE |
| `Example_5_4_1.dta` | Section 5.4 | HSLS:09 extract with NCES missing-value codes already recoded to Stata system missing |
| `Example_5_4.dta` | Section 5.4 | IPEDS institution-level panel dataset used for the legacy `xtmis` demonstration |

> **Note:** Section 5.4.2 downloads a second, full-range copy of the SHEEO panel directly from the data repository as `Example5_2.dta` (no underscore before "2"), distinct from the `Example_5_2.dta` name used when the panel is built and saved locally in Section 5.2. Only the no-underscore file is hosted in the data repository; it spans FY 1980–2024 and includes the pre-2001 years in which `Appropriations` is missing, which gives the missing-data demonstration more to work with than the FY 2010–2024 file built locally in Section 5.2.

## Running the Code

**Stata** (requires version 19; tested in Stata 19.5):
```stata
do Stata_code5.do
```
Several user-written commands must be installed first:
```stata
ssc install statastates, replace
ssc install mdesc, replace
net install dm0085_1.pkg, replace      // missings
ssc install tomata, replace
ssc install xtmis, replace
net install st0318.pkg, replace        // mcartest
ssc install xtmispanel, replace
```

**R** (requires R 4.4.x or later):
```r
source("R_code5.R")
```
Required packages (`haven`, `readxl`, `dplyr`, `tidyr`, `plm`, `naniar`, `mice`, `ggplot2`, `scales`) are installed automatically if missing; `patchwork`, needed only for the combined missingness dashboard, is checked and installed separately at the point it's first used.

> **Note:** Output paths switch automatically based on the OS username (`if c(username) == "marvi"` in Stata). The R script's output paths are hardcoded to the same Dropbox location rather than branching on username — no path configuration is needed, but R users working outside that environment will need to edit `graphs_dir` and `log_path` at the top of the script before running.

## Methods Covered

- **Dataset structure (Section 5.2):** storage-type inspection, compression, and type recasting (`describe`, `compress`, `recast`); panel declaration (`xtset` / `pdata.frame()`)
- **Missingness summaries (Section 5.4):** variable-level missing-value tabulation and nested pattern detection (`mdesc`, `misstable tree`, `misstable patterns`; `naniar::miss_var_summary()`, `mice::md.pattern()`)
- **Subgroup missingness (Section 5.4):** missingness patterns by categorical subgroup (`missings`)
- **Legacy panel missingness (Section 5.4):** missing-observation counts by panel unit (`xtmis`)
- **Testing for Missing Completely at Random (Section 5.4.1):** Little's (1988) MCAR test, equal- and unequal-variance versions, and a covariate-dependent missingness (CDM) variant (`mcartest`; `naniar::mcar_test()` with a logistic-regression CDM fallback in R)
- **Panel-specific missing-data diagnostics (Section 5.4.2):** the `xtmispanel` detect/test/graph workflow — missingness by variable, panel unit, and time period; a panel-aware MCAR test with stored results; and a missingness heatmap and combined dashboard

## Output

Running the scripts produces the missingness tables and diagnostic output referenced in Chapter 5, written to the log, and the `xtmispanel` graph suite exported as PNG files. Stata exports all seven graph types (`heatmap`, `barvar`, `barpanel`, `bartime`, `pattern`, `timeline`, `combined`) with a `_Stata.png` suffix; R reproduces the five that have a direct `ggplot2` equivalent (`heatmap`, `barvar`, `barpanel`, `bartime`, `combined`) with an `_R.png` suffix — `pattern` and `timeline` are Stata-only. Logs are written as `Chapter5_Stata_output.log` and `Chapter5_R_output.log`.

## Related Chapters

Chapter 5 extends the time-series dataset introduced in Chapter 4 and prepares the SHEEO finance panel and HSLS:09 extract that Chapter 6 draws on for its descriptive-statistics and exploratory-graphics examples. More broadly, the missing-data awareness developed here — knowing what is missing, why, and how it is distributed across panel units and time — underlies the panel-data methods introduced starting in Chapter 7.

## Citation

If you use these materials in research or teaching, please cite:

> Titus, M. A. (2025). *Higher Education Policy Analysis Using Quantitative Techniques* (2nd ed.). Springer.

GitHub repository: https://github.com/higher-ed-policy-analysis-2nd-edition/

## Author & Contact

**Marvin A. Titus, Ph.D.**
Email: marvinatitus@gmail.com

## License

Code is provided for educational and research purposes. Refer to the repository license for terms of use.
