# Chapter 4 — Creating Datasets and Managing Data

This folder holds the Stata and R code for Chapter 4 of *Higher Education Policy Analysis Using Quantitative Techniques* (2nd Edition, Springer). The chapter covers dataset creation and management: entering primary data by hand, importing and cleaning secondary NCES tables, declaring time-series and panel structures, reshaping wide to long, and merging multiple panel sources into a single analysis-ready file.

## Empirical Setting

Chapter 4 builds up several small datasets rather than working with one running example. A hand-entered primary dataset illustrates manual data entry; a reformatted NCES cross-sectional table (high school graduates enrolling in postsecondary education by state, 2012) illustrates cross-sectional construction and state-identifier matching; a second NCES table (percent of high school graduates enrolled in postsecondary education, 1960–2016) illustrates time-series declaration; and two further NCES tables (undergraduate enrollment and high school graduates, both by state and year) are each reshaped from wide to long and joined with need- and merit-based state financial aid panels to produce the chapter's combined panel dataset.

## File Structure

### Stata Script

| File | Description |
|---|---|
| `Stata_code4.do` | Single Stata script. Sets output paths and log, then runs all analyses in Section 4.2.1 (primary data entry) and Section 4.2.2 (cross-sectional, time-series, and panel dataset construction, reshaping, and joining), in order. |

### R Translation

| File | Stata Counterpart | Description |
|---|---|---|
| `R_code4.R` | `Stata_code4.do` | Complete R translation. Includes a `parse_nces_wide()` helper that parses NCES Digest tables' multi-row headers (title, sub-header, and column-number rows) without relying on a fixed cell range, and builds state abbreviation/FIPS lookups from R's built-in `state.name`/`state.abb` vectors in place of `statastates`. If a source Excel file cannot be downloaded, the script substitutes a synthetic dataset of the same structure and logs `[Using synthetic data for ...]` so the script can still run to completion; synthetic-fallback output will not match the Stata results. |

## Data

Datasets are in the [data repository](https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch4) and are downloaded automatically by both scripts at runtime, with two exceptions noted below.

| File | Used in | Description |
|---|---|---|
| `tabn302_50.xlsx` | Section 4.2.2 | NCES Digest Table 302.50: high school graduates enrolled in postsecondary education, by state, 2012 |
| `tabn302_10.xlsx` | Section 4.2.2 | NCES Digest Table 302.10: percent of high school graduates enrolled in postsecondary education, 1960–2016 (time series) |
| `tabn304_70.xlsx` | Section 4.2.2 | NCES Digest Table 304.70, sheet `Undergrads`: undergraduate enrollment by state and year (reshaped wide to long) |
| `tabn219_20.xlsx` | Section 4.2.2 | NCES Digest Table 219.20, sheet `HSGrad`: high school graduates by state and year (reshaped wide to long) |
| `Undergraduate state financial aid - need.dta` | Section 4.2.2 | State-level need-based financial aid panel, joined onto the enrollment panel |
| `Undergraduate state financial aid - merit.dta` | Section 4.2.2 | State-level merit-based financial aid panel, joined onto the enrollment panel |

> **Note:** Unlike the four Excel source files, the two financial-aid `.dta` files are **not** downloaded automatically by either script. Both scripts check for them with a local file-existence test and skip the join silently if they're absent. Download them from the data repository and place them in the working directory before running the `joinby` / `inner_join` steps in Section 4.2.2.

## Running the Code

**Stata** (requires version 19; tested in Stata 19.5):
```stata
do Stata_code4.do
```
Two user-written commands are required: `statastates` (Schpero 2018), used to add state identifiers, and `sreshape` (Simons 2016), used for the wide-to-long reshapes.
```stata
ssc install statastates, replace
* sreshape is not on SSC — install from within Stata with:
search sreshape, all
```

**R** (requires R 4.4.x or later):
```r
source("R_code4.R")
```
Required packages (`readxl`, `writexl`, `haven`, `dplyr`, `tidyr`, `psych`, `plm`) are installed automatically if missing. The R script needs no equivalent of `statastates` or `sreshape` — state identifiers are built from base R's `state.name`/`state.abb`, and reshaping uses `tidyr::pivot_longer()`.

> **Note:** Output paths switch automatically based on the OS username (`if c(username) == "marvi"` in Stata; `Sys.info()[["user"]]` in R). All other users get the default relative-path output (`Output/graphs/`, `Output/logs/`), created automatically at runtime. A `graphs_dir` global is defined in both scripts for structural consistency with other chapters, but Chapter 4 produces no plots, so it goes unused.

## Methods Covered

- **Primary data entry (Section 4.2.1):** manual data entry and export to `.dta`/`.csv` (`input`; R `data.frame()`)
- **Cross-sectional dataset construction (Section 4.2.2):** importing a reformatted NCES worksheet and adding state identifiers (`statastates`; R lookup built from `state.name`/`state.abb`)
- **Time-series declaration (Section 4.2.2):** declaring and verifying time-series structure (`tsset`, `tsdes`)
- **Panel dataset construction (Section 4.2.2):** reshaping wide to long (`sreshape`; R `pivot_longer()`) and declaring panel structure (`xtset`, `xtdes`; R `pdata.frame()`)
- **Joining panel datasets (Section 4.2.2):** merging multiple panel sources on `id`/`year` (`joinby`; R `inner_join()`)
- **Basic panel data management:** lagged and first-differenced variables (`L.`/`D.` operators), per-student ratio variables, subsetting, and exporting to CSV/Excel

## Output

This chapter produces no exported figures — `describe`, `summarize`, and `browse` are used throughout to inspect and verify the data, and all of that output is captured in the log (`Chapter4_Stata_output.log` / `Chapter4_R_output.log`). When the R script falls back to synthetic data for a missing download, that substitution is also recorded in the log.

## Related Chapters

Chapter 4 produces the datasets that later chapters build on directly: the time-series file (`Example_4_2_2_TS.dta`) and the cross-sectional file (`tabn302_50.xlsx`) are reused in Chapters 5 and 6 for data-structure inspection and descriptive statistics, respectively. The chapter also introduces synthetic data conceptually as a third data type alongside primary and secondary data; Chapter 12 returns to this idea and demonstrates full synthetic-dataset construction and Bayesian microsimulation.
