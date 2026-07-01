# Chapter 6 — Using Descriptive Statistics and Graphs

This folder holds the Stata and R code for Chapter 6 of *Higher Education Policy Analysis Using Quantitative Techniques* (2nd Edition, Springer). The chapter introduces descriptive statistics and exploratory data analysis (EDA) for higher education policy research: measures of central tendency and dispersion, frequency distributions and crosstabulations, one- and two-way ANOVA, panel-data description, and exploratory graphics.

## Empirical Setting

Unlike later chapters built around a single running example, Chapter 6 works through several previously introduced datasets so that each descriptive technique stays tied to a familiar substantive context. Arithmetic, geometric, and harmonic means are illustrated using the cross-sectional public/private high school graduate data first introduced in Chapter 4; the coefficient of variation is illustrated using a SHEEO state finance panel; frequency distributions, crosstabulations, and ANOVA use the condensed HSLS:09 dataset with an earnings variable introduced in Chapter 5; and panel-data description and exploratory graphics use a 50-state, 27-year (1990–2016) state appropriations and tuition panel.

## File Structure

### Stata Script

| File | Description |
|---|---|
| `Stata_Code6.do` | Single Stata script. Sets output paths and log, then runs all analyses in Section 6.2 (central tendency, dispersion, distributions, panel description, ANOVA) and Section 6.3 (exploratory graphics), in order. |

### R Translation

| File | Stata Counterpart | Description |
|---|---|---|
| `R_code6.R` | `Stata_Code6.do` | Complete R translation. Reproduces all descriptive statistics, crosstabulations, ANOVA tests, and graphics using `psych`, `plm`, and `ggplot2` equivalents of the Stata commands. |

## Data

Datasets are in the [data repository](https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch6) and are downloaded automatically by both scripts at runtime.

| File | Used in | Description |
|---|---|---|
| `tabn302_50.xlsx` (from `/data/ch4`) | Section 6.2.1 | Public/private high school graduates enrolling in postsecondary education, by state and DC, 2012 |
| `Example_6_2_2.dta` | Section 6.2.2 | SHEEO state finance panel: net tuition revenue and FTE enrollment, 50 states × 9 fiscal years (FY 2010–FY 2018) |
| `Example_6_2_3.dta` | Sections 6.2.3–6.2.4 | Condensed HSLS:09 dataset with hourly earnings (`EarnHr`), race/ethnicity (`X1RACE`), and sex (`X1SEX`) |
| `Example_6_3.dta` | Sections 6.2.3, 6.3.1 | State-level panel: state appropriations (`stapr`), net tuition revenue (`netuit`), FTE enrollment (`fte`), regional-compact membership (`region_compact`), undergraduate merit-aid indicator (`ugradmerit`); 50 states × 27 years (1990–2016) |

## Running the Code

**Stata** (requires version 19; tested in Stata 19.5):
```stata
do Stata_Code6.do
```
The user-written `aaplot` command (Cox 2015) is required for the annotated scatter plots in Section 6.3.1; install it before running with:
```stata
ssc install aaplot, replace
```

**R** (requires R 4.4.x or later):
```r
source("R_code6.R")
```
Required packages (`readxl`, `haven`, `dplyr`, `tidyr`, `psych`, `plm`, `ggplot2`, `scales`, `patchwork`) are installed automatically if missing.

> **Note:** Output paths switch automatically based on the OS username (`if c(username) == "marvi"` in Stata). All other users receive the default relative-path output (`Output/graphs/`, `Output/logs/`), which is created automatically at runtime.

## Methods Covered

- **Measures of central tendency (Section 6.2.1):** arithmetic, geometric, and harmonic means (`ameans`)
- **Measures of dispersion (Section 6.2.2):** coefficient of variation, by state and by fiscal year (`tabstat`)
- **Distributions (Section 6.2.3):** frequency distributions and crosstabulations of a categorical variable, with and without recoding (`prop`, `tab`, `tabulate`); panel-data description (`xtset`, `xtdescribe`, `xttab`, `xttrans`)
- **Testing differences in means across groups (Section 6.2.4):** one-way and two-way ANOVA, post-hoc pairwise comparisons with Bonferroni correction, and interaction tests (`anova`, `oneway`, `pwmean`, `testparm`)
- **Exploratory graphics (Section 6.3.1):** histograms with normal-curve overlays, box charts (overall and by category), scatter plots with fitted regression lines and point labels, and annotated regression scatter plots (`aaplot`)

## Output

Running the scripts produces the full sequence of output referenced in Chapter 6. Figs. 6.1–6.7 are descriptive-statistics tables, frequency distributions, and crosstabulations captured in the log output; Figs. 6.8–6.17 are the histograms, box charts, and scatter plots exported as PNG files to `Output/graphs/`, with Stata figures carrying a `_Stata.png` suffix and R figures a `_R.png` suffix.

## Related Chapters

Chapter 6 reuses datasets first introduced in Chapter 4 (cross-sectional enrollment data) and Chapter 5 (the HSLS:09 dataset), extending them with additional descriptive and graphical techniques. The state-level panel used in Sections 6.2.3 and 6.3.1 previews the panel structure used throughout Chapter 7, which builds on these descriptive foundations to introduce pooled OLS, fixed-effects and random-effects regression, and instrumental variables estimation.
