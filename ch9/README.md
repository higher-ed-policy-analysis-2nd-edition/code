# Chapter 9 - Advanced Statistical Techniques: II

This directory contains the complete code for replicating the analyses in Chapter 9 of _Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)_ by Marvin A. Titus.

The focus of this chapter is on heterogeneous coefficient regression (HCR) methods using dynamic common correlated effects (DCCE) and mean group (MG) estimators for macro panel data.

## Files

| File              | Description                                                |
|-------------------|------------------------------------------------------------|
| `Stata_code9.do`  | Stata code for Chapter 9 analyses. Requires Stata 19+.     |
| `R_code9.txt`     | R code translation of the Stata script, using packages like `plm`, `urca`, and `dplyr`. Compatible with R 4.2+. |
| `R_code9`         | Native R script file version of `R_code9.txt`.             |
| `README.md`       | This file. Overview of code and usage instructions.        |

## Description

The analyses in this chapter demonstrate:

- Panel unit root tests using Im‑Pesaran‑Shin (IPS)
- Panel cointegration tests (Johansen and residual‑based)
- Testing for cross‑sectional dependence
- Testing for coefficient homogeneity
- Estimation using HCR with DCCE and MG estimators

The R code approximates the Stata routines using open‑source equivalents and includes custom functions to estimate DCCE‑MG models with inference.

## Requirements

### R (for `R_code9.txt` / `R_code9`)

- R version 4.2 or higher
- R packages:
  - `plm`
  - `urca`
  - `dplyr`
  - `ggplot2`
  - `lmtest`
  - `sandwich`
  - `tseries`
  - `zoo`

Install all packages via:

```r
install.packages(c("plm", "urca", "dplyr", "ggplot2", "lmtest", "sandwich", "tseries", "zoo"))
```