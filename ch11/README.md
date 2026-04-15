# Chapter 11 Code: Bayesian MTE Microsimulation

**Repository:** `higher-ed-policy-analysis-2nd-edition / code`  
**Path:** [`code/ch11`](https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch11)  
**Book:** *Higher Education Policy Analysis Using Quantitative Techniques*, Second Edition — Springer

\---

## Overview

This directory contains the analysis scripts for **Chapter 11: Bayesian MTE Microsimulation — Cost–Benefit Analysis of a $100k Lifetime Cap on Grad PLUS Loans**. The scripts implement a ten-section pipeline that generates a synthetic Baccalaureate and Beyond (B\&B)-mirroring dataset, estimates the marginal treatment effect (MTE) of master's degree completion using a cubic polynomial control function, and evaluates the net social benefit of a $100,000 lifetime cap on federal Grad PLUS borrowing through a Bayesian parametric-bootstrap microsimulation (S = 1,000 posterior draws).

The Stata script (`Stata\\\\\\\_code11.do`) is the primary implementation. The R script (`R\\\\\\\_code11.R`) is a full cross-platform translation that produces equivalent figures and results for validation. Both scripts use random seed `20251201` and identical global parameters.

\---

## Files

|File|Language|Description|
|-|-|-|
|[`Stata\\\\\\\_code11.do`](https://github.com/higher-ed-policy-analysis-2nd-edition/code/blob/main/ch11/Stata_code11.do)|Stata|Primary script — data generation through figures; Stata 19+|
|[`R\\\\\\\_code11.R`](https://github.com/higher-ed-policy-analysis-2nd-edition/code/blob/main/ch11/R_code11.R)|R|Full R translation — Figures 11.1–11.7; R 4.4+|

The companion dataset is in the data repository:  
[`data/ch11/Example\\\\\\\_11\\\\\\\_1.dta`](https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch11)

\---

## Pipeline

Both scripts follow the same ten-section structure:

|Section|Description|
|-|-|
|1|Setup, directory structure, global parameters|
|2|Synthetic data generation — B\&B-mirroring panel, N = 8,000|
|3|Descriptive statistics and policy exposure|
|4|Probit selection model and propensity score (`ga\\\\\\\_funding\\\\\\\_adj` as instrument)|
|5|MTE estimation — cubic polynomial control function|
|6|Policy simulation — cap-binding student identification and displacement probabilities|
|7|Bayesian microsimulation — S = 1,000 parametric posterior draws|
|8|Cost–benefit decomposition|
|9|Posterior summaries, credible intervals, and formal inference|
|10|Figures 11.1–11.7|

### CBA Framework

```
Net Benefit(s) = Fiscal\\\\\\\_Savings(s) − Behavioral\\\\\\\_Cost(s) + Efficiency\\\\\\\_Gain(s)

Fiscal savings:    reduced federal lending on overage × subsidy rate
Behavioral cost:   lost human capital for cap-displaced completers with MTE > 0
Efficiency gain:   avoided loss from removing completers with MTE ≤ 0
```

\---

## Global Parameters

Both scripts use the following shared parameters:

|Parameter|Value|Description|
|-|-|-|
|`seed`|`20251201`|Random seed (identical in Stata and R)|
|`discount\\\\\\\_rate`|0.03|Annual social discount rate (3%)|
|`career\\\\\\\_years`|30|Earnings horizon for PV calculation|
|`subsidy\\\\\\\_rate`|0.20|Government subsidy rate on Grad PLUS principal|
|`cap\\\\\\\_threshold`|100|Policy cap ($000s)|
|`base\\\\\\\_salary`|47,000|Pooled mean annual salary for non-completers ($)|
|`S\\\\\\\_draws`|1,000|Number of Bayesian posterior draws|

\---

## Output Files

Running either script creates the following directory structure under the working directory:

```
Output/
├── logs/
│   ├── Chapter11\\\\\\\_Stata\\\\\\\_output.log
│   └── Chapter11\\\\\\\_R\\\\\\\_output.log
├── tables/
│   ├── Table11\\\\\\\_1\\\\\\\_CBA\\\\\\\_Summary.tex
│   └── sim\\\\\\\_results\\\\\\\_ch11.dta          (Stata only; simulation posterior draws)
└── graphs/
    ├── fig11\\\\\\\_1\\\\\\\_posterior\\\\\\\_nb\\\\\\\_Stata.png     / fig11\\\\\\\_1\\\\\\\_posterior\\\\\\\_nb\\\\\\\_R.png
    ├── fig11\\\\\\\_2\\\\\\\_mte\\\\\\\_policy\\\\\\\_Stata.png       / fig11\\\\\\\_2\\\\\\\_mte\\\\\\\_policy\\\\\\\_R.png
    ├── fig11\\\\\\\_3\\\\\\\_cba\\\\\\\_decomp\\\\\\\_Stata.png       / fig11\\\\\\\_3\\\\\\\_cba\\\\\\\_decomp\\\\\\\_R.png
    ├── fig11\\\\\\\_4\\\\\\\_param\\\\\\\_posteriors\\\\\\\_Stata.png / fig11\\\\\\\_4\\\\\\\_param\\\\\\\_posteriors\\\\\\\_R.png
    ├── fig11\\\\\\\_5\\\\\\\_mte\\\\\\\_byfield\\\\\\\_Stata.png      / fig11\\\\\\\_5\\\\\\\_mte\\\\\\\_byfield\\\\\\\_R.png
    ├── fig11\\\\\\\_6\\\\\\\_inst\\\\\\\_rev\\\\\\\_byfield\\\\\\\_Stata.png / fig11\\\\\\\_6\\\\\\\_inst\\\\\\\_rev\\\\\\\_byfield\\\\\\\_R.png
    └── fig11\\\\\\\_7\\\\\\\_full\\\\\\\_cost\\\\\\\_stack\\\\\\\_Stata.png  / fig11\\\\\\\_7\\\\\\\_full\\\\\\\_cost\\\\\\\_stack\\\\\\\_R.png
```

All figures are exported in grayscale at 1,200-pixel width for Springer print standards.

Published output files are archived at:  
[`output/ch11`](https://github.com/higher-ed-policy-analysis-2nd-edition/output/tree/main/graphs/ch11)

\---

## Requirements

### Stata (`Stata\\\\\\\_code11.do`)

* **Version:** Stata 19 or later (`version 19` is set at the top of the script)
* **User-written packages:** None required
* All estimation uses built-in `probit`, `reg`, `drawnorm`, `postfile`, and `kdensity` commands
* Working directory: set via `cd` at line 63; update the path for non-Dropbox machines

### R (`R\\\\\\\_code11.R`)

* **Version:** R 4.4 or later
* **Required packages:** `tidyverse`, `fixest`, `MASS`, `scales`, `broom`

```r
install.packages(c("tidyverse", "fixest", "MASS", "scales", "broom"))
```

* Working directory: set via `setwd()` at line 18; update the path for non-Dropbox machines

\---

## Notes on Script Alignment

The Stata and R scripts are designed to produce distributional equivalents, not row-identical results, because Stata and R use different pseudo-random number generators. Both scripts use seed `20251201` and identical DGP parameters; the resulting datasets share the same marginal distributions and structural relationships but differ at the individual observation level. The Stata `.dta` file is the canonical dataset referenced in the chapter text.

The Bayesian simulation loop in Stata uses `drawnorm` + `postfile`/`forvalues` with a pre-stored coefficient matrix (`B\\\\\\\_draws`) to avoid repeated dataset switching. The R translation uses `MASS::mvrnorm()` with a `for` loop over a `tibble`. Both approaches draw from the same asymptotic normal posterior `N(θ̂, V̂)` and produce equivalent posterior distributions.

\---

## Cross-Chapter Dependencies

Chapter 11 builds directly on the MTE estimation framework from Chapter 10:

* Chapter 10 code: [`code/ch10`](https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch10)
* The field-specific ATE differentials applied in Figure 11.5 (STEM: +0.134, Business: +0.885, Education: −0.165, Health: +0.038) are taken from Table 10.1 produced by `Stata\\\\\\\_code10.do`
* The MPRTE posterior saved as `mte\\\\\\\_bootstrap\\\\\\\_prte\\\\\\\_post.dta` in Chapter 10 is imported as the treatment-effect prior for the Chapter 12 simulation-based CBA

\---

## Citation

> Titus, M. A. (forthcoming). \\\\\\\*Higher Education Policy Analysis Using Quantitative Techniques\\\\\\\* (2nd ed.). Springer.

Code development was assisted by Claude (Anthropic, 2025). The author provided all methodological specifications and reviewed, tested, and validated all code and output.

