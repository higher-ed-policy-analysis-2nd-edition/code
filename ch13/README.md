# Chapter 13 — Presenting Analyses to Policymakers

Code companion for Chapter 13 of *Higher Education Policy Analysis Using Quantitative Techniques* (2nd Ed.), Marvin A. Titus. Every analytical routine in this chapter is fully implemented in **both Stata and R**, run from a single master driver in each language. This README documents the code only — for the datasets these scripts load, see the [`data`](https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch13) repository's `ch13` folder.

## Master drivers

| File | Role |
|---|---|
| `Stata_code13.do` | Stata master driver. Sets up paths, log, package checks, and presentation-style globals, then calls each sub-script below in order. |
| `R_code13.R` | R translation of the same driver. Sets up paths, log, a `fig13_registry` environment (used later by `PolicymakerDeck13.R`), and package checks, then sources each `.R` sub-script in order. |

Both drivers use username-conditional path logic (`c(username) == "marvi"` in Stata, `Sys.getenv("USERNAME") == "marvi"` in R) so the same script runs unmodified on the author's machine or a clean clone, falling back to the current working directory for anyone else.

## Sub-scripts (Stata / R pairs)

| Section | Stata | R | Content |
|---|---|---|---|
| 13.2 | `DescriptiveTables13.do` | `DescriptiveTables13.R` | Descriptive statistics tables (Table 13.1, 13.2), exported to Word |
| 13.2.2 / 13.7.4 | `EstimationTables13.do` | `EstimationTables13.R` | Driscoll-Kraay panel SEs and elasticity table (Table 13.Appendix) |
| 13.3 | `Maps13.do` | `Maps13.R` | Choropleth map, percent change in state appropriations per FTE (Fig. 13.2). AK/HI not shown — see file header for the limitation note. |
| 13.4 | `TrendGraphs13.do` | `TrendGraphs13.R` | State/regional trend comparisons (Figs. 13.3–13.6) |
| 13.5 | `RegressionPlots13.do` | `RegressionPlots13.R` | Associational coefficient plots and marginal effects — pooled OLS, CCEMG, DCCE-MG/ARDL (Figs. 13.7–13.11). Deliberately kept separate from Section 13.6: these are associational, not causal, results. Fig. 13.9 (DCCE-MG/ARDL) has no R equivalent implemented yet, matching the Stata original's own incomplete state. |
| 13.6 | `CausalPlots13.do` | `CausalPlots13.R` | Causal inference results: TWFE event study (Fig. 13.12), ETWFE staggered adoption (Fig. 13.13), RD (Figs. 13.14–13.15), synthetic control (Figs. 13.16–13.17). Fig. 13.13b (`xthdidregress`/`atetplot`) has no CRAN-available R equivalent in this environment — documented placeholder. |
| 13.7 | `MTE_CATE_Plots13.do` | `MTE_CATE_Plots13.R` | IV, CATE, and MTE/MPRTE presentation. Sections 13.7.1, 13.7.2a, and 13.7.3 are documented stubs in both languages, pending Chapter 11/12 finalization — this section is a pointer to Chapter 11's worked examples, not a standalone one. |
| 13.8 | `BayesianPlots13.do` | `BayesianPlots13.R` | Bayesian posterior density (Fig. 13.18) and CBA waterfall (Fig. 13.19), drawing on Chapter 12's microsimulation output. See **Known issues** below. |

## Supplementary: PowerPoint export

`PolicymakerDeck13.R` (**R only** — Stata has no `putpptx` command and no supported dependency-free path to an editable `.pptx`) builds an example five-slide deck directly from this chapter's own validated figures, read live from the `fig13_registry` environment populated during the `R_code13.R` run:

1. Descriptive — Fig. 13.2 (choropleth map)
2. Associational — Fig. 13.7 (OLS coefficient plot)
3. Causal — Fig. 13.12 (TWFE event study)
4. Uncertainty — Fig. 13.18 (posterior density)
5. Bottom line — Fig. 13.19 (CBA waterfall)

Figures are embedded as fully editable vector graphics via `officer::read_pptx()` and `rvg::dml()` when the `rvg` package is available, falling back to static PNG embedding otherwise (`rvg` is intentionally warn-only rather than auto-installed — see the driver's comments for the Windows font-lock issue that motivated this). `PolicymakerDeck13.R` is called at the very end of `R_code13.R`, after every other sub-script, so the deck always reflects a fully validated run rather than partial or stale figures.

## Known issues

- **Fixed**: `BayesianPlots13.do` previously used an invalid Stata display format (`%+9.0f` — Stata has no `+` sign flag) to label the CBA waterfall's dollar values, which aborted the script before Fig. 13.19 was ever built. Fixed by formatting the magnitude with `%9.0f` and prepending `+` manually for benefit bars; cost bars already carry their own `-` sign from the negated `delta` values. Confirmed producing `fig13_19_cba_waterfall` in both languages as of this fix.
- **Open**: `BayesianPlots13.do` reads `ch12/sim_results_ch12.dta` from a local path rather than downloading it from GitHub the way `CausalPlots13.do` does for its Chapter 10 CSVs. This is a reproducibility gap for anyone running from a clean clone — see the [`data`](https://github.com/higher-ed-policy-analysis-2nd-edition/data) repo's README for the same note. Not yet fixed.
- **Documented stubs, not bugs**: Fig. 13.9, Fig. 13.13b, and Sections 13.7.1/13.7.2a/13.7.3/13.7.4 are intentionally incomplete in both languages, matching the Stata original's own scope. These are not translation gaps — the content genuinely doesn't exist yet pending other chapters' finalization.

## System requirements (R)

The R pipeline (`R_code13.R` and its eight sub-scripts) was tested and confirmed to run error-free under **R 4.5.2**.

If more than one R version is installed (common on Windows, where each release installs to its own folder under `C:/Program Files/R/`), confirm which version is active before running this chapter's R code:

```r
R.version.string
```

In RStudio: **Tools > Global Options > General > R version**. Each R version maintains a separate, independent package library — installing a package while running one version does not make it available under a different version (e.g. via double-click, `Rscript`, or a batch file pointing at another installation).

`svglite` requires `systemfonts >= 1.3.0`. If you see:

```
Error in loadNamespace(...) :
  namespace 'systemfonts' 1.2.2 is already loaded, but >= 1.3.0 is required
```

an outdated `systemfonts` is installed under the R version currently running the script. Fix: close all R/RStudio processes completely, open one fresh session under the intended R version, run `install.packages(c("systemfonts", "svglite", "gdtools", "rvg"), type = "binary")`, restart R once more, and confirm `packageVersion("systemfonts")` reports `>= 1.3.0` before re-running.

Warnings such as `package 'officer'/'rvg' was built under R version 4.5.3` are harmless and can be ignored — they only mean the package was compiled under a slightly newer R than the one in use.

## Package requirements

**Stata** (19.5): `asdoc`, `rescale`, `maptile`, `spmap`, `statastates`, `lgraph`, `coefplot`, `xtmg`, `xtdcce2`, `xtscc`, `rdrobust`, `synth`, `sdid`, `jwdid`, `reghdfe`, plus `grstyle`/`palettes`/`colrspace` for presentation styling (auto-installed; safe since these only affect graph appearance, not estimation). `dtable`, `etable`/`collect`, and `cate`/`categraph` are native Stata 18/19 commands — no install needed, but the driver checks `c(stata_version)` and warns if unavailable. Several non-SSC installs are required once (`asdoc`, `rescale`, `maptile`'s shapefile, `esttab`/`estout`) — see the driver's preamble for the exact `net install` commands.

**R**: `haven`, `dplyr`, `tidyr`, `ggplot2`, `plm`, `sandwich`, `lmtest`, `quadprog`, `sf`, `maps`, `officer` (warn-only, not auto-installed — these carry real estimation-method choices). `rvg` (plus `systemfonts`/`gdtools`) is optional, needed only for editable-vector figures in `PolicymakerDeck13.R`; install once in a fresh R session with `install.packages(c("systemfonts", "gdtools", "rvg"), type = "binary")`.

## Output

Both drivers write to the same structure under the chapter root:
```
Output/
├── figures/   svg + pdf + high-res png (Stata: pubexport; R: matching helper)
├── tables/    Word (.docx) tables via asdoc / officer
├── logs/      Chapter13_Stata_output.log, Chapter13_R_output.log
└── Chapter13_Policymaker_Deck_R.pptx   (R only)
```

Figures follow the naming convention `fig13_N_descriptive_name`. In Stata, all named graphs are saved as `.gph` files (via `graph save`) at the end of the master run, before the closing `clear`/`log close` block, so they remain reloadable via `graph use` even after the script finishes.

## Presentation style

Both languages target grayscale-safe print output — `s2mono` scheme in Stata, `theme_springer()` in R — since the printed book renders in black and white. Section 13.9 (prose-only; no executable code) discusses the presentation habits these figures are built to demonstrate: leading with the finding rather than the method, one idea per visual, stating uncertainty as a probability rather than a hedge, and closing on a single actionable number.
