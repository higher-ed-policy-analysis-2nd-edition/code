*========================================================================
* Chapter 13: Presenting Analyses to Policymakers
* Higher Education Policy Analysis Using Quantitative Techniques, 2nd Ed.
* Marvin A. Titus
*
* Source (code): https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch13
* Source (data): https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch13
*
* Code development assisted by Claude (Anthropic). The author provided
* specifications and reviewed, tested, and validated all code.
*========================================================================
* Script tested in Stata 19.5
* Compatible with Stata version 19 or later
*
* IMPORTANT: Several routines in this chapter (dtable, etable/collect,
* cate/categraph, xthdidregress/hdidregress, and the native meanci/
* heatmap/rpspike plottypes) require Stata 18 or 19. Where a Stata-
* version dependency applies, the relevant sub-script documents a
* legacy fallback for earlier versions.
*========================================================================

*------------------------------------------------------------------------
* PREAMBLE
*------------------------------------------------------------------------
clear all
set more off
version 19
set scheme s2mono        // Monochrome scheme for Springer B&W print
set graphics on          // Ensure graph window is active throughout

*------------------------------------------------------------------------
* PATH SETUP (username-conditional)
*------------------------------------------------------------------------
if c(username) == "marvi" {
    global root_dir "C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 13"
}
else {
    global root_dir "`c(pwd)'"
}

global graphs_dir "$root_dir/Output/graphs"
global tables_dir "$root_dir/Output/tables"
global logdir     "$root_dir/Output/logs"
global syntax_dir "$root_dir/Syntax/Stata"
global data_dir   "$root_dir/Data/Stata"

capture mkdir "$graphs_dir"
capture mkdir "$tables_dir"
capture mkdir "$logdir"

*------------------------------------------------------------------------
* LOG
*------------------------------------------------------------------------
capture log close
log using "$logdir/Chapter13_Stata_output.log", replace text

di "Chapter 13 log opened: " c(current_date) " " c(current_time)
di "Graphs directory: $graphs_dir"
di "Tables directory: $tables_dir"

*========================================================================
* PACKAGE INSTALLATIONS (run once; comment out thereafter)
*========================================================================
* NOTE: dtable, etable, collect, and cate/categraph are official Stata
* commands (Stata 18/19) -- no installation required, only confirm the
* running version below. Everything else is community-contributed.

if c(stata_version) < 18 {
    di as error "WARNING: dtable, etable/collect not available -- Stata 18+ required."
}
if c(stata_version) < 19 {
    di as error "WARNING: cate/categraph and native meanci/heatmap/rpspike plottypes not available -- Stata 19 required."
}

local required_pkgs asdoc rescale maptile spmap statastates lgraph coefplot xtmg xtdcce2 xtscc rdrobust synth sdid jwdid reghdfe
foreach pkg of local required_pkgs {
    capture which `pkg'
    if _rc != 0 {
        di as error "Package `pkg' not found -- install with: ssc install `pkg', replace"
    }
}

* asdoc requires a non-SSC install:
* net install asdoc, from(http://fintechprofessor.com) replace
* rescale requires a non-SSC install:
* net install rescale, from(http://digital.cgdev.org/doc/stata/MO/Misc) replace
* maptile's US state shapefile requires a one-time geography install:
* maptile_install using "http://files.michaelstepner.com/geo_state.zip", replace
* esttab/estout requires:
* net install st0085_2.pkg, replace

*========================================================================
* SECTION 13.2 -- PRESENTING DESCRIPTIVE STATISTICS
*========================================================================
do "$syntax_dir/DescriptiveTables13.do"

*========================================================================
* SECTION 13.2.2 / 13.7.4 -- ESTIMATION RESULTS & MARGINAL EFFECTS TABLES
*========================================================================
do "$syntax_dir/EstimationTables13.do"

*========================================================================
* SECTION 13.3 -- CHOROPLETH MAPS
*========================================================================
do "$syntax_dir/Maps13.do"

*========================================================================
* SECTION 13.4 -- TREND GRAPHS AND SIMPLE COMPARISONS
*========================================================================
do "$syntax_dir/TrendGraphs13.do"

*========================================================================
* SECTION 13.5 -- PRESENTING MULTIVARIATE PANEL REGRESSION RESULTS
* (associational coefplot/marginal-effects examples -- not causal
* inference; kept as its own section ahead of 13.6 for that reason)
*========================================================================
do "$syntax_dir/RegressionPlots13.do"

*========================================================================
* SECTION 13.6 -- PRESENTING CAUSAL INFERENCE RESULTS
*========================================================================
do "$syntax_dir/CausalPlots13.do"

*========================================================================
* SECTION 13.7 -- PRESENTING IV, CATE, AND MTE RESULTS
*========================================================================
do "$syntax_dir/MTE_CATE_Plots13.do"

*========================================================================
* SECTION 13.8 -- PRESENTING BAYESIAN MICROSIMULATION AND CBA RESULTS
*========================================================================
do "$syntax_dir/BayesianPlots13.do"

*========================================================================
* SECTION 13.9 -- TAILORING PRESENTATION TO POLICYMAKER AUDIENCES
*========================================================================
* NOTE: primarily prose in the chapter text (state legislators vs.
* institutional leaders vs. federal policymakers). If a one-page Word
* template artifact is developed per the outline's open question #3, it
* belongs in its own AudienceTemplate13.do (putdocx-based), called here.

*------------------------------------------------------------------------
* SAVE ALL NAMED GRAPHS AS .GPH (for later recall via `graph use`)
*------------------------------------------------------------------------
* NOTE -- FIX: the original line here, `local gnames : dir "graph" dir
* "*"', used the wrong syntax -- Stata's extended macro function `dir`
* only lists files/folders on the filesystem, so it looked for a literal
* folder named "graph" and failed ("directory graph not found", r(601)).
* The correct way to enumerate graphs currently held in memory is the
* `graph dir` command, which returns the list in r(list).
*
* NOTE -- BUG FIX (critical ordering issue): this block used to run
* AFTER "clear all" below. Per Stata's own documentation, "clear all"
* closes all open graph windows and drops every graph from memory --
* which explains both "the plots are no longer shown in Stata" (clear
* all was closing them the moment the master script finished) and why
* this save loop was silently doing nothing (graph dir found zero
* graphs, since clear all had already wiped them out before this ran).
* Moving this block BEFORE clear all/log close fixes both problems.
quietly graph dir
local gnames "`r(list)'"
foreach g of local gnames {
    graph save `g' "$graphs_dir/`g'.gph", replace
}
di as result "Reload any graph with: graph use ""$graphs_dir/<name>.gph"""

*------------------------------------------------------------------------
* CLOSING BLOCK
*------------------------------------------------------------------------
* NOTE: "clear all" was changed to plain "clear" here per the author's
* choice (Option C among three alternatives discussed): "clear" without
* "all" only clears the dataset from memory -- it does NOT close graph
* windows or drop named graphs, unlike "clear all", which does both as
* documented Stata behavior. This means every fig13_* graph remains
* browsable (via the Graphs tab or `graph display <name>`) after the
* script finishes, at the cost of no longer resetting matrices/scalars/
* stored estimates/Mata objects the way "clear all" would. This is a
* deliberate divergence from Stata_code10.do's closing convention
* (which uses "clear all"), made specifically so graphs persist.
clear
log close

* END OF CHAPTER 13 CODE

*========================================================================
* SECTION MAP
*========================================================================
* 13.2   Presenting Descriptive Statistics ....... DescriptiveTables13.do
* 13.2.2 Estimation Results / Marginal Effects .... EstimationTables13.do
* 13.3   Choropleth Maps .......................... Maps13.do
* 13.4   Trend Graphs and Simple Comparisons ...... TrendGraphs13.do
* 13.5   Multivariate Panel Regression Results .... RegressionPlots13.do
*          (associational -- not causal inference; coefplot/margins on
*          pooled OLS, CCEMG, DCCE-MG/ARDL, and CGB-subgroup models)
*          13.5.1 Regression coefficient plots
*          13.5.2 Marginal effects (continuous variables)
*          13.5.3 Marginal effects (categorical variables)
* 13.6   Presenting Causal Inference Results ...... CausalPlots13.do
*          13.6.1 Event-study / DiD plots
*          13.6.2 RD plots
*          13.6.3 Synthetic control / SDiD plots
* 13.7   IV, CATE, and MTE Results ................ MTE_CATE_Plots13.do
*          13.7.1 IV/2SLS first-stage & reduced-form
*          13.7.2 CATE by subgroup (margins-based)
*          13.7.2a Official cate/categraph workflow
*          13.7.3 MTE and MPRTE curves
*          13.7.4 PRTE summary table
* 13.8   Bayesian Microsimulation and CBA Results . BayesianPlots13.do
*          13.8.1 Posterior distributions / credible intervals
*          13.8.2 CBA component breakdown (waterfall)
*          13.8.3 Sensitivity analysis (tornado)
*          13.8.4 Single policy number
* 13.9   Audience Tailoring ...................... (prose; optional template)
*========================================================================
