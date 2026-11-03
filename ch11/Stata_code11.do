*========================================================================
* Chapter 11 - Instrumental Variables and Marginal Treatment Effects:
*              Returns to a Master's Degree
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative Techniques
* (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch11
* Author: Marvin A. Titus
* Date: June 2026
* NOTE: Code development was assisted by Claude (Anthropic). The author
* provided specifications and reviewed, tested, and validated all code.
*========================================================================
* Script tested in Stata 19.5
* Compatible with Stata version 19 or later
*
* Sub-scripts (all in $syntax_dir = Syntax/Stata/):
*   CATE.do         Section 11.1.3     — Conditional Average Treatment Effects
*   MTE_MPRTE.do    Section 11.2       — MTE, PRTE/MPRTE, CBA
*
*   Section 11.1: Instrumental Variables and the LATE  →  CATE.do
*     OLS → IV/2SLS (LATE) — returns to master's degree.
*     Instrument: state-funded graduate assistantship (GA) amount.
*     Data: synthetic B&B panel (Example_7_5_3_updated.dta)
*
*   Section 11.1.3: Conditional Average Treatment Effects  →  CATE.do
*     Heterogeneous IV returns to master's degree by observed subgroups
*     (field, income quintile, first-generation status).
*     Data: Example_7_5_3_updated.dta (CATE.do now runs first, so this
*     is the script's first data load for the chapter run)
*
*   Section 11.2.1: Polynomial Control Function Approach  →  MTE_MPRTE.do
*     Pooled MTE (mtefe, quadratic) and area-specific MTE (mtefe,
*     linear); state-clustered bootstrap SEs.
*
*   Section 11.2.2: MTE Results  →  MTE_MPRTE.do
*     ATE/ATT/ATU/LATE/MPRTE (Tables 11.2-11.3); PRTE for eight GA
*     funding scenarios (Tables 11.4-11.5); benefit-cost ratios
*     (Table 11.6); MTE, policy-weight, and PRTE-intensity figures.
*
* NOTE: This chapter continues from Chapter 10, which demonstrated
* causal inference methods for state- and institution-level policies
* (RDD, DiD, SCM, SDID, ETWFE). This script covers only the IV/CATE/MTE/
* PRTE/CBA content described above (Sections 11.1-11.2); see Chapter
* 10's master script (Stata_code10.do) for the earlier material.
*========================================================================

*========================================================================
* IMPORTANT: Set working directory (customize this for your system)
*========================================================================

* Use a global path to make it easy to update in one place
* global ch11data "C:/Users/YourName/Documents/book-materials/ch11/data"
* cd "$ch11data"

*========================================================================
* OUTPUT DIRECTORIES AND LOG FILE
* Paths switch automatically based on the OS username (c(username)).
* The instructor's personal paths are used when username == "marvi";
* all other users get the generic relative paths.
*========================================================================

* Close any stale log silently, then open a fresh one
capture log close

if c(username) == "marvi" {
    global graphs_dir  "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/graphs"
    global tables_dir  "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/tables"
    global logdir      "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/logs"
    global syntax_dir  "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Syntax/Stata"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/graphs"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/tables"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/logs"
    log using ///
        "C:\\Users\\marvi\\Dropbox\\Book\\2nd Edition\\Chapter 11\\Output\\logs\\Chapter11_Stata_output.log", ///
        replace text
}
else {
    global graphs_dir  "Output/graphs"
    global tables_dir  "Output/tables"
    global logdir      "Output/logs"
    global syntax_dir  "Syntax/Stata"
    capture mkdir "Output"
    capture mkdir "Output/graphs"
    capture mkdir "Output/tables"
    capture mkdir "Output/logs"
    log using "Output/logs/Chapter11_Stata_output.log", replace text
}

di "Chapter 11 log opened: " c(current_date) " " c(current_time)
di "Graphs directory: $graphs_dir"

clear all
set more off
version 19
set scheme s2mono        // Monochrome scheme for Springer B&W print
set graphics on          // Ensure graph window is active throughout

*========================================================================
* PACKAGE INSTALLATIONS (run once; comment out thereafter)
*========================================================================

* MTE / IV packages
capture ssc install mtefe,   replace
capture ssc install moremata, replace
capture ssc install boottest, replace   // wild cluster bootstrap (boottest), used by MTE_MPRTE.do
capture ssc install estout, replace   // esttab — used by CATE.do for RTF tables
di as text "MTE/IV packages confirmed."

*========================================================================
*========================================================================
*
*    INSTRUMENTAL VARIABLES AND MARGINAL TREATMENT EFFECTS
*    (Sections 11.1 – 11.2)
*
*========================================================================
*========================================================================

*========================================================================
* SECTION 11.1.3: CONDITIONAL AVERAGE TREATMENT EFFECTS (CATE)
*   Heterogeneous IV returns to master's degree by observed subgroups.
*   Strategy:
*     (a) Subgroup IV/2SLS — run the baseline IV model separately within
*         cells defined by field, income quintile, and first-generation
*         status; collect point estimates and SEs for a forest plot.
*     (b) Interaction IV — include field × treatment and
*         income_q1 × treatment interactions in the full-sample IV model
*         to test whether subgroup CATEs differ significantly.
*     (c) Forest-plot visualization (Fig. 11.1) and interaction-margins
*         plot (Fig. 11.2), both from the interaction IV model in (b);
*         these are also the source of the CATE values reported in the
*         chapter's Table 11.1.
*     (d) Supplementary comparison table: OLS vs. full-sample IV/LATE
*         vs. separately-estimated subgroup IV models from (a). It is
*         NOT reproduced in the chapter (Tables 11.2-11.6 in the book
*         are the MTE tables); the file keeps its original name,
*         tab11_2_cate_subgroup.rtf, so CATE.do need not change.
*
*   Script: $syntax_dir/CATE.do
*   Inherits: $graphs_dir, $tables_dir, log, set scheme s2mono
*   Data:     Example_7_5_3_updated.dta (CATE.do now runs first, so this
*             is the script's first data load; falls back to
*             Example_7_5_3.dta if the updated file is unavailable, per
*             CATE.do's own logic). Globals defined below remain in
*             scope for Section 11.2.
*   Produces: fig11_1 (CATE forest plot, by subgroup)
*             fig11_2 (CATE interaction plot, STEM × income)
*             tab11_2_cate_subgroup.rtf (supplementary; not a
*               numbered table in the chapter — see (d) above)
*
*   Placement in chapter: Section 11.1.3, immediately after the
*   OLS/IV/LATE comparison in Section 11.1.2 and before MTE (11.2).
*   CATE bridges LATE (a single complier average) and MTE (continuous
*   heterogeneity along unobserved resistance) by characterising
*   heterogeneity along observed dimensions (field, income, generation).
*========================================================================

do "$syntax_dir/CATE.do"

*------------------------------------------------------------------------
* Display CATE figures in the Stata graph window
*------------------------------------------------------------------------
capture graph display fig11_1    // forest plot: CATE by subgroup
capture graph display fig11_2    // interaction-term CATE margins

*========================================================================
* SECTION 11.2: MARGINAL TREATMENT EFFECTS (external script)
*   OLS → IV/2SLS → MTE → PRTE/MPRTE → CBA — returns to master's degree
*   Script: $syntax_dir/MTE_MPRTE.do (opens its own log,
*           $logdir/MTE_MPRTE_output.log, which is the source of
*           Tables 11.2-11.6)
*   Inherits: $graphs_dir, $logdir, set scheme s2mono
*   Data:     Example_7_5_3_updated.dta
*
*   Key sections (of MTE_MPRTE.do):
*     Sec 1-5    Data, summary statistics, first stage, OLS, IV/2SLS
*     Sec 6a     Probit propensity score P(Z,X)
*     Sec 6b     Manual polynomial MTE (teaching illustration only)
*     Sec 6c     Pooled MTE via mtefe, pol(2): ATE, ATT, ATU, LATE,
*                  MPRTE1-3; Heckman comparison           -> Table 11.2
*     Sec 6d     Area-specific MTE via mtefe, pol(1), one model per
*                  program area; non-completers assigned a prospective
*                  area (seed 20260102)                     -> Table 11.3
*     Sec 6e     State-clustered bootstrap (G=50, R=500, seed 20260101)
*                  for pooled and area-specific SEs; wild cluster
*                  bootstrap (boottest) for the IV estimate
*     Sec 9-10b  PRTE for Scenarios 1-8 via mtefe prte() -> Tables 11.4-11.5
*     Sec 11     PRTE by policy intensity
*     Sec 13     Policy weights and propensity-score support
*     Sec 14     Cost-benefit analysis (B/C ratios)     -> Table 11.6
*
*   RUN TIME: by default MTE_MPRTE.do reuses the published bootstrap
*   draws (mte_bootstrap_draws.dta, downloaded from the book's data
*   repository if not found locally): about 5-10 minutes on Stata MP,
*   10-15 minutes on Stata SE. To rerun the bootstrap yourself, set
*   use_saved_boot = 0 in Section 6e of MTE_MPRTE.do: about 7 hours
*   (MP, 2 cores) to 9-12 hours (SE). The published draws are valid
*   ONLY for the book's exact specification; if you change the data,
*   controls, or models, you must set use_saved_boot = 0. See the
*   RUN TIME and CAUTION notes at the top of MTE_MPRTE.do.
*
*   Produces: bb_mte_analysis.dta, mte_summary_by_field.csv
*             mte_summary_by_program_area.csv, mte_bootstrap_draws.dta
*             fig11_3 (pooled MTE curve with bootstrap band)
*             fig11_4 (propensity-score histogram with the MTE curve)
*             fig11_5 (MTE curves by graduate program area)
*             fig11_6 (MTE curve with PRTE weights: uniform +$1k vs.
*                      low-income +$2k)
*             fig11_8 (PRTE by policy intensity)
*   (Fig. 11.7, MTE by propensity-score decile, is no longer produced.)
*========================================================================

do "$syntax_dir/MTE_MPRTE.do"

*------------------------------------------------------------------------
* Display MTE/MPRTE figures in the Stata graph window
* MTE_MPRTE.do already displays and saves (.gph and .png) each figure
* at its own end. The block below provides a single consolidated recall.
*------------------------------------------------------------------------
capture graph display fig11_3
capture graph display fig11_4
capture graph display fig11_5
capture graph display fig11_6
capture graph display fig11_8

*========================================================================
* Close log and exit
* (Each sub-script saves its figures as .gph and .png. Reload a figure
*  after Stata closes with:  graph use "$graphs_dir/fig11_3.gph")
*========================================================================

clear all

log close

*========================================================================
* END OF CHAPTER 11 CODE
*========================================================================

/* Chapter 11 Section Map
   ======================================================================

   11.0  Introduction

   11.1  Instrumental Variables and the LATE
     11.1.1  First Stage and Instrument Relevance
     11.1.2  OLS and IV/LATE Estimation
             (MTE_MPRTE.do Sections 1-5: data, summary statistics,
              first stage, OLS, IV/2SLS)
     11.1.3  Conditional Average Treatment Effects (CATE)  -> CATE.do
             Subgroup IV, interaction IV, and forest-plot visualization
                 -> Table 11.1, Fig. 11.1 (forest plot),
                    Fig. 11.2 (interaction plot)

   11.2  Marginal Treatment Effects (MTE)                 -> MTE_MPRTE.do
     11.2.1  Polynomial Control Function Approach
             Section 6a: probit propensity score
             Section 6b: manual polynomial MTE (teaching illustration)
             Section 6c: pooled mtefe, pol(2); Heckman comparison
             Section 6d: area-specific mtefe, pol(1)
             Section 6e: state-clustered bootstrap; wild cluster bootstrap
     11.2.2  MTE Results
             Sections 6c, 6e, 7  -> Table 11.2 (pooled parameters)
             Sections 6d, 6e     -> Table 11.3 (parameters by area)
             Sections 9-10b      -> Tables 11.4-11.5 (PRTE, Scenarios 1-8)
             Section 14          -> Table 11.6 (cost-benefit analysis)
             Section 8           -> Fig. 11.3, Fig. 11.5
             Section 11          -> Fig. 11.8
             Section 13          -> Fig. 11.4, Fig. 11.6
             Sections 15-16: save results, final summary

   11.3  Summary

   11.4  Appendix
     11.4.1  Data
     11.4.2  Code, log files, and figures

   NOTE: This chapter continues from Chapter 10 (state- and institution-
   level causal inference: RDD, DiD, SCM, SDID, ETWFE). See Chapter 10's
   own master script and section map for that earlier material.

   Figure inventory (script names; final numbering set in the chapter):
     Fig. 11.1  CATE Forest Plot by Subgroup                  (CATE.do)
     Fig. 11.2  CATE Interaction Plot (STEM x Income)         (CATE.do)
     Fig. 11.3  Estimated MTE Curve, pooled (mtefe, quadratic), with
                state-clustered bootstrap band
     Fig. 11.4  Propensity-Score Distribution with the MTE Curve
     Fig. 11.5  MTE Curves by Graduate Program Area (mtefe, linear)
     Fig. 11.6  MTE Curve with PRTE Weights (uniform +$1k vs.
                low-income +$2k)
     Fig. 11.8  PRTE by Policy Intensity
     (Fig. 11.7, MTE by propensity-score decile, is no longer produced.)

   Table inventory:
     Table 11.1  CATE of Master's Degree Completion on Log Annual Salary
                  (interaction IV model, CATE.do Strategy (b); reported
                  in the chapter text, not exported as a standalone .rtf)
     Table 11.2  MTE-Based Treatment Parameters for Master's Degree
                  Completion (MTE_MPRTE.do Sections 6c, 6e, 7)
     Table 11.3  MTE-Based Treatment Parameters by Graduate Program Area
                  (Sections 6d, 6e)
     Table 11.4  PRTE for Four GA Funding Targeting Scenarios
                  (Sections 9-10b)
     Table 11.5  PRTE for Four Graduate Program Area Scenarios
                  (Sections 9-10b)
     Table 11.6  Cost-Benefit Analysis for Eight GA Funding Scenarios
                  (Section 14)
     Tables 11.2-11.6 are assembled from MTE_MPRTE_output.log; the
     script does not export them as .rtf files.
     Supplementary (not in the chapter): tab11_2_cate_subgroup.rtf,
                  OLS vs. LATE vs. separate-model subgroup IV (CATE.do).
                  The "tab11_2" prefix is a holdover from an earlier
                  draft and does not correspond to Table 11.2.

   Data files saved:
     bb_mte_analysis.dta             (MTE analysis; includes ma_*_pro vars)
     mte_summary_by_field.csv
     mte_summary_by_program_area.csv
     mte_bootstrap_draws.dta         (bootstrap draws; reused by default)
*/
