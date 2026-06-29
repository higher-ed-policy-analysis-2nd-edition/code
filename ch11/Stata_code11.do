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
*   MTE_MPRTE.do    Sections 11.2-11.3 — MTE/MPRTE, CBA
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
*   Section 11.2: Marginal Treatment Effects (MTE)  →  MTE_MPRTE.do
*     Pooled and area-specific MTE via polynomial control function;
*     cluster bootstrap SEs; treatment effect comparison (ATE/ATT/ATU).
*
*   Section 11.2.3: MTE Visualization  →  MTE_MPRTE.do
*     MTE curve, propensity-score distribution, by-area curves,
*     policy-relevant margins.
*
*   Section 11.3: Cost-Benefit Analysis  →  MTE_MPRTE.do
*     PRTE and MPRTE policy simulations (Scenarios 1-8); benefit-cost
*     ratios translating MPRTE estimates into policy-relevant terms.
*
* NOTE: This chapter continues from Chapter 10, which demonstrated
* causal inference methods for state- and institution-level policies
* (RDD, DiD, SCM, SDID, ETWFE). This script covers only the IV/CATE/MTE/
* MPRTE/CBA content described above (Sections 11.1-11.3); see Chapter
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
capture ssc install fwildclusterboot, replace
capture ssc install estout, replace   // esttab — used by CATE.do for RTF tables
di as text "MTE/IV packages confirmed."

*========================================================================
*========================================================================
*
*    INSTRUMENTAL VARIABLES AND MARGINAL TREATMENT EFFECTS
*    (Sections 11.1 – 11.3)
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
*     (d) Comparison table (Table 11.2): OLS vs. full-sample IV/LATE vs.
*         separately-estimated subgroup IV models from (a). This is a
*         distinct table from Table 11.1 — it compares estimators rather
*         than reporting interaction-model subgroup CATEs.
*
*   Script: $syntax_dir/CATE.do
*   Inherits: $graphs_dir, $tables_dir, log, set scheme s2mono
*   Data:     Example_7_5_3_updated.dta (CATE.do now runs first, so this
*             is the script's first data load; falls back to
*             Example_7_5_3.dta if the updated file is unavailable, per
*             CATE.do's own logic). Globals defined below remain in
*             scope for Sections 11.2-11.3.
*   Produces: fig11_1 (CATE forest plot, by subgroup)
*             fig11_2 (CATE interaction plot, STEM × income)
*             tab11_2_cate_subgroup.rtf (Table 11.2)
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
* SECTIONS 11.2–11.3: MARGINAL TREATMENT EFFECTS (external script)
*   OLS → IV/2SLS → MTE/MPRTE → CBA — returns to master's degree
*   Script: $syntax_dir/MTE_MPRTE.do
*   Inherits: $graphs_dir, log, set scheme s2mono
*   Data:     Example_7_5_3_updated.dta (or Example_7_5_3.dta fallback)
*
*   Key sections:
*     Sec 6    Pooled cubic polynomial MTE (ATE, ATT, ATU); mtefe; Heckman
*     Sec 6b   Area-specific MTE by graduate program area (fully interacted)
*     Sec 6b-ATU  Prospective program area assignment for untreated obs;
*                  area-specific ATU via counterfactual assignment
*                  (seed 20260102; mirrors treated assignment in Sec 1b)
*     Sec 6c   Cluster bootstrap (G=50, R=500): SEs for ATE, ATT, ATU
*                  — pooled and area-specific, with 95% CIs
*     Sec 9-11 PRTE and MPRTE policy simulations (Scenarios 1-8)
*     Sec 14   Cost-benefit analysis (B/C ratios)
*
*   Produces: bb_mte_analysis.dta, mte_summary_by_field.csv
*             mte_summary_by_program_area.csv
*             fig11_3 (pooled MTE curve)
*             fig11_4 (MTE by propensity score)
*             fig11_5 (MTE curves by graduate program area)
*             fig11_6 (MTE curve with policy-relevant margins)
*             fig11_7 (MTE by propensity score decile)
*             fig11_8 (MPRTE by policy intensity)
*========================================================================

do "$syntax_dir/MTE_MPRTE.do"

*------------------------------------------------------------------------
* Display MTE/MPRTE figures in the Stata graph window
* MTE_MPRTE.do already calls graph display for fig11_3 – fig11_8
* at its own end. The block below provides a single consolidated recall.
*------------------------------------------------------------------------
capture graph display fig11_3
capture graph display fig11_4
capture graph display fig11_5
capture graph display fig11_6
capture graph display fig11_7
capture graph display fig11_8

*========================================================================
* Close log and exit
*========================================================================

clear all

log close

*========================================================================
* END OF CHAPTER 11 CODE
*========================================================================

/* Chapter 11 Section Map
   ======================================================================

   11.1  Instrumental Variables and the LATE
     Sections 1-4: Data loading, summary statistics, first-stage, OLS
     Section 5: IV/2SLS (LATE)

   11.1.3  Conditional Average Treatment Effects (CATE)
     Section 5b: Subgroup CATEs via IV/2SLS with interaction terms
     Section 5c: Lasso-based heterogeneous treatment effects (ivlasso / post-lasso)
     Section 5d: Forest-plot visualization of CATEs by field, income, and generation
                  -> Fig. 11.1 (forest plot), Fig. 11.2 (interaction plot)
     Section 5e: CATE comparison table (OLS vs. LATE vs. separate-model
                 subgroup IV) -> Table 11.2

   11.2  Marginal Treatment Effects
     Section 6: Manual polynomial MTE (quadratic and cubic), mtefe, Heckman
     Section 6b: Area-specific MTE by graduate program field
     Section 6c: Cluster bootstrap SEs, wild cluster bootstrap
     Section 7: Treatment effect comparison (ATE/ATT/ATU/LATE/CATE)

   11.2.3  Marginal Treatment Effects: Visualization
     Section 8: MTE curve, decile plot, by-area curves, policy margins
                 -> Fig. 11.3 (pooled MTE curve)
                 -> Fig. 11.4 (MTE by propensity score)
                 -> Fig. 11.5 (MTE curves by graduate program area)
                 -> Fig. 11.6 (MTE curve with policy-relevant margins)
                 -> Fig. 11.7 (MTE by propensity score decile)
                 -> Fig. 11.8 (MPRTE by policy intensity; produced in
                    Section 11 of MTE_MPRTE.do, listed here for the
                    complete figure inventory)

   11.3  Cost-Benefit Analysis
     Sections 9-11: PRTE and MPRTE policy simulations (Scenarios 1-8)
     Sections 12-13: Parameter comparison table, MPRTE visualization
     Section 14: Cost-benefit analysis (B/C ratios)

   11.4  Summary
     Sections 15-16: Save results, final summary

   11.5  Appendix
     11.5.1  Data
     11.5.2  Code, log files, and figures

   NOTE: This chapter continues from Chapter 10 (state- and institution-
   level causal inference: RDD, DiD, SCM, SDID, ETWFE). See Chapter 10's
   own master script and section map for that earlier material.

   NOTE ON FIG. 11.7 / FIG. 11.8: these two figures (MTE by propensity
   score decile; MPRTE by policy intensity) are produced by the script
   but are not currently discussed by name in the chapter's prose. They
   are numbered here for consistency with everything else this chapter
   produces, not because the text references them yet.

   Figure inventory (in order produced):
     Fig. 11.1  CATE Forest Plot by Subgroup
     Fig. 11.2  CATE Interaction Plot (STEM x Income)
     Fig. 11.3  Estimated MTE Curve (pooled, cubic polynomial)
     Fig. 11.4  MTE by Propensity Score (diamond markers + frequency bars)
     Fig. 11.5  MTE Curves by Graduate Program Area
     Fig. 11.6  MTE Curve with Policy-Relevant Margins
     Fig. 11.7  MTE by Propensity Score Decile
     Fig. 11.8  MPRTE by Policy Intensity

   Table inventory:
     Table 11.1  CATE of Master's Degree Completion on Log Annual Salary
                  (interaction-model subgroup CATEs, reported in the
                  chapter text; values come from the interaction IV
                  model in CATE.do Strategy (b) — not directly exported
                  as a standalone .rtf by this script)
     Table 11.2  OLS, LATE, and Conditional Average Treatment Effects
                  (separate-model subgroup IV comparison, produced by
                  CATE.do; tab11_2_cate_subgroup.rtf)

   Data files saved:
     bb_mte_analysis.dta             (MTE analysis; includes ma_*_pro vars)
     mte_summary_by_field.csv
     mte_summary_by_program_area.csv
*/


*========================================================================
* SAVE ALL NAMED GRAPHS AS .GPH
* Sub-scripts save .gph files alongside each export; this block is a
* final safety net. Reload after Stata closes:
*   graph use "$graphs_dir/graphname.gph"
*========================================================================
di _n as text "Saving all named graphs as .gph files..."

foreach gname in                                                        ///
    fig11_1  fig11_2  fig11_3  fig11_4  fig11_5  fig11_6               ///
    fig11_7  fig11_8 {
    capture graph save "$graphs_dir/`gname'.gph", replace
}

di as text _n "========================================================"
di as text "All graphs saved as .gph to:"
di as text "  $graphs_dir"
di as text _n "Reopen any graph after closing Stata with:"
di as text `"  graph use "$graphs_dir/graphname.gph""'
di as text _n "All graph names:"
di as text "  CATE:        fig11_1 (forest plot) fig11_2 (interaction plot)"
di as text "  MTE/MPRTE:  fig11_3 (MTE curve) fig11_4 (by propensity score)"
di as text "               fig11_5 (by program area) fig11_6 (policy margins)"
di as text "               fig11_7 (by decile) fig11_8 (by policy intensity)"
di as text "  (ATU):      area-specific ATU via prospective assignment (Sec 6b-ATU)"
di as text "  Table:       tab11_2_cate_subgroup.rtf"
di as text "========================================================"
