*========================================================================
* CATE.do  —  Section 10.10.3: Conditional Average Treatment Effects
* Sub-script called by Stata_code10.do
* Higher Education Policy Analysis Using Quantitative Techniques (2nd ed.)
* Author: Marvin A. Titus
* Date: June 2026
* NOTE: Code development was assisted by Claude (Anthropic). The author
* provided specifications and reviewed, tested, and validated all code.
*========================================================================
*
* PURPOSE
* -------
* Estimate Conditional Average Treatment Effects (CATEs) for the return
* to master's degree completion using the same instrument (state-funded
* graduate assistantship funding, ga_funding_adj) and data as
* Sections 10.10–10.11.
*
* A CATE conditions on observed covariates X:
*
*   CATE(x) = E[Y(1) - Y(0) | X = x]
*
* Unlike the LATE — which averages over all compliers near the instrument
* margin — a CATE asks: *for whom* does the treatment effect differ by
* observed characteristics (field of study, family income, first-gen status)?
*
* Three complementary strategies are implemented:
*   (a) Subgroup IV/2SLS  — run the baseline IV model separately for each
*       subgroup cell and collect point estimates + SEs.
*   (b) Interaction IV    — include subgroup × masters interaction terms
*       in the full-sample model and test for differential effects.
*   (c) Forest plot and comparison table.
*
* VARIABLE NAMES (match MTE_MPRTE.do exactly)
* -------
*   Outcome:    ln_salary
*   Treatment:  masters          (Completed Master's Degree, 1=Yes)
*   Instrument: ga_funding_adj   (state GA funding, $1,000s)
*   Controls:   $X_controls = female black hispanic asian age_ba firstgen
*                 parent_income_q parent_grad ugpa stem_major bus_major
*                 ed_major selective_inst public_ug state_unemp metro
*
* OUTPUTS
* -------
*   fig10_cate_forest.png   — forest plot of CATE by subgroup
*   fig10_cate_interact.png — interaction IV coefficient plot
*   tab10_cate_subgroup.rtf — comparison table (OLS / LATE / CATE)
*
* INHERITS (from Stata_code10.do)
*   $graphs_dir, $tables_dir, log, set scheme s2mono
*
* DATA
*   Example_7_5_3_updated.dta  (reloaded here for a clean workspace)
*========================================================================

di _n as text "=========================================================="
di      as text " Section 10.10.3: Conditional Average Treatment Effects"
di      as text "=========================================================="

*------------------------------------------------------------------------
* 0. Reload the Part B dataset for a clean workspace
*    (MTE_MPRTE.do may have left tempvars or reshaped data in memory)
*------------------------------------------------------------------------

capture use "Example_7_5_3_updated.dta", clear
if _rc != 0 {
    capture use "Example_7_5_3.dta", clear
    if _rc != 0 {
        di as error "CATE.do: Cannot find Part B dataset. Skipping CATE section."
        exit 0
    }
}

*------------------------------------------------------------------------
* 1. Globals — mirror MTE_MPRTE.do exactly
*------------------------------------------------------------------------

global X_controls "female black hispanic asian age_ba firstgen parent_income_q parent_grad ugpa stem_major bus_major ed_major selective_inst public_ug state_unemp metro"

global Z "ga_funding_adj"

*------------------------------------------------------------------------
* Output directory fallback
* When run standalone (not via Stata_code10.do), $graphs_dir and
* $tables_dir may be undefined. Define them here if missing, using the
* same logic as Stata_code10.do (personal paths for marvi, relative
* paths otherwise).
*------------------------------------------------------------------------

if "$graphs_dir" == "" {
    if c(username) == "marvi" {
        global graphs_dir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/graphs"
        global tables_dir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/tables"
        capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output"
        capture mkdir "$graphs_dir"
        capture mkdir "$tables_dir"
    }
    else {
        global graphs_dir "Output/graphs"
        global tables_dir "Output/tables"
        capture mkdir "Output"
        capture mkdir "Output/graphs"
        capture mkdir "Output/tables"
    }
    di as text "CATE.do: output directories set to:"
    di as text "  graphs: $graphs_dir"
    di as text "  tables: $tables_dir"
}

* Low-income subgroup: parent_income_q == 1 (lowest income quintile)
* This is the same policy-scenario margin used in the MPRTE analysis.

*========================================================================
* STRATEGY (a): Subgroup IV/2SLS — separate models for each cell
*
* For each subgroup, estimate:
*   ln_salary = b0 + b1*masters + controls + e
* where masters is instrumented by ga_funding_adj.
* Syntax mirrors Section 10.10.2: ivregress 2sls ln_salary $X (masters = $Z)
*
* Subgroups:
*   1. Full sample      (LATE benchmark from Section 10.10.2)
*   2. STEM majors
*   3. Non-STEM majors
*   4. Business majors
*   5. First-generation students
*   6. Non-first-generation
*   7. Low parent income (parent_income_q == 1)
*   8. Higher parent income (parent_income_q > 1)
*========================================================================

di _n as text "--- Strategy (a): Subgroup IV/2SLS ---"

local n_groups = 8
matrix CATE_results = J(`n_groups', 5, .)
matrix rownames CATE_results = "Full_sample" "STEM" "Non_STEM" ///
    "Business" "First_gen" "Non_first_gen" "Low_income" "Higher_income"
matrix colnames CATE_results = "b" "se" "lo95" "hi95" "N"

local lbl1 "Full sample (LATE)"
local lbl2 "STEM majors"
local lbl3 "Non-STEM majors"
local lbl4 "Business majors"
local lbl5 "First-generation"
local lbl6 "Non-first-generation"
local lbl7 "Low parental income"
local lbl8 "Higher parental income"

local cond1 "1 == 1"
local cond2 "stem_major == 1"
local cond3 "stem_major == 0"
local cond4 "bus_major == 1"
local cond5 "firstgen == 1"
local cond6 "firstgen == 0"
local cond7 "parent_income_q == 1"
local cond8 "parent_income_q > 1"

forvalues row = 1/`n_groups' {

    di as text _n "  Subgroup `row': `lbl`row''"

    capture {
        quietly ivregress 2sls ln_salary $X_controls ///
            (masters = ga_funding_adj)               ///
            if `cond`row'', vce(robust)

        local b_iv  = _b[masters]
        local se_iv = _se[masters]
        local n_iv  = e(N)
        matrix CATE_results[`row', 1] = `b_iv'
        matrix CATE_results[`row', 2] = `se_iv'
        matrix CATE_results[`row', 3] = `b_iv' - 1.96 * `se_iv'
        matrix CATE_results[`row', 4] = `b_iv' + 1.96 * `se_iv'
        matrix CATE_results[`row', 5] = `n_iv'
        di as text "    b = " %6.4f `b_iv' ///
                   "  SE = " %6.4f `se_iv' ///
                   "  N = " `n_iv'
    }
    if _rc != 0 {
        di as text "    (Subgroup `row' skipped — insufficient obs or first-stage failure)"
    }
}

di _n as text "Subgroup CATE matrix:"
matrix list CATE_results

*========================================================================
* STRATEGY (b): Interaction IV — test whether CATEs differ
*
* Full-sample IV model with three interaction terms added:
*   masters × stem_major
*   masters × (parent_income_q == 1)
*   masters × firstgen
*
* Each interaction is also instrumented by the corresponding
* ga_funding_adj interaction (Wooldridge 2010 heterogeneous-effects IV).
*========================================================================

di _n as text "--- Strategy (b): Interaction IV ---"

* Low-income indicator for interactions
capture drop lowinc
gen byte lowinc = (parent_income_q == 1)
label var lowinc "Low parental income (parent_income_q = 1)"

* Generate treatment × subgroup interaction terms
foreach var in stem_major lowinc firstgen {
    capture drop D_x_`var'
    gen double D_x_`var' = masters * `var'
    label var D_x_`var' "masters × `var'"

    capture drop Z_x_`var'
    gen double Z_x_`var' = ga_funding_adj * `var'
    label var Z_x_`var' "ga_funding_adj × `var'"
}

* Interaction IV: endogenous = masters + three interactions;
* instruments = ga_funding_adj + three Z×subgroup interactions.
ivregress 2sls ln_salary $X_controls                            ///
    (masters D_x_stem_major D_x_lowinc D_x_firstgen             ///
     = ga_funding_adj Z_x_stem_major Z_x_lowinc Z_x_firstgen),  ///
    vce(robust)

estimates store iv_interact

di _n as text "Interaction IV — key coefficients:"
di   as text "  Base CATE (non-STEM, higher-income, non-firstgen):"
di   as text "    b = " %6.4f _b[masters]
di   as text "  STEM increment:       " %6.4f _b[D_x_stem_major]
di   as text "  Low-income increment: " %6.4f _b[D_x_lowinc]
di   as text "  First-gen increment:  " %6.4f _b[D_x_firstgen]

* Joint Wald test: are the interaction coefficients jointly zero?
test D_x_stem_major D_x_lowinc D_x_firstgen
di _n as text "Joint Wald test (all interactions = 0): " ///
        "F(" r(df) "," r(df_r) ") = " %6.3f r(F) ///
        "  p = " %6.4f r(p)

* Implied CATEs for the four STEM × income cells
local cate_base   = _b[masters]
local cate_stem   = _b[masters] + _b[D_x_stem_major]
local cate_lowinc = _b[masters] + _b[D_x_lowinc]
local cate_stem_lowinc = _b[masters] + _b[D_x_stem_major] + _b[D_x_lowinc]

di _n as text "Implied CATEs (log-points):"
di   as text "  Non-STEM, Higher-income: " %6.4f `cate_base'
di   as text "  STEM, Higher-income:     " %6.4f `cate_stem'
di   as text "  Non-STEM, Low-income:    " %6.4f `cate_lowinc'
di   as text "  STEM, Low-income:        " %6.4f `cate_stem_lowinc'

*========================================================================
* STRATEGY (c): Visualization
*========================================================================

*------------------------------------------------------------------------
* Fig 10.CATE(a): Forest plot of subgroup CATEs from Strategy (a)
*------------------------------------------------------------------------

di _n as text "--- Producing CATE forest plot ---"

preserve

    clear
    quietly set obs `n_groups'
    gen int    j    = _n
    gen double b    = .
    gen double lo   = .
    gen double hi   = .
    gen byte   is_late = 0

    forvalues r = 1/`n_groups' {
        quietly replace b      = CATE_results[`r', 1] in `r'
        quietly replace lo     = CATE_results[`r', 3] in `r'
        quietly replace hi     = CATE_results[`r', 4] in `r'
        quietly replace is_late = (`r' == 1)           in `r'
    }

    * Reverse j so full sample (row 1) plots at the top
    gen int j2 = `n_groups' + 1 - j

    label define jlbl                    ///
        1 "`lbl8'"  2 "`lbl7'"           ///
        3 "`lbl6'"  4 "`lbl5'"           ///
        5 "`lbl4'"  6 "`lbl3'"           ///
        7 "`lbl2'"  8 "`lbl1'", replace
    label values j2 jlbl

    twoway                                                          ///
        (rcap lo hi j2 if is_late == 0,                             ///
            horizontal lcolor(gs6) lwidth(medthin))                ///
        (scatter j2 b if is_late == 0,                              ///
            mcolor(gs4) msymbol(D) msize(medium))                  ///
        (rcap lo hi j2 if is_late == 1,                             ///
            horizontal lcolor(gs0) lwidth(medium))                 ///
        (scatter j2 b if is_late == 1,                              ///
            mcolor(gs0) msymbol(D) msize(large)),                  ///
        xline(0, lpattern(dash) lcolor(gs8))                       ///
        ylabel(1(1)`n_groups', valuelabel angle(0) labsize(small)) ///
        xtitle("IV Estimate (log-points)", size(small))             ///
        ytitle("")                                                  ///
        title("Conditional Average Treatment Effects"               ///
              "Return to Master's Degree by Subgroup")              ///
        subtitle("IV/2SLS; instrument: state GA funding; 95% CI")  ///
        note("Diamond = point estimate. Bars = 95% CI."             ///
             "Full-sample LATE (black) shown as benchmark."         ///
             "Estimates based on synthetic data (illustrative).")   ///
        legend(off) scheme(s2mono)                                  ///
        name(fig10_cate_forest, replace)

    graph export "$graphs_dir/fig10_cate_forest.png",              ///
        replace width(1400)

restore

di as text "  fig10_cate_forest exported."

*------------------------------------------------------------------------
* Fig 10.CATE(b): Interaction IV — STEM × income 2×2 coefficient plot
*------------------------------------------------------------------------

di _n as text "--- Producing CATE interaction coefficient plot ---"

preserve

    clear
    quietly set obs 4
    gen str40  lbl     = ""
    gen double b_cell  = .
    gen double se_cell = .
    gen byte   is_stem = .

    * Use conservative delta-method SE: sqrt(sum of squared SEs)
    replace lbl     = "Non-STEM, Higher-income"                         in 1
    replace b_cell  = `cate_base'                                       in 1
    replace se_cell = _se[masters]                                      in 1
    replace is_stem = 0                                                  in 1

    replace lbl     = "STEM, Higher-income"                             in 2
    replace b_cell  = `cate_stem'                                       in 2
    replace se_cell = sqrt(_se[masters]^2 + _se[D_x_stem_major]^2)     in 2
    replace is_stem = 1                                                  in 2

    replace lbl     = "Non-STEM, Low-income"                            in 3
    replace b_cell  = `cate_lowinc'                                     in 3
    replace se_cell = sqrt(_se[masters]^2 + _se[D_x_lowinc]^2)         in 3
    replace is_stem = 0                                                  in 3

    replace lbl     = "STEM, Low-income"                                in 4
    replace b_cell  = `cate_stem_lowinc'                                in 4
    replace se_cell = sqrt(_se[masters]^2 + _se[D_x_stem_major]^2      ///
                           + _se[D_x_lowinc]^2)                         in 4
    replace is_stem = 1                                                  in 4

    gen double lo95 = b_cell - 1.96 * se_cell
    gen double hi95 = b_cell + 1.96 * se_cell
    gen int j = _n

    twoway                                                              ///
        (rcap lo95 hi95 j,                                              ///
            horizontal lcolor(gs6) lwidth(medthin))                    ///
        (scatter j b_cell if is_stem == 0,                              ///
            mcolor(gs0) msymbol(O) msize(large))                       ///
        (scatter j b_cell if is_stem == 1,                              ///
            mcolor(gs0) msymbol(D) msize(large)),                      ///
        xline(0, lpattern(dash) lcolor(gs8))                           ///
        ylabel(1 "STEM, Low-income"                                     ///
               2 "Non-STEM, Low-income"                                 ///
               3 "STEM, Higher-income"                                  ///
               4 "Non-STEM, Higher-income",                             ///
               angle(0) labsize(small))                                 ///
        xtitle("IV Estimate (log-points)", size(small))                 ///
        ytitle("")                                                      ///
        title("CATE: STEM × Income Interaction")                        ///
        subtitle("Interaction IV/2SLS; instrument: state GA funding")   ///
        note("Circle = Non-STEM. Diamond = STEM."                       ///
             "Error bars = approx. 95% CI (conservative delta method)." ///
             "Estimates based on synthetic data (illustrative).")       ///
        legend(order(2 "Non-STEM" 3 "STEM") rows(1))                    ///
        scheme(s2mono)                                                  ///
        name(fig10_cate_interact, replace)

    graph export "$graphs_dir/fig10_cate_interact.png",                ///
        replace width(1400)

restore

di as text "  fig10_cate_interact exported."

*========================================================================
* COMPARISON TABLE: OLS / LATE / Subgroup CATEs
*========================================================================

di _n as text "--- Producing CATE comparison table ---"

* OLS benchmark (mirrors Section 10.10.2)
quietly reg ln_salary masters $X_controls, robust
estimates store cate_ols

* Full-sample LATE (mirrors Section 10.10.2)
quietly ivregress 2sls ln_salary $X_controls ///
    (masters = ga_funding_adj), vce(robust)
estimates store cate_late

* CATE: STEM majors
quietly ivregress 2sls ln_salary $X_controls ///
    (masters = ga_funding_adj)               ///
    if stem_major == 1, vce(robust)
estimates store cate_stem

* CATE: First-generation
quietly ivregress 2sls ln_salary $X_controls ///
    (masters = ga_funding_adj)               ///
    if firstgen == 1, vce(robust)
estimates store cate_firstgen

* CATE: Low parental income
quietly ivregress 2sls ln_salary $X_controls ///
    (masters = ga_funding_adj)               ///
    if parent_income_q == 1, vce(robust)
estimates store cate_lowinc

estout cate_ols cate_late cate_stem cate_firstgen cate_lowinc           ///
    using "$tables_dir/tab10_cate_subgroup.rtf",                        ///
    replace style(fixed)                                                ///
    keep(masters)                                                       ///
    cells(b(star fmt(4)) se(par fmt(4)))                                ///
    starlevels(* 0.10 ** 0.05 *** 0.01)                                 ///
    stats(N r2, labels("N" "R-squared") fmt(0 3))                       ///
    mlabels("OLS" "IV/LATE" "CATE: STEM" "CATE: First-gen"             ///
            "CATE: Low-income")                                         ///
    title("Table 10.CATE. OLS, LATE, and Conditional Average Treatment Effects") ///
    note("Outcome: log annual salary."                                  ///
         "Instrument: state-funded GA amount (ga_funding_adj)."         ///
         "Robust SEs in parentheses."                                   ///
         "CATE columns restrict the sample to the indicated subgroup."  ///
         "Estimates based on synthetic data (illustrative only).")

di as text "  tab10_cate_subgroup.rtf exported."

*========================================================================
* Clean up temporary interaction variables
*========================================================================

capture drop D_x_stem_major D_x_lowinc D_x_firstgen
capture drop Z_x_stem_major Z_x_lowinc Z_x_firstgen
capture drop lowinc

*========================================================================
* Display figures
*========================================================================

capture graph display fig10_cate_forest
capture graph display fig10_cate_interact

di _n as text "=========================================================="
di      as text " Section 10.10.3 (CATE.do) complete."
di      as text " Outputs:"
di      as text "   $graphs_dir/fig10_cate_forest.png"
di      as text "   $graphs_dir/fig10_cate_interact.png"
di      as text "   $tables_dir/tab10_cate_subgroup.rtf"
di      as text "=========================================================="

*========================================================================
* END OF CATE.do
*========================================================================
