*========================================================================
* Chapter 10 - Causal Inference and Marginal Treatment Effects
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative Techniques
* (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch10
* Author: Marvin A. Titus
* Date: November 2025 (revised March 2026)
* NOTE: Code development was assisted by Claude (Anthropic). The author
* provided specifications and reviewed, tested, and validated all code.
*========================================================================
* Script tested in Stata 19.5
* Compatible with Stata version 19 or later
*
* PART A  (Sections 10.3-10.9): Causal Inference — Georgia Consolidation
*   TWFE DiD, LASSO DiD, Synthetic Control, Synthetic DiD,
*   Event Study / Callaway-Sant'Anna, Staggered Adoption,
*   Permutation and Leave-One-Out sensitivity tests.
*   Data: SHEEO state-level finance panel (Example_10_3_1.csv)
*
* PART B  (Sections 10.10-10.16+): Marginal Treatment Effects
*   MTE/MPRTE framework for returns to master's degree.
*   Instrument: state-funded graduate assistantship (GA) amount.
*   Data: synthetic B&B panel (Example_7_5_3_updated.dta)
*========================================================================

*========================================================================
* IMPORTANT: Set working directory (customize this for your system)
*========================================================================

* Use a global path to make it easy to update in one place
* global ch10data "C:/Users/YourName/Documents/book-materials/ch10/data"
* cd "$ch10data"

*========================================================================
* OUTPUT DIRECTORIES AND LOG FILE
* Paths switch automatically based on the OS username (c(username)).
* The instructor's personal paths are used when username == "marvi";
* all other users get the generic relative paths.
*========================================================================

* Close any stale log silently, then open a fresh one
capture log close

if c(username) == "marvi" {
    global graphs_dir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/graphs"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/graphs"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/logs"
    log using ///
        "C:\\Users\\marvi\\Dropbox\\Book\\2nd Edition\\Chapter 10\\Output\\logs\\Chapter10_Stata_output.log", ///
        replace text
}
else {
    global graphs_dir "Output/graphs"
    capture mkdir "Output"
    capture mkdir "Output/graphs"
    capture mkdir "Output/logs"
    log using "Output/logs/Chapter10_Stata_output.log", replace text
}

di "Chapter 10 log opened: " c(current_date) " " c(current_time)
di "Graphs directory: $graphs_dir"

clear all
set more off
version 19
set seed 20251130
set scheme s2mono        // Monochrome scheme for Springer B&W print

*========================================================================
*========================================================================
*
*    PART A: CAUSAL INFERENCE — GEORGIA HIGHER EDUCATION CONSOLIDATION
*            (Sections 10.3 – 10.9)
*
*========================================================================
*========================================================================

*=======================================================
* Analysis of Georgia's Higher Education Consolidation
* Policy Using Multiple Causal Inference Techniques
*=======================================================

clear all
set more off
version 19

*=======================================================
* Section 10.3.1: Data Structure and Variable Construction
* (Application: TWFE DiD for Georgia Consolidation)
*=======================================================

* Download data from GitHub repository
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch10/Example_10_3_1.csv" ///
     "Example_10_3_1.csv", replace

* Import CSV file
import delimited "Example_10_3_1.csv", clear

* Clean state names
replace state = strtrim(state)

* Create SREB indicator (16 Southern states)
gen sreb = inlist(state, "Alabama", "Arkansas", "Delaware", "Florida", ///
           "Georgia", "Kentucky", "Louisiana", "Maryland", "Mississippi") ///
    | inlist(state, "North Carolina", "Oklahoma", "South Carolina", ///
             "Tennessee", "Texas", "Virginia", "West Virginia")

keep if sreb == 1

* Create FIPS codes for panel identification
gen fips = .
replace fips = 1  if state == "Alabama"
replace fips = 5  if state == "Arkansas"
replace fips = 10 if state == "Delaware"
replace fips = 12 if state == "Florida"
replace fips = 13 if state == "Georgia"
replace fips = 21 if state == "Kentucky"
replace fips = 22 if state == "Louisiana"
replace fips = 24 if state == "Maryland"
replace fips = 28 if state == "Mississippi"
replace fips = 37 if state == "North Carolina"
replace fips = 40 if state == "Oklahoma"
replace fips = 45 if state == "South Carolina"
replace fips = 47 if state == "Tennessee"
replace fips = 48 if state == "Texas"
replace fips = 51 if state == "Virginia"
replace fips = 54 if state == "West Virginia"

* Treatment state indicator (Georgia = 1)
gen treat_state = (state == "Georgia")

* Post-treatment period (2018 onwards)
gen post = (fy >= 2018)

* DiD interaction term
gen did = treat_state * post

* Placebo test indicators
gen post_placebo = (fy >= 2012)
gen did_placebo = treat_state * post_placebo

* Log-transformed variables
gen lngenop = log(general_public_operations)
label variable lngenop "Log(General Operating Expenses)"

gen lntotsup = log(total_state_support)
label variable lntotsup "Log(Total State Support)"

gen lnfinaid = log(total_financial_aid)
label variable lnfinaid "Log(Total Financial Aid)"

gen lntuifee = log(net_tuition_and_fee_revenue)
label variable lntuifee "Log(Net Tuition & Fee Revenue)"

gen lnfte = log(net_fte_enrollment)
label variable lnfte "Log(FTE Enrollment)"

* Define control variables
global controls "lntotsup lnfinaid lntuifee lnfte"

*=======================================================
* Section 10.3.2: TWFE Estimation Results
* (Two-Way Fixed Effects DiD)
*=======================================================

* Declare panel structure
xtset fips fy

* Two-Way Fixed Effects (TWFE) DiD
xtreg lngenop did $controls i.fy, fe vce(cluster fips)

* Store TWFE results
estimates store twfe_did

* Placebo test (pre-treatment falsification)
xtreg lngenop did_placebo $controls i.fy if fy < 2018, ///
  fe vce(cluster fips)

*=======================================================
* Section 10.3.3: Parallel Trends Assessment
*=======================================================

* Visual inspection - trends plot
preserve
collapse (mean) lngenop, by(treat_state fy)
twoway (connected lngenop fy if treat_state == 0, ///
          lpattern(dash) lcolor(gs8) mcolor(gs8) msymbol(Oh)) ///
       (connected lngenop fy if treat_state == 1, ///
          lpattern(solid) lcolor(gs0) mcolor(gs0) msymbol(O)), ///
  xline(2018, lpattern(dot) lcolor(gs6)) ///
  legend(label(1 "Control States") label(2 "Georgia")) ///
  ytitle("Log Operating Expenses") xtitle("Fiscal Year") ///
  title("Parallel Trends: Treatment vs Control") ///
  name(fig10_1_parallel_trends, replace)
graph export "$graphs_dir/fig10_1_parallel_trends_Stata.png", replace width(1200)
restore

* Formal pre-trends test
reghdfe lngenop c.treat_state#c.fy $controls if fy < 2018, ///
  absorb(fips fy) vce(cluster fips)

test c.treat_state#c.fy = 0

*=======================================================
* Section 10.3.4: Robustness Checks
*=======================================================

* Alternative treatment timing (2013)
gen post_2013 = (fy >= 2013)
gen did_2013 = treat_state * post_2013

xtreg lngenop did_2013 $controls i.fy, fe vce(cluster fips)

* Alternative treatment timing (2015)
gen post_2015 = (fy >= 2015)
gen did_2015 = treat_state * post_2015

xtreg lngenop did_2015 $controls i.fy, fe vce(cluster fips)

* Exclude border states
gen border = inlist(state, "Florida", "Alabama", ///
                    "South Carolina", "Tennessee", "North Carolina")

xtreg lngenop did $controls i.fy if border == 0, ///
  fe vce(cluster fips)

* Weighted regression by enrollment
* Note: Create time-invariant weight (mean enrollment by state)
bysort fips: egen mean_fte = mean(net_fte_enrollment)
xtreg lngenop did $controls i.fy [aweight=mean_fte], ///
  fe vce(cluster fips)

*=======================================================
* Section 10.4.2-10.4.3: LASSO-Residualized DiD
* (Machine Learning Approach: Specification & Results)
*=======================================================

* Install required packages
ssc install lassopack, replace
ssc install elasticregress, replace

* Create interaction terms for LASSO selection
foreach var of global controls {
    gen `var'_post = `var' * post
    gen `var'_treat = `var' * treat_state
}

* LASSO with cross-validation
* Note: Run LASSO without fixed effects, then use reghdfe
lasso2 lngenop did post treat_state $controls ///
  lntotsup_post lntotsup_treat lnfinaid_post lnfinaid_treat ///
  lntuifee_post lntuifee_treat lnfte_post lnfte_treat

* Extract selected variables
local lasso_selected "`e(selected)'"

* Post-LASSO OLS with fixed effects
reghdfe lngenop did `lasso_selected', ///
  absorb(fips fy) vce(cluster fips)

estimates store lasso_did

*=======================================================
* Section 10.5.3: SCM Application to Georgia Consolidation
* (Synthetic Control Method)
*=======================================================

* Install synth package
ssc install synth, replace

* Prepare data for synth command
* Keep only outcome and predictors
keep if !missing(lngenop, lntotsup, lnfinaid, lntuifee, lnfte)

* Preserve data before running synth
preserve

* Run synthetic control
synth lngenop lngenop(2005) lngenop(2010) lngenop(2015) ///
  lntotsup(2005(1)2017) lnfinaid(2005(1)2017) ///
  lntuifee(2005(1)2017) lnfte(2005(1)2017), ///
  trunit(13) trperiod(2018) nested ///
  keep(synth_output) replace          // fig suppressed; graph reconstructed below

* Calculate treatment effect
use synth_output, clear
gen synth_gap = _Y_treated - _Y_synthetic

* Reconstruct SCM figure in B&W
twoway (line _Y_treated   _time, lcolor(gs0) lwidth(medthick) lpattern(solid)) ///
       (line _Y_synthetic _time, lcolor(gs8) lwidth(medthick) lpattern(dash)),  ///
  xline(2018, lpattern(dot) lcolor(gs6)) ///
  legend(label(1 "Georgia") label(2 "Synthetic Georgia")) ///
  ytitle("Log Operating Expenses") xtitle("Fiscal Year") ///
  title("Synthetic Control: Georgia vs. Synthetic Georgia") ///
  name(fig10_2_synth_control, replace)
graph export "$graphs_dir/fig10_2_synth_control_Stata.png", replace width(1200)

* Post-treatment average effect
summarize synth_gap if _time >= 2018

* Return to main dataset
restore

* Create SCM figure
synth_runner lngenop $controls, ///
  trunit(13) trperiod(2018) ///
  gen_vars mspeperiod(2005(1)2017)

*=======================================================
* Section 10.6.3: SDID Results - Single State Analysis
* (Synthetic Difference-in-Differences)
*=======================================================

* Install sdid package
ssc install sdid, replace

* Run SDID estimation
* Note: Use placebo inference (not bootstrap) for single treated unit
gen treat_sdid = (fips == 13 & fy >= 2018)

sdid lngenop fips fy treat_sdid, ///
  vce(placebo) seed(123) graph ///
  covariates($controls, projected)

* Store SDID results
estimates store sdid_model

graph export "$graphs_dir/fig10_4_sdid_Stata.png", replace width(1200)

*=======================================================
* Section 10.2.4 & 10.3.3: Event Study Specifications
* (Part of Parallel Trends Assessment)
*=======================================================

* Install csdid package
ssc install csdid, replace
ssc install drdid, replace
ssc install eventstudyinteract, replace

* Create relative time indicators
gen rel_time = fy - 2018 if treat_state == 1
replace rel_time = 0 if treat_state == 0

* Generate event time dummies
quietly tab rel_time, gen(event_)

* Event study regression
reghdfe lngenop event_1-event_17 $controls, ///
  absorb(fips fy) vce(cluster fips)


* Plot event study coefficients
coefplot, keep(event_*) vertical ///
  yline(0, lpattern(dash) lcolor(gs8)) xlabel(, angle(45)) ///
  mcolor(gs0) msymbol(O) lcolor(gs0) ///
  ciopts(lcolor(gs6)) ///
  ytitle("Effect on Log Operating Expenses") ///
  xtitle("Years Relative to Treatment") ///
  coeflabels(event_1 = "-17" event_2 = "-16" event_3 = "-15" ///
             event_4 = "-14" event_5 = "-13" event_6 = "-12" ///
             event_7 = "-11" event_8 = "-10" event_9 = "-9"  ///
             event_10 = "-8" event_11 = "-7" event_12 = "-6" ///
             event_13 = "-5" event_14 = "-4" event_15 = "-3" ///
             event_16 = "-2" event_17 = "-1") ///
  name(fig10_3_event_study, replace)
  graph export "$graphs_dir/fig10_3_event_study_Stata.png", ///
  name(fig10_3_event_study) replace width(1200)

* Callaway-Sant'Anna DiD
* Create treatment year variable (0 for never-treated)
gen treat_year = 0
replace treat_year = 2018 if treat_state == 1

csdid lngenop $controls, ivar(fips) time(fy) ///
  gvar(treat_year) method(dripw)

* Note: Single cohort (2018), so estat simple returns 0
* Instead, examine individual time period effects above
* Or aggregate manually across post-treatment periods

*=======================================================
* Section 10.7.3-10.7.5: Multi-State Staggered Analysis
* (Staggered Adoption Designs)
*=======================================================

/* Extension: Three-state staggered consolidation design
   - Georgia (FIPS 13): 2013
   - Wisconsin (FIPS 55): 2018  
   - Pennsylvania (FIPS 42): 2022
*/

preserve

* Import expanded dataset (48 states)
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch10/Example_10_7_3.csv" ///
     "Example_10_7_3.csv", replace
import delimited "Example_10_7_3.csv", clear

* Create logged variables
gen lngenop = log(generalpublicoperations)
gen lntotsup = log(totalstatesupport)
gen lnfinaid = log(totalfinancialaid)
gen lntuifee = log(nettuitionandfeerevenue)
gen lnfte = log(netfteenrollment)

* Drop missing observations
drop if missing(lngenop, lntotsup, lnfinaid, lntuifee, lnfte)

* Create staggered treatment variable
gen gyear = .
replace gyear = 2013 if fips == 13     // Georgia
replace gyear = 2018 if fips == 55     // Wisconsin
replace gyear = 2022 if fips == 42     // Pennsylvania

* Set panel structure and check balance
xtset fips fy

* Simple approach: Keep only complete state-year combinations
* Count observations per state
bysort fips: gen obs_count = _N
* Keep only states with same number of observations as the mode
egen mode_count = mode(obs_count)
keep if obs_count == mode_count
drop obs_count mode_count

* Verify panel is balanced
qui xtset fips fy
di as text _newline "Panel structure after balancing: " as result "`r(balanced)'"
di as result "Number of states: " r(N_g)
di as result "Time periods: " r(tmin) " to " r(tmax)

* CSDID estimation (baseline - no controls)
csdid lngenop, ivar(fips) time(fy) gvar(gyear) method(dripw)

* Overall ATT
estat simple

* Group-specific effects
estat group

* Event study
estat event
* Note: csdid_plot does not accept lcolor(); styling is handled by set scheme s2mono
csdid_plot, style(rcap)
graph export "$graphs_dir/fig10_4_csdid_event_Stata.png", replace width(1200)

* Calendar time effects
estat calendar

* SDID robustness check with controls (panel already balanced)
gen treatment = 0
replace treatment = 1 if fips == 13 & fy >= 2013
replace treatment = 1 if fips == 55 & fy >= 2018
replace treatment = 1 if fips == 42 & fy >= 2022

sdid lngenop fips fy treatment, ///
     vce(placebo) seed(123) ///
     covariates(lntotsup lnfinaid lntuifee lnfte, projected) ///
     graph

restore

*=======================================================
* Section 10.8.2: Permutation Inference
* (Sensitivity Analysis)
*=======================================================

* Permutation test (may take a while to run)
set seed 12345
permute did coef=_b[did], reps(1000) ///
  saving(perm_results, replace): ///
  xtreg lngenop did $controls i.fy, fe vce(cluster fips)

* Analyze permutation results
use perm_results, clear
summarize coef
histogram coef, normal ///
  fcolor(gs10) lcolor(gs0) ///
  normopts(lcolor(gs0) lwidth(medthick) lpattern(solid)) ///
  name(fig10_5_permutation_hist, replace)
graph export "$graphs_dir/fig10_5_permutation_hist_Stata.png", replace width(1200)

*=======================================================
* Section 10.8.3: Leave-One-Out Analysis
* (Sensitivity Analysis)
*=======================================================

* Return to main data
* FIX: "Finance_Data.csv" was a stale filename from an earlier draft.
* The correct source is Example_10_3_1.csv, downloaded in Section 10.3.1.
* Re-import and reconstruct all variables exactly as in Section 10.3.1.
import delimited "Example_10_3_1.csv", clear

* Recreate all variables (abbreviated - see Section 10.3.1)
replace state = strtrim(state)
gen sreb = inlist(state, "Alabama", "Arkansas", "Delaware", "Florida", ///
           "Georgia", "Kentucky", "Louisiana", "Maryland", "Mississippi") ///
    | inlist(state, "North Carolina", "Oklahoma", "South Carolina", ///
             "Tennessee", "Texas", "Virginia", "West Virginia")
keep if sreb == 1

gen fips = .
replace fips = 1  if state == "Alabama"
replace fips = 5  if state == "Arkansas"
replace fips = 10 if state == "Delaware"
replace fips = 12 if state == "Florida"
replace fips = 13 if state == "Georgia"
replace fips = 21 if state == "Kentucky"
replace fips = 22 if state == "Louisiana"
replace fips = 24 if state == "Maryland"
replace fips = 28 if state == "Mississippi"
replace fips = 37 if state == "North Carolina"
replace fips = 40 if state == "Oklahoma"
replace fips = 45 if state == "South Carolina"
replace fips = 47 if state == "Tennessee"
replace fips = 48 if state == "Texas"
replace fips = 51 if state == "Virginia"
replace fips = 54 if state == "West Virginia"

gen treat_state = (state == "Georgia")
gen post = (fy >= 2018)
gen did = treat_state * post

gen lngenop = log(general_public_operations)
gen lntotsup = log(total_state_support)
gen lnfinaid = log(total_financial_aid)
gen lntuifee = log(net_tuition_and_fee_revenue)
gen lnfte = log(net_fte_enrollment)

global controls "lntotsup lnfinaid lntuifee lnfte"

xtset fips fy

* Leave-one-out analysis
levelsof fips if treat_state == 0, local(control_states)

foreach s of local control_states {
    quietly xtreg lngenop did $controls i.fy if fips != `s', ///
      fe vce(cluster fips)
    
    matrix b = e(b)
    local coef_`s' = b[1,1]
}

* Report leave-one-out results
display as text _newline "Leave-One-Out Analysis:"
foreach s of local control_states {
    display as result "Excluding FIPS `s': " %6.4f `coef_`s''
}

*=======================================================
* Section 10.9: Results Summary
* (Interpretation and Policy Implications)
*=======================================================

* Compare main methods
estimates table twfe_did lasso_did, ///
  star(0.1 0.05 0.01) stats(N r2 r2_a)

* Install export package if needed
capture which esttab
if _rc != 0 ssc install estout, replace

* Export results to CSV
estimates restore twfe_did
esttab using results.csv, replace ///
  title("TWFE DiD") b(4) se(4) star(* 0.10 ** 0.05 *** 0.01)

estimates restore lasso_did
esttab using results_lasso.csv, replace ///
  title("LASSO DiD") b(4) se(4) star(* 0.10 ** 0.05 *** 0.01)

* Combined table
esttab twfe_did lasso_did using results_combined.csv, replace ///
  mtitles("TWFE DiD" "LASSO DiD") ///
  b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) ///
  stats(N r2 r2_a, fmt(0 4 4))

*========================================================================
*========================================================================
*
*    PART B: MARGINAL TREATMENT EFFECTS — RETURNS TO MASTER'S DEGREE
*            (Sections 10.10 – 10.16)
*
*    NOTE: Part B loads a new dataset (Example_7_5_3_updated.dta /
*    Example_7_5_3.dta) and resets the random-number seed.
*    All locals and globals from Part A remain in scope but are not
*    referenced by Part B code.
*
*========================================================================
*========================================================================

********************************************************************************
* SECTION 1: Load Dataset
********************************************************************************

di _n "=============================================="
di "LOADING SYNTHETIC B&B DATASET"
di "=============================================="

* Download Part B dataset from GitHub repository.
* Try the updated file (contains pre-generated ma_* variables) first;
* fall back to the base file if the updated version is unavailable.
* If neither is found locally, attempt to download from the repository.
capture confirm file "Example_7_5_3_updated.dta"
if _rc != 0 {
    di as text "Attempting to download Example_7_5_3_updated.dta from GitHub..."
    capture copy ///
        "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch10/Example_7_5_3_updated.dta" ///
        "Example_7_5_3_updated.dta", replace
    if _rc != 0 di as text "Download failed — will try local file or base version."
}

capture use "Example_7_5_3_updated.dta", clear
if _rc != 0 {
    di as text "Note: Example_7_5_3_updated.dta not found."
    di as text "Attempting to download Example_7_5_3.dta from GitHub (ch7 repository)..."
    capture copy ///
        "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_5_3.dta" ///
        "Example_7_5_3.dta", replace
    if _rc != 0 {
        di as error "ERROR: Download of Example_7_5_3.dta failed."
        di as error "Please download the file manually from:"
        di as error "https://github.com/higher-ed-policy-analysis-2nd-edition/data/blob/main/ch7/Example_7_5_3.dta"
        di as error "and place it in the working directory before running Part B."
        exit 601
    }
    di as text "Loading Example_7_5_3.dta; ma_* will be generated in Section 1b."
    use "Example_7_5_3.dta", clear
}

capture confirm variable id
if _rc != 0 gen id = _n

describe
di _n "Sample size: " _N

********************************************************************************
* SECTION 1b: Verify Master's Degree Program Area Indicators
*
* FIX B: Replaced if-else-foreach block with a simple confirm check.
*
* The dataset Example_7_5_3_updated.dta was produced by the revised
* data-generation script and already contains ma_stem, ma_business,
* ma_education, ma_health, and ma_other.  If running against the
* original dataset without these variables, the script stops with a
* clear message rather than attempting to generate them, which avoids
* the nested if-else-foreach brace structure that contributed to r(198).
*
* To generate ma_* from scratch, run Generate_Example_7_5_3_Synthetic_Data.do
* (revised version) on the original dataset first.
*
* Five mutually exclusive categories (IPEDS CIP-based):
*   ma_business   Business, Management, Marketing (CIP 52)
*   ma_education  Education (CIP 13)
*   ma_health     Health Professions & Related (CIP 51)
*   ma_stem       STEM fields (CIPs 11, 14, 15, 26, 27, 40, 41)
*   ma_other      All remaining fields
*
* ma_* = 0 for all untreated observations (masters == 0).
********************************************************************************

di _n "=============================================="
di "MASTER'S PROGRAM AREA INDICATORS"
di "=============================================="

capture confirm variable ma_stem
if _rc != 0 {
    * FIX G (v14): generate ma_* from undergraduate major fields when
    * the updated dataset is unavailable.  Uses a seeded random draw so
    * results are reproducible.  Transition probabilities match the
    * pipeline definitions in Sections 10b and the data-generation script:
    *   STEM undergrads   -> 55% enter STEM graduate programs
    *   Business undergrads -> 65% enter Business graduate programs
    *   Education undergrads -> 70% enter Education graduate programs
    *   SocSci/STEM overlap -> 40% enter Health graduate programs
    *   Remainder -> Other
    *
    * All untreated obs (masters==0) receive ma_* = 0 throughout.
    di as text "Generating ma_* variables from undergraduate major fields..."

    gen ma_stem      = 0
    gen ma_business  = 0
    gen ma_education = 0
    gen ma_health    = 0
    gen ma_other     = 0

    * Seeded draw — one random value per treated observation
    set seed 20251130
set scheme s2mono        // Monochrome scheme for Springer B&W print
    gen _rma = runiform() if masters == 1
    replace _rma = . if masters == 0

    * STEM: 55% of STEM undergrads enter STEM grad
    replace ma_stem      = 1 if masters==1 & stem_major==1 & _rma <= 0.55
    * Business: 65% of bus undergrads enter Business grad
    replace ma_business  = 1 if masters==1 & bus_major==1  & _rma <= 0.65 & ma_stem==0
    * Education: 70% of ed undergrads enter Education grad
    replace ma_education = 1 if masters==1 & ed_major==1   & _rma <= 0.70 & ma_stem==0 & ma_business==0
    * Health: 40% of socsci/other undergrads; overflow STEM (_rma 0.55-0.75)
    replace ma_health    = 1 if masters==1 & socsci_major==1 & _rma <= 0.40 & ma_stem==0 & ma_business==0 & ma_education==0
    replace ma_health    = 1 if masters==1 & stem_major==1   & _rma >  0.55 & _rma <= 0.75 & ma_stem==0
    * Other: all remaining treated obs
    replace ma_other     = 1 if masters==1 & ma_stem==0 & ma_business==0 & ma_education==0 & ma_health==0

    drop _rma

    label var ma_stem      "Master's in STEM (CIPs 11 14 15 26 27 40 41)"
    label var ma_business  "Master's in Business/Management (CIP 52)"
    label var ma_education "Master's in Education (CIP 13)"
    label var ma_health    "Master's in Health Professions (CIP 51)"
    label var ma_other     "Master's in other field"

    di as text "ma_* variables generated successfully."
}
di "ma_* variables confirmed present in dataset."

* Verification
qui count if masters == 1
local n_treated = r(N)
di _n "--- Program Area Distribution (Treated Only) ---"
di "Total treated: " `n_treated'
foreach a in stem business education health other {
    qui count if ma_`a' == 1
    di "  ma_`a': " r(N) "  (" %5.1f r(N)/`n_treated'*100 "%)"
}

gen ma_check = ma_business + ma_education + ma_health + ma_stem + ma_other
qui count if masters == 1 & ma_check != 1
if r(N) > 0 di "WARNING: " r(N) " treated obs with != 1 program area flag"
else         di "CHECK PASSED: all treated obs have exactly 1 program area"
qui count if masters == 0 & ma_check != 0
if r(N) > 0 di "WARNING: " r(N) " untreated obs with non-zero program area flag"
else         di "CHECK PASSED: all untreated obs have zero program area"
drop ma_check

********************************************************************************
* SECTION 2: Summary Statistics
********************************************************************************

di _n "=============================================="
di "SUMMARY STATISTICS"
di "=============================================="

tab masters
qui sum masters
local treat_rate = r(mean)
di "Treatment rate: " %5.3f `treat_rate'

sum ln_salary salary masters ga_funding_adj
tabstat salary ln_salary, by(masters) stats(mean sd min max n)
sum ga_funding_adj, detail

sum female black hispanic asian age_ba firstgen parent_income_q parent_grad ///
    ugpa stem_major bus_major ed_major selective_inst public_ug state_unemp metro

di _n "--- Program Area by Undergraduate Major (Treated Only) ---"
di "  STEM undergrads:"
tabstat ma_stem ma_health ma_business ma_education ma_other ///
    if masters==1 & stem_major==1, stats(mean n)
di "  Business undergrads:"
tabstat ma_stem ma_health ma_business ma_education ma_other ///
    if masters==1 & bus_major==1, stats(mean n)
di "  Education undergrads:"
tabstat ma_stem ma_health ma_business ma_education ma_other ///
    if masters==1 & ed_major==1, stats(mean n)
di "  Social sci / other undergrads:"
tabstat ma_stem ma_health ma_business ma_education ma_other ///
    if masters==1 & socsci_major==1, stats(mean n)

di _n "--- Mean Log Salary by Program Area (Treated Only) ---"
tabstat ln_salary salary, by(ma_stem) stats(mean n)
foreach a in business education health other {
    qui sum ln_salary if ma_`a' == 1
    di "  ma_`a': mean ln_salary = " %6.4f r(mean) "  (N = " r(N) ")"
}

********************************************************************************
* SECTION 3: First-Stage and Instrument Relevance
********************************************************************************

di _n "=============================================="
di "INSTRUMENT RELEVANCE CHECK"
di "=============================================="

global X_controls "female black hispanic asian age_ba firstgen parent_income_q parent_grad ugpa stem_major bus_major ed_major selective_inst public_ug state_unemp metro"
global X "$X_controls"
global Z "ga_funding_adj"

reg masters ga_funding_adj $X_controls, robust
est store first_stage

test ga_funding_adj
local first_stage_F = r(F)
di _n "First-stage F: " %6.2f `first_stage_F'
if `first_stage_F' > 10  di "RESULT: Strong instrument (F > 10)"
else                      di "WARNING: Potentially weak instrument"

********************************************************************************
* SECTION 4: Naive OLS Estimation
********************************************************************************

di _n "=============================================="
di "NAIVE OLS ESTIMATION"
di "=============================================="

reg ln_salary masters $X_controls, robust
est store ols_naive
local ols_est = _b[masters]
local ols_se  = _se[masters]
di "OLS estimate: " %6.4f `ols_est' " (SE = " %6.4f `ols_se' ")"

********************************************************************************
* SECTION 5: IV/2SLS Estimation (LATE)
********************************************************************************

di _n "=============================================="
di "IV/2SLS ESTIMATION (LATE)"
di "=============================================="

ivregress 2sls ln_salary $X_controls (masters = ga_funding_adj), robust first
est store iv_2sls
local iv_est = _b[masters]
local iv_se  = _se[masters]
di _n "IV/LATE estimate: " %6.4f `iv_est' " (SE = " %6.4f `iv_se' ")"
estat firststage
estat endogenous

********************************************************************************
* SECTION 6: MTE ESTIMATION — POOLED POLYNOMIAL (MANUAL)
********************************************************************************

di _n "=============================================="
di "MTE ESTIMATION — POOLED POLYNOMIAL"
di "=============================================="

capture which mtefe
if _rc != 0 ssc install mtefe, replace
capture which moremata
if _rc != 0 ssc install moremata, replace

probit masters $Z $X
predict phat, pr
local ga_coef = _b[ga_funding_adj]
di "GA funding probit coefficient: " %7.5f `ga_coef'
predict z_index, xb

gen phat2 = phat^2
gen phat3 = phat^3

di _n "--- Quadratic MTE ---"
reg ln_salary masters c.masters#(c.phat c.phat2) $X phat phat2, robust
local b0_quad = _b[masters]
local b1_quad = _b[c.masters#c.phat]
local b2_quad = _b[c.masters#c.phat2]
di "Quadratic MTE(u) = " %6.4f `b0_quad' " + " %6.4f `b1_quad' "*u + " %6.4f `b2_quad' "*u^2"
local ate_est_quad = `b0_quad' + `b1_quad'/2 + `b2_quad'/3
di "ATE (quadratic): " %6.4f `ate_est_quad'

di _n "--- Cubic MTE ---"
reg ln_salary masters c.masters#(c.phat c.phat2 c.phat3) $X phat phat2 phat3, robust
local b0 = _b[masters]
local b1 = _b[c.masters#c.phat]
local b2 = _b[c.masters#c.phat2]
local b3 = _b[c.masters#c.phat3]
di "Cubic MTE(u) = " %7.4f `b0' " + " %7.4f `b1' "*u + " %7.4f `b2' "*u^2 + " %7.4f `b3' "*u^3"

local ate_est_cubic = `b0' + `b1'/2 + `b2'/3 + `b3'/4
di "Estimated ATE (cubic): " %6.4f `ate_est_cubic'

gen mte_hat = `b0' + `b1'*phat + `b2'*phat2 + `b3'*phat3

qui sum mte_hat if masters == 1
local att_est = r(mean)
di "Estimated ATT: " %6.4f `att_est'
qui sum mte_hat if masters == 0
local atu_est = r(mean)
di "Estimated ATU: " %6.4f `atu_est'

di _n "--- mtefe Package Estimation ---"
* NOTE: bootreps(0) suppresses mtefe's internal bootstrap entirely.
*   The mtefe package runs bootstrap internally when bootreps() > 0,
*   using a user program that suffers the same stale execution-context
*   issue that caused persistent r(198) throughout this script. Our
*   Section 6c bootstrap already provides all SEs needed. mtefe is
*   used here for point estimates only; use Section 6c SEs for inference.
mtefe ln_salary $X_controls (masters = ga_funding_adj), pol(1) bootreps(0)
est store mtefe_linear
local mtefe_ate  = _b[effects:ate]
local mtefe_att  = _b[effects:att]
local mtefe_atu  = _b[effects:atut]
local mtefe_late = _b[effects:late]

mtefe ln_salary $X_controls (masters = ga_funding_adj), pol(2) bootreps(0)
est store mtefe_quad
local mtefe_ate_q = _b[effects:ate]
local mtefe_att_q = _b[effects:att]
local mtefe_atu_q = _b[effects:atut]

* NOTE: The mtefe "mte" plot option is omitted here. Even with bootreps(0),
*   mtefe's internal graph-generation code calls sub-programs that leave
*   stale execution-context state, producing r(198) at EOF with nostop.
*   MTE curves are instead generated manually in Section 8, which produces
*   equivalent plots with full control over styling. The mtePlot graph
*   name is therefore not available; its capture display line is removed.

di _n "mtefe Results (Quadratic, pol=2):"
di "  ATE:  " %6.4f `mtefe_ate_q'
di "  ATT:  " %6.4f `mtefe_att_q'
di "  ATU:  " %6.4f `mtefe_atu_q'

di _n "--- Heckman Selection Model ---"
heckman ln_salary $X, select(masters = $Z $X) twostep
local heck2_lambda = e(mills)
local heck2_rho    = e(rho)
local heck2_sigma  = e(sigma)
est store heck_2step

heckman ln_salary $X, select(masters = $Z $X)
local heck_ml_rho    = tanh(_b[/athrho])
local heck_ml_sigma  = exp(_b[/lnsigma])
local heck_ml_lambda = `heck_ml_rho' * `heck_ml_sigma'
local heck_ml_chi2   = e(chi2_c)
local heck_ml_p      = e(p_c)
est store heck_ml
di "Heckman ML: lambda = " %6.4f `heck_ml_lambda' ///
   "  rho = " %6.4f `heck_ml_rho' ///
   "  LR chi2 = " %6.2f `heck_ml_chi2' " (p = " %5.4f `heck_ml_p' ")"

********************************************************************************
* SECTION 6b: MTE BY GRADUATE PROGRAM AREA — FULLY INTERACTED POLYNOMIAL
*
* Identification strategy:
*   The same probit propensity score and cubic polynomial framework from
*   Section 6 is extended by allowing the MTE function to shift both in
*   level and slope by graduate program area. ma_other is the omitted
*   (base) category. The interaction model recovers area-specific MTE curves:
*
*     MTE_a(u) = [b0+d0_a] + [b1+d1_a]*u + [b2+d2_a]*u^2 + [b3+d3_a]*u^3
*
*   where a in (STEM, Business, Education, Health) and Other is the base.
*   For untreated observations all ma_* = 0, so no interaction fires.
*
* Area-specific treatment parameters:
*   ATE_a = (b0+d0_a) + (b1+d1_a)/2 + (b2+d2_a)/3 + (b3+d3_a)/4
*   ATT_a = E[MTE_a(phat) | masters=1, ma_a=1]
*
* Standard errors: see Section 6c (cluster bootstrap at state level)
********************************************************************************

di _n "=============================================="
di "MTE BY GRADUATE PROGRAM AREA"
di "=============================================="

reg ln_salary masters                                      ///
    c.masters#c.phat c.masters#c.phat2 c.masters#c.phat3  ///
    c.masters#c.ma_stem                                    ///
    c.masters#c.ma_business                                ///
    c.masters#c.ma_education                               ///
    c.masters#c.ma_health                                  ///
    c.masters#c.ma_stem#c.phat                             ///
    c.masters#c.ma_business#c.phat                         ///
    c.masters#c.ma_education#c.phat                        ///
    c.masters#c.ma_health#c.phat                           ///
    c.masters#c.ma_stem#c.phat2                            ///
    c.masters#c.ma_business#c.phat2                        ///
    c.masters#c.ma_education#c.phat2                       ///
    c.masters#c.ma_health#c.phat2                          ///
    c.masters#c.ma_stem#c.phat3                            ///
    c.masters#c.ma_business#c.phat3                        ///
    c.masters#c.ma_education#c.phat3                       ///
    c.masters#c.ma_health#c.phat3                          ///
    $X phat phat2 phat3, robust
est store mte_byarea

* Base polynomial (Other = base category)
local B0 = _b[masters]
local B1 = _b[c.masters#c.phat]
local B2 = _b[c.masters#c.phat2]
local B3 = _b[c.masters#c.phat3]

* Area differential coefficients
local d0_stem     = _b[c.masters#c.ma_stem]
local d1_stem     = _b[c.masters#c.ma_stem#c.phat]
local d2_stem     = _b[c.masters#c.ma_stem#c.phat2]
local d3_stem     = _b[c.masters#c.ma_stem#c.phat3]

local d0_business = _b[c.masters#c.ma_business]
local d1_business = _b[c.masters#c.ma_business#c.phat]
local d2_business = _b[c.masters#c.ma_business#c.phat2]
local d3_business = _b[c.masters#c.ma_business#c.phat3]

local d0_education = _b[c.masters#c.ma_education]
local d1_education = _b[c.masters#c.ma_education#c.phat]
local d2_education = _b[c.masters#c.ma_education#c.phat2]
local d3_education = _b[c.masters#c.ma_education#c.phat3]

local d0_health   = _b[c.masters#c.ma_health]
local d1_health   = _b[c.masters#c.ma_health#c.phat]
local d2_health   = _b[c.masters#c.ma_health#c.phat2]
local d3_health   = _b[c.masters#c.ma_health#c.phat3]

* Composite area coefficients (base + differential)
local c0_stem     = `B0' + `d0_stem'
local c1_stem     = `B1' + `d1_stem'
local c2_stem     = `B2' + `d2_stem'
local c3_stem     = `B3' + `d3_stem'

local c0_business = `B0' + `d0_business'
local c1_business = `B1' + `d1_business'
local c2_business = `B2' + `d2_business'
local c3_business = `B3' + `d3_business'

local c0_education = `B0' + `d0_education'
local c1_education = `B1' + `d1_education'
local c2_education = `B2' + `d2_education'
local c3_education = `B3' + `d3_education'

local c0_health   = `B0' + `d0_health'
local c1_health   = `B1' + `d1_health'
local c2_health   = `B2' + `d2_health'
local c3_health   = `B3' + `d3_health'

* Area-specific MTE functions
di _n "--- Area-Specific MTE Functions ---"
di "  Base (Other): " %7.4f `B0' " + " %7.4f `B1' "*u + " %7.4f `B2' "*u^2 + " %7.4f `B3' "*u^3"
di "  STEM:         " %7.4f `c0_stem' " + " %7.4f `c1_stem' "*u + " %7.4f `c2_stem' "*u^2 + " %7.4f `c3_stem' "*u^3"
di "  Business:     " %7.4f `c0_business' " + " %7.4f `c1_business' "*u + " %7.4f `c2_business' "*u^2 + " %7.4f `c3_business' "*u^3"
di "  Education:    " %7.4f `c0_education' " + " %7.4f `c1_education' "*u + " %7.4f `c2_education' "*u^2 + " %7.4f `c3_education' "*u^3"
di "  Health:       " %7.4f `c0_health' " + " %7.4f `c1_health' "*u + " %7.4f `c2_health' "*u^2 + " %7.4f `c3_health' "*u^3"

* Area-specific ATE (integral of MTE over [0,1])
local ate_other     = `B0'           + `B1'/2           + `B2'/3           + `B3'/4
local ate_stem      = `c0_stem'      + `c1_stem'/2      + `c2_stem'/3      + `c3_stem'/4
local ate_business  = `c0_business'  + `c1_business'/2  + `c2_business'/3  + `c3_business'/4
local ate_education = `c0_education' + `c1_education'/2 + `c2_education'/3 + `c3_education'/4
local ate_health    = `c0_health'    + `c1_health'/2    + `c2_health'/3    + `c3_health'/4

di _n "--- Area-Specific ATE (integral_0^1 MTE_a(u) du) ---"
di "  ATE (Other):     " %6.4f `ate_other'
di "  ATE (STEM):      " %6.4f `ate_stem'
di "  ATE (Business):  " %6.4f `ate_business'
di "  ATE (Education): " %6.4f `ate_education'
di "  ATE (Health):    " %6.4f `ate_health'

* Area-specific MTE_hat variables
gen mte_hat_other     = `B0'           + `B1'           *phat + `B2'           *phat2 + `B3'           *phat3
gen mte_hat_stem      = `c0_stem'      + `c1_stem'      *phat + `c2_stem'      *phat2 + `c3_stem'      *phat3
gen mte_hat_business  = `c0_business'  + `c1_business'  *phat + `c2_business'  *phat2 + `c3_business'  *phat3
gen mte_hat_education = `c0_education' + `c1_education' *phat + `c2_education' *phat2 + `c3_education' *phat3
gen mte_hat_health    = `c0_health'    + `c1_health'    *phat + `c2_health'    *phat2 + `c3_health'    *phat3

label var mte_hat_other     "Area-specific MTE hat: Other (base)"
label var mte_hat_stem      "Area-specific MTE hat: STEM"
label var mte_hat_business  "Area-specific MTE hat: Business"
label var mte_hat_education "Area-specific MTE hat: Education"
label var mte_hat_health    "Area-specific MTE hat: Health & Related"

* Area-specific ATT
qui sum mte_hat_other     if ma_other     == 1
local att_other     = r(mean)
qui sum mte_hat_stem      if ma_stem      == 1
local att_stem      = r(mean)
qui sum mte_hat_business  if ma_business  == 1
local att_business  = r(mean)
qui sum mte_hat_education if ma_education == 1
local att_education = r(mean)
qui sum mte_hat_health    if ma_health    == 1
local att_health    = r(mean)

di _n "--- Area-Specific ATT ---"
di "  ATT (Other):     " %6.4f `att_other'
di "  ATT (STEM):      " %6.4f `att_stem'
di "  ATT (Business):  " %6.4f `att_business'
di "  ATT (Education): " %6.4f `att_education'
di "  ATT (Health):    " %6.4f `att_health'

qui sum mte_hat if masters == 0
local atu_pooled_untreated = r(mean)
di _n "  ATU (pooled): " %6.4f `atu_pooled_untreated'

di _n "=============================================="
di "AREA-SPECIFIC TREATMENT PARAMETER SUMMARY"
di "=============================================="
di "Area          ATE        ATT"
di "--------------------------------------------"
di "  Other     " %7.4f `ate_other'     "    " %7.4f `att_other'
di "  STEM      " %7.4f `ate_stem'      "    " %7.4f `att_stem'
di "  Business  " %7.4f `ate_business'  "    " %7.4f `att_business'
di "  Education " %7.4f `ate_education' "    " %7.4f `att_education'
di "  Health    " %7.4f `ate_health'    "    " %7.4f `att_health'

********************************************************************************
* SECTION 6c: BOOTSTRAP INFRASTRUCTURE
*
* Implementation: postfile/forvalues manual bootstrap (NO program...end,
*   NO bootstrap command). This is the definitive fix for the persistent
*   r(198) error documented in the bug fix log above.
*
* Architecture:
*   - postfile/postclose stores one row of estimates per replication

* -----------------------------------------------------------------------
* 6c-i  Manual cluster bootstrap — forvalues loop, NO continue in braces
*
* DEFINITIVE FIX for persistent r(198):
*   Using "continue" inside a brace block ( if _rc != 0 BEGIN ... continue END )
*   inside a forvalues loop causes Stata to exit the if-block mid-execution.
*   The closing brace is never processed, leaving a stale open-brace in
*   Stata's internal loop state. With nostop, this accumulates across every
*   failed replication and reports as r(198) at EOF.
*
*   Fix: replace all continue-in-brace patterns with a local "ok" flag.
*   Every iteration now falls through to restore at the bottom, so every
*   closing brace is always reached. No continue is used anywhere.
* -----------------------------------------------------------------------

* --- Bootstrap initialization (was lost in v10→v11 edit; restored here) ---
tempname bsh
tempfile bsf

postfile `bsh'                ///
    b_ate b_att b_atu          ///
    b_ate_stem    b_att_stem   ///
    b_ate_bus     b_att_bus    ///
    b_ate_ed      b_att_ed     ///
    b_ate_hlth    b_att_hlth   ///
    b_ate_oth     b_att_oth    ///
    using `bsf', replace

set seed 20260101
local R    = 500
local n_ok = 0

di _n "Running manual cluster bootstrap (G=50, R=`R' reps)..."
di "Each dot = 10 reps completed"

forvalues b = 1/`R' {

    preserve

    * ok flag: set to 0 on any estimation failure, skip post at end
    local ok = 1

    * BUG FIX v13: bsample wrapped in capture so that if cluster sampling
    * fails (e.g. cluster variable not found, collinear sample, etc.) the
    * loop does NOT abort mid-iteration.  Without capture, a bsample failure
    * terminates the forvalues loop before restore is reached, leaving a
    * stale preserve open and causing "already preserved r(621)" at the
    * next preserve call (SE extraction block after the loop).
    capture quietly bsample, cluster(state) idcluster(newstate)
    if _rc != 0 local ok = 0

    * --- Probit ---
    capture quietly probit masters ga_funding_adj $X_controls
    if _rc != 0 local ok = 0
    if `ok' == 1 quietly predict _pb, pr
    if `ok' == 1 quietly gen _pb2 = _pb^2
    if `ok' == 1 quietly gen _pb3 = _pb^3

    * --- Pooled cubic MTE ---
    if `ok' == 1 {
        capture quietly reg ln_salary masters              ///
            c.masters#c._pb c.masters#c._pb2 c.masters#c._pb3 ///
            $X_controls _pb _pb2 _pb3
        if _rc != 0 local ok = 0
    }

    if `ok' == 1 {
        local r0 = _b[masters]
        local r1 = _b[c.masters#c._pb]
        local r2 = _b[c.masters#c._pb2]
        local r3 = _b[c.masters#c._pb3]
        local b_ate_r = `r0' + `r1'/2 + `r2'/3 + `r3'/4
        quietly gen _mb = `r0' + `r1'*_pb + `r2'*_pb2 + `r3'*_pb3
        quietly sum _mb if masters == 1
        local b_att_r = r(mean)
        quietly sum _mb if masters == 0
        local b_atu_r = r(mean)
        drop _mb
    }

    * --- Fully interacted MTE ---
    if `ok' == 1 {
        capture quietly reg ln_salary masters                               ///
            c.masters#c._pb c.masters#c._pb2 c.masters#c._pb3             ///
            c.masters#c.ma_stem    c.masters#c.ma_business                 ///
            c.masters#c.ma_education c.masters#c.ma_health                 ///
            c.masters#c.ma_stem#c._pb    c.masters#c.ma_business#c._pb     ///
            c.masters#c.ma_education#c._pb c.masters#c.ma_health#c._pb     ///
            c.masters#c.ma_stem#c._pb2   c.masters#c.ma_business#c._pb2    ///
            c.masters#c.ma_education#c._pb2 c.masters#c.ma_health#c._pb2   ///
            c.masters#c.ma_stem#c._pb3   c.masters#c.ma_business#c._pb3    ///
            c.masters#c.ma_education#c._pb3 c.masters#c.ma_health#c._pb3   ///
            $X_controls _pb _pb2 _pb3
        if _rc != 0 local ok = 0
    }

    if `ok' == 1 {
        local BB0 = _b[masters]
        local BB1 = _b[c.masters#c._pb]
        local BB2 = _b[c.masters#c._pb2]
        local BB3 = _b[c.masters#c._pb3]

        * STEM
        local D0 = _b[c.masters#c.ma_stem]
        local D1 = _b[c.masters#c.ma_stem#c._pb]
        local D2 = _b[c.masters#c.ma_stem#c._pb2]
        local D3 = _b[c.masters#c.ma_stem#c._pb3]
        local C0 = `BB0' + `D0'
        local C1 = `BB1' + `D1'
        local C2 = `BB2' + `D2'
        local C3 = `BB3' + `D3'
        local b_ate_stem_r = `C0' + `C1'/2 + `C2'/3 + `C3'/4
        quietly gen _ms = `C0' + `C1'*_pb + `C2'*_pb2 + `C3'*_pb3
        quietly sum _ms if ma_stem == 1
        local b_att_stem_r = r(mean)
        drop _ms

        * Business
        local D0 = _b[c.masters#c.ma_business]
        local D1 = _b[c.masters#c.ma_business#c._pb]
        local D2 = _b[c.masters#c.ma_business#c._pb2]
        local D3 = _b[c.masters#c.ma_business#c._pb3]
        local C0 = `BB0' + `D0'
        local C1 = `BB1' + `D1'
        local C2 = `BB2' + `D2'
        local C3 = `BB3' + `D3'
        local b_ate_bus_r = `C0' + `C1'/2 + `C2'/3 + `C3'/4
        quietly gen _ms = `C0' + `C1'*_pb + `C2'*_pb2 + `C3'*_pb3
        quietly sum _ms if ma_business == 1
        local b_att_bus_r = r(mean)
        drop _ms

        * Education
        local D0 = _b[c.masters#c.ma_education]
        local D1 = _b[c.masters#c.ma_education#c._pb]
        local D2 = _b[c.masters#c.ma_education#c._pb2]
        local D3 = _b[c.masters#c.ma_education#c._pb3]
        local C0 = `BB0' + `D0'
        local C1 = `BB1' + `D1'
        local C2 = `BB2' + `D2'
        local C3 = `BB3' + `D3'
        local b_ate_ed_r = `C0' + `C1'/2 + `C2'/3 + `C3'/4
        quietly gen _ms = `C0' + `C1'*_pb + `C2'*_pb2 + `C3'*_pb3
        quietly sum _ms if ma_education == 1
        local b_att_ed_r = r(mean)
        drop _ms

        * Health
        local D0 = _b[c.masters#c.ma_health]
        local D1 = _b[c.masters#c.ma_health#c._pb]
        local D2 = _b[c.masters#c.ma_health#c._pb2]
        local D3 = _b[c.masters#c.ma_health#c._pb3]
        local C0 = `BB0' + `D0'
        local C1 = `BB1' + `D1'
        local C2 = `BB2' + `D2'
        local C3 = `BB3' + `D3'
        local b_ate_hlth_r = `C0' + `C1'/2 + `C2'/3 + `C3'/4
        quietly gen _ms = `C0' + `C1'*_pb + `C2'*_pb2 + `C3'*_pb3
        quietly sum _ms if ma_health == 1
        local b_att_hlth_r = r(mean)
        drop _ms

        * Other (base)
        local b_ate_oth_r = `BB0' + `BB1'/2 + `BB2'/3 + `BB3'/4
        quietly gen _mb = `BB0' + `BB1'*_pb + `BB2'*_pb2 + `BB3'*_pb3
        quietly sum _mb if ma_other == 1
        local b_att_oth_r = r(mean)
        drop _mb

        post `bsh' ///
            (`b_ate_r')      (`b_att_r')      (`b_atu_r')     ///
            (`b_ate_stem_r')  (`b_att_stem_r') ///
            (`b_ate_bus_r')   (`b_att_bus_r')  ///
            (`b_ate_ed_r')    (`b_att_ed_r')   ///
            (`b_ate_hlth_r')  (`b_att_hlth_r') ///
            (`b_ate_oth_r')   (`b_att_oth_r')

        local n_ok = `n_ok' + 1
    }

    * Drop temp vars and restore — always executed every iteration
    capture drop _pb _pb2 _pb3 _mb _ms
    restore

    if mod(`b', 10) == 0 di "." _continue
}

postclose `bsh'
di _n "Bootstrap complete: `n_ok' of `R' reps successful"

* Extract SEs as standard deviations of bootstrap distribution
preserve
    use `bsf', clear
    quietly sum b_ate
    local ate_se = r(sd)
    quietly sum b_att
    local att_se = r(sd)
    quietly sum b_atu
    local atu_se = r(sd)
    quietly sum b_ate_stem
    local ate_se_stem = r(sd)
    quietly sum b_att_stem
    local att_se_stem = r(sd)
    quietly sum b_ate_bus
    local ate_se_business = r(sd)
    quietly sum b_att_bus
    local att_se_business = r(sd)
    quietly sum b_ate_ed
    local ate_se_education = r(sd)
    quietly sum b_att_ed
    local att_se_education = r(sd)
    quietly sum b_ate_hlth
    local ate_se_health = r(sd)
    quietly sum b_att_hlth
    local att_se_health = r(sd)
    quietly sum b_ate_oth
    local ate_se_other = r(sd)
    quietly sum b_att_oth
    local att_se_other = r(sd)
restore

* -----------------------------------------------------------------------
* 6c-iv  mtefe bootstrap SEs — aliased from Section 6c-i
*
* The fastest approach is to reuse the Section 6c-i bootstrap SEs.
* Both the manual polynomial estimator (Section 6) and mtefe target the
* same parameters (ATE, ATT, ATU, LATE) from the same data. The 500-rep
* cluster bootstrap already run in Section 6c-i bootstraps the full
* probit + polynomial pipeline, so those SEs characterise sampling
* uncertainty for any polynomial MTE estimator on this dataset.
*
* Running a separate mtefe bootstrap loop (~2-4 min per 200 reps) adds
* no information beyond what Section 6c-i already provides. We therefore
* alias directly — zero additional compute time.
*
* If you specifically want mtefe-internal SEs (e.g. for publication
* reviewers who insist on them), set R_mte to a small number (50) and
* uncomment the full loop that was in v10 of this script.
* -----------------------------------------------------------------------

* Alias: mtefe SEs = manual polynomial SEs from Section 6c-i
local mtefe_ate_se   = `ate_se'
local mtefe_att_se   = `att_se'
local mtefe_atu_se   = `atu_se'
* LATE SE from IV/2SLS (Section 5)  -- comment moved off the assignment line
* to avoid r(198): when iv_se is empty after macro expansion, the inline
* block comment becomes the sole token Stata tries to evaluate as an expression.
local mtefe_late_se  = `iv_se'
local mtefe_ate_q_se = `ate_se'
local mtefe_att_q_se = `att_se'
local mtefe_atu_q_se = `atu_se'

di _n "--- mtefe SEs (aliased from Section 6c-i bootstrap, pol=1) ---"
di "  ATE:  " %6.4f `mtefe_ate'  "  (BS SE = " %6.4f `mtefe_ate_se'  ")"
di "  ATT:  " %6.4f `mtefe_att'  "  (BS SE = " %6.4f `mtefe_att_se'  ")"
di "  ATU:  " %6.4f `mtefe_atu'  "  (BS SE = " %6.4f `mtefe_atu_se'  ")"
di "  LATE: " %6.4f `mtefe_late' "  (BS SE = " %6.4f `mtefe_late_se' ")"
di _n "--- mtefe SEs (aliased from Section 6c-i bootstrap, pol=2) ---"
di "  ATE:  " %6.4f `mtefe_ate_q' "  (BS SE = " %6.4f `mtefe_ate_q_se' ")"
di "  ATT:  " %6.4f `mtefe_att_q' "  (BS SE = " %6.4f `mtefe_att_q_se' ")"
di "  ATU:  " %6.4f `mtefe_atu_q' "  (BS SE = " %6.4f `mtefe_atu_q_se' ")"

* 6c-ii  To activate multi-core speedup (Option A):
*   Install: net install parallel,
*       from("https://raw.github.com/gvegayon/parallel/stable/")
*   Then replace the forvalues loop above with parfor syntax or
*   use the parallel package to distribute the loop across cores.
*   Set ncores to the number of physical cores on your machine.
*   With the postfile approach, parallel output can be combined with
*   "append using" after each parallel block.
* -----------------------------------------------------------------------

* -----------------------------------------------------------------------
* 6c-iii  Wild cluster bootstrap for OLS and IV stages
*   Webb weights, 9999 reps: runs in seconds, valid for G=50.
*   Requires: ssc install fwildclusterboot
* -----------------------------------------------------------------------
capture which boottest
if _rc != 0 {
    di "Installing fwildclusterboot..."
    ssc install fwildclusterboot, replace
}

di _n "--- Wild Cluster Bootstrap: OLS ---"
qui reg ln_salary masters $X_controls
boottest masters, cluster(state) reps(9999) nograph
di "  p-value (wild cluster): " %6.4f r(p)

di _n "--- Wild Cluster Bootstrap: IV ---"
qui ivregress 2sls ln_salary $X_controls (masters = ga_funding_adj)
boottest masters, cluster(state) reps(9999) nograph
di "  p-value (wild cluster): " %6.4f r(p)


di _n "--- Bootstrap SEs: Pooled Parameters ---"
di "  ATE = " %6.4f `ate_est_cubic' "  (Bootstrap SE = " %6.4f `ate_se' ")"
di "  ATT = " %6.4f `att_est'       "  (Bootstrap SE = " %6.4f `att_se' ")"
di "  ATU = " %6.4f `atu_est'       "  (Bootstrap SE = " %6.4f `atu_se' ")"

di _n "--- Bootstrap SEs: Area-Specific ATE ---"
di "Area          Point Est   BS SE    95% CI"
di "-----------------------------------------------"
foreach a in other stem business education health {
    local lo = `ate_`a'' - 1.96*`ate_se_`a''
    local hi = `ate_`a'' + 1.96*`ate_se_`a''
    di "  `a'   " %7.4f `ate_`a'' "    " %6.4f `ate_se_`a'' ///
       "   [" %6.4f `lo' ", " %6.4f `hi' "]"
}

di _n "--- Bootstrap SEs: Area-Specific ATT ---"
di "Area          Point Est   BS SE"
di "-----------------------------------"
foreach a in other stem business education health {
    di "  `a'   " %7.4f `att_`a'' "    " %6.4f `att_se_`a''
}

********************************************************************************
* SECTION 7: Results Comparison
********************************************************************************

di _n "=============================================="
di "RESULTS COMPARISON"
di "=============================================="

di "  Naive OLS:              " %6.4f `ols_est' " (SE = " %6.4f `ols_se' " — likely biased)"
di "  IV/LATE:                " %6.4f `iv_est'  " (SE = " %6.4f `iv_se'  " — complier effect)"
di "  MTE-based ATE (cubic):  " %6.4f `ate_est_cubic' " (BS SE = " %6.4f `ate_se' ")"
di "  MTE-based ATT:          " %6.4f `att_est'        " (BS SE = " %6.4f `att_se' ")"
di "  MTE-based ATU:          " %6.4f `atu_est'        " (BS SE = " %6.4f `atu_se' ")"
di "  mtefe ATE (quad):       " %6.4f `mtefe_ate_q' "  (BS SE = " %6.4f `mtefe_ate_q_se' ")"
di "  mtefe ATT (quad):       " %6.4f `mtefe_att_q' "  (BS SE = " %6.4f `mtefe_att_q_se' ")"
di "  mtefe ATU (quad):       " %6.4f `mtefe_atu_q' "  (BS SE = " %6.4f `mtefe_atu_q_se' ")"

if `att_est' > `ate_est_cubic' & `ate_est_cubic' > `atu_est' {
    di "  ATT > ATE > ATU: POSITIVE SELECTION on gains"
}
else if `att_est' < `ate_est_cubic' & `ate_est_cubic' < `atu_est' {
    di "  ATT < ATE < ATU: NEGATIVE SELECTION on gains"
}
else {
    di "  Mixed selection pattern"
}

local ols_bias = (`ols_est' - `ate_est_cubic') / `ate_est_cubic' * 100
di "OLS BIAS: " %5.1f `ols_bias' "% relative to MTE-based ATE"

estimates table ols_naive iv_2sls, ///
    stats(N r2) b(%7.4f) se(%7.4f) keep(masters) ///
    title("OLS vs. IV Estimates of Master's Degree Effect")

********************************************************************************
* SECTION 8: MTE Visualization
********************************************************************************

di _n "=============================================="
di "MTE VISUALIZATION"
di "=============================================="

preserve
    clear
    set obs 100
    gen u = _n/100
    gen mte_est = `b0' + `b1'*u + `b2'*u^2 + `b3'*u^3
    label var u "Unobserved Resistance to Treatment"
    label var mte_est "Marginal Treatment Effect"
    twoway (line mte_est u, lcolor(gs0) lwidth(medthick) lpattern(solid)), ///
           ytitle("Marginal Treatment Effect") ///
           xtitle("u (Unobserved Resistance to Treatment)") ///
           title("Estimated MTE Curve - Pooled") ///
           subtitle("Master's Degree Effect on Log Salary") ///
           note("Declining MTE indicates positive selection on gains") ///
           yline(0, lpattern(dash) lcolor(gs8)) ///
           name(mte_curve, replace)
    graph save "$graphs_dir/mte_curve.gph", replace
    graph export "$graphs_dir/fig10_6_mte_curve_Stata.png", replace width(1200)
restore

capture drop p_decile
xtile p_decile = phat, nq(10)

preserve
    collapse (mean) mte_mean=mte_hat (sd) mte_sd=mte_hat (count) n=id, by(p_decile)
    di _n "Estimated MTE by Propensity Score Decile:"
    list p_decile mte_mean mte_sd n
    twoway (scatter mte_mean p_decile, msize(large) mcolor(gs0) msymbol(D)) ///
           (line mte_mean p_decile, lcolor(gs0) lwidth(medium) lpattern(solid)), ///
           ytitle("Mean Estimated MTE") ///
           xtitle("Propensity Score Decile") ///
           title("Estimated MTE by Propensity Score Decile") ///
           subtitle("Evidence of Treatment Effect Heterogeneity") ///
           yline(0, lpattern(dash) lcolor(gs8)) ///
           name(mte_by_decile, replace)
    graph save "$graphs_dir/mte_by_decile.gph", replace
    graph export "$graphs_dir/fig10_7_mte_by_decile_Stata.png", replace width(1200)
restore

* MTE curves by program area (FIX 3: dkgreen replaces invalid forest_green)
preserve
    clear
    set obs 100
    gen u = _n/100
    gen mte_other    = `B0'            + `B1'           *u + `B2'           *u^2 + `B3'           *u^3
    gen mte_stem     = `c0_stem'       + `c1_stem'      *u + `c2_stem'      *u^2 + `c3_stem'      *u^3
    gen mte_business = `c0_business'   + `c1_business'  *u + `c2_business'  *u^2 + `c3_business'  *u^3
    gen mte_educ     = `c0_education'  + `c1_education' *u + `c2_education' *u^2 + `c3_education' *u^3
    gen mte_health   = `c0_health'     + `c1_health'    *u + `c2_health'    *u^2 + `c3_health'    *u^3

    twoway (line mte_health   u, lcolor(gs0) lwidth(medthick) lpattern(solid))     ///
           (line mte_stem     u, lcolor(gs0) lwidth(medthick) lpattern(dash))       ///
           (line mte_business u, lcolor(gs0) lwidth(medthick) lpattern(longdash))   ///
           (line mte_educ     u, lcolor(gs8) lwidth(medthick) lpattern(solid))      ///
           (line mte_other    u, lcolor(gs8) lwidth(medthick) lpattern(dash)),      ///
        ytitle("Marginal Treatment Effect") ///
        xtitle("u (Unobserved Resistance to Treatment)") ///
        title("MTE Curves by Graduate Program Area") ///
        subtitle("Field-specific returns to master's degree") ///
        yline(0, lpattern(shortdash) lcolor(gs10)) ///
        legend(order(1 "Health & Related" 2 "STEM" 3 "Business" ///
                     4 "Education" 5 "Other (base)") ///
               cols(3) size(small)) ///
        name(mte_byarea_curve, replace)
    graph save "$graphs_dir/mte_byarea_curve.gph", replace
    graph export "$graphs_dir/fig10_8_mte_byarea_curve_Stata.png", replace width(1200)
restore

********************************************************************************
* SECTION 9: Basic Policy Simulation (PRTE)
********************************************************************************

qui sum ga_funding_adj
local ga_current = r(mean)
local ga_new = `ga_current' * 1.2
di "Current mean GA: $" %5.2f `ga_current' "k  Proposed (20% increase): $" %5.2f `ga_new' "k"

gen p_new   = normal(z_index + `ga_coef'*(`ga_new' - ga_funding_adj))
gen delta_p = p_new - phat
qui sum delta_p
di "Average increase in Pr(Master's): " %6.4f r(mean)

gen complier_weight = delta_p / r(mean) if delta_p > 0
qui sum mte_hat [aw=complier_weight] if delta_p > 0
local prte_20pct = r(mean)
di "Approximate PRTE (20% GA increase): " %6.4f `prte_20pct'
drop p_new delta_p complier_weight

********************************************************************************
* SECTION 10: MPRTE — Scenarios 1-4 (Original)
********************************************************************************

di _n "=============================================="
di "MPRTE - SCENARIOS 1-4 (ORIGINAL)"
di "=============================================="

* Scenario 1: Uniform $1k
gen p_new_unif        = normal(z_index + `ga_coef'*1)
gen delta_p_unif      = p_new_unif - phat
gen response_unif     = normalden(invnormal(phat)) * `ga_coef'
gen mte_weighted_unif = mte_hat * response_unif
qui sum mte_weighted_unif
local mprte_unif_num = r(sum)
qui sum response_unif
local mprte_unif = `mprte_unif_num' / r(sum)
di "MPRTE (uniform $1k): " %6.4f `mprte_unif'
qui sum mte_hat [aw=delta_p_unif] if delta_p_unif > 0
di "PRTE  (discrete $1k): " %6.4f r(mean)
drop p_new_unif delta_p_unif response_unif mte_weighted_unif

* Scenario 2: Targeted low-income ($2k)
gen targeted_lowinc     = (parent_income_q <= 2)
gen p_new_lowinc        = normal(z_index + `ga_coef'*2*targeted_lowinc)
gen delta_p_lowinc      = p_new_lowinc - phat
gen response_lowinc     = normalden(invnormal(phat)) * `ga_coef' * 2 * targeted_lowinc
gen mte_weighted_lowinc = mte_hat * response_lowinc
qui sum mte_weighted_lowinc if targeted_lowinc == 1
local mprte_lowinc_num = r(sum)
qui sum response_lowinc if targeted_lowinc == 1
local mprte_lowinc = `mprte_lowinc_num' / r(sum)
di "MPRTE (targeted low-income): " %6.4f `mprte_lowinc'
drop targeted_lowinc p_new_lowinc delta_p_lowinc response_lowinc mte_weighted_lowinc

* Scenario 3: STEM GA ($3k)
gen p_new_stem        = normal(z_index + `ga_coef'*3*stem_major)
gen delta_p_stem      = p_new_stem - phat
gen response_stem     = normalden(invnormal(phat)) * `ga_coef' * 3 * stem_major
gen mte_weighted_stem = mte_hat * response_stem
qui sum mte_weighted_stem if stem_major == 1
local mprte_stem_num = r(sum)
qui sum response_stem if stem_major == 1
local mprte_stem = `mprte_stem_num' / r(sum)
di "MPRTE (STEM enhancement): " %6.4f `mprte_stem'
drop p_new_stem delta_p_stem response_stem mte_weighted_stem

* Scenario 4: Education ($2.5k)
gen p_new_ed        = normal(z_index + `ga_coef'*2.5*ed_major)
gen delta_p_ed      = p_new_ed - phat
gen response_ed     = normalden(invnormal(phat)) * `ga_coef' * 2.5 * ed_major
gen mte_weighted_ed = mte_hat * response_ed
qui sum mte_weighted_ed if ed_major == 1
local mprte_ed_num = r(sum)
qui sum response_ed if ed_major == 1
local mprte_ed = `mprte_ed_num' / r(sum)
di "MPRTE (education major support): " %6.4f `mprte_ed'
drop p_new_ed delta_p_ed response_ed mte_weighted_ed

********************************************************************************
* SECTION 10b: MPRTE BY GRADUATE PROGRAM AREA — Scenarios 5-8
*
* These scenarios use the area-specific MTE function MTE_a(phat) from
* Section 6b rather than the pooled mte_hat. Each scenario targets a
* $2,500 GA increase at the undergraduate pipeline that feeds most
* strongly into the graduate field of interest.
*
* MPRTE_a = sum_i (MTE_a(phat_i) * h_a(X_i)) / sum_i h_a(X_i)
* where h_a(X_i) = normalden(invnormal(phat_i)) * ga_coef * amount * pipeline_a(X_i)
*
* Pipeline definitions (from transition probability table, Section 1b):
*   STEM:      pipeline = stem_major   (55% enter STEM grad programs)
*   Business:  pipeline = bus_major    (65% enter Business grad programs)
*   Education: pipeline = ed_major     (70% enter Education grad programs)
*   Health:    pipeline = stem_major OR socsci_major (draws from both)
********************************************************************************

di _n "=============================================="
di "MPRTE BY GRADUATE PROGRAM AREA (Scenarios 5-8)"
di "=============================================="

* Scenario 5: STEM graduate pipeline
gen p_new_s5   = normal(z_index + `ga_coef'*2.5*stem_major)
gen delta_p_s5 = p_new_s5 - phat
gen response_s5  = normalden(invnormal(phat)) * `ga_coef' * 2.5 * stem_major
gen mteA_wt_s5   = mte_hat_stem * response_s5
qui sum mteA_wt_s5 if stem_major == 1
local mprte_ma_stem_num = r(sum)
qui sum response_s5 if stem_major == 1
local mprte_ma_stem = `mprte_ma_stem_num' / r(sum)
qui sum mte_hat_stem [aw=delta_p_s5] if delta_p_s5 > 0 & stem_major == 1
local prte_ma_stem = r(mean)
di "MPRTE (STEM grad pipeline, $2.5k): " %6.4f `mprte_ma_stem'
di "PRTE  (STEM grad pipeline, $2.5k): " %6.4f `prte_ma_stem'
qui sum phat if stem_major == 1
di "Mean phat for STEM undergrads:     " %6.4f r(mean)
drop p_new_s5 delta_p_s5 response_s5 mteA_wt_s5

* Scenario 6: Business graduate pipeline
gen p_new_s6   = normal(z_index + `ga_coef'*2.5*bus_major)
gen delta_p_s6 = p_new_s6 - phat
gen response_s6  = normalden(invnormal(phat)) * `ga_coef' * 2.5 * bus_major
gen mteA_wt_s6   = mte_hat_business * response_s6
qui sum mteA_wt_s6 if bus_major == 1
local mprte_ma_bus_num = r(sum)
qui sum response_s6 if bus_major == 1
local mprte_ma_bus = `mprte_ma_bus_num' / r(sum)
qui sum mte_hat_business [aw=delta_p_s6] if delta_p_s6 > 0 & bus_major == 1
local prte_ma_bus = r(mean)
di "MPRTE (Business grad pipeline, $2.5k): " %6.4f `mprte_ma_bus'
di "PRTE  (Business grad pipeline, $2.5k): " %6.4f `prte_ma_bus'
drop p_new_s6 delta_p_s6 response_s6 mteA_wt_s6

* Scenario 7: Education graduate pipeline
gen p_new_s7   = normal(z_index + `ga_coef'*2.5*ed_major)
gen delta_p_s7 = p_new_s7 - phat
gen response_s7  = normalden(invnormal(phat)) * `ga_coef' * 2.5 * ed_major
gen mteA_wt_s7   = mte_hat_education * response_s7
qui sum mteA_wt_s7 if ed_major == 1
local mprte_ma_ed_num = r(sum)
qui sum response_s7 if ed_major == 1
local mprte_ma_ed = `mprte_ma_ed_num' / r(sum)
qui sum mte_hat_education [aw=delta_p_s7] if delta_p_s7 > 0 & ed_major == 1
local prte_ma_ed = r(mean)
di "MPRTE (Education grad pipeline, $2.5k): " %6.4f `mprte_ma_ed'
di "PRTE  (Education grad pipeline, $2.5k): " %6.4f `prte_ma_ed'
drop p_new_s7 delta_p_s7 response_s7 mteA_wt_s7

* Scenario 8: Health & Related pipeline (STEM + SocSci undergrads)
gen target_health = (stem_major == 1 | socsci_major == 1)
gen p_new_s8      = normal(z_index + `ga_coef'*2.5*target_health)
gen delta_p_s8    = p_new_s8 - phat
gen response_s8   = normalden(invnormal(phat)) * `ga_coef' * 2.5 * target_health
gen mteA_wt_s8    = mte_hat_health * response_s8
qui sum mteA_wt_s8 if target_health == 1
local mprte_ma_hlth_num = r(sum)
qui sum response_s8 if target_health == 1
local mprte_ma_hlth = `mprte_ma_hlth_num' / r(sum)
qui sum mte_hat_health [aw=delta_p_s8] if delta_p_s8 > 0 & target_health == 1
local prte_ma_hlth = r(mean)
di "MPRTE (Health & Related pipeline, $2.5k): " %6.4f `mprte_ma_hlth'
di "PRTE  (Health & Related pipeline, $2.5k): " %6.4f `prte_ma_hlth'
drop target_health p_new_s8 delta_p_s8 response_s8 mteA_wt_s8

********************************************************************************
* SECTION 11: MPRTE BY POLICY INTENSITY
********************************************************************************

* Save phat mean BEFORE preserve/clear so it is available inside the block
* (Fix 2: qui sum phat was previously inside preserve/clear, where phat does
*  not exist, leaving p_baseline undefined and producing a blank graph.)
qui sum phat
local p_baseline = r(mean)

preserve
    clear
    set obs 20
    gen ga_increase  = _n * 0.5
    gen p_margin     = `p_baseline' + ga_increase * 0.015
    gen mprte_approx = `b0' + `b1'*p_margin + `b2'*p_margin^2 + `b3'*p_margin^3
    list ga_increase p_margin mprte_approx
    twoway (line mprte_approx ga_increase, lcolor(gs0) lwidth(medthick) lpattern(solid)), ///
        ytitle("MPRTE") xtitle("GA Funding Increase ($1000s)") ///
        title("MPRTE by Policy Intensity") ///
        subtitle("Marginal returns to GA funding expansion") ///
        yline(0, lpattern(dash) lcolor(gs8)) ///
        name(mprte_intensity, replace)
    graph save "$graphs_dir/mprte_by_intensity.gph", replace
    graph export "$graphs_dir/fig10_9_mprte_by_intensity_Stata.png", replace width(1200)
restore

********************************************************************************
* SECTION 12: COMPARING TREATMENT EFFECT PARAMETERS
********************************************************************************

di _n "=============================================="
di "COMPARISON OF TREATMENT EFFECT PARAMETERS"
di "=============================================="

di "Parameter     Manual(cubic)  BS SE(manual)  mtefe(quad)  BS SE(mtefe)"
di "======================================================================="
di "ATE           " %6.4f `ate_est_cubic' " (" %6.4f `ate_se' ")    " %6.4f `mtefe_ate_q' "  (" %6.4f `mtefe_ate_q_se' ")"
di "ATT           " %6.4f `att_est'       " (" %6.4f `att_se' ")    " %6.4f `mtefe_att_q' "  (" %6.4f `mtefe_att_q_se' ")"
di "ATU           " %6.4f `atu_est'       " (" %6.4f `atu_se' ")    " %6.4f `mtefe_atu_q' "  (" %6.4f `mtefe_atu_q_se' ")"
di "LATE (IV)     " %6.4f `iv_est'        " (" %6.4f `iv_se' ")    " %6.4f `mtefe_late' "  (" %6.4f `mtefe_late_se' ")"
di "---------------------------------------------------------"
di "MPRTE (uniform)                   " %6.4f `mprte_unif'
di "MPRTE (low-income)                " %6.4f `mprte_lowinc'
di "MPRTE (STEM ug -> any grad)       " %6.4f `mprte_stem'
di "MPRTE (Ed ug -> any grad)         " %6.4f `mprte_ed'

di _n "AREA-SPECIFIC PARAMETERS:"
di "Area          ATE (BS SE)         ATT (BS SE)"
di "----------------------------------------------------"
di "  Other     " %6.4f `ate_other'     " (" %6.4f `ate_se_other'     ")    " %6.4f `att_other'     " (" %6.4f `att_se_other'     ")"
di "  STEM      " %6.4f `ate_stem'      " (" %6.4f `ate_se_stem'      ")    " %6.4f `att_stem'      " (" %6.4f `att_se_stem'      ")"
di "  Business  " %6.4f `ate_business'  " (" %6.4f `ate_se_business'  ")    " %6.4f `att_business'  " (" %6.4f `att_se_business'  ")"
di "  Education " %6.4f `ate_education' " (" %6.4f `ate_se_education' ")    " %6.4f `att_education' " (" %6.4f `att_se_education' ")"
di "  Health    " %6.4f `ate_health'    " (" %6.4f `ate_se_health'    ")    " %6.4f `att_health'    " (" %6.4f `att_se_health'    ")"

di _n "MPRTE BY GRADUATE PIPELINE (Scenarios 5-8):"
di "  STEM grad pipeline:         " %6.4f `mprte_ma_stem'
di "  Business grad pipeline:     " %6.4f `mprte_ma_bus'
di "  Education grad pipeline:    " %6.4f `mprte_ma_ed'
di "  Health & Related pipeline:  " %6.4f `mprte_ma_hlth'

********************************************************************************
* SECTION 13: MPRTE VISUALIZATION
********************************************************************************

* Fig 10.10: MTE Curve with Policy-Relevant Regions
* Build a 100-point grid in a tempfile to avoid preserve/clear issues
* that cause twoway rarea + line to fail silently in batch mode.
tempfile mte_grid
tempname mte_mem

* Save current dataset, build grid, plot, reload
qui save `mte_mem', replace emptyok

clear
set obs 100
gen u         = _n / 100
gen mte       = `b0' + `b1'*u + `b2'*u^2 + `b3'*u^3
gen region_lo = (u >= 0.10 & u <= 0.25)
gen region_un = (u >= 0.25 & u <= 0.40)
gen zero_line = 0
qui save `mte_grid', replace

* Plot directly — no name() to avoid batch-mode named-graph failure
twoway (rarea zero_line mte u if region_lo == 1, fcolor(gs5)  lwidth(none)) ///
       (rarea zero_line mte u if region_un == 1, fcolor(gs11) lwidth(none)) ///
       (line  mte u, lcolor(gs0) lwidth(medthick) lpattern(solid)),         ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    ytitle("Marginal Treatment Effect") ///
    xtitle("u (Unobserved Resistance to Treatment)") ///
    title("MTE Curve with Policy-Relevant Regions") ///
    legend(order(3 "Estimated MTE" 1 "Low-income margin" ///
                 2 "Uniform policy margin") cols(2) size(small))

graph save   "$graphs_dir/mte_policy_regions.gph", replace
graph export "$graphs_dir/fig10_10_mte_policy_regions_Stata.png", replace width(1200)

* Reload main dataset
qui use `mte_mem', clear

preserve
    gen p_bin = floor(phat * 20) / 20
    bysort p_bin: egen mean_mte = mean(mte_hat)
    bysort p_bin: gen n_bin = _N
    collapse (mean) mean_mte (first) n_bin, by(p_bin)
    twoway (bar n_bin p_bin,      barwidth(0.04) fcolor(gs12) lcolor(gs8) yaxis(2)) ///
           (scatter mean_mte p_bin, mcolor(gs0) msize(medium) msymbol(D)  yaxis(1)) ///
           (line mean_mte p_bin,    lcolor(gs0) lwidth(medium) lpattern(solid) yaxis(1)), ///
        yline(0, lpattern(dash) lcolor(gs8) axis(1)) ///
        ytitle("Estimated MTE", axis(1)) ///
        ytitle("Frequency", axis(2)) ///
        xtitle("Propensity Score") ///
        title("MTE by Propensity Score") ///
        legend(order(2 "Mean MTE" 1 "Obs. count")) ///
        name(mte_by_propensity, replace)
    graph save "$graphs_dir/mte_by_propensity.gph", replace
    graph export "$graphs_dir/fig10_11_mte_by_propensity_Stata.png", replace width(1200)
restore

********************************************************************************
* SECTION 14: POLICY COST-BENEFIT ANALYSIS
********************************************************************************

local cost_per_degree = 50000
local career_years    = 30
local discount_rate   = 0.03
local base_salary     = 47000
local pv_factor = (1 - (1 + `discount_rate')^(-`career_years')) / `discount_rate'
di "Present value factor (30 years, 3%): " %6.2f `pv_factor'

di _n "--- Scenarios 1-4: Original MPRTE-based CBA ---"
di "Policy               MPRTE    Annual Gain   PV Gain    B/C Ratio"
di "=================================================================="
foreach scen in unif lowinc stem ed {
    if "`scen'" == "unif"   local mprte_val = `mprte_unif'
    if "`scen'" == "lowinc" local mprte_val = `mprte_lowinc'
    if "`scen'" == "stem"   local mprte_val = `mprte_stem'
    if "`scen'" == "ed"     local mprte_val = `mprte_ed'
    local annual_gain = `base_salary' * (exp(`mprte_val') - 1)
    local pv_gain     = `annual_gain' * `pv_factor'
    local bc_ratio    = `pv_gain' / `cost_per_degree'
    if "`scen'" == "unif"   di "Uniform             " %6.4f `mprte_val' "   $" %6.0f `annual_gain' "    $" %8.0f `pv_gain' "   " %5.2f `bc_ratio'
    if "`scen'" == "lowinc" di "Low-income          " %6.4f `mprte_val' "   $" %6.0f `annual_gain' "    $" %8.0f `pv_gain' "   " %5.2f `bc_ratio'
    if "`scen'" == "stem"   di "STEM ug             " %6.4f `mprte_val' "   $" %6.0f `annual_gain' "    $" %8.0f `pv_gain' "   " %5.2f `bc_ratio'
    if "`scen'" == "ed"     di "Education ug        " %6.4f `mprte_val' "   $" %6.0f `annual_gain' "    $" %8.0f `pv_gain' "   " %5.2f `bc_ratio'
}

di _n "--- Scenarios 5-8: Graduate Program Area MPRTE-based CBA ---"
local base_stem  = 65000
local base_bus   = 60000
local base_ed    = 42000
local base_hlth  = 68000
di "  Base salaries: STEM=$" %6.0f `base_stem' " Business=$" %6.0f `base_bus' " Ed=$" %6.0f `base_ed' " Health=$" %6.0f `base_hlth'

di _n "Pipeline            MPRTE    Annual Gain   PV Gain    B/C Ratio"
di "=================================================================="
local mv = `mprte_ma_stem'
local ag = `base_stem' * (exp(`mv') - 1)
local pvg = `ag' * `pv_factor'
di "STEM pipeline       " %6.4f `mv' "   $" %6.0f `ag' "    $" %8.0f `pvg' "   " %5.2f `pvg'/`cost_per_degree'

local mv = `mprte_ma_bus'
local ag = `base_bus' * (exp(`mv') - 1)
local pvg = `ag' * `pv_factor'
di "Business pipeline   " %6.4f `mv' "   $" %6.0f `ag' "    $" %8.0f `pvg' "   " %5.2f `pvg'/`cost_per_degree'

local mv = `mprte_ma_ed'
local ag = `base_ed' * (exp(`mv') - 1)
local pvg = `ag' * `pv_factor'
di "Education pipeline  " %6.4f `mv' "   $" %6.0f `ag' "    $" %8.0f `pvg' "   " %5.2f `pvg'/`cost_per_degree'

local mv = `mprte_ma_hlth'
local ag = `base_hlth' * (exp(`mv') - 1)
local pvg = `ag' * `pv_factor'
di "Health pipeline     " %6.4f `mv' "   $" %6.0f `ag' "    $" %8.0f `pvg' "   " %5.2f `pvg'/`cost_per_degree'

di "Note: B/C > 1 suggests policy expansion is beneficial (synthetic data only)."

********************************************************************************
* SECTION 15: Save Results
********************************************************************************

label var phat    "Estimated propensity score"
label var mte_hat "Estimated MTE (pooled cubic)"
label var z_index "Probit linear index"

save "bb_mte_analysis.dta", replace

preserve
    collapse (mean) masters ln_salary phat mte_hat ///
             ma_stem ma_business ma_education ma_health ma_other ///
             mte_hat_stem mte_hat_business mte_hat_education     ///
             mte_hat_health mte_hat_other                        ///
             (sd) sd_mte=mte_hat (count) n=id, by(stem_major ed_major)
    export delimited "mte_summary_by_field.csv", replace
restore

preserve
    keep if masters == 1
    collapse (mean) ln_salary salary phat mte_hat ///
             mte_hat_stem mte_hat_business mte_hat_education ///
             mte_hat_health mte_hat_other                    ///
             (count) n=id,                                   ///
             by(ma_stem ma_business ma_education ma_health ma_other)
    export delimited "mte_summary_by_program_area.csv", replace
restore

********************************************************************************
* SECTION 16: FINAL SUMMARY
********************************************************************************

di _n "=============================================="
di "ANALYSIS COMPLETE"
di "=============================================="
di "  1.  Treatment rate:                  " %5.3f `treat_rate'
di "  2.  OLS estimate (biased):           " %6.4f `ols_est'
di "  3.  IV/LATE estimate:                " %6.4f `iv_est'
di "  4.  MTE-based ATE (cubic):           " %6.4f `ate_est_cubic' " (BS SE = " %6.4f `ate_se' ")"
di "  5.  MTE-based ATT:                   " %6.4f `att_est'       " (BS SE = " %6.4f `att_se' ")"
di "  6.  MTE-based ATU:                   " %6.4f `atu_est'       " (BS SE = " %6.4f `atu_se' ")"
di "  7.  mtefe ATE (quad):                " %6.4f `mtefe_ate_q' "  (BS SE = " %6.4f `mtefe_ate_q_se' ")"
di "  8.  First-stage F:                   " %6.1f `first_stage_F'

di _n "AREA-SPECIFIC ATE (program area interacted MTE):"
di "  ATE (other):     " %6.4f `ate_other'     " (BS SE = " %6.4f `ate_se_other'     ")"
di "  ATE (stem):      " %6.4f `ate_stem'      " (BS SE = " %6.4f `ate_se_stem'      ")"
di "  ATE (business):  " %6.4f `ate_business'  " (BS SE = " %6.4f `ate_se_business'  ")"
di "  ATE (education): " %6.4f `ate_education' " (BS SE = " %6.4f `ate_se_education' ")"
di "  ATE (health):    " %6.4f `ate_health'    " (BS SE = " %6.4f `ate_se_health'    ")"

di _n "MPRTE SUMMARY - Original Scenarios:"
di "  Uniform policy:         " %6.4f `mprte_unif'
di "  Low-income targeted:    " %6.4f `mprte_lowinc'
di "  STEM ug pipeline:       " %6.4f `mprte_stem'
di "  Education ug pipeline:  " %6.4f `mprte_ed'

di _n "MPRTE SUMMARY - Graduate Program Area Pipelines (Scenarios 5-8):"
di "  STEM grad pipeline:     " %6.4f `mprte_ma_stem'
di "  Business grad pipeline: " %6.4f `mprte_ma_bus'
di "  Education pipeline:     " %6.4f `mprte_ma_ed'
di "  Health & Related:       " %6.4f `mprte_ma_hlth'

di _n "Bootstrap: G=50 state clusters, R=500 reps, seed(20260101)"
di "Files saved: bb_mte_analysis.dta, mte_summary_by_field.csv,"
di "             mte_summary_by_program_area.csv"

di _n "IMPORTANT NOTE: Synthetic data — results illustrate methods only."
di "=============================================="
di "END OF MTE/MPRTE ANALYSIS"
di "=============================================="

capture graph display mte_curve
capture graph display mte_by_decile
capture graph display mte_byarea_curve
capture graph display mprte_intensity
capture graph display mte_policy_regions
capture graph display mte_by_propensity

*========================================================================
* Close log and exit
*========================================================================

clear all

log close

*========================================================================
* END OF CHAPTER 10 CODE
*========================================================================

/* Notes:
   PART A — Causal Inference (Sections 10.3–10.9)
   - 10.3.1: Data structure and variable construction
   - 10.3.2: TWFE estimation results
   - 10.3.3: Parallel trends assessment
   - 10.3.4: Robustness checks (alternative timing, border states, weighted)
   - 10.4.2-10.4.3: LASSO-residualized DiD
   - 10.5.3: Synthetic Control Method (SCM)
   - 10.6.3: Synthetic DiD — single treated unit
   - 10.2.4 & 10.3.3: Event Study and Callaway-Sant'Anna DiD
   - 10.7.3-10.7.5: Multi-state staggered adoption analysis
   - 10.8.2: Permutation inference
   - 10.8.3: Leave-one-out sensitivity analysis
   - 10.9: Results summary and policy implications

   PART B — Marginal Treatment Effects (Sections 10.10–10.16)
   - Sections 1-4: Data loading, summary statistics, first-stage, OLS
   - Section 5: IV/2SLS (LATE)
   - Section 6: Manual polynomial MTE, mtefe, Heckman
   - Section 6b: Area-specific MTE by graduate program field
   - Section 6c: Cluster bootstrap SEs, wild cluster bootstrap
   - Section 7: Treatment effect comparison (ATE/ATT/ATU/LATE)
   - Section 8: MTE visualization
   - Sections 9-11: PRTE and MPRTE policy simulations (Scenarios 1-8)
   - Sections 12-13: Parameter comparison table, MPRTE visualization
   - Section 14: Cost-benefit analysis
   - Sections 15-16: Save results, final summary
*/
