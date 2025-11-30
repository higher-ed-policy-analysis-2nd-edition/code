*=======================================================
* Chapter 10 - Causal Inference Techniques
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative
* Techniques (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-
* 2nd-edition/tree/main/code/ch10
* Author: Marvin A. Titus
* Date: November 30, 2025
*=======================================================
* Script tested in Stata 19.5
* Compatible with Stata version 19 or later
*=======================================================
* IMPORTANT: Set working directory (customize this for
* your system)
*=======================================================
/* Use a global path to make it easy to update in one place
global ch10data "C:/Users/YourName/Documents/book-materials/ch10/data"
cd "$ch10data"
*/
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
          lpattern(dash) mcolor(navy) lcolor(navy)) ///
       (connected lngenop fy if treat_state == 1, ///
          mcolor(maroon) lcolor(maroon)), ///
  xline(2018, lpattern(dot)) ///
  legend(label(1 "Control States") label(2 "Georgia")) ///
  ytitle("Log Operating Expenses") xtitle("Fiscal Year") ///
  title("Parallel Trends: Treatment vs Control")
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
  fig keep(synth_output) replace

* Calculate treatment effect
use synth_output, clear
gen synth_gap = _Y_treated - _Y_synthetic

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
  yline(0) xlabel(, angle(45)) ///
  ytitle("Effect on Log Operating Expenses") ///
  xtitle("Years Relative to Treatment")

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
csdid_plot, style(rcap)

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
histogram coef, normal

*=======================================================
* Section 10.8.3: Leave-One-Out Analysis
* (Sensitivity Analysis)
*=======================================================

* Return to main data
use "Finance_Data.csv", clear

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

clear all

exit

*=======================================================
* END OF CHAPTER 10 CODE
*=======================================================

/* Notes:
 - Script sections aligned with Chapter 10 outline
 - Section 10.3.1: Data structure and variable construction
 - Section 10.3.2: TWFE estimation results
 - Section 10.3.3: Parallel trends assessment
 - Section 10.3.4: Robustness checks
 - Section 10.4.2-10.4.3: LASSO-residualized DiD
 - Section 10.5.3: SCM application
 - Section 10.6.3: SDID results (single-state)
 - Section 10.7.3-10.7.5: Multi-state staggered analysis
 - Section 10.8.2: Permutation inference
 - Section 10.8.3: Leave-one-out analysis
 - Section 10.9: Results summary and interpretation
 
 Theoretical sections (10.1, 10.2, etc.) covered in chapter text
 This script provides empirical implementation
*/
