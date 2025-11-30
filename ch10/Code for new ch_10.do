*=======================================================
* Chapter 10 - Causal Inference Techniques
* Complete Stata Code
* Higher Education Policy Analysis Using Quantitative
* Techniques (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis- * 2nd-edition/tree/main/code/ch9
* Author: Marvin A. Titus
* Date: November 27, 2025
*=======================================================
* Script tested in Stata 19.5
* Compatible with Stata version 19 or later
*=======================================================
* IMPORTANT: Set working directory (customize this for your system)
*=======================================================
/* Use a global path to make it easy to update in one place
global ch9data "C:/Users/YourName/Documents/book-materials/ch9/data"
cd "$ch9data"
*/
*=======================================================
* Causal Inference Techniques
*=======================================================

clear all

/* The reserch question: Did Georgia's system-wide consolidation policy
   (treatment)that was phased in between 2013 and 2018 and fully implemented by
   2018 (start of the "steady-state" post-consolidation regime) have an effect
   on Georgia's higher education general operating expenses?" 
*/
* Access the data using the copy and import delimited commands
copy "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch10/Finance_Data.csv" ///
     "Finance_Data.csv", replace
import delimited "Finance_Data.csv", clear

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

gen post_placebo = (fy >= 2012)
gen did_placebo = treat_state * post_placebo

*===============================================================================
* Section:  Difference-in-Differences (DiD)
*===============================================================================
*create log transformed variables
gen lngenop  = log(general_public_operations)
*gen lnactax  = log(actual_tax_revenue)
gen lntotsup = log(total_state_support)
gen lnfinaid = log(total_financial_aid)
gen lntuifee = log(net_tuition_and_fee_revenue)
gen lnfte    = log(net_fte_enrollment) 

global controls "lntotsup lnfinaid lntuifee lnfte"


// ─── 1. DiD ────────────────────────────────────────────────────
reghdfe lngenop did ///
    $controls, ///
    absorb(fips fy) vce(cluster fips)
	
cap which synth
if _rc ssc install synth, replace

xtset fips fy

xtdidregress (lngenop $controls) (did), group(fips) time(fy)

// ─── 2. LASSO-Residualized DiD ───────────────────────────────────────
cap which lasso
if _rc {
    display as text "LASSO not available, using OLS residualization"
    reg lngenop ///
     $controls  
    predict yhat
    gen resid_y = lngenop - yhat
} 
else {
    display as text "Using LASSO residualization"
    lasso linear lngenop ///
        $controls, selection(adaptive)
    predict yhat
    gen resid_y = lngenop - yhat
}

reghdfe resid_y did, absorb(fips fy) vce(cluster fips)

// Store with consistent naming for summary table
estimates store lasso  // Changed back to 'lasso' for consistency with export script
estimates save "${output}\lasso_results", replace

if "`mode'" == "standalone" {
    esttab lasso using "${output}\lasso_did.rtf", replace ///
        se star(* 0.1 ** 0.05 *** 0.01) ///
        title("LASSO-Residualized DiD") ///
        label compress
}

	

/*==============================================================================
  Section:  Synthetic (SCM) Control Methods 
 ===============================================================================
 If needed, ssc install synth2, replace 

To save graphs
 Step 1: Create a temp folder path
local tmpdir = "`c(tmpdir)'/synth_graphs"

 Step 2: Create the directory
mkdir "`tmpdir'"

 Step 3: Set global macro so synth2 can use it
global figures "`tmpdir'"

* (Optional) Check it worked
display "$figures"
*/	
* run synth2	
synth2 lngenop ///
    $controls, ///
    trunit(13) trperiod(2018) ///
    preperiod(2005(1)2017) ///
    postperiod(2018(1)2021) ///
    placebo(unit) ///
    frame(ga_scm) ///
    savegraph("$figures/ga_scm", replace)
	
/* To save all graphs 
* Define base name (must match what you used in savegraph())
local base "$figures/ga_scm"

* Define desired format
local format png   // or: pdf, eps

* List of expected graphs
local suffixes effect balance placebo bias

foreach suf in `suffixes' {
    local gfile = "`base'_`suf'.gph"
    local ofile = "`base'_`suf'.`format'"

    capture noisily {
        graph use "`gfile'", name(gout, replace)
        graph export "`ofile'", name(gout) replace
        display as result "Exported: `ofile'"
    }
}
*/

rename lngenop actual
rename pred·lngenop synth
rename tr·lngenop effect

list fips fy actual synth effect pvalTwo if fips == 13

twoway (line actual fy if fips == 13, lcolor(navy)) ///
       (line synth fy if fips == 13, lcolor(cranberry)), ///
       title("Actual vs Synthetic: Georgia") ///
       xtitle("Fiscal Year") ///
       legend(order(1 "Actual" 2 "Synthetic"))

line effect fy if fips == 13, ///
    yline(0, lpattern(dash)) ///
    title("Treatment Effect Over Time (Georgia)") ///
    xtitle("Fiscal Year") ytitle("Effect (Actual − Synthetic)")

	
// ─── Run SDID Estimation ──────────────────────────────────────────────
cap which sdid
if _rc ssc install sdid, replace

sdid lngenop fips fy did, vce(placebo) ///
     covariates($controls)
	 
// ─── Event study ──────────────────────────────────────────────	 
*----------------------------------------*
* 0. Install necessary packages (only once)
*----------------------------------------*
cap which csdid
if _rc ssc install csdid, replace

cap which drdid
if _rc ssc install drdid, replace

*----------------------------------------*
* Generate gvar = first treatment year for treated units
*----------------------------------------*
gen gyear = .                             // Initialize
replace gyear = 2018 if fips == 13  // Georgia treated in 2018

* Drop any remaining missing outcome
drop if missing(lngenop)

*----------------------------------------*
* Run csdid using dynamic DRIPW estimator
*----------------------------------------*
csdid lngenop $controls, ///
    ivar(fips) ///
    time(fy) ///
    gvar(gyear) ///
    method(dripw) ///
    vce(cluster fips)


* -----------------------------------------------------------------------------


   
   