********************************************************************************
* Synthetic B&B Dataset Generation
* Creates: Example_7_5_3.dta
*
* Application: Effect of Master's Degree on Salary Outcomes
* Instrument: State-Funded Graduate Assistantship (GA) Dollar Amount
*
* Based on synthetic data mirroring NCES B&B Longitudinal Study characteristics
* and higher education finance literature (Titus 2007; Bound, Lovenheim &
* Turner 2010; Zhang 2005; Ehrenberg et al. 2007)
*
* Author: Marvin A. Titus
* Date: November 2025 (revised March 2026)
* Purpose: Generate synthetic dataset for textbook Chapters 7 and 10
*
* REVISION NOTES (March 2026):
* ----------------------------
* Added Section 7b: Master's degree program area categories
*   (Business, Education, Health & Related, STEM, Other)
* Program areas are generated conditional on treatment (masters == 1)
* using multinomial transition probabilities from undergraduate major,
* calibrated to IPEDS Completions Survey field distributions.
* Section 8 (te_masters) updated to use graduate program area as the
* primary source of field-specific return heterogeneity for treated
* observations; undergraduate major retained as counterfactual proxy
* for untreated observations.
* Section 9 expanded with program-area verification tabulations.
*
* NOTE ON SYNTHETIC DATA:
* -----------------------
* This simulation uses synthetic data calibrated to mirror the Baccalaureate
* and Beyond Longitudinal Study (B&B). We use synthetic rather than actual
* B&B data for several reasons:
*
* 1. ACCESS RESTRICTIONS: B&B restricted-use data requires NCES license
* 2. PEDAGOGICAL TRANSPARENCY: Known true parameters allow validation
* 3. REPRODUCIBILITY: Readers can generate identical datasets
* 4. CONTINUITY: Same dataset used in Chapter 10 for MTE analysis
*
* NOTE ON AI-ASSISTED CODE DEVELOPMENT:
* -------------------------------------
* The simulation code was developed with assistance from Claude (Anthropic).
* The author provided specifications based on B&B characteristics and higher
* education finance literature. Claude assisted in translating specifications
* to executable code. The author reviewed, tested, and validated all code.
********************************************************************************
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
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output"
    capture mkdir "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/logs"
    log using ///
        "C:\\Users\\marvi\\Dropbox\\Book\\2nd Edition\\Chapter 10\\Output\\logs\\SyntheticBB_generation.log", ///
        replace text
}
else {
    capture mkdir "Output"
    capture mkdir "Output/logs"
    log using "Output/logs/SyntheticBB_generation.log", replace text
}

di "Synthetic B&B generation log opened: " c(current_date) " " c(current_time)

clear all
set more off
set seed 20251130

* Set sample size
local N = 8000
set obs `N'
gen id = _n

********************************************************************************
* SECTION 1: Demographics
* Based on B&B:08/18 restricted-use data distributions
********************************************************************************

* Female (B&B shows ~57% of bachelor's recipients are female)
gen female = rbinomial(1, 0.57)
label var female "Female (1=Yes)"

* Race/Ethnicity (approximate B&B distributions)
gen race_rand = runiform()
gen byte white = (race_rand < 0.62)
gen byte black = (race_rand >= 0.62 & race_rand < 0.72)
gen byte hispanic = (race_rand >= 0.72 & race_rand < 0.84)
gen byte asian = (race_rand >= 0.84 & race_rand < 0.92)
gen byte other_race = (race_rand >= 0.92)
drop race_rand

label var white "White non-Hispanic"
label var black "Black non-Hispanic"
label var hispanic "Hispanic"
label var asian "Asian"
label var other_race "Other race/ethnicity"

* Age at bachelor's degree
gen age_ba = 22 + rpoisson(1.5)
replace age_ba = 22 if age_ba < 20
replace age_ba = 35 if age_ba > 35

********************************************************************************
* SECTION 2: Family Background
********************************************************************************

gen firstgen = rbinomial(1, 0.35)
gen parent_income_q = 1 + rbinomial(4, 0.55)
gen parent_grad = rbinomial(1, 0.25)

label var firstgen "First-generation college student"
label var parent_income_q "Parent income quintile (1-5)"
label var parent_grad "Parent has graduate degree"

********************************************************************************
* SECTION 3: Academic Background
********************************************************************************

* Undergraduate GPA (beta distribution scaled to 2.0-4.0)
gen ugpa = 2.0 + 1.2*rbeta(5, 3)
replace ugpa = 4.0 if ugpa > 4.0
replace ugpa = 2.0 if ugpa < 2.0

* Major field (mutually exclusive)
gen stem_major = rbinomial(1, 0.25)
gen bus_major = rbinomial(1, 0.20) if stem_major == 0
replace bus_major = 0 if stem_major == 1
gen ed_major = rbinomial(1, 0.15) if stem_major == 0 & bus_major == 0
replace ed_major = 0 if stem_major == 1 | bus_major == 1
gen socsci_major = (stem_major == 0 & bus_major == 0 & ed_major == 0)

label var stem_major "STEM major"
label var bus_major "Business major"
label var ed_major "Education major"
label var socsci_major "Social science/other major"

* Institution characteristics
gen selective_inst = rbinomial(1, 0.30)
gen public_ug = rbinomial(1, 0.65)

label var selective_inst "Attended selective institution"
label var public_ug "Attended public undergraduate institution"

********************************************************************************
* SECTION 4: Labor Market Context
********************************************************************************

gen state_unemp = 4 + 6*rbeta(2, 3)
gen metro = rbinomial(1, 0.75)

label var state_unemp "State unemployment rate"
label var metro "Lives in metropolitan area"

********************************************************************************
* SECTION 5: Generate Instrument - State GA Funding
********************************************************************************

* Assign to states (50 states)
gen state = ceil(50*runiform())

* State-level GA funding (with state fixed effects)
* Generate one state effect per state
bysort state: gen state_effect = rnormal(0, 4) if _n == 1
bysort state: replace state_effect = state_effect[1]

* Base GA funding with state variation
gen ga_funding = 18 + state_effect + rnormal(0, 2)
replace ga_funding = 8 if ga_funding < 8
replace ga_funding = 35 if ga_funding > 35

* Field-adjusted GA funding (STEM gets more, Business gets less)
gen ga_field_mult = 1.3 if stem_major == 1
replace ga_field_mult = 0.9 if bus_major == 1
replace ga_field_mult = 1.1 if ed_major == 1
replace ga_field_mult = 1.0 if socsci_major == 1

gen ga_funding_adj = ga_funding * ga_field_mult
drop state_effect ga_field_mult

label var ga_funding "State GA Funding (base, $1000s)"
label var ga_funding_adj "State GA Funding (field-adjusted, $1000s)"

********************************************************************************
* SECTION 6: Generate Latent Factors (Unobserved)
* These create essential heterogeneity for MTE analysis
********************************************************************************

* Unobserved ability (affects both selection and outcomes)
gen eta_ability = rnormal(0, 1)

* Unobserved taste for education (correlated with ability)
gen eta_taste = 0.3*eta_ability + rnormal(0, 0.9)

* Unobserved productivity (correlated with ability)
gen eta_prod = 0.5*eta_ability + rnormal(0, 0.85)

label var eta_ability "Latent ability factor"
label var eta_taste "Latent taste for education"
label var eta_prod "Latent productivity factor"

********************************************************************************
* SECTION 7: Generate Treatment (Master's Degree)
* Selection equation with essential heterogeneity
********************************************************************************

* Latent index for treatment selection
gen z_masters = ///
    -0.9 + ///                              /* Baseline */
    0.15*female + ///
    0.10*black + ///
    0.05*hispanic + ///
    0.20*asian + ///
    -0.03*(age_ba - 22) + ///
    -0.25*firstgen + ///
    0.08*parent_income_q + ///
    0.35*parent_grad + ///
    0.60*(ugpa - 3.0) + ///
    0.20*stem_major + ///
    -0.15*bus_major + ///
    0.45*ed_major + ///
    0.30*selective_inst + ///
    -0.02*state_unemp + ///
    0.15*metro + ///
    0.06*(ga_funding_adj - 18) + ///       /* INSTRUMENT EFFECT */
    0.40*eta_taste + ///                   /* Unobserved taste for education */
    0.25*eta_ability                       /* Unobserved ability */

* Convert to probability via probit link
gen p_masters = normal(z_masters)

* Generate treatment via threshold crossing
gen u_d = runiform()
gen masters = (p_masters > u_d)

label var z_masters "Latent index for master's selection"
label var p_masters "Propensity Score (true)"
label var u_d "Uniform draw for treatment assignment"
label var masters "Completed Master's Degree (1=Yes)"

********************************************************************************
* SECTION 7b: Master's Degree Program Area
*
* Five mutually exclusive categories (IPEDS CIP-based groupings):
*   ma_business  — Business, Management, Marketing (CIP 52)
*   ma_education — Education (CIP 13)
*   ma_health    — Health Professions & Related (CIP 51)
*   ma_stem      — STEM fields (CIPs 01,03,04,11,14,15,26,27,29,40,41)
*   ma_other     — All remaining fields (Social sciences, Humanities,
*                  Public admin, Arts, Law, etc.)
*
* Generated ONLY for treated observations (masters == 1); set to 0 for
* untreated. Transition probabilities from undergraduate major to graduate
* field are calibrated to IPEDS Completions Survey patterns.
*
* Key empirical regularities encoded:
*   - Education undergrads overwhelmingly pursue Education master's (70%)
*   - Business undergrads strongly favor Business master's (65%)
*   - STEM undergrads split between STEM (55%) and Health & Related (15%)
*   - Social science/other undergrads distribute broadly, with notable
*     shares entering Health, Business, and Education programs
*   - Health & Related master's draws substantially from STEM and social
*     science pipelines (nursing, public health, kinesiology, etc.)
********************************************************************************

* Initialize indicators (byte for efficiency)
gen byte ma_business  = .
gen byte ma_education = .
gen byte ma_health    = .
gen byte ma_stem      = .
gen byte ma_other     = .

* Single uniform draw governs field assignment for treated observations
gen ma_rand = runiform() if masters == 1

*-----------------------------------------------------------------------
* Transition block 1: STEM undergraduates
*   STEM → STEM:      55%  (0.00–0.55)
*   STEM → Health:    15%  (0.55–0.70)
*   STEM → Business:  12%  (0.70–0.82)
*   STEM → Education:  6%  (0.82–0.88)
*   STEM → Other:     12%  (0.88–1.00)
*-----------------------------------------------------------------------
replace ma_stem      = (ma_rand < 0.55)                           if masters==1 & stem_major==1
replace ma_health    = (ma_rand >= 0.55 & ma_rand < 0.70)         if masters==1 & stem_major==1
replace ma_business  = (ma_rand >= 0.70 & ma_rand < 0.82)         if masters==1 & stem_major==1
replace ma_education = (ma_rand >= 0.82 & ma_rand < 0.88)         if masters==1 & stem_major==1
replace ma_other     = (ma_rand >= 0.88)                          if masters==1 & stem_major==1

*-----------------------------------------------------------------------
* Transition block 2: Business undergraduates
*   Bus → Business:   65%  (0.00–0.65)
*   Bus → STEM:        7%  (0.65–0.72)
*   Bus → Health:      8%  (0.72–0.80)
*   Bus → Education:   6%  (0.80–0.86)
*   Bus → Other:      14%  (0.86–1.00)
*-----------------------------------------------------------------------
replace ma_business  = (ma_rand < 0.65)                           if masters==1 & bus_major==1
replace ma_stem      = (ma_rand >= 0.65 & ma_rand < 0.72)         if masters==1 & bus_major==1
replace ma_health    = (ma_rand >= 0.72 & ma_rand < 0.80)         if masters==1 & bus_major==1
replace ma_education = (ma_rand >= 0.80 & ma_rand < 0.86)         if masters==1 & bus_major==1
replace ma_other     = (ma_rand >= 0.86)                          if masters==1 & bus_major==1

*-----------------------------------------------------------------------
* Transition block 3: Education undergraduates
*   Ed → Education:   70%  (0.00–0.70)
*   Ed → Business:    10%  (0.70–0.80)
*   Ed → Health:       8%  (0.80–0.88)
*   Ed → STEM:         5%  (0.88–0.93)
*   Ed → Other:        7%  (0.93–1.00)
*-----------------------------------------------------------------------
replace ma_education = (ma_rand < 0.70)                           if masters==1 & ed_major==1
replace ma_business  = (ma_rand >= 0.70 & ma_rand < 0.80)         if masters==1 & ed_major==1
replace ma_health    = (ma_rand >= 0.80 & ma_rand < 0.88)         if masters==1 & ed_major==1
replace ma_stem      = (ma_rand >= 0.88 & ma_rand < 0.93)         if masters==1 & ed_major==1
replace ma_other     = (ma_rand >= 0.93)                          if masters==1 & ed_major==1

*-----------------------------------------------------------------------
* Transition block 4: Social science / other undergraduates
*   SocSci → Business:   28%  (0.00–0.28)
*   SocSci → Education:  17%  (0.28–0.45)
*   SocSci → Health:     17%  (0.45–0.62)
*   SocSci → STEM:       10%  (0.62–0.72)
*   SocSci → Other:      28%  (0.72–1.00)
*-----------------------------------------------------------------------
replace ma_business  = (ma_rand < 0.28)                           if masters==1 & socsci_major==1
replace ma_education = (ma_rand >= 0.28 & ma_rand < 0.45)         if masters==1 & socsci_major==1
replace ma_health    = (ma_rand >= 0.45 & ma_rand < 0.62)         if masters==1 & socsci_major==1
replace ma_stem      = (ma_rand >= 0.62 & ma_rand < 0.72)         if masters==1 & socsci_major==1
replace ma_other     = (ma_rand >= 0.72)                          if masters==1 & socsci_major==1

drop ma_rand

* Set non-treated to 0 (no graduate program area)
foreach v of varlist ma_business ma_education ma_health ma_stem ma_other {
    replace `v' = 0 if masters == 0
}

label var ma_business  "Master's degree: Business (1=Yes)"
label var ma_education "Master's degree: Education (1=Yes)"
label var ma_health    "Master's degree: Health & Related (1=Yes)"
label var ma_stem      "Master's degree: STEM (1=Yes)"
label var ma_other     "Master's degree: Other field (1=Yes)"

********************************************************************************
* SECTION 8: Generate Outcome (Salary)
* Potential outcomes framework with heterogeneous treatment effects
*
* Treatment effect heterogeneity now reflects GRADUATE program area
* (for treated obs) rather than undergraduate major. Field premia are
* calibrated to wage-premium literature on master's degrees by field:
*
*   Health & Related:  +0.14 log pts  (nursing, public health, pharmacy)
*   STEM:              +0.10 log pts  (engineering, CS, physical sciences)
*   Business:          +0.08 log pts  (MBA and related management degrees)
*   Education:         +0.04 log pts  (teacher licensure, admin programs)
*   Other:             +0.00 log pts  (base; social work, humanities, etc.)
*
* For untreated observations, undergraduate major proxies the likely
* field of a counterfactual graduate degree (magnitudes reduced relative
* to graduate field effects to reflect the additional uncertainty).
*
* Sources: Webber (2014); Carnevale, Cheah & Wenzinger (2021);
*          Tamborini, Kim & Sakamoto (2015); Zhang (2008)
********************************************************************************

* Potential outcome without treatment (Y0)
* Unchanged from original specification
gen ln_salary_0 = ///
    10.50 + ///
    -0.08*female + ///
    -0.05*black + ///
    -0.03*hispanic + ///
    0.06*asian + ///
    0.02*(age_ba - 22) + ///
    -0.03*firstgen + ///
    0.03*parent_income_q + ///
    0.04*parent_grad + ///
    0.10*(ugpa - 3.0) + ///
    0.25*stem_major + ///
    0.15*bus_major + ///
    -0.12*ed_major + ///
    0.08*selective_inst + ///
    -0.01*state_unemp + ///
    0.10*metro + ///
    0.20*eta_prod + ///                    /* Unobserved productivity */
    rnormal(0, 0.25)

label var ln_salary_0 "Log salary without master's (potential outcome)"

*-----------------------------------------------------------------------
* Field-specific return to master's degree
*
* For treated (masters == 1): use actual graduate program area.
* For untreated (masters == 0): use undergraduate major as counterfactual
*   proxy (magnitudes slightly smaller to reflect field-switching risk).
*-----------------------------------------------------------------------
gen te_field_return = .

* Treated: actual graduate program area effects
replace te_field_return = ///
    0.14*ma_health    + ///
    0.10*ma_stem      + ///
    0.08*ma_business  + ///
    0.04*ma_education + ///
    0.00*ma_other           if masters == 1

* Untreated: undergraduate major as counterfactual proxy
replace te_field_return = ///
    0.08*stem_major   + ///
    0.05*bus_major    + ///
    0.10*ed_major     + ///
    0.00*socsci_major       if masters == 0

label var te_field_return "Field-specific return to master's (actual or proxy)"

* Heterogeneous treatment effect (essential heterogeneity)
* Treatment effect varies with observables AND unobservables
gen te_masters = ///
    0.12             + ///   /* Base effect (Other/no-field baseline)  */
    te_field_return  + ///   /* Field-specific graduate return         */
    0.03*selective_inst + ///
    0.05*(ugpa - 3.0) + ///
    0.08*eta_ability + ///   /* Ability-education complementarity      */
    -0.10*(p_masters - 0.5) + /// /* Essential heterogeneity           */
    rnormal(0, 0.05)

label var te_masters "True Individual Treatment Effect"

* Potential outcome with treatment (Y1)
gen ln_salary_1 = ln_salary_0 + te_masters
label var ln_salary_1 "Log salary with master's (potential outcome)"

* Observed outcome
gen ln_salary = masters*ln_salary_1 + (1-masters)*ln_salary_0
gen salary = exp(ln_salary)

label var ln_salary "Observed log salary"
label var salary "Observed salary ($)"

********************************************************************************
* SECTION 9: Verify Data Properties
********************************************************************************

di _n "=============================================="
di "SYNTHETIC DATASET SUMMARY"
di "=============================================="

di _n "--- Sample Size ---"
count
di "N = " r(N)

di _n "--- Treatment Rate ---"
sum masters
di "Treatment rate: " %5.3f r(mean)

di _n "--- Instrument Summary ---"
sum ga_funding_adj

di _n "--- Outcome Summary ---"
sum ln_salary salary

di _n "--- True Treatment Effect Parameters ---"
sum te_masters
di "Mean TE (ATE approximation):        " %6.4f r(mean)

sum te_masters if masters == 1
di "Mean TE for treated (ATT approx.):  " %6.4f r(mean)

sum te_masters if masters == 0
di "Mean TE for untreated (ATU approx.):" %6.4f r(mean)

di _n "--- Correlation Structure ---"
correlate eta_ability eta_taste eta_prod

*-----------------------------------------------------------------------
* Master's Program Area Verification
*-----------------------------------------------------------------------

di _n "=============================================="
di "MASTER'S DEGREE PROGRAM AREA SUMMARY"
di "=============================================="

di _n "--- Program Area Distribution (Treated Only) ---"
di "(N = treated observations)"

count if masters == 1
local n_treated = r(N)
di "  Total treated: " `n_treated'

foreach area in stem business education health other {
    count if ma_`area' == 1
    di "  ma_`area': " r(N) "  (" %5.1f r(N)/`n_treated'*100 "%)"
}

di _n "--- Mutual Exclusivity Check ---"
gen ma_check = ma_business + ma_education + ma_health + ma_stem + ma_other
count if masters == 1 & ma_check != 1
di "  Treated obs with != 1 program area: " r(N) "  (should be 0)"
count if masters == 0 & ma_check != 0
di "  Untreated obs with != 0 program area: " r(N) "  (should be 0)"
drop ma_check

di _n "--- Program Area by Undergraduate Major (Treated Only) ---"
di "(Row: undergrad major | Col: graduate program area)"
di "  (Rows sum to 100% within each undergrad major)"
tab1 ma_business ma_education ma_health ma_stem ma_other if masters==1 & stem_major==1
tab1 ma_business ma_education ma_health ma_stem ma_other if masters==1 & bus_major==1
tab1 ma_business ma_education ma_health ma_stem ma_other if masters==1 & ed_major==1
tab1 ma_business ma_education ma_health ma_stem ma_other if masters==1 & socsci_major==1

di _n "--- Mean Treatment Effect by Master's Program Area ---"
di "  (For treated observations)"
foreach area in stem business education health other {
    sum te_masters if ma_`area' == 1
    di "  ma_`area': Mean TE = " %6.4f r(mean) "  (N=" r(N) ")"
}

di _n "--- Mean Salary by Master's Program Area ---"
foreach area in stem business education health other {
    sum salary if ma_`area' == 1
    di "  ma_`area': Mean salary = $" %9.0fc r(mean)
}

********************************************************************************
* SECTION 10: Save Dataset
********************************************************************************

* Order variables logically
order id female white black hispanic asian other_race age_ba ///
      firstgen parent_income_q parent_grad ugpa ///
      stem_major bus_major ed_major socsci_major ///
      selective_inst public_ug state_unemp metro state ///
      ga_funding ga_funding_adj ///
      eta_ability eta_taste eta_prod ///
      z_masters p_masters u_d masters ///
      ma_stem ma_business ma_education ma_health ma_other ///
      te_field_return ///
      ln_salary_0 te_masters ln_salary_1 ln_salary salary

* Compress and save
compress
save "Example_7_5_3.dta", replace

di _n "=============================================="
di "Dataset saved: Example_7_5_3.dta"
di "=============================================="

********************************************************************************
* END OF DATA GENERATION
********************************************************************************

* Close log
capture log close

exit
