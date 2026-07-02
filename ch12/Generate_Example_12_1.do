*========================================================================
* Synthetic Grad PLUS / Master's Degree Dataset Generation
* Creates: Example_12_1.dta
*
* Application: Chapter 12 -- Bayesian MTE Microsimulation / Cost-Benefit
*              Analysis of a $100k Lifetime Cap on Grad PLUS Loans
* Instrument:  State-Funded Graduate Assistantship (GA) Funding Level
*
* Higher Education Policy Analysis Using Quantitative Techniques
* (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch12
* Author: Marvin A. Titus
* Date: June 2026
* NOTE: Code development was assisted by Claude (Anthropic). The author
* provided specifications and reviewed, tested, and validated all code.
*========================================================================
* Script tested in Stata 19.5
* Compatible with Stata version 19 or later
*
* PURPOSE:
*   This script generates the canonical synthetic population used by
*   Chapter 12 (Stata_code12.do and R_code12.R). It exists as a
*   standalone utility so the dataset can be regenerated independently
*   of the full chapter script -- for example, to produce a fresh draw
*   with a different seed, or to rebuild Example_12_1.dta from scratch
*   after a change to the data-generating process below.
*
*   Both Stata_code12.do and R_code12.R already contain this identical
*   generation logic as an inline fallback (used only if the canonical
*   file cannot be downloaded from the GitHub data repository), so this
*   script is a convenience copy, not a dependency of the chapter script.
*   If you change the DGP here, update the inline copies in both
*   Stata_code12.do and R_code12.R to match, and re-push the resulting
*   Example_12_1.dta to:
*     https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch12
*
* NOTE ON SYNTHETIC DATA:
*   This simulation uses synthetic data calibrated to mirror the
*   Baccalaureate and Beyond Longitudinal Study (B&B) population used
*   elsewhere in the book, extended with a Grad PLUS loan amount and
*   institutional revenue variables specific to the Chapter 12 policy
*   simulation. Synthetic rather than restricted-use B&B data is used
*   for license-free access, known true parameters for validation, and
*   full reproducibility by readers.
*========================================================================

*========================================================================
* OUTPUT DATA DIRECTORY
* Paths switch automatically based on the OS username (c(username)).
* The instructor's personal path is used when username == "marvi";
* all other users get a generic relative path.
*========================================================================

if c(username) == "marvi" {
    global data_dir "C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 12\Data"
    capture mkdir "C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 12"
    capture mkdir "$data_dir"
}
else {
    global data_dir "Data"
    capture mkdir "$data_dir"
}

di as text "Data directory: $data_dir"

version 19
set more off
set seed 20251201

* ── DGP ────────────────────────────────────────────────────────────────
clear
set obs 8000
gen id = _n

* ── Individual characteristics ──────────────────────────────────────────────
gen female          = (runiform() < 0.52)
gen black           = (runiform() < 0.13)
gen hispanic        = (runiform() < 0.10)
gen asian           = (runiform() < 0.07)
gen age_ba          = round(rnormal(24, 3))
replace age_ba      = max(21, min(35, age_ba))
gen firstgen        = (runiform() < 0.26)
gen parent_income_q = ceil(runiform() * 4)          // 1 = bottom, 4 = top
gen parent_grad     = (runiform() < 0.38)
gen ugpa            = round(rnormal(3.3, 0.45) * 100) / 100
replace ugpa        = max(2.0, min(4.0, ugpa))

* ── Program area ────────────────────────────────────────────────────────────
gen prog_draw   = runiform()
gen stem_major  = (prog_draw < 0.22)
gen bus_major   = (prog_draw >= 0.22 & prog_draw < 0.40)
gen ed_major    = (prog_draw >= 0.40 & prog_draw < 0.56)
gen health_major = (prog_draw >= 0.56 & prog_draw < 0.70)
* Base category: Other / Social Sciences

* ── Institution characteristics ─────────────────────────────────────────────
gen selective_inst = (runiform() < 0.28)
gen public_ug      = (runiform() < 0.72)
gen state_unemp    = round(rnormal(5.2, 1.3) * 10) / 10
replace state_unemp = max(2.5, min(10.5, state_unemp))
gen metro          = (runiform() < 0.68)

* ── State GA funding (instrument) ───────────────────────────────────────────
* Each student is assigned a state GA funding level. Variation is driven by
* state budgetary conditions and is plausibly exogenous to individual earnings.
gen ga_funding_adj = round(rnormal(7.5, 2.2) * 10) / 10
replace ga_funding_adj = max(2.0, min(14.0, ga_funding_adj))

* ── Grad PLUS loan amount ────────────────────────────────────────────────────
* Loan amounts reflect program costs (STEM and business programs cost more),
* institutional selectivity, and family income need. An individual-specific
* unobserved borrowing propensity adds idiosyncratic variation.
* Units: $000s. The $100k policy cap implies threshold = 100.
gen loan_noise = rnormal(0, 22)
gen grad_plus_loans = 30 ///
    + 25 * stem_major             ///  STEM: higher program costs
    + 35 * bus_major              ///  Business/MBA: highest costs
    + 10 * ed_major               ///  Education: moderate
    + 20 * health_major           ///  Health: moderate-high
    + 15 * selective_inst         ///  Selective institutions: higher CoA
    + 12 * (4 - parent_income_q)  ///  Lower income → higher borrowing need
    - 0.8 * ga_funding_adj        ///  Higher GA funding → less need to borrow
    + loan_noise
replace grad_plus_loans = max(0, grad_plus_loans)
replace grad_plus_loans = min(250, grad_plus_loans)

* ── Institutional revenue variables ─────────────────────────────────────────
* Annual tuition and program length determine the institutional revenue stake.
* Business/MBA programs carry the highest sticker price; Education the lowest.
* Selective institutions charge a premium of roughly $12–15k per year.
* Net revenue to the institution after variable (instructional) costs is
* approximately 65 cents per tuition dollar — the marginal cost share is ~35%.
* An additional 20% of net revenue cross-subsidizes undergraduate programs
* and need-based aid; a graduate enrollment drop therefore has a ripple effect
* on the institution's broader financial model.
gen tuition_noise = rnormal(0, 4)
gen annual_tuition = 25 ///
    + 35 * bus_major              ///  Business/MBA: highest tuition
    + 10 * stem_major             ///  STEM: moderate (TA/RA lowers net cost)
    +  0 * ed_major               ///  Education: at baseline
    + 20 * health_major           ///  Health: high program costs
    + 12 * selective_inst         ///  Selective institutions: premium
    -  3 * public_ug              ///  Public institutions: lower sticker price
    + tuition_noise
replace annual_tuition = max(10, annual_tuition)   // Floor at $10k/year
label variable annual_tuition "Annual Graduate Tuition ($000s)"

* Program length (years to degree): MBA = 2, STEM = 2.5, others = 2
gen program_years = 2 + 0.5 * stem_major
label variable program_years "Expected Years to Degree"

* Total gross tuition revenue per enrolled student
gen gross_tuition = annual_tuition * program_years
label variable gross_tuition "Gross Tuition Revenue per Student ($000s)"

* Net institutional revenue (65% of gross; 35% is variable instructional cost)
gen net_inst_rev = 0.65 * gross_tuition
label variable net_inst_rev "Net Institutional Revenue per Student ($000s)"

drop tuition_noise

* ── Latent propensity to complete ───────────────────────────────────────────
* The selection equation follows Chapter 10. Students select into completion
* based on observed covariates, GA funding, and an unobserved individual factor.
gen epsilon = rnormal(0, 1)    // Latent selection error (positive correlation
gen nu      = rnormal(0, 1)    //   with earnings error to create selection bias)

* Latent index (linear combination of instrument + controls + noise)
gen index_latent = -1.97 ///
    + 0.13 * ga_funding_adj ///
    + 0.04 * ugpa * 10 ///
    + 0.08 * parent_grad ///
    - 0.10 * firstgen ///
    + 0.06 * (parent_income_q - 2) ///
    + 0.05 * selective_inst ///
    + 0.05 * metro ///
    + 0.07 * ed_major ///
    - 0.03 * age_ba ///
    + epsilon

gen masters = (index_latent > 0)
label variable masters "Completed Master's Degree (1=Yes)"

* ── Propensity score (true) ─────────────────────────────────────────────────
gen phat_true = normal(index_latent)

* ── Heterogeneous treatment effects ─────────────────────────────────────────
* MTE(u) = b0 + b1*u + b2*u^2 + b3*u^3  (cubic polynomial)
* True parameters imply declining MTE → positive selection on gains.
* Interpretation: students most likely to complete (low u) benefit the most.
local b0_true = -2.50
local b1_true =  19.30
local b2_true = -30.25
local b3_true =  15.12

gen u_true      = 1 - phat_true
gen mte_true    = `b0_true' + `b1_true'*u_true + `b2_true'*u_true^2 + `b3_true'*u_true^3
gen Y1_latent   = mte_true + rnormal(0, 0.40)    // Log-salary under D=1
gen Y0_latent   = rnormal(0, 0.70)               // Log-salary under D=0

* ── Observed log salary ─────────────────────────────────────────────────────
gen ln_salary = 10.0 ///
    + Y1_latent * masters ///
    + Y0_latent * (1 - masters) ///
    + 0.25 * ugpa ///
    + 0.15 * stem_major ///
    + 0.25 * bus_major ///
    - 0.18 * ed_major ///
    + 0.10 * selective_inst ///
    - 0.07 * female ///
    - 0.06 * black ///
    + 0.025 * (parent_income_q - 2) ///
    + rnormal(0, 0.20)
label variable ln_salary "Log Annual Salary"

gen salary = exp(ln_salary)
label variable salary "Annual Salary ($)"

* ── Drop construction intermediates ─────────────────────────────────────────
drop prog_draw loan_noise epsilon nu index_latent phat_true u_true ///
     mte_true Y1_latent Y0_latent

* ── Save synthetic dataset ──────────────────────────────────────────────
save "$data_dir/Example_12_1.dta", replace

di as text "Synthetic dataset saved as $data_dir/Example_12_1.dta"
di as text "N = " _N " observations, " c(k) " variables"

*========================================================================
* Quick sanity checks
*========================================================================
di _n "--- Sanity Checks ---"
tab masters
summarize grad_plus_loans ln_salary salary, detail
qui count if grad_plus_loans > 100
di "Share above $100k cap: " %5.3f (r(N) / _N)

*========================================================================
* END OF SCRIPT
*========================================================================
