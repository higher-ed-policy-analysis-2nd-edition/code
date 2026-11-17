*========================================================================
* Synthetic Grad PLUS / Master's Degree Dataset Generation
* Creates: Example_12_1.dta
*
* Application: Chapter 12 -- Bayesian MTE Microsimulation / Cost-Benefit
*              Analysis of the OBBBA $100k Aggregate Limit on Federal
*              Graduate Loans
* Instrument:  State-Funded Graduate Assistantship (GA) Funding Level
*
* Higher Education Policy Analysis Using Quantitative Techniques
* (2nd Edition)
* Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch12
* Author: Marvin A. Titus
* Date: June 2026 (revised November 2026; see REVISION below)
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
* REVISION (November 2026): resistance to treatment is now defined as
*   U_D = normal(-epsilon) instead of 1 - normal(index_latent). U_D is
*   uniform on [0,1], and a student completes (index_latent > 0) exactly
*   when U_D is below the propensity score, as the MTE framework requires
*   (Chapter 11, Eq. 11.1). The old definition mixed the instrument and
*   the controls into each student's return, so GA funding affected
*   salaries directly and no MTE estimator could recover the true
*   effects. This change uses no random numbers, so every variable except
*   ln_salary and salary is identical to the earlier canonical file
*   (superseded by REVISION 3 below, which changes the data). The
*   same change has been made in the inline fallback copies in
*   Stata_code12.do and R_code12.R. Re-push the regenerated
*   Example_12_1.dta to the GitHub data repository.
*   The script now also reports the TRUE ATE, ATT, and ATU (computed from
*   the potential outcomes before they are dropped) for validating the
*   chapter's MTE estimates, and writes its output to
*   Output/logs/Generate_Example_12_1.log.
*
*   REVISION 3 (November 2026): recalibrated DGP. Realistic returns that
*   decline with resistance, MTE(u) = 0.40 - 0.40u + 0.10u^2 (true ATE
*   0.233; positive selection on gains); potential outcomes with SD 0.45
*   and selection on levels; GA funding drawn with SD 4.0 (bounds 0-20)
*   and coefficient 0.25 in the selection index (intercept -3.00), so that
*   propensity scores span nearly all of [0,1] and completion is about 38%.
*   Verified by Monte Carlo (200 datasets, N = 8,000): the quadratic
*   control-function estimator recovers the true ATE, ATT, ATU, and PRTE
*   with negligible bias (SD of estimates about 0.03-0.04).
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

*========================================================================
* LOG FILE
* Written to Output/logs (the author's Dropbox Chapter 12 folder for
* username "marvi"; a relative Output/logs folder otherwise). A named log
* is used so it does not interfere with any other open log.
*========================================================================

if c(username) == "marvi" {
    global log_dir "C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 12\Output\logs"
    capture mkdir "C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 12\Output"
    capture mkdir "$log_dir"
}
else {
    global log_dir "Output/logs"
    capture mkdir "Output"
    capture mkdir "$log_dir"
}

capture log close gen12
log using "$log_dir/Generate_Example_12_1.log", replace text name(gen12)
di as text "Generate_Example_12_1.do log opened: " c(current_date) " " c(current_time)
di as text "Log file: $log_dir/Generate_Example_12_1.log"

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
* Calibration (REVISION 3): SD 4.0 and bounds 0-20 (previously SD 2.2,
* bounds 2-14) so that propensity scores span nearly all of [0,1].
gen ga_funding_adj = round(rnormal(7.5, 4.0) * 10) / 10
replace ga_funding_adj = max(0, min(20, ga_funding_adj))

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
* The selection equation mirrors Chapter 11. Students select into completion
* based on observed covariates, GA funding, and an unobserved individual factor.
gen epsilon = rnormal(0, 1)    // Latent selection error; defines resistance U_D
gen nu      = rnormal(0, 1)    // Not used below; retained so the random-number
                               //   sequence is unchanged across revisions.

* Latent index (linear combination of instrument + controls + noise)
gen index_latent = -3.00 ///          // REVISION 3 (was -1.97)
    + 0.25 * ga_funding_adj ///      // REVISION 3 (was 0.13)
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

* ── Heterogeneous treatment effects (REVISION 3) ────────────────────────────
* MTE(u) = a0 + a1*u + a2*u^2  (quadratic in resistance u, as in Ch. 11)
* Resistance is U_D = normal(-epsilon): uniform on [0,1], and a student
* completes (index_latent > 0) exactly when U_D is below the propensity
* score.
* The MTE declines from 0.40 at u = 0 to 0.10 at u = 1: positive selection
* on gains, so students most likely to complete benefit the most
* (Carneiro, Heckman, & Vytlacil, 2011). True ATE = a0 + a1/2 + a2/3 = 0.233.
* Selection on levels: students with higher resistance have higher
* earnings without the degree (better outside options), so a naive OLS
* comparison of completers and non-completers is biased.
local a0_true =  0.40
local a1_true = -0.40
local a2_true =  0.10

gen u_true      = normal(-epsilon)                // U_D: resistance
gen mte_true    = `a0_true' + `a1_true'*u_true + `a2_true'*u_true^2
gen Y0_latent   = 0.15*(u_true - 0.5) + rnormal(0, 0.45)   // Log-salary under D=0
gen Y1_latent   = Y0_latent + mte_true + rnormal(0, 0.10)  // Log-salary under D=1

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

* ── True treatment effects (for validating the chapter's estimates) ─────────
* The individual effect of completion on log salary is Y1_latent - Y0_latent.
* These are the targets the corrected MTE estimates in Stata_code12.do
* (Section 5) should approximate. No random numbers are used here.
gen te_true = Y1_latent - Y0_latent
qui sum te_true
local true_ate = r(mean)
qui sum te_true if masters == 1
local true_att = r(mean)
qui sum te_true if masters == 0
local true_atu = r(mean)

* ── Drop construction intermediates ─────────────────────────────────────────
drop prog_draw loan_noise epsilon nu index_latent phat_true u_true ///
     mte_true Y1_latent Y0_latent te_true

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

di _n "--- True treatment effects (log salary units) ---"
di "True ATE: " %6.4f `true_ate'
di "True ATT: " %6.4f `true_att'
di "True ATU: " %6.4f `true_atu'

di _n as text "Generate_Example_12_1.do log closed: " c(current_date) " " c(current_time)
log close gen12

*========================================================================
* END OF SCRIPT
*========================================================================
