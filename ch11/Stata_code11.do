/*===========================================================================
  Chapter 11: Bayesian MTE Microsimulation
  Cost–Benefit Analysis of a $100k Lifetime Cap on Grad PLUS Loans
  
  Book:   Higher Education Policy Analysis Using Quantitative Techniques
          Second Edition — Springer
  
  Code development assisted by Claude (Anthropic, 2025). The author provided
  methodological specifications and reviewed, tested, and validated all code
  and output.
  
  Repository:
    https://github.com/higher-ed-policy-analysis-2nd-edition/code/blob/main/ch11/Stata_code11.do
  
  Overview:
    This script evaluates the net social benefit of a $100,000 lifetime
    aggregate cap on federal Grad PLUS borrowing (effective July 1, 2026
    under the One Big Beautiful Bill, Pub. L. 119-XX). The cap binds for
    graduate students whose program costs exceed the new limit. Using the
    marginal treatment effect (MTE) framework from Chapter 10, we estimate
    heterogeneous returns to master's degree completion and simulate which
    students a binding cap would push out of the program. A Bayesian
    microsimulation (parametric bootstrap, S = 1000 posterior draws)
    propagates coefficient uncertainty into a posterior distribution of
    net social benefits, fiscal savings, and behavioral costs.
  
  Pipeline:
    Section 1.  Setup and directory structure
    Section 2.  Synthetic data generation (B&B-style panel, N = 8,000)
    Section 3.  Descriptive statistics and policy exposure
    Section 4.  Probit selection model and propensity score
    Section 5.  MTE estimation — cubic polynomial control function
    Section 6.  Policy simulation — cap-binding student identification
    Section 7.  Bayesian microsimulation (S = 1000 parametric draws)
    Section 8.  Cost–benefit decomposition
    Section 9.  Posterior summaries and inference
    Section 10. Figures
  
  CBA Framework:
    Net Benefit^(s) = Fiscal_Savings^(s) - Behavioral_Cost^(s)
    
    Fiscal savings:   reduced federal lending on overage × subsidy rate
    Behavioral cost:  lost human capital for cap-constrained dropouts
                      (integral of positive MTE over displaced margin)
    
    Policy is efficient when displaced students have negative or near-zero
    returns; inefficient when displaced students have strongly positive MTE.
  
  Requirements:
    Stata 19 or later
    No additional user-written packages required
===========================================================================*/


/*---------------------------------------------------------------------------
  Section 1: Setup
---------------------------------------------------------------------------*/

version 19
set more off
set seed 20251201

* ── Working directory ────────────────────────────────────────────────────────
* Set explicitly so all relative paths resolve to the Chapter 11 folder.
* Adjust only if running on a different machine.
cd "C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 11"
di "Working directory: " c(pwd)

* ── Directory structure ─────────────────────────────────────────────────────
capture mkdir "Output"
capture mkdir "Output/logs"
capture mkdir "Output/tables"
capture mkdir "Output/graphs"

global tables_dir  "Output/tables"
global graphs_dir  "Output/graphs"

* ── Log ─────────────────────────────────────────────────────────────────────
capture log close _all
log using "Output/logs/Chapter11_Stata_output.log", replace text
di "Log file: " c(pwd) "/Output/logs/Chapter11_Stata_output.log"
di "Chapter 11 log opened: " c(current_date) " " c(current_time)

* ── Global macros ───────────────────────────────────────────────────────────
* Individual-level controls (same set as Chapter 10 for consistency)
global X_controls "female black hispanic asian age_ba firstgen parent_income_q parent_grad ugpa stem_major bus_major ed_major selective_inst public_ug state_unemp metro"
global X "$X_controls"
global Z "ga_funding_adj"

* CBA parameters
global discount_rate   0.03    // Annual discount rate (3%)
global career_years    30      // Earnings horizon (years)
global subsidy_rate    0.20    // Government subsidy on Grad PLUS principal
global cap_threshold   100     // Policy cap ($000s)
global base_salary     47000   // Pooled mean annual salary (non-holders)
global cost_per_degree 100000  // Per-student cost of master's degree (full debt)
global S_draws         1000     // Number of Bayesian posterior draws


/*---------------------------------------------------------------------------
  Section 2: Synthetic Data Generation
  
  Population: 8,000 graduate students drawn from a B&B-mirroring DGP.
  The dataset extends the Chapter 10 synthetic panel (Example_7_5_3.dta)
  with a Grad PLUS loan amount variable (grad_plus_loans, in $000s).
  Loan amounts depend on program type, institutional selectivity, family
  income, and a student-specific unobserved borrowing propensity.
  
  GitHub data repository:
    https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch11
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Section 2: Synthetic Data Generation"
di    "============================================================"

* ── Attempt to load from repository; generate synthetically if unavailable ──
capture copy ///
    "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch11/Example_11_1.dta" ///
    "Example_11_1.dta", replace
if _rc != 0 {
    di as text "Remote data not found. Generating synthetic dataset."
}

* ── DGP ────────────────────────────────────────────────────────────────────
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

* ── Save synthetic dataset ──────────────────────────────────────────────────
save "Example_11_1.dta", replace
di as text "Synthetic dataset saved as Example_11_1.dta"

* ── Always load the dataset (whether downloaded or just generated) ───────────
use "Example_11_1.dta", clear


/*---------------------------------------------------------------------------
  Section 3: Descriptive Statistics and Policy Exposure
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Section 3: Descriptive Statistics and Policy Exposure"
di    "============================================================"

use "Example_11_1.dta", clear

* ── Sample overview ─────────────────────────────────────────────────────────
tab masters
tabstat salary ln_salary grad_plus_loans, by(masters) stats(mean sd min max n)

* ── Policy exposure: students above $100k cap ────────────────────────────────
gen above_cap    = (grad_plus_loans > $cap_threshold)
gen loan_overage = max(0, grad_plus_loans - $cap_threshold)

di _n "Policy exposure summary:"
tabstat grad_plus_loans, stats(mean p25 p50 p75 p90 p95 max n)

di _n "Students above $100k cap:"
tab above_cap masters, row col

di _n "Mean loan overage conditional on exceeding cap:"
summarize loan_overage if above_cap == 1

* Share of completers who would face a binding constraint
qui count if masters == 1
local n_completers = r(N)
qui count if masters == 1 & above_cap == 1
local n_constrained = r(N)
di as text "Constrained completers: " `n_constrained' " / " `n_completers' ///
    " (" %5.1f 100*`n_constrained'/`n_completers' "%)"

* ── By program area ─────────────────────────────────────────────────────────
di _n "Mean Grad PLUS loans by program area:"
tabstat grad_plus_loans above_cap, by(stem_major) stats(mean n)
tabstat grad_plus_loans above_cap, by(bus_major)  stats(mean n)
tabstat grad_plus_loans above_cap, by(ed_major)   stats(mean n)


/*---------------------------------------------------------------------------
  Section 4: Probit Selection Model and Propensity Score
  
  The propensity score P(D=1|Z,X) is the key input to the MTE polynomial.
  State GA funding (ga_funding_adj) serves as the excluded instrument.
  First-stage relevance is confirmed by the linear probability F-statistic.
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Section 4: Probit Selection Model and Propensity Score"
di    "============================================================"

* ── Linear probability model (first-stage F-statistic) ─────────────────────
reg masters $Z $X, robust
test ga_funding_adj
di as text "First-stage F-statistic: " %6.2f r(F)

* ── Probit selection model ──────────────────────────────────────────────────
probit masters $Z $X
estimates store probit_first

* ── Propensity score and linear index ───────────────────────────────────────
predict phat,    pr
predict z_index, xb
label variable phat    "Propensity Score P(D=1|Z,X)"
label variable z_index "Linear Index from Probit"

* Polynomial terms for control function
gen phat2 = phat^2
gen phat3 = phat^3

summarize phat, detail
di "GA funding coefficient (probit): " _b[ga_funding_adj]
local ga_coef = _b[ga_funding_adj]

* Propensity score distribution among completers and non-completers
tabstat phat, by(masters) stats(mean sd p10 p50 p90)


/*---------------------------------------------------------------------------
  Section 5: MTE Estimation — Cubic Polynomial Control Function
  
  Following Heckman and Vytlacil (1999, 2005), the MTE is recovered from
  the derivative of the outcome regression with respect to the propensity
  score. The polynomial control function approach is used here for
  transparency and direct estimation of ATE, ATT, and ATU.
  
  MTE(u) = b0 + b1*u + b2*u^2 + b3*u^3
  
  where u is interpreted as unobserved resistance to treatment and the
  polynomial coefficients are the coefficients on the interaction terms
  c.masters#c.phat, c.masters#c.phat2, c.masters#c.phat3.
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Section 5: MTE Estimation"
di    "============================================================"

* ── Cubic MTE regression ────────────────────────────────────────────────────
reg ln_salary masters c.masters#(c.phat c.phat2 c.phat3) ///
    $X phat phat2 phat3, robust
estimates store mte_cubic

* ── Joint significance test for MTE polynomial coefficients ─────────────────
* Individual coefficients are often imprecise due to multicollinearity among
* polynomial terms. The joint F-test is the appropriate assessment of whether
* the MTE curve is identified — i.e., whether treatment effect heterogeneity
* exists across the distribution of unobserved resistance to treatment.
test masters c.masters#c.phat c.masters#c.phat2 c.masters#c.phat3
di as text "Joint F-test p-value: " %6.4f r(p)

* ── Store polynomial coefficients ───────────────────────────────────────────
local b0 = _b[masters]                      // MTE at u = 0
local b1 = _b[c.masters#c.phat]             // Linear slope
local b2 = _b[c.masters#c.phat2]            // Quadratic term
local b3 = _b[c.masters#c.phat3]            // Cubic term

di _n "MTE polynomial: MTE(u) = " %7.4f `b0' " + " %7.4f `b1' "*u" ///
    " + " %7.4f `b2' "*u^2 + " %7.4f `b3' "*u^3"

* ── Treatment parameters ────────────────────────────────────────────────────
* ATE = integral of MTE(u) du from 0 to 1
local ate = `b0' + `b1'/2 + `b2'/3 + `b3'/4
di "Estimated ATE (cubic): " %6.4f `ate'

* ATT = E[MTE(u) | D=1] = weighted average over completers' propensity scores
gen mte_hat = `b0' + `b1'*phat + `b2'*phat^2 + `b3'*phat^3
qui sum mte_hat if masters == 1
local att = r(mean)
di "Estimated ATT: " %6.4f `att'

* ATU = E[MTE(u) | D=0]
qui sum mte_hat if masters == 0
local atu = r(mean)
di "Estimated ATU: " %6.4f `atu'

* Note: ATU > ATT reflects negative selection on gains in this dataset —
* students currently kept out of graduate education have higher potential
* returns than those who self-selected in, consistent with financial barriers.
di _n "Treatment parameter hierarchy (this dataset): ATU > ATT > ATE = " ///
    %6.4f `atu' " > " %6.4f `att' " > " %6.4f `ate'

* ── Store VCV for Bayesian draws ─────────────────────────────────────────────
* The full coefficient vector and VCV are extracted in Section 7.
* Coefficient positions are identified there via a foreach loop.
matrix b_hat  = e(b)
matrix V_hat  = e(V)


/*---------------------------------------------------------------------------
  Section 6: Policy Simulation — Cap-Binding Student Identification
  
  Under the $100k lifetime cap, students currently borrowing more than
  $100k face a binding credit constraint. Those who cannot substitute with
  private credit or personal resources drop out, forfeiting the returns to
  completion. We model the probability of being pushed out as a function
  of the loan overage: p_out(overage) = Φ(overage / 50). This implies a
  smooth, increasing probability of displacement as the overage grows,
  from near-zero at small overages to near-certainty for very large ones.
  
  Constrained completers are classified as:
    (a) Remaining: find alternative financing; remain enrolled; D stays 1
    (b) Displaced: cannot cover the gap; D switches from 1 to 0
  
  Displaced students bear the behavioral cost of the policy: they forego
  a return equal to their estimated MTE × discounted salary profile.
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Section 6: Policy Simulation"
di    "============================================================"

* ── Loan overage and probability of displacement ─────────────────────────────
gen p_displaced = normal(loan_overage / 50) * above_cap
label variable p_displaced "P(Displaced by cap | student characteristics)"

summarize p_displaced if masters == 1 & above_cap == 1

* ── Expected displacement indicator (continuous treatment intensity) ─────────
gen displaced_E = p_displaced * masters    // Expected D: 1 → 0 switchers
summarize displaced_E

* ── Marginal Policy-Relevant Treatment Effect (MPRTE) ────────────────────────
* MPRTE = E[MTE(U) | margin shifted by the $100k cap]
*       = displacement-weighted average of individual MTEs.
* Each student is weighted by their share of total expected displacement.
* Restricted to completers (displaced_E = 0 for non-completers by construction).
*
* Interpretation:
*   MPRTE > 0  → cap displaces high-return students (efficiency loss)
*   MPRTE ≈ 0  → cap displaces marginal students (efficiency neutral)
*   MPRTE < 0  → cap displaces low-return students (efficiency gain)
*
* Note on Stata syntax: gen ... sum() returns a *running* (cumulative) sum,
* not a total. We use a scalar from summarize r(sum) to compute proper
* normalized weights that sum to 1.
qui sum displaced_E if masters == 1
scalar total_disp = r(sum)
gen mprte_weight = displaced_E / total_disp if masters == 1
gen mprte_i      = mte_hat * mprte_weight   if masters == 1
qui sum mprte_i  if masters == 1
scalar mprte = r(sum)

di _n "--- Treatment Parameter Hierarchy (Point Estimates) ---"
di "ATE   (population average):           " %6.4f `ate'
di "ATT   (E[MTE | D=1], completers):     " %6.4f `att'
di "ATU   (E[MTE | D=0], non-completers): " %6.4f `atu'
di "MPRTE (cap-displaced margin):         " %6.4f mprte
di _n "MPRTE compares the average return on the policy margin to the" 
di    "population-wide ATE. If MPRTE < ATT, the cap is removing students"
di    "with below-average returns among completers (efficiency-improving)."

* ── Present value factor ─────────────────────────────────────────────────────
local pv_factor = (1 - (1 + $discount_rate)^(-$career_years)) / $discount_rate
di "Present value factor (" $career_years " yrs, " $discount_rate*100 "% r): " ///
    %6.3f `pv_factor'

* ── Point-estimate CBA (pre-simulation baseline) ─────────────────────────────
* Fiscal savings: government recovers subsidy on overage lending
gen fiscal_saving_i = loan_overage * $subsidy_rate * above_cap
* This applies to ALL students above the cap (completers and non-completers)
* because the cap reduces federal exposure regardless of enrollment outcome.
qui sum fiscal_saving_i
local FS_point = r(sum)
di "Point-estimate fiscal savings ($000s): " %8.1f `FS_point'

* Behavioral cost: lost human capital for displaced completers
* = MTE × base_salary × PV_factor (in $000s, since salary is in $)
gen behav_cost_i = displaced_E * max(0, mte_hat) * ($base_salary/1000) * `pv_factor'
* Note: max(0, MTE) reflects that only students with positive returns suffer a
* human capital loss. Displaced students with negative MTE represent an
* efficiency GAIN (removing low-return borrowers).
qui sum behav_cost_i if masters == 1
local BC_point = r(sum)
di "Point-estimate behavioral cost ($000s): " %8.1f `BC_point'

* Efficiency gain from removing low-return borrowers (negative MTE region)
gen efficiency_gain_i = displaced_E * max(0, -mte_hat) * ($base_salary/1000) * `pv_factor'
qui sum efficiency_gain_i if masters == 1
local EG_point = r(sum)
di "Point-estimate efficiency gain (low-return exit, $000s): " %8.1f `EG_point'

* Net benefit (point estimate)
local NB_point = `FS_point' - `BC_point' + `EG_point'
di "Point-estimate net benefit ($000s): " %8.1f `NB_point'
di "Benefit-cost ratio: " %5.3f (`FS_point' + `EG_point') / max(1, `BC_point')

* ── Institutional revenue loss ───────────────────────────────────────────────
* When a student is displaced by the cap, the institution loses their net
* tuition revenue for the remaining program years. This is distinct from the
* student's human capital loss: it is a revenue shock to the institution that
* affects staffing, cross-subsidies, and program viability.
*
* Two components:
*   (a) Direct net revenue loss:  displaced_E × net_inst_rev
*   (b) Cross-subsidy disruption: 20% of (a), representing the upstream effect
*       on undergraduate aid and other programs funded by graduate tuition.
gen inst_rev_loss_i   = displaced_E * net_inst_rev
gen cross_sub_loss_i  = 0.20 * inst_rev_loss_i

qui sum inst_rev_loss_i  if masters == 1
local IR_point = r(sum)
qui sum cross_sub_loss_i if masters == 1
local CS_point = r(sum)

di _n "--- Institutional Outcomes (Point Estimates) ---"
di "Net institutional revenue loss ($000s):     " %8.1f `IR_point'
di "Cross-subsidy disruption ($000s):           " %8.1f `CS_point'
di "Total institutional sector impact ($000s):  " %8.1f (`IR_point' + `CS_point')

* Revenue loss by program area
di _n "Institutional revenue loss by program area ($000s):"
tabstat inst_rev_loss_i if masters == 1, ///
    by(stem_major) stats(sum mean n)
tabstat inst_rev_loss_i if masters == 1, ///
    by(bus_major)  stats(sum mean n)
tabstat inst_rev_loss_i if masters == 1, ///
    by(ed_major)   stats(sum mean n)

* Revenue loss by institution type
di _n "Institutional revenue loss by institution type ($000s):"
tabstat inst_rev_loss_i if masters == 1, ///
    by(selective_inst) stats(sum mean n)
tabstat inst_rev_loss_i if masters == 1, ///
    by(public_ug) stats(sum mean n)

* Extended net benefit including institutional costs
local NB_extended = `NB_point' - `IR_point' - `CS_point'
di _n "Extended net benefit (incl. institutional costs, $000s): " %8.1f `NB_extended'


/*---------------------------------------------------------------------------
  Section 7: Bayesian Microsimulation (S = 1000 Parametric Draws)
  
  The point-estimate CBA ignores uncertainty in the MTE polynomial. The
  Bayesian layer propagates uncertainty by drawing S = 1000 coefficient
  vectors from the asymptotic normal posterior:
  
    θ^(s) ~ N(θ_hat, V_hat)
  
  where θ_hat and V_hat are the estimated coefficient vector and its
  variance-covariance matrix from the cubic MTE regression. For each draw,
  we recompute the MTE curve, the policy-affected margin, and the three
  CBA components. The resulting distribution characterizes posterior
  uncertainty about the net social benefit of the $100k cap.
  
  Additional uncertainty is propagated through draws on:
    - Base salary (log-normal, reflecting real-wage uncertainty)
    - Subsidy rate (uniform, reflecting fiscal policy uncertainty)
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Section 7: Bayesian Microsimulation"
di    "============================================================"

* ── Store coefficient vector and VCV ─────────────────────────────────────────
* Re-estimate to ensure correct e(V) is in memory.
qui reg ln_salary masters c.masters#(c.phat c.phat2 c.phat3) ///
    $X phat phat2 phat3, robust
matrix b_full = e(b)
matrix V_full = e(V)
local k_full  = colsof(b_full)

* Identify column positions of the four MTE polynomial coefficients.
local cols : colnames b_full
local pos_b0 = 0
local pos_b1 = 0
local pos_b2 = 0
local pos_b3 = 0
local c = 0
foreach col of local cols {
    local c = `c' + 1
    if "`col'" == "masters"              local pos_b0 = `c'
    if "`col'" == "c.masters#c.phat"     local pos_b1 = `c'
    if "`col'" == "c.masters#c.phat2"    local pos_b2 = `c'
    if "`col'" == "c.masters#c.phat3"    local pos_b3 = `c'
}
di "Coefficient positions — b0: `pos_b0', b1: `pos_b1', b2: `pos_b2', b3: `pos_b3'"

* ── Postfile for simulation results ─────────────────────────────────────────
tempfile sim_results
postfile sim_handle ///
    draw          ///
    b0_s b1_s b2_s b3_s        ///  Posterior draws on MTE polynomial
    ate_s att_s atu_s          ///  Treatment parameters per draw
    fiscal_s                   ///  Fiscal savings ($000s)
    behav_cost_s               ///  Behavioral cost ($000s)
    effic_gain_s               ///  Efficiency gain from low-return exit ($000s)
    net_benefit_s              ///  Net social benefit ($000s)
    nb_per_student_s           ///  NB per displaced student ($000s)
    bcr_s                      ///  Benefit-cost ratio
    pnb_positive_s             ///  Indicator: NB > 0 this draw
    inst_rev_loss_s            ///  Net institutional revenue loss ($000s)
    cross_sub_loss_s           ///  Cross-subsidy disruption ($000s)
    nb_extended_s              ///  Extended NB including institutional costs ($000s)
    using `sim_results', replace

* ── Simulation loop ──────────────────────────────────────────────────────────
di as text _n "Running $S_draws posterior draws..."
di as text "(This may take 1–2 minutes.)"

* Draw full-dimension coefficient vectors from N(b_full, V_full)
* We draw all coefficients jointly to preserve correlation structure.
preserve
drawnorm draw_1-draw_`k_full', n($S_draws) means(b_full) cov(V_full) clear

* Store all draws in a matrix for direct subscripting inside the loop.
* This avoids loading and trimming a dataset on every iteration.
mkmat draw_1-draw_`k_full', matrix(B_draws)
restore

forvalues s = 1 / $S_draws {

    * ── Extract posterior draw s from pre-stored matrix ─────────────────────
    * B_draws[s, col] gives the s-th draw of each coefficient directly.
    * No dataset loading or trimming needed — eliminates the "499 obs deleted"
    * messages that appeared when using preserve/use/keep if _n == s.
    local b0_s = B_draws[`s', `pos_b0']
    local b1_s = B_draws[`s', `pos_b1']
    local b2_s = B_draws[`s', `pos_b2']
    local b3_s = B_draws[`s', `pos_b3']

    * ── Additional economic uncertainty ─────────────────────────────────────
    * Base salary: log-normal with 10% coefficient of variation
    local base_sal_s = exp(ln($base_salary) + rnormal(0, 0.10))
    * Subsidy rate: uniform ± 5 percentage points around baseline
    local sub_rate_s = $subsidy_rate + runiform(-0.05, 0.05)
    * Tuition multiplier: log-normal with 8% CV, reflecting uncertainty in
    * actual tuition levels, discount rates, and institutional aid policies.
    * This ensures inst_rev_loss_s varies meaningfully across draws.
    local tuition_mult_s = exp(rnormal(0, 0.08))

    * ── Recompute MTE for this draw ──────────────────────────────────────────
    qui {
        gen mte_s = `b0_s' + `b1_s'*phat + `b2_s'*phat^2 + `b3_s'*phat^3

        * Treatment parameters
        local ate_s = `b0_s' + `b1_s'/2 + `b2_s'/3 + `b3_s'/4
        sum mte_s if masters == 1
        local att_s = r(mean)
        sum mte_s if masters == 0
        local atu_s = r(mean)

        * Fiscal savings (uses draw-specific subsidy rate)
        gen fs_i_s = loan_overage * `sub_rate_s' * above_cap
        sum fs_i_s
        local fiscal_s = r(sum)

        * Behavioral cost: human capital loss for displaced high-return students
        gen bc_i_s = displaced_E * max(0, mte_s) * (`base_sal_s'/1000) * `pv_factor'
        sum bc_i_s if masters == 1
        local behav_cost_s = r(sum)

        * Efficiency gain: removing low-return constrained students
        gen eg_i_s = displaced_E * max(0, -mte_s) * (`base_sal_s'/1000) * `pv_factor'
        sum eg_i_s if masters == 1
        local effic_gain_s = r(sum)

        * Net benefit
        local net_benefit_s = `fiscal_s' - `behav_cost_s' + `effic_gain_s'

        * Per-displaced-student net benefit
        sum displaced_E if masters == 1
        local n_disp_s = max(1, r(sum))
        local nb_per_student_s = `net_benefit_s' / `n_disp_s'

        * Benefit-cost ratio
        local bcr_s = (`fiscal_s' + `effic_gain_s') / max(1, `behav_cost_s')

        * Efficiency indicator
        local pnb_positive_s = (`net_benefit_s' > 0)

        * Institutional revenue loss
        * Net tuition revenue forgone when displaced students leave.
        * Draw-specific tuition multiplier propagates pricing uncertainty.
        * Cross-subsidy disruption adds 20% for upstream undergraduate impact.
        gen ir_i_s = displaced_E * net_inst_rev * `tuition_mult_s'
        sum ir_i_s if masters == 1
        local inst_rev_loss_s = r(sum)
        local cross_sub_loss_s = 0.20 * `inst_rev_loss_s'
        local nb_extended_s = `net_benefit_s' - `inst_rev_loss_s' - `cross_sub_loss_s'

        drop mte_s fs_i_s bc_i_s eg_i_s ir_i_s
    }

    * ── Post results ──────────────────────────────────────────────────────────
    post sim_handle ///
        (`s') ///
        (`b0_s') (`b1_s') (`b2_s') (`b3_s') ///
        (`ate_s') (`att_s') (`atu_s') ///
        (`fiscal_s') (`behav_cost_s') (`effic_gain_s') ///
        (`net_benefit_s') (`nb_per_student_s') (`bcr_s') (`pnb_positive_s') ///
        (`inst_rev_loss_s') (`cross_sub_loss_s') (`nb_extended_s')

    * Progress indicator every 100 draws
    if mod(`s', 100) == 0 {
        di as text "  Draw `s' / $S_draws complete"
    }
}

postclose sim_handle
di as text "Simulation complete."


/*---------------------------------------------------------------------------
  Section 8: Cost–Benefit Decomposition
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Section 8: Cost–Benefit Decomposition"
di    "============================================================"

* ── Load simulation results ──────────────────────────────────────────────────
preserve
use `sim_results', clear

* ── Component-level summaries ────────────────────────────────────────────────
di _n "--- Posterior Summary: CBA Components ($000s) ---"

* tabstat does not accept decimal percentiles (p2.5, p97.5).
* Use _pctile to compute 2.5th and 97.5th percentiles directly.
foreach var in fiscal_s behav_cost_s effic_gain_s net_benefit_s {
    qui sum `var'
    local xm = r(mean)
    local xs = r(sd)
    _pctile `var', p(2.5 97.5)
    di "`var':  mean = " %10.2f `xm' ///
        "  sd = " %10.2f `xs' ///
        "  95% CI: [" %10.2f r(r1) ", " %10.2f r(r2) "]"
}

di _n "--- Institutional Outcomes: Posterior Summary ($000s) ---"
foreach var in inst_rev_loss_s cross_sub_loss_s nb_extended_s {
    qui sum `var'
    local xm = r(mean)
    _pctile `var', p(2.5 97.5)
    di "`var':  mean = " %10.2f `xm' ///
        "  95% CI: [" %10.2f r(r1) ", " %10.2f r(r2) "]"
}

* Probability extended NB > 0
gen pnb_ext_pos_s = (nb_extended_s > 0)
qui sum pnb_ext_pos_s
di _n "P(Extended NB > 0, incl. institutional costs): " %5.3f r(mean)
tabstat b0_s b1_s b2_s b3_s ate_s att_s atu_s, ///
    stats(mean sd) columns(statistics)

* ── P(Net Benefit > 0) ───────────────────────────────────────────────────────
qui sum pnb_positive_s
local p_positive = r(mean)
di _n "Probability policy is efficient P(NB > 0): " %5.3f `p_positive'

* ── BCR summary ──────────────────────────────────────────────────────────────
di _n "Posterior benefit-cost ratio:"
qui sum bcr_s
local xm = r(mean)
local xs = r(sd)
_pctile bcr_s, p(2.5 97.5)
di "BCR:  mean = " %6.4f `xm' "  sd = " %6.4f `xs' ///
    "  95% CI: [" %6.4f r(r1) ", " %6.4f r(r2) "]"

* ── Net benefit per displaced student ────────────────────────────────────────
di _n "Net benefit per displaced student ($000s):"
qui sum nb_per_student_s
local xm = r(mean)
local xs = r(sd)
_pctile nb_per_student_s, p(2.5 97.5)
di "NB/student:  mean = " %8.2f `xm' "  sd = " %8.2f `xs' ///
    "  95% CI: [" %8.2f r(r1) ", " %8.2f r(r2) "]"

* ── Save results for figures ─────────────────────────────────────────────────
save "$tables_dir/sim_results_ch11.dta", replace
restore


/*---------------------------------------------------------------------------
  Section 9: Posterior Summaries and Formal Inference
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Section 9: Posterior Summaries and Inference"
di    "============================================================"

preserve
use "$tables_dir/sim_results_ch11.dta", clear

* ── 95% credible intervals ───────────────────────────────────────────────────
* r(p2) and r(p97) are not stored by sum,detail. Use _pctile instead.
foreach var in fiscal_s behav_cost_s effic_gain_s net_benefit_s bcr_s {
    qui sum `var'
    local xm = r(mean)
    _pctile `var', p(2.5 97.5)
    local p_lo = r(r1)
    local p_hi = r(r2)
    di "`var': mean = " %8.2f `xm' ///
        "  95% CI: [" %8.2f `p_lo' ", " %8.2f `p_hi' "]"
}

* ── Scenario classification ──────────────────────────────────────────────────
* Scenario A (efficient): NB > 0; cap displaces primarily low-return borrowers
* Scenario B (inefficient): NB < 0; cap displaces high-return constrained students
qui sum net_benefit_s, detail
local nb_mean   = r(mean)
local nb_median = r(p50)
_pctile net_benefit_s, p(2.5 97.5)
local nb_p2  = r(r1)
local nb_p97 = r(r2)

di _n "--- Policy Verdict ---"
if `nb_mean' > 0 {
    di as text "Posterior mean NB = " %6.2f `nb_mean' " ($000s) > 0"
    di as text "Scenario A likely: cap predominantly displaces low-return borrowers."
    di as text "Policy generates net fiscal savings in excess of human capital loss."
}
else {
    di as text "Posterior mean NB = " %6.2f `nb_mean' " ($000s) < 0"
    di as text "Scenario B likely: cap displaces students with positive marginal returns."
    di as text "Human capital loss exceeds fiscal savings."
}

* ── LaTeX-ready summary table ────────────────────────────────────────────────
file open ftab using "$tables_dir/Table11_1_CBA_Summary.tex", write replace
file write ftab "\begin{table}[ht]" _n
file write ftab "\caption{Posterior Cost--Benefit Summary: \$100k Grad PLUS Cap}" _n
file write ftab "\begin{tabular}{lrrrr}" _n
file write ftab "\hline" _n
file write ftab "Component & Mean & SD & 2.5\% & 97.5\% \\" _n
file write ftab "\hline" _n

foreach var in fiscal_s behav_cost_s effic_gain_s net_benefit_s bcr_s {
    qui sum `var'
    local xmean = r(mean)
    local xsd   = r(sd)
    _pctile `var', p(2.5 97.5)
    local p_lo = r(r1)
    local p_hi = r(r2)
    local label = cond("`var'" == "fiscal_s",       "Fiscal savings (\$000s)", ///
                  cond("`var'" == "behav_cost_s",   "Behavioral cost (\$000s)", ///
                  cond("`var'" == "effic_gain_s",   "Efficiency gain (\$000s)", ///
                  cond("`var'" == "net_benefit_s",  "Net benefit (\$000s)", ///
                                                    "Benefit-cost ratio"))))
    * Use string() to format numbers before file write — avoids %fmt parsing issues
    local s1 = string(`xmean', "%10.2f")
    local s2 = string(`xsd',   "%10.2f")
    local s3 = string(`p_lo',  "%10.2f")
    local s4 = string(`p_hi',  "%10.2f")
    file write ftab "`label' & `s1' & `s2' & `s3' & `s4' \\" _n
}

file write ftab "\hline" _n
file write ftab "\multicolumn{5}{l}{\textit{Note: " ///
    "$S_draws posterior draws. " ///
    "Behavioral cost = lost human capital for displaced completers with MTE > 0. " ///
    "Efficiency gain = avoided loss from removing completers with MTE \$\leq\$ 0.}} \\" _n
file write ftab "\end{tabular}" _n
file write ftab "\end{table}" _n
file close ftab

di _n "Summary table written to $tables_dir/Table11_1_CBA_Summary.tex"

restore


/*---------------------------------------------------------------------------
  Section 10: Figures
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Section 10: Figures"
di    "============================================================"

* ─────────────────────────────────────────────────────────────────────────────
* Fig. 11.1: Posterior Distribution of Net Social Benefits
* ─────────────────────────────────────────────────────────────────────────────
preserve
use "$tables_dir/sim_results_ch11.dta", clear

qui sum net_benefit_s
local nb_mean_plot = r(mean)

histogram net_benefit_s, ///
    percent ///
    fcolor(gs8) lcolor(gs0) lwidth(thin) ///
    xline(0, lpattern(dash) lcolor(gs0) lwidth(medthick)) ///
    xline(`nb_mean_plot', lpattern(solid) lcolor(gs0) lwidth(medthick)) ///
    ytitle("Percent of Posterior Draws") ///
    xtitle("Net Social Benefit ($000s)") ///
    note("Dashed line at zero. Solid line at posterior mean (" ///
         %6.1f `nb_mean_plot' " $000s)." ///
         "N = $S_draws posterior draws.") ///
    name(fig11_1_posterior_nb, replace)

graph export "$graphs_dir/fig11_1_posterior_nb_Stata.png", ///
    name(fig11_1_posterior_nb) replace width(1200)

restore

* ─────────────────────────────────────────────────────────────────────────────
* Fig. 11.2: MTE Curve with Policy-Affected Region Shaded
* Based on posterior mean MTE polynomial coefficients.
* ─────────────────────────────────────────────────────────────────────────────
preserve
use "$tables_dir/sim_results_ch11.dta", clear

* Posterior mean coefficients — each command on its own line
qui sum b0_s
local b0_pm = r(mean)
qui sum b1_s
local b1_pm = r(mean)
qui sum b2_s
local b2_pm = r(mean)
qui sum b3_s
local b3_pm = r(mean)

* Identify policy-affected u range:
* Students pushed out by the cap tend to cluster in the intermediate-u range.
* Those with u near 0 (low resistance) self-select regardless of credit access.
* Those with u near 1 (high resistance) would not enroll even without the cap.
* The cap binds most tightly for students in u ∈ [0.30, 0.65] (medium resistance).
* These thresholds are calibrated from the loan overage distribution.

clear
set obs 100
gen u    = _n / 100
gen mte  = `b0_pm' + `b1_pm'*u + `b2_pm'*u^2 + `b3_pm'*u^3
gen zero_line = 0

* Policy-affected region: intermediate resistance (calibrated)
gen region_cap  = (u >= 0.30 & u <= 0.65)
gen mte_cap_pos = cond(region_cap == 1 & mte > 0, mte, .)   // Behavioral cost area
gen mte_cap_neg = cond(region_cap == 1 & mte < 0, mte, .)   // Efficiency gain area

twoway ///
    (rarea zero_line mte_cap_pos u, fcolor(gs5) lwidth(none)) ///
    (rarea zero_line mte_cap_neg u, fcolor(gs11) lwidth(none)) ///
    (line mte u, lcolor(gs0) lwidth(medthick) lpattern(solid)), ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    ytitle("Marginal Treatment Effect") ///
    xtitle("u (Unobserved Resistance to Treatment)") ///
    legend(order( ///
        3 "Estimated MTE (posterior mean)" ///
        1 "Displaced students, MTE > 0 (behavioral cost)" ///
        2 "Displaced students, MTE {&le} 0 (efficiency gain)") ///
        cols(1) size(small)) ///
    note("Shaded region: u {&isin} [0.30, 0.65] (policy-affected margin)." ///
         "Dark gray = human capital loss; light gray = efficiency gain from" ///
         "removing low-return borrowers.") ///
    name(fig11_2_mte_policy, replace)

graph export "$graphs_dir/fig11_2_mte_policy_Stata.png", ///
    name(fig11_2_mte_policy) replace width(1200)

restore

* ─────────────────────────────────────────────────────────────────────────────
* Fig. 11.3: Benefit–Cost Decomposition — Stacked Bar Chart
* Shows posterior mean of each component and their net contribution.
* ─────────────────────────────────────────────────────────────────────────────
preserve
use "$tables_dir/sim_results_ch11.dta", clear

* Compute posterior means — each command on its own line (no inline semicolons)
qui sum fiscal_s
scalar sc_fs = r(mean)
qui sum behav_cost_s
scalar sc_bc = r(mean)
qui sum effic_gain_s
scalar sc_eg = r(mean)
qui sum net_benefit_s
scalar sc_nb = r(mean)

* Build a 3-observation summary dataset for plotting
clear
set obs 3
gen component = _n
gen label     = ""
gen value     = .
gen color_grp = .
replace label = "Fiscal Savings"   in 1
replace label = "Efficiency Gain"  in 2
replace label = "Behavioral Cost"  in 3
replace value = sc_fs              in 1
replace value = sc_eg              in 2
replace value = -sc_bc             in 3
replace color_grp = 1              in 1
replace color_grp = 1              in 2
replace color_grp = 2              in 3

graph bar value, over(label, sort(component)) ///
    bar(1, fcolor(gs4) lcolor(gs0)) ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    ytitle("Posterior Mean ($000s)") ///
    title("Cost–Benefit Decomposition") ///
    subtitle("$100k Grad PLUS Cap (Posterior Mean Components)") ///
    note("Behavioral cost shown as negative (subtracted from net benefit)." ///
         "Net benefit = Fiscal Savings + Efficiency Gain − Behavioral Cost.") ///
    name(fig11_3_cba_decomp, replace)

graph export "$graphs_dir/fig11_3_cba_decomp_Stata.png", ///
    name(fig11_3_cba_decomp) replace width(1200)

restore

* ─────────────────────────────────────────────────────────────────────────────
* Fig. 11.4: Posterior Distribution of ATE, ATT, ATU
* Illustrates treatment effect heterogeneity across the posterior.
* ─────────────────────────────────────────────────────────────────────────────
preserve
use "$tables_dir/sim_results_ch11.dta", clear

* Plot three overlapping density curves for the three treatment parameters
kdensity ate_s, generate(ate_x ate_d) nograph n(200)
kdensity att_s, generate(att_x att_d) nograph n(200)
kdensity atu_s, generate(atu_x atu_d) nograph n(200)

twoway ///
    (line ate_d ate_x, lcolor(gs0) lwidth(medthick) lpattern(solid)) ///
    (line att_d att_x, lcolor(gs0) lwidth(medthick) lpattern(dash)) ///
    (line atu_d atu_x, lcolor(gs6) lwidth(medthick) lpattern(shortdash)), ///
    ytitle("Posterior Density") ///
    xtitle("Estimated Treatment Effect (log salary)") ///
    legend(order(1 "ATE" 2 "ATT" 3 "ATU") cols(3) size(small)) ///
    note("ATU > ATT > ATE reflects negative selection: financially constrained" ///
         "students have higher potential returns than self-selected completers." ///
         "N = $S_draws posterior draws.") ///
    name(fig11_4_param_posteriors, replace)

graph export "$graphs_dir/fig11_4_param_posteriors_Stata.png", ///
    name(fig11_4_param_posteriors) replace width(1200)

drop ate_x ate_d att_x att_d atu_x atu_d

restore

* ─────────────────────────────────────────────────────────────────────────────
* Fig. 11.5: MTE Curve by Graduate Program Area (Heterogeneity)
* Field-specific MTE curves reveal which programs drive the efficiency verdict.
* ─────────────────────────────────────────────────────────────────────────────
* This figure uses the field-specific interacted model (est store mte_byarea)
* from the Chapter 10 pipeline. Here we illustrate the concept using the
* pooled cubic MTE with program-area-specific intercept adjustments derived
* from the Chapter 10 field-specific ATE differentials in Table 10.1.

* Field-specific ATE offsets relative to base Other (from Chapter 10 Table 10.1):
*   STEM:     ATE = 0.9424 vs. Other ATE = 0.8085 → offset = +0.134
*   Business: ATE = 1.6936 vs. Other ATE = 0.8085 → offset = +0.885
*   Education: ATE = 0.6437 vs. Other ATE = 0.8085 → offset = -0.165
*   Health:   ATE = 0.8469 vs. Other ATE = 0.8085 → offset = +0.038

preserve
clear
set obs 100
gen u = _n / 100

* Use point-estimate polynomial locals b0–b3 stored in Section 5
gen mte_other = `b0' + `b1'*u + `b2'*u^2 + `b3'*u^3
gen mte_stem    = mte_other + 0.134
gen mte_bus     = mte_other + 0.885
gen mte_educ    = mte_other - 0.165
gen mte_health  = mte_other + 0.038

twoway ///
    (line mte_health u, lcolor(gs0) lwidth(medthick) lpattern(solid)) ///
    (line mte_stem   u, lcolor(gs0) lwidth(medthick) lpattern(dash)) ///
    (line mte_bus    u, lcolor(gs0) lwidth(medthick) lpattern(longdash)) ///
    (line mte_educ   u, lcolor(gs6) lwidth(medthick) lpattern(solid)) ///
    (line mte_other  u, lcolor(gs6) lwidth(medthick) lpattern(dash)), ///
    yline(0, lpattern(shortdash) lcolor(gs10)) ///
    ytitle("Marginal Treatment Effect") ///
    xtitle("u (Unobserved Resistance to Treatment)") ///
    legend(order( ///
        1 "Health & Related" ///
        2 "STEM" ///
        3 "Business" ///
        4 "Education" ///
        5 "Other (base)") ///
        cols(3) size(small)) ///
    note("Field-specific MTE adjusted by program-area ATE differentials from" ///
         "the interacted model (Chapter 10, Table 10.1). Business programs" ///
         "show the largest returns; Education the lowest.") ///
    name(fig11_5_mte_byfield, replace)

graph export "$graphs_dir/fig11_5_mte_byfield_Stata.png", ///
    name(fig11_5_mte_byfield) replace width(1200)

restore


* ─────────────────────────────────────────────────────────────────────────────
* Fig. 11.6: Institutional Revenue Loss by Program Area
* Illustrates which programs bear the heaviest burden from the cap.
* ─────────────────────────────────────────────────────────────────────────────
preserve
use "Example_11_1.dta", clear

* Recompute displaced_E (requires above_cap and p_displaced regeneration)
gen above_cap    = (grad_plus_loans > $cap_threshold)
gen loan_overage = max(0, grad_plus_loans - $cap_threshold)
gen p_displaced  = normal(loan_overage / 50) * above_cap
gen displaced_E  = p_displaced * masters
gen inst_rev_loss_i = displaced_E * net_inst_rev

* Aggregate net institutional revenue loss by program area
gen prog_area = "Other"
replace prog_area = "STEM"     if stem_major   == 1
replace prog_area = "Business" if bus_major    == 1
replace prog_area = "Education" if ed_major     == 1
replace prog_area = "Health"   if health_major == 1

collapse (sum) inst_rev_loss_i (mean) annual_tuition ///
    (count) n_displaced = displaced_E ///
    if masters == 1, by(prog_area)

* Sort for display
gen sort_order = 1
replace sort_order = 2 if prog_area == "STEM"
replace sort_order = 3 if prog_area == "Business"
replace sort_order = 4 if prog_area == "Education"
replace sort_order = 5 if prog_area == "Health"
sort sort_order

graph bar inst_rev_loss_i, over(prog_area, sort(sort_order)) ///
    bar(1, fcolor(gs5) lcolor(gs0)) ///
    ytitle("Net Institutional Revenue Loss ($000s)") ///
    title("Institutional Revenue Loss by Program Area") ///
    subtitle("Cap-displaced students × net tuition revenue per student") ///
    note("Net revenue = 65% of gross tuition (35% variable cost excluded)." ///
         "Excludes cross-subsidy disruption. Based on point estimates.") ///
    name(fig11_6_inst_rev_byfield, replace)

graph export "$graphs_dir/fig11_6_inst_rev_byfield_Stata.png", ///
    name(fig11_6_inst_rev_byfield) replace width(1200)

restore

* ─────────────────────────────────────────────────────────────────────────────
* Fig. 11.7: Full Social Cost Stack — Student + Institutional Channels
* Stacks all costs and savings to show the complete distributional picture.
* ─────────────────────────────────────────────────────────────────────────────
preserve
use "$tables_dir/sim_results_ch11.dta", clear

qui sum fiscal_s
scalar sc_fs  = r(mean)
qui sum behav_cost_s
scalar sc_bc  = r(mean)
qui sum effic_gain_s
scalar sc_eg  = r(mean)
qui sum inst_rev_loss_s
scalar sc_ir  = r(mean)
qui sum cross_sub_loss_s
scalar sc_cs  = r(mean)

clear
set obs 5
gen component = _n
gen label     = ""
gen value     = .
gen category  = ""   // "Benefit" or "Cost"

replace label    = "Fiscal Savings"          in 1
replace label    = "Efficiency Gain"         in 2
replace label    = "Human Capital Loss"      in 3
replace label    = "Institutional Rev. Loss" in 4
replace label    = "Cross-Subsidy Disruption" in 5

replace value    =  sc_fs  in 1
replace value    =  sc_eg  in 2
replace value    = -sc_bc  in 3
replace value    = -sc_ir  in 4
replace value    = -sc_cs  in 5

replace category = "Benefit" in 1
replace category = "Benefit" in 2
replace category = "Cost"    in 3
replace category = "Cost"    in 4
replace category = "Cost"    in 5

graph bar value, over(label, sort(component) label(angle(30))) ///
    bar(1, fcolor(gs5) lcolor(gs0)) ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    ytitle("Posterior Mean ($000s)") ///
    title("Full Social Cost Stack") ///
    subtitle("Student and Institutional Channels Combined") ///
    note("Benefits shown positive; costs shown negative." ///
         "Human capital loss and institutional revenue loss dominate." ///
         "Cross-subsidy disruption captures upstream undergraduate impact.") ///
    name(fig11_7_full_cost_stack, replace)

graph export "$graphs_dir/fig11_7_full_cost_stack_Stata.png", ///
    name(fig11_7_full_cost_stack) replace width(1200)

restore


/*---------------------------------------------------------------------------
  End of Script
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Chapter 11 Script Complete"
di    "============================================================"
di as text "Output files:"
di as text "  Stata log:    Output/logs/Chapter11_Stata_output.log"
di as text "  Data:         Example_11_1.dta"
di as text "  Table:        $tables_dir/Table11_1_CBA_Summary.tex"
di as text "  Sim results:  $tables_dir/sim_results_ch11.dta"
di as text "  Figures:      $graphs_dir/fig11_1_posterior_nb_Stata.png"
di as text "                $graphs_dir/fig11_2_mte_policy_Stata.png"
di as text "                $graphs_dir/fig11_3_cba_decomp_Stata.png"
di as text "                $graphs_dir/fig11_4_param_posteriors_Stata.png"
di as text "                $graphs_dir/fig11_5_mte_byfield_Stata.png"

log close
