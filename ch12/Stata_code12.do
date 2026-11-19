/*===========================================================================
  Chapter 12: Bayesian MTE Microsimulation
  Cost–Benefit Analysis of the $100k Aggregate Limit on Federal Graduate Loans
  
  Book:   Higher Education Policy Analysis Using Quantitative Techniques
          Second Edition — Springer
  
  Code development assisted by Claude (Anthropic, 2025). The author provided
  methodological specifications and reviewed, tested, and validated all code
  and output.
  
  Repository:
    https://github.com/higher-ed-policy-analysis-2nd-edition/code/blob/main/ch12/Stata_code12.do
  
  Overview:
    This script evaluates the net social benefit of the $100,000 aggregate
    limit on federal graduate borrowing enacted in the One Big Beautiful
    Bill Act (OBBBA; Pub. L. 119-21), effective July 1, 2026. The OBBBA
    ends Grad PLUS loans for new borrowers and limits Direct Unsubsidized
    loans for students in non-professional graduate programs to $20,500 a
    year and $100,000 in total (excluding undergraduate loans). This script
    models the $100,000 aggregate limit only (variable names such as
    grad_plus_loans and "cap" are retained from earlier drafts; the annual
    limit is not modeled). Using the marginal treatment effect (MTE)
    framework from Chapter 11, we estimate
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
    Section 5.  MTE estimation — quadratic polynomial control function
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

  REVISION (Nov 2026): comment, labeling, and reporting changes only. No
  estimate, simulation draw, or figure value changes, EXCEPT that
  (a) the working-directory cd now runs only for the author's machine,
  (b) Section 3 and Section 6 now report exposure and institutional
      revenue loss for all five program areas (adds Health and Other),
  (c) Section 5 adds a heterogeneity test of the three MTE interaction
      terms, and
  (d) Fig. 12.5's on-figure note no longer cites "Chapter 10, Table 10.1".
  Corrected comments: policy description, Chapter 10 -> Chapter 11
  references, data provenance, displacement-function behavior, the
  joint-test interpretation, and the MPRTE-vs-ATT display text.
  Items flagged "UNDER REVIEW" change results and await the author's
  decision; they are described where they occur.

  REVISION 2 (Nov 2026): MTE corrections. THESE CHANGE RESULTS.
  (1) MTE (Section 5): with the treatment indicator interacted with the
      propensity-score polynomial, the beta polynomial is NOT the MTE. The
      MTE is MTE(u) = beta(u) + u*beta'(u) + pi'(u), where pi(.) is the
      polynomial on phat, phat2, phat3 (the control function). For the
      cubic, MTE(u) = m0 + m1*u + m2*u^2 + m3*u^3 with m0 = b0 + c1,
      m1 = 2(b1 + c2), m2 = 3(b2 + c3), m3 = 4*b3 (Eq. 12.3).
  (2) ATE, ATT, ATU now use the standard weights (Chapter 11, Eq. 11.3):
      ATE = int_0^1 MTE; ATT = sum_i I(p_i) / sum_i p_i;
      ATU = sum_i [I(1) - I(p_i)] / sum_i (1 - p_i), where
      I(p) = int_0^p MTE(u) du.
  (3) A displaced completer's expected return is TT(p_i) = I(p_i)/p_i, the
      average MTE over u < p_i, because a completer's resistance lies below
      his or her propensity score and displacement depends on the loan
      overage, not on resistance. TT replaces MTE(phat) in the PRTE
      ("MPRTE"), behavioral cost, and efficiency gain, and in every draw.
  (4) DGP (Section 2): resistance is now U_D = normal(-epsilon), which is
      uniform and satisfies D = 1 iff U_D < P(Z,X). The old definition,
      u = 1 - normal(index), mixed observables and the instrument into the
      treatment effect, so GA funding affected salaries directly and no MTE
      estimator could recover the true effects. The SAME one-line change
      must be made in Generate_Example_12_1.do and Example_12_1.dta
      regenerated and re-uploaded; until then this script downloads the old
      canonical file.
  Formulas verified by Monte Carlo (corrected estimator recovers the true
  MTE, ATE, ATT, ATU; the previous beta-only version does not).

  REVISION 3 (Nov 2026): QUADRATIC control function (as in Chapter 11's
  pol(2)) and recalibrated DGP (see Generate_Example_12_1.do, REVISION 3).
  Eq. 12.2 now uses phat and phat2 and their interactions with masters;
  the MTE is MTE(u) = m0 + m1*u + m2*u^2 with m0 = b0 + c1,
  m1 = 2(b1 + c2), m2 = 3*b2 (Eq. 12.3); this supersedes the cubic
  formulas in item (1) of REVISION 2. Point-estimate net benefit
  display widened (was printing as -1.4e+05).

  REVISION 4 (Nov 2026): (a) Section 6 adds a sensitivity check of the
  point-estimate CBA: two displacement functions (baseline normal(o/50),
  which starts at 0.50; alternative 2*normal(o/50) - 1, which starts at 0)
  crossed with two earnings horizons (30 and 10 years). (b) Fig. 12.5's
  program-area offsets are now estimated from the data (masters x program
  area intercept shifts in the quadratic MTE regression) instead of being
  hard-coded. No other results change.

  REVISION 5 (Nov 2026): the two joint tests in Section 5 now test the MTE
  coefficients. "No effect" is m0 = m1 = m2 = 0 and "MTE flat" is
  m1 = m2 = 0 (b2 = 0 and b1 + c2 = 0). The earlier tests (beta terms = 0)
  were not tests of these hypotheses under the control-function approach.
  No estimates change; only the two test statistics do.

  REVISION 6 (Nov 2026): the CBA is reorganized into three accounts --
  students, the federal government, and institutions -- plus a social
  total in which transfers (tuition, loans, subsidies, taxes) net out.
  New items: lost tax revenue, avoided tuition, avoided debt service, and
  avoided instructional cost; subsidy savings now cover all borrowing by
  displaced students. The cross-subsidy is now reported as part of the
  institutional revenue loss rather than added to it, and the "extended"
  net benefit is dropped (it double-counted tuition). New global tax_rate
  (prior: uniform 0.15-0.25). Table 12.1 and Figs. 12.3 and 12.7 change.

  REVISION 7 (Nov 2026): tables are written as Word files with putdocx
  (built into Stata) instead of LaTeX: Table 12.1 (descriptive statistics,
  Panel A student-level and Panel B institution- and state-level variables,
  Section 3), Table 12.2 (simulation assumptions, Section 6),
  and Table 12.3 (posterior CBA summary, Section 9; formerly Table 12.1).
  The prior settings, the cross-subsidy share, and the tax-rate range are
  now globals, so Table 12.2 always matches the values the code uses.
  No results change.
===========================================================================*/


/*---------------------------------------------------------------------------
  Section 1: Setup
---------------------------------------------------------------------------*/

version 19
set more off
set seed 20251201

* ── Working directory ────────────────────────────────────────────────────────
* The author's machine uses the Dropbox Chapter 12 folder. Other users run
* the script from the folder where they want the data and Output/ written.
if c(username) == "marvi" {
    cd "C:\Users\marvi\Dropbox\Book\2nd Edition\Chapter 12"
}
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
log using "Output/logs/Chapter12_Stata_output.log", replace text
di "Log file: " c(pwd) "/Output/logs/Chapter12_Stata_output.log"
di "Chapter 12 log opened: " c(current_date) " " c(current_time)

* ── Global macros ───────────────────────────────────────────────────────────
* Individual-level controls (same set as Chapter 11 for consistency)
global X_controls "female black hispanic asian age_ba firstgen parent_income_q parent_grad ugpa stem_major bus_major ed_major selective_inst public_ug state_unemp metro"
global X "$X_controls"
global Z "ga_funding_adj"

* CBA parameters
global discount_rate   0.03    // Annual discount rate (3%)
global career_years    30      // Earnings horizon (years)
global subsidy_rate    0.20    // Government subsidy on Grad PLUS principal
global tax_rate        0.20    // Marginal tax rate on forgone earnings (REVISION 6)
global cap_threshold   100     // Policy cap ($000s)
global base_salary     47000   // Pooled mean annual salary (non-holders)
global cost_per_degree 100000  // Not used in any calculation (retained for reference)
global S_draws         1000     // Number of Bayesian posterior draws
global cross_sub_share 0.20    // Share of net revenue that cross-subsidizes other programs
* Prior settings for the posterior simulation (Section 7; Table 12.2)
global prior_sd_salary  0.10   // SD of log base salary
global prior_hw_subsidy 0.05   // Half-width of uniform prior on subsidy rate
global prior_sd_tuition 0.08   // SD of log tuition multiplier
global tax_lo           0.15   // Tax-rate prior: uniform lower bound
global tax_hi           0.25   // Tax-rate prior: uniform upper bound


/*---------------------------------------------------------------------------
  Section 2: Synthetic Data Generation
  
  Population: 8,000 graduate students drawn from a B&B-mirroring DGP.
  The dataset is generated separately from the Chapter 11 data
  (Example_7_5_3_updated.dta); it reuses many of the same variable names
  but not the same draws, program areas, or GA funding distribution. The
  canonical file is produced by Generate_Example_12_1.do; the block below
  is an identical copy that runs only if the GitHub download fails. Keep
  the two in sync if the DGP is changed.
  Federal graduate borrowing (grad_plus_loans, $000s) depends on program
  area, institutional selectivity, family income, GA funding, and a
  student-specific unobserved borrowing propensity.
  Unlike Chapter 11, where only completers have a master's program area
  (ma_*), every student here is assigned the graduate program area they
  are enrolled in or would enter (stem_major, bus_major, ed_major,
  health_major; Other is the base), because borrowing and tuition depend
  on the program.
  
  GitHub data repository:
    https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch12
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Section 2: Synthetic Data Generation"
di    "============================================================"

* ── Attempt to load from repository; generate synthetically if unavailable ──
capture copy ///
    "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch12/Example_12_1.dta" ///
    "Example_12_1.dta", replace

if _rc != 0 {
    di as text "Remote data not found. Generating synthetic dataset."

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
    gen nu      = rnormal(0, 1)    // Not used; retained so the random-number
                                   //   sequence matches Generate_Example_12_1.do

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

    * ── Drop construction intermediates ─────────────────────────────────────────
    drop prog_draw loan_noise epsilon nu index_latent phat_true u_true ///
         mte_true Y1_latent Y0_latent

    * ── Save synthetic dataset ──────────────────────────────────────────────
    save "Example_12_1.dta", replace
    di as text "Synthetic dataset saved as Example_12_1.dta"
}
else {
    di as text "Canonical dataset downloaded from GitHub repository."
}

* ── Always load the dataset (whether downloaded or just generated) ───────────
use "Example_12_1.dta", clear


/*---------------------------------------------------------------------------
  Section 3: Descriptive Statistics and Policy Exposure
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Section 3: Descriptive Statistics and Policy Exposure"
di    "============================================================"

use "Example_12_1.dta", clear

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
* All five graduate program areas, including Health and the Other base.
* The above_cap mean is the share of ALL students in each area (completers
* and non-completers) borrowing above the limit, not a completer-only share.
gen prog_area = 5
replace prog_area = 1 if stem_major   == 1
replace prog_area = 2 if bus_major    == 1
replace prog_area = 3 if ed_major     == 1
replace prog_area = 4 if health_major == 1
label define prog_area_lbl 1 "STEM" 2 "Business" 3 "Education" 4 "Health" 5 "Other", replace
label values prog_area prog_area_lbl
label variable prog_area "Graduate program area"

di _n "Mean federal graduate borrowing and share above the limit, by program area:"
tabstat grad_plus_loans above_cap, by(prog_area) stats(mean n)

* ── Table 12.1: Descriptive statistics (Word) ──────────────────────────────
* Panel A: student-level variables; Panel B: institution- and state-level
* variables. Mean, SD, minimum, and maximum for all students, plus means for
* completers and non-completers. Written with putdocx so it pastes into Word.
gen other_major = 1 - stem_major - bus_major - ed_major - health_major

* Row list: a word starting with "@" is a panel heading ("_" = space)
local t1rows "@Panel_A:_Student-level_variables ln_salary salary female black"
local t1rows "`t1rows' hispanic asian age_ba firstgen parent_income_q parent_grad ugpa"
local t1rows "`t1rows' stem_major bus_major ed_major health_major other_major"
local t1rows "`t1rows' grad_plus_loans above_cap"
local t1rows "`t1rows' @Panel_B:_Institution-_and_state-level_variables selective_inst"
local t1rows "`t1rows' public_ug annual_tuition program_years net_inst_rev"
local t1rows "`t1rows' ga_funding_adj state_unemp metro"
local t1bin "female black hispanic asian firstgen parent_grad stem_major bus_major"
local t1bin "`t1bin' ed_major health_major other_major selective_inst public_ug metro above_cap"

local lab_ln_salary       "Log annual salary"
local lab_salary          "Annual salary ($)"
local lab_female          "Female"
local lab_black           "Black"
local lab_hispanic        "Hispanic"
local lab_asian           "Asian"
local lab_age_ba          "Age at bachelor's degree"
local lab_firstgen        "First-generation student"
local lab_parent_income_q "Parental income quartile (1-4)"
local lab_parent_grad     "Parent holds a graduate degree"
local lab_ugpa            "Undergraduate GPA"
local lab_stem_major      "Program area: STEM"
local lab_bus_major       "Program area: Business"
local lab_ed_major        "Program area: Education"
local lab_health_major    "Program area: Health"
local lab_other_major     "Program area: Other"
local lab_grad_plus_loans "Federal graduate borrowing ($000s)"
local lab_above_cap       "Borrowing above the $100,000 limit"
local lab_selective_inst  "Selective institution"
local lab_public_ug       "Public undergraduate institution"
local lab_annual_tuition  "Annual tuition ($000s)"
local lab_program_years   "Program length (years)"
local lab_net_inst_rev    "Net institutional revenue per student ($000s)"
local lab_ga_funding_adj  "State GA funding (instrument)"
local lab_state_unemp     "State unemployment rate (%)"
local lab_metro           "Metropolitan area"

local nrow : word count `t1rows'
local R1 = `nrow' + 3                       // header + rows + N + note
putdocx clear
putdocx begin, font("Times New Roman", 10)
putdocx paragraph
putdocx text ("Table 12.1 "), bold
putdocx text ("Descriptive statistics")
putdocx table t1 = (`R1', 7), border(all, nil) layout(autofitcontents)
putdocx table t1(1,1) = ("Variable")
local c = 1
foreach h in "Mean" "SD" "Min" "Max" "Completers" "Non-completers" {
    local ++c
    putdocx table t1(1,`c') = ("`h'"), halign(right)
}
local r = 1
foreach w of local t1rows {
    local ++r
    if substr("`w'", 1, 1) == "@" {
        local g = subinstr(substr("`w'", 2, .), "_", " ", .)
        putdocx table t1(`r',1) = ("`g'"), italic
        continue
    }
    putdocx table t1(`r',1) = ("    `lab_`w''")
    local isbin : list w in t1bin
    qui sum `w'
    local xm  = r(mean)
    local xsd = r(sd)
    local xlo = r(min)
    local xhi = r(max)
    * Formats: binary = 3 decimals (min/max 0); large amounts = 0; others = 2
    if `isbin' {
        local fm "%9.3f"
        local fx "%9.0f"
    }
    else if abs(`xm') >= 1000 {
        local fm "%12.0fc"
        local fx "%12.0fc"
    }
    else {
        local fm "%12.2fc"
        local fx "%12.2fc"
    }
    qui sum `w' if masters == 1
    local xm1 = r(mean)
    qui sum `w' if masters == 0
    local xm0 = r(mean)
    putdocx table t1(`r',2) = (strtrim(string(`xm',  "`fm'"))), halign(right)
    putdocx table t1(`r',3) = (strtrim(string(`xsd', "`fm'"))), halign(right)
    putdocx table t1(`r',4) = (strtrim(string(`xlo', "`fx'"))), halign(right)
    putdocx table t1(`r',5) = (strtrim(string(`xhi', "`fx'"))), halign(right)
    putdocx table t1(`r',6) = (strtrim(string(`xm1', "`fm'"))), halign(right)
    putdocx table t1(`r',7) = (strtrim(string(`xm0', "`fm'"))), halign(right)
}
local ++r
putdocx table t1(`r',1) = ("N")
local nall = strtrim(string(_N, "%9.0fc"))
qui count if masters == 1
local n1 = strtrim(string(r(N), "%9.0fc"))
qui count if masters == 0
local n0 = strtrim(string(r(N), "%9.0fc"))
putdocx table t1(`r',2) = ("`nall'"), halign(right)
putdocx table t1(`r',6) = ("`n1'"), halign(right)
putdocx table t1(`r',7) = ("`n0'"), halign(right)
local ++r
putdocx table t1(`r',1) = ("Note: Completers and Non-completers columns report means. Binary variables are shown as proportions. Panel B characteristics are attached to each student's record; the data contain no institution or state identifiers. Amounts marked $000s are in thousands of dollars. Source: Synthetic data (Example_12_1.dta), available in the book's GitHub data repository (see Sect. 12.2.3)."), colspan(7)
putdocx table t1(1,.), border(top)
putdocx table t1(1,.), border(bottom)
putdocx table t1(`=`R1'-1',.), border(bottom)
putdocx save "$tables_dir/Table12_1_Descriptives_Stata.docx", replace
di as text "Table 12.1 written to $tables_dir/Table12_1_Descriptives_Stata.docx"


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

* Polynomial term for the quadratic control function
gen phat2 = phat^2

summarize phat, detail
di "GA funding coefficient (probit): " _b[ga_funding_adj]
local ga_coef = _b[ga_funding_adj]

* Propensity score distribution among completers and non-completers
tabstat phat, by(masters) stats(mean sd p10 p50 p90)


/*---------------------------------------------------------------------------
  Section 5: MTE Estimation — Quadratic Polynomial Control Function
  
  Following Heckman and Vytlacil (1999, 2005), the MTE is recovered from
  the derivative of the outcome regression with respect to the propensity
  score. The polynomial control function approach is used here for
  transparency and direct estimation of ATE, ATT, and ATU.
  
  MTE(u) = m0 + m1*u + m2*u^2   (Eq. 12.3)
  
  where u is unobserved resistance to treatment and the m coefficients are
  built from the coefficients on masters, c.masters#c.phat,
  c.masters#c.phat2 (beta) and on phat, phat2 (pi, the control function).
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Section 5: MTE Estimation"
di    "============================================================"

* ── Quadratic MTE regression (Eq. 12.2) ─────────────────────────────────────
reg ln_salary masters c.masters#(c.phat c.phat2) ///
    $X phat phat2, robust
estimates store mte_quad

* ── Joint tests for the MTE polynomial ──────────────────────────────────────
* Individual coefficients are often imprecise due to multicollinearity among
* polynomial terms, so joint tests are used.
* The tests are on the MTE coefficients (Eq. 12.3), m0 = b0 + c1,
* m1 = 2(b1 + c2), m2 = 3*b2, not on the beta terms alone (REVISION 5).
* (1) MTE(u) = 0 for every u (m0 = m1 = m2 = 0): completion has no effect
*     on log salary for anyone.
test (masters + phat = 0) (c.masters#c.phat + phat2 = 0) (c.masters#c.phat2 = 0)
di as text "Joint F-test, no effect (MTE = 0) p-value: " %6.4f r(p)

* (2) The MTE is flat in u (m1 = m2 = 0). Rejecting this is evidence of
*     essential heterogeneity in returns.
test (c.masters#c.phat + phat2 = 0) (c.masters#c.phat2 = 0)
di as text "Heterogeneity F-test (MTE flat in u) p-value: " %6.4f r(p)

* ── Store regression coefficients (Eq. 12.2) ────────────────────────────────
* beta polynomial: completion and its interactions with the phat polynomial
local b0 = _b[masters]
local b1 = _b[c.masters#c.phat]
local b2 = _b[c.masters#c.phat2]
* pi polynomial (control function): coefficients on phat, phat2
local c1 = _b[phat]
local c2 = _b[phat2]

* ── MTE (Eq. 12.3) ──────────────────────────────────────────────────────────
* In Eq. 12.2, E[Y | D=0, p] contains pi(p) and E[Y | D=1, p] contains
* pi(p) + beta(p). Under the control-function (separate) approach
* (Heckman & Vytlacil, 2005; Andresen, 2018), the MTE is
*     MTE(u) = beta(u) + u*beta'(u) + pi'(u),
* which for the quadratic gives MTE(u) = m0 + m1*u + m2*u^2:
local m0 = `b0' + `c1'
local m1 = 2*(`b1' + `c2')
local m2 = 3*`b2'

di _n "MTE polynomial: MTE(u) = " %7.4f `m0' " + " %7.4f `m1' "*u" ///
    " + " %7.4f `m2' "*u^2"

* ── Treatment parameters (Chapter 11, Eq. 11.3 weights) ─────────────────────
* I(p) = integral of MTE(u) du from 0 to p
*      = m0*p + m1*p^2/2 + m2*p^3/3
* ATE  = I(1)
* ATT  = sum_i I(p_i) / sum_i p_i              (average MTE over u < P)
* ATU  = sum_i [I(1) - I(p_i)] / sum_i (1-p_i) (average MTE over u > P)
local ate = `m0' + `m1'/2 + `m2'/3
di "Estimated ATE: " %6.4f `ate'

gen double I_hat = `m0'*phat + `m1'*phat^2/2 + `m2'*phat^3/3
qui sum I_hat
local sum_I = r(sum)
qui sum phat
local sum_p = r(sum)
local n_obs = r(N)
local att = `sum_I' / `sum_p'
local atu = (`n_obs'*`ate' - `sum_I') / (`n_obs' - `sum_p')
di "Estimated ATT: " %6.4f `att'
di "Estimated ATU: " %6.4f `atu'

* Expected return for a completer with propensity score p:
* TT(p) = I(p)/p, the average MTE over resistance levels below p. This is the
* return a completer forfeits if displaced (used in Sections 6 and 7).
gen double tt_hat = I_hat / phat
label variable tt_hat "Expected return to completion for a completer at phat"

* Check: ATE = E[P]*ATT + (1 - E[P])*ATU (should match ATE)
di "Check (should equal ATE): " %6.4f ///
    ((`sum_p'/`n_obs')*`att' + (1 - `sum_p'/`n_obs')*`atu')

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
  of the loan overage: p_out(overage) = Φ(overage / 50). The probability
  rises smoothly with the overage, but because Φ(0) = 0.5 every student
  above the limit has at least a 50% displacement probability, even one
  $1 over (the log minimum is 0.5002). UNDER REVIEW (outline Author
  note 2): alternatives such as 2Φ(overage/50) − 1 start near zero.
  
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

* ── Policy-relevant treatment effect for displaced students ("MPRTE") ───────
* Displacement-weighted average of each completer's expected return,
* TT(phat) = I(phat)/phat from Section 5 (REVISION 2). Variable names keep
* the "mprte" label from earlier drafts; conceptually this is a PRTE for a
* discrete policy (see Chapter 11, Table 11.4).
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
gen mprte_i      = tt_hat * mprte_weight    if masters == 1
qui sum mprte_i  if masters == 1
scalar mprte = r(sum)

di _n "--- Treatment Parameter Hierarchy (Point Estimates) ---"
di "ATE   (population average):           " %6.4f `ate'
di "ATT   (E[MTE | D=1], completers):     " %6.4f `att'
di "ATU   (E[MTE | D=0], non-completers): " %6.4f `atu'
di "MPRTE (cap-displaced margin):         " %6.4f mprte
di _n "MPRTE compares the average return of displaced students with the"
di    "average return of all completers (ATT). If MPRTE > ATT, the cap removes"
di    "students with above-average returns among completers (efficiency loss)."

* ── Present value factor ─────────────────────────────────────────────────────
local pv_factor = (1 - (1 + $discount_rate)^(-$career_years)) / $discount_rate
di "Present value factor (" $career_years " yrs, " $discount_rate*100 "% r): " ///
    %6.3f `pv_factor'

* ── Table 12.2: Simulation assumptions (Word) ───────────────────────────────
* Built from the globals at the top of the script and from the data, so the
* table always matches the values the code uses.
qui sum net_inst_rev
local net_tot = r(sum)
qui sum gross_tuition
local instr_share = 1 - `net_tot' / r(sum)
local pct = "%"
putdocx clear
putdocx begin, font("Times New Roman", 10)
putdocx paragraph
putdocx text ("Table 12.2 "), bold
putdocx text ("Simulation assumptions")
putdocx table t2 = (15, 4), border(all, nil) layout(autofitwindow)
putdocx table t2(1,1) = ("Parameter")
putdocx table t2(1,2) = ("Baseline value")
putdocx table t2(1,3) = ("Prior distribution (Sect. 12.6)")
putdocx table t2(1,4) = ("Sensitivity check (Sect. 12.7.3)")
putdocx table t2(2,1) = ("Policy and behavior"), italic
putdocx table t2(3,1) = ("    Aggregate federal loan limit")
putdocx table t2(3,2) = (char(36) + strtrim(string(1000*$cap_threshold, "%12.0fc")))
putdocx table t2(3,3) = ("Fixed")
putdocx table t2(3,4) = ("None")
putdocx table t2(4,1) = ("    Probability that a completer above the limit leaves")
putdocx table t2(4,2) = ("Φ(overage / 50), overage in $000s")
putdocx table t2(4,3) = ("Fixed")
putdocx table t2(4,4) = ("2Φ(overage / 50) − 1")
putdocx table t2(5,1) = ("Earnings"), italic
putdocx table t2(6,1) = ("    Base salary (non-degree holders)")
putdocx table t2(6,2) = (char(36) + strtrim(string($base_salary, "%12.0fc")))
putdocx table t2(6,3) = ("Log-normal, SD " + strtrim(string($prior_sd_salary, "%4.2f")) + " on the log scale")
putdocx table t2(6,4) = ("None")
putdocx table t2(7,1) = ("    Earnings horizon")
putdocx table t2(7,2) = ("$career_years years")
putdocx table t2(7,3) = ("Fixed")
putdocx table t2(7,4) = ("10 years")
putdocx table t2(8,1) = ("    Discount rate")
putdocx table t2(8,2) = (strtrim(string(100*$discount_rate, "%4.0f")) + "`pct'")
putdocx table t2(8,3) = ("Fixed")
putdocx table t2(8,4) = ("None")
putdocx table t2(9,1) = ("Federal government"), italic
putdocx table t2(10,1) = ("    Federal subsidy rate")
putdocx table t2(10,2) = (strtrim(string(100*$subsidy_rate, "%4.0f")) + "`pct'")
putdocx table t2(10,3) = ("Uniform, " + strtrim(string(100*($subsidy_rate - $prior_hw_subsidy), "%4.0f")) + "`pct' to " + strtrim(string(100*($subsidy_rate + $prior_hw_subsidy), "%4.0f")) + "`pct'")
putdocx table t2(10,4) = ("None")
putdocx table t2(11,1) = ("    Tax rate on forgone earnings")
putdocx table t2(11,2) = (strtrim(string(100*$tax_rate, "%4.0f")) + "`pct'")
putdocx table t2(11,3) = ("Uniform, " + strtrim(string(100*$tax_lo, "%4.0f")) + "`pct' to " + strtrim(string(100*$tax_hi, "%4.0f")) + "`pct'")
putdocx table t2(11,4) = ("None")
putdocx table t2(12,1) = ("Institutions"), italic
putdocx table t2(13,1) = ("    Tuition")
putdocx table t2(13,2) = ("Program values in the data")
putdocx table t2(13,3) = ("Log-normal multiplier, SD " + strtrim(string($prior_sd_tuition, "%4.2f")))
putdocx table t2(13,4) = ("None")
putdocx table t2(14,1) = ("    Instructional cost share of tuition; cross-subsidy share of net revenue")
putdocx table t2(14,2) = (strtrim(string(100*`instr_share', "%4.0f")) + "`pct'; " + strtrim(string(100*$cross_sub_share, "%4.0f")) + "`pct'")
putdocx table t2(14,3) = ("Fixed")
putdocx table t2(14,4) = ("None")
putdocx table t2(15,1) = ("Note: MTE coefficients are drawn from a normal distribution centered on the estimates (Eq. 12.3), with the robust variance-covariance matrix. The simulation uses $S_draws posterior draws. Source: Parameter settings in Stata_code12.do and R_code12.R, available in the book's GitHub code repository."), colspan(4)
putdocx table t2(1,.), border(top)
putdocx table t2(1,.), border(bottom)
putdocx table t2(14,.), border(bottom)
putdocx save "$tables_dir/Table12_2_Assumptions_Stata.docx", replace
di as text "Table 12.2 written to $tables_dir/Table12_2_Assumptions_Stata.docx"

* ── Point-estimate CBA by account (REVISION 6) ──────────────────────────────
* Three accounts plus a social total. Amounts are present values in $000s.
*
*   Students:     - after-tax forgone earnings (+ after-tax efficiency gain)
*                 + avoided tuition (displaced students no longer pay it)
*                 + avoided debt service (repayments they no longer make)
*                 - forgone loan disbursements (loans they no longer receive)
*                 - higher financing cost for students who stay above the
*                   limit (the subsidy they lose on borrowing above $100k)
*   Federal:      + subsidy savings (all borrowing by displaced students,
*                   plus the overage of students who stay)
*                 - lost tax revenue on forgone earnings
*   Institutions: - lost net tuition revenue (gross tuition minus the
*                   variable instructional cost avoided)
*   Society:      sum of the three accounts. Tuition, loans, subsidies, and
*                 taxes are transfers between accounts and net out, leaving
*                 - forgone earnings + efficiency gain + avoided
*                   instructional cost.
*
* The present value of repayments on a dollar borrowed is (1 - subsidy
* rate), so avoided debt service and the subsidy are consistent.

* Forgone earnings (pre-tax): TT(phat) x base_salary x PV factor ($000s)
gen behav_cost_i = displaced_E * max(0, tt_hat) * ($base_salary/1000) * `pv_factor'
* Note: max(0, TT) reflects that only students with positive returns suffer a
* human capital loss. Displaced students with negative TT represent an
* efficiency GAIN (removing low-return borrowers).
qui sum behav_cost_i if masters == 1
local BC_point = r(sum)
gen efficiency_gain_i = displaced_E * max(0, -tt_hat) * ($base_salary/1000) * `pv_factor'
qui sum efficiency_gain_i if masters == 1
local EG_point = r(sum)

* Federal subsidy savings
gen fiscal_saving_i = $subsidy_rate * (displaced_E * grad_plus_loans ///
    + (above_cap - displaced_E) * loan_overage)
qui sum fiscal_saving_i
local FS_point = r(sum)
* Lost tax revenue on (net) forgone earnings
local TL_point = $tax_rate * (`BC_point' - `EG_point')

* Students' tuition and loans (displaced completers)
gen avoid_tuition_i = displaced_E * gross_tuition
qui sum avoid_tuition_i
local AT_point = r(sum)
gen loans_disp_i = displaced_E * grad_plus_loans
qui sum loans_disp_i
local LD_point = r(sum)
local DS_point = (1 - $subsidy_rate) * `LD_point'     // avoided debt service
gen stay_cost_i = $subsidy_rate * (above_cap - displaced_E) * loan_overage
qui sum stay_cost_i
local SC_point = r(sum)

* Institutions: lost net tuition revenue; cross-subsidy is part of it
* As implemented, the loss covers the FULL program (all program years); if
* displaced students leave partway through, this overstates it (UNDER REVIEW).
gen inst_rev_loss_i   = displaced_E * net_inst_rev
gen cross_sub_loss_i  = $cross_sub_share * inst_rev_loss_i  // of which: cross-subsidy
gen avoid_instr_i     = displaced_E * (gross_tuition - net_inst_rev)
qui sum inst_rev_loss_i  if masters == 1
local IR_point = r(sum)
qui sum cross_sub_loss_i if masters == 1
local CS_point = r(sum)
qui sum avoid_instr_i    if masters == 1
local AI_point = r(sum)

* Account totals
local STU_point = -(1 - $tax_rate)*(`BC_point' - `EG_point') + `AT_point' ///
                  + `DS_point' - `LD_point' - `SC_point'
local FED_point = `FS_point' - `TL_point'
local INS_point = -`IR_point'
local NB_point  = `STU_point' + `FED_point' + `INS_point'
local BCR_point = (`AI_point' + `EG_point') / max(1, `BC_point')

di _n "--- Point-estimate CBA by account ($000s) ---"
di "Students"
di "  Forgone after-tax earnings              " %12.1fc -(1 - $tax_rate)*(`BC_point' - `EG_point')
di "  Avoided tuition                         " %12.1fc `AT_point'
di "  Avoided debt service                    " %12.1fc `DS_point'
di "  Forgone loan disbursements              " %12.1fc -`LD_point'
di "  Higher financing cost (students who stay)" %11.1fc -`SC_point'
di "  Student account                         " %12.1fc `STU_point'
di "Federal government"
di "  Subsidy savings                         " %12.1fc `FS_point'
di "  Lost tax revenue                        " %12.1fc -`TL_point'
di "  Federal account                         " %12.1fc `FED_point'
di "Institutions"
di "  Lost net tuition revenue                " %12.1fc -`IR_point'
di "    of which cross-subsidy to other programs" %10.1fc -`CS_point'
di "  Institutional account                   " %12.1fc `INS_point'
di "Society (transfers net out)"
di "  Forgone earnings (pre-tax)              " %12.1fc -`BC_point'
di "  Efficiency gain                         " %12.1fc `EG_point'
di "  Avoided instructional cost              " %12.1fc `AI_point'
di "Point-estimate net benefit ($000s):       " %12.1fc `NB_point'
di "Check (- forgone earnings + efficiency gain + avoided instructional cost): " ///
    %12.1fc (-`BC_point' + `EG_point' + `AI_point')
di "Benefit-cost ratio: " %5.3f `BCR_point'

* Revenue loss by program area (all five areas; prog_area from Section 3)
di _n "Institutional revenue loss by program area ($000s):"
tabstat inst_rev_loss_i if masters == 1, ///
    by(prog_area) stats(sum mean n)

* Revenue loss by institution type
di _n "Institutional revenue loss by institution type ($000s):"
tabstat inst_rev_loss_i if masters == 1, ///
    by(selective_inst) stats(sum mean n)
tabstat inst_rev_loss_i if masters == 1, ///
    by(public_ug) stats(sum mean n)

* ── Sensitivity of the point-estimate CBA (REVISIONS 4 and 6) ───────────────
* Two assumptions drive the size of the net benefit:
*   (1) Displacement function. Baseline: normal(overage/50), which gives a
*       student $1 over the limit a 0.50 probability of leaving. Alternative:
*       2*normal(overage/50) - 1, which starts at 0 and rises toward 1.
*   (2) Earnings horizon. Baseline: 30 years. Alternative: 10 years.
* The PRTE depends only on the displacement function (it changes the weights).
di _n "--- Sensitivity of the point-estimate CBA ($000s) ---"
di "Displacement    Horizon  Displaced    PRTE  Forgone earn.  Net social ben.    BCR  Federal acct."
foreach alt in 0 1 {
    foreach yrs in 30 10 {
        if `alt' == 0 {
            qui gen double sens_pd = normal(loan_overage/50) * above_cap
            local dlab "normal(o/50)"
        }
        else {
            qui gen double sens_pd = (2*normal(loan_overage/50) - 1) * above_cap
            local dlab "2normal(o/50)-1"
        }
        qui gen double sens_de = sens_pd * masters
        qui sum sens_de if masters == 1
        local nd_k = r(sum)
        qui gen double sens_w = sens_de * tt_hat / `nd_k' if masters == 1
        qui sum sens_w
        local prte_k = r(sum)
        local pv_k = (1 - (1 + $discount_rate)^(-`yrs')) / $discount_rate
        qui replace sens_w = sens_de * max(0, tt_hat) * ($base_salary/1000) * `pv_k' if masters == 1
        qui sum sens_w
        local bc_k = r(sum)
        qui replace sens_w = sens_de * max(0, -tt_hat) * ($base_salary/1000) * `pv_k' if masters == 1
        qui sum sens_w
        local eg_k = r(sum)
        qui replace sens_w = sens_de * (gross_tuition - net_inst_rev) if masters == 1
        qui sum sens_w
        local ai_k = r(sum)
        qui replace sens_w = $subsidy_rate * (sens_de * grad_plus_loans ///
            + (above_cap - sens_de) * loan_overage)
        qui sum sens_w
        local fs_k = r(sum)
        local nb_k  = -`bc_k' + `eg_k' + `ai_k'
        local fed_k = `fs_k' - $tax_rate * (`bc_k' - `eg_k')
        di %-15s "`dlab'" %9.0f `yrs' %11.1f `nd_k' %8.4f `prte_k' %15.1fc `bc_k' ///
           %17.1fc `nb_k' %7.3f (`ai_k' + `eg_k')/max(1, `bc_k') %15.1fc `fed_k'
        drop sens_pd sens_de sens_w
    }
}


/*---------------------------------------------------------------------------
  Section 7: Bayesian Microsimulation (S = 1000 Parametric Draws)
  
  The point-estimate CBA ignores uncertainty in the MTE polynomial. The
  Bayesian layer propagates uncertainty by drawing S = 1000 coefficient
  vectors from the asymptotic normal posterior:
  
    θ^(s) ~ N(θ_hat, V_hat)
  
  where θ_hat and V_hat are the estimated coefficient vector and its
  variance-covariance matrix from the quadratic MTE regression. For each draw,
  we recompute the MTE curve, the policy-affected margin, and the three
  CBA components. The resulting distribution characterizes posterior
  uncertainty about the net social benefit of the $100k cap.
  
  Additional uncertainty is propagated through draws on:
    - Base salary (log-normal, reflecting real-wage uncertainty)
    - Subsidy rate (uniform, reflecting fiscal policy uncertainty)
    - Tuition multiplier (log-normal, reflecting pricing uncertainty)
    - Tax rate (uniform, 0.15 to 0.25; REVISION 6)
  The propensity score (phat) is held fixed across draws, so uncertainty
  in the first-stage probit is not propagated.
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Section 7: Bayesian Microsimulation"
di    "============================================================"

* ── Store coefficient vector and VCV ─────────────────────────────────────────
* Re-estimate to ensure correct e(V) is in memory.
qui reg ln_salary masters c.masters#(c.phat c.phat2) ///
    $X phat phat2, robust
matrix b_full = e(b)
matrix V_full = e(V)
local k_full  = colsof(b_full)

* Identify column positions of the four MTE polynomial coefficients.
local cols : colnames b_full
local pos_b0 = 0
local pos_b1 = 0
local pos_b2 = 0
local pos_c1 = 0
local pos_c2 = 0
local c = 0
foreach col of local cols {
    local c = `c' + 1
    if "`col'" == "masters"              local pos_b0 = `c'
    if "`col'" == "c.masters#c.phat"     local pos_b1 = `c'
    if "`col'" == "c.masters#c.phat2"    local pos_b2 = `c'
    if "`col'" == "phat"                 local pos_c1 = `c'
    if "`col'" == "phat2"                local pos_c2 = `c'
}
di "Coefficient positions — b0: `pos_b0', b1: `pos_b1', b2: `pos_b2'"
di "                         c1: `pos_c1', c2: `pos_c2'"

* ── Postfile for simulation results ─────────────────────────────────────────
tempfile sim_results
postfile sim_handle ///
    draw          ///
    b0_s b1_s b2_s             ///  Posterior draws on beta polynomial (Eq. 12.2)
    m0_s m1_s m2_s             ///  Implied MTE polynomial (Eq. 12.3)
    ate_s att_s atu_s mprte_s  ///  Treatment parameters per draw (incl. MPRTE)
    fiscal_s                   ///  Federal subsidy savings ($000s)
    tax_loss_s                 ///  Lost tax revenue ($000s)
    behav_cost_s               ///  Forgone earnings, pre-tax ($000s)
    effic_gain_s               ///  Efficiency gain from low-return exit ($000s)
    avoid_tuition_s            ///  Avoided tuition, students ($000s)
    avoid_instr_s              ///  Avoided instructional cost ($000s)
    student_acct_s             ///  Student account ($000s)
    federal_acct_s             ///  Federal account ($000s)
    inst_acct_s                ///  Institutional account ($000s)
    net_benefit_s              ///  Net social benefit ($000s)
    nb_per_student_s           ///  NB per displaced student ($000s)
    bcr_s                      ///  Benefit-cost ratio
    pnb_positive_s             ///  Indicator: NB > 0 this draw
    inst_rev_loss_s            ///  Net institutional revenue loss ($000s)
    cross_sub_loss_s           ///  of which: cross-subsidy ($000s)
    using `sim_results', replace

* ── Simulation loop ──────────────────────────────────────────────────────────
di as text _n "Running $S_draws posterior draws..."
di as text "(This typically takes under a minute.)"

* Draw full-dimension coefficient vectors from N(b_full, V_full)
* We draw all coefficients jointly to preserve correlation structure.
preserve
drawnorm draw_1-draw_`k_full', n($S_draws) means(b_full) cov(V_full) clear

* Store all draws in a matrix for direct subscripting inside the loop.
* This avoids loading and trimming a dataset on every iteration.
mkmat draw_1-draw_`k_full', matrix(B_draws)
restore

* Quantities that do not change across draws (displacement is fixed)
qui sum loans_disp_i
local LD_0 = r(sum)
qui sum avoid_tuition_i
local AT_0 = r(sum)
qui sum avoid_instr_i if masters == 1
local AI_0 = r(sum)
qui sum inst_rev_loss_i if masters == 1
local IR_0 = r(sum)
gen double ov_stay_i = (above_cap - displaced_E) * loan_overage
qui sum ov_stay_i
local OS_0 = r(sum)
drop ov_stay_i

forvalues s = 1 / $S_draws {

    * ── Extract posterior draw s from pre-stored matrix ─────────────────────
    * B_draws[s, col] gives the s-th draw of each coefficient directly.
    * No dataset loading or trimming needed — eliminates the "499 obs deleted"
    * messages that appeared when using preserve/use/keep if _n == s.
    local b0_s = B_draws[`s', `pos_b0']
    local b1_s = B_draws[`s', `pos_b1']
    local b2_s = B_draws[`s', `pos_b2']
    local c1_s = B_draws[`s', `pos_c1']
    local c2_s = B_draws[`s', `pos_c2']
    * Implied MTE coefficients for this draw (Eq. 12.3)
    local m0_s = `b0_s' + `c1_s'
    local m1_s = 2*(`b1_s' + `c2_s')
    local m2_s = 3*`b2_s'

    * ── Additional economic uncertainty ─────────────────────────────────────
    * Base salary: log-normal with 10% coefficient of variation
    local base_sal_s = exp(ln($base_salary) + rnormal(0, $prior_sd_salary))
    * Subsidy rate: uniform ± 5 percentage points around baseline
    local sub_rate_s = $subsidy_rate + runiform(-$prior_hw_subsidy, $prior_hw_subsidy)
    * Tuition multiplier: log-normal with 8% CV, reflecting uncertainty in
    * actual tuition levels, discount rates, and institutional aid policies.
    * This ensures inst_rev_loss_s varies meaningfully across draws.
    local tuition_mult_s = exp(rnormal(0, $prior_sd_tuition))
    * Tax rate on forgone earnings: uniform 0.15 to 0.25 (REVISION 6)
    local tax_rate_s = runiform($tax_lo, $tax_hi)

    * ── Recompute MTE for this draw ──────────────────────────────────────────
    qui {
        * I(p) for this draw and treatment parameters (Eq. 11.3 weights)
        gen double I_s = `m0_s'*phat + `m1_s'*phat^2/2 + `m2_s'*phat^3/3
        local ate_s = `m0_s' + `m1_s'/2 + `m2_s'/3
        sum I_s
        local sumI_s = r(sum)
        local att_s = `sumI_s' / `sum_p'
        local atu_s = (`n_obs'*`ate_s' - `sumI_s') / (`n_obs' - `sum_p')

        * Expected return for each completer, TT(phat) = I(phat)/phat
        gen double mte_s = I_s / phat

        * PRTE ("MPRTE"): displacement-weighted average of TT for this draw.
        * Reuses total_disp scalar from Section 6 (constant across draws since
        * displaced_E is a function of loan_overage, not the MTE polynomial).
        gen mprte_i_s = displaced_E * mte_s / total_disp if masters == 1
        sum mprte_i_s if masters == 1
        local mprte_s = r(sum)

        * Behavioral cost: human capital loss for displaced high-return students
        gen bc_i_s = displaced_E * max(0, mte_s) * (`base_sal_s'/1000) * `pv_factor'
        sum bc_i_s if masters == 1
        local behav_cost_s = r(sum)

        * Efficiency gain: removing low-return constrained students
        gen eg_i_s = displaced_E * max(0, -mte_s) * (`base_sal_s'/1000) * `pv_factor'
        sum eg_i_s if masters == 1
        local effic_gain_s = r(sum)

        * Accounts (REVISION 6; see Section 6)
        local fiscal_s        = `sub_rate_s' * (`LD_0' + `OS_0')
        local tax_loss_s      = `tax_rate_s' * (`behav_cost_s' - `effic_gain_s')
        local avoid_tuition_s = `AT_0' * `tuition_mult_s'
        local avoid_instr_s   = `AI_0' * `tuition_mult_s'
        local inst_rev_loss_s = `IR_0' * `tuition_mult_s'
        local cross_sub_loss_s = $cross_sub_share * `inst_rev_loss_s'
        * Students: after-tax earnings, tuition, and the subsidy they lose
        * (avoided debt service minus forgone loans = -subsidy on displaced
        * borrowing; plus the subsidy lost by students who stay)
        local student_acct_s = -(1 - `tax_rate_s')*(`behav_cost_s' - `effic_gain_s') ///
                               + `avoid_tuition_s' - `fiscal_s'
        local federal_acct_s = `fiscal_s' - `tax_loss_s'
        local inst_acct_s    = -`inst_rev_loss_s'

        * Net social benefit = sum of the accounts
        local net_benefit_s = `student_acct_s' + `federal_acct_s' + `inst_acct_s'

        * Per-displaced-student net benefit
        sum displaced_E if masters == 1
        local n_disp_s = max(1, r(sum))
        local nb_per_student_s = `net_benefit_s' / `n_disp_s'

        * Benefit-cost ratio
        local bcr_s = (`avoid_instr_s' + `effic_gain_s') / max(1, `behav_cost_s')

        * Efficiency indicator
        local pnb_positive_s = (`net_benefit_s' > 0)

        drop I_s mte_s mprte_i_s bc_i_s eg_i_s
    }

    * ── Post results ──────────────────────────────────────────────────────────
    post sim_handle ///
        (`s') ///
        (`b0_s') (`b1_s') (`b2_s') ///
        (`m0_s') (`m1_s') (`m2_s') ///
        (`ate_s') (`att_s') (`atu_s') (`mprte_s') ///
        (`fiscal_s') (`tax_loss_s') (`behav_cost_s') (`effic_gain_s') ///
        (`avoid_tuition_s') (`avoid_instr_s') ///
        (`student_acct_s') (`federal_acct_s') (`inst_acct_s') ///
        (`net_benefit_s') (`nb_per_student_s') (`bcr_s') (`pnb_positive_s') ///
        (`inst_rev_loss_s') (`cross_sub_loss_s')

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
foreach var in behav_cost_s effic_gain_s avoid_instr_s net_benefit_s {
    qui sum `var'
    local xm = r(mean)
    local xs = r(sd)
    _pctile `var', p(2.5 97.5)
    di "`var':  mean = " %10.2f `xm' ///
        "  sd = " %10.2f `xs' ///
        "  95% CI: [" %10.2f r(r1) ", " %10.2f r(r2) "]"
}

di _n "--- Accounts by Perspective: Posterior Summary ($000s) ---"
foreach var in student_acct_s federal_acct_s fiscal_s tax_loss_s ///
    inst_acct_s cross_sub_loss_s {
    qui sum `var'
    local xm = r(mean)
    _pctile `var', p(2.5 97.5)
    di "`var':  mean = " %10.2f `xm' ///
        "  95% CI: [" %10.2f r(r1) ", " %10.2f r(r2) "]"
}

* Probability that the federal government comes out ahead
gen pfed_pos_s = (federal_acct_s > 0)
qui sum pfed_pos_s
di _n "P(Federal account > 0): " %5.3f r(mean)
drop pfed_pos_s
tabstat m0_s m1_s m2_s ate_s att_s atu_s mprte_s, ///
    stats(mean sd) columns(statistics)

* ── Treatment Parameter Posterior Summary (Sects. 12.3.3 and 12.6.3) ─────────
* Posterior means and 95% credible intervals for ATE, ATT, MPRTE, ATU.
* These four numbers are reported in the text and provide
* the inferential basis for the MPRTE > ATT claim in the chapter narrative.
di _n "--- Treatment Parameter Posterior (log-salary units) ---"
foreach var in ate_s att_s mprte_s atu_s {
    qui sum `var'
    local xm = r(mean)
    local xs = r(sd)
    _pctile `var', p(2.5 97.5)
    di "`var':  mean = " %6.4f `xm' ///
        "  sd = " %6.4f `xs' ///
        "  95% CI: [" %6.4f r(r1) ", " %6.4f r(r2) "]"
}

* ── P(MPRTE > ATT) — formal inferential statement ────────────────────────────
* The chapter's central policy claim — that the cap pushes out students with
* above-average returns among completers — is true if MPRTE > ATT in the
* posterior. We compute this probability directly from the joint draws.
gen mprte_gt_att = (mprte_s > att_s)
qui sum mprte_gt_att
di _n "P(MPRTE > ATT | data): " %5.3f r(mean) ///
    "   (posterior probability cap binds high-return margin)"
drop mprte_gt_att

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
save "$tables_dir/sim_results_ch12.dta", replace
restore


/*---------------------------------------------------------------------------
  Section 9: Posterior Summaries and Formal Inference
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Section 9: Posterior Summaries and Inference"
di    "============================================================"

preserve
use "$tables_dir/sim_results_ch12.dta", clear

* ── 95% credible intervals ───────────────────────────────────────────────────
* r(p2) and r(p97) are not stored by sum,detail. Use _pctile instead.
foreach var in behav_cost_s effic_gain_s avoid_instr_s net_benefit_s bcr_s ///
    student_acct_s federal_acct_s inst_acct_s {
    qui sum `var'
    local xm = r(mean)
    _pctile `var', p(2.5 97.5)
    local p_lo = r(r1)
    local p_hi = r(r2)
    di "`var': mean = " %12.2fc `xm' ///
        "  95% CI: [" %12.2fc `p_lo' ", " %12.2fc `p_hi' "]"
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
    di as text "Posterior mean NB = " %12.2fc `nb_mean' " ($000s) > 0"
    di as text "Scenario A likely: cap predominantly displaces low-return borrowers."
    di as text "Resources saved exceed forgone earnings."
}
else {
    di as text "Posterior mean NB = " %12.2fc `nb_mean' " ($000s) < 0"
    di as text "Scenario B likely: cap displaces students with positive marginal returns."
    di as text "Forgone earnings exceed the resources saved."
}

* ── Table 12.3: Posterior cost-benefit summary (Word) ───────────────────────
local t3vars "behav_cost_s effic_gain_s avoid_instr_s net_benefit_s bcr_s @ student_acct_s federal_acct_s inst_acct_s"
local lab_behav_cost_s   "Forgone earnings"
local lab_effic_gain_s   "Efficiency gain"
local lab_avoid_instr_s  "Avoided instructional cost"
local lab_net_benefit_s  "Net social benefit"
local lab_bcr_s          "Benefit-cost ratio"
local lab_student_acct_s "Students"
local lab_federal_acct_s "Federal government"
local lab_inst_acct_s    "Institutions"

putdocx clear
putdocx begin, font("Times New Roman", 10)
putdocx paragraph
putdocx text ("Table 12.3 "), bold
putdocx text ("Posterior cost-benefit summary: $100,000 federal graduate loan limit ($000s)")
putdocx table t3 = (12, 5), border(all, nil) layout(autofitcontents)
putdocx table t3(1,1) = ("Component")
putdocx table t3(1,2) = ("Mean"), halign(right)
putdocx table t3(1,3) = ("SD"), halign(right)
putdocx table t3(1,4) = ("2.5th pct."), halign(right)
putdocx table t3(1,5) = ("97.5th pct."), halign(right)
putdocx table t3(2,1) = ("Society"), italic
local r = 2
foreach var of local t3vars {
    local ++r
    if "`var'" == "@" {
        putdocx table t3(`r',1) = ("By account"), italic
        continue
    }
    local fmt = cond("`var'" == "bcr_s", "%9.3f", "%12.1fc")
    qui sum `var'
    local xmean = r(mean)
    local xsd   = r(sd)
    _pctile `var', p(2.5 97.5)
    local p_lo = r(r1)
    local p_hi = r(r2)
    putdocx table t3(`r',1) = ("    `lab_`var''")
    putdocx table t3(`r',2) = (strtrim(string(`xmean', "`fmt'"))), halign(right)
    putdocx table t3(`r',3) = (strtrim(string(`xsd',   "`fmt'"))), halign(right)
    putdocx table t3(`r',4) = (strtrim(string(`p_lo',  "`fmt'"))), halign(right)
    putdocx table t3(`r',5) = (strtrim(string(`p_hi',  "`fmt'"))), halign(right)
}
putdocx table t3(12,1) = ("Note: Based on $S_draws posterior draws. Net social benefit is the sum of the three accounts; tuition, loans, subsidies, and taxes are transfers between accounts and net out. Benefit-cost ratio = (avoided instructional cost + efficiency gain) / forgone earnings. Source: Posterior simulation using Example_12_1.dta (see Sect. 12.6)."), colspan(5)
putdocx table t3(1,.), border(top)
putdocx table t3(1,.), border(bottom)
putdocx table t3(11,.), border(bottom)
putdocx save "$tables_dir/Table12_3_CBA_Summary_Stata.docx", replace
di _n "Table 12.3 written to $tables_dir/Table12_3_CBA_Summary_Stata.docx"

restore


/*---------------------------------------------------------------------------
  Section 10: Figures
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Section 10: Figures"
di    "============================================================"

* ─────────────────────────────────────────────────────────────────────────────
* Fig. 12.1: Posterior Distribution of Net Social Benefits
* ─────────────────────────────────────────────────────────────────────────────
preserve
use "$tables_dir/sim_results_ch12.dta", clear

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
    name(fig12_1_posterior_nb, replace)

graph export "$graphs_dir/fig12_1_posterior_nb_Stata.png", ///
    name(fig12_1_posterior_nb) replace width(1200)

restore

* ─────────────────────────────────────────────────────────────────────────────
* Fig. 12.2: MTE Curve with Policy-Affected Region Shaded
* Based on posterior mean MTE polynomial coefficients.
* ─────────────────────────────────────────────────────────────────────────────
preserve
use "$tables_dir/sim_results_ch12.dta", clear

* Posterior mean coefficients — each command on its own line
qui sum m0_s
local b0_pm = r(mean)
qui sum m1_s
local b1_pm = r(mean)
qui sum m2_s
local b2_pm = r(mean)

* Identify policy-affected u range:
* Students pushed out by the cap tend to cluster in the intermediate-u range.
* Those with u near 0 (low resistance) self-select regardless of credit access.
* Those with u near 1 (high resistance) would not enroll even without the cap.
* The cap binds most tightly for students in u ∈ [0.30, 0.65] (medium resistance).
* UNDER REVIEW (outline Author note 6): these thresholds are hardcoded, not
* derived from the displacement weights or the loan overage distribution.
* mte_cap_neg is empty whenever the MTE is positive throughout [0.30, 0.65].

clear
set obs 100
gen u    = _n / 100
gen mte  = `b0_pm' + `b1_pm'*u + `b2_pm'*u^2
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
    name(fig12_2_mte_policy, replace)

graph export "$graphs_dir/fig12_2_mte_policy_Stata.png", ///
    name(fig12_2_mte_policy) replace width(1200)

restore

* ─────────────────────────────────────────────────────────────────────────────
* Fig. 12.3: Benefit–Cost Decomposition — Stacked Bar Chart
* Shows posterior mean of each component and their net contribution.
* ─────────────────────────────────────────────────────────────────────────────
preserve
use "$tables_dir/sim_results_ch12.dta", clear

* Posterior means of the real (non-transfer) components (REVISION 6)
qui sum avoid_instr_s
scalar sc_ai = r(mean)
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
replace label = "Avoided Instructional Cost" in 1
replace label = "Efficiency Gain"            in 2
replace label = "Forgone Earnings"           in 3
replace value = sc_ai              in 1
replace value = sc_eg              in 2
replace value = -sc_bc             in 3

graph bar value, over(label, sort(component)) ///
    bar(1, fcolor(gs4) lcolor(gs0)) ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    ytitle("Posterior Mean ($000s)") ///
    title("Cost–Benefit Decomposition") ///
    subtitle("$100k Federal Loan Cap (Posterior Mean Components)") ///
    note("Forgone earnings shown as negative. Net social benefit =" ///
         "Avoided Instructional Cost + Efficiency Gain − Forgone Earnings;" ///
         "tuition, loans, subsidies, and taxes are transfers and net out.") ///
    name(fig12_3_cba_decomp, replace)

graph export "$graphs_dir/fig12_3_cba_decomp_Stata.png", ///
    name(fig12_3_cba_decomp) replace width(1200)

restore

* ─────────────────────────────────────────────────────────────────────────────
* Fig. 12.4: Posterior Distribution of ATE, ATT, ATU
* Illustrates treatment effect heterogeneity across the posterior.
* ─────────────────────────────────────────────────────────────────────────────
preserve
use "$tables_dir/sim_results_ch12.dta", clear

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
    note("Posterior distributions of the average treatment effects" ///
         "(weights as in Chapter 11, Eq. 11.3)." ///
         "N = $S_draws posterior draws.") ///
    name(fig12_4_param_posteriors, replace)

graph export "$graphs_dir/fig12_4_param_posteriors_Stata.png", ///
    name(fig12_4_param_posteriors) replace width(1200)

drop ate_x ate_d att_x att_d atu_x atu_d

restore

* ─────────────────────────────────────────────────────────────────────────────
* Fig. 12.5: MTE Curve by Graduate Program Area (Heterogeneity)
* Field-specific MTE curves reveal which programs drive the efficiency verdict.
* ─────────────────────────────────────────────────────────────────────────────
* REVISION 4: program-area offsets are estimated from the data. The
* quadratic MTE regression (Eq. 12.2) is re-run with masters x program-area
* intercept shifts (Other = base), so each area's MTE curve is the base
* curve shifted up or down by its estimated offset. Health is added as a
* main effect here so that each shift is measured against the right base.
gen m_stem   = masters * stem_major
gen m_bus    = masters * bus_major
gen m_educ   = masters * ed_major
gen m_health = masters * health_major
qui reg ln_salary masters c.masters#(c.phat c.phat2) ///
    m_stem m_bus m_educ m_health $X health_major phat phat2, robust
local mo0 = _b[masters] + _b[phat]
local mo1 = 2*(_b[c.masters#c.phat] + _b[phat2])
local mo2 = 3*_b[c.masters#c.phat2]
di _n "Program-area MTE offsets (relative to Other), log-salary units:"
foreach v in m_stem m_bus m_educ m_health {
    local o_`v' = _b[`v']
    di "  `v': " %7.4f _b[`v'] "  (SE " %6.4f _se[`v'] ")"
}
test m_stem m_bus m_educ m_health
local p_area = r(p)
local p_area_s = string(`p_area', "%5.3f")
di "Joint test, no differences by program area: p = " %6.4f `p_area'
drop m_stem m_bus m_educ m_health

preserve
clear
set obs 100
gen u = _n / 100

* Base (Other) MTE from the program-area regression, shifted by each offset
gen mte_other   = `mo0' + `mo1'*u + `mo2'*u^2
gen mte_stem    = mte_other + `o_m_stem'
gen mte_bus     = mte_other + `o_m_bus'
gen mte_educ    = mte_other + `o_m_educ'
gen mte_health  = mte_other + `o_m_health'

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
    note("MTE shifted by program-area offsets estimated from the data (Other = base)." ///
         "Joint test of no differences by program area: p = `p_area_s'.") ///
    name(fig12_5_mte_byfield, replace)

graph export "$graphs_dir/fig12_5_mte_byfield_Stata.png", ///
    name(fig12_5_mte_byfield) replace width(1200)

restore


* ─────────────────────────────────────────────────────────────────────────────
* Fig. 12.6: Institutional Revenue Loss by Program Area
* Illustrates which programs bear the heaviest burden from the cap.
* ─────────────────────────────────────────────────────────────────────────────
preserve
use "Example_12_1.dta", clear

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
    name(fig12_6_inst_rev_byfield, replace)

graph export "$graphs_dir/fig12_6_inst_rev_byfield_Stata.png", ///
    name(fig12_6_inst_rev_byfield) replace width(1200)

restore

* ─────────────────────────────────────────────────────────────────────────────
* Fig. 12.7: Who Bears the Cost — Accounts by Perspective (REVISION 6)
* Posterior means of the student, federal, and institutional accounts and
* their sum, the net social benefit.
* ─────────────────────────────────────────────────────────────────────────────
preserve
use "$tables_dir/sim_results_ch12.dta", clear

qui sum student_acct_s
scalar sc_st = r(mean)
qui sum federal_acct_s
scalar sc_fe = r(mean)
qui sum inst_acct_s
scalar sc_in = r(mean)
qui sum net_benefit_s
scalar sc_nb = r(mean)

clear
set obs 4
gen component = _n
gen label     = ""
gen value     = .
replace label = "Students"           in 1
replace label = "Federal Government" in 2
replace label = "Institutions"       in 3
replace label = "Society (Total)"    in 4
replace value = sc_st in 1
replace value = sc_fe in 2
replace value = sc_in in 3
replace value = sc_nb in 4

graph bar value, over(label, sort(component)) ///
    bar(1, fcolor(gs5) lcolor(gs0)) ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    ytitle("Posterior Mean ($000s)") ///
    title("Who Bears the Cost") ///
    subtitle("Accounts by Perspective (Posterior Means)") ///
    note("Society = Students + Federal Government + Institutions." ///
         "Students: after-tax earnings, tuition, and lost loan subsidies." ///
         "Federal: subsidy savings minus lost tax revenue.") ///
    name(fig12_7_full_cost_stack, replace)

graph export "$graphs_dir/fig12_7_full_cost_stack_Stata.png", ///
    name(fig12_7_full_cost_stack) replace width(1200)

restore


/*---------------------------------------------------------------------------
  End of Script
---------------------------------------------------------------------------*/

di _n "============================================================"
di    "  Chapter 12 Script Complete"
di    "============================================================"
di as text "Output files:"
di as text "  Stata log:    Output/logs/Chapter12_Stata_output.log"
di as text "  Data:         Example_12_1.dta"
di as text "  Tables:       $tables_dir/Table12_1_Descriptives_Stata.docx"
di as text "                $tables_dir/Table12_2_Assumptions_Stata.docx"
di as text "                $tables_dir/Table12_3_CBA_Summary_Stata.docx"
di as text "  Sim results:  $tables_dir/sim_results_ch12.dta"
di as text "  Figures:      $graphs_dir/fig12_1_posterior_nb_Stata.png"
di as text "                $graphs_dir/fig12_2_mte_policy_Stata.png"
di as text "                $graphs_dir/fig12_3_cba_decomp_Stata.png"
di as text "                $graphs_dir/fig12_4_param_posteriors_Stata.png"
di as text "                $graphs_dir/fig12_5_mte_byfield_Stata.png"
di as text "                $graphs_dir/fig12_6_inst_rev_byfield_Stata.png"
di as text "                $graphs_dir/fig12_7_full_cost_stack_Stata.png"

log close
