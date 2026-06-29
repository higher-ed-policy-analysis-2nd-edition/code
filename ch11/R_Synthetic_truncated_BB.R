#===============================================================================
# Synthetic B&B Dataset Generation
# Creates: Example_7_5_3.dta
#
# Application: Effect of Master's Degree on Salary Outcomes
# Instrument: State-Funded Graduate Assistantship (GA) Dollar Amount
#
# Based on synthetic data mirroring NCES B&B Longitudinal Study characteristics
# and higher education finance literature (Titus 2007; Bound, Lovenheim &
# Turner 2010; Zhang 2005; Ehrenberg et al. 2007)
#
# Author: Marvin A. Titus
# Date: November 2025 (revised March 2026)
# Purpose: Generate synthetic dataset for textbook Chapters 7 and 11
#
# R translation of Synthetic_truncated_BB.do
#
# REVISION NOTES (March 2026):
# ----------------------------
# Added Section 7b: Master's degree program area categories
#   (Business, Education, Health & Related, STEM, Other)
# Program areas are generated conditional on treatment (masters == 1)
# using multinomial transition probabilities from undergraduate major,
# calibrated to IPEDS Completions Survey field distributions.
# Section 8 (te_masters) updated to use graduate program area as the
# primary source of field-specific return heterogeneity for treated
# observations; undergraduate major retained as counterfactual proxy
# for untreated observations.
# Section 9 expanded with program-area verification tabulations.
#
# NOTE ON SYNTHETIC DATA:
# -----------------------
# This simulation uses synthetic data calibrated to mirror the Baccalaureate
# and Beyond Longitudinal Study (B&B). We use synthetic rather than actual
# B&B data for several reasons:
#
# 1. ACCESS RESTRICTIONS: B&B restricted-use data requires NCES license
# 2. PEDAGOGICAL TRANSPARENCY: Known true parameters allow validation
# 3. REPRODUCIBILITY: Readers can generate identical datasets
# 4. CONTINUITY: Same dataset used in Chapters 7 and 11
#
# NOTE ON AI-ASSISTED CODE DEVELOPMENT:
# -------------------------------------
# The simulation code was developed with assistance from Claude (Anthropic).
# The author provided specifications based on B&B characteristics and higher
# education finance literature. Claude assisted in translating specifications
# to executable code. The author reviewed, tested, and validated all code.
#
# NOTE ON REPRODUCIBILITY ACROSS LANGUAGES:
# ------------------------------------------
# R and Stata use different pseudo-random number generators, so set.seed(20251130)
# here will NOT reproduce the exact same observations as Stata's set seed 20251130.
# The statistical properties of the simulated dataset (distributions, means,
# correlations, treatment rates) will match within sampling variation, but
# individual rows will differ. If you need a dataset that is bit-for-bit
# identical to the one used in the book, use Example_7_5_3.dta (or
# Example_7_5_3_updated.dta) directly from the Chapter 7 data repository
# rather than re-running this script in R.
#===============================================================================
# IMPORTANT: Set working directory (customize this for your system)
#===============================================================================
# ch10data <- "C:/Users/YourName/Documents/book-materials/ch10/data"
# setwd(ch10data)

# Required packages: haven (to write .dta), dplyr (for case_when / mutate
# convenience). Both are used sparingly below; base R handles most of the
# simulation.
library(haven)
library(dplyr)

rm(list = ls())
set.seed(20251130)

# Set sample size
N <- 8000
id <- 1:N

#===============================================================================
# SECTION 1: Demographics
# Based on B&B:08/18 restricted-use data distributions
#===============================================================================

# Female (B&B shows ~57% of bachelor's recipients are female)
female <- rbinom(N, 1, 0.57)

# Race/Ethnicity (approximate B&B distributions)
race_rand  <- runif(N)
white      <- as.integer(race_rand < 0.62)
black      <- as.integer(race_rand >= 0.62 & race_rand < 0.72)
hispanic   <- as.integer(race_rand >= 0.72 & race_rand < 0.84)
asian      <- as.integer(race_rand >= 0.84 & race_rand < 0.92)
other_race <- as.integer(race_rand >= 0.92)
rm(race_rand)

# Age at bachelor's degree
age_ba <- 22 + rpois(N, 1.5)
age_ba[age_ba < 20] <- 22
age_ba[age_ba > 35] <- 35

#===============================================================================
# SECTION 2: Family Background
#===============================================================================

firstgen        <- rbinom(N, 1, 0.35)
parent_income_q <- 1 + rbinom(N, 4, 0.55)
parent_grad     <- rbinom(N, 1, 0.25)

#===============================================================================
# SECTION 3: Academic Background
#===============================================================================

# Undergraduate GPA (beta distribution scaled to 2.0-4.0)
ugpa <- 2.0 + 1.2 * rbeta(N, 5, 3)
ugpa[ugpa > 4.0] <- 4.0
ugpa[ugpa < 2.0] <- 2.0

# Major field (mutually exclusive)
# Stata generates these sequentially with `if` conditions that leave
# untouched observations as missing, then later conditions only ever
# touch rows that are still eligible. The block below reproduces that
# exact sequential, mutually-exclusive assignment.
stem_major <- rbinom(N, 1, 0.25)

bus_major <- rep(NA_integer_, N)
bus_major[stem_major == 0] <- rbinom(sum(stem_major == 0), 1, 0.20)
bus_major[stem_major == 1] <- 0L

ed_major <- rep(NA_integer_, N)
elig_ed <- (stem_major == 0 & bus_major == 0)
ed_major[elig_ed] <- rbinom(sum(elig_ed), 1, 0.15)
ed_major[stem_major == 1 | bus_major == 1] <- 0L

socsci_major <- as.integer(stem_major == 0 & bus_major == 0 & ed_major == 0)

# Institution characteristics
selective_inst <- rbinom(N, 1, 0.30)
public_ug      <- rbinom(N, 1, 0.65)

#===============================================================================
# SECTION 4: Labor Market Context
#===============================================================================

state_unemp <- 4 + 6 * rbeta(N, 2, 3)
metro       <- rbinom(N, 1, 0.75)

#===============================================================================
# SECTION 5: Generate Instrument - State GA Funding
#===============================================================================

# Assign to states (50 states)
state <- ceiling(50 * runif(N))

# State-level GA funding (with state fixed effects)
# Generate one state effect per state, then broadcast to all members of
# that state -- equivalent to Stata's
#   bysort state: gen state_effect = rnormal(0, 4) if _n == 1
#   bysort state: replace state_effect = state_effect[1]
state_effect_lookup <- setNames(rnorm(50, 0, 4), as.character(1:50))
state_effect <- state_effect_lookup[as.character(state)]

# Base GA funding with state variation
ga_funding <- 18 + state_effect + rnorm(N, 0, 2)
ga_funding[ga_funding < 8]  <- 8
ga_funding[ga_funding > 35] <- 35

# Field-adjusted GA funding (STEM gets more, Business gets less)
ga_field_mult <- rep(1.0, N)
ga_field_mult[stem_major == 1] <- 1.3
ga_field_mult[bus_major == 1]  <- 0.9
ga_field_mult[ed_major == 1]   <- 1.1
ga_field_mult[socsci_major == 1] <- 1.0

ga_funding_adj <- ga_funding * ga_field_mult
rm(state_effect, state_effect_lookup, ga_field_mult)

#===============================================================================
# SECTION 6: Generate Latent Factors (Unobserved)
# These create essential heterogeneity for MTE analysis
#===============================================================================

# Unobserved ability (affects both selection and outcomes)
eta_ability <- rnorm(N, 0, 1)

# Unobserved taste for education (correlated with ability)
eta_taste <- 0.3 * eta_ability + rnorm(N, 0, 0.9)

# Unobserved productivity (correlated with ability)
eta_prod <- 0.5 * eta_ability + rnorm(N, 0, 0.85)

#===============================================================================
# SECTION 7: Generate Treatment (Master's Degree)
# Selection equation with essential heterogeneity
#===============================================================================

# Latent index for treatment selection
z_masters <-
  -0.9 +                                  # Baseline
  0.15 * female +
  0.10 * black +
  0.05 * hispanic +
  0.20 * asian +
  -0.03 * (age_ba - 22) +
  -0.25 * firstgen +
  0.08 * parent_income_q +
  0.35 * parent_grad +
  0.60 * (ugpa - 3.0) +
  0.20 * stem_major +
  -0.15 * bus_major +
  0.45 * ed_major +
  0.30 * selective_inst +
  -0.02 * state_unemp +
  0.15 * metro +
  0.06 * (ga_funding_adj - 18) +          # INSTRUMENT EFFECT
  0.40 * eta_taste +                      # Unobserved taste for education
  0.25 * eta_ability                      # Unobserved ability

# Convert to probability via probit link
p_masters <- pnorm(z_masters)

# Generate treatment via threshold crossing
u_d <- runif(N)
masters <- as.integer(p_masters > u_d)

#===============================================================================
# SECTION 7b: Master's Degree Program Area
#
# Five mutually exclusive categories (IPEDS CIP-based groupings):
#   ma_business  -- Business, Management, Marketing (CIP 52)
#   ma_education -- Education (CIP 13)
#   ma_health    -- Health Professions & Related (CIP 51)
#   ma_stem      -- STEM fields (CIPs 01,03,04,11,14,15,26,27,29,40,41)
#   ma_other     -- All remaining fields (Social sciences, Humanities,
#                   Public admin, Arts, Law, etc.)
#
# Generated ONLY for treated observations (masters == 1); set to 0 for
# untreated. Transition probabilities from undergraduate major to graduate
# field are calibrated to IPEDS Completions Survey patterns.
#
# Key empirical regularities encoded:
#   - Education undergrads overwhelmingly pursue Education master's (70%)
#   - Business undergrads strongly favor Business master's (65%)
#   - STEM undergrads split between STEM (55%) and Health & Related (15%)
#   - Social science/other undergrads distribute broadly, with notable
#     shares entering Health, Business, and Education programs
#   - Health & Related master's draws substantially from STEM and social
#     science pipelines (nursing, public health, kinesiology, etc.)
#===============================================================================

# Initialize indicators
ma_business  <- rep(NA_integer_, N)
ma_education <- rep(NA_integer_, N)
ma_health    <- rep(NA_integer_, N)
ma_stem      <- rep(NA_integer_, N)
ma_other     <- rep(NA_integer_, N)

# Single uniform draw governs field assignment for treated observations
ma_rand <- rep(NA_real_, N)
ma_rand[masters == 1] <- runif(sum(masters == 1))

#-------------------------------------------------------------------------------
# Transition block 1: STEM undergraduates
#   STEM -> STEM:      55%  (0.00-0.55)
#   STEM -> Health:    15%  (0.55-0.70)
#   STEM -> Business:  12%  (0.70-0.82)
#   STEM -> Education:  6%  (0.82-0.88)
#   STEM -> Other:     12%  (0.88-1.00)
#-------------------------------------------------------------------------------
sel <- masters == 1 & stem_major == 1
ma_stem[sel]      <- as.integer(ma_rand[sel] < 0.55)
ma_health[sel]    <- as.integer(ma_rand[sel] >= 0.55 & ma_rand[sel] < 0.70)
ma_business[sel]  <- as.integer(ma_rand[sel] >= 0.70 & ma_rand[sel] < 0.82)
ma_education[sel] <- as.integer(ma_rand[sel] >= 0.82 & ma_rand[sel] < 0.88)
ma_other[sel]     <- as.integer(ma_rand[sel] >= 0.88)

#-------------------------------------------------------------------------------
# Transition block 2: Business undergraduates
#   Bus -> Business:   65%  (0.00-0.65)
#   Bus -> STEM:        7%  (0.65-0.72)
#   Bus -> Health:      8%  (0.72-0.80)
#   Bus -> Education:   6%  (0.80-0.86)
#   Bus -> Other:      14%  (0.86-1.00)
#-------------------------------------------------------------------------------
sel <- masters == 1 & bus_major == 1
ma_business[sel]  <- as.integer(ma_rand[sel] < 0.65)
ma_stem[sel]      <- as.integer(ma_rand[sel] >= 0.65 & ma_rand[sel] < 0.72)
ma_health[sel]    <- as.integer(ma_rand[sel] >= 0.72 & ma_rand[sel] < 0.80)
ma_education[sel] <- as.integer(ma_rand[sel] >= 0.80 & ma_rand[sel] < 0.86)
ma_other[sel]     <- as.integer(ma_rand[sel] >= 0.86)

#-------------------------------------------------------------------------------
# Transition block 3: Education undergraduates
#   Ed -> Education:   70%  (0.00-0.70)
#   Ed -> Business:    10%  (0.70-0.80)
#   Ed -> Health:       8%  (0.80-0.88)
#   Ed -> STEM:         5%  (0.88-0.93)
#   Ed -> Other:        7%  (0.93-1.00)
#-------------------------------------------------------------------------------
sel <- masters == 1 & ed_major == 1
ma_education[sel] <- as.integer(ma_rand[sel] < 0.70)
ma_business[sel]  <- as.integer(ma_rand[sel] >= 0.70 & ma_rand[sel] < 0.80)
ma_health[sel]    <- as.integer(ma_rand[sel] >= 0.80 & ma_rand[sel] < 0.88)
ma_stem[sel]      <- as.integer(ma_rand[sel] >= 0.88 & ma_rand[sel] < 0.93)
ma_other[sel]     <- as.integer(ma_rand[sel] >= 0.93)

#-------------------------------------------------------------------------------
# Transition block 4: Social science / other undergraduates
#   SocSci -> Business:   28%  (0.00-0.28)
#   SocSci -> Education:  17%  (0.28-0.45)
#   SocSci -> Health:     17%  (0.45-0.62)
#   SocSci -> STEM:       10%  (0.62-0.72)
#   SocSci -> Other:      28%  (0.72-1.00)
#-------------------------------------------------------------------------------
sel <- masters == 1 & socsci_major == 1
ma_business[sel]  <- as.integer(ma_rand[sel] < 0.28)
ma_education[sel] <- as.integer(ma_rand[sel] >= 0.28 & ma_rand[sel] < 0.45)
ma_health[sel]    <- as.integer(ma_rand[sel] >= 0.45 & ma_rand[sel] < 0.62)
ma_stem[sel]      <- as.integer(ma_rand[sel] >= 0.62 & ma_rand[sel] < 0.72)
ma_other[sel]     <- as.integer(ma_rand[sel] >= 0.72)

rm(ma_rand, sel)

# Set non-treated to 0 (no graduate program area)
ma_business[masters == 0]  <- 0L
ma_education[masters == 0] <- 0L
ma_health[masters == 0]    <- 0L
ma_stem[masters == 0]      <- 0L
ma_other[masters == 0]     <- 0L

#===============================================================================
# SECTION 8: Generate Outcome (Salary)
# Potential outcomes framework with heterogeneous treatment effects
#
# Treatment effect heterogeneity now reflects GRADUATE program area
# (for treated obs) rather than undergraduate major. Field premia are
# calibrated to wage-premium literature on master's degrees by field:
#
#   Health & Related:  +0.14 log pts  (nursing, public health, pharmacy)
#   STEM:               +0.10 log pts  (engineering, CS, physical sciences)
#   Business:           +0.08 log pts  (MBA and related management degrees)
#   Education:          +0.04 log pts  (teacher licensure, admin programs)
#   Other:              +0.00 log pts  (base; social work, humanities, etc.)
#
# For untreated observations, undergraduate major proxies the likely
# field of a counterfactual graduate degree (magnitudes reduced relative
# to graduate field effects to reflect the additional uncertainty).
#
# Sources: Webber (2014); Carnevale, Cheah & Wenzinger (2021);
#          Tamborini, Kim & Sakamoto (2015); Zhang (2008)
#===============================================================================

# Potential outcome without treatment (Y0)
# Unchanged from original specification
ln_salary_0 <-
  10.50 +
  -0.08 * female +
  -0.05 * black +
  -0.03 * hispanic +
  0.06 * asian +
  0.02 * (age_ba - 22) +
  -0.03 * firstgen +
  0.03 * parent_income_q +
  0.04 * parent_grad +
  0.10 * (ugpa - 3.0) +
  0.25 * stem_major +
  0.15 * bus_major +
  -0.12 * ed_major +
  0.08 * selective_inst +
  -0.01 * state_unemp +
  0.10 * metro +
  0.20 * eta_prod +                       # Unobserved productivity
  rnorm(N, 0, 0.25)

#-------------------------------------------------------------------------------
# Field-specific return to master's degree
#
# For treated (masters == 1): use actual graduate program area.
# For untreated (masters == 0): use undergraduate major as counterfactual
#   proxy (magnitudes slightly smaller to reflect field-switching risk).
#-------------------------------------------------------------------------------
te_field_return <- rep(NA_real_, N)

# Treated: actual graduate program area effects
te_field_return[masters == 1] <-
  0.14 * ma_health[masters == 1] +
  0.10 * ma_stem[masters == 1] +
  0.08 * ma_business[masters == 1] +
  0.04 * ma_education[masters == 1] +
  0.00 * ma_other[masters == 1]

# Untreated: undergraduate major as counterfactual proxy
te_field_return[masters == 0] <-
  0.08 * stem_major[masters == 0] +
  0.05 * bus_major[masters == 0] +
  0.10 * ed_major[masters == 0] +
  0.00 * socsci_major[masters == 0]

# Heterogeneous treatment effect (essential heterogeneity)
# Treatment effect varies with observables AND unobservables
te_masters <-
  0.12 +                                  # Base effect (Other/no-field baseline)
  te_field_return +                       # Field-specific graduate return
  0.03 * selective_inst +
  0.05 * (ugpa - 3.0) +
  0.08 * eta_ability +                    # Ability-education complementarity
  -0.10 * (p_masters - 0.5) +             # Essential heterogeneity
  rnorm(N, 0, 0.05)

# Potential outcome with treatment (Y1)
ln_salary_1 <- ln_salary_0 + te_masters

# Observed outcome
ln_salary <- masters * ln_salary_1 + (1 - masters) * ln_salary_0
salary <- exp(ln_salary)

#===============================================================================
# SECTION 9: Verify Data Properties
#===============================================================================

cat("\n==============================================\n")
cat("SYNTHETIC DATASET SUMMARY\n")
cat("==============================================\n")

cat("\n--- Sample Size ---\n")
cat("N =", N, "\n")

cat("\n--- Treatment Rate ---\n")
cat("Treatment rate:", sprintf("%5.3f", mean(masters)), "\n")

cat("\n--- Instrument Summary ---\n")
print(summary(ga_funding_adj))

cat("\n--- Outcome Summary ---\n")
print(summary(ln_salary))
print(summary(salary))

cat("\n--- True Treatment Effect Parameters ---\n")
cat("Mean TE (ATE approximation):        ", sprintf("%6.4f", mean(te_masters)), "\n")
cat("Mean TE for treated (ATT approx.):  ", sprintf("%6.4f", mean(te_masters[masters == 1])), "\n")
cat("Mean TE for untreated (ATU approx.):", sprintf("%6.4f", mean(te_masters[masters == 0])), "\n")

cat("\n--- Correlation Structure ---\n")
print(cor(data.frame(eta_ability, eta_taste, eta_prod)))

#-------------------------------------------------------------------------------
# Master's Program Area Verification
#-------------------------------------------------------------------------------

cat("\n==============================================\n")
cat("MASTER'S DEGREE PROGRAM AREA SUMMARY\n")
cat("==============================================\n")

cat("\n--- Program Area Distribution (Treated Only) ---\n")
cat("(N = treated observations)\n")

n_treated <- sum(masters == 1)
cat("  Total treated:", n_treated, "\n")

ma_areas <- list(stem = ma_stem, business = ma_business,
                  education = ma_education, health = ma_health,
                  other = ma_other)
for (area in names(ma_areas)) {
  n_area <- sum(ma_areas[[area]] == 1)
  cat(sprintf("  ma_%s: %d  (%5.1f%%)\n", area, n_area, 100 * n_area / n_treated))
}

cat("\n--- Mutual Exclusivity Check ---\n")
ma_check <- ma_business + ma_education + ma_health + ma_stem + ma_other
cat("  Treated obs with != 1 program area:",
    sum(masters == 1 & ma_check != 1), " (should be 0)\n")
cat("  Untreated obs with != 0 program area:",
    sum(masters == 0 & ma_check != 0), " (should be 0)\n")
rm(ma_check)

cat("\n--- Program Area by Undergraduate Major (Treated Only) ---\n")
cat("(Row: undergrad major | Col: graduate program area)\n")
cat("  (Rows sum to 100% within each undergrad major)\n")

prog_area_table <- function(mask) {
  data.frame(
    ma_business  = sum(ma_business[mask]),
    ma_education = sum(ma_education[mask]),
    ma_health    = sum(ma_health[mask]),
    ma_stem      = sum(ma_stem[mask]),
    ma_other     = sum(ma_other[mask])
  )
}
cat("STEM undergrads:\n");    print(prog_area_table(masters == 1 & stem_major == 1))
cat("Business undergrads:\n"); print(prog_area_table(masters == 1 & bus_major == 1))
cat("Education undergrads:\n"); print(prog_area_table(masters == 1 & ed_major == 1))
cat("Social science/other undergrads:\n"); print(prog_area_table(masters == 1 & socsci_major == 1))

cat("\n--- Mean Treatment Effect by Master's Program Area ---\n")
cat("  (For treated observations)\n")
for (area in names(ma_areas)) {
  mask <- ma_areas[[area]] == 1
  cat(sprintf("  ma_%s: Mean TE = %6.4f  (N=%d)\n",
              area, mean(te_masters[mask]), sum(mask)))
}

cat("\n--- Mean Salary by Master's Program Area ---\n")
for (area in names(ma_areas)) {
  mask <- ma_areas[[area]] == 1
  cat(sprintf("  ma_%s: Mean salary = $%9.0f\n", area, mean(salary[mask])))
}

#===============================================================================
# SECTION 10: Save Dataset
#===============================================================================

# Order variables logically
synthetic_bb <- data.frame(
  id, female, white, black, hispanic, asian, other_race, age_ba,
  firstgen, parent_income_q, parent_grad, ugpa,
  stem_major, bus_major, ed_major, socsci_major,
  selective_inst, public_ug, state_unemp, metro, state,
  ga_funding, ga_funding_adj,
  eta_ability, eta_taste, eta_prod,
  z_masters, p_masters, u_d, masters,
  ma_stem, ma_business, ma_education, ma_health, ma_other,
  te_field_return,
  ln_salary_0, te_masters, ln_salary_1, ln_salary, salary
)

# Variable labels (Stata's `label var`; haven::write_dta preserves these
# as attr(x, "label") so they round-trip back into Stata if needed)
var_labels <- c(
  female = "Female (1=Yes)",
  white = "White non-Hispanic",
  black = "Black non-Hispanic",
  hispanic = "Hispanic",
  asian = "Asian",
  other_race = "Other race/ethnicity",
  firstgen = "First-generation college student",
  parent_income_q = "Parent income quintile (1-5)",
  parent_grad = "Parent has graduate degree",
  stem_major = "STEM major",
  bus_major = "Business major",
  ed_major = "Education major",
  socsci_major = "Social science/other major",
  selective_inst = "Attended selective institution",
  public_ug = "Attended public undergraduate institution",
  state_unemp = "State unemployment rate",
  metro = "Lives in metropolitan area",
  ga_funding = "State GA Funding (base, $1000s)",
  ga_funding_adj = "State GA Funding (field-adjusted, $1000s)",
  eta_ability = "Latent ability factor",
  eta_taste = "Latent taste for education",
  eta_prod = "Latent productivity factor",
  z_masters = "Latent index for master's selection",
  p_masters = "Propensity Score (true)",
  u_d = "Uniform draw for treatment assignment",
  masters = "Completed Master's Degree (1=Yes)",
  ma_business = "Master's degree: Business (1=Yes)",
  ma_education = "Master's degree: Education (1=Yes)",
  ma_health = "Master's degree: Health & Related (1=Yes)",
  ma_stem = "Master's degree: STEM (1=Yes)",
  ma_other = "Master's degree: Other field (1=Yes)",
  te_field_return = "Field-specific return to master's (actual or proxy)",
  ln_salary_0 = "Log salary without master's (potential outcome)",
  te_masters = "True Individual Treatment Effect",
  ln_salary_1 = "Log salary with master's (potential outcome)",
  ln_salary = "Observed log salary",
  salary = "Observed salary ($)"
)
for (v in names(var_labels)) {
  attr(synthetic_bb[[v]], "label") <- var_labels[[v]]
}

# Compress (R has no direct analogue to Stata's `compress`; haven::write_dta
# already chooses compact on-disk types) and save
# write_dta(synthetic_bb, "Example_7_5_3.dta")
#
# cat("\n==============================================\n")
# cat("Dataset saved: Example_7_5_3.dta\n")
# cat("==============================================\n")

#===============================================================================
# END OF DATA GENERATION
#===============================================================================
