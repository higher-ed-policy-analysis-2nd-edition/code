# ==============================================================================
# Synthetic Grad PLUS / Master's Degree Dataset Generation
# Creates: Example_12_1.dta
# R Translation of Complete Stata Code
#
# Application: Chapter 12 -- Bayesian MTE Microsimulation / Cost-Benefit
#              Analysis of a $100k Lifetime Cap on Grad PLUS Loans
# Instrument:  State-Funded Graduate Assistantship (GA) Funding Level
#
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch12
# Author: Marvin A. Titus
# Date: June 2026
# NOTE: Code development was assisted by Claude (Anthropic). The author
#       provided specifications and reviewed, tested, and validated all code.
# ==============================================================================
#
# PURPOSE:
#   This script generates the canonical synthetic population used by
#   Chapter 12 (Stata_code12.do and R_code12.R). It exists as a
#   standalone utility so the dataset can be regenerated independently
#   of the full chapter script -- for example, to produce a fresh draw
#   with a different seed, or to rebuild Example_12_1.dta from scratch
#   after a change to the data-generating process below.
#
#   Both Stata_code12.do and R_code12.R already contain this identical
#   generation logic as an inline fallback (used only if the canonical
#   file cannot be downloaded from the GitHub data repository), so this
#   script is a convenience copy, not a dependency of the chapter script.
#   If you change the DGP here, update the inline copies in both
#   Stata_code12.do and R_code12.R to match, and re-push the resulting
#   Example_12_1.dta to:
#     https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch12
#
# NOTE ON SYNTHETIC DATA:
#   This simulation uses synthetic data calibrated to mirror the
#   Baccalaureate and Beyond Longitudinal Study (B&B) population used
#   elsewhere in the book, extended with a Grad PLUS loan amount and
#   institutional revenue variables specific to the Chapter 12 policy
#   simulation. Synthetic rather than restricted-use B&B data is used
#   for license-free access, known true parameters for validation, and
#   full reproducibility by readers.
#
# NOTE ON RANDOM NUMBER GENERATION:
#   Stata and R use different RNG algorithms. Even with matching seeds,
#   running this script will NOT reproduce the exact same observations
#   as the Stata version (Generate_Example_12_1.do). This is expected
#   and mirrors the documented RNG divergence noted elsewhere in this
#   book's R translations. Running this script is only necessary if you
#   want to regenerate the canonical dataset from scratch; to just use
#   the existing canonical file, see Stata_code12.do / R_code12.R, which
#   download it automatically.
#
# Requirements:
#   R 4.4.x
#   haven (for Stata .dta output)
# ==============================================================================

# ----------------------------------------------------------------------------
# Install any missing packages (run once)
# ----------------------------------------------------------------------------
install_if_missing <- function(pkgs) {
  to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(to_install) > 0)
    install.packages(to_install, dependencies = TRUE)
}

install_if_missing(c("haven"))
suppressPackageStartupMessages(library(haven))

# ==============================================================================
# OUTPUT DATA DIRECTORY
# Paths switch automatically by username, mirroring the Stata logic.
# ==============================================================================

user <- Sys.info()[["user"]]

if (user == "marvi") {
  data_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 12/Data"
} else {
  data_dir <- "Data"
}
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
cat("Data directory:", data_dir, "\n")

set.seed(20251201)

# ── DGP ────────────────────────────────────────────────────────────────────
n <- 8000
dat <- data.frame(id = 1:n)

# ── Individual characteristics ──────────────────────────────────────────────
dat$female   <- as.numeric(runif(n) < 0.52)
dat$black    <- as.numeric(runif(n) < 0.13)
dat$hispanic <- as.numeric(runif(n) < 0.10)
dat$asian    <- as.numeric(runif(n) < 0.07)
dat$age_ba   <- round(rnorm(n, 24, 3))
dat$age_ba   <- pmax(21, pmin(35, dat$age_ba))
dat$firstgen <- as.numeric(runif(n) < 0.26)
dat$parent_income_q <- ceiling(runif(n) * 4)          # 1 = bottom, 4 = top
dat$parent_grad     <- as.numeric(runif(n) < 0.38)
dat$ugpa <- round(rnorm(n, 3.3, 0.45) * 100) / 100
dat$ugpa <- pmax(2.0, pmin(4.0, dat$ugpa))

# ── Program area ────────────────────────────────────────────────────────────
prog_draw <- runif(n)
dat$stem_major   <- as.numeric(prog_draw < 0.22)
dat$bus_major    <- as.numeric(prog_draw >= 0.22 & prog_draw < 0.40)
dat$ed_major     <- as.numeric(prog_draw >= 0.40 & prog_draw < 0.56)
dat$health_major <- as.numeric(prog_draw >= 0.56 & prog_draw < 0.70)
# Base category: Other / Social Sciences

# ── Institution characteristics ─────────────────────────────────────────────
dat$selective_inst <- as.numeric(runif(n) < 0.28)
dat$public_ug       <- as.numeric(runif(n) < 0.72)
dat$state_unemp     <- round(rnorm(n, 5.2, 1.3) * 10) / 10
dat$state_unemp     <- pmax(2.5, pmin(10.5, dat$state_unemp))
dat$metro           <- as.numeric(runif(n) < 0.68)

# ── State GA funding (instrument) ───────────────────────────────────────────
# Each student is assigned a state GA funding level. Variation is driven by
# state budgetary conditions and is plausibly exogenous to individual earnings.
dat$ga_funding_adj <- round(rnorm(n, 7.5, 2.2) * 10) / 10
dat$ga_funding_adj <- pmax(2.0, pmin(14.0, dat$ga_funding_adj))

# ── Grad PLUS loan amount ────────────────────────────────────────────────────
# Loan amounts reflect program costs (STEM and business programs cost more),
# institutional selectivity, and family income need. An individual-specific
# unobserved borrowing propensity adds idiosyncratic variation.
# Units: $000s. The $100k policy cap implies threshold = 100.
loan_noise <- rnorm(n, 0, 22)
dat$grad_plus_loans <- 30 +
  25 * dat$stem_major +             #  STEM: higher program costs
  35 * dat$bus_major +              #  Business/MBA: highest costs
  10 * dat$ed_major +               #  Education: moderate
  20 * dat$health_major +           #  Health: moderate-high
  15 * dat$selective_inst +         #  Selective institutions: higher CoA
  12 * (4 - dat$parent_income_q) -  #  Lower income -> higher borrowing need
  0.8 * dat$ga_funding_adj +        #  Higher GA funding -> less need to borrow
  loan_noise
dat$grad_plus_loans <- pmax(0, dat$grad_plus_loans)
dat$grad_plus_loans <- pmin(250, dat$grad_plus_loans)

# ── Institutional revenue variables ─────────────────────────────────────────
# Annual tuition and program length determine the institutional revenue stake.
# Business/MBA programs carry the highest sticker price; Education the lowest.
# Selective institutions charge a premium of roughly $12-15k per year.
# Net revenue to the institution after variable (instructional) costs is
# approximately 65 cents per tuition dollar - the marginal cost share is ~35%.
# An additional 20% of net revenue cross-subsidizes undergraduate programs
# and need-based aid; a graduate enrollment drop therefore has a ripple effect
# on the institution's broader financial model.
tuition_noise <- rnorm(n, 0, 4)
dat$annual_tuition <- 25 +
  35 * dat$bus_major +              #  Business/MBA: highest tuition
  10 * dat$stem_major +             #  STEM: moderate (TA/RA lowers net cost)
   0 * dat$ed_major +               #  Education: at baseline
  20 * dat$health_major +           #  Health: high program costs
  12 * dat$selective_inst -         #  Selective institutions: premium
   3 * dat$public_ug +              #  Public institutions: lower sticker price
  tuition_noise
dat$annual_tuition <- pmax(10, dat$annual_tuition)   # Floor at $10k/year
attr(dat$annual_tuition, "label") <- "Annual Graduate Tuition ($000s)"

# Program length (years to degree): MBA = 2, STEM = 2.5, others = 2
dat$program_years <- 2 + 0.5 * dat$stem_major
attr(dat$program_years, "label") <- "Expected Years to Degree"

# Total gross tuition revenue per enrolled student
dat$gross_tuition <- dat$annual_tuition * dat$program_years
attr(dat$gross_tuition, "label") <- "Gross Tuition Revenue per Student ($000s)"

# Net institutional revenue (65% of gross; 35% is variable instructional cost)
dat$net_inst_rev <- 0.65 * dat$gross_tuition
attr(dat$net_inst_rev, "label") <- "Net Institutional Revenue per Student ($000s)"

# ── Latent propensity to complete ───────────────────────────────────────────
# The selection equation follows Chapter 10. Students select into completion
# based on observed covariates, GA funding, and an unobserved individual factor.
epsilon <- rnorm(n, 0, 1)    # Latent selection error (positive correlation
                              #   with earnings error to create selection bias)

# Latent index (linear combination of instrument + controls + noise)
index_latent <- -1.97 +
  0.13 * dat$ga_funding_adj +
  0.04 * dat$ugpa * 10 +
  0.08 * dat$parent_grad -
  0.10 * dat$firstgen +
  0.06 * (dat$parent_income_q - 2) +
  0.05 * dat$selective_inst +
  0.05 * dat$metro +
  0.07 * dat$ed_major -
  0.03 * dat$age_ba +
  epsilon

dat$masters <- as.numeric(index_latent > 0)
attr(dat$masters, "label") <- "Completed Master's Degree (1=Yes)"

# ── Propensity score (true) ─────────────────────────────────────────────────
phat_true <- pnorm(index_latent)

# ── Heterogeneous treatment effects ─────────────────────────────────────────
# MTE(u) = b0 + b1*u + b2*u^2 + b3*u^3  (cubic polynomial)
# True parameters imply declining MTE -> positive selection on gains.
# Interpretation: students most likely to complete (low u) benefit the most.
b0_true <- -2.50
b1_true <-  19.30
b2_true <- -30.25
b3_true <-  15.12

u_true    <- 1 - phat_true
mte_true  <- b0_true + b1_true*u_true + b2_true*u_true^2 + b3_true*u_true^3
Y1_latent <- mte_true + rnorm(n, 0, 0.40)    # Log-salary under D=1
Y0_latent <- rnorm(n, 0, 0.70)               # Log-salary under D=0

# ── Observed log salary ─────────────────────────────────────────────────────
dat$ln_salary <- 10.0 +
  Y1_latent * dat$masters +
  Y0_latent * (1 - dat$masters) +
  0.25 * dat$ugpa +
  0.15 * dat$stem_major +
  0.25 * dat$bus_major -
  0.18 * dat$ed_major +
  0.10 * dat$selective_inst -
  0.07 * dat$female -
  0.06 * dat$black +
  0.025 * (dat$parent_income_q - 2) +
  rnorm(n, 0, 0.20)
attr(dat$ln_salary, "label") <- "Log Annual Salary"

dat$salary <- exp(dat$ln_salary)
attr(dat$salary, "label") <- "Annual Salary ($)"

# ── Save synthetic dataset ──────────────────────────────────────────────────
# haven::write_dta() keeps the output in Stata's native format, consistent
# with every other chapter's R translation, and preserves the "label"
# attributes set above as Stata variable labels.
haven::write_dta(dat, file.path(data_dir, "Example_12_1.dta"))

cat("Synthetic dataset saved as", file.path(data_dir, "Example_12_1.dta"), "\n")
cat("N =", nrow(dat), "observations,", ncol(dat), "variables\n")

# ------------------------------------------------------------------------------
# Quick sanity checks
# ------------------------------------------------------------------------------
cat("\n--- Sanity Checks ---\n")
print(table(dat$masters))
print(summary(dat[, c("grad_plus_loans", "ln_salary", "salary")]))
cat(sprintf("Share above $100k cap: %5.3f\n", mean(dat$grad_plus_loans > 100)))

# ------------------------------------------------------------------------------
# END OF SCRIPT
# ------------------------------------------------------------------------------
