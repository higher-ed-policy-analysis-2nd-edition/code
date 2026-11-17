# ==============================================================================
# Synthetic Grad PLUS / Master's Degree Dataset Generation
# Creates: Example_12_1_R.dta (an R-generated draw; NOT the canonical file)
# R Translation of Complete Stata Code
#
# Application: Chapter 12 -- Bayesian MTE Microsimulation / Cost-Benefit
#              Analysis of the OBBBA $100k Aggregate Limit on Federal
#              Graduate Loans
# Instrument:  State-Funded Graduate Assistantship (GA) Funding Level
#
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch12
# Author: Marvin A. Titus
# Date: June 2026 (revised November 2026; see REVISION below)
# NOTE: Code development was assisted by Claude (Anthropic). The author
#       provided specifications and reviewed, tested, and validated all code.
# ==============================================================================
#
# PURPOSE:
#   This script is the R translation of Generate_Example_12_1.do. It
#   applies the same data-generating process (DGP) and saves the result
#   as Example_12_1_R.dta, a Stata-format file written by haven, so that
#   readers can see how the Chapter 12 data are built using R alone.
#
#   The CANONICAL dataset used in the chapter, Example_12_1.dta, is
#   generated in Stata by Generate_Example_12_1.do and posted to:
#     https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch12
#   Do not upload this script's output as the canonical file.
#
# FOR READERS WHO USE R ONLY:
#   Readers who use R only do not need this script or Stata:
#   R_code12.R downloads the canonical Example_12_1.dta (generated in
#   Stata) and reads it with haven, reproducing the chapter's results.
#   Running this script instead produces a different R-generated draw
#   (Example_12_1_R.dta) whose results will not match the chapter.
#
#   Both Stata_code12.do and R_code12.R contain the same generation logic
#   as an inline fallback (used only if the canonical file cannot be
#   downloaded). If the DGP is changed, update Generate_Example_12_1.do,
#   this script, and both inline copies to match, then regenerate and
#   re-push Example_12_1.dta from the Stata script.
#
# REVISION (November 2026; mirrors Generate_Example_12_1.do): resistance
#   to treatment is now U_D = pnorm(-epsilon) instead of
#   1 - pnorm(index_latent). U_D is uniform on [0,1], and a student
#   completes exactly when U_D is below the propensity score (Chapter 11,
#   Eq. 11.1). The old definition mixed the instrument and controls into
#   each student's return, so no MTE estimator could recover the true
#   effects. The change uses no random numbers, so every variable except
#   ln_salary and salary is identical to an earlier run of this script
#   (superseded by REVISION 3 below, which changes the data).
#   The script now also reports the TRUE ATE, ATT, and ATU, and writes its
#   output to Output/logs/Generate_Example_12_1_R.log.
#
#   REVISION 3 (November 2026): recalibrated DGP. Realistic returns that
#   decline with resistance, MTE(u) = 0.40 - 0.40u + 0.10u^2 (true ATE
#   0.233; positive selection on gains); potential outcomes with SD 0.45
#   and selection on levels; GA funding drawn with SD 4.0 (bounds 0-20)
#   and coefficient 0.25 in the selection index (intercept -3.00), so that
#   propensity scores span nearly all of [0,1] and completion is about 38%.
#   Verified by Monte Carlo (200 datasets, N = 8,000): the quadratic
#   control-function estimator recovers the true ATE, ATT, ATU, and PRTE
#   with negligible bias (SD of estimates about 0.03-0.04).
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

# ==============================================================================
# LOG FILE
# Written to Output/logs with an _R suffix so it never overwrites the Stata
# log (Generate_Example_12_1.log). Output is echoed to the console as well.
# ==============================================================================
if (user == "marvi") {
  log_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 12/Output/logs"
} else {
  log_dir <- "Output/logs"
}
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
log_file <- file.path(log_dir, "Generate_Example_12_1_R.log")

while (sink.number() > 0) sink()          # clear any sink left by an aborted run
log_con <- file(log_file, open = "wt")
sink(log_con, split = TRUE)
cat("Generate_Example_12_1.R log opened:", format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n")
cat("Log file:", log_file, "\n")
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
# Calibration (REVISION 3): SD 4.0 and bounds 0-20 (previously SD 2.2,
# bounds 2-14) so that propensity scores span nearly all of [0,1].
dat$ga_funding_adj <- round(rnorm(n, 7.5, 4.0) * 10) / 10
dat$ga_funding_adj <- pmax(0, pmin(20, dat$ga_funding_adj))

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
# The selection equation mirrors Chapter 11. Students select into completion
# based on observed covariates, GA funding, and an unobserved individual factor.
epsilon <- rnorm(n, 0, 1)    # Latent selection error; defines resistance U_D

# Latent index (linear combination of instrument + controls + noise)
index_latent <- -3.00 +                # REVISION 3 (was -1.97)
  0.25 * dat$ga_funding_adj +          # REVISION 3 (was 0.13)
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

# ── Heterogeneous treatment effects (REVISION 3) ────────────────────────────
# MTE(u) = a0 + a1*u + a2*u^2  (quadratic in resistance u, as in Ch. 11)
# Resistance is U_D = pnorm(-epsilon): uniform on [0,1], and a student
# completes exactly when U_D is below the propensity score.
# The MTE declines from 0.40 at u = 0 to 0.10 at u = 1: positive selection
# on gains (Carneiro, Heckman, & Vytlacil, 2011). True ATE = 0.233.
# Selection on levels: students with higher resistance have higher
# earnings without the degree, so naive OLS is biased.
a0_true <-  0.40
a1_true <- -0.40
a2_true <-  0.10

u_true    <- pnorm(-epsilon)                       # U_D: resistance
mte_true  <- a0_true + a1_true*u_true + a2_true*u_true^2
Y0_latent <- 0.15*(u_true - 0.5) + rnorm(n, 0, 0.45)   # Log-salary under D=0
Y1_latent <- Y0_latent + mte_true + rnorm(n, 0, 0.10)  # Log-salary under D=1

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

# ── True treatment effects (for validating the chapter's estimates) ─────────
# Individual effect of completion on log salary: Y1_latent - Y0_latent.
# Computed before saving; not stored in the dataset. No random numbers used.
te_true  <- Y1_latent - Y0_latent
true_ate <- mean(te_true)
true_att <- mean(te_true[dat$masters == 1])
true_atu <- mean(te_true[dat$masters == 0])

# ── Save synthetic dataset ──────────────────────────────────────────────────
# haven::write_dta() keeps the output in Stata's native format, consistent
# with every other chapter's R translation, and preserves the "label"
# attributes set above as Stata variable labels.
haven::write_dta(dat, file.path(data_dir, "Example_12_1_R.dta"))

cat("Synthetic dataset saved as", file.path(data_dir, "Example_12_1_R.dta"), "\n")
cat("N =", nrow(dat), "observations,", ncol(dat), "variables\n")

# ------------------------------------------------------------------------------
# Quick sanity checks
# ------------------------------------------------------------------------------
cat("\n--- Sanity Checks ---\n")
print(table(dat$masters))
print(summary(dat[, c("grad_plus_loans", "ln_salary", "salary")]))
cat(sprintf("Share above $100k cap: %5.3f\n", mean(dat$grad_plus_loans > 100)))

cat("\n--- True treatment effects (log salary units) ---\n")
cat(sprintf("True ATE: %6.4f\n", true_ate))
cat(sprintf("True ATT: %6.4f\n", true_att))
cat(sprintf("True ATU: %6.4f\n", true_atu))

cat("\nGenerate_Example_12_1.R log closed:", format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n")
sink()
close(log_con)

# ------------------------------------------------------------------------------
# END OF SCRIPT
# ------------------------------------------------------------------------------
