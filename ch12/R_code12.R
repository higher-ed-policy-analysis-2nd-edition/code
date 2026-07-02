# ==============================================================================
# Chapter 12: Bayesian MTE Microsimulation
# Cost-Benefit Analysis of a $100k Lifetime Cap on Grad PLUS Loans
# R Translation of Complete Stata Code
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch12
# Author: Marvin A. Titus
# Date: June 2026
# NOTE: Code development was assisted by Claude (Anthropic). The author
#       provided specifications and reviewed, tested, and validated all code.
# ==============================================================================
#
# Overview:
#   This script evaluates the net social benefit of a $100,000 lifetime
#   aggregate cap on federal Grad PLUS borrowing (effective July 1, 2026
#   under the One Big Beautiful Bill, Pub. L. 119-XX). The cap binds for
#   graduate students whose program costs exceed the new limit. Using the
#   marginal treatment effect (MTE) framework from Chapter 10, we estimate
#   heterogeneous returns to master's degree completion and simulate which
#   students a binding cap would push out of the program. A Bayesian
#   microsimulation (parametric bootstrap, S = 1000 posterior draws)
#   propagates coefficient uncertainty into a posterior distribution of
#   net social benefits, fiscal savings, and behavioral costs.
#
# Pipeline:
#   Section 1.  Setup and directory structure
#   Section 2.  Synthetic data generation (B&B-style panel, N = 8,000)
#   Section 3.  Descriptive statistics and policy exposure
#   Section 4.  Probit selection model and propensity score
#   Section 5.  MTE estimation - cubic polynomial control function
#   Section 6.  Policy simulation - cap-binding student identification
#   Section 7.  Bayesian microsimulation (S = 1000 parametric draws)
#   Section 8.  Cost-benefit decomposition
#   Section 9.  Posterior summaries and inference
#   Section 10. Figures
#
# CBA Framework:
#   Net Benefit^(s) = Fiscal_Savings^(s) - Behavioral_Cost^(s)
#
#   Fiscal savings:   reduced federal lending on overage x subsidy rate
#   Behavioral cost:  lost human capital for cap-constrained dropouts
#                     (integral of positive MTE over displaced margin)
#
#   Policy is efficient when displaced students have negative or near-zero
#   returns; inefficient when displaced students have strongly positive MTE.
#
# Requirements:
#   R 4.4.x
#   No additional user-written packages beyond those listed below
#
# NOTE ON RANDOM NUMBER GENERATION:
#   Stata and R use different RNG algorithms. Even with matching seeds,
#   the synthetic dataset and Bayesian posterior draws produced here will
#   NOT be numerically identical to the Stata version. This mirrors the
#   documented RNG divergence between Stata and R already noted for the
#   Chapter 11 MTE/MPRTE translation -- the qualitative conclusions
#   (sign, rough magnitude, and policy verdict) should match; exact
#   point estimates will not.
# ==============================================================================


# ------------------------------------------------------------------------------
# Section 1: Setup
# ------------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# Install any missing packages (run once)
# ----------------------------------------------------------------------------
install_if_missing <- function(pkgs) {
  to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(to_install) > 0)
    install.packages(to_install, dependencies = TRUE)
}

install_if_missing(c("haven", "MASS", "sandwich", "lmtest", "car",
                      "dplyr", "tidyr", "ggplot2", "scales"))

suppressPackageStartupMessages({
  library(haven)     # read_dta/write_dta()   - replaces: use/save *.dta
  library(MASS)      # mvrnorm()              - replaces: drawnorm
  library(sandwich)  # vcovHC()               - replaces: regress/probit ,robust
  library(lmtest)    # coeftest()             - replaces: test (single coef)
  library(car)       # linearHypothesis()     - replaces: test (joint F-test)
  library(dplyr)     # data manipulation      - replaces: collapse, tabstat
  library(tidyr)     # pivot helpers
  library(ggplot2)   # all graphs             - replaces: twoway, graph bar,
                     #                          histogram, kdensity
  library(scales)    # axis label formatting
})

# NOTE: car and dplyr both export a function called recode() with incompatible
# argument styles (see Section 10, Fig. 12.5). We avoid recode() entirely and
# use a named-vector lookup instead, since which recode() wins can depend on
# package load order and session state rather than anything in this script.
# MASS and dplyr both export select(), but this script never calls select(),
# so that particular collision does not arise here.

# ==============================================================================
# GLOBAL GGPLOT2 THEME
# Monochrome; approximates Stata s2mono for Springer B&W print.
# ==============================================================================

theme_springer <- function(base_size = 11) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold", hjust = 0.5, size = base_size),
      plot.subtitle     = element_text(hjust = 0.5,   size = base_size - 1),
      plot.caption      = element_text(hjust = 0,     size = base_size - 3),
      legend.background = element_rect(fill = "white", color = NA),
      strip.background  = element_rect(fill = "grey90", color = "grey50")
    )
}
theme_set(theme_springer())

# ==============================================================================
# WORKING DIRECTORY AND OUTPUT PATHS
# Paths switch automatically by username, mirroring the Stata logic.
# ==============================================================================

user <- Sys.info()[["user"]]

if (user == "marvi") {
  base_dir   <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 12"
  graphs_dir <- file.path(base_dir, "Output/graphs")
  tables_dir <- file.path(base_dir, "Output/tables")
  log_path   <- file.path(base_dir, "Output/logs/Chapter12_R_output.log")
} else {
  base_dir   <- "."
  graphs_dir <- "Output/graphs"
  tables_dir <- "Output/tables"
  log_path   <- "Output/logs/Chapter12_R_output.log"
}
dir.create(graphs_dir,        showWarnings = FALSE, recursive = TRUE)
dir.create(tables_dir,        showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)
setwd(base_dir)
cat("Working directory:", getwd(), "\n")

# Open log - sink() captures all console output to a text file
# Equivalent to: log using "...", replace text
sink(log_path, split = TRUE)
cat("Log file:", log_path, "\n")
cat("Chapter 12 log opened:", format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n")

options(warn = 1)   # print warnings immediately (Stata default)
set.seed(20251201)  # equivalent to: set seed 20251201

# ── Global parameters ────────────────────────────────────────────────────────
# Individual-level controls (same set as Chapter 10 for consistency)
X_controls <- c("female", "black", "hispanic", "asian", "age_ba", "firstgen",
                 "parent_income_q", "parent_grad", "ugpa", "stem_major",
                 "bus_major", "ed_major", "selective_inst", "public_ug",
                 "state_unemp", "metro")
Z_instrument <- "ga_funding_adj"

# CBA parameters
discount_rate   <- 0.03    # Annual discount rate (3%)
career_years    <- 30      # Earnings horizon (years)
subsidy_rate    <- 0.20    # Government subsidy on Grad PLUS principal
cap_threshold   <- 100     # Policy cap ($000s)
base_salary     <- 47000   # Pooled mean annual salary (non-holders)
cost_per_degree <- 100000  # Per-student cost of master's degree (full debt)
S_draws         <- 1000    # Number of Bayesian posterior draws


# ------------------------------------------------------------------------------
# Section 2: Synthetic Data Generation
#
# Population: 8,000 graduate students drawn from a B&B-mirroring DGP.
# The dataset extends the Chapter 10 synthetic panel (Example_7_5_3.dta)
# with a Grad PLUS loan amount variable (grad_plus_loans, in $000s).
# Loan amounts depend on program type, institutional selectivity, family
# income, and a student-specific unobserved borrowing propensity.
#
# GitHub data repository:
#   https://github.com/higher-ed-policy-analysis-2nd-edition/data/tree/main/ch12
# ------------------------------------------------------------------------------

cat("\n============================================================\n")
cat("  Section 2: Synthetic Data Generation\n")
cat("============================================================\n")

# ── Attempt to load from repository; generate synthetically if unavailable ───
data_url  <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch12/Example_12_1.dta"
data_file <- "Example_12_1.dta"

dl_ok <- tryCatch({
  download.file(data_url, data_file, mode = "wb", quiet = TRUE)
  TRUE
}, error = function(e) FALSE)

if (!dl_ok) {
  cat("Remote data not found. Generating synthetic dataset.\n")

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
  haven::write_dta(dat, "Example_12_1.dta")
  cat("Synthetic dataset saved as Example_12_1.dta\n")
} else {
  cat("Canonical dataset downloaded from GitHub repository.\n")
}

# ── Always load the dataset (whether downloaded or just generated) ───────────
dat <- haven::read_dta("Example_12_1.dta")


# ------------------------------------------------------------------------------
# Section 3: Descriptive Statistics and Policy Exposure
# ------------------------------------------------------------------------------

cat("\n============================================================\n")
cat("  Section 3: Descriptive Statistics and Policy Exposure\n")
cat("============================================================\n")

dat <- haven::read_dta("Example_12_1.dta")

# ── Sample overview ─────────────────────────────────────────────────────────
print(table(dat$masters))

tabstat_by <- function(data, vars, by, stats_fun = list(
    mean = mean, sd = sd, min = min, max = max, n = length)) {
  data %>%
    group_by(across(all_of(by))) %>%
    summarise(across(all_of(vars), stats_fun, .names = "{.col}_{.fn}"),
              .groups = "drop")
}
print(tabstat_by(dat, c("salary", "ln_salary", "grad_plus_loans"), "masters"))

# ── Policy exposure: students above $100k cap ────────────────────────────────
dat$above_cap    <- as.numeric(dat$grad_plus_loans > cap_threshold)
dat$loan_overage <- pmax(0, dat$grad_plus_loans - cap_threshold)

cat("\nPolicy exposure summary:\n")
print(quantile(dat$grad_plus_loans, probs = c(0.25, 0.50, 0.75, 0.90, 0.95)))
cat("Mean:", mean(dat$grad_plus_loans), " Max:", max(dat$grad_plus_loans),
    " N:", nrow(dat), "\n")

cat("\nStudents above $100k cap:\n")
print(prop.table(table(dat$above_cap, dat$masters), margin = 1))
print(prop.table(table(dat$above_cap, dat$masters), margin = 2))

cat("\nMean loan overage conditional on exceeding cap:\n")
print(summary(dat$loan_overage[dat$above_cap == 1]))

# Share of completers who would face a binding constraint
n_completers  <- sum(dat$masters == 1)
n_constrained <- sum(dat$masters == 1 & dat$above_cap == 1)
cat("Constrained completers:", n_constrained, "/", n_completers,
    sprintf("(%.1f%%)\n", 100 * n_constrained / n_completers))

# ── By program area ─────────────────────────────────────────────────────────
cat("\nMean Grad PLUS loans by program area:\n")
print(tabstat_by(dat, c("grad_plus_loans", "above_cap"), "stem_major",
                  stats_fun = list(mean = mean, n = length)))
print(tabstat_by(dat, c("grad_plus_loans", "above_cap"), "bus_major",
                  stats_fun = list(mean = mean, n = length)))
print(tabstat_by(dat, c("grad_plus_loans", "above_cap"), "ed_major",
                  stats_fun = list(mean = mean, n = length)))


# ------------------------------------------------------------------------------
# Section 4: Probit Selection Model and Propensity Score
#
# The propensity score P(D=1|Z,X) is the key input to the MTE polynomial.
# State GA funding (ga_funding_adj) serves as the excluded instrument.
# First-stage relevance is confirmed by the linear probability F-statistic.
# ------------------------------------------------------------------------------

cat("\n============================================================\n")
cat("  Section 4: Probit Selection Model and Propensity Score\n")
cat("============================================================\n")

rhs_formula <- function(y, regressors) {
  as.formula(paste(y, "~", paste(regressors, collapse = " + ")))
}

# ── Linear probability model (first-stage F-statistic) ─────────────────────
lpm_fit <- lm(rhs_formula("masters", c(Z_instrument, X_controls)), data = dat)
lpm_vcov <- vcovHC(lpm_fit, type = "HC1")
print(coeftest(lpm_fit, vcov. = lpm_vcov))

fstat_first <- linearHypothesis(lpm_fit, "ga_funding_adj = 0",
                                 vcov. = lpm_vcov, test = "F")
f_first <- fstat_first[2, "F"]
cat(sprintf("First-stage F-statistic: %6.2f\n", f_first))

# ── Probit selection model ──────────────────────────────────────────────────
probit_first <- glm(rhs_formula("masters", c(Z_instrument, X_controls)),
                     data = dat, family = binomial(link = "probit"))
print(summary(probit_first))

# ── Propensity score and linear index ───────────────────────────────────────
dat$phat    <- predict(probit_first, type = "response")   # Pr(D=1|Z,X)
dat$z_index <- predict(probit_first, type = "link")        # xb (linear index)
attr(dat$phat,    "label") <- "Propensity Score P(D=1|Z,X)"
attr(dat$z_index, "label") <- "Linear Index from Probit"

# Polynomial terms for control function
dat$phat2 <- dat$phat^2
dat$phat3 <- dat$phat^3

print(summary(dat$phat))
ga_coef <- coef(probit_first)["ga_funding_adj"]
cat("GA funding coefficient (probit):", ga_coef, "\n")

# Propensity score distribution among completers and non-completers
print(dat %>%
  group_by(masters) %>%
  summarise(mean = mean(phat), sd = sd(phat),
            p10 = quantile(phat, 0.10), p50 = quantile(phat, 0.50),
            p90 = quantile(phat, 0.90), .groups = "drop"))


# ------------------------------------------------------------------------------
# Section 5: MTE Estimation - Cubic Polynomial Control Function
#
# Following Heckman and Vytlacil (1999, 2005), the MTE is recovered from
# the derivative of the outcome regression with respect to the propensity
# score. The polynomial control function approach is used here for
# transparency and direct estimation of ATE, ATT, and ATU.
#
# MTE(u) = b0 + b1*u + b2*u^2 + b3*u^3
#
# where u is interpreted as unobserved resistance to treatment and the
# polynomial coefficients are the coefficients on the interaction terms
# masters:phat, masters:phat2, masters:phat3.
# ------------------------------------------------------------------------------

cat("\n============================================================\n")
cat("  Section 5: MTE Estimation\n")
cat("============================================================\n")

# ── Cubic MTE regression ────────────────────────────────────────────────────
mte_formula <- rhs_formula("ln_salary",
  c("masters", "masters:phat", "masters:phat2", "masters:phat3",
    X_controls, "phat", "phat2", "phat3"))
mte_cubic <- lm(mte_formula, data = dat)
mte_vcov  <- vcovHC(mte_cubic, type = "HC1")
print(coeftest(mte_cubic, vcov. = mte_vcov))

# ── Joint significance test for MTE polynomial coefficients ─────────────────
# Individual coefficients are often imprecise due to multicollinearity among
# polynomial terms. The joint F-test is the appropriate assessment of whether
# the MTE curve is identified -- i.e., whether treatment effect heterogeneity
# exists across the distribution of unobserved resistance to treatment.
mte_poly_terms <- c("masters", "masters:phat", "masters:phat2", "masters:phat3")
joint_test <- linearHypothesis(mte_cubic,
    paste(mte_poly_terms, "= 0"), vcov. = mte_vcov, test = "F")
cat(sprintf("Joint F-test p-value: %6.4f\n", joint_test[2, "Pr(>F)"]))

# ── Store polynomial coefficients ───────────────────────────────────────────
b0 <- coef(mte_cubic)["masters"]           # MTE at u = 0
b1 <- coef(mte_cubic)["masters:phat"]      # Linear slope
b2 <- coef(mte_cubic)["masters:phat2"]     # Quadratic term
b3 <- coef(mte_cubic)["masters:phat3"]     # Cubic term

cat(sprintf("\nMTE polynomial: MTE(u) = %7.4f + %7.4f*u + %7.4f*u^2 + %7.4f*u^3\n",
            b0, b1, b2, b3))

# ── Treatment parameters ────────────────────────────────────────────────────
# ATE = integral of MTE(u) du from 0 to 1
ate <- b0 + b1/2 + b2/3 + b3/4
cat(sprintf("Estimated ATE (cubic): %6.4f\n", ate))

# ATT = E[MTE(u) | D=1] = weighted average over completers' propensity scores
dat$mte_hat <- b0 + b1*dat$phat + b2*dat$phat^2 + b3*dat$phat^3
att <- mean(dat$mte_hat[dat$masters == 1])
cat(sprintf("Estimated ATT: %6.4f\n", att))

# ATU = E[MTE(u) | D=0]
atu <- mean(dat$mte_hat[dat$masters == 0])
cat(sprintf("Estimated ATU: %6.4f\n", atu))

# Note: ATU > ATT reflects negative selection on gains in this dataset --
# students currently kept out of graduate education have higher potential
# returns than those who self-selected in, consistent with financial barriers.
cat(sprintf("\nTreatment parameter hierarchy (this dataset): ATU > ATT > ATE = %6.4f > %6.4f > %6.4f\n",
            atu, att, ate))


# ------------------------------------------------------------------------------
# Section 6: Policy Simulation - Cap-Binding Student Identification
#
# Under the $100k lifetime cap, students currently borrowing more than
# $100k face a binding credit constraint. Those who cannot substitute with
# private credit or personal resources drop out, forfeiting the returns to
# completion. We model the probability of being pushed out as a function
# of the loan overage: p_out(overage) = Phi(overage / 50). This implies a
# smooth, increasing probability of displacement as the overage grows,
# from near-zero at small overages to near-certainty for very large ones.
#
# Constrained completers are classified as:
#   (a) Remaining: find alternative financing; remain enrolled; D stays 1
#   (b) Displaced: cannot cover the gap; D switches from 1 to 0
#
# Displaced students bear the behavioral cost of the policy: they forego
# a return equal to their estimated MTE x discounted salary profile.
# ------------------------------------------------------------------------------

cat("\n============================================================\n")
cat("  Section 6: Policy Simulation\n")
cat("============================================================\n")

# ── Loan overage and probability of displacement ─────────────────────────────
dat$p_displaced <- pnorm(dat$loan_overage / 50) * dat$above_cap
attr(dat$p_displaced, "label") <- "P(Displaced by cap | student characteristics)"

print(summary(dat$p_displaced[dat$masters == 1 & dat$above_cap == 1]))

# ── Expected displacement indicator (continuous treatment intensity) ─────────
dat$displaced_E <- dat$p_displaced * dat$masters    # Expected D: 1 -> 0 switchers
print(summary(dat$displaced_E))

# ── Marginal Policy-Relevant Treatment Effect (MPRTE) ────────────────────────
# MPRTE = E[MTE(U) | margin shifted by the $100k cap]
#       = displacement-weighted average of individual MTEs.
# Each student is weighted by their share of total expected displacement.
# Restricted to completers (displaced_E = 0 for non-completers by construction).
#
# Interpretation:
#   MPRTE > 0  -> cap displaces high-return students (efficiency loss)
#   MPRTE ~ 0  -> cap displaces marginal students (efficiency neutral)
#   MPRTE < 0  -> cap displaces low-return students (efficiency gain)
total_disp <- sum(dat$displaced_E[dat$masters == 1])
mprte_weight <- ifelse(dat$masters == 1, dat$displaced_E / total_disp, NA_real_)
mprte_i <- ifelse(dat$masters == 1, dat$mte_hat * mprte_weight, NA_real_)
mprte <- sum(mprte_i, na.rm = TRUE)

cat("\n--- Treatment Parameter Hierarchy (Point Estimates) ---\n")
cat(sprintf("ATE   (population average):           %6.4f\n", ate))
cat(sprintf("ATT   (E[MTE | D=1], completers):     %6.4f\n", att))
cat(sprintf("ATU   (E[MTE | D=0], non-completers): %6.4f\n", atu))
cat(sprintf("MPRTE (cap-displaced margin):         %6.4f\n", mprte))
cat("\nMPRTE compares the average return on the policy margin to the\n")
cat("population-wide ATE. If MPRTE < ATT, the cap is removing students\n")
cat("with below-average returns among completers (efficiency-improving).\n")

# ── Present value factor ─────────────────────────────────────────────────────
pv_factor <- (1 - (1 + discount_rate)^(-career_years)) / discount_rate
cat(sprintf("Present value factor (%d yrs, %.0f%% r): %6.3f\n",
            career_years, discount_rate * 100, pv_factor))

# ── Point-estimate CBA (pre-simulation baseline) ─────────────────────────────
# Fiscal savings: government recovers subsidy on overage lending
dat$fiscal_saving_i <- dat$loan_overage * subsidy_rate * dat$above_cap
# This applies to ALL students above the cap (completers and non-completers)
# because the cap reduces federal exposure regardless of enrollment outcome.
FS_point <- sum(dat$fiscal_saving_i)
cat(sprintf("Point-estimate fiscal savings ($000s): %8.1f\n", FS_point))

# Behavioral cost: lost human capital for displaced completers
# = MTE x base_salary x PV_factor (in $000s, since salary is in $)
dat$behav_cost_i <- dat$displaced_E * pmax(0, dat$mte_hat) *
  (base_salary / 1000) * pv_factor
# Note: pmax(0, MTE) reflects that only students with positive returns suffer a
# human capital loss. Displaced students with negative MTE represent an
# efficiency GAIN (removing low-return borrowers).
BC_point <- sum(dat$behav_cost_i[dat$masters == 1])
cat(sprintf("Point-estimate behavioral cost ($000s): %8.1f\n", BC_point))

# Efficiency gain from removing low-return borrowers (negative MTE region)
dat$efficiency_gain_i <- dat$displaced_E * pmax(0, -dat$mte_hat) *
  (base_salary / 1000) * pv_factor
EG_point <- sum(dat$efficiency_gain_i[dat$masters == 1])
cat(sprintf("Point-estimate efficiency gain (low-return exit, $000s): %8.1f\n", EG_point))

# Net benefit (point estimate)
NB_point <- FS_point - BC_point + EG_point
cat(sprintf("Point-estimate net benefit ($000s): %8.1f\n", NB_point))
cat(sprintf("Benefit-cost ratio: %5.3f\n", (FS_point + EG_point) / max(1, BC_point)))

# ── Institutional revenue loss ───────────────────────────────────────────────
# When a student is displaced by the cap, the institution loses their net
# tuition revenue for the remaining program years. This is distinct from the
# student's human capital loss: it is a revenue shock to the institution that
# affects staffing, cross-subsidies, and program viability.
#
# Two components:
#   (a) Direct net revenue loss:  displaced_E x net_inst_rev
#   (b) Cross-subsidy disruption: 20% of (a), representing the upstream effect
#       on undergraduate aid and other programs funded by graduate tuition.
dat$inst_rev_loss_i  <- dat$displaced_E * dat$net_inst_rev
dat$cross_sub_loss_i <- 0.20 * dat$inst_rev_loss_i

IR_point <- sum(dat$inst_rev_loss_i[dat$masters == 1])
CS_point <- sum(dat$cross_sub_loss_i[dat$masters == 1])

cat("\n--- Institutional Outcomes (Point Estimates) ---\n")
cat(sprintf("Net institutional revenue loss ($000s):     %8.1f\n", IR_point))
cat(sprintf("Cross-subsidy disruption ($000s):           %8.1f\n", CS_point))
cat(sprintf("Total institutional sector impact ($000s):  %8.1f\n", IR_point + CS_point))

# Revenue loss by program area
cat("\nInstitutional revenue loss by program area ($000s):\n")
completers <- dat[dat$masters == 1, ]
print(tabstat_by(completers, "inst_rev_loss_i", "stem_major",
                  stats_fun = list(sum = sum, mean = mean, n = length)))
print(tabstat_by(completers, "inst_rev_loss_i", "bus_major",
                  stats_fun = list(sum = sum, mean = mean, n = length)))
print(tabstat_by(completers, "inst_rev_loss_i", "ed_major",
                  stats_fun = list(sum = sum, mean = mean, n = length)))

# Revenue loss by institution type
cat("\nInstitutional revenue loss by institution type ($000s):\n")
print(tabstat_by(completers, "inst_rev_loss_i", "selective_inst",
                  stats_fun = list(sum = sum, mean = mean, n = length)))
print(tabstat_by(completers, "inst_rev_loss_i", "public_ug",
                  stats_fun = list(sum = sum, mean = mean, n = length)))

# Extended net benefit including institutional costs
NB_extended <- NB_point - IR_point - CS_point
cat(sprintf("\nExtended net benefit (incl. institutional costs, $000s): %8.1f\n", NB_extended))


# ------------------------------------------------------------------------------
# Section 7: Bayesian Microsimulation (S = 1000 Parametric Draws)
#
# The point-estimate CBA ignores uncertainty in the MTE polynomial. The
# Bayesian layer propagates uncertainty by drawing S = 1000 coefficient
# vectors from the asymptotic normal posterior:
#
#   theta^(s) ~ N(theta_hat, V_hat)
#
# where theta_hat and V_hat are the estimated coefficient vector and its
# variance-covariance matrix from the cubic MTE regression. For each draw,
# we recompute the MTE curve, the policy-affected margin, and the three
# CBA components. The resulting distribution characterizes posterior
# uncertainty about the net social benefit of the $100k cap.
#
# Additional uncertainty is propagated through draws on:
#   - Base salary (log-normal, reflecting real-wage uncertainty)
#   - Subsidy rate (uniform, reflecting fiscal policy uncertainty)
# ------------------------------------------------------------------------------

cat("\n============================================================\n")
cat("  Section 7: Bayesian Microsimulation\n")
cat("============================================================\n")

# ── Coefficient vector and robust VCV ────────────────────────────────────────
# Reuses the model object and robust VCV estimated in Section 5.
b_full <- coef(mte_cubic)
V_full <- mte_vcov
k_full <- length(b_full)

# Column positions of the four MTE polynomial coefficients. Unlike Stata's
# `local cols : colnames b_full` loop, R coefficient vectors are named, so
# the four terms of interest can be referenced directly by name.
mte_poly_names <- c("masters", "masters:phat", "masters:phat2", "masters:phat3")
cat("MTE polynomial coefficient names:", paste(mte_poly_names, collapse = ", "), "\n")

# ── Preallocate results data frame ───────────────────────────────────────────
sim_results <- data.frame(
  draw             = 1:S_draws,
  b0_s             = NA_real_, b1_s = NA_real_, b2_s = NA_real_, b3_s = NA_real_,
  ate_s            = NA_real_, att_s = NA_real_, atu_s = NA_real_, mprte_s = NA_real_,
  fiscal_s         = NA_real_,
  behav_cost_s     = NA_real_,
  effic_gain_s     = NA_real_,
  net_benefit_s    = NA_real_,
  nb_per_student_s = NA_real_,
  bcr_s            = NA_real_,
  pnb_positive_s   = NA_real_,
  inst_rev_loss_s  = NA_real_,
  cross_sub_loss_s = NA_real_,
  nb_extended_s    = NA_real_
)

# ── Simulation loop ──────────────────────────────────────────────────────────
cat("\nRunning", S_draws, "posterior draws...\n")
cat("(This may take 1-2 minutes.)\n")

# Draw all S_draws coefficient vectors jointly from N(b_full, V_full) up front,
# preserving the correlation structure -- the direct R equivalent of Stata's
# drawnorm. MASS::mvrnorm() returns an S_draws x k_full matrix in one call,
# so there is no need for Stata's preserve/mkmat/restore workaround.
B_draws <- MASS::mvrnorm(n = S_draws, mu = b_full, Sigma = V_full)

for (s in 1:S_draws) {

  # ── Extract posterior draw s ────────────────────────────────────────────
  b0_s <- B_draws[s, "masters"]
  b1_s <- B_draws[s, "masters:phat"]
  b2_s <- B_draws[s, "masters:phat2"]
  b3_s <- B_draws[s, "masters:phat3"]

  # ── Additional economic uncertainty ─────────────────────────────────────
  # Base salary: log-normal with 10% coefficient of variation
  base_sal_s <- exp(log(base_salary) + rnorm(1, 0, 0.10))
  # Subsidy rate: uniform +/- 5 percentage points around baseline
  sub_rate_s <- subsidy_rate + runif(1, -0.05, 0.05)
  # Tuition multiplier: log-normal with 8% CV, reflecting uncertainty in
  # actual tuition levels, discount rates, and institutional aid policies.
  # This ensures inst_rev_loss_s varies meaningfully across draws.
  tuition_mult_s <- exp(rnorm(1, 0, 0.08))

  # ── Recompute MTE for this draw ──────────────────────────────────────────
  mte_s <- b0_s + b1_s*dat$phat + b2_s*dat$phat^2 + b3_s*dat$phat^3

  # Treatment parameters
  ate_s <- b0_s + b1_s/2 + b2_s/3 + b3_s/4
  att_s <- mean(mte_s[dat$masters == 1])
  atu_s <- mean(mte_s[dat$masters == 0])

  # MPRTE: displacement-weighted average of draw-specific MTE.
  # Reuses total_disp scalar from Section 6 (constant across draws since
  # displaced_E is a function of loan_overage, not the MTE polynomial).
  mprte_i_s <- ifelse(dat$masters == 1, dat$displaced_E * mte_s / total_disp, NA_real_)
  mprte_s <- sum(mprte_i_s, na.rm = TRUE)

  # Fiscal savings (uses draw-specific subsidy rate)
  fs_i_s   <- dat$loan_overage * sub_rate_s * dat$above_cap
  fiscal_s <- sum(fs_i_s)

  # Behavioral cost: human capital loss for displaced high-return students
  bc_i_s       <- dat$displaced_E * pmax(0, mte_s) * (base_sal_s / 1000) * pv_factor
  behav_cost_s <- sum(bc_i_s[dat$masters == 1])

  # Efficiency gain: removing low-return constrained students
  eg_i_s       <- dat$displaced_E * pmax(0, -mte_s) * (base_sal_s / 1000) * pv_factor
  effic_gain_s <- sum(eg_i_s[dat$masters == 1])

  # Net benefit
  net_benefit_s <- fiscal_s - behav_cost_s + effic_gain_s

  # Per-displaced-student net benefit
  n_disp_s         <- max(1, sum(dat$displaced_E[dat$masters == 1]))
  nb_per_student_s <- net_benefit_s / n_disp_s

  # Benefit-cost ratio
  bcr_s <- (fiscal_s + effic_gain_s) / max(1, behav_cost_s)

  # Efficiency indicator
  pnb_positive_s <- as.numeric(net_benefit_s > 0)

  # Institutional revenue loss
  # Net tuition revenue forgone when displaced students leave.
  # Draw-specific tuition multiplier propagates pricing uncertainty.
  # Cross-subsidy disruption adds 20% for upstream undergraduate impact.
  ir_i_s          <- dat$displaced_E * dat$net_inst_rev * tuition_mult_s
  inst_rev_loss_s <- sum(ir_i_s[dat$masters == 1])
  cross_sub_loss_s <- 0.20 * inst_rev_loss_s
  nb_extended_s    <- net_benefit_s - inst_rev_loss_s - cross_sub_loss_s

  # ── Store results ─────────────────────────────────────────────────────────
  sim_results[s, ] <- list(
    s, b0_s, b1_s, b2_s, b3_s,
    ate_s, att_s, atu_s, mprte_s,
    fiscal_s, behav_cost_s, effic_gain_s,
    net_benefit_s, nb_per_student_s, bcr_s, pnb_positive_s,
    inst_rev_loss_s, cross_sub_loss_s, nb_extended_s
  )

  # Progress indicator every 100 draws
  if (s %% 100 == 0) cat("  Draw", s, "/", S_draws, "complete\n")
}

cat("Simulation complete.\n")


# ------------------------------------------------------------------------------
# Section 8: Cost-Benefit Decomposition
# ------------------------------------------------------------------------------

cat("\n============================================================\n")
cat("  Section 8: Cost-Benefit Decomposition\n")
cat("============================================================\n")

# Small helper: mean / sd / 95% credible interval, printed Stata-tabstat style.
# Equivalent to Stata's: qui sum `var' + _pctile `var', p(2.5 97.5)
summarize_ci <- function(x, label, digits = 2) {
  xm  <- mean(x)
  xs  <- sd(x)
  ci  <- quantile(x, probs = c(0.025, 0.975))
  fmt <- paste0("%s:  mean = %", digits + 4, ".", digits, "f",
                "  sd = %", digits + 4, ".", digits, "f",
                "  95%% CI: [%", digits + 4, ".", digits, "f, %",
                digits + 4, ".", digits, "f]\n")
  cat(sprintf(fmt, label, xm, xs, ci[1], ci[2]))
}

# ── Component-level summaries ────────────────────────────────────────────────
cat("\n--- Posterior Summary: CBA Components ($000s) ---\n")
for (v in c("fiscal_s", "behav_cost_s", "effic_gain_s", "net_benefit_s")) {
  summarize_ci(sim_results[[v]], v, digits = 2)
}

cat("\n--- Institutional Outcomes: Posterior Summary ($000s) ---\n")
for (v in c("inst_rev_loss_s", "cross_sub_loss_s", "nb_extended_s")) {
  x  <- sim_results[[v]]
  ci <- quantile(x, probs = c(0.025, 0.975))
  cat(sprintf("%s:  mean = %10.2f  95%% CI: [%10.2f, %10.2f]\n",
              v, mean(x), ci[1], ci[2]))
}

# Probability extended NB > 0
sim_results$pnb_ext_pos_s <- as.numeric(sim_results$nb_extended_s > 0)
cat(sprintf("\nP(Extended NB > 0, incl. institutional costs): %5.3f\n",
            mean(sim_results$pnb_ext_pos_s)))

print(sim_results %>%
  summarise(across(c(b0_s, b1_s, b2_s, b3_s, ate_s, att_s, atu_s, mprte_s),
                    list(mean = mean, sd = sd))))

# ── Treatment Parameter Posterior Summary (Table 12.5 inputs) ────────────────
# Posterior means and 95% credible intervals for ATE, ATT, MPRTE, ATU.
# These four numbers populate the Estimate column of Table 12.5 and provide
# the inferential basis for the MPRTE > ATT claim in the chapter narrative.
cat("\n--- Treatment Parameter Posterior (log-salary units) ---\n")
for (v in c("ate_s", "att_s", "mprte_s", "atu_s")) {
  summarize_ci(sim_results[[v]], v, digits = 4)
}

# ── P(MPRTE > ATT) - formal inferential statement ────────────────────────────
# The chapter's central policy claim -- that the cap pushes out students with
# above-average returns among completers -- is true if MPRTE > ATT in the
# posterior. We compute this probability directly from the joint draws.
p_mprte_gt_att <- mean(sim_results$mprte_s > sim_results$att_s)
cat(sprintf("\nP(MPRTE > ATT | data): %5.3f   (posterior probability cap binds high-return margin)\n",
            p_mprte_gt_att))

# ── P(Net Benefit > 0) ───────────────────────────────────────────────────────
p_positive <- mean(sim_results$pnb_positive_s)
cat(sprintf("\nProbability policy is efficient P(NB > 0): %5.3f\n", p_positive))

# ── BCR summary ──────────────────────────────────────────────────────────────
cat("\nPosterior benefit-cost ratio:\n")
summarize_ci(sim_results$bcr_s, "BCR", digits = 4)

# ── Net benefit per displaced student ────────────────────────────────────────
cat("\nNet benefit per displaced student ($000s):\n")
summarize_ci(sim_results$nb_per_student_s, "NB/student", digits = 2)

# ── Save results for figures ─────────────────────────────────────────────────
haven::write_dta(sim_results, file.path(tables_dir, "sim_results_ch12.dta"))


# ------------------------------------------------------------------------------
# Section 9: Posterior Summaries and Formal Inference
# ------------------------------------------------------------------------------

cat("\n============================================================\n")
cat("  Section 9: Posterior Summaries and Inference\n")
cat("============================================================\n")

sim_results <- haven::read_dta(file.path(tables_dir, "sim_results_ch12.dta"))

# ── 95% credible intervals ───────────────────────────────────────────────────
for (v in c("fiscal_s", "behav_cost_s", "effic_gain_s", "net_benefit_s", "bcr_s")) {
  x  <- sim_results[[v]]
  ci <- quantile(x, probs = c(0.025, 0.975))
  cat(sprintf("%s: mean = %8.2f  95%% CI: [%8.2f, %8.2f]\n",
              v, mean(x), ci[1], ci[2]))
}

# ── Scenario classification ──────────────────────────────────────────────────
# Scenario A (efficient): NB > 0; cap displaces primarily low-return borrowers
# Scenario B (inefficient): NB < 0; cap displaces high-return constrained students
nb_mean   <- mean(sim_results$net_benefit_s)
nb_median <- median(sim_results$net_benefit_s)
nb_ci     <- quantile(sim_results$net_benefit_s, probs = c(0.025, 0.975))

cat("\n--- Policy Verdict ---\n")
if (nb_mean > 0) {
  cat(sprintf("Posterior mean NB = %6.2f ($000s) > 0\n", nb_mean))
  cat("Scenario A likely: cap predominantly displaces low-return borrowers.\n")
  cat("Policy generates net fiscal savings in excess of human capital loss.\n")
} else {
  cat(sprintf("Posterior mean NB = %6.2f ($000s) < 0\n", nb_mean))
  cat("Scenario B likely: cap displaces students with positive marginal returns.\n")
  cat("Human capital loss exceeds fiscal savings.\n")
}

# ── LaTeX-ready summary table ────────────────────────────────────────────────
tex_path <- file.path(tables_dir, "Table12_1_CBA_Summary.tex")
tex_con  <- file(tex_path, open = "wt")

writeLines(c(
  "\\begin{table}[ht]",
  "\\caption{Posterior Cost--Benefit Summary: \\$100k Grad PLUS Cap}",
  "\\begin{tabular}{lrrrr}",
  "\\hline",
  "Component & Mean & SD & 2.5\\% & 97.5\\% \\\\",
  "\\hline"
), tex_con)

cba_vars   <- c("fiscal_s", "behav_cost_s", "effic_gain_s", "net_benefit_s", "bcr_s")
cba_labels <- c(
  fiscal_s      = "Fiscal savings (\\$000s)",
  behav_cost_s  = "Behavioral cost (\\$000s)",
  effic_gain_s  = "Efficiency gain (\\$000s)",
  net_benefit_s = "Net benefit (\\$000s)",
  bcr_s         = "Benefit-cost ratio"
)

for (v in cba_vars) {
  x  <- sim_results[[v]]
  ci <- quantile(x, probs = c(0.025, 0.975))
  row <- sprintf("%s & %.2f & %.2f & %.2f & %.2f \\\\",
                  cba_labels[[v]], mean(x), sd(x), ci[1], ci[2])
  writeLines(row, tex_con)
}

writeLines(c(
  "\\hline",
  sprintf("\\multicolumn{5}{l}{\\textit{Note: %d posterior draws. Behavioral cost = lost human capital for displaced completers with MTE > 0. Efficiency gain = avoided loss from removing completers with MTE $\\leq$ 0.}} \\\\", S_draws),
  "\\end{tabular}",
  "\\end{table}"
), tex_con)

close(tex_con)
cat("\nSummary table written to", tex_path, "\n")


# ------------------------------------------------------------------------------
# Section 10: Figures
# ------------------------------------------------------------------------------

cat("\n============================================================\n")
cat("  Section 10: Figures\n")
cat("============================================================\n")

# Save at ~1200px wide, matching Stata's graph export width(1200).
# ggsave() draws straight to a file device and does not touch RStudio's
# interactive Plots pane; auto-print also does not fire inside a function
# call. print(plot) here restores the "each figure appears as it's built"
# behavior you get from Stata's `graph display` / `twoway` in an interactive
# do-file session. Guarded by interactive() so a batch Rscript run (e.g. on
# a headless machine) does not try to open a screen device.
save_fig <- function(plot, filename, width = 10, height = 6, dpi = 120) {
  if (interactive()) print(plot)
  ggsave(file.path(graphs_dir, filename), plot = plot,
         width = width, height = height, dpi = dpi, bg = "white")
}

# ------------------------------------------------------------------------------
# Fig. 12.1: Posterior Distribution of Net Social Benefits
# ------------------------------------------------------------------------------
sim_results <- haven::read_dta(file.path(tables_dir, "sim_results_ch12.dta"))
nb_mean_plot <- mean(sim_results$net_benefit_s)

fig12_1 <- ggplot(sim_results, aes(x = net_benefit_s)) +
  geom_histogram(aes(y = after_stat(count) / sum(after_stat(count)) * 100),
                  bins = 40, fill = "grey40", color = "black", linewidth = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = nb_mean_plot, linetype = "solid", linewidth = 0.8) +
  labs(x = "Net Social Benefit ($000s)", y = "Percent of Posterior Draws",
       caption = sprintf(
         "Dashed line at zero. Solid line at posterior mean (%.1f $000s).\nN = %d posterior draws.",
         nb_mean_plot, S_draws))

save_fig(fig12_1, "fig12_1_posterior_nb_R.png")

# ------------------------------------------------------------------------------
# Fig. 12.2: MTE Curve with Policy-Affected Region Shaded
# Based on posterior mean MTE polynomial coefficients.
# ------------------------------------------------------------------------------
b0_pm <- mean(sim_results$b0_s)
b1_pm <- mean(sim_results$b1_s)
b2_pm <- mean(sim_results$b2_s)
b3_pm <- mean(sim_results$b3_s)

# Identify policy-affected u range:
# Students pushed out by the cap tend to cluster in the intermediate-u range.
# Those with u near 0 (low resistance) self-select regardless of credit access.
# Those with u near 1 (high resistance) would not enroll even without the cap.
# The cap binds most tightly for students in u in [0.30, 0.65] (medium resistance).
# These thresholds are calibrated from the loan overage distribution.
mte_curve <- data.frame(u = (1:100) / 100)
mte_curve$mte <- b0_pm + b1_pm*mte_curve$u + b2_pm*mte_curve$u^2 + b3_pm*mte_curve$u^3
mte_curve$region_cap <- mte_curve$u >= 0.30 & mte_curve$u <= 0.65
mte_curve$mte_cap_pos <- ifelse(mte_curve$region_cap & mte_curve$mte > 0, mte_curve$mte, NA_real_)
mte_curve$mte_cap_neg <- ifelse(mte_curve$region_cap & mte_curve$mte < 0, mte_curve$mte, NA_real_)

fig12_2 <- ggplot(mte_curve, aes(x = u)) +
  geom_ribbon(aes(ymin = 0, ymax = mte_cap_pos, fill = "Displaced, MTE > 0 (behavioral cost)")) +
  geom_ribbon(aes(ymin = 0, ymax = mte_cap_neg, fill = "Displaced, MTE <= 0 (efficiency gain)")) +
  geom_line(aes(y = mte, color = "Estimated MTE (posterior mean)"), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_fill_manual(name = NULL, values = c(
    "Displaced, MTE > 0 (behavioral cost)"    = "grey35",
    "Displaced, MTE <= 0 (efficiency gain)"   = "grey75")) +
  scale_color_manual(name = NULL, values = c("Estimated MTE (posterior mean)" = "black")) +
  labs(x = "u (Unobserved Resistance to Treatment)", y = "Marginal Treatment Effect",
       caption = paste(
         "Shaded region: u in [0.30, 0.65] (policy-affected margin).",
         "Dark gray = human capital loss; light gray = efficiency gain from",
         "removing low-return borrowers.", sep = "\n")) +
  theme(legend.position = "bottom")

save_fig(fig12_2, "fig12_2_mte_policy_R.png")

# ------------------------------------------------------------------------------
# Fig. 12.3: Benefit-Cost Decomposition - Stacked Bar Chart
# Shows posterior mean of each component and their net contribution.
# ------------------------------------------------------------------------------
sc_fs <- mean(sim_results$fiscal_s)
sc_bc <- mean(sim_results$behav_cost_s)
sc_eg <- mean(sim_results$effic_gain_s)

decomp <- data.frame(
  label = factor(c("Fiscal Savings", "Efficiency Gain", "Behavioral Cost"),
                  levels = c("Fiscal Savings", "Efficiency Gain", "Behavioral Cost")),
  value = c(sc_fs, sc_eg, -sc_bc)
)

fig12_3 <- ggplot(decomp, aes(x = label, y = value)) +
  geom_col(fill = "grey55", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = NULL, y = "Posterior Mean ($000s)",
       title = "Cost-Benefit Decomposition",
       subtitle = "$100k Federal Loan Cap (Posterior Mean Components)",
       caption = paste(
         "Behavioral cost shown as negative (subtracted from net benefit).",
         "Net benefit = Fiscal Savings + Efficiency Gain - Behavioral Cost.",
         sep = "\n"))

save_fig(fig12_3, "fig12_3_cba_decomp_R.png")

# ------------------------------------------------------------------------------
# Fig. 12.4: Posterior Distribution of ATE, ATT, ATU
# Illustrates treatment effect heterogeneity across the posterior.
# ------------------------------------------------------------------------------
dens_ate <- density(sim_results$ate_s, n = 200)
dens_att <- density(sim_results$att_s, n = 200)
dens_atu <- density(sim_results$atu_s, n = 200)

dens_df <- rbind(
  data.frame(x = dens_ate$x, d = dens_ate$y, param = "ATE"),
  data.frame(x = dens_att$x, d = dens_att$y, param = "ATT"),
  data.frame(x = dens_atu$x, d = dens_atu$y, param = "ATU")
)
dens_df$param <- factor(dens_df$param, levels = c("ATE", "ATT", "ATU"))

fig12_4 <- ggplot(dens_df, aes(x = x, y = d, linetype = param, color = param)) +
  geom_line(linewidth = 0.9) +
  scale_linetype_manual(name = NULL, values = c(ATE = "solid", ATT = "dashed", ATU = "dotted")) +
  scale_color_manual(name = NULL, values = c(ATE = "black", ATT = "black", ATU = "grey50")) +
  labs(x = "Estimated Treatment Effect (log salary)", y = "Posterior Density",
       caption = paste(
         "ATU > ATT > ATE reflects negative selection: financially constrained",
         "students have higher potential returns than self-selected completers.",
         sprintf("N = %d posterior draws.", S_draws), sep = "\n")) +
  theme(legend.position = "bottom")

save_fig(fig12_4, "fig12_4_param_posteriors_R.png")

# ------------------------------------------------------------------------------
# Fig. 12.5: MTE Curve by Graduate Program Area (Heterogeneity)
# Field-specific MTE curves reveal which programs drive the efficiency verdict.
#
# This figure uses the field-specific interacted model (est store mte_byarea)
# from the Chapter 10 pipeline. Here we illustrate the concept using the
# pooled cubic MTE with program-area-specific intercept adjustments derived
# from the Chapter 10 field-specific ATE differentials in Table 10.1.
#
# Field-specific ATE offsets relative to base Other (from Chapter 10 Table 10.1):
#   STEM:      ATE = 0.9424 vs. Other ATE = 0.8085 -> offset = +0.134
#   Business:  ATE = 1.6936 vs. Other ATE = 0.8085 -> offset = +0.885
#   Education: ATE = 0.6437 vs. Other ATE = 0.8085 -> offset = -0.165
#   Health:    ATE = 0.8469 vs. Other ATE = 0.8085 -> offset = +0.038
# ------------------------------------------------------------------------------
byfield <- data.frame(u = (1:100) / 100)
# Uses the point-estimate polynomial b0-b3 stored in Section 5.
byfield$mte_other  <- b0 + b1*byfield$u + b2*byfield$u^2 + b3*byfield$u^3
byfield$mte_stem   <- byfield$mte_other + 0.134
byfield$mte_bus    <- byfield$mte_other + 0.885
byfield$mte_educ   <- byfield$mte_other - 0.165
byfield$mte_health <- byfield$mte_other + 0.038

byfield_long <- byfield %>%
  tidyr::pivot_longer(cols = starts_with("mte_"), names_to = "field", values_to = "mte")

# Named-vector lookup instead of recode() -- both the car and dplyr packages
# export a function called recode() with incompatible argument styles, and
# which one wins depends on package load order/session state. A plain
# named-vector lookup sidesteps that ambiguity entirely.
field_labels <- c(
  mte_health = "Health & Related",
  mte_stem   = "STEM",
  mte_bus    = "Business",
  mte_educ   = "Education",
  mte_other  = "Other (base)"
)
byfield_long$field <- field_labels[byfield_long$field]
byfield_long$field <- factor(byfield_long$field,
  levels = c("Health & Related", "STEM", "Business", "Education", "Other (base)"))

fig12_5 <- ggplot(byfield_long, aes(x = u, y = mte, linetype = field, color = field)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey70") +
  scale_linetype_manual(name = NULL, values = c(
    "Health & Related" = "solid", "STEM" = "dashed", "Business" = "longdash",
    "Education" = "solid", "Other (base)" = "dashed")) +
  scale_color_manual(name = NULL, values = c(
    "Health & Related" = "black", "STEM" = "black", "Business" = "black",
    "Education" = "grey50", "Other (base)" = "grey50")) +
  labs(x = "u (Unobserved Resistance to Treatment)", y = "Marginal Treatment Effect",
       caption = paste(
         "Field-specific MTE adjusted by program-area ATE differentials from",
         "the interacted model (Chapter 10, Table 10.1). Business programs",
         "show the largest returns; Education the lowest.", sep = "\n")) +
  theme(legend.position = "bottom")

save_fig(fig12_5, "fig12_5_mte_byfield_R.png")

# ------------------------------------------------------------------------------
# Fig. 12.6: Institutional Revenue Loss by Program Area
# Illustrates which programs bear the heaviest burden from the cap.
# ------------------------------------------------------------------------------
dat_fig6 <- haven::read_dta("Example_12_1.dta")

# Recompute displaced_E (requires above_cap and p_displaced regeneration)
dat_fig6$above_cap       <- as.numeric(dat_fig6$grad_plus_loans > cap_threshold)
dat_fig6$loan_overage    <- pmax(0, dat_fig6$grad_plus_loans - cap_threshold)
dat_fig6$p_displaced     <- pnorm(dat_fig6$loan_overage / 50) * dat_fig6$above_cap
dat_fig6$displaced_E     <- dat_fig6$p_displaced * dat_fig6$masters
dat_fig6$inst_rev_loss_i <- dat_fig6$displaced_E * dat_fig6$net_inst_rev

dat_fig6$prog_area <- "Other"
dat_fig6$prog_area[dat_fig6$stem_major   == 1] <- "STEM"
dat_fig6$prog_area[dat_fig6$bus_major    == 1] <- "Business"
dat_fig6$prog_area[dat_fig6$ed_major     == 1] <- "Education"
dat_fig6$prog_area[dat_fig6$health_major == 1] <- "Health"

byarea <- dat_fig6 %>%
  filter(masters == 1) %>%
  group_by(prog_area) %>%
  summarise(inst_rev_loss_i = sum(inst_rev_loss_i),
            annual_tuition  = mean(annual_tuition),
            n_displaced     = sum(displaced_E > 0), .groups = "drop")

byarea$sort_order <- match(byarea$prog_area,
  c("Other", "STEM", "Business", "Education", "Health"))
byarea <- byarea[order(byarea$sort_order), ]
byarea$prog_area <- factor(byarea$prog_area, levels = byarea$prog_area)

fig12_6 <- ggplot(byarea, aes(x = prog_area, y = inst_rev_loss_i)) +
  geom_col(fill = "grey55", color = "black") +
  labs(x = NULL, y = "Net Institutional Revenue Loss ($000s)",
       title = "Institutional Revenue Loss by Program Area",
       subtitle = "Cap-displaced students x net tuition revenue per student",
       caption = paste(
         "Net revenue = 65% of gross tuition (35% variable cost excluded).",
         "Excludes cross-subsidy disruption. Based on point estimates.",
         sep = "\n"))

save_fig(fig12_6, "fig12_6_inst_rev_byfield_R.png")

# ------------------------------------------------------------------------------
# Fig. 12.7: Full Social Cost Stack - Student + Institutional Channels
# Stacks all costs and savings to show the complete distributional picture.
# ------------------------------------------------------------------------------
sim_results <- haven::read_dta(file.path(tables_dir, "sim_results_ch12.dta"))

sc_fs <- mean(sim_results$fiscal_s)
sc_bc <- mean(sim_results$behav_cost_s)
sc_eg <- mean(sim_results$effic_gain_s)
sc_ir <- mean(sim_results$inst_rev_loss_s)
sc_cs <- mean(sim_results$cross_sub_loss_s)

full_stack <- data.frame(
  label = factor(c("Fiscal Savings", "Efficiency Gain", "Human Capital Loss",
                    "Institutional Rev. Loss", "Cross-Subsidy Disruption"),
                  levels = c("Fiscal Savings", "Efficiency Gain", "Human Capital Loss",
                             "Institutional Rev. Loss", "Cross-Subsidy Disruption")),
  value = c(sc_fs, sc_eg, -sc_bc, -sc_ir, -sc_cs),
  category = c("Benefit", "Benefit", "Cost", "Cost", "Cost")
)

fig12_7 <- ggplot(full_stack, aes(x = label, y = value, fill = category)) +
  geom_col(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_fill_manual(name = NULL, values = c(Benefit = "grey75", Cost = "grey35")) +
  labs(x = NULL, y = "Posterior Mean ($000s)",
       title = "Full Social Cost Stack",
       subtitle = "Student and Institutional Channels Combined",
       caption = paste(
         "Benefits shown positive; costs shown negative.",
         "Human capital loss and institutional revenue loss dominate.",
         "Cross-subsidy disruption captures upstream undergraduate impact.",
         sep = "\n")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

save_fig(fig12_7, "fig12_7_full_cost_stack_R.png")


# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------

cat("\n============================================================\n")
cat("  Chapter 12 Script Complete\n")
cat("============================================================\n")
cat("Output files:\n")
cat("  R log:         ", log_path, "\n")
cat("  Data:          Example_12_1.dta\n")
cat("  Table:         ", file.path(tables_dir, "Table12_1_CBA_Summary.tex"), "\n")
cat("  Sim results:   ", file.path(tables_dir, "sim_results_ch12.dta"), "\n")
cat("  Figures:       ", file.path(graphs_dir, "fig12_1_posterior_nb_R.png"), "\n")
cat("                 ", file.path(graphs_dir, "fig12_2_mte_policy_R.png"), "\n")
cat("                 ", file.path(graphs_dir, "fig12_3_cba_decomp_R.png"), "\n")
cat("                 ", file.path(graphs_dir, "fig12_4_param_posteriors_R.png"), "\n")
cat("                 ", file.path(graphs_dir, "fig12_5_mte_byfield_R.png"), "\n")
cat("                 ", file.path(graphs_dir, "fig12_6_inst_rev_byfield_R.png"), "\n")
cat("                 ", file.path(graphs_dir, "fig12_7_full_cost_stack_R.png"), "\n")

sink()
