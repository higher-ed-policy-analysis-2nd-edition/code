# ==============================================================================
# Chapter 12: Bayesian MTE Microsimulation
# Cost-Benefit Analysis of the $100k Aggregate Limit on Federal Graduate Loans
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
#   This script evaluates the net social benefit of the $100,000 aggregate
#   limit on federal graduate borrowing enacted in the One Big Beautiful
#   Bill Act (OBBBA; Pub. L. 119-21), effective July 1, 2026. The OBBBA
#   ends Grad PLUS loans for new borrowers and limits Direct Unsubsidized
#   loans for students in non-professional graduate programs to $20,500 a
#   year and $100,000 in total (excluding undergraduate loans). This script
#   models the $100,000 aggregate limit only (variable names such as
#   grad_plus_loans and "cap" are retained from earlier drafts; the annual
#   limit is not modeled). Using the marginal treatment effect (MTE)
#   framework from Chapter 11, we estimate
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
#   Section 5.  MTE estimation - quadratic polynomial control function
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
#
# REVISION (Nov 2026; mirrors Stata_code12.do): comment, labeling, and
#   reporting changes only. No estimate, simulation draw, or figure value
#   changes, EXCEPT that (a) Sections 3 and 6 now report exposure and
#   institutional revenue loss for all five program areas (adds Health and
#   Other), (b) Section 5 adds a heterogeneity test of the three MTE
#   interaction terms, and (c) Fig. 12.5's caption no longer cites
#   "Chapter 10, Table 10.1". Items flagged "UNDER REVIEW" change results
#   and await the author's decision.
#
# REVISION 2 (Nov 2026; mirrors Stata_code12.do): MTE corrections. THESE
#   CHANGE RESULTS.
#   (1) MTE (Section 5): MTE(u) = beta(u) + u*beta'(u) + pi'(u), where pi(.)
#       is the polynomial on phat, phat2, phat3. For the cubic,
#       MTE(u) = m0 + m1*u + m2*u^2 + m3*u^3 with m0 = b0 + c1,
#       m1 = 2(b1 + c2), m2 = 3(b2 + c3), m3 = 4*b3 (Eq. 12.3).
#   (2) ATE = int_0^1 MTE; ATT = sum I(p_i) / sum p_i;
#       ATU = sum [I(1) - I(p_i)] / sum (1 - p_i), I(p) = int_0^p MTE.
#   (3) A displaced completer's expected return is TT(p_i) = I(p_i)/p_i;
#       TT replaces MTE(phat) in the PRTE ("MPRTE"), behavioral cost, and
#       efficiency gain, and in every draw.
#   (4) DGP fallback (Section 2): resistance is U_D = pnorm(-epsilon).
#       Generate_Example_12_1.do needs the same change, and
#       Example_12_1.dta must be regenerated and re-uploaded.
#
# REVISION 3 (Nov 2026; mirrors Stata_code12.do): QUADRATIC control function
#   (as in Chapter 11's pol(2)) and recalibrated DGP (see
#   Generate_Example_12_1.do, REVISION 3). Eq. 12.2 now uses phat and phat2
#   and their interactions with masters; MTE(u) = m0 + m1*u + m2*u^2 with
#   m0 = b0 + c1, m1 = 2(b1 + c2), m2 = 3*b2 (Eq. 12.3). This supersedes
#   the cubic formulas in item (1) of REVISION 2.
#
# REVISION 4 (Nov 2026; mirrors Stata_code12.do): (a) Section 6 adds a
#   sensitivity check of the point-estimate CBA: two displacement functions
#   (baseline pnorm(o/50), which starts at 0.50; alternative
#   2*pnorm(o/50) - 1, which starts at 0) crossed with two earnings horizons
#   (30 and 10 years). (b) Fig. 12.5's program-area offsets are now estimated
#   from the data (masters x program-area intercept shifts in the quadratic
#   MTE regression) instead of being hard-coded. No other results change.
#
# REVISION 5 (Nov 2026; mirrors Stata_code12.do): the two joint tests in
#   Section 5 now test the MTE coefficients: "no effect" is m0 = m1 = m2 = 0
#   and "MTE flat" is m1 = m2 = 0 (b2 = 0 and b1 + c2 = 0). The earlier
#   tests (beta terms = 0) were not tests of these hypotheses under the
#   control-function approach. Only the two test statistics change.
#
# REVISION 6 (Nov 2026; mirrors Stata_code12.do): the CBA is reorganized
#   into three accounts -- students, the federal government, and
#   institutions -- plus a social total in which transfers (tuition, loans,
#   subsidies, taxes) net out. New items: lost tax revenue, avoided tuition,
#   avoided debt service, and avoided instructional cost; subsidy savings now
#   cover all borrowing by displaced students. The cross-subsidy is reported
#   as part of the institutional revenue loss rather than added to it, and
#   the "extended" net benefit is dropped (it double-counted tuition). New
#   parameter tax_rate (prior: uniform 0.15-0.25). Table 12.1 and Figs. 12.3
#   and 12.7 change.
#
# REVISION 7 (Nov 2026; mirrors Stata_code12.do): tables are written as Word
#   files with flextable/officer instead of LaTeX: Table 12.1 (descriptive
#   statistics, Panel A student-level and Panel B institution- and
#   state-level variables, Section 3), Table 12.2 (simulation
#   assumptions, Section 6), and Table 12.3 (posterior CBA summary, Section 9;
#   formerly Table 12.1). The prior settings, the cross-subsidy share, and the
#   tax-rate range are now parameters, so Table 12.2 always matches the
#   values the code uses. No results change.
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
                      "dplyr", "tidyr", "ggplot2", "scales",
                      "flextable", "officer"))

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
  library(flextable) # Word tables            - replaces: putdocx table
  library(officer)   # Word documents         - replaces: putdocx begin/save
})

# ----------------------------------------------------------------------------
# Helper: write a table to Word in the book's style (REVISION 7)
# Times New Roman 10 pt, black text, horizontal rules only, a bold table
# number, and a note below. group_rows are italic group headings; all other
# rows are indented under them.
# ----------------------------------------------------------------------------
write_word_table <- function(df, number, title, note, path,
                             group_rows = integer(0), right_align = TRUE,
                             fit_page = FALSE, col_widths = NULL) {
  rule <- officer::fp_border(color = "black", width = 0.75)
  ft <- flextable::flextable(df)
  ft <- flextable::border_remove(ft)
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  if (right_align && ncol(df) > 1)
    ft <- flextable::align(ft, j = 2:ncol(df), align = "right", part = "all")
  if (length(group_rows) > 0) {
    ft <- flextable::italic(ft, i = group_rows, j = 1, part = "body")
    ft <- flextable::padding(ft, i = setdiff(seq_len(nrow(df)), group_rows),
                             j = 1, padding.left = 14, part = "body")
  }
  ft <- flextable::hline_top(ft, border = rule, part = "header")
  ft <- flextable::hline_bottom(ft, border = rule, part = "header")
  ft <- flextable::hline_bottom(ft, border = rule, part = "body")
  ft <- flextable::add_footer_lines(ft, note)
  ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::color(ft, color = "black", part = "all")
  ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1, part = "all")
  ft <- flextable::line_spacing(ft, space = 1, part = "all")
  ft <- flextable::autofit(ft)
  # Text-heavy tables: fit to the page width so long text wraps in its cell
  if (fit_page)
    ft <- flextable::set_table_properties(ft, width = 1, layout = "autofit")
  # Explicit column widths in inches (overrides the above)
  if (!is.null(col_widths)) {
    ft <- flextable::width(ft, j = seq_along(col_widths), width = col_widths)
    ft <- flextable::set_table_properties(ft, layout = "fixed")
  }
  txt  <- officer::fp_text(font.family = "Times New Roman", font.size = 10)
  txtb <- officer::fp_text(font.family = "Times New Roman", font.size = 10,
                           bold = TRUE)
  cap <- officer::fpar(officer::ftext(paste0("Table ", number, " "), txtb),
                       officer::ftext(title, txt))
  doc <- officer::read_docx()
  doc <- officer::body_add_fpar(doc, cap)
  doc <- flextable::body_add_flextable(doc, ft, align = "left")
  print(doc, target = path)
  cat("Table", number, "written to", path, "\n")
}

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
# Individual-level controls (same set as Chapter 11 for consistency)
X_controls <- c("female", "black", "hispanic", "asian", "age_ba", "firstgen",
                 "parent_income_q", "parent_grad", "ugpa", "stem_major",
                 "bus_major", "ed_major", "selective_inst", "public_ug",
                 "state_unemp", "metro")
Z_instrument <- "ga_funding_adj"

# CBA parameters
discount_rate   <- 0.03    # Annual discount rate (3%)
career_years    <- 30      # Earnings horizon (years)
subsidy_rate    <- 0.20    # Government subsidy on Grad PLUS principal
tax_rate        <- 0.20    # Marginal tax rate on forgone earnings (REVISION 6)
cap_threshold   <- 100     # Policy cap ($000s)
base_salary     <- 47000   # Pooled mean annual salary (non-holders)
cost_per_degree <- 100000  # Not used in any calculation (retained for reference)
S_draws         <- 1000    # Number of Bayesian posterior draws
cross_sub_share <- 0.20    # Share of net revenue that cross-subsidizes other programs
# Prior settings for the posterior simulation (Section 7; Table 12.2)
prior_sd_salary  <- 0.10   # SD of log base salary
prior_hw_subsidy <- 0.05   # Half-width of uniform prior on subsidy rate
prior_sd_tuition <- 0.08   # SD of log tuition multiplier
tax_lo           <- 0.15   # Tax-rate prior: uniform lower bound
tax_hi           <- 0.25   # Tax-rate prior: uniform upper bound


# ------------------------------------------------------------------------------
# Section 2: Synthetic Data Generation
#
# Population: 8,000 graduate students drawn from a B&B-mirroring DGP.
# The dataset is generated separately from the Chapter 11 data
# (Example_7_5_3_updated.dta); it reuses many of the same variable names
# but not the same draws, program areas, or GA funding distribution. The
# canonical file is produced by Generate_Example_12_1.do and downloaded
# below; the R block that follows runs only if the download fails and,
# because R's RNG differs from Stata's, does NOT reproduce the canonical
# file exactly.
# Federal graduate borrowing (grad_plus_loans, $000s) depends on program
# area, institutional selectivity, family income, GA funding, and a
# student-specific unobserved borrowing propensity.
# Unlike Chapter 11, where only completers have a master's program area
# (ma_*), every student here is assigned the graduate program area they
# are enrolled in or would enter (stem_major, bus_major, ed_major,
# health_major; Other is the base), because borrowing and tuition depend
# on the program.
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
# All five graduate program areas, including Health and the Other base.
# The above_cap mean is the share of ALL students in each area (completers
# and non-completers) borrowing above the limit, not a completer-only share.
dat$prog_area <- factor(
  ifelse(dat$stem_major == 1, "STEM",
  ifelse(dat$bus_major == 1, "Business",
  ifelse(dat$ed_major == 1, "Education",
  ifelse(dat$health_major == 1, "Health", "Other")))),
  levels = c("STEM", "Business", "Education", "Health", "Other"))

cat("\nMean federal graduate borrowing and share above the limit, by program area:\n")
print(tabstat_by(dat, c("grad_plus_loans", "above_cap"), "prog_area",
                  stats_fun = list(mean = mean, n = length)))

# ── Table 12.1: Descriptive statistics (Word) ──────────────────────────────
# Panel A: student-level variables; Panel B: institution- and state-level
# variables. Mean, SD, minimum, and maximum for all students, plus means for
# completers and non-completers. Written with flextable so it pastes into Word.
dat$other_major <- 1 - dat$stem_major - dat$bus_major - dat$ed_major -
  dat$health_major
t1_spec <- list(
  "Panel A: Student-level variables" = c(
    ln_salary = "Log annual salary", salary = "Annual salary ($)",
    female = "Female", black = "Black", hispanic = "Hispanic", asian = "Asian",
    age_ba = "Age at bachelor's degree", firstgen = "First-generation student",
    parent_income_q = "Parental income quartile (1-4)",
    parent_grad = "Parent holds a graduate degree", ugpa = "Undergraduate GPA",
    stem_major = "Program area: STEM", bus_major = "Program area: Business",
    ed_major = "Program area: Education", health_major = "Program area: Health",
    other_major = "Program area: Other",
    grad_plus_loans = "Federal graduate borrowing ($000s)",
    above_cap = "Borrowing above the $100,000 limit"),
  "Panel B: Institution- and state-level variables" = c(
    selective_inst = "Selective institution",
    public_ug = "Public undergraduate institution",
    annual_tuition = "Annual tuition ($000s)",
    program_years = "Program length (years)",
    net_inst_rev = "Net institutional revenue per student ($000s)",
    ga_funding_adj = "State GA funding (instrument)",
    state_unemp = "State unemployment rate (%)", metro = "Metropolitan area"))
t1_bin <- c("female", "black", "hispanic", "asian", "firstgen", "parent_grad",
            "stem_major", "bus_major", "ed_major", "health_major",
            "other_major", "selective_inst", "public_ug", "metro", "above_cap")
# Formats: binary = 3 decimals (min/max 0); large amounts = 0; others = 2
t1_fmt <- function(x, bin, big, minmax = FALSE) {
  d <- if (bin) (if (minmax) 0 else 3) else if (big) 0 else 2
  formatC(x, format = "f", digits = d, big.mark = ",")
}
t1_names <- c("Variable", "Mean", "SD", "Min", "Max",
              "Completers", "Non-completers")
t1 <- setNames(data.frame(matrix(character(0), ncol = 7)), t1_names)
t1_groups <- integer(0)
for (g in names(t1_spec)) {
  t1 <- rbind(t1, setNames(data.frame(g, "", "", "", "", "", ""), t1_names))
  t1_groups <- c(t1_groups, nrow(t1))
  for (v in names(t1_spec[[g]])) {
    x   <- dat[[v]]
    b   <- v %in% t1_bin
    big <- abs(mean(x)) >= 1000
    t1 <- rbind(t1, setNames(data.frame(
      t1_spec[[g]][[v]],
      t1_fmt(mean(x), b, big), t1_fmt(sd(x), b, big),
      t1_fmt(min(x), b, big, TRUE), t1_fmt(max(x), b, big, TRUE),
      t1_fmt(mean(x[dat$masters == 1]), b, big),
      t1_fmt(mean(x[dat$masters == 0]), b, big)), t1_names))
  }
}
t1 <- rbind(t1, setNames(data.frame("N", format(nrow(dat), big.mark = ","),
  "", "", "", format(sum(dat$masters == 1), big.mark = ","),
  format(sum(dat$masters == 0), big.mark = ",")), t1_names))
write_word_table(t1, "12.1", "Descriptive statistics",
  "Note: Completers and Non-completers columns report means. Binary variables are shown as proportions. Panel B characteristics are attached to each student's record; the data contain no institution or state identifiers. Amounts marked $000s are in thousands of dollars. Source: Synthetic data (Example_12_1.dta), available in the book's GitHub data repository (see Sect. 12.2.3).",
  file.path(tables_dir, "Table12_1_Descriptives_R.docx"),
  group_rows = t1_groups,
  col_widths = c(2.45, 0.6, 0.6, 0.55, 0.65, 0.8, 0.85))


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
# Section 5: MTE Estimation - Quadratic Polynomial Control Function
#
# Following Heckman and Vytlacil (1999, 2005), the MTE is recovered from
# the derivative of the outcome regression with respect to the propensity
# score. The polynomial control function approach is used here for
# transparency and direct estimation of ATE, ATT, and ATU.
#
# MTE(u) = m0 + m1*u + m2*u^2   (Eq. 12.3)
#
# where u is unobserved resistance to treatment and the m coefficients are
# built from the coefficients on masters, masters:phat, masters:phat2
# (beta) and on phat, phat2 (pi, the control function).
# ------------------------------------------------------------------------------

cat("\n============================================================\n")
cat("  Section 5: MTE Estimation\n")
cat("============================================================\n")

# ── Quadratic MTE regression (Eq. 12.2) ─────────────────────────────────────
mte_formula <- rhs_formula("ln_salary",
  c("masters", "masters:phat", "masters:phat2",
    X_controls, "phat", "phat2"))
mte_quad <- lm(mte_formula, data = dat)
mte_vcov  <- vcovHC(mte_quad, type = "HC1")
print(coeftest(mte_quad, vcov. = mte_vcov))

# ── Joint tests for the MTE polynomial ──────────────────────────────────────
# Individual coefficients are often imprecise due to multicollinearity among
# polynomial terms, so joint tests are used.
# The tests are on the MTE coefficients (Eq. 12.3), m0 = b0 + c1,
# m1 = 2(b1 + c2), m2 = 3*b2, not on the beta terms alone (REVISION 5).
# (1) MTE(u) = 0 for every u (m0 = m1 = m2 = 0): completion has no effect
#     on log salary for anyone.
joint_test <- linearHypothesis(mte_quad,
    c("masters + phat = 0", "masters:phat + phat2 = 0", "masters:phat2 = 0"),
    vcov. = mte_vcov, test = "F")
cat(sprintf("Joint F-test, no effect (MTE = 0) p-value: %6.4f\n",
            joint_test[2, "Pr(>F)"]))

# (2) The MTE is flat in u (m1 = m2 = 0). Rejecting this is evidence of
#     essential heterogeneity in returns.
het_test <- linearHypothesis(mte_quad,
    c("masters:phat + phat2 = 0", "masters:phat2 = 0"),
    vcov. = mte_vcov, test = "F")
cat(sprintf("Heterogeneity F-test (MTE flat in u) p-value: %6.4f\n",
            het_test[2, "Pr(>F)"]))

# ── Store regression coefficients (Eq. 12.2) ────────────────────────────────
b0 <- unname(coef(mte_quad)["masters"])
b1 <- unname(coef(mte_quad)["masters:phat"])
b2 <- unname(coef(mte_quad)["masters:phat2"])
# pi polynomial (control function): coefficients on phat, phat2
c1 <- unname(coef(mte_quad)["phat"])
c2 <- unname(coef(mte_quad)["phat2"])

# ── MTE (Eq. 12.3) ──────────────────────────────────────────────────────────
# MTE(u) = beta(u) + u*beta'(u) + pi'(u)  (control-function / separate
# approach; Heckman & Vytlacil, 2005; Andresen, 2018)
m0 <- b0 + c1
m1 <- 2 * (b1 + c2)
m2 <- 3 * b2

cat(sprintf("\nMTE polynomial: MTE(u) = %7.4f + %7.4f*u + %7.4f*u^2\n",
            m0, m1, m2))

# ── Treatment parameters (Chapter 11, Eq. 11.3 weights) ─────────────────────
# I(p) = integral of MTE(u) du from 0 to p
I_fun <- function(p, m0, m1, m2) m0*p + m1*p^2/2 + m2*p^3/3
ate <- m0 + m1/2 + m2/3
cat(sprintf("Estimated ATE: %6.4f\n", ate))

# ATT = sum I(p_i) / sum p_i ; ATU = sum [I(1) - I(p_i)] / sum (1 - p_i)
dat$I_hat <- I_fun(dat$phat, m0, m1, m2)
sum_p <- sum(dat$phat)
n_obs <- nrow(dat)
att <- sum(dat$I_hat) / sum_p
atu <- (n_obs * ate - sum(dat$I_hat)) / (n_obs - sum_p)
cat(sprintf("Estimated ATT: %6.4f\n", att))
cat(sprintf("Estimated ATU: %6.4f\n", atu))

# Expected return for a completer with propensity score p: TT(p) = I(p)/p,
# the average MTE over resistance levels below p (used in Sections 6 and 7).
dat$tt_hat <- dat$I_hat / dat$phat

# Check: ATE = E[P]*ATT + (1 - E[P])*ATU
cat(sprintf("Check (should equal ATE): %6.4f\n",
            (sum_p / n_obs) * att + (1 - sum_p / n_obs) * atu))


# ------------------------------------------------------------------------------
# Section 6: Policy Simulation - Cap-Binding Student Identification
#
# Under the $100k lifetime cap, students currently borrowing more than
# $100k face a binding credit constraint. Those who cannot substitute with
# private credit or personal resources drop out, forfeiting the returns to
# completion. We model the probability of being pushed out as a function
# of the loan overage: p_out(overage) = Phi(overage / 50). The probability
# rises smoothly with the overage, but because Phi(0) = 0.5 every student
# above the limit has at least a 50% displacement probability, even one
# $1 over. UNDER REVIEW (outline Author note 2): alternatives such as
# 2*Phi(overage/50) - 1 start near zero.
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

# ── Policy-relevant treatment effect for displaced students ("MPRTE") ───────
# Displacement-weighted average of each completer's expected return,
# TT(phat) = I(phat)/phat (REVISION 2). Names keep the "mprte" label from
# earlier drafts; conceptually this is a PRTE for a discrete policy.
# Each student is weighted by their share of total expected displacement.
# Restricted to completers (displaced_E = 0 for non-completers by construction).
#
# Interpretation:
#   MPRTE > 0  -> cap displaces high-return students (efficiency loss)
#   MPRTE ~ 0  -> cap displaces marginal students (efficiency neutral)
#   MPRTE < 0  -> cap displaces low-return students (efficiency gain)
total_disp <- sum(dat$displaced_E[dat$masters == 1])
mprte_weight <- ifelse(dat$masters == 1, dat$displaced_E / total_disp, NA_real_)
mprte_i <- ifelse(dat$masters == 1, dat$tt_hat * mprte_weight, NA_real_)
mprte <- sum(mprte_i, na.rm = TRUE)

cat("\n--- Treatment Parameter Hierarchy (Point Estimates) ---\n")
cat(sprintf("ATE   (population average):           %6.4f\n", ate))
cat(sprintf("ATT   (E[MTE | D=1], completers):     %6.4f\n", att))
cat(sprintf("ATU   (E[MTE | D=0], non-completers): %6.4f\n", atu))
cat(sprintf("MPRTE (cap-displaced margin):         %6.4f\n", mprte))
cat("\nMPRTE compares the average return of displaced students with the\n")
cat("average return of all completers (ATT). If MPRTE > ATT, the cap removes\n")
cat("students with above-average returns among completers (efficiency loss).\n")

# ── Present value factor ─────────────────────────────────────────────────────
pv_factor <- (1 - (1 + discount_rate)^(-career_years)) / discount_rate
cat(sprintf("Present value factor (%d yrs, %.0f%% r): %6.3f\n",
            career_years, discount_rate * 100, pv_factor))

# ── Table 12.2: Simulation assumptions (Word) ───────────────────────────────
# Built from the parameters at the top of the script and from the data, so
# the table always matches the values the code uses.
pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 0), "%")
instr_share <- 1 - sum(dat$net_inst_rev) / sum(dat$gross_tuition)
t2 <- data.frame(
  Parameter = c(
    "Policy and behavior",
    "Aggregate federal loan limit",
    "Probability that a completer above the limit leaves",
    "Earnings",
    "Base salary (non-degree holders)",
    "Earnings horizon",
    "Discount rate",
    "Federal government",
    "Federal subsidy rate",
    "Tax rate on forgone earnings",
    "Institutions",
    "Tuition",
    "Instructional cost share of tuition; cross-subsidy share of net revenue"),
  `Baseline value` = c(
    "", paste0("$", formatC(1000 * cap_threshold, format = "f", digits = 0,
                            big.mark = ",")),
    "\u03a6(overage / 50), overage in $000s",
    "", paste0("$", formatC(base_salary, format = "f", digits = 0, big.mark = ",")),
    paste(career_years, "years"), pct(discount_rate),
    "", pct(subsidy_rate), pct(tax_rate),
    "", "Program values in the data",
    paste0(pct(instr_share), "; ", pct(cross_sub_share))),
  `Prior distribution (Sect. 12.6)` = c(
    "", "Fixed", "Fixed",
    "", sprintf("Log-normal, SD %.2f on the log scale", prior_sd_salary),
    "Fixed", "Fixed",
    "", paste0("Uniform, ", pct(subsidy_rate - prior_hw_subsidy), " to ",
               pct(subsidy_rate + prior_hw_subsidy)),
    paste0("Uniform, ", pct(tax_lo), " to ", pct(tax_hi)),
    "", sprintf("Log-normal multiplier, SD %.2f", prior_sd_tuition), "Fixed"),
  `Sensitivity check (Sect. 12.7.3)` = c(
    "", "None", "2\u03a6(overage / 50) \u2212 1",
    "", "None", "10 years", "None",
    "", "None", "None",
    "", "None", "None"),
  check.names = FALSE)
write_word_table(t2, "12.2", "Simulation assumptions",
  paste0("Note: MTE coefficients are drawn from a normal distribution ",
         "centered on the estimates (Eq. 12.3), with the robust ",
         "variance-covariance matrix. The simulation uses ",
         format(S_draws, big.mark = ","), " posterior draws.",
         " Source: Parameter settings in Stata_code12.do and R_code12.R, available in the book's GitHub code repository."),
  file.path(tables_dir, "Table12_2_Assumptions_R.docx"),
  group_rows = c(1L, 4L, 8L, 11L), right_align = FALSE, fit_page = TRUE)

# ── Point-estimate CBA by account (REVISION 6) ──────────────────────────────
# Three accounts plus a social total. Amounts are present values in $000s.
#
#   Students:     - after-tax forgone earnings (+ after-tax efficiency gain)
#                 + avoided tuition (displaced students no longer pay it)
#                 + avoided debt service (repayments they no longer make)
#                 - forgone loan disbursements (loans they no longer receive)
#                 - higher financing cost for students who stay above the
#                   limit (the subsidy they lose on borrowing above $100k)
#   Federal:      + subsidy savings (all borrowing by displaced students,
#                   plus the overage of students who stay)
#                 - lost tax revenue on forgone earnings
#   Institutions: - lost net tuition revenue (gross tuition minus the
#                   variable instructional cost avoided)
#   Society:      sum of the three accounts. Tuition, loans, subsidies, and
#                 taxes are transfers between accounts and net out, leaving
#                 - forgone earnings + efficiency gain + avoided
#                   instructional cost.
#
# The present value of repayments on a dollar borrowed is (1 - subsidy
# rate), so avoided debt service and the subsidy are consistent.
comp <- dat$masters == 1

# Forgone earnings (pre-tax): TT(phat) x base_salary x PV factor ($000s)
dat$behav_cost_i <- dat$displaced_E * pmax(0, dat$tt_hat) *
  (base_salary / 1000) * pv_factor
# Note: pmax(0, TT) reflects that only students with positive returns suffer a
# human capital loss. Displaced students with negative TT represent an
# efficiency GAIN (removing low-return borrowers).
BC_point <- sum(dat$behav_cost_i[comp])
dat$efficiency_gain_i <- dat$displaced_E * pmax(0, -dat$tt_hat) *
  (base_salary / 1000) * pv_factor
EG_point <- sum(dat$efficiency_gain_i[comp])

# Federal subsidy savings
dat$fiscal_saving_i <- subsidy_rate * (dat$displaced_E * dat$grad_plus_loans +
  (dat$above_cap - dat$displaced_E) * dat$loan_overage)
FS_point <- sum(dat$fiscal_saving_i)
# Lost tax revenue on (net) forgone earnings
TL_point <- tax_rate * (BC_point - EG_point)

# Students' tuition and loans (displaced completers)
AT_point <- sum(dat$displaced_E * dat$gross_tuition)
LD_point <- sum(dat$displaced_E * dat$grad_plus_loans)
DS_point <- (1 - subsidy_rate) * LD_point                 # avoided debt service
SC_point <- sum(subsidy_rate * (dat$above_cap - dat$displaced_E) * dat$loan_overage)

# Institutions: lost net tuition revenue; cross-subsidy is part of it
# As implemented, the loss covers the FULL program (all program years); if
# displaced students leave partway through, this overstates it (UNDER REVIEW).
dat$inst_rev_loss_i  <- dat$displaced_E * dat$net_inst_rev
dat$cross_sub_loss_i <- cross_sub_share * dat$inst_rev_loss_i  # of which: cross-subsidy
dat$avoid_instr_i    <- dat$displaced_E * (dat$gross_tuition - dat$net_inst_rev)
IR_point <- sum(dat$inst_rev_loss_i[comp])
CS_point <- sum(dat$cross_sub_loss_i[comp])
AI_point <- sum(dat$avoid_instr_i[comp])

# Account totals
STU_point <- -(1 - tax_rate) * (BC_point - EG_point) + AT_point +
  DS_point - LD_point - SC_point
FED_point <- FS_point - TL_point
INS_point <- -IR_point
NB_point  <- STU_point + FED_point + INS_point
BCR_point <- (AI_point + EG_point) / max(1, BC_point)

acct_line <- function(label, x) cat(sprintf("%-44s %12s\n", label,
  formatC(x, format = "f", digits = 1, big.mark = ",")))
cat("\n--- Point-estimate CBA by account ($000s) ---\n")
cat("Students\n")
acct_line("  Forgone after-tax earnings", -(1 - tax_rate) * (BC_point - EG_point))
acct_line("  Avoided tuition", AT_point)
acct_line("  Avoided debt service", DS_point)
acct_line("  Forgone loan disbursements", -LD_point)
acct_line("  Higher financing cost (students who stay)", -SC_point)
acct_line("  Student account", STU_point)
cat("Federal government\n")
acct_line("  Subsidy savings", FS_point)
acct_line("  Lost tax revenue", -TL_point)
acct_line("  Federal account", FED_point)
cat("Institutions\n")
acct_line("  Lost net tuition revenue", -IR_point)
acct_line("    of which cross-subsidy to other programs", -CS_point)
acct_line("  Institutional account", INS_point)
cat("Society (transfers net out)\n")
acct_line("  Forgone earnings (pre-tax)", -BC_point)
acct_line("  Efficiency gain", EG_point)
acct_line("  Avoided instructional cost", AI_point)
acct_line("Point-estimate net benefit ($000s):", NB_point)
acct_line("Check (- forgone earn. + eff. gain + instr.):", -BC_point + EG_point + AI_point)
cat(sprintf("Benefit-cost ratio: %5.3f\n", BCR_point))

# Revenue loss by program area
cat("\nInstitutional revenue loss by program area ($000s):\n")
completers <- dat[dat$masters == 1, ]
# All five areas (prog_area from Section 3)
print(tabstat_by(completers, "inst_rev_loss_i", "prog_area",
                  stats_fun = list(sum = sum, mean = mean, n = length)))

# Revenue loss by institution type
cat("\nInstitutional revenue loss by institution type ($000s):\n")
print(tabstat_by(completers, "inst_rev_loss_i", "selective_inst",
                  stats_fun = list(sum = sum, mean = mean, n = length)))
print(tabstat_by(completers, "inst_rev_loss_i", "public_ug",
                  stats_fun = list(sum = sum, mean = mean, n = length)))

# ── Sensitivity of the point-estimate CBA (REVISIONS 4 and 6) ───────────────
# Two assumptions drive the size of the net benefit:
#   (1) Displacement function. Baseline: pnorm(overage/50), which gives a
#       student $1 over the limit a 0.50 probability of leaving. Alternative:
#       2*pnorm(overage/50) - 1, which starts at 0 and rises toward 1.
#   (2) Earnings horizon. Baseline: 30 years. Alternative: 10 years.
# The PRTE depends only on the displacement function (it changes the weights).
sens <- expand.grid(horizon = c(30, 10),
                    displacement = c("pnorm(o/50)", "2pnorm(o/50)-1"),
                    stringsAsFactors = FALSE)[, c("displacement", "horizon")]
sens[, c("displaced", "prte", "forgone_earn", "net_social_benefit", "bcr",
         "federal_acct")] <- NA_real_
for (k in seq_len(nrow(sens))) {
  pd_k <- if (sens$displacement[k] == "pnorm(o/50)") {
    pnorm(dat$loan_overage / 50) * dat$above_cap
  } else {
    (2 * pnorm(dat$loan_overage / 50) - 1) * dat$above_cap
  }
  de_all <- pd_k * dat$masters
  de_k <- de_all[comp]
  tt_k <- dat$tt_hat[comp]
  pv_k <- (1 - (1 + discount_rate)^(-sens$horizon[k])) / discount_rate
  bc_k <- sum(de_k * pmax(0,  tt_k) * (base_salary / 1000) * pv_k)
  eg_k <- sum(de_k * pmax(0, -tt_k) * (base_salary / 1000) * pv_k)
  ai_k <- sum(de_k * (dat$gross_tuition[comp] - dat$net_inst_rev[comp]))
  fs_k <- sum(subsidy_rate * (de_all * dat$grad_plus_loans +
                (dat$above_cap - de_all) * dat$loan_overage))
  sens$displaced[k]          <- sum(de_k)
  sens$prte[k]               <- sum(de_k * tt_k) / sum(de_k)
  sens$forgone_earn[k]       <- bc_k
  sens$net_social_benefit[k] <- -bc_k + eg_k + ai_k
  sens$bcr[k]                <- (ai_k + eg_k) / max(1, bc_k)
  sens$federal_acct[k]       <- fs_k - tax_rate * (bc_k - eg_k)
}
cat("\n--- Sensitivity of the point-estimate CBA ($000s) ---\n")
print(format(sens, digits = 4, nsmall = 1, big.mark = ","), row.names = FALSE)


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
# variance-covariance matrix from the quadratic MTE regression. For each draw,
# we recompute the MTE curve, the policy-affected margin, and the three
# CBA components. The resulting distribution characterizes posterior
# uncertainty about the net social benefit of the $100k cap.
#
# Additional uncertainty is propagated through draws on:
#   - Base salary (log-normal, reflecting real-wage uncertainty)
#   - Subsidy rate (uniform, reflecting fiscal policy uncertainty)
#   - Tuition multiplier (log-normal, reflecting pricing uncertainty)
#   - Tax rate (uniform, 0.15 to 0.25; REVISION 6)
# The propensity score (phat) is held fixed across draws, so uncertainty
# in the first-stage probit is not propagated.
# ------------------------------------------------------------------------------

cat("\n============================================================\n")
cat("  Section 7: Bayesian Microsimulation\n")
cat("============================================================\n")

# ── Coefficient vector and robust VCV ────────────────────────────────────────
# Reuses the model object and robust VCV estimated in Section 5.
b_full <- coef(mte_quad)
V_full <- mte_vcov
k_full <- length(b_full)

# Column positions of the four MTE polynomial coefficients. Unlike Stata's
# `local cols : colnames b_full` loop, R coefficient vectors are named, so
# the four terms of interest can be referenced directly by name.
mte_poly_names <- c("masters", "masters:phat", "masters:phat2")
cat("MTE polynomial coefficient names:", paste(mte_poly_names, collapse = ", "), "\n")

# ── Preallocate results data frame ───────────────────────────────────────────
sim_results <- data.frame(
  draw             = 1:S_draws,
  b0_s             = NA_real_, b1_s = NA_real_, b2_s = NA_real_,
  m0_s             = NA_real_, m1_s = NA_real_, m2_s = NA_real_,
  ate_s            = NA_real_, att_s = NA_real_, atu_s = NA_real_, mprte_s = NA_real_,
  fiscal_s         = NA_real_,   # Federal subsidy savings
  tax_loss_s       = NA_real_,   # Lost tax revenue
  behav_cost_s     = NA_real_,   # Forgone earnings, pre-tax
  effic_gain_s     = NA_real_,
  avoid_tuition_s  = NA_real_,   # Avoided tuition, students
  avoid_instr_s    = NA_real_,   # Avoided instructional cost
  student_acct_s   = NA_real_,
  federal_acct_s   = NA_real_,
  inst_acct_s      = NA_real_,
  net_benefit_s    = NA_real_,   # Net social benefit
  nb_per_student_s = NA_real_,
  bcr_s            = NA_real_,
  pnb_positive_s   = NA_real_,
  inst_rev_loss_s  = NA_real_,
  cross_sub_loss_s = NA_real_    # of which: cross-subsidy
)

# Quantities that do not change across draws (displacement is fixed)
LD_0 <- LD_point
AT_0 <- AT_point
AI_0 <- AI_point
IR_0 <- IR_point
OS_0 <- sum((dat$above_cap - dat$displaced_E) * dat$loan_overage)

# ── Simulation loop ──────────────────────────────────────────────────────────
cat("\nRunning", S_draws, "posterior draws...\n")
cat("(This typically takes under a minute.)\n")

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
  c1_s <- B_draws[s, "phat"]
  c2_s <- B_draws[s, "phat2"]
  # Implied MTE coefficients for this draw (Eq. 12.3)
  m0_s <- b0_s + c1_s
  m1_s <- 2 * (b1_s + c2_s)
  m2_s <- 3 * b2_s

  # ── Additional economic uncertainty ─────────────────────────────────────
  # Base salary: log-normal with 10% coefficient of variation
  base_sal_s <- exp(log(base_salary) + rnorm(1, 0, prior_sd_salary))
  # Subsidy rate: uniform +/- 5 percentage points around baseline
  sub_rate_s <- subsidy_rate + runif(1, -prior_hw_subsidy, prior_hw_subsidy)
  # Tuition multiplier: log-normal with 8% CV, reflecting uncertainty in
  # actual tuition levels, discount rates, and institutional aid policies.
  # This ensures inst_rev_loss_s varies meaningfully across draws.
  tuition_mult_s <- exp(rnorm(1, 0, prior_sd_tuition))
  # Tax rate on forgone earnings: uniform 0.15 to 0.25 (REVISION 6)
  tax_rate_s <- runif(1, tax_lo, tax_hi)

  # ── Recompute MTE for this draw ──────────────────────────────────────────
  I_s   <- I_fun(dat$phat, m0_s, m1_s, m2_s)

  # Treatment parameters (Eq. 11.3 weights)
  ate_s <- m0_s + m1_s/2 + m2_s/3
  att_s <- sum(I_s) / sum_p
  atu_s <- (n_obs * ate_s - sum(I_s)) / (n_obs - sum_p)

  # Expected return for each completer, TT(phat) = I(phat)/phat
  mte_s <- I_s / dat$phat

  # PRTE ("MPRTE"): displacement-weighted average of TT for this draw.
  # Reuses total_disp scalar from Section 6 (constant across draws since
  # displaced_E is a function of loan_overage, not the MTE polynomial).
  mprte_i_s <- ifelse(dat$masters == 1, dat$displaced_E * mte_s / total_disp, NA_real_)
  mprte_s <- sum(mprte_i_s, na.rm = TRUE)

  # Behavioral cost: human capital loss for displaced high-return students
  bc_i_s       <- dat$displaced_E * pmax(0, mte_s) * (base_sal_s / 1000) * pv_factor
  behav_cost_s <- sum(bc_i_s[dat$masters == 1])

  # Efficiency gain: removing low-return constrained students
  eg_i_s       <- dat$displaced_E * pmax(0, -mte_s) * (base_sal_s / 1000) * pv_factor
  effic_gain_s <- sum(eg_i_s[dat$masters == 1])

  # Accounts (REVISION 6; see Section 6)
  fiscal_s         <- sub_rate_s * (LD_0 + OS_0)
  tax_loss_s       <- tax_rate_s * (behav_cost_s - effic_gain_s)
  avoid_tuition_s  <- AT_0 * tuition_mult_s
  avoid_instr_s    <- AI_0 * tuition_mult_s
  inst_rev_loss_s  <- IR_0 * tuition_mult_s
  cross_sub_loss_s <- cross_sub_share * inst_rev_loss_s
  # Students: after-tax earnings, tuition, and the subsidy they lose
  # (avoided debt service minus forgone loans = -subsidy on displaced
  # borrowing; plus the subsidy lost by students who stay)
  student_acct_s <- -(1 - tax_rate_s) * (behav_cost_s - effic_gain_s) +
    avoid_tuition_s - fiscal_s
  federal_acct_s <- fiscal_s - tax_loss_s
  inst_acct_s    <- -inst_rev_loss_s

  # Net social benefit = sum of the accounts
  net_benefit_s <- student_acct_s + federal_acct_s + inst_acct_s

  # Per-displaced-student net benefit
  n_disp_s         <- max(1, sum(dat$displaced_E[dat$masters == 1]))
  nb_per_student_s <- net_benefit_s / n_disp_s

  # Benefit-cost ratio
  bcr_s <- (avoid_instr_s + effic_gain_s) / max(1, behav_cost_s)

  # Efficiency indicator
  pnb_positive_s <- as.numeric(net_benefit_s > 0)

  # ── Store results ─────────────────────────────────────────────────────────
  sim_results[s, ] <- list(
    s, b0_s, b1_s, b2_s,
    m0_s, m1_s, m2_s,
    ate_s, att_s, atu_s, mprte_s,
    fiscal_s, tax_loss_s, behav_cost_s, effic_gain_s,
    avoid_tuition_s, avoid_instr_s,
    student_acct_s, federal_acct_s, inst_acct_s,
    net_benefit_s, nb_per_student_s, bcr_s, pnb_positive_s,
    inst_rev_loss_s, cross_sub_loss_s
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
for (v in c("behav_cost_s", "effic_gain_s", "avoid_instr_s", "net_benefit_s")) {
  summarize_ci(sim_results[[v]], v, digits = 2)
}

cat("\n--- Accounts by Perspective: Posterior Summary ($000s) ---\n")
for (v in c("student_acct_s", "federal_acct_s", "fiscal_s", "tax_loss_s",
            "inst_acct_s", "cross_sub_loss_s")) {
  x  <- sim_results[[v]]
  ci <- quantile(x, probs = c(0.025, 0.975))
  cat(sprintf("%s:  mean = %10.2f  95%% CI: [%10.2f, %10.2f]\n",
              v, mean(x), ci[1], ci[2]))
}

# Probability that the federal government comes out ahead
cat(sprintf("\nP(Federal account > 0): %5.3f\n",
            mean(sim_results$federal_acct_s > 0)))

print(sim_results %>%
  summarise(across(c(m0_s, m1_s, m2_s, ate_s, att_s, atu_s, mprte_s),
                    list(mean = mean, sd = sd))))

# ── Treatment Parameter Posterior Summary (Sects. 12.3.3 and 12.6.3) ─────────
# Posterior means and 95% credible intervals for ATE, ATT, MPRTE, ATU.
# These four numbers are reported in the text and provide
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
for (v in c("behav_cost_s", "effic_gain_s", "avoid_instr_s", "net_benefit_s", "bcr_s",
            "student_acct_s", "federal_acct_s", "inst_acct_s")) {
  x  <- sim_results[[v]]
  ci <- quantile(x, probs = c(0.025, 0.975))
  cat(sprintf("%s: mean = %12.2f  95%% CI: [%12.2f, %12.2f]\n",
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
  cat(sprintf("Posterior mean NB = %12.2f ($000s) > 0\n", nb_mean))
  cat("Scenario A likely: cap predominantly displaces low-return borrowers.\n")
  cat("Resources saved exceed forgone earnings.\n")
} else {
  cat(sprintf("Posterior mean NB = %12.2f ($000s) < 0\n", nb_mean))
  cat("Scenario B likely: cap displaces students with positive marginal returns.\n")
  cat("Forgone earnings exceed the resources saved.\n")
}

# ── Table 12.3: Posterior cost-benefit summary (Word) ───────────────────────
t3_spec <- list(
  "Society" = c(behav_cost_s = "Forgone earnings", effic_gain_s = "Efficiency gain",
                avoid_instr_s = "Avoided instructional cost",
                net_benefit_s = "Net social benefit", bcr_s = "Benefit-cost ratio"),
  "By account" = c(student_acct_s = "Students", federal_acct_s = "Federal government",
                   inst_acct_s = "Institutions"))
t3_fmt <- function(x, v) {
  if (v == "bcr_s") return(formatC(x, format = "f", digits = 3))
  formatC(x, format = "f", digits = 1, big.mark = ",")
}
t3 <- data.frame(Component = character(0), Mean = character(0), SD = character(0),
                 `2.5th pct.` = character(0), `97.5th pct.` = character(0),
                 check.names = FALSE)
t3_groups <- integer(0)
for (g in names(t3_spec)) {
  t3 <- rbind(t3, setNames(data.frame(g, "", "", "", ""), names(t3)))
  t3_groups <- c(t3_groups, nrow(t3))
  for (v in names(t3_spec[[g]])) {
    x  <- sim_results[[v]]
    ci <- quantile(x, probs = c(0.025, 0.975))
    t3 <- rbind(t3, setNames(data.frame(t3_spec[[g]][[v]],
      t3_fmt(mean(x), v), t3_fmt(sd(x), v), t3_fmt(ci[[1]], v),
      t3_fmt(ci[[2]], v)), names(t3)))
  }
}
write_word_table(t3, "12.3",
  "Posterior cost-benefit summary: $100,000 federal graduate loan limit ($000s)",
  paste0("Note: Based on ", format(S_draws, big.mark = ","), " posterior ",
         "draws. Net social benefit is the sum of the three accounts; tuition, ",
         "loans, subsidies, and taxes are transfers between accounts and net ",
         "out. Benefit-cost ratio = (avoided instructional cost + efficiency ",
         "gain) / forgone earnings. Source: Posterior simulation using Example_12_1.dta (see Sect. 12.6)."),
  file.path(tables_dir, "Table12_3_CBA_Summary_R.docx"),
  group_rows = t3_groups)


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
b0_pm <- mean(sim_results$m0_s)
b1_pm <- mean(sim_results$m1_s)
b2_pm <- mean(sim_results$m2_s)

# Identify policy-affected u range:
# Students pushed out by the cap tend to cluster in the intermediate-u range.
# Those with u near 0 (low resistance) self-select regardless of credit access.
# Those with u near 1 (high resistance) would not enroll even without the cap.
# The cap binds most tightly for students in u in [0.30, 0.65] (medium resistance).
# UNDER REVIEW (outline Author note 6): these thresholds are hardcoded, not
# derived from the displacement weights or the loan overage distribution.
# mte_cap_neg is empty whenever the MTE is positive throughout [0.30, 0.65].
mte_curve <- data.frame(u = (1:100) / 100)
mte_curve$mte <- b0_pm + b1_pm*mte_curve$u + b2_pm*mte_curve$u^2
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
# Posterior means of the real (non-transfer) components (REVISION 6)
sc_ai <- mean(sim_results$avoid_instr_s)
sc_bc <- mean(sim_results$behav_cost_s)
sc_eg <- mean(sim_results$effic_gain_s)

decomp_levels <- c("Avoided Instructional Cost", "Efficiency Gain", "Forgone Earnings")
decomp <- data.frame(
  label = factor(decomp_levels, levels = decomp_levels),
  value = c(sc_ai, sc_eg, -sc_bc)
)

fig12_3 <- ggplot(decomp, aes(x = label, y = value)) +
  geom_col(fill = "grey55", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = NULL, y = "Posterior Mean ($000s)",
       title = "Cost-Benefit Decomposition",
       subtitle = "$100k Federal Loan Cap (Posterior Mean Components)",
       caption = paste(
         "Forgone earnings shown as negative. Net social benefit =",
         "Avoided Instructional Cost + Efficiency Gain - Forgone Earnings;",
         "tuition, loans, subsidies, and taxes are transfers and net out.",
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
         "Posterior distributions of the average treatment effects",
         "(weights as in Chapter 11, Eq. 11.3).",
         sprintf("N = %d posterior draws.", S_draws), sep = "\n")) +
  theme(legend.position = "bottom")

save_fig(fig12_4, "fig12_4_param_posteriors_R.png")

# ------------------------------------------------------------------------------
# Fig. 12.5: MTE Curve by Graduate Program Area (Heterogeneity)
# Field-specific MTE curves reveal which programs drive the efficiency verdict.
#
# REVISION 4: program-area offsets are estimated from the data. The
# quadratic MTE regression (Eq. 12.2) is re-run with masters x program-area
# intercept shifts (Other = base), so each area's MTE curve is the base
# curve shifted up or down by its estimated offset. Health is added as a
# main effect here so that each shift is measured against the right base.
# ------------------------------------------------------------------------------
dat$m_stem   <- dat$masters * dat$stem_major
dat$m_bus    <- dat$masters * dat$bus_major
dat$m_educ   <- dat$masters * dat$ed_major
dat$m_health <- dat$masters * dat$health_major
area_terms <- c("m_stem", "m_bus", "m_educ", "m_health")
mte_area <- lm(rhs_formula("ln_salary",
  c("masters", "masters:phat", "masters:phat2", area_terms,
    X_controls, "health_major", "phat", "phat2")), data = dat)
area_vcov <- vcovHC(mte_area, type = "HC1")
ba <- coef(mte_area)
mo0 <- unname(ba["masters"] + ba["phat"])
mo1 <- unname(2 * (ba["masters:phat"] + ba["phat2"]))
mo2 <- unname(3 * ba["masters:phat2"])
off <- ba[area_terms]
cat("\nProgram-area MTE offsets (relative to Other), log-salary units:\n")
print(round(cbind(offset = off, se = sqrt(diag(area_vcov))[area_terms]), 4))
area_test <- linearHypothesis(mte_area, paste(area_terms, "= 0"),
                              vcov. = area_vcov, test = "F")
p_area <- area_test[2, "Pr(>F)"]
cat(sprintf("Joint test, no differences by program area: p = %6.4f\n", p_area))

byfield <- data.frame(u = (1:100) / 100)
# Base (Other) MTE from the program-area regression, shifted by each offset
byfield$mte_other  <- mo0 + mo1*byfield$u + mo2*byfield$u^2
byfield$mte_stem   <- byfield$mte_other + off["m_stem"]
byfield$mte_bus    <- byfield$mte_other + off["m_bus"]
byfield$mte_educ   <- byfield$mte_other + off["m_educ"]
byfield$mte_health <- byfield$mte_other + off["m_health"]

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
         "MTE shifted by program-area offsets estimated from the data (Other = base).",
         sprintf("Joint test of no differences by program area: p = %5.3f.", p_area),
         sep = "\n")) +
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
# Fig. 12.7: Who Bears the Cost - Accounts by Perspective (REVISION 6)
# Posterior means of the student, federal, and institutional accounts and
# their sum, the net social benefit.
# ------------------------------------------------------------------------------
sim_results <- haven::read_dta(file.path(tables_dir, "sim_results_ch12.dta"))

acct_levels <- c("Students", "Federal Government", "Institutions", "Society (Total)")
accounts <- data.frame(
  label = factor(acct_levels, levels = acct_levels),
  value = c(mean(sim_results$student_acct_s), mean(sim_results$federal_acct_s),
            mean(sim_results$inst_acct_s),    mean(sim_results$net_benefit_s))
)

fig12_7 <- ggplot(accounts, aes(x = label, y = value)) +
  geom_col(fill = "grey45", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = NULL, y = "Posterior Mean ($000s)",
       title = "Who Bears the Cost",
       subtitle = "Accounts by Perspective (Posterior Means)",
       caption = paste(
         "Society = Students + Federal Government + Institutions.",
         "Students: after-tax earnings, tuition, and lost loan subsidies.",
         "Federal: subsidy savings minus lost tax revenue.",
         sep = "\n"))

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
cat("  Tables:        ", file.path(tables_dir, "Table12_1_Descriptives_R.docx"), "\n")
cat("                 ", file.path(tables_dir, "Table12_2_Assumptions_R.docx"), "\n")
cat("                 ", file.path(tables_dir, "Table12_3_CBA_Summary_R.docx"), "\n")
cat("  Sim results:   ", file.path(tables_dir, "sim_results_ch12.dta"), "\n")
cat("  Figures:       ", file.path(graphs_dir, "fig12_1_posterior_nb_R.png"), "\n")
cat("                 ", file.path(graphs_dir, "fig12_2_mte_policy_R.png"), "\n")
cat("                 ", file.path(graphs_dir, "fig12_3_cba_decomp_R.png"), "\n")
cat("                 ", file.path(graphs_dir, "fig12_4_param_posteriors_R.png"), "\n")
cat("                 ", file.path(graphs_dir, "fig12_5_mte_byfield_R.png"), "\n")
cat("                 ", file.path(graphs_dir, "fig12_6_inst_rev_byfield_R.png"), "\n")
cat("                 ", file.path(graphs_dir, "fig12_7_full_cost_stack_R.png"), "\n")

sink()
