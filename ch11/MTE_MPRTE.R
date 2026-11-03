#========================================================================
# Chapter 11 – Sections 11.1–11.3: Marginal Treatment Effects
#             Returns to Master's Degree Completion
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch11
# Author: Marvin A. Titus
# R translation of MTE_MPRTE.do (October 2026 revision)
#------------------------------------------------------------------------
# RUN TIME — PLEASE READ BEFORE RUNNING
#   The R version is much faster than the Stata version because the MTE
#   estimator below is a few regressions rather than a call to mtefe.
#   Measured run times (R 4.3, one core):
#
#   Full run (default, area models on)    about 4 minutes
#   Area models off (run_areas <- FALSE)  about 3 minutes
#   Reusing saved bootstrap draws         about 2 minutes
#
#   - Slower machines may take up to twice as long.
#   - The state-clustered bootstrap (Section 6e, R_boot = 500) takes
#     about 2 minutes with area models on; the two wild cluster
#     bootstraps (9,999 draws each) take most of the remaining time.
#   - The bootstrap saves its draws to mte_bootstrap_draws_R.rds. Set
#     use_saved_boot <- TRUE (Section 6e) to reuse them on later runs.
#   - The script prints a projected total after the first replication.
#   - Switches: run_areas (search "OPTIONAL ANALYSIS SWITCH"); R_boot and
#     use_saved_boot (Section 6e).
#------------------------------------------------------------------------
# HOW THIS TRANSLATION MATCHES THE STATA SCRIPT
#   * mtefe has no R port. mte_poly() below re-implements mtefe's
#     parametric polynomial local IV estimator (Andresen 2018):
#       E[Y | X, P] = X b0 + P X (b1 - b0) + K(P),
#       k(u) = sum_j pi_j (u^j - 1/(j+1)),  K(p) = integral_0^p k(u) du,
#     with the treatment-parameter weights, mean-X vectors, and 99-point
#     grid taken from mtefe.ado. On the chapter data it reproduces the
#     Stata mtefe estimates to 4 decimal places (ATE, ATT, ATU, MPRTE1-3,
#     PRTE; LATE to about 0.0001). Analytic SEs use the same delta method.
#   * Heckman models (Stata heckman) are estimated directly: two-step by
#     probit + inverse Mills ratio, ML by optim(). No extra package.
#   * Wild cluster bootstraps (Stata boottest) are coded directly:
#     WCR for OLS and WRE for IV, Webb weights, 9,999 draws.
#   * RANDOM NUMBERS: R and Stata use different generators. Anything that
#     depends on a random draw differs between the two languages:
#       - ma_* program areas, IF they are generated here (Section 1b),
#       - prospective areas for non-completers (Section 6d),
#       - bootstrap standard errors and bands (Section 6e).
#     Point estimates for the pooled model do not depend on random draws
#     and match Stata. Area-specific estimates match only if both
#     languages read the same ma_* and ma_*_pro variables from
#     Example_7_5_3_updated.dta (created by make_Example_7_5_3_updated.do).
#
# NOTE: Code development was assisted by Claude (Anthropic). The author
# provided specifications and reviewed, tested, and validated all code.
#========================================================================
# Called by: R_code11.R (inherits graphs_dir, log_dir, theme_springer)
# Standalone: can also be run directly; uses fallback paths if needed.
#
# Data: synthetic B&B panel
#   Example_7_5_3_updated.dta  (primary; contains pre-generated ma_* vars)
#   Example_7_5_3.dta          (fallback; ma_* generated in Section 1b)
#
# Required packages: haven, sandwich, lmtest, AER, ggplot2
#
# Sections (Chapter 11 numbering; same as MTE_MPRTE.do):
#   1     Load dataset                                                  [11.1]
#   1b    Verify / generate master's program area indicators (ma_*)     [11.1]
#   2     Summary statistics                                            [11.1]
#   3     First stage and instrument relevance                          [11.1]
#   4     Naive OLS estimation                                          [11.1]
#   5     IV/2SLS estimation (LATE)                                     [11.1]
#   6     MTE estimation: probit, manual illustration, mtefe-equivalent [11.2]
#   6d    MTE by graduate program area (separate model per area)        [11.2]
#   6e    State-clustered bootstrap + wild cluster bootstrap            [11.2]
#   7     Results comparison                                            [11.2]
#   8     MTE visualization (Figs 11.3, 11.5)                           [11.2.3]
#   9-10b Policy-relevant treatment effects, Scenarios 1-8              [11.3]
#   11    PRTE by policy intensity (Fig 11.8)                           [11.3]
#   12    Comparing treatment effect parameters                         [11.3]
#   13    Policy weights and propensity support (Figs 11.6, 11.4)       [11.3]
#   14    Policy cost-benefit analysis (PRTE-based)                     [11.3]
#   15    Save results                                                  [11.4]
#   16    Final summary                                                 [11.4]
#========================================================================

#------------------------------------------------------------------------
# Packages
#------------------------------------------------------------------------
if (!exists("install_if_missing")) {
  install_if_missing <- function(pkgs) {
    new <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(new) > 0) install.packages(new, repos = "https://cloud.r-project.org")
    invisible(lapply(pkgs, library, character.only = TRUE))
  }
}
install_if_missing(c("haven", "sandwich", "lmtest", "AER", "ggplot2"))

#------------------------------------------------------------------------
# Fallback paths when running standalone (not called from R_code11.R)
#------------------------------------------------------------------------
if (!exists("graphs_dir") || !exists("log_dir")) {
  out_root <- if (Sys.info()[["user"]] == "marvi") {
    "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output"
  } else {
    "Output"
  }
  graphs_dir <- file.path(out_root, "graphs")
  log_dir    <- file.path(out_root, "logs")
  message("MTE_MPRTE.R (standalone): output root set to ", out_root)
}
dir.create(graphs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir,    recursive = TRUE, showWarnings = FALSE)

#------------------------------------------------------------------------
# Dedicated log (the _R suffix keeps it apart from the Stata log)
#------------------------------------------------------------------------
while (sink.number() > 0) sink()
log_file <- file.path(log_dir, "MTE_MPRTE_R.log")
log_con  <- file(log_file, open = "wt")
sink(log_con, split = TRUE)
cat("MTE_MPRTE.R log opened:", format(Sys.time()), "\n")
cat("Log file:", log_file, "\n")
script_start <- Sys.time()

#------------------------------------------------------------------------
# Helpers: Springer black-and-white theme, figure saver, banners
#------------------------------------------------------------------------
if (!exists("theme_springer")) {
  theme_springer <- function(base_size = 11) {
    ggplot2::theme_bw(base_size = base_size, base_family = "serif") +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        legend.position  = "bottom",
        plot.title       = ggplot2::element_text(hjust = 0.5),
        plot.subtitle    = ggplot2::element_text(hjust = 0.5),
        plot.caption     = ggplot2::element_text(hjust = 0, size = base_size - 2))
  }
}
fig_registry <- character(0)
save_fig <- function(p, name) {
  f <- file.path(graphs_dir, paste0(name, "_R.png"))
  ggplot2::ggsave(f, p, width = 6, height = 4.4, dpi = 200)
  fig_registry <<- c(fig_registry, f)
  cat("Figure saved:", f, "\n")
}
banner <- function(txt) cat("\n==============================================\n",
                            txt, "\n==============================================\n", sep = "")
f4 <- function(x) formatC(x, format = "f", digits = 4)

#------------------------------------------------------------------------
# OPTIONAL ANALYSIS SWITCH
#   run_areas = TRUE (default): run the area-specific MTE models
#     (Section 6d), their bootstrap replications, Fig 11.5, and the area
#     tables reported in the book.
#   run_areas = FALSE: skip them.
#------------------------------------------------------------------------
run_areas <- TRUE

areas <- c("stem", "business", "education", "health", "other")

#------------------------------------------------------------------------
# Data location: the author's Dropbox data folder; readers use the
# current working directory (data are downloaded from GitHub if absent).
#------------------------------------------------------------------------
if (Sys.info()[["user"]] == "marvi") {
  setwd("C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Data/Stata")
}
cat("Data directory:", getwd(), "\n")

################################################################################
# SECTION 1: Load Dataset
################################################################################
banner("LOADING SYNTHETIC B&B DATASET")

gh <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch11/"
get_file <- function(f) {
  if (!file.exists(f)) {
    cat("Attempting to download", f, "from GitHub...\n")
    ok <- tryCatch({ download.file(paste0(gh, f), f, mode = "wb", quiet = TRUE); TRUE },
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok) { if (file.exists(f)) file.remove(f); cat("Download failed:", f, "\n") }
  }
  file.exists(f)
}
if (get_file("Example_7_5_3_updated.dta")) {
  d <- haven::read_dta("Example_7_5_3_updated.dta")
} else if (get_file("Example_7_5_3.dta")) {
  cat("Loading Example_7_5_3.dta; ma_* will be generated in Section 1b.\n")
  d <- haven::read_dta("Example_7_5_3.dta")
} else {
  stop("Download of Example_7_5_3.dta failed. Download it manually from\n",
       "https://github.com/higher-ed-policy-analysis-2nd-edition/data/blob/main/ch11/Example_7_5_3.dta\n",
       "and place it in the working directory.")
}
d <- as.data.frame(haven::zap_labels(d))
d <- d[, !grepl("^_est_", names(d))]          # drop Stata estimation markers
if (!"id" %in% names(d)) d$id <- seq_len(nrow(d))
cat("Sample size:", nrow(d), "\n")

################################################################################
# SECTION 1b: Verify / generate master's program area indicators
#   ma_* = 0 for all untreated observations (masters == 0).
#   NOTE: if generated here, R's random draws differ from Stata's.
################################################################################
banner("MASTER'S PROGRAM AREA INDICATORS")

if (!"ma_stem" %in% names(d)) {
  cat("Generating ma_* variables from undergraduate major fields...\n")
  set.seed(20251130)
  r <- ifelse(d$masters == 1, runif(nrow(d)), NA)
  t1 <- d$masters == 1
  d$ma_stem      <- as.integer(t1 & d$stem_major == 1 & r <= 0.55)
  d$ma_business  <- as.integer(t1 & d$bus_major  == 1 & r <= 0.65 & d$ma_stem == 0)
  d$ma_education <- as.integer(t1 & d$ed_major   == 1 & r <= 0.70 & d$ma_stem == 0 & d$ma_business == 0)
  d$ma_health    <- as.integer(t1 & d$socsci_major == 1 & r <= 0.40 & d$ma_stem == 0 &
                                 d$ma_business == 0 & d$ma_education == 0)
  d$ma_health    <- pmax(d$ma_health, as.integer(t1 & d$stem_major == 1 & r > 0.55 & r <= 0.75 & d$ma_stem == 0))
  d$ma_other     <- as.integer(t1 & d$ma_stem == 0 & d$ma_business == 0 & d$ma_education == 0 & d$ma_health == 0)
  cat("ma_* variables generated (R random draws; differ from Stata's).\n")
}
n_treated <- sum(d$masters == 1)
cat("\n--- Program Area Distribution (Treated Only) ---\nTotal treated:", n_treated, "\n")
for (a in areas) {
  n_a <- sum(d[[paste0("ma_", a)]] == 1)
  cat(sprintf("  ma_%s: %d  (%5.1f%%)\n", a, n_a, 100 * n_a / n_treated))
}
ma_check <- rowSums(d[, paste0("ma_", areas)])
cat(if (any(ma_check[d$masters == 1] != 1)) "WARNING: treated obs with != 1 program area flag\n"
    else "CHECK PASSED: all treated obs have exactly 1 program area\n")
cat(if (any(ma_check[d$masters == 0] != 0)) "WARNING: untreated obs with non-zero program area flag\n"
    else "CHECK PASSED: all untreated obs have zero program area\n")

################################################################################
# SECTION 2: Summary Statistics
################################################################################
banner("SUMMARY STATISTICS")

print(table(d$masters))
treat_rate <- mean(d$masters)
cat("Treatment rate:", formatC(treat_rate, format = "f", digits = 3), "\n")

X <- c("female", "black", "hispanic", "asian", "age_ba", "firstgen", "parent_income_q",
       "parent_grad", "ugpa", "stem_major", "bus_major", "ed_major", "selective_inst",
       "public_ug", "state_unemp", "metro")
Z <- "ga_funding_adj"

sumstats <- function(v) data.frame(N = sapply(d[v], function(x) sum(!is.na(x))),
                                   Mean = sapply(d[v], mean, na.rm = TRUE),
                                   SD = sapply(d[v], sd, na.rm = TRUE),
                                   Min = sapply(d[v], min, na.rm = TRUE),
                                   Max = sapply(d[v], max, na.rm = TRUE))
print(round(sumstats(c("ln_salary", "salary", "masters", "ga_funding_adj")), 4))
cat("\nSalary by master's completion:\n")
print(round(do.call(rbind, lapply(split(d[, c("salary", "ln_salary")], d$masters),
                                  function(s) c(mean = colMeans(s), sd = sapply(s, sd), n = nrow(s)))), 4))
print(round(sumstats(X), 4))

cat("\n--- Mean Log Salary by Program Area (Treated Only) ---\n")
for (a in areas) {
  s <- d$ln_salary[d[[paste0("ma_", a)]] == 1]
  cat(sprintf("  ma_%s: mean ln_salary = %6.4f  (N = %d)\n", a, mean(s), length(s)))
}

################################################################################
# SECTION 3: First-Stage and Instrument Relevance
################################################################################
banner("INSTRUMENT RELEVANCE CHECK")

fs <- lm(reformulate(c(Z, X), "masters"), data = d)
fs_ct <- lmtest::coeftest(fs, vcov. = sandwich::vcovHC(fs, type = "HC1"))
print(fs_ct[Z, , drop = FALSE])
first_stage_F <- unname(fs_ct[Z, "t value"]^2)
cat("\nFirst-stage F (robust):", formatC(first_stage_F, format = "f", digits = 2), "\n")
cat(if (first_stage_F > 10) "RESULT: Strong instrument (F > 10)\n" else "WARNING: Potentially weak instrument\n")

################################################################################
# SECTION 4: Naive OLS Estimation
################################################################################
banner("NAIVE OLS ESTIMATION")

ols <- lm(reformulate(c("masters", X), "ln_salary"), data = d)
ols_ct <- lmtest::coeftest(ols, vcov. = sandwich::vcovHC(ols, type = "HC1"))
ols_est <- ols_ct["masters", 1]; ols_se <- ols_ct["masters", 2]
cat("OLS estimate:", f4(ols_est), "(SE =", f4(ols_se), ")\n")

################################################################################
# SECTION 5: IV/2SLS Estimation (LATE)
################################################################################
banner("IV/2SLS ESTIMATION (LATE)")

iv_f <- as.formula(paste("ln_salary ~ masters +", paste(X, collapse = " + "),
                         "|", Z, "+", paste(X, collapse = " + ")))
iv <- AER::ivreg(iv_f, data = d)
iv_ct <- lmtest::coeftest(iv, vcov. = sandwich::vcovHC(iv, type = "HC1"))
iv_est <- iv_ct["masters", 1]; iv_se <- iv_ct["masters", 2]
cat("IV/LATE estimate:", f4(iv_est), "(SE =", f4(iv_se), ")\n")
# Robust regression-based endogeneity test (Stata: estat endogenous)
d$fs_resid <- resid(fs)
cf <- lm(reformulate(c("masters", X, "fs_resid"), "ln_salary"), data = d)
cf_t <- lmtest::coeftest(cf, vcov. = sandwich::vcovHC(cf, type = "HC1"))["fs_resid", "t value"]
cat("Robust regression F(1,", cf$df.residual, ") =", formatC(cf_t^2, format = "f", digits = 1),
    " (p =", formatC(pf(cf_t^2, 1, cf$df.residual, lower.tail = FALSE), format = "f", digits = 4), ")\n")
d$fs_resid <- NULL

################################################################################
# SECTION 6: MTE ESTIMATION
#
# 6a  Propensity score (probit) — used for policy scenarios and figures
# 6b  Manual polynomial control function — TEACHING ILLUSTRATION ONLY
# 6c  mte_poly() — R equivalent of mtefe, pol(2) — MAIN ESTIMATOR
################################################################################
banner("MTE ESTIMATION")

# --- 6a. Propensity score ----------------------------------------------------
probit <- glm(reformulate(c(Z, X), "masters"), family = binomial("probit"), data = d)
d$phat    <- fitted(probit)
d$z_index <- predict(probit, type = "link")
ga_coef   <- unname(coef(probit)[Z])
cat("GA funding probit coefficient:", formatC(ga_coef, format = "f", digits = 5), "\n")

# --- 6b. Manual polynomial control function (teaching illustration) ---------
# The coefficients on masters and its interactions (theta) are NOT the MTE:
# they still contain selection. The MTE is recovered by the separate-
# approach conversion MTE(u) = d/du[u*theta(u)] + gamma'(u). See
# MTE_MPRTE.do, Section 6b, for the full explanation.
cat("\n--- 6b. Manual polynomial (illustration only) ---\n")
d$phat2 <- d$phat^2; d$phat3 <- d$phat^3

mq <- lm(reformulate(c("masters", "masters:phat", "masters:phat2", X, "phat", "phat2"), "ln_salary"), data = d)
bq <- coef(mq)
qc <- c(bq["masters"] + bq["phat"], 2 * bq["masters:phat"] + 2 * bq["phat2"], 3 * bq["masters:phat2"])
ate_man_quad <- qc[1] + qc[2] / 2 + qc[3] / 3
cat(sprintf("Quadratic MTE(u) = %7.4f + %7.4f*u + %7.4f*u^2\n", qc[1], qc[2], qc[3]))
cat("ATE (manual quadratic):", f4(ate_man_quad), "\n")

mc <- lm(reformulate(c("masters", "masters:phat", "masters:phat2", "masters:phat3",
                       X, "phat", "phat2", "phat3"), "ln_salary"), data = d)
bc <- coef(mc)
cc <- c(bc["masters"] + bc["phat"], 2 * bc["masters:phat"] + 2 * bc["phat2"],
        3 * bc["masters:phat2"] + 3 * bc["phat3"], 4 * bc["masters:phat3"])
ate_man_cubic <- unname(cc[1] + cc[2] / 2 + cc[3] / 3 + cc[4] / 4)
cat(sprintf("Cubic MTE(u) = %7.4f + %7.4f*u + %7.4f*u^2 + %7.4f*u^3\n", cc[1], cc[2], cc[3], cc[4]))
cat("ATE (manual cubic):", f4(ate_man_cubic), "\n")
area0P  <- with(d, cc[1] * phat + cc[2] * phat^2 / 2 + cc[3] * phat^3 / 3 + cc[4] * phat^4 / 4)
att_man <- unname(mean(area0P) / mean(d$phat))
atu_man <- unname((ate_man_cubic - mean(area0P)) / (1 - mean(d$phat)))
cat("ATT (manual cubic):", f4(att_man), "\n")
cat("ATU (manual cubic):", f4(atu_man), "\n")
cat("Note: illustration only — compare with the main estimates in Section 6c.\n")

# --- 6c. mte_poly(): R equivalent of mtefe, polynomial local IV -------------
# Arguments: data, outcome, treatment, instrument(s), controls, polynomial
# degree, and an optional function returning the counterfactual propensity
# score for a policy (PRTE), given the probit index.
mte_poly <- function(data, y, D, Z, X, pol = 2, prte_fun = NULL, se = TRUE) {
  n  <- nrow(data)
  pr <- glm(reformulate(c(Z, X), D), family = binomial("probit"), data = data)
  P  <- fitted(pr); gz <- predict(pr, type = "link")
  Dv <- data[[D]]
  Xm <- cbind(as.matrix(data[, X]), `_cons` = 1)
  kx <- ncol(Xm)
  # Second stage (local IV): Y on X, P*X, and K(P) with mean-zero k(u)
  K  <- sapply(1:pol, function(j) P^(j + 1) / (j + 1) - P / (j + 1))
  R  <- cbind(Xm, Xm * P, K)
  qrR <- qr(R)
  b   <- qr.coef(qrR, data[[y]]); b[is.na(b)] <- 0
  i10 <- (kx + 1):(2 * kx); ik <- (2 * kx + 1):(2 * kx + pol)
  b10 <- b[i10]; names(b10) <- colnames(Xm); pk <- b[ik]
  u   <- (1:99) / 100
  S   <- sapply(1:pol, function(j) u^j - 1 / (j + 1))          # 99 x pol basis of k(u)
  ku  <- drop(S %*% pk)
  # Treatment-parameter weights (as in mtefe.ado)
  prop_gt <- function(p, x) if (min(p) >= x) 1 else if (max(p) <= x) 0 else mean(p > x)
  pg   <- sapply(u, function(x) prop_gt(P, x)); pbar <- mean(P)
  dhat <- fitted(lm.fit(cbind(1, as.matrix(data[, c(Z, X)])), Dv))
  ups  <- lm.fit(cbind(1, as.matrix(data[, X])), dhat)$residuals
  covdu <- cov(Dv, ups)
  wl <- sapply(u, function(x) { g <- prop_gt(P, x)
                 if (g > 0) (1 / 99) * g * (mean(ups[P > x]) - mean(ups)) / covdu else 0 })
  bin  <- cut(P, breaks = seq(0.005, 0.995, by = 0.01), labels = FALSE, right = FALSE)
  pden <- tabulate(bin, nbins = 99) / sum(!is.na(bin))
  fgz  <- dnorm(gz)
  W <- list(
    ate    = list(u = rep(1 / 99, 99),                     x = rep(1, n)),
    att    = list(u = (1 / 99) * pg / pbar,                x = P / pbar),
    atu    = list(u = (1 / 99) * (1 - pg) / (1 - pbar),    x = (1 - P) / (1 - pbar)),
    late   = list(u = wl,                                  x = (Dv - mean(Dv)) * (ups - mean(ups)) / covdu),
    mprte1 = list(u = pden * dnorm(qnorm(u)) / mean(fgz),  x = fgz / mean(fgz)),
    mprte2 = list(u = pden,                                x = rep(1, n)),
    mprte3 = list(u = pden * u / pbar,                     x = rep(1, n)))
  if (!is.null(prte_fun)) {
    Pp  <- prte_fun(gz)
    pgp <- sapply(u, function(x) prop_gt(Pp, x))
    W$prte <- list(u = (pgp - pg) / (99 * (mean(Pp) - pbar)), x = (Pp - P) / (mean(Pp) - pbar))
  }
  # Mean of X for each parameter (mtefe: means of x * weight, _cons = 1)
  xbar <- function(w) { m <- colSums(Xm * w) / n; m["_cons"] <- 1; m }
  # Each parameter is a linear combination c'b of (b1 - b0, pi)
  cvec <- lapply(W, function(w) c(sum(w$u) * xbar(w$x), drop(crossprod(S, w$u))))
  eff  <- sapply(cvec, function(cv) sum(cv * c(b10, pk)))
  out  <- list(effects = eff, u = u, k = ku, mte = sum(xbar(rep(1, n)) * b10) + ku,
               b10 = b10, pk = pk, P = P, gz = gz, weights = lapply(W, `[[`, "u"))
  if (se) {
    # Analytic SEs (OLS vcov), as mtefe reports without bootreps()
    e   <- data[[y]] - drop(R %*% b)
    XtXi <- chol2inv(qr.R(qrR))
    V   <- sum(e^2) / (n - qrR$rank) * XtXi
    Vsub <- V[c(i10, ik), c(i10, ik)]
    out$se <- sapply(cvec, function(cv) sqrt(drop(t(cv) %*% Vsub %*% cv)))
    out$p_ess_het <- {                                   # Wald test of the k(u) terms
      bk <- pk; Vk <- V[ik, ik]; w <- drop(t(bk) %*% solve(Vk) %*% bk)
      pf(w / pol, pol, n - qrR$rank, lower.tail = FALSE) }
  }
  out
}

cat("\n--- 6c. MTE estimation (R equivalent of mtefe, main estimator) ---\n")
m_lin  <- mte_poly(d, "ln_salary", "masters", Z, X, pol = 1)
m_main <- mte_poly(d, "ln_salary", "masters", Z, X, pol = 2)
e_main <- m_main$effects
ate_est <- e_main["ate"]; att_est <- e_main["att"]; atu_est <- e_main["atu"]
late_est <- e_main["late"]; mprte1_est <- e_main["mprte1"]
mprte2_est <- e_main["mprte2"]; mprte3_est <- e_main["mprte3"]
p_ess_het <- m_main$p_ess_het
MTE_pooled <- m_main$mte; SUP <- m_main$u

cat("\nMTE results (pol = 2), analytic SEs in parentheses:\n")
for (s in names(e_main)) cat(sprintf("  %-7s %8.4f  (%6.4f)\n", toupper(s), e_main[s], m_main$se[s]))
cat("  2SLS for comparison:", f4(iv_est), "\n")
cat("  Test of essential heterogeneity, p =", f4(p_ess_het), "\n")
cat(sprintf("  MTE at u = 0.10, 0.50, 0.90: %7.4f, %7.4f, %7.4f\n",
            MTE_pooled[10], MTE_pooled[50], MTE_pooled[90]))
cat("Check: max |mte_poly P - phat| =", format(max(abs(m_main$P - d$phat)), digits = 3), "\n")

# Individual-level MTE at each student's own X and propensity score
Xm_all <- cbind(as.matrix(d[, X]), `_cons` = 1)
d$mte_hat <- drop(Xm_all %*% m_main$b10) +
  drop(sapply(1:2, function(j) d$phat^j - 1 / (j + 1)) %*% m_main$pk)

# --- Heckman selection model (Stata: heckman ..., twostep and ML) ----------
cat("\n--- Heckman Selection Model ---\n")
sel  <- d$masters == 1
imr  <- dnorm(d$z_index) / pnorm(d$z_index)
h2   <- lm(d$ln_salary[sel] ~ as.matrix(d[sel, X]) + imr[sel])
heck2_lambda <- unname(tail(coef(h2), 1))
delta  <- imr[sel] * (imr[sel] + d$z_index[sel])
heck2_sigma <- sqrt(mean(resid(h2)^2) + heck2_lambda^2 * mean(delta))
heck2_rho   <- heck2_lambda / heck2_sigma
cat(sprintf("Two-step: lambda = %7.4f  rho = %7.4f  sigma = %7.4f\n", heck2_lambda, heck2_rho, heck2_sigma))

Wsel <- cbind(1, as.matrix(d[, c(Z, X)])); Xo <- cbind(1, as.matrix(d[, X])); yv <- d$ln_salary
heck_ll <- function(th) {
  g <- th[1:ncol(Wsel)]; bb <- th[ncol(Wsel) + 1:ncol(Xo)]
  athrho <- th[length(th) - 1]; lns <- th[length(th)]
  rho <- tanh(athrho); s <- exp(lns)
  zg <- drop(Wsel %*% g)
  e  <- (yv[sel] - drop(Xo[sel, ] %*% bb)) / s
  sum(pnorm(-zg[!sel], log.p = TRUE)) +
    sum(pnorm((zg[sel] + rho * e) / sqrt(1 - rho^2), log.p = TRUE) + dnorm(e, log = TRUE) - lns)
}
th0 <- c(coef(probit), coef(h2)[1:ncol(Xo)], atanh(max(min(heck2_rho, 0.9), -0.9)), log(heck2_sigma))
hml <- optim(th0, heck_ll, method = "BFGS", control = list(fnscale = -1, maxit = 2000, reltol = 1e-12))
heck_ml_rho <- tanh(hml$par[length(th0) - 1]); heck_ml_sigma <- exp(hml$par[length(th0)])
heck_ml_lambda <- heck_ml_rho * heck_ml_sigma
ll_indep <- as.numeric(logLik(probit)) + as.numeric(logLik(lm(yv[sel] ~ Xo[sel, ] - 1)))
heck_ml_chi2 <- 2 * (hml$value - ll_indep)
cat(sprintf("Heckman ML: lambda = %6.4f  rho = %6.4f  LR chi2 = %6.2f (p = %6.4f)\n",
            heck_ml_lambda, heck_ml_rho, heck_ml_chi2, pchisq(heck_ml_chi2, 1, lower.tail = FALSE)))

################################################################################
# SECTION 6d: MTE BY GRADUATE PROGRAM AREA — SEPARATE MODEL PER AREA
#   Group a = completers in area a plus non-completers prospectively
#   assigned to area a (same transition probabilities as Section 1b).
#   NOTE: the prospective draw uses R's generator, so area results differ
#   from Stata's (see header).
#   Area models use a LINEAR MTE (pol = 1); the pooled model is quadratic.
#   The quadratic was unstable in groups of 500-2,000 students.
################################################################################
area_res <- list()
if (run_areas) {
  banner("MTE BY GRADUATE PROGRAM AREA (separate model per area)")
  # Use prospective areas from the dataset when present
  # (Example_7_5_3_updated.dta), so Stata and R share the same draws.
  pro_vars <- paste0("ma_", areas, "_pro")
  if (all(pro_vars %in% names(d))) {
    cat("Prospective areas (ma_*_pro) found in dataset; no new draws.\n")
    pro <- setNames(d[, pro_vars], areas)
  } else {
    set.seed(20260102)
    r  <- ifelse(d$masters == 0, runif(nrow(d)), NA)
    t0 <- d$masters == 0
    pro <- data.frame(stem = as.integer(t0 & d$stem_major == 1 & r <= 0.55))
    pro$business  <- as.integer(t0 & d$bus_major == 1 & r <= 0.65 & pro$stem == 0)
    pro$education <- as.integer(t0 & d$ed_major == 1 & r <= 0.70 & pro$stem == 0 & pro$business == 0)
    pro$health    <- as.integer(t0 & d$socsci_major == 1 & r <= 0.40 & pro$stem == 0 &
                                  pro$business == 0 & pro$education == 0)
    pro$health    <- pmax(pro$health, as.integer(t0 & d$stem_major == 1 & r > 0.55 & r <= 0.75 & pro$stem == 0))
    pro$other     <- as.integer(t0 & rowSums(pro[, c("stem", "business", "education", "health")]) == 0)
    pro[is.na(pro)] <- 0
  }
  for (a in areas) {
    d[[paste0("ma_", a, "_pro")]] <- pro[[a]]
    d[[paste0("grp_", a)]] <- as.integer(d[[paste0("ma_", a)]] == 1 | pro[[a]] == 1)
  }
  cat(if (any(rowSums(pro)[d$masters == 0] != 1)) "WARNING: untreated obs with != 1 prospective area\n"
      else "CHECK PASSED: all untreated obs have exactly 1 prospective area\n")

  cat("\nArea         N    Completers    ATE      ATT      ATU      LATE\n")
  for (a in areas) {
    da <- d[d[[paste0("grp_", a)]] == 1, ]
    keepX <- X[sapply(da[X], function(v) length(unique(v)) > 1)]   # drop constants
    fit <- tryCatch(mte_poly(da, "ln_salary", "masters", Z, keepX, pol = 1),
                    error = function(e) { cat("Model failed for", a, ":", conditionMessage(e), "\n"); NULL })
    if (!is.null(fit)) {
      lo <- max(min(fit$P[da$masters == 1]), min(fit$P[da$masters == 0]))
      hi <- min(max(fit$P[da$masters == 1]), max(fit$P[da$masters == 0]))
      area_res[[a]] <- list(eff = fit$effects, mte = fit$mte, lo = lo, hi = hi,
                            n = nrow(da), ntr = sum(da$masters), X = keepX)
      cat(sprintf("  %-10s %5d  %5d     %8.4f %8.4f %8.4f %8.4f\n", a, nrow(da), sum(da$masters),
                  fit$effects["ate"], fit$effects["att"], fit$effects["atu"], fit$effects["late"]))
    }
  }
  cat("\nCommon support of each area's propensity score:\n")
  for (a in names(area_res)) cat(sprintf("  %s: [%5.3f, %5.3f]\n", a, area_res[[a]]$lo, area_res[[a]]$hi))
} else {
  cat("\nSection 6d skipped (run_areas = FALSE).\n")
}

################################################################################
# SECTION 6e: STATE-CLUSTERED BOOTSTRAP (model re-estimated each rep)
#   Each replication resamples the 50 states with replacement and
#   re-estimates the pooled model (probit + MTE) and the five area models.
#   SEs = SD of replicates; Fig 11.3 band = pointwise 2.5/97.5 percentiles.
################################################################################
R_boot         <- 500
use_saved_boot <- FALSE
bootfile       <- "mte_bootstrap_draws_R.rds"

if (use_saved_boot && !file.exists(bootfile)) {
  cat("Saved bootstrap file not found; running the bootstrap.\n"); use_saved_boot <- FALSE
}
if (!use_saved_boot) {
  banner(paste0("STATE-CLUSTERED BOOTSTRAP (G=50, R=", R_boot, ")"))
  set.seed(20260101)
  states  <- unique(d$state)
  rows_by <- split(seq_len(nrow(d)), d$state)
  draws <- vector("list", R_boot)
  t0 <- Sys.time()
  for (b in seq_len(R_boot)) {
    db <- d[unlist(rows_by[as.character(sample(states, length(states), replace = TRUE))]), ]
    out <- tryCatch({
      m <- mte_poly(db, "ln_salary", "masters", Z, X, pol = 2, se = FALSE)
      res <- c(m$effects[c("ate", "att", "atu", "late", "mprte1")], setNames(m$mte, paste0("m", 1:99)))
      if (run_areas) for (a in names(area_res)) {
        da <- db[db[[paste0("grp_", a)]] == 1, ]
        ea <- tryCatch(mte_poly(da, "ln_salary", "masters", Z, area_res[[a]]$X, pol = 1, se = FALSE)$effects,
                       error = function(e) c(ate = NA, att = NA, atu = NA, late = NA))
        res <- c(res, setNames(ea[c("ate", "att", "atu", "late")], paste0(c("ate", "att", "atu", "late"), "_", a)))
      }
      res
    }, error = function(e) NULL)
    draws[[b]] <- out
    if (b == 1) {
      secs1 <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      cat(sprintf("First replication took %4.1f seconds. Projected total: about %3.0f minutes.\n",
                  secs1, secs1 * R_boot / 60))
    }
    if (b %% 50 == 0) cat("  ", b, "reps done\n")
  }
  draws <- do.call(rbind, draws[!vapply(draws, is.null, logical(1))])
  saveRDS(draws, bootfile)
  cat("Bootstrap complete:", nrow(draws), "of", R_boot, "reps successful\n")
} else {
  draws <- readRDS(bootfile)
  cat("\nUsing saved bootstrap draws from", bootfile, "\n")
}
n_boot <- nrow(draws)
se_b   <- apply(draws, 2, sd, na.rm = TRUE)
MLO <- apply(draws[, paste0("m", 1:99)], 2, quantile, 0.025, na.rm = TRUE)
MHI <- apply(draws[, paste0("m", 1:99)], 2, quantile, 0.975, na.rm = TRUE)

cat("\n--- Bootstrap SEs: Pooled Parameters (", n_boot, " reps) ---\n", sep = "")
for (s in c("ate", "att", "atu", "late", "mprte1"))
  cat(sprintf("  %-7s = %7.4f  (BS SE = %6.4f)\n", toupper(s), e_main[s], se_b[s]))

area_table <- function(s) {
  cat("\nArea          Estimate   BS SE    95% CI                  Sig\n")
  cat("----------------------------------------------------------------\n")
  for (a in c("other", "stem", "business", "education", "health")) {
    if (is.null(area_res[[a]])) next
    est <- area_res[[a]]$eff[s]; sev <- se_b[paste0(s, "_", a)]
    lo <- est - 1.96 * sev; hi <- est + 1.96 * sev
    sig <- if (lo > 0 || hi < 0) "***" else "   "
    cat(sprintf("  %-11s %7.4f    %6.4f   [%7.4f, %7.4f]   %s\n", a, est, sev, lo, hi, sig))
  }
}
if (run_areas) {
  for (s in c("ate", "att", "atu")) { cat("\n--- Bootstrap SEs: Area-Specific", toupper(s), "---"); area_table(s) }
  cat("  *** = 95% CI excludes zero (p < 0.05, two-tailed)\n")
  cat("  Note: ATU based on prospective program area assignment (seed 20260102).\n")
}

# --- Wild cluster bootstrap for OLS (WCR) and IV (WRE), Webb weights --------
webb <- function(G) sample(c(-sqrt(3 / 2), -1, -sqrt(1 / 2), sqrt(1 / 2), 1, sqrt(3 / 2)), G, replace = TRUE)
cl_t <- function(Xr, e, beta, j, cl, XtXi, adj) {   # cluster-robust t for coefficient j
  sc <- rowsum(Xr * e, cl); V <- adj * XtXi %*% crossprod(sc) %*% XtXi
  beta[j] / sqrt(V[j, j])
}
wild_ols <- function(B = 9999) {
  cl <- d$state; G <- length(unique(cl)); n <- nrow(d)
  Xr <- cbind(1, d$masters, as.matrix(d[, X])); k <- ncol(Xr)
  adj <- G / (G - 1) * (n - 1) / (n - k)
  XtXi <- solve(crossprod(Xr)); bh <- XtXi %*% crossprod(Xr, d$ln_salary)
  t_obs <- cl_t(Xr, d$ln_salary - drop(Xr %*% bh), bh, 2, cl, XtXi, adj)
  Xres <- cbind(1, as.matrix(d[, X])); r0 <- lm.fit(Xres, d$ln_salary)
  ufit <- r0$fitted.values; ures <- r0$residuals
  gi <- match(cl, unique(cl))
  tb <- replicate(B, { ys <- ufit + ures * webb(G)[gi]
                       bs <- XtXi %*% crossprod(Xr, ys)
                       cl_t(Xr, ys - drop(Xr %*% bs), bs, 2, cl, XtXi, adj) })
  mean(abs(tb) >= abs(t_obs))
}
wild_iv <- function(B = 9999) {
  cl <- d$state; G <- length(unique(cl)); n <- nrow(d); gi <- match(cl, unique(cl))
  Xc <- cbind(1, as.matrix(d[, X])); Zf <- cbind(d[[Z]], Xc); y <- d$ln_salary; Dv <- d$masters
  tsls <- function(y, Dv) {
    Dh <- drop(Zf %*% qr.coef(qr(Zf), Dv)); Xh <- cbind(Dh, Xc); Xr <- cbind(Dv, Xc)
    XtXi <- solve(crossprod(Xh)); bb <- XtXi %*% crossprod(Xh, y)
    e <- y - drop(Xr %*% bb); k <- ncol(Xr)
    cl_t(Xh, e, bb, 1, cl, XtXi, G / (G - 1) * (n - 1) / (n - k))
  }
  t_obs <- tsls(y, Dv)
  r0 <- lm.fit(Xc, y); u0 <- r0$residuals; yfit <- r0$fitted.values   # restricted (beta = 0)
  f1 <- lm.fit(cbind(Zf, u0), Dv); cf1 <- f1$coefficients
  Dfit <- drop(Zf %*% cf1[1:ncol(Zf)]); v0 <- Dv - Dfit
  tb <- replicate(B, { w <- webb(G)[gi]; tsls(yfit + u0 * w, Dfit + v0 * w) })
  mean(abs(tb) >= abs(t_obs))
}
set.seed(20260103)
cat("\n--- Wild Cluster Bootstrap: OLS (WCR, Webb, 9,999 draws) ---\n")
cat("  p-value (wild cluster):", f4(wild_ols()), "\n")
cat("--- Wild Cluster Bootstrap: IV (WRE, Webb, 9,999 draws) ---\n")
cat("  p-value (wild cluster):", f4(wild_iv()), "\n")

################################################################################
# SECTION 7: Results Comparison
################################################################################
banner("RESULTS COMPARISON")
cat(sprintf("  Naive OLS:        %7.4f (SE = %6.4f — likely biased)\n", ols_est, ols_se))
cat(sprintf("  IV/2SLS:          %7.4f (SE = %6.4f — complier effect)\n", iv_est, iv_se))
for (s in c("late", "ate", "att", "atu", "mprte1"))
  cat(sprintf("  MTE %-7s       %7.4f (BS SE = %6.4f)\n", toupper(s), e_main[s], se_b[s]))
e_lin <- m_lin$effects
cat(sprintf("\n  Sensitivity — linear MTE (pol = 1): ATE = %7.4f  ATT = %7.4f  ATU = %7.4f  LATE = %7.4f\n",
            e_lin["ate"], e_lin["att"], e_lin["atu"], e_lin["late"]))
cat(sprintf("  Illustration — manual cubic (Section 6b): ATE = %7.4f  ATT = %7.4f  ATU = %7.4f\n",
            ate_man_cubic, att_man, atu_man))
cat(if (att_est > ate_est && ate_est > atu_est) "\n  ATT > ATE > ATU: POSITIVE SELECTION on gains\n"
    else if (att_est < ate_est && ate_est < atu_est) "\n  ATT < ATE < ATU: NEGATIVE (REVERSE) SELECTION on gains\n"
    else "\n  Mixed selection pattern\n")
ols_bias <- (ols_est - ate_est) / ate_est * 100
cat(sprintf("OLS BIAS: %6.1f%% relative to MTE-based ATE\n", ols_bias))

################################################################################
# SECTION 8: MTE Visualization (Figs 11.3, 11.5)
################################################################################
banner("MTE VISUALIZATION")

g3 <- ggplot2::ggplot(data.frame(u = SUP, mte = MTE_pooled, lo = MLO, hi = MHI), ggplot2::aes(u)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi, fill = "95% bootstrap band")) +
  ggplot2::geom_line(ggplot2::aes(y = mte, linetype = "Estimated MTE"), linewidth = 0.9) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  ggplot2::scale_fill_manual(NULL, values = "grey85") +
  ggplot2::scale_linetype_manual(NULL, values = "solid") +
  ggplot2::labs(title = "Estimated MTE Curve - Pooled", subtitle = "Master's Degree Effect on Log Salary",
                x = "u (Unobserved Resistance to Treatment)", y = "Marginal Treatment Effect",
                caption = paste0("MTE at mean X (quadratic, local IV). Shaded: 95% state-clustered bootstrap band.\n",
                                 "Declining MTE indicates positive selection on gains.")) +
  theme_springer()
save_fig(g3, "fig11_3_mte_curve")

if (run_areas && length(area_res) > 0) {
  lab <- c(health = "Health & Related", stem = "STEM", business = "Business",
           education = "Education", other = "Other")
  df5 <- do.call(rbind, lapply(names(area_res), function(a) {
    ar <- area_res[[a]]; u <- (1:99) / 100
    data.frame(u = u, mte = ifelse(u < ar$lo | u > ar$hi, NA, ar$mte), area = unname(lab[a])) }))
  df5$area <- factor(df5$area, levels = lab)
  g5 <- ggplot2::ggplot(df5, ggplot2::aes(u, mte, linetype = area, colour = area)) +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
    ggplot2::scale_colour_manual(NULL, values = c("black", "black", "black", "grey50", "grey50")) +
    ggplot2::scale_linetype_manual(NULL, values = c("solid", "dashed", "longdash", "solid", "dashed")) +
    ggplot2::labs(title = "MTE Curves by Graduate Program Area",
                  subtitle = "Field-specific returns to master's degree",
                  x = "u (Unobserved Resistance to Treatment)", y = "Marginal Treatment Effect",
                  caption = paste0("Separate model per area (linear MTE), at the area's mean X.\n",
                                   "Curves restricted to each area's common propensity-score support.")) +
    theme_springer()
  save_fig(g5, "fig11_5_mte_byarea_curve")
}

################################################################################
# SECTIONS 9-10b: POLICY-RELEVANT TREATMENT EFFECTS (PRTE)
#   Each scenario gives the counterfactual propensity score as a function
#   of the probit index (gz); mte_poly() then computes the PRTE.
#   SEs are analytic (they ignore first-stage error).
################################################################################
banner("POLICY-RELEVANT TREATMENT EFFECTS")

ga_current <- mean(d$ga_funding_adj); ga_new <- ga_current * 1.2
cat(sprintf("Current mean GA: $%5.2fk  Proposed (20%% above mean): $%5.2fk\n", ga_current, ga_new))
targeted_lowinc <- as.integer(d$parent_income_q <= 2)
target_health   <- as.integer(d$stem_major == 1 | d$socsci_major == 1)
scen <- list(
  ga20   = list(lbl = "GA set to 120% of mean (Sec. 9)",   f = function(gz) pnorm(gz + ga_coef * (ga_new - d$ga_funding_adj))),
  unif   = list(lbl = "Uniform +$1k (Scen. 1)",            f = function(gz) pnorm(gz + ga_coef * 1)),
  lowinc = list(lbl = "Low-income +$2k (Scen. 2)",         f = function(gz) pnorm(gz + ga_coef * 2 * targeted_lowinc)),
  stem   = list(lbl = "STEM ug +$3k (Scen. 3)",            f = function(gz) pnorm(gz + ga_coef * 3 * d$stem_major)),
  ed     = list(lbl = "Education ug +$2.5k (Scen. 4)",     f = function(gz) pnorm(gz + ga_coef * 2.5 * d$ed_major)),
  pstem  = list(lbl = "STEM pipeline +$2.5k (Scen. 5)",    f = function(gz) pnorm(gz + ga_coef * 2.5 * d$stem_major)),
  pbus   = list(lbl = "Business pipeline +$2.5k (Scen. 6)", f = function(gz) pnorm(gz + ga_coef * 2.5 * d$bus_major)),
  ped    = list(lbl = "Education pipeline +$2.5k (Scen. 7)", f = function(gz) pnorm(gz + ga_coef * 2.5 * d$ed_major)),
  phlth  = list(lbl = "Health pipeline +$2.5k (Scen. 8)",  f = function(gz) pnorm(gz + ga_coef * 2.5 * target_health)))

cat("\nScenario                              Mean dP     PRTE    (SE, analytic)\n")
cat("-------------------------------------------------------------------------\n")
prte <- prte_se <- dp <- c(); W_prte <- list()
for (s in names(scen)) {
  dp[s] <- mean(scen[[s]]$f(d$z_index) - d$phat)
  m <- mte_poly(d, "ln_salary", "masters", Z, X, pol = 2, prte_fun = scen[[s]]$f)
  prte[s] <- m$effects["prte"]; prte_se[s] <- m$se["prte"]; W_prte[[s]] <- m$weights$prte
  cat(sprintf("  %-36s %7.4f   %7.4f   (%6.4f)\n", scen[[s]]$lbl, dp[s], prte[s], prte_se[s]))
}
cat(sprintf("\n  Marginal versions: MPRTE1 = %6.4f  MPRTE2 = %6.4f  MPRTE3 = %6.4f\n",
            mprte1_est, mprte2_est, mprte3_est))

################################################################################
# SECTION 11: PRTE BY POLICY INTENSITY (uniform GA increase) — Fig 11.8
################################################################################
banner("PRTE BY POLICY INTENSITY")
INT <- data.frame(ga_increase = (1:20) * 0.5)
INT$prte_int <- sapply(INT$ga_increase, function(inc)
  mte_poly(d, "ln_salary", "masters", Z, X, pol = 2, se = FALSE,
           prte_fun = function(gz) pnorm(gz + ga_coef * inc))$effects["prte"])
print(round(INT, 4), row.names = FALSE)
g8 <- ggplot2::ggplot(INT, ggplot2::aes(ga_increase, prte_int)) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  ggplot2::expand_limits(y = 0) +
  ggplot2::labs(title = "PRTE by Policy Intensity",
                subtitle = "Average return for students induced by a uniform GA increase",
                x = "GA Funding Increase ($1000s)", y = "PRTE") +
  theme_springer()
save_fig(g8, "fig11_8_mprte_by_intensity")

################################################################################
# SECTION 12: COMPARING TREATMENT EFFECT PARAMETERS
################################################################################
banner("COMPARISON OF TREATMENT EFFECT PARAMETERS")
cat("Parameter     MTE (pol=2)   BS SE      MTE (pol=1)\n")
cat("====================================================\n")
for (s in c("ate", "att", "atu", "late"))
  cat(sprintf("%-13s %8.4f     (%6.4f)   %8.4f\n", toupper(s), e_main[s], se_b[s], e_lin[s]))
cat(sprintf("%-13s %8.4f     (%6.4f)\n", "2SLS", iv_est, iv_se))
cat(sprintf("%-13s %8.4f     (%6.4f)\n", "MPRTE1", mprte1_est, se_b["mprte1"]))
cat("----------------------------------------------------\nPRTE by scenario (analytic SEs):\n")
for (s in names(scen)[-1]) cat(sprintf("  %-36s %7.4f   (%6.4f)\n", scen[[s]]$lbl, prte[s], prte_se[s]))

################################################################################
# SECTION 13: POLICY WEIGHTS AND PROPENSITY-SCORE SUPPORT (Figs 11.6, 11.4)
################################################################################
# Fig 11.6: MTE with policy weights (uniform +$1k vs low-income +$2k)
wmax <- max(c(W_prte$unif, W_prte$lowinc)); mrng <- range(MTE_pooled)
sc6  <- diff(mrng) / wmax
df6  <- data.frame(u = SUP, mte = MTE_pooled, w_unif = W_prte$unif, w_low = W_prte$lowinc)
g6 <- ggplot2::ggplot(df6, ggplot2::aes(u)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = mrng[1], ymax = mrng[1] + w_unif * sc6,
                                    fill = "Uniform +$1k weights"), colour = "grey60") +
  ggplot2::geom_line(ggplot2::aes(y = mrng[1] + w_low * sc6, linetype = "Low-income +$2k weights"),
                     colour = "grey35", linewidth = 0.8) +
  ggplot2::geom_line(ggplot2::aes(y = mte, linetype = "Estimated MTE"), linewidth = 0.9) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
  ggplot2::scale_y_continuous("Marginal Treatment Effect",
                              sec.axis = ggplot2::sec_axis(~ (. - mrng[1]) / sc6, name = "Policy weight")) +
  ggplot2::scale_fill_manual(NULL, values = "grey85") +
  ggplot2::scale_linetype_manual(NULL, values = c("Estimated MTE" = "solid", "Low-income +$2k weights" = "dashed")) +
  ggplot2::coord_cartesian(ylim = c(mrng[1], mrng[2])) +
  ggplot2::labs(title = "MTE Curve and Policy-Relevant Weights", x = "u (Unobserved Resistance to Treatment)",
                caption = "Weights show where each policy's induced completers lie on the resistance scale.") +
  theme_springer()
save_fig(g6, "fig11_6_mte_policy_regions")

# Fig 11.4: propensity-score histogram with the MTE curve
bins <- as.data.frame(table(p_bin = floor(d$phat * 20) / 20 + 0.025))
bins$p_bin <- as.numeric(as.character(bins$p_bin))
yb4 <- min(MTE_pooled, 0)                              # bars rise from the bottom of the MTE axis
sc4 <- max(bins$Freq) / (max(MTE_pooled) - yb4)
g4 <- ggplot2::ggplot() +
  ggplot2::geom_rect(data = bins, ggplot2::aes(xmin = p_bin - 0.0225, xmax = p_bin + 0.0225,
                                               ymin = yb4, ymax = yb4 + Freq / sc4, fill = "Students (count)"),
                     colour = "grey50") +
  ggplot2::geom_line(data = data.frame(u = SUP, mte = MTE_pooled),
                     ggplot2::aes(u, mte, linetype = "Estimated MTE"), linewidth = 0.9) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  ggplot2::scale_y_continuous("Marginal Treatment Effect",
                              sec.axis = ggplot2::sec_axis(~ (. - yb4) * sc4, name = "Number of students")) +
  ggplot2::scale_fill_manual(NULL, values = "grey80") +
  ggplot2::scale_linetype_manual(NULL, values = "solid") +
  ggplot2::labs(title = "MTE and the Distribution of Propensity Scores", x = "Propensity score / u",
                caption = paste0("Bars: students by propensity score. Line: MTE at mean X.\n",
                                 "Where bars are short, the curve relies on the polynomial form.")) +
  theme_springer()
save_fig(g4, "fig11_4_mte_by_propensity")

################################################################################
# SECTION 14: POLICY COST-BENEFIT ANALYSIS (PRTE-based)
################################################################################
banner("POLICY COST-BENEFIT ANALYSIS")
cost_per_degree <- 50000; career_years <- 30; discount_rate <- 0.03; base_salary <- 47000
pv_factor <- (1 - (1 + discount_rate)^(-career_years)) / discount_rate
cat("Present value factor (30 years, 3%):", formatC(pv_factor, format = "f", digits = 2), "\n")
cba <- function(s, base) {
  ag <- base * (exp(prte[s]) - 1); pvg <- ag * pv_factor
  cat(sprintf("  %-36s %6.4f   $%7.0f   $%9.0f   %5.2f\n", scen[[s]]$lbl, prte[s], ag, pvg, pvg / cost_per_degree))
}
cat("\n--- Scenarios 1-4: PRTE-based CBA ---\n")
cat("Policy                                 PRTE   Annual Gain   PV Gain    B/C Ratio\n")
for (s in c("unif", "lowinc", "stem", "ed")) cba(s, base_salary)
cat("\n--- Scenarios 5-8: Graduate Pipeline PRTE-based CBA ---\n")
base_pipe <- c(pstem = 65000, pbus = 60000, ped = 42000, phlth = 68000)
cat("  Base salaries: STEM=$65,000 Business=$60,000 Ed=$42,000 Health=$68,000\n")
for (s in names(base_pipe)) cba(s, base_pipe[s])
cat("Note: B/C > 1 suggests policy expansion is beneficial (synthetic data only).\n")

################################################################################
# SECTION 15: Save Results
################################################################################
drop_cols <- c("phat2", "phat3")
haven::write_dta(d[, setdiff(names(d), drop_cols)], "bb_mte_analysis_R.dta")
fld <- aggregate(cbind(masters, ln_salary, phat, mte_hat, ma_stem, ma_business, ma_education,
                       ma_health, ma_other) ~ stem_major + ed_major, data = d, FUN = mean)
fld$sd_mte <- aggregate(mte_hat ~ stem_major + ed_major, data = d, FUN = sd)$mte_hat
fld$n      <- aggregate(id ~ stem_major + ed_major, data = d, FUN = length)$id
write.csv(fld, "mte_summary_by_field_R.csv", row.names = FALSE)
trt <- d[d$masters == 1, ]
pa  <- aggregate(cbind(ln_salary, salary, phat, mte_hat) ~ ma_stem + ma_business + ma_education +
                   ma_health + ma_other, data = trt, FUN = mean)
pa$n <- aggregate(id ~ ma_stem + ma_business + ma_education + ma_health + ma_other, data = trt, FUN = length)$id
write.csv(pa, "mte_summary_by_program_area_R.csv", row.names = FALSE)

################################################################################
# SECTION 16: FINAL SUMMARY
################################################################################
banner("ANALYSIS COMPLETE")
cat(sprintf("  1.  Treatment rate:          %5.3f\n", treat_rate))
cat(sprintf("  2.  First-stage F:           %6.1f\n", first_stage_F))
cat(sprintf("  3.  OLS estimate (biased):   %7.4f\n", ols_est))
cat(sprintf("  4.  IV/2SLS (LATE):          %7.4f\n", iv_est))
i <- 5
for (s in c("ate", "att", "atu", "late", "mprte1")) {
  cat(sprintf("  %d.  MTE %-7s              %7.4f (BS SE = %6.4f)\n", i, toupper(s), e_main[s], se_b[s])); i <- i + 1
}
cat(sprintf(" 10.  Essential heterogeneity: p = %6.4f\n", p_ess_het))
if (run_areas) {
  cat("\nAREA-SPECIFIC ATE, ATT, AND ATU (separate model per area)\n")
  cat("(State-clustered bootstrap, G=50, seed 20260101; R random draws)\n")
  for (s in c("ate", "att", "atu")) { cat("\n", toupper(s), " estimates:", sep = ""); area_table(s) }
  cat("  *** = 95% CI excludes zero (p < 0.05, two-tailed)\n")
  cat("  Note: area groups are small (Business ~179 completers); interpret with caution.\n")
}
cat("\nPRTE SUMMARY:\n")
for (s in names(scen)[-1]) cat(sprintf("  %-36s %7.4f\n", scen[[s]]$lbl, prte[s]))
cat("\nBootstrap: G=50 state clusters,", n_boot, "reps, seed 20260101\n")
cat("Files saved: bb_mte_analysis_R.dta, mte_summary_by_field_R.csv,\n",
    "            mte_summary_by_program_area_R.csv,", bootfile, "\n")
cat("Figures:\n"); cat(paste0("  ", fig_registry, "\n"), sep = "")
cat("\nIMPORTANT NOTE: Synthetic data — results illustrate methods only.\n")
cat("Total run time:", format(round(difftime(Sys.time(), script_start, units = "mins"), 1)), "\n")
banner("END OF MTE/MPRTE ANALYSIS")

#------------------------------------------------------------------------
# Close this sub-script's dedicated log
#------------------------------------------------------------------------
print(warnings())
cat("MTE_MPRTE.R log closed:", format(Sys.time()), "\n")
sink()
close(log_con)

#========================================================================
# END OF MTE_MPRTE.R
#========================================================================
