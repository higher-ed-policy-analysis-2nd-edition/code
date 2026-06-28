#=========================================================================
# CATE_R.R  --  Section 11.1.3: Conditional Average Treatment Effects
# R translation of CATE.do
# Higher Education Policy Analysis Using Quantitative Techniques (2nd ed.)
# Author: Marvin A. Titus
# Date: June 2026
# NOTE: Code development was assisted by Claude (Anthropic). The author
# provided specifications and reviewed, tested, and validated all code.
#=========================================================================
#
# PURPOSE
# -------
# Estimate Conditional Average Treatment Effects (CATEs) for the return
# to master's degree completion using the same instrument (state-funded
# graduate assistantship funding, ga_funding_adj) and data as
# Sections 11.1-11.1.2.
#
# A CATE conditions on observed covariates X:
#
#   CATE(x) = E[Y(1) - Y(0) | X = x]
#
# Unlike the LATE -- which averages over all compliers near the instrument
# margin -- a CATE asks: *for whom* does the treatment effect differ by
# observed characteristics (field of study, family income, first-gen status)?
#
# Three complementary strategies are implemented:
#   (a) Subgroup IV/2SLS  -- run the baseline IV model separately for each
#       subgroup cell and collect point estimates + SEs.
#   (b) Interaction IV    -- include subgroup x masters interaction terms
#       in the full-sample model and test for differential effects.
#   (c) Forest plot and comparison table.
#
# VARIABLE NAMES (match MTE_MPRTE.R exactly)
# -------
#   Outcome:    ln_salary
#   Treatment:  masters          (Completed Master's Degree, 1=Yes)
#   Instrument: ga_funding_adj   (state GA funding, $1,000s)
#   Controls:   x_controls = female black hispanic asian age_ba firstgen
#                 parent_income_q parent_grad ugpa stem_major bus_major
#                 ed_major selective_inst public_ug state_unemp metro
#
# OUTPUTS
# -------
#   fig11_7_cate_forest.png   -- forest plot of CATE by subgroup
#   fig11_8_cate_interact.png -- interaction IV coefficient plot
#   tab11_2_cate_subgroup.rtf -- comparison table (OLS / LATE / CATE)
#
# INHERITS (from R_code11.R / driver environment)
#   graphs_dir, tables_dir, log connection, theme_springer()
#
# DATA
#   Example_7_5_3_updated.dta  (reloaded here for a clean workspace)
#=========================================================================

cat("\n==========================================================\n")
cat(" Section 11.1.3: Conditional Average Treatment Effects\n")
cat("==========================================================\n")

#-------------------------------------------------------------------------
# Required packages
#-------------------------------------------------------------------------
suppressMessages({
  library(haven)      # read .dta files
  library(dplyr)
  library(tidyr)
  library(ivreg)      # ivreg() -- 2SLS, mirrors Stata's ivregress 2sls
  library(sandwich)   # vcovHC -- robust ("HC1") standard errors
  library(lmtest)      # coeftest, waldtest, linearHypothesis-style tests
  library(car)         # linearHypothesis() -- joint Wald test
  library(ggplot2)
  library(stringr)
})

#-------------------------------------------------------------------------
# 0. Reload the Part B dataset for a clean workspace
#    (MTE_MPRTE.R may have left objects from a reshaped or resampled
#    data frame in the R session; reloading here mirrors Stata's
#    "use ..., clear" behavior for a fresh start.)
#-------------------------------------------------------------------------

df <- NULL
if (file.exists("Example_7_5_3_updated.dta")) {
  df <- tryCatch(read_dta("Example_7_5_3_updated.dta"), error = function(e) NULL)
}
if (is.null(df) && file.exists("Example_7_5_3.dta")) {
  df <- tryCatch(read_dta("Example_7_5_3.dta"), error = function(e) NULL)
}
if (is.null(df)) {
  cat("CATE.R: Cannot find Part B dataset. Skipping CATE section.\n")
  # In an interactive/sourced context, halt this script's execution here.
  # quit() is intentionally not called so this can be `source()`-d safely;
  # callers should check for the existence of `df` before proceeding.
  stop("CATE.R: Part B dataset not found.")
}

#-------------------------------------------------------------------------
# 1. Controls / instrument -- mirror MTE_MPRTE.R exactly
#-------------------------------------------------------------------------

x_controls <- c("female", "black", "hispanic", "asian", "age_ba", "firstgen",
                "parent_income_q", "parent_grad", "ugpa", "stem_major",
                "bus_major", "ed_major", "selective_inst", "public_ug",
                "state_unemp", "metro")
z_inst <- "ga_funding_adj"

x_rhs <- paste(x_controls, collapse = " + ")

#-------------------------------------------------------------------------
# Output directory fallback
# When run standalone (not via a master driver script), graphs_dir and
# tables_dir may not exist in the environment. Define them here if
# missing, mirroring the driver's own logic (personal paths for "marvi",
# relative paths otherwise).
#-------------------------------------------------------------------------

if (!exists("graphs_dir") || is.null(graphs_dir)) {
  if (Sys.info()[["user"]] == "marvi") {
    graphs_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/graphs"
    tables_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/tables"
  } else {
    graphs_dir <- "Output/graphs"
    tables_dir <- "Output/tables"
  }
  dir.create(graphs_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
  cat("CATE.R: output directories set to:\n")
  cat("  graphs:", graphs_dir, "\n")
  cat("  tables:", tables_dir, "\n")
}

#-------------------------------------------------------------------------
# Dedicated log for this sub-script
# Opens its own log file (sink), separate from the driver's master log
# and from MTE_MPRTE.R's log, so that Section 11.1.3 (CATE) output can
# be reviewed independently. Fallback logdir is defined here if not
# already set by the caller.
#-------------------------------------------------------------------------

if (!exists("logdir") || is.null(logdir)) {
  if (Sys.info()[["user"]] == "marvi") {
    logdir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11/Output/logs"
  } else {
    logdir <- "Output/logs"
  }
  dir.create(logdir, recursive = TRUE, showWarnings = FALSE)
}

cate_log_path <- file.path(logdir, "CATE_output.log")
cate_log_con  <- file(cate_log_path, open = "wt")
sink(cate_log_con, split = TRUE)        # split = TRUE echoes to console too
sink(cate_log_con, type = "message", append = TRUE)

cat("CATE.R log opened:", format(Sys.time()), "\n")
cat("Log file:", cate_log_path, "\n")

# Low-income subgroup: parent_income_q == 1 (lowest income quintile)
# This is the same policy-scenario margin used in the MPRTE analysis.

#=========================================================================
# STRATEGY (a): Subgroup IV/2SLS -- separate models for each cell
#
# For each subgroup, estimate:
#   ln_salary = b0 + b1*masters + controls + e
# where masters is instrumented by ga_funding_adj.
# Mirrors Section 11.1.2: ivreg(ln_salary ~ masters + X | ga_funding_adj + X)
#
# Subgroups:
#   1. Full sample      (LATE benchmark from Section 11.1.2)
#   2. STEM majors
#   3. Non-STEM majors
#   4. Business majors
#   5. First-generation students
#   6. Non-first-generation
#   7. Low parent income (parent_income_q == 1)
#   8. Higher parent income (parent_income_q > 1)
#=========================================================================

cat("\n--- Strategy (a): Subgroup IV/2SLS ---\n")

n_groups <- 8

# Matrix of results: rows = subgroups, cols = b, se, lo95, hi95, N
cate_results <- matrix(NA_real_, nrow = n_groups, ncol = 5)
rownames(cate_results) <- c("Full_sample", "STEM", "Non_STEM", "Business",
                             "First_gen", "Non_first_gen", "Low_income",
                             "Higher_income")
colnames(cate_results) <- c("b", "se", "lo95", "hi95", "N")

lbl <- c(
  "Full sample (LATE)",
  "STEM majors",
  "Non-STEM majors",
  "Business majors",
  "First-generation",
  "Non-first-generation",
  "Low parental income",
  "Higher parental income"
)

# Subgroup filter conditions, expressed as logical vectors evaluated
# against `df` (mirrors Stata's local cond1..cond8 string macros).
subgroup_filter <- function(row, data) {
  switch(row,
    `1` = rep(TRUE, nrow(data)),
    `2` = data$stem_major == 1,
    `3` = data$stem_major == 0,
    `4` = data$bus_major == 1,
    `5` = data$firstgen == 1,
    `6` = data$firstgen == 0,
    `7` = data$parent_income_q == 1,
    `8` = data$parent_income_q > 1
  )
}

iv_formula <- as.formula(
  paste0("ln_salary ~ masters + ", x_rhs, " | ", z_inst, " + ", x_rhs)
)

for (row in 1:n_groups) {

  cat("\n  Subgroup", row, ":", lbl[row], "\n")

  fit <- tryCatch({
    keep <- subgroup_filter(row, df)
    sub_data <- df[keep, ]
    m <- ivreg(iv_formula, data = sub_data)
    vc <- vcovHC(m, type = "HC1")          # robust SEs, mirrors vce(robust)
    b_iv  <- coef(m)["masters"]
    se_iv <- sqrt(vc["masters", "masters"])
    n_iv  <- nobs(m)
    list(b = b_iv, se = se_iv, n = n_iv)
  }, error = function(e) NULL)

  if (!is.null(fit)) {
    cate_results[row, "b"]    <- fit$b
    cate_results[row, "se"]   <- fit$se
    cate_results[row, "lo95"] <- fit$b - 1.96 * fit$se
    cate_results[row, "hi95"] <- fit$b + 1.96 * fit$se
    cate_results[row, "N"]    <- fit$n
    cat(sprintf("    b = %6.4f  SE = %6.4f  N = %d\n", fit$b, fit$se, fit$n))
  } else {
    cat("    (Subgroup", row, "skipped -- insufficient obs or first-stage failure)\n")
  }
}

cat("\nSubgroup CATE matrix:\n")
print(round(cate_results, 4))

#=========================================================================
# STRATEGY (b): Interaction IV -- test whether CATEs differ
#
# Full-sample IV model with three interaction terms added:
#   masters x stem_major
#   masters x (parent_income_q == 1)
#   masters x firstgen
#
# Each interaction is also instrumented by the corresponding
# ga_funding_adj interaction (Wooldridge 2010 heterogeneous-effects IV).
#=========================================================================

cat("\n--- Strategy (b): Interaction IV ---\n")

# Low-income indicator for interactions
df$lowinc <- as.integer(df$parent_income_q == 1)

# Generate treatment x subgroup interaction terms
df$D_x_stem_major <- df$masters * df$stem_major
df$D_x_lowinc      <- df$masters * df$lowinc
df$D_x_firstgen    <- df$masters * df$firstgen

df$Z_x_stem_major <- df$ga_funding_adj * df$stem_major
df$Z_x_lowinc      <- df$ga_funding_adj * df$lowinc
df$Z_x_firstgen    <- df$ga_funding_adj * df$firstgen

# Interaction IV: endogenous = masters + three interactions;
# instruments = ga_funding_adj + three Z x subgroup interactions.
iv_interact_formula <- as.formula(
  paste0(
    "ln_salary ~ masters + D_x_stem_major + D_x_lowinc + D_x_firstgen + ", x_rhs,
    " | ", z_inst, " + Z_x_stem_major + Z_x_lowinc + Z_x_firstgen + ", x_rhs
  )
)

iv_interact <- ivreg(iv_interact_formula, data = df)
vc_interact <- vcovHC(iv_interact, type = "HC1")
print(coeftest(iv_interact, vcov = vc_interact))

cat("\nInteraction IV -- key coefficients:\n")
cat("  Base CATE (non-STEM, higher-income, non-firstgen):\n")
b_masters       <- coef(iv_interact)["masters"]
b_stem_inter    <- coef(iv_interact)["D_x_stem_major"]
b_lowinc_inter  <- coef(iv_interact)["D_x_lowinc"]
b_firstgen_inter<- coef(iv_interact)["D_x_firstgen"]
cat(sprintf("    b = %6.4f\n", b_masters))
cat(sprintf("  STEM increment:       %6.4f\n", b_stem_inter))
cat(sprintf("  Low-income increment: %6.4f\n", b_lowinc_inter))
cat(sprintf("  First-gen increment:  %6.4f\n", b_firstgen_inter))

# Joint Wald test: are the interaction coefficients jointly zero?
# car::linearHypothesis defaults to an F-test for lm objects, but for
# ivreg objects with a supplied robust vcov it reports a Chi-squared
# test statistic, matching Stata's `test` after ivregress (chi2, not F,
# since no F-test degrees of freedom are stored after IV/GMM estimation).
wald_test <- linearHypothesis(
  iv_interact,
  c("D_x_stem_major = 0", "D_x_lowinc = 0", "D_x_firstgen = 0"),
  vcov. = vc_interact, test = "Chisq"
)
print(wald_test)

chi2_stat <- wald_test$Chisq[2]
chi2_df   <- wald_test$Df[2]
chi2_p    <- wald_test$`Pr(>Chisq)`[2]

cat(sprintf("\nJoint Wald test (all interactions = 0): chi2(%d) = %6.3f  p = %6.4f\n",
            chi2_df, chi2_stat, chi2_p))

# Implied CATEs for the four STEM x income cells
cate_base        <- b_masters
cate_stem         <- b_masters + b_stem_inter
cate_lowinc       <- b_masters + b_lowinc_inter
cate_stem_lowinc  <- b_masters + b_stem_inter + b_lowinc_inter

cat("\nImplied CATEs (log-points):\n")
cat(sprintf("  Non-STEM, Higher-income: %6.4f\n", cate_base))
cat(sprintf("  STEM, Higher-income:     %6.4f\n", cate_stem))
cat(sprintf("  Non-STEM, Low-income:    %6.4f\n", cate_lowinc))
cat(sprintf("  STEM, Low-income:        %6.4f\n", cate_stem_lowinc))

#=========================================================================
# STRATEGY (c): Visualization
#=========================================================================

# Springer monochrome ggplot theme, mirroring Stata's `set scheme s2mono`
theme_springer <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey85"),
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(hjust = 0),
      legend.background = element_blank(),
      legend.key = element_blank()
    )
}

#-------------------------------------------------------------------------
# Fig 11.7: Forest plot of subgroup CATEs from Strategy (a)
#-------------------------------------------------------------------------

cat("\n--- Producing CATE forest plot ---\n")

forest_df <- data.frame(
  j        = 1:n_groups,
  b        = cate_results[, "b"],
  lo       = cate_results[, "lo95"],
  hi       = cate_results[, "hi95"],
  is_late  = as.integer(1:n_groups == 1)
)

# Reverse j so full sample (row 1) plots at the top
forest_df$j2 <- n_groups + 1 - forest_df$j
forest_df$label <- factor(forest_df$j2,
                           levels = 1:n_groups,
                           labels = rev(lbl))

fig11_7 <- ggplot(forest_df, aes(x = b, y = label)) +
  geom_errorbarh(
    data = subset(forest_df, is_late == 0),
    aes(xmin = lo, xmax = hi), height = 0, color = "grey40", linewidth = 0.5
  ) +
  geom_point(
    data = subset(forest_df, is_late == 0),
    shape = 18, size = 3, color = "grey30"
  ) +
  geom_errorbarh(
    data = subset(forest_df, is_late == 1),
    aes(xmin = lo, xmax = hi), height = 0, color = "black", linewidth = 0.8
  ) +
  geom_point(
    data = subset(forest_df, is_late == 1),
    shape = 18, size = 4.5, color = "black"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "Conditional Average Treatment Effects\nReturn to Master's Degree by Subgroup",
    subtitle = "IV/2SLS; instrument: state GA funding; 95% CI",
    x = "IV Estimate (log-points)", y = NULL,
    caption = paste(
      "Diamond = point estimate. Bars = 95% CI.",
      "Full-sample LATE (black) shown as benchmark.",
      "Estimates based on synthetic data (illustrative).",
      sep = "\n"
    )
  ) +
  theme_springer()

ggsave(file.path(graphs_dir, "fig11_7_cate_forest.png"),
       fig11_7, width = 14, height = 14 * 0.68, units = "in", dpi = 100)

cat("  fig11_7_cate_forest exported.\n")

#-------------------------------------------------------------------------
# Fig 11.8: Interaction IV -- STEM x income 2x2 coefficient plot
#-------------------------------------------------------------------------

cat("\n--- Producing CATE interaction coefficient plot ---\n")

se_masters <- sqrt(vc_interact["masters", "masters"])
se_stem    <- sqrt(vc_interact["D_x_stem_major", "D_x_stem_major"])
se_lowinc  <- sqrt(vc_interact["D_x_lowinc", "D_x_lowinc"])

# Conservative delta-method SE: sqrt(sum of squared SEs)
interact_df <- data.frame(
  lbl      = c("Non-STEM, Higher-income", "STEM, Higher-income",
               "Non-STEM, Low-income", "STEM, Low-income"),
  b_cell   = c(cate_base, cate_stem, cate_lowinc, cate_stem_lowinc),
  se_cell  = c(
    se_masters,
    sqrt(se_masters^2 + se_stem^2),
    sqrt(se_masters^2 + se_lowinc^2),
    sqrt(se_masters^2 + se_stem^2 + se_lowinc^2)
  ),
  is_stem  = c(0, 1, 0, 1),
  j        = 1:4
)
interact_df$lo95 <- interact_df$b_cell - 1.96 * interact_df$se_cell
interact_df$hi95 <- interact_df$b_cell + 1.96 * interact_df$se_cell

# y-axis ordering to match Stata's ylabel(1 "STEM, Low-income" 2 "Non-STEM,
# Low-income" 3 "STEM, Higher-income" 4 "Non-STEM, Higher-income")
interact_df$y_pos <- c(4, 3, 2, 1)   # row order: base, stem, lowinc, stem_lowinc
y_labels <- c("STEM, Low-income", "Non-STEM, Low-income",
              "STEM, Higher-income", "Non-STEM, Higher-income")

fig11_8 <- ggplot(interact_df, aes(x = b_cell, y = y_pos)) +
  geom_errorbarh(aes(xmin = lo95, xmax = hi95), height = 0,
                 color = "grey50", linewidth = 0.5) +
  geom_point(aes(shape = factor(is_stem)), size = 4, color = "black") +
  scale_shape_manual(values = c(`0` = 16, `1` = 18),
                      labels = c("Non-STEM", "STEM"), name = NULL) +
  scale_y_continuous(breaks = 1:4, labels = y_labels) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "CATE: STEM \u00d7 Income Interaction",
    subtitle = "Interaction IV/2SLS; instrument: state GA funding",
    x = "IV Estimate (log-points)", y = NULL,
    caption = paste(
      "Circle = Non-STEM. Diamond = STEM.",
      "Error bars = approx. 95% CI (conservative delta method).",
      "Estimates based on synthetic data (illustrative).",
      sep = "\n"
    )
  ) +
  theme_springer() +
  theme(legend.position = "bottom")

ggsave(file.path(graphs_dir, "fig11_8_cate_interact.png"),
       fig11_8, width = 14, height = 14 * 0.68, units = "in", dpi = 100)

cat("  fig11_8_cate_interact exported.\n")

#=========================================================================
# COMPARISON TABLE: OLS / LATE / Subgroup CATEs
#=========================================================================

cat("\n--- Producing CATE comparison table ---\n")

ols_formula <- as.formula(paste0("ln_salary ~ masters + ", x_rhs))

# OLS benchmark (mirrors Section 11.1.2)
cate_ols <- lm(ols_formula, data = df)
vc_ols   <- vcovHC(cate_ols, type = "HC1")

# Full-sample LATE (mirrors Section 11.1.2)
cate_late <- ivreg(as.formula(paste0("ln_salary ~ masters + ", x_rhs,
                                      " | ", z_inst, " + ", x_rhs)), data = df)
vc_late   <- vcovHC(cate_late, type = "HC1")

# CATE: STEM majors
cate_stem_mod <- ivreg(iv_formula, data = df[df$stem_major == 1, ])
vc_stem_mod   <- vcovHC(cate_stem_mod, type = "HC1")

# CATE: First-generation
cate_firstgen_mod <- ivreg(iv_formula, data = df[df$firstgen == 1, ])
vc_firstgen_mod   <- vcovHC(cate_firstgen_mod, type = "HC1")

# CATE: Low parental income
cate_lowinc_mod <- ivreg(iv_formula, data = df[df$parent_income_q == 1, ])
vc_lowinc_mod   <- vcovHC(cate_lowinc_mod, type = "HC1")

# Helper: extract masters coefficient, SE, stars, N, R^2 from a fitted model
extract_row <- function(model, vc) {
  b   <- coef(model)["masters"]
  se  <- sqrt(vc["masters", "masters"])
  tstat <- b / se
  # two-sided p-value using a normal approximation (robust Wald), matching
  # the asymptotic inference used by ivregress/estout in Stata
  pval <- 2 * pnorm(-abs(tstat))
  stars <- dplyr::case_when(
    pval < 0.01 ~ "***",
    pval < 0.05 ~ "**",
    pval < 0.10 ~ "*",
    TRUE ~ ""
  )
  n <- nobs(model)
  r2 <- tryCatch(summary(model)$r.squared, error = function(e) NA_real_)
  list(b = b, se = se, stars = stars, n = n, r2 = r2)
}

rows <- list(
  OLS              = extract_row(cate_ols, vc_ols),
  `IV/LATE`        = extract_row(cate_late, vc_late),
  `CATE: STEM`     = extract_row(cate_stem_mod, vc_stem_mod),
  `CATE: First-gen`= extract_row(cate_firstgen_mod, vc_firstgen_mod),
  `CATE: Low-income`= extract_row(cate_lowinc_mod, vc_lowinc_mod)
)

# Build a simple fixed-width text table and write as RTF (mirrors
# Stata's estout ..., style(fixed) ... using "tab11_2_cate_subgroup.rtf")
col_names <- names(rows)
b_row    <- sprintf("%.4f%s", sapply(rows, `[[`, "b"), sapply(rows, `[[`, "stars"))
se_row   <- sprintf("(%.4f)", sapply(rows, `[[`, "se"))
n_row    <- sprintf("%d", sapply(rows, `[[`, "n"))
r2_row   <- sprintf("%.3f", sapply(rows, `[[`, "r2"))

rtf_lines <- c(
  "{\\rtf1\\ansi",
  "\\b Table 11.2. OLS, LATE, and Conditional Average Treatment Effects \\b0\\par",
  "\\par",
  paste0("\\trowd ", paste(col_names, collapse = " \\cell "), " \\cell\\row"),
  paste0("masters \\tab ", paste(b_row, collapse = " \\tab "), " \\par"),
  paste0("        \\tab ", paste(se_row, collapse = " \\tab "), " \\par"),
  paste0("N       \\tab ", paste(n_row, collapse = " \\tab "), " \\par"),
  paste0("R-squared \\tab ", paste(r2_row, collapse = " \\tab "), " \\par"),
  "\\par",
  "\\fs16 Outcome: log annual salary. Instrument: state-funded GA amount (ga_funding_adj). Robust SEs in parentheses. CATE columns restrict the sample to the indicated subgroup. Estimates based on synthetic data (illustrative only).\\fs20\\par",
  "}"
)
writeLines(rtf_lines, file.path(tables_dir, "tab11_2_cate_subgroup.rtf"))

# Also print a console-friendly version of the same table
cat("\nTable 11.2. OLS, LATE, and Conditional Average Treatment Effects\n")
tbl_print <- data.frame(
  Statistic = c("b", "se", "N", "R-squared"),
  rbind(b_row, se_row, n_row, r2_row)
)
colnames(tbl_print) <- c("Statistic", col_names)
print(tbl_print, row.names = FALSE)

cat("  tab11_2_cate_subgroup.rtf exported.\n")

#=========================================================================
# Display figures
#=========================================================================

print(fig11_7)
print(fig11_8)

cat("\n==========================================================\n")
cat(" Section 11.1.3 (CATE.R) complete.\n")
cat(" Outputs:\n")
cat("  ", file.path(graphs_dir, "fig11_7_cate_forest.png"), "\n")
cat("  ", file.path(graphs_dir, "fig11_8_cate_interact.png"), "\n")
cat("  ", file.path(tables_dir, "tab11_2_cate_subgroup.rtf"), "\n")
cat("==========================================================\n")

#-------------------------------------------------------------------------
# Close this sub-script's dedicated log
# (The driver script's own master log, if any, remains open.)
#-------------------------------------------------------------------------
cat("\nCATE.R log closed:", format(Sys.time()), "\n")
sink(type = "message")
sink()
close(cate_log_con)

#=========================================================================
# END OF CATE_R.R
#=========================================================================
