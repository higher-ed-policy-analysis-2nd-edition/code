# ============================================================================
# CATE.R  —  Section 10.10.3: Conditional Average Treatment Effects
# R translation of CATE.do
# Higher Education Policy Analysis Using Quantitative Techniques (2nd ed.)
# Author: Marvin A. Titus
# Date: June 2026
# NOTE: Code development was assisted by Claude (Anthropic). The author
#       provided specifications and reviewed, tested, and validated all code.
# ============================================================================
#
# PURPOSE
# -------
# Estimate Conditional Average Treatment Effects (CATEs) for the return
# to master's degree completion using the same instrument (state-funded
# graduate assistantship funding, ga_funding_adj) and data as
# Sections 10.10–10.11.
#
# A CATE conditions on observed covariates X:
#
#   CATE(x) = E[Y(1) - Y(0) | X = x]
#
# Unlike the LATE — which averages over all compliers near the instrument
# margin — a CATE asks: *for whom* does the treatment effect differ by
# observed characteristics (field of study, family income, first-gen status)?
#
# Three complementary strategies are implemented:
#   (a) Subgroup IV/2SLS  — run the baseline IV model separately for each
#       subgroup cell and collect point estimates + SEs.
#   (b) Interaction IV    — include subgroup × masters interaction terms
#       in the full-sample model and test for differential effects.
#   (c) Forest plot and comparison table.
#
# VARIABLE NAMES (match MTE_MPRTE.R exactly)
# ------------------------------------------
#   Outcome:    ln_salary
#   Treatment:  masters          (Completed Master's Degree, 1=Yes)
#   Instrument: ga_funding_adj   (state GA funding, $1,000s)
#   Controls:   X_controls (see global below)
#
# OUTPUTS
# -------
#   fig10_cate_forest.png   — forest plot of CATE by subgroup
#   fig10_cate_interact.png — interaction IV coefficient plot
#   tab10_cate_subgroup.rtf — comparison table (OLS / LATE / CATE)
#
# INHERITS (from R_code10.R or run standalone)
#   graphs_dir, tables_dir
#
# DATA
#   Example_7_5_3_updated.dta  (or Example_7_5_3.dta as fallback)
# ============================================================================

# ----------------------------------------------------------------------------
# 0. Packages
# ----------------------------------------------------------------------------

library(haven)      # read_dta()
library(ivreg)      # ivreg() — IV/2SLS with robust SEs
library(lmtest)     # coeftest()
library(sandwich)   # vcovHC() — heteroskedasticity-robust SEs
library(car)        # linearHypothesis() — joint Wald test
library(ggplot2)    # forest plots
library(flextable)    # comparison table (no pandoc dependency)

cat("\n==========================================================\n")
cat(" Section 10.10.3: Conditional Average Treatment Effects\n")
cat("==========================================================\n")

# ----------------------------------------------------------------------------
# 1. Controls vector — mirrors MTE_MPRTE.R exactly
# ----------------------------------------------------------------------------

X_controls <- c("female", "black", "hispanic", "asian", "age_ba", "firstgen",
                 "parent_income_q", "parent_grad", "ugpa", "stem_major",
                 "bus_major", "ed_major", "selective_inst", "public_ug",
                 "state_unemp", "metro")

# ----------------------------------------------------------------------------
# 2. Output directory fallback
#    When run standalone, graphs_dir / tables_dir may not exist in the
#    environment. Define them here using the same logic as R_code10.R.
# ----------------------------------------------------------------------------

if (!exists("graphs_dir")) {
  if (Sys.info()[["user"]] == "marvi") {
    graphs_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/graphs"
    tables_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/tables"
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

# ----------------------------------------------------------------------------
# 3. Load dataset — mirrors Stata's capture / fallback logic
# ----------------------------------------------------------------------------

df <- tryCatch(
  read_dta("Example_7_5_3_updated.dta"),
  error = function(e) {
    tryCatch(
      read_dta("Example_7_5_3.dta"),
      error = function(e2) {
        message("CATE.R: Cannot find Part B dataset. Skipping CATE section.")
        return(NULL)
      }
    )
  }
)

if (is.null(df)) stop("Dataset not found — exiting CATE.R.")

# Ensure logical/numeric types are consistent
df$masters          <- as.numeric(df$masters)
df$ga_funding_adj   <- as.numeric(df$ga_funding_adj)
df$ln_salary        <- as.numeric(df$ln_salary)
df$parent_income_q  <- as.numeric(df$parent_income_q)
df$stem_major       <- as.numeric(df$stem_major)
df$bus_major        <- as.numeric(df$bus_major)
df$firstgen         <- as.numeric(df$firstgen)

# ============================================================================
# STRATEGY (a): Subgroup IV/2SLS — separate models for each cell
#
# For each subgroup, estimate:
#   ln_salary ~ masters + controls  |  instrument: ga_funding_adj
# Syntax mirrors Section 10.10.2: ivreg() with HC1 robust SEs.
# ============================================================================

cat("\n--- Strategy (a): Subgroup IV/2SLS ---\n")

# Build the IV formula once; the data subset handles the subgroup restriction.
iv_formula <- as.formula(
  paste("ln_salary ~ ", paste(X_controls, collapse = " + "),
        "+ masters |",
        paste(X_controls, collapse = " + "),
        "+ ga_funding_adj")
)

# Subgroup definitions: label and logical filter expression
subgroups <- list(
  list(label = "Full sample (LATE)",    cond = rep(TRUE,  nrow(df))),
  list(label = "STEM majors",           cond = df$stem_major == 1),
  list(label = "Non-STEM majors",       cond = df$stem_major == 0),
  list(label = "Business majors",       cond = df$bus_major  == 1),
  list(label = "First-generation",      cond = df$firstgen   == 1),
  list(label = "Non-first-generation",  cond = df$firstgen   == 0),
  list(label = "Low parental income",   cond = df$parent_income_q == 1),
  list(label = "Higher parental income",cond = df$parent_income_q >  1)
)

n_groups     <- length(subgroups)
CATE_results <- data.frame(
  label = character(n_groups),
  b     = NA_real_,
  se    = NA_real_,
  lo95  = NA_real_,
  hi95  = NA_real_,
  N     = NA_integer_,
  stringsAsFactors = FALSE
)

for (i in seq_len(n_groups)) {
  sg    <- subgroups[[i]]
  df_sg <- df[sg$cond & is.finite(df$ln_salary) & is.finite(df$masters) &
                is.finite(df$ga_funding_adj), ]

  cat(sprintf("\n  Subgroup %d: %s\n", i, sg$label))

  fit <- tryCatch(
    ivreg(iv_formula, data = df_sg),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    cat("    (Skipped — insufficient obs or first-stage failure)\n")
    CATE_results$label[i] <- sg$label
    next
  }

  ct  <- coeftest(fit, vcov = vcovHC(fit, type = "HC1"))
  b   <- ct["masters", "Estimate"]
  se  <- ct["masters", "Std. Error"]

  CATE_results$label[i] <- sg$label
  CATE_results$b[i]     <- b
  CATE_results$se[i]    <- se
  CATE_results$lo95[i]  <- b - 1.96 * se
  CATE_results$hi95[i]  <- b + 1.96 * se
  CATE_results$N[i]     <- nrow(df_sg)

  cat(sprintf("    b = %.4f  SE = %.4f  N = %d\n", b, se, nrow(df_sg)))
}

cat("\nSubgroup CATE matrix:\n")
print(CATE_results, digits = 4, row.names = FALSE)

# ============================================================================
# STRATEGY (b): Interaction IV — test whether CATEs differ
#
# Full-sample IV model with three interaction terms:
#   masters × stem_major
#   masters × lowinc  (parent_income_q == 1)
#   masters × firstgen
#
# Each interaction is instrumented by the corresponding ga_funding_adj
# interaction (Wooldridge 2010 heterogeneous-effects IV).
# ============================================================================

cat("\n--- Strategy (b): Interaction IV ---\n")

# Low-income indicator
df$lowinc <- as.numeric(df$parent_income_q == 1)

# Treatment × subgroup interactions
df$D_x_stem_major <- df$masters * df$stem_major
df$D_x_lowinc     <- df$masters * df$lowinc
df$D_x_firstgen   <- df$masters * df$firstgen

# Instrument × subgroup interactions
df$Z_x_stem_major <- df$ga_funding_adj * df$stem_major
df$Z_x_lowinc     <- df$ga_funding_adj * df$lowinc
df$Z_x_firstgen   <- df$ga_funding_adj * df$firstgen

# Interaction IV formula:
#   endogenous regressors: masters + three D_x interactions
#   instruments:           ga_funding_adj + three Z_x interactions
iv_interact_formula <- as.formula(
  paste(
    "ln_salary ~",
    paste(X_controls, collapse = " + "),
    "+ masters + D_x_stem_major + D_x_lowinc + D_x_firstgen |",
    paste(X_controls, collapse = " + "),
    "+ ga_funding_adj + Z_x_stem_major + Z_x_lowinc + Z_x_firstgen"
  )
)

fit_interact <- ivreg(iv_interact_formula, data = df)
ct_interact  <- coeftest(fit_interact, vcov = vcovHC(fit_interact, type = "HC1"))

cat("\nInteraction IV — key coefficients:\n")
cat("  Base CATE (non-STEM, higher-income, non-firstgen):\n")
cat(sprintf("    b = %.4f\n", ct_interact["masters",       "Estimate"]))
cat(sprintf("  STEM increment:       %.4f\n", ct_interact["D_x_stem_major", "Estimate"]))
cat(sprintf("  Low-income increment: %.4f\n", ct_interact["D_x_lowinc",     "Estimate"]))
cat(sprintf("  First-gen increment:  %.4f\n", ct_interact["D_x_firstgen",   "Estimate"]))

# Joint Wald test: are all three interaction coefficients jointly zero?
# linearHypothesis() with the robust vcov replicates Stata's -test- post ivreg.
wald_test <- linearHypothesis(
  fit_interact,
  c("D_x_stem_major = 0", "D_x_lowinc = 0", "D_x_firstgen = 0"),
  vcov. = vcovHC(fit_interact, type = "HC1")
)
print(wald_test)
chi2_col <- grep("Chisq|Chi|F$",  colnames(wald_test), value = TRUE)[1]
pval_col <- grep("Pr\\(",         colnames(wald_test), value = TRUE)[1]
chi2_val <- wald_test[2, chi2_col]
p_val    <- wald_test[2, pval_col]
cat(sprintf("\nJoint Wald test (all interactions = 0): Chi2(3) = %.3f  p = %.4f\n",
            chi2_val, p_val))

# Implied CATEs for the four STEM × income cells
b_masters       <- ct_interact["masters",       "Estimate"]
b_stem          <- ct_interact["D_x_stem_major", "Estimate"]
b_lowinc        <- ct_interact["D_x_lowinc",     "Estimate"]
se_masters      <- ct_interact["masters",       "Std. Error"]
se_stem         <- ct_interact["D_x_stem_major", "Std. Error"]
se_lowinc_coef  <- ct_interact["D_x_lowinc",     "Std. Error"]

cate_base        <- b_masters
cate_stem        <- b_masters + b_stem
cate_lowinc      <- b_masters + b_lowinc
cate_stem_lowinc <- b_masters + b_stem + b_lowinc

cat("\nImplied CATEs (log-points):\n")
cat(sprintf("  Non-STEM, Higher-income: %.4f\n", cate_base))
cat(sprintf("  STEM, Higher-income:     %.4f\n", cate_stem))
cat(sprintf("  Non-STEM, Low-income:    %.4f\n", cate_lowinc))
cat(sprintf("  STEM, Low-income:        %.4f\n", cate_stem_lowinc))

# ============================================================================
# STRATEGY (c): Visualization
# ============================================================================

# ----------------------------------------------------------------------------
# Fig 10.CATE(a): Forest plot of subgroup CATEs from Strategy (a)
# ----------------------------------------------------------------------------

cat("\n--- Producing CATE forest plot ---\n")

# Reverse row order so full sample plots at top (ggplot reads bottom-to-top)
forest_df <- CATE_results
forest_df$label   <- factor(forest_df$label,
                             levels = rev(forest_df$label))
forest_df$is_late <- forest_df$label == "Full sample (LATE)"

p_forest <- ggplot(forest_df, aes(x = b, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  geom_errorbarh(aes(xmin = lo95, xmax = hi95,
                     linewidth = is_late, color = is_late),
                 height = 0.25) +
  geom_point(aes(size = is_late, color = is_late), shape = 18) +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "grey40"),
                     guide = "none") +
  scale_size_manual(values  = c("TRUE" = 4, "FALSE" = 3),  guide = "none") +
  scale_linewidth_manual(values = c("TRUE" = 0.8, "FALSE" = 0.4), guide = "none") +
  labs(
    title    = "Conditional Average Treatment Effects\nReturn to Master's Degree by Subgroup",
    subtitle = "IV/2SLS; instrument: state GA funding; 95% CI",
    x        = "IV Estimate (log-points)",
    y        = NULL,
    caption  = paste("Diamond = point estimate. Bars = 95% CI.",
                     "Full-sample LATE (black) shown as benchmark.",
                     "Estimates based on synthetic data (illustrative).",
                     sep = "\n")
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    plot.caption  = element_text(hjust = 0, size = 7),
    panel.grid.major.y = element_blank()
  )

ggsave(file.path(graphs_dir, "fig10_cate_forest.png"),
       plot = p_forest, width = 7, height = 5, dpi = 200)
print(p_forest)
cat("  fig10_cate_forest exported.\n")

# ----------------------------------------------------------------------------
# Fig 10.CATE(b): Interaction IV — STEM × income 2×2 coefficient plot
# ----------------------------------------------------------------------------

cat("\n--- Producing CATE interaction coefficient plot ---\n")

# Conservative delta-method SEs: sqrt(sum of squared component SEs)
interact_df <- data.frame(
  label   = c("Non-STEM, Higher-income", "STEM, Higher-income",
              "Non-STEM, Low-income",   "STEM, Low-income"),
  b_cell  = c(cate_base, cate_stem, cate_lowinc, cate_stem_lowinc),
  se_cell = c(
    se_masters,
    sqrt(se_masters^2 + se_stem^2),
    sqrt(se_masters^2 + se_lowinc_coef^2),
    sqrt(se_masters^2 + se_stem^2 + se_lowinc_coef^2)
  ),
  is_stem = c(FALSE, TRUE, FALSE, TRUE),
  stringsAsFactors = FALSE
)
interact_df$lo95  <- interact_df$b_cell - 1.96 * interact_df$se_cell
interact_df$hi95  <- interact_df$b_cell + 1.96 * interact_df$se_cell
interact_df$label <- factor(interact_df$label, levels = rev(interact_df$label))

p_interact <- ggplot(interact_df, aes(x = b_cell, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                 height = 0.2, color = "grey50", linewidth = 0.4) +
  geom_point(aes(shape = is_stem), color = "black", size = 4) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 18),
                     labels = c("FALSE" = "Non-STEM", "TRUE" = "STEM"),
                     name   = NULL) +
  labs(
    title    = "CATE: STEM \u00d7 Income Interaction",
    subtitle = "Interaction IV/2SLS; instrument: state GA funding",
    x        = "IV Estimate (log-points)",
    y        = NULL,
    caption  = paste("Circle = Non-STEM. Diamond = STEM.",
                     "Error bars = approx. 95% CI (conservative delta method).",
                     "Estimates based on synthetic data (illustrative).",
                     sep = "\n")
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    plot.caption  = element_text(hjust = 0, size = 7),
    panel.grid.major.y = element_blank(),
    legend.position    = "bottom"
  )

ggsave(file.path(graphs_dir, "fig10_cate_interact.png"),
       plot = p_interact, width = 7, height = 4, dpi = 200)
print(p_interact)
cat("  fig10_cate_interact exported.\n")

# ============================================================================
# COMPARISON TABLE: OLS / LATE / Subgroup CATEs
# ============================================================================

cat("\n--- Producing CATE comparison table ---\n")

ols_formula <- as.formula(
  paste("ln_salary ~ masters +", paste(X_controls, collapse = " + "))
)
late_formula <- as.formula(
  paste("ln_salary ~", paste(X_controls, collapse = " + "),
        "+ masters |", paste(X_controls, collapse = " + "),
        "+ ga_funding_adj")
)

fit_ols      <- lm(ols_formula,  data = df)
fit_late     <- ivreg(late_formula, data = df)
fit_stem     <- ivreg(late_formula, data = df[df$stem_major == 1, ])
fit_firstgen <- ivreg(late_formula, data = df[df$firstgen   == 1, ])
fit_lowinc   <- ivreg(late_formula, data = df[df$parent_income_q == 1, ])

# Robust vcov for each model
vcov_ols      <- vcovHC(fit_ols,      type = "HC1")
vcov_late     <- vcovHC(fit_late,     type = "HC1")
vcov_stem     <- vcovHC(fit_stem,     type = "HC1")
vcov_firstgen <- vcovHC(fit_firstgen, type = "HC1")
vcov_lowinc   <- vcovHC(fit_lowinc,   type = "HC1")

# Helper: extract b, SE, stars, N, R² for one model given its robust vcov
extract_row <- function(fit, vc) {
  ct  <- coeftest(fit, vcov = vc)
  b   <- ct["masters", "Estimate"]
  se  <- ct["masters", "Std. Error"]
  pv_col <- intersect(c("Pr(>|z|)", "Pr(>|t|)"), colnames(ct))[1]
  pv     <- ct["masters", pv_col]
  stars <- dplyr::case_when(
    pv < 0.01 ~ "***",
    pv < 0.05 ~ "**",
    pv < 0.10 ~ "*",
    TRUE      ~ ""
  )
  n   <- nobs(fit)
  r2  <- if (inherits(fit, "ivreg")) summary(fit)$r.squared else summary(fit)$r.squared
  list(
    coef = sprintf("%.4f%s", b, stars),
    se   = sprintf("(%.4f)", se),
    N    = as.character(n),
    R2   = sprintf("%.3f", r2)
  )
}

model_list <- list(
  list(fit = fit_ols,      vc = vcov_ols,      col = "OLS"),
  list(fit = fit_late,     vc = vcov_late,      col = "IV/LATE"),
  list(fit = fit_stem,     vc = vcov_stem,      col = "CATE: STEM"),
  list(fit = fit_firstgen, vc = vcov_firstgen,  col = "CATE: First-gen"),
  list(fit = fit_lowinc,   vc = vcov_lowinc,    col = "CATE: Low-income")
)

rows_extracted <- lapply(model_list, function(m) extract_row(m$fit, m$vc))
col_names      <- sapply(model_list, `[[`, "col")

tab_df <- data.frame(
  Statistic = c("masters", "", "N", "R-squared"),
  matrix(
    c(sapply(rows_extracted, `[[`, "coef"),
      sapply(rows_extracted, `[[`, "se"),
      sapply(rows_extracted, `[[`, "N"),
      sapply(rows_extracted, `[[`, "R2")),
    nrow = 4, byrow = TRUE
  ),
  stringsAsFactors = FALSE
)
names(tab_df) <- c("", col_names)

# flextable requires syntactically valid column names internally;
# use safe names for the data frame, then relabel for display.
display_names <- c("", col_names)
safe_names    <- paste0("col", seq_along(display_names))
names(tab_df) <- safe_names

ft <- flextable(tab_df, col_keys = safe_names) |>
  set_header_labels(values = setNames(display_names, safe_names)) |>
  set_caption(
    caption = "Table 10.CATE. OLS, LATE, and Conditional Average Treatment Effects"
  ) |>
  add_footer_lines(
    values = c(
      "Outcome: log annual salary.",
      "Instrument: state-funded GA amount (ga_funding_adj).",
      "Robust SEs (HC1) in parentheses. * p<0.10, ** p<0.05, *** p<0.01.",
      "CATE columns restrict the sample to the indicated subgroup.",
      "Estimates based on synthetic data (illustrative only)."
    )
  ) |>
  autofit()

save_as_docx(ft, path = file.path(tables_dir, "tab10_cate_subgroup.docx"))
cat("  tab10_cate_subgroup.docx exported.\n")

# ============================================================================
# Clean up temporary interaction columns
# ============================================================================

df$lowinc         <- NULL
df$D_x_stem_major <- NULL
df$D_x_lowinc     <- NULL
df$D_x_firstgen   <- NULL
df$Z_x_stem_major <- NULL
df$Z_x_lowinc     <- NULL
df$Z_x_firstgen   <- NULL

cat("\n==========================================================\n")
cat(" Section 10.10.3 (CATE.R) complete.\n")
cat(" Outputs:\n")
cat("  ", file.path(graphs_dir, "fig10_cate_forest.png"),  "\n")
cat("  ", file.path(graphs_dir, "fig10_cate_interact.png"),"\n")
cat("  ", file.path(tables_dir, "tab10_cate_subgroup.docx"),"\n")
cat("==========================================================\n")

# ============================================================================
# END OF CATE.R
# ============================================================================
