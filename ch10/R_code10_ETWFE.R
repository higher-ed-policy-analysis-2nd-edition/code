# ========================================================================
# R_code10_ETWFE.R
# Section 10.7.4: Extended Two-Way Fixed Effects (ETWFE)
#   via the etwfe package (Wooldridge 2021/2023 estimator)
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch10
# Author: Marvin A. Titus
# Date: June 2026
# NOTE: Code development was assisted by Claude (Anthropic). The author
#       provided specifications and reviewed, tested, and validated all code.
# ========================================================================
# Called by R_code10.R AFTER Georgia_DiD.R.
# Standalone: can also be sourced directly.
#
# PURPOSE
#   Demonstrate the Wooldridge (2021, 2023) Extended TWFE estimator on the
#   three-state staggered adoption design (48-state panel, FY 2001-2024).
#   ETWFE recovers heterogeneity-robust ATT(g,t) that align with the
#   Callaway-Sant'Anna (2021) estimates from Section 10.7.
#
#   Treated states (gyear = treatment cohort):
#     Georgia      (FIPS 13)  -> 2013
#     Wisconsin    (FIPS 55)  -> 2018
#     Pennsylvania (FIPS 42)  -> 2022
#   Never-treated comparison units: remaining states (gyear = 0).
#
#   Outcome:  lngenop  (log general public higher education operations)
#   Controls: lntotsup lnfinaid lntuifee lnfte
#
# SPECIFICATION NOTE
#   With only THREE treated cohorts and FOUR time-varying covariates, the
#   fully saturated ETWFE (covariates interacted with every cohort x year
#   cell) is rank-deficient. We therefore follow the Stata script and run:
#     (a) a no-covariate specification as a design check;
#     (b) a covariate-adjusted specification — the preferred model.
#   In the etwfe R package, covariate partialling is handled automatically
#   via the xvar argument, which is equivalent to jwdid's hettype(cohort).
#
# Required packages:
#   etwfe    — Wooldridge ETWFE estimator (mirrors Stata jwdid)
#   marginaleffects — emfx() aggregations (simple / group / event)
#   fixest   — underlying FE engine
#   sandwich — vcovHC() robust SEs
#   dplyr, ggplot2 — wrangling and plots
#
# Install once:
#   install.packages(c("etwfe", "marginaleffects", "fixest", "sandwich",
#                      "dplyr", "ggplot2"))

# ========================================================================

# -----------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------
# etwfe requires fixest >= 0.13.2. If the namespace conflict error
# "fixest X.Y.Z is already loaded, but >= 0.13.2 is required" appears,
# run the following once in a FRESH R session (no fixest loaded yet):
#   install.packages("fixest")      # updates to current CRAN version
#   install.packages("etwfe")       # then re-install etwfe
# Then restart R and source this script again.

# Enforce version requirement before loading anything else.
local({
  fx_ver <- tryCatch(packageVersion("fixest"), error = function(e) NULL)
  if (is.null(fx_ver) || fx_ver < "0.13.2") {
    stop(
      "fixest >= 0.13.2 is required by etwfe.\n",
      "  Currently installed: ", if (is.null(fx_ver)) "not found" else fx_ver, "\n",
      "  Fix: run  install.packages(\"fixest\")  in a fresh R session,\n",
      "  then restart R and source this script again."
    )
  }
})

suppressPackageStartupMessages({
  library(fixest)           # load fixest FIRST to claim the namespace
  library(etwfe)
  library(marginaleffects)
  library(dplyr)
  library(ggplot2)
  library(sandwich)         # vcovHC() -- robust SEs
                             
})

# -----------------------------------------------------------------------
# Fallback output paths (overridden when sourced from R_code10.R)
# -----------------------------------------------------------------------
if (!exists("graphs_dir")) {
  graphs_dir <- "Output/graphs"
  dir.create("Output",         showWarnings = FALSE, recursive = TRUE)
  dir.create("Output/graphs",  showWarnings = FALSE, recursive = TRUE)
}
if (!exists("tables_dir")) {
  tables_dir <- "Output/tables"
  dir.create("Output/tables",  showWarnings = FALSE, recursive = TRUE)
}

# Springer B&W ggplot2 theme (mirrors Stata s2mono)
theme_springer <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(colour = "grey85", linewidth = 0.3),
      plot.title        = element_text(size = base_size,     face = "bold"),
      plot.subtitle     = element_text(size = base_size - 1, colour = "grey30"),
      plot.caption      = element_text(size = base_size - 2, colour = "grey40",
                                       hjust = 0),
      axis.title        = element_text(size = base_size - 1),
      legend.position   = "none"
    )
}

gh_raw <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch10"

# ========================================================================
# 1. IMPORT EXPANDED 48-STATE PANEL
# ========================================================================
cat("\n=== Importing 48-state panel (Example_10_7_3.csv) ===\n")

df_raw <- tryCatch(
  read.csv(paste0(gh_raw, "/Example_10_7_3.csv")),
  error = function(e) {
    if (file.exists("Example_10_7_3.csv")) {
      read.csv("Example_10_7_3.csv")
    } else {
      stop("Cannot load Example_10_7_3.csv — check network or working directory.")
    }
  }
)
write.csv(df_raw, "Example_10_7_3.csv", row.names = FALSE)

# Normalise column names: lowercase, collapse dots/underscores/spaces to "_"
names(df_raw) <- tolower(gsub("[._\\s]+", "_", trimws(names(df_raw)),
                              perl = TRUE))
names(df_raw) <- gsub("^_|_$", "", names(df_raw))
names(df_raw)[1] <- gsub("^\\W+", "", names(df_raw)[1])   # strip BOM

cat(sprintf("Loaded: %d rows x %d columns\n", nrow(df_raw), ncol(df_raw)))
cat(sprintf("Columns: %s\n", paste(names(df_raw), collapse = ", ")))

# ========================================================================
# 2. CONSTRUCT LOGGED OUTCOME AND CONTROLS
# ========================================================================

# Resolve the raw column names for the financial variables.
# The CSV may use camelCase or underscore_separated depending on version.
resolve_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) stop(sprintf(
    "None of these columns found: %s\nActual columns: %s",
    paste(candidates, collapse = ", "),
    paste(names(df), collapse = ", ")
  ))
  hit[1]
}

col_genop  <- resolve_col(df_raw, c("generalpublicoperations",
                                     "general_public_operations"))
col_totsup <- resolve_col(df_raw, c("totalstatesupport",
                                     "total_state_support"))
col_finaid <- resolve_col(df_raw, c("totalfinancialaid",
                                     "total_financial_aid"))
col_tuifee <- resolve_col(df_raw, c("nettuitionandfeerevenue",
                                     "net_tuition_and_fee_revenue"))
col_fte    <- resolve_col(df_raw, c("netfteenrollment",
                                     "net_fte_enrollment"))

df <- df_raw %>%
  mutate(
    lngenop  = log(.data[[col_genop]]),
    lntotsup = log(.data[[col_totsup]]),
    lnfinaid = log(.data[[col_finaid]]),
    lntuifee = log(.data[[col_tuifee]]),
    lnfte    = log(.data[[col_fte]])
  ) %>%
  # is.finite() (not complete.cases) is required here: log(0) = -Inf and
  # log(negative) = NaN. complete.cases() catches NaN but NOT -Inf, so a
  # zero-valued financial figure (e.g. TotalFinancialAid = 0) would slip
  # through and later be dropped by fixest/etwfe, causing a row-count
  # mismatch in emfx(). Filtering on is.finite() removes all such rows up front.
  filter(if_all(c(lngenop, lntotsup, lnfinaid, lntuifee, lnfte), is.finite))

controls <- c("lntotsup", "lnfinaid", "lntuifee", "lnfte")
cat(sprintf("After dropping missing log-values: %d rows\n", nrow(df)))

# ========================================================================
# 3. STAGGERED TREATMENT-COHORT VARIABLE
#   gyear = first treatment period; 0 for never-treated.
# ========================================================================

# Resolve FIPS and fiscal-year column names
col_fips <- resolve_col(df, c("fips", "state_id", "stateid"))
col_fy   <- resolve_col(df, c("fy", "year", "fiscal_year"))

df <- df %>%
  rename(fips = all_of(col_fips),
         fy   = all_of(col_fy)) %>%
  mutate(
    gyear = case_when(
      fips == 13 ~ 2013L,   # Georgia
      fips == 55 ~ 2018L,   # Wisconsin
      fips == 42 ~ 2022L,   # Pennsylvania
      TRUE       ~ 0L       # never-treated
    )
  )

cat(sprintf("Panel: %d states, FY %d-%d\n",
            n_distinct(df$fips), min(df$fy), max(df$fy)))
cat("Cohort distribution:\n")
print(df %>% distinct(fips, gyear) %>% count(gyear))

# ========================================================================
# 4a. ETWFE — BASELINE, NO COVARIATES (design check)
#   control_var = "never" uses never-treated units as the control group.
#   Mirrors: jwdid lngenop, ivar(fips) tvar(fy) gvar(gyear) never
# ========================================================================
cat("\n=== ETWFE (no covariates, never-treated controls) ===\n")

# df_model: the exact rows etwfe/fixest will use. Filter on is.finite()
# for the model variables so that no -Inf (from log(0)) or NaN rows remain.
# This guarantees df_model, fit_4a, and fit_4b all share the same row set,
# so emfx() aggregations align with the fitted model frame.
df_model <- df %>%
  filter(if_all(c(lngenop, lntotsup, lnfinaid, lntuifee, lnfte), is.finite),
         !is.na(fips), !is.na(fy), !is.na(gyear))
cat(sprintf("df_model: %d rows used for both etwfe models\n", nrow(df_model)))

fit_4a <- etwfe(
  fml       = lngenop ~ 1,         # no covariates
  tvar      = fy,
  gvar      = gyear,
  data      = df_model,
  ivar      = fips,
  cgroup    = "never",              # never-treated controls only --
                                     # matches jwdid's "never" option.
                                     # etwfe's DEFAULT is cgroup="notyet",
                                     # which silently suppresses pre-
                                     # treatment leads in emfx(type="event")
                                     # output. Omitting this argument was a
                                     # real discrepancy from the Stata
                                     # specification this script mirrors.
  vcov      = ~fips                 # cluster by state
)
print(summary(fit_4a))

# Post-estimation aggregations (mirrors estat simple / group / event)
agg_4a_simple <- emfx(fit_4a, type = "simple")
agg_4a_group  <- emfx(fit_4a, type = "group")
# post_only = FALSE: explicit insurance to retrieve pre-treatment event
# rows now that cgroup = "never" makes them genuinely estimated effects
# rather than mechanical zeros (see note on fit_4a above).
agg_4a_event  <- emfx(fit_4a, type = "event", post_only = FALSE)

cat("\n--- 4a: Overall ATT (simple average) ---\n")
print(agg_4a_simple)
cat("\n--- 4a: Cohort-specific ATT ---\n")
print(agg_4a_group)
cat("\n--- 4a: Event-study ATT ---\n")
print(agg_4a_event)

# Store 4a scalars for comparison table
a_simple_b  <- agg_4a_simple$estimate[1]
a_simple_se <- agg_4a_simple$std.error[1]
a_group_b   <- agg_4a_group$estimate
a_group_se  <- agg_4a_group$std.error

# ========================================================================
# 4b. ETWFE — WITH COVARIATES (preferred specification)
#   xvar passes the four controls; etwfe partials them out in a way that
#   avoids rank deficiency — equivalent to jwdid's hettype(cohort) approach.
#   Mirrors: jwdid lngenop $controls, ivar(fips) tvar(fy) gvar(gyear)
#            never hettype(cohort) cluster(fips)
# ========================================================================
cat("\n=== ETWFE (covariates, never-treated controls) ===\n")

controls_fml <- as.formula(paste("~", paste(controls, collapse = " + ")))

fit_4b <- etwfe(
  fml       = lngenop ~ lntotsup + lnfinaid + lntuifee + lnfte,
  tvar      = fy,
  gvar      = gyear,
  data      = df_model,
  ivar      = fips,
  cgroup    = "never",              # see note under fit_4a above
  vcov      = ~fips
)
print(summary(fit_4b))

# ========================================================================
# 5. POST-ESTIMATION AGGREGATIONS (Model 4b)
#   Directly comparable to csdid estat output in Section 10.7.
# ========================================================================

# Overall ATT (simple average across post-treatment ATT(g,t))
agg_4b_simple   <- emfx(fit_4b, type = "simple")
# Group-specific ATT (one per cohort: 2013, 2018, 2022)
agg_4b_group    <- emfx(fit_4b, type = "group")
# Calendar-time ATT
agg_4b_calendar <- emfx(fit_4b, type = "calendar")
# Event-study (dynamic effects relative to treatment onset)
# post_only = FALSE: see note on fit_4a above.
agg_4b_event    <- emfx(fit_4b, type = "event", post_only = FALSE)

cat("\n--- 4b: Overall ATT (simple average) ---\n")
print(agg_4b_simple)
cat("\n--- 4b: Cohort-specific ATT ---\n")
print(agg_4b_group)
cat("\n--- 4b: Calendar-time ATT ---\n")
print(agg_4b_calendar)
cat("\n--- 4b: Event-study ATT ---\n")
print(agg_4b_event)

# Store 4b scalars for comparison table
b_simple_b  <- agg_4b_simple$estimate[1]
b_simple_se <- agg_4b_simple$std.error[1]
b_group_b   <- agg_4b_group$estimate
b_group_se  <- agg_4b_group$std.error

# ========================================================================
# 6. EVENT-STUDY PLOT (fig10_7)
#   Built on the Model 4b (covariate-adjusted) event-study aggregation.
#   Reference period (event time = -1) is the row where estimate = 0 or
#   std.error = NA; all remaining rows are shifted accordingly.
# ========================================================================

# emfx(type = "event") returns a data frame with an "event" column
# containing the relative time period. Reference period may appear as
# NA estimate or be excluded; we add it back explicitly at event = -1.

es_df <- agg_4b_event %>%
  select(event, estimate, std.error) %>%
  rename(b = estimate, se = std.error) %>%
  mutate(
    lo = b - 1.96 * se,
    hi = b + 1.96 * se
  )

# Add the omitted reference period at event = -1 if not already present
if (!(-1L %in% es_df$event)) {
  ref_row <- data.frame(event = -1L, b = 0, se = NA_real_,
                        lo = 0, hi = 0)
  es_df <- bind_rows(es_df, ref_row) %>% arrange(event)
}

p_es <- ggplot(es_df, aes(x = event, y = b)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dotted",
             colour = "grey50", linewidth = 0.4) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                width = 0.3, colour = "grey50", linewidth = 0.5,
                na.rm = TRUE) +
  geom_line(colour = "black", linewidth = 0.7, na.rm = TRUE) +
  geom_point(shape = 21, size = 2.5,
             colour = "black", fill = "black", na.rm = TRUE) +
  labs(
    title    = "ETWFE Event Study: Staggered Adoption",
    subtitle = "Covariate-adjusted, never-treated controls",
    x        = "Years relative to treatment",
    y        = "ATT on log operations"
  ) +
  theme_springer()

ggsave(file.path(graphs_dir, "fig10_7_R.png"),
       p_es, width = 7, height = 5, dpi = 200)
print(p_es)   # render to the RStudio Plots pane (ggsave only writes to disk)
cat(sprintf("\nfig10_7_R.png exported to %s\n", graphs_dir))

# ========================================================================
# 6b. COMPARISON TABLE: ETWFE WITHOUT vs WITH COVARIATES
#   Rows: Overall, G2013 (Georgia), G2018 (Wisconsin), G2022 (Pennsylvania)
#   Cols: ATT(4a), SE(4a), ATT(4b), SE(4b)
# ========================================================================

# Align group rows: match cohort years 2013, 2018, 2022
cohort_years <- c(2013L, 2018L, 2022L)
cohort_labels <- c("G2013_Georgia", "G2018_Wisconsin", "G2022_Pennsylvania")

# Helper: extract estimate/SE for a given cohort from emfx group output
get_group <- function(agg, gyear_val, col) {
  row <- agg[agg$gyear == gyear_val, col, drop = TRUE]
  if (length(row) == 0) NA_real_ else row[1]
}

tab_df <- data.frame(
  row     = c("Overall", cohort_labels),
  ATT_4a  = c(a_simple_b,
              sapply(cohort_years, get_group, agg = agg_4a_group,
                     col = "estimate")),
  SE_4a   = c(a_simple_se,
              sapply(cohort_years, get_group, agg = agg_4a_group,
                     col = "std.error")),
  ATT_4b  = c(b_simple_b,
              sapply(cohort_years, get_group, agg = agg_4b_group,
                     col = "estimate")),
  SE_4b   = c(b_simple_se,
              sapply(cohort_years, get_group, agg = agg_4b_group,
                     col = "std.error"))
)

cat("\n", strrep("=", 72), "\n", sep = "")
cat("ETWFE ATT estimates: unconditional (4a) vs covariate-adjusted (4b)\n")
cat(strrep("=", 72), "\n", sep = "")
cat(sprintf("  %-26s %9s %9s %9s %9s\n",
            "Row", "ATT(4a)", "SE(4a)", "ATT(4b)", "SE(4b)"))
cat("  ", strrep("-", 65), "\n", sep = "")
for (i in seq_len(nrow(tab_df))) {
  cat(sprintf("  %-26s %9.4f %9.4f %9.4f %9.4f\n",
              tab_df$row[i],
              tab_df$ATT_4a[i], tab_df$SE_4a[i],
              tab_df$ATT_4b[i], tab_df$SE_4b[i]))
}
cat("  ", strrep("-", 65), "\n", sep = "")
cat("  Never-treated controls; SEs clustered by state (fips).\n")
cat("  Cols 1-2: no covariates. Cols 3-4: covariate-adjusted.\n")
cat("  CAUTION: Cols 3-4 (4b) do NOT match the Stata jwdid hettype(cohort)\n")
cat("  results for this design -- see INTERPRETATION block below for why.\n")
cat("  G2022/Pennsylvania is inestimable here (collinearity); compare 4a\n")
cat("  across languages, treat 4b as Stata-only for this application.\n")

# Save as CSV (replaces Stata RTF; import into Word/Excel for Springer table)
write.csv(tab_df, file.path(tables_dir, "tab10_7_etwfe.csv"),
          row.names = FALSE)
cat(sprintf("\ntab10_7_etwfe.csv exported to %s\n", tables_dir))

cat("\nETWFE / etwfe section complete.\n")
cat(sprintf("Figure: %s/fig10_7_R.png\n", graphs_dir))
cat(sprintf("Table:  %s/tab10_7_etwfe.csv\n", tables_dir))

# ========================================================================
# INTERPRETATION (for chapter prose)
# ------------------------------------------------------------------------
# - The unconditional ETWFE (4a) reproduces the CS-DiD pattern: overall
#   ATT ~0.051 (p=0.001), driven by G2013/Georgia (~0.114, p<0.001), with
#   G2018 null and G2022 negative. Pre-treatment leads are large and
#   significant, so this estimate rests on a questionable parallel-trends
#   assumption. R (etwfe) matches the Stata (jwdid) 4a numbers closely.
#
# - IMPORTANT DIVERGENCE, Model 4b (covariate-adjusted): Stata's jwdid,
#   using hettype(cohort), absorbs the differential pre-trends and
#   collapses the overall ATT to a precise null (~-0.002, p=0.90). The R
#   etwfe package does NOT reproduce this. etwfe's covariate handling
#   demeans and fully interacts covariates with every cohort x period
#   cell -- there is no exposed option corresponding to jwdid's
#   hettype(cohort) restriction (additive, cohort-level-only corrections).
#   As a result, R's fit_4b suffers the SAME rank-deficiency problem
#   documented for the fully saturated Stata specification: 302 terms are
#   dropped for collinearity, and the G2022/Pennsylvania cohort effect is
#   inestimable (estimate = 0, SE = NA). R's overall ATT for 4b is
#   +0.116 (p<0.001) -- opposite sign and significance from Stata's null.
#
# - PRACTICAL IMPLICATION: do not report R's Model 4b as equivalent to
#   Stata's Model 4b in the chapter text. The two languages' "covariate-
#   adjusted ETWFE" results are NOT comparable for this three-cohort,
#   four-covariate design; this is a genuine difference in estimator
#   construction (etwfe vs. jwdid covariate handling), not a coding bug
#   in either script. If both languages' results are presented side by
#   side, this divergence should be stated explicitly as a limitation of
#   the etwfe package for small-T, few-cohort panels with covariates --
#   it is a second, independent illustration of the same rank-deficiency
#   caution documented for the saturated default below, not a one-off.
#
# - Report 4a from BOTH languages as the comparable unconditional
#   benchmark. Treat 4b as Stata-only (or as a documented R limitation)
#   for this specific three-cohort design.
# - The fully saturated default ETWFE is NOT appropriate here: with three
#   treated cohorts and four time-varying covariates it is rank-deficient.
#   Note this as a practical caution on ETWFE in small-T, few-cohort panels
#   -- and note that the R package's only covariate mode inherits this
#   same caution, even in the "adjusted" specification meant to avoid it.
# ========================================================================

# ============================================================================
# END OF R_code10_ETWFE.R
# ============================================================================
