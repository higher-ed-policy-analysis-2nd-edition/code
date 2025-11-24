# ================================================================
# Chapter 9 - Advanced Statistical Techniques II
# Full script with integrated fixes:
#  - robust first differences
#  - hardened dccemg() (CSA, lagging, eligibility, MG summary fixes)
#  - diagnostics and formatted export
# ================================================================

# -------------------------
# Required packages
# -------------------------
required_packages <- c(
  "haven", "dplyr", "ggplot2", "plm", "urca", "tidyr",
  "lmtest", "zoo", "tseries", "mFilter", "sandwich", "purrr",
  "broom", "kableExtra", "knitr"
)

installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    message("Installing missing package: ", pkg)
    install.packages(pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# -------------------------
# Load data
# -------------------------
data_url <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch9/Example_9_3_1.dta"
local_file <- "Example_9_3_1.dta"
if (!file.exists(local_file)) {
  download.file(data_url, local_file, mode = "wb")
}
data <- read_dta(local_file)

# Convert to pdata.frame for plm where needed
pdata <- pdata.frame(data, index = c("fips", "FY"))

# -------------------------
# Robust first differences (per-group lag subtraction)
# -------------------------
vars_to_diff <- c("lny1", "lnx1", "lnx2", "lnx3")

pdata_df <- pdata %>%
  as.data.frame() %>%
  arrange(fips, FY) %>%
  group_by(fips) %>%
  mutate(across(all_of(vars_to_diff),
                ~ . - dplyr::lag(.),
                .names = "d{.col}")) %>%
  ungroup()

# Reapply pdata.frame structure
pdata <- pdata.frame(pdata_df, index = c("fips", "FY"))

# -------------------------
# Section 9.3.2: Tests for Nonstationary Data
# -------------------------
# OLS regression with state fixed effects referred to in the text
# Stata equivalent: xtreg lny1 lnx1 lnx2 lnx3, fe
message("\n=== Section 9.3.2: OLS regression with state fixed effects ===")
model_fe_9_3_2 <- plm(lny1 ~ lnx1 + lnx2 + lnx3, data = pdata, model = "within")
print(summary(model_fe_9_3_2))

# Additional diagnostics for the fixed effects model
message("\n=== Fixed Effects Model Diagnostics ===")
# Extract fixed effects (state-specific intercepts)
fixed_effects <- fixef(model_fe_9_3_2)
message("Number of state fixed effects: ", length(fixed_effects))
message("Range of fixed effects: [", round(min(fixed_effects), 3), ", ", 
        round(max(fixed_effects), 3), "]")

# Test for time fixed effects
model_fe_time <- plm(lny1 ~ lnx1 + lnx2 + lnx3, data = pdata, 
                      model = "within", effect = "time")
message("\n=== Time Fixed Effects Test ===")
print(summary(model_fe_time))

# Test for two-way fixed effects
model_fe_twoways <- plm(lny1 ~ lnx1 + lnx2 + lnx3, data = pdata, 
                         model = "within", effect = "twoways")
message("\n=== Two-way Fixed Effects Test ===")
print(summary(model_fe_twoways))

# F-test for fixed effects (vs pooled OLS)
model_pooled <- plm(lny1 ~ lnx1 + lnx2 + lnx3, data = pdata, model = "pooling")
pFtest_result <- pFtest(model_fe_9_3_2, model_pooled)
message("\n=== F-test for Fixed Effects (H0: pooled OLS is adequate) ===")
print(pFtest_result)

# -------------------------
# Section 9.6.2: Unit Root Tests (IPS)
# -------------------------
message("\n=== IPS unit-root tests (levels) ===")
print(purtest(lny1 ~ 1, data = pdata, test = "ips", exo = "intercept"))
print(purtest(lnx1 ~ 1, data = pdata, test = "ips", exo = "intercept"))
print(purtest(lnx2 ~ 1, data = pdata, test = "ips", exo = "intercept"))
print(purtest(lnx3 ~ 1, data = pdata, test = "ips", exo = "intercept"))

message("\n=== IPS unit-root tests (first differences) ===")
# The d* variables exist as dlny1, dlnx1, etc.
print(purtest(dlny1 ~ 1, data = pdata, test = "ips", exo = "intercept"))
print(purtest(dlnx1 ~ 1, data = pdata, test = "ips", exo = "intercept"))
print(purtest(dlnx2 ~ 1, data = pdata, test = "ips", exo = "intercept"))
print(purtest(dlnx3 ~ 1, data = pdata, test = "ips", exo = "intercept"))

# -------------------------
# Section 9.6.3: Cointegration Tests
# -------------------------
# Johansen test on complete cases (approximation)
jodata <- na.omit(cbind(
  lny1 = pdata$lny1, lnx1 = pdata$lnx1,
  lnx2 = pdata$lnx2, lnx3 = pdata$lnx3
))
if (nrow(jodata) > 0) {
  message("\n=== Johansen cointegration test (ca.jo) ===")
  jotest <- ca.jo(jodata, type = "eigen", K = 2, ecdet = "const")
  print(summary(jotest))
} else {
  message("Not enough complete cases for Johansen test.")
}

# Residual-based cointegration (Pedroni-style approximation)
message("\n=== Residual-based cointegration (Pedroni-style approx) ===")
model_within <- plm(lny1 ~ lnx1 + lnx2 + lnx3, data = pdata, model = "within")
index_df <- as.data.frame(index(pdata))
colnames(index_df) <- c("fips", "FY")
resid_df <- cbind(index_df, residuals = resid(model_within))
resid_df$FY <- as.numeric(as.character(resid_df$FY))
resid_avg <- resid_df %>%
  group_by(FY) %>%
  summarise(mean_resid = mean(residuals, na.rm = TRUE), .groups = "drop")
resid_ts <- ts(resid_avg$mean_resid, start = min(resid_avg$FY), frequency = 1)
print(summary(ur.df(resid_ts, type = "drift", lags = 2)))

# -------------------------
# Section 9.6.5: Homogeneous Coefficient Test (pooling)
# -------------------------
message("\n=== Pooling model (homogeneous coefficients approx) ===")
model_homog <- plm(lny1 ~ lag(lny1) + lnx1 + lnx2 + lnx3, data = pdata, model = "pooling")
print(summary(model_homog))

# -------------------------
# Section 9.6.6: Hardened DCCE/MG estimator
# -------------------------
# This version includes:
#  - CSA creation and joining by time
#  - robust ordering and lag creation
#  - eligibility filter (min_obs) to skip tiny panels
#  - MG summary without naming collision (mean_est / sd_est)
dccemg <- function(data, id, time, y, xvars, lag_dv = TRUE, min_obs = 4) {
  library(dplyr)
  library(purrr)
  library(broom)

  data <- as.data.frame(data)

  # Ensure time is orderable (coerce if possible)
  if (!is.numeric(data[[time]])) {
    suppressWarnings({
      tmp_time <- as.numeric(as.character(data[[time]]))
    })
    if (all(!is.na(tmp_time))) {
      data[[time]] <- tmp_time
    } else {
      data[[time]] <- as.integer(as.factor(data[[time]]))
    }
  }

  # 1) Cross-sectional averages by time
  csa <- data %>%
    group_by(across(all_of(time))) %>%
    summarise(across(all_of(c(y, xvars)), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  csa_cols <- setdiff(names(csa), time)
  names(csa)[names(csa) %in% csa_cols] <- paste0("csa_", csa_cols)

  # 2) join CSA back to the data
  data <- left_join(data, csa, by = time)

  # 3) create lagged dependent variable per id (robust)
  if (lag_dv) {
    data <- data %>% arrange(.data[[id]], .data[[time]])
    data <- data %>%
      group_by(across(all_of(id))) %>%
      mutate(L_y = dplyr::lag(.data[[y]])) %>%
      ungroup()
    rhs_vars <- c("L_y", xvars, paste0("csa_", y), paste0("csa_", xvars))
  } else {
    rhs_vars <- c(xvars, paste0("csa_", y), paste0("csa_", xvars))
  }

  # 4) formula
  formula_str <- paste0(y, " ~ ", paste(rhs_vars, collapse = " + "))

  # 5) split and estimate individual models; filter eligible units
  by_id <- split(data, data[[id]])
  eligible_ids <- names(by_id)[
    vapply(by_id, function(df) {
      if (lag_dv) {
        sum(!is.na(df[[y]]) & !is.na(df$L_y)) >= min_obs
      } else {
        sum(!is.na(df[[y]])) >= min_obs
      }
    }, logical(1))
  ]
  if (length(eligible_ids) == 0) {
    stop("No individuals have enough observations to estimate individual models. Increase data or reduce min_obs.")
  }

  models <- map(eligible_ids, function(i) {
    df <- by_id[[i]]
    tryCatch(lm(as.formula(formula_str), data = df), error = function(e) NULL)
  })
  names(models) <- eligible_ids

  # 6) tidy
  tidy_results <- imap_dfr(models, function(mod, iid) {
    if (is.null(mod)) return(NULL)
    td <- broom::tidy(mod)
    if (nrow(td) == 0) return(NULL)
    td$id <- iid
    td
  })

  if (nrow(tidy_results) == 0) stop("No individual models were successfully estimated.")

  # 7) MG summary (use mean_est / sd_est to avoid name collision)
  mg_summary <- tidy_results %>%
    mutate(estimate = as.numeric(estimate)) %>%
    group_by(term) %>%
    summarise(
      n_est    = sum(!is.na(estimate)),
      mean_est = if (n_est > 0) mean(estimate, na.rm = TRUE) else NA_real_,
      sd_est   = if (n_est > 1) sd(estimate, na.rm = TRUE) else NA_real_,
      std.error= if (n_est > 1) sd_est / sqrt(n_est) else NA_real_,
      t_stat   = if (!is.na(std.error) && std.error > 0) mean_est / std.error else NA_real_,
      p_value  = if (!is.na(t_stat)) 2 * pt(-abs(t_stat), df = pmax(n_est - 1, 1)) else NA_real_,
      ci_lower = if (!is.na(std.error)) mean_est - qt(0.975, df = pmax(n_est - 1, 1)) * std.error else NA_real_,
      ci_upper = if (!is.na(std.error)) mean_est + qt(0.975, df = pmax(n_est - 1, 1)) * std.error else NA_real_,
      .groups = "drop"
    ) %>%
    rename(estimate = mean_est)

  # 8) small-sample warning
  single_est_terms <- mg_summary$term[mg_summary$n_est <= 1]
  if (length(single_est_terms) > 0) {
    warning("Some terms have <= 1 individual estimates; standard errors and t-stats are NA for: ",
            paste(single_est_terms, collapse = ", "))
  }

  list(
    mg_estimates = mg_summary,
    individual_coefs = tidy_results,
    eligible_ids = eligible_ids,
    formula = formula_str
  )
}

# -------------------------
# Run DCCE/MG and export results
# -------------------------
message("\n=== Running DCCE/MG estimation ===")
dccemg_result <- dccemg(
  data = pdata,
  id = "fips",
  time = "FY",
  y = "lny1",
  xvars = c("lnx1", "lnx2", "lnx3"),
  lag_dv = TRUE,
  min_obs = 4
)

mg_summary <- dccemg_result$mg_estimates
message("\n=== MG summary (raw) ===")
print(mg_summary)

# Diagnostics: counts per unit / check eligible_ids
message("\n=== Diagnostics: units used and counts ===")
message("Number of eligible ids: ", length(dccemg_result$eligible_ids))
print(table(dccemg_result$individual_coefs$id))

# -------------------------
# Format table for writeup and save
# -------------------------
fmt_mg <- mg_summary %>%
  mutate(
    estimate_r = round(estimate, 3),
    se_r = ifelse(!is.na(std.error), round(std.error, 3), NA_real_),
    ci_lo = ifelse(!is.na(ci_lower), round(ci_lower, 3), NA_real_),
    ci_hi = ifelse(!is.na(ci_upper), round(ci_upper, 3), NA_real_),
    coef_se = ifelse(is.na(se_r),
                     sprintf("%s (NA)", estimate_r),
                     sprintf("%s (%s)", estimate_r, se_r)),
    ci = ifelse(is.na(ci_lo) | is.na(ci_hi), NA, sprintf("(%s, %s)", ci_lo, ci_hi)),
    p_value_fmt = ifelse(is.na(p_value), NA, format.pval(p_value, digits = 3, eps = .0001)),
    signif = dplyr::case_when(
      !is.na(p_value) & p_value < 0.001 ~ "***",
      !is.na(p_value) & p_value < 0.01  ~ "**",
      !is.na(p_value) & p_value < 0.05  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(term, coef_se, ci, t_stat, p_value_fmt, signif) %>%
  rename(
    Term = term,
    `Estimate (SE)` = coef_se,
    `95% CI` = ci,
    t = t_stat,
    `p-value` = p_value_fmt,
    Signif = signif
  )

message("\n=== Formatted MG table ===")
print(knitr::kable(fmt_mg, caption = "Mean Group (MG) estimates — DCCE/MG"))

# Save CSV and LaTeX
write.csv(fmt_mg, "mg_estimates_table.csv", row.names = FALSE)
message("Saved MG estimates to: mg_estimates_table.csv")
try({
  kbl <- kableExtra::kable(fmt_mg, format = "latex", booktabs = TRUE, caption = "Mean Group (MG) estimates")
  cat(kbl, file = "mg_estimates_table.tex")
  message("Saved LaTeX table to: mg_estimates_table.tex")
}, silent = TRUE)

message("\nAll steps completed. Notes:\n - MG std.errors use sd(estimate)/sqrt(n_units). This assumes independence across units; consider bootstrap or robust variance if cross-sectional dependence remains.\n - To replicate particular xtdcce2 options from Stata (e.g., ARDL or xtpmg transformations) we can add a function to compute long-run coefficients from short-run + adjustment terms; tell me if you want that next.\n")
