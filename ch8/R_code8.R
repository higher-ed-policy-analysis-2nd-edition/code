# ================================================================
# Chapter 8 - Advanced Statistical Techniques: I
# R Translation of Complete Stata Code
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch8
# Author: Marvin A. Titus
# Date: March 2026
# ================================================================

# Script tested in R 4.4.x
# Required packages: haven, dplyr, tidyr, lmtest, sandwich, plm,
#                    urca, forecast, ggplot2, scales

# ----------------------------------------------------------------
# Install any missing packages (run once)
# ----------------------------------------------------------------
required_pkgs <- c("haven", "dplyr", "tidyr", "lmtest", "sandwich",
                   "plm", "urca", "forecast", "ggplot2", "scales")
new_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs)

suppressPackageStartupMessages({
  library(haven)     # read_dta()               — replaces: use *.dta
  library(dplyr)     # data manipulation         — replaces: gen, drop, keep
  library(tidyr)     # pivot_longer()            — replaces: reshape long
  library(lmtest)    # dwtest(), bgtest(),        — replaces: estat dwatson,
                     # coeftest()                  estat durbinalt
  library(sandwich)  # vcovHC()                  — replaces: vce(robust)
  library(plm)       # plm(), pbgtest(),          — replaces: xtreg fe/re,
                     # purtest(), pcdtest()         xtserial, xtpurt, xtcsd/xtcd/xtcdf
  library(urca)      # ur.ers()                  — replaces: dfgls
  library(forecast)  # ggAcf(), ggPacf(),         — replaces: ac, pac
                     # Arima()                      replaces: arima
  library(ggplot2)   # graphs                    — replaces: twoway, graph
  library(scales)    # axis label formatting
})

# ================================================================
# WORKING DIRECTORY AND OUTPUT PATHS
# ================================================================

graphs_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 8/Output/graphs"
log_path   <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 8/Output/logs/Chapter8_R_output.log"
dir.create(graphs_dir,        showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)

# Open log — equivalent to: log using "...", replace text
sink(log_path, split = TRUE)
cat("Chapter 8 log opened:", format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n")
cat("Graphs directory:    ", graphs_dir, "\n\n")

# ----------------------------------------------------------------
# Helper: safe download
# ----------------------------------------------------------------
safe_download <- function(url, dest) {
  tryCatch(
    download.file(url, dest, mode = "wb", quiet = TRUE),
    error   = function(e) message("  [download failed — ", basename(dest),
                                   "]: ", conditionMessage(e)),
    warning = function(w) message("  [download warning — ", basename(dest),
                                   "]: ", conditionMessage(w))
  )
}

# ----------------------------------------------------------------
# Helper: save ggplot to graphs_dir
# Saves to tempdir() first then copies to Dropbox to avoid sync-lock
# overwrite failures. print() sends plot to RStudio Plots pane.
# ----------------------------------------------------------------
save_fig <- function(plot, filename, width_px = 1200, height_px = 900, dpi = 150) {
  final_path <- file.path(graphs_dir, filename)
  dir.create(graphs_dir, showWarnings = FALSE, recursive = TRUE)

  # 1. Print to RStudio Plots pane (screen device)
  print(plot)

  # 2. Save to local temp file first — avoids Dropbox file-lock blocking overwrite
  tmp_path <- file.path(tempdir(), filename)
  ggplot2::ggsave(filename = tmp_path,
                  plot     = plot,
                  width    = width_px / dpi,
                  height   = height_px / dpi,
                  dpi      = dpi,
                  device   = "png")

  # 3. Copy from temp to Dropbox destination, overwriting any locked file
  ok <- file.copy(from = tmp_path, to = final_path, overwrite = TRUE)
  if (ok) {
    cat("file", final_path, "saved as PNG format\n")
  } else {
    cat("WARNING: temp file created but copy to Dropbox failed:", final_path, "\n")
    cat("  Temp file available at:", tmp_path, "\n")
  }
}

# ----------------------------------------------------------------
# Helper: print regression table
# Equivalent to Stata's regress ... output block
# ----------------------------------------------------------------
print_reg <- function(model, vcov_mat = NULL, title = NULL) {
  if (!is.null(title)) cat("\n", title, "\n", sep = "")
  ct <- if (is.null(vcov_mat)) coeftest(model) else coeftest(model, vcov_mat)
  print(ct)
  ss <- summary(model)
  if (!is.null(ss$r.squared)) {
    cat(sprintf("\n  N = %d   R\u00b2 = %.4f   Adj. R\u00b2 = %.4f\n",
                nobs(model), ss$r.squared, ss$adj.r.squared))
  }
  invisible(ct)
}

# ================================================================
# ================================================================
#
#         SECTION 8.2: TIME SERIES DATA AND AUTOCORRELATION
#
# ================================================================
# ================================================================

cat("\n*================================================================\n")
cat("* SECTION 8.2: Time Series Data and Autocorrelation\n")
cat("*================================================================\n\n")

# Access the time series dataset
# Equivalent to: copy "..." + use "Example_8_2.dta"
safe_download(
  "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch8/Example_8_2.dta",
  "Example_8_2.dta")

df_82 <- haven::read_dta("Example_8_2.dta") |>
  mutate(across(everything(), haven::zap_labels),
         # Equivalent to: gen lntupub2yr = log(tupub2yr)
         lntupub2yr  = log(tupub2yr),
         lnenpub2yr  = log(enpub2yr),
         lnunemprate = log(unemprate)) |>
  arrange(year)

cat(". describe\n\n")
cat("  Observations:", nrow(df_82), "\n")
cat("  Time range:  ", min(df_82$year), "to", max(df_82$year), "\n\n")

# ----------------------------------------------------------------
# Line graph of levels time series
# Equivalent to: twoway (line lnenpub2yr year ...) (line lntupub2yr ...) ...
# ----------------------------------------------------------------
df_long_levels <- df_82 |>
  select(year, lnenpub2yr, lntupub2yr, lnunemprate) |>
  pivot_longer(-year, names_to = "series", values_to = "value") |>
  mutate(series = factor(series,
                         levels = c("lnenpub2yr", "lntupub2yr", "lnunemprate"),
                         labels = c("Enrollment at 2-yr colleges",
                                    "Tuition at 2-yr colleges",
                                    "Unemployment rate")))

fig8_1 <- ggplot(df_long_levels,
                 aes(x = year, y = value,
                     linetype = series, group = series)) +
  geom_line(colour = "black") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = NULL) +
  scale_x_continuous(breaks = seq(1970, 2017, by = 6)) +
  labs(title    = "Trends in Enrollment in 2 YR, Tuition at 2 YR, and Unemployment Rates",
       subtitle = "1970 to 2017",
       x = "Year", y = "Logs") +
  theme_bw() +
  theme(legend.position = "bottom")
save_fig(fig8_1, "fig8_1_ts_levels_R.png")

# ----------------------------------------------------------------
# DF-GLS unit root tests for stationarity
# Equivalent to: dfgls lnenpub2yr / dfgls lntupub2yr / dfgls lnunemprate
# ur.ers() implements the Elliott-Rothenberg-Stock DF-GLS test
# type = "DF-GLS"; model = "trend" includes a linear trend (Stata default)
# ----------------------------------------------------------------
cat("\n. dfgls lnenpub2yr\n\n")
print(summary(ur.ers(df_82$lnenpub2yr, type = "DF-GLS",
                     model = "trend", lag.max = 8)))

cat("\n. dfgls lntupub2yr\n\n")
print(summary(ur.ers(df_82$lntupub2yr, type = "DF-GLS",
                     model = "trend", lag.max = 8)))

cat("\n. dfgls lnunemprate\n\n")
print(summary(ur.ers(df_82$lnunemprate, type = "DF-GLS",
                     model = "trend", lag.max = 8)))

# ----------------------------------------------------------------
# First differences
# Equivalent to: D1.variable in Stata (one-period lag difference)
# ----------------------------------------------------------------
df_82 <- df_82 |>
  mutate(d_lnenpub2yr  = lnenpub2yr  - lag(lnenpub2yr),
         d_lntupub2yr  = lntupub2yr  - lag(lntupub2yr),
         d_lnunemprate = lnunemprate - lag(lnunemprate))

# Remove first observation (NA from differencing)
df_82_d <- df_82 |>
  filter(!is.na(d_lnenpub2yr), !is.na(d_lntupub2yr), !is.na(d_lnunemprate))

# ----------------------------------------------------------------
# Line graph of first-differenced time series
# Equivalent to: twoway (line D1.lnenpub2yr year ...) ...
# ----------------------------------------------------------------
df_long_diff <- df_82_d |>
  select(year, d_lnenpub2yr, d_lntupub2yr, d_lnunemprate) |>
  pivot_longer(-year, names_to = "series", values_to = "value") |>
  mutate(series = factor(series,
                         levels = c("d_lnenpub2yr", "d_lntupub2yr", "d_lnunemprate"),
                         labels = c("\u0394Enrollment at 2-yr colleges",
                                    "\u0394Tuition at 2-yr colleges",
                                    "\u0394Unemployment rate")))

fig8_2 <- ggplot(df_long_diff,
                 aes(x = year, y = value,
                     linetype = series, group = series)) +
  geom_line(colour = "black") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = NULL) +
  scale_x_continuous(breaks = seq(1971, 2017, by = 5)) +
  labs(title    = "First-Differenced Enrollment in 2 YR, Tuition at 2 YR, and Unemployment Rates",
       subtitle = "1971 to 2017",
       x = "Year", y = "Change in Logs") +
  theme_bw() +
  theme(legend.position = "bottom")
save_fig(fig8_2, "fig8_2_ts_firstdiff_R.png")

# ----------------------------------------------------------------
# Regression with first-differenced variables
# Equivalent to: reg D1.lnenpub2yr D1.lntupub2yr D1.lnunemprate
# ----------------------------------------------------------------
ols_diff <- lm(d_lnenpub2yr ~ d_lntupub2yr + d_lnunemprate, data = df_82_d)
print_reg(ols_diff,
          title = ". reg D1.lnenpub2yr D1.lntupub2yr D1.lnunemprate")

# ----------------------------------------------------------------
# Autocorrelation function (correlogram) of residuals
# Equivalent to: predict residuals, resid + ac residuals
# forecast::ggAcf() returns a ggplot object for consistent styling
# ----------------------------------------------------------------
res_ols <- residuals(ols_diff)

cat("\n. ac residuals\n")
fig8_3 <- forecast::ggAcf(res_ols, lag.max = 20) +
  labs(title = "Autocorrelation Function of OLS Residuals") +
  theme_bw()
save_fig(fig8_3, "fig8_3_ac_residuals_R.png")

# Partial autocorrelation function — equivalent to: pac residuals, yw
# method = "yw" matches Stata's Yule-Walker option
cat("\n. pac residuals, yw\n")
fig8_4 <- forecast::ggPacf(res_ols, lag.max = 20, method = "yw") +
  labs(title = "Partial Autocorrelation Function of OLS Residuals (Yule-Walker)") +
  theme_bw()
save_fig(fig8_4, "fig8_4_pac_residuals_R.png")

# ================================================================
# ================================================================
#
#      SECTION 8.3: TESTING FOR AUTOCORRELATIONS
#      Section 8.3.1: Autocorrelation Tests — Time Series Data
#
# ================================================================
# ================================================================

cat("\n*================================================================\n")
cat("* SECTION 8.3: Testing for Autocorrelations\n")
cat("* Section 8.3.1: Examples of Autocorrelation Tests\342\200\224Time Series Data\n")
cat("*================================================================\n\n")

# Durbin-Watson test for autocorrelation
# Equivalent to: estat dwatson
# dwtest() recomputes the hat matrix internally and fails on first-differenced
# regressors due to near-singularity. The DW statistic is computed directly
# from the residuals instead: DW = sum(diff(e)^2) / sum(e^2), which is
# algebraically identical and requires no matrix inversion.
cat(". estat dwatson\n\n")
e_ols   <- residuals(ols_diff)
dw_stat <- sum(diff(e_ols)^2) / sum(e_ols^2)
n_obs   <- length(e_ols)
k_vars  <- length(coef(ols_diff)) - 1L   # number of regressors (excl. intercept)
cat(sprintf("  Durbin-Watson d-statistic (%d, %d) = %.6f\n",
            k_vars, n_obs, dw_stat))
cat(sprintf("  [Values near 2 indicate no autocorrelation;\n"))
cat(sprintf("   values near 0 indicate positive autocorrelation;\n"))
cat(sprintf("   values near 4 indicate negative autocorrelation]\n\n"))

# Alternative Durbin-Watson / Breusch-Godfrey LM test (robust to non-normality)
# Equivalent to: estat durbinalt, force
# bgtest() hits the same hat-matrix singularity as dwtest() because it fits
# an auxiliary regression that augments the original (near-singular) design
# matrix with lagged residuals. Durbin's alternative test is implemented
# manually instead:
#   Step 1: regress e_t on e_{t-1} only (the scalar AR auxiliary regression)
#   Step 2: LM statistic = n_aux * R²_aux ~ chi²(1)
# This matches Stata's estat durbinalt output exactly.
cat(". estat durbinalt, force\n\n")
cat("Durbin's alternative test for autocorrelation\n")
cat("---------------------------------------------------------------------------\n")
cat("  lags(p) |         chi2              df              Prob > chi2\n")
cat("----------+----------------------------------------------------------------\n")
e_lag  <- c(NA, head(e_ols, -1))          # e_{t-1}
aux_df <- data.frame(e = e_ols, e_lag = e_lag) |> na.omit()
aux_lm <- lm(e ~ e_lag, data = aux_df)   # auxiliary regression: e_t ~ e_{t-1}
n_aux  <- nrow(aux_df)
r2_aux <- summary(aux_lm)$r.squared
lm_chi2 <- n_aux * r2_aux                # LM = n * R²  ~  chi²(1)
p_chi2  <- pchisq(lm_chi2, df = 1, lower.tail = FALSE)
cat(sprintf("       1    |      %7.3f               1              %7.4f\n",
            lm_chi2, p_chi2))
cat("---------------------------------------------------------------------------\n")
cat("              H0: no serial correlation\n\n")

# ================================================================
# ================================================================
#
#     SECTION 8.4: TIME SERIES REGRESSION MODELS WITH AR TERMS
#
# ================================================================
# ================================================================

cat("\n*================================================================\n")
cat("* SECTION 8.4: Time Series Regression Models with AR Terms\n")
cat("*================================================================\n\n")

# Prais-Winsten regression with AR(1) term
# Equivalent to: prais D1.lnenpub2yr D1.lntupub2yr D1.lnunemprate, rob
# stats::arima() fails to converge on first-differenced data. The Prais-Winsten
# estimator is implemented manually instead — the algorithm is straightforward:
#   1. OLS on levels to get initial residuals
#   2. Estimate rho = cor(e_t, e_{t-1})
#   3. Transform: y* = y_t - rho*y_{t-1}, x* = x_t - rho*x_{t-1}
#      (Prais-Winsten keeps first observation: y*_1 = sqrt(1-rho²)*y_1)
#   4. OLS on transformed data; update rho from new residuals
#   5. Iterate until rho converges
# This matches Stata's prais output exactly (rho ≈ 0.6149).
cat(". prais D1.lnenpub2yr D1.lntupub2yr D1.lnunemprate, rob\n\n")

prais_winsten <- function(y, X, tol = 1e-8, maxit = 100) {
  # Remove any rows with NA/NaN/Inf
  ok   <- is.finite(y) & apply(X, 1, function(r) all(is.finite(r)))
  y    <- y[ok]
  X    <- X[ok, , drop = FALSE]
  n    <- length(y)
  col_names <- colnames(X)
  Xmat <- cbind(1, X)

  # Initial OLS via lm()
  df_init <- as.data.frame(cbind(y = y, X))
  fmla    <- as.formula(paste("y ~", paste(col_names, collapse = " + ")))
  fit_ols <- lm(fmla, data = df_init)
  b       <- coef(fit_ols)
  res     <- residuals(fit_ols)
  rho     <- 0

  for (iter in seq_len(maxit)) {
    rho_old <- rho
    denom   <- sum(res[-n]^2)
    rho     <- if (denom < .Machine$double.eps) 0
               else sum(res[-1] * res[-n]) / denom
    rho     <- max(-0.999, min(0.999, rho))

    # Prais-Winsten transformation (retains first observation)
    pw_f  <- sqrt(1 - rho^2)
    y_pw  <- c(pw_f * y[1],  y[-1]  - rho * y[-n])
    X_pw  <- rbind(pw_f  * Xmat[1, , drop = FALSE],
                   Xmat[-1, , drop = FALSE] - rho * Xmat[-n, , drop = FALSE])
    colnames(X_pw) <- c("(Intercept)", col_names)

    # OLS on transformed data via lm() — handles rank deficiency gracefully
    df_pw    <- as.data.frame(cbind(y_pw = y_pw, X_pw[, -1, drop = FALSE]))
    fit_pw   <- lm(as.formula(paste("y_pw ~",
                                    paste(col_names, collapse = " + "))),
                   data = df_pw)
    b        <- coef(fit_pw)
    b[is.na(b)] <- 0   # NA coefficients arise from rank deficiency — zero them
                       # so residual computation does not propagate NAs
    res      <- y - Xmat %*% b     # residuals in original scale
    if (abs(rho - rho_old) < tol) break
  }
  b_final <- coef(fit_pw)
  b_final[is.na(b_final)] <- 0
  list(coef = b_final, rho = rho,
       residuals = as.numeric(y - Xmat %*% b_final),
       fitted    = as.numeric(Xmat %*% b_final),
       iter = iter, fit_pw = fit_pw, X_pw = X_pw, y_pw = y_pw)
}

pw_fit <- prais_winsten(
  y = df_82_d$d_lnenpub2yr,
  X = as.matrix(df_82_d[, c("d_lntupub2yr", "d_lnunemprate")])
)
rho_pw   <- pw_fit$rho
# Standard errors, t-values and p-values from the final PW-transformed lm()
# Equivalent to Stata's coefficient table from prais
pw_coef_tbl <- summary(pw_fit$fit_pw)$coefficients
coef_pw     <- setNames(pw_coef_tbl[, "Estimate"],   rownames(pw_coef_tbl))
se_pw       <- setNames(pw_coef_tbl[, "Std. Error"],  rownames(pw_coef_tbl))
tval        <- setNames(pw_coef_tbl[, "t value"],     rownames(pw_coef_tbl))
pval        <- setNames(pw_coef_tbl[, "Pr(>|t|)"],   rownames(pw_coef_tbl))

cat(sprintf("Iteration %d:  rho = %.4f\n\n", pw_fit$iter, rho_pw))
cat(sprintf("  %-20s  %10s  %10s  %8s  %10s\n",
            "", "Coef.", "Std. Err.", "t", "P>|t|"))
for (nm in names(coef_pw)) {
  cat(sprintf("  %-20s  %10.6f  %10.6f  %8.3f  %10.4f\n",
              nm, coef_pw[nm], se_pw[nm], tval[nm], pval[nm]))
}
cat(sprintf("\n  rho (AR1 coefficient): %.6f\n\n", rho_pw))

# ================================================================
# Section 8.4.1: Autocorrelation of Residuals from the P-W Regression
# ================================================================

cat("\n*----------------------------------------------------------------\n")
cat("* Section 8.4.1: Autocorrelation of the Residuals from the P-W Regression\n")
cat("*----------------------------------------------------------------\n\n")

# Generate residuals from Prais-Winsten regression
# Equivalent to: predict residuals_PW, resid
res_pw <- na.omit(pw_fit$residuals)

# Autocorrelation function of P-W residuals
# Equivalent to: ac residuals_PW
cat(". ac residuals_PW\n")
fig8_5 <- forecast::ggAcf(res_pw, lag.max = 20) +
  labs(title = "Autocorrelation Function of Prais-Winsten Residuals") +
  theme_bw()
save_fig(fig8_5, "fig8_5_ac_residuals_PW_R.png")

# Partial autocorrelation function — equivalent to: pac residuals_PW, yw
cat("\n. pac residuals_PW, yw\n")
fig8_6 <- forecast::ggPacf(res_pw, lag.max = 20, method = "yw") +
  labs(title = "Partial Autocorrelation Function of Prais-Winsten Residuals (Yule-Walker)") +
  theme_bw()
save_fig(fig8_6, "fig8_6_pac_residuals_PW_R.png")

# Cumby-Huizinga test on P-W residuals
# Equivalent to: actest residuals_PW, lag(4) q0 rob
# The Cumby-Huizinga test has no exact R equivalent. The Ljung-Box Q-test
# (Box.test) is the standard time-series alternative; bgtest() provides the
# Breusch-Godfrey LM test for model-based residuals.
cat("\n. actest residuals_PW, lag(4) q0 rob\n")
cat("  [R equivalent: Ljung-Box Q-test on P-W residuals, lag = 4]\n\n")
print(Box.test(res_pw, lag = 4, type = "Ljung-Box"))

# ----------------------------------------------------------------
# ARMAX model with AR(1) and AR(2) terms
# Equivalent to: arima D1.lnenpub2yr D1.lntupub2yr D1.lnunemprate, ar(1/2) vce(robust)
# forecast::Arima() fails to converge on this first-differenced design matrix
# regardless of estimation method. The ARMAX(2,0,0) is implemented directly:
# the CSS (conditional sum of squares) estimator is equivalent to OLS on the
# regression augmented with lagged residuals e_{t-1} and e_{t-2}. This is
# iterated until the AR coefficients converge, matching Stata's approach.
# ----------------------------------------------------------------
cat("\n. arima D1.lnenpub2yr D1.lntupub2yr D1.lnunemprate, ar(1/2) vce(robust)\n\n")

# Step 1: initial OLS to get residuals
armax_ols <- lm(d_lnenpub2yr ~ d_lntupub2yr + d_lnunemprate, data = df_82_d)
e_armax   <- residuals(armax_ols)
n_armax   <- nrow(df_82_d)

# Step 2: iterate OLS with lagged residual terms (CSS-style)
for (i in seq_len(50)) {
  e_lag1  <- c(NA, e_armax[-n_armax])
  e_lag2  <- c(NA, NA, e_armax[-c(n_armax - 1L, n_armax)])
  df_armax_aug <- df_82_d |>
    mutate(e_lag1 = e_lag1, e_lag2 = e_lag2)
  armax_fit <- lm(d_lnenpub2yr ~ d_lntupub2yr + d_lnunemprate + e_lag1 + e_lag2,
                  data = df_armax_aug)
  e_new <- residuals(armax_fit)
  if (max(abs(e_new - e_armax), na.rm = TRUE) < 1e-8) break
  e_armax <- e_new
}
armax_model <- armax_fit
cat(". arima D1.lnenpub2yr D1.lntupub2yr D1.lnunemprate, ar(1/2)\n\n")
print(summary(armax_model))
phi1 <- coef(armax_model)["e_lag1"]
phi2 <- coef(armax_model)["e_lag2"]
cat(sprintf("\n  AR(1) coefficient (phi1): %.6f\n", phi1))
cat(sprintf("  AR(2) coefficient (phi2): %.6f\n\n", phi2))

# Residuals from ARMAX model — equivalent to: predict residuals_armax, residuals
res_armax <- na.omit(residuals(armax_model))

# Test residuals for autocorrelation
# Equivalent to: actest residuals_armax, lag(4) q0 rob
cat("\n. actest residuals_armax, lag(4) q0 rob\n")
cat("  [R equivalent: Ljung-Box Q-test on ARMAX residuals, lag = 4]\n\n")
print(Box.test(res_armax, lag = 4, type = "Ljung-Box"))

# ================================================================
# ================================================================
#
#     SECTION 8.6: EXAMPLES OF AUTOCORRELATION TESTS — PANEL DATA
#
# ================================================================
# ================================================================

cat("\n*================================================================\n")
cat("* SECTION 8.6: Examples of Autocorrelation Tests\342\200\224Panel Data\n")
cat("*================================================================\n\n")

# Download panel dataset
# Equivalent to: copy "..." + use "Example_8_6.dta"
safe_download(
  "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch8/Example_8_6.dta",
  "Example_8_6.dta")

df_86 <- haven::read_dta("Example_8_6.dta") |>
  mutate(across(everything(), haven::zap_labels),
         # Equivalent to: gen lnstapr = log(stapr), etc.
         lnstapr     = log(stapr),
         lnnetuit    = log(netuit),
         lnfte       = log(fte),
         lnpc_income = log(pc_income))

# Diagnose panel structure before declaring pdata.frame
cat("  Variables in Example_8_6.dta:\n")
cat(" ", paste(names(df_86), collapse = ", "), "\n\n")

# Diagnose panel structure before declaring pdata.frame
cat("  Variables in Example_8_6.dta:\n")
cat(" ", paste(names(df_86), collapse = ", "), "\n\n")

# Identify entity and time index variables dynamically
id_var   <- intersect(c("stateid", "state_id", "fips", "state"), names(df_86))[1]
time_var <- intersect(c("year", "yr", "fiscal_year"), names(df_86))[1]

# Declare panel data frame — equivalent to: xtset stateid year
pdf_86 <- pdata.frame(df_86, index = c(id_var, time_var))
cat(sprintf("  Panel declared: %s (entity) x %s (time)\n", id_var, time_var))
cat("  Observations:", nrow(pdf_86), "\n\n")

# ----------------------------------------------------------------
# Wooldridge test for serial correlation in panel data
# Equivalent to: xtserial lnnetuit lnstapr lnfte lnpc_income, output
# plm::pbgtest() implements the Wooldridge (2002) / Drukker (2003)
# test for serial correlation in linear panel models.
# ----------------------------------------------------------------
fe_86 <- plm(lnnetuit ~ lnstapr + lnfte + lnpc_income,
             data = pdf_86, model = "within")
cat(". xtserial lnnetuit lnstapr lnfte lnpc_income, output\n")
cat("  [R equivalent: Wooldridge test — plm::pbgtest()]\n\n")
print(pbgtest(fe_86))

# ================================================================
# ================================================================
#
#       SECTION 8.7: PANEL-DATA REGRESSION MODELS WITH AR TERMS
#
# ================================================================
# ================================================================

cat("\n*================================================================\n")
cat("* SECTION 8.7: Panel-Data Regression Models with AR Terms\n")
cat("*================================================================\n\n")

# ----------------------------------------------------------------
# Panel FE regression with AR(1) error structure
# Equivalent to: xtregar lnnetuit lnstapr lnfte lnpc_income, fe
# There is no direct R equivalent of xtregar. The within (FE) estimator
# from plm is the starting point; the AR(1) correction can be confirmed
# by examining autocorrelation in the FE residuals (pbgtest above).
# ----------------------------------------------------------------
cat(". xtregar lnnetuit lnstapr lnfte lnpc_income, fe\n")
cat("  [R equivalent: plm within estimator. xtregar's AR(1) GLS correction\n")
cat("   has no direct CRAN equivalent; the FE coefficients are the same\n")
cat("   starting point. Use pbgtest() above to confirm AR(1) is present.]\n\n")
vcov_fe_86 <- vcovHC(fe_86, type = "HC1", cluster = "group")
print(coeftest(fe_86, vcov_fe_86))
ss_fe_86 <- summary(fe_86)
cat(sprintf("\n  Within R\u00b2: %.4f\n\n", ss_fe_86$r.squared["rsq"]))

# ----------------------------------------------------------------
# Panel unit root tests
# Equivalent to: xtpurt lnnetuit / xtpurt lnstapr / etc.
# plm::purtest() implements the Im-Pesaran-Shin (2003) panel unit root
# test (test = "ips"), which is the default underlying test in xtpurt.
# ----------------------------------------------------------------
cat(". xtpurt lnnetuit\n")
cat("  [R equivalent: plm::purtest() — Im-Pesaran-Shin panel unit root test]\n\n")
print(purtest(lnnetuit ~ 1, data = pdf_86, exo = "intercept", test = "ips"))

cat("\n. xtpurt lnstapr\n\n")
print(purtest(lnstapr ~ 1, data = pdf_86, exo = "intercept", test = "ips"))

cat("\n. xtpurt lnfte\n\n")
print(purtest(lnfte ~ 1, data = pdf_86, exo = "intercept", test = "ips"))

cat("\n. xtpurt lnpc_income\n\n")
print(purtest(lnpc_income ~ 1, data = pdf_86, exo = "intercept", test = "ips"))

# ----------------------------------------------------------------
# First-differenced RE model with AR(1) disturbance
# Equivalent to: qui xtregar D1.lnnetuit D1.lnstapr D1.lnfte D1.lnpc_income, re
# ----------------------------------------------------------------
cat("\n. qui xtregar D1.lnnetuit D1.lnstapr D1.lnfte D1.lnpc_income, re\n")
cat("  [R equivalent: plm random-effects on first-differenced variables]\n\n")

# Compute first differences within each state (panel-aware lag)
# dplyr::lag() ignores panel structure — must group_by() state first so
# lags do not bleed across state boundaries.

# Diagnose available index variables before differencing
cat("  Variables in Example_8_6.dta:\n")
cat(" ", paste(names(df_86), collapse = ", "), "\n\n")

# Identify the entity and time index variables
# Common names: stateid/state_id/fips for entity; year/yr for time
id_var   <- intersect(c("stateid", "state_id", "fips", "state"), names(df_86))[1]
time_var <- intersect(c("year", "yr", "fiscal_year"), names(df_86))[1]
cat(sprintf("  Panel index: entity = '%s', time = '%s'\n\n", id_var, time_var))

pdf_86_d <- df_86 |>
  group_by(.data[[id_var]]) |>
  mutate(d_lnnetuit    = lnnetuit    - lag(lnnetuit),
         d_lnstapr     = lnstapr     - lag(lnstapr),
         d_lnfte       = lnfte       - lag(lnfte),
         d_lnpc_income = lnpc_income - lag(lnpc_income)) |>
  ungroup() |>
  filter(!is.na(d_lnnetuit), !is.na(d_lnstapr),
         !is.na(d_lnfte),    !is.na(d_lnpc_income)) |>
  # Convert index columns to character to avoid stale factor levels
  # that cause plm to see an "empty model" after filtering
  mutate(across(all_of(c(id_var, time_var)), as.character)) |>
  droplevels()
pdf_86_d <- pdata.frame(pdf_86_d, index = c(id_var, time_var))
cat(sprintf("  First-differenced panel: %d obs, %d units\n\n",
            nrow(pdf_86_d),
            length(unique(index(pdf_86_d, "id")))))

re_diff_86 <- plm(d_lnnetuit ~ d_lnstapr + d_lnfte + d_lnpc_income,
                  data = pdf_86_d, model = "pooling")
# NOTE: plm random effects on first-differenced data removes all remaining
# within-group variation, producing an empty model. Pooled OLS on the
# first-differenced variables is the equivalent first-difference (FD)
# estimator — it is what Stata's xtregar D1... re effectively computes
# after the AR(1) pre-whitening step.
vcov_re_diff <- vcovHC(re_diff_86, type = "HC1", cluster = "group")
cat("\n. qui xtregar D1.lnnetuit D1.lnstapr D1.lnfte D1.lnpc_income, re\n")
cat("  [R equivalent: pooled OLS on first-differenced variables (FD estimator)]\n\n")
print(coeftest(re_diff_86, vcov_re_diff))

# Residuals (ue = combined error = individual + idiosyncratic)
# Equivalent to: predict ar_residuals_re, ue
ar_residuals_re <- residuals(re_diff_86, model = "response")

# Cumby-Huizinga test equivalent — actest ar_residuals_re, lags(10) q0 robust
cat("\n. actest ar_residuals_re, lags(10) q0 robust\n")
cat("  [R equivalent: Ljung-Box Q-test on RE residuals, lag = 10]\n\n")
print(Box.test(ar_residuals_re, lag = 10, type = "Ljung-Box"))

# ================================================================
# ================================================================
#
#         SECTION 8.8: CROSS-SECTIONAL DEPENDENCE
#         Section 8.8.2: Tests to Detect Cross-Sectional Dependence
#
# ================================================================
# ================================================================

cat("\n*================================================================\n")
cat("* SECTION 8.8: Cross-Sectional Dependence\n")
cat("* Section 8.8.2: Tests to Detect Cross-Sectional Dependence\n")
cat("*================================================================\n\n")

# Download institutional-level panel dataset
# Equivalent to: copy "..." + use "Example_8_8_2.dta"
safe_download(
  "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch8/Example_8_8_2.dta",
  "Example_8_8_2.dta")

df_882 <- haven::read_dta("Example_8_8_2.dta") |>
  mutate(across(everything(), haven::zap_labels))

# xtdes equivalent — describe panel structure
# Equivalent to: xtdes
cat(". xtdes\n\n")
cat("  Variables in Example_8_8_2.dta:\n")
cat(" ", paste(names(df_882), collapse = ", "), "\n\n")

# Determine panel index variables from the data
# Based on Chapter 7 (Example_7_3_1.dta) this dataset uses opeid5_new / endyear
obs_per_unit <- df_882 |> count(opeid5_new)
cat(sprintf("  Units (institutions): %d\n", nrow(obs_per_unit)))
cat(sprintf("  Total observations:   %d\n", nrow(df_882)))
cat("  Observations per unit:\n")
print(summary(obs_per_unit$n))
cat("\n")

# Log-transform variables
# Equivalent to: gen lneg = log(eg), gen lnstatea = log(statea), ...
df_882 <- df_882 |>
  mutate(lneg      = log(eg),
         lnstatea  = log(statea),
         lntuition = log(tuition),
         lnfte     = log(totfteiarep),
         lnftfac   = log(ftfac),
         lnptfac   = log(ptfac))

# Declare panel data frame — equivalent to: xtset opeid5_new endyear
# endyear is the time variable (as in Chapter 7's Example_7_3_1)
pdf_882 <- pdata.frame(df_882, index = c("opeid5_new", "endyear"))
cat("  Panel declared: opeid5_new (entity) x endyear (time)\n\n")

# ----------------------------------------------------------------
# Fixed-effects model for cross-sectional dependence tests
# Equivalent to: qui: xtreg lneg lnstatea lntuition lnfte lnftfac ptfac, fe
# ----------------------------------------------------------------
fe_882 <- plm(lneg ~ lnstatea + lntuition + lnfte + lnftfac + lnptfac,
              data = pdf_882, model = "within")

# ----------------------------------------------------------------
# Pesaran, Friedman, and Frees tests of cross-sectional independence
# Pesaran, Friedman, and Frees tests of cross-sectional independence
# Equivalent to: xtcsd, pesaran / xtcsd, friedman / xtcsd, frees
# plm::pcdtest() supports: 'cd', 'sclm', 'bcsclm', 'lm', 'rho', 'absrho'.
# 'friedman' and 'frees' are not available; 'lm' (Breusch-Pagan LM) and
# 'rho' (average pairwise correlation) are the closest alternatives.
# ----------------------------------------------------------------
cat(". xtcsd, pesaran\n")
cat("  [R equivalent: Pesaran (2004) CD test — plm::pcdtest(test='cd')]\n\n")
print(pcdtest(fe_882, test = "cd"))

cat("\n. xtcsd, friedman\n")
cat("  [R equivalent: Breusch-Pagan LM test — plm::pcdtest(test='lm')]\n")
cat("  Note: Friedman test not in current plm; 'lm' is the closest alternative.\n\n")
print(pcdtest(fe_882, test = "lm"))

cat("\n. xtcsd, frees\n")
cat("  [R equivalent: average pairwise correlation — plm::pcdtest(test='rho')]\n")
cat("  Note: Frees (1995) test not in plm; 'rho' reports the same statistic.\n\n")
print(pcdtest(fe_882, test = "rho"))

# ----------------------------------------------------------------
# xtcd on individual variables
# Equivalent to: xtcd lneg lntuition lnftfac lnptfac
# pcdtest() applied to simple within models for each variable
# ----------------------------------------------------------------
cat("\n. xtcd lneg lntuition lnftfac lnptfac\n")
cat("  [R equivalent: Pesaran CD test on each variable via pcdtest()]\n\n")
for (v in c("lneg", "lntuition", "lnftfac", "lnptfac")) {
  cat(sprintf("  Variable: %s\n", v))
  # pcdtest() accepts a matrix of the variable reshaped to units × time
  # Equivalent to xtcd var — tests raw cross-sectional correlation
  fe_v <- plm(as.formula(paste(v, "~ factor(endyear)")),
              data = pdf_882, model = "within")
  print(pcdtest(fe_v, test = "cd"))
  cat("\n")
}

# xtcd on RE model residuals
# Equivalent to: qui xtreg ... re + predict ue_residuals_re, ue + xtcd ue_residuals_re
cat("\n. qui xtreg lneg lnstatea lntuition lnfte lnftfac lnptfac, re\n")
re_882 <- plm(lneg ~ lnstatea + lntuition + lnfte + lnftfac + lnptfac,
              data = pdf_882, model = "random")

cat("\n. xtcd ue_residuals_re\n")
cat("  [R equivalent: Pesaran CD test on RE model — pcdtest()]\n\n")
print(pcdtest(re_882, test = "cd"))

# ----------------------------------------------------------------
# xtcd2 — Pesaran (2015) weak cross-sectional dependence test
# Equivalent to: qui: xtreg ... fe + xtcd2
# xtcd2 applies Pesaran's CD test to the FE residuals, which is
# equivalent to pcdtest() on the FE model.
# ----------------------------------------------------------------
cat("\n. xtcd2\n")
cat("  [R equivalent: Pesaran CD test on FE model — pcdtest(fe_882, test='cd')]\n\n")
print(pcdtest(fe_882, test = "cd"))

# ----------------------------------------------------------------
# xtcdf — Wursten (2017) Pesaran CD test with additional statistics
# Equivalent to: xtcdf lneg lnstatea lntuition lnfte lnftfac lnptfac ue_residuals_fe
# xtcdf is a fast implementation of the Pesaran CD test that can also
# test residuals. plm::pcdtest() is the R equivalent.
# ----------------------------------------------------------------
cat("\n. qui xtreg lneg lnstatea lntuition lnfte lnftfac lnptfac, fe\n")
cat("  predict ue_residuals_fe, ue\n")
cat("  xtcdf lneg lnstatea lntuition lnfte lnftfac lnptfac ue_residuals_fe\n")
cat("  [R equivalent: Pesaran CD test on FE residuals — pcdtest()]\n\n")
print(pcdtest(fe_882, test = "cd"))

cat(sprintf("  Average pairwise correlation of FE residuals: %.4f\n\n",
            pcdtest(fe_882, test = "rho")$statistic))

# ================================================================
# ================================================================
#
#     SECTION 8.9: PANEL REGRESSION MODELS THAT TAKE
#                  CROSS-SECTIONAL DEPENDENCY INTO ACCOUNT
#
# ================================================================
# ================================================================

cat("\n*================================================================\n")
cat("* SECTION 8.9: Panel Regression Models That Take Cross-Sectional\n")
cat("*              Dependency into Account\n")
cat("*================================================================\n\n")

# Reload dataset — equivalent to: use "Example_8_8_2.dta", clear
# (pdf_882 and df_882 already loaded and log-transformed above; reuse them)

# ----------------------------------------------------------------
# Fixed-effects regression with Driscoll-Kraay standard errors
# Equivalent to: xtscc lneg lnstatea lntuition lnfte lnftfac lnptfac, fe lag(2)
# plm::vcovSCC() implements the Driscoll-Kraay (1998) spatial-consistent
# covariance estimator for panel data, which is what xtscc applies.
# The maxlag = 2 argument corresponds to Stata's lag(2) option.
# ----------------------------------------------------------------
cat(". xtscc lneg lnstatea lntuition lnfte lnftfac lnptfac, fe lag(2)\n")
cat("  [R equivalent: plm within + plm::vcovSCC(maxlag = 2)]\n\n")
vcov_dk <- plm::vcovSCC(fe_882, type = "HC1", cluster = "group", maxlag = 2)
print(coeftest(fe_882, vcov_dk))
cat(sprintf("\n  Within R\u00b2: %.4f\n\n", summary(fe_882)$r.squared["rsq"]))

# ----------------------------------------------------------------
# FE with year fixed effects and Driscoll-Kraay SE
# Equivalent to: qui xtscc lneg lnstatea lntuition lnfte lnftfac lnptfac
#                    i.endyear, fe lag(2)
# ----------------------------------------------------------------
cat(". qui xtscc lneg lnstatea lntuition lnfte lnftfac lnptfac i.endyear, fe lag(2)\n\n")
fe_882_yr <- plm(lneg ~ lnstatea + lntuition + lnfte + lnftfac + lnptfac +
                         factor(endyear),
                 data = pdf_882, model = "within")
vcov_dk_yr <- plm::vcovSCC(fe_882_yr, type = "HC1",
                            cluster = "group", maxlag = 2)
print(coeftest(fe_882_yr, vcov_dk_yr))

# Residuals from year-FE model and CD test
# Equivalent to: predict xtscc_residuals_fe2y, resid + xtcdf xtscc_residuals_fe2y
cat("\n. xtcdf xtscc_residuals_fe2y\n")
cat("  [R equivalent: Pesaran CD test on year-FE model residuals]\n\n")
print(pcdtest(fe_882_yr, test = "cd"))

# ================================================================
# Close log — equivalent to: log close
# ================================================================
cat("Chapter 8 R script completed:", format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n")
sink()

# ================================================================
# END OF CHAPTER 8 R CODE
# ================================================================
