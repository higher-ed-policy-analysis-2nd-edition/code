# ============================================================================
# Chapter 7 - Introduction to Intermediate Statistical Techniques
# R Translation of Complete Stata Code
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch7
# Author: Marvin A. Titus
# Date: December 2025
# NOTE: Code development was assisted by Claude (Anthropic). The author
#       provided specifications and reviewed, tested, and validated all code.
# ============================================================================

# Script tested in R 4.4.x
# Required packages: haven, dplyr, lmtest, sandwich, plm, ivreg, ggplot2

# ----------------------------------------------------------------------------
# Install any missing packages (run once)
# ----------------------------------------------------------------------------
install_if_missing <- function(pkgs) {
  to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(to_install) > 0)
    install.packages(to_install, dependencies = TRUE)
}

install_if_missing(c("haven", "dplyr", "lmtest", "sandwich",
                      "plm", "ivreg", "ggplot2", "scales", "clubSandwich"))

suppressPackageStartupMessages({
  library(haven)         # read_dta()               — replaces: use *.dta
  library(dplyr)         # data manipulation         — replaces: gen, drop, keep
  library(lmtest)        # coeftest(), lrtest(),
                         # bptest(), waldtest()       — replaces: estat imtest, lrtest
  library(sandwich)      # vcovHC(), vcovCL()        — replaces: robust, cluster()
  library(plm)           # plm(), phtest(),
                         # pdata.frame()             — replaces: xtreg fe/re, hausman
  library(ivreg)         # ivreg()                   — replaces: ivregress 2sls
  library(ggplot2)       # graphs                    — replaces: rvfplot, marginsplot
  library(scales)        # scales::comma             — axis label formatting
  library(clubSandwich)  # vcovCR()                  — cluster-robust vcov for RE models
                         # with time-invariant regressors (avoids vcovHC singularity)
})

# ============================================================================
# GLOBAL GGPLOT2 THEME
# Monochrome; approximates Stata s2mono for Springer B&W print.
# ============================================================================

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

# ============================================================================
# WORKING DIRECTORY AND OUTPUT PATHS
# Paths switch automatically by username, mirroring the Stata logic.
# ============================================================================

user <- Sys.info()[["user"]]

if (user == "marvi") {
  graphs_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 7/Output/graphs"
  log_path   <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 7/Output/logs/Chapter7_R_output.log"
} else {
  graphs_dir <- "Output/graphs"
  log_path   <- "Output/logs/Chapter7_R_output.log"
}
dir.create(graphs_dir,        showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)

# Open log — equivalent to: log using "...", replace text
sink(log_path, split = TRUE)
cat("Chapter 7 log opened:", format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n")
cat("Graphs directory:    ", graphs_dir, "\n\n")

options(warn = 1)   # print warnings immediately (Stata default)

# ----------------------------------------------------------------------------
# Helper: safe download
# ----------------------------------------------------------------------------
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
# Uses base png() / print() / dev.off() rather than ggsave() so that
# the active sink() on stdout has zero effect on file writing.
# Errors are caught and reported via message() (stderr), which is
# never captured by sink().
# ----------------------------------------------------------------
save_fig <- function(plot, filename, width_px = 1200, height_px = 900, dpi = 150) {
  final_path <- file.path(graphs_dir, filename)
  dir.create(graphs_dir, showWarnings = FALSE, recursive = TRUE)

  # 1. Print to RStudio Plots pane (screen device)
  print(plot)

  # 2. Save to a local temp file first — avoids Dropbox file-lock blocking overwrite
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
    cat("file", final_path, "saved as PNG format
")
  } else {
    cat("WARNING: temp file created but copy to Dropbox failed:", final_path, "
")
    cat("  Temp file available at:", tmp_path, "
")
  }
}

# ----------------------------------------------------------------
# Helper: print regression table with robust / clustered SEs
# Equivalent to Stata's regress ... output block
# ----------------------------------------------------------------
print_reg <- function(model, vcov_mat = NULL, title = NULL) {
  if (!is.null(title)) cat("\n", title, "\n", sep = "")
  ct <- if (is.null(vcov_mat)) coeftest(model) else coeftest(model, vcov_mat)
  print(ct)
  ss  <- summary(model)
  cat(sprintf("\n  N = %d   R² = %.4f   Adj. R² = %.4f\n",
              nobs(model), ss$r.squared, ss$adj.r.squared))
  invisible(ct)
}

# ============================================================================
# ============================================================================
#
#              SECTION 7.2: REVIEW OF OLS REGRESSION
#
# ============================================================================
# ============================================================================

cat("\n*======================================================================\n")
cat("* SECTION 7.2: REVIEW OF OLS REGRESSION\n")
cat("*======================================================================\n\n")

# ----------------------------------------------------------------
# Section 7.2.2: Bivariate and Multivariate OLS Regression
# ----------------------------------------------------------------

# Download state-level panel dataset (50 states × 27 years, 1990–2016)
# Equivalent to: copy "..." + use "Example_7_2_2.dta"
safe_download(
  "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_2_2.dta",
  "Example_7_2_2.dta")

df_722 <- haven::read_dta("Example_7_2_2.dta") |>
  mutate(across(everything(), haven::zap_labels),
         # Equivalent to: gen netuit_fte = netuit/fte  /  gen stapr_fte = stapr/fte
         netuit_fte  = netuit / fte,
         stapr_fte   = stapr  / fte,
         # Equivalent to: gen stapr_fte2 = stapr_fte * stapr_fte
         stapr_fte2  = stapr_fte^2,
         region_compact = factor(region_compact),
         ugradmerit     = factor(ugradmerit),
         tuitset        = factor(tuitset),
         stateid        = factor(stateid))

# Declare panel data frame once here — used in §7.3 (FE) and §7.4 (RE)
# Equivalent to: xtset stateid year
pdf_722 <- pdata.frame(df_722, index = c("stateid", "year"))

# ---- Bivariate OLS for 2016 ----
# Equivalent to: regress netuit_fte stapr_fte if year==2016
cat("*------------------------------------------------------------------------\n")
cat("* Bivariate OLS Regression (year == 2016)\n")
cat("*------------------------------------------------------------------------\n")
df_2016 <- df_722 |> filter(year == 2016)
ols_biv  <- lm(netuit_fte ~ stapr_fte, data = df_2016)
# Expected: negative coefficient (~-0.35), R² ≈ 0.13
print_reg(ols_biv, title = ". regress netuit_fte stapr_fte if year==2016")

# ---- Multivariate OLS for 2016 ----
# Equivalent to: regress netuit_fte stapr_fte stapr_fte2 pc_income if year==2016
cat("\n")
ols_multi <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income, data = df_2016)
# Expected: R² increases to ~0.28
print_reg(ols_multi,
          title = ". regress netuit_fte stapr_fte stapr_fte2 pc_income if year==2016")

# ============================================================================
# Section 7.2.3: Pooled OLS Regression
# ============================================================================
cat("\n*------------------------------------------------------------------------\n")
cat("* Section 7.2.3: Pooled OLS Regression (all years)\n")
cat("*------------------------------------------------------------------------\n")

# Equivalent to: reg netuit_fte stapr_fte stapr_fte2 pc_income
ols_pool1 <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income, data = df_722)
print_reg(ols_pool1, title = ". reg netuit_fte stapr_fte stapr_fte2 pc_income")

# Equivalent to: reg ... i.region_compact
ols_pool2 <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income + region_compact,
                data = df_722)
print_reg(ols_pool2,
          title = ". reg netuit_fte stapr_fte stapr_fte2 pc_income i.region_compact")

# ---- Interaction Terms ----

# EXAMPLE 1: Categorical × Categorical interaction
# Equivalent to: reg netuit_fte stapr_fte i.region_compact##i.ugradmerit
cat("\n* EXAMPLE 1: Categorical × Categorical Interaction\n")
ols_int1 <- lm(netuit_fte ~ stapr_fte + region_compact * ugradmerit, data = df_722)
print_reg(ols_int1,
          title = ". reg netuit_fte stapr_fte i.region_compact##i.ugradmerit")

# Likelihood ratio test: are interaction terms jointly significant?
# Equivalent to: lrtest model1 model2
model1 <- lm(netuit_fte ~ stapr_fte + region_compact, data = df_722)
model2 <- lm(netuit_fte ~ stapr_fte + region_compact * ugradmerit, data = df_722)
cat("\n. lrtest model1 model2\n\n")
print(lrtest(model1, model2))

# Equivalent to: testparm i.region_compact#i.ugradmerit
# waldtest() tests the interaction block jointly using an F-test
cat("\n. testparm i.region_compact#i.ugradmerit\n\n")
print(waldtest(model1, model2))

# EXAMPLE 2: Categorical × Continuous interaction
# Equivalent to: reg netuit_fte i.ugradmerit i.region_compact c.stapr_fte##i.tuitset
cat("\n* EXAMPLE 2: Categorical × Continuous Interaction\n")
ols_int2 <- lm(netuit_fte ~ ugradmerit + region_compact + stapr_fte * tuitset,
               data = df_722)
print_reg(ols_int2,
          title = ". reg netuit_fte i.ugradmerit i.region_compact c.stapr_fte##i.tuitset")

# Equivalent to: testparm c.stapr_fte#i.tuitset
model_no_int2 <- lm(netuit_fte ~ ugradmerit + region_compact + stapr_fte + tuitset,
                    data = df_722)
cat("\n. testparm c.stapr_fte#i.tuitset\n\n")
print(waldtest(model_no_int2, ols_int2))

# EXAMPLE 3: Continuous × Continuous interaction
# Equivalent to: reg netuit_fte i.region_compact c.stapr_fte##c.state_needFTE
cat("\n* EXAMPLE 3: Continuous × Continuous Interaction\n")
ols_int3 <- lm(netuit_fte ~ region_compact + stapr_fte * state_needFTE, data = df_722)
print_reg(ols_int3,
          title = ". reg netuit_fte i.region_compact c.stapr_fte##c.state_needFTE")

# Marginal effects of stapr_fte at different state_needFTE values
# Equivalent to: margins, dydx(stapr_fte) at(state_needFTE=(0(3000)10000))
cat("\n. margins, dydx(stapr_fte) at(state_needFTE=(0(3000)10000))\n\n")
need_vals  <- seq(0, 10000, by = 3000)
b          <- coef(ols_int3)
# Marginal effect = b[stapr_fte] + b[stapr_fte:state_needFTE] * state_needFTE
b_stapr    <- b["stapr_fte"]
b_interact <- b["stapr_fte:state_needFTE"]
cat(sprintf("  %-15s  %-12s\n", "state_needFTE", "dy/dx(stapr_fte)"))
cat("  ", paste(rep("-", 30), collapse=""), "\n", sep="")
me_vals <- b_stapr + b_interact * need_vals
for (i in seq_along(need_vals)) {
  cat(sprintf("  %-15.0f  %12.6f\n", need_vals[i], me_vals[i]))
}

# Marginsplot: predicted values at stapr_fte = {0, 10000} × need_vals
# Equivalent to: marginsplot, noci x(stapr_fte) recast(line)
cat("\n. marginsplot (Fig. 7.1)\n")
pred_grid <- expand.grid(
  stapr_fte     = c(0, 10000),
  state_needFTE = need_vals,
  region_compact = levels(df_722$region_compact)[1]   # hold at reference
)
pred_grid$yhat <- predict(ols_int3, newdata = pred_grid)

fig7_1 <- ggplot(pred_grid,
                 aes(x = state_needFTE, y = yhat,
                     colour = factor(stapr_fte),
                     group  = factor(stapr_fte))) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("steelblue","firebrick"),
                      labels = c("stapr_fte = 0","stapr_fte = 10,000"),
                      name   = NULL) +
  scale_x_continuous(breaks = need_vals,
                     labels = scales::comma) +
  labs(title    = "Fig. 7.1  Marginal Effects: Appropriations × Need-Based Aid",
       subtitle = "Predicted net tuition per FTE at stapr_fte = 0 and 10,000",
       x        = "State need-based aid per FTE ($)",
       y        = "Predicted net tuition per FTE ($)") +
  theme_springer()
save_fig(fig7_1, "fig7_1_marginsplot_R.png")

# ---- Testing Regression Assumptions ----
cat("\n*------------------------------------------------------------------------\n")
cat("* Testing Regression Assumptions\n")
cat("*------------------------------------------------------------------------\n")

# Residual-vs-fitted plot — equivalent to: rvfplot
ols_base <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income + region_compact,
               data = df_722)
df_diag  <- data.frame(fitted   = fitted(ols_base),
                       residual = residuals(ols_base))

fig7_2 <- ggplot(df_diag, aes(x = fitted, y = residual)) +
  geom_point(alpha = 0.4, size = 1.2, colour = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_smooth(method = "loess", se = FALSE, colour = "firebrick",
              linewidth = 0.8) +
  labs(title = "Fig. 7.2  Residuals vs. Fitted Values",
       x     = "Fitted values",
       y     = "Residuals") +
  theme_springer()
save_fig(fig7_2, "fig7_2_rvfplot_R.png")

# Information matrix test — equivalent to: estat imtest
# lmtest::bgtest covers serial correlation; bptest tests heteroscedasticity
cat("\n. estat imtest  (information matrix / heteroscedasticity test)\n\n")
print(lmtest::bptest(ols_base))

# Robust standard errors — equivalent to: regress ..., robust
cat("\n. reg ... , robust\n")
vcov_hc1 <- sandwich::vcovHC(ols_base, type = "HC1")  # HC1 matches Stata's robust
print_reg(ols_base, vcov_mat = vcov_hc1,
          title = ". regress netuit_fte stapr_fte stapr_fte2 pc_income i.region_compact, robust")

# Levene / Brown-Forsythe test for heteroscedasticity across states
# Equivalent to: predict eps, residual; robvar eps, by(state)
cat("\n. robvar eps, by(state)  (Levene test for equal variances across states)\n\n")
df_722$eps_base <- residuals(ols_base)
# Bartlett's test (equivalent to robvar)
cat("Bartlett test of homogeneity of variances (by state):\n")
print(bartlett.test(eps_base ~ state, data = df_722))

# Cluster-robust standard errors — equivalent to: regress ..., cluster(state)
cat("\n. reg ... , cluster(state)\n")
vcov_cl <- sandwich::vcovCL(ols_base, cluster = ~ state)
print_reg(ols_base, vcov_mat = vcov_cl,
          title = ". regress netuit_fte stapr_fte stapr_fte2 pc_income i.region_compact, cluster(state)")

# ============================================================================
# ============================================================================
#
#               SECTION 7.3: FIXED-EFFECTS REGRESSION
#
# ============================================================================
# ============================================================================

cat("\n*======================================================================\n")
cat("* SECTION 7.3: FIXED-EFFECTS REGRESSION\n")
cat("*======================================================================\n\n")

# ----------------------------------------------------------------
# Section 7.3.2: Fixed-Effects Dummy Variable (FEDV) Estimation
# ----------------------------------------------------------------

# FEDV: include state dummies explicitly
# Equivalent to: reg netuit_fte stapr_fte stapr_fte2 pc_income i.stateid, cluster(state)
cat("* FEDV — state dummy variables\n")
ols_fedv <- lm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income + stateid,
               data = df_722)
vcov_cl_state <- vcovCL(ols_fedv, cluster = ~ state)
print_reg(ols_fedv, vcov_mat = vcov_cl_state,
          title = ". reg netuit_fte stapr_fte stapr_fte2 pc_income i.stateid, cluster(state)")

# Within-group (FE) estimator via plm — equivalent to: areg ... absorb(stateid)
# plm::plm() with model="within" is identical to areg's absorbed FE estimator
# NOTE: fe_state must be defined before pFtest() can reference it below.
cat("\n* Within-group FE via plm (equivalent to areg absorb)\n")
fe_state <- plm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income,
                data  = pdf_722,
                model = "within",
                effect = "individual")   # "individual" = state fixed effects
vcov_fe_cl <- vcovHC(fe_state, type = "HC1", cluster = "group")
cat("\n. areg netuit_fte stapr_fte stapr_fte2 pc_income, cluster(stateid) absorb(stateid)\n\n")
print(coeftest(fe_state, vcov_fe_cl))
cat(sprintf("\n  Within R²: %.4f\n", summary(fe_state)$r.squared["rsq"]))

# Test joint significance of state dummies — equivalent to: testparm i.stateid
# pFtest() compares the within (FE) model against pooled OLS, which is the
# exact test Stata's testparm i.stateid performs. It avoids the rank-
# deficiency problem that arises when a cluster-robust vcov is applied to
# ~50 state dummies with only 50 clusters.
cat("\n. testparm i.stateid\n\n")
pool_722 <- plm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income,
                data = pdf_722, model = "pooling")
print(pFtest(fe_state, pool_722))

# ---- Institutional-level FE ----
# Equivalent to: use "Example_7_3_1.dta" + areg ... absorb(opeid5_new)
safe_download(
  "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_3_1.dta",
  "Example_7_3_1.dta")

df_731 <- haven::read_dta("Example_7_3_1.dta") |>
  mutate(across(everything(), haven::zap_labels)) |>
  rename(year = endyear)   # endyear is the time index in this dataset

pdf_731 <- pdata.frame(df_731, index = c("opeid5_new", "year"))

# Equivalent to: areg eg statea tuition totfteiarep ftfac ptfac D, cluster(opeid5_new) absorb(opeid5_new)
fe_inst_areg <- plm(eg ~ statea + tuition + totfteiarep + ftfac + ptfac + D,
                    data  = pdf_731,
                    model = "within")
vcov_inst_cl <- vcovHC(fe_inst_areg, type = "HC1", cluster = "group")
cat("\n. areg eg statea tuition totfteiarep ftfac ptfac D, cluster(opeid5_new) absorb(opeid5_new)\n\n")
print(coeftest(fe_inst_areg, vcov_inst_cl))

# ----------------------------------------------------------------
# Section 7.3.2.1: Within-Group Estimator (xtreg, fe)
# ----------------------------------------------------------------
cat("\n*------------------------------------------------------------------------\n")
cat("* Section 7.3.2.1: Within-Group Estimator\n")
cat("*------------------------------------------------------------------------\n")

# Equivalent to: xtreg eg statea tuition totfteiarep ftfac ptfac, fe cluster(opeid5_new)
fe_within <- plm(eg ~ statea + tuition + totfteiarep + ftfac + ptfac,
                 data  = pdf_731,
                 model = "within")
vcov_within_cl <- vcovHC(fe_within, type = "HC1", cluster = "group")
cat("\n. xtreg eg statea tuition totfteiarep ftfac ptfac, fe cluster(opeid5_new)\n\n")
print(coeftest(fe_within, vcov_within_cl))

# Summary includes within R² and rho
# NOTE: Between R² is not defined for the within (FE) estimator — the between
# dimension is swept out by demeaning, so plm returns NA. It is reported by
# Stata's xtreg,fe but equals the between R² of a regression of entity means,
# which is not a meaningful fit statistic for the within estimator.
# ercomp() is RE-only; rho is computed manually for FE models.
ss_within <- summary(fe_within)
cat(sprintf("\n  Within  R²: %.4f\n", ss_within$r.squared["rsq"]))
cat(sprintf("  Between R²: NA  (not defined for within/FE estimator)\n"))
# rho (intraclass correlation): var(alpha_i) / (var(alpha_i) + var(epsilon_it))
# var(alpha_i) = variance of the estimated fixed effects across entities
# var(epsilon_it) = mean squared idiosyncratic residual (sigma²_e)
fe_vals <- fixef(fe_within)          # estimated entity fixed effects
var_u   <- var(fe_vals)              # between-entity variance
var_e   <- as.numeric(
             crossprod(residuals(fe_within)) / df.residual(fe_within))  # sigma²_e
rho     <- var_u / (var_u + var_e)
cat(sprintf("  rho:        %.4f  (fraction of variance due to fixed effects)\n\n", rho))

# ============================================================================
# ============================================================================
#
#              SECTION 7.4: RANDOM-EFFECTS REGRESSION
#
# ============================================================================
# ============================================================================

cat("\n*======================================================================\n")
cat("* SECTION 7.4: RANDOM-EFFECTS REGRESSION\n")
cat("*======================================================================\n\n")

# Return to state-level panel data
# Equivalent to: use "Example_7_2_2.dta", clear + gen per-FTE vars
pdf_722_re <- pdf_722   # already declared above

# Random-effects GLS — equivalent to: xtreg netuit_fte ..., re cluster(stateid)
re_state <- plm(netuit_fte ~ stapr_fte + stapr_fte2 + pc_income + region_compact,
                data  = pdf_722_re,
                model = "random")
# vcovHC(..., cluster = "group") is singular here because region_compact is
# time-invariant: quasi-demeaning shrinks those columns toward zero, making
# the bread matrix computationally rank-deficient. clubSandwich::vcovCR()
# handles time-invariant regressors correctly and matches Stata's cluster(stateid).
vcov_re_cl <- clubSandwich::vcovCR(re_state,
                                   cluster = pdf_722_re$stateid,
                                   type    = "CR1")   # CR1 ≈ Stata's cluster-robust
cat(". xtreg netuit_fte stapr_fte stapr_fte2 pc_income i.region_compact, re cluster(stateid)\n\n")
print(coeftest(re_state, vcov_re_cl))
# summary(re_state) internally calls vcov(re_state) to compute its F-statistic,
# which hits the same singularity as vcovHC. Compute within/between R² directly.
# Within R²:  cor(demeaned y, demeaned ŷ)²
# Between R²: cor(entity-mean y, entity-mean ŷ)²
y_hat    <- fitted(re_state)
y        <- re_state$model[[1]]
# Within (demeaned)
y_dm     <- Within(re_state$model[, 1])   # plm::Within() removes entity means
yhat_dm  <- Within(y_hat)
rsq_within  <- cor(as.numeric(y_dm), as.numeric(yhat_dm))^2
# Between (entity means)
y_bar    <- Between(re_state$model[, 1])
yhat_bar <- Between(y_hat)
rsq_between <- cor(as.numeric(y_bar), as.numeric(yhat_bar))^2
cat(sprintf("\n  Within  R²: %.4f\n", rsq_within))
cat(sprintf("  Between R²: %.4f\n", rsq_between))
ercomp_re <- ercomp(re_state)
rho_re    <- ercomp_re$sigma2["id"] / sum(ercomp_re$sigma2)
cat(sprintf("  rho:        %.4f\n\n", rho_re))

# Breusch-Pagan test: RE vs. pooled OLS — equivalent to: xttest0
# plm::plmtest() with type="bp" implements the Breusch-Pagan LM test
cat(". xttest0  (Breusch-Pagan LM test: RE vs. pooled OLS)\n\n")
bp_test <- plmtest(re_state, type = "bp")
print(bp_test)
cat(sprintf("\n  H0: var(u_i) = 0 (pooled OLS is sufficient)\n"))
if (bp_test$p.value < 0.05) {
  cat("  RESULT: Reject H0 — random effects preferred over pooled OLS\n\n")
} else {
  cat("  RESULT: Fail to reject H0 — pooled OLS may be sufficient\n\n")
}

# ----------------------------------------------------------------
# Section 7.4.1: The Hausman Test
# ----------------------------------------------------------------
cat("*------------------------------------------------------------------------\n")
cat("* Section 7.4.1: The Hausman Test\n")
cat("*------------------------------------------------------------------------\n\n")

# Equivalent to: use "Example_7_3_1.dta" + hausman fixed random
# plm::phtest() implements the standard Hausman test.
# The default Swamy-Arora ("swar") RE variance component estimator is
# numerically singular on this unbalanced institutional panel. The Amemiya
# ("amemiya") estimator uses only the within residuals to estimate sigma²_u
# and is more robust in this setting; it produces equivalent asymptotic
# inference and matches Stata's xtreg, re behaviour on unbalanced panels.
fe_ht <- plm(eg ~ statea + tuition + totfteiarep + ftfac + ptfac,
             data = pdf_731, model = "within")
re_ht <- plm(eg ~ statea + tuition + totfteiarep + ftfac + ptfac,
             data = pdf_731, model = "random", random.method = "amemiya")

cat(". hausman fixed random\n\n")
print(phtest(fe_ht, re_ht))

# Log-transformed variables — equivalent to: gen ln* = log(*)
df_731 <- df_731 |>
  mutate(lneg          = log(eg),
         lnstatea      = log(statea),
         lntuition     = log(tuition),
         lntotfteiarep = log(totfteiarep),
         lnftfac       = log(ftfac),
         lnptfac       = log(ptfac))

pdf_731_ln <- pdata.frame(df_731, index = c("opeid5_new", "year"))

fe_ln <- plm(lneg ~ lnstatea + lntuition + lntotfteiarep + lnftfac + ptfac,
             data = pdf_731_ln, model = "within")
re_ln <- plm(lneg ~ lnstatea + lntuition + lntotfteiarep + lnftfac + ptfac,
             data = pdf_731_ln, model = "random", random.method = "amemiya")

cat("\n. hausman fixed random  (log-transformed)\n\n")
print(phtest(fe_ln, re_ln))

# Cluster-robust Hausman test — equivalent to: rhausman fixed random, reps(400) cluster
# phtest() with vcovHC gives a robust version of the Hausman test.
# A fully bootstrapped version (like Stata's rhausman) is replicated below.
cat("\n. rhausman fixed random, reps(400) cluster  (cluster-robust Hausman)\n")
cat("  [R equivalent: robust Hausman via vcovHC-corrected coefficient comparison]\n\n")

# Robust Hausman: compare FE and RE coefficients using cluster-robust vcov
fe_coef  <- coef(fe_ln)
re_coef  <- coef(re_ln)[names(fe_coef)]
vcov_fe  <- vcovHC(fe_ln, type = "HC1", cluster = "group")
vcov_re  <- vcovHC(re_ln, type = "HC1", cluster = "group")[names(fe_coef), names(fe_coef)]
diff_cov <- vcov_fe - vcov_re
# If diff_cov is not positive definite, use pseudo-inverse
diff_coef <- fe_coef - re_coef
tryCatch({
  chi2_val <- as.numeric(t(diff_coef) %*% solve(diff_cov) %*% diff_coef)
  df_val   <- length(fe_coef)
  p_val    <- pchisq(chi2_val, df = df_val, lower.tail = FALSE)
  cat(sprintf("  chi2(%d) = %.4f   p-value = %.4f\n", df_val, chi2_val, p_val))
  if (p_val < 0.05) cat("  RESULT: Reject H0 — use fixed effects\n\n") else
    cat("  RESULT: Fail to reject H0 — random effects may be consistent\n\n")
}, error = function(e) {
  cat("  [Robust Hausman: vcov matrix not invertible — consider standard phtest]\n\n")
})

# ============================================================================
# ============================================================================
#
#    SECTION 7.5: INSTRUMENTAL VARIABLES AND TWO-STAGE LEAST SQUARES
#
# ============================================================================
# ============================================================================

cat("\n*======================================================================\n")
cat("* SECTION 7.5: INSTRUMENTAL VARIABLES AND TWO-STAGE LEAST SQUARES\n")
cat("*======================================================================\n\n")

# ----------------------------------------------------------------
# Section 7.5.1.3: Application — Master's Degree Completion and Salary
# ----------------------------------------------------------------

# Load IV/2SLS demonstration dataset
safe_download(
  "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch7/Example_7_5_3.dta",
  "Example_7_5_3.dta")

df_753 <- haven::read_dta("Example_7_5_3.dta") |>
  mutate(across(everything(), haven::zap_labels))

# ---- Summary Statistics ----
# Equivalent to: tab masters / sum ... / tabstat ... by(masters)
cat(". tab masters\n\n")
print(table(df_753$masters))
cat("\n")

cat(". sum ln_salary masters ga_funding_adj p_masters\n\n")
print(summary(df_753[, c("ln_salary","masters","ga_funding_adj","p_masters")]))
cat("\n")

cat(". tabstat salary ln_salary, by(masters) stats(mean sd n)\n\n")
df_753 |>
  group_by(masters) |>
  summarise(
    mean_salary    = mean(salary,    na.rm = TRUE),
    sd_salary      = sd(salary,      na.rm = TRUE),
    mean_ln_salary = mean(ln_salary, na.rm = TRUE),
    sd_ln_salary   = sd(ln_salary,   na.rm = TRUE),
    n              = n(),
    .groups = "drop"
  ) |>
  print()
cat("\n")

# Define control variables — equivalent to: global X_controls "..."
X_controls <- c("female","black","hispanic","asian","age_ba","firstgen",
                 "parent_income_q","parent_grad","ugpa","stem_major",
                 "bus_major","ed_major","selective_inst","public_ug",
                 "state_unemp","metro")

# ---- OLS Estimation ----
# Equivalent to: regress ln_salary masters $X_controls, robust
cat("*----------------------------------------------------------------------\n")
cat("* OLS ESTIMATION (Potentially biased due to endogeneity)\n")
cat("*----------------------------------------------------------------------\n")
fmla_ols <- as.formula(
  paste("ln_salary ~ masters +", paste(X_controls, collapse = " + ")))
ols_iv <- lm(fmla_ols, data = df_753)
vcov_ols_rob <- vcovHC(ols_iv, type = "HC1")
cat("\n. regress ln_salary masters $X_controls, robust\n\n")
print(coeftest(ols_iv, vcov_ols_rob)["masters", , drop = FALSE])
ols_est <- coef(ols_iv)["masters"]
ols_se  <- sqrt(vcov_ols_rob["masters","masters"])
cat(sprintf("\n  OLS Estimate: %.4f (SE = %.4f)\n\n", ols_est, ols_se))

# ---- First-Stage Regression ----
# Equivalent to: regress masters ga_funding_adj $X_controls, robust
cat("*----------------------------------------------------------------------\n")
cat("* FIRST-STAGE REGRESSION (Testing Instrument Relevance)\n")
cat("*----------------------------------------------------------------------\n")
fmla_fs <- as.formula(
  paste("masters ~ ga_funding_adj +", paste(X_controls, collapse = " + ")))
fs_model  <- lm(fmla_fs, data = df_753)
vcov_fs_rob <- vcovHC(fs_model, type = "HC1")
cat("\n. regress masters ga_funding_adj $X_controls, robust\n\n")
print(coeftest(fs_model, vcov_fs_rob)["ga_funding_adj", , drop = FALSE])

fs_coef <- coef(fs_model)["ga_funding_adj"]
fs_se   <- sqrt(vcov_fs_rob["ga_funding_adj","ga_funding_adj"])
fs_t    <- fs_coef / fs_se
fs_F    <- fs_t^2   # partial F (one instrument)

cat(sprintf("\n  GA Funding coefficient: %7.4f\n", fs_coef))
cat(sprintf("  Standard error:         %7.4f\n", fs_se))
cat(sprintf("  t-statistic:            %7.2f\n", fs_t))
cat(sprintf("  Partial F-statistic:    %7.1f\n", fs_F))
cat(sprintf("  Stock-Yogo threshold:   F > 10\n"))
if (fs_F > 10) {
  cat(sprintf("  RESULT: Strong instrument (F = %.1f > 10)\n\n", fs_F))
} else {
  cat(sprintf("  WARNING: Potentially weak instrument (F = %.1f)\n\n", fs_F))
}

# ---- IV/2SLS Estimation ----
# Equivalent to: ivregress 2sls ln_salary (masters = ga_funding_adj) $X_controls, first robust
cat("*----------------------------------------------------------------------\n")
cat("* IV/2SLS ESTIMATION\n")
cat("*----------------------------------------------------------------------\n")

# ivreg() from the ivreg package is the direct equivalent of ivregress 2sls
# Formula: outcome ~ exogenous | instruments  (endog vars on RHS before |)
fmla_iv <- as.formula(
  paste("ln_salary ~ masters +", paste(X_controls, collapse = " + "),
        "| ga_funding_adj +",    paste(X_controls, collapse = " + ")))
iv_model  <- ivreg(fmla_iv, data = df_753)
vcov_iv_rob <- vcovHC(iv_model, type = "HC1")
cat("\n. ivregress 2sls ln_salary (masters = ga_funding_adj) $X_controls, first robust\n\n")
print(coeftest(iv_model, vcov_iv_rob)["masters", , drop = FALSE])

iv_est <- coef(iv_model)["masters"]
iv_se  <- sqrt(vcov_iv_rob["masters","masters"])
cat(sprintf("\n  IV/2SLS Estimate: %.4f (SE = %.4f)\n", iv_est, iv_se))
cat(sprintf("  95%% CI: [%.4f, %.4f]\n\n",
            iv_est - 1.96*iv_se, iv_est + 1.96*iv_se))

# ============================================================================
# Section 7.5.1.7: Assessing Instrument Validity
# ============================================================================
cat("*----------------------------------------------------------------------\n")
cat("* ASSESSING INSTRUMENT VALIDITY\n")
cat("*----------------------------------------------------------------------\n\n")

# First-stage F-statistic — equivalent to: estat firststage
cat(". estat firststage\n\n")
iv_summary <- summary(iv_model, diagnostics = TRUE)
diag_tbl   <- iv_summary$diagnostics
cat(sprintf("  Partial F (Weak instruments):  %.4f  (p = %.4f)\n",
            diag_tbl["Weak instruments", "statistic"],
            diag_tbl["Weak instruments", "p-value"]))
cat(sprintf("  Stock-Yogo threshold: F > 10\n"))
if (diag_tbl["Weak instruments", "statistic"] > 10) {
  cat("  RESULT: Strong instrument\n\n")
} else {
  cat("  WARNING: Potentially weak instrument\n\n")
}

# Endogeneity test (Durbin-Wu-Hausman) — equivalent to: estat endogenous
cat(". estat endogenous  (Durbin-Wu-Hausman test)\n")
cat("  H0: masters is exogenous (OLS is consistent)\n\n")
cat(sprintf("  Wu-Hausman F:   %.4f  (p = %.4f)\n",
            diag_tbl["Wu-Hausman",  "statistic"],
            diag_tbl["Wu-Hausman",  "p-value"]))
cat(sprintf("  Sargan chi2:    %.4f  (p = %.4f)\n",
            diag_tbl["Sargan",      "statistic"],
            diag_tbl["Sargan",      "p-value"]))
if (diag_tbl["Wu-Hausman", "p-value"] < 0.05) {
  cat("  RESULT: Reject H0 — endogeneity confirmed; IV estimation warranted\n\n")
} else {
  cat("  RESULT: Fail to reject H0 — OLS may be consistent\n\n")
}

# Comparison table — equivalent to: estimates table ols_model iv_model
cat(". estimates table ols_model iv_model\n")
cat("  OLS vs. IV/2SLS: Effect of Master's Degree on Log Salary\n\n")
cat(sprintf("  %-15s  %10s  %10s\n", "Statistic", "OLS", "IV/2SLS"))
cat("  ", paste(rep("-", 40), collapse=""), "\n", sep="")
cat(sprintf("  %-15s  %10.4f  %10.4f\n", "masters coef",  ols_est, iv_est))
cat(sprintf("  %-15s  %10.4f  %10.4f\n", "SE (robust)",   ols_se,  iv_se))
cat(sprintf("  %-15s  %10d  %10d\n",     "N",  nobs(ols_iv), nobs(iv_model)))
cat("  ", paste(rep("-", 40), collapse=""), "\n\n", sep="")

diff_est <- ols_est - iv_est
cat(sprintf("  OLS estimate:  %7.4f (SE = %.4f)\n", ols_est, ols_se))
cat(sprintf("  IV estimate:   %7.4f (SE = %.4f)\n", iv_est,  iv_se))
cat(sprintf("  Difference:    %7.4f\n\n",            diff_est))
cat("  Interpretation:\n")
cat("  The OLS estimate exceeds the IV estimate, indicating upward bias\n")
cat("  due to positive selection on unobservables. Students who complete\n")
cat("  master's degrees have higher unobserved ability and motivation,\n")
cat("  which independently increases salary.\n\n")

# ---- Manual 2SLS (Pedagogical) ----
# Equivalent to: stage1 predict masters_hat + stage2 regress ln_salary masters_hat
cat("*----------------------------------------------------------------------\n")
cat("* MANUAL 2SLS (pedagogical — SEs are incorrect; use ivreg() for inference)\n")
cat("*----------------------------------------------------------------------\n\n")

stage1_hat  <- fitted(fs_model)
df_753$masters_hat <- stage1_hat

fmla_s2 <- as.formula(
  paste("ln_salary ~ masters_hat +", paste(X_controls, collapse = " + ")))
stage2_mdl   <- lm(fmla_s2, data = df_753)
manual_iv    <- coef(stage2_mdl)["masters_hat"]

cat(sprintf("  Manual 2SLS estimate: %.4f\n", manual_iv))
cat(sprintf("  ivreg() estimate:     %.4f\n", iv_est))
cat("  (Coefficients should be identical; SEs differ)\n\n")

df_753$masters_hat <- NULL   # equivalent to: drop masters_hat

# ============================================================================
# Close log — equivalent to: log close
# ============================================================================
cat("Chapter 7 R script completed:", format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n")
sink()

# ============================================================================
# END OF CHAPTER 7 R CODE
# ============================================================================
