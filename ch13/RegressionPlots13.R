#=========================================================================
# RegressionPlots13.R
# Section 13.5: Presenting Multivariate Panel Regression Results
# R translation of RegressionPlots13.do
#
# Package substitutions:
#   Stata coefplot          -> ggplot2::geom_pointrange (manual "coefplot")
#   Stata xtmg ..., cce      -> plm::pmg(..., model = "cmg")  [CCEMG]
#   Stata xtdcce2 (DCCE-MG/ARDL) -> NO CRAN-available equivalent found;
#     left as a documented placeholder, mirroring the Stata original,
#     which itself never completed a coefplot call for this model either
#     ("confirm final coefplot spec... before finalizing Figure 13.9").
#   Stata xtscc              -> plm::plm(model="within") + plm::vcovSCC()
#=========================================================================

if (!exists("root_dir")) {
  # Mirrors Stata_code13.do's own username-conditional path logic
  # (`if c(username) == "marvi"`) -- defaults to the real Chapter 13
  # folder on the author's machine regardless of R's current working
  # directory, and falls back to getwd() for anyone else running this
  # standalone.
  if (Sys.getenv("USERNAME") == "marvi") {
    root_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 13"
    data_dir <- file.path(root_dir, "Data", "Stata")  # matches Stata's global data_dir
  } else {
    root_dir <- getwd()
    data_dir <- file.path(root_dir, "Data")
  }
  graphs_dir <- file.path(root_dir, "Output", "figures")
  tables_dir <- file.path(root_dir, "Output", "tables")
  logdir     <- file.path(root_dir, "Output", "logs")
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(plm)
  library(ggplot2)
})

# Local-file-with-GitHub-fallback loader -- if the .dta isn't found
# locally (e.g. a cloud-sync placeholder that hasn't downloaded yet),
# pull it directly from the chapter's public data repo instead of
# hanging on an unresolved local path.
load_ch13_dta <- function(filename, data_dir) {
  local_path <- file.path(data_dir, filename)
  if (!file.exists(local_path)) {
    url <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch13/", filename)
    cat("Local file not found -- downloading from GitHub:", url, "\n")
    dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
    download.file(url, local_path, mode = "wb", quiet = TRUE)
  }
  # IMPORTANT: copy to a plain local temp folder before reading. haven's
  # underlying ReadStat parser can become extremely slow (sometimes
  # appearing to hang indefinitely) when reading directly from a
  # Dropbox/OneDrive-synced path on Windows, even once the file is fully
  # downloaded -- the sync client's filesystem driver intercepts each of
  # ReadStat's many small low-level reads. A plain file.exists()/
  # file.size() check bypasses this (metadata-only), which is why those
  # returned instantly while read_dta() itself did not. Copying first
  # avoids the synced path entirely for the actual parse.
  tmp_path <- file.path(tempdir(), filename)
  file.copy(local_path, tmp_path, overwrite = TRUE)
  haven::read_dta(tmp_path)
}


cat("RegressionPlots13.R running:", format(Sys.time()), "\n")

pubexport <- function(p, gname, w = 8, h = 5.8) {
  ggsave(file.path(graphs_dir, paste0(gname, "_R.svg")), p, width = w, height = h)
  ggsave(file.path(graphs_dir, paste0(gname, "_R.pdf")), p, width = w, height = h)
  ggsave(file.path(graphs_dir, paste0(gname, "_R.png")), p, width = w, height = h, dpi = 300)
  # Register the live ggplot object (keyed by its canonical export
  # name, not its R variable name) for PolicymakerDeck13.R to pull
  # from later -- avoids the variable-name-collision risk that would
  # occur if the deck script instead grabbed figures by loose global
  # variable name (e.g. "fig13_15" is reused across two different
  # sub-scripts for two different figures).
  if (exists("fig13_registry", envir = .GlobalEnv)) {
    assign(gname, p, envir = get("fig13_registry", envir = .GlobalEnv))
  }
}

# Manual "coefplot" builder: point + 95% CI, horizontal bars
coefplot_r <- function(est_df, title, subtitle = NULL, xlab = "Percent Change in . . .") {
  ggplot(est_df, aes(x = est, y = label)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.7, color = "black") +
    geom_text(aes(label = sprintf("%.2f", est)), vjust = -1.2, size = 3.5) +
    labs(title = title, subtitle = subtitle, x = xlab, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
          plot.subtitle = element_text(hjust = 0.5, size = 10))
}

#=========================================================================
# 13.5.1 Regression Coefficient Plots
#=========================================================================
# DATA PROVENANCE (Figures 13.7-13.9 only): Example_13_1.dta is the same
# state appropriations panel as Chapter 10's Example_10_2_1.dta
# (confirmed in the data repository's own README) -- same source as
# Tables 13.1/13.2, Maps13.R's Figure 13.1, and TrendGraphs13.R's
# Figures 13.3-13.4. Section 13.5.2 below switches to a different
# Chapter 10 file -- see the note above its own load call.
d1 <- load_ch13_dta("Example_13_1.dta", data_dir) %>% arrange(fips, fy)

#-------------------------------------------------------------------------
# Figure 13.7: Pooled OLS on first-differenced logs
# Stata: reg D1.lnnetut L1.D1.lnstateap L1.D1.lnfte L1.D1.lnperinc
#-------------------------------------------------------------------------
pd1 <- pdata.frame(d1, index = c("fips", "fy"))
pd1$D_lnnetut    <- diff(pd1$lnnetut, 1)
pd1$D_lnstateap  <- diff(pd1$lnstateap, 1)
pd1$D_lnfte      <- diff(pd1$lnfte, 1)
pd1$D_lnperinc   <- diff(pd1$lnperinc, 1)
pd1$LD_lnstateap <- plm::lag(pd1$D_lnstateap, 1)
pd1$LD_lnfte     <- plm::lag(pd1$D_lnfte, 1)
pd1$LD_lnperinc  <- plm::lag(pd1$D_lnperinc, 1)

fit7 <- lm(D_lnnetut ~ LD_lnstateap + LD_lnfte + LD_lnperinc, data = as.data.frame(pd1))
s7 <- summary(fit7)
ci7 <- confint(fit7)

fig13_7_data <- data.frame(
  label = c("State Appropriations", "FTE Enrollment", "State Personal Income"),
  est = coef(fit7)[-1] * 10,   # rescale(10): 10-pt change presentation
  lo  = ci7[-1, 1] * 10,
  hi  = ci7[-1, 2] * 10
)

fig13_7 <- coefplot_r(fig13_7_data,
                       title = "Pct. Change in Appropriations, FTE, and Personal Income",
                       xlab = "10 Percent Change in . . . (Change in Net Tuition Revenue)")
pubexport(fig13_7, "fig13_7_ols_coefplot")

#-------------------------------------------------------------------------
# Figure 13.8: CCEMG estimator (cross-sectional dependence)
# Stata: xtmg Dlnnetut LDlnstateap LDlnfte LDlnperinc, cce
#-------------------------------------------------------------------------
cce_data <- as.data.frame(pd1) %>%
  filter(!is.na(D_lnnetut), !is.na(LD_lnstateap), !is.na(LD_lnfte), !is.na(LD_lnperinc))
pcce_data <- pdata.frame(cce_data, index = c("fips", "fy"))

fit8 <- pmg(D_lnnetut ~ LD_lnstateap + LD_lnfte + LD_lnperinc,
            data = pcce_data, model = "cmg")
s8 <- summary(fit8)

# NOTE: pmg(model="cmg") returns 8 coefficients -- the 3 structural
# regressors + intercept, PLUS 4 cross-sectional-average (".bar") CCE
# nuisance terms used to purge unobserved common factors. Select the
# structural coefficients by name (not by position) so this can't
# silently break if pmg's internal term ordering ever changes.
struct_names <- c("LD_lnstateap", "LD_lnfte", "LD_lnperinc")
b8  <- coef(fit8)[struct_names]
se8 <- sqrt(diag(vcov(fit8)))[struct_names]

fig13_8_data <- data.frame(
  label = c("State Appropriations", "FTE Enrollment", "State Personal Income"),
  est = b8 * 10,
  lo  = (b8 - 1.96 * se8) * 10,
  hi  = (b8 + 1.96 * se8) * 10
)

fig13_8 <- coefplot_r(fig13_8_data,
                       title = "Short-Run Change in Net Tuition Revenue Due to a\n10% Change in State Appropriations (CCEMG)")
pubexport(fig13_8, "fig13_8_ccemg_coefplot")

#-------------------------------------------------------------------------
# Figure 13.9: HCR Model with DCCE-MG Estimator and ARDL
#-------------------------------------------------------------------------
# NOTE -- NO CRAN-AVAILABLE R EQUIVALENT FOUND for xtdcce2 (dynamic CCE
# with ARDL long-run/short-run decomposition). The Stata original itself
# never completed a coefplot call for this figure either ("confirm final
# coefplot spec... before finalizing Figure 13.9") -- so this placeholder
# carries the same incompleteness forward rather than fabricating a
# result neither language has actually produced yet.
cat("\nFigure 13.9 (DCCE-MG/ARDL): no R equivalent implemented -- matches\n")
cat("the Stata original's own incomplete state for this figure.\n")

#=========================================================================
# 13.5.2 Marginal Effects (Continuous Variables) and Graphs
#=========================================================================
# DATA PROVENANCE: Example_13_4.dta is the same file as Chapter 10's
# "Example 10.dta" (net tuition/administrative staffing panel),
# confirmed in the data repository's own README -- a DIFFERENT Chapter
# 10 dataset than 13.5.1's Example_13_1.dta above. This same file is
# also used by EstimationTables13.R (Section 13.2.2/13.7.4) and, via
# pd4 still in memory, Section 13.5.3 below.
d4 <- load_ch13_dta("Example_13_4.dta", data_dir)
pd4 <- pdata.frame(d4, index = c("id", "year"))
pd4$L_lnnet_tuition_rev_adj <- plm::lag(pd4$lnnet_tuition_rev_adj, 1)
pd4$L_lnstate_appro_adj     <- plm::lag(pd4$lnstate_appro_adj, 1)
pd4$L_lnfedrev_r            <- plm::lag(pd4$lnfedrev_r, 1)
pd4$L_lnFTE_enroll          <- plm::lag(pd4$lnFTE_enroll, 1)

fit_xtscc <- plm(lnadminstaff ~ L_lnnet_tuition_rev_adj + L_lnstate_appro_adj +
                    L_lnfedrev_r + L_lnFTE_enroll, data = pd4, model = "within")
vc_xtscc  <- vcovSCC(fit_xtscc, method = "arellano", type = "HC1")
b_xtscc   <- coef(fit_xtscc)
se_xtscc  <- sqrt(diag(vc_xtscc))

#-------------------------------------------------------------------------
# Figures 13.10.1-13.10.3: Elasticities at Median, 25th, 75th Percentiles
# NOTE: model is fully log-log, so the elasticity (= the coefficient
# itself) is constant and does not vary across percentiles -- confirmed
# identical in EstimationTables13.R and consistent with a constant-
# elasticity specification, not a translation artifact.
#-------------------------------------------------------------------------
for (pctl in c("median", "p25", "p75")) {
  fig_data <- data.frame(
    label = c("Net Tuition Revenue", "State Appropriations", "Federal Revenue", "FTE Enrollment"),
    est = b_xtscc * 10,
    lo  = (b_xtscc - 1.96 * se_xtscc) * 10,
    hi  = (b_xtscc + 1.96 * se_xtscc) * 10
  )
  p <- coefplot_r(fig_data,
                   title = "Percent Change in Administrators Due to a 10% Change in\nNet Tuition Revenue (controlling for other factors)",
                   xlab = paste0("At the ", pctl))
  pubexport(p, paste0("fig13_10_", pctl))
}

#=========================================================================
# 13.5.3 Marginal Effects (Categorical Variables) and Graphs
#=========================================================================
#-------------------------------------------------------------------------
# Figure 13.11: Pct. Change in Administrators, by Consolidated Governing
# Board (CGB) Status
#-------------------------------------------------------------------------
fit_nocgb <- plm(lnadminstaff ~ L_lnnet_tuition_rev_adj + L_lnstate_appro_adj +
                    L_lnfedrev_r + L_lnFTE_enroll,
                  data = pd4, subset = (CGB == 0), model = "within")
fit_cgb   <- plm(lnadminstaff ~ L_lnnet_tuition_rev_adj + L_lnstate_appro_adj +
                    L_lnfedrev_r + L_lnFTE_enroll,
                  data = pd4, subset = (CGB == 1), model = "within")

vc_nocgb <- vcovSCC(fit_nocgb, method = "arellano", type = "HC1")
vc_cgb   <- vcovSCC(fit_cgb,   method = "arellano", type = "HC1")

fig13_11_data <- data.frame(
  label = rep(c("Net Tuition Revenue", "State Appropriations", "Federal Revenues", "FTE Enrollment"), 2),
  group = rep(c("No CGB", "CGB"), each = 4),
  est = c(coef(fit_nocgb) * 10, coef(fit_cgb) * 10),
  se  = c(sqrt(diag(vc_nocgb)), sqrt(diag(vc_cgb))) * 10
) %>%
  mutate(lo = est - 1.96 * se, hi = est + 1.96 * se)

fig13_11 <- ggplot(fig13_11_data, aes(x = label, y = est, fill = group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(width = 0.7), width = 0.15) +
  scale_fill_manual(values = c("No CGB" = "gray70", "CGB" = "gray20")) +
  labs(title = "Percent Change in Administrators Due to a 10% Change in\nNet Tuition Revenue (controlling for other factors)",
       x = NULL, y = "Percent", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
        axis.text.x = element_text(size = 9))

pubexport(fig13_11, "fig13_11_cgb_coefplot")

cat("\nRegressionPlots13.R completed:", format(Sys.time()), "\n")
