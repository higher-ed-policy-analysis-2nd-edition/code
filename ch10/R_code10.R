# ============================================================================
# Chapter 10 — Causal Inference and Marginal Treatment Effects
# R Translation of Complete Stata Code
# Higher Education Policy Analysis Using Quantitative Techniques (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/tree/main/ch10
# Author: Marvin A. Titus
# NOTE: Code development was assisted by Claude (Anthropic). The author
# provided specifications and reviewed, tested, and validated all code.
# Date: March 2026
# ============================================================================
# Script tested in R 4.4.x
# Required packages: see install block below
#
# PART A (Sections 10.3–10.9): Causal Inference — Georgia Consolidation
#   R equivalents: fixest, plm, clubSandwich, hdm, Synth, synthdid, did
#
# PART B (Sections 10.10–10.16): Marginal Treatment Effects
#   R equivalents: haven, ivreg, sampleSelection, fwildclusterboot (optional)
#   NOTE: mtefe has no CRAN equivalent — manual polynomial MTE implemented.
#         synth_runner has no CRAN equivalent — noted inline.
# ============================================================================

options(crayon.enabled = FALSE)   # clean log output — replaces: set more off

# ── Install missing packages (run once) ─────────────────────────────────────
required_pkgs <- c(
  # Part A
  "readr", "dplyr", "tidyr", "ggplot2", "scales", "patchwork",
  "janitor",
  "plm", "fixest", "clubSandwich", "lmtest", "sandwich",
  "hdm",          # LASSO — replaces: lasso2
  "Synth",        # Synthetic Control — replaces: synth
  "synthdid",     # Synthetic DiD — replaces: sdid
  "did",          # Callaway-Sant'Anna — replaces: csdid
  "modelsummary", # tables — replaces: esttab
  # Part B
  "haven", "ivreg", "sampleSelection"
)
new_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs)

suppressPackageStartupMessages({
  library(janitor);     library(readr);       library(dplyr);     library(tidyr)
  library(ggplot2);     library(scales);    library(patchwork)
  library(plm);         library(fixest);    library(clubSandwich)
  library(lmtest);      library(sandwich);  library(hdm)
  library(Synth);       library(synthdid);  library(did)
  library(modelsummary)
  library(haven);       library(ivreg)
  library(sampleSelection)
})

# fwildclusterboot: optional package for wild cluster bootstrap
# Replaces: boottest masters, cluster(state) reps(9999) [Webb weights]
# Falls back to sandwich clustered SEs if unavailable.
has_fwcb <- requireNamespace("fwildclusterboot", quietly = TRUE)
if (!has_fwcb) {
  tryCatch(install.packages("fwildclusterboot", quiet = TRUE),
           error   = function(e) invisible(NULL),
           warning = function(w) invisible(NULL))
  has_fwcb <- requireNamespace("fwildclusterboot", quietly = TRUE)
}
if (!has_fwcb)
  message("Note: fwildclusterboot unavailable — ",
          "wild cluster bootstrap will use sandwich cluster SEs as fallback.",
          "\nTo install manually: install.packages(\"fwildclusterboot\")")

# ── Output paths ─────────────────────────────────────────────────────────────
if (Sys.info()[["user"]] == "marvi") {
  graphs_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/graphs"
  log_path   <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 10/Output/logs/Chapter10_R_output.log"
} else {
  graphs_dir <- "Output/graphs"
  log_path   <- "Output/logs/Chapter10_R_output.log"
}
dir.create(graphs_dir,        showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)

sink(log_path, split = TRUE)
cat("Chapter 10 R log opened:", format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n")
cat("Graphs directory:", graphs_dir, "\n\n")

set.seed(20251130)

# ── Helpers ──────────────────────────────────────────────────────────────────
safe_download <- function(url, dest) {
  tryCatch(
    download.file(url, dest, mode = "wb", quiet = TRUE),
    error   = function(e) message("[download failed] ", basename(dest), ": ", e$message),
    warning = function(w) message("[download warning] ", basename(dest), ": ", w$message)
  )
}

# save_fig(): print to RStudio pane + export to graphs_dir via temp copy
# — replaces: graph export "$graphs_dir/figXX_name_Stata.png"
save_fig <- function(p, filename, width_px = 1200, height_px = 900, dpi = 150) {
  print(p)
  tmp <- file.path(tempdir(), filename)
  ggplot2::ggsave(tmp, plot = p, width = width_px / dpi, height = height_px / dpi,
                  dpi = dpi, bg = "white")
  file.copy(tmp, file.path(graphs_dir, filename), overwrite = TRUE)
  invisible(p)
}

# cluster_vcov(): clustered SEs via clubSandwich — replaces: vce(cluster fips)
cluster_vcov <- function(model, cluster_var, type = "CR1") {
  clubSandwich::vcovCR(model, cluster = cluster_var, type = type)
}

# coef_test_cl(): coefficient table with clustered SEs
coef_test_cl <- function(model, cluster_var, type = "CR1") {
  V <- cluster_vcov(model, cluster_var, type)
  lmtest::coeftest(model, vcov. = V)
}

# ============================================================================
# ============================================================================
#
#   PART A: CAUSAL INFERENCE — GEORGIA HIGHER EDUCATION CONSOLIDATION
#           (Sections 10.3 – 10.9)
#
# ============================================================================
# ============================================================================

cat("\n", strrep("=", 60), "\n")
cat("PART A: CAUSAL INFERENCE\n")
cat(strrep("=", 60), "\n\n")

# ── Section 10.3.1: Data Structure and Variable Construction ─────────────────
cat("* Section 10.3.1: Data Structure and Variable Construction\n")

url_a <- paste0("https://raw.githubusercontent.com/",
                "higher-ed-policy-analysis-2nd-edition/data/main/ch10/",
                "Example_10_3_1.csv")
safe_download(url_a, "Example_10_3_1.csv")
df <- readr::read_csv("Example_10_3_1.csv", show_col_types = FALSE) |>
  janitor::clean_names()  # lowercase snake_case — replicates Stata import delimited

# Clean state names — replaces: replace state = strtrim(state)
df <- df |> dplyr::mutate(state = trimws(state))

# SREB indicator (16 Southern states)
sreb_states <- c("Alabama", "Arkansas", "Delaware", "Florida", "Georgia",
                 "Kentucky", "Louisiana", "Maryland", "Mississippi",
                 "North Carolina", "Oklahoma", "South Carolina",
                 "Tennessee", "Texas", "Virginia", "West Virginia")
df <- df |> dplyr::filter(state %in% sreb_states)

# FIPS codes
fips_map <- c(Alabama=1, Arkansas=5, Delaware=10, Florida=12, Georgia=13,
              Kentucky=21, Louisiana=22, Maryland=24, Mississippi=28,
              `North Carolina`=37, Oklahoma=40, `South Carolina`=45,
              Tennessee=47, Texas=48, Virginia=51, `West Virginia`=54)
df <- df |> dplyr::mutate(fips = fips_map[state])

# Treatment and time indicators
df <- df |> dplyr::mutate(
  treat_state  = as.integer(state == "Georgia"),
  post         = as.integer(fy >= 2018),
  did          = treat_state * post,
  post_placebo = as.integer(fy >= 2012),
  did_placebo  = treat_state * post_placebo,
  # Log-transformed outcome and controls
  lngenop  = log(general_public_operations),
  lntotsup = log(total_state_support),
  lnfinaid = log(total_financial_aid),
  lntuifee = log(net_tuition_and_fee_revenue),
  lnfte    = log(net_fte_enrollment)
)

controls <- c("lntotsup", "lnfinaid", "lntuifee", "lnfte")
controls_f <- paste(controls, collapse = " + ")

# Panel data frame — replaces: xtset fips fy
pdf_a <- plm::pdata.frame(df, index = c("fips", "fy"))

cat("Data loaded: N =", nrow(df), "  States =", length(unique(df$fips)),
    "  Years =", length(unique(df$fy)), "\n\n")

# ── Section 10.3.2: TWFE Estimation ─────────────────────────────────────────
cat("* Section 10.3.2: TWFE Estimation Results\n")

# Two-Way Fixed Effects DiD — replaces: xtreg lngenop did $controls i.fy, fe vce(cluster fips)
twfe_fml <- as.formula(paste("lngenop ~ did +", controls_f, "| fips + fy"))
twfe_did  <- fixest::feols(twfe_fml, data = df, cluster = ~fips)
print(summary(twfe_did))

# Placebo test (pre-treatment)
twfe_placebo_fml <- as.formula(paste("lngenop ~ did_placebo +", controls_f, "| fips + fy"))
twfe_placebo <- fixest::feols(twfe_placebo_fml,
                              data = dplyr::filter(df, fy < 2018), cluster = ~fips)
cat("\nPlacebo test (pre-2018):\n")
print(summary(twfe_placebo))

# ── Section 10.3.3: Parallel Trends Assessment ───────────────────────────────
cat("\n* Section 10.3.3: Parallel Trends Assessment\n")

# Visual inspection — replaces: collapse + twoway parallel trends
trends_df <- df |>
  dplyr::group_by(treat_state, fy) |>
  dplyr::summarise(lngenop = mean(lngenop, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(Group = ifelse(treat_state == 0, "Control States", "Georgia"))

fig_pt <- ggplot(trends_df, aes(x = fy, y = lngenop,
                                colour = Group, shape = Group)) +
  geom_point() + geom_line() +
  geom_vline(xintercept = 2018, linetype = "dotted") +
  scale_colour_manual(values = c("Control States" = "navy", "Georgia" = "firebrick")) +
  labs(title = "Parallel Trends: Treatment vs Control",
       x = "Fiscal Year", y = "Log Operating Expenses",
       colour = NULL, shape = NULL) +
  theme_bw()
save_fig(fig_pt, "fig10_1_parallel_trends_R.png")

# Formal pre-trends test — replaces: reghdfe lngenop c.treat_state#c.fy ... if fy < 2018
pretrend_fml <- as.formula(
  paste("lngenop ~ treat_state:fy +", controls_f, "| fips + fy"))
pretrend <- fixest::feols(pretrend_fml,
                          data = dplyr::filter(df, fy < 2018), cluster = ~fips)
cat("\nFormal pre-trends test (interaction treat_state x fy, pre-2018):\n")
print(summary(pretrend))
cat("H0: treat_state:fy = 0  p-value:", summary(pretrend)$coeftable["treat_state:fy", "Pr(>|t|)"], "\n\n")

# ── Section 10.3.4: Robustness Checks ────────────────────────────────────────
cat("* Section 10.3.4: Robustness Checks\n")

df <- df |> dplyr::mutate(
  post_2013 = as.integer(fy >= 2013),  did_2013 = treat_state * post_2013,
  post_2015 = as.integer(fy >= 2015),  did_2015 = treat_state * post_2015,
  border    = as.integer(state %in% c("Florida","Alabama","South Carolina",
                                      "Tennessee","North Carolina"))
)

rob_2013 <- fixest::feols(
  as.formula(paste("lngenop ~ did_2013 +", controls_f, "| fips + fy")),
  data = df, cluster = ~fips)
rob_2015 <- fixest::feols(
  as.formula(paste("lngenop ~ did_2015 +", controls_f, "| fips + fy")),
  data = df, cluster = ~fips)
rob_noborder <- fixest::feols(
  as.formula(paste("lngenop ~ did +", controls_f, "| fips + fy")),
  data = dplyr::filter(df, border == 0), cluster = ~fips)

# Weighted by mean FTE — replaces: bysort fips: egen mean_fte + xtreg [aweight=mean_fte]
df <- df |>
  dplyr::group_by(fips) |>
  dplyr::mutate(mean_fte = mean(net_fte_enrollment, na.rm = TRUE)) |>
  dplyr::ungroup()
rob_wtd <- fixest::feols(
  as.formula(paste("lngenop ~ did +", controls_f, "| fips + fy")),
  data = df, weights = ~mean_fte, cluster = ~fips)

cat("Robustness — Alt timing 2013:\n"); print(summary(rob_2013))
cat("Robustness — Alt timing 2015:\n"); print(summary(rob_2015))
cat("Robustness — No border states:\n"); print(summary(rob_noborder))
cat("Robustness — Weighted:\n");        print(summary(rob_wtd))

# ── Section 10.4: LASSO-Residualized DiD ─────────────────────────────────────
cat("\n* Section 10.4.2-10.4.3: LASSO-Residualized DiD\n")
# Replaces: lasso2 + reghdfe post-LASSO
# hdm::rlasso implements rigorous LASSO (Belloni, Chernozhukov & Hansen 2014)

df <- df |> dplyr::mutate(
  lntotsup_post  = lntotsup * post,  lntotsup_treat  = lntotsup * treat_state,
  lnfinaid_post  = lnfinaid * post,  lnfinaid_treat  = lnfinaid * treat_state,
  lntuifee_post  = lntuifee * post,  lntuifee_treat  = lntuifee * treat_state,
  lnfte_post     = lnfte    * post,  lnfte_treat     = lnfte    * treat_state
)

lasso_vars <- c("did", "post", "treat_state", controls,
                "lntotsup_post","lntotsup_treat","lnfinaid_post","lnfinaid_treat",
                "lntuifee_post","lntuifee_treat","lnfte_post","lnfte_treat")

df_lasso <- df |> dplyr::select(lngenop, fips, fy, dplyr::all_of(lasso_vars)) |>
  tidyr::drop_na()

X_lasso <- as.matrix(df_lasso[, lasso_vars])
y_lasso <- df_lasso$lngenop

rlasso_fit    <- hdm::rlasso(X_lasso, y_lasso)
lasso_selected <- names(which(coef(rlasso_fit)[-1] != 0))
cat("LASSO-selected variables:", paste(lasso_selected, collapse = ", "), "\n")

# Post-LASSO OLS with two-way FE — replaces: reghdfe lngenop did `lasso_selected'
post_lasso_vars <- unique(c("did", lasso_selected))
lasso_fml <- as.formula(
  paste("lngenop ~", paste(post_lasso_vars, collapse = " + "), "| fips + fy"))
lasso_did <- fixest::feols(lasso_fml, data = df_lasso, cluster = ~fips)
cat("\nPost-LASSO DiD:\n")
print(summary(lasso_did))

# ── Section 10.5.3: Synthetic Control Method ─────────────────────────────────
cat("\n* Section 10.5.3: Synthetic Control Method\n")
# Replaces: synth + synth_runner
# synth_runner has no CRAN equivalent; synth is replicated via Synth package

df_synth <- df |>
  dplyr::filter(!is.na(lngenop), !is.na(lntotsup),
                !is.na(lnfinaid), !is.na(lntuifee), !is.na(lnfte))

# Build predictors matrix for synth()
# Replaces: synth lngenop lngenop(2005) lngenop(2010) lngenop(2015) ...
tryCatch({
  synth_data <- Synth::dataprep(
    foo            = as.data.frame(df_synth),
    predictors     = c("lntotsup","lnfinaid","lntuifee","lnfte"),
    predictors.op  = "mean",
    special.predictors = list(
      list("lngenop", 2005, "mean"),
      list("lngenop", 2010, "mean"),
      list("lngenop", 2015, "mean")
    ),
    dependent      = "lngenop",
    unit.variable  = "fips",
    unit.names.variable = "state",
    time.variable  = "fy",
    treatment.identifier = 13,
    controls.identifier  = setdiff(unique(df_synth$fips), 13),
    time.predictors.prior = 2005:2017,
    time.optimize.ssr     = 2005:2017,
    time.plot             = min(df_synth$fy):max(df_synth$fy)
  )
  synth_out <- Synth::synth(synth_data, optimxmethod = "All")

  # Extract and plot
  synth_tab <- Synth::synth.tab(synth_out, synth_data)
  gaps      <- synth_data$Y1plot - (synth_data$Y0plot %*% synth_out$solution.w)
  synth_df  <- data.frame(
    fy       = as.integer(rownames(gaps)),
    synth_gap = as.numeric(gaps)
  )
  cat("\nPost-treatment average synthetic control gap:\n")
  print(summary(synth_df$synth_gap[synth_df$fy >= 2018]))

  fig_sc <- ggplot(synth_df, aes(x = fy, y = synth_gap)) +
    geom_line(colour = "navy", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 2018, linetype = "dotted", colour = "red") +
    labs(title = "Synthetic Control: Georgia Treatment Gap",
         x = "Fiscal Year", y = "Treated − Synthetic (Log Operating Expenses)") +
    theme_bw()
  save_fig(fig_sc, "fig10_2_synth_control_R.png")

}, error = function(e) {
  cat("[Synth] Error:", e$message, "\n")
  cat("[Synth] Note: synth_runner has no CRAN equivalent; manual gap plotted above.\n")
})

# ── Section 10.6.3: Synthetic DiD ────────────────────────────────────────────
cat("\n* Section 10.6.3: Synthetic DiD (single treated unit)\n")
# Replaces: sdid lngenop fips fy treat_sdid, vce(placebo) seed(123) graph

df <- df |> dplyr::mutate(treat_sdid = as.integer(fips == 13 & fy >= 2018))

tryCatch({
  # synthdid requires a balanced panel matrix
  sdid_panel  <- synthdid::panel.matrices(as.data.frame(df),
                                          unit = "fips", time = "fy",
                                          outcome = "lngenop",
                                          treatment = "treat_sdid")
  sdid_est    <- synthdid::synthdid_estimate(sdid_panel$Y, sdid_panel$N0,
                                              sdid_panel$T0)
  sdid_se     <- sqrt(vcov(sdid_est, method = "placebo"))

  cat("SDID estimate:", round(sdid_est, 4),
      "  Placebo SE:", round(sdid_se, 4), "\n")

  fig_sdid <- plot(sdid_est) +
    labs(title = "Synthetic DiD — Georgia Consolidation") +
    theme_bw()
  # synthdid returns a ggplot; wrap in save_fig
  save_fig(fig_sdid, "fig10_2b_sdid_R.png")

}, error = function(e) cat("[synthdid] Error:", e$message, "\n"))

# ── Sections 10.2.4 & 10.3.3: Event Study ────────────────────────────────────
cat("\n* Sections 10.2.4 & 10.3.3: Event Study\n")

df <- df |>
  dplyr::mutate(rel_time = ifelse(treat_state == 1, fy - 2018L, 0L))

# Bin endpoints to avoid perfect multicollinearity
# Replaces: quietly tab rel_time, gen(event_) + reghdfe event_1-event_17
df <- df |>
  dplyr::mutate(rel_time_bin = dplyr::case_when(
    rel_time <= -8 ~ -8L,
    rel_time >=  6 ~  6L,
    TRUE           ~  rel_time
  ))

event_fml <- as.formula(
  paste("lngenop ~ i(rel_time_bin, treat_state, ref = -1) +",
        controls_f, "| fips + fy"))
event_fit <- fixest::feols(event_fml, data = df, cluster = ~fips)
cat("Event study results:\n")
print(summary(event_fit))

fig_es <- fixest::iplot(event_fit,
                        main  = "Event Study: Effect on Log Operating Expenses",
                        xlab  = "Years Relative to Treatment",
                        ylab  = "Effect on Log Operating Expenses",
                        ci_level = 0.95)
# iplot returns invisible(NULL); re-draw via ggplot using broom
es_coefs <- broom::tidy(event_fit, conf.int = TRUE) |>
  dplyr::filter(grepl("rel_time_bin", term)) |>
  dplyr::mutate(t = as.integer(gsub(".*rel_time_bin::([-0-9]+).*", "\\1", term)))

fig_es2 <- ggplot(es_coefs, aes(x = t, y = estimate,
                                 ymin = conf.low, ymax = conf.high)) +
  geom_point() + geom_errorbar(width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, linetype = "dotted", colour = "red") +
  labs(title = "Event Study: Georgia Higher Education Consolidation",
       x = "Years Relative to Treatment",
       y = "Effect on Log Operating Expenses") +
  theme_bw()
save_fig(fig_es2, "fig10_3_event_study_R.png")

# Callaway-Sant'Anna DiD — replaces: csdid lngenop $controls, ivar(fips) time(fy) gvar(treat_year)
df <- df |> dplyr::mutate(treat_year = ifelse(treat_state == 1, 2018L, 0L))

csa_out <- did::att_gt(
  yname   = "lngenop",
  tname   = "fy",
  idname  = "fips",
  gname   = "treat_year",
  xformla = as.formula(paste("~", controls_f)),
  data    = df,
  est_method = "dr",   # doubly-robust — replaces: method(dripw)
  control_group = "nevertreated"
)
cat("\nCallaway-Sant'Anna ATT(g,t) estimates:\n")
print(summary(csa_out))

# csdid_plot — replaces: csdid_plot, style(rcap)
fig_csa <- did::ggdid(csa_out) +
  labs(title = "Callaway-Sant'Anna Event Study (Single Cohort 2018)") +
  theme_bw()
save_fig(fig_csa, "fig10_4_csdid_event_R.png")

# ── Section 10.7.3-10.7.5: Multi-State Staggered Analysis ───────────────────
cat("\n* Section 10.7.3-10.7.5: Multi-State Staggered Analysis\n")

url_stag <- paste0("https://raw.githubusercontent.com/",
                   "higher-ed-policy-analysis-2nd-edition/data/main/ch10/",
                   "Example_10_7_3.csv")
safe_download(url_stag, "Example_10_7_3.csv")
df_stag <- readr::read_csv("Example_10_7_3.csv", show_col_types = FALSE) |>
  dplyr::rename_with(tolower) |>  # plain lowercase — replicates Stata import delimited
  dplyr::mutate(
    lngenop  = log(generalpublicoperations),
    lntotsup = log(totalstatesupport),
    lnfinaid = log(totalfinancialaid),
    lntuifee = log(nettuitionandfeerevenue),
    lnfte    = log(netfteenrollment)
  ) |>
  tidyr::drop_na(lngenop, lntotsup, lnfinaid, lntuifee, lnfte)

# Balance panel — replaces: bysort fips: gen obs_count / keep if obs_count == mode
obs_counts <- df_stag |> dplyr::count(fips, name = "n_obs")
mode_count <- as.integer(names(sort(table(obs_counts$n_obs), decreasing = TRUE)[1]))
balanced_fips <- obs_counts$fips[obs_counts$n_obs == mode_count]
df_stag <- df_stag |> dplyr::filter(fips %in% balanced_fips)
cat("Balanced staggered panel: N states =", length(balanced_fips),
    "  Periods =", mode_count, "\n")

# Staggered treatment variable (0 = never treated)
df_stag <- df_stag |>
  dplyr::mutate(gyear = dplyr::case_when(
    fips == 13 ~ 2013L,
    fips == 55 ~ 2018L,
    fips == 42 ~ 2022L,
    TRUE       ~ 0L
  ))

# CSDID estimation — replaces: csdid lngenop, ivar(fips) time(fy) gvar(gyear) method(dripw)
csa_stag <- did::att_gt(
  yname   = "lngenop",
  tname   = "fy",
  idname  = "fips",
  gname   = "gyear",
  data    = df_stag,
  est_method    = "dr",
  control_group = "nevertreated"
)

# Overall ATT — replaces: estat simple
agg_simple <- did::aggte(csa_stag, type = "simple")
cat("\nOverall ATT (simple aggregation):\n"); print(summary(agg_simple))

# Group-specific effects — replaces: estat group
agg_group <- did::aggte(csa_stag, type = "group")
cat("\nGroup-specific ATT:\n"); print(summary(agg_group))

# Event study — replaces: estat event + csdid_plot
agg_event <- did::aggte(csa_stag, type = "dynamic")
cat("\nEvent study aggregation:\n"); print(summary(agg_event))

fig_stag <- did::ggdid(agg_event) +
  labs(title = "CSDID Event Study — Multi-State Staggered Adoption") +
  theme_bw()
save_fig(fig_stag, "fig10_4b_csdid_staggered_R.png")

# Calendar time — replaces: estat calendar
agg_cal <- did::aggte(csa_stag, type = "calendar")
cat("\nCalendar-time ATT:\n"); print(summary(agg_cal))

# SDID robustness with staggered data — replaces: sdid ... covariates(..., projected)
df_stag <- df_stag |>
  dplyr::mutate(treatment = as.integer(
    (fips == 13 & fy >= 2013) |
    (fips == 55 & fy >= 2018) |
    (fips == 42 & fy >= 2022)
  ))

tryCatch({
  sdid_stag_panel <- synthdid::panel.matrices(
    as.data.frame(df_stag), unit = "fips", time = "fy",
    outcome = "lngenop", treatment = "treatment")
  sdid_stag_est   <- synthdid::synthdid_estimate(
    sdid_stag_panel$Y, sdid_stag_panel$N0, sdid_stag_panel$T0)
  cat("\nStaggered SDID estimate:", round(sdid_stag_est, 4), "\n")
}, error = function(e) cat("[synthdid staggered] Error:", e$message, "\n"))

# ── Section 10.8.2: Permutation Inference ────────────────────────────────────
cat("\n* Section 10.8.2: Permutation Inference\n")
# Replaces: permute did coef=_b[did], reps(1000)

set.seed(12345)
n_perm   <- 1000
obs_coef <- coef(twfe_did)["did"]
perm_coefs <- numeric(n_perm)

for (pp in seq_len(n_perm)) {
  df_perm <- df |>
    dplyr::group_by(fy) |>
    dplyr::mutate(did_perm = sample(did)) |>
    dplyr::ungroup()
  fit_perm <- tryCatch(
    fixest::feols(as.formula(paste("lngenop ~ did_perm +", controls_f, "| fips + fy")),
                  data = df_perm, cluster = ~fips, warn = FALSE),
    error = function(e) NULL
  )
  perm_coefs[pp] <- if (!is.null(fit_perm)) coef(fit_perm)["did_perm"] else NA_real_
}
perm_coefs <- perm_coefs[!is.na(perm_coefs)]
p_perm     <- mean(abs(perm_coefs) >= abs(obs_coef))
cat("Permutation test: observed coef =", round(obs_coef, 4),
    "  p-value =", round(p_perm, 4), "\n")

fig_perm <- ggplot(data.frame(coef = perm_coefs), aes(x = coef)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "grey70", colour = "white") +
  stat_function(fun = dnorm,
                args = list(mean = mean(perm_coefs), sd = sd(perm_coefs)),
                colour = "navy", linewidth = 1) +
  geom_vline(xintercept = obs_coef, colour = "red",
             linetype = "dashed", linewidth = 1) +
  labs(title = "Permutation Distribution of DiD Coefficient",
       x = "Permuted coefficient", y = "Density") +
  theme_bw()
save_fig(fig_perm, "fig10_5_permutation_hist_R.png")

# ── Section 10.8.3: Leave-One-Out Analysis ───────────────────────────────────
cat("\n* Section 10.8.3: Leave-One-Out Analysis\n")
# Replaces: foreach s of local control_states + xtreg if fips != `s'

control_fips <- unique(df$fips[df$treat_state == 0])
loo_results  <- data.frame(excl_fips = control_fips, did_coef = NA_real_)

for (ii in seq_along(control_fips)) {
  s <- control_fips[ii]
  fit_loo <- tryCatch(
    fixest::feols(as.formula(paste("lngenop ~ did +", controls_f, "| fips + fy")),
                  data = dplyr::filter(df, fips != s), cluster = ~fips,
                  warn = FALSE),
    error = function(e) NULL
  )
  if (!is.null(fit_loo)) loo_results$did_coef[ii] <- coef(fit_loo)["did"]
}
cat("\nLeave-One-Out Analysis:\n")
print(loo_results)

# ── Section 10.9: Results Summary ────────────────────────────────────────────
cat("\n* Section 10.9: Results Summary\n")
# Replaces: estimates table twfe_did lasso_did / esttab using results_combined.csv

modelsummary::modelsummary(
  list("TWFE DiD" = twfe_did, "LASSO DiD" = lasso_did),
  coef_omit  = "^(?!did)",
  coef_rename = c("did" = "DiD (Treatment × Post)"),
  gof_map    = c("nobs","r.squared"),
  stars      = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  title      = "OLS vs LASSO DiD Estimates"
)

# Export to CSV — replaces: esttab using results_combined.csv
modelsummary::modelsummary(
  list("TWFE DiD" = twfe_did, "LASSO DiD" = lasso_did),
  output = "results_combined.csv",
  stars  = c("*" = 0.10, "**" = 0.05, "***" = 0.01)
)
cat("Saved: results_combined.csv\n")

# ============================================================================
# ============================================================================
#
#   PART B: MARGINAL TREATMENT EFFECTS — RETURNS TO MASTER'S DEGREE
#           (Sections 10.10 – 10.16)
#
#   NOTE: No CRAN equivalent for mtefe; manual polynomial MTE implemented.
#
# ============================================================================
# ============================================================================

cat("\n", strrep("=", 60), "\n")
cat("PART B: MARGINAL TREATMENT EFFECTS\n")
cat(strrep("=", 60), "\n\n")

set.seed(20251130)

# ── Section 1: Load Dataset ───────────────────────────────────────────────────
cat("* SECTION 1: Load Dataset\n")

# Replaces: capture use "Example_7_5_3_updated.dta" + fallback
bb <- tryCatch(
  haven::read_dta("Example_7_5_3_updated.dta") |> haven::zap_labels(),
  error = function(e) {
    message("Note: Example_7_5_3_updated.dta not found; loading Example_7_5_3.dta")
    haven::read_dta("Example_7_5_3.dta") |> haven::zap_labels()
  }
)

if (!"id" %in% names(bb)) bb <- bb |> dplyr::mutate(id = dplyr::row_number())

cat("Sample size:", nrow(bb), "\n")
cat("Variables:", paste(names(bb), collapse = ", "), "\n\n")

# ── Section 1b: ma_* Program Area Indicators ─────────────────────────────────
# Replaces: FIX G — generate ma_* if absent
if (!"ma_stem" %in% names(bb)) {
  cat("Generating ma_* variables from undergraduate major fields...\n")
  set.seed(20251130)
  bb <- bb |>
    dplyr::mutate(
      ma_stem = 0L, ma_business = 0L, ma_education = 0L,
      ma_health = 0L, ma_other = 0L,
      .rma = dplyr::if_else(masters == 1, runif(dplyr::n()), NA_real_)
    ) |>
    dplyr::mutate(
      ma_stem      = dplyr::if_else(masters==1 & stem_major==1 & .rma<=0.55, 1L, ma_stem),
      ma_business  = dplyr::if_else(masters==1 & bus_major==1  & .rma<=0.65 & ma_stem==0, 1L, ma_business),
      ma_education = dplyr::if_else(masters==1 & ed_major==1   & .rma<=0.70 & ma_stem==0 & ma_business==0, 1L, ma_education),
      ma_health    = dplyr::if_else(masters==1 & socsci_major==1 & .rma<=0.40 & ma_stem==0 & ma_business==0 & ma_education==0, 1L, ma_health),
      ma_health    = dplyr::if_else(masters==1 & stem_major==1 & .rma>0.55 & .rma<=0.75 & ma_stem==0, 1L, ma_health),
      ma_other     = dplyr::if_else(masters==1 & ma_stem==0 & ma_business==0 & ma_education==0 & ma_health==0, 1L, ma_other)
    ) |>
    dplyr::select(-.rma)
  cat("ma_* variables generated successfully.\n")
}

# Verification — replaces: foreach + gen ma_check
n_treated <- sum(bb$masters == 1, na.rm = TRUE)
cat("\n--- Program Area Distribution (Treated Only) ---\n")
cat("Total treated:", n_treated, "\n")
for (a in c("stem","business","education","health","other")) {
  n_a <- sum(bb[[paste0("ma_",a)]] == 1, na.rm = TRUE)
  cat(sprintf("  ma_%s: %d  (%5.1f%%)\n", a, n_a, 100*n_a/n_treated))
}
bb <- bb |> dplyr::mutate(
  ma_check = ma_business + ma_education + ma_health + ma_stem + ma_other)
bad_treated   <- sum(bb$masters == 1 & bb$ma_check != 1, na.rm = TRUE)
bad_untreated <- sum(bb$masters == 0 & bb$ma_check != 0, na.rm = TRUE)
if (bad_treated   > 0) { cat("WARNING:", bad_treated, "treated obs with != 1 program area flag\n")
} else              { cat("CHECK PASSED: all treated obs have exactly 1 program area\n") }
if (bad_untreated > 0) { cat("WARNING:", bad_untreated, "untreated obs with non-zero program area flag\n")
} else              { cat("CHECK PASSED: all untreated obs have zero program area\n") }
bb <- dplyr::select(bb, -ma_check)

# ── Section 2: Summary Statistics ────────────────────────────────────────────
cat("\n* SECTION 2: Summary Statistics\n")

treat_rate <- mean(bb$masters, na.rm = TRUE)
cat(sprintf("Treatment rate: %.3f\n", treat_rate))
print(summary(dplyr::select(bb, ln_salary, salary, masters, ga_funding_adj)))

# By treatment group — replaces: tabstat salary ln_salary, by(masters)
cat("\nSalary statistics by treatment group:\n")
bb |> dplyr::group_by(masters) |>
  dplyr::summarise(
    mean_salary    = mean(salary,    na.rm=TRUE),
    sd_salary      = sd(salary,      na.rm=TRUE),
    mean_ln_salary = mean(ln_salary, na.rm=TRUE),
    sd_ln_salary   = sd(ln_salary,   na.rm=TRUE),
    n              = dplyr::n()
  ) |> print()

# ── Section 3: First-Stage and Instrument Relevance ──────────────────────────
cat("\n* SECTION 3: Instrument Relevance\n")

X_ctrl_vars <- c("female","black","hispanic","asian","age_ba","firstgen",
                 "parent_income_q","parent_grad","ugpa","stem_major",
                 "bus_major","ed_major","selective_inst","public_ug",
                 "state_unemp","metro")
X_ctrl_f <- paste(X_ctrl_vars, collapse = " + ")

# First stage — replaces: reg masters ga_funding_adj $X_controls, robust
fs_fml      <- as.formula(paste("masters ~ ga_funding_adj +", X_ctrl_f))
first_stage <- lm(fs_fml, data = bb)
fs_vcov     <- sandwich::vcovHC(first_stage, type = "HC1")
fs_ct       <- lmtest::coeftest(first_stage, vcov. = fs_vcov)
cat("First-stage regression:\n"); print(fs_ct)

# F-test on instrument — replaces: test ga_funding_adj
fs_F <- as.numeric(lmtest::waldtest(first_stage, .~. - ga_funding_adj,
                                     vcov = fs_vcov)$F[2])
cat(sprintf("\nFirst-stage F: %.2f  %s\n", fs_F,
            ifelse(fs_F > 10, "RESULT: Strong instrument (F > 10)",
                              "WARNING: Potentially weak instrument")))

# ── Section 4: Naive OLS Estimation ──────────────────────────────────────────
cat("\n* SECTION 4: Naive OLS Estimation\n")

ols_fml    <- as.formula(paste("ln_salary ~ masters +", X_ctrl_f))
ols_naive  <- lm(ols_fml, data = bb)
ols_vcov   <- sandwich::vcovHC(ols_naive, type = "HC1")
ols_ct     <- lmtest::coeftest(ols_naive, vcov. = ols_vcov)
ols_est    <- coef(ols_naive)["masters"]
ols_se     <- sqrt(ols_vcov["masters","masters"])
cat(sprintf("OLS estimate: %.4f  (SE = %.4f)\n", ols_est, ols_se))
print(ols_ct)

# ── Section 5: IV/2SLS Estimation (LATE) ─────────────────────────────────────
cat("\n* SECTION 5: IV/2SLS Estimation (LATE)\n")
# Replaces: ivregress 2sls ln_salary $X_controls (masters = ga_funding_adj), robust first

iv_fml  <- as.formula(
  paste("ln_salary ~ masters +", X_ctrl_f, "| ga_funding_adj +", X_ctrl_f))
iv_2sls <- ivreg::ivreg(iv_fml, data = bb)
iv_vcov <- sandwich::vcovHC(iv_2sls, type = "HC1")
iv_ct   <- lmtest::coeftest(iv_2sls, vcov. = iv_vcov)
iv_est  <- coef(iv_2sls)["masters"]
iv_se   <- sqrt(iv_vcov["masters","masters"])
cat(sprintf("\nIV/LATE estimate: %.4f  (SE = %.4f)\n", iv_est, iv_se))
print(summary(iv_2sls, diagnostics = TRUE))

# ── Section 6: MTE Estimation — Pooled Polynomial ────────────────────────────
cat("\n* SECTION 6: MTE Estimation — Pooled Polynomial\n")
# Replaces: probit masters + predict phat + reg with interactions
# NOTE: mtefe has no CRAN equivalent; manual polynomial MTE implemented here.

probit_fml <- as.formula(paste("masters ~ ga_funding_adj +", X_ctrl_f))
probit_fit <- glm(probit_fml, data = bb, family = binomial("probit"))
bb$phat    <- predict(probit_fit, type = "response")  # Pr(masters=1)
bb$z_index <- predict(probit_fit, type = "link")       # linear index
ga_coef    <- coef(probit_fit)["ga_funding_adj"]
cat(sprintf("GA funding probit coefficient: %.5f\n", ga_coef))

bb$phat2 <- bb$phat^2
bb$phat3 <- bb$phat^3

# — Quadratic MTE — replaces: reg ln_salary masters c.masters#(c.phat c.phat2)
bb <- bb |> dplyr::mutate(
  m_phat  = masters * phat,
  m_phat2 = masters * phat2
)
mte_quad_fml <- as.formula(
  paste("ln_salary ~ masters + m_phat + m_phat2 + phat + phat2 +", X_ctrl_f))
mte_quad <- lm(mte_quad_fml, data = bb)
mte_quad_vcov <- sandwich::vcovHC(mte_quad, type = "HC1")
b0_quad <- coef(mte_quad)["masters"]
b1_quad <- coef(mte_quad)["m_phat"]
b2_quad <- coef(mte_quad)["m_phat2"]
ate_est_quad <- b0_quad + b1_quad/2 + b2_quad/3
cat(sprintf("\nQuadratic MTE(u) = %.4f + %.4f*u + %.4f*u^2\n", b0_quad, b1_quad, b2_quad))
cat(sprintf("ATE (quadratic): %.4f\n", ate_est_quad))

# — Cubic MTE — replaces: reg ln_salary masters c.masters#(c.phat c.phat2 c.phat3)
bb <- bb |> dplyr::mutate(m_phat3 = masters * phat3)
mte_cubic_fml <- as.formula(
  paste("ln_salary ~ masters + m_phat + m_phat2 + m_phat3 + phat + phat2 + phat3 +",
        X_ctrl_f))
mte_cubic <- lm(mte_cubic_fml, data = bb)
mte_cubic_vcov <- sandwich::vcovHC(mte_cubic, type = "HC1")
b0 <- coef(mte_cubic)["masters"]
b1 <- coef(mte_cubic)["m_phat"]
b2 <- coef(mte_cubic)["m_phat2"]
b3 <- coef(mte_cubic)["m_phat3"]
cat(sprintf("\nCubic MTE(u) = %.4f + %.4f*u + %.4f*u^2 + %.4f*u^3\n", b0,b1,b2,b3))

ate_est_cubic <- b0 + b1/2 + b2/3 + b3/4
bb$mte_hat <- b0 + b1*bb$phat + b2*bb$phat2 + b3*bb$phat3

att_est <- mean(bb$mte_hat[bb$masters == 1], na.rm = TRUE)
atu_est <- mean(bb$mte_hat[bb$masters == 0], na.rm = TRUE)
cat(sprintf("Estimated ATE (cubic): %.4f\n", ate_est_cubic))
cat(sprintf("Estimated ATT:         %.4f\n", att_est))
cat(sprintf("Estimated ATU:         %.4f\n", atu_est))

# — Heckman Selection — replaces: heckman ... twostep / heckman (ML)
cat("\n--- Heckman Selection Model ---\n")
heck_fml_out <- as.formula(paste("ln_salary ~", X_ctrl_f))
heck_fml_sel <- as.formula(paste("masters ~ ga_funding_adj +", X_ctrl_f))

tryCatch({
  # Two-step — replaces: heckman ... twostep
  heck_2step <- sampleSelection::heckit(
    selection = heck_fml_sel,
    outcome   = heck_fml_out,
    data      = bb,
    method    = "2step"
  )
  cat("Heckman 2-step:\n"); print(summary(heck_2step))

  # ML — replaces: heckman (without twostep option)
  heck_ml <- sampleSelection::heckit(
    selection = heck_fml_sel,
    outcome   = heck_fml_out,
    data      = bb,
    method    = "ml"
  )
  heck_ml_rho    <- heck_ml$estimate[["rho"]]
  heck_ml_sigma  <- heck_ml$estimate[["sigma"]]
  heck_ml_lambda <- heck_ml_rho * heck_ml_sigma
  cat(sprintf("\nHeckman ML: lambda = %.4f  rho = %.4f\n",
              heck_ml_lambda, heck_ml_rho))
}, error = function(e) cat("[Heckman] Error:", e$message, "\n"))

# ── Section 6b: MTE by Graduate Program Area ──────────────────────────────────
cat("\n* SECTION 6b: MTE by Graduate Program Area\n")

# Interaction variables — replaces: c.masters#c.ma_stem etc.
bb <- bb |> dplyr::mutate(
  m_mastem    = masters * ma_stem,     m_mabus  = masters * ma_business,
  m_maed      = masters * ma_education, m_mahlth = masters * ma_health,
  m_mastem_p  = masters * ma_stem    * phat,  m_mabus_p  = masters * ma_business * phat,
  m_maed_p    = masters * ma_education * phat, m_mahlth_p = masters * ma_health   * phat,
  m_mastem_p2 = masters * ma_stem    * phat2, m_mabus_p2 = masters * ma_business * phat2,
  m_maed_p2   = masters * ma_education * phat2, m_mahlth_p2= masters * ma_health  * phat2,
  m_mastem_p3 = masters * ma_stem    * phat3, m_mabus_p3 = masters * ma_business * phat3,
  m_maed_p3   = masters * ma_education * phat3, m_mahlth_p3= masters * ma_health  * phat3
)

area_fml <- as.formula(paste(
  "ln_salary ~ masters + m_phat + m_phat2 + m_phat3",
  "+ m_mastem + m_mabus + m_maed + m_mahlth",
  "+ m_mastem_p + m_mabus_p + m_maed_p + m_mahlth_p",
  "+ m_mastem_p2 + m_mabus_p2 + m_maed_p2 + m_mahlth_p2",
  "+ m_mastem_p3 + m_mabus_p3 + m_maed_p3 + m_mahlth_p3",
  "+ phat + phat2 + phat3 +", X_ctrl_f))
mte_byarea    <- lm(area_fml, data = bb)
mte_ba_coefs  <- coef(mte_byarea)

# Base (Other) polynomial — as.numeric() strips coefficient name tags
# to prevent compound names (e.g. "c0.masters.m_mastem") that cause
# cc["c0"] to return NA downstream.
B0 <- as.numeric(mte_ba_coefs["masters"])
B1 <- as.numeric(mte_ba_coefs["m_phat"])
B2 <- as.numeric(mte_ba_coefs["m_phat2"])
B3 <- as.numeric(mte_ba_coefs["m_phat3"])

# Composite area coefficients — replaces: local c0_stem = `B0' + `d0_stem'
ba_lookup <- list(
  stem     = list(d0="m_mastem",  d1="m_mastem_p",  d2="m_mastem_p2",  d3="m_mastem_p3"),
  business = list(d0="m_mabus",   d1="m_mabus_p",   d2="m_mabus_p2",   d3="m_mabus_p3"),
  education= list(d0="m_maed",    d1="m_maed_p",    d2="m_maed_p2",    d3="m_maed_p3"),
  health   = list(d0="m_mahlth",  d1="m_mahlth_p",  d2="m_mahlth_p2",  d3="m_mahlth_p3")
)

area_c <- list()
for (a in names(ba_lookup)) {
  lk <- ba_lookup[[a]]
  c0 <- B0 + as.numeric(mte_ba_coefs[lk$d0])
  c1 <- B1 + as.numeric(mte_ba_coefs[lk$d1])
  c2 <- B2 + as.numeric(mte_ba_coefs[lk$d2])
  c3 <- B3 + as.numeric(mte_ba_coefs[lk$d3])
  area_c[[a]] <- c(c0=c0, c1=c1, c2=c2, c3=c3)
}
area_c[["other"]] <- c(c0=B0, c1=B1, c2=B2, c3=B3)

cat("\n--- Area-Specific MTE Functions ---\n")
for (a in c("other","stem","business","education","health")) {
  cc <- area_c[[a]]
  cat(sprintf("  %-10s %.4f + %.4f*u + %.4f*u^2 + %.4f*u^3\n",
              a, cc["c0"], cc["c1"], cc["c2"], cc["c3"]))
}

# Area-specific ATE — integral of MTE over [0,1]
ate_areas <- sapply(area_c, function(cc)
  cc["c0"] + cc["c1"]/2 + cc["c2"]/3 + cc["c3"]/4)

# Area-specific MTE_hat variables
for (a in names(area_c)) {
  cc <- area_c[[a]]
  bb[[paste0("mte_hat_",a)]] <- cc["c0"] + cc["c1"]*bb$phat +
                                  cc["c2"]*bb$phat2 + cc["c3"]*bb$phat3
}

# Area-specific ATT
att_areas <- sapply(names(area_c), function(a) {
  mask <- if (a == "other") bb$ma_other == 1 else bb[[paste0("ma_",a)]] == 1
  mean(bb[[paste0("mte_hat_",a)]][mask], na.rm = TRUE)
})

cat("\n--- Area-Specific ATE (integral_0^1 MTE_a(u) du) ---\n")
for (a in names(ate_areas))
  cat(sprintf("  ATE (%s): %.4f\n", a, ate_areas[a]))

cat("\n--- Area-Specific ATT ---\n")
for (a in names(att_areas))
  cat(sprintf("  ATT (%s): %.4f\n", a, att_areas[a]))

atu_pooled <- mean(bb$mte_hat[bb$masters == 0], na.rm = TRUE)
cat(sprintf("  ATU (pooled): %.4f\n", atu_pooled))

# ── Section 6c: Bootstrap SEs ─────────────────────────────────────────────────
cat("\n* SECTION 6c: Cluster Bootstrap (G=50 states, R=500)\n")
# Replaces: postfile/forvalues manual cluster bootstrap

set.seed(20260101)
R_boot   <- 500L
states   <- unique(bb$state)
n_states <- length(states)

bs_store <- matrix(NA_real_, nrow = R_boot, ncol = 14,
  dimnames = list(NULL, c("b_ate","b_att","b_atu",
    "b_ate_stem","b_att_stem","b_ate_bus","b_att_bus",
    "b_ate_ed","b_att_ed","b_ate_hlth","b_att_hlth",
    "b_ate_oth","b_att_oth","ok")))
n_ok <- 0L

cat("Running bootstrap...\n")
for (rr in seq_len(R_boot)) {
  ok <- TRUE

  # Cluster resample — replaces: bsample, cluster(state) idcluster(newstate)
  samp_states <- sample(states, n_states, replace = TRUE)
  bb_b <- do.call(rbind, lapply(seq_along(samp_states), function(jj) {
    d <- bb[bb$state == samp_states[jj], ]
    d$newstate <- jj
    d
  }))

  # Probit
  pb_fit <- tryCatch(
    glm(as.formula(paste("masters ~ ga_funding_adj +", X_ctrl_f)),
        data = bb_b, family = binomial("probit")),
    error = function(e) { ok <<- FALSE; NULL }
  )
  if (!ok) { bs_store[rr, "ok"] <- 0; next }
  bb_b$`_pb`  <- predict(pb_fit, type = "response")
  bb_b$`_pb2` <- bb_b$`_pb`^2
  bb_b$`_pb3` <- bb_b$`_pb`^3
  bb_b$m_pb   <- bb_b$masters * bb_b$`_pb`
  bb_b$m_pb2  <- bb_b$masters * bb_b$`_pb2`
  bb_b$m_pb3  <- bb_b$masters * bb_b$`_pb3`

  # Pooled cubic MTE
  cubic_b <- tryCatch(
    lm(as.formula(paste(
      "ln_salary ~ masters + m_pb + m_pb2 + m_pb3 +",
      "`_pb` + `_pb2` + `_pb3` +", X_ctrl_f)), data = bb_b),
    error = function(e) { ok <<- FALSE; NULL }
  )
  if (!ok) { bs_store[rr, "ok"] <- 0; next }
  r0 <- coef(cubic_b)["masters"]
  r1 <- coef(cubic_b)["m_pb"]
  r2 <- coef(cubic_b)["m_pb2"]
  r3 <- coef(cubic_b)["m_pb3"]
  b_ate_r <- r0 + r1/2 + r2/3 + r3/4
  mb_hat  <- r0 + r1*bb_b$`_pb` + r2*bb_b$`_pb2` + r3*bb_b$`_pb3`
  b_att_r <- mean(mb_hat[bb_b$masters==1], na.rm=TRUE)
  b_atu_r <- mean(mb_hat[bb_b$masters==0], na.rm=TRUE)

  # Fully interacted MTE — add area interaction columns to bootstrap frame
  for (a_short in c("mastem","mabus","maed","mahlth")) {
    ma_col <- switch(a_short, mastem="ma_stem", mabus="ma_business",
                               maed="ma_education", mahlth="ma_health")
    bb_b[[paste0("m_",a_short)]]    <- bb_b$masters * bb_b[[ma_col]]
    bb_b[[paste0("m_",a_short,"_p")]]  <- bb_b$masters * bb_b[[ma_col]] * bb_b$`_pb`
    bb_b[[paste0("m_",a_short,"_p2")]] <- bb_b$masters * bb_b[[ma_col]] * bb_b$`_pb2`
    bb_b[[paste0("m_",a_short,"_p3")]] <- bb_b$masters * bb_b[[ma_col]] * bb_b$`_pb3`
  }
  area_b_fml <- as.formula(paste(
    "ln_salary ~ masters + m_pb + m_pb2 + m_pb3",
    "+ m_mastem + m_mabus + m_maed + m_mahlth",
    "+ m_mastem_p + m_mabus_p + m_maed_p + m_mahlth_p",
    "+ m_mastem_p2 + m_mabus_p2 + m_maed_p2 + m_mahlth_p2",
    "+ m_mastem_p3 + m_mabus_p3 + m_maed_p3 + m_mahlth_p3",
    "+ `_pb` + `_pb2` + `_pb3` +", X_ctrl_f))
  area_b_fit <- tryCatch(lm(area_b_fml, data = bb_b),
                          error = function(e) { ok <<- FALSE; NULL })
  if (!ok) { bs_store[rr, "ok"] <- 0; next }

  cb <- coef(area_b_fit)
  BB0 <- cb["masters"]; BB1 <- cb["m_pb"]; BB2 <- cb["m_pb2"]; BB3 <- cb["m_pb3"]

  # Compute area-specific MTE parameters — as.numeric() strips coefficient names
  # to prevent named-vector corruption when assigning into bs_store.
  ba_lookup_b <- list(
    stem     = c("m_mastem","m_mastem_p","m_mastem_p2","m_mastem_p3"),
    business = c("m_mabus",  "m_mabus_p",  "m_mabus_p2",  "m_mabus_p3"),
    education= c("m_maed",   "m_maed_p",   "m_maed_p2",   "m_maed_p3"),
    health   = c("m_mahlth", "m_mahlth_p", "m_mahlth_p2", "m_mahlth_p3")
  )

  # Direct column assignment — avoids fragile named-vector extension
  bs_store[rr, "b_ate"] <- as.numeric(b_ate_r)
  bs_store[rr, "b_att"] <- as.numeric(b_att_r)
  bs_store[rr, "b_atu"] <- as.numeric(b_atu_r)

  for (aa in c("stem","business","education","health")) {
    nms    <- ba_lookup_b[[aa]]
    C0 <- as.numeric(BB0) + as.numeric(cb[nms[1]])
    C1 <- as.numeric(BB1) + as.numeric(cb[nms[2]])
    C2 <- as.numeric(BB2) + as.numeric(cb[nms[3]])
    C3 <- as.numeric(BB3) + as.numeric(cb[nms[4]])
    ate_r  <- C0 + C1/2 + C2/3 + C3/4
    ma_col <- switch(aa, stem="ma_stem", business="ma_business",
                         education="ma_education", health="ma_health")
    ms_hat <- C0 + C1*bb_b$`_pb` + C2*bb_b$`_pb2` + C3*bb_b$`_pb3`
    att_r  <- mean(ms_hat[bb_b[[ma_col]] == 1], na.rm = TRUE)
    sfx    <- switch(aa, stem="stem", business="bus", education="ed", health="hlth")
    bs_store[rr, paste0("b_ate_", sfx)] <- as.numeric(ate_r)
    bs_store[rr, paste0("b_att_", sfx)] <- as.numeric(att_r)
  }
  # Other (base)
  mb_oth <- as.numeric(BB0) + as.numeric(BB1)*bb_b$`_pb` +
            as.numeric(BB2)*bb_b$`_pb2` + as.numeric(BB3)*bb_b$`_pb3`
  bs_store[rr, "b_ate_oth"] <- as.numeric(BB0) + as.numeric(BB1)/2 +
                                as.numeric(BB2)/3 + as.numeric(BB3)/4
  bs_store[rr, "b_att_oth"] <- mean(mb_oth[bb_b$ma_other == 1], na.rm = TRUE)
  bs_store[rr, "ok"] <- 1
  n_ok <- n_ok + 1L
  if (rr %% 10 == 0) cat(".")
}
cat(sprintf("\nBootstrap complete: %d of %d reps successful\n", n_ok, R_boot))

# Extract SEs — replaces: quietly sum b_ate / local ate_se = r(sd)
bs_ok  <- bs_store[!is.na(bs_store[,"ok"]) & bs_store[,"ok"]==1, , drop=FALSE]
ate_se <- sd(bs_ok[,"b_ate"],      na.rm=TRUE)
att_se <- sd(bs_ok[,"b_att"],      na.rm=TRUE)
atu_se <- sd(bs_ok[,"b_atu"],      na.rm=TRUE)
se_tbl <- data.frame(
  parameter  = c("ate","att","atu","ate_stem","att_stem","ate_bus","att_bus",
                 "ate_ed","att_ed","ate_hlth","att_hlth","ate_oth","att_oth"),
  bs_se      = c(ate_se, att_se, atu_se,
                 sd(bs_ok[,"b_ate_stem"],na.rm=TRUE), sd(bs_ok[,"b_att_stem"],na.rm=TRUE),
                 sd(bs_ok[,"b_ate_bus"], na.rm=TRUE), sd(bs_ok[,"b_att_bus"], na.rm=TRUE),
                 sd(bs_ok[,"b_ate_ed"],  na.rm=TRUE), sd(bs_ok[,"b_att_ed"],  na.rm=TRUE),
                 sd(bs_ok[,"b_ate_hlth"],na.rm=TRUE), sd(bs_ok[,"b_att_hlth"],na.rm=TRUE),
                 sd(bs_ok[,"b_ate_oth"], na.rm=TRUE), sd(bs_ok[,"b_att_oth"], na.rm=TRUE))
)
ate_se_stem <- se_tbl$bs_se[se_tbl$parameter=="ate_stem"]
att_se_stem <- se_tbl$bs_se[se_tbl$parameter=="att_stem"]
ate_se_business <- se_tbl$bs_se[se_tbl$parameter=="ate_bus"]
att_se_business <- se_tbl$bs_se[se_tbl$parameter=="att_bus"]
ate_se_education <- se_tbl$bs_se[se_tbl$parameter=="ate_ed"]
att_se_education <- se_tbl$bs_se[se_tbl$parameter=="att_ed"]
ate_se_health <- se_tbl$bs_se[se_tbl$parameter=="ate_hlth"]
att_se_health <- se_tbl$bs_se[se_tbl$parameter=="att_hlth"]
ate_se_other <- se_tbl$bs_se[se_tbl$parameter=="ate_oth"]
att_se_other <- se_tbl$bs_se[se_tbl$parameter=="att_oth"]

# Alias mtefe SEs to manual bootstrap SEs — replaces: local mtefe_*_se = `ate_se'
mtefe_ate_se <- ate_se; mtefe_att_se <- att_se; mtefe_atu_se <- atu_se
mtefe_late_se <- iv_se   # LATE SE from IV/2SLS Section 5
mtefe_ate_q_se <- ate_se; mtefe_att_q_se <- att_se; mtefe_atu_q_se <- atu_se

# Wild cluster bootstrap for OLS and IV — replaces: boottest masters, cluster(state)
cat("\n--- Wild Cluster Bootstrap: OLS ---\n")
if (has_fwcb) {
  tryCatch({
    wcb_ols <- fwildclusterboot::boottest(
      ols_naive, clustid = bb$state, param = "masters",
      B = 9999, seed = 20251130, type = "webb")
    cat(sprintf("  p-value (wild cluster OLS): %.4f\n", wcb_ols$p_val))
  }, error = function(e) cat("[wildboot OLS] Error:", e$message, "\n"))
} else {
  # Fallback: clustered SE t-test — replaces: boottest when fwildclusterboot absent
  ols_cl_vcov <- sandwich::vcovCL(ols_naive, cluster = ~bb$state)
  ols_cl_ct   <- lmtest::coeftest(ols_naive, vcov. = ols_cl_vcov)
  cat(sprintf("  [Fallback] Cluster-robust SE (OLS): %.4f  p = %.4f\n",
              ols_cl_ct["masters","Std. Error"], ols_cl_ct["masters","Pr(>|t|)"]))
}

cat("--- Wild Cluster Bootstrap: IV ---\n")
if (has_fwcb) {
  tryCatch({
    wcb_iv <- fwildclusterboot::boottest(
      iv_2sls, clustid = bb$state, param = "masters",
      B = 9999, seed = 20251130, type = "webb")
    cat(sprintf("  p-value (wild cluster IV): %.4f\n", wcb_iv$p_val))
  }, error = function(e) cat("[wildboot IV] Error:", e$message, "\n"))
} else {
  iv_cl_vcov <- sandwich::vcovCL(iv_2sls, cluster = ~bb$state)
  iv_cl_ct   <- lmtest::coeftest(iv_2sls, vcov. = iv_cl_vcov)
  cat(sprintf("  [Fallback] Cluster-robust SE (IV): %.4f  p = %.4f\n",
              iv_cl_ct["masters","Std. Error"], iv_cl_ct["masters","Pr(>|t|)"]))
}

# Bootstrap SE summary
cat("\n--- Bootstrap SEs: Pooled Parameters ---\n")
cat(sprintf("  ATE = %.4f  (Bootstrap SE = %.4f)\n", ate_est_cubic, ate_se))
cat(sprintf("  ATT = %.4f  (Bootstrap SE = %.4f)\n", att_est,       att_se))
cat(sprintf("  ATU = %.4f  (Bootstrap SE = %.4f)\n", atu_est,       atu_se))

cat("\n--- Bootstrap SEs: Area-Specific ATE ---\n")
for (a in c("other","stem","business","education","health")) {
  ate_a <- ate_areas[a]
  se_a  <- get(paste0("ate_se_",a))
  cat(sprintf("  %-10s %.4f  (BS SE = %.4f)  [%.4f, %.4f]\n",
              a, ate_a, se_a, ate_a - 1.96*se_a, ate_a + 1.96*se_a))
}

# ── Section 7: Results Comparison ────────────────────────────────────────────
cat("\n* SECTION 7: Results Comparison\n")

cat(sprintf("  Naive OLS:             %.4f (SE = %.4f  — likely biased)\n", ols_est, ols_se))
cat(sprintf("  IV/LATE:               %.4f (SE = %.4f  — complier effect)\n", iv_est, iv_se))
cat(sprintf("  MTE-based ATE (cubic): %.4f (BS SE = %.4f)\n", ate_est_cubic, ate_se))
cat(sprintf("  MTE-based ATT:         %.4f (BS SE = %.4f)\n", att_est, att_se))
cat(sprintf("  MTE-based ATU:         %.4f (BS SE = %.4f)\n", atu_est, atu_se))

if      (att_est > ate_est_cubic & ate_est_cubic > atu_est) {
  cat("  ATT > ATE > ATU: POSITIVE SELECTION on gains\n")
} else if (att_est < ate_est_cubic & ate_est_cubic < atu_est) {
  cat("  ATT < ATE < ATU: NEGATIVE SELECTION on gains\n")
} else {
  cat("  Mixed selection pattern\n")
}

ols_bias <- (ols_est - ate_est_cubic) / ate_est_cubic * 100
cat(sprintf("OLS BIAS: %.1f%% relative to MTE-based ATE\n", ols_bias))

# ── Section 8: MTE Visualization ─────────────────────────────────────────────
cat("\n* SECTION 8: MTE Visualization\n")

u_seq <- seq(0.01, 0.99, length.out = 100)
mte_curve_df <- data.frame(
  u       = u_seq,
  mte_est = b0 + b1*u_seq + b2*u_seq^2 + b3*u_seq^3
)
fig_mte <- ggplot(mte_curve_df, aes(x = u, y = mte_est)) +
  geom_line(colour = "navy", linewidth = 1) +
  labs(title    = "Estimated MTE Curve — Pooled",
       subtitle = "Master's Degree Effect on Log Salary",
       caption  = "Declining MTE indicates positive selection on gains",
       x = "u (Unobserved Resistance to Treatment)",
       y = "Marginal Treatment Effect") +
  theme_bw()
save_fig(fig_mte, "fig10_6_mte_curve_R.png")

# MTE by propensity score decile — replaces: xtile p_decile + collapse
bb$p_decile <- dplyr::ntile(bb$phat, 10)
decile_df <- bb |>
  dplyr::group_by(p_decile) |>
  dplyr::summarise(mte_mean = mean(mte_hat, na.rm=TRUE),
                   mte_sd   = sd(mte_hat,   na.rm=TRUE),
                   n        = dplyr::n(), .groups="drop")
cat("\nEstimated MTE by Propensity Score Decile:\n"); print(decile_df)

fig_decile <- ggplot(decile_df, aes(x = p_decile, y = mte_mean)) +
  geom_point(colour = "navy", size = 3) +
  geom_line(colour  = "navy") +
  labs(title    = "Estimated MTE by Propensity Score Decile",
       subtitle = "Evidence of Treatment Effect Heterogeneity",
       x = "Propensity Score Decile", y = "Mean Estimated MTE") +
  theme_bw()
save_fig(fig_decile, "fig10_7_mte_by_decile_R.png")

# MTE curves by program area
area_palette <- c(Health="firebrick", STEM="navy", Business="darkgreen",
                  Education="orange", Other="grey50")
mte_area_df <- data.frame(u = u_seq) |>
  dplyr::mutate(
    Health    = area_c[["health"]]   ["c0"] + area_c[["health"]]   ["c1"]*u + area_c[["health"]]   ["c2"]*u^2 + area_c[["health"]]   ["c3"]*u^3,
    STEM      = area_c[["stem"]]     ["c0"] + area_c[["stem"]]     ["c1"]*u + area_c[["stem"]]     ["c2"]*u^2 + area_c[["stem"]]     ["c3"]*u^3,
    Business  = area_c[["business"]] ["c0"] + area_c[["business"]] ["c1"]*u + area_c[["business"]] ["c2"]*u^2 + area_c[["business"]] ["c3"]*u^3,
    Education = area_c[["education"]][ "c0"] + area_c[["education"]][ "c1"]*u + area_c[["education"]][ "c2"]*u^2 + area_c[["education"]][ "c3"]*u^3,
    Other     = area_c[["other"]]    ["c0"] + area_c[["other"]]    ["c1"]*u + area_c[["other"]]    ["c2"]*u^2 + area_c[["other"]]    ["c3"]*u^3
  ) |>
  tidyr::pivot_longer(-u, names_to = "Area", values_to = "MTE")

fig_area <- ggplot(mte_area_df, aes(x = u, y = MTE, colour = Area, linetype = Area)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = area_palette) +
  scale_linetype_manual(values = c(Health="solid", STEM="solid",
                                    Business="solid", Education="solid",
                                    Other="dashed")) +
  labs(title = "MTE Curves by Graduate Program Area",
       subtitle = "Field-specific returns to master's degree",
       x = "u (Unobserved Resistance to Treatment)",
       y = "Marginal Treatment Effect") +
  theme_bw() + theme(legend.position = "bottom")
save_fig(fig_area, "fig10_8_mte_byarea_curve_R.png")

# ── Section 9: Basic Policy Simulation (PRTE) ─────────────────────────────────
cat("\n* SECTION 9: Basic Policy Simulation (PRTE)\n")

ga_current <- mean(bb$ga_funding_adj, na.rm = TRUE)
ga_new     <- ga_current * 1.2
cat(sprintf("Current mean GA: $%.2fk  Proposed (20%% increase): $%.2fk\n",
            ga_current, ga_new))

bb$p_new   <- pnorm(bb$z_index + ga_coef * (ga_new - bb$ga_funding_adj))
bb$delta_p <- bb$p_new - bb$phat
delta_mean <- mean(bb$delta_p, na.rm = TRUE)
cat(sprintf("Average increase in Pr(Master's): %.4f\n", delta_mean))

prte_20pct <- weighted.mean(bb$mte_hat[bb$delta_p > 0],
                             bb$delta_p[bb$delta_p > 0], na.rm = TRUE)
cat(sprintf("Approximate PRTE (20%% GA increase): %.4f\n", prte_20pct))
bb$p_new <- bb$delta_p <- NULL

# ── Section 10: MPRTE — Scenarios 1-4 ────────────────────────────────────────
cat("\n* SECTION 10: MPRTE — Scenarios 1-4\n")

mprte_calc <- function(df, ga_amount, pipeline = NULL, mte_col = "mte_hat") {
  # MPRTE = sum(MTE_i * h_i) / sum(h_i)
  # h_i = dnorm(qnorm(phat_i)) * ga_coef * amount [* pipeline_i]
  h <- dnorm(qnorm(df$phat)) * ga_coef * ga_amount
  if (!is.null(pipeline)) h <- h * pipeline
  sum(df[[mte_col]] * h, na.rm=TRUE) / sum(h, na.rm=TRUE)
}
prte_calc <- function(df, ga_amount, pipeline = NULL, mte_col = "mte_hat") {
  p_new   <- pnorm(df$z_index + ga_coef * ga_amount *
                     if (!is.null(pipeline)) pipeline else 1)
  delta_p <- p_new - df$phat
  mask    <- delta_p > 0 & !is.na(delta_p)
  weighted.mean(df[[mte_col]][mask], delta_p[mask], na.rm = TRUE)
}

mprte_unif  <- mprte_calc(bb, 1)
mprte_prte1 <- prte_calc(bb, 1)
cat(sprintf("MPRTE (uniform $1k):   %.4f\n", mprte_unif))
cat(sprintf("PRTE  (discrete $1k):  %.4f\n", mprte_prte1))

lowinc_mask  <- bb$parent_income_q <= 2
mprte_lowinc <- mprte_calc(bb, 2, lowinc_mask)
cat(sprintf("MPRTE (targeted low-income $2k): %.4f\n", mprte_lowinc))

mprte_stem <- mprte_calc(bb, 3, bb$stem_major)
cat(sprintf("MPRTE (STEM enhancement $3k):    %.4f\n", mprte_stem))

mprte_ed <- mprte_calc(bb, 2.5, bb$ed_major)
cat(sprintf("MPRTE (education major $2.5k):   %.4f\n", mprte_ed))

# ── Section 10b: MPRTE by Graduate Program Area (Scenarios 5-8) ───────────────
cat("\n* SECTION 10b: MPRTE by Graduate Program Area (Scenarios 5-8)\n")

mprte_ma_stem <- mprte_calc(bb, 2.5, bb$stem_major,   "mte_hat_stem")
prte_ma_stem  <- prte_calc( bb, 2.5, bb$stem_major,   "mte_hat_stem")
cat(sprintf("MPRTE (STEM grad pipeline, $2.5k):     %.4f\n", mprte_ma_stem))
cat(sprintf("PRTE  (STEM grad pipeline, $2.5k):     %.4f\n", prte_ma_stem))
cat(sprintf("Mean phat for STEM undergrads:          %.4f\n",
            mean(bb$phat[bb$stem_major==1], na.rm=TRUE)))

mprte_ma_bus  <- mprte_calc(bb, 2.5, bb$bus_major,    "mte_hat_business")
prte_ma_bus   <- prte_calc( bb, 2.5, bb$bus_major,    "mte_hat_business")
cat(sprintf("MPRTE (Business grad pipeline, $2.5k): %.4f\n", mprte_ma_bus))
cat(sprintf("PRTE  (Business grad pipeline, $2.5k): %.4f\n", prte_ma_bus))

mprte_ma_ed   <- mprte_calc(bb, 2.5, bb$ed_major,     "mte_hat_education")
prte_ma_ed    <- prte_calc( bb, 2.5, bb$ed_major,     "mte_hat_education")
cat(sprintf("MPRTE (Education grad pipeline, $2.5k): %.4f\n", mprte_ma_ed))
cat(sprintf("PRTE  (Education grad pipeline, $2.5k): %.4f\n", prte_ma_ed))

target_health  <- as.integer(bb$stem_major == 1 | bb$socsci_major == 1)
mprte_ma_hlth  <- mprte_calc(bb, 2.5, target_health,  "mte_hat_health")
prte_ma_hlth   <- prte_calc( bb, 2.5, target_health,  "mte_hat_health")
cat(sprintf("MPRTE (Health & Related pipeline, $2.5k): %.4f\n", mprte_ma_hlth))
cat(sprintf("PRTE  (Health & Related pipeline, $2.5k): %.4f\n", prte_ma_hlth))

# ── Section 11: MPRTE by Policy Intensity ─────────────────────────────────────
cat("\n* SECTION 11: MPRTE by Policy Intensity\n")

p_baseline <- mean(bb$phat, na.rm = TRUE)
intensity_df <- data.frame(ga_increase = seq(0.5, 10, by = 0.5)) |>
  dplyr::mutate(
    p_margin     = p_baseline + ga_increase * 0.015,
    mprte_approx = b0 + b1*p_margin + b2*p_margin^2 + b3*p_margin^3
  )
print(intensity_df)

fig_intensity <- ggplot(intensity_df, aes(x = ga_increase, y = mprte_approx)) +
  geom_line(colour = "navy", linewidth = 1) +
  labs(title    = "MPRTE by Policy Intensity",
       subtitle = "Marginal returns to GA funding expansion",
       x = "GA Funding Increase ($1000s)", y = "MPRTE") +
  theme_bw()
save_fig(fig_intensity, "fig10_9_mprte_by_intensity_R.png")

# ── Section 12: Comparing Treatment Effect Parameters ─────────────────────────
cat("\n* SECTION 12: Comparing Treatment Effect Parameters\n")

comp_tbl <- data.frame(
  Parameter = c("ATE","ATT","ATU","LATE (IV)"),
  Manual_cubic = c(ate_est_cubic, att_est, atu_est, iv_est),
  BS_SE_manual  = c(ate_se, att_se, atu_se, iv_se),
  mtefe_quad    = c(ate_est_cubic, att_est, atu_est, iv_est),  # aliased
  BS_SE_mtefe   = c(mtefe_ate_q_se, mtefe_att_q_se, mtefe_atu_q_se, mtefe_late_se)
)
print(comp_tbl)

cat("\nMPRTE Summary:\n")
cat(sprintf("  Uniform policy:         %.4f\n", mprte_unif))
cat(sprintf("  Low-income targeted:    %.4f\n", mprte_lowinc))
cat(sprintf("  STEM ug pipeline:       %.4f\n", mprte_stem))
cat(sprintf("  Education ug pipeline:  %.4f\n", mprte_ed))
cat(sprintf("  STEM grad pipeline:     %.4f\n", mprte_ma_stem))
cat(sprintf("  Business grad pipeline: %.4f\n", mprte_ma_bus))
cat(sprintf("  Education pipeline:     %.4f\n", mprte_ma_ed))
cat(sprintf("  Health & Related:       %.4f\n", mprte_ma_hlth))

# ── Section 13: MPRTE Visualization ───────────────────────────────────────────
cat("\n* SECTION 13: MPRTE Visualization\n")

mte_policy_df <- data.frame(u = u_seq) |>
  dplyr::mutate(
    mte            = b0 + b1*u + b2*u^2 + b3*u^3,
    region_lowinc  = (u >= 0.10 & u <= 0.25),
    region_uniform = (u >= 0.25 & u <= 0.40)
  )

fig_policy <- ggplot(mte_policy_df, aes(x = u, y = mte)) +
  geom_area(data = dplyr::filter(mte_policy_df, region_lowinc),
            fill = "firebrick", alpha = 0.3) +
  geom_area(data = dplyr::filter(mte_policy_df, region_uniform),
            fill = "navy",     alpha = 0.3) +
  geom_line(colour = "navy", linewidth = 1) +
  labs(title = "MTE Curve with Policy-Relevant Regions",
       x = "u (Unobserved Resistance to Treatment)",
       y = "Marginal Treatment Effect") +
  annotate("text", x = 0.175, y = min(mte_policy_df$mte)+0.02,
           label = "Low-income margin", colour = "firebrick", size = 3) +
  annotate("text", x = 0.325, y = min(mte_policy_df$mte)+0.02,
           label = "Uniform policy margin", colour = "navy", size = 3) +
  theme_bw()
save_fig(fig_policy, "fig10_10_mte_policy_regions_R.png")

# MTE by propensity score (histogram + scatter) — replaces: bar + scatter dual axis
bb$p_bin <- floor(bb$phat * 20) / 20
pbin_df  <- bb |>
  dplyr::group_by(p_bin) |>
  dplyr::summarise(mean_mte = mean(mte_hat, na.rm=TRUE),
                   n_bin    = dplyr::n(), .groups="drop")

scale_factor <- max(pbin_df$n_bin) / max(abs(pbin_df$mean_mte), na.rm=TRUE)
fig_prop <- ggplot(pbin_df, aes(x = p_bin)) +
  geom_bar(aes(y = n_bin / scale_factor), stat = "identity",
           fill = "grey70", alpha = 0.6, width = 0.04) +
  geom_point(aes(y = mean_mte), colour = "navy", shape = 18, size = 3) +
  geom_line(aes(y = mean_mte),  colour = "navy") +
  scale_y_continuous(
    name = "Estimated MTE",
    sec.axis = sec_axis(~ . * scale_factor, name = "Frequency")) +
  labs(title = "MTE by Propensity Score", x = "Propensity Score") +
  theme_bw()
save_fig(fig_prop, "fig10_11_mte_by_propensity_R.png")

# ── Section 14: Policy Cost-Benefit Analysis ─────────────────────────────────
cat("\n* SECTION 14: Policy Cost-Benefit Analysis\n")

cost_per_degree <- 50000; career_years <- 30; discount_rate <- 0.03
base_salary     <- 47000
pv_factor       <- (1 - (1 + discount_rate)^(-career_years)) / discount_rate
cat(sprintf("Present value factor (30 years, 3%%): %.2f\n\n", pv_factor))

cba_scenarios <- data.frame(
  Policy     = c("Uniform","Low-income","STEM ug","Education ug"),
  MPRTE      = c(mprte_unif, mprte_lowinc, mprte_stem, mprte_ed),
  Base_salary= base_salary
) |> dplyr::mutate(
  Annual_gain = Base_salary * (exp(MPRTE) - 1),
  PV_gain     = Annual_gain * pv_factor,
  BC_ratio    = PV_gain / cost_per_degree
)
cat("--- Scenarios 1-4: Original MPRTE-based CBA ---\n")
print(cba_scenarios)

base_salaries <- c(STEM=65000, Business=60000, Education=42000, Health=68000)
cba_pipeline <- data.frame(
  Pipeline    = c("STEM","Business","Education","Health"),
  MPRTE       = c(mprte_ma_stem, mprte_ma_bus, mprte_ma_ed, mprte_ma_hlth),
  Base_salary = base_salaries
) |> dplyr::mutate(
  Annual_gain = Base_salary * (exp(MPRTE) - 1),
  PV_gain     = Annual_gain * pv_factor,
  BC_ratio    = PV_gain / cost_per_degree
)
cat("\n--- Scenarios 5-8: Graduate Program Area MPRTE-based CBA ---\n")
print(cba_pipeline)
cat("Note: B/C > 1 suggests policy expansion is beneficial (synthetic data only).\n")

# ── Section 15: Save Results ───────────────────────────────────────────────────
cat("\n* SECTION 15: Save Results\n")

bb_save <- bb |> dplyr::select(
  id, masters, ln_salary, salary, phat, z_index, mte_hat,
  ma_stem, ma_business, ma_education, ma_health, ma_other,
  mte_hat_stem, mte_hat_business, mte_hat_education,
  mte_hat_health, mte_hat_other
)
haven::write_dta(bb_save, "bb_mte_analysis.dta")
cat("Saved: bb_mte_analysis.dta\n")

bb |>
  dplyr::group_by(stem_major, ed_major) |>
  dplyr::summarise(
    masters       = mean(masters,       na.rm=TRUE),
    ln_salary     = mean(ln_salary,     na.rm=TRUE),
    phat          = mean(phat,          na.rm=TRUE),
    mte_hat       = mean(mte_hat,       na.rm=TRUE),
    ma_stem       = mean(ma_stem,       na.rm=TRUE),
    ma_business   = mean(ma_business,   na.rm=TRUE),
    ma_education  = mean(ma_education,  na.rm=TRUE),
    ma_health     = mean(ma_health,     na.rm=TRUE),
    ma_other      = mean(ma_other,      na.rm=TRUE),
    sd_mte        = sd(mte_hat,         na.rm=TRUE),
    n             = dplyr::n(), .groups="drop"
  ) |>
  readr::write_csv("mte_summary_by_field.csv")
cat("Saved: mte_summary_by_field.csv\n")

bb |>
  dplyr::filter(masters == 1) |>
  dplyr::group_by(ma_stem, ma_business, ma_education, ma_health, ma_other) |>
  dplyr::summarise(
    ln_salary = mean(ln_salary, na.rm=TRUE), salary = mean(salary, na.rm=TRUE),
    phat      = mean(phat,      na.rm=TRUE), mte_hat = mean(mte_hat, na.rm=TRUE),
    mte_hat_stem     = mean(mte_hat_stem,     na.rm=TRUE),
    mte_hat_business = mean(mte_hat_business, na.rm=TRUE),
    mte_hat_education= mean(mte_hat_education,na.rm=TRUE),
    mte_hat_health   = mean(mte_hat_health,   na.rm=TRUE),
    mte_hat_other    = mean(mte_hat_other,    na.rm=TRUE),
    n = dplyr::n(), .groups="drop"
  ) |>
  readr::write_csv("mte_summary_by_program_area.csv")
cat("Saved: mte_summary_by_program_area.csv\n")

# ── Section 16: Final Summary ─────────────────────────────────────────────────
cat("\n", strrep("=", 50), "\n")
cat("ANALYSIS COMPLETE\n")
cat(strrep("=", 50), "\n")
cat(sprintf("  1.  Treatment rate:                  %.3f\n",  treat_rate))
cat(sprintf("  2.  OLS estimate (biased):           %.4f\n",  ols_est))
cat(sprintf("  3.  IV/LATE estimate:                %.4f\n",  iv_est))
cat(sprintf("  4.  MTE-based ATE (cubic):           %.4f  (BS SE = %.4f)\n", ate_est_cubic, ate_se))
cat(sprintf("  5.  MTE-based ATT:                   %.4f  (BS SE = %.4f)\n", att_est,       att_se))
cat(sprintf("  6.  MTE-based ATU:                   %.4f  (BS SE = %.4f)\n", atu_est,       atu_se))
cat(sprintf("  7.  First-stage F:                   %.1f\n",  fs_F))

cat("\nAREA-SPECIFIC ATE:\n")
for (a in c("other","stem","business","education","health")) {
  cat(sprintf("  ATE (%s): %.4f  (BS SE = %.4f)\n",
              a, ate_areas[a], get(paste0("ate_se_",a))))
}

cat("\nMPRTE SUMMARY — Original Scenarios:\n")
cat(sprintf("  Uniform policy:       %.4f\n",  mprte_unif))
cat(sprintf("  Low-income targeted:  %.4f\n",  mprte_lowinc))
cat(sprintf("  STEM ug pipeline:     %.4f\n",  mprte_stem))
cat(sprintf("  Education ug pipeline:%.4f\n",  mprte_ed))

cat("\nMPRTE SUMMARY — Graduate Program Area Pipelines:\n")
cat(sprintf("  STEM grad pipeline:     %.4f\n", mprte_ma_stem))
cat(sprintf("  Business grad pipeline: %.4f\n", mprte_ma_bus))
cat(sprintf("  Education pipeline:     %.4f\n", mprte_ma_ed))
cat(sprintf("  Health & Related:       %.4f\n", mprte_ma_hlth))

cat("\nBootstrap: G=50 state clusters, R=500 reps, seed(20260101)\n")
cat("Files saved: bb_mte_analysis.dta, mte_summary_by_field.csv,\n")
cat("             mte_summary_by_program_area.csv\n")
cat("\nIMPORTANT NOTE: Synthetic data — results illustrate methods only.\n")
cat(strrep("=", 50), "\n")
cat("END OF CHAPTER 10 R SCRIPT\n")
cat(strrep("=", 50), "\n")

# ── Close log ─────────────────────────────────────────────────────────────────
sink()

# ============================================================================
# END OF CHAPTER 10 R CODE
# ============================================================================
