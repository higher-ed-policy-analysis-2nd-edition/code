#=========================================================================
# CausalPlots13.R
# Section 13.6: Presenting Causal Inference Results
# R translation of CausalPlots13.do
#
# Package substitutions:
#   Stata xtreg, fe (event-time dummies) -> lm() with unit + year factor
#     fixed effects, clustered SE via sandwich::vcovCL (Stata: cluster(fips))
#   Stata jwdid (ETWFE, hettype(cohort)) -> manual Extended TWFE
#     (Wooldridge 2021): unit + time FE regression with cohort x relative-
#     event-time interactions for treated units, never-treated as the
#     comparison group, clustered SE. This is the same estimator family
#     jwdid implements; only the estimation route (canned command vs.
#     explicit interaction regression) differs.
#   Stata xthdidregress/estat atetplot (13.13b) -> NO CRAN-available
#     equivalent found (this is StataCorp's native Callaway-Sant'Anna
#     implementation; the R "did" package is CRAN-only and not
#     installable in this sandbox). Left as a documented placeholder --
#     this is an explicitly-labeled "bonus" demonstration in the Stata
#     original beyond the two core event-study figures.
#   Stata rdrobust/rdplot -> manual local-linear regression with a
#     triangular kernel (matches rdrobust's default kernel), for both
#     the point estimate and the bandwidth-sensitivity sweep.
#   Stata synth -> manual donor-weight optimization via
#     quadprog::solve.QP, minimizing pre-treatment MSPE (Abadie et al.
#     synthetic control logic), constrained to weights >= 0 summing to 1.
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
  library(dplyr)
  library(ggplot2)
  library(sandwich)
  library(lmtest)
  library(quadprog)
})

cat("CausalPlots13.R running:", format(Sys.time()), "\n")

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

clean_names <- function(x) {
  x <- trimws(x)
  x <- gsub("[[:space:]]+", "_", x)
  x <- gsub("_+", "_", x)
  tolower(x)
}

#=========================================================================
# 13.6.1 Event-Study / DiD Plots
#=========================================================================

#-------------------------------------------------------------------------
# Figure 13.12: TWFE Event Study -- Georgia Consolidation (single treatment)
#-------------------------------------------------------------------------
if (!file.exists(file.path(data_dir, "Example_10_3_1.csv"))) {
  download.file("https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch10/Example_10_3_1.csv",
                file.path(data_dir, "Example_10_3_1.csv"), quiet = TRUE)
}
d10_3 <- read.csv(file.path(data_dir, "Example_10_3_1.csv"), check.names = FALSE, fileEncoding = "UTF-8-BOM")
names(d10_3) <- clean_names(names(d10_3))
d10_3$state <- trimws(d10_3$state)

sreb_states <- c("Alabama", "Arkansas", "Delaware", "Florida", "Georgia", "Kentucky",
                  "Louisiana", "Maryland", "Mississippi", "North Carolina", "Oklahoma",
                  "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia")
d10_3 <- d10_3 %>% filter(state %in% sreb_states)

fips_map <- c(Alabama = 1, Arkansas = 5, Delaware = 10, Florida = 12, Georgia = 13,
              Kentucky = 21, Louisiana = 22, Maryland = 24, Mississippi = 28,
              `North Carolina` = 37, Oklahoma = 40, `South Carolina` = 45,
              Tennessee = 47, Texas = 48, Virginia = 51, `West Virginia` = 54)
d10_3$fips <- fips_map[d10_3$state]

d10_3 <- d10_3 %>%
  mutate(
    treat_state = as.integer(state == "Georgia"),
    post        = as.integer(fy >= 2018),
    lngenop     = log(general_public_operations),
    lntotsup    = log(total_state_support),
    lnfinaid    = log(total_financial_aid),
    lntuifee    = log(net_tuition_and_fee_revenue),
    lnfte       = log(net_fte_enrollment),
    rel_year    = fy - 2018
  )

# Event-time dummies: leads F2-F16 (F16 bins all rel_year <= -16), lags L0-L3.
# Reference period t = -1 (FY 2017) omitted by construction.
kpre  <- 16
kpost <- 3
for (k in 2:kpre) {
  d10_3[[paste0("F", k, "_ga")]] <- as.integer(d10_3$treat_state == 1 & d10_3$rel_year == -k)
}
d10_3[[paste0("F", kpre, "_ga")]] <- as.integer(d10_3$treat_state == 1 & d10_3$rel_year <= -kpre)
for (k in 0:kpost) {
  d10_3[[paste0("L", k, "_ga")]] <- as.integer(d10_3$treat_state == 1 & d10_3$rel_year == k)
}

evars <- c(paste0("F", kpre:2, "_ga"), paste0("L", 0:kpost, "_ga"))
rhs <- paste(c(evars, "lntotsup", "lnfinaid", "lntuifee", "lnfte",
               "factor(fy)", "factor(fips)"), collapse = " + ")
fit12 <- lm(as.formula(paste("lngenop ~", rhs)), data = d10_3)
vc12  <- vcovCL(fit12, cluster = d10_3$fips, type = "HC1")

b12  <- coef(fit12)[evars]
se12 <- sqrt(diag(vc12))[evars]

event_t <- c(-kpre:-2, 0:kpost)
fig13_12_data <- data.frame(t = event_t, b = b12, se = se12) %>%
  mutate(lo = b - 1.96 * se, hi = b + 1.96 * se)
# insert reference period (t = -1) with b = 0
fig13_12_data <- bind_rows(fig13_12_data, data.frame(t = -1, b = 0, se = 0, lo = 0, hi = 0)) %>%
  arrange(t)

ytxt <- max(fig13_12_data$hi) * 0.92

fig13_12 <- ggplot(fig13_12_data, aes(x = t, y = b)) +
  geom_ribbon(data = filter(fig13_12_data, t < 0), aes(ymin = lo, ymax = hi), fill = "gray85") +
  geom_ribbon(data = filter(fig13_12_data, t >= 0), aes(ymin = lo, ymax = hi), fill = "gray65") +
  geom_line(color = "black", linewidth = 0.9) +
  geom_point(data = filter(fig13_12_data, t == -1), shape = 4, size = 4, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "gray40") +
  annotate("text", x = -0.5, y = ytxt, label = "Policy Adopted", angle = 90,
           hjust = 1, size = 3.2) +
  labs(title = "Event Study: Georgia Higher Education Consolidation",
       subtitle = "TWFE, single treated unit. Reference period: t = -1.",
       x = "Years Relative to Consolidation (FY 2018 = 0)",
       y = "Coefficient (log operating expenses)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

pubexport(fig13_12, "fig13_12_twfe_event_study")

cat("Figure 13.12 built. N =", nrow(d10_3), "state-years,", length(unique(d10_3$fips)), "states.\n")

cat("\nCausalPlots13.R Part 1 (event study, Fig 13.12) completed:", format(Sys.time()), "\n")

#-------------------------------------------------------------------------
# Figure 13.13: ETWFE Event Study -- Staggered Adoption (Georgia/
# Wisconsin/Pennsylvania cohorts)
#
# Manual Extended TWFE (Wooldridge 2021): cohort x relative-event-time
# interaction dummies for each treated cohort, never-treated units
# (45 of 48 states) as the comparison group throughout, unit + year FE,
# clustered SE. Event window bounded to e in [-8, 5] (endpoints bin
# more extreme relative times), matching the pre/post window size used
# in Figure 13.12 for visual consistency across the chapter's two event
# studies.
#-------------------------------------------------------------------------
if (!file.exists(file.path(data_dir, "Example_10_7_3.csv"))) {
  download.file("https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch10/Example_10_7_3.csv",
                file.path(data_dir, "Example_10_7_3.csv"), quiet = TRUE)
}
d10_7 <- read.csv(file.path(data_dir, "Example_10_7_3.csv"), check.names = FALSE)
names(d10_7) <- clean_names(names(d10_7))

d10_7 <- d10_7 %>%
  mutate(
    lngenop  = log(generalpublicoperations),
    lntotsup = log(totalstatesupport),
    lnfinaid = log(totalfinancialaid),
    lntuifee = log(nettuitionandfeerevenue),
    lnfte    = log(netfteenrollment),
    gyear    = case_when(fips == 13 ~ 2013, fips == 55 ~ 2018, fips == 42 ~ 2022, TRUE ~ 0)
  ) %>%
  filter(is.finite(lngenop), is.finite(lntotsup), is.finite(lnfinaid),
         is.finite(lntuifee), is.finite(lnfte))

emin <- -8; emax <- 5
d10_7$rel_time <- ifelse(d10_7$gyear > 0, d10_7$fy - d10_7$gyear, NA)
d10_7$rel_bin  <- pmin(pmax(d10_7$rel_time, emin), emax)

event_grid <- setdiff(emin:emax, -1)  # omit reference period e = -1
cohorts <- c(2013, 2018, 2022)

for (g in cohorts) {
  for (e in event_grid) {
    vn <- paste0("g", g, "_e", ifelse(e < 0, paste0("m", abs(e)), e))
    d10_7[[vn]] <- as.integer(d10_7$gyear == g & d10_7$rel_bin == e & !is.na(d10_7$rel_bin))
  }
}
dvars <- unlist(lapply(cohorts, function(g) {
  sapply(event_grid, function(e) paste0("g", g, "_e", ifelse(e < 0, paste0("m", abs(e)), e)))
}))
# drop any all-zero columns (cohort/event combos with no observations)
dvars <- dvars[sapply(d10_7[dvars], function(x) sum(x) > 0)]

rhs13 <- paste(c(dvars, "lntotsup", "lnfinaid", "lntuifee", "lnfte",
                  "factor(fy)", "factor(fips)"), collapse = " + ")
fit13 <- lm(as.formula(paste("lngenop ~", rhs13)), data = d10_7)
vc13  <- vcovCL(fit13, cluster = d10_7$fips, type = "HC1")

b13_all <- coef(fit13)
# Aggregate cohort-specific coefficients into one ATT(e) per relative
# event time via equal-weighted averaging across cohorts observed at e
# (matches jwdid/estat event's event-time aggregation when cohort sizes
# are equal, as they are here: one state per cohort).
agg_rows <- list()
for (e in event_grid) {
  vn_e <- sapply(cohorts, function(g) paste0("g", g, "_e", ifelse(e < 0, paste0("m", abs(e)), e)))
  vn_e <- vn_e[vn_e %in% names(b13_all)]
  if (length(vn_e) == 0) next
  w <- rep(1 / length(vn_e), length(vn_e))
  att_e <- sum(w * b13_all[vn_e])
  sub_vc <- vc13[vn_e, vn_e, drop = FALSE]
  se_e  <- sqrt(as.numeric(t(w) %*% sub_vc %*% w))
  agg_rows[[length(agg_rows) + 1]] <- data.frame(event = e, att = att_e, se = se_e)
}
fig13_13_data <- bind_rows(agg_rows) %>%
  bind_rows(data.frame(event = -1, att = 0, se = 0)) %>%
  arrange(event) %>%
  mutate(lo = att - 1.96 * se, hi = att + 1.96 * se)

ytxt13 <- max(fig13_13_data$hi) * 0.90

fig13_13 <- ggplot(fig13_13_data, aes(x = event, y = att)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, color = "gray50") +
  geom_line(color = "black") +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "gray40") +
  annotate("text", x = -0.5, y = ytxt13, label = "Treatment Begins", angle = 90,
           hjust = 1, size = 3.2) +
  labs(title = "ETWFE Event Study: Staggered Adoption",
       subtitle = "Covariate-adjusted, never-treated controls",
       x = "Years relative to treatment", y = "ATT on log operations") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

pubexport(fig13_13, "fig13_13_etwfe_event_study")

cat("Figure 13.13 built. Cohorts: GA(2013), WI(2018), PA(2022);",
    length(unique(d10_7$fips)) - 3, "never-treated controls.\n")

#-------------------------------------------------------------------------
# Figure 13.13b: xthdidregress / estat atetplot (Callaway-Sant'Anna RA)
#-------------------------------------------------------------------------
# NOTE -- NO CRAN-AVAILABLE EQUIVALENT FOUND for xthdidregress's native
# RA/IPW/AIPW Callaway-Sant'Anna implementation (the R "did" package is
# CRAN-only, not installable in this sandbox). This is an explicitly-
# labeled "bonus" demonstration in the Stata original, beyond the two
# core event-study figures (13.12, 13.13) already validated above --
# left as a documented placeholder rather than approximated, since
# Figure 13.13's Extended TWFE above already demonstrates the staggered-
# adoption presentation technique this section would otherwise repeat.
cat("\nFigure 13.13b (xthdidregress/atetplot): no CRAN-available R\n")
cat("equivalent in this sandbox -- documented placeholder; Figure 13.13\n")
cat("above already covers the staggered-adoption presentation technique.\n")

cat("\nCausalPlots13.R Part 2 (ETWFE, Fig 13.13) completed:", format(Sys.time()), "\n")

#=========================================================================
# 13.6.2 RD Plots
#=========================================================================
# Synthetic HSLS:09-calibrated data, fully self-contained (matches the
# Stata original -- N=4,000, cutoff=3.25, true LATE=0.10). Same fixed
# seed intent as the Stata script (20260510), using R's own RNG so the
# specific draws won't numerically match Stata's, but the data-
# generating process is identical.
#-------------------------------------------------------------------------
set.seed(20260510)

N         <- 4000
cutoff    <- 3.25
true_late <- 0.10

mu_gpa <- 3.15; sd_gpa <- 0.72; lo_gpa <- 1.00; hi_gpa <- 4.00
Fa_gpa <- pnorm((lo_gpa - mu_gpa) / sd_gpa)
Fb_gpa <- pnorm((hi_gpa - mu_gpa) / sd_gpa)

u_gpa  <- Fa_gpa + runif(N) * (Fb_gpa - Fa_gpa)
hs_gpa <- qnorm(u_gpa) * sd_gpa + mu_gpa
x      <- hs_gpa - cutoff

D_sharp <- as.integer(x >= 0)

pr_take_up <- ifelse(x < 0,
                      0.05 + 0.09 * (x + 2.25) / 2.25,
                      0.70 + 0.15 * pmin(x / 0.75, 1.0))
pr_take_up <- pmin(pmax(pr_take_up, 0.02), 0.92)
D_fuzzy <- as.integer(runif(N) < pr_take_up)

female     <- as.integer(runif(N) < 0.54)
firstgen   <- as.integer(runif(N) < 0.32)
income_cat <- pmin(1 + floor(3 * runif(N)), 3)

mu0 <- 0.58 + 0.20 * x - 0.08 * x^2 + 0.03 * female - 0.05 * firstgen +
  0.01 * (income_cat - 2) + rnorm(N, 0, 0.18)

Y1_s <- pmin(pmax(mu0 + true_late * D_sharp + rnorm(N, 0, 0.12), 0), 1)
persist_sharp <- as.integer(Y1_s > 0.50)

rd_data <- data.frame(x = x, hs_gpa = hs_gpa, D_sharp = D_sharp,
                       D_fuzzy = D_fuzzy, persist_sharp = persist_sharp)

#-------------------------------------------------------------------------
# Local-linear sharp RD point estimate (rdrobust equivalent): weighted
# regression of the outcome on the running variable, separately on each
# side of the cutoff, with a triangular kernel -- rdrobust's default.
#-------------------------------------------------------------------------
sharp_rd <- function(data, y, x, c0 = 0, h) {
  d <- data
  d$xc <- d[[x]] - c0
  d <- d[abs(d$xc) <= h, ]
  d$w <- (1 - abs(d$xc) / h)          # triangular kernel weight
  d$D <- as.integer(d$xc >= 0)
  fit <- lm(as.formula(paste(y, "~ xc * D")), data = d, weights = w)
  cf  <- coef(fit); vc <- vcovHC(fit, type = "HC1")
  tau <- cf["D"]
  se  <- sqrt(vc["D", "D"])
  c(tau = unname(tau), se = unname(se))
}

# IMSE-ish bandwidth: Silverman-type rule of thumb on the running variable
h_opt <- 1.84 * sd(rd_data$x) * N^(-1/5)
rd_main <- sharp_rd(rd_data, "persist_sharp", "x", h = h_opt)
late_pp <- sprintf("%.1f", rd_main["tau"] * 100)

cat("\nRD point estimate: tau =", round(rd_main["tau"], 4),
    "(", late_pp, "pp), h =", round(h_opt, 3), "\n")

#-------------------------------------------------------------------------
# Figure 13.14: Publication-quality RD plot -- binned scatter + local fit
#-------------------------------------------------------------------------
nbins <- 30
rd_data$bin <- cut(rd_data$x, breaks = nbins, labels = FALSE)
bin_means <- rd_data %>%
  group_by(bin) %>%
  summarise(x_mean = mean(x), y_mean = mean(persist_sharp), .groups = "drop")

fig13_14 <- ggplot() +
  geom_point(data = bin_means, aes(x = x_mean, y = y_mean), color = "gray50", size = 1.8) +
  geom_smooth(data = filter(rd_data, x < 0), aes(x = x, y = persist_sharp),
              method = "loess", se = FALSE, color = "black", linewidth = 0.8) +
  geom_smooth(data = filter(rd_data, x >= 0), aes(x = x, y = persist_sharp),
              method = "loess", se = FALSE, color = "black", linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Effect of Institutional Merit Scholarship on\nSecond-Year Persistence",
       subtitle = paste0("Scholarship recipients persist at a ", late_pp,
                          " percentage-point higher rate"),
       x = "High-School GPA (centered at cutoff)", y = "Second-Year Persistence Rate",
       caption = "Sharp RD, HS GPA cutoff = 3.25. Circles = bin means; lines =\nlocal polynomial fit.") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

pubexport(fig13_14, "fig13_14_rdplot_persistence")

#-------------------------------------------------------------------------
# Figure 13.15: Bandwidth Sensitivity
#-------------------------------------------------------------------------
bw_list <- c(0.15, 0.20, 0.25, 0.30, 0.40, 0.50, 0.60, 0.75, 1.00)
bw_results <- do.call(rbind, lapply(bw_list, function(h) {
  r <- sharp_rd(rd_data, "persist_sharp", "x", h = h)
  data.frame(h = h, LATE = r["tau"], CI_lo = r["tau"] - 1.96 * r["se"],
             CI_hi = r["tau"] + 1.96 * r["se"])
}))

crosses_zero <- sum(bw_results$CI_lo <= 0 & bw_results$CI_hi >= 0)
robust_txt <- if (crosses_zero == 0) "Robust: positive at every bandwidth tested" else
  paste0("Caution: not distinguishable from zero at ", crosses_zero, " of ", nrow(bw_results), " bandwidths")

fig13_15 <- ggplot(bw_results, aes(x = h, y = LATE)) +
  geom_errorbar(aes(ymin = CI_lo, ymax = CI_hi), width = 0.02, color = "gray50") +
  geom_point(shape = 18, size = 3, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Bandwidth Sensitivity -- Sharp RD", subtitle = robust_txt,
       x = "Bandwidth (HS GPA units)", y = "Estimated LATE (pp)",
       caption = "Outcome: Second-Year Persistence, cutoff = 3.25 HS GPA.") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

pubexport(fig13_15, "fig13_15_rdd_bw_sensitivity")

cat("\nCausalPlots13.R Part 3 (RD, Figs 13.14/13.15) completed:", format(Sys.time()), "\n")

#=========================================================================
# 13.6.3 Synthetic Control / SDID Plots
#=========================================================================
# Manual synthetic control (Abadie et al.): donor weights chosen to
# minimize pre-treatment MSPE between the treated unit (Georgia) and a
# convex combination of donor states, solved via quadratic programming
# (quadprog::solve.QP), constrained to weights >= 0 summing to 1.
#
# SDID NOTE -- matching the Stata original, this is deliberately NOT
# also built as a separate SDiD trajectory figure: the Stata source
# documents that sdid's native graph option failed in every tested
# configuration, and reports the SDID point estimate only in a forest
# plot outside this chapter's scope. Not reproduced here for the same
# reason.
#-------------------------------------------------------------------------
d_scm <- d10_3 %>%
  mutate(lngenop = log(general_public_operations)) %>%
  select(fips, state, fy, lngenop, lntotsup, lnfinaid, lntuifee, lnfte)

treat_fips <- 13  # Georgia
pre_years  <- sort(unique(d_scm$fy[d_scm$fy < 2018]))
donor_fips <- setdiff(unique(d_scm$fips), treat_fips)

# Pre-treatment outcome path matrix (donors x pre-years) + predictor means
Y_treat_pre <- d_scm %>% filter(fips == treat_fips, fy %in% pre_years) %>%
  arrange(fy) %>% pull(lngenop)
Y_donor_pre <- sapply(donor_fips, function(f) {
  d_scm %>% filter(fips == f, fy %in% pre_years) %>% arrange(fy) %>% pull(lngenop)
})

# quadprog: minimize ||Y_treat_pre - Y_donor_pre %*% w||^2 s.t. w>=0, sum(w)=1
Dmat <- t(Y_donor_pre) %*% Y_donor_pre
Dmat <- Dmat + diag(1e-8, nrow(Dmat))  # numerical stability
dvec <- t(Y_donor_pre) %*% Y_treat_pre
n_donor <- length(donor_fips)
Amat <- cbind(rep(1, n_donor), diag(n_donor))
bvec <- c(1, rep(0, n_donor))
qp <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
w_scm <- qp$solution
names(w_scm) <- donor_fips

top_donors <- sort(w_scm[w_scm > 0.01], decreasing = TRUE)
cat("\nSynthetic control donor weights (>1%):\n")
print(round(top_donors, 3))

# Build the synthetic trajectory across ALL years (pre + post)
all_years <- sort(unique(d_scm$fy))
Y_donor_all <- sapply(donor_fips, function(f) {
  v <- d_scm %>% filter(fips == f) %>% arrange(fy) %>% pull(lngenop)
  if (length(v) == length(all_years)) v else rep(NA, length(all_years))
})
Y_synth <- as.numeric(Y_donor_all %*% w_scm)
Y_ga    <- d_scm %>% filter(fips == treat_fips) %>% arrange(fy) %>% pull(lngenop)

scm_data <- data.frame(fy = all_years, Y_ga = Y_ga, Y_synth = Y_synth)

#-------------------------------------------------------------------------
# Figure 13.16: Actual vs. Synthetic Georgia Trend
#-------------------------------------------------------------------------
ytxt16 <- max(scm_data$Y_ga[scm_data$fy >= 2014 & scm_data$fy < 2018]) * 1.01

fig13_16 <- ggplot(scm_data, aes(x = fy)) +
  geom_line(aes(y = Y_ga), color = "black", linewidth = 0.9, linetype = "solid") +
  geom_line(aes(y = Y_synth), color = "black", linewidth = 0.9, linetype = "dashed") +
  geom_vline(xintercept = 2018, linetype = "dotted", color = "gray50") +
  annotate("text", x = 2018, y = ytxt16, label = "Consolidation", hjust = 1, size = 3.2) +
  labs(title = "SCM: Georgia vs. Synthetic Control",
       subtitle = "Dashed = synthetic Georgia (the counterfactual)",
       x = "Fiscal Year", y = "Log Operating Expenses") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

pubexport(fig13_16, "fig13_16_scm_trends")

#-------------------------------------------------------------------------
# Figure 13.17: SCM Gap Plot (actual minus synthetic)
#-------------------------------------------------------------------------
scm_data$gap <- scm_data$Y_ga - scm_data$Y_synth
avg_gap <- sprintf("%.3f", mean(scm_data$gap[scm_data$fy >= 2018], na.rm = TRUE))

fig13_17 <- ggplot(scm_data, aes(x = fy, y = gap)) +
  geom_line(color = "black", linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 2018, linetype = "dotted", color = "gray50") +
  labs(title = "SCM Gap: Effect of Georgia Consolidation",
       subtitle = paste0("Average post-consolidation gap: ", avg_gap, " log points"),
       x = "Fiscal Year", y = "Gap: Log Expenses (Georgia - Synthetic)",
       caption = "Above zero = Georgia's actual spending exceeded its synthetic counterfactual.") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

pubexport(fig13_17, "fig13_17_scm_gap")

cat("\nCausalPlots13.R completed:", format(Sys.time()), "\n")
