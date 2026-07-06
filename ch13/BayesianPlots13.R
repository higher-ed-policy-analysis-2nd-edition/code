#=========================================================================
# BayesianPlots13.R
# Section 13.8: Presenting Bayesian Microsimulation and CBA Results
# R translation of BayesianPlots13.do
#
# Uses the same sim_results_ch12.dta posterior draws (S=1000) as the
# Stata original. All figures below are built with base R + ggplot2 --
# no specialized package substitution needed for this file, since the
# underlying work (summarizing existing posterior draws) is generic.
#
# NOTE -- BUG NOT CARRIED FORWARD: the Stata original's waterfall
# section (13.8.2) fails at runtime with "invalid %format" (r(120)) on
# `local dv : display %+9.0f delta[`i'']` -- Stata's numeric display
# formats do not support a "+" sign flag. That is a Stata-syntax-specific
# bug with no R analog (R's sprintf natively supports "+" via "%+.0f"),
# so it is not reproduced here; the dollar-value bar labels below are
# built correctly using sprintf.
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

# sim_results_ch12.dta lives in Chapter 12's own output folder, not
# Chapter 13's -- matches Stata's separate $ch12_tables_dir global.
if (!exists("ch12_tables_dir")) {
  if (Sys.getenv("USERNAME") == "marvi") {
    ch12_tables_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 12/Output/tables"
  } else {
    ch12_tables_dir <- data_dir  # standalone/testing fallback: same folder as everything else
  }
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
})

cat("BayesianPlots13.R running:", format(Sys.time()), "\n")

# Local-file-with-GitHub-fallback loader (Ch. 12's data repo, not Ch. 13's,
# since sim_results_ch12.dta lives in the Chapter 12 folder)
load_ch12_dta <- function(filename, data_dir) {
  local_path <- file.path(data_dir, filename)
  if (!file.exists(local_path)) {
    url <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch12/", filename)
    cat("Local file not found -- downloading from GitHub:", url, "\n")
    dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
    download.file(url, local_path, mode = "wb", quiet = TRUE)
  }
  # See DescriptiveTables13.R's load_ch13_dta() for why this local-temp
  # copy step matters: haven's ReadStat parser can hang/stall reading
  # directly from a Dropbox/OneDrive-synced path on Windows.
  tmp_path <- file.path(tempdir(), filename)
  file.copy(local_path, tmp_path, overwrite = TRUE)
  haven::read_dta(tmp_path)
}

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

sim <- load_ch12_dta("sim_results_ch12.dta", ch12_tables_dir)

#=========================================================================
# 13.8.1 Posterior Distributions and Credible Intervals
#=========================================================================
nb_mean_plot <- mean(sim$net_benefit_s)

fig13_18 <- ggplot(sim, aes(x = net_benefit_s)) +
  geom_histogram(aes(y = after_stat(count) / sum(after_stat(count)) * 100),
                 bins = 30, fill = "gray50", color = "black", linewidth = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(xintercept = nb_mean_plot, linetype = "solid", color = "black", linewidth = 0.8) +
  labs(title = "$100k Grad PLUS Lifetime Cap: Net Social Benefit",
       subtitle = "Bottom line: this policy would REDUCE net social value",
       x = "Net Social Benefit ($000s)", y = "Percent of Posterior Draws",
       caption = sprintf("95%% of simulated outcomes fall below zero (posterior mean: %.1f thousand).\nHuman capital loss from displaced students outweighs the fiscal savings.",
                          nb_mean_plot)) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

pubexport(fig13_18, "fig13_18_posterior_density")

cat("Posterior mean net benefit:", round(nb_mean_plot, 1), "(thousands)\n")

#=========================================================================
# 13.8.2 CBA Component Breakdown (Waterfall Chart)
#=========================================================================
sc_fs <- mean(sim$fiscal_s)
sc_eg <- mean(sim$effic_gain_s)
sc_bc <- mean(sim$behav_cost_s)
sc_ir <- mean(sim$inst_rev_loss_s)
sc_cs <- mean(sim$cross_sub_loss_s)

waterfall <- data.frame(
  step = 1:5,
  complabel = c("Fiscal Savings", "Efficiency Gain", "Human Capital Loss",
                "Institutional Rev. Loss", "Cross-Subsidy Disruption"),
  delta = c(sc_fs, sc_eg, -sc_bc, -sc_ir, -sc_cs)
) %>%
  mutate(
    running_end   = cumsum(delta),
    running_start = running_end - delta,
    bar_lo = pmin(running_start, running_end),
    bar_hi = pmax(running_start, running_end),
    mid    = (bar_lo + bar_hi) / 2,
    is_cost = delta < 0,
    # dollar-value bar label, correctly signed (fixes the Stata "+" format bug)
    dlabel = paste0(sprintf("%+.0f", delta), "K")
  )

nb_final <- waterfall$running_end[5]

fig13_19 <- ggplot(waterfall) +
  geom_rect(aes(xmin = step - 0.3, xmax = step + 0.3, ymin = bar_lo, ymax = bar_hi,
                fill = is_cost), color = "black") +
  geom_text(aes(x = step, y = mid, label = dlabel), size = 3.5, vjust = 0.5) +
  scale_fill_manual(values = c(`FALSE` = "gray85", `TRUE` = "gray40"),
                     labels = c(`FALSE` = "Benefit", `TRUE` = "Cost"), name = NULL) +
  scale_x_continuous(breaks = 1:5, labels = waterfall$complabel) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(title = "Cost-Benefit Decomposition: $100k Grad PLUS Cap",
       subtitle = sprintf("Bottom line: costs exceed savings by %.0f thousand per student", -nb_final),
       x = NULL, y = "Cumulative Net Benefit ($000s)",
       caption = "Light bars = benefits; dark bars = costs. Values on each bar are\nposterior-mean dollar amounts ($000s); final bar = net social benefit\n(matches fig13_18).") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        legend.position = "top")

pubexport(fig13_19, "fig13_19_cba_waterfall")

cat("Waterfall final cumulative net benefit:", round(nb_final, 1), "(thousands)\n")

#=========================================================================
# 13.8.3 Sensitivity Analysis (Tornado Diagram)
#=========================================================================
complist <- c("fiscal_s", "effic_gain_s", "behav_cost_s", "inst_rev_loss_s", "cross_sub_loss_s")
complabels <- c("Fiscal Savings", "Efficiency Gain", "Human Capital Loss",
                 "Institutional Rev. Loss", "Cross-Subsidy Disruption")

tornado <- data.frame(
  complabel = complabels,
  mean = sapply(complist, function(v) mean(sim[[v]])),
  lo   = sapply(complist, function(v) quantile(sim[[v]], 0.025)),
  hi   = sapply(complist, function(v) quantile(sim[[v]], 0.975))
) %>%
  mutate(sens = hi - lo) %>%
  arrange(desc(sens)) %>%
  mutate(complabel = factor(complabel, levels = rev(complabel)))

most_uncertain <- as.character(tornado$complabel[nrow(tornado)])
# (after releveling for plotting, so re-derive from the pre-sort order)
most_uncertain <- tornado %>% arrange(desc(sens)) %>% slice(1) %>% pull(complabel) %>% as.character()

fig13_20 <- ggplot(tornado, aes(y = complabel, x = mean)) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.3, color = "black") +
  labs(title = "Posterior Uncertainty by CBA Component",
       subtitle = paste0("Most uncertain: ", most_uncertain),
       x = "95% Credible Interval ($000s)", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

pubexport(fig13_20, "fig13_20_tornado")

#=========================================================================
# 13.8.4 Presenting a Single Policy Number
#=========================================================================
nb_mean <- mean(sim$net_benefit_s)
nb_lo   <- quantile(sim$net_benefit_s, 0.025)
nb_hi   <- quantile(sim$net_benefit_s, 0.975)

single_num <- data.frame(j = 1, b = nb_mean, lo = nb_lo, hi = nb_hi)

fig13_21 <- ggplot(single_num, aes(x = b, y = factor(j))) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.1, color = "black", linewidth = 0.8) +
  geom_point(shape = 18, size = 5, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  scale_y_discrete(labels = "Net Social Benefit") +
  labs(title = "$100k Grad PLUS Cap: Net Social Benefit",
       subtitle = "Recommendation: does not pay for itself",
       x = "$000s", y = NULL,
       caption = sprintf("Point estimate = %.0f thousand per student (95%% credible interval entirely below zero).\nFull distribution in fig13_18; see Section 13.9 for audience tailoring.", nb_mean)) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

pubexport(fig13_21, "fig13_21_single_policy_number")

cat("\nBayesianPlots13.R completed:", format(Sys.time()), "\n")
