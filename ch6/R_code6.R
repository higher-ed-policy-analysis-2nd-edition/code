# ================================================================
# Chapter 6 - Using Descriptive Statistics and Graphs
# R Translation of Complete Stata Code
# Higher Education Policy Analysis Using Quantitative Techniques
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-
#         edition/tree/main/code/ch6
# Author: Marvin A. Titus
# Date: November 14, 2025
# ================================================================

# Script tested in R 4.4.x
# Required packages: readxl, haven, dplyr, tidyr, psych,
#                    plm, ggplot2, scales, patchwork

# ----------------------------------------------------------------
# Install any missing packages (run once)
# ----------------------------------------------------------------
required_pkgs <- c("readxl", "haven", "dplyr", "tidyr", "psych",
                   "plm", "ggplot2", "scales", "patchwork")
new_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs)

library(readxl)    # read_excel()        — replaces: import excel
library(haven)     # read_dta()          — replaces: use *.dta
library(dplyr)     # data manipulation   — replaces: gen, replace, tabstat
library(tidyr)     # pivot_longer/wider  — used for summary tables
library(psych)     # describe(), geometric.mean(), harmonic.mean()
library(plm)       # pdata.frame()       — replaces: xtset / xtdescribe
library(ggplot2)   # all graphs          — replaces: histogram, graph box, twoway
library(scales)    # percent_format()    — axis formatting
library(patchwork) # plot composition    — side-by-side panels

# ================================================================
# WORKING DIRECTORY AND OUTPUT PATHS
# Paths switch automatically by username, mirroring the Stata logic.
# ================================================================

graphs_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 6/Output/graphs"
log_path   <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 6/Output/logs/Chapter6_R_output.log"
dir.create(graphs_dir,        showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)

# Open log — sink() captures all console output to a text file
# Equivalent to: log using "...", replace text
sink(log_path, split = TRUE)   # split=TRUE also prints to console
cat("Chapter 6 log opened:", format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n")
cat("Graphs directory:", graphs_dir, "\n\n")

# Helper: save ggplot to graphs_dir at 1200px wide (matches Stata width(1200))
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

# ================================================================
# Section 6.2.1: Measures of Central Tendency
# ================================================================
cat("\n")
cat("*========================================================================\n")
cat("* Section 6.2.1: Measures of Central Tendency\n")
cat("*========================================================================\n\n")

# Method 1: Import from local file (if previously downloaded)
# df_621 <- read_excel("tabn302_50.xlsx", sheet = "reformatted")

# Method 2: Download from GitHub and import
# Equivalent to: copy "..." + import excel
url_621 <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                  "2nd-edition/data/main/ch4/tabn302_50.xlsx")
download.file(url_621, "tabn302_50.xlsx", mode = "wb", quiet = TRUE)
df_621 <- read_excel("tabn302_50.xlsx", sheet = "reformatted")

# --- Arithmetic, geometric, and harmonic means ---
# Equivalent to: ameans Public Private
# Note: psych::describe() gives arithmetic mean; geometric.mean() and
#       harmonic.mean() from psych give the other two.
cat(". ameans Public Private\n\n")
for (var in c("Public", "Private")) {
  x   <- df_621[[var]]
  cat(sprintf("  Variable: %-10s\n", var))
  cat(sprintf("    Arithmetic mean: %12.4f\n", mean(x, na.rm = TRUE)))
  cat(sprintf("    Geometric  mean: %12.4f\n", psych::geometric.mean(x)))
  cat(sprintf("    Harmonic   mean: %12.4f\n", psych::harmonic.mean(x)))
  cat("\n")
}

# --- Arithmetic mean with 95% CI ---
# Equivalent to: mean Public Private
cat(". mean Public Private\n\n")
for (var in c("Public", "Private")) {
  x   <- df_621[[var]]
  n   <- sum(!is.na(x))
  m   <- mean(x, na.rm = TRUE)
  se  <- sd(x, na.rm = TRUE) / sqrt(n)
  ci  <- m + qt(c(0.025, 0.975), df = n - 1) * se
  cat(sprintf("  %-10s  Mean: %10.4f  Std.Err.: %10.4f  95%% CI: [%10.4f, %10.4f]\n",
              var, m, se, ci[1], ci[2]))
}
cat("\n")

# --- Detailed summary statistics including median ---
# Equivalent to: sum, detail
cat(". summarize, detail\n\n")
print(psych::describe(df_621[, c("Public", "Private")],
                      quant = c(.01, .05, .10, .25, .50, .75, .90, .95, .99)))
cat("\n")

# ================================================================
# Section 6.2.2: Measures of Dispersion
# ================================================================
cat("*========================================================================\n")
cat("* Section 6.2.2: Measures of Dispersion\n")
cat("*========================================================================\n\n")

# Download SHEEO finance dataset
# Equivalent to: copy "..." + use "Example_6_2_2.dta"
url_622 <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                  "2nd-edition/data/main/ch6/Example_6_2_2.dta")
download.file(url_622, "Example_6_2_2.dta", mode = "wb", quiet = TRUE)
df_622 <- read_dta("Example_6_2_2.dta")

# --- Coefficient of variation ---
# Equivalent to: tabstat NetTuition FTEStudents, stat(cv)
cat(". tabstat NetTuition FTEStudents, stat(cv)\n\n")
cv_tbl <- data.frame(
  Variable = c("NetTuition", "FTEStudents"),
  CV       = c(sd(df_622$NetTuition,  na.rm = TRUE) / mean(df_622$NetTuition,  na.rm = TRUE),
               sd(df_622$FTEStudents, na.rm = TRUE) / mean(df_622$FTEStudents, na.rm = TRUE))
)
print(cv_tbl, row.names = FALSE)
cat("\n")

# Helper: descriptive stats table (mean, median, sd, min, max, cv) by group
# Equivalent to: tabstat ..., stat(mean median sd min max cv) by() ...
tabstat_by <- function(data, vars, group_var) {
  data |>
    group_by(across(all_of(group_var))) |>
    summarise(across(all_of(vars), list(
      mean   = \(x) mean(x,   na.rm = TRUE),
      median = \(x) median(x, na.rm = TRUE),
      sd     = \(x) sd(x,     na.rm = TRUE),
      min    = \(x) min(x,    na.rm = TRUE),
      max    = \(x) max(x,    na.rm = TRUE),
      cv     = \(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
    ), .names = "{.col}_{.fn}"), .groups = "drop") |>
    pivot_longer(-all_of(group_var),
                 names_to  = c("variable", "stat"),
                 names_sep = "_(?=[^_]+$)") |>
    pivot_wider(names_from = "stat", values_from = "value")
}

# --- Descriptive statistics by state (Fig. 6.1) ---
# Equivalent to: tabstat NetTuition FTEStudents, ... by(State)
cat(". tabstat NetTuition FTEStudents, stat(mean median sd min max cv) by(State)\n\n")
tbl_state <- tabstat_by(df_622, c("NetTuition", "FTEStudents"), "State")
print(as.data.frame(tbl_state), digits = 4, row.names = FALSE)
cat("\n")

# --- Descriptive statistics by fiscal year (Fig. 6.2) ---
# Equivalent to: tabstat NetTuition FTEStudents, ... by(FY)
cat(". tabstat NetTuition FTEStudents, stat(mean median sd min max cv) by(FY)\n\n")
tbl_fy <- tabstat_by(df_622, c("NetTuition", "FTEStudents"), "FY")
print(as.data.frame(tbl_fy), digits = 4, row.names = FALSE)
cat("\n")

# ================================================================
# Section 6.2.3: Distributions
# ================================================================
cat("*========================================================================\n")
cat("* Section 6.2.3: Distributions\n")
cat("*========================================================================\n\n")

# Download HSLS:09 condensed dataset with earnings variable
# Equivalent to: copy "..." + use "Example_6_2_3.dta"
url_623 <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                  "2nd-edition/data/main/ch6/Example_6_2_3.dta")
download.file(url_623, "Example_6_2_3.dta", mode = "wb", quiet = TRUE)
df_623 <- read_dta("Example_6_2_3.dta")

# --- Examine race/ethnicity variable ---
# Equivalent to: codebook X1RACE
cat(". codebook X1RACE\n\n")
cat("  Type:", class(df_623$X1RACE), "\n")
cat("  Range:", range(df_623$X1RACE, na.rm = TRUE), "\n")
cat("  Unique values:", length(unique(df_623$X1RACE)), "\n")
cat("  Missing:", sum(is.na(df_623$X1RACE)), "/", nrow(df_623), "\n")
cat("  Value labels:\n")
if (!is.null(attr(df_623$X1RACE, "labels"))) {
  lbl <- attr(df_623$X1RACE, "labels")
  for (i in seq_along(lbl)) cat(sprintf("    %d  %s\n", lbl[i], names(lbl)[i]))
}
cat("\n")

# --- Recode race/ethnicity ---
# Equivalent to: gen RaceEthnic + replace ... + label define + label values
df_623 <- df_623 |>
  mutate(
    RaceEthnic = case_when(
      X1RACE == 2            ~ 1L,   # Asian
      X1RACE == 3            ~ 2L,   # Black
      X1RACE %in% c(4, 5)   ~ 3L,   # Hispanic
      X1RACE == 6            ~ 4L,   # Multiracial
      X1RACE %in% c(1, 7)   ~ 5L,   # Other
      X1RACE == 8            ~ 6L,   # White
      TRUE                   ~ 0L
    ),
    RaceEthnic = factor(RaceEthnic,
                        levels = 1:6,
                        labels = c("Asian", "Black", "Hispanic",
                                   "Multiracial", "Other", "White"))
  )

# --- Frequency distribution using original variable (Fig. 6.3) ---
# Equivalent to: prop X1RACE
cat(". prop X1RACE\n\n")
prop_tbl <- df_623 |>
  count(X1RACE) |>
  mutate(proportion = n / sum(n),
         pct        = proportion * 100) |>
  arrange(X1RACE)
print(as.data.frame(prop_tbl), row.names = FALSE)
cat("\n")

# --- Tabulate with frequencies and percentages, sorted (Fig. 6.4) ---
# Equivalent to: tab X1RACE, sort
cat(". tab X1RACE, sort\n\n")
tab_race <- df_623 |>
  count(X1RACE) |>
  mutate(percent    = n / sum(n) * 100,
         cumulative = cumsum(percent)) |>
  arrange(desc(n))
print(as.data.frame(tab_race), digits = 2, row.names = FALSE)
cat("\n")

# --- One-way table with mean earnings by race (Fig. 6.5) ---
# Equivalent to: tab X1RACE, summarize(EarnHr)
cat(". tab X1RACE, summarize(EarnHr)\n\n")
tab_earn <- df_623 |>
  group_by(X1RACE) |>
  summarise(mean   = mean(EarnHr,   na.rm = TRUE),
            sd     = sd(EarnHr,     na.rm = TRUE),
            n      = n(),
            .groups = "drop")
print(as.data.frame(tab_earn), digits = 4, row.names = FALSE)
cat("\n")

# --- Two-way table of mean earnings by race and sex (Fig. 6.6) ---
# Equivalent to: tab X1RACE X1SEX, sum(EarnHr) means
cat(". tab X1RACE X1SEX, sum(EarnHr) means\n\n")
tab_2way <- df_623 |>
  group_by(X1RACE, X1SEX) |>
  summarise(mean_EarnHr = mean(EarnHr, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = X1SEX, values_from = mean_EarnHr,
              names_prefix = "Sex_")
print(as.data.frame(tab_2way), digits = 4, row.names = FALSE)
cat("\n")

# --- Two-way table using recoded variable (Fig. 6.7) ---
# Equivalent to: tabulate RaceEthnic X1SEX, sum(EarnHr) means
cat(". tabulate RaceEthnic X1SEX, sum(EarnHr) means\n\n")
tab_2way_re <- df_623 |>
  group_by(RaceEthnic, X1SEX) |>
  summarise(mean_EarnHr = mean(EarnHr, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = X1SEX, values_from = mean_EarnHr,
              names_prefix = "Sex_")
print(as.data.frame(tab_2way_re), digits = 4, row.names = FALSE)
cat("\n")

# ----------------------------------------------------------------
# Panel data — state-level dataset
# Equivalent to: use "Example_6_3.dta" + xtset fips year, yearly
# ----------------------------------------------------------------

# Download state-level panel dataset
url_63 <- paste0("https://raw.githubusercontent.com/higher-ed-policy-analysis-",
                 "2nd-edition/data/main/ch6/Example_6_3.dta")
download.file(url_63, "Example_6_3.dta", mode = "wb", quiet = TRUE)
df_63 <- read_dta("Example_6_3.dta")

# Declare as panel data (plm equivalent of xtset fips year)
# Equivalent to: xtset fips year, yearly
pdf_63 <- pdata.frame(df_63, index = c("fips", "year"))

# --- Check panel structure ---
# Equivalent to: xtdescribe
cat(". xtdescribe\n\n")
cat("  Panel variable: fips\n")
cat("  Time variable:  year,", min(df_63$year), "to", max(df_63$year), "\n")
cat("  Delta: 1 year\n")
cat("  n =", length(unique(df_63$fips)), "panels\n")
cat("  T =", length(unique(df_63$year)), "periods\n")
cat("  N =", nrow(df_63), "observations\n\n")
cat("  Balance:\n")
t_per_panel <- df_63 |> count(fips, name = "T_i")
cat("    min T =", min(t_per_panel$T_i),
    " max T =", max(t_per_panel$T_i), "\n")
cat("  Panels with all", length(unique(df_63$year)),
    "periods:", sum(t_per_panel$T_i == length(unique(df_63$year))), "\n\n")

# --- Cross-tabulation for time-invariant categorical variable ---
# Equivalent to: xttab region_compact
# xttab reports overall, between (# of panels), and within (always 100% for
# time-invariant variables) frequencies.
cat(". xttab region_compact\n\n")
overall <- df_63 |> count(region_compact, name = "overall_freq")
between <- df_63 |>
  distinct(fips, region_compact) |>
  count(region_compact, name = "between_freq")
xttab <- overall |>
  left_join(between, by = "region_compact") |>
  mutate(overall_pct = overall_freq / sum(overall_freq) * 100,
         between_pct = between_freq / sum(between_freq) * 100,
         within_pct  = 100)
print(as.data.frame(xttab), digits = 2, row.names = FALSE)
cat("\n")

# --- Transition probabilities for time-variant categorical variable ---
# Equivalent to: xttrans ugradmerit
# xttrans computes year-on-year transition probabilities within panels.
cat(". xttrans ugradmerit\n\n")
trans <- df_63 |>
  arrange(fips, year) |>
  group_by(fips) |>
  mutate(ugradmerit_cur  = as.numeric(ugradmerit),
         ugradmerit_next = dplyr::lead(ugradmerit_cur)) |>
  ungroup() |>
  filter(!is.na(ugradmerit_next))

trans_tbl <- trans |>
  group_by(ugradmerit_cur, ugradmerit_next) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(ugradmerit_cur) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup() |>
  select(ugradmerit_cur, ugradmerit_next, pct) |>
  pivot_wider(names_from = ugradmerit_next, values_from = pct,
              names_prefix = "to_")
print(as.data.frame(trans_tbl), digits = 2, row.names = FALSE)
cat("\n")

# ================================================================
# Section 6.2.4: Testing Differences in Means Across Groups (ANOVA)
# ================================================================
cat("*========================================================================\n")
cat("* Section 6.2.4: Testing Differences in Means Across Groups (ANOVA)\n")
cat("*========================================================================\n\n")

# Reload HSLS:09 dataset (df_623 is still in memory — recreating RaceEthnic
# mirrors the Stata reload to keep the workflow identical)
df_anova <- read_dta("Example_6_2_3.dta") |>
  mutate(
    RaceEthnic = case_when(
      X1RACE == 2            ~ 1L,
      X1RACE == 3            ~ 2L,
      X1RACE %in% c(4, 5)   ~ 3L,
      X1RACE == 6            ~ 4L,
      X1RACE %in% c(1, 7)   ~ 5L,
      X1RACE == 8            ~ 6L,
      TRUE                   ~ 0L
    ),
    RaceEthnic = factor(RaceEthnic,
                        levels = 1:6,
                        labels = c("Asian", "Black", "Hispanic",
                                   "Multiracial", "Other", "White")),
    X1SEX = factor(X1SEX)
  )

# --- One-way ANOVA: earnings by race/ethnicity ---
# Equivalent to: anova EarnHr RaceEthnic
cat(". anova EarnHr RaceEthnic\n\n")
aov1 <- aov(EarnHr ~ RaceEthnic, data = df_anova)
print(summary(aov1))
cat("\n")

# --- Alternative: oneway with group means and Bartlett test ---
# Equivalent to: oneway EarnHr RaceEthnic, tabulate
cat(". oneway EarnHr RaceEthnic, tabulate\n\n")
oneway_tbl <- df_anova |>
  group_by(RaceEthnic) |>
  summarise(mean = mean(EarnHr, na.rm = TRUE),
            sd   = sd(EarnHr,   na.rm = TRUE),
            n    = n(), .groups = "drop")
print(as.data.frame(oneway_tbl), digits = 4, row.names = FALSE)
cat("\n")
cat("Bartlett's equal-variances test:\n")
print(bartlett.test(EarnHr ~ RaceEthnic, data = df_anova))
cat("\n")

# --- Post-hoc pairwise comparisons with Bonferroni correction ---
# Equivalent to: pwmean EarnHr, over(RaceEthnic) mcompare(bonferroni) effects
cat(". pwmean EarnHr, over(RaceEthnic) mcompare(bonferroni) effects\n\n")
pw_bonf <- pairwise.t.test(df_anova$EarnHr, df_anova$RaceEthnic,
                            p.adjust.method = "bonferroni",
                            pool.sd         = TRUE)
print(pw_bonf)
cat("\n")

# --- Two-way ANOVA: earnings by race/ethnicity and sex ---
# Equivalent to: anova EarnHr RaceEthnic##X1SEX
cat(". anova EarnHr RaceEthnic##X1SEX\n\n")
aov2 <- aov(EarnHr ~ RaceEthnic * X1SEX, data = df_anova)
print(summary(aov2))
cat("\n")

# --- Test for interaction effect ---
# Equivalent to: testparm RaceEthnic#X1SEX
# In R, drop() on the full model vs. the additive model gives the F-test
# for the interaction term, which is equivalent to testparm.
cat(". testparm RaceEthnic#X1SEX\n\n")
aov2_add <- aov(EarnHr ~ RaceEthnic + X1SEX, data = df_anova)
interaction_test <- anova(aov2_add, aov2)
print(interaction_test)
cat("\n")

# ================================================================
# Section 6.3.1: Graphs — Exploratory Data Analysis (EDA)
# ================================================================
cat("*========================================================================\n")
cat("* Section 6.3.1: Graphs — Exploratory Data Analysis (EDA)\n")
cat("*========================================================================\n\n")

# Load panel dataset and create derived variables
df_graphs <- read_dta("Example_6_3.dta") |>
  mutate(
    stapr_fte  = stapr  / fte,
    netuit_fte = netuit / fte,
    region_compact = haven::as_factor(region_compact)
  )

# --- Fig. 6.8: Histogram of State Appropriations per FTE Student ---
# Equivalent to: histogram stapr_fte, normal
cat("* --- Fig. 6.8: Histogram of State Appropriations per FTE Student ---\n")
fig6_8 <- ggplot(df_graphs, aes(x = stapr_fte)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins  = 31,
                 fill  = "steelblue", colour = "white", alpha = 0.8) +
  stat_function(fun  = dnorm,
                args = list(mean = mean(df_graphs$stapr_fte, na.rm = TRUE),
                            sd   = sd(df_graphs$stapr_fte,   na.rm = TRUE)),
                colour = "black", linewidth = 0.8) +
  labs(title = "Fig. 6.8  Histogram of State Appropriations per FTE Student",
       x = "State Appropriations per FTE Student ($)",
       y = "Density") +
  theme_bw()
save_fig(fig6_8, "fig6_8_histogram_stapr_fte_R.png")

# --- Fig. 6.9: Box Chart of State Appropriations per FTE Student ---
# Equivalent to: graph box stapr_fte
cat("* --- Fig. 6.9: Box Chart of State Appropriations per FTE Student ---\n")
fig6_9 <- ggplot(df_graphs, aes(y = stapr_fte)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, width = 0.4,
               outlier.shape = 16, outlier.colour = "firebrick") +
  labs(title = "Fig. 6.9  Box Chart of State Appropriations per FTE Student",
       y = "State Appropriations per FTE Student ($)") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
save_fig(fig6_9, "fig6_9_box_stapr_fte_R.png")

# --- Fig. 6.10: Histogram of Membership in Regional Compacts ---
# Equivalent to: histogram region_compact, discrete addlabels ylabel(,grid) percent
cat("* --- Fig. 6.10: Histogram of Membership in Regional Compacts ---\n")
rc_pct <- df_graphs |>
  distinct(fips, region_compact) |>     # one row per state (time-invariant)
  count(region_compact) |>
  mutate(pct = n / sum(n) * 100)

fig6_10 <- ggplot(rc_pct, aes(x = region_compact, y = pct,
                               label = sprintf("%.1f%%", pct))) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(vjust = -0.4, size = 3.5) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Fig. 6.10  Histogram of Membership in Regional Compacts",
       x = "Regional Compact",
       y = "Percent") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())
save_fig(fig6_10, "fig6_10_histogram_region_compact_R.png")

# --- Fig. 6.11: State Appropriations per FTE Student by Regional Compact ---
# Equivalent to: histogram stapr_fte, by(region_compact)
cat("* --- Fig. 6.11: State Appropriations per FTE by Regional Compact ---\n")
fig6_11 <- ggplot(df_graphs, aes(x = stapr_fte)) +
  geom_histogram(bins = 20, fill = "steelblue", colour = "white", alpha = 0.8) +
  facet_wrap(~ region_compact, scales = "free_y") +
  labs(title = "Fig. 6.11  State Appropriations per FTE Student by Regional Compact",
       x = "State Appropriations per FTE Student ($)",
       y = "Frequency") +
  theme_bw()
save_fig(fig6_11, "fig6_11_histogram_stapr_fte_by_region_R.png")

# --- Fig. 6.12: Box Chart of State Appropriations per FTE by Regional Compact ---
# Equivalent to: graph box stapr_fte, by(region_compact)
cat("* --- Fig. 6.12: Box Chart of State Appropriations per FTE by Regional Compact ---\n")
fig6_12 <- ggplot(df_graphs, aes(x = region_compact, y = stapr_fte,
                                  fill = region_compact)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.colour = "firebrick") +
  facet_wrap(~ region_compact, scales = "free_x", nrow = 1) +
  labs(title = "Fig. 6.12  Box Chart of State Appropriations per FTE by Regional Compact",
       x = NULL, y = "State Appropriations per FTE Student ($)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank())
save_fig(fig6_12, "fig6_12_box_stapr_fte_by_region_R.png")

# Data subset for 2016 scatter plots
df_2016 <- df_graphs |> filter(year == 2016)

# --- Fig. 6.13: Scatter Plot — State Appropriations vs Net Tuition (2016) ---
# Equivalent to: graph twoway scatter stapr_fte netuit_fte if year==2016
cat("* --- Fig. 6.13: Scatter Plot (2016) ---\n")
fig6_13 <- ggplot(df_2016, aes(x = netuit_fte, y = stapr_fte)) +
  geom_point(colour = "steelblue", size = 2) +
  labs(title = "Fig. 6.13  State Appropriations and Net Tuition Revenue per FTE Student, FY2016",
       x = "Net Tuition Revenue per FTE Student ($)",
       y = "State Appropriations per FTE Student ($)") +
  theme_bw()
save_fig(fig6_13, "fig6_13_scatter_2016_R.png")

# --- Fig. 6.14: Scatter Plot with Fitted Regression Line (Method 1) ---
# Equivalent to: twoway (scatter) (lfit) if year==2016
cat("* --- Fig. 6.14: Scatter Plot with Fitted Line (Method 1, 2016) ---\n")
fig6_14 <- ggplot(df_2016, aes(x = netuit_fte, y = stapr_fte)) +
  geom_point(colour = "steelblue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.8) +
  labs(title = "Fig. 6.14  State Appropriations and Net Tuition Revenue per FTE with Fitted Line, FY2016",
       x = "Net Tuition Revenue per FTE Student ($)",
       y = "State Appropriations per FTE Student ($)") +
  theme_bw()
save_fig(fig6_14, "fig6_14_scatter_fitted_2016_R.png")

# --- Fig. 6.15: Scatter Plot with Fitted Line and State Labels (Method 2) ---
# Equivalent to: twoway scatter, mlabel(state) || lfit if year==2016
cat("* --- Fig. 6.15: Scatter Plot with Fitted Line and State Labels (Method 2, 2016) ---\n")
fig6_15 <- ggplot(df_2016, aes(x = netuit_fte, y = stapr_fte, label = state)) +
  geom_point(colour = "steelblue", size = 2) +
  geom_text(vjust = -0.6, size = 2.5, colour = "grey30") +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.8) +
  labs(title = "Fig. 6.15  State Appropriations and Net Tuition Revenue per FTE with State Labels, FY2016",
       x = "Net Tuition Revenue per FTE Student ($)",
       y = "State Appropriations per FTE Student ($)") +
  theme_bw()
save_fig(fig6_15, "fig6_15_scatter_labels_2016_R.png")

# --- Fig. 6.16 & 6.17: aaplot equivalent — scatter with regression line,
#     annotations (R², intercept, slope), mimicking Stata's aaplot output ---
# Equivalent to: aaplot netuit_fte stapr_fte if year==1990 / ==2016

aaplot_r <- function(data, year_val, fig_num, file_name) {
  d    <- data |> filter(year == year_val)
  fit  <- lm(netuit_fte ~ stapr_fte, data = d)
  r2   <- summary(fit)$r.squared
  b0   <- coef(fit)[1]
  b1   <- coef(fit)[2]
  anno <- sprintf("y = %.4f + %.4f x\nR² = %.4f", b0, b1, r2)

  p <- ggplot(d, aes(x = stapr_fte, y = netuit_fte)) +
    geom_point(colour = "steelblue", size = 2) +
    geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.8) +
    annotate("text",
             x     = max(d$stapr_fte, na.rm = TRUE) * 0.65,
             y     = max(d$netuit_fte, na.rm = TRUE) * 0.95,
             label = anno, hjust = 0, size = 3.5) +
    labs(
      title = sprintf(
        "Fig. 6.%d  State Appropriations and Net Tuition Revenue per FTE, FY%d",
        fig_num, year_val),
      x = "State Appropriations per FTE Student ($)",
      y = "Net Tuition Revenue per FTE Student ($)"
    ) +
    theme_bw()

  save_fig(p, file_name)
}

cat("* --- Fig. 6.16: aaplot equivalent, FY 1990 ---\n")
aaplot_r(df_graphs, 1990, 16, "fig6_16_aaplot_1990_R.png")

cat("* --- Fig. 6.17: aaplot equivalent, FY 2016 ---\n")
aaplot_r(df_graphs, 2016, 17, "fig6_17_aaplot_2016_R.png")

cat("\nAll graphs saved to:", graphs_dir, "\n")

# ================================================================
# Close log
# Equivalent to: log close
# ================================================================
cat("\nChapter 6 R script completed:", format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n")
sink()   # closes the log connection

# ================================================================
# END OF CHAPTER 6 R CODE
# ================================================================
