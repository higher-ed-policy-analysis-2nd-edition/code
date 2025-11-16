# ================================================================
# Chapter 6 - Using Descriptive Statistics and Graphs
# Complete R translation of Stata_Code6.do
# Higher Education Policy Analysis Using Quantitative Techniques 
# (2nd Edition)
# Source: https://github.com/higher-ed-policy-analysis-2nd-edition/code/ch6
# Author: Marvin A. Titus (original Stata)
# Date: November 16, 2025
# ================================================================

# Script tested in R >= 4.0
# NOTE: Set working directory if you want to save .dta/.xlsx files persistently:
# ch6data <- "C:/Users/YourName/Documents/book-materials/ch6/data"
# dir.create(ch6data, recursive = TRUE, showWarnings = FALSE)
# setwd(ch6data)

# ----------------------------------------------------------------
# REQUIRED PACKAGES
# ----------------------------------------------------------------
required_packages <- c(
  "tidyverse", "haven", "readxl", "psych", "plm",
  "car", "DescTools", "lmtest", "sandwich", "ggplot2"
)

new_pkgs <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) {
  message("Installing missing packages: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs, dependencies = TRUE)
}

library(tidyverse)
library(haven)
library(readxl)
library(psych)
library(plm)
library(car)
library(DescTools)
library(lmtest)
library(sandwich)
library(ggplot2)

# Helper: safe check for variable existence
have_vars <- function(df, vars) {
  missing <- vars[!(vars %in% names(df))]
  list(ok = length(missing) == 0, missing = missing)
}

# ----------------------------------------------------------------
# Section 6.2.1: Measures of Central Tendency
# ----------------------------------------------------------------

# Download or read tabn302_50.xlsx (reformatted sheet)
url_tabn <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch4/tabn302_50.xlsx"
dest_tabn <- file.path(tempdir(), "tabn302_50.xlsx")
download.file(url_tabn, dest_tabn, mode = "wb")
data_6_1 <- read_excel(dest_tabn, sheet = "reformatted")

message("Loaded tabn302_50.xlsx (reformatted): rows=", nrow(data_6_1), " cols=", ncol(data_6_1))

# Arithmetic, geometric, harmonic means for Public and Private
means_6_1 <- data_6_1 %>%
  summarise(
    Public_arith = mean(Public, na.rm = TRUE),
    Private_arith = mean(Private, na.rm = TRUE),
    Public_geom = exp(mean(log(Public), na.rm = TRUE)),
    Private_geom = exp(mean(log(Private), na.rm = TRUE)),
    Public_harm = 1 / mean(1 / Public, na.rm = TRUE),
    Private_harm = 1 / mean(1 / Private, na.rm = TRUE)
  )

print(means_6_1)

# Equivalent of Stata's mean (mean and standard error)
mean_se_6_1 <- data_6_1 %>%
  summarise(
    Public_mean = mean(Public, na.rm = TRUE),
    Private_mean = mean(Private, na.rm = TRUE),
    Public_se = sd(Public, na.rm = TRUE) / sqrt(sum(!is.na(Public))),
    Private_se = sd(Private, na.rm = TRUE) / sqrt(sum(!is.na(Private)))
  )
print(mean_se_6_1)

# Detailed summary (equivalent to sum, detail)
message("Detailed summary (psych::describe):")
print(describe(data_6_1 %>% select(where(is.numeric))))

# Additional detailed stats similar to Stata 'sum, detail'
detailed_public <- data_6_1 %>%
  summarise(
    n = sum(!is.na(Public)),
    mean = mean(Public, na.rm = TRUE),
    sd = sd(Public, na.rm = TRUE),
    min = min(Public, na.rm = TRUE),
    p25 = quantile(Public, 0.25, na.rm = TRUE),
    median = median(Public, na.rm = TRUE),
    p75 = quantile(Public, 0.75, na.rm = TRUE),
    max = max(Public, na.rm = TRUE),
    skew = DescTools::Skew(Public),
    kurtosis = DescTools::Kurt(Public) # Note: DescTools reports regular kurtosis (not excess)
  )
print(detailed_public)

# ----------------------------------------------------------------
# Section 6.2.2: Measures of Dispersion
# ----------------------------------------------------------------

# Download SHEEO finance dataset
url_6_2_2 <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch6/Example_6_2_2.dta"
dest_6_2_2 <- file.path(tempdir(), "Example_6_2_2.dta")
download.file(url_6_2_2, dest_6_2_2, mode = "wb")
data_6_2_2 <- read_dta(dest_6_2_2)

message("Loaded Example_6_2_2.dta: rows=", nrow(data_6_2_2), " cols=", ncol(data_6_2_2))

# Coefficient of variation for NetTuition and FTEStudents
cv_6_2 <- data_6_2_2 %>%
  summarise(
    NetTuition_cv = sd(NetTuition, na.rm = TRUE) / mean(NetTuition, na.rm = TRUE),
    FTEStudents_cv = sd(FTEStudents, na.rm = TRUE) / mean(FTEStudents, na.rm = TRUE)
  )
print(cv_6_2)

# Descriptive statistics by State
desc_by_state <- data_6_2_2 %>%
  group_by(State) %>%
  summarise(
    NetTuition_mean = mean(NetTuition, na.rm = TRUE),
    NetTuition_median = median(NetTuition, na.rm = TRUE),
    NetTuition_sd = sd(NetTuition, na.rm = TRUE),
    NetTuition_min = min(NetTuition, na.rm = TRUE),
    NetTuition_max = max(NetTuition, na.rm = TRUE),
    NetTuition_cv = sd(NetTuition, na.rm = TRUE) / mean(NetTuition, na.rm = TRUE),
    FTEStudents_mean = mean(FTEStudents, na.rm = TRUE),
    FTEStudents_median = median(FTEStudents, na.rm = TRUE),
    FTEStudents_sd = sd(FTEStudents, na.rm = TRUE),
    FTEStudents_min = min(FTEStudents, na.rm = TRUE),
    FTEStudents_max = max(FTEStudents, na.rm = TRUE),
    FTEStudents_cv = sd(FTEStudents, na.rm = TRUE) / mean(FTEStudents, na.rm = TRUE),
    .groups = "drop"
  )
print(head(desc_by_state, 10))

# Descriptive statistics by FY (year)
desc_by_year <- data_6_2_2 %>%
  group_by(FY) %>%
  summarise(
    NetTuition_mean = mean(NetTuition, na.rm = TRUE),
    NetTuition_median = median(NetTuition, na.rm = TRUE),
    NetTuition_sd = sd(NetTuition, na.rm = TRUE),
    NetTuition_min = min(NetTuition, na.rm = TRUE),
    NetTuition_max = max(NetTuition, na.rm = TRUE),
    NetTuition_cv = sd(NetTuition, na.rm = TRUE) / mean(NetTuition, na.rm = TRUE),
    FTEStudents_mean = mean(FTEStudents, na.rm = TRUE),
    FTEStudents_median = median(FTEStudents, na.rm = TRUE),
    FTEStudents_sd = sd(FTEStudents, na.rm = TRUE),
    FTEStudents_min = min(FTEStudents, na.rm = TRUE),
    FTEStudents_max = max(FTEStudents, na.rm = TRUE),
    FTEStudents_cv = sd(FTEStudents, na.rm = TRUE) / mean(FTEStudents, na.rm = TRUE),
    .groups = "drop"
  )
print(desc_by_year)

# ----------------------------------------------------------------
# Section 6.2.3: Distributions
# ----------------------------------------------------------------

# Download HSLS:09 condensed dataset with earnings variable
url_6_2_3 <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch6/Example_6_2_3.dta"
dest_6_2_3 <- file.path(tempdir(), "Example_6_2_3.dta")
download.file(url_6_2_3, dest_6_2_3, mode = "wb")
data_6_2_3 <- read_dta(dest_6_2_3)

message("Loaded Example_6_2_3.dta: rows=", nrow(data_6_2_3), " cols=", ncol(data_6_2_3))

# Examine race variable codebook / labels
if (is.labelled(data_6_2_3$X1RACE)) {
  message("X1RACE is labelled; levels and counts:")
  print(table(data_6_2_3$X1RACE))
} else {
  print(table(data_6_2_3$X1RACE))
}

# Recode race/ethnicity to simpler factor RaceEthnic
data_6_2_3 <- data_6_2_3 %>%
  mutate(
    RaceEthnic = case_when(
      X1RACE == 2 ~ 1,                    # Asian
      X1RACE == 3 ~ 2,                    # Black
      X1RACE %in% c(4,5) ~ 3,             # Hispanic
      X1RACE == 6 ~ 4,                    # Multiracial
      X1RACE %in% c(1,7) ~ 5,             # Other
      X1RACE == 8 ~ 6,                    # White
      TRUE ~ NA_real_
    ),
    RaceEthnic = factor(RaceEthnic,
                        levels = 1:6,
                        labels = c("Asian","Black","Hispanic","Multiracial","Other","White"))
  )

# Frequency distribution (proportions)
prop_table <- prop.table(table(data_6_2_3$X1RACE))
print(prop_table)

# Sorted counts and percentages (equivalent to tab, sort)
freq_sorted <- data_6_2_3 %>%
  count(X1RACE) %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(desc(n))
print(freq_sorted)

# One-way table summarizing EarnHr by X1RACE
earn_by_race <- data_6_2_3 %>%
  group_by(X1RACE) %>%
  summarise(n = n(), mean_EarnHr = mean(EarnHr, na.rm = TRUE), sd_EarnHr = sd(EarnHr, na.rm = TRUE), .groups = "drop")
print(earn_by_race)

# Two-way table: means by race and sex
two_way <- data_6_2_3 %>%
  group_by(X1RACE, X1SEX) %>%
  summarise(n = n(), mean_EarnHr = mean(EarnHr, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = X1SEX, values_from = c(mean_EarnHr, n), names_prefix = "Sex_")
print(two_way)

# Alternative: use recoded RaceEthnic
two_way_recoded <- data_6_2_3 %>%
  group_by(RaceEthnic, X1SEX) %>%
  summarise(n = n(), mean_EarnHr = mean(EarnHr, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = X1SEX, values_from = c(mean_EarnHr, n), names_prefix = "Sex_")
print(two_way_recoded)

# Panel data: Download Example_6_3.dta and examine panel structure
url_6_3 <- "https://raw.githubusercontent.com/higher-ed-policy-analysis-2nd-edition/data/main/ch6/Example_6_3.dta"
dest_6_3 <- file.path(tempdir(), "Example_6_3.dta")
download.file(url_6_3, dest_6_3, mode = "wb")
data_6_3 <- read_dta(dest_6_3)

# Declare panel using plm::pdata.frame
pdata_6_3 <- pdata.frame(data_6_3, index = c("fips", "year"))
message("Panel dimensions (pdim):")
print(pdim(pdata_6_3)) # Balanced panel expected: n = 50, T = 27, N = 1350

# Summarize panel structure by fips
panel_summary <- data_6_3 %>%
  group_by(fips) %>%
  summarise(n_years = n(), min_year = min(year), max_year = max(year), .groups = "drop")
print(summary(panel_summary))

# Cross-tabulation for region_compact (time-invariant)
region_tab <- data_6_3 %>%
  group_by(fips) %>%
  summarise(region_compact = first(region_compact), .groups = "drop") %>%
  count(region_compact) %>%
  mutate(percent = n / sum(n) * 100)
print(region_tab)

# ----------------------------------------------------------------
# Section 6.2.4: ANOVA (Testing Differences in Means Across Groups)
# ----------------------------------------------------------------

# One-way ANOVA: EarnHr by RaceEthnic
anova_model <- aov(EarnHr ~ RaceEthnic, data = data_6_2_3)
message("One-way ANOVA (EarnHr ~ RaceEthnic):")
print(summary(anova_model))

# Alternative: Welch's one-way test (does not assume equal variances)
message("Welch's ANOVA (oneway.test):")
print(oneway.test(EarnHr ~ RaceEthnic, data = data_6_2_3))

# Group means and sample sizes
group_summary <- data_6_2_3 %>%
  group_by(RaceEthnic) %>%
  summarise(n = n(), mean = mean(EarnHr, na.rm = TRUE), sd = sd(EarnHr, na.rm = TRUE),
            se = sd(EarnHr, na.rm = TRUE) / sqrt(n()), .groups = "drop")
print(group_summary)

# Post-hoc pairwise comparisons with Bonferroni correction
pairwise_bonf <- pairwise.t.test(data_6_2_3$EarnHr, data_6_2_3$RaceEthnic, p.adjust.method = "bonferroni")
message("Pairwise comparisons (Bonferroni):")
print(pairwise_bonf)

# Tukey HSD for all pairwise comparisons
tukey_res <- TukeyHSD(anova_model)
message("Tukey HSD results:")
print(tukey_res)

# Two-way ANOVA: EarnHr by RaceEthnic × X1SEX
anova_twoway <- aov(EarnHr ~ RaceEthnic * factor(X1SEX), data = data_6_2_3)
message("Two-way ANOVA (RaceEthnic * X1SEX):")
print(summary(anova_twoway))

# Type III tests (car::Anova) for marginal tests (similar to Stata's anova with interactions)
message("Type III tests (Anova from car):")
print(Anova(anova_twoway, type = "III"))

# Test for interaction effect (equivalent to Stata testparm)
# In R, the summary or Anova above shows the interaction. For a joint test:
interaction_terms <- grep("RaceEthnic:factor\\(X1SEX\\)", names(coef(lm(EarnHr ~ RaceEthnic * factor(X1SEX), data = data_6_2_3))), value = TRUE)
if (length(interaction_terms) > 0) {
  message("Testing interaction terms jointly using linearHypothesis (car):")
  print(linearHypothesis(lm(EarnHr ~ RaceEthnic * factor(X1SEX), data = data_6_2_3), interaction_terms))
} else {
  message("Could not identify interaction term names for joint test; check model terms.")
}

# ----------------------------------------------------------------
# Section 6.3.1: Graphs—Exploratory Data Analysis (EDA)
# ----------------------------------------------------------------

# Use the panel dataset (data_6_3) loaded earlier
# Create stapr_fte variable
if (!"stapr" %in% names(data_6_3) || !"fte" %in% names(data_6_3)) {
  stop("data_6_3 must contain 'stapr' and 'fte' variables to compute stapr_fte.")
}
data_6_3 <- data_6_3 %>% mutate(stapr_fte = stapr / fte)

# Histogram with normal curve overlay
p_hist <- ggplot(data_6_3, aes(x = stapr_fte)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm,
                args = list(mean = mean(data_6_3$stapr_fte, na.rm = TRUE),
                            sd = sd(data_6_3$stapr_fte, na.rm = TRUE)),
                color = "red", size = 1) +
  labs(title = "Distribution of State Appropriations per FTE",
       x = "State Appropriations per FTE", y = "Density") +
  theme_minimal()
print(p_hist)

# Boxplot
p_box <- ggplot(data_6_3, aes(y = stapr_fte)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box Plot of State Appropriations per FTE", y = "State Appropriations per FTE") +
  theme_minimal()
print(p_box)

# Histogram of categorical variable (region_compact) as percent with labels
if ("region_compact" %in% names(data_6_3)) {
  p_cat_hist <- ggplot(data_6_3, aes(x = factor(region_compact))) +
    geom_bar(aes(y = after_stat(count) / sum(after_stat(count)) * 100),
             fill = "lightblue", color = "black") +
    geom_text(stat = "count", aes(label = after_stat(count),
                                  y = after_stat(count) / sum(after_stat(count)) * 100),
              vjust = -0.5) +
    scale_x_discrete(labels = function(x) x) +
    labs(title = "Distribution of Regional Compact Membership",
         x = "Regional Compact", y = "Percent") +
    theme_minimal()
  print(p_cat_hist)
} else {
  message("region_compact not present in data_6_3; skipping categorical histogram.")
}

# Faceted histograms by region_compact
if ("region_compact" %in% names(data_6_3)) {
  p_facet <- ggplot(data_6_3, aes(x = stapr_fte)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "black") +
    facet_wrap(~ region_compact, ncol = 2) +
    labs(title = "State Appropriations per FTE by Regional Compact",
         x = "State Appropriations per FTE", y = "Frequency") +
    theme_minimal()
  print(p_facet)
}

# Boxplot by region_compact
if ("region_compact" %in% names(data_6_3)) {
  p_box_region <- ggplot(data_6_3, aes(x = factor(region_compact), y = stapr_fte)) +
    geom_boxplot(fill = "lightblue") +
    labs(title = "State Appropriations per FTE by Regional Compact",
         x = "Regional Compact", y = "State Appropriations per FTE") +
    theme_minimal()
  print(p_box_region)
}

# Create netuit_fte for scatter plots
if (!("netuit" %in% names(data_6_3) && "fte" %in% names(data_6_3))) {
  stop("data_6_3 must contain 'netuit' and 'fte' to compute netuit_fte.")
}
data_6_3 <- data_6_3 %>% mutate(netuit_fte = netuit / fte)

# Scatter plot (2016)
scatter_2016 <- ggplot(data_6_3 %>% filter(year == 2016), aes(x = netuit_fte, y = stapr_fte)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "State Appropriations vs. Net Tuition per FTE (2016)",
       x = "Net Tuition per FTE", y = "State Appropriations per FTE") +
  theme_minimal()
print(scatter_2016)

# Scatter plot with fitted regression line
scatter_with_fit <- ggplot(data_6_3 %>% filter(year == 2016), aes(x = netuit_fte, y = stapr_fte)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "State Appropriations vs. Net Tuition per FTE (2016) - with fit",
       x = "Net Tuition per FTE", y = "State Appropriations per FTE") +
  theme_minimal()
print(scatter_with_fit)

# Scatter with fitted line and labels (may overlap; ggrepel recommended)
# If ggrepel is available, use it; otherwise use geom_text with check_overlap
if (requireNamespace("ggrepel", quietly = TRUE)) {
  library(ggrepel)
  scatter_labels <- ggplot(data_6_3 %>% filter(year == 2016), aes(x = netuit_fte, y = stapr_fte, label = state)) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    geom_text_repel(size = 2.5) +
    labs(title = "State Appropriations vs. Net Tuition per FTE (2016) - labeled",
         x = "Net Tuition per FTE", y = "State Appropriations per FTE") +
    theme_minimal()
  print(scatter_labels)
} else {
  scatter_labels <- ggplot(data_6_3 %>% filter(year == 2016), aes(x = netuit_fte, y = stapr_fte, label = state)) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    geom_text(size = 2, hjust = 0, vjust = 0, check_overlap = TRUE) +
    labs(title = "State Appropriations vs. Net Tuition per FTE (2016) - labeled (basic)",
         x = "Net Tuition per FTE", y = "State Appropriations per FTE") +
    theme_minimal()
  print(scatter_labels)
}

# Scatter fit for 1990
scatter_1990 <- ggplot(data_6_3 %>% filter(year == 1990), aes(x = stapr_fte, y = netuit_fte)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE, formula = y ~ x) +
  labs(title = "Net Tuition vs. State Appropriations per FTE (1990)",
       x = "State Appropriations per FTE", y = "Net Tuition per FTE") +
  theme_minimal()
print(scatter_1990)

# Scatter fit for 2016 (stapr_fte -> netuit_fte)
scatter_fit_2016 <- ggplot(data_6_3 %>% filter(year == 2016), aes(x = stapr_fte, y = netuit_fte)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE, formula = y ~ x) +
  labs(title = "Net Tuition vs. State Appropriations per FTE (2016)",
       x = "State Appropriations per FTE", y = "Net Tuition per FTE") +
  theme_minimal()
print(scatter_fit_2016)

# ----------------------------------------------------------------
# Clean up (optional)
# ----------------------------------------------------------------
# If you want to remove large objects from the workspace uncomment:
# rm(list = ls())
# gc()

# ================================================================
# END OF CHAPTER
# ================================================================