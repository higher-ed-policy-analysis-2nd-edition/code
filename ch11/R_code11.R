#===========================================================================
# Chapter 11: Bayesian MTE Microsimulation
# Cost–Benefit Analysis of a $100k Lifetime Cap on Grad PLUS Loans
#
# FULL R REPRODUCTION (Figures 11.1 - 11.7)
#===========================================================================

# 1. Setup and Libraries ---------------------------------------------------
library(tidyverse)
library(fixest)    # For robust regressions
library(MASS)      # For multivariate normal draws (mvrnorm)
library(scales)    # For formatting numbers
library(broom)     # For tidying model outputs

set.seed(20251201)

# ── Working directory ────────────────────────────────────────────────────────
main_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 11"
setwd(main_dir)

# ── Directory structure ─────────────────────────────────────────────────────
dir.create("Output/logs", recursive = TRUE, showWarnings = FALSE)
dir.create("Output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("Output/graphs", recursive = TRUE, showWarnings = FALSE)

tables_dir <- "Output/tables"
graphs_dir <- "Output/graphs"

# ── Log Initialization ──────────────────────────────────────────────────────
while(sink.number() > 0) sink()
log_path <- file.path(main_dir, "Output/logs/Chapter11_R_output.log")
sink(log_path, append = FALSE, split = TRUE) 

cat("============================================================\n")
cat("Chapter 11 R Log Opened:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================\n\n")

# Global Parameters
discount_rate   <- 0.03
career_years    <- 30
subsidy_rate    <- 0.20
cap_threshold   <- 100
base_salary     <- 47000
S_draws         <- 1000

pv_factor <- (1 - (1 + discount_rate)^(-career_years)) / discount_rate

# 2. Data Generation -------------------------------------------------------
cat("Section 2: Synthetic Data Generation\n")

n <- 8000
df <- tibble(id = 1:n) %>%
  mutate(
    female = rbinom(n, 1, 0.52), black = rbinom(n, 1, 0.13),
    hispanic = rbinom(n, 1, 0.10), asian = rbinom(n, 1, 0.07),
    age_ba = pmax(21, pmin(35, round(rnorm(n, 24, 3)))),
    firstgen = rbinom(n, 1, 0.26), parent_income_q = ceiling(runif(n) * 4),
    parent_grad = rbinom(n, 1, 0.38), ugpa = pmax(2.0, pmin(4.0, round(rnorm(n, 3.3, 0.45), 2))),
    prog_draw = runif(n),
    stem_major = as.numeric(prog_draw < 0.22),
    bus_major = as.numeric(prog_draw >= 0.22 & prog_draw < 0.40),
    ed_major = as.numeric(prog_draw >= 0.40 & prog_draw < 0.56),
    health_major = as.numeric(prog_draw >= 0.56 & prog_draw < 0.70),
    selective_inst = rbinom(n, 1, 0.28), public_ug = rbinom(n, 1, 0.72),
    state_unemp = pmax(2.5, pmin(10.5, round(rnorm(n, 5.2, 1.3), 1))),
    metro = rbinom(n, 1, 0.68), ga_funding_adj = pmax(2.0, pmin(14.0, round(rnorm(n, 7.5, 2.2), 1))),
    loan_noise = rnorm(n, 0, 22),
    grad_plus_loans = pmax(0, pmin(250, 30 + 25*stem_major + 35*bus_major + 10*ed_major + 
                        20*health_major + 15*selective_inst + 12*(4-parent_income_q) - 
                        0.8*ga_funding_adj + loan_noise)),
    annual_tuition = pmax(10, 25 + 35*bus_major + 10*stem_major + 20*health_major + 
                         12*selective_inst - 3*public_ug + rnorm(n, 0, 4)),
    program_years = 2 + 0.5*stem_major, gross_tuition = annual_tuition * program_years,
    net_inst_rev = 0.65 * gross_tuition, epsilon = rnorm(n),
    index_latent = -1.97 + 0.13*ga_funding_adj + 0.04*ugpa*10 + 0.08*parent_grad - 
                   0.10*firstgen + 0.06*(parent_income_q-2) + 0.05*selective_inst + 
                   0.05*metro + 0.07*ed_major - 0.03*age_ba + epsilon,
    masters = as.numeric(index_latent > 0), phat_true = pnorm(index_latent)
  )

# Outcomes
b0_true <- -2.50; b1_true <- 19.30; b2_true <- -30.25; b3_true <- 15.12
df <- df %>% mutate(
    u_true = 1 - phat_true, mte_true = b0_true + b1_true*u_true + b2_true*u_true^2 + b3_true*u_true^3,
    Y1_latent = mte_true + rnorm(n, 0, 0.40), Y0_latent = rnorm(n, 0, 0.70),
    ln_salary = 10.0 + Y1_latent*masters + Y0_latent*(1-masters) + 0.25*ugpa + 
                0.15*stem_major + 0.25*bus_major - 0.18*ed_major + 0.10*selective_inst - 
                0.07*female - 0.06*black + 0.025*(parent_income_q-2) + rnorm(n, 0, 0.20),
    salary = exp(ln_salary), above_cap = as.numeric(grad_plus_loans > cap_threshold),
    loan_overage = pmax(0, grad_plus_loans - cap_threshold)
  )

# 4. Estimation ------------------------------------------------------------
cat("\nSection 4 & 5: Estimation\n")
X_vars <- c("female", "black", "hispanic", "asian", "age_ba", "firstgen", "parent_income_q", "parent_grad", "ugpa", "stem_major", "bus_major", "ed_major", "selective_inst", "public_ug", "state_unemp", "metro")
formula_probit <- as.formula(paste("masters ~ ga_funding_adj +", paste(X_vars, collapse = " + ")))
probit_mod <- glm(formula_probit, family = binomial(link = "probit"), data = df)
df$phat <- predict(probit_mod, type = "response")
df <- df %>% mutate(phat2 = phat^2, phat3 = phat^3)

formula_mte <- as.formula(paste("ln_salary ~ masters + masters:phat + masters:phat2 + masters:phat3 + phat + phat2 + phat3 +", paste(X_vars, collapse = " + ")))
mte_mod <- feols(formula_mte, data = df, vcov = "hc1")

# 7. Simulation ------------------------------------------------------------
cat("\nSection 7: Running Bayesian Microsimulation...\n")
b_full <- coef(mte_mod); V_full <- vcov(mte_mod)
B_draws <- mvrnorm(S_draws, mu = b_full, Sigma = V_full)

sim_results <- tibble(draw = 1:S_draws) %>%
  mutate(fiscal_s = 0, behav_cost_s = 0, effic_gain_s = 0, net_benefit_s = 0, inst_rev_loss_s = 0, bcr_s = 0, ate_s = 0, att_s = 0, atu_s = 0, b0_s=0, b1_s=0, b2_s=0, b3_s=0)

phat_vec <- df$phat; loan_overage_vec <- df$loan_overage; above_cap_vec <- df$above_cap; masters_vec <- df$masters; net_inst_rev_vec <- df$net_inst_rev
displaced_E <- (pnorm(df$loan_overage / 50) * df$above_cap) * df$masters

for(s in 1:S_draws) {
  b0_s <- B_draws[s, "masters"]; b1_s <- B_draws[s, "masters:phat"]; b2_s <- B_draws[s, "masters:phat2"]; b3_s <- B_draws[s, "masters:phat3"]
  base_sal_s <- exp(log(base_salary) + rnorm(1, 0, 0.10)); sub_rate_s <- subsidy_rate + runif(1, -0.05, 0.05); tuition_mult_s <- exp(rnorm(1, 0, 0.08))
  mte_s <- b0_s + b1_s*phat_vec + b2_s*phat_vec^2 + b3_s*phat_vec^3
  
  fiscal_s <- sum(loan_overage_vec * sub_rate_s * above_cap_vec)
  behav_cost_s <- sum(displaced_E * pmax(0, mte_s) * (base_sal_s/1000) * pv_factor)
  effic_gain_s <- sum(displaced_E * pmax(0, -mte_s) * (base_sal_s/1000) * pv_factor)
  inst_rev_loss_s <- sum(displaced_E * net_inst_rev_vec * tuition_mult_s)
  
  sim_results[s, 2:14] <- list(fiscal_s, behav_cost_s, effic_gain_s, fiscal_s - behav_cost_s + effic_gain_s, inst_rev_loss_s, (fiscal_s + effic_gain_s)/max(1, behav_cost_s), b0_s + b1_s/2 + b2_s/3 + b3_s/4, mean(mte_s[masters_vec==1]), mean(mte_s[masters_vec==0]), b0_s, b1_s, b2_s, b3_s)
  if(s %% 100 == 0) cat(paste(" Draw", s, "complete\n"))
}

# 10. Figures 11.1 - 11.7 --------------------------------------------------
cat("\nSection 10: Generating All Figures...\n")

# 11.1: Posterior Net Benefit
ggplot(sim_results, aes(x = net_benefit_s)) + geom_histogram(fill = "gray50", color = "black") + geom_vline(xintercept = 0, linetype = "dashed") + labs(title = "Fig 11.1: Posterior Net Social Benefits", x = "$000s") + theme_minimal()
ggsave(file.path(graphs_dir, "fig11_1_posterior_nb_R.png"), width = 8, height = 6)

# 11.2: MTE with Shaded Policy Region
b0_pm <- mean(sim_results$b0_s); b1_pm <- mean(sim_results$b1_s); b2_pm <- mean(sim_results$b2_s); b3_pm <- mean(sim_results$b3_s)
plot_df <- tibble(u = seq(0, 1, 0.01)) %>% mutate(mte = b0_pm + b1_pm*u + b2_pm*u^2 + b3_pm*u^3, region = u >= 0.30 & u <= 0.65, mte_pos = ifelse(region & mte > 0, mte, 0), mte_neg = ifelse(region & mte < 0, mte, 0))
ggplot(plot_df, aes(x = u)) + geom_area(aes(y = mte_pos), fill = "gray40") + geom_area(aes(y = mte_neg), fill = "gray80") + geom_line(aes(y = mte), size = 1) + labs(title = "Fig 11.2: MTE Curve & Policy Margin") + theme_minimal()
ggsave(file.path(graphs_dir, "fig11_2_mte_policy_R.png"), width = 8, height = 6)

# 11.3: CBA Decomposition
summary_stats <- sim_results %>% pivot_longer(cols = c(fiscal_s, behav_cost_s, effic_gain_s)) %>% group_by(name) %>% summarize(val = mean(value)) %>% mutate(val = ifelse(name == "behav_cost_s", -val, val))
ggplot(summary_stats, aes(x = name, y = val)) + geom_bar(stat = "identity", fill = "gray40") + labs(title = "Fig 11.3: CBA Decomposition") + theme_minimal()
ggsave(file.path(graphs_dir, "fig11_3_cba_decomp_R.png"), width = 8, height = 6)

# 11.4: Parameter Posteriors
sim_results %>% pivot_longer(cols = c(ate_s, att_s, atu_s)) %>% ggplot(aes(x = value, linetype = name)) + geom_density() + labs(title = "Fig 11.4: ATE, ATT, ATU Posteriors") + theme_minimal()
ggsave(file.path(graphs_dir, "fig11_4_param_posteriors_R.png"), width = 8, height = 6)

# 11.5: MTE by Field (Approximation)
plot_df_fields <- plot_df %>% mutate(STEM = mte + 0.134, Business = mte + 0.885, Education = mte - 0.165) %>% pivot_longer(cols = c(mte, STEM, Business, Education))
ggplot(plot_df_fields, aes(x = u, y = value, color = name)) + geom_line() + labs(title = "Fig 11.5: MTE by Field") + theme_minimal()
ggsave(file.path(graphs_dir, "fig11_5_mte_byfield_R.png"), width = 8, height = 6)

# 11.6 & 11.7: Institutional Impacts & Social Stack
inst_df <- df %>% mutate(prog = case_when(stem_major==1 ~ "STEM", bus_major==1 ~ "Business", ed_major==1 ~ "Education", TRUE ~ "Other"), inst_loss = (pnorm(loan_overage/50)*above_cap*masters)*net_inst_rev) %>% group_by(prog) %>% summarize(total_loss = sum(inst_loss))
ggplot(inst_df, aes(x = prog, y = total_loss)) + geom_bar(stat = "identity", fill = "gray40") + labs(title = "Fig 11.6: Institutional Revenue Loss") + theme_minimal()
ggsave(file.path(graphs_dir, "fig11_6_inst_rev_byfield_R.png"), width = 8, height = 6)

# 11.7: Full Social Cost Stack ----------------------------------------------
# Stacks all costs and savings to show the complete distributional picture.

# Extract posterior means for the stack
stack_data <- tibble(
  label = c("Fiscal Savings", "Efficiency Gain", "Human Capital Loss", 
            "Institutional Rev. Loss", "Cross-Subsidy Disruption"),
  value = c(
    mean(sim_results$fiscal_s),
    mean(sim_results$effic_gain_s),
    -mean(sim_results$behav_cost_s),
    -mean(sim_results$inst_rev_loss_s),
    -0.20 * mean(sim_results$inst_rev_loss_s) # Cross-subsidy is 20% of inst. loss
  ),
  category = c("Benefit", "Benefit", "Cost", "Cost", "Cost")
)

# Reorder factor for plotting (matches Stata sort_order)
stack_data$label <- factor(stack_data$label, levels = stack_data$label)

ggplot(stack_data, aes(x = label, y = value, fill = category)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("Benefit" = "gray70", "Cost" = "gray30")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Fig 11.7: Full Social Cost Stack",
    subtitle = "Student and Institutional Channels Combined (Posterior Mean)",
    x = "Policy Component",
    y = "Posterior Mean ($000s)",
    caption = "Negative values represent social costs; Positive values represent social benefits."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

ggsave(file.path(graphs_dir, "fig11_7_full_cost_stack_R.png"), width = 9, height = 7)

cat("\nChapter 11 R Script Complete.\n")
sink()