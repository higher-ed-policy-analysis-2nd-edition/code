#=========================================================================
# MTE_CATE_Plots13.R
# Section 13.7: Presenting IV, CATE, and MTE Results
# R translation of MTE_CATE_Plots13.do
#
# NOTE: matching the Stata original, almost this entire file is
# documented placeholder/sketch code pending Ch. 11/12 finalization --
# only the illustrative CATE forest plot (13.7.2, placeholder data) is
# actually executable in either language. Sections 13.7.1, 13.7.2a, and
# 13.7.3 are carried forward as NOTE-only stubs, exactly as they exist
# in MTE_CATE_Plots13.do, rather than fabricating results neither
# language has produced yet.
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
})

cat("MTE_CATE_Plots13.R running:", format(Sys.time()), "\n")

pubexport <- function(p, gname, w = 8, h = 5.5) {
  # NOTE: slightly shorter default height than the chapter's other
  # pubexport() copies (h = 5.8 elsewhere) -- intentional, not drift:
  # this section's illustrative CATE/MTE curves read cleanly at a
  # slightly flatter aspect. See Technical Appendix 13 for the
  # cross-script comparison of all pubexport() defaults used in this
  # chapter.
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

#=========================================================================
# 13.7.1 IV/2SLS: First-Stage Strength and Reduced-Form Visuals
#=========================================================================
# NOTE -- NEW CODE NEEDED (matches Stata original -- no executable code
# exists yet in either language). Pull from Ch. 11 (ivregress 2sls model
# on the B&B synthetic data). R equivalent would use AER::ivreg(), e.g.:
#
#   library(AER)
#   fit_iv <- ivreg(completion ~ treatment_var + controls |
#                      state_ga_funding + controls, data = ...)
#   summary(fit_iv, diagnostics = TRUE)  # first-stage F-stat via diagnostics
#
# AER is CRAN-standard and confirmed installable/available in this
# environment -- ready to implement once the Ch. 11 model is finalized.

#=========================================================================
# 13.7.2 CATE by Subgroup (margins-based)
#=========================================================================
# NOTE -- NEW CODE NEEDED (real version), matching Stata original. Pull
# from Ch. 11 CATE.do once available.
#
#-------------------------------------------------------------------------
# ILLUSTRATIVE DEMONSTRATION (placeholder data) -- mirrors the Stata
# original exactly: clearly-synthetic subgroup values so the figure runs
# today. Replace with actual per-subgroup CATE point estimates and SEs
# from Ch. 11's CATE.do the moment those are available. NOT a real
# finding.
#-------------------------------------------------------------------------
cate_illus <- data.frame(
  subgroup = c("First-Generation", "Non-First-Generation", "Low-Income",
               "Higher-Income", "STEM Majors", "Non-STEM Majors"),
  cate = c(0.062, 0.041, 0.078, 0.033, 0.028, 0.055),
  se   = c(0.021, 0.015, 0.024, 0.014, 0.018, 0.016)
) %>%
  mutate(
    lo = cate - 1.96 * se,
    hi = cate + 1.96 * se,
    # preserve intended display order (not alphabetical -- matches
    # Stata's explicit label-order construction, avoiding "encode"'s
    # alphabetizing behavior)
    subgroup = factor(subgroup, levels = rev(subgroup))
  )

fig13_15 <- ggplot(cate_illus, aes(x = cate, y = subgroup)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.7, color = "black") +
  labs(title = "Illustrative CATE by Subgroup (PLACEHOLDER DATA)",
       subtitle = "Replace with Ch. 11 CATE.do estimates -- not a real finding",
       x = "Conditional Average Treatment Effect", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
        plot.subtitle = element_text(hjust = 0.5, size = 9, face = "italic"))

pubexport(fig13_15, "fig13_15_cate_illus")

#=========================================================================
# 13.7.2a Official CATE Workflow (Stata 19 cate/categraph)
#=========================================================================
# NOTE -- NEW CODE NEEDED (matches Stata original). No direct R
# equivalent of Stata 19's native cate/categraph command exists; the
# closest CRAN-standard substitute is the `grf` (generalized random
# forests) package's causal_forest() + best_linear_projection(), or
# marginaleffects::avg_comparisons() with a flexible ML nuisance model.
# Both are legitimate substitutes but represent a different estimator
# family than Stata's cate command -- confirm the intended nuisance-model
# approach (lasso vs. random forest) against the Ch. 11 MTE model's own
# assumptions before implementing, exactly as the Stata original flags.

#=========================================================================
# 13.7.3 MTE and MPRTE Curves
#=========================================================================
# NOTE -- NEW CODE NEEDED (matches Stata original). Pull from Ch. 11
# MTE_MPRTE.do (mtefe output). R equivalent once available:
#
#   ggplot(mte_df, aes(x = u_support)) +
#     geom_ribbon(aes(ymin = mte_lo, ymax = mte_hi), fill = "gray80") +
#     geom_line(aes(y = mte_hat), color = "black") +
#     labs(x = "Unobserved Resistance to Treatment (u)",
#          y = "Marginal Treatment Effect")

#=========================================================================
# 13.7.4 Policy-Relevant Treatment Effect (PRTE) Summary Table
#=========================================================================
# NOTE -- table content lives in EstimationTables13.R; this file supplies
# the underlying PRTE point estimates from the Ch. 12 Grad PLUS cap
# application, once available (matches Stata original).

cat("MTE_CATE_Plots13.R completed:", format(Sys.time()), "\n")
cat("NOTE: Sections 13.7.1, 13.7.2a, 13.7.3 are documented stubs,\n")
cat("matching the Stata original's own incomplete state.\n")
