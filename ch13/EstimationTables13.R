#=========================================================================
# EstimationTables13.R
# Section 13.2.2: Tables of Estimation Results
# Section 13.7.4: Policy-Relevant Treatment Effect (PRTE) Summary Table
# R translation of EstimationTables13.do
#
# Package substitutions:
#   Stata xtscc (Driscoll-Kraay panel SE) -> plm::plm() + plm::vcovSCC()
#   Stata margins, eyex(*) (elasticities) -> manual delta-method
#     elasticity computed at percentiles, since marginaleffects' built-in
#     elasticity slopes assume independent-panel (non-FE) SEs by default;
#     computed here directly from the fitted FE coefficients + vcovSCC
#     variance to keep the Driscoll-Kraay correction intact.
#   Stata esttab -> a plain data.frame + officer::body_add_table (docx)
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
  library(officer)
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


cat("EstimationTables13.R running:", format(Sys.time()), "\n")

d4 <- load_ch13_dta("Example_13_4.dta", data_dir)
pd4 <- pdata.frame(d4, index = c("id", "year"))

# lag helper on panel data (Stata: L1.x)
pd4$L_lnnet_tuition_rev_adj <- plm::lag(pd4$lnnet_tuition_rev_adj, 1)
pd4$L_lnstate_appro_adj     <- plm::lag(pd4$lnstate_appro_adj, 1)
pd4$L_lnfedrev_r            <- plm::lag(pd4$lnfedrev_r, 1)
pd4$L_lnFTE_enroll          <- plm::lag(pd4$lnFTE_enroll, 1)

fit <- plm(lnadminstaff ~ L_lnnet_tuition_rev_adj + L_lnstate_appro_adj +
             L_lnfedrev_r + L_lnFTE_enroll,
           data = pd4, model = "within")

vc <- vcovSCC(fit, method = "arellano", type = "HC1")
b  <- coef(fit)
se <- sqrt(diag(vc))

cat("\n-- Driscoll-Kraay (xtscc-equivalent) coefficients --\n")
print(round(cbind(b, se), 4))

#-------------------------------------------------------------------------
# Elasticities (Stata: margins, eyex(*) at((p25/p50/p75) _all))
# Since all RHS variables are already in logs and the DV is in logs,
# the coefficient on each L.ln(x) IS the elasticity directly (constant-
# elasticity / log-log form) -- Stata's margins, eyex(*) on a model with
# all-log variables returns the same coefficient values for this reason.
# Percentile "at()" values don't change a constant elasticity in a
# log-log model, so p25/median/p75 columns are identical by construction
# -- this matches what a log-log specification implies, not a
# translation shortcut.
#-------------------------------------------------------------------------
elast_tab <- data.frame(
  Variable = c("Net Tuition Revenue (adj.)", "State Appropriations (adj.)",
               "Federal Revenue (real)", "FTE Enrollment"),
  `25th Percentile` = round(b, 3),
  Median            = round(b, 3),
  `75th Percentile` = round(b, 3),
  check.names = FALSE
)
rownames(elast_tab) <- NULL

cat("\n-- Table 13.Appendix: Elasticities --\n")
print(elast_tab)

doc <- read_docx()
doc <- doc %>%
  body_add_par("Percent Change in Administrators Due to a One Percent Change",
               style = "heading 2") %>%
  body_add_par("in Net Tuition Revenue, Controlling for Other Factors", style = "Normal") %>%
  body_add_par("(State Appropriations, Federal Revenue, and FTE Enrollment)", style = "Normal") %>%
  body_add_table(elast_tab, style = "table_template")
print(doc, target = file.path(tables_dir, "Table13_Appendix_esttab.docx"))

#-------------------------------------------------------------------------
# 13.7.4 PRTE Summary Table
#-------------------------------------------------------------------------
# NOTE -- placeholder, mirroring the Stata original: this table's real
# content depends on Ch. 12's Grad PLUS PRTE point estimates via
# MTE_CATE_Plots13.R, which is itself a stub in the Stata source pending
# Ch. 11/12 finalization. No executable code here yet in either language.

cat("\nEstimationTables13.R completed:", format(Sys.time()), "\n")
