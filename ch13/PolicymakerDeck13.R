#=========================================================================
# PolicymakerDeck13.R
# Supplementary: Example PowerPoint Deck for Policymaker Audiences
#
# Called by:  R_code13.R (end of master run, after all figures exist)
# Reads from: fig13_registry (live ggplot objects, keyed by canonical
#             export name) populated by each sub-script's pubexport()
#             helper during this same run.
#
# This is the R-native PowerPoint export path discussed alongside the
# chapter: unlike Stata (which has no putpptx command and would need an
# external Node.js/pptxgenjs dependency), R's officer package writes
# .pptx files natively, and the rvg package embeds ggplot2 figures as
# fully editable vector graphics (colors/fonts/labels editable directly
# in PowerPoint) via officer::ph_with(rvg::dml(ggobj = p), ...).
#
# FIGURE SELECTION (assumption -- change the `deck_figs` list below to
# use different figures): mirrors the five-slide arc validated earlier
# in the chat -- descriptive, associational, causal, uncertainty, and
# bottom-line -- but now pulling from this run's own real, validated
# Chapter 13 figures rather than a placeholder set.
#
# EDITABLE VS. STATIC: if the `rvg` package is installed, figures are
# embedded as editable vector shapes (dml()). If not, this falls back
# to static PNG embedding (officer::external_img()) so the script still
# runs -- rvg is CRAN-only and could not be installed/tested in the
# validation sandbox used to build this chapter's R translation, so the
# editable-vector path should be spot-checked once run in an environment
# with full CRAN access (which the author's machine has).
#=========================================================================

if (!exists("root_dir")) {
  if (Sys.getenv("USERNAME") == "marvi") {
    root_dir <- "C:/Users/marvi/Dropbox/Book/2nd Edition/Chapter 13"
    data_dir <- file.path(root_dir, "Data", "Stata")
  } else {
    root_dir <- getwd()
    data_dir <- file.path(root_dir, "Data")
  }
  graphs_dir <- file.path(root_dir, "Output", "figures")
  tables_dir <- file.path(root_dir, "Output", "tables")
  logdir     <- file.path(root_dir, "Output", "logs")
}

suppressPackageStartupMessages({
  library(officer)
  library(ggplot2)
})

has_rvg <- requireNamespace("rvg", quietly = TRUE)
if (!has_rvg) {
  cat("PolicymakerDeck13.R: 'rvg' not found -- falling back to static PNG embedding.\n")
  cat("To enable editable vector figures, in a FRESH R session run:\n")
  cat("  install.packages(c('systemfonts', 'gdtools', 'rvg'), type = 'binary')\n")
} else {
  library(rvg)
  cat("PolicymakerDeck13.R: rvg found -- embedding figures as editable vector graphics.\n")
}

cat("PolicymakerDeck13.R running:", format(Sys.time()), "\n")

if (!exists("fig13_registry") || length(ls(fig13_registry)) == 0) {
  stop("fig13_registry is empty -- run the full R_code13.R chapter pipeline ",
       "before sourcing PolicymakerDeck13.R, so the figures it needs actually exist.")
}

#-------------------------------------------------------------------------
# Figure selection for the deck (see header note -- edit this list to
# change which figures are included)
#-------------------------------------------------------------------------
deck_figs <- list(
  list(gname = "fig13_2_pctchange_appro_fte",
       kicker = "Descriptive Statistics -- What's Happening",
       title  = "Where state support has grown -- and where it hasn't",
       lesson = "A map earns its place by making geographic variation legible at a glance."),
  list(gname = "fig13_7_ols_coefplot",
       kicker = "Associational Regression -- Not Yet Causal",
       title  = "How tuition revenue moves with its main drivers",
       lesson = "Coefficient plots let an audience see the estimate and its uncertainty at once."),
  list(gname = "fig13_12_twfe_event_study",
       kicker = "Causal Inference -- What the Policy Actually Did",
       title  = "Did consolidation change spending behavior?",
       lesson = "A flat pre-trend is what makes a causal claim credible -- and visible."),
  list(gname = "fig13_18_posterior_density",
       kicker = "Bayesian Uncertainty",
       title  = "How confident are we in the projected impact?",
       lesson = "A credible interval is a probability statement about the parameter itself."),
  list(gname = "fig13_19_cba_waterfall",
       kicker = "Bottom Line -- Cost-Benefit Summary",
       title  = "What the policy would cost -- and save",
       lesson = "A decision-ready deck closes on one number the audience can act on.")
)

missing_figs <- setdiff(sapply(deck_figs, `[[`, "gname"), ls(fig13_registry))
if (length(missing_figs) > 0) {
  stop("These figures are not in fig13_registry (check spelling or whether that ",
       "sub-script ran): ", paste(missing_figs, collapse = ", "))
}

#-------------------------------------------------------------------------
# Deck assembly
#-------------------------------------------------------------------------
NAVY <- "#1E2761"

doc <- read_pptx()

# Title slide
doc <- doc %>%
  add_slide(layout = "Title Slide", master = "Office Theme") %>%
  ph_with(value = "Presenting Analyses to Policymakers",
          location = ph_location_type(type = "ctrTitle")) %>%
  ph_with(value = "Example figures from Chapter 13 -- Higher Education Policy Analysis Using Quantitative Techniques, 2nd Ed.",
          location = ph_location_type(type = "subTitle"))

# Figure-slide fixed geometry (inches) -- overrides the "Title Only" master
# placeholder's own position/size, which in the default Office Theme sits
# too low and too large for a slide that's mostly figure. Explicit fpar/
# ftext/fp_text formatting is used instead of a bare string so font size
# and color are controlled directly rather than inherited from the master
# placeholder (that inheritance is what made the title look oversized and
# too low in the first exported deck).
TITLE_TOP    <- 0.25
TITLE_HEIGHT <- 0.7
TITLE_SIZE   <- 24

KICKER_TOP    <- 6.35
KICKER_HEIGHT <- 0.5
KICKER_SIZE   <- 13

add_figure_slide <- function(doc, fig_spec) {
  p <- get(fig_spec$gname, envir = fig13_registry)

  title_fpar <- fpar(ftext(fig_spec$title,
                            fp_text(font.size = TITLE_SIZE, bold = TRUE,
                                    color = NAVY)))

  doc <- doc %>%
    add_slide(layout = "Title Only", master = "Office Theme") %>%
    ph_with(value = title_fpar,
            location = ph_location(left = 0.5, top = TITLE_TOP,
                                    width = 9, height = TITLE_HEIGHT))

  FIG_TOP <- TITLE_TOP + TITLE_HEIGHT + 0.15  # 1.1" -- right under the title

  if (has_rvg) {
    doc <- doc %>%
      ph_with(value = dml(ggobj = p),
              location = ph_location(left = 1, top = FIG_TOP, width = 8, height = 5.1))
  } else {
    png_path <- file.path(graphs_dir, paste0(fig_spec$gname, "_R.png"))
    if (!file.exists(png_path)) {
      stop("Static PNG not found for ", fig_spec$gname, " at ", png_path,
           " -- was this figure's sub-script actually run in this session?")
    }
    doc <- doc %>%
      ph_with(value = external_img(png_path, width = 8, height = 5.1),
              location = ph_location(left = 1, top = FIG_TOP, width = 8, height = 5.1))
  }

  kicker_fpar <- fpar(ftext(paste0(fig_spec$kicker, "  |  ", fig_spec$lesson),
                            fp_text(font.size = KICKER_SIZE, italic = TRUE,
                                    color = "#595959")))

  doc <- doc %>%
    ph_with(value = kicker_fpar,
            location = ph_location(left = 0.5, top = KICKER_TOP,
                                    width = 9, height = KICKER_HEIGHT))

  doc
}

for (fig_spec in deck_figs) {
  doc <- add_figure_slide(doc, fig_spec)
}

deck_path <- file.path(root_dir, "Output", "Chapter13_Policymaker_Deck_R.pptx")
print(doc, target = deck_path)

cat("\nPolicymakerDeck13.R completed:", format(Sys.time()), "\n")
cat("Deck written to:", deck_path, "\n")
cat("Figures embedded as:", if (has_rvg) "editable vector graphics (rvg::dml)" else "static PNG images", "\n")
