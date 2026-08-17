# ------------------------------------------------------------------------------
# pst_paths.R
#
# Single source of truth for every path the PST freshwater effort pipeline
# touches. The assembly script, the P2 module, and PST_FW_Effort.qmd all read
# from here so that a relocated file is a one-line change rather than a
# three-file hunt.
#
# Repo layout assumed:
#
#   CreelEstimates/
#   |-- R/
#   |   |-- pst_paths.R                  <- this file
#   |   |-- pst_p2_block_ratio.R
#   |   `-- integrate_manual_runs.R
#   |-- analysis/
#   |   |-- pst_fw_effort_assembly.R
#   |   |-- PST_FW_Effort.qmd            <- parent doc
#   |   |-- interview_proportions.qmd    <- child
#   |   |-- registry/
#   |   |   |-- pst_input_manifest.csv
#   |   |   `-- pst_river_block_crosswalk.csv
#   |   `-- outputs/
#   |-- PST_ANALYSIS.md
#   `-- .cache/
#
# here::here() resolves from the project root regardless of where a script is
# invoked or a document is rendered, so these are stable under both
# `source()` from the console and `quarto render` from analysis/.
# ------------------------------------------------------------------------------

library(here)

PST_PATHS <- list(

  # --- directories ---
  r_dir        = here::here("R_scripts"),
  analysis_dir = here::here("analysis"),
  registry_dir = here::here("analysis", "registry"),
  out_dir      = here::here("analysis", "outputs"),
  cache_dir    = here::here(".cache"),

  # --- registry inputs ---
  manifest  = here::here("analysis", "registry", "pst_input_manifest.csv"),
  crosswalk = here::here("analysis", "registry", "pst_river_block_crosswalk.csv"),

  # --- modules ---
  p2_module      = here::here("R_scripts", "pst_p2_block_ratio.R"),
  manual_runs_fn = here::here("R_scripts", "integrate_manual_runs.R")
)

# Output filenames, resolved against out_dir. Keeping these named rather than
# inline means the parent doc and the assembly script cannot drift apart on a
# filename typo.
PST_OUTPUTS <- list(
  deliverable      = "pst_fw_angler_trips_deliverable.csv",
  provenance       = "pst_fw_provenance_ledger.csv",
  gaps             = "pst_fw_gap_ledger.csv",
  coverage         = "pst_fw_coverage_ledger.csv",
  reconciliation   = "pst_fw_reconciliation_failures.csv",
  p2_ratios        = "pst_p2_block_ratios.csv",
  p2_donors        = "pst_p2_donors.csv",
  p2_loo_detail    = "pst_p2_loo_detail.csv",
  p2_loo_summary   = "pst_p2_loo_summary.csv"
)

#' Resolve a registered output name to a full path.
pst_out <- function(name) {
  if (!name %in% names(PST_OUTPUTS)) {
    stop(sprintf("Unknown output '%s'. Known: %s", name, paste(names(PST_OUTPUTS), collapse = ", ")))
  }
  file.path(PST_PATHS$out_dir, PST_OUTPUTS[[name]])
}

dir.create(PST_PATHS$out_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(PST_PATHS$cache_dir, recursive = TRUE, showWarnings = FALSE)
