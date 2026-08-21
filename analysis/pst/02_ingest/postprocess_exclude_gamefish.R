# ==============================================================================
# postprocess_exclude_gamefish.R
#
# Purpose:
#   Retroactively apply the winter/summer gamefish exclusion (see
#   multi_fishery_creel_summary.R sections 1c and the final-output filter) to
#   an existing multi_fishery_creel_summary.R run, without re-running the
#   full pipeline. Reloads the five saved .rds outputs, drops any
#   gamefish-primary fishery_name rows, and overwrites both the .rds and
#   .csv files in place.
#
# Usage:
#   Rscript analysis/pst/02_ingest/postprocess_exclude_gamefish.R
# ==============================================================================

library(dplyr)
library(stringr)
library(readr)
library(here)
library(cli)

out_dir <- here("analysis", "pst", "outputs")

gamefish_filter <- stringr::regex("(winter|summer) gamefish", ignore_case = TRUE)

targets <- tibble::tribble(
  ~object_name,       ~file_stem,
  "trips_combined",   "multi_fishery_creel_trips",
  "harvest_combined", "multi_fishery_creel_harvest",
  "qa_combined",      "multi_fishery_creel_qa",
  "run_ledger",        "multi_fishery_creel_run_ledger",
  "period_comparison", "multi_fishery_creel_week_vs_month"
)

for (i in seq_len(nrow(targets))) {

  file_stem <- targets$file_stem[i]
  rds_path  <- file.path(out_dir, paste0(file_stem, ".rds"))
  csv_path  <- file.path(out_dir, paste0(file_stem, ".csv"))

  if (!file.exists(rds_path)) {
    cli::cli_alert_warning("Skipping {.val {file_stem}}: {.path {rds_path}} not found.")
    next
  }

  dat <- readRDS(rds_path)

  n_before   <- nrow(dat)
  gamefish   <- stringr::str_detect(dat$fishery_name, gamefish_filter)
  n_dropped  <- sum(gamefish)

  if (n_dropped > 0) {
    cli::cli_alert_info(
      "{.val {file_stem}}: dropping {n_dropped} of {n_before} row{?s} \\
       (fishery/fisheries: {.val {sort(unique(dat$fishery_name[gamefish]))}})."
    )
  }

  dat <- dat[!gamefish, , drop = FALSE]

  saveRDS(dat, rds_path)
  readr::write_csv(dat, csv_path)

  cli::cli_alert_success(
    "{.val {file_stem}}: {nrow(dat)} row{?s} remain (was {n_before})."
  )
}

cli::cli_alert_success("Gamefish exclusion re-applied to all saved outputs.")
