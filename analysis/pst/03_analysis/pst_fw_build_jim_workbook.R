# =============================================================================
# pst_fw_build_jim_workbook.R
# Location: analysis/pst/03_analysis/pst_fw_build_jim_workbook.R
#
# Builds PST_FW_Jim_Update.xlsx: a seven-tab status workbook so a non-R reader
# (Jim) can see what the PST freshwater effort pipeline currently supports
# without running anything. This is a STATUS UPDATE, not the Northern
# Economics deliverable - the filename and the cover tab both say so, and
# every gap/blocker visible in pst_fw_effort_assembly.R's console summary is
# visible here too, not smoothed over for presentation.
#
# MUST RUN AFTER pst_fw_effort_assembly.R. This script does not compute
# anything itself - it reads the CSVs that script writes to OUT_DIR and lays
# them out into tabs. If the CSVs are stale, this workbook is stale.
#
# Inputs (all from OUT_DIR = analysis/pst/outputs, written by
# pst_fw_effort_assembly.R):
#   pst_fw_trips_by_mode_location_INTERMEDIATE.csv  (effort_by_mode_location)
#   pst_fw_trips_by_crc_area_INTERMEDIATE.csv       (effort_by_area)
#   pst_fw_p2_area_ratios.csv                       (p2x$ratios)
#   pst_fw_p2_donors.csv                            (p2x$donors)
#   pst_fw_p2_loo_summary.csv                       (p2x$loo_summary)
#   pst_fw_gap_register.csv                         (gaps)
#   pst_fw_provenance_ledger.csv                    (provenance)
#   pst_fw_crc_vs_creel_bias.csv                    (crc_vs_creel)
#
# Output:
#   analysis/pst/outputs/PST_FW_Jim_Update.xlsx
#
# How to run:
#   Rscript analysis/pst/03_analysis/pst_fw_build_jim_workbook.R
#
# Missing upstream CSVs (interview props not run, P2 not wired, a prior
# assembly run that predates a given file, etc.) are handled the same way as
# pst_fw_effort_assembly.R handles them: log and skip or empty the affected
# tab, never fabricate one. [R2] A missing dimension stays coded "unknown",
# never zero, never imputed. [R3]
# =============================================================================

library(tidyverse)
library(here)
library(glue)
library(openxlsx)

OUT_DIR <- here("analysis", "pst", "outputs")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- 0. Defensive read helper ------------------------------------------------
# Same convention as pst_fw_effort_assembly.R's read_if(): a missing input is
# noted, never fatal, and the caller gets NULL back to branch on. [R2]

log_note <- function(source_id, detail) {
  message(glue("[note] {source_id}: {detail}"))
  invisible(NULL)
}

read_if <- function(path, source_id, detail = NULL, reader = readr::read_csv) {
  if (!file.exists(path)) {
    log_note(source_id, detail %||% glue(
      "not found at {path} - tab skipped or shown empty in the workbook. ",
      "Run pst_fw_effort_assembly.R first."
    ))
    return(NULL)
  }
  reader(path, show_col_types = FALSE)
}

# ---- 1. Load intermediate outputs --------------------------------------------

effort_by_mode_location <- read_if(
  file.path(OUT_DIR, "pst_fw_trips_by_mode_location_INTERMEDIATE.csv"),
  "effort_by_mode_location"
)
effort_by_area <- read_if(
  file.path(OUT_DIR, "pst_fw_trips_by_crc_area_INTERMEDIATE.csv"),
  "effort_by_area"
)
p2_ratios <- read_if(
  file.path(OUT_DIR, "pst_fw_p2_area_ratios.csv"), "p2_ratios",
  detail = "not found - P2 did not run in the assembly pass; P2 tabs omitted."
)
p2_donors <- read_if(file.path(OUT_DIR, "pst_fw_p2_donors.csv"), "p2_donors")
p2_loo_summary <- read_if(
  file.path(OUT_DIR, "pst_fw_p2_loo_summary.csv"), "p2_loo_summary",
  detail = paste(
    "not found - either P2 did not run, or no block-year had enough donor",
    "areas for the leave-one-out check. p2_median_error_pct will be absent."
  )
)
gaps <- read_if(file.path(OUT_DIR, "pst_fw_gap_register.csv"), "gaps")
provenance <- read_if(file.path(OUT_DIR, "pst_fw_provenance_ledger.csv"), "provenance")
crc_vs_creel <- read_if(file.path(OUT_DIR, "pst_fw_crc_vs_creel_bias.csv"), "crc_vs_creel")

if (is.null(effort_by_mode_location) || is.null(effort_by_area)) {
  log_note("workbook", paste(
    "the two roll-up tables are the core of this workbook and are missing -",
    "run pst_fw_effort_assembly.R before this script. Continuing so whatever",
    "IS available (gaps, provenance) still gets written."
  ))
}

# p2x is treated as "ran" iff pst_fw_p2_area_ratios.csv exists: the assembly
# script writes that file (possibly 0 rows) whenever run_p2_extrapolation()
# returned non-NULL, and omits it entirely otherwise. Mirrors the assembly
# script's `if (!is.null(p2x))` check exactly.
p2_ran <- !is.null(p2_ratios)

# =============================================================================
# ---- 2. Excel export for Jim's status update ---------------------------------
# =============================================================================
# One workbook, one tab per artifact, so a non-R reader can open it and see
# what the pipeline currently supports without running anything.
#
# Sheet order matches the story, not the code order: start with what changed
# (coverage before/after P2), then the two roll-ups, then the evidence for
# trusting P2 (ratios + leave-one-out), then the open items.

xlsx_path <- file.path(OUT_DIR, "PST_FW_Jim_Update.xlsx")

wb <- createWorkbook()

hdr_style <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2",
                         border = "Bottom", wrapText = TRUE)
title_style <- createStyle(textDecoration = "bold", fontSize = 13)
pct_style   <- createStyle(numFmt = "0.0%")
num_style   <- createStyle(numFmt = "#,##0")

#' Write one data frame to one tab with a title row, bold header, frozen
#' panes, and auto-sized columns. Centralized so every tab looks the same
#' without repeating the formatting calls seven times.
add_sheet <- function(wb, sheet_name, df, title = NULL, freeze = TRUE) {
  addWorksheet(wb, sheet_name)
  start_row <- 1
  if (!is.null(title)) {
    writeData(wb, sheet_name, title, startRow = 1, startCol = 1)
    addStyle(wb, sheet_name, title_style, rows = 1, cols = 1)
    start_row <- 3
  }
  writeData(wb, sheet_name, df, startRow = start_row, headerStyle = hdr_style)
  if (freeze) freezePane(wb, sheet_name, firstActiveRow = start_row + 1)
  setColWidths(wb, sheet_name, cols = seq_along(df), widths = "auto")
  invisible(NULL)
}

## 2a. Cover / coverage summary -----------------------------------------------
# The single table that answers "what changed": rows and trips by block/year,
# split by whether P2 contributed, plus the P2 confidence figure alongside it
# so coverage and trust are read together, not from two different tabs.

coverage_summary <- NULL
if (!is.null(effort_by_mode_location)) {
  coverage_summary <- effort_by_mode_location |>
    group_by(block, year) |>
    summarise(
      river_years   = n_distinct(river_label),
      angler_trips  = round(sum(angler_trips, na.rm = TRUE)),
      pct_from_p2   = round(100 * sum(angler_trips[grepl("P2", tier)], na.rm = TRUE) /
                             sum(angler_trips, na.rm = TRUE), 1),
      pct_mode_known = round(100 * sum(angler_trips[mode != "unknown"], na.rm = TRUE) /
                              sum(angler_trips, na.rm = TRUE), 1),
      .groups = "drop"
    ) |>
    arrange(block, year)

  if (!is.null(p2_loo_summary) && nrow(p2_loo_summary) > 0) {
    coverage_summary <- coverage_summary |>
      left_join(p2_loo_summary |> select(block, p2_median_error_pct = median_ape),
                by = "block") |>
      mutate(p2_median_error_pct = round(p2_median_error_pct, 1))
  }

  add_sheet(wb, "Summary", coverage_summary,
           title = glue("PST Freshwater Effort — Status Update — {format(Sys.Date(), '%Y-%m-%d')}"))
} else {
  log_note("xlsx_export", "effort_by_mode_location missing - Summary tab omitted.")
}

## 2b. Deliverable shape (river x year x mode x location) ---------------------

if (!is.null(effort_by_mode_location)) {
  add_sheet(wb, "Deliverable (by River)", effort_by_mode_location,
           title = "Year x River x Mode x Location — angler trips (INTERMEDIATE, not final)")
}

## 2c. CRC-area grain -----------------------------------------------------------

if (!is.null(effort_by_area)) {
  add_sheet(wb, "By CRC Area", effort_by_area,
           title = "Same data at CRC catch_area_code grain (audit / CRC join key)")
}

## 2d. P2 evidence: ratios + leave-one-out -------------------------------------

if (p2_ran) {
  if (!is.null(p2_ratios) && nrow(p2_ratios) > 0) {
    add_sheet(wb, "P2 Ratios", p2_ratios,
             title = "Block-year expansion ratio (creel trips / CRC salmon) — CRC-denominated")
  }
  if (!is.null(p2_loo_summary) && nrow(p2_loo_summary) > 0) {
    add_sheet(wb, "P2 Validation (LOO)", p2_loo_summary,
             title = "Leave-one-out: predict each surveyed area from the rest — median_ape is the number to quote")
  }
  if (!is.null(p2_donors) && nrow(p2_donors) > 0) {
    add_sheet(wb, "P2 Donor Areas", p2_donors,
             title = "Areas with BOTH a creel survey and CRC harvest — the evidence base P2 is built from")
  }
} else {
  log_note("xlsx_export", "P2 did not run in the assembly pass; P2 tabs omitted from the workbook.")
}

## 2e. CRC vs. creel bias -------------------------------------------------------

if (!is.null(crc_vs_creel) && nrow(crc_vs_creel) > 0) {
  add_sheet(wb, "CRC vs Creel Bias", crc_vs_creel,
           title = "Same area-year, two harvest sources — P1 only. Median CRC/creel ~0.77 (see Summary)")
}

## 2f. Open items ---------------------------------------------------------------
# Blockers first: these are the rows Jim needs to see without scrolling.

if (!is.null(gaps) && nrow(gaps) > 0) {
  gaps_ordered <- gaps |>
    mutate(severity = factor(severity, levels = c("blocker", "gap", "defect", "note"))) |>
    arrange(severity, source_id)
  add_sheet(wb, "Gap Register", gaps_ordered,
           title = "Blockers first, then gaps, defects, notes — full detail behind the Summary tab")
}

## 2g. Provenance (collapsed, not the full row-level ledger) -------------------
# The row-level ledger is large and not useful in Excel; a block/year/tier/
# source rollup is. Full detail stays in pst_fw_provenance_ledger.csv for
# anyone who needs to trace a specific number.

if (!is.null(provenance)) {
  provenance_summary <- provenance |>
    group_by(block, year, tier, source_id) |>
    summarise(n_rows = sum(n_rows), .groups = "drop") |>
    arrange(block, year, tier)

  add_sheet(wb, "Provenance (rollup)", provenance_summary,
           title = "Where each block/year/tier's numbers come from — full row-level detail in the CSV export")
} else {
  log_note("xlsx_export", "provenance ledger missing - Provenance tab omitted.")
}

## Apply percent formatting where relevant, then save --------------------------

if (!is.null(coverage_summary)) {
  pct_cols <- c("pct_from_p2", "pct_mode_known", "p2_median_error_pct")
  present  <- intersect(pct_cols, names(coverage_summary))
  if (length(present) > 0) {
    addStyle(wb, "Summary", pct_style,
            rows = 4:(3 + nrow(coverage_summary)),
            cols = which(names(coverage_summary) %in% present),
            gridExpand = TRUE, stack = TRUE)
  }
}

saveWorkbook(wb, xlsx_path, overwrite = TRUE)

cli_ok <- tryCatch({ cli::cli_alert_success(glue("Wrote status workbook: {xlsx_path}")); TRUE },
                   error = function(e) FALSE)
if (!cli_ok) message(glue("Wrote status workbook: {xlsx_path}"))
