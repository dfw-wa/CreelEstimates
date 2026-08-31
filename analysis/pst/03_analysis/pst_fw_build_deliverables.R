# =============================================================================
# pst_fw_build_deliverables.R
# Location: analysis/pst/03_analysis/pst_fw_build_deliverables.R
#
# Builds two Excel workbooks from the assembly script's outputs:
#
#   PST_FW_Status_Report.xlsx - a multi-tab internal status workbook so a
#     non-R reader can see what the PST freshwater effort pipeline
#     currently supports without running anything. Every gap/blocker visible
#     in pst_fw_angler_trips_assembly.R's console summary is visible here
#     too, not smoothed over for presentation.
#
#   PST_FW_Deliverable.xlsx - the simplified, consultant-facing export:
#     Year x River x Mode x Location x Angler Trips, exactly the format
#     Northern Economics requested (see _01_scope_and_contract.qmd). No
#     audit columns, no gap register, no P2 diagnostics - those stay in the
#     status workbook. This is NOT a claim that the numbers are final; it's
#     the current best estimate in the requested shape, regenerated fresh
#     every run from whatever pst_fw_angler_trips_assembly.R currently
#     supports.
#
# MUST RUN AFTER pst_fw_angler_trips_assembly.R. This script does not compute
# anything itself - it reads the CSVs that script writes to IN_DIR and lays
# them out into tabs. If the CSVs are stale, both workbooks are stale.
#
# Inputs (all from IN_DIR = analysis/pst/outputs/05_assembly, written by
# pst_fw_angler_trips_assembly.R):
#   pst_fw_trips_by_mode_location.csv               (effort_by_mode_location)
#   pst_fw_trips_by_crc_area.csv                    (effort_by_area)
#   pst_fw_p2_area_ratios.csv                       (p2x$ratios)
#   pst_fw_p2_donors.csv                            (p2x$donors)
#   pst_fw_p2_loo_summary.csv                       (p2x$loo_summary)
#   pst_fw_gap_register.csv                         (gaps)
#   pst_fw_provenance_ledger.csv                    (provenance)
#   pst_fw_crc_vs_creel_bias.csv                    (crc_vs_creel)
#
# Outputs:
#   analysis/pst/outputs/deliverables/PST_FW_Status_Report.xlsx
#   analysis/pst/outputs/deliverables/PST_FW_Deliverable.xlsx
#
# How to run:
#   Rscript analysis/pst/03_analysis/pst_fw_build_deliverables.R
#
# Missing upstream CSVs (interview props not run, P2 not wired, a prior
# assembly run that predates a given file, etc.) are handled the same way as
# pst_fw_angler_trips_assembly.R handles them: log and skip or empty the affected
# tab, never fabricate one. [R2] A missing dimension stays coded "unknown",
# never zero, never imputed. [R3]
# =============================================================================

library(tidyverse)
library(here)
library(glue)
library(openxlsx)

IN_DIR <- here("analysis", "pst", "outputs", "05_assembly")
DELIVERABLES_DIR <- here("analysis", "pst", "outputs", "deliverables")
dir.create(DELIVERABLES_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- 0. Defensive read helper ------------------------------------------------
# Same convention as pst_fw_angler_trips_assembly.R's read_if(): a missing input is
# noted, never fatal, and the caller gets NULL back to branch on. [R2]

log_note <- function(source_id, detail) {
  message(glue("[note] {source_id}: {detail}"))
  invisible(NULL)
}

read_if <- function(path, source_id, detail = NULL, reader = readr::read_csv) {
  if (!file.exists(path)) {
    log_note(source_id, detail %||% glue(
      "not found at {path} - tab skipped or shown empty in the workbook. ",
      "Run pst_fw_angler_trips_assembly.R first."
    ))
    return(NULL)
  }
  reader(path, show_col_types = FALSE)
}

# ---- 1. Load intermediate outputs --------------------------------------------

effort_by_mode_location <- read_if(
  file.path(IN_DIR, "pst_fw_trips_by_mode_location.csv"),
  "effort_by_mode_location"
)
effort_by_area <- read_if(
  file.path(IN_DIR, "pst_fw_trips_by_crc_area.csv"),
  "effort_by_area"
)
p2_ratios <- read_if(
  file.path(IN_DIR, "pst_fw_p2_area_ratios.csv"), "p2_ratios",
  detail = "not found - P2 did not run in the assembly pass; P2 tabs omitted."
)
p2_donors <- read_if(file.path(IN_DIR, "pst_fw_p2_donors.csv"), "p2_donors")
p2_loo_summary <- read_if(
  file.path(IN_DIR, "pst_fw_p2_loo_summary.csv"), "p2_loo_summary",
  detail = paste(
    "not found - either P2 did not run, or no block-year had enough donor",
    "areas for the leave-one-out check. p2_median_error_pct will be absent."
  )
)
gaps <- read_if(file.path(IN_DIR, "pst_fw_gap_register.csv"), "gaps")
provenance <- read_if(file.path(IN_DIR, "pst_fw_provenance_ledger.csv"), "provenance")
crc_vs_creel <- read_if(file.path(IN_DIR, "pst_fw_crc_vs_creel_bias.csv"), "crc_vs_creel")

# Official CRC catch-area code -> description, used only to label the
# individual member areas behind a composite River row (section 3 below).
# Not an assembly output -- lives in input_files, not IN_DIR.
crc_area_lut <- read_if(
  here("input_files", "pst", "lookup_tables", "crc_area_lut.csv"), "crc_area_lut",
  detail = "not found - composite-river CRC area descriptions omitted from the deliverable workbook."
)

if (is.null(effort_by_mode_location) || is.null(effort_by_area)) {
  log_note("workbook", paste(
    "the two roll-up tables are the core of this workbook and are missing -",
    "run pst_fw_angler_trips_assembly.R before this script. Continuing so whatever",
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

xlsx_path <- file.path(DELIVERABLES_DIR, "PST_FW_Status_Report.xlsx")

wb <- createWorkbook()

hdr_style <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2",
                         border = "Bottom", wrapText = TRUE)
title_style <- createStyle(textDecoration = "bold", fontSize = 13)
pct_style   <- createStyle(numFmt = "0.0%")
num_style   <- createStyle(numFmt = "#,##0")

#' Character width of one data column, ignoring the header (it wraps via
#' hdr_style, so it never needs to dictate column width) and anything
#' outside df entirely (notably the title row - see add_sheet below).
data_col_width <- function(x) {
  x_chr <- if (is.numeric(x)) format(x, big.mark = ",", trim = TRUE) else as.character(x)
  max(c(4, nchar(x_chr, type = "chars")), na.rm = TRUE) + 2
}

#' Write one data frame to one tab with a title row, bold header, frozen
#' panes, and data-sized columns. Centralized so every tab looks the same
#' without repeating the formatting calls seven times.
add_sheet <- function(wb, sheet_name, df, title = NULL, freeze = TRUE) {
  addWorksheet(wb, sheet_name)
  start_row <- if (!is.null(title)) 3 else 1
  writeData(wb, sheet_name, df, startRow = start_row, headerStyle = hdr_style)
  if (freeze) freezePane(wb, sheet_name, firstActiveRow = start_row + 1)
  # Explicit widths from df's own content, NOT setColWidths(widths = "auto"):
  # openxlsx's "auto" is resolved at saveWorkbook() time by scanning the
  # sheet's FINAL state, so it sees the title text in column A's row 1
  # regardless of write order and stretches that column to the title's full
  # length. Computing widths from df directly sidesteps that entirely - the
  # title (written below, after this) never enters the calculation.
  setColWidths(wb, sheet_name, cols = seq_along(df),
              widths = vapply(df, data_col_width, numeric(1)))
  if (!is.null(title)) {
    writeData(wb, sheet_name, title, startRow = 1, startCol = 1)
    addStyle(wb, sheet_name, title_style, rows = 1, cols = 1)
  }
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

## 2b. Trips by river, with audit columns (river x year x mode x location) ----
# Same grain as the deliverable export below, plus the harvest/tier/source_id/
# method columns the consultant didn't ask for - kept here for anyone tracing
# a specific number back to its source. See PST_FW_Deliverable.xlsx (built in
# section 3 below) for the simplified consultant-facing shape.

if (!is.null(effort_by_mode_location)) {
  add_sheet(wb, "Trips by River (detail)", effort_by_mode_location,
           title = "Year x River x Mode x Location — angler trips, with harvest/tier/source audit columns")
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

# =============================================================================
# ---- 3. Excel export for the consultant-facing deliverable ------------------
# =============================================================================
# The actual Northern Economics request (_01_scope_and_contract.qmd):
# Year x River x Mode x Location x Angler trips, 2022-2025, total salmon,
# nothing else. One tab, no audit columns - no harvest, tier, source_id,
# method, and none of the gap/P2/provenance tabs above. Those stay in
# PST_FW_Status_Report.xlsx.
#
# One addition beyond the base request: "Composite Estimate" + "CRC Areas"
# columns, and a second lookup tab. Several River rows (e.g. Quillayute,
# Chehalis, Puyallup Carbon) are not single CRC catch areas - they're a sum
# across 2+ areas the crosswalk groups under one river name (see
# pst_river_block_crosswalk.csv's crc_areas column, or - for a genuinely
# ambiguous single area straddling two names - expand_crosswalk_areas()'s
# " + " merge in pst_p2_block_ratio.R). A consultant reading a River total
# has no way to tell a single-area number from a multi-area sum without this;
# both cases already carry every member catch_area_code in
# effort_by_mode_location$catch_area_codes, so exposing it costs nothing new
# to compute. mode/location values are shown Title Case for a non-R reader.
# "Unknown" (attribution failed) and "Combined" (source never split bank/
# boat) are kept as-is rather than hidden or dropped - per [R3], a missing
# dimension is never fabricated, so the consultant sees the same completeness
# picture the status workbook does, just without the internal audit trail
# explaining it.
#
# Zero-trip rows are dropped (Evan's call, 2026-08-31), UNLIKE the status
# workbook's detail tabs above. Several late-stage corrections in this
# pipeline zero a row rather than delete it, specifically so the ORIGINAL
# (now-corrected) number stays visible in the audit trail: a distrusted P1
# survey superseded by a P2 regional estimate (pst_p1_distrust_overrides.csv),
# a closed/out-of-scope area (pst_closed_areas_lookup.csv), a season-status
# closure (pst_season_status_lookup.csv). All of that machinery is exactly
# why a River can show a real, sensible number in this tab (e.g. Nooksack
# River (below North Fork) 2023 = 4,844 trips) while the SAME row's Bank/
# Boat strata sit at a leftover 0 next to it - confirmed the fix DID take
# effect when the leftover zero rows read as if it hadn't. A zero conveys
# nothing to the consultant on its own; the "why" behind it belongs in
# PST_FW_Status_Report.xlsx's method/gap-register columns, not repeated here.

deliverable_path <- file.path(DELIVERABLES_DIR, "PST_FW_Deliverable.xlsx")

deliverable_trips <- NULL
if (!is.null(effort_by_mode_location)) {
  deliverable_trips <- effort_by_mode_location |>
    filter(angler_trips > 0) |>
    transmute(
      `CRC Region`       = block,
      Year               = year,
      River              = river_label,
      Mode               = str_to_title(mode),
      Location           = str_to_title(location),
      `Angler Trips`     = angler_trips,
      catch_area_codes,
      `Composite Estimate` = if_else(
        str_detect(coalesce(catch_area_codes, ""), "\\|"), "Yes", "No"
      ),
      `CRC Areas`        = catch_area_codes
    ) |>
    select(-catch_area_codes) |>
    arrange(`CRC Region`, River, Year, Location, Mode)
}

# Second tab: every River's member CRC area(s) with the official area
# description, so a River row made of more than one CRC area is traceable
# without leaving the workbook. Membership is the UNION of catch_area_codes
# across ALL of that river's rows (every year/mode/location), not a per-row
# copy - a single area-year (e.g. Chehalis before 2023, when only one of its
# two areas had data yet) would otherwise show incomplete membership for a
# river that has more area coverage in other years. The main tab's own
# per-row "CRC Areas" column already shows which areas actually fed THAT
# row; this tab answers the separate question of what the river is
# structurally made of. Included for single-area rivers too (one row each)
# rather than filtered to multi-area rivers only - a single reference tab
# covering every River is easier to use than one that only sometimes has an
# entry.
river_crc_lookup <- NULL
if (!is.null(deliverable_trips)) {
  river_membership <- deliverable_trips |>
    filter(!is.na(`CRC Areas`), `CRC Areas` != "") |>
    group_by(River) |>
    summarise(
      codes = paste(sort(unique(unlist(strsplit(`CRC Areas`, "\\|")))),
                    collapse = "|"),
      .groups = "drop"
    )

  river_crc_lookup <- river_membership |>
    separate_longer_delim(codes, delim = "|") |>
    rename(`CRC Area Code` = codes) |>
    distinct()

  if (!is.null(crc_area_lut)) {
    # A few codes (559, 566, 618) carry two distinct non-obsolete
    # descriptions in the LUT (e.g. 618 = both "Drano Lake" and "Little
    # White Salmon River"). Collapsed to one row per code so the join below
    # stays one-to-one; both names are kept, joined, rather than picking one
    # arbitrarily.
    lut_join <- crc_area_lut |>
      transmute(`CRC Area Code` = as.character(catch_area_code),
               catch_area_description) |>
      distinct() |>
      group_by(`CRC Area Code`) |>
      summarise(`CRC Area Description` = paste(sort(unique(catch_area_description)),
                                                collapse = " / "),
                .groups = "drop")
    river_crc_lookup <- river_crc_lookup |>
      mutate(`CRC Area Code` = as.character(`CRC Area Code`)) |>
      left_join(lut_join, by = "CRC Area Code") |>
      # A code absent from crc_area_lut.csv (e.g. 547, 764) would otherwise
      # leave this column blank - the River name is the best available
      # stand-in, not a fabricated value: [R3] concerns a missing DIMENSION,
      # not a missing lookup-table row for a dimension the River column
      # already answers.
      mutate(`CRC Area Description` = coalesce(`CRC Area Description`, River))
  }

  river_crc_lookup <- river_crc_lookup |>
    arrange(River, `CRC Area Code`)
}

if (!is.null(deliverable_trips)) {
  wb_deliverable <- createWorkbook()
  add_sheet(wb_deliverable, "Angler Trips", deliverable_trips,
           title = paste(
             "PST Freshwater Recreational Angler Trips",
             "— Year x River x Mode x Location, 2022–2025",
             "(total salmon)"
           ))
  addStyle(wb_deliverable, "Angler Trips", num_style,
          rows = 4:(3 + nrow(deliverable_trips)),
          cols = which(names(deliverable_trips) == "Angler Trips"),
          gridExpand = TRUE, stack = TRUE)

  if (!is.null(river_crc_lookup) && nrow(river_crc_lookup) > 0) {
    add_sheet(wb_deliverable, "River CRC Area Lookup", river_crc_lookup,
             title = "Member CRC catch area(s) behind each River row")
  }

  saveWorkbook(wb_deliverable, deliverable_path, overwrite = TRUE)

  cli_ok2 <- tryCatch({
    cli::cli_alert_success(glue("Wrote deliverable workbook: {deliverable_path}"))
    TRUE
  }, error = function(e) FALSE)
  if (!cli_ok2) message(glue("Wrote deliverable workbook: {deliverable_path}"))
} else {
  log_note("deliverable_export", paste(
    "effort_by_mode_location missing - PST_FW_Deliverable.xlsx not written.",
    "Run pst_fw_angler_trips_assembly.R first."
  ))
}
