# ==============================================================================
# mid_columbia_yakima_creel_ingestion.R
#
# Purpose:
#   Parse ad hoc weekly creel-summary workbooks provided by Todd Miller (WDFW
#   R-district creel staff) for mid-Columbia mainstem and Yakima River
#   fisheries, and reshape them into a table matching the schema of
#   multi_fishery_trip_summary.csv so the two sources can be row-bound for the
#   freshwater PST effort deliverable.
#
# Usage:
#   Source interactively or run with Rscript from the repo root:
#     Rscript analysis/mid_columbia_yakima_creel_ingestion.R
#   Requires no DB access — reads xlsx files from input_files/mid-Columbia/.
#
#   Output: analysis/outputs/mid_columbia_yakima_creel_summary.csv
#           (A downstream step row-binds this with multi_fishery_trip_summary.csv.)
#
# Source files used (in input_files/mid-Columbia/):
#   Hanford Reach (2022–2025):  "20YY Hanford Reach Boat Harvest Model.xlsx"
#     Sheet used:  "Summary" (clean weekly table, rows detected by datetime in
#                  col 0 after a multi-row merged header block).
#     Sheets ignored:
#       "Summary Print"  — print-formatted duplicate of "Summary"; redundant.
#       "Bank"           — sampled daily counts only; no expansion factor
#                          visible in this workbook (see Open Question 2 below).
#                          Parsing is stubbed in ingest_hanford_bank().
#       "Boats"/"Factors"— trailer-count model underlying the expansion;
#                          not needed because Summary already has expanded totals.
#       "Steelhead"      — steelhead excluded from PST scope entirely.
#       "Sheet1"         — season totals 2012–2022; useful cross-check only.
#       "Sheet2"         — extra data; not used.
#       "Harvest"        — harvest detail; not needed for trip/effort schema.
#       "Other"          — not relevant.
#
#   Yakima (2022):  "2022 Yakima Fall Fishery Summary Sept 1 - Oct 31.xlsx"
#     Sheet used:  "Sheet1" — weekly table with sampled and expanded total trips.
#     Sheets ignored:
#       "Summary"        — season-level aggregate rows only; used as cross-check.
#       "Sheet2"/"Table inc C&R" — sub-reach breakouts (Prosser/Horn Rapids),
#                          raw pre-expansion. See TODO flag in ingest_yakima().
#
#   Yakima (2023–2025):  "20YY  Yakima River Fall Salmon Sport Fishery Harvest vers 2.xlsx"
#     Sheet used:  "Summary" — weekly combined totals, sampled data only.
#     NOTE: These files do NOT contain an expanded total angler trips column
#     equivalent to the 2022 "Sheet1" (see Open Question 4). total_trips_est is
#     set to NA and flagged. Do not silently treat sampled_anglers as total trips.
#     Sheets ignored:
#       "Prosser"/"Horn Rapids" — section-level raw daily data; see TODO flag.
#       "Sheet1"      — year-over-year season summary table, not weekly input.
#       "Other"/"Schedule"/"Flow"/"Flow2"/"Check"/"AFC" — model inputs/metadata.
#
#   McNary (2023–2025):  "20YY McNary Reservoir Harvest Model vers 2.xlsx"
#     Sheet used:  "Print" — weekly Bank & Boat combined table with sampled and
#                  expanded (estimated) boats, anglers, and hours.
#     NOTE: The Print sheet combines bank and boat modes; angler_final is set to
#     "combined" and flagged (see Open Question 5).
#     Sheets ignored:
#       "Summary"           — harvest-focused detail; wider than effort scope.
#       "Boat Expansion"    — underlying trailer-count model; not needed.
#       "Boat"/"Bank"       — raw daily sampled counts; Print already has totals.
#       "Schedule"/"ScaleCards" — operational metadata.
#
# Open questions — RESOLVE WITH TODD MILLER / MARK S. BEFORE FINALIZING:
#
#   OQ1 [RESOLVED 2026-08-11, Todd Miller] CRC area codes:
#       Yakima River = 690, McNary Reservoir = 533.
#       Hanford Reach spans areas 534, 535, 536 (composite — no single code).
#       crc_area is set to NA for Hanford; the three constituent area codes are
#       stored in HANFORD_CRC_AREAS for reference. See CRC_AREA_LUT comment.
#
#   OQ2 Hanford bank expansion factor: The "Bank" sheet contains only sampled
#       daily counts (anglers, poles, hours). No expansion formula or factor is
#       visible in any audited sheet. Confirm with Todd whether the bank
#       component is (a) negligible and excluded from the deliverable, (b)
#       expanded using a factor stored elsewhere (Factors/Other sheet?), or
#       (c) being added to the Summary expanded total — in which case the
#       Summary already includes bank. ingest_hanford_bank() is stubbed below.
#
#   OQ3 Yakima boat component: Is the Yakima fishery bank-only by regulation
#       or access? The 2022 Sheet1 has no boat column, and the 2023+ "Boat
#       Summary" weekly totals are all zero. Confirm whether to label as
#       angler_final = "bank" (done here) or whether a rare boat component
#       should be captured separately.
#
#   OQ4 Yakima 2023+ expansion: The 2023+ workbooks restructured around section-
#       level (Prosser/Horn Rapids) sampled data and dropped the single "Total
#       Angler Trips" column present in the 2022 Sheet1. Confirm the intended
#       expansion method (same sampled% ratio as 2022, or different?), or provide
#       the expanded season-level trips visible in Sheet1's historical table so
#       that weekly proration is possible.
#
#   OQ5 McNary mode split: The "Print" sheet reports bank-and-boat combined
#       ("Estimated Anglers" includes both modes). Confirm whether a mode-split
#       version exists (e.g., separate Boat and Bank sheets already have raw
#       counts — are those expanded anywhere?) or whether combined is acceptable
#       for the PST deliverable.
#
#   OQ6 pole-hours vs. angler-hours: Yakima "Total Effort" is labelled "pole hrs"
#       and McNary "Estimated Hours" comes from a boat-expansion model. Confirm
#       whether these correspond to angler-hours (one angler, one rod) or pole-
#       hours (one angler with two rods counts double), since multi-pole rigs are
#       common. mean_trip_length derivation depends on this distinction.
#
#   OQ7 McNary 2022: No 2022 McNary file is present (oldest available is 2023).
#       Flag to Evan that if 2022 McNary effort is needed, the file is still
#       outstanding.
#
# ==============================================================================

# 0. Setup -------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(cli)
library(here)

# Input directory holding all mid-Columbia workbooks
MID_COL_DIR <- here("input_files", "mid-Columbia")

# Target years — mirrors the 2022–2025 window in multi_fishery_trip_summary.R
TARGET_YEARS <- 2022L:2025L

# CRC catch record card area codes confirmed by Todd Miller (2026-08-11).
#
# Yakima River:         690
# McNary Reservoir:     533
# Hanford Reach:        COMPOSITE — the fishery spans areas 534, 535, and 536.
#   The Summary sheet provides a single fleet-wide weekly total with NO
#   section-level breakdown by CRC area.  We therefore cannot allocate effort
#   to individual areas from this source; hanford is set to NA and the
#   constituent areas are documented here for the analyst's reference.
#   If section-level effort is needed later, see the Boats/Bank sheets and
#   coordinate with Todd on the section → CRC-area mapping.
HANFORD_CRC_AREAS <- c(534L, 535L, 536L)  # documented; not used as crc_area

CRC_AREA_LUT <- c(
  hanford = NA_integer_,  # composite: 534 + 535 + 536; cannot partition
  yakima  = 690L,
  mcnary  = 533L
)


# 1. Week-label date parsing helpers ------------------------------------------

# Parse free-text week labels such as "Sept 1 - 8", "Sept 30 - Oct 6",
# "Aug 28-Sept 3", "Oct 30- Nov 5" into a list(start = Date, end = Date).
# The `year` argument supplies the year when only month names are given.
#
# Returns NULL (with a warning) for labels that cannot be parsed rather than
# erroring, so callers can filter and flag.
parse_week_label <- function(label, year) {
  if (is.na(label) || !is.character(label)) return(NULL)
  label <- trimws(label)

  # Normalise month-name spellings and separators before parsing.
  label <- gsub("\\bSept\\b", "Sep", label, ignore.case = FALSE)  # Sept → Sep
  label <- gsub("\u2013|\u2014", "-", label)   # en/em dash → hyphen
  label <- gsub("\\s*-\\s*", " - ", label)      # pad hyphens consistently

  # Split on the central " - "
  parts <- strsplit(label, " - ", fixed = TRUE)[[1]]
  if (length(parts) != 2) {
    cli::cli_warn("Cannot parse week label: {.val {label}}")
    return(NULL)
  }

  parse_side <- function(s, fallback_month = NULL) {
    s <- trimws(s)
    # If the side contains a month name it is self-contained; otherwise
    # inherit the month from the other side (e.g. "Sept 1 - 8").
    has_month <- grepl("[A-Za-z]", s)
    if (!has_month) {
      if (is.null(fallback_month))
        stop("Cannot infer month for '", s, "'")
      s <- paste(fallback_month, s)
    }
    as.Date(paste(s, year), format = "%b %d %Y")
  }

  # Extract month from the start side for inheritance
  start_month_match <- regmatches(parts[1], regexpr("[A-Za-z]+", parts[1]))
  start_month <- if (length(start_month_match) == 1) start_month_match else NULL

  start_date <- tryCatch(parse_side(parts[1]), error = function(e) NA_Date_)
  end_date   <- tryCatch(parse_side(parts[2], start_month),
                         error = function(e) NA_Date_)

  if (is.na(start_date) || is.na(end_date)) {
    cli::cli_warn("Date parse failed for week label: {.val {label}}")
    return(NULL)
  }

  list(start = start_date, end = end_date)
}


# Expand a single-row weekly record into per-month rows, prorating numeric
# effort columns by the number of days in each calendar month.
#
# `row_data`: a named list or 1-row data frame containing at minimum
#   week_start (Date), week_end (Date), plus the numeric effort columns named
#   in `effort_cols`.
# Returns a data frame with additional columns `year`, `month`, and
#   prorated versions of each effort column. Non-effort columns (fishery_name,
#   angler_final, etc.) are repeated unchanged across the split rows.
prorate_week_to_months <- function(row_data, effort_cols) {
  start_d <- row_data[["week_start"]]
  end_d   <- row_data[["week_end"]]

  if (is.na(start_d) || is.na(end_d)) {
    return(tibble::as_tibble(row_data) |>
             dplyr::mutate(year = NA_integer_, month = NA_integer_))
  }

  days_seq    <- seq(start_d, end_d, by = "day")
  total_days  <- length(days_seq)

  if (total_days == 0) {
    return(tibble::as_tibble(row_data) |>
             dplyr::mutate(year = NA_integer_, month = NA_integer_))
  }

  month_counts <- tibble::tibble(date = days_seq) |>
    dplyr::mutate(
      year  = as.integer(format(date, "%Y")),
      month = as.integer(format(date, "%m"))
    ) |>
    dplyr::count(year, month, name = "n_days")

  base <- tibble::as_tibble(row_data)

  purrr::pmap_dfr(month_counts, function(year, month, n_days) {
    frac <- n_days / total_days
    out  <- base
    for (col in effort_cols) {
      out[[col]] <- out[[col]] * frac
    }
    out[["year"]]  <- year
    out[["month"]] <- month
    out
  })
}


# 2. Hanford Reach boat parser (Summary sheet) --------------------------------

#' Ingest one Hanford Reach Boat Harvest Model workbook.
#'
#' @param path   Full path to the xlsx file.
#' @param year   Integer calendar year extracted from the filename.
#' @return A data frame in the target schema (one row per month × angler_final).
ingest_hanford_boat <- function(path, year) {

  fishery_label <- sprintf("Hanford Reach fall Chinook %d", year)
  cli::cli_alert_info("Ingesting Hanford boat: {.path {basename(path)}}")

  # Read with .name_repair='unique' so dplyr can work with the columns.
  # Default col_types (guessed): readxl reads the date column as numeric
  # (Excel serial) because the first rows are headers/NA, so we convert
  # explicitly.  Suppressing the "New names:" message is intentional —
  # the auto-generated names (...1, ...2, ...) are internal only.
  raw <- suppressMessages(readxl::read_excel(
    path,
    sheet     = "Summary",
    col_names = FALSE,
    .name_repair = "unique"
  ))

  # Summary sheet column layout (1-indexed in R after readxl):
  #  col 1:  Week Ending (numeric Excel serial date for data rows)
  #  col 2:  Sampled Boats
  #  col 3:  Sampled Anglers
  #  col 4:  Sampled Pole Hrs
  #  col 5:  Estimated Boats
  #  col 6:  Estimated Anglers  → total_trips_est
  #  col 7:  Estimated Hours    → total_effort_hrs
  #  col 8:  % of Effort Sampled
  #  cols 9+: harvest columns (not used here)
  COL_DATE      <- 1L
  COL_SMP_BOATS <- 2L
  COL_SMP_ANG   <- 3L
  COL_EST_ANG   <- 6L   # total_trips_est
  COL_EST_HRS   <- 7L   # total_effort_hrs

  # Data rows: col 1 is numeric and > 40000 (valid Excel date range covers
  # years 2009+; header/title rows have NA or character in this column).
  # IMPORTANT: The Summary sheet contains MULTIPLE data blocks (e.g., combined
  # totals, weekday, weekend sub-tables). Only the FIRST contiguous run of date
  # rows is the all-angler total we want. Stop at the first non-date gap.
  date_col <- suppressWarnings(as.numeric(unlist(raw[[COL_DATE]])))
  is_valid_date <- !is.na(date_col) & date_col > 40000

  # Find the first data row, then take only the first contiguous block.
  first_row <- which(is_valid_date)[1]
  if (is.na(first_row)) {
    cli::cli_abort("No data rows found in Summary sheet of {.path {path}}")
  }
  # Walk forward from first_row; stop at any non-date row.
  last_row <- first_row
  for (i in seq(first_row + 1, nrow(raw))) {
    if (is_valid_date[i]) last_row <- i else break
  }
  is_data_row <- seq_len(nrow(raw)) >= first_row & seq_len(nrow(raw)) <= last_row

  safe_num <- function(col_idx) {
    suppressWarnings(as.numeric(unlist(raw[[col_idx]])[is_data_row]))
  }

  weekly <- tibble::tibble(
    week_end        = as.Date(date_col[is_data_row], origin = "1899-12-30"),
    sampled_boats   = safe_num(COL_SMP_BOATS),
    sampled_anglers = safe_num(COL_SMP_ANG),
    est_anglers     = safe_num(COL_EST_ANG),
    est_hours       = safe_num(COL_EST_HRS)
  ) |>
    dplyr::filter(!is.na(week_end), !is.na(est_hours)) |>
    dplyr::mutate(
      # Hanford weekly periods end on Sunday; the week covers the 7 days
      # ending on week_end.
      week_start = week_end - lubridate::days(6L),
      # Sampled anglers / sampled boats ≈ mean group size (boat trips)
      mean_group_size = dplyr::if_else(
        sampled_boats > 0,
        sampled_anglers / sampled_boats,
        NA_real_
      ),
      # mean_trip_length: estimated hours per estimated angler-trip
      mean_trip_length = dplyr::if_else(
        est_anglers > 0,
        est_hours / est_anglers,
        NA_real_
      )
    )

  # Prorate across calendar months for cross-month weeks
  effort_cols <- c("est_hours", "est_anglers", "sampled_anglers")

  monthly <- purrr::pmap_dfr(weekly, function(...) {
    row <- list(...)
    prorate_week_to_months(row, effort_cols)
  }) |>
    dplyr::group_by(year, month) |>
    dplyr::summarize(
      total_effort_hrs         = sum(est_hours,       na.rm = TRUE),
      total_trips_est          = sum(est_anglers,     na.rm = TRUE),
      n_completed_angler_trips = sum(sampled_anglers, na.rm = TRUE),
      mean_trip_length         = dplyr::if_else(
        sum(est_anglers, na.rm = TRUE) > 0,
        sum(est_hours,   na.rm = TRUE) / sum(est_anglers, na.rm = TRUE),
        NA_real_
      ),
      # Group-size and sd are per-week statistics; take the effort-weighted
      # mean across weeks within the month.
      mean_group_size = stats::weighted.mean(
        mean_group_size, est_hours, na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      fishery_name             = fishery_label,
      crc_area                 = CRC_AREA_LUT[["hanford"]],
      angler_final             = "boat",
      sd                       = NA_real_,   # not derivable from weekly totals
      pe_period                = "week",
      data_provider            = "Todd Miller / R-district weekly summary"
    )

  build_target_schema(monthly)
}


# Stub for the Hanford bank component — cannot expand without the expansion
# factor (Open Question 2).  The Bank sheet carries only sampled daily counts.
ingest_hanford_bank <- function(path, year) {
  # TODO: Implement once the bank expansion factor is confirmed with Todd Miller.
  # The Bank sheet contains: Date (col 1), Day (col 2), Anglers sampled (col 4),
  # Poles sampled (col 5), Hours sampled (col 6), Pole hours sampled (col 7).
  # Weekly subtotal rows are identified by: col 1 is blank AND col 4 is numeric.
  # An expansion factor (sampled → estimated) must be applied to each subtotal
  # before these can feed total_effort_hrs / total_trips_est.
  # See prompt source notes and Open Question 2 for context.
  cli::cli_alert_warning(
    "Hanford bank expansion factor not yet known — bank component skipped for \\
     year {year}. Resolve Open Question 2 with Todd Miller before including \\
     bank effort in the deliverable."
  )
  invisible(NULL)
}


# 3. Yakima River bank parser -------------------------------------------------

#' Ingest one Yakima Fall Fishery Summary workbook.
#'
#' @param path   Full path to the xlsx file.
#' @param year   Integer calendar year.
#' @return A data frame in the target schema, or NULL if no expansion is
#'   available (2023+ format with sampled data only).
ingest_yakima <- function(path, year) {

  fishery_label <- sprintf("Yakima fall salmon %d", year)
  cli::cli_alert_info("Ingesting Yakima: {.path {basename(path)}}")

  sheets <- readxl::excel_sheets(path)

  if ("Sheet1" %in% sheets && year == 2022L) {
    # ── 2022 format: Sheet1 has a weekly table with expanded total trips ──────
    ingest_yakima_2022(path, year, fishery_label)
  } else {
    # ── 2023+ format: Summary sheet has weekly sampled data only ─────────────
    # total_trips_est is not available; flag and return sampled-only output.
    # RESOLVE Open Question 4 before using this output in any analysis.
    cli::cli_alert_warning(
      "Yakima {year}: no expanded total trips column found (2023+ workbook \\
       format). total_trips_est set to NA. Resolve Open Question 4."
    )
    ingest_yakima_sampled_only(path, year, fishery_label)
  }
}


# 2022 Yakima: Sheet1 has the fully-expanded weekly table.
#
# Sheet1 column layout as read by readxl (1-indexed; readxl starts from the
# first non-empty Excel column and row):
#   col 1:  Week label  (e.g. "Sept 1 - 8")
#   col 2-6: harvest columns
#   col 7:  Sampled Anglers
#   col 8:  Sampled Hours
#   col 9:  Total Effort (pole hrs)   → total_effort_hrs
#   col 10: Sampled %
#   col 11: (empty)
#   col 12: Total Angler Trips        → total_trips_est
#
# Data rows start at R row 4 (rows 1-3 are multi-row headers).
# The season-total row has label "Season"; skip it.
ingest_yakima_2022 <- function(path, year, fishery_label) {

  raw <- suppressMessages(readxl::read_excel(
    path,
    sheet     = "Sheet1",
    col_names = FALSE,
    .name_repair = "unique"
  ))

  COL_LABEL   <- 1L
  COL_SMP_ANG <- 7L
  COL_EFF_HRS <- 9L   # Total Effort (pole hrs) = total_effort_hrs
  COL_TRIPS   <- 12L  # Total Angler Trips       = total_trips_est

  # Data rows: col 1 is a string with a month name AND digit, not a header
  lbl_col <- as.character(unlist(raw[[COL_LABEL]]))
  MONTH_RE <- "^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept?|Sep|Oct|Nov|Dec)"
  is_data_row <- grepl(MONTH_RE, trimws(lbl_col), ignore.case = TRUE) &
    !grepl("^Season", trimws(lbl_col), ignore.case = TRUE)

  if (!any(is_data_row)) {
    cli::cli_abort(
      "No weekly data rows found in Sheet1 of {.path {basename(path)}}"
    )
  }

  safe_num <- function(col_idx) {
    suppressWarnings(as.numeric(unlist(raw[[col_idx]])[is_data_row]))
  }

  weekly <- tibble::tibble(
    week_label  = lbl_col[is_data_row],
    sampled_ang = safe_num(COL_SMP_ANG),
    effort_hrs  = safe_num(COL_EFF_HRS),
    total_trips = safe_num(COL_TRIPS)
  ) |>
    dplyr::filter(!is.na(effort_hrs))

  # Parse week labels into start/end dates
  parsed_dates <- purrr::map(weekly$week_label, parse_week_label, year = year)

  weekly <- weekly |>
    dplyr::mutate(
      week_start = purrr::map_vec(parsed_dates,
                                  ~ if (is.null(.x)) NA_Date_ else .x$start,
                                  .ptype = as.Date(NA)),
      week_end   = purrr::map_vec(parsed_dates,
                                  ~ if (is.null(.x)) NA_Date_ else .x$end,
                                  .ptype = as.Date(NA)),
      mean_trip_length = dplyr::if_else(
        total_trips > 0,
        effort_hrs / total_trips,
        NA_real_
      )
    )

  effort_cols <- c("effort_hrs", "total_trips", "sampled_ang")

  monthly <- purrr::pmap_dfr(weekly, function(...) {
    row <- list(...)
    prorate_week_to_months(row, effort_cols)
  }) |>
    dplyr::group_by(year, month) |>
    dplyr::summarize(
      total_effort_hrs         = sum(effort_hrs,   na.rm = TRUE),
      total_trips_est          = sum(total_trips,  na.rm = TRUE),
      n_completed_angler_trips = sum(sampled_ang,  na.rm = TRUE),
      mean_trip_length         = dplyr::if_else(
        sum(total_trips, na.rm = TRUE) > 0,
        sum(effort_hrs,  na.rm = TRUE) / sum(total_trips, na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      fishery_name  = fishery_label,
      crc_area      = CRC_AREA_LUT[["yakima"]],
      angler_final  = "bank",
      mean_group_size = NA_real_,   # not reported in Yakima workbook
      sd            = NA_real_,
      pe_period     = "week",
      data_provider = "Todd Miller / R-district weekly summary"
    )

  build_target_schema(monthly)
}


# 2023+ Yakima: "Summary" sheet has sampled counts only.
# Column layout as read by readxl (1-indexed):
#   col 1:  Week label (e.g. "Sept 1-3") — data rows, and "Combined"/
#           "Bank Summary"/"Boat Summary" are section headers to skip
#   col 3:  Sampled Boats
#   col 4:  Sampled Anglers   → n_completed_angler_trips (sampled only)
#   col 5:  Sampled Hours     → total_effort_hrs (sampled; NOT expanded)
# total_trips_est = NA — expansion not available (OQ4).
ingest_yakima_sampled_only <- function(path, year, fishery_label) {

  raw <- suppressMessages(readxl::read_excel(
    path,
    sheet     = "Summary",
    col_names = FALSE,
    .name_repair = "unique"
  ))

  COL_LABEL   <- 1L
  COL_SMP_ANG <- 4L
  COL_SMP_HRS <- 5L

  # Data rows: col 1 is a string matching a month name (week label pattern).
  # Skip header rows ("Combined", "Bank Summary", "Boat Summary", title).
  MONTH_RE <- "^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept?|Sep|Oct|Nov|Dec)"
  lbl_col  <- as.character(unlist(raw[[COL_LABEL]]))
  is_data_row <- grepl(MONTH_RE, trimws(lbl_col), ignore.case = TRUE)

  if (!any(is_data_row)) {
    cli::cli_alert_warning(
      "No weekly data rows detected in Summary of {.path {basename(path)}} \\
       — skipping Yakima {year}."
    )
    return(NULL)
  }

  safe_num <- function(col_idx) {
    suppressWarnings(as.numeric(unlist(raw[[col_idx]])[is_data_row]))
  }

  weekly <- tibble::tibble(
    week_label  = lbl_col[is_data_row],
    sampled_ang = safe_num(COL_SMP_ANG),
    sampled_hrs = safe_num(COL_SMP_HRS)
  ) |>
    dplyr::filter(!is.na(sampled_hrs))

  parsed_dates <- purrr::map(weekly$week_label, parse_week_label, year = year)

  weekly <- weekly |>
    dplyr::mutate(
      week_start = purrr::map_vec(parsed_dates,
                                  ~ if (is.null(.x)) NA_Date_ else .x$start,
                                  .ptype = as.Date(NA)),
      week_end   = purrr::map_vec(parsed_dates,
                                  ~ if (is.null(.x)) NA_Date_ else .x$end,
                                  .ptype = as.Date(NA))
    )

  effort_cols <- c("sampled_hrs", "sampled_ang")

  monthly <- purrr::pmap_dfr(weekly, function(...) {
    row <- list(...)
    prorate_week_to_months(row, effort_cols)
  }) |>
    dplyr::group_by(year, month) |>
    dplyr::summarize(
      total_effort_hrs         = sum(sampled_hrs, na.rm = TRUE),
      n_completed_angler_trips = sum(sampled_ang, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      fishery_name    = fishery_label,
      crc_area        = CRC_AREA_LUT[["yakima"]],
      angler_final    = "bank",
      total_trips_est = NA_real_,   # expansion not available — see OQ4
      mean_trip_length = NA_real_,
      mean_group_size  = NA_real_,
      sd               = NA_real_,
      pe_period        = "week",
      data_provider    = "Todd Miller / R-district weekly summary"
    )

  build_target_schema(monthly)
}


# 4. McNary Reservoir parser (Print sheet) ------------------------------------

#' Ingest one McNary Reservoir Harvest Model workbook.
#'
#' The "Print" sheet contains a weekly "Bank & Boat Combined" table with:
#'   col 1:  Week label (e.g. "Aug 28-Sept 3")
#'   col 2:  Sampled Boats
#'   col 3:  Sampled Anglers
#'   col 4:  Sampled Hours
#'   col 6:  Estimated Boats (expansion)
#'   col 7:  Estimated Anglers   → total_trips_est
#'   col 8:  Estimated Hours     → total_effort_hrs
#'   col 9:  % Anglers Sampled
#'
#' NOTE: angler_final = "combined" because no bank/boat split is available in
#' the Print sheet (see Open Question 5).
#'
#' @param path   Full path to the xlsx file.
#' @param year   Integer calendar year.
#' @return A data frame in the target schema.
ingest_mcnary <- function(path, year) {

  fishery_label <- sprintf("McNary Reservoir fall salmon %d", year)
  cli::cli_alert_info("Ingesting McNary: {.path {basename(path)}}")

  # Print sheet starts at row 1 (0-indexed).  Skip the header block (rows 1–6
  # in Excel = indices 0–5 in R after skip=0) by detecting data rows: col 1 is
  # a non-blank character string containing a digit (week label) and not "Total".
  raw <- suppressMessages(readxl::read_excel(
    path,
    sheet     = "Print",
    col_names = FALSE,
    .name_repair = "unique"
  ))

  COL_LABEL     <- 1L
  COL_SMP_BOATS <- 2L
  COL_SMP_ANG   <- 3L
  COL_EST_ANG   <- 7L   # total_trips_est
  COL_EST_HRS   <- 8L   # total_effort_hrs

  MONTH_RE  <- "^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept?|Sep|Oct|Nov|Dec)"
  lbl_col   <- as.character(unlist(raw[[COL_LABEL]]))
  is_data_row <- grepl(MONTH_RE, trimws(lbl_col), ignore.case = TRUE) &
    !grepl("^Total", trimws(lbl_col), ignore.case = TRUE)

  if (!any(is_data_row)) {
    cli::cli_abort("No data rows found in Print sheet of {.path {path}}")
  }

  safe_num <- function(col_idx) {
    suppressWarnings(as.numeric(unlist(raw[[col_idx]])[is_data_row]))
  }

  weekly <- tibble::tibble(
    week_label    = lbl_col[is_data_row],
    sampled_boats = safe_num(COL_SMP_BOATS),
    sampled_ang   = safe_num(COL_SMP_ANG),
    est_ang       = safe_num(COL_EST_ANG),
    est_hrs       = safe_num(COL_EST_HRS)
  ) |>
    dplyr::filter(!is.na(est_hrs), !is.na(est_ang))

  # McNary week labels: "Aug 28-Sept 3", "Sept 4-10", "Oct 17-Oct 22"
  # These use dash without spaces; normalise before parse_week_label.
  weekly <- weekly |>
    dplyr::mutate(
      week_label_norm = stringr::str_replace_all(
        week_label,
        # Insert spaces around "-" only when not already padded
        "(?<=[A-Za-z0-9])-(?=[A-Za-z0-9])", " - "
      )
    )

  parsed_dates <- purrr::map(weekly$week_label_norm,
                             parse_week_label, year = year)

  weekly <- weekly |>
    dplyr::mutate(
      week_start = purrr::map_vec(parsed_dates,
                                  ~ if (is.null(.x)) NA_Date_ else .x$start,
                                  .ptype = as.Date(NA)),
      week_end   = purrr::map_vec(parsed_dates,
                                  ~ if (is.null(.x)) NA_Date_ else .x$end,
                                  .ptype = as.Date(NA)),
      mean_group_size = dplyr::if_else(
        sampled_boats > 0,
        sampled_ang / sampled_boats,
        NA_real_
      )
    )

  effort_cols <- c("est_hrs", "est_ang", "sampled_ang")

  monthly <- purrr::pmap_dfr(weekly, function(...) {
    row <- list(...)
    prorate_week_to_months(row, effort_cols)
  }) |>
    dplyr::group_by(year, month) |>
    dplyr::summarize(
      total_effort_hrs         = sum(est_hrs,    na.rm = TRUE),
      total_trips_est          = sum(est_ang,    na.rm = TRUE),
      n_completed_angler_trips = sum(sampled_ang, na.rm = TRUE),
      mean_trip_length         = dplyr::if_else(
        sum(est_ang,  na.rm = TRUE) > 0,
        sum(est_hrs,  na.rm = TRUE) / sum(est_ang, na.rm = TRUE),
        NA_real_
      ),
      mean_group_size = stats::weighted.mean(
        mean_group_size, est_hrs, na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      fishery_name  = fishery_label,
      crc_area      = CRC_AREA_LUT[["mcnary"]],
      # Combined bank+boat — no mode split in Print sheet (OQ5)
      angler_final  = "combined",
      sd            = NA_real_,
      pe_period     = "week",
      data_provider = "Todd Miller / R-district weekly summary"
    )

  build_target_schema(monthly)
}


# 5. Target schema enforcer ---------------------------------------------------

# Required column order matches multi_fishery_trip_summary.csv plus
# the `data_provider` provenance column.
TARGET_COLS <- c(
  "fishery_name", "year", "month", "crc_area", "angler_final",
  "total_effort_hrs", "n_completed_angler_trips", "mean_trip_length",
  "mean_group_size", "sd", "total_trips_est", "pe_period",
  "data_provider"
)

build_target_schema <- function(df) {
  # Add any missing columns as NA
  for (col in TARGET_COLS) {
    if (!col %in% names(df)) df[[col]] <- NA
  }
  df |>
    dplyr::mutate(
      year  = as.integer(year),
      month = as.integer(month)
    ) |>
    dplyr::select(dplyr::all_of(TARGET_COLS))
}


# 6. Combining wrapper --------------------------------------------------------

#' Discover and ingest all mid-Columbia workbooks for a given set of years.
#'
#' Matches filenames by pattern:
#'   "^<year> Hanford Reach Boat Harvest Model"
#'   "^<year>[ ]+Yakima (Fall|River Fall)"
#'   "^<year> McNary Reservoir Harvest Model"
#'
#' @param dir       Directory containing the xlsx files.
#' @param years     Integer vector of target years.
#' @return A data frame with all successfully ingested records.
ingest_mid_columbia <- function(dir = MID_COL_DIR, years = TARGET_YEARS) {

  cli::cli_h2("Mid-Columbia creel workbook ingestion")

  all_files <- list.files(dir, pattern = "\\.xlsx$", full.names = TRUE,
                          ignore.case = TRUE)

  if (length(all_files) == 0) {
    cli::cli_abort("No xlsx files found in {.path {dir}}")
  }

  results <- vector("list", length = 0L)

  for (yr in years) {

    # ── Hanford boat ──────────────────────────────────────────────────────────
    hanford_pat <- sprintf("^%d\\s+Hanford Reach Boat Harvest Model", yr)
    hanford_files <- all_files[grepl(hanford_pat, basename(all_files),
                                     ignore.case = TRUE)]

    if (length(hanford_files) == 0) {
      cli::cli_alert_warning("No Hanford workbook found for year {yr}")
    } else {
      if (length(hanford_files) > 1) {
        cli::cli_alert_warning(
          "Multiple Hanford files for {yr}; using first: \\
           {.path {basename(hanford_files[1])}}"
        )
      }
      res <- tryCatch(
        ingest_hanford_boat(hanford_files[1], yr),
        error = function(e) {
          cli::cli_alert_danger(
            "Hanford boat {yr} failed: {conditionMessage(e)}"
          )
          NULL
        }
      )
      if (!is.null(res)) results <- c(results, list(res))

      # Bank component: stubbed — call and discard NULL return
      ingest_hanford_bank(hanford_files[1], yr)
    }

    # ── Yakima ────────────────────────────────────────────────────────────────
    yakima_pat <- sprintf("^%d\\s+Yakima", yr)
    yakima_files <- all_files[grepl(yakima_pat, basename(all_files),
                                    ignore.case = TRUE)]

    if (length(yakima_files) == 0) {
      cli::cli_alert_warning("No Yakima workbook found for year {yr}")
    } else {
      if (length(yakima_files) > 1) {
        cli::cli_alert_warning(
          "Multiple Yakima files for {yr}; using first: \\
           {.path {basename(yakima_files[1])}}"
        )
      }
      res <- tryCatch(
        ingest_yakima(yakima_files[1], yr),
        error = function(e) {
          cli::cli_alert_danger(
            "Yakima {yr} failed: {conditionMessage(e)}"
          )
          NULL
        }
      )
      if (!is.null(res)) results <- c(results, list(res))
    }

    # ── McNary ────────────────────────────────────────────────────────────────
    mcnary_pat <- sprintf("^%d\\s+McNary Reservoir Harvest Model", yr)
    mcnary_files <- all_files[grepl(mcnary_pat, basename(all_files),
                                    ignore.case = TRUE)]

    if (length(mcnary_files) == 0) {
      # No 2022 file exists; 2023–2025 files are present.
      if (yr >= 2023L) {
        cli::cli_alert_warning("No McNary workbook found for year {yr}")
      } else {
        # OQ7: 2022 McNary file outstanding — flag to Evan.
        cli::cli_alert_info(
          "McNary {yr}: file not yet received (OQ7 — flag to Evan). Skipping."
        )
      }
    } else {
      if (length(mcnary_files) > 1) {
        cli::cli_alert_warning(
          "Multiple McNary files for {yr}; using first: \\
           {.path {basename(mcnary_files[1])}}"
        )
      }
      res <- tryCatch(
        ingest_mcnary(mcnary_files[1], yr),
        error = function(e) {
          cli::cli_alert_danger(
            "McNary {yr} failed: {conditionMessage(e)}"
          )
          NULL
        }
      )
      if (!is.null(res)) results <- c(results, list(res))
    }
  }

  if (length(results) == 0) {
    cli::cli_abort("No workbooks were successfully ingested.")
  }

  combined <- dplyr::bind_rows(results)
  cli::cli_alert_success(
    "Ingested {nrow(combined)} rows across \\
     {dplyr::n_distinct(combined$fishery_name)} fisheries."
  )
  combined
}


# 7. Run and save output -------------------------------------------------------

columbia_creel <- ingest_mid_columbia()

out_dir  <- here("analysis", "outputs")
out_path <- file.path(out_dir, "mid_columbia_yakima_creel_summary.csv")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(columbia_creel, out_path)

cli::cli_alert_success(
  "Saved {nrow(columbia_creel)} rows to {.path {out_path}}"
)

# Preview
cli::cli_h3("Row counts by fishery")
columbia_creel |>
  dplyr::count(fishery_name, angler_final) |>
  print(n = 30)

cli::cli_h3("Rows with NA total_trips_est (review before analysis)")
columbia_creel |>
  dplyr::filter(is.na(total_trips_est)) |>
  dplyr::count(fishery_name) |>
  print(n = 20)
