# ==============================================================================
# district_creel_ingestion.R
#
# Purpose:
#   Parse ad hoc creel-summary workbooks provided directly by WDFW district
#   creel staff — pre-computed trip/effort totals that never touch this
#   repo's own PE estimators — and reshape them into a table matching the
#   schema of multi_fishery_creel_trips.csv so they can be row-bound for the
#   freshwater PST effort deliverable. Originally written for Todd Miller's
#   (R3) mid-Columbia and Yakima weekly models; extended 2026-08-18 to also
#   cover Jeremy Trump's (R1) Snake River annual summary. R2 is expected to
#   supply a workbook eventually — see "R2 — anticipated" below.
#
# Usage:
#   Source interactively or run with Rscript from the repo root:
#     Rscript analysis/pst/02_ingest/district_creel_ingestion.R
#   Requires no DB access — reads xlsx files from input_files/pst/R3_creel/
#   and input_files/pst/R1_creel/.
#
#   Output: analysis/pst/outputs/03_district_creel/district_creel_summary.csv
#           (A downstream step row-binds this with multi_fishery_creel_trips.csv.)
#
# Every row carries a `district` column (R1/R3, eventually R2) so that
# pst_fw_angler_trips_assembly.R can attribute source_id and method per row
# instead of assuming everything in this file came from one place — that
# assumption held when this file was Todd-Miller-only; it stopped holding the
# moment a second district's data landed in the same output.
#
# R1 — Snake River (Jeremy Trump / Mike Gembala), added 2026-08-18, data
# refreshed 2026-08-20:
#   Source file:  input_files/pst/R1_creel/Salmon Angler Days 2022-2025.xlsx
#   One small annual summary, not a weekly model — see ingest_snake_river()
#   for the full parsing notes, including the "Angler Days" label issue
#   (Jeremy confirms these are trips, not days — see the LABEL NOTE there).
#   The crosswalk gap noted here originally is RESOLVED (2026-08-19) — see the
#   CROSSWALK GAP note in ingest_snake_river()'s docstring.
#
# R4 — Green/Duwamish (Nathanael Overman), added 2026-08-21:
#   Source files:
#     input_files/pst/R4_creel/GreenDuwamish Creel Survey Raw Interviews .xlsx
#     input_files/pst/R4_creel/GreenDuwamish Creel Survey Estimated Anglers .xlsx
#   Unlike R1/R3, R4 does NOT supply a finished trip total. The "Estimated
#   Anglers" workbook gives only ANGLER HOURS (total effort, boat/shore, by
#   year) that we trust as-is, plus an ANGLERS block that looks like it could
#   be a trip count but is NOT used as one here — see ingest_green_duwamish()
#   for why it's kept strictly as a cross-check. We derive total_trips_est
#   ourselves: mean angler-hours-per-angler-trip is computed from the paired
#   Raw Interviews workbook (completed trips only), then
#   total_trips_est = total_effort_hrs / mean_trip_length. Trip length is
#   pooled across the full year (not by sub-period) per Evan's call
#   2026-08-21. See ingest_green_duwamish_interviews() for the five
#   different raw-interview column layouts (one per sheet/year) and a real
#   data-quality find in the 2023 sheet: its "Duration (hrs)" column is
#   already angler-hours (verified against Start/End timestamps: Duration ==
#   (End-Start) x #Angler for every row), not per-trip length as the header
#   implies — treating it as per-trip length would double-multiply by
#   #Angler and inflate 2023 boat trip length ~2.7x.
#
# R2 — Upper Columbia (Chad Jackson), received 2026-08-28:
#   Source file:  input_files/pst/R2_creel/R2_Angler_Trips_2022-2025.pdf
#   Unlike R1/R3/R4, this arrived as a PDF capture of an email table (four
#   rows: 2022-2025 combined district trip totals), not a workbook — there
#   is nothing to parse cell-by-cell, so ingest_r2_upper_columbia() hardcodes
#   the table directly. It is also the coarsest source in the pipeline: one
#   combined total per year for the whole "Upper Columbia" district (mainstem
#   Priest Rapids-Chief Joseph Dam, Icicle River, Lake Wenatchee, and several
#   named tributaries per Chad's own footnote), no river/mode/location split
#   at all. See ingest_r2_upper_columbia()'s header for the real scope
#   mismatches this raises against the existing crosswalk (Methow/Chelan/
#   mainstem boundary) — logged as open gaps, not resolved silently.
#
# Source files used (in input_files/pst/R3_creel/):
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
#     Sheets used: "Prosser" and "Horn Rapids" — per-site weekly "Combined"
#     tables, each with its own expanded Angler Trips / Effort Pole Hrs
#     columns (see Open Question 4, RESOLVED 2026-08-21). Summed per week
#     across both sites, then prorated to months.
#     Sheets ignored:
#       "Summary"     — weekly combined totals, but SAMPLED data only; no
#                        expansion. Kept as a cross-check candidate, not used
#                        as the source (this is what the superseded parser
#                        read, which is why OQ4 concluded no expansion existed).
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
#       Summary" weekly totals are all zero -- at Horn Rapids because zero
#       boats/anglers are ever sampled there (2023-2025, all weeks), at
#       Prosser because its own Boat Summary Angler Trips formula is broken
#       (workbook's own "No Boats=FIX FORMULA" note) even in the one week
#       with boats actually sampled. Still open: confirm with Todd whether
#       this reflects a real bank-only fishery/regulation or an undercounted
#       boat mode; angler_final = "bank" is used here either way since the
#       boat contribution is negligible in the data as recorded.
#
#   OQ4 Yakima 2023+ expansion — RESOLVED 2026-08-21 (Evan, by direct
#       inspection of the workbooks, not correspondence). The expansion was
#       never dropped -- it was never read from the right sheet. The
#       "Summary" sheet this parser used to read really is sampled-only, but
#       the per-site "Prosser" and "Horn Rapids" tabs (previously ignored
#       entirely) each carry a "Combined" table with the same expanded
#       Angler Trips / Effort Pole Hrs columns 2022's Sheet1 has, verified
#       present and identically laid out in the 2023, 2024, and 2025
#       workbooks. ingest_yakima_site_combined() now reads both site tabs,
#       sums them per week, and prorates to months exactly as the 2022
#       parser does.
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

# Input directories, one per contributing district.
R3_DIR <- here("input_files", "pst", "R3_creel")   # Todd Miller: mid-Columbia/Yakima
R1_DIR <- here("input_files", "pst", "R1_creel")    # Jeremy Trump / Mike Gembala: Snake River
R2_DIR <- here("input_files", "pst", "R2_creel")    # anticipated, not yet received
R4_DIR <- here("input_files", "pst", "R4_creel")    # Nathanael Overman: Green/Duwamish

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

  # A single-date label with no range (e.g. "Sept 15", seen in the Yakima
  # Prosser/Horn Rapids tabs as a partial first week when the season opens
  # mid-month) has no " - " to split on. Treated as a one-day week rather
  # than failing to parse -- the alternative is silently dropping that
  # week's real effort/trips via the caller's is.na(week_start) filter,
  # which is a worse failure than a one-day week.
  if (!grepl(" - ", label, fixed = TRUE)) {
    single_date <- tryCatch(as.Date(paste(label, year), format = "%b %d %Y"),
                            error = function(e) NA_Date_)
    if (is.na(single_date)) {
      cli::cli_warn("Cannot parse week label: {.val {label}}")
      return(NULL)
    }
    return(list(start = single_date, end = single_date))
  }

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
  # Harvest: expanded kept salmon (steelhead excluded per PST scope).
  # Header layout confirmed from 2022 workbook:
  #   col 18 = Exp. Chinook Adult Harvest
  #   col 19 = Exp. Chinook Jacks Harvest
  #   col 20 = Exp. Sockeye Adult Harvest
  #   col 21 = Exp. Coho Adult Harvest
  #   col 22 = Exp. Coho Jacks Harvest
  HARVEST_COLS  <- 18L:22L

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
    est_hours       = safe_num(COL_EST_HRS),
    # Row-wise sum across all expanded kept salmon harvest columns.
    # na.rm = TRUE so a single NA species (e.g. no sockeye) doesn't zero the row.
    salmon_harvest  = purrr::pmap_dbl(
      purrr::map(HARVEST_COLS, safe_num) |> purrr::set_names(paste0("h", HARVEST_COLS)),
      ~ sum(c(...), na.rm = TRUE)
    )
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
  effort_cols <- c("est_hours", "est_anglers", "sampled_anglers", "salmon_harvest")

  monthly <- purrr::pmap_dfr(weekly, function(...) {
    row <- list(...)
    prorate_week_to_months(row, effort_cols)
  }) |>
    dplyr::group_by(year, month) |>
    dplyr::summarize(
      total_effort_hrs         = sum(est_hours,       na.rm = TRUE),
      total_trips_est          = sum(est_anglers,     na.rm = TRUE),
      n_completed_angler_trips = sum(sampled_anglers, na.rm = TRUE),
      total_salmon_harvest     = sum(salmon_harvest,  na.rm = TRUE),
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
      sd                       = NA_real_,
      pe_period                = "week",
      harvest_expansion        = "expanded",
      data_provider            = "Todd Miller / R-district weekly summary",
      district                 = "R3"
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
#' @return A data frame in the target schema.
ingest_yakima <- function(path, year) {

  fishery_label <- sprintf("Yakima fall salmon %d", year)
  cli::cli_alert_info("Ingesting Yakima: {.path {basename(path)}}")

  sheets <- readxl::excel_sheets(path)

  if ("Sheet1" %in% sheets && year == 2022L) {
    # ── 2022 format: Sheet1 has a weekly table with expanded total trips ──────
    ingest_yakima_2022(path, year, fishery_label)
  } else {
    # ── 2023+ format: OQ4 RESOLVED 2026-08-21. The "Summary" sheet this
    # branch used to read really does carry sampled-only totals -- but the
    # workbook was never restructured to drop the expansion, as OQ4 assumed.
    # The per-site "Prosser" and "Horn Rapids" tabs (ignored entirely by the
    # old code) each carry their own Combined table with the SAME expanded
    # Angler Trips / Effort Pole Hrs columns the 2022 Sheet1 has, just at
    # different column positions. See ingest_yakima_site_combined().
    ingest_yakima_site_combined(path, year, fishery_label)
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
  # Harvest: expanded kept salmon (cols 2–5 confirmed from 2022 workbook header):
  #   col 2 = Chinook Adult, col 3 = Chinook Jacks,
  #   col 4 = Coho Adult,    col 5 = Coho Jacks  (col 6 = C&R steelhead, excluded)
  HARVEST_COLS <- 2L:5L

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
    week_label     = lbl_col[is_data_row],
    sampled_ang    = safe_num(COL_SMP_ANG),
    effort_hrs     = safe_num(COL_EFF_HRS),
    total_trips    = safe_num(COL_TRIPS),
    salmon_harvest = purrr::pmap_dbl(
      purrr::map(HARVEST_COLS, safe_num) |> purrr::set_names(paste0("h", HARVEST_COLS)),
      ~ sum(c(...), na.rm = TRUE)
    )
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

  effort_cols <- c("effort_hrs", "total_trips", "sampled_ang", "salmon_harvest")

  monthly <- purrr::pmap_dfr(weekly, function(...) {
    row <- list(...)
    prorate_week_to_months(row, effort_cols)
  }) |>
    dplyr::group_by(year, month) |>
    dplyr::summarize(
      total_effort_hrs         = sum(effort_hrs,      na.rm = TRUE),
      total_trips_est          = sum(total_trips,     na.rm = TRUE),
      n_completed_angler_trips = sum(sampled_ang,     na.rm = TRUE),
      total_salmon_harvest     = sum(salmon_harvest,  na.rm = TRUE),
      mean_trip_length         = dplyr::if_else(
        sum(total_trips, na.rm = TRUE) > 0,
        sum(effort_hrs,  na.rm = TRUE) / sum(total_trips, na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      fishery_name      = fishery_label,
      crc_area          = CRC_AREA_LUT[["yakima"]],
      angler_final      = "bank",
      mean_group_size   = NA_real_,
      sd                = NA_real_,
      pe_period         = "week",
      harvest_expansion = "expanded",
      data_provider     = "Todd Miller / R-district weekly summary",
      district          = "R3"
    )

  build_target_schema(monthly)
}


# 2023+ Yakima: per-site Combined tables carry the real expansion (OQ4 RESOLVED
# 2026-08-21).
#
# The "Summary" sheet the old parser read genuinely has sampled-only totals --
# that observation in the superseded version of this function was correct.
# What was wrong was concluding from it that the 2023+ workbooks dropped the
# expansion entirely. They didn't: two other sheets, "Prosser" and "Horn
# Rapids" (one per physical creel site), were never read by this parser and
# each carries its own "Combined" weekly table with the SAME expanded Angler
# Trips / Effort Pole Hrs columns the 2022 Sheet1 has -- just at different
# column positions, and split per-site rather than pooled. Verified present,
# non-error, and identically laid out across the 2023, 2024, and 2025
# workbooks by direct inspection (openpyxl) before writing this parser.
#
# "Combined" column layout (1-indexed, verified 2023-2025):
#   col 3:  Week label
#   col 6:  Sampled Anglers        → n_completed_angler_trips (informational)
#   col 8-9, 10-11:  Chinook kept  Adults NM/AD, Jacks NM/AD
#   col 14-15, 16-17: Coho kept    Adults NM/AD, Jacks NM/AD
#   (cols 12-13, 18-27 are Released/Steelhead/incidental catch -- excluded
#   per PST scope [S1]/[S2])
#   col 29: Angler Trips    (expanded) → total_trips_est
#   col 30: Effort Pole Hrs (expanded) → total_effort_hrs
#
# Horn Rapids reports zero sampled anglers/boats in EVERY week of all three
# years -- no creel coverage at that site in this data. Summing it with
# Prosser is a no-op today but keeps the code correct if that changes.
#
# Prosser's own "Boat Summary" sub-table is broken: the workbook itself flags
# this with a "No Boats=FIX FORMULA" note (row 2), and its Angler Trips column
# reads 0 even in the one week with boats actually sampled. "Combined" already
# includes that (effectively zero) boat contribution, so reading it under
# angler_final = "bank" is consistent with the existing OQ3 framing (Yakima
# treated as bank-only), not a new assumption layered on top.
ingest_yakima_site_combined <- function(path, year, fishery_label) {

  SITE_SHEETS  <- c("Prosser", "Horn Rapids")
  COL_LABEL    <- 3L
  COL_SMP_ANG  <- 6L
  COL_TRIPS    <- 29L
  COL_POLE_HRS <- 30L
  HARVEST_COLS_KEPT <- c(8L, 9L, 10L, 11L, 14L, 15L, 16L, 17L)

  MONTH_RE <- "^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept?|Sep|Oct|Nov|Dec)"

  site_weekly <- purrr::map_dfr(SITE_SHEETS, function(sheet_name) {
    raw <- suppressMessages(readxl::read_excel(
      path, sheet = sheet_name, col_names = FALSE, .name_repair = "unique"
    ))

    lbl_col <- as.character(unlist(raw[[COL_LABEL]]))

    # RESTRICT TO THE "Combined" BLOCK. Each sheet stacks three tables in this
    # same column -- Combined, Bank Summary, Boat Summary -- reusing the exact
    # same week-label text for all three ("Sept 1-3" appears once per block).
    # A plain regex match on the label column across the whole sheet would
    # therefore match rows from all three blocks and TRIPLE-COUNT every week
    # (caught by cross-checking a prototype against the sheet's own season-
    # total row before trusting this parser -- the season total came out at
    # roughly 2x the true "Combined" total). Only rows strictly between the
    # "Combined" and "Bank Summary" section labels are read.
    combined_row <- which(tolower(trimws(lbl_col)) == "combined")[1]
    bank_row     <- which(tolower(trimws(lbl_col)) == "bank summary")[1]

    if (is.na(combined_row) || is.na(bank_row) || bank_row <= combined_row) {
      cli::cli_alert_warning(
        "Could not locate the Combined/Bank Summary block boundary in \\
         {.val {sheet_name}} of {.path {basename(path)}} — skipping this \\
         sheet."
      )
      return(tibble::tibble())
    }

    in_combined_block <- seq_along(lbl_col) > combined_row &
      seq_along(lbl_col) < bank_row
    is_data_row <- in_combined_block &
      grepl(MONTH_RE, trimws(lbl_col), ignore.case = TRUE)

    if (!any(is_data_row)) {
      cli::cli_alert_warning(
        "No weekly data rows found in the Combined block of {.val {sheet_name}} \\
         of {.path {basename(path)}}."
      )
      return(tibble::tibble())
    }

    safe_num <- function(col_idx) {
      suppressWarnings(as.numeric(unlist(raw[[col_idx]])[is_data_row]))
    }

    tibble::tibble(
      week_label     = trimws(lbl_col[is_data_row]),
      sampled_ang    = safe_num(COL_SMP_ANG),
      total_trips    = safe_num(COL_TRIPS),
      effort_hrs     = safe_num(COL_POLE_HRS),
      salmon_harvest = purrr::pmap_dbl(
        purrr::map(HARVEST_COLS_KEPT, safe_num) |>
          purrr::set_names(paste0("h", HARVEST_COLS_KEPT)),
        ~ sum(c(...), na.rm = TRUE)
      )
    )
  })

  if (nrow(site_weekly) == 0) {
    cli::cli_alert_warning(
      "No usable weekly data in Prosser/Horn Rapids for {fishery_label} \\
       — skipping."
    )
    return(NULL)
  }

  # Weeks with nothing sampled read back as NA (Excel #DIV/0! or a blank cell
  # on the Trips/Pole Hrs formulas), not 0 -- coalesce so an unsampled
  # site-week contributes zero to the cross-site sum below instead of
  # propagating NA through it.
  site_weekly <- site_weekly |>
    dplyr::mutate(
      sampled_ang    = dplyr::coalesce(sampled_ang, 0),
      total_trips    = dplyr::coalesce(total_trips, 0),
      effort_hrs     = dplyr::coalesce(effort_hrs, 0),
      salmon_harvest = dplyr::coalesce(salmon_harvest, 0)
    )

  # Parse dates PER SITE ROW before combining: week-label text is not
  # guaranteed byte-identical between the Prosser and Horn Rapids tabs, but
  # the calendar week is. Grouping on parsed dates rather than the raw label
  # string avoids silently splitting one week into two groups over a
  # formatting difference between sheets.
  parsed_dates <- purrr::map(site_weekly$week_label, parse_week_label, year = year)

  site_weekly <- site_weekly |>
    dplyr::mutate(
      week_start = purrr::map_vec(parsed_dates,
                                  ~ if (is.null(.x)) NA_Date_ else .x$start,
                                  .ptype = as.Date(NA)),
      week_end   = purrr::map_vec(parsed_dates,
                                  ~ if (is.null(.x)) NA_Date_ else .x$end,
                                  .ptype = as.Date(NA))
    ) |>
    dplyr::filter(!is.na(week_start))

  weekly <- site_weekly |>
    dplyr::group_by(week_start, week_end) |>
    dplyr::summarize(
      sampled_ang    = sum(sampled_ang,    na.rm = TRUE),
      total_trips    = sum(total_trips,    na.rm = TRUE),
      effort_hrs     = sum(effort_hrs,     na.rm = TRUE),
      salmon_harvest = sum(salmon_harvest, na.rm = TRUE),
      .groups = "drop"
    )

  effort_cols <- c("effort_hrs", "total_trips", "sampled_ang", "salmon_harvest")

  monthly <- purrr::pmap_dfr(weekly, function(...) {
    row <- list(...)
    prorate_week_to_months(row, effort_cols)
  }) |>
    dplyr::group_by(year, month) |>
    dplyr::summarize(
      total_effort_hrs         = sum(effort_hrs,     na.rm = TRUE),
      total_trips_est          = sum(total_trips,    na.rm = TRUE),
      n_completed_angler_trips = sum(sampled_ang,    na.rm = TRUE),
      total_salmon_harvest     = sum(salmon_harvest, na.rm = TRUE),
      mean_trip_length         = dplyr::if_else(
        sum(total_trips, na.rm = TRUE) > 0,
        sum(effort_hrs,  na.rm = TRUE) / sum(total_trips, na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      fishery_name      = fishery_label,
      crc_area          = CRC_AREA_LUT[["yakima"]],
      angler_final      = "bank",
      mean_group_size   = NA_real_,
      sd                = NA_real_,
      pe_period         = "week",
      harvest_expansion = "expanded",
      data_provider     = "Todd Miller / R-district weekly summary (Prosser + Horn Rapids Combined tabs)",
      district          = "R3"
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
  # Harvest: expanded kept salmon (steelhead excluded per PST scope).
  # Column layout confirmed from 2023 workbook Print sheet header:
  #   col 11-16 = Steelhead (excluded)
  #   col 17 = Coho Harvest NM,  col 18 = Coho Harvest AD
  #   col 19-20 = Coho C&R (excluded)
  #   col 21 = Chinook Adult Harvest NM, col 22 = Chinook Adult Harvest AD
  #   col 23 = Chinook Jack Harvest NM,  col 24 = Chinook Jack Harvest AD
  HARVEST_COLS  <- c(17L, 18L, 21L, 22L, 23L, 24L)

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
    est_hrs       = safe_num(COL_EST_HRS),
    salmon_harvest = purrr::pmap_dbl(
      purrr::map(HARVEST_COLS, safe_num) |> purrr::set_names(paste0("h", HARVEST_COLS)),
      ~ sum(c(...), na.rm = TRUE)
    )
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

  effort_cols <- c("est_hrs", "est_ang", "sampled_ang", "salmon_harvest")

  monthly <- purrr::pmap_dfr(weekly, function(...) {
    row <- list(...)
    prorate_week_to_months(row, effort_cols)
  }) |>
    dplyr::group_by(year, month) |>
    dplyr::summarize(
      total_effort_hrs         = sum(est_hrs,       na.rm = TRUE),
      total_trips_est          = sum(est_ang,       na.rm = TRUE),
      n_completed_angler_trips = sum(sampled_ang,   na.rm = TRUE),
      total_salmon_harvest     = sum(salmon_harvest, na.rm = TRUE),
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
      fishery_name      = fishery_label,
      crc_area          = CRC_AREA_LUT[["mcnary"]],
      # Combined bank+boat — no mode split in Print sheet (OQ5)
      angler_final      = "combined",
      sd                = NA_real_,
      pe_period         = "week",
      harvest_expansion = "expanded",
      data_provider     = "Todd Miller / R-district weekly summary",
      district          = "R3"
    )

  build_target_schema(monthly)
}


# 5. Target schema enforcer ---------------------------------------------------

# Required column order matches multi_fishery_creel_trips.csv plus
# the `data_provider` provenance column.
TARGET_COLS <- c(
  "fishery_name", "year", "month", "crc_area", "angler_final",
  "total_effort_hrs", "n_completed_angler_trips", "mean_trip_length",
  "mean_group_size", "sd", "total_trips_est", "pe_period",
  "total_salmon_harvest", "harvest_expansion", "data_provider", "district"
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


# 6. Snake River (R1) annual angler-trip parser -------------------------------

# The Snake River in WA spans six CRC areas (Ice Harbor Dam up to the WA/ID
# state line; see input_files/pst/lookup_tables/crc_area_lut.csv). The R1
# source file gives one fleet-wide total per species-year with no area-level
# breakdown, so crc_area is set to NA in ingest_snake_river(), mirroring how
# ingest_hanford_boat() handles the same composite-area problem for Hanford
# Reach. The six constituent areas are documented here for reference only.
#
# Per Jeremy Trump (2026-08-20): the creel's open zones are a SUBSET of each
# CRC area, not the whole area, and a different subset by season - spring
# Chinook covers a portion of 640 and 644; fall Chinook a portion of 644 and
# 648; 642/646/650 have no creel presence in either season. Confirmed clean,
# not a mismatch: harvest can only occur where fishing is legally open, so
# CRC harvest for 640/644 (spring) or 644/648 (fall) reflects the same water
# the creel already covers completely - the CRC area code being coarser than
# the open zone doesn't matter, since the closed portion of the area
# contributes no harvest to miss. What's still missing is a per-area (or at
# least per-season area-pair) TRIP breakdown from Jeremy - the R1 summary
# gives one combined total per species-year, not split by which of that
# season's open areas it came from - so a season-specific Snake-River-
# internal trips/CRC-harvest ratio (matching combined area harvest to
# Jeremy's combined trip total) is feasible in principle but not yet built;
# see pst_river_block_crosswalk.csv's Snake River notes and
# analysis/pst/03_analysis/_22_status_and_gaps.qmd.
SNAKE_CRC_AREAS <- c(640L, 642L, 644L, 646L, 648L, 650L)  # not used as crc_area

#' Ingest the R1 Snake River annual angler-trip workbook.
#'
#' Source: Jeremy Trump (WDFW Region 1). Unlike the R3 workbooks above, this
#' is a single small annual summary, not a weekly creel model — one row per
#' species x year, already split into boat and shore (bank) components. There
#' is no effort-hours column, no harvest column, and no sampled/expanded
#' distinction to reconstruct; these are finished trip totals, not raw counts
#' to expand.
#'
#' LABEL NOTE: the source workbook's own column headers say "Angler Days"
#' ("Boat Angler Days", "Shore Angler Days", "Total Angler Days"). Per
#' Jeremy Trump, these values are angler TRIPS — the workbook's column
#' headers are simply mislabeled. This function copies the values into
#' total_trips_est unchanged; NO trips<->days conversion is applied anywhere
#' in this pipeline (see the UNITS NOTE in pst_fw_angler_trips_assembly.R).
#' Reconfirm this reading with Jeremy before the deliverable ships — if the
#' numbers really are angler-days for some species/year, they must NOT be
#' row-bound with everything else here as if they were trips.
#'
#' CROSSWALK GAP: RESOLVED 2026-08-19. pst_river_block_crosswalk.csv now has
#' six source_id = "R1_external" rows (river_label = "Snake River",
#' block = "ColumbiaSnake" — its own region-derived block, split from the
#' single "ColumbiaTrib" the same day, since Snake River geographically and
#' administratively has nothing to do with Hanford/Yakima/McNary's
#' Columbia - Upper region), one per fishery_name, area_coverage =
#' "covered_unpartitioned" since this function's totals have no per-CRC-area
#' breakdown (crc_area is NA - see SNAKE_CRC_AREAS below).
#'
#' DATA QUALITY: the source workbook flags its own Fall CH 2023 row with an
#' inline note — Boat (4827) + Shore (173) = 5000, but the Total Angler Days
#' column shows 4990. This function uses the Boat and Shore columns directly
#' (never Total), so the discrepancy does not propagate into our output, but
#' is logged as a warning for traceability.
#'
#' @param path  Full path to the xlsx file.
#' @return A data frame in the target schema — one row per species x year x
#'   angler_final (boat/bank) with a usable value. Rows where the source is
#'   blank or "N/A" are dropped, not coded as zero (Fall CH 2022 is all
#'   "N/A"). Fall CH 2025 was a blank row as of 2026-08-18 but is now
#'   populated in the 2026-08-20 refresh from Jeremy Trump / Mike Gembala —
#'   see the matching new crosswalk row added the same day.
ingest_snake_river <- function(path) {

  cli::cli_alert_info("Ingesting Snake River (R1): {.path {basename(path)}}")

  raw <- suppressMessages(readxl::read_excel(
    path, sheet = 1, col_names = FALSE, .name_repair = "unique"
  ))

  # Column layout (1-indexed, confirmed from the 2022–2025 workbook):
  #   row 1: title.  row 2: blank.
  #   row 3: header ("Species", "Year", "Boat Angler Days", "Shore Angler
  #          Days", "Total Angler Days").  rows 4+: data.
  #   col 1: blank.  col 2: Species.  col 3: Year.
  #   col 4: Boat Angler Days.  col 5: Shore Angler Days.
  #   col 6: Total Angler Days (cross-check only — never read into output).
  COL_SPECIES <- 2L
  COL_YEAR    <- 3L
  COL_BOAT    <- 4L
  COL_SHORE   <- 5L
  COL_TOTAL   <- 6L

  species_col <- as.character(unlist(raw[[COL_SPECIES]]))
  is_data_row <- !is.na(species_col) & species_col %in% c("Spring CH", "Fall CH")

  if (!any(is_data_row)) {
    cli::cli_abort("No data rows found in {.path {basename(path)}}")
  }

  safe_num <- function(col_idx) {
    # "N/A" (and any other non-numeric text) becomes NA via suppressWarnings,
    # not zero — a fishery-year with unusable data must not read as "no
    # trips" [R2].
    suppressWarnings(as.numeric(unlist(raw[[col_idx]])[is_data_row]))
  }

  SPECIES_LABEL <- c("Spring CH" = "spring Chinook", "Fall CH" = "fall Chinook")

  annual <- tibble::tibble(
    species     = species_col[is_data_row],
    year        = as.integer(unlist(raw[[COL_YEAR]])[is_data_row]),
    boat_trips  = safe_num(COL_BOAT),
    shore_trips = safe_num(COL_SHORE),
    total_trips = safe_num(COL_TOTAL)
  )

  # Cross-check, not a correction: the source's own Total column sometimes
  # disagrees with Boat + Shore. We never use Total; Boat and Shore are
  # authoritative here. Log every year the two disagree so the discrepancy
  # stays visible rather than silently resolved one way or the other.
  mismatch <- annual |>
    dplyr::filter(!is.na(boat_trips), !is.na(shore_trips), !is.na(total_trips),
                  (boat_trips + shore_trips) != total_trips)
  if (nrow(mismatch) > 0) {
    purrr::pwalk(mismatch, function(species, year, boat_trips, shore_trips,
                                    total_trips, ...)
      cli::cli_alert_warning(
        "Snake River {species} {year}: Boat ({boat_trips}) + Shore \\
         ({shore_trips}) = {boat_trips + shore_trips}, but source Total \\
         column shows {total_trips}. Using Boat/Shore; Total column ignored."
      ))
  }

  # Long: one row per species x year x angler_final, dropping N/A / blank
  # cells rather than coding them zero.
  long <- dplyr::bind_rows(
    annual |> dplyr::transmute(species, year, angler_final = "boat",
                               total_trips_est = boat_trips),
    annual |> dplyr::transmute(species, year, angler_final = "bank",
                               total_trips_est = shore_trips)
  ) |>
    dplyr::filter(!is.na(total_trips_est))

  dropped <- annual |>
    dplyr::filter(is.na(boat_trips) & is.na(shore_trips))
  if (nrow(dropped) > 0) {
    purrr::pwalk(dropped, function(species, year, ...)
      cli::cli_alert_warning(
        "Snake River {species} {year}: no usable Boat or Shore value \\
         (source is blank or 'N/A') — dropped, not coded as zero."
      ))
  }

  long |>
    dplyr::mutate(
      fishery_name      = sprintf("Snake River %s %d",
                                  SPECIES_LABEL[species], year),
      month             = NA_integer_,   # annual grain, no month breakdown
      crc_area          = NA_integer_,   # composite; see SNAKE_CRC_AREAS
      total_effort_hrs  = NA_real_,      # not provided in source
      n_completed_angler_trips = NA_real_,
      mean_trip_length  = NA_real_,
      mean_group_size   = NA_real_,
      sd                = NA_real_,
      pe_period         = "year",
      total_salmon_harvest = NA_real_,   # source has no harvest column
      harvest_expansion = NA_character_, # not applicable — trips only
      data_provider     = "Jeremy Trump / Mike Gembala / R1 annual angler-trip summary",
      district          = "R1"
    ) |>
    build_target_schema()
}


# 6b. Green/Duwamish (R4) interview + effort parser ---------------------------

# Unlike R1/R3, R4 does not hand us a finished trip total. We derive
# total_trips_est ourselves from two paired workbooks:
#   - Raw Interviews: individual creel interviews, one sheet per year (two
#     for 2025, split Aug-Oct / Nov-Dec) — used ONLY to compute mean
#     angler-hours per angler-trip ("mean_trip_length") from COMPLETED
#     trips.
#   - Estimated Anglers ("Creel Survey" sheet): boat/shore ANGLER HOURS
#     totals by year, already expanded by R4 — trusted as total_effort_hrs.
#     Also carries an ANGLERS block that looks like it could be a trip
#     count; it is deliberately NOT used as one — see
#     ingest_green_duwamish()'s docstring for why — only as a cross-check
#     against what we derive.
#
#     total_trips_est = total_effort_hrs / mean_trip_length
#
# Green/Duwamish is CRC area 746, a single non-composite area (unlike
# Hanford or Snake River) per crc_area_lut.csv ("Green/Duwamish River
# (King Co.)"). No per-section split is applied even though the raw
# interviews carry Location/MA codes — the whole survey area (mouth to
# Hwy 18, expanded to Flaming Geyser SP in 2025) sits inside CRC 746.

GREEN_DUWAMISH_CRC_AREA <- 746L

# Each Raw Interviews sheet has its own column layout AND, in one case, a
# misleadingly-named column. The one invariant across all five sheets:
# there IS a column that is already angler-hours (a per-interview total
# already multiplied by #Angler), just under a different name/position
# each time. No sheet needs an angler-hours DERIVATION — 2022's "Total
# Time Fished" and 2024/2025's "Angler Hours" are self-explanatory, but
# 2023's "Duration (hrs)" is ALSO already angler-hours despite the header
# implying per-trip length: verified against Start/End timestamps
# (Duration (hrs) == (End-Start span) x #Angler for every one of 754
# checked rows, 2026-08-21). Treating it as per-trip length and dividing
# by #Angler again would double-count group size and inflate 2023 boat
# trip length ~2.7x — caught by the cross-check against the workbook's own
# ANGLERS block before this was fixed.
#
# Columns given as 1-indexed positions (sheet read with col_names = FALSE,
# matching the R3 parsers above) rather than names, because 2022 and 2023
# each carry trailing junk/summary rows below the real data (43 rows in
# 2022, 1 in 2023) that would otherwise contaminate readxl's per-column
# type guessing under col_names = TRUE.
GD_SHEET_MAP <- list(
  list(sheet = "2022", year = 2022L,
       col_date = 1L, col_mode = 6L, col_n = 7L, col_ah = 12L, col_ct = 13L),
  list(sheet = "2023", year = 2023L,
       col_date = 2L, col_mode = 7L, col_n = 8L, col_ah = 11L, col_ct = 12L),
  list(sheet = "2024", year = 2024L,
       col_date = 1L, col_mode = 8L, col_n = 9L, col_ah = 14L, col_ct = 15L),
  list(sheet = "2025 (Aug-Oct)", year = 2025L,
       col_date = 1L, col_mode = 8L, col_n = 9L, col_ah = 14L, col_ct = 15L),
  list(sheet = "2025 (Nov-Dec)", year = 2025L,
       col_date = 1L, col_mode = 8L, col_n = 9L, col_ah = 14L, col_ct = 15L)
)

#' Read and filter one Raw Interviews sheet to completed-trip angler-hours.
#'
#' Filters to rows with a real date (Excel serial > 40000, same threshold
#' used by ingest_hanford_boat() — this is what drops the trailing
#' summary/junk blocks) and to completed trips only (CT / Completed Trip ==
#' "y", case-insensitive; an interview taken mid-trip is not a valid
#' trip-length sample). Also drops rows with negative angler-hours (one bad
#' Start/End entry found in 2023), logging how many were dropped.
gd_read_interview_sheet <- function(path, spec) {
  raw <- suppressMessages(readxl::read_excel(
    path, sheet = spec$sheet, col_names = FALSE, .name_repair = "unique"
  ))

  date_serial   <- suppressWarnings(as.numeric(unlist(raw[[spec$col_date]])))
  is_valid_date <- !is.na(date_serial) & date_serial > 40000

  mode_raw <- tolower(trimws(as.character(unlist(raw[[spec$col_mode]]))))
  mode <- dplyr::case_when(
    mode_raw == "b" ~ "boat",
    mode_raw == "s" ~ "bank",
    TRUE ~ NA_character_
  )
  ct <- tolower(trimws(as.character(unlist(raw[[spec$col_ct]])))) == "y"

  n_anglers    <- suppressWarnings(as.numeric(unlist(raw[[spec$col_n]])))
  angler_hours <- suppressWarnings(as.numeric(unlist(raw[[spec$col_ah]])))

  base_keep <- is_valid_date & !is.na(ct) & ct & !is.na(mode) &
    !is.na(n_anglers) & !is.na(angler_hours)

  n_dropped_negative <- sum(base_keep & angler_hours < 0)
  if (n_dropped_negative > 0) {
    cli::cli_alert_warning(
      "Green-Duwamish {spec$sheet}: {n_dropped_negative} completed-trip \\
       row(s) with negative angler-hours (bad Start/End entry) dropped."
    )
  }

  keep <- base_keep & angler_hours >= 0

  tibble::tibble(
    year         = spec$year,
    mode         = mode[keep],
    n_anglers    = n_anglers[keep],
    angler_hours = angler_hours[keep]
  )
}

#' Ingest all five Raw Interviews sheets and compute annual mean trip length
#' per mode (boat/bank), pooled across sub-periods within a year — both 2025
#' sheets (Aug-Oct, Nov-Dec) combine into one 2025 estimate per Evan's call
#' 2026-08-21, rather than being kept as separate period-level estimates.
#'
#' @param path  Full path to the Raw Interviews workbook.
#' @return A tibble: year, mode, n_completed_angler_trips, mean_trip_length.
ingest_green_duwamish_interviews <- function(path) {

  cli::cli_alert_info("Ingesting Green-Duwamish interviews: {.path {basename(path)}}")

  purrr::map_dfr(GD_SHEET_MAP, ~ gd_read_interview_sheet(path, .x)) |>
    dplyr::group_by(year, mode) |>
    dplyr::summarize(
      n_completed_angler_trips = sum(n_anglers),
      sample_angler_hours      = sum(angler_hours),
      mean_trip_length         = sample_angler_hours / n_completed_angler_trips,
      .groups = "drop"
    )
}

#' Ingest the Estimated Anglers ("Creel Survey") workbook: the ANGLER HOURS
#' block (trusted as total_effort_hrs) and the ANGLERS block (cross-check
#' only — see ingest_green_duwamish()'s docstring).
#'
#' Sheet layout (1-indexed, col_names = FALSE): col 1 is a row label (block
#' header text, or a year for data rows); cols 2-3 are Boat/Shore for the
#' Aug20-Oct31 period; cols 6-7 are Boat/Shore for the Nov1-Dec31 period
#' (the cell reads "Not surveyed" for years without a second period —
#' coerced to NA, not 0, so it doesn't zero out the sum); cols 10-11 are a
#' Boat/Shore "Total" column that the workbook only actually populates for
#' 2025 (blank formulas for 2022-2024). total_effort_hrs is therefore
#' always computed here as period1 + period2 ourselves, never read from
#' that Total column; where it IS populated (2025) it's used only as an
#' internal cross-check, logged if it disagrees.
#'
#' @param path  Full path to the Estimated Anglers workbook.
#' @return A tibble: year, mode, total_effort_hrs, angler_est (cross-check).
ingest_green_duwamish_effort <- function(path) {

  cli::cli_alert_info("Ingesting Green-Duwamish effort estimates: {.path {basename(path)}}")

  raw  <- suppressMessages(readxl::read_excel(
    path, sheet = "Creel Survey", col_names = FALSE, .name_repair = "unique"
  ))
  col1 <- as.character(unlist(raw[[1]]))

  parse_block <- function(label) {
    start_row <- which(col1 == label)
    if (length(start_row) == 0) {
      cli::cli_abort("No '{label}' block found in Creel Survey sheet.")
    }
    start_row <- start_row[1]
    safe_num  <- function(x) suppressWarnings(as.numeric(x))

    rows <- list()
    r <- start_row + 1L
    while (r <= nrow(raw) && !is.na(suppressWarnings(as.integer(col1[r])))) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        year      = as.integer(col1[r]),
        p1_boat   = safe_num(raw[[2]][r]),  p1_shore  = safe_num(raw[[3]][r]),
        p2_boat   = safe_num(raw[[6]][r]),  p2_shore  = safe_num(raw[[7]][r]),
        tot_boat  = safe_num(raw[[10]][r]), tot_shore = safe_num(raw[[11]][r])
      )
      r <- r + 1L
    }
    dplyr::bind_rows(rows) |>
      dplyr::mutate(
        boat  = dplyr::coalesce(p1_boat, 0)  + dplyr::coalesce(p2_boat, 0),
        shore = dplyr::coalesce(p1_shore, 0) + dplyr::coalesce(p2_shore, 0)
      )
  }

  angler_hours <- parse_block("ANGLER HOURS")
  anglers      <- parse_block("ANGLERS")

  mismatch <- angler_hours |>
    dplyr::filter(!is.na(tot_boat),
                  abs(boat - tot_boat) > 0.5 | abs(shore - tot_shore) > 0.5)
  if (nrow(mismatch) > 0) {
    cli::cli_alert_warning(
      "Green-Duwamish ANGLER HOURS: {nrow(mismatch)} year(s) where the \\
       workbook's own Total column disagrees with period1+period2. Using \\
       the computed sum; Total column ignored."
    )
  }

  dplyr::bind_rows(
    angler_hours |> dplyr::transmute(year, mode = "boat", total_effort_hrs = boat),
    angler_hours |> dplyr::transmute(year, mode = "bank", total_effort_hrs = shore),
    anglers      |> dplyr::transmute(year, mode = "boat", angler_est = boat),
    anglers      |> dplyr::transmute(year, mode = "bank", angler_est = shore)
  ) |>
    dplyr::group_by(year, mode) |>
    dplyr::summarize(
      total_effort_hrs = sum(total_effort_hrs, na.rm = TRUE),
      angler_est        = sum(angler_est, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Combine Green-Duwamish interview-derived trip length with R4's own
#' effort-hours estimate to derive total_trips_est.
#'
#' total_trips_est = total_effort_hrs / mean_trip_length. The workbook's own
#' ANGLERS block (angler_est) is deliberately NOT used as total_trips_est —
#' Evan's call 2026-08-21 was to derive trips from effort divided by
#' interview-based trip length rather than trust a source-provided count of
#' uncertain accounting basis, unlike R1/R3 where the district hands us a
#' finished trip total directly (see MD1 vs. the R4-specific deviation note
#' in _22_status_and_gaps.qmd). angler_est is carried through only as a
#' cross-check, printed below: derived trips landed within ~2-9% of
#' angler_est across all 8 year x mode combinations in the 2026-08-21 run,
#' boat consistently a little low — reasonable agreement for two
#' independently-derived quantities, not proof they should match exactly.
#'
#' @param interviews_path  Full path to the Raw Interviews workbook.
#' @param effort_path      Full path to the Estimated Anglers workbook.
#' @return A data frame in the target schema.
ingest_green_duwamish <- function(interviews_path, effort_path) {

  trip_length <- ingest_green_duwamish_interviews(interviews_path)
  effort      <- ingest_green_duwamish_effort(effort_path)

  combined <- effort |>
    dplyr::left_join(trip_length, by = c("year", "mode"))

  missing_trip_length <- combined |> dplyr::filter(is.na(mean_trip_length))
  if (nrow(missing_trip_length) > 0) {
    purrr::pwalk(missing_trip_length, function(year, mode, ...)
      cli::cli_alert_warning(
        "Green-Duwamish {year} {mode}: no completed-trip interviews found — \\
         total_trips_est cannot be derived, left NA."
      ))
  }

  combined <- combined |>
    dplyr::mutate(total_trips_est = total_effort_hrs / mean_trip_length)

  cli::cli_h3("Green-Duwamish: derived trips vs. workbook's own ANGLERS cross-check")
  combined |>
    dplyr::mutate(pct_diff = round(100 * (total_trips_est - angler_est) / angler_est, 1)) |>
    dplyr::select(year, mode, total_effort_hrs, mean_trip_length,
                  total_trips_est, angler_est, pct_diff) |>
    print(n = 20)

  combined |>
    dplyr::mutate(
      fishery_name             = sprintf("Green-Duwamish salmon %d", year),
      month                    = NA_integer_,
      crc_area                 = GREEN_DUWAMISH_CRC_AREA,
      angler_final             = mode,
      mean_group_size          = NA_real_,
      sd                       = NA_real_,
      pe_period                = "year",
      total_salmon_harvest     = NA_real_,   # effort workbook has no harvest column
      harvest_expansion        = NA_character_,
      data_provider            = "Nathanael Overman / R4 creel interviews + effort estimates",
      district                 = "R4"
    ) |>
    build_target_schema()
}


# 6c. Upper Columbia (R2) annual angler-trip total ----------------------------

# Source: Chad Jackson (WDFW Region 2 Fish Program Manager), email to Evan
# Booher 2026-08-27, replying to outreach sent 2026-08-11 (see
# input_files/pst/R2_creel/R2_Angler_Trips_2022-2025.pdf for the full email
# chain, including Jim Scott's original 2026-06-09 data request). Unlike
# every other district source (R1/R3/R4), no workbook exists here at all -
# the data arrived as a four-row table pasted into an email body, captured
# as a PDF because there was nothing else to attach. This function therefore
# hardcodes the table rather than parsing a file; R2_PDF_PATH below exists
# only so the function can confirm its source document is still on disk
# before returning numbers attributed to it - if the file goes missing, that
# is a provenance problem worth erroring on loudly, not silently ignoring [R2].
#
# Chad's own footnote on "Upper Columbia": "the mainstem between Priest
# Rapids Dam and Chief Joseph Dam, Icicle River, Lake Wenatchee, and select
# mainstem tributaries that may or may not be open depending upon the year
# (Wenatchee, Entiat, Chelan, Okanogan, and Similkameen)." This is a SINGLE
# COMBINED total per year across that entire footprint - no river, mode
# (private/guided), or location (boat/bank) breakdown, the coarsest of any
# district source in this pipeline (R1/R3/R4 at least split boat vs. bank).
# angler_final is set to "combined" throughout, matching the precedent
# ingest_mcnary() already uses for an unsplittable bank+boat total (OQ5).
#
# SCOPE MISMATCH — logged as an open gap in _22_status_and_gaps.qmd, not
# resolved here. Chad's footprint does not line up exactly with
# pst_river_block_crosswalk.csv's existing ColumbiaUpper CRC_only rows for
# this district:
#   - Methow River (621) is in our crosswalk but ABSENT from Chad's footnote.
#   - Chelan (CRC 552, confirmed via crc_area_lut.csv - "Chelan River") is in
#     Chad's footnote but has NO crosswalk row at all; never added.
#   - Chad's mainstem boundary stops at Chief Joseph Dam; the crosswalk's
#     existing mainstem CRC_only row spans Priest Rapids all the way to
#     Grand Coulee (7 areas: 537|539|541|543|545|547|549). Per
#     crc_area_lut.csv, 537/539/541/543/545 fall between Priest Rapids and
#     Chief Joseph Dam (matches Chad's stated boundary exactly); 547 and 549
#     (Roosevelt Lake, i.e. above Grand Coulee) are further upstream than
#     Chad's own definition claims to cover.
# The new R2_external crosswalk rows mark ONLY the unambiguous subset
# (Entiat 586, Okanogan 627, Similkameen 629, Lake Wenatchee 670, Icicle
# Creek 672, Wenatchee River 674, and mainstem 537|539|541|543|545) as
# covered_unpartitioned. Methow (621) and mainstem 547|549 are left as-is
# (still CRC_only, still eligible for independent P2 expansion) rather than
# guessed into either "covered" (risking a silent double-count if Chad's
# number secretly includes them) or "excluded" (risking a silently discarded
# real signal if it doesn't). Chelan (552) has no crosswalk row to mark
# either way and stays invisible to P2 until one is added.
#
# UNITS: the table's own column header is "ANGLER TRIPS" (matches this
# pipeline's own unit) - unlike Jeremy Trump's R1 workbook, no trips-vs-days
# relabeling question here. Per Evan's instruction (2026-08-28), treated as
# PST-scope salmon trips per the original request (Jim Scott's email: "the
# U.S. has contracted...to assess the economic value of PST salmon...
# fisheries"); Chad's reply does not itself reconfirm the species scope, so
# this is an assumption carried from the original request, not an explicit
# confirmation from Chad — logged as an open gap.
R2_PDF_PATH <- here("input_files", "pst", "R2_creel", "R2_Angler_Trips_2022-2025.pdf")
R2_UPPER_COLUMBIA_CRC_AREAS <- c(586L, 627L, 629L, 670L, 672L, 674L,
                                 537L, 539L, 541L, 543L, 545L)  # documented; not used as crc_area

#' Return Chad Jackson's (R2) Upper Columbia annual angler-trip totals.
#'
#' Hardcoded, not parsed — see the section header above for why. Confirms
#' the source PDF is still present before returning anything, so a moved or
#' deleted source document surfaces as a loud failure rather than orphaned
#' numbers with no traceable origin.
#'
#' @return A data frame in the target schema, one row per year 2022-2025.
ingest_r2_upper_columbia <- function() {

  if (!file.exists(R2_PDF_PATH)) {
    cli::cli_abort(
      "R2 source document not found at {.path {R2_PDF_PATH}} - the ",
      "hardcoded totals in ingest_r2_upper_columbia() are attributed to ",
      "this file. Restore it or update this function's provenance."
    )
  }

  cli::cli_alert_info(
    "Ingesting Upper Columbia (R2): hardcoded from {.path {basename(R2_PDF_PATH)}}"
  )

  tibble::tibble(
    year            = 2022L:2025L,
    total_trips_est = c(55036, 53647, 55868, 19770)
  ) |>
    dplyr::mutate(
      fishery_name             = sprintf("Upper Columbia salmon %d", year),
      month                    = NA_integer_,   # annual grain, no month breakdown
      crc_area                 = NA_integer_,   # composite; see R2_UPPER_COLUMBIA_CRC_AREAS
      angler_final             = "combined",    # no boat/bank split in source
      total_effort_hrs         = NA_real_,      # not provided in source
      n_completed_angler_trips = NA_real_,
      mean_trip_length         = NA_real_,
      mean_group_size          = NA_real_,
      sd                       = NA_real_,
      pe_period                = "year",
      total_salmon_harvest     = NA_real_,      # source has no harvest column
      harvest_expansion        = NA_character_, # not applicable — trips only
      data_provider            = "Chad Jackson / WDFW Region 2, email 2026-08-27",
      district                 = "R2"
    ) |>
    build_target_schema()
}


# 7. Combining wrapper --------------------------------------------------------

#' Discover and ingest all R3 mid-Columbia/Yakima workbooks for a given set
#' of years.
#'
#' Matches filenames by pattern:
#'   "^<year> Hanford Reach Boat Harvest Model"
#'   "^<year>[ ]+Yakima (Fall|River Fall)"
#'   "^<year> McNary Reservoir Harvest Model"
#'
#' @param dir       Directory containing the xlsx files.
#' @param years     Integer vector of target years.
#' @return A data frame with all successfully ingested records.
ingest_r3_mid_columbia <- function(dir = R3_DIR, years = TARGET_YEARS) {

  cli::cli_h2("R3 mid-Columbia/Yakima creel workbook ingestion")

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
    cli::cli_abort("No R3 workbooks were successfully ingested.")
  }

  combined <- dplyr::bind_rows(results)
  cli::cli_alert_success(
    "R3: ingested {nrow(combined)} rows across \\
     {dplyr::n_distinct(combined$fishery_name)} fisheries."
  )
  combined
}


# 8. Top-level wrapper: all districts -----------------------------------------

#' Discover and ingest every district-supplied creel source this script knows
#' how to handle (R3, R1, R4, R2).
#'
#' @return A data frame with all successfully ingested records across every
#'   district, each row carrying `district`.
ingest_district_creel_files <- function() {

  cli::cli_h1("District-supplied creel workbook ingestion")

  results <- list()

  r3 <- tryCatch(
    ingest_r3_mid_columbia(),
    error = function(e) {
      cli::cli_alert_danger("R3 ingestion failed: {conditionMessage(e)}")
      NULL
    }
  )
  if (!is.null(r3)) results <- c(results, list(r3))

  r1_files <- list.files(R1_DIR, pattern = "\\.xlsx$", full.names = TRUE,
                        ignore.case = TRUE)
  if (length(r1_files) == 0) {
    cli::cli_alert_warning("No R1 xlsx files found in {.path {R1_DIR}}")
  } else {
    if (length(r1_files) > 1) {
      cli::cli_alert_warning(
        "Multiple R1 files found; using first: \\
         {.path {basename(r1_files[1])}}"
      )
    }
    r1 <- tryCatch(
      ingest_snake_river(r1_files[1]),
      error = function(e) {
        cli::cli_alert_danger("R1 (Snake River) ingestion failed: {conditionMessage(e)}")
        NULL
      }
    )
    if (!is.null(r1)) results <- c(results, list(r1))
  }

  r4_files <- list.files(R4_DIR, pattern = "\\.xlsx$", full.names = TRUE,
                        ignore.case = TRUE)
  interviews_file <- r4_files[grepl("Raw Interviews", basename(r4_files),
                                    ignore.case = TRUE)]
  effort_file     <- r4_files[grepl("Estimated Anglers", basename(r4_files),
                                    ignore.case = TRUE)]
  if (length(interviews_file) == 0 || length(effort_file) == 0) {
    cli::cli_alert_warning(
      "R4 (Green-Duwamish): expected both a 'Raw Interviews' and an \\
       'Estimated Anglers' workbook in {.path {R4_DIR}}; found \\
       {length(interviews_file)} and {length(effort_file)} respectively. \\
       Skipping."
    )
  } else {
    r4 <- tryCatch(
      ingest_green_duwamish(interviews_file[1], effort_file[1]),
      error = function(e) {
        cli::cli_alert_danger("R4 (Green-Duwamish) ingestion failed: {conditionMessage(e)}")
        NULL
      }
    )
    if (!is.null(r4)) results <- c(results, list(r4))
  }

  # R2 — Upper Columbia (Chad Jackson), received 2026-08-28 as a PDF, not a
  # workbook. ingest_r2_upper_columbia() hardcodes the four-row table it
  # contains — see that function's header for the real scope-mismatch gaps
  # this data raises (Methow/Chelan/mainstem-boundary).
  r2 <- tryCatch(
    ingest_r2_upper_columbia(),
    error = function(e) {
      cli::cli_alert_danger("R2 (Upper Columbia) ingestion failed: {conditionMessage(e)}")
      NULL
    }
  )
  if (!is.null(r2)) results <- c(results, list(r2))

  if (length(results) == 0) {
    cli::cli_abort("No district workbooks were successfully ingested.")
  }

  combined <- dplyr::bind_rows(results)
  cli::cli_alert_success(
    "Ingested {nrow(combined)} rows across \\
     {dplyr::n_distinct(combined$fishery_name)} fisheries, \\
     {dplyr::n_distinct(combined$district)} district(s): \\
     {paste(sort(unique(combined$district)), collapse = ', ')}."
  )
  combined
}


# 9. Run and save output -------------------------------------------------------

district_creel <- ingest_district_creel_files()

out_dir  <- here("analysis", "pst", "outputs", "03_district_creel")
out_path <- file.path(out_dir, "district_creel_summary.csv")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(district_creel, out_path, row.names = FALSE)

cli::cli_alert_success(
  "Saved {nrow(district_creel)} rows to {.path {out_path}}"
)

# Preview
cli::cli_h3("Row counts by fishery")
district_creel |>
  dplyr::count(district, fishery_name, angler_final) |>
  print(n = 40)

cli::cli_h3("Rows with NA total_trips_est (review before analysis)")
district_creel |>
  dplyr::filter(is.na(total_trips_est)) |>
  dplyr::count(district, fishery_name) |>
  print(n = 20)
