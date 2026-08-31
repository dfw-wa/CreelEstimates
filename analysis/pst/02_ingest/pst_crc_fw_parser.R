# ==============================================================================
# pst_crc_fw_parser.R
# Location: analysis/pst/02_ingest/pst_crc_fw_parser.R
#
# Shared parser for the "Salmon Freshwater Estimates" CRC workbook family.
# Extracted 2026-08-31 from parse_crc_freshwater_harvest.R so a second,
# distinct ingestion step (parse_crc_creel_subs_final.R, the "final"/creel-
# substituted 2022-2024 workbooks under input_files/pst/CRC/crc_edits_creel_subs/)
# can reuse the exact same layout-parsing logic instead of duplicating it -
# both file sets share the Region | System | Stream | Code | Species + 12
# monthly columns layout (Layout A or B, see parse_fw_workbook()'s header).
#
# This file defines functions and constants only - it has no top-level
# side effects (no file reads, no output writes). Source it; don't run it.
# ==============================================================================

library(tidyverse)
library(readxl)
library(openxlsx)
library(glue)
library(here)
library(cli)

#' Read a raw, unheadered sheet, falling back from readxl to openxlsx.
#' See parse_crc_freshwater_harvest.R's "Reader note" for why: readxl throws
#' an internal C++ range-check error on the 2017 workbook specifically, even
#' though the file is not corrupt (openpyxl and openxlsx both read it fine).
#' Returned shape matches read_excel(col_names = FALSE) closely enough for
#' every downstream positional (`raw[[i]]`, `raw[row, ]`) access in
#' parse_fw_workbook() to work unmodified against either source.
read_fw_raw <- function(path, sheet) {
  tryCatch({
    read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  }, error = function(e) {
    cli::cli_alert_warning(
      "readxl failed for {basename(path)} ({conditionMessage(e)}) - falling \\
       back to openxlsx."
    )
    raw_df <- openxlsx::read.xlsx(path, sheet = sheet, colNames = FALSE,
                                  skipEmptyRows = FALSE)
    tibble::as_tibble(raw_df, .name_repair = "minimal")
  })
}

# License year runs Apr of the named year through Mar of the following year.
# Map month abbreviations to (calendar_year_offset, calendar_month) where
# offset 0 = named year (Apr–Dec) and offset 1 = following year (Jan–Mar).
MONTH_MAP <- tibble(
  month_abbr     = c("Apr", "May", "Jun", "Jul", "Aug", "Sep",
                     "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
  calendar_month = c(  4L,    5L,    6L,    7L,    8L,    9L,
                      10L,   11L,   12L,    1L,    2L,    3L),
  yr_offset      = c(  0L,    0L,    0L,    0L,    0L,    0L,
                       0L,    0L,    0L,    1L,    1L,    1L)
)

# Species that are valid leaf-level rows (not subtotals).
# Jack variants are retained as distinct species — do NOT merge with adults.
VALID_SPECIES <- c("Chinook", "Chum", "Pink", "Coho",
                   "Sockeye", "Jackchin", "Jackcoho")


# 1. Parser -------------------------------------------------------------------
# Returns a list with:
#   $leaf      — tidy long-format leaf rows (species != "Total")
#   $subtotals — stream-species Total rows (for validation check 1)
#   $grandtots — "Total - All Areas" rows (for validation check 2)
#
#' @param subdir  Path component under input_files/pst/CRC/ where `filename`
#'   lives, e.g. "" (default, the main 15-year manifest) or
#'   "crc_edits_creel_subs" (the final/creel-substituted 2022-2024 release).
#'   Built via do.call(here, ...) rather than here(..., subdir, ...) directly -
#'   here() returns character(0) for the WHOLE call if any one argument is
#'   character(0) or "", not just that segment, so an empty subdir has to be
#'   filtered out of the parts vector before the call, not passed through.
parse_fw_workbook <- function(license_year, filename, sheet, subdir = "") {
  path_parts <- c("input_files", "pst", "CRC", subdir, filename)
  path <- do.call(here, as.list(path_parts[nzchar(path_parts)]))

  cli::cli_h2(glue("Parsing license year {license_year} — {basename(path)} / {sheet}"))

  # Read without column names; everything as text to preserve merged cells.
  # Falls back readxl -> openxlsx automatically for the one file that needs
  # it (2017) - see read_fw_raw()'s own header note.
  raw <- read_fw_raw(path, sheet)

  # ---- Locate header rows ----
  # Two layouts exist across these workbooks:
  #
  #  Layout A (2021/2022 Draft files):
  #    Row 5 = Region | System | Stream | Code | Species | <year> | ... | Total
  #    Row 6 = month abbreviations (Apr–Dec | Jan–Mar)
  #    Row 7+ = data
  #
  #  Layout B (2023/2024 files):
  #    Row 3 = year labels
  #    Row 4 = month abbreviations
  #    Row 5 = Region | System | Stream | Code | Species (month cols are empty)
  #    Row 6+ = data
  #
  # In both cases "Region" appears in col 1 of a specific row. We find that
  # row, then probe one row above and one row below for month abbreviations.
  header_row <- which(
    str_detect(as.character(raw[[1]]), regex("^region$", ignore_case = TRUE))
  )
  if (length(header_row) == 0L) {
    cli::cli_abort("Could not find 'Region' header row in {filename} / {sheet}.")
  }
  header_row <- header_row[[1L]]

  # ---- Detect month row and data start row ----
  find_month_cols <- function(row_idx) {
    vals <- as.character(unlist(raw[row_idx, ]))
    which(vals %in% MONTH_MAP$month_abbr)
  }

  # Try one row above first (Layout B), then one row below (Layout A).
  month_row <- NULL
  for (candidate in c(header_row - 1L, header_row + 1L)) {
    if (candidate >= 1L && length(find_month_cols(candidate)) >= 12L) {
      month_row <- candidate
      break
    }
  }
  if (is.null(month_row)) {
    cli::cli_abort(
      "Could not find a row with 12 month abbreviations adjacent to the \\
       'Region' header row (row {header_row}) in {filename} / {sheet}."
    )
  }

  data_start_row <- max(header_row, month_row) + 1L  # skip both header and month rows

  # ---- Extract month abbreviation column indices ----
  month_abbrs_raw <- as.character(unlist(raw[month_row, ]))
  month_col_idx   <- which(month_abbrs_raw %in% MONTH_MAP$month_abbr)

  if (length(month_col_idx) < 12L) {
    cli::cli_abort(
      "Expected 12 month columns in {filename}; found {length(month_col_idx)}: \\
       {paste(month_abbrs_raw[month_col_idx], collapse = ', ')}."
    )
  }

  month_col_names <- month_abbrs_raw[month_col_idx]

  # ---- Build column name vector ----
  # Cols 1–5 = Region, System, Stream, Code, Species.  Then monthly cols.
  # The "Total" column header may appear in either the header row (Layout B) or
  # the month row (Layout A, where the header row carries year labels); scan both.
  header_vals    <- as.character(unlist(raw[header_row, ]))
  month_row_vals <- as.character(unlist(raw[month_row, ]))
  # Combine: use header_row label when not empty, else month_row label
  combined_labels <- ifelse(nchar(trimws(header_vals)) > 0, header_vals, month_row_vals)
  total_col_idx   <- which(str_detect(combined_labels, regex("^total$", ignore_case = TRUE)))
  total_col_idx   <- total_col_idx[total_col_idx > max(month_col_idx)]
  has_total_col   <- length(total_col_idx) > 0L

  # ---- Extract data rows ----
  data_rows <- raw[data_start_row:nrow(raw), ]

  # Assign working column names
  fixed_names <- c("region", "system", "stream", "stream_code", "species")
  month_rename <- setNames(as.character(month_col_idx), month_col_names)

  # Select only the 5 fixed cols + 12 month cols (+ optional Total col)
  keep_cols <- c(1:5, month_col_idx)
  if (has_total_col) keep_cols <- c(keep_cols, total_col_idx[[1L]])

  data_sub <- data_rows[, keep_cols, drop = FALSE]

  col_labels <- c(fixed_names,
                  month_col_names,
                  if (has_total_col) "row_total" else character(0))
  names(data_sub) <- col_labels

  # Coerce fixed cols to character; convert empty strings to NA so downstream
  # detections work on clean NA, not empty string.
  #
  # str_squish() (added 2026-08-31): source workbook cells with wrapped text
  # come through readxl with literal embedded \n/\r\n where Excel wrapped the
  # line - confirmed for "system": "Nooksack Samish R. System" exists in the
  # tidy CRC output as four distinct strings ("Nooksack Samish R. System",
  # "Nooksack Samish R.\nSystem", "Nooksack\r\nSamish R.\r\nSystem",
  # "Nooksack Samish\r\nR. System") purely from where Excel happened to wrap
  # that cell in different source rows. group_by(system) downstream
  # (estimate_block_ratios()'s system_year/system_pooled tiers) then treats
  # these as four unrelated systems instead of one, fragmenting what should
  # be a shared donor pool into thin slivers - confirmed as the direct cause
  # of Samish (CRC 816) failing P2's harvest-scale guardrail in 2022/2024:
  # system_year matched it to a lone 208-fish Nooksack North Fork row (one of
  # the four fragments) instead of the full multi-area Nooksack+Samish system,
  # producing a ~45x apparent extrapolation that doesn't exist once the
  # system name is one consistent string. str_squish() collapses all
  # whitespace runs (including newlines) to single spaces and trims ends -
  # applied to every fixed column, not just system, since the same wrapping
  # could affect stream/region/species text identically.
  data_sub <- data_sub |>
    mutate(across(all_of(fixed_names), as.character)) |>
    mutate(across(all_of(fixed_names), ~ na_if(str_squish(.), "")))

  # ---- Coerce monthly cols to numeric ----
  data_sub <- data_sub |>
    mutate(across(all_of(month_col_names),
                  ~ suppressWarnings(as.numeric(.)),
                  .names = "{.col}")) |>
    mutate(across(all_of(month_col_names), ~ replace_na(., 0L)))

  if (has_total_col) {
    data_sub <- data_sub |>
      mutate(row_total = suppressWarnings(as.numeric(row_total)),
             row_total = replace_na(row_total, 0L))
  }

  # ---- Drop skeleton empty rows (no species value at all) ----
  data_sub <- data_sub |>
    filter(!is.na(species), species != "")

  # ---- Step A: capture region from section-header rows BEFORE removing them ----
  # In Layout B (2023/2024), each region block begins with a row where
  #   col_A = "<region_name>"  col_B = "System"  col_E = "Species"
  # These rows carry the only occurrence of the region name; the actual data
  # rows that follow have NA in col_A. We must fill region down from these
  # section-header rows before dropping them, otherwise every data row in
  # every region except the first would inherit "Coastal" from the initial
  # header row of the file (Layout A files are unaffected: their data rows
  # already carry the region name directly).
  section_header_mask <- (
    str_detect(coalesce(data_sub$system,  ""), regex("^system$",  ignore_case = TRUE)) &
    str_detect(coalesce(data_sub$species, ""), regex("^species$", ignore_case = TRUE))
  )
  # Forward-fill region only (section headers supply the region; data rows below
  # them have NA and will inherit via fill-down).
  data_sub <- data_sub |>
    tidyr::fill(region, .direction = "down")
  # Normalise embedded newlines in region names (e.g. "Columbia\n- Lower")
  data_sub <- data_sub |>
    mutate(region = str_replace_all(region, "\\s*\n\\s*", " "))
  # Now remove the section-header rows — their region value has been propagated.
  data_sub <- data_sub[!section_header_mask, ]

  # ---- Step B: identify and remove grand-total rows BEFORE the subtotal loop --
  # "Total - All Areas:" is still visible in the (region-filled) region column.
  # It must be removed BEFORE the region-subtotal loop; otherwise the loop's
  # state machine stays in 'in_block' past the last regional subtotal block and
  # consumes the grand-total rows too (grand total rows all have system = NA).
  grandtot_mask <- str_detect(
    coalesce(data_sub$region, ""),
    regex("total.*all.*area|total\\s*-\\s*all", ignore_case = TRUE)
  )
  first_gt <- which(grandtot_mask)
  if (length(first_gt) > 0L) {
    grandtot_mask[first_gt[[1L]]:nrow(data_sub)] <- TRUE
  }
  grandtots_raw <- data_sub[grandtot_mask, ]
  data_sub      <- data_sub[!grandtot_mask, ]

  # ---- Step C: remove region-subtotal blocks BEFORE fill-down ---------------
  # Rows where col_B (system) = "Total" with no stream/code mark the START of
  # a per-region aggregate block. Each block spans multiple rows: the first row
  # has system = "Total"; the continuation rows (species = Chum, Coho, …, Total)
  # have system = NA and stream = NA. Only when a row has a non-null, non-"Total"
  # system value does the block end (i.e. the next real stream begins).
  #
  # We walk through the rows with a state flag to capture the full block:
  system_vals <- coalesce(data_sub$system, "")
  in_region_subtot <- logical(nrow(data_sub))
  in_block <- FALSE
  for (i in seq_len(nrow(data_sub))) {
    sv <- system_vals[i]
    if (grepl("^total$", sv, ignore.case = TRUE) && is.na(data_sub$stream[i])) {
      in_block <- TRUE
    } else if (sv != "" && !grepl("^total$", sv, ignore.case = TRUE)) {
      in_block <- FALSE     # non-null, non-"Total" system = start of real stream
    }
    in_region_subtot[i] <- in_block
  }

  region_subtots <- data_sub[in_region_subtot, ]
  data_sub       <- data_sub[!in_region_subtot, ]

  # ---- Step D: fill down remaining merged cells ----
  data_sub <- data_sub |>
    tidyr::fill(system, stream, stream_code, .direction = "down")

  # ---- Identify stream-species subtotal rows (species == "Total") ----
  subtot_mask <- str_detect(data_sub$species, regex("^total$", ignore_case = TRUE))
  subtotals   <- data_sub[subtot_mask, ]
  leaf        <- data_sub[!subtot_mask, ]

  # ---- Add license_year ----
  leaf           <- mutate(leaf,           license_year = license_year)
  subtotals      <- mutate(subtotals,      license_year = license_year)
  region_subtots <- mutate(region_subtots, license_year = license_year)
  grandtots_raw  <- mutate(grandtots_raw,  license_year = license_year)

  # ---- Pivot leaf to long format ----
  leaf_long <- leaf |>
    pivot_longer(
      cols      = all_of(month_col_names),
      names_to  = "month_abbr",
      values_to = "harvest_count"
    ) |>
    left_join(MONTH_MAP, by = "month_abbr") |>
    mutate(calendar_year = license_year + yr_offset) |>
    select(license_year, region, system, stream, stream_code, species,
           calendar_year, calendar_month, harvest_count)

  cli::cli_alert_success(
    glue(
      "{nrow(leaf)} leaf rows × 12 months = {nrow(leaf_long)} long rows | ",
      "{nrow(subtotals)} stream-subtotal rows | ",
      "{nrow(region_subtots)} region-subtotal rows | ",
      "{nrow(grandtots_raw)} grand-total rows."
    )
  )

  list(leaf_long = leaf_long, leaf = leaf,
       subtotals = subtotals, region_subtots = region_subtots,
       grandtots = grandtots_raw,
       month_col_names = month_col_names,
       license_year = license_year)
}
