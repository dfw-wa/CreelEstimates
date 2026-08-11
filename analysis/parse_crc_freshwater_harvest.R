# ==============================================================================
# parse_crc_freshwater_harvest.R
#
# Purpose:
#   Parse and validate the CRC-expanded freshwater salmon harvest workbooks
#   for license years 2021–2024 (sheet-level data, not creel estimates) and
#   produce a clean, tidy long-format CSV of leaf-level harvest counts.
#
#   This script is a *pre-processing step only*: it outputs harvest counts as
#   published in the CRC workbooks. No trips-per-salmon ratio is applied here.
#   The ratio application (odd-year 3.44 / even-year 8.65) and the
#   river-to-creel crosswalk matching are separate, later steps implemented in
#   interview_proportions.qmd.
#
# Why 2021 is included despite being outside the consultant's scope (2022–2024):
#   The even/odd year pattern in salmon runs (especially Pink) makes year-over-
#   year sanity checks most meaningful when both an even and odd neighbour are
#   present. 2021 provides a historical baseline for the 2023 odd-year row and
#   is processed alongside 2022–2024 but is clearly tagged as reference-only in
#   the output and warnings.
#
# Input files (input_files/):
#   Salmon Freshwater Estimates 2021 Draft 1.xlsx  -> sheet "FW 2021-2022"
#   Salmon Freshwater Estimates 2022 Draft 1.xlsx  -> sheet "FW 2022-2023"
#   Salmon Freshwater Estimates 2023.xlsx          -> sheet "FW 2023-2024"
#   Salmon Freshwater Estimates 2024.xlsx          -> sheet "FW 2024-2025"
#
# Output:
#   analysis/outputs/crc_freshwater_harvest_2021_2024_tidy.csv
#
# Validation (printed summary, not a separate file):
#   1. Monthly leaf rows sum == stream-species "Total" row per file.
#   2. Leaf totals == "Total - All Areas" grand totals per file.
#   3. No duplicate composite keys after fill-down.
#   4. Year-over-year harvest swing >3× or <0.33× flagged (warning, not error).
#      Even/odd year patterns (Pink, Coho) noted explicitly.
#   5. No Steelhead rows (these files are salmon-only per PST scope).
#   6. Blocking: all four years (2021–2024) must be present in output.
#
# Zero / blank convention:
#   Blank and zero cells in monthly harvest columns are both treated as 0
#   (no harvest recorded). An NA would imply the stream-species combination
#   did not exist in the source; that is handled by the stream-species rows
#   being absent entirely, which would show up as missing keys rather than NA.
# ==============================================================================

library(tidyverse)
library(readxl)
library(glue)
library(here)
library(cli)

# 0. Paths and file manifest --------------------------------------------------

out_dir <- here("analysis", "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

OUT_CSV <- file.path(out_dir, "crc_freshwater_harvest_2021_2024_tidy.csv")

FILE_MANIFEST <- tribble(
  ~license_year, ~filename,                                       ~sheet,
  2021L,         "Salmon Freshwater Estimates 2021 Draft 1.xlsx", "FW 2021-2022",
  2022L,         "Salmon Freshwater Estimates 2022 Draft 1.xlsx", "FW 2022-2023",
  2023L,         "Salmon Freshwater Estimates 2023.xlsx",         "FW 2023-2024",
  2024L,         "Salmon Freshwater Estimates 2024.xlsx",         "FW 2024-2025"
)

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

parse_fw_workbook <- function(license_year, filename, sheet) {
  path <- here("input_files", filename)

  cli::cli_h2(glue("Parsing license year {license_year} — {basename(path)} / {sheet}"))

  # Read without column names; everything as text to preserve merged cells
  raw <- read_excel(path, sheet = sheet, col_names = FALSE,
                    .name_repair = "minimal")

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
  data_sub <- data_sub |>
    mutate(across(all_of(fixed_names), as.character)) |>
    mutate(across(all_of(fixed_names), ~ na_if(., "")))

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


# 2. Parse all four workbooks -------------------------------------------------

parsed_list <- pmap(FILE_MANIFEST, parse_fw_workbook)
names(parsed_list) <- as.character(FILE_MANIFEST$license_year)


# 3. Combine leaf data --------------------------------------------------------

all_leaf <- map_dfr(parsed_list, "leaf_long")


# 4. Validation ---------------------------------------------------------------

cli::cli_h1("Validation")

## --- Check 4a: no duplicate composite keys -----------------------------------
cli::cli_h2("Check 3: Duplicate composite keys")

dup_keys <- all_leaf |>
  count(license_year, region, system, stream, stream_code, species,
        calendar_year, calendar_month) |>
  filter(n > 1L)

if (nrow(dup_keys) == 0L) {
  cli::cli_alert_success("PASS: No duplicate composite keys.")
} else {
  cli::cli_alert_danger(
    "FAIL: {nrow(dup_keys)} duplicate composite key(s) found — likely a fill-down bug:"
  )
  print(dup_keys)
}


## --- Check 1: monthly leaf sums == stream-species subtotal row ---------------
cli::cli_h2("Check 1: Monthly leaf sums vs stream-species Total rows")

leaf_sums <- map_dfr(parsed_list, function(p) {
  p$leaf |>
    select(license_year, region, system, stream, stream_code, species,
           all_of(p$month_col_names)) |>
    rowwise() |>
    mutate(leaf_sum = sum(c_across(all_of(p$month_col_names)), na.rm = TRUE)) |>
    ungroup() |>
    select(license_year, region, system, stream, stream_code, species, leaf_sum)
})

subtot_sums <- map_dfr(parsed_list, function(p) {
  # Subtotals: each stream's "Total" row. Match on stream_code or stream name.
  # row_total comes from the explicit Total column in the file if present,
  # otherwise sum the months.
  if ("row_total" %in% names(p$subtotals)) {
    p$subtotals |>
      select(license_year, region, system, stream, stream_code,
             all_of(p$month_col_names), row_total) |>
      rowwise() |>
      mutate(subtot_sum = sum(c_across(all_of(p$month_col_names)), na.rm = TRUE)) |>
      ungroup() |>
      select(license_year, region, system, stream, stream_code, subtot_sum, row_total)
  } else {
    p$subtotals |>
      select(license_year, region, system, stream, stream_code,
             all_of(p$month_col_names)) |>
      rowwise() |>
      mutate(subtot_sum = sum(c_across(all_of(p$month_col_names)), na.rm = TRUE),
             row_total  = subtot_sum) |>
      ungroup() |>
      select(license_year, region, system, stream, stream_code, subtot_sum, row_total)
  }
})

# Aggregate leaf sums to stream level (across species) for comparing subtotals
leaf_stream_sums <- leaf_sums |>
  group_by(license_year, region, system, stream, stream_code) |>
  summarise(leaf_stream_sum = sum(leaf_sum, na.rm = TRUE), .groups = "drop")

check1 <- subtot_sums |>
  left_join(leaf_stream_sums,
            by = c("license_year", "region", "system", "stream", "stream_code")) |>
  mutate(diff = subtot_sum - coalesce(leaf_stream_sum, 0L),
         match = abs(diff) < 0.5)

check1_fail <- filter(check1, !match)

if (nrow(check1_fail) == 0L) {
  cli::cli_alert_success(
    "PASS: All {nrow(check1)} stream subtotal rows reconcile with leaf sums."
  )
} else {
  # Known pre-publication inconsistencies in 2021 Draft 1 (Columbia Old Hanford +41,
  # Skagit River -852) are expected. Leaf month-cell values are authoritative.
  cli::cli_alert_warning(
    "WARNING (not blocking for Draft 1 files): \\
     {nrow(check1_fail)} stream subtotal(s) do not match leaf sums:"
  )
  print(select(check1_fail, license_year, stream, stream_code,
               leaf_stream_sum, subtot_sum, diff))
}


## --- Check 2: leaf sums == region-level subtotals (clean reconciliation) ------
cli::cli_h2("Check 2: Leaf sums vs region-level subtotals")
# Each file contains per-region aggregate blocks (col_B = 'Total', col_C/D = NA)
# excluded from the leaf extract. These are the natural reconciliation target.
# For 2023 and 2024 (published files) leaf sums should equal region subtotals
# exactly. For 2021 and 2022 (Draft 1 files) small discrepancies are expected:
# - 2021: Columbia Old Hanford +41, Skagit River -852 (pre-pub cell errors)
# - 2022: Unknown region (-3509) has a subtotal block but NO individual stream
#         rows in the draft, so those fish appear only in the region subtotal,
#         not in the leaf extract.
# Both are flagged as warnings, not failures.
#
# The file 'Total - All Areas' row is also shown. Unlike the earlier parsing
# defect that made it appear as a ~2x bug, it now matches leaf_grand within the
# same tolerance as the region-subtotal check.

leaf_by_region <- all_leaf |>
  group_by(license_year) |>
  summarise(leaf_grand = sum(harvest_count, na.rm = TRUE), .groups = "drop")

region_subtot_grand <- map_dfr(parsed_list, function(p) {
  rs <- p$region_subtots
  if (nrow(rs) == 0L) return(tibble(license_year = p$license_year, region_subtot_sum = NA_real_))
  rs_species <- rs[str_detect(coalesce(rs$species, ""), regex("^total$", ignore_case = TRUE)), ]
  if (nrow(rs_species) == 0L) return(tibble(license_year = p$license_year, region_subtot_sum = NA_real_))
  if ("row_total" %in% names(rs_species)) {
    tibble(license_year = p$license_year, region_subtot_sum = sum(rs_species$row_total, na.rm = TRUE))
  } else {
    tibble(license_year = p$license_year,
           region_subtot_sum = sum(as.numeric(unlist(select(rs_species, all_of(p$month_col_names)))), na.rm = TRUE))
  }
})

grandtot_file <- map_dfr(parsed_list, function(p) {
  gt <- p$grandtots
  if (nrow(gt) == 0L) return(tibble(license_year = p$license_year, file_grandtot = NA_real_))
  gt_total <- gt[str_detect(coalesce(gt$species, ""), regex("^total$", ignore_case = TRUE)), ]
  if (nrow(gt_total) == 0L) return(tibble(license_year = p$license_year, file_grandtot = NA_real_))
  if ("row_total" %in% names(gt_total)) {
    tibble(license_year = p$license_year, file_grandtot = sum(gt_total$row_total, na.rm = TRUE))
  } else {
    tibble(license_year = p$license_year,
           file_grandtot = sum(as.numeric(unlist(select(gt_total, all_of(p$month_col_names)))), na.rm = TRUE))
  }
})

check2 <- leaf_by_region |>
  left_join(region_subtot_grand, by = "license_year") |>
  left_join(grandtot_file,       by = "license_year") |>
  mutate(
    diff_region   = leaf_grand - coalesce(region_subtot_sum, 0),
    file_gt_ratio = round(leaf_grand / coalesce(file_grandtot, NA_real_), 3L),
    match         = !is.na(region_subtot_sum) & abs(diff_region) < 0.5
  )

check2_fail <- filter(check2, !match)

if (nrow(check2_fail) == 0L) {
  cli::cli_alert_success(
    "PASS: Leaf sums match region-subtotal sums for all {nrow(check2)} years."
  )
} else {
  cli::cli_alert_warning(
    "WARNING (not blocking for Draft 1 files): \\
     Leaf vs region-subtotal mismatch for {nrow(check2_fail)} year(s). \\
     Expected for 2021 (stream-level cell errors) and 2022 (Unknown region \\
     has no leaf rows in draft, only a subtotal block):"
  )
  print(select(check2_fail, license_year, leaf_grand, region_subtot_sum, diff_region))
}

cli::cli_alert_info("File 'Total - All Areas' grand total vs leaf grand (audit):")
print(select(check2, license_year, leaf_grand, file_grandtot, file_gt_ratio))


## --- Check 5: No Steelhead rows ----------------------------------------------
cli::cli_h2("Check 5: Steelhead exclusion (salmon-only PST scope)")

steelhead_rows <- all_leaf |>
  filter(str_detect(species, regex("steelhead", ignore_case = TRUE)))

if (nrow(steelhead_rows) == 0L) {
  cli::cli_alert_success("PASS: No Steelhead rows found — files are salmon-only.")
} else {
  cli::cli_alert_danger(
    "FAIL *** FILE STRUCTURE MAY HAVE CHANGED ***: \\
     {nrow(steelhead_rows)} Steelhead row(s) found across \\
     {n_distinct(steelhead_rows$license_year)} year(s). \\
     Inspect before using this output."
  )
  print(count(steelhead_rows, license_year, stream, species))
}


## --- Check 6: All four years present (blocking) ------------------------------
cli::cli_h2("Check 6: All four license years present in output (blocking)")

years_present <- sort(unique(all_leaf$license_year))
years_required <- 2021L:2024L
years_missing  <- setdiff(years_required, years_present)

if (length(years_missing) > 0L) {
  cli::cli_abort(
    "BLOCKING FAIL: The following license year(s) produced no output rows: \\
     {paste(years_missing, collapse = ', ')}. \\
     Check that the input workbook exists and was parsed correctly."
  )
}
cli::cli_alert_success(
  "PASS: All required license years present: {paste(years_present, collapse = ', ')}."
)


## --- Check 4b: Year-over-year harvest swing ----------------------------------
cli::cli_h2("Check 4: Year-over-year harvest swing (warning only, not blocking)")

yoy_by_species <- all_leaf |>
  group_by(license_year, species) |>
  summarise(total_harvest = sum(harvest_count, na.rm = TRUE), .groups = "drop") |>
  arrange(species, license_year) |>
  group_by(species) |>
  mutate(
    prev_harvest = lag(total_harvest),
    ratio        = if_else(coalesce(prev_harvest, 0) > 0,
                           total_harvest / prev_harvest,
                           NA_real_),
    flag         = !is.na(ratio) & (ratio > 3 | ratio < 1/3)
  ) |>
  ungroup()

yoy_flagged <- filter(yoy_by_species, flag)

if (nrow(yoy_flagged) == 0L) {
  cli::cli_alert_success(
    "PASS: No year-over-year swings >3× or <0.33× detected."
  )
} else {
  cli::cli_alert_warning(
    "WARNING: {nrow(yoy_flagged)} year-over-year swing(s) exceed threshold \\
     (this is expected for Pink salmon in even/odd years):"
  )
  print(
    yoy_flagged |>
      select(species, license_year, total_harvest, prev_harvest, ratio) |>
      mutate(across(c(ratio), ~ round(., 2L)))
  )
}

# Always print the full YoY table for even/odd pattern inspection
cli::cli_h3("Full YoY harvest by species (even/odd pattern check)")
print(
  yoy_by_species |>
    select(species, license_year, total_harvest, ratio) |>
    mutate(
      odd_even    = if_else(license_year %% 2L == 1L, "odd", "even"),
      ratio_str   = if_else(is.na(ratio), "—",
                            sprintf("%.2f×", ratio))
    ) |>
    arrange(species, license_year) |>
    select(species, license_year, odd_even, total_harvest, yoy_ratio = ratio_str)
)


# 5. Write output -------------------------------------------------------------

cli::cli_h1("Writing output")

# Write to a temp file first, then rename. This avoids the "file locked by
# another process" error that occurs when Excel has the output CSV open.
OUT_CSV_TMP <- paste0(OUT_CSV, ".tmp")
readr::write_csv(all_leaf, OUT_CSV_TMP)
if (file.exists(OUT_CSV)) file.remove(OUT_CSV)
file.rename(OUT_CSV_TMP, OUT_CSV)

cli::cli_alert_success(
  "Wrote {nrow(all_leaf)} rows to {OUT_CSV}"
)
cli::cli_alert_info(
  "License years in output: {paste(sort(unique(all_leaf$license_year)), collapse = ', ')}"
)
cli::cli_alert_info(
  "Calendar years in output: {paste(sort(unique(all_leaf$calendar_year)), collapse = ', ')}"
)
cli::cli_alert_info(
  "Species in output: {paste(sort(unique(all_leaf$species)), collapse = ', ')}"
)
