# ==============================================================================
# validate_batch_vs_manual.R
#
# Purpose:
#   Verify that the three previously-failing fisheries integrated via
#   integrate_manual_runs.R produce results equivalent to a fresh batch run
#   of multi_fishery_trip_summary.R and multi_fishery_harvest_summary.R, and
#   that all other fisheries' outputs are unchanged by the integration.
#
# When to run:
#   After BOTH of the following steps are complete:
#     1. integrate_manual_runs.R has produced
#          analysis/outputs/integrated_trip_summary.csv / .rds
#          analysis/outputs/integrated_harvest_summary.csv / .rds
#          analysis/outputs/integration_ledger.csv
#     2. A fresh full batch run has produced
#          analysis/outputs/multi_fishery_trip_summary.rds
#          analysis/outputs/multi_fishery_harvest_summary.csv
#
# Usage:
#   Rscript analysis/validate_batch_vs_manual.R
#   No VPN / DB access required; runs entirely from committed and locally-
#   produced files.
#
# Checks performed:
#   Section 1 — Row-level equivalence for the 3 reintegrated fisheries
#   Section 2 — Non-regression for all other fisheries vs. old/ baseline
#   Section 3 — Ledger reconciliation (all fisheries accounted for)
#   Section 4 — Drano Lake study-design resolution check
#
# Exit behaviour:
#   Section 2 failures call cli::cli_abort() — the integration is unsafe to
#   merge if other fisheries changed as a side effect. Sections 1, 3, and 4
#   call cli::cli_warn() so every finding is visible before aborting.
#
# Output:
#   analysis/outputs/batch_vs_manual_validation_report.csv
# ==============================================================================

library(tidyverse)
library(cli)
library(glue)
library(here)

# ── File paths ──────────────────────────────────────────────────────────────

OUT_DIR <- here("analysis", "outputs")

# Manual-run rows produced by integrate_manual_runs.R
INT_TRIP_PATH    <- file.path(OUT_DIR, "integrated_trip_summary.csv")
INT_HARV_PATH    <- file.path(OUT_DIR, "integrated_harvest_summary.csv")
LEDGER_PATH      <- file.path(OUT_DIR, "integration_ledger.csv")

# Fresh batch run outputs (produced by multi_fishery_trip_summary.R and
# multi_fishery_harvest_summary.R after the integration is applied)
BATCH_TRIP_PATH  <- file.path(OUT_DIR, "multi_fishery_trip_summary.rds")
BATCH_HARV_PATH  <- file.path(OUT_DIR, "multi_fishery_harvest_summary.csv")

# Pre-integration baseline — the last batch outputs before any of the
# Puyallup/Carbon, Nisqually, or Drano Lake manual-run work started.
# These live in analysis/outputs/old/ (committed).
OLD_TRIP_PATH    <- file.path(OUT_DIR, "old", "multi_fishery_trip_summary.rds")
OLD_HARV_PATH    <- file.path(OUT_DIR, "old", "multi_fishery_harvest_summary.csv")

# Harvest run ledger produced by the batch harvest script
HARV_LEDGER_PATH <- file.path(OUT_DIR, "multi_fishery_harvest_run_ledger.csv")

REPORT_PATH      <- file.path(OUT_DIR, "batch_vs_manual_validation_report.csv")


# ── Constants ────────────────────────────────────────────────────────────────

# The three fisheries whose estimates come from manual runs.
# Use exact DB fishery_name values (two Puyallup/Carbon spellings are distinct).
MANUAL_FISHERIES <- c(
  "Drano Lake salmon and steelhead 2022",
  "Drano Lake salmon and steelhead 2023",
  "Drano Lake salmon and steelhead 2024",
  "Drano Lake salmon and steelhead 2025",
  "Nisqually salmon 2022",
  "Nisqually salmon 2023",
  "Puyallup Carbon salmon 2024",
  "Puyallup Carbon salmon 2025",
  "Puyallup_Carbon salmon 2022",
  "Puyallup_Carbon salmon 2023"
)

# Natural key for trip summary joins
TRIP_KEY  <- c("fishery_name", "year", "month", "catch_area_code", "angler_final",
               "pe_period")

# Natural key for harvest summary joins (adds catch-group dimension)
HARV_KEY  <- c("fishery_name", "year", "month", "catch_area_code", "angler_final",
               "est_cg", "pe_period")

# Numeric tolerance for floating-point equivalence (accommodates accumulation
# across ~12 months × 2 angler types × 2+ sections but not meaningful drift).
NUM_TOL   <- 1e-8

# Fisheries that should NOT appear for non-regression (they are manually integrated).
# Rows for these are skipped in Section 2.
SKIP_IN_NONREG <- MANUAL_FISHERIES

# Lower Cowlitz documented skip: confirm it appears in the ledger as a skip,
# not as a silent drop or a success.
LOWER_COWLITZ_PATTERN <- "Lower Cowlitz"


# ── Helpers ──────────────────────────────────────────────────────────────────

# Collect mismatch rows into a tidy report frame
report_rows <- list()

append_mismatches <- function(section, df) {
  if (nrow(df) > 0) {
    report_rows[[length(report_rows) + 1]] <<- df |>
      dplyr::mutate(validation_section = section, .before = 1)
  }
}

# Compare two numeric vectors element-wise; return indices where |a-b| > tol.
num_diff <- function(a, b, tol = NUM_TOL) {
  both_na <- is.na(a) & is.na(b)
  one_na  <- xor(is.na(a), is.na(b))
  diff    <- !both_na & !one_na & (abs(a - b) > tol)
  one_na | diff
}


# ── Preflight: required files ─────────────────────────────────────────────────

cli::cli_h1("validate_batch_vs_manual.R")
cli::cli_alert_info("Checking required files ...")

required <- c(
  INT_TRIP_PATH, INT_HARV_PATH, LEDGER_PATH,
  BATCH_TRIP_PATH, BATCH_HARV_PATH,
  OLD_TRIP_PATH, OLD_HARV_PATH,
  HARV_LEDGER_PATH
)

missing_files <- required[!file.exists(required)]
if (length(missing_files) > 0) {
  cli::cli_abort(
    c("Required file(s) not found. Re-run integrate_manual_runs.R and the \\
       batch scripts before running this validation.",
      "x" = "{.path {missing_files}}")
  )
}

cli::cli_alert_success("All required files found.")


# ── Load data ─────────────────────────────────────────────────────────────────

cli::cli_alert_info("Loading integrated outputs ...")
int_trip    <- readr::read_csv(INT_TRIP_PATH,  show_col_types = FALSE)
int_harv    <- readr::read_csv(INT_HARV_PATH,  show_col_types = FALSE)
ledger      <- readr::read_csv(LEDGER_PATH,    show_col_types = FALSE)

cli::cli_alert_info("Loading fresh batch outputs ...")
batch_trip  <- readRDS(BATCH_TRIP_PATH)
batch_harv  <- readr::read_csv(BATCH_HARV_PATH, show_col_types = FALSE)

cli::cli_alert_info("Loading old baseline outputs ...")
old_trip    <- readRDS(OLD_TRIP_PATH)
old_harv    <- readr::read_csv(OLD_HARV_PATH, show_col_types = FALSE)

harv_ledger <- readr::read_csv(HARV_LEDGER_PATH, show_col_types = FALSE)


# ── Normalise column names ────────────────────────────────────────────────────
# The batch trip CSV/RDS may carry `crc_area` (pre-integration naming); the
# integrated output uses `catch_area_code`. Normalise to catch_area_code.

normalise_area_col <- function(df) {
  dplyr::rename(df, dplyr::any_of(c(catch_area_code = "crc_area")))
}

int_trip   <- normalise_area_col(int_trip)
batch_trip <- normalise_area_col(batch_trip)
old_trip   <- normalise_area_col(old_trip)


# ── Ensure key types are consistent ──────────────────────────────────────────

harmonise_key_types <- function(df) {
  df |> dplyr::mutate(
    fishery_name    = as.character(fishery_name),
    catch_area_code = as.character(catch_area_code),
    angler_final    = as.character(angler_final),
    pe_period       = as.character(pe_period),
    year            = as.integer(year),
    month           = as.integer(month)
  )
}

int_trip   <- harmonise_key_types(int_trip)
batch_trip <- harmonise_key_types(batch_trip)
old_trip   <- harmonise_key_types(old_trip)

int_harv   <- int_harv |>
  dplyr::mutate(across(c(fishery_name, catch_area_code, angler_final,
                         pe_period, est_cg), as.character),
                across(c(year, month), as.integer))

batch_harv <- batch_harv |>
  dplyr::mutate(across(c(fishery_name, catch_area_code, angler_final,
                         pe_period, est_cg), as.character),
                across(c(year, month), as.integer))

old_harv   <- old_harv |>
  dplyr::mutate(across(c(fishery_name, catch_area_code, angler_final,
                         pe_period, est_cg), as.character),
                across(c(year, month), as.integer))


# ── Derive manual rows from integrated output ─────────────────────────────────

manual_trip <- int_trip |> dplyr::filter(source == "manual")
manual_harv <- int_harv |> dplyr::filter(source == "manual")

cli::cli_alert_info(
  "Manual trip rows: {nrow(manual_trip)} across \\
   {dplyr::n_distinct(manual_trip$fishery_name)} fisheries."
)
cli::cli_alert_info(
  "Manual harvest rows: {nrow(manual_harv)} across \\
   {dplyr::n_distinct(manual_harv$fishery_name)} fisheries."
)

batch_manual_fish_trip <- batch_trip |>
  dplyr::filter(fishery_name %in% unique(manual_trip$fishery_name))
batch_manual_fish_harv <- batch_harv |>
  dplyr::filter(fishery_name %in% unique(manual_harv$fishery_name))


# =============================================================================
# Section 1 — Row-level equivalence for the 3 reintegrated fisheries
# =============================================================================

cli::cli_h2("Section 1: Manual vs. Batch — three reintegrated fisheries")
s1_warns <- character(0)

compare_tables <- function(manual, batch, key_cols, num_cols, char_cols,
                           label, section) {

  # Full outer join to catch rows present in only one side
  joined <- dplyr::full_join(
    manual |> dplyr::select(dplyr::all_of(c(key_cols, num_cols, char_cols))) |>
      dplyr::mutate(.src_manual = TRUE),
    batch  |> dplyr::select(dplyr::all_of(c(key_cols, num_cols, char_cols))) |>
      dplyr::mutate(.src_batch  = TRUE),
    by     = key_cols,
    suffix = c("_manual", "_batch")
  )

  only_in_manual <- joined |> dplyr::filter(is.na(.src_batch))
  only_in_batch  <- joined |> dplyr::filter(is.na(.src_manual))
  both           <- joined |> dplyr::filter(!is.na(.src_manual), !is.na(.src_batch))

  # Pivot a one-sided frame long so every numeric/char column value is visible
  # in the report rather than being replaced with NA.
  pivot_one_side <- function(df, mismatch_type, value_col_name) {
    val_cols <- intersect(c(num_cols, char_cols), names(df))
    df |>
      dplyr::select(dplyr::all_of(c(key_cols, val_cols))) |>
      tidyr::pivot_longer(
        cols             = dplyr::all_of(val_cols),
        names_to         = "column",
        values_to        = "present_value",
        values_transform = list(present_value = as.character)
      ) |>
      dplyr::mutate(
        mismatch_type = mismatch_type,
        manual_value  = if (value_col_name == "manual") present_value else NA_character_,
        batch_value   = if (value_col_name == "batch")  present_value else NA_character_
      ) |>
      dplyr::select(-present_value)
  }

  if (nrow(only_in_manual) > 0) {
    cli::cli_warn(
      "{label}: {nrow(only_in_manual)} row(s) present in MANUAL but missing \\
       from BATCH — may indicate a batch-run failure for those strata."
    )
    s1_warns <<- c(s1_warns, glue::glue("{label}: {nrow(only_in_manual)} rows only in manual"))
    append_mismatches(section,
      pivot_one_side(only_in_manual, "only_in_manual", "manual"))
  }
  if (nrow(only_in_batch) > 0) {
    cli::cli_warn(
      "{label}: {nrow(only_in_batch)} row(s) present in BATCH but missing \\
       from MANUAL — unexpected; the manual run should be a superset."
    )
    s1_warns <<- c(s1_warns, glue::glue("{label}: {nrow(only_in_batch)} rows only in batch"))
    append_mismatches(section,
      pivot_one_side(only_in_batch, "only_in_batch", "batch"))
  }

  mismatch_list <- list()

  # Numeric comparison with tolerance
  for (col in num_cols) {
    m_col <- paste0(col, "_manual")
    b_col <- paste0(col, "_batch")
    if (!m_col %in% names(both) || !b_col %in% names(both)) next
    bad <- num_diff(both[[m_col]], both[[b_col]])
    if (any(bad)) {
      mismatch_list[[col]] <- both[bad, key_cols, drop = FALSE] |>
        dplyr::mutate(
          mismatch_type = "numeric_drift",
          column        = col,
          manual_value  = as.character(both[[m_col]][bad]),
          batch_value   = as.character(both[[b_col]][bad])
        )
    }
  }

  # Character/factor: exact match required
  for (col in char_cols) {
    m_col <- paste0(col, "_manual")
    b_col <- paste0(col, "_batch")
    if (!m_col %in% names(both) || !b_col %in% names(both)) next
    bad <- both[[m_col]] != both[[b_col]] |
           xor(is.na(both[[m_col]]), is.na(both[[b_col]]))
    bad[is.na(bad)] <- FALSE
    if (any(bad)) {
      mismatch_list[[col]] <- both[bad, key_cols, drop = FALSE] |>
        dplyr::mutate(
          mismatch_type = "character_mismatch",
          column        = col,
          manual_value  = as.character(both[[m_col]][bad]),
          batch_value   = as.character(both[[b_col]][bad])
        )
    }
  }

  all_mismatches <- dplyr::bind_rows(mismatch_list)

  if (nrow(all_mismatches) > 0) {
    cli::cli_warn(
      "{label}: {nrow(all_mismatches)} cell mismatch{?es} across \\
       {dplyr::n_distinct(all_mismatches$column)} column(s)."
    )
    print(all_mismatches, n = 30)
    append_mismatches(section, all_mismatches)
    s1_warns <<- c(s1_warns,
                   glue::glue("{label}: {nrow(all_mismatches)} cell mismatches"))
  } else {
    cli::cli_alert_success(
      "{label}: {nrow(both)} matched rows — all numeric and character \\
       columns agree."
    )
  }

  invisible(all_mismatches)
}

# -- 1a. Trip summary ---------------------------------------------------------

TRIP_NUM_COLS  <- c("total_effort_hrs", "n_completed_angler_trips",
                    "mean_trip_length", "mean_group_size", "sd",
                    "total_trips_est")
TRIP_CHAR_COLS <- c("study_design", "pe_period")

if (nrow(batch_manual_fish_trip) == 0) {
  cli::cli_warn(
    "Section 1 (trip): batch output contains NO rows for the manual-run \\
     fisheries. If the batch scripts still fail for these fisheries, this \\
     is expected and Section 1 cannot be evaluated. Check the batch run logs."
  )
} else {
  compare_tables(
    manual     = manual_trip,
    batch      = batch_manual_fish_trip,
    key_cols   = TRIP_KEY,
    num_cols   = TRIP_NUM_COLS,
    char_cols  = TRIP_CHAR_COLS,
    label      = "Trip summary (manual fisheries)",
    section    = "S1_trip"
  )
}

# -- 1b. Harvest summary ------------------------------------------------------

HARV_NUM_COLS  <- c("harvest_est", "harvest_var", "harvest_se", "harvest_cv",
                    "harvest_l95", "harvest_u95", "total_effort_hrs",
                    "harvest_per_hr")
# catch_group and species_scope are NOT in HARV_CHAR_COLS: the manual runs
# produced a single dynamic total-salmon group whose est_cg string encodes
# the species present (e.g. "Chinook|Coho_Adult|Jack_AD|UM_Kept"), while the
# batch produces one row per species PLUS a TotalSalmon row. We scope the
# harvest comparison to TotalSalmon rows only (see filter below), so the
# species_scope values will legitimately differ in wording but the numbers
# should agree. life_stage_levels / fin_mark_levels / fate_levels can still
# be exact-matched within TotalSalmon rows.
HARV_CHAR_COLS <- c("study_design", "pe_period",
                    "life_stage_levels", "fin_mark_levels", "fate_levels")

if (nrow(batch_manual_fish_harv) == 0) {
  cli::cli_warn(
    "Section 1 (harvest): batch output contains NO rows for the manual-run \\
     fisheries. If the batch scripts still fail for these fisheries, this \\
     is expected and Section 1 cannot be evaluated."
  )
} else {
  # Manual runs produced a single pooled total-salmon catch group only.
  # Restrict both sides to TotalSalmon so species-specific batch rows
  # (which have no manual counterpart) do not generate spurious "only in batch"
  # flags.
  manual_harv_total <- manual_harv |>
    dplyr::filter(catch_group == "TotalSalmon" | is_total == TRUE)
  batch_manual_fish_harv_total <- batch_manual_fish_harv |>
    dplyr::filter(catch_group == "TotalSalmon" | is_total == TRUE)

  cli::cli_alert_info(
    "Section 1 (harvest): comparing TotalSalmon rows only \\
     ({nrow(manual_harv_total)} manual, {nrow(batch_manual_fish_harv_total)} batch)."
  )

  compare_tables(
    manual     = manual_harv_total,
    batch      = batch_manual_fish_harv_total,
    key_cols   = HARV_KEY,
    num_cols   = HARV_NUM_COLS,
    char_cols  = HARV_CHAR_COLS,
    label      = "Harvest summary — TotalSalmon rows (manual fisheries)",
    section    = "S1_harvest"
  )
}

if (length(s1_warns) == 0) {
  cli::cli_alert_success("Section 1 PASS — manual and batch rows are equivalent.")
} else {
  cli::cli_warn(
    "Section 1 WARN — {length(s1_warns)} issue(s) found. See report for details."
  )
}


# =============================================================================
# Section 2 — Non-regression on the existing "Standard" fisheries
# =============================================================================
#
# For every fishery OTHER than the manually-integrated ones, compare the
# fresh batch output against the old baseline. Any numeric drift here is a
# red flag (global state contamination, shared-cache issue, etc.) and causes
# an abort so the integration is not merged.
# =============================================================================

cli::cli_h2("Section 2: Non-regression — all other fisheries vs. old baseline")

# -- 2a. Trip summary ---------------------------------------------------------

# Columns that may legitimately differ (run metadata added post-baseline)
TRIP_METADATA_COLS <- c("source", "run_id", "study_design")

other_batch_trip <- batch_trip |>
  dplyr::filter(!fishery_name %in% SKIP_IN_NONREG)

other_old_trip <- old_trip |>
  dplyr::filter(!fishery_name %in% SKIP_IN_NONREG) |>
  # Drop metadata columns that did not exist in the pre-DESIGN_RULES baseline
  dplyr::select(-dplyr::any_of(TRIP_METADATA_COLS))

other_batch_trip_cmp <- other_batch_trip |>
  dplyr::select(-dplyr::any_of(TRIP_METADATA_COLS))

# Full outer join to detect silent drops and insertions
nonreg_trip_joined <- dplyr::full_join(
  other_old_trip   |> dplyr::mutate(.src_old   = TRUE),
  other_batch_trip_cmp |> dplyr::mutate(.src_new = TRUE),
  by = TRIP_KEY
)

trip_only_in_old   <- nonreg_trip_joined |> dplyr::filter(is.na(.src_new))
trip_only_in_new   <- nonreg_trip_joined |> dplyr::filter(is.na(.src_old))
trip_both          <- nonreg_trip_joined |> dplyr::filter(!is.na(.src_old), !is.na(.src_new))

nonreg_trip_errs <- character(0)

# Pivot a one-sided frame from the full-join (which has _old/_new suffixes)
# so every numeric column value is visible in the report.
pivot_nonreg_one_side <- function(df, key_cols, num_cols, mismatch_type,
                                  present_suffix, absent_col_name) {
  present_cols <- paste0(num_cols, present_suffix)
  avail        <- intersect(present_cols, names(df))
  df |>
    dplyr::select(dplyr::all_of(c(key_cols, avail))) |>
    dplyr::rename_with(~ sub(present_suffix, "", .x), dplyr::all_of(avail)) |>
    tidyr::pivot_longer(
      cols             = dplyr::all_of(sub(present_suffix, "", avail)),
      names_to         = "column",
      values_to        = "present_value",
      values_transform = list(present_value = as.character)
    ) |>
    dplyr::mutate(
      mismatch_type = mismatch_type,
      manual_value  = if (absent_col_name == "batch")  present_value else NA_character_,
      batch_value   = if (absent_col_name == "manual") present_value else NA_character_
    ) |>
    dplyr::select(-present_value)
}

if (nrow(trip_only_in_old) > 0) {
  nonreg_trip_errs <- c(nonreg_trip_errs,
    glue::glue("{nrow(trip_only_in_old)} rows present in OLD baseline but \\
                MISSING from new batch — possible silent drop!"))
  append_mismatches("S2_trip_drop",
    pivot_nonreg_one_side(trip_only_in_old, TRIP_KEY, TRIP_NUM_COLS,
                          "dropped_in_batch", "_old", "batch"))
}
if (nrow(trip_only_in_new) > 0) {
  # New fisheries added to scope is not an error, but is worth flagging.
  cli::cli_alert_warning(
    "Non-regression (trip): {nrow(trip_only_in_new)} row(s) appear in the \\
     new batch but not the old baseline — new fishery added to scope? \\
     Verify deliberately."
  )
}

# Inner join with explicit suffixes to compare numeric columns row-by-row
nonreg_trip_mismatch_list <- list()
nonreg_trip_inner <- dplyr::inner_join(
  other_old_trip   |> dplyr::select(dplyr::all_of(c(TRIP_KEY, TRIP_NUM_COLS))),
  other_batch_trip_cmp |> dplyr::select(dplyr::all_of(c(TRIP_KEY, TRIP_NUM_COLS))),
  by     = TRIP_KEY,
  suffix = c("_old", "_new")
)

for (col in TRIP_NUM_COLS) {
  o_col <- paste0(col, "_old")
  n_col <- paste0(col, "_new")
  if (!o_col %in% names(nonreg_trip_inner) ||
      !n_col %in% names(nonreg_trip_inner)) next
  bad <- num_diff(nonreg_trip_inner[[o_col]], nonreg_trip_inner[[n_col]])
  if (any(bad)) {
    nonreg_trip_mismatch_list[[col]] <-
      nonreg_trip_inner[bad, TRIP_KEY, drop = FALSE] |>
        dplyr::mutate(
          mismatch_type = "numeric_drift",
          column        = col,
          old_value     = as.character(nonreg_trip_inner[[o_col]][bad]),
          new_value     = as.character(nonreg_trip_inner[[n_col]][bad])
        )
  }
}

if (length(nonreg_trip_mismatch_list) > 0) {
  nonreg_trip_df <- dplyr::bind_rows(nonreg_trip_mismatch_list)
  nonreg_trip_errs <- c(nonreg_trip_errs,
    glue::glue("{nrow(nonreg_trip_df)} numeric drift cell(s) in \\
                {dplyr::n_distinct(nonreg_trip_df$column)} column(s)"))
  cli::cli_alert_danger(
    "Non-regression (trip): NUMERIC DRIFT detected in {nrow(nonreg_trip_df)} \\
     cell(s). The integration may have had a side effect on global state."
  )
  print(nonreg_trip_df |> dplyr::count(column, fishery_name), n = 40)
  append_mismatches("S2_trip_drift", nonreg_trip_df |>
    dplyr::rename(manual_value = old_value, batch_value = new_value))
} else if (nrow(trip_only_in_old) == 0) {
  cli::cli_alert_success(
    "Non-regression (trip): {nrow(nonreg_trip_inner)} rows checked — \\
     no numeric drift."
  )
}

# -- 2b. Harvest summary ------------------------------------------------------

other_batch_harv <- batch_harv |>
  dplyr::filter(!fishery_name %in% SKIP_IN_NONREG)

other_old_harv <- old_harv |>
  dplyr::filter(!fishery_name %in% SKIP_IN_NONREG)

# Full outer join for drop/insert detection
nonreg_harv_joined <- dplyr::full_join(
  other_old_harv   |> dplyr::mutate(.src_old = TRUE),
  other_batch_harv |> dplyr::mutate(.src_new = TRUE),
  by = HARV_KEY,
  suffix = c("_old", "_new")
)

harv_only_in_old   <- nonreg_harv_joined |> dplyr::filter(is.na(.src_new))
harv_only_in_new   <- nonreg_harv_joined |> dplyr::filter(is.na(.src_old))

nonreg_harv_errs <- character(0)

if (nrow(harv_only_in_old) > 0) {
  nonreg_harv_errs <- c(nonreg_harv_errs,
    glue::glue("{nrow(harv_only_in_old)} harvest rows present in OLD baseline \\
                but MISSING from new batch — possible silent drop!"))
  append_mismatches("S2_harv_drop",
    pivot_nonreg_one_side(harv_only_in_old, HARV_KEY, HARV_NUM_COLS,
                          "dropped_in_batch", "_old", "batch"))
}
if (nrow(harv_only_in_new) > 0) {
  cli::cli_alert_warning(
    "Non-regression (harvest): {nrow(harv_only_in_new)} row(s) in new batch \\
     but not old baseline."
  )
}

nonreg_harv_inner <- dplyr::inner_join(
  other_old_harv   |> dplyr::select(dplyr::all_of(c(HARV_KEY, HARV_NUM_COLS))),
  other_batch_harv |> dplyr::select(dplyr::all_of(c(HARV_KEY, HARV_NUM_COLS))),
  by     = HARV_KEY,
  suffix = c("_old", "_new")
)

nonreg_harv_mismatch_list <- list()
for (col in HARV_NUM_COLS) {
  o_col <- paste0(col, "_old")
  n_col <- paste0(col, "_new")
  if (!o_col %in% names(nonreg_harv_inner) ||
      !n_col %in% names(nonreg_harv_inner)) next
  bad <- num_diff(nonreg_harv_inner[[o_col]], nonreg_harv_inner[[n_col]])
  if (any(bad)) {
    nonreg_harv_mismatch_list[[col]] <-
      nonreg_harv_inner[bad, HARV_KEY, drop = FALSE] |>
        dplyr::mutate(
          mismatch_type = "numeric_drift",
          column        = col,
          old_value     = as.character(nonreg_harv_inner[[o_col]][bad]),
          new_value     = as.character(nonreg_harv_inner[[n_col]][bad])
        )
  }
}

if (length(nonreg_harv_mismatch_list) > 0) {
  nonreg_harv_df <- dplyr::bind_rows(nonreg_harv_mismatch_list)
  nonreg_harv_errs <- c(nonreg_harv_errs,
    glue::glue("{nrow(nonreg_harv_df)} numeric drift cell(s) in \\
                {dplyr::n_distinct(nonreg_harv_df$column)} column(s)"))
  cli::cli_alert_danger(
    "Non-regression (harvest): NUMERIC DRIFT detected in {nrow(nonreg_harv_df)} \\
     cell(s)."
  )
  print(nonreg_harv_df |> dplyr::count(column, fishery_name), n = 40)
  append_mismatches("S2_harv_drift", nonreg_harv_df |>
    dplyr::rename(manual_value = old_value, batch_value = new_value))
} else if (nrow(harv_only_in_old) == 0) {
  cli::cli_alert_success(
    "Non-regression (harvest): {nrow(nonreg_harv_inner)} rows checked — \\
     no numeric drift."
  )
}

# Abort if any Section 2 non-regression failure is found
all_s2_errs <- c(nonreg_trip_errs, nonreg_harv_errs)
if (length(all_s2_errs) > 0) {
  cli::cli_abort(
    c("Section 2 FAIL — integration is unsafe to merge.",
      "x" = "{all_s2_errs}")
  )
}
cli::cli_alert_success("Section 2 PASS — no non-regression failures.")


# =============================================================================
# Section 3 — Outcome ledger reconciliation
# =============================================================================

cli::cli_h2("Section 3: Ledger reconciliation")
s3_warns <- character(0)

# -- 3a. Integration ledger: manually-integrated fisheries show action --

for (fn in unique(c(manual_trip$fishery_name, manual_harv$fishery_name))) {
  ledger_rows <- ledger |>
    dplyr::filter(fishery_name == fn, action %in% c("replaced", "inserted"))
  if (nrow(ledger_rows) == 0) {
    s3_warns <- c(s3_warns,
      glue::glue("{fn}: no 'replaced' or 'inserted' entry in integration_ledger.csv"))
    cli::cli_warn(
      "{.val {fn}}: expected a 'replaced' or 'inserted' ledger entry but found none."
    )
  } else {
    cli::cli_alert_success(
      "Ledger: {.val {fn}} -> {.val {paste(ledger_rows$action, collapse = ', ')}}"
    )
  }
}

# -- 3b. Harvest run ledger: manual fisheries now show status == "ok" ---------

for (fn in unique(manual_harv$fishery_name)) {
  hl_rows <- harv_ledger |> dplyr::filter(fishery_name == fn)
  if (nrow(hl_rows) == 0) {
    s3_warns <- c(s3_warns,
      glue::glue("{fn}: absent from multi_fishery_harvest_run_ledger.csv"))
    cli::cli_warn("{.val {fn}}: absent from harvest run ledger.")
    next
  }
  non_ok <- hl_rows |> dplyr::filter(status != "ok")
  if (nrow(non_ok) > 0) {
    s3_warns <- c(s3_warns,
      glue::glue("{fn}: harvest ledger status is not 'ok': \\
                  {paste(non_ok$status, collapse = ', ')}"))
    cli::cli_warn(
      "{.val {fn}}: harvest ledger has non-ok status: \\
       {.val {paste(non_ok$status, collapse = ', ')}}"
    )
  } else {
    cli::cli_alert_success(
      "Harvest ledger: {.val {fn}} status = ok."
    )
  }
}

# -- 3c. Lower Cowlitz: must be a DOCUMENTED SKIP, not silently gone ----------

cowlitz_ledger <- harv_ledger |>
  dplyr::filter(stringr::str_detect(fishery_name,
                                    stringr::regex(LOWER_COWLITZ_PATTERN,
                                                   ignore_case = TRUE)))

cowlitz_integ <- ledger |>
  dplyr::filter(stringr::str_detect(fishery_name,
                                    stringr::regex(LOWER_COWLITZ_PATTERN,
                                                   ignore_case = TRUE)))

if (nrow(cowlitz_ledger) == 0 && nrow(cowlitz_integ) == 0) {
  s3_warns <- c(s3_warns,
    "Lower Cowlitz: absent from both ledgers — may be silently dropped!")
  cli::cli_warn(
    "Lower Cowlitz: absent from both harvest run ledger and integration \\
     ledger. If it was never in scope this is expected; if it should be \\
     accounted for, investigate."
  )
} else if (nrow(cowlitz_ledger) > 0) {
  non_skip <- cowlitz_ledger |> dplyr::filter(!status %in% c("skipped", "ok"))
  if (nrow(non_skip) > 0) {
    s3_warns <- c(s3_warns,
      glue::glue("Lower Cowlitz: unexpected harvest ledger status: \\
                  {paste(non_skip$status, collapse = ', ')}"))
    cli::cli_warn("Lower Cowlitz: unexpected harvest ledger status.")
    print(cowlitz_ledger, n = 10)
  } else {
    cli::cli_alert_success(
      "Lower Cowlitz: documented in harvest ledger with status \\
       {.val {paste(cowlitz_ledger$status, collapse = ', ')}}."
    )
  }
}

if (length(s3_warns) == 0) {
  cli::cli_alert_success("Section 3 PASS — ledger reconciliation clean.")
} else {
  cli::cli_warn(
    "Section 3 WARN — {length(s3_warns)} issue(s) found:\\n{paste(s3_warns, collapse = '\\n')}"
  )
}


# =============================================================================
# Section 4 — Study design resolution check (Drano-specific)
# =============================================================================

cli::cli_h2("Section 4: Drano Lake study_design resolution")
s4_warns <- character(0)

drano_pattern <- "Drano Lake"

# -- 4a. Manual-run rows in integrated trip output ----------------------------

drano_manual <- manual_trip |>
  dplyr::filter(stringr::str_detect(fishery_name,
                                    stringr::regex(drano_pattern, ignore_case = TRUE)))

if (nrow(drano_manual) == 0) {
  s4_warns <- c(s4_warns,
    "Drano Lake: no manual rows found in integrated_trip_summary.csv")
  cli::cli_warn("Drano Lake: no manual rows found in integrated_trip_summary.csv.")
} else {
  wrong_design <- drano_manual |>
    dplyr::filter(is.na(study_design) | study_design != "Drano")
  if (nrow(wrong_design) > 0) {
    s4_warns <- c(s4_warns,
      glue::glue("Drano Lake: {nrow(wrong_design)} manual trip row(s) with \\
                  study_design != 'Drano'"))
    cli::cli_warn(
      "Drano Lake: {nrow(wrong_design)} manual trip row(s) carry \\
       study_design != 'Drano'. The resolver may have picked the wrong design."
    )
    print(wrong_design |> dplyr::select(fishery_name, year, month, study_design),
          n = 20)
    append_mismatches("S4_design", wrong_design |>
      dplyr::select(fishery_name, year, month,
                    catch_area_code, angler_final, pe_period, study_design) |>
      dplyr::mutate(mismatch_type = "wrong_study_design",
                    column        = "study_design",
                    manual_value  = as.character(study_design),
                    batch_value   = "Drano"))
  } else {
    cli::cli_alert_success(
      "Drano Lake (manual rows, {nrow(drano_manual)} rows): \\
       study_design == 'Drano' on all rows."
    )
  }
}

# -- 4b. Batch-run rows for Drano (if the batch now produces them) ------------

drano_batch <- batch_trip |>
  dplyr::filter(stringr::str_detect(fishery_name,
                                    stringr::regex(drano_pattern, ignore_case = TRUE)))

if (nrow(drano_batch) == 0) {
  cli::cli_alert_info(
    "Drano Lake: no batch rows in new multi_fishery_trip_summary.rds — \\
     batch still failing for this fishery (expected if not yet fixed)."
  )
} else {
  # The batch output predates DESIGN_RULES and may carry NA study_design
  # rather than "Drano". Any "Standard" value here is the pre-fix bug.
  batch_wrong_design <- drano_batch |>
    dplyr::filter(!is.na(study_design) & study_design != "Drano")
  if (nrow(batch_wrong_design) > 0) {
    s4_warns <- c(s4_warns,
      glue::glue("Drano Lake: {nrow(batch_wrong_design)} BATCH trip row(s) \\
                  with study_design != 'Drano' — batch still uses wrong design!"))
    cli::cli_warn(
      "Drano Lake: {nrow(batch_wrong_design)} batch trip row(s) carry \\
       study_design != 'Drano'. The batch script has not yet picked up the \\
       DESIGN_RULES fix."
    )
    print(batch_wrong_design |> dplyr::select(fishery_name, year, month,
                                               study_design), n = 20)
  } else {
    cli::cli_alert_success(
      "Drano Lake (batch rows, {nrow(drano_batch)} rows): \\
       study_design correct on all rows."
    )
  }
}

# -- 4c. Drano not bleeding into non-Drano fisheries -------------------------
# Guard against the pattern regex matching an unintended fishery name

all_batch_drano_check <- batch_trip |>
  dplyr::filter(study_design == "Drano",
                !stringr::str_detect(fishery_name,
                                     stringr::regex(drano_pattern,
                                                    ignore_case = TRUE)))
if (nrow(all_batch_drano_check) > 0) {
  s4_warns <- c(s4_warns,
    glue::glue("DESIGN_RULES bleed: {nrow(all_batch_drano_check)} batch row(s) \\
                for non-Drano fisheries have study_design == 'Drano'"))
  cli::cli_warn(
    "DESIGN_RULES bleed: {nrow(all_batch_drano_check)} batch trip row(s) for \\
     non-Drano fisheries resolved to study_design == 'Drano'. \\
     The regex pattern in DESIGN_RULES is too broad."
  )
  print(all_batch_drano_check |> dplyr::count(fishery_name), n = 20)
  append_mismatches("S4_design_bleed", all_batch_drano_check |>
    dplyr::select(fishery_name, year, month,
                  catch_area_code, angler_final, pe_period, study_design) |>
    dplyr::mutate(mismatch_type = "design_bleed",
                  column        = "study_design",
                  manual_value  = NA_character_,
                  batch_value   = "Drano"))
}

if (length(s4_warns) == 0) {
  cli::cli_alert_success("Section 4 PASS — Drano study-design resolution correct.")
} else {
  cli::cli_warn(
    "Section 4 WARN — {length(s4_warns)} issue(s):\\n{paste(s4_warns, collapse = '\\n')}"
  )
}


# =============================================================================
# Summary & report
# =============================================================================

cli::cli_h1("Validation Summary")

all_report <- dplyr::bind_rows(report_rows)

if (nrow(all_report) > 0) {
  readr::write_csv(all_report, REPORT_PATH)
  cli::cli_alert_info(
    "Mismatch report ({nrow(all_report)} row{?s}) written to \\
     {.path {REPORT_PATH}}"
  )
} else {
  cli::cli_alert_success(
    "No mismatches recorded. Report file not written (nothing to report)."
  )
  # Write an empty report so downstream scripts can always check for the file
  readr::write_csv(
    tibble::tibble(
      validation_section = character(), fishery_name = character(),
      mismatch_type = character(), column = character(),
      manual_value = character(), batch_value = character()
    ),
    REPORT_PATH
  )
}

# Section-level result table
section_results <- tibble::tribble(
  ~section, ~label,                                    ~issues,
  "S1",     "Manual vs. Batch (3 fisheries)",          length(s1_warns),
  "S2",     "Non-regression (45 other fisheries)",     0L,   # abort prevents reaching here
  "S3",     "Ledger reconciliation",                   length(s3_warns),
  "S4",     "Drano study-design resolution",           length(s4_warns)
) |>
  dplyr::mutate(result = dplyr::if_else(issues == 0, "PASS", "WARN"))

cli::cli_h3("Results by section")
print(section_results)

overall <- if (all(section_results$issues == 0)) {
  cli::cli_alert_success("Overall: ALL CHECKS PASSED.")
  "PASS"
} else {
  cli::cli_warn("Overall: WARNINGS PRESENT — review report before merging.")
  "WARN (see above)"
}
