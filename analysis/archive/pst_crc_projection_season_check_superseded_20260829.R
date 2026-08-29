# ==============================================================================
# SUPERSEDED 2026-08-29 - archived, not part of the run order.
#
# This script's logic (compute_monthly_harvest_share(),
# scaffold_season_status_lookup(), apply_season_status_correction()) was
# folded directly into pst_crc_harvest_projection.R (PART 3) and wired into
# pst_fw_angler_trips_assembly.R's own P3 step, so the correction now runs
# automatically on every pipeline run instead of as a separate manual pass -
# Evan's request once a full round of verification was done. Kept here for
# reference only; do not run this standalone anymore, it will drift from the
# real mechanism.
# ==============================================================================
#
# pst_crc_projection_season_check.R
# Location: analysis/pst/03_analysis/pst_crc_projection_season_check.R
#
# Cross-references pst_crc_harvest_projection.R's projected 2025 CRC harvest
# against WDFW's actual fishing regulations, closing the gap logged in
# _22_status_and_gaps.qmd as "2025 CRC harvest projection vs. actual season
# closures": a stream with a real regulatory closure in 2025 would otherwise
# still get a nonzero historical-mean harvest projected onto it, silently
# overstating that stream's contribution. 2025 is not a forecast year we are
# uncertain about - it already happened; whether a given fishery opened is a
# checkable fact in WDFW's own regulations, not a statistical question.
#
# WHY THIS IS A MANUAL-LOOKUP TOOL, NOT A LIVE SCRAPER
# WDFW publishes no API for this. The baseline season dates live in an annual
# PDF pamphlet (not structured data); mid-season closures/reopenings are
# announced via a free-text emergency-rules feed, keyed to water bodies WDFW
# names in prose, not to this pipeline's CRC area codes. Auto-parsing either
# source reliably enough to justify REMOVING dollars from an economic
# valuation was judged too failure-prone (Evan's call, 2026-08-28) - the risk
# is FALSE CONFIDENCE: a wrongly-parsed "open" status would leave an
# already-wrong number looking checked, which is worse than an honest open
# gap. Same design choice this pipeline already made for
# SEASON_TRUNCATED_MONTHS in pst_p2_block_ratio.R: a human-verified lookup
# table is the actual mechanism; code only does the cross-reference and the
# arithmetic once a human has filled it in.
#
# WORKFLOW
#   1. Run this script (Rscript, after pst_crc_harvest_projection.R has run
#      as part of the assembly). It reads pst_fw_crc_projection.csv (the
#      actual P3 output) and:
#        a. Writes pst_fw_crc_projection_worklist.csv - one row per projected
#           CRC area, with the two license-year pamphlets that need checking
#           (Jan-Mar 2025 falls under the "2024-25" license year; Apr-Dec
#           2025 under "2025-26" - see license_year_label() below). This is
#           the "where to look" list, refreshed every run from whatever P3 is
#           currently producing.
#        b. Upserts input_files/pst/lookup_tables/pst_season_status_lookup.csv:
#           adds a status = "UNVERIFIED" row for every (catch_area_code,
#           month) combination newly appearing in the worklist that isn't
#           already in the file. Existing rows - including ones a human has
#           already verified - are NEVER overwritten or removed by this
#           script. This file is the actual source of truth and is meant to
#           be hand-edited.
#   2. A human checks each worklist row against the WDFW pamphlet for that
#      row's license year(s), plus the emergency-rules feed, and edits
#      pst_season_status_lookup.csv's `status` column for the relevant months
#      to "open", "closed", or "restricted" (see that column's own notes).
#   3. Re-run this script. It writes
#      pst_fw_crc_projection_season_corrected.csv: same grain as
#      pst_fw_crc_projection.csv, but -
#        - an area with EVERY one of its 12 months verified "closed" has its
#          projected harvest/trips forced to zero (full override).
#        - an area with SOME months verified "closed" gets a prorated cap:
#          the fraction of its OWN historical annual harvest that typically
#          falls in those specific closed months (from the full CRC tidy
#          history, SEASONALITY_HISTORY_YEARS below) is subtracted from the
#          projection - not a guess, the same per-area seasonality basis
#          pst_crc_harvest_projection.R already computes for its Jan-Mar
#          partial-actual sanity check, generalized to arbitrary months.
#        - an area with ANY month still "UNVERIFIED" is left UNCHANGED and
#          reported separately - never assumed open by default [R2].
#        - "restricted" (e.g. a bag-limit cut, not a closure) is treated as
#          open for the arithmetic - there is no reliable way to convert a
#          bag-limit change into a harvest fraction from this pipeline's own
#          data - but is called out in the report so it stays visible rather
#          than silently folded into "open".
#      The ORIGINAL (pre-correction) total_salmon_harvest/angler_trips are
#      retained alongside the corrected values on every row - never
#      overwritten silently.
#
# Usage:
#   Rscript analysis/pst/03_analysis/pst_crc_projection_season_check.R
#   Reads analysis/pst/outputs/pst_fw_crc_projection.csv and
#   analysis/pst/outputs/crc_freshwater_harvest_2010_2024_tidy.csv (both
#   produced by earlier pipeline steps - run those first). Not yet part of
#   run_pst_pipeline.R's STEPS vector: with the lookup table starting empty,
#   running this automatically on every pipeline run would just churn the
#   worklist/scaffold with no correction ever applied. Add it once the lookup
#   table carries real verified data.
#
# Design rules: R2 gaps logged not fatal (UNVERIFIED areas are reported, not
# guessed); R5 basis labeled (season_status/season_check_basis on every row,
# original values retained for audit).
# ==============================================================================

library(tidyverse)
library(here)
library(glue)
library(cli)

OUT_DIR <- here("analysis", "pst", "outputs")
PST_DIR <- here("input_files", "pst", "lookup_tables")

PROJECTION_PATH <- file.path(OUT_DIR, "pst_fw_crc_projection.csv")
CRC_HIST_PATH   <- file.path(OUT_DIR, "crc_freshwater_harvest_2010_2024_tidy.csv")
LOOKUP_PATH     <- file.path(PST_DIR, "pst_season_status_lookup.csv")
WORKLIST_PATH   <- file.path(OUT_DIR, "pst_fw_crc_projection_worklist.csv")
CORRECTED_PATH  <- file.path(OUT_DIR, "pst_fw_crc_projection_season_corrected.csv")

TARGET_YEAR <- 2025L

# History window for the historical month-of-year harvest share used to
# prorate a partial closure - same idea as pst_crc_harvest_projection.R's
# jan_mar_share, generalized to every month. Kept as its own constant rather
# than reusing either CRC_PROJECTION_CONTROL or CRC_PROJECTION_CONTROL_PS
# (pst_crc_harvest_projection.R): those two use different windows depending
# on which control produced a given area's projection, and this check should
# rest on one stable, wide "typical seasonality" basis rather than silently
# inheriting whichever projection variant happened to apply.
SEASONALITY_HISTORY_YEARS <- 2019L:2024L

VALID_STATUSES <- c("UNVERIFIED", "open", "closed", "restricted")

# license_year runs Apr(Y)-Mar(Y+1), same convention as
# parse_crc_freshwater_harvest.R's MONTH_MAP. Calendar months 1-3 of the
# target year are governed by the license year that STARTED the previous
# April; months 4-12 by the license year starting in the target year itself.
license_year_label <- function(calendar_month, target_year = TARGET_YEAR) {
  start_year <- if_else(calendar_month <= 3L, target_year - 1L, target_year)
  glue("{start_year}-{substr(start_year + 1L, 3, 4)}")
}


# --- 1. Load P3's projected 2025 CRC areas -----------------------------------

if (!file.exists(PROJECTION_PATH)) {
  cli::cli_abort(paste(
    "Not found: {.path {PROJECTION_PATH}}. Run pst_fw_angler_trips_assembly.R",
    "first (it sources pst_crc_harvest_projection.R and writes this file)."
  ))
}

projection <- readr::read_csv(PROJECTION_PATH, show_col_types = FALSE)

if (nrow(projection) == 0) {
  cli::cli_alert_info("pst_fw_crc_projection.csv has no rows - nothing to check.")
  quit(save = "no", status = 0)
}

candidates <- projection |>
  filter(year == TARGET_YEAR) |>
  # catch_area_code as character from here on: the lookup table is a
  # hand-edited CSV (readr infers it as character), and this avoids a
  # double-vs-character join mismatch against it everywhere below.
  mutate(catch_area_code = as.character(catch_area_code)) |>
  distinct(catch_area_code, river_label, block, system,
           total_salmon_harvest, angler_trips, method)

cli::cli_alert_info(
  "{nrow(candidates)} CRC area(s) with a projected {TARGET_YEAR} harvest to check."
)


# --- 2. Worklist: the human-facing "where to look" list ----------------------
# Coarse grain (one row per area) for cross-referencing against the two
# license-year pamphlets - a human checks a river once per pamphlet, not
# once per month.

worklist <- candidates |>
  transmute(
    catch_area_code, river_label, block, system,
    projected_total_salmon_harvest = round(total_salmon_harvest, 1),
    projected_angler_trips         = round(angler_trips, 1),
    license_years_to_check         = "2024-25 (Jan-Mar) | 2025-26 (Apr-Dec)",
    projection_method              = method
  ) |>
  arrange(block, river_label)

readr::write_csv(worklist, WORKLIST_PATH)
cli::cli_alert_success("Wrote {nrow(worklist)}-row worklist to {.path {WORKLIST_PATH}}")


# --- 3. Upsert the verified-status lookup table ------------------------------
# Fine grain (one row per area x month of TARGET_YEAR) - this is what the
# capping arithmetic in step 4 needs. Existing rows are NEVER touched; only
# genuinely new (catch_area_code, month) combinations get appended, so a
# human's verification work survives every re-run.

scaffold <- candidates |>
  distinct(catch_area_code, river_label) |>
  tidyr::crossing(month = 1:12) |>
  mutate(
    license_year   = license_year_label(month),
    status         = "UNVERIFIED",
    verified_source = NA_character_,
    verified_by     = NA_character_,
    verified_date   = NA_character_,
    notes           = NA_character_
  )

if (file.exists(LOOKUP_PATH)) {
  existing <- readr::read_csv(LOOKUP_PATH, show_col_types = FALSE,
                              col_types = readr::cols(.default = "c")) |>
    mutate(month = as.integer(month))
  new_rows <- scaffold |>
    anti_join(existing, by = c("catch_area_code", "month"))
  lookup <- bind_rows(existing, new_rows) |>
    arrange(catch_area_code, month)
  if (nrow(new_rows) > 0) {
    cli::cli_alert_info(paste(
      "{nrow(new_rows)} new (area, month) row(s) added to the status lookup",
      "as UNVERIFIED - {nrow(existing)} existing row(s) (verified or not)",
      "left untouched."
    ))
  }
} else {
  lookup <- scaffold
  cli::cli_alert_warning(paste(
    "No existing lookup table found - creating {.path {LOOKUP_PATH}} with",
    "{nrow(lookup)} UNVERIFIED rows. Nothing is corrected until a human",
    "fills in `status` and re-runs this script."
  ))
}

bad_status <- lookup |> filter(!status %in% VALID_STATUSES)
if (nrow(bad_status) > 0) {
  cli::cli_abort(paste(
    "{nrow(bad_status)} row(s) in {.path {LOOKUP_PATH}} have a status not in",
    "{paste(VALID_STATUSES, collapse = ', ')}:",
    "{paste(unique(bad_status$status), collapse = ', ')}. Fix before re-running."
  ))
}

readr::write_csv(lookup, LOOKUP_PATH)


# --- 4. Historical month-of-year harvest share, per area ---------------------
# Used only to prorate a PARTIAL closure. Full closures (every month verified
# "closed") don't need this - they zero out regardless of seasonality.

if (!file.exists(CRC_HIST_PATH)) {
  cli::cli_abort(paste(
    "Not found: {.path {CRC_HIST_PATH}}. Run",
    "parse_crc_freshwater_harvest.R first."
  ))
}

crc_hist <- readr::read_csv(CRC_HIST_PATH, show_col_types = FALSE)

monthly_share <- crc_hist |>
  filter(calendar_year %in% SEASONALITY_HISTORY_YEARS) |>
  mutate(stream_code = as.character(stream_code)) |>
  group_by(stream_code, calendar_month) |>
  summarise(month_harvest = sum(harvest_count, na.rm = TRUE), .groups = "drop") |>
  group_by(stream_code) |>
  mutate(annual_harvest = sum(month_harvest, na.rm = TRUE),
         month_share    = if_else(annual_harvest > 0,
                                  month_harvest / annual_harvest, NA_real_)) |>
  ungroup() |>
  select(stream_code, calendar_month, month_share)


# --- 5. Apply the correction --------------------------------------------------

area_status <- lookup |>
  group_by(catch_area_code) |>
  summarise(
    n_unverified  = sum(status == "UNVERIFIED"),
    n_closed      = sum(status == "closed"),
    n_restricted  = sum(status == "restricted"),
    closed_months = list(sort(as.integer(month[status == "closed"]))),
    .groups = "drop"
  )

corrected <- candidates |>
  left_join(area_status, by = "catch_area_code") |>
  rowwise() |>
  mutate(
    season_status = case_when(
      n_unverified > 0                 ~ "needs_verification",
      n_closed == 12                   ~ "fully_closed",
      n_closed > 0                     ~ "partially_closed",
      TRUE                              ~ "open"
    ),
    closed_month_share = if (season_status == "partially_closed") {
      sub <- monthly_share |>
        filter(stream_code == catch_area_code,
               calendar_month %in% unlist(closed_months))
      if (nrow(sub) == 0 || any(is.na(sub$month_share))) NA_real_
      else sum(sub$month_share)
    } else {
      NA_real_
    },
    season_check_basis = case_when(
      season_status == "needs_verification" ~ glue(
        "{n_unverified} of 12 month(s) still UNVERIFIED in {basename(LOOKUP_PATH)} - ",
        "left unchanged pending manual regulatory check"
      ),
      season_status == "fully_closed" ~
        "all 12 months verified closed - projection zeroed",
      season_status == "partially_closed" & !is.na(closed_month_share) ~ glue(
        "{n_closed} month(s) verified closed, historically ",
        "{scales::percent(closed_month_share, accuracy = 0.1)} of this area's ",
        "annual harvest - projection capped by that fraction"
      ),
      season_status == "partially_closed" & is.na(closed_month_share) ~ glue(
        "{n_closed} month(s) verified closed, but this area has no usable ",
        "{min(SEASONALITY_HISTORY_YEARS)}-{max(SEASONALITY_HISTORY_YEARS)} ",
        "history to compute a seasonality share from - cannot safely prorate, ",
        "left unchanged pending manual review"
      ),
      n_restricted > 0 ~ glue(
        "verified open with {n_restricted} restricted month(s) (e.g. a bag-",
        "limit cut) - not adjusted, no reliable harvest-fraction conversion ",
        "for a restriction; flagged for awareness only"
      ),
      TRUE ~ "all 12 months verified open"
    ),
    total_salmon_harvest_corrected = case_when(
      season_status == "fully_closed" ~ 0,
      season_status == "partially_closed" & !is.na(closed_month_share) ~
        total_salmon_harvest * (1 - closed_month_share),
      TRUE ~ total_salmon_harvest
    ),
    angler_trips_corrected = if_else(
      total_salmon_harvest > 0,
      angler_trips * (total_salmon_harvest_corrected / total_salmon_harvest),
      angler_trips
    )
  ) |>
  ungroup() |>
  select(
    catch_area_code, river_label, block, system,
    season_status, season_check_basis,
    total_salmon_harvest_original     = total_salmon_harvest,
    total_salmon_harvest_corrected,
    angler_trips_original             = angler_trips,
    angler_trips_corrected,
    projection_method                 = method
  )

readr::write_csv(corrected, CORRECTED_PATH)
cli::cli_alert_success(
  "Wrote {nrow(corrected)}-row corrected projection to {.path {CORRECTED_PATH}}"
)


# --- 6. Summary ----------------------------------------------------------------

cli::cli_h1("Season check summary")

corrected |>
  count(season_status) |>
  print(n = 10)

n_changed <- sum(corrected$total_salmon_harvest_corrected !=
                   corrected$total_salmon_harvest_original)
if (n_changed > 0) {
  cli::cli_alert_warning(glue(
    "{n_changed} area(s) had their projected harvest changed by this check. ",
    "See {basename(CORRECTED_PATH)} for before/after values - this file does ",
    "NOT feed back into pst_fw_crc_projection.csv automatically; wire it in ",
    "manually once you're satisfied with the corrections."
  ))
}

n_unverified_areas <- sum(corrected$season_status == "needs_verification")
if (n_unverified_areas > 0) {
  cli::cli_alert_info(glue(
    "{n_unverified_areas} area(s) still need manual verification. See ",
    "{basename(WORKLIST_PATH)} for what to check and ",
    "{basename(LOOKUP_PATH)} to record the result."
  ))
}
