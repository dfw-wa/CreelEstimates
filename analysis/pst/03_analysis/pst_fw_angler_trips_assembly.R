# =============================================================================
# pst_fw_angler_trips_assembly.R
# Location: analysis/pst/03_analysis/pst_fw_angler_trips_assembly.R
#
# Combines angler trips with creel-derived mode/location proportions to produce
# an INTERMEDIATE table at Year x River x Mode (guided/unguided) x Location
# (bank/boat) x Angler Trips, 2022-2025.
#
# THIS IS NOT THE FINAL DELIVERABLE. It is one input to it. What's still
# outstanding before anything goes to Northern Economics:
#   - Fisheries currently DROPPED for failed trip expansion (see the blocker
#     entries in pst_fw_gap_register.csv) have to be recovered - right now
#     their effort is simply missing from block totals. Yakima 2023-2025 is
#     the live case: 13,455 effort-hrs with no total_trips_est (OQ4).
#   - mode = "unknown" rows are unresolved, not a finished answer. Note that
#     P2 and district_creel rows can NEVER be resolved by Track B - neither
#     source collects the field - so this figure will not reach zero.
#   - P3 published fallbacks (NEPA projection for PS 2025, coastal 2025) are
#     not ingested.
#   - trips vs. angler-days is still an open question with the consultant.
#   - PE_PERIOD (week vs. month) is a live decision, not a default: 17
#     fisheries differ by >15% between the two stratifications.
# Treat the output as "what we can currently support," not "what we owe."
#
# UNITS NOTE: every quantity in this pipeline is an angler TRIP count
# (total_trips_est = total_effort_hrs / mean_trip_length). The consultant's
# request is phrased in "angler days," and a trip is not always a day (some
# fisheries run multi-day trips; some anglers take more than one trip in a
# day). We are NOT converting trips to days anywhere in this script - that
# conversion, if needed, is a decision to work out with Melissa/Northern
# Economics, not an assumption to bake into the pipeline silently. Every
# object, column, and output file below says "trips."
#
# This is the ONLY script that writes the combined effort table. Everything
# upstream (creel PE runs, CRC harvest parsing, Todd's workbooks, interview
# proportions) writes to a registered path; this script reads the registry and
# stacks them.
#
# Run order on chore/multi-fishery-trip-summary:
#   1. analysis/pst/02_ingest/parse_crc_freshwater_harvest.R        -> crc_freshwater_harvest_*.csv
#   2. analysis/pst/02_ingest/multi_fishery_creel_summary.R         -> multi_fishery_creel_trips.csv
#                                                                       multi_fishery_creel_harvest.csv
#   3. analysis/pst/02_ingest/district_creel_ingestion.R            -> district_creel_summary.csv
#   4. analysis/pst/02_ingest/interview_proportions.qmd             -> interview_mode_location_props.csv
#   5. analysis/pst/03_analysis/pst_fw_angler_trips_assembly.R      <- THIS SCRIPT
#   6. analysis/pst/03_analysis/pst_fw_build_jim_workbook.R         -> PST_FW_Jim_Update.xlsx
#                                                                       (run AFTER this script)
#
# Design rules:
#   [R1] Every output row carries tier + source_id. No unattributed numbers.
#   [R2] A missing input is logged as a gap, never silently dropped and never
#        fatal. Partial assembly is the expected state until all providers return.
#   [R3] A missing dimension (e.g. Hanford has no guided field) is coded
#        "unknown", NOT zero, and NOT imputed by default.
#   [R4] Ratios never cross blocks. P2 donors are drawn within-block only.
#   [R5] Projections and donor-borrowed values are labeled in `method` and are
#        never collapsed into an unlabeled total.
# =============================================================================

library(tidyverse)
library(here)
library(glue)

# P2 within-block expansion. Keyed on catch_area_code and CRC-denominated;
# see the header of that file for why the denominator matters.
source(here("analysis", "pst", "03_analysis", "pst_p2_block_ratio.R"))

# P3 CRC-harvest projection. Reuses P2's block-pooled ratio on a projected
# (not real) CRC harvest, for blocks NEPA's own 2014+ projection doesn't
# cover. See that file's header for why block-pooled only, never block-year.
source(here("analysis", "pst", "03_analysis", "pst_crc_harvest_projection.R"))

# ---- 0. Config --------------------------------------------------------------

YEARS_SCOPE <- 2022:2025

# ColumbiaMainstem (Buoy 10 / LCR / Bonneville-McNary specifically - NOT every
# mainstem-shaped CRC reach) came to us as ODFW files that Northern Economics
# itself transmitted - they already have this data, so we do not redeliver it.
# The rows stay in the crosswalk permanently, for documentation only, and are
# filtered here rather than deleted from the source file.
#
# ColumbiaTrib split into four region-derived blocks (2026-08-19): the single
# "ColumbiaTrib" label masked that Hanford Reach and McNary are, by the CRC
# file's own region/system fields, in the same "Columbia - Upper"/"Upper
# Columbia" mainstem-reach territory as several areas that WERE correctly
# excluded as ColumbiaMainstem - a real inconsistency, not just a naming
# preference. Blocks now follow CRC's own region field directly: ColumbiaLower
# (Cowlitz/Lewis/small Lower-Columbia tributaries), ColumbiaMiddle (Drano,
# Klickitat, Wind, Big White Salmon), ColumbiaUpper (Hanford, McNary, Yakima,
# Wenatchee/Entiat/Methow/Okanogan), ColumbiaSnake (Snake River, R1_external).
DELIVER_BLOCKS <- c("PugetSound", "WACoast",
                    "ColumbiaLower", "ColumbiaMiddle", "ColumbiaUpper", "ColumbiaSnake")

OUT_DIR <- here("analysis", "pst", "outputs")
PST_DIR <- here("input_files", "pst", "lookup_tables")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Canonical schema every ingest function must return.
# catch_area_code is carried all the way through deliberately: it is the ONLY
# key that joins creel effort back to CRC harvest, which is what both the P2
# expansion and the CRC/creel bias question depend on. Collapsing to
# river_label early would make both impossible to reconstruct. Rivers can span
# several CRC areas (Quillayute is 398|400|402|404|406), so river-level
# aggregation is lossy in one direction only - keep the finer key, roll up last.
CANON <- c("block", "river_label", "fishery_name", "catch_area_code",
           "year", "month",
           "location", "mode", "angler_trips", "total_salmon_harvest",
           "tier", "source_id", "method", "location_basis", "mode_basis")

canon <- function(df) {
  missing <- setdiff(CANON, names(df))
  for (m in missing) df[[m]] <- NA
  df |>
    select(all_of(CANON)) |>
    mutate(
      location = replace_na(as.character(location), "unknown"),
      mode     = replace_na(as.character(mode),     "unknown"),
      year     = as.integer(year),
      # P1 sources (creel_pe, district_creel) carry catch_area_code as numeric,
      # coming straight off est_pe_effort(). pst_p2_block_ratio.R works in
      # character throughout -- required because crc_area_lut.csv mixes numeric
      # freshwater codes with string marine sub-area codes ("2-1", "8-2"), which
      # forces that whole column to character on read. Without this coercion,
      # every P1 batch enters effort_long as double and the P2 append at 5b
      # fails bind_rows() on the type mismatch. Character is intentionally
      # chosen (not numeric) since it's the type CRC-side data can't avoid.
      catch_area_code = as.character(catch_area_code)
    )
}

# Gap register accumulates rather than stopping the run. [R2]
.gaps <- list()
log_gap <- function(source_id, block, severity, detail) {
  .gaps[[length(.gaps) + 1]] <<- tibble(
    source_id = source_id, block = block, severity = severity, detail = detail
  )
  message(glue("[{severity}] {source_id}: {detail}"))
  invisible(NULL)
}

read_if <- function(path, source_id, block = NA, severity = "gap",
                    detail = NULL, reader = readr::read_csv) {
  if (!file.exists(path)) {
    log_gap(source_id, block, severity,
            detail %||% glue("not found at {path}"))
    return(NULL)
  }
  reader(path, show_col_types = FALSE)
}

# ---- 1. Registry ------------------------------------------------------------
# [R2]: neither read is allowed to be fatal. Both files govern real steps
# below (manifest gates blocking-source checks; crosswalk drives every
# fishery_name -> river_label/block assignment and the CRC area map) but
# their absence must degrade, not abort - see the guards at each use site.

manifest <- read_if(file.path(PST_DIR, "pst_input_manifest.csv"), "manifest",
                    detail = paste(
                      "not found - governs which upstream sources are marked",
                      "blocking; the manifest blocking-check is skipped entirely"
                    ))
if (is.null(manifest)) {
  cli::cli_alert_warning(glue(
    "pst_input_manifest.csv not found at {file.path(PST_DIR, 'pst_input_manifest.csv')} - ",
    "manifest blocking-check skipped, execution continues."
  ))
}

crosswalk <- read_if(file.path(PST_DIR, "pst_river_block_crosswalk.csv"), "crosswalk",
                     detail = paste(
                       "not found - governs fishery_name -> river_label/block",
                       "assignment and the CRC area map; every ingested row will",
                       "carry block = 'unknown' [R3] and P2 expansion cannot run",
                       "until this file is restored"
                     ))
if (is.null(crosswalk)) {
  cli::cli_alert_warning(glue(
    "pst_river_block_crosswalk.csv not found at ",
    "{file.path(PST_DIR, 'pst_river_block_crosswalk.csv')} - block assignment ",
    "cannot happen this run. Rows will carry block = 'unknown' and be excluded ",
    "from the DELIVER_BLOCKS-filtered outputs until this file is restored."
  ))
}

if (!is.null(crosswalk)) {
  # Mainstem rows are documentation-only; drop them here so every downstream
  # block test is against the delivery set. The scope decision is still logged
  # below - the note must survive even though the rows don't.
  n_mainstem <- sum(crosswalk$block == "ColumbiaMainstem", na.rm = TRUE)
  if (n_mainstem > 0) {
    log_gap("crosswalk", "ColumbiaMainstem", "note",
            glue("{n_mainstem} ColumbiaMainstem crosswalk row(s) filtered at load. ",
                 "Kept in the source file for documentation only; see the pipeline registry."))
    crosswalk <- crosswalk |> filter(block != "ColumbiaMainstem")
  }

  stopifnot(all(crosswalk$block %in% DELIVER_BLOCKS))

  if (any(crosswalk$block == "REVIEW")) {
    log_gap("crosswalk", NA, "blocker",
            "fisheries with unresolved block assignment - fix before delivery")
  }

  # Areas with no crc_areas mapping are unreachable by P2 and invisible to the
  # coverage map. Worth surfacing once at load rather than discovering later.
  n_no_area <- sum(is.na(crosswalk$crc_areas) | crosswalk$crc_areas == "")
  if (n_no_area > 0) {
    log_gap("crosswalk", NA, "gap",
            glue("{n_no_area} crosswalk row(s) have no crc_areas value - these ",
                 "cannot be reached by P2 and will not appear in area coverage."))
  }
}

if (!is.null(manifest)) {
  blocked <- manifest |> filter(blocking)
  if (nrow(blocked) > 0) {
    walk2(blocked$source_id, blocked$notes, ~ log_gap(.x, NA, "blocker", .y))
  }
}

# Every place downstream that needs fishery_name -> river_label/block MUST go
# through this helper, not a bare left_join(crosswalk, ...): when the
# crosswalk failed to load, the join is skipped and the rows are coded
# block = "unknown" [R3] instead of left_join() erroring against a NULL
# crosswalk. Behavior is byte-for-byte identical to the old bare join when
# crosswalk is present.
#
# Joins on (source_id, fishery_name) rather than fishery_name alone: once a
# single ingested file can carry more than one source_id - district_creel_
# summary.csv now mixes R1_external and R3_external rows, with R2_external
# anticipated - a fishery_name-only join risks matching a crosswalk row
# registered under the wrong source. `df` must already carry its final
# source_id column before this is called; every caller below sets source_id
# earlier in its pipe for exactly this reason.
#
# A (source_id, fishery_name) with no crosswalk match logs a gap [R2] rather
# than silently vanishing from the DELIVER_BLOCKS-filtered rollups later.
# This is the guard that makes a real gap visible: Snake River (R1) has no
# crosswalk rows yet as of this writing (see district_creel_ingestion.R), so
# without this check those rows would resolve block = "unknown" and drop out
# of section 6 with no trace in the gap register.
attach_crosswalk_block <- function(df) {
  if (is.null(crosswalk)) {
    return(df |> mutate(river_label = NA_character_, block = "unknown"))
  }
  joined <- df |>
    left_join(crosswalk |> select(source_id, fishery_name, river_label, block),
              by = c("source_id", "fishery_name"))

  unmatched <- joined |> filter(is.na(block)) |> distinct(source_id, fishery_name)
  if (nrow(unmatched) > 0) {
    pwalk(unmatched, function(source_id, fishery_name)
      log_gap(source_id, NA, "gap",
              glue("{fishery_name}: no crosswalk row for source_id='{source_id}' - ",
                   "block unresolved, coded 'unknown' and excluded from ",
                   "DELIVER_BLOCKS rollups until pst_river_block_crosswalk.csv ",
                   "is patched")))
  }

  joined |> mutate(block = coalesce(block, "unknown"))
}

# ---- 2. Track A ingest: total angler trips per river-year --------------------

## 2a. Design-based creel PE (P1) --------------------------------------------
# Reads the paired outputs of the merged multi_fishery_creel_summary.R:
#   multi_fishery_creel_trips.csv     - effort + trip expansion
#   multi_fishery_creel_harvest.csv   - catch by catch_group
#
# These come from a SINGLE database fetch and a single est_pe_effort() /
# est_pe_catch() run per fishery, so they are identical by construction on the
# shared design grain:
#   fishery_name x study_design x year x month x catch_area_code x angler_final
#   x pe_period
# Verified: total_effort_hrs matches to 0.0 across every joined row. That is
# what makes a paired trips-per-salmon ratio defensible - both sides are the
# same estimator on the same strata, not two sources reconciled after the fact.
#
# TWO THINGS THAT WILL BITE IF IGNORED:
#
# 1. pe_period. Both files stack the week- AND month-stratified runs - every
#    stratum appears twice. Summing without filtering double-counts every trip
#    (1,881,197 vs the correct ~954,177). We take PE_PERIOD and assert it.
#
# 2. is_total / catch_group. The harvest file holds one row per catch_group
#    (Chinook, Coho, Chum, Pink, Sockeye) PLUS a TotalSalmon row flagged
#    is_total = TRUE. Summing all rows double-counts the total. PST scope is
#    total salmon, so we take is_total == TRUE only, and never sum species.

PE_PERIOD <- "month"   # week|month - a decision, not a default. See header.

read_merged_creel <- function(file, source_id) {
  d <- read_if(file.path(OUT_DIR, file), source_id,
               detail = "run analysis/pst/02_ingest/multi_fishery_creel_summary.R first")
  if (is.null(d)) return(NULL)

  if (!"pe_period" %in% names(d)) {
    log_gap(source_id, NA, "blocker",
            "no pe_period column - cannot rule out stacked week+month rows")
  } else {
    if (length(unique(d$pe_period)) > 1) {
      log_gap(source_id, NA, "note",
              glue("stacks pe_period = {paste(unique(d$pe_period), collapse = '/')}; ",
                   "keeping '{PE_PERIOD}' only - summing both would double every ",
                   "estimate"))
    }
    d <- d |> filter(pe_period == PE_PERIOD)
    if (nrow(d) == 0) {
      log_gap(source_id, NA, "blocker",
              glue("no rows with pe_period == '{PE_PERIOD}'"))
      return(NULL)
    }
  }

  d |> filter(year %in% YEARS_SCOPE)
}

GRAIN <- c("fishery_name", "year", "month", "catch_area_code", "angler_final")

load_creel_harvest <- function() {
  h <- read_merged_creel("multi_fishery_creel_harvest.csv", "creel_harvest")
  if (is.null(h)) return(NULL)

  if (!"is_total" %in% names(h)) {
    log_gap("creel_harvest", NA, "blocker",
            "no is_total flag - cannot isolate TotalSalmon without summing species")
    return(NULL)
  }

  ht <- h |> filter(is_total)

  # Same NA-catch_area_code shadowing as the trips file (see ingest_creel_pe).
  # Must be dropped here too or the join fans out and harvest is double-counted.
  ht <- ht |>
    group_by(fishery_name, year, month, angler_final) |>
    filter(!(is.na(catch_area_code) & any(!is.na(catch_area_code)))) |>
    ungroup()

  dup_h <- ht |> count(across(all_of(GRAIN))) |> filter(n > 1) |> nrow()
  if (dup_h > 0) {
    log_gap("creel_harvest", NA, "blocker",
            glue("{dup_h} duplicated TotalSalmon rows at the design grain - ",
                 "the harvest join will fan out and inflate catch."))
  }

  # [S3] smolts are excluded upstream in the catch-group construction; this is
  # the total-salmon rollup the PST scope asks for, not a species sum.
  ht |>
    select(all_of(GRAIN), harvest_est, species_scope,
           harvest_cv, total_effort_hrs) |>
    rename(total_salmon_harvest = harvest_est,
           harvest_effort_hrs   = total_effort_hrs)
}

ingest_creel_pe <- function() {
  d <- read_merged_creel("multi_fishery_creel_trips.csv", "creel_pe")
  if (is.null(d)) return(NULL)

  # --- Duplicate guard 1: exact design grain -------------------------------
  dup_n <- d |> count(across(all_of(GRAIN))) |> filter(n > 1) |> nrow()
  if (dup_n > 0) {
    log_gap("creel_pe", NA, "blocker",
            glue("{dup_n} duplicated strata at {paste(GRAIN, collapse = ' x ')} ",
                 "after filtering to pe_period == '{PE_PERIOD}'. ",
                 "Totals are inflated - do not use until resolved."))
  }

  # --- Duplicate guard 2: NA catch_area_code shadowing a coded row ---------
  # Guard 1 cannot catch this: NA != 317, so a stratum emitted twice - once
  # coded, once with catch_area_code = NA - looks like two distinct rows.
  # Observed in Lower Chehalis salmon 2023: all 8 month strata duplicated with
  # byte-identical total_effort_hrs, inflating the pipeline by 16,541 trips
  # (1.8%). The NA copies are dropped; if a fishery is ONLY ever NA-coded that
  # is a different problem (unmappable area) and is left alone and logged.
  if (any(is.na(d$catch_area_code))) {
    shadow <- d |>
      group_by(fishery_name, year, month, angler_final) |>
      filter(any(is.na(catch_area_code)) & any(!is.na(catch_area_code))) |>
      ungroup()
    if (nrow(shadow) > 0) {
      shadow_eff <- shadow |> filter(is.na(catch_area_code)) |>
        summarise(e = sum(total_effort_hrs, na.rm = TRUE),
                  t = sum(total_trips_est, na.rm = TRUE))
      log_gap("creel_pe", NA, "defect",
              glue("{paste(unique(shadow$fishery_name), collapse = ', ')}: ",
                   "strata emitted TWICE, once with a catch_area_code and once ",
                   "with NA, same total_effort_hrs. Dropping the NA copies ",
                   "({format(round(shadow_eff$e), big.mark = ',')} effort-hrs / ",
                   "{format(round(shadow_eff$t), big.mark = ',')} trips of ",
                   "double-count). FIX UPSTREAM in multi_fishery_creel_summary.R ",
                   "- the section_num -> crc_area join is not matching for ",
                   "every section."))
      d <- d |>
        group_by(fishery_name, year, month, angler_final) |>
        filter(!(is.na(catch_area_code) & any(!is.na(catch_area_code)))) |>
        ungroup()
    }
    orphan <- d |> filter(is.na(catch_area_code))
    if (nrow(orphan) > 0) {
      log_gap("creel_pe", NA, "gap",
              glue("{nrow(orphan)} rows have catch_area_code = NA with no coded ",
                   "counterpart - unmappable to a CRC area, retained in trip ",
                   "totals but excluded from area-based coverage and from the ",
                   "P2 donor pool."))
    }
  }

  # --- angler_final = "fail" -----------------------------------------------
  # Not a location. It's an unresolved classification leaking out of the Drano
  # PE branch. Coded "unknown" per [R3] rather than silently binned as bank or
  # boat, and logged so it gets fixed upstream.
  if (any(d$angler_final == "fail", na.rm = TRUE)) {
    f <- d |> filter(angler_final == "fail")
    log_gap("creel_pe", NA, "defect",
            glue("{nrow(f)} rows with angler_final = 'fail' ",
                 "({paste(unique(f$fishery_name), collapse = ', ')}) carrying ",
                 "{format(round(sum(f$total_effort_hrs, na.rm = TRUE)), big.mark = ',')} ",
                 "effort-hrs. This is a failed bank/boat classification, not a ",
                 "location - recoded 'unknown'. Fix in the Drano PE branch."))
    d <- d |> mutate(angler_final = if_else(angler_final == "fail",
                                            NA_character_, angler_final))
  }

  # Diagnose what we're about to lose BEFORE filtering. A fishery with real
  # effort hours but no total_trips_est is not a data gap - it's a broken
  # trip expansion, and silently dropping it understates the block. [R2]
  lost <- d |>
    group_by(fishery_name) |>
    summarise(
      n_rows     = n(),
      trips_ok   = sum(!is.na(total_trips_est)),
      effort_hrs = sum(total_effort_hrs, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(trips_ok == 0)

  if (nrow(lost) > 0) {
    gap_only       <- lost |> filter(effort_hrs == 0)
    expansion_fail <- lost |> filter(effort_hrs > 0)

    walk2(gap_only$fishery_name, gap_only$n_rows, \(fn, n)
      log_gap("creel_pe", NA, "gap",
              glue("{fn}: no effort and no trips across {n} rows - ",
                   "genuine data gap, not recoverable from this input")))

    walk2(expansion_fail$fishery_name, expansion_fail$effort_hrs, \(fn, hrs)
      log_gap("creel_pe", NA, "blocker",
              glue("{fn}: {format(round(hrs), big.mark = ',')} effort-hrs ",
                   "present but total_trips_est is NA for every row - trip ",
                   "expansion failed. DROPPED; block total is an undercount. ",
                   "Recover via the [S4] within-season trip-length donor ",
                   "hierarchy in multi_fishery_creel_summary.R.")))
  }

  # Attach paired total-salmon harvest from the same estimator run. This is a
  # left join FROM trips: a stratum with real effort but no TotalSalmon row
  # means zero salmon were harvested there, not a missing record, so harvest
  # coalesces to 0 while trips are preserved. (37 such strata in the current
  # run - Quillayute, Chehalis, Nisqually and others - carrying ~1,800 real
  # trips that a harvest-side inner join would have silently deleted.)
  harv <- load_creel_harvest()
  if (!is.null(harv)) {
    d <- d |>
      left_join(harv |> select(all_of(GRAIN), total_salmon_harvest,
                               species_scope, harvest_cv),
                by = GRAIN) |>
      mutate(
        harvest_matched      = !is.na(total_salmon_harvest),
        total_salmon_harvest = coalesce(total_salmon_harvest, 0)
      )
    n_zero <- sum(!d$harvest_matched & d$total_trips_est > 0, na.rm = TRUE)
    if (n_zero > 0) {
      log_gap("creel_harvest", NA, "note",
              glue("{n_zero} strata have effort/trips but no TotalSalmon row - ",
                   "treated as zero harvest, trips retained. Trips-per-salmon ",
                   "is undefined for these and they are excluded from ratio ",
                   "derivation, not from the trip totals."))
    }
  } else {
    d <- d |> mutate(total_salmon_harvest = NA_real_, harvest_matched = FALSE)
  }

  # trip_expansion and trip_length_source distinguish own-month estimates from
  # donor-borrowed trip lengths, which [R5] requires be labelled not absorbed.
  d |>
    filter(!is.na(total_trips_est)) |>
    mutate(source_id = "creel_pe") |>
    attach_crosswalk_block() |>
    mutate(
      location       = coalesce(angler_final, "unknown"),
      mode           = "unknown",           # resolved in Track B  [R3]
      angler_trips   = total_trips_est,
      tier           = "P1",
      method         = glue("design-based PE (pe_period={PE_PERIOD}); ",
                            "trip_length_source={trip_length_source}; ",
                            "{trip_expansion}"),
      location_basis = "design_stratum",
      mode_basis     = "pending_track_b"
    ) |>
    canon()
}

## 2b. district_creel: pre-computed external district totals (P1, no mode) ---
# Trip totals calculated by WDFW district staff in their own spreadsheets -
# NOT run through this repo's design-based PE estimators. Currently R3 (Todd
# Miller: Hanford Reach / Yakima / McNary) and R1 (Jeremy Trump: Snake
# River); R2 is expected to contribute a workbook eventually - see
# district_creel_ingestion.R's header. source_id is derived per row from the
# ingestion script's `district` column (R1_external / R3_external, etc.)
# rather than fixed to one district, because that assumption stopped holding
# the moment a second district landed in the same file. Named for the
# source, not the person, because that's the fact that matters for the
# provenance ledger: this is an unvetted external calculation. We don't have
# the capacity to audit each district's own effort-expansion methodology, so
# it stays a visibly distinct source_id rather than folded into "creel_pe"
# where it would read as equivalent to a PE run.
#
# CRC areas (Todd, 2026-08-11, OQ1): Yakima 690, McNary 533, Hanford Reach
# 534|535|536. Hanford is a composite - the Summary sheet gives one fleet-wide
# weekly total with no section breakdown, so catch_area_code stays NA on these
# rows and the crosswalk carries area_coverage = "covered_unpartitioned" to
# stop P2 re-expanding those three areas. See section 3. Snake River (R1) is
# the same composite-area situation (six CRC areas, no per-area breakdown in
# source) and additionally has NO crosswalk rows at all yet - those rows will
# resolve block = "unknown" via attach_crosswalk_block()'s unmatched-row gap
# log until pst_river_block_crosswalk.csv is patched.

ingest_district_creel <- function() {
  d <- read_if(file.path(OUT_DIR, "district_creel_summary.csv"),
               "district_creel")
  if (is.null(d)) return(NULL)

  if (!"district" %in% names(d)) {
    log_gap("district_creel", NA, "blocker",
            paste("no district column - cannot attribute rows to a",
                  "source_id; re-run district_creel_ingestion.R"))
    return(NULL)
  }

  # DEFECT 2026-08-13: 'Hanford Reach fall Chinook 2025' rows carry year = 2024.
  # Trust the fishery_name year token over the year column until the ingestion
  # script is patched, and log every row we correct.
  d <- d |>
    mutate(year_from_name = suppressWarnings(
             as.integer(str_extract(fishery_name, "\\d{4}(?=\\s*$)")))) |>
    mutate(year_mismatch = !is.na(year_from_name) & year_from_name != year)
  if (any(d$year_mismatch, na.rm = TRUE)) {
    log_gap("district_creel", NA, "defect",
            glue("{sum(d$year_mismatch, na.rm = TRUE)} rows where year column ",
                 "disagrees with fishery_name year token; using name token. ",
                 "Patch district_creel_ingestion.R."))
  }

  d <- d |>
    mutate(year      = coalesce(year_from_name, as.integer(year)),
           source_id = paste0(district, "_external")) |>
    filter(year %in% YEARS_SCOPE)

  # Same silent-drop hazard as creel_pe: log any district fishery-year that
  # has effort but no trips rather than letting it vanish. [R2]
  district_lost <- d |>
    group_by(source_id, fishery_name) |>
    summarise(trips_ok   = sum(!is.na(total_trips_est)),
              effort_hrs = sum(total_effort_hrs, na.rm = TRUE),
              .groups = "drop") |>
    filter(trips_ok == 0)
  if (nrow(district_lost) > 0) {
    pwalk(district_lost, function(source_id, fishery_name, trips_ok, effort_hrs)
      log_gap(source_id, NA,
              if (effort_hrs > 0) "blocker" else "gap",
              glue("{fishery_name}: no usable total_trips_est",
                   if (effort_hrs > 0) glue(" despite {format(round(effort_hrs), big.mark = ',')} effort-hrs - expansion failed, DROPPED")
                   else " and no effort - genuine gap")))
  }

  # District files name the area column crc_area, not catch_area_code.
  # Without this rename canon() would emit an all-NA catch_area_code for the
  # whole block and the CRC join would silently find nothing.
  if ("crc_area" %in% names(d) && !"catch_area_code" %in% names(d)) {
    d <- d |> rename(catch_area_code = crc_area)
  }

  d |>
    filter(!is.na(total_trips_est)) |>
    attach_crosswalk_block() |>
    mutate(
      location       = coalesce(angler_final, "unknown"),
      mode           = "unknown",           # structural gap, not zero  [R3]
      angler_trips   = total_trips_est,
      tier           = "P1",
      method         = glue("external district-supplied calculation, not ",
                            "independently vetted by WDFW HQ ({data_provider})"),
      location_basis = "design_stratum",
      mode_basis     = "not_collected"
    ) |>
    canon()
}

## 2c. Columbia mainstem - OUT OF SCOPE, not ingested -------------------------
# Buoy 10 / Lower Columbia / Bonneville-McNary came to us as ODFW files that
# Northern Economics itself transmitted - they already have this data, so we do
# not redeliver it. No parser, no ingestion. The old blockers (undocumented LCR
# scalar, Bonneville-McNary totals-only, the Buoy 10 Charter Boat mode
# question) are moot for our scope; they only matter again if Jim/Melissa ask
# us to independently reproduce or reconcile against what the consultant has.
#
# The crosswalk rows are removed (section 1) but this note stays: without it,
# the gap register would suggest mainstem was never considered.

log_gap("ColumbiaMainstem", "ColumbiaMainstem", "note",
        paste("out of delivery scope - Buoy 10 / LCR / Bonneville-McNary",
              "were transmitted to us by the consultant via ODFW; not",
              "ingested and not written to the output."))

## 2d. Published fallbacks: NEPA / coastal (P3) -------------------------------
# Only reached for river-years with no P1 and no within-block P2 donor.
# 2025 is the live case: CRC harvest stops at license year 2024.

ingest_published_fallback <- function() {
  log_gap("nepa_workbook", "PugetSound", "note",
          "2025 PS values are a projection; must be labeled method='projection'")
  log_gap("coastal_2025", "WACoast", "gap",
          "no 2025 fallback identified for coastal freshwater - open item")
  NULL
}

# ---- 3. Track A P2: within-block ratio expansion ----------------------------
# Two distinct quantities live here and must not be confused:
#
#   (a) The DESCRIPTIVE trips-per-salmon ratio, creel trips over creel harvest.
#       Both sides come from the same est_pe_effort()/est_pe_catch() run on the
#       same strata, so it is an internally consistent statement about fishery
#       behaviour. Reported below and written to pst_fw_p2_block_ratios.csv.
#
#   (b) The EXPANSION ratio, creel trips over CRC harvest. This is what P2
#       actually applies, because the thing being expanded is a CRC harvest
#       figure. Using (a) on a CRC numerator would assume CRC and creel harvest
#       agree; section 6b shows the median CRC/creel ratio is 0.77 with an IQR
#       of 0.52-1.93, so that assumption would understate uncovered areas by
#       roughly a quarter at the median. Computed inside pst_p2_block_ratio.R.
#
# Empirical justification for [R4] (JSR-verified, Spring T24 / Fall T25-26):
# Buoy 10 1.4-2.8 trips/salmon, LCR fall (Aug-Oct) 2.75-3.95, LCR spring
# (Feb-Jun15) 7.0-13.0, LCR summer (Jun16-Jul) 9.8-60.7 (the 60.7 is real -
# Jun16-30 2025 and Jul 2024 both had zero kept Chinook against thousands of
# trips), Hanford Reach 2.48, Puget Sound 3.5-4.8. No defensible single rate at
# any scale broader than one fishery-month.

build_block_ratios <- function(trips_p1) {
  if (is.null(trips_p1)) return(NULL)

  # --- 3a. Descriptive trips-per-salmon, creel-denominated -------------------
  # Aggregate to block x year before dividing. A ratio of sums, not a mean of
  # per-stratum ratios: strata with tiny harvest produce enormous unstable
  # ratios that a simple mean would let dominate.
  paired <- trips_p1 |>
    filter(!is.na(total_salmon_harvest), !is.na(angler_trips)) |>
    group_by(block, year) |>
    summarise(
      trips       = sum(angler_trips, na.rm = TRUE),
      harvest     = sum(total_salmon_harvest, na.rm = TRUE),
      n_fisheries = n_distinct(fishery_name),
      .groups = "drop"
    ) |>
    mutate(trips_per_salmon = if_else(harvest > 0, trips / harvest, NA_real_))

  zero_harv <- paired |> filter(is.na(trips_per_salmon))
  if (nrow(zero_harv) > 0) {
    walk2(zero_harv$block, zero_harv$year, \(b, y)
      log_gap("creel_harvest", b, "gap",
              glue("{b} {y}: creel trips present but zero total-salmon harvest ",
                   "- trips-per-salmon undefined, no descriptive ratio for this ",
                   "block-year")))
  }

  ratios <- paired |> filter(!is.na(trips_per_salmon))
  if (nrow(ratios) > 0) {
    walk(seq_len(nrow(ratios)), \(i) {
      r <- ratios[i, ]
      log_gap("p2_ratio", r$block, "note",
              glue("{r$block} {r$year}: {round(r$trips_per_salmon, 2)} ",
                   "trips/salmon (creel-denominated) from {r$n_fisheries} creel ",
                   "fisheries ({format(round(r$trips), big.mark = ',')} trips / ",
                   "{format(round(r$harvest), big.mark = ',')} salmon)"))
    })
  }

  # --- 3b. CRC harvest, the P2 denominator and expansion base ----------------
  # crc_area_lut.csv's catch_area_code is character, not numeric: it carries a
  # handful of marine sub-area codes ("2-1", "2-2", "8-1", "8-2") alongside the
  # numeric freshwater codes, which forces the whole column to character on
  # read. stream_code in the CRC harvest file is purely numeric and comes in as
  # a double. Coerce both sides explicitly - inferred types won't agree.
  crc <- read_if(file.path(OUT_DIR, "crc_freshwater_harvest_2010_2024_tidy.csv"),
                 "crc_harvest")
  crc_yr <- NULL
  crc_month <- NULL
  if (!is.null(crc)) {
    crc_lut <- read_csv(here("input_files", "pst", "lookup_tables", "crc_area_lut.csv"),
                        show_col_types = FALSE) |>
      mutate(catch_area_code = as.character(catch_area_code)) |>
      distinct(catch_area_code, catch_area_region)

    crc_yr <- crc |>
      filter(calendar_year %in% YEARS_SCOPE) |>
      mutate(stream_code = as.character(stream_code)) |>
      left_join(crc_lut, by = c("stream_code" = "catch_area_code")) |>
      group_by(catch_area_region, stream_code, calendar_year) |>
      summarise(harvest = sum(harvest_count, na.rm = TRUE), .groups = "drop")

    # Month grain, kept alongside crc_yr rather than replacing it: apply_p2()
    # still expands a TARGET area's full-year CRC harvest (that's the point -
    # a full-year trip estimate for an area with no survey at all), but the
    # DONOR ratio in build_p2_donors() must not divide creel trips by CRC
    # harvest from months the creel survey never ran - that inflates the
    # denominator with harvest the trips side had no chance to explain and
    # biases the ratio low. build_p2_donors() uses this table to restrict CRC
    # harvest to the same months effort_long actually has creel trips in, per
    # area-year - driven off effort_long's own month field so it self-corrects
    # as more creel months arrive rather than needing a season list maintained
    # by hand.
    crc_month <- crc |>
      filter(calendar_year %in% YEARS_SCOPE) |>
      mutate(
        stream_code = as.character(stream_code),
        # Raw region/system text carries CRLF and double-space noise (e.g.
        # "Columbia -  Middle" vs "Columbia - Middle" for the same
        # stream_code) - squish before using either as a grouping key, or
        # identical systems silently split into separate donor groups.
        region      = str_squish(str_replace_all(region, "[\r\n]+", " ")),
        system      = str_squish(str_replace_all(system, "[\r\n]+", " "))
      ) |>
      group_by(stream_code, calendar_year, calendar_month, region, system) |>
      summarise(harvest = sum(harvest_count, na.rm = TRUE), .groups = "drop")

    log_gap("crc_harvest", NA, "note",
            paste("license-year basis (Apr 1 - Mar 31): calendar 2025 has only",
                  "Jan-Mar coverage and calendar 2022 depends on the FW 2022-23",
                  "file. Confirm with Heidi before P2 is used for edge years."))
  }

  list(ratios = ratios, crc = crc_yr, crc_month = crc_month)
}

# ---- 4. Track B: mode x location -------------------------------------------
# location  = design stratum from est_pe_effort() -> already on the P1 rows
# mode      = post-stratification of interviews within the boat stratum,
#             applied across all four combined categories (Guided-Bank,
#             Guided-Boat, Unguided-Bank, Unguided-Boat) under A1-A4.
# Sample size - not structural assumption - governs fallback to block-level
# proportions.

MIN_INTERVIEWS <- 30   # below this, fall back to block pooled proportions

apply_track_b <- function(trips) {
  props <- read_if(file.path(OUT_DIR, "interview_mode_location_props.csv"),
                   "interview_prop",
                   detail = paste("run analysis/pst/02_ingest/interview_proportions.qmd and",
                                  "export the fishery x year x location x mode",
                                  "proportion table to this path"))
  if (is.null(props) || is.null(trips)) {
    log_gap("interview_prop", NA, "blocker",
            "no mode proportions available - output stays mode='unknown'")
    return(trips)
  }

  # Normalize the join key BEFORE joining, not after failing silently. A case
  # or spelling mismatch between this table's `location` ("Boat"/"Bank", as
  # recorded by angler_final) and interview_proportions.qmd's `location`
  # column produces exactly the failure mode this guards against: every join
  # misses, prop is NA everywhere, and mode = "unknown" on every row with no
  # visible error - indistinguishable from Track B never having run at all.
  norm_loc <- function(x) tolower(trimws(as.character(x)))

  trips <- trips |> mutate(.location_norm = norm_loc(location))
  props <- props |> mutate(.location_norm = norm_loc(location)) |>
    select(-location)

  fishery_lvl <- props |> filter(n_interviews >= MIN_INTERVIEWS)

  # Block-pooled fallback needs fishery_name -> block from the crosswalk. If
  # the crosswalk didn't load, there is no way to pool by block [R2]: every
  # row simply has no prop_block to fall back on below and, absent a
  # fishery-level match too, resolves to mode = "unknown" via
  # no_proportion_available, same as any other unresolved row. [R3]
  block_lvl <- if (is.null(crosswalk)) {
    tibble(block = character(), .location_norm = character(),
          mode = character(), prop = double())
  } else {
    props |>
      left_join(crosswalk |> select(fishery_name, block), by = "fishery_name") |>
      group_by(block, .location_norm, mode) |>
      summarise(prop = weighted.mean(prop, n_interviews), .groups = "drop") |>
      group_by(block, .location_norm) |>
      mutate(prop = prop / sum(prop)) |>
      ungroup()
  }

  splittable <- trips |> filter(mode_basis == "pending_track_b")
  passthru   <- trips |> filter(mode_basis != "pending_track_b")

  out <- splittable |>
    select(-mode) |>
    left_join(fishery_lvl |> select(fishery_name, year, .location_norm, mode, prop),
              by = c("fishery_name", "year", ".location_norm"),
              relationship = "many-to-many") |>
    left_join(block_lvl |> rename(prop_block = prop),
              by = c("block", ".location_norm", "mode")) |>
    mutate(
      used_block   = is.na(prop),
      prop         = coalesce(prop, prop_block),
      mode         = coalesce(mode, "unknown"),
      angler_trips = angler_trips * coalesce(prop, 1),
      # Harvest has to be apportioned by the SAME proportion. Splitting one
      # stratum into guided/unguided while leaving harvest at its full value on
      # each row would duplicate the catch and wreck any downstream
      # trips-per-salmon computed off this table.
      total_salmon_harvest = total_salmon_harvest * coalesce(prop, 1),
      mode_basis = case_when(
        is.na(prop) ~ "no_proportion_available",
        used_block  ~ glue("block_pooled (n < {MIN_INTERVIEWS})"),
        TRUE        ~ "fishery_year_interviews"
      )
    ) |>
    select(-prop, -prop_block, -used_block, -.location_norm)

  result <- bind_rows(out, passthru |> select(-.location_norm)) |> canon()

  # Report the match rate instead of letting a 100%-unknown result pass
  # unremarked. This is the check that would have caught this exact failure.
  splittable_trips <- sum(splittable$angler_trips, na.rm = TRUE)
  resolved_trips   <- sum(out$angler_trips[out$mode != "unknown"], na.rm = TRUE)
  pct_resolved <- if (splittable_trips > 0) {
    round(100 * resolved_trips / splittable_trips, 1)
  } else NA_real_

  if (!is.na(pct_resolved) && pct_resolved == 0) {
    log_gap("track_b_join", NA, "blocker",
            glue("apply_track_b() resolved 0% of splittable trips ({nrow(splittable)} ",
                 "rows attempted). The fishery_name x year x location join is not ",
                 "matching ANYTHING against interview_mode_location_props.csv. ",
                 "Check location value spelling/casing (this run compared ",
                 "case-insensitively after trimming whitespace, so the mismatch ",
                 "is deeper - likely fishery_name spelling or year type)."))
  } else if (!is.na(pct_resolved) && pct_resolved < 50) {
    log_gap("track_b_join", NA, "gap",
            glue("apply_track_b() resolved only {pct_resolved}% of splittable ",
                 "trips ({nrow(splittable)} rows attempted) - most rows are ",
                 "falling through to mode='unknown'. Spot-check fishery_name ",
                 "matches between the trips table and interview_mode_location_props.csv."))
  } else if (!is.na(pct_resolved)) {
    log_gap("track_b_join", NA, "note",
            glue("apply_track_b() resolved {pct_resolved}% of splittable trips."))
  }

  result
}

# ---- 5. Assemble ------------------------------------------------------------

trips_p1 <- bind_rows(ingest_creel_pe(), ingest_district_creel())
invisible(ingest_published_fallback())

p2 <- build_block_ratios(trips_p1)

effort_long <- apply_track_b(trips_p1)

# ---- 5b. P2 expansion -------------------------------------------------------
# Runs AFTER Track B on purpose: P2 rows carry no fishery_name and no location,
# so passing them through the proportion join would accomplish nothing and
# risks a fan-out. They enter the stack already canonical and already
# mode/location = "unknown" per [R3].

# run_p2_extrapolation() (pst_p2_block_ratio.R) expands crc_areas on the
# crosswalk unconditionally once crc_yr is non-NULL, so a NULL crosswalk must
# be intercepted here rather than passed through. [R2]
p2x <- if (is.null(crosswalk)) {
  NULL
} else {
  run_p2_extrapolation(
    effort_long    = effort_long,
    crc_yr         = p2$crc,
    crc_month      = p2$crc_month,
    crosswalk      = crosswalk,
    deliver_blocks = DELIVER_BLOCKS,
    years_scope    = YEARS_SCOPE
  )
}

if (!is.null(p2x)) {
  effort_long <- bind_rows(effort_long, canon(p2x$trips))

  if (nrow(p2x$gaps) > 0) {
    walk(seq_len(nrow(p2x$gaps)), \(i) {
      g <- p2x$gaps[i, ]
      log_gap("p2_expansion", g$block, "gap",
              glue("area {g$catch_area_code} {g$year}: {g$reason}"))
    })
  }

  write_csv(p2x$ratios, file.path(OUT_DIR, "pst_fw_p2_area_ratios.csv"))
  write_csv(p2x$donors, file.path(OUT_DIR, "pst_fw_p2_donors.csv"))
  if (nrow(p2x$loo) > 0) {
    write_csv(p2x$loo,         file.path(OUT_DIR, "pst_fw_p2_loo_detail.csv"))
    write_csv(p2x$loo_summary, file.path(OUT_DIR, "pst_fw_p2_loo_summary.csv"))
  } else {
    log_gap("p2_loo", NA, "note",
            glue("no block-year had more than {P2_CONTROL$min_donor_areas} donor ",
                 "areas, so the leave-one-out check could not run. P2 output ",
                 "currently carries no empirical error band."))
  }
} else {
  log_gap("p2_expansion", NA, "blocker",
          "P2 did not run - no crosswalk, no CRC table, or no CRC/creel area overlap.")
}

# ---- 5c. P3 CRC-harvest projection -------------------------------------------
# Runs AFTER P2 on purpose: "already covered?" for this tier means covered by
# EITHER P1 or real P2, so effort_long must already carry the P2 merge above.
# Never touches PugetSound - that block's 2025 projection is NEPA-derived,
# a separate longer even/odd series back to 2014, not this 6-year CRC mean.
#
# crc_hist is read independently here rather than reusing p2$crc: p2$crc is
# filtered to YEARS_SCOPE (2022-2025), which drops 2019-2021 - too late for a
# 6-year mean. This re-reads the same file build_block_ratios() already read
# once internally; the duplicate read is deliberate so build_block_ratios()
# itself stays untouched.
crc_hist <- read_if(file.path(OUT_DIR, "crc_freshwater_harvest_2010_2024_tidy.csv"),
                    "crc_harvest_history")

p3x <- if (is.null(p2x)) {
  message(paste("[gap] crc_projection: P2 did not run, so no donor pairs are",
               "available to derive a ratio from; P3 projection skipped."))
  NULL
} else {
  run_crc_projection(
    crc_hist       = crc_hist,
    effort_long    = effort_long,
    donors         = p2x$donors,
    crosswalk      = crosswalk,
    deliver_blocks = DELIVER_BLOCKS
  )
}

if (!is.null(p3x)) {
  effort_long <- bind_rows(effort_long, canon(p3x$trips))

  if (nrow(p3x$gaps) > 0) {
    walk(seq_len(nrow(p3x$gaps)), \(i) {
      g <- p3x$gaps[i, ]
      log_gap("crc_projection", g$block, "gap",
              glue("area {g$catch_area_code}: {g$reason}"))
    })
  }

  write_csv(p3x$trips,        file.path(OUT_DIR, "pst_fw_crc_projection.csv"))
  write_csv(p3x$sanity_check, file.path(OUT_DIR, "pst_fw_crc_projection_sanity.csv"))
  write_csv(p3x$coverage,     file.path(OUT_DIR, "pst_fw_crc_projection_coverage.csv"))
}

# ---- 5d. NEPA vs. pure-CRC 5-year even/odd mean comparison (diagnostic) -----
# Purely diagnostic - does not feed effort_long or any deliverable row. Only
# runs if the NEPA workbook is present; a missing workbook is a logged gap,
# never fatal (R2). See pst_crc_harvest_projection.R PART 2 header for why
# this comparison exists and what it does/doesn't decide.
nepa_cmp <- run_nepa_pure_crc_comparison(
  crc_hist = crc_hist,
  crosswalk = crosswalk,
  nepa_dir = here("input_files", "pst", "external_data")
)

if (!is.null(nepa_cmp)) {
  write_csv(nepa_cmp, file.path(OUT_DIR, "pst_fw_nepa_vs_pure_crc_comparison.csv"))
}

# ---- 6. Intermediate output (NOT the deliverable) ---------------------------
# Year x River x Mode x Location x Angler Trips, rolled up from month grain.
# Filtered to DELIVER_BLOCKS: nothing ingests ColumbiaMainstem today, but the
# filter is explicit here too so a future ingestion function added under
# section 2 can't silently leak consultant-supplied data back into the output.
#
# Two roll-ups are written rather than one. The consultant asked for river
# grain, but rivers can span several CRC areas (Quillayute = 398|400|402|404|
# 406), so rolling to river destroys the only key that joins to CRC harvest.
# The area-grain table is the one to use for the CRC/creel bias work.

effort_by_area <- effort_long |>
  filter(block %in% DELIVER_BLOCKS) |>
  group_by(block, river_label, catch_area_code, year, mode, location) |>
  summarise(
    angler_trips         = sum(angler_trips, na.rm = TRUE),
    total_salmon_harvest = sum(total_salmon_harvest, na.rm = TRUE),
    tier      = paste(sort(unique(tier)), collapse = "|"),
    source_id = paste(sort(unique(source_id)), collapse = "|"),
    .groups = "drop"
  ) |>
  mutate(trips_per_salmon = if_else(total_salmon_harvest > 0,
                                    angler_trips / total_salmon_harvest,
                                    NA_real_)) |>
  arrange(block, river_label, catch_area_code, year, location, mode)

effort_by_mode_location <- effort_long |>
  filter(block %in% DELIVER_BLOCKS) |>
  group_by(block, river_label, year, mode, location) |>
  summarise(
    angler_trips         = sum(angler_trips, na.rm = TRUE),
    total_salmon_harvest = sum(total_salmon_harvest, na.rm = TRUE),
    # Preserved as a pipe-delimited list so the river row can still be traced
    # back to the areas behind it without re-running the assembly.
    catch_area_codes = paste(sort(unique(na.omit(catch_area_code))),
                             collapse = "|"),
    tier      = paste(sort(unique(tier)), collapse = "|"),
    source_id = paste(sort(unique(source_id)), collapse = "|"),
    method    = paste(sort(unique(method)), collapse = "; "),
    .groups = "drop"
  ) |>
  mutate(trips_per_salmon = if_else(total_salmon_harvest > 0,
                                    angler_trips / total_salmon_harvest,
                                    NA_real_)) |>
  arrange(block, river_label, year, location, mode)

# ---- 6b. CRC vs creel: the bias question ------------------------------------
# Both sides estimate the same quantity - total salmon kept in a CRC area-year
# - by completely different means. Creel is a design-based on-the-ground
# estimate; CRC is an angler-reported card expansion. Their ratio is the
# empirical handle on the reporting bias the framework flags, and it is only
# computable because catch_area_code survives to here.
#
# P1 ONLY. P2 rows take their harvest straight from the CRC file, so including
# them would compare CRC against itself and drag the ratio toward 1.
#
# Basis mismatch is real and not a bug: CRC is license-year (Apr 1 - Mar 31)
# while creel is calendar-month. Indicative at annual grain, not a
# month-for-month reconciliation.

crc_vs_creel <- NULL
if (!is.null(p2$crc)) {
  creel_area <- effort_long |>
    filter(block %in% DELIVER_BLOCKS, tier == "P1", !is.na(catch_area_code)) |>
    group_by(catch_area_code, year) |>
    summarise(creel_trips   = sum(angler_trips, na.rm = TRUE),
              creel_harvest = sum(total_salmon_harvest, na.rm = TRUE),
              fisheries     = paste(sort(unique(fishery_name)), collapse = "|"),
              .groups = "drop") |>
    mutate(catch_area_code = as.character(catch_area_code))

  crc_vs_creel <- creel_area |>
    inner_join(p2$crc |>
                 select(catch_area_code = stream_code,
                        year = calendar_year,
                        crc_harvest = harvest),
               by = c("catch_area_code", "year")) |>
    mutate(
      crc_minus_creel = crc_harvest - creel_harvest,
      crc_over_creel  = if_else(creel_harvest > 0,
                                crc_harvest / creel_harvest, NA_real_),
      trips_per_salmon_creel = if_else(creel_harvest > 0,
                                       creel_trips / creel_harvest, NA_real_),
      trips_per_salmon_crc   = if_else(crc_harvest > 0,
                                       creel_trips / crc_harvest, NA_real_),
      # CRC is published on a license year and stops at license year 2024, so
      # calendar 2025 rows join but carry ~zero harvest. That is a coverage
      # artifact, not a real reporting collapse - flag it rather than let a
      # 0.00 ratio be read as a finding. [R3]
      comparable = !(crc_harvest == 0 & creel_harvest > 0),
      note = if_else(comparable, NA_character_,
                     "CRC coverage absent for this year - ratio not meaningful")
    ) |>
    arrange(desc(abs(crc_minus_creel)))

  n_incomp <- sum(!crc_vs_creel$comparable)
  if (n_incomp > 0) {
    log_gap("crc_vs_creel", NA, "note",
            glue("{n_incomp} area-years have creel harvest but zero CRC ",
                 "harvest - CRC is published through license year 2024 only. ",
                 "Excluded from the median ratio and flagged comparable=FALSE."))
  }

  if (nrow(crc_vs_creel) == 0) {
    log_gap("crc_vs_creel", NA, "gap",
            paste("no CRC area-years overlap the creel areas - the bias",
                  "comparison cannot be computed. Check that CRC stream_code",
                  "and creel catch_area_code use the same coding scheme."))
  } else {
    med <- median(crc_vs_creel$crc_over_creel[crc_vs_creel$comparable],
                  na.rm = TRUE)
    log_gap("crc_vs_creel", NA, "note",
            glue("{sum(crc_vs_creel$comparable)} comparable CRC area-years ",
                 "paired with creel; median CRC/creel harvest ratio ",
                 "{round(med, 2)}. Below 1 means CRC reports less harvest than ",
                 "the design-based creel estimate for the same area-year. ",
                 "Basis differs (CRC license-year vs creel calendar) - ",
                 "indicative, not a reconciliation."))
  }
}

provenance <- effort_long |>
  count(block, river_label, year, tier, source_id, method,
        location_basis, mode_basis, name = "n_rows") |>
  arrange(block, river_label, year)

gaps <- if (length(.gaps)) bind_rows(.gaps) else tibble()

# Coverage against the CRC map (evanbooher/crc_mapping) so the status map and
# the output are driven by the same table rather than diverging. No crosswalk
# means no area map to build - an empty, correctly-shaped table rather than
# an error. [R2]
coverage <- if (is.null(crosswalk)) {
  tibble(catch_area_code = integer(), block = character(),
        river_label = character(), source_id = character(), tier = character())
} else {
  crosswalk |>
    separate_longer_delim(crc_areas, delim = "|") |>
    filter(crc_areas != "", !is.na(crc_areas)) |>
    transmute(catch_area_code = as.integer(crc_areas), block, river_label,
              source_id, tier) |>
    distinct()
}

# Filenames say "intermediate" so nobody picks the wrong file off disk and
# sends it out. Rename only when the gap register is empty enough to justify it.
write_csv(effort_by_mode_location,
          file.path(OUT_DIR, "pst_fw_trips_by_mode_location_INTERMEDIATE.csv"))
write_csv(effort_by_area,
          file.path(OUT_DIR, "pst_fw_trips_by_crc_area_INTERMEDIATE.csv"))
write_csv(effort_long, file.path(OUT_DIR, "pst_fw_effort_long.csv"))
write_csv(provenance,  file.path(OUT_DIR, "pst_fw_provenance_ledger.csv"))
write_csv(gaps,        file.path(OUT_DIR, "pst_fw_gap_register.csv"))
write_csv(coverage,    file.path(OUT_DIR, "pst_fw_crc_coverage.csv"))
if (!is.null(p2$ratios)) {
  write_csv(p2$ratios, file.path(OUT_DIR, "pst_fw_p2_block_ratios.csv"))
}
if (!is.null(crc_vs_creel)) {
  write_csv(crc_vs_creel, file.path(OUT_DIR, "pst_fw_crc_vs_creel_bias.csv"))
}

# ---- 7. Console summary -----------------------------------------------------

message("\n== PST FW effort assembly (INTERMEDIATE - not the deliverable) ==")
message(glue("rows: {nrow(effort_by_mode_location)} | river-years: ",
             "{n_distinct(effort_by_mode_location$river_label, effort_by_mode_location$year)}"))
print(effort_by_mode_location |> count(block, year, name = "rows") |>
        pivot_wider(names_from = year, values_from = rows, values_fill = 0))

message("\nangler trips by tier:")
print(effort_by_mode_location |> group_by(tier) |>
        summarise(angler_trips = sum(angler_trips), .groups = "drop"))

# pct_mode_unknown RISES when P2 is on. CRC has no bank/boat or guided field,
# so P2 rows are permanently unknown on both dimensions. That is the honest
# number, not a regression - the denominator grew, the resolvable share didn't.
message("\nunresolved mode share (P2 and district_creel can never be resolved):")
print(effort_by_mode_location |> group_by(block) |>
        summarise(angler_trips = sum(angler_trips),
                  pct_mode_unknown =
                    round(100 * sum(angler_trips[mode == "unknown"]) /
                            sum(angler_trips), 1),
                  pct_from_p2 =
                    round(100 * sum(angler_trips[grepl("P2", tier)]) /
                            sum(angler_trips), 1),
                  .groups = "drop"))

if (!is.null(p2$ratios) && nrow(p2$ratios) > 0) {
  message("\nDescriptive trips-per-salmon by block-year (creel trips / creel harvest):")
  print(p2$ratios |>
          select(block, year, trips_per_salmon, n_fisheries) |>
          mutate(trips_per_salmon = round(trips_per_salmon, 2)) |>
          pivot_wider(id_cols = block, names_from = year,
                      values_from = trips_per_salmon))
  message("  (Puget Sound odd years should sit well below even years - pink ",
          "salmon inflate catch. If they don't, check the pairing.)")
}

if (!is.null(p2x) && nrow(p2x$ratios) > 0) {
  message("\nP2 EXPANSION ratio by block (creel trips / CRC harvest - the one applied):")
  print(p2x$ratios |>
          filter(ratio_basis == "block_year", usable) |>
          select(block, year, ratio, n_donor_areas, ratio_creel_denom) |>
          mutate(across(c(ratio, ratio_creel_denom), \(x) round(x, 2))) |>
          arrange(block, year))
  message("  (ratio vs ratio_creel_denom is the CRC reporting bias made visible. ",
          "The CRC-denominated column is what P2 applies.)")
}

if (!is.null(p2x) && nrow(p2x$loo_summary) > 0) {
  message("\nP2 leave-one-out error by block (hold out each donor area, predict it):")
  print(p2x$loo_summary |>
          mutate(across(c(median_ape, p90_ape, bias_pct), \(x) round(x, 1))))
  message("  (median_ape is the number to quote when asked how much confidence ",
          "P2 deserves. bias_pct sign matters: systematic over/under.)")
}

# Fisheries that had real effort but produced no trips are the most important
# thing on this console - they are silent undercounts, not visible zeros.
dropped <- gaps |>
  filter(severity == "blocker", str_detect(detail, "expansion failed"))
if (nrow(dropped) > 0) {
  message(glue("\n!! {nrow(dropped)} fisheries DROPPED with effort present but ",
               "no trip expansion - block totals above are UNDERCOUNTS:"))
  walk(dropped$detail, \(d) message("   - ", str_extract(d, "^[^:]+")))
}

if (nrow(gaps)) {
  message(glue("\n{sum(gaps$severity == 'blocker')} blockers, ",
               "{sum(gaps$severity == 'gap')} gaps, ",
               "{sum(gaps$severity == 'defect')} defects ",
               "-> analysis/pst/outputs/pst_fw_gap_register.csv"))
}

if (!is.null(crc_vs_creel) && nrow(crc_vs_creel) > 0) {
  message("\nCRC vs creel harvest, same area-year (P1 only, bias check):")
  print(crc_vs_creel |>
          filter(comparable) |>
          summarise(
            n_area_years          = n(),
            median_crc_over_creel = round(median(crc_over_creel, na.rm = TRUE), 2),
            q25 = round(quantile(crc_over_creel, 0.25, na.rm = TRUE), 2),
            q75 = round(quantile(crc_over_creel, 0.75, na.rm = TRUE), 2)
          ))
  message("  (<1 = CRC reports less than design-based creel. Basis differs: ",
          "CRC license-year vs creel calendar.)")
}

message("\nThis output is an INTERMEDIATE. See the header comment for what ",
        "remains before anything is sent to Northern Economics.")

cli_ok <- tryCatch({
  cli::cli_alert_success(paste(
    "Assembly complete. Next: Rscript",
    "analysis/pst/03_analysis/pst_fw_build_jim_workbook.R"
  ))
  TRUE
}, error = function(e) FALSE)
if (!cli_ok) {
  message(paste(
    "Assembly complete. Next: Rscript",
    "analysis/pst/03_analysis/pst_fw_build_jim_workbook.R"
  ))
}
