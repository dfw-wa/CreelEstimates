# ==============================================================================
# pst_crc_harvest_projection.R
# Location: analysis/pst/03_analysis/pst_crc_harvest_projection.R
#
# Tier P3: project 2025 CRC harvest from a 6-year mean (2019-2024) for CRC
# areas with no 2025 creel coverage and no real 2025 P2 donor, then expand it
# to angler trips through the SAME creel-trips/CRC-harvest ratio machinery
# pst_p2_block_ratio.R already uses for real CRC harvest.
#
# WHY THIS EXISTS
# CRC publishes through license year 2024 only - calendar 2025 has just
# Jan-Mar coverage (see pst_p2_block_ratio.R's own STANDING CAVEAT 2: "2025 IS
# NOT EXPANDABLE ... P2 will produce almost nothing for 2025"). For river-years
# with no P1 and no real P2 for 2025, there is currently no trip estimate at
# all. This module fills exactly that hole: PROJECT what 2025 CRC harvest
# would be (6-year mean per CRC area), then run the projection through the
# same ratio P2 already validated, so a projected number is estimated the
# same way a real one would be, not by a second, different method.
#
# SCOPE - WHAT THIS DOES NOT COVER
# Puget Sound 2025 is out of scope, deliberately and permanently. It already
# has its own projection derived from NEPA analysis, averaging by even/odd
# year back to 2014 - a longer, methodologically distinct series this 6-year
# CRC-only average is not a substitute for. This module only ever targets
# DELIVER_BLOCKS minus control$nepa_blocks (WACoast, ColumbiaTrib as of this
# writing) - see CRC_PROJECTION_CONTROL$nepa_blocks below. Any Puget Sound
# candidate area is logged as excluded, not silently dropped, so the reason
# it's missing from this module's output is visible.
#
# WHY THE RATIO NEVER COMES FROM control$target_year's OWN block_year DONORS
# apply_p2() borrows a block-year ratio from OTHER donor areas in the same
# block-year when one exists, falling back to block_pooled otherwise. Doing
# that for target_year itself is NOT safe: a target_year (2025) block-year
# donor pair would be built from PARTIAL Jan-Mar 2025 CRC harvest (the only
# 2025 CRC data that exists), which has a small denominator and biases the
# ratio HIGH - exactly pst_p2_block_ratio.R's own STANDING CAVEAT 1 (season
# alignment), reintroduced through the back door. Confirmed empirically, not
# just in theory: the first real run of this module found ColumbiaTrib's own
# 2025 block-year ratio at 750 and WACoast's at 82, both correctly rejected
# by validate_ratios()'s plausible-range guardrail (their 2022-2024 values
# run 1.3-3.0). Applying either to a full-year PROJECTED harvest would have
# compounded that distortion rather than being caught by it.
#
# The fix is NOT to fall back straight to block_pooled, though - block_pooled
# pools 2022-2024 donors together per block, and for ColumbiaTrib and WACoast
# that pooled ratio fails validate_ratios()'s own CV guardrail ("donor ratios
# incoherent within block"): PugetSound's pooled ratio is coherent (CV passes)
# but ColumbiaTrib's and WACoast's are not, and those two are this module's
# ONLY targets (PugetSound is NEPA's). Falling back to block_pooled here would
# have made the projection produce nothing for either target block. Instead,
# the ratio comes from the most recent COMPLETE history year's block-year
# ratio - max(control$history_years), i.e. 2024, which excludes target_year by
# construction and is a real, complete, already-validated season-aligned
# number (ColumbiaTrib 1.56, WACoast 1.44 in the first real run). block_pooled
# is kept only as the final fallback if even that specific year's ratio is
# unusable for a given block.
#
# Design rules: R1 tier+source_id on every row; R2 gaps logged not fatal;
# R3 mode/location "unknown" not zero; R4 no cross-block ratios; R5 basis
# labeled ("projected", never collapsed into an unlabeled total).
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(glue)


# --- Tunables -----------------------------------------------------------------
# Exposed rather than buried, same philosophy as P2_CONTROL in
# pst_p2_block_ratio.R: these are the judgment calls to move if Jim or Melissa
# want them moved.

CRC_PROJECTION_CONTROL <- list(
  history_years     = 2019L:2024L,  # 6 calendar years = 3 full even/odd Pink cycles
  target_year       = 2025L,
  min_history_years = 3,            # a mean from 1-2 years is that year's value
                                     # wearing an average's name
  nepa_blocks       = "PugetSound"  # blocks this module must NEVER touch -
                                     # owned by the separate NEPA 2014+
                                     # even/odd-year projection
)


# --- 1. Six-year mean CRC harvest per area -------------------------------------
#' Aggregate leaf-level harvest to annual totals per CRC area (stream_code),
#' then take the mean across control$history_years. Also computes each area's
#' historical Jan-Mar share of its annual harvest, and its target_year
#' Jan-Mar partial actual (already present in the tidy CSV, since license
#' year max(history_years) runs into target_year's first quarter) - both feed
#' the sanity check in step 2.
#'
#' @param crc_hist  the FULL crc_freshwater_harvest_*_tidy.csv, unfiltered by
#'                  YEARS_SCOPE. The assembly script's p2$crc IS filtered to
#'                  YEARS_SCOPE (2022-2025) - too late for a 6-year mean that
#'                  needs 2019-2021. This function reads the full history
#'                  window itself rather than reusing p2$crc.
#' @return list(coverage, jan_mar_share, partial_actual)
project_crc_harvest <- function(crc_hist, control = CRC_PROJECTION_CONTROL) {

  history <- crc_hist |>
    filter(calendar_year %in% control$history_years) |>
    mutate(stream_code = as.character(stream_code)) |>
    group_by(region, stream_code, calendar_year) |>
    summarise(annual_harvest = sum(harvest_count, na.rm = TRUE), .groups = "drop")

  coverage <- history |>
    group_by(region, stream_code) |>
    summarise(
      n_years       = n_distinct(calendar_year),
      years_present = paste(sort(unique(calendar_year)), collapse = "|"),
      mean_harvest  = mean(annual_harvest, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      usable = n_years >= control$min_history_years,
      fail_reason = if_else(
        usable, NA_character_,
        glue("only {n_years} of {length(control$history_years)} history ",
             "year(s) present ({years_present}); need {control$min_history_years}")
      )
    )

  # Historical Jan-Mar share per area. Salmon runs are NOT evenly distributed
  # across months, so comparing a partial Jan-Mar actual against 1/4 of the
  # projected annual total would be wrong for most streams - use each
  # stream's OWN historical seasonality instead of a uniform assumption.
  jan_mar_share <- crc_hist |>
    filter(calendar_year %in% control$history_years) |>
    mutate(stream_code = as.character(stream_code)) |>
    group_by(stream_code) |>
    summarise(
      jan_mar_hist = sum(harvest_count[calendar_month %in% 1:3], na.rm = TRUE),
      full_yr_hist = sum(harvest_count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(jan_mar_frac = if_else(full_yr_hist > 0,
                                  jan_mar_hist / full_yr_hist, NA_real_))

  partial_actual <- crc_hist |>
    filter(calendar_year == control$target_year) |>
    mutate(stream_code = as.character(stream_code)) |>
    group_by(stream_code) |>
    summarise(partial_actual_jan_mar = sum(harvest_count, na.rm = TRUE),
              .groups = "drop")

  list(coverage = coverage, jan_mar_share = jan_mar_share,
       partial_actual = partial_actual)
}


# --- 2. Sanity check: projection vs. partial actual ----------------------------
#' Expected Jan-Mar target_year total, from the projected mean x each area's
#' own historical seasonality, compared against what has actually landed so
#' far. A flag here is something to look at, not a blocking failure - the
#' partial actual is itself thin (one quarter, one season) and can
#' legitimately differ from a 6-year mean without either number being wrong.
check_projection_vs_partial_actual <- function(proj, control = CRC_PROJECTION_CONTROL) {
  proj$coverage |>
    filter(usable) |>
    left_join(proj$jan_mar_share, by = "stream_code") |>
    left_join(proj$partial_actual, by = "stream_code") |>
    mutate(
      expected_jan_mar        = mean_harvest * jan_mar_frac,
      partial_actual_jan_mar  = coalesce(partial_actual_jan_mar, 0),
      ratio = if_else(expected_jan_mar > 0,
                      partial_actual_jan_mar / expected_jan_mar, NA_real_),
      flag  = !is.na(ratio) & (ratio > 3 | ratio < 1 / 3)
    ) |>
    select(region, stream_code, mean_harvest, jan_mar_frac,
           expected_jan_mar, partial_actual_jan_mar, ratio, flag)
}


# --- 3. Expand projected harvest to trips, non-NEPA blocks only ---------------
#' Structurally mirrors pst_p2_block_ratio.R's apply_p2() on purpose - same
#' "already covered? skip" / "no ratio? gap, don't guess" shape - with two
#' differences:
#'   (a) "covered" means ANY tier already present in effort_long for
#'       target_year (real P1 creel OR real P2), not just P1 donor pairs,
#'       because this runs AFTER P2 has already been merged into effort_long.
#'   (b) only block_pooled ratios are used - see the header note on why.
#'
#' @param proj           output of project_crc_harvest()
#' @param effort_long    assembly's running table, AFTER P1 + real P2 merge
#' @param ratios         list(block_year, block_pooled) from
#'                       estimate_block_ratios() on REAL donor pairs - not
#'                       recomputed here [R4]: a projected harvest gets
#'                       exactly the ratio a real one would.
#' @param xw_area        output of expand_crosswalk_areas()
#' @param target_blocks  DELIVER_BLOCKS minus control$nepa_blocks
apply_crc_projection <- function(proj, effort_long, ratios, xw_area,
                                 target_blocks, control = CRC_PROJECTION_CONTROL) {

  already_covered <- effort_long |>
    filter(year == control$target_year, !is.na(catch_area_code)) |>
    distinct(catch_area_code) |>
    mutate(is_covered = TRUE)

  candidates <- proj$coverage |>
    left_join(xw_area, by = c("stream_code" = "catch_area_code")) |>
    rename(catch_area_code = stream_code) |>
    left_join(already_covered, by = "catch_area_code") |>
    filter(is.na(is_covered)) |>
    select(-is_covered)

  # CRC areas absent from the crosswalk are unassignable, not zero - includes
  # Snake River (R1) as of this writing, which has no crosswalk rows yet.
  unmapped <- candidates |> filter(is.na(block))
  targets  <- candidates |> filter(block %in% target_blocks)

  excluded_nepa <- candidates |>
    filter(!is.na(block), block %in% control$nepa_blocks)

  # Covered-but-unpartitioned areas (Hanford Reach 534|535|536): trips are
  # ALREADY in the deliverable via R3_external, but at fishery grain with
  # catch_area_code = NA, so the already_covered test above cannot see them.
  # Same double-count hazard apply_p2() guards against, same fix.
  unpartitioned <- xw_area |>
    filter(area_coverage == "covered_unpartitioned") |>
    distinct(catch_area_code)
  excluded_unpart <- targets |> semi_join(unpartitioned, by = "catch_area_code")
  targets <- targets |> anti_join(unpartitioned, by = "catch_area_code")

  no_history <- targets |> filter(!usable)
  targets    <- targets |> filter(usable)

  # Most recent COMPLETE history year's block-year ratio first (real,
  # season-aligned, excludes target_year by construction since target_year is
  # never in history_years), block_pooled only as fallback if that specific
  # year's ratio is unusable for a block. See the header note for why this
  # order, not block_pooled alone.
  most_recent_complete_year <- max(control$history_years)

  ry <- ratios$block_year |> filter(usable, year == most_recent_complete_year) |>
    select(block, ratio, ratio_basis, n_donor_areas, donor_areas, donor_ratio_cv)
  rp <- ratios$block_pooled |> filter(usable) |>
    select(block, ratio, ratio_basis, n_donor_areas, donor_areas, donor_ratio_cv)

  resolved <- targets |>
    left_join(ry, by = "block") |>
    left_join(rp, by = "block", suffix = c("", "_pooled")) |>
    mutate(
      ratio          = coalesce(ratio, ratio_pooled),
      ratio_basis    = coalesce(ratio_basis, ratio_basis_pooled),
      n_donor_areas  = coalesce(n_donor_areas, n_donor_areas_pooled),
      donor_areas    = coalesce(donor_areas, donor_areas_pooled),
      donor_ratio_cv = coalesce(donor_ratio_cv, donor_ratio_cv_pooled)
    ) |>
    select(-ends_with("_pooled"))

  p3_trips <- resolved |>
    filter(!is.na(ratio)) |>
    mutate(
      angler_trips         = mean_harvest * ratio,
      total_salmon_harvest = mean_harvest,   # projected, not creel-estimated
      year                 = control$target_year,
      tier                 = "P3",
      source_id            = "crc_harvest_projection",
      method = glue(
        "P3 projected: {length(control$history_years)}-yr mean CRC harvest ",
        "{round(mean_harvest)} ({n_years} of {length(control$history_years)} ",
        "years: {years_present}) x {ratio_basis} ratio {round(ratio, 3)} ",
        "({n_donor_areas} donor area(s) [{donor_areas}]) from ",
        "{if_else(ratio_basis == 'block_year', as.character(most_recent_complete_year), 'pooled 2022-2024')}. ",
        "Excludes {paste(control$nepa_blocks, collapse = ', ')}, which use a ",
        "separate NEPA-derived 2014+ even/odd-year projection."
      ),
      mode           = "unknown",
      location       = "unknown",
      location_basis = "crc_no_split",
      mode_basis     = "not_collected",
      fishery_name   = NA_character_,
      month          = NA_integer_
    )

  p3_gaps <- bind_rows(
    unmapped |> transmute(
      block = NA_character_, catch_area_code, mean_harvest,
      reason = "CRC stream_code not present in pst_river_block_crosswalk crc_areas"
    ),
    no_history |> transmute(
      block, catch_area_code, mean_harvest, reason = fail_reason
    ),
    resolved |> filter(is.na(ratio)) |> transmute(
      block, catch_area_code, mean_harvest,
      reason = glue("no usable ratio for {most_recent_complete_year} or pooled ",
                    "(guardrails failed - see pst_fw_p2_area_ratios.csv)")
    ),
    excluded_unpart |> transmute(
      block, catch_area_code, mean_harvest,
      reason = paste("area covered by an unpartitioned source (trips already in",
                     "deliverable, no per-area split) - excluded to avoid",
                     "double-counting")
    )
  ) |>
    mutate(tier_attempted = "P3")

  list(trips = p3_trips, gaps = p3_gaps, n_excluded_nepa = nrow(excluded_nepa))
}


# --- 4. Driver ------------------------------------------------------------------
#' @param crc_hist        full, unfiltered CRC harvest tidy CSV (2019-2024)
#' @param effort_long     assembly's running table, AFTER P1 + real P2 merge
#' @param donors          p2x$donors from run_p2_extrapolation() - real P1/CRC
#'                        donor pairs, used to derive the block-pooled ratio
#' @param crosswalk       pst_river_block_crosswalk.csv, already loaded
#' @param deliver_blocks  DELIVER_BLOCKS; control$nepa_blocks is subtracted
#'                        internally, so pass the full set
#' @return list(trips, gaps, sanity_check, coverage) or NULL
run_crc_projection <- function(crc_hist, effort_long, donors, crosswalk,
                               deliver_blocks, control = CRC_PROJECTION_CONTROL) {

  if (is.null(crc_hist)) {
    message("[gap] crc_projection: no CRC harvest history table; P3 projection skipped entirely.")
    return(NULL)
  }
  if (is.null(donors) || nrow(donors) == 0) {
    message(paste("[blocker] crc_projection: no P2 donor area-years available",
                  "to derive a ratio from; P3 projection skipped."))
    return(NULL)
  }

  target_blocks <- setdiff(deliver_blocks, control$nepa_blocks)

  proj    <- project_crc_harvest(crc_hist, control)
  sanity  <- check_projection_vs_partial_actual(proj, control)
  xw_area <- expand_crosswalk_areas(crosswalk)
  ratios  <- estimate_block_ratios(donors, P2_CONTROL)

  applied <- apply_crc_projection(proj, effort_long, ratios, xw_area,
                                  target_blocks, control)

  n_flagged <- sum(sanity$flag, na.rm = TRUE)
  if (n_flagged > 0) {
    message(glue(
      "[note] crc_projection: {n_flagged} area(s) where the Jan-Mar ",
      "{control$target_year} partial actual differs from the ",
      "seasonality-adjusted projection by >3x or <0.33x - see ",
      "pst_fw_crc_projection_sanity.csv."
    ))
  }

  blocks_with_rows <- sort(unique(applied$trips$block))
  message(glue(
    "[note] crc_projection: {nrow(applied$trips)} area-year(s) projected for ",
    "{control$target_year} - eligible blocks {paste(target_blocks, collapse = ', ')}, ",
    "rows landed in {if (length(blocks_with_rows) > 0) paste(blocks_with_rows, collapse = ', ') else 'none'} ",
    "({format(round(sum(applied$trips$angler_trips, na.rm = TRUE)), big.mark = ',')} trips); ",
    "{nrow(applied$gaps)} unresolved. {applied$n_excluded_nepa} area(s) excluded - ",
    "covered by NEPA's separate 2014+ projection instead."
  ))

  list(trips = applied$trips, gaps = applied$gaps, sanity_check = sanity,
       coverage = proj$coverage)
}
