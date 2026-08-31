# ==============================================================================
# pst_crc_harvest_projection.R
# Location: analysis/pst/03_analysis/pst_crc_harvest_projection.R
#
# Tier P3: project 2025 CRC harvest for CRC areas with no 2025 creel coverage
# and no real 2025 P2 donor, then expand it to angler trips through the SAME
# creel-trips/CRC-harvest ratio machinery pst_p2_block_ratio.R already uses
# for real CRC harvest. Two variants of the SAME functions run today, differing
# only in their CRC_PROJECTION_CONTROL (history_years / nepa_blocks /
# variant_label) and which blocks they target - see PART 1b below:
#
#   WACoast + the four Columbia blocks: 6-year TRAILING mean (2019-2024).
#   PugetSound: same-parity (odd-year) 5-year mean (2015-2023), added
#     2026-08-21 - see that section's own header for why PS needed a
#     different projection basis, not just a different control value.
#
# WHY THIS EXISTS
# CRC publishes through license year 2024 only - calendar 2025 has just
# Jan-Mar coverage (see pst_p2_block_ratio.R's own STANDING CAVEAT 2: "2025 IS
# NOT EXPANDABLE ... P2 will produce almost nothing for 2025"). For river-years
# with no P1 and no real P2 for 2025, there is currently no trip estimate at
# all. This module fills exactly that hole: PROJECT what 2025 CRC harvest
# would be, then run the projection through the same ratio P2 already
# validated, so a projected number is estimated the same way a real one
# would be, not by a second, different method.
#
# SCOPE - WHAT PART 1 (THE 6-YEAR TRAILING MEAN) DOES NOT COVER
# PugetSound is excluded from the trailing-mean variant, deliberately and
# permanently - its own even/odd Pink-salmon dominance would have a trailing
# mean average an odd year's inflated harvest together with even years' near-
# absence of it, understating any odd target year (2025 included). See PART
# 1b for what covers PugetSound instead. This module's PART 1 call only ever
# targets DELIVER_BLOCKS minus control$nepa_blocks (WACoast plus the four
# Columbia blocks - ColumbiaLower/Middle/Upper/Snake, split from a single
# "ColumbiaTrib" 2026-08-19, see pst_fw_angler_trips_assembly.R's
# DELIVER_BLOCKS comment) - see CRC_PROJECTION_CONTROL$nepa_blocks below. Any
# Puget Sound candidate area reaching PART 1's call is logged as excluded, not
# silently dropped, so the reason it's missing from THAT call's output is
# visible (it should show up in PART 1b's output instead).
#
# WHY THE RATIO NEVER COMES FROM control$target_year's OWN block_year DONORS
# apply_p2() borrows a block-year ratio from OTHER donor areas in the same
# block-year when one exists, falling back to block_pooled otherwise. Doing
# that for target_year itself is NOT safe: a target_year (2025) block-year
# donor pair would be built from PARTIAL Jan-Mar 2025 CRC harvest (the only
# 2025 CRC data that exists), which has a small denominator and biases the
# ratio HIGH - exactly pst_p2_block_ratio.R's own STANDING CAVEAT 1 (season
# alignment), reintroduced through the back door. Confirmed empirically, not
# just in theory: an early run of this module (under the single-block
# "ColumbiaTrib" scheme, since split) found that block's own 2025 block-year
# ratio at 750 and WACoast's at 82, both correctly rejected by
# validate_ratios()'s plausible-range guardrail (their 2022-2024 values run
# 1.3-3.0). Applying either to a full-year PROJECTED harvest would have
# compounded that distortion rather than being caught by it.
#
# The fix is NOT to fall back straight to block_pooled, though - a pooled
# ratio can fail validate_ratios()'s own CV guardrail ("donor ratios
# incoherent within block") for a target block even when a more recent
# single-year ratio is coherent, and some Columbia blocks have too few donor
# area-years for either grouping to ever pass alone (see build_p2_donors()'s
# docstring in pst_p2_block_ratio.R for the concrete donor-count picture).
# Instead, the ratio comes from the most recent COMPLETE history year's
# block-year ratio - max(control$history_years), i.e. 2024, which excludes
# target_year by construction and is real, season-aligned data - falling back
# to block_pooled only if that specific year's ratio is unusable for a given
# block.
#
# HARVEST-SCALE GUARDRAIL (added 2026-08-19)
# A ratio can be well-calibrated and still not be safe to apply this far
# outside the harvest scale it was calibrated on. Confirmed necessary, not
# theoretical: reclassifying CRC areas 537-549 out of ColumbiaMainstem the
# same day (they're in-scope Upper Columbia tributary-district territory, not
# the actual out-of-scope Buoy 10/LCR/Bonneville-McNary reaches) exposed area
# 545 (CRC harvest over 100,000 in 2024) getting expanded through a ratio
# calibrated on Yakima/McNary donors whose largest area-year is under 800 -
# producing a single area-year of "trips" larger than the rest of the
# pipeline combined. P2_CONTROL$max_target_harvest_multiple (pst_p2_block_
# ratio.R) now refuses to apply a ratio when the target's harvest exceeds that
# multiple of the largest donor area's harvest, logged as a distinct gap
# reason rather than silently producing an implausible number.
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
  nepa_blocks       = "PugetSound", # blocks this call must NEVER touch - see
                                     # PART 1b for what covers them instead
  area_allowlist    = character(0), # empty = no area-level restriction beyond
                                     # nepa_blocks/deliver_blocks
  variant_label     = "6-yr trailing mean"  # carried into every row's method
                                     # string (R5) so a reader can tell which
                                     # projection basis produced it without
                                     # cross-referencing this file
)

# --- Data-driven Puget Sound pink-river classification ------------------------
# Pink salmon return to Puget Sound almost exclusively in ODD years - that's
# the entire premise behind CRC_PROJECTION_CONTROL_PS's same-parity (odd-year)
# mean below. Applying it to EVERY PugetSound area regardless of species mix
# was confirmed wrong (2026-08-31, real production data): Baker (sockeye/
# kokanee), Minter Creek, and Big Quilcene (fall Chinook) are non-pink
# fisheries - restricting THEIR historical mean to just 5 odd years is not
# capturing any real seasonal pattern for them, it's averaging an arbitrary
# subset of their history, and for all three it happened to net out well
# below their recent actuals. The result: PugetSound's real, large pink-driven
# 2025 increase in the big pink rivers (Skagit/Puyallup-Carbon/Snohomish, all
# real P1 data, +43-142% over 2024) was almost entirely offset in the BLOCK
# total by these non-pink rivers' projections dropping for an unrelated
# reason, making the total look like the pink effect barely showed up at all
# when it's actually there in full in the rivers that carry it.
#
# Which areas are "pink-dominant" is decided from CRC's own harvest history,
# not asserted - Evan's call to keep this reproducible as new CRC years land
# rather than a hand-typed river list. min_total_harvest excludes streams
# whose 5-year odd-year total is too small for a species-share ratio to mean
# anything (e.g. a stream at 145 total fish over 5 years reading "100% pink"
# is noise, not a finding). min_pink_share = 0.30 is the point above which
# pink is clearly a major component of that area's harvest, not merely
# present alongside other species.
PINK_CLASSIFICATION_CONTROL <- list(
  odd_years         = c(2015L, 2017L, 2019L, 2021L, 2023L),
  min_total_harvest = 5000,
  min_pink_share    = 0.30
)

#' Which Puget Sound CRC stream_codes are pink-salmon-dominated, computed
#' fresh from CRC harvest history every run.
#'
#' @param crc_hist  full, unfiltered CRC harvest tidy CSV
#' @return character vector of stream_codes (as character) classified
#'   pink-dominant
classify_pink_dominant_areas <- function(crc_hist, control = PINK_CLASSIFICATION_CONTROL) {
  crc_hist |>
    filter(region == "Puget Sound", calendar_year %in% control$odd_years) |>
    mutate(stream_code = as.character(stream_code)) |>
    group_by(stream_code) |>
    summarise(
      total = sum(harvest_count, na.rm = TRUE),
      pink  = sum(harvest_count[species == "Pink"], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(pink_share = if_else(total > 0, pink / total, NA_real_)) |>
    filter(total >= control$min_total_harvest, !is.na(pink_share),
           pink_share >= control$min_pink_share) |>
    pull(stream_code)
}

# PART 1b: PugetSound's own variant - see that section below for why. Same
# functions, same P2_CONTROL guardrails, only history_years/nepa_blocks/
# variant_label differ. min_history_years stays at 3: with 5 candidate odd
# years instead of 6, that is still "needs data in a majority of the
# candidate years," the same floor logic as the trailing-mean variant, not a
# separately chosen number. area_allowlist is left empty here in the static
# definition - the assembly script fills it in at call time with
# classify_pink_dominant_areas(crc_hist)'s result, since crc_hist isn't
# available yet when this file is sourced.
CRC_PROJECTION_CONTROL_PS <- list(
  history_years     = c(2015L, 2017L, 2019L, 2021L, 2023L),  # matches NEPA's
                                     # own "odd 5 yr" window - see PART 2's
                                     # NEPA_COMPARISON_CONTROL$odd_years,
                                     # reused here as the actual projection
                                     # basis rather than only a diagnostic
  target_year       = 2025L,
  min_history_years = 3,
  nepa_blocks       = character(0), # this call targets PugetSound directly;
                                     # nothing to exclude from within it
  area_allowlist    = character(0), # filled in at call time - see above
  variant_label     = "same-parity (odd-year) 5-yr mean"
)

# PugetSound's NON-pink areas (Baker, Minter Creek, Big Quilcene, etc.) -
# added 2026-08-31 alongside the pink-area classification above. Same 6-yr
# trailing mean as the main (non-PugetSound) variant - there is no odd/even
# effect to correct for in a river that isn't pink-dominant, so it gets the
# same treatment WACoast/Columbia already get. deliver_blocks is passed as
# "PugetSound" alone at the call site (same as CRC_PROJECTION_CONTROL_PS), so
# nepa_blocks has nothing to exclude; area_allowlist is deliberately left
# empty (not the pink-set's complement) - the assembly script's merge already
# lets CRC_PROJECTION_CONTROL_PS's results win on any area it resolved (an
# anti_join, same pattern as the thin-history fallback below), so this call
# only ever ends up filling the areas the pink-scoped call didn't touch.
CRC_PROJECTION_CONTROL_PS_NONPINK <- list(
  history_years     = 2019L:2024L,
  target_year       = 2025L,
  min_history_years = 3,
  nepa_blocks       = character(0),
  area_allowlist    = character(0),
  variant_label     = "6-yr trailing mean (non-pink Puget Sound)"
)

# PART 1c: thin-history fallback - added 2026-08-31, Evan's call. Areas whose
# CRC area code doesn't reach back far enough for either primary variant's
# min_history_years=3 floor (confirmed real, not rare: 15 area-years across
# WACoast/ColumbiaLower/PugetSound as of this writing, including all three
# Nooksack sub-reach splits - 790/792/794 have literally ZERO rows before
# 2020, most likely because WDFW split a single combined "Nooksack River"
# CRC code into these reach-specific codes around then) get a straight mean
# of whatever real years exist, ANY parity, rather than being left with no
# 2025 estimate at all. Explicitly NOT a substitute for either primary
# variant - it only ever fires for an area BOTH primary passes already
# failed on min_history_years (see the assembly script's merge logic,
# which only keeps a fallback row where no primary row exists for that
# area-year) - and it is visibly weaker: min_history_years = 1 accepts even
# a single real year as its own "mean," which is why every fallback row's
# method string is flagged FALLBACK and its year count is always reported,
# so a reader can immediately tell a 1-year fallback from a proper 5-6 year
# mean rather than mistaking the two for equally reliable.
#
# history_years spans the tidy CRC extract's entire real range rather than a
# fixed recent window, by design - "whatever years exist" per Evan, not a
# wider-but-still-bounded recent window. nepa_blocks is empty (unlike the
# main variant) since this fallback must be eligible to run for PugetSound
# too - it is PugetSound's own same-parity variant that most of the areas
# needing this fallback failed.
CRC_PROJECTION_CONTROL_FALLBACK <- list(
  history_years     = 2010L:2024L,
  target_year       = 2025L,
  min_history_years = 1,
  nepa_blocks       = character(0),
  area_allowlist    = character(0),
  variant_label     = "FALLBACK: straight mean, any available year(s) (thin history)"
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
#' @param ratios         list(system_year, block_year, system_pooled,
#'                       block_pooled, columbia_pooled) from
#'                       estimate_block_ratios() on REAL donor pairs - not
#'                       recomputed here [R4]: a projected harvest gets
#'                       exactly the ratio a real one would.
#' @param xw_area        output of expand_crosswalk_areas()
#' @param area_system    catch_area_code -> CRC system, all areas not just donors
#' @param target_blocks  DELIVER_BLOCKS minus control$nepa_blocks
apply_crc_projection <- function(proj, effort_long, ratios, xw_area, area_system,
                                 target_blocks, control = CRC_PROJECTION_CONTROL) {

  already_covered <- effort_long |>
    filter(year == control$target_year, !is.na(catch_area_code)) |>
    distinct(catch_area_code) |>
    mutate(is_covered = TRUE)

  candidates <- proj$coverage |>
    left_join(xw_area, by = c("stream_code" = "catch_area_code")) |>
    rename(catch_area_code = stream_code) |>
    left_join(area_system, by = "catch_area_code") |>
    left_join(already_covered, by = "catch_area_code") |>
    filter(is.na(is_covered)) |>
    select(-is_covered)

  # CRC areas absent from the crosswalk are unassignable, not zero - includes
  # Snake River (R1) as of this writing, which has no crosswalk rows yet.
  unmapped <- candidates |> filter(is.na(block))
  targets  <- candidates |> filter(block %in% target_blocks)

  # Area-level restriction on top of the block-level one above - used to
  # split PugetSound between the pink-dominant same-parity variant and the
  # non-pink 6-yr-trailing variant (see CRC_PROJECTION_CONTROL_PS/_PS_
  # NONPINK above). Empty (the default for every other variant) means no
  # additional restriction beyond target_blocks.
  if (length(control$area_allowlist) > 0) {
    targets <- targets |> filter(catch_area_code %in% control$area_allowlist)
  }

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

  # Most recent COMPLETE history year's ratio first (real, season-aligned,
  # excludes target_year by construction since target_year is never in
  # history_years), finest tier (system_year) before coarser ones, pooled
  # tiers only as fallback if no single-year ratio is usable for a block.
  # See the header note for why this order, not block_pooled alone. Uses
  # fill_ratio_tier() (pst_p2_block_ratio.R) - the same cascade helper
  # apply_p2() uses, restricted here to most_recent_complete_year for the
  # two single-year tiers.
  most_recent_complete_year <- max(control$history_years)

  system_year_recent <- ratios$system_year |> filter(year == most_recent_complete_year)
  block_year_recent  <- ratios$block_year  |> filter(year == most_recent_complete_year)

  resolved <- targets |>
    mutate(ratio = NA_real_, ratio_basis = NA_character_,
           n_donor_areas = NA_integer_, donor_areas = NA_character_,
           donor_ratio_cv = NA_real_, max_donor_area_harvest = NA_real_,
           ever_out_of_scale = NA, last_rejected_max_donor = NA_real_)

  # Same harvest-scale guardrail apply_p2() uses (pst_p2_block_ratio.R) - a
  # ratio calibrated on small donor areas should not be extrapolated onto a
  # projected mean harvest many times larger than anything it was validated
  # against. P2_CONTROL is passed in unchanged (see run_crc_projection()),
  # not CRC_PROJECTION_CONTROL, so this reuses the same tunable.
  #
  # apply_out_of_scale_guard() (pst_p2_block_ratio.R) runs after EVERY tier,
  # not once at the end - see that function's header for why: a target
  # rejected on scale at a fine tier must fall through to try the next,
  # coarser tier rather than being locked out the moment the finest tier
  # that filled it turns out to be thinly calibrated. Same fix as apply_p2()
  # (2026-08-31), same real case (Samish/CRC 816) exposed it here too.
  resolved <- resolved |> fill_ratio_tier(system_year_recent, c("block", "system")) |>
    apply_out_of_scale_guard(P2_CONTROL, "mean_harvest")
  resolved <- resolved |> fill_ratio_tier(block_year_recent,  c("block")) |>
    apply_out_of_scale_guard(P2_CONTROL, "mean_harvest")
  resolved <- resolved |> fill_ratio_tier(ratios$system_pooled,   c("block", "system")) |>
    apply_out_of_scale_guard(P2_CONTROL, "mean_harvest")
  resolved <- resolved |> fill_ratio_tier(ratios$block_pooled,    c("block")) |>
    apply_out_of_scale_guard(P2_CONTROL, "mean_harvest")
  resolved <- resolved |> fill_ratio_tier(ratios$columbia_pooled, c("block")) |>
    apply_out_of_scale_guard(P2_CONTROL, "mean_harvest")

  resolved <- resolved |>
    mutate(
      out_of_scale           = coalesce(ever_out_of_scale, FALSE),
      max_donor_area_harvest = coalesce(max_donor_area_harvest, last_rejected_max_donor)
    )

  p3_trips <- resolved |>
    filter(!is.na(ratio)) |>
    mutate(
      angler_trips         = mean_harvest * ratio,
      total_salmon_harvest = mean_harvest,   # projected, not creel-estimated
      year                 = control$target_year,
      tier                 = "P3",
      source_id            = "crc_harvest_projection",
      method = glue(
        "P3 projected ({control$variant_label}): {length(control$history_years)}-yr ",
        "mean CRC harvest {round(mean_harvest)} ({n_years} of ",
        "{length(control$history_years)} years: {years_present}) x ",
        "{ratio_basis} ratio {round(ratio, 3)} ({n_donor_areas} donor area(s) ",
        "[{donor_areas}]) from {if_else(ratio_basis == 'block_year', ",
        "as.character(most_recent_complete_year), 'pooled 2022-2024')}."
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
    resolved |> filter(is.na(ratio), !out_of_scale) |> transmute(
      block, catch_area_code, mean_harvest,
      reason = glue("no usable ratio at any tier for {most_recent_complete_year} or ",
                    "pooled (system_year, block_year, system_pooled, block_pooled, ",
                    "columbia_pooled all failed guardrails - see pst_fw_p2_area_ratios.csv)")
    ),
    resolved |> filter(is.na(ratio), out_of_scale) |> transmute(
      block, catch_area_code, mean_harvest,
      reason = glue(
        "projected CRC harvest {round(mean_harvest)} exceeds ",
        "{P2_CONTROL$max_target_harvest_multiple}x the largest donor area's ",
        "harvest ({round(max_donor_area_harvest)}) - ratio not extrapolated ",
        "this far outside its calibration range"
      )
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

  # crc_hist carries region/system directly (parse_crc_freshwater_harvest.R's
  # own output columns) - same squish/CRLF cleanup build_block_ratios() does
  # in pst_fw_angler_trips_assembly.R, needed here too since this is a
  # separate read of the same raw file.
  area_system <- crc_hist |>
    mutate(system = stringr::str_squish(stringr::str_replace_all(system, "[\r\n]+", " "))) |>
    distinct(stream_code, system) |>
    transmute(catch_area_code = as.character(stream_code), system)

  applied <- apply_crc_projection(proj, effort_long, ratios, xw_area, area_system,
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


# ==============================================================================
# PART 2: NEPA vs. pure-CRC 5-year even/odd mean comparison (Puget Sound)
# ==============================================================================
# NOT part of the P3 projection above and does not feed the deliverable. This
# is a diagnostic: does reconstructing the Puget Sound freshwater effort
# projection's own governing statistic - a 5-year SAME-PARITY mean catch per
# stream (previous 5 even years, or previous 5 odd years, not a straight
# trailing mean) - from pure CRC data land close to what the existing NEPA-
# derived file already has, or does it diverge?
#
# BACKGROUND, so this isn't read as a P3-style projection with a different
# window: the Puget Sound freshwater effort projection is not itself part of
# this pipeline. It is a separate dataset WDFW produces in conjunction with
# NWIFC and hands to BIA, which uses it to satisfy NEPA (federal law)
# analysis requirements for the Puget Sound fishing package. "NEPA" here
# names the legal requirement the deliverable serves, not the entity that
# built it - WDFW/NWIFC did.
#
# WHY THIS COMPARISON MATTERS, CONCRETELY
# Cross-checking against the same NEPA/pure-CRC discrepancy already found and
# quantified per-stream-per-year earlier in this workstream: rivers with a
# WDFW design-based creel program (Cascade, Nisqually, Nooksack, Puyallup,
# Carbon, Skagit, Snohomish, Stillaguamish - per source_id = "creel_pe" in
# pst_river_block_crosswalk.csv) get creel-SUBSTITUTED harvest in the
# existing NEPA-derived file, not raw CRC card-count harvest. Streams with no
# creel program get raw CRC either way, so those should - and do - land
# close to the existing file. This section reconstructs the SAME even/odd
# 5-year mean statistic from pure CRC for every Puget Sound stream in the
# workbook and reports the two groups (creel-covered vs. not) separately, so
# the size of the substitution effect is visible as a number, not an
# impression from a handful of spot checks.
#
# WHAT THIS DOES NOT DECIDE
# Whether to keep taking the existing NEPA-derived file as-is, or switch to a
# pure-CRC reconstruction, is a judgment call about which basis produces more
# defensible/credible trip estimates for the rivers where the two disagree -
# not something to default silently in either direction here. This section's
# job is to make the size and shape of the disagreement measurable so that
# decision can be made with real numbers instead of a general sense that they
# probably differ.
#
# NEPA WORKBOOK MECHANICS (reverse-engineered and verified by hand against
# the file, not assumed): each "20XX FW Effort Projection" tab covers a pair
# of years - even year 20XX and the following odd year 20XX+1. Per stream, a
# "Historic Salmon Catch" block gives ALL-SPECIES-COMBINED catch by calendar
# year plus four precomputed columns: 10 yr, 5 yr (straight trailing means -
# NOT used here), even 5 yr, and odd 5 yr. even 5 yr is the mean of the 5
# most recent EVEN calendar years strictly before 20XX; odd 5 yr is the mean
# of the 5 most recent ODD calendar years strictly before 20XX (equivalently,
# before 20XX+1, since 20XX is even). Verified against "2026 FW Effort
# Projection" / Samish River by hand: even 5 yr = mean(2016,2018,2020,2022,
# 2024 catch) = 7878.8, matching the file's own value to the decimal; odd
# 5 yr = mean(2015,2017,2019,2021,2023 catch) = 8391.6, same match. These
# even 5 yr / odd 5 yr columns - not the plain 5 yr / 10 yr ones - are what
# fold into the file's final Even/Odd Effort output via the trips-per-salmon
# ratio (3.44 odd / 8.65 even) and a regulation-closure scaling factor; this
# section stops at the catch-mean layer since that is what "harvest" means
# in the ask this section answers, not the fully-scaled effort figure.

NEPA_COMPARISON_CONTROL <- list(
  nepa_workbook   = "NEPA PS_Recreational Effort Estimates 2025_2026_for 2026_2027 Projections_4_21_2026.xlsx",
  nepa_sheet      = "2026 FW Effort Projection",
  even_years      = c(2016L, 2018L, 2020L, 2022L, 2024L),  # matches nepa_sheet's own "even 5 yr" window - see mechanics note above
  odd_years       = c(2015L, 2017L, 2019L, 2021L, 2023L),  # matches nepa_sheet's own "odd 5 yr" window
  min_years       = 3,   # a mean from 1-2 years is that year's value wearing an average's name - same floor as CRC_PROJECTION_CONTROL$min_history_years
  flag_threshold  = 0.15 # |pct diff| beyond this is flagged in the printed summary; the CSV keeps every stream regardless
)


# --- 5. Read NEPA's own precomputed even/odd 5-year means ---------------------
#' Extract, per stream, the ALL-SPECIES TOTAL row's precomputed "even 5 yr"
#' and "odd 5 yr" catch columns from a "20XX FW Effort Projection" tab. Column
#' positions are read from the tab's own year-header row (row 3), not
#' hardcoded, so this survives the workbook adding or removing species/year
#' columns in a future edition.
#'
#' @param path   path to the NEPA workbook
#' @param sheet  the "20XX FW Effort Projection" tab name
#' @return tibble(stream, nepa_even5yr, nepa_odd5yr)
read_nepa_5yr_means <- function(path, sheet) {
  raw <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE))

  col1 <- as.character(unlist(raw[[1]]))
  col2 <- as.character(unlist(raw[[2]]))

  # Stream name only appears on the block's first row; forward-fill down
  # through its per-month rows to the TOTAL row, same fill-down pattern used
  # throughout parse_crc_freshwater_harvest.R for the same reason.
  stream_filled <- col1
  for (i in seq_along(stream_filled)) {
    if (is.na(stream_filled[i]) || stream_filled[i] == "") {
      stream_filled[i] <- if (i > 1) stream_filled[i - 1] else NA_character_
    }
  }

  total_rows <- which(col2 == "TOTAL")
  if (length(total_rows) == 0) {
    cli::cli_abort("No stream TOTAL rows found in {sheet} of {basename(path)}.")
  }

  year_header <- as.character(unlist(raw[3, ]))
  even5_col   <- which(year_header == "even 5 yr")
  odd5_col    <- which(year_header == "odd 5 yr")
  if (length(even5_col) == 0 || length(odd5_col) == 0) {
    cli::cli_abort(
      "Could not find 'even 5 yr' / 'odd 5 yr' columns in row 3 of {sheet} - \\
       the workbook's layout may have changed. Found headers: \\
       {paste(unique(na.omit(year_header)), collapse = ', ')}."
    )
  }
  # Two "5 yr"-family blocks exist in this tab (Historic Salmon Catch, then a
  # derived Historic Salmon Effort block downstream) - take the FIRST
  # occurrence of each, which is the catch block this section is about.
  even5_col <- even5_col[1]
  odd5_col  <- odd5_col[1]

  tibble::tibble(
    stream       = stream_filled[total_rows],
    nepa_even5yr = suppressWarnings(as.numeric(unlist(raw[total_rows, even5_col]))),
    nepa_odd5yr  = suppressWarnings(as.numeric(unlist(raw[total_rows, odd5_col])))
  )
}


#' Reduce a stream name to a bare-base comparison key: strip embedded
#' newlines, whitespace, a trailing period, and the common River/Creek/Lake
#' suffix (spelled out or abbreviated) from either side. NEPA's names are
#' abbreviated ("SAMISH R.", "GREEN-DUWAMISH") and ours are spelled out
#' ("Samish River") - a plain case/whitespace normalization alone leaves
#' every ordinary river name unmatched (confirmed: the first version of this
#' comparison without suffix-stripping matched only 1 of 54 NEPA streams).
#' Not exhaustive - a handful of disambiguated or compound names (e.g. "Purdy
#' Cr.- HC" for the Hood Canal Purdy Creek, distinct from the South Sound
#' one) won't reduce to a match either side and surface in the run's
#' "unmatched" note instead of being forced.
normalize_stream_key <- function(x) {
  x |>
    stringr::str_replace_all("[\r\n]+", " ") |>
    stringr::str_squish() |>
    toupper() |>
    stringr::str_remove("\\.$") |>
    stringr::str_replace_all("\\bRIVER\\b", "R") |>
    stringr::str_replace_all("\\bCREEK\\b", "CR") |>
    stringr::str_replace_all("\\bLAKE\\b", "LK") |>
    stringr::str_remove("\\s+(R|CR|LK)\\.?$") |>
    stringr::str_squish()
}


# --- 6. Reconstruct the same statistic from pure CRC ---------------------------
#' Same even/odd 5-year mean, computed from OUR pure-CRC tidy data instead of
#' NEPA's file. All-species-combined per stream per calendar year, to match
#' the NEPA tab's own grain (verified in the mechanics note above - its
#' "Historic Salmon Catch" block is not species-specific).
#'
#' Matches NEPA's stream-name rows on normalize_stream_key() rather than
#' stream_code/catch_area_code: the NEPA tab's rows ARE named streams, not
#' CRC area codes, and the two aren't always 1:1 (a named stream can span
#' multiple CRC areas). A name-based join is the same key NEPA itself
#' reports against.
#'
#' @param crc_hist  full, unfiltered CRC harvest tidy CSV
#' @return tibble(stream_norm, our_even5yr, n_even, our_odd5yr, n_odd)
reconstruct_pure_crc_5yr_means <- function(crc_hist, control = NEPA_COMPARISON_CONTROL) {
  crc_hist |>
    mutate(stream_norm = normalize_stream_key(stream)) |>
    group_by(stream_norm, calendar_year) |>
    summarise(annual_total = sum(harvest_count, na.rm = TRUE), .groups = "drop") |>
    group_by(stream_norm) |>
    summarise(
      our_even5yr = mean(annual_total[calendar_year %in% control$even_years], na.rm = TRUE),
      n_even      = sum(calendar_year %in% control$even_years & !is.na(annual_total)),
      our_odd5yr  = mean(annual_total[calendar_year %in% control$odd_years], na.rm = TRUE),
      n_odd       = sum(calendar_year %in% control$odd_years & !is.na(annual_total)),
      .groups = "drop"
    )
}


# --- 7. Compare, split by creel coverage ---------------------------------------
#' Join NEPA's precomputed means against the pure-CRC reconstruction, and tag
#' each stream as creel-covered or not using pst_river_block_crosswalk.csv's
#' own creel_pe river_label list - not a hardcoded guess-list. A stream is
#' "creel-covered" if any creel_pe river_label, reduced through the same
#' normalize_stream_key() used for the join, appears as a substring of the
#' stream's normalized NEPA name; river_label groupings are coarser than
#' individual stream names (e.g. "Puyallup Carbon" covers both "PUYALLUP R."
#' and "CARBON R."), so substring containment is the correct direction to
#' test, not exact equality.
#'
#' @param nepa_means  output of read_nepa_5yr_means()
#' @param our_means   output of reconstruct_pure_crc_5yr_means()
#' @param crosswalk   pst_river_block_crosswalk.csv, already loaded
#' @return tibble with one row per NEPA stream that also has pure-CRC data
compare_nepa_vs_pure_crc <- function(nepa_means, our_means, crosswalk,
                                     control = NEPA_COMPARISON_CONTROL) {

  creel_rivers <- crosswalk |>
    filter(source_id == "creel_pe") |>
    distinct(river_label) |>
    pull(river_label) |>
    normalize_stream_key()
  creel_rivers <- creel_rivers[nchar(creel_rivers) > 0]

  is_creel_covered <- function(stream_norm) {
    any(purrr::map_lgl(creel_rivers, ~ stringr::str_detect(stream_norm, stringr::fixed(.x))))
  }

  nepa_means |>
    mutate(stream_norm = normalize_stream_key(stream)) |>
    inner_join(our_means, by = "stream_norm") |>
    rowwise() |>
    mutate(creel_covered = is_creel_covered(stream_norm)) |>
    ungroup() |>
    mutate(
      even_usable   = n_even >= control$min_years,
      odd_usable    = n_odd  >= control$min_years,
      diff_even     = our_even5yr - nepa_even5yr,
      diff_odd      = our_odd5yr  - nepa_odd5yr,
      pct_diff_even = if_else(nepa_even5yr > 0, diff_even / nepa_even5yr, NA_real_),
      pct_diff_odd  = if_else(nepa_odd5yr  > 0, diff_odd  / nepa_odd5yr,  NA_real_),
      flag_even     = even_usable & !is.na(pct_diff_even) &
                       abs(pct_diff_even) > control$flag_threshold,
      flag_odd      = odd_usable & !is.na(pct_diff_odd) &
                       abs(pct_diff_odd) > control$flag_threshold
    ) |>
    select(stream, creel_covered,
           nepa_even5yr, our_even5yr, n_even, even_usable, diff_even, pct_diff_even, flag_even,
           nepa_odd5yr, our_odd5yr, n_odd, odd_usable, diff_odd, pct_diff_odd, flag_odd) |>
    arrange(desc(creel_covered), stream)
}


# --- 8. Driver -------------------------------------------------------------------
#' @param crc_hist   full, unfiltered CRC harvest tidy CSV (2010-2024)
#' @param crosswalk  pst_river_block_crosswalk.csv, already loaded
#' @param nepa_dir   directory holding the NEPA workbook (input_files/pst/external_data)
#' @return tibble (the comparison table) or NULL
run_nepa_pure_crc_comparison <- function(crc_hist, crosswalk, nepa_dir,
                                         control = NEPA_COMPARISON_CONTROL) {

  if (is.null(crc_hist)) {
    message("[gap] nepa_comparison: no CRC harvest history table; comparison skipped entirely.")
    return(NULL)
  }
  if (is.null(crosswalk)) {
    message("[gap] nepa_comparison: no crosswalk; cannot classify streams as creel-covered or not, comparison skipped.")
    return(NULL)
  }

  nepa_path <- file.path(nepa_dir, control$nepa_workbook)
  if (!file.exists(nepa_path)) {
    message(glue(
      "[gap] nepa_comparison: NEPA workbook not found at {nepa_path}; ",
      "comparison skipped."
    ))
    return(NULL)
  }

  nepa_means <- read_nepa_5yr_means(nepa_path, control$nepa_sheet)
  our_means  <- reconstruct_pure_crc_5yr_means(crc_hist, control)
  cmp        <- compare_nepa_vs_pure_crc(nepa_means, our_means, crosswalk, control)

  n_matched   <- nrow(cmp)
  n_unmatched <- nrow(nepa_means) - n_matched
  if (n_unmatched > 0) {
    unmatched_names <- setdiff(stringr::str_squish(nepa_means$stream),
                               stringr::str_squish(cmp$stream))
    message(glue(
      "[note] nepa_comparison: {n_unmatched} of {nrow(nepa_means)} NEPA stream(s) ",
      "had no matching stream name in the pure-CRC data - {paste(unmatched_names, collapse = ', ')}."
    ))
  }

  creel_grp    <- filter(cmp, creel_covered)
  noncreel_grp <- filter(cmp, !creel_covered)

  summarize_grp <- function(g, label) {
    both <- c(g$pct_diff_even[g$even_usable], g$pct_diff_odd[g$odd_usable])
    both <- both[!is.na(both)]
    if (length(both) == 0) {
      message(glue("[note] nepa_comparison: {label} - no usable comparisons."))
      return(invisible(NULL))
    }
    message(glue(
      "[note] nepa_comparison: {label} ({length(both)} even/odd comparisons across ",
      "{n_distinct(g$stream)} streams) - median |% diff| {round(median(abs(both)) * 100, 1)}%, ",
      "mean |% diff| {round(mean(abs(both)) * 100, 1)}%, ",
      "{sum(g$flag_even, na.rm = TRUE) + sum(g$flag_odd, na.rm = TRUE)} of {length(both)} ",
      "exceed the {control$flag_threshold * 100}% flag threshold."
    ))
  }

  message(glue(
    "[note] nepa_comparison: {n_matched} Puget Sound stream(s) compared ",
    "({nrow(creel_grp)} creel-covered, {nrow(noncreel_grp)} not) against ",
    "{control$nepa_sheet} in {basename(nepa_path)}."
  ))
  summarize_grp(creel_grp,    "creel-covered streams (creel-substituted in the NEPA file)")
  summarize_grp(noncreel_grp, "non-creel streams (raw CRC in the NEPA file either way)")

  cmp
}


# ==============================================================================
# PART 3: Season-status correction (WDFW regulatory verification)
#
# Wired into the assembly script's P3 step 2026-08-29, so every pipeline run
# applies it automatically. Was originally a separate, manually-run script
# (pst_crc_projection_season_check.R, archived the same day into
# analysis/archive/ once this superseded it) - promoted to a real pipeline
# step once Evan finished a full round of verification and asked for it to
# apply on every run rather than as a one-off manual pass.
#
# WHY THIS IS A HUMAN-VERIFIED LOOKUP TABLE, NOT A LIVE SCRAPE
# WDFW publishes no API for season open/closed status. The baseline pamphlet
# is PDF prose; mid-season changes come via a free-text emergency-rules feed.
# Auto-parsing either reliably enough to justify REMOVING dollars from an
# economic valuation was judged too failure-prone - a wrongly-parsed "open"
# would leave an already-wrong number looking checked, worse than an honest
# gap. input_files/pst/lookup_tables/pst_season_status_lookup.csv is the
# actual source of truth: one row per (CRC area, month of the target year),
# `status` = open/closed/restricted/UNVERIFIED, hand-maintained by Evan
# against the WDFW pamphlets. scaffold_season_status_lookup() below upserts
# new candidate rows as UNVERIFIED but never touches an existing row - a
# human's verification work always survives a re-run.
#
# WHAT THE CORRECTION DOES
# An area verified closed for every one of the target year's 12 months has
# its projected harvest/trips forced to zero. An area verified closed for
# SOME months gets a prorated cut: the fraction of its OWN historical annual
# harvest that typically falls in those specific closed months (from the
# full CRC tidy history, SEASONALITY_HISTORY_YEARS below) is subtracted from
# the projection - not a guess, the same per-area seasonality basis PART 1's
# sanity check already computes for Jan-Mar, generalized to any month. An
# area with ANY month still UNVERIFIED is left completely unchanged and
# logged as a gap - never assumed open [R2]. "restricted" (e.g. a bag-limit
# cut, not a closure) is treated as open for the arithmetic - there is no
# reliable way to convert a bag-limit change into a harvest fraction from
# this pipeline's own data.
# ==============================================================================

# Deliberately independent of either CRC_PROJECTION_CONTROL's own
# history_years: the two projection variants (6-yr trailing vs. PS same-
# parity) use different windows, and this check should rest on one stable,
# wide "typical seasonality" basis rather than silently inheriting whichever
# projection variant happened to produce a given area's row.
SEASONALITY_HISTORY_YEARS <- 2019L:2024L

VALID_SEASON_STATUSES <- c("UNVERIFIED", "open", "closed", "restricted")

#' Historical month-of-year harvest share, per CRC area (stream_code).
#' @return tibble(stream_code, calendar_month, month_share)
compute_monthly_harvest_share <- function(crc_hist, history_years = SEASONALITY_HISTORY_YEARS) {
  crc_hist |>
    filter(calendar_year %in% history_years) |>
    mutate(stream_code = as.character(stream_code)) |>
    group_by(stream_code, calendar_month) |>
    summarise(month_harvest = sum(harvest_count, na.rm = TRUE), .groups = "drop") |>
    group_by(stream_code) |>
    mutate(annual_harvest = sum(month_harvest, na.rm = TRUE),
           month_share    = if_else(annual_harvest > 0,
                                    month_harvest / annual_harvest, NA_real_)) |>
    ungroup() |>
    select(stream_code, calendar_month, month_share)
}

#' Upsert new (catch_area_code, month) candidates from this run's P3 output
#' into the season status lookup table as UNVERIFIED. Writes the file back
#' to lookup_path (an input_files/pst/lookup_tables/ path, not an output) -
#' this is the one place in the assembly script that writes back to an
#' input, because the lookup table IS the mechanism, same as
#' SEASON_TRUNCATED_MONTHS in pst_p2_block_ratio.R being empty-until-a-human-
#' fills-it-in.
#'
#' @return the full (existing + newly scaffolded) lookup table
scaffold_season_status_lookup <- function(p3_trips, lookup_path, target_year) {
  license_year_label <- function(calendar_month) {
    start_year <- if_else(calendar_month <= 3L, target_year - 1L, target_year)
    glue("{start_year}-{substr(start_year + 1L, 3, 4)}")
  }

  if (nrow(p3_trips) == 0) {
    if (file.exists(lookup_path)) {
      return(readr::read_csv(lookup_path, show_col_types = FALSE,
                             col_types = readr::cols(.default = "c")) |>
               mutate(month = as.integer(month)))
    }
    return(tibble())
  }

  scaffold <- p3_trips |>
    mutate(catch_area_code = as.character(catch_area_code)) |>
    distinct(catch_area_code, river_label) |>
    tidyr::crossing(month = 1:12) |>
    mutate(
      license_year    = license_year_label(month),
      status          = "UNVERIFIED",
      verified_source = NA_character_,
      verified_by     = NA_character_,
      verified_date   = NA_character_,
      notes           = NA_character_
    )

  if (file.exists(lookup_path)) {
    existing <- readr::read_csv(lookup_path, show_col_types = FALSE,
                                col_types = readr::cols(.default = "c")) |>
      mutate(month = as.integer(month))
    new_rows <- scaffold |> anti_join(existing, by = c("catch_area_code", "month"))
    lookup <- bind_rows(existing, new_rows) |> arrange(catch_area_code, month)
    if (nrow(new_rows) > 0) {
      message(glue(
        "[note] crc_projection_season: {nrow(new_rows)} new (area, month) row(s) ",
        "added to {basename(lookup_path)} as UNVERIFIED - {nrow(existing)} ",
        "existing row(s) (verified or not) left untouched."
      ))
    }
  } else {
    lookup <- scaffold
    message(glue(
      "[note] crc_projection_season: no {basename(lookup_path)} found - creating ",
      "with {nrow(lookup)} UNVERIFIED rows. Nothing corrected until a human fills ",
      "in `status`."
    ))
  }

  bad_status <- lookup |> filter(!status %in% VALID_SEASON_STATUSES)
  if (nrow(bad_status) > 0) {
    cli::cli_abort(paste(
      "{nrow(bad_status)} row(s) in {basename(lookup_path)} have a status not in",
      "{paste(VALID_SEASON_STATUSES, collapse = ', ')}. Fix before re-running."
    ))
  }

  readr::write_csv(lookup, lookup_path)
  lookup
}

#' Apply verified season status to P3's projected trips/harvest.
#'
#' @param p3_trips  this run's combined P3 output (both control variants),
#'                  before canon() - must still carry catch_area_code,
#'                  river_label, block, year, total_salmon_harvest,
#'                  angler_trips, method
#' @param crc_hist  full, unfiltered CRC harvest tidy CSV
#' @param lookup    output of scaffold_season_status_lookup()
#' @param target_year  calendar year being corrected (matches control$target_year)
#' @return list(trips = p3_trips with total_salmon_harvest/angler_trips
#'   corrected in place (original values kept alongside as
#'   total_salmon_harvest_uncorrected/angler_trips_uncorrected) and method
#'   annotated where changed, gaps = tibble of areas still needing verification)
apply_season_status_correction <- function(p3_trips, crc_hist, lookup, target_year,
                                           history_years = SEASONALITY_HISTORY_YEARS) {
  if (nrow(p3_trips) == 0 || nrow(lookup) == 0) {
    return(list(
      trips = p3_trips |> mutate(total_salmon_harvest_uncorrected = total_salmon_harvest,
                                 angler_trips_uncorrected         = angler_trips),
      gaps  = tibble()
    ))
  }

  monthly_share <- compute_monthly_harvest_share(crc_hist, history_years)

  area_status <- lookup |>
    mutate(catch_area_code = as.character(catch_area_code), month = as.integer(month)) |>
    group_by(catch_area_code) |>
    summarise(
      n_unverified  = sum(status == "UNVERIFIED"),
      n_closed      = sum(status == "closed"),
      n_restricted  = sum(status == "restricted"),
      closed_months = list(sort(month[status == "closed"])),
      .groups = "drop"
    )

  corrected <- p3_trips |>
    mutate(catch_area_code = as.character(catch_area_code)) |>
    left_join(area_status, by = "catch_area_code") |>
    rowwise() |>
    mutate(
      season_status = case_when(
        is.na(n_unverified) | n_unverified > 0 ~ "needs_verification",
        n_closed == 12                         ~ "fully_closed",
        n_closed > 0                            ~ "partially_closed",
        TRUE                                     ~ "open"
      ),
      closed_month_share = if (season_status == "partially_closed") {
        sub <- monthly_share |>
          filter(stream_code == catch_area_code,
                 calendar_month %in% unlist(closed_months))
        if (nrow(sub) == 0 || any(is.na(sub$month_share))) NA_real_ else sum(sub$month_share)
      } else {
        NA_real_
      },
      total_salmon_harvest_uncorrected = total_salmon_harvest,
      angler_trips_uncorrected         = angler_trips,
      total_salmon_harvest = case_when(
        season_status == "fully_closed" ~ 0,
        season_status == "partially_closed" & !is.na(closed_month_share) ~
          total_salmon_harvest * (1 - closed_month_share),
        TRUE ~ total_salmon_harvest
      ),
      angler_trips = if_else(
        total_salmon_harvest_uncorrected > 0,
        angler_trips * (total_salmon_harvest / total_salmon_harvest_uncorrected),
        angler_trips
      ),
      method = if (total_salmon_harvest != total_salmon_harvest_uncorrected) {
        glue(
          "{method} SEASON-CORRECTED ({season_status}, WDFW regulatory ",
          "verification): harvest {round(total_salmon_harvest_uncorrected)} -> ",
          "{round(total_salmon_harvest)} - see pst_season_status_lookup.csv."
        )
      } else {
        method
      }
    ) |>
    ungroup()

  gaps <- corrected |>
    filter(season_status == "needs_verification") |>
    transmute(
      block, catch_area_code, year,
      reason = "P3 projection not yet verified against WDFW season regulations - see pst_season_status_lookup.csv"
    )

  corrected <- corrected |>
    select(-n_unverified, -n_closed, -n_restricted, -closed_months,
           -closed_month_share, -season_status)

  list(trips = corrected, gaps = gaps)
}
