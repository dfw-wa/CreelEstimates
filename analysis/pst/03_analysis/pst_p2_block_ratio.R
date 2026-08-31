# ==============================================================================
# pst_p2_block_ratio.R
# Location: analysis/pst/03_analysis/pst_p2_block_ratio.R
#
# Tier P2: expand CRC harvest to angler trips for CRC areas with NO creel
# coverage, using a within-block ratio estimated from the areas that have both.
#
# ------------------------------------------------------------------------------
# WHY THIS REPLACES THE EARLIER DRAFT
#
# The first version keyed on a `river` column that does not exist, and used a
# denominator inconsistent with the assembly script's build_block_ratios().
# Both were wrong, in different ways:
#
#   - Wrong key. The join key to CRC is catch_area_code, not river. Rivers span
#     several areas (Quillayute = 398|400|402|404|406), so a river-keyed ratio
#     cannot be applied to a CRC row at all. Everything below works at
#     catch_area_code and rolls up to river_label at the end.
#
#   - Basis mismatch. build_block_ratios() computes trips/salmon with CREEL
#     harvest underneath. Applying that ratio to CRC harvest assumes the two
#     harvest measures agree. The run's own bias check says they don't:
#     median CRC/creel = 0.77 over 62 comparable area-years, IQR 0.52-1.93.
#     A creel-denominated ratio on a CRC numerator understates uncovered areas
#     by roughly a quarter at the median, with an IQR wide enough that the
#     direction isn't stable per area.
#
#     Fix: denominate the DONOR ratio the way the TARGET is measured -- creel
#     trips over CRC harvest. The CRC reporting bias then sits inside the ratio,
#     where it cancels, instead of between the ratio and its application, where
#     it doesn't. Both ratios are computed and written side by side so the
#     difference stays visible rather than assumed away.
#
# ------------------------------------------------------------------------------
# THE CROSSWALK BLOCKER IS RESOLVED
#
# The gap register currently carries:
#   [blocker] p2_expansion: ... needs a validated CRC-stream-to-river crosswalk
#
# That crosswalk exists. crc_vs_creel already joins CRC stream_code to creel
# catch_area_code and returns 62 comparable area-years, so the two share a
# coding scheme. pst_river_block_crosswalk.csv's crc_areas column (pipe-
# delimited, already expanded in the `coverage` step) supplies
# area -> river_label -> block. This module consumes that expansion.
#
# What remains unresolved is narrower and is re-logged as such: CRC stream_codes
# present in the harvest file but in NO crosswalk row. Those are unassignable
# and are logged, not guessed.
#
# ------------------------------------------------------------------------------
# Design rules: R1 tier+source_id on every row; R2 gaps logged not fatal;
# R3 mode/location "unknown" not zero; R4 no cross-block ratios; R5 basis labeled.
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(glue)


# --- Tunables -----------------------------------------------------------------
# Exposed rather than buried: these are the judgment calls to move if Jim or
# Melissa want them moved.

P2_CONTROL <- list(
  min_donor_areas       = 1,     # a block-year ratio from one area is that
                                 # area's ratio wearing a block's name
  min_donor_harvest     = 20,   # CRC salmon; below this the denominator is noise
  ratio_plausible_range = c(0.3, 60),
  max_donor_cv          = 2.5,   # CV of per-area ratios within block-year.
                                 # Loosened from 1.0: the observed CRC/creel IQR
                                 # (0.52-1.93) shows real area-level scatter is
                                 # wide, and 1.0 rejected coherent blocks.
  allow_pooled_fallback = TRUE,
  max_target_harvest_multiple = 10   # a target area's CRC harvest can be at
                                 # most this many times the largest single
                                 # DONOR area's harvest before the ratio is
                                 # refused, not applied. ratio_plausible_range
                                 # only checks the ratio's own value, which
                                 # says nothing about whether extrapolating it
                                 # this far outside its calibration range is
                                 # sound. Confirmed necessary, not theoretical:
                                 # reclassifying areas 537-549 out of
                                 # ColumbiaMainstem (2026-08-19) exposed area
                                 # 545 (Columbia River above McNary, CRC
                                 # harvest 105,247 in 2024) getting expanded
                                 # through a ratio calibrated on Yakima/McNary
                                 # donors whose largest is 748 - a 141x
                                 # extrapolation producing 1.15M "trips" for
                                 # one area-year, 60% of the entire pipeline's
                                 # total. The ratio itself (10.9) passed
                                 # ratio_plausible_range fine; the harvest
                                 # scale it was applied to was never validated
                                 # at anything remotely like that size.
)


# --- Manually verified creel-season truncation exclusions ---------------------
#
# A month at the edge of a creel program's operating window can be
# "incomplete" for two entirely different reasons, and only one of them is
# visible in the data:
#
#   (a) Sampling gap within a fully-covered season. Some section x period x
#       day_type strata that month didn't get an interview-based estimate and
#       defaulted to zero (see effort_monthly's na.rm = TRUE convention in
#       multi_fishery_creel_summary.R). Visible as low prop_strata_estimated.
#       Still a real, if noisy, measurement of that whole month.
#
#   (b) The creel program itself stopped running before the legal harvest
#       period -- and therefore CRC's own reporting window -- closed that
#       year. This is invisible in prop_strata_estimated: a creel program
#       that operated flawlessly for the days it ran can show 100% stratum
#       coverage and still only be measuring PART of the month's true
#       activity, because harvest continued after the survey packed up.
#       This can only be resolved by checking the fishery's actual
#       regulations (season open/close dates) against the creel program's
#       own operating window -- not derivable from any column in this
#       pipeline. See the creel_season_edges diagnostic in
#       pst_fw_angler_trips_assembly.R, which flags EVERY fishery-year's
#       first/last surveyed month as a candidate needing this check, and
#       analysis/pst/03_analysis/_22_status_and_gaps.qmd for the open task.
#
# This table is the mechanism for (b) once a human has actually done that
# check: a row here means a specific fishery/year/month combination is
# CONFIRMED truncated relative to the legal season, so its creel trips/
# harvest are excluded -- not just flagged -- from every creel-trips/CRC-
# harvest ratio construction (both build_p2_donors() below and the
# crc_vs_creel bias comparison in the assembly script). Empty until that
# verification work is done; adding a row here is the entire mechanism, no
# other code change is needed to make an exclusion take effect.
SEASON_TRUNCATED_MONTHS <- tibble::tibble(
  fishery_name = character(),
  year         = integer(),
  month        = integer(),
  reason       = character()
)

#' Drop any (fishery_name, year, month) row confirmed in
#' SEASON_TRUNCATED_MONTHS. A no-op (returns df unchanged) while that table
#' is empty, so this is safe to call unconditionally everywhere a
#' creel-trips/CRC-harvest ratio gets built from monthly creel rows.
exclude_truncated_months <- function(df, control = SEASON_TRUNCATED_MONTHS) {
  if (nrow(control) == 0) return(df)
  df |> dplyr::anti_join(control, by = c("fishery_name", "year", "month"))
}


# --- 1. Area-level crosswalk --------------------------------------------------
#' Expand the pipe-delimited crc_areas column into one row per catch_area_code.
#' Same expansion the assembly script's `coverage` step performs -- kept as a
#' function so both use identical logic instead of drifting.
#'
#' GUARD: a catch_area_code must map to exactly one river_label. If the source
#' crosswalk gives it two (e.g. a CRC area shared between "Chehalis" and
#' "Upper Chehalis" reporting boundaries), a plain left_join in apply_p2()
#' fans out -- the same CRC harvest figure gets attached to both rivers and
#' expanded twice, full value each time, with no split. That is silent
#' double-counting, not an edge case to shrug at. Ambiguous areas are
#' collapsed to a single combined-name row instead, so the harvest is
#' expanded once under a name that says it's shared, and the ambiguity is
#' printed so it can be resolved in the crosswalk rather than papered over
#' here every run.
expand_crosswalk_areas <- function(crosswalk) {

  raw <- crosswalk |>
    separate_longer_delim(crc_areas, delim = "|") |>
    filter(!is.na(crc_areas), crc_areas != "") |>
    transmute(
      catch_area_code = as.character(crc_areas),
      river_label,
      block,
      area_coverage = if ("area_coverage" %in% names(crosswalk)) {
        coalesce(area_coverage, "standard")
      } else "standard"
    ) |>
    distinct()

  dupe_areas <- raw |>
    group_by(catch_area_code) |>
    filter(n_distinct(river_label) > 1) |>
    ungroup()

  if (nrow(dupe_areas) > 0) {
    affected <- dupe_areas |>
      group_by(catch_area_code) |>
      summarise(rivers = paste(sort(unique(river_label)), collapse = " + "),
                blocks = paste(sort(unique(block)), collapse = "|"),
                .groups = "drop")

    message(glue(
      "[defect] p2_crosswalk: {nrow(affected)} CRC area(s) map to more than ",
      "one river in the crosswalk -- collapsed to a combined row so CRC ",
      "harvest is expanded ONCE, not duplicated per river. Affected: ",
      "{paste(glue('{affected$catch_area_code} [{affected$rivers}]'), collapse = '; ')}. ",
      "Resolve in pst_river_block_crosswalk.csv if these areas should be ",
      "split rather than combined."
    ))

    if (any(affected$blocks |> str_detect("\\|"))) {
      cross_block <- affected |> filter(str_detect(blocks, "\\|"))
      message(glue(
        "[blocker] p2_crosswalk: area(s) {paste(cross_block$catch_area_code, collapse = ', ')} ",
        "map to river_labels in DIFFERENT blocks ({cross_block$blocks}). A ",
        "combined row cannot pick a single block; these area(s) are dropped ",
        "from P2 entirely rather than guessed. Resolve manually."
      ))
    }
  }

  raw |>
    group_by(catch_area_code) |>
    summarise(
      river_label   = paste(sort(unique(river_label)), collapse = " + "),
      block         = if (n_distinct(block) == 1) block[1] else NA_character_,
      # "standard" wins ties only when every colliding row agrees.
      # "covered_unpartitioned" wins ANY collision, unanimous or not: it
      # means some P1 source already reports this area's trips in aggregate
      # (e.g. a district-wide total colliding with the area's own no-creel
      # CRC_only row - exactly what happens when a district-supplied source
      # like R2_external/R3_external/R1_external is added for an area that
      # already had a standalone CRC_only fallback row). Defaulting a
      # disagreement to "standard" re-enables P2 to independently expand an
      # area that is already covered - a real double-count, not a
      # hypothetical one (found 2026-08-28 rolling out R2_external: Entiat,
      # Okanogan, Similkameen, Wenatchee River, and Icicle Creek all kept
      # getting P2-expanded on top of R2's combined Upper Columbia total
      # because their standalone CRC_only rows disagreed with the new
      # covered_unpartitioned row for the same catch_area_code). Collapsing
      # a real split disagreement to "standard" would at worst suppress a
      # legitimate expansion that stale bookkeeping should have removed
      # anyway - the safe direction to default toward.
      area_coverage = if ("covered_unpartitioned" %in% area_coverage) {
        "covered_unpartitioned"
      } else if (n_distinct(area_coverage) == 1) {
        area_coverage[1]
      } else {
        "standard"
      },
      .groups = "drop"
    ) |>
    filter(!is.na(block))
}


# --- 2. Donor set -------------------------------------------------------------
#' Area-years with BOTH design-based creel trips and a CRC harvest figure.
#'
#' @param effort_long  assembly output: block, river_label, catch_area_code,
#'                     year, month, angler_trips, total_salmon_harvest, tier
#' @param crc_month    p2$crc_month: stream_code (chr), calendar_year,
#'                     calendar_month, region, system, harvest
#' @param xw_area      output of expand_crosswalk_areas()
#'
#' Two ratios are carried. ratio_crc_denom is the estimator; ratio_creel_denom
#' exists only so the gap between the two is inspectable per area-year.
#'
#' CRC harvest is restricted to the SAME months effort_long has creel trips
#' in, per area-year, before it becomes the denominator. Confirmed empirically
#' (2024 PugetSound donors): every donor area has creel coverage narrower than
#' its CRC-reported months - e.g. area 830 has creel trips in 3 months
#' (Sep-Nov) but CRC harvest reported across 9 (Apr-Dec). Dividing trips by the
#' full-year CRC total would let months with zero survey presence dilute the
#' denominator, biasing the ratio low. The restriction is driven off
#' effort_long's own month column, not a hardcoded season, so it widens
#' automatically as more creel months are ingested. apply_p2() deliberately
#' does NOT use this restriction - a target area with no survey at all still
#' gets its full-year CRC harvest expanded; only the ratio's construction
#' needs the matched months.
#'
#' ANNUAL-ONLY DONORS (added 2026-08-31): a P1 source whose own trip total has
#' no month breakdown at all (month is NA in effort_long) can't be restricted
#' to matched months the way the above paragraph describes - there is no
#' partial-season creel presence to match against, because the trip total
#' already represents the WHOLE year, not a subset of it. Green-Duwamish
#' (R4_external, area 746) is the case in point: ingest_green_duwamish()
#' pools trip length across the full year, not by month (see district_creel_
#' ingestion.R), so month is NA even though CRC independently reports real,
#' non-zero monthly harvest for area 746 going back to 2010 - CRC's own
#' harvest doesn't depend on R4's own (absent) harvest column at all. For
#' these rows the denominator is CRC's own FULL-YEAR harvest for that
#' area-year rather than a month-restricted subset - not a looser standard,
#' the correct match for a trip total that already covers the full year.
#' Composite-area sources with the same NA month (R1_external/Snake River,
#' R2_external/Upper Columbia, R3_external/Hanford Reach) are NOT swept up by
#' this path: none of them carry a real single catch_area_code in
#' effort_long (see p1_rows's `!is.na(catch_area_code)` filter below) - their
#' problem is an unsplit AREA total, not an unsplit MONTH total, and remains
#' the separate, not-yet-built gap documented in estimate_block_ratios()'s
#' ColumbiaSnake note.
#'
#' Two distinct data-quality filters apply before p1_rows becomes anything:
#' exclude_truncated_months() removes fishery/year/months a human has
#' CONFIRMED the creel program stopped before the legal season did (see that
#' function's header) -- a no-op today since that table starts empty.
#' min_prop_strata_estimated below is the OTHER, automatically-detectable
#' issue: a month that IS within the survey's real operating window but had
#' some strata default to zero effort. That one is surfaced, not excluded --
#' it's still a real (if noisy) measurement of the month it covers, unlike a
#' confirmed truncation.
build_p2_donors <- function(effort_long, crc_month, xw_area,
                            deliver_blocks, control = P2_CONTROL) {

  p1_rows <- effort_long |>
    filter(block %in% deliver_blocks, tier == "P1", !is.na(catch_area_code)) |>
    mutate(catch_area_code = as.character(catch_area_code)) |>
    exclude_truncated_months()

  creel_area <- p1_rows |>
    group_by(block, catch_area_code, year) |>
    summarise(
      creel_trips   = sum(angler_trips,         na.rm = TRUE),
      creel_harvest = sum(total_salmon_harvest, na.rm = TRUE),
      fisheries     = paste(sort(unique(fishery_name)), collapse = "|"),
      # Weakest single month/angler-type cell behind this area-year's totals
      # (NA for non-creel_pe sources, e.g. district_creel -- correctly "not
      # applicable", not a zero). Low values mean some of what's summed above
      # defaulted to zero effort rather than a real interview-based estimate
      # (effort_monthly's na.rm = TRUE convention in
      # multi_fishery_creel_summary.R), understating creel_trips/creel_harvest
      # in the same direction the ratio would otherwise be biased.
      min_prop_strata_estimated = suppressWarnings(
        min(prop_strata_estimated, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    mutate(min_prop_strata_estimated = if_else(
      is.finite(min_prop_strata_estimated), min_prop_strata_estimated, NA_real_
    ))

  crc_month_key <- crc_month |>
    transmute(catch_area_code = as.character(stream_code),
             year            = calendar_year,
             month           = calendar_month,
             system,
             harvest)

  # Partition by (catch_area_code, year) GROUP, not by individual row: an
  # area-year with real per-month rows can still carry one stray NA-month
  # row (confirmed real case - McNary Reservoir 533/2024 has a 229.66-trip,
  # 0-harvest row with month = NA alongside its real months 9/10/11, likely
  # an unassigned prorated residual). Splitting on row-level month would let
  # that one stray row ALSO match via the annual path below, fanning the
  # join out to two crc_harvest rows for the same area-year and silently
  # double-counting the donor. Routing every row of an area-year through
  # whichever path the GROUP qualifies for keeps the two paths mutually
  # exclusive - McNary 2024 stays entirely month-matched, its stray NA row
  # contributes to creel_area's trip total same as before but is invisible
  # to crc_matched exactly as it was before this annual path existed.
  has_real_month <- p1_rows |>
    filter(!is.na(month)) |>
    distinct(catch_area_code, year)

  # Month-matched path: real per-month P1 rows, restricted to the months the
  # creel actually covers (see this function's header for why).
  month_matched <- p1_rows |>
    semi_join(has_real_month, by = c("catch_area_code", "year")) |>
    filter(!is.na(month)) |>
    distinct(catch_area_code, year, month) |>
    inner_join(crc_month_key, by = c("catch_area_code", "year", "month")) |>
    group_by(catch_area_code, year) |>
    # system is 1:1 with catch_area_code (same CRC "system" field regardless
    # of month), so first() is a constant, not an arbitrary pick.
    summarise(crc_harvest = sum(harvest, na.rm = TRUE),
              system      = first(system), .groups = "drop")

  # Annual-only path: area-years with NO real month anywhere (see the
  # ANNUAL-ONLY DONORS note above) - matched against CRC's full-year harvest
  # for that area-year instead of a month-restricted subset.
  annual_matched <- p1_rows |>
    anti_join(has_real_month, by = c("catch_area_code", "year")) |>
    distinct(catch_area_code, year) |>
    inner_join(crc_month_key, by = c("catch_area_code", "year")) |>
    group_by(catch_area_code, year) |>
    summarise(crc_harvest = sum(harvest, na.rm = TRUE),
              system      = first(system), .groups = "drop")

  crc_matched <- bind_rows(month_matched, annual_matched)

  donors <- creel_area |>
    inner_join(crc_matched, by = c("catch_area_code", "year")) |>
    filter(crc_harvest > 0, creel_trips > 0) |>
    mutate(
      ratio_crc_denom   = creel_trips / crc_harvest,
      ratio_creel_denom = if_else(creel_harvest > 0,
                                  creel_trips / creel_harvest, NA_real_),
      crc_over_creel    = if_else(creel_harvest > 0,
                                  crc_harvest / creel_harvest, NA_real_)
    )

  attr(donors, "n_creel_areas") <- nrow(creel_area)
  donors
}


# --- 2b. Composite-area donors -------------------------------------------------
#' P1 sources whose own trip total already spans MULTIPLE CRC areas at once
#' (composite crc_areas, area_coverage = "covered_unpartitioned" in the
#' crosswalk) can still donate a real ratio if CRC harvest is summed across
#' that SAME combined footprint rather than restricted to one area -
#' build_p2_donors() above can't do this: it joins on a single
#' catch_area_code, and these rows carry catch_area_code = NA in effort_long
#' (see ingest_hanford_boat()/ingest_r2_upper_columbia()'s "composite-area
#' problem" comments in district_creel_ingestion.R).
#'
#' NOT built for every covered_unpartitioned composite source - only an
#' explicit allowlist (COMPOSITE_DONOR_RIVERS below, EMPTY by default - see
#' "WHY THIS IS EMPTY" below), because building this correctly requires the
#' combined footprint to not dilute the ratio with an excluded area's real,
#' non-zero harvest. Snake River (R1_external) is a case this would exclude
#' even if the list were populated: Jeremy Trump's total only covers a
#' season-specific SUBSET of the six Snake CRC areas (640+644 spring,
#' 644+648 fall - see pst_river_block_crosswalk.csv's Snake River notes),
#' but 642/646/650 are NOT harvest-free the way that note's "closed portions
#' produce no harvest to miss" framing would suggest - confirmed empirically
#' against crc_freshwater_harvest_2010_2024_tidy.csv: 642/646/650 carry
#' real, non-zero all-time harvest (268/991/2051 respectively). Summing CRC
#' harvest across all six areas would fold that harvest - from water
#' Jeremy's creel never measured - into the denominator, biasing the ratio
#' down exactly the way this file's month-restriction rule exists to
#' prevent for the per-area path. Doing Snake correctly needs season-
#' specific area AND month attribution (spring vs. fall Chinook run timing)
#' that isn't sourced anywhere in this repo; logged as an open gap rather
#' than guessed at here.
#'
#' WHY THIS IS EMPTY (2026-08-31): Hanford Reach and Upper Columbia
#' (R2_external) were both implemented and tested here - mechanically
#' correct, no double-counting, verified with a full pipeline re-run - but
#' the RESULT is worse, not better. Hanford's computed ratio is ~2.0-2.3
#' trips/CRC-salmon; Upper Columbia's is ~0.36-0.64. Both are far below
#' McNary/Yakima's ~9-10, and it isn't noise: Hanford Reach and R2's
#' mainstem Priest Rapids-Chief Joseph reach (537|539|541|543|545, which
#' account for 72% of R2's combined 2022 CRC harvest per Evan's check) are
#' both large, high-catch-rate mainstem fall Chinook fisheries - physically
#' a different kind of fishery from McNary Reservoir/Yakima's smaller
#' tributary programs, not a data error. Pooling them into ColumbiaUpper's
#' SAME block_year/block_pooled ratio - the ratio applied to every other
#' uncovered ColumbiaUpper area, e.g. Methow River - measurably wrecks the
#' block's own leave-one-out fit: median_ape 56.6% -> 92.1%, bias_pct
#' +7.1% -> -82.8% with both added. `system` is set to a synthetic
#' "combined:<areas>" label specifically so a composite donor can't
#' accidentally collide into a real single-area donor's system_year/
#' system_pooled tier - but that same isolation means it never gets a
#' chance to form ITS OWN comparable-fishery-type tier either; it falls
#' straight through to block_year/block_pooled, which don't discriminate by
#' fishery type at all. Needed before COMPOSITE_DONOR_RIVERS is populated:
#' either a real "mainstem vs. tributary" system distinction feeding
#' system_year/system_pooled properly, or restricting a composite donor to
#' inform ONLY a target area confirmed comparable to it - not a blanket
#' block-wide pool. Left in place as tested, working machinery for that
#' follow-up rather than deleted.
#'
#' `system` is set to a synthetic "combined:<areas>" label rather than any
#' real CRC system name, since Upper Columbia's 11 areas span several real
#' systems and picking one with first() would be arbitrary and could
#' silently pool this composite donor into an unrelated single-area donor's
#' system_year/system_pooled tier. The synthetic label can't collide with a
#' real system, so estimate_block_ratios()'s system tiers simply treat each
#' composite donor as its own singleton system (a no-op) while the
#' block_year/block_pooled/columbia_pooled tiers - which don't key on system
#' at all - pick it up normally.
COMPOSITE_DONOR_RIVERS <- character(0)  # empty - see "WHY THIS IS EMPTY" above

build_composite_donors <- function(effort_long, crc_month, crosswalk,
                                   deliver_blocks) {

  eligible <- crosswalk |>
    filter(river_label %in% COMPOSITE_DONOR_RIVERS,
           area_coverage == "covered_unpartitioned") |>
    distinct(river_label, crc_areas) |>
    filter(str_detect(crc_areas, "\\|"))  # truly composite, >1 area

  if (nrow(eligible) == 0) return(tibble())

  p1_rows <- effort_long |>
    filter(block %in% deliver_blocks, tier == "P1",
           is.na(catch_area_code), river_label %in% eligible$river_label) |>
    left_join(eligible, by = "river_label")

  if (nrow(p1_rows) == 0) return(tibble())

  creel_area <- p1_rows |>
    group_by(block, crc_areas, year) |>
    summarise(
      creel_trips   = sum(angler_trips,         na.rm = TRUE),
      creel_harvest = sum(total_salmon_harvest, na.rm = TRUE),
      fisheries     = paste(sort(unique(fishery_name)), collapse = "|"),
      min_prop_strata_estimated = suppressWarnings(
        min(prop_strata_estimated, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    mutate(min_prop_strata_estimated = if_else(
      is.finite(min_prop_strata_estimated), min_prop_strata_estimated, NA_real_
    ))

  # One row per (combined key, component area code) - the join table that
  # lets a combined trip total be matched to the SUM of CRC harvest across
  # its listed component areas.
  component_areas <- eligible |>
    distinct(crc_areas) |>
    mutate(catch_area_code = crc_areas) |>
    separate_longer_delim(catch_area_code, delim = "|")

  crc_month_key <- crc_month |>
    transmute(catch_area_code = as.character(stream_code),
             year            = calendar_year,
             month           = calendar_month,
             harvest)

  has_real_month <- p1_rows |> filter(!is.na(month)) |> distinct(crc_areas, year)

  month_matched <- p1_rows |>
    semi_join(has_real_month, by = c("crc_areas", "year")) |>
    filter(!is.na(month)) |>
    distinct(crc_areas, year, month) |>
    left_join(component_areas, by = "crc_areas", relationship = "many-to-many") |>
    inner_join(crc_month_key, by = c("catch_area_code", "year", "month")) |>
    group_by(crc_areas, year) |>
    summarise(crc_harvest = sum(harvest, na.rm = TRUE), .groups = "drop")

  annual_matched <- p1_rows |>
    anti_join(has_real_month, by = c("crc_areas", "year")) |>
    distinct(crc_areas, year) |>
    left_join(component_areas, by = "crc_areas", relationship = "many-to-many") |>
    inner_join(crc_month_key, by = c("catch_area_code", "year")) |>
    group_by(crc_areas, year) |>
    summarise(crc_harvest = sum(harvest, na.rm = TRUE), .groups = "drop")

  crc_matched <- bind_rows(month_matched, annual_matched)

  creel_area |>
    inner_join(crc_matched, by = c("crc_areas", "year")) |>
    filter(crc_harvest > 0, creel_trips > 0) |>
    rename(catch_area_code = crc_areas) |>
    mutate(
      system            = paste0("combined:", catch_area_code),
      ratio_crc_denom   = creel_trips / crc_harvest,
      ratio_creel_denom = if_else(creel_harvest > 0,
                                  creel_trips / creel_harvest, NA_real_),
      crc_over_creel    = if_else(creel_harvest > 0,
                                  crc_harvest / creel_harvest, NA_real_)
    )
}


# --- 3. Block ratios ----------------------------------------------------------
#' Ratio of sums within group, not mean of area ratios: the sum form weights
#' donors by harvest so a small area with a thin denominator can't swing the
#' group. Per-area ratios survive only as a coherence diagnostic.
#'
#' Five tiers, tried in priority order by apply_p2()/apply_crc_projection()
#' (finest first, each falling back to the next if it fails validate_ratios()):
#'   1. system_year    - (block, system, year): CRC's own "system" field
#'                       (e.g. "Cowlitz R. System"), the finest grouping with
#'                       any real data behind it.
#'   2. block_year      - (block, year): today's original grouping, now block
#'                       = a CRC-region-derived block (see DELIVER_BLOCKS).
#'   3. system_pooled   - (block, system), pooled across years.
#'   4. block_pooled     - (block), pooled across years - today's original.
#'   5. columbia_pooled - ALL FOUR Columbia blocks pooled together, no block
#'                       or system split at all. Necessary, not optional: the
#'                       real donor pairs behind the pre-split "ColumbiaTrib"
#'                       ratio are Drano (ColumbiaMiddle) paired with Yakima
#'                       or McNary (ColumbiaUpper) - never two donors in the
#'                       same region in the same year. Splitting the block
#'                       without this bottom rung would leave ColumbiaMiddle
#'                       with a single ever-donor area (fails min_donor_areas
#'                       at every other tier) and ColumbiaUpper with no real
#'                       block_year ratio at all, regressing a currently-
#'                       working ratio into nothing. PugetSound/WACoast never
#'                       reach this tier - they have real system_year/
#'                       block_year coverage already. ColumbiaSnake reaches it
#'                       on every row today: Snake River's own creel
#'                       (R1_external, covered_unpartitioned - see
#'                       pst_river_block_crosswalk.csv) is never a donor,
#'                       NOT because trips and CRC harvest are spatially
#'                       mismatched - Jeremy Trump confirmed (2026-08-20) the
#'                       creel covers all open water completely and CRC
#'                       harvest can only come from the same open water, so
#'                       640/644 (spring) and 644/648 (fall) pair cleanly -
#'                       but because his summary gives one combined trip
#'                       total per season, not split by area, so there is no
#'                       per-area donor pair to build yet. A real Snake-
#'                       calibrated donor (season-specific combined area
#'                       harvest vs. Jeremy's combined trip total) is a
#'                       plausible future addition, not a structural dead end.
#' Every tier reuses the SAME validate_ratios() guardrails - no guardrail
#' loosens; the cascade just gives a row more chances to find a donor set
#' that already passes them.
estimate_block_ratios <- function(donors, control = P2_CONTROL) {

  summarise_ratios <- function(d, basis) {
    d |>
      summarise(
        n_donor_areas  = n_distinct(catch_area_code),
        n_donor_years  = n_distinct(year),
        donor_trips    = sum(creel_trips,   na.rm = TRUE),
        donor_crc      = sum(crc_harvest,   na.rm = TRUE),
        donor_creel    = sum(creel_harvest, na.rm = TRUE),
        donor_areas    = paste(sort(unique(catch_area_code)), collapse = "|"),
        donor_ratio_cv = if (n() > 1) {
          sd(ratio_crc_denom) / mean(ratio_crc_denom)
        } else NA_real_,
        max_donor_area_harvest = max(crc_harvest, na.rm = TRUE),
        # Weakest area-year behind this tier's donor pool. NA if every donor
        # is a non-creel_pe source (not applicable) or if data predates this
        # column; a low value means this tier's ratio rests at least partly
        # on a donor area-year with a real stratum-coverage gap, see
        # build_p2_donors()'s min_prop_strata_estimated for what it measures.
        min_prop_strata_estimated = suppressWarnings(
          min(min_prop_strata_estimated, na.rm = TRUE)
        ),
        .groups = "drop"
      ) |>
      mutate(
        ratio             = donor_trips / donor_crc,   # THE estimator
        ratio_creel_denom = if_else(donor_creel > 0,
                                    donor_trips / donor_creel, NA_real_),
        crc_over_creel    = if_else(donor_creel > 0,
                                    donor_crc / donor_creel, NA_real_),
        ratio_basis       = basis,
        min_prop_strata_estimated = if_else(
          is.finite(min_prop_strata_estimated), min_prop_strata_estimated, NA_real_
        )
      )
  }

  columbia_blocks <- c("ColumbiaLower", "ColumbiaMiddle", "ColumbiaUpper", "ColumbiaSnake")
  donors_columbia <- donors |> filter(block %in% columbia_blocks)

  # One donor set, but a target still joins on ITS OWN real block name - so
  # the single pooled-across-all-four-blocks row is replicated to one row
  # per Columbia block rather than left under a synthetic group label.
  columbia_pooled_row <- if (nrow(donors_columbia) > 0) {
    donors_columbia |> summarise_ratios("columbia_pooled") |> validate_ratios(control)
  } else {
    tibble()
  }
  columbia_pooled <- if (nrow(columbia_pooled_row) > 0) {
    tidyr::crossing(block = columbia_blocks, columbia_pooled_row)
  } else {
    columbia_pooled_row
  }

  list(
    system_year     = donors |> group_by(block, system, year) |>
                         summarise_ratios("system_year")     |> validate_ratios(control),
    block_year      = donors |> group_by(block, year) |>
                         summarise_ratios("block_year")       |> validate_ratios(control),
    system_pooled   = donors |> group_by(block, system) |>
                         summarise_ratios("system_pooled")    |> validate_ratios(control),
    block_pooled    = donors |> group_by(block) |>
                         summarise_ratios("block_pooled")     |> validate_ratios(control),
    columbia_pooled = columbia_pooled
  )
}

#' Fill still-unresolved rows of `resolved` from one ratio tier, joining on
#' `join_cols`. Only fills what's still NA - never overwrites a match already
#' found by a finer (earlier-tried) tier. Shared by apply_p2() and P3's
#' apply_crc_projection() (pst_crc_harvest_projection.R) so the cascade logic
#' exists in exactly one place.
fill_ratio_tier <- function(resolved, tier, join_cols) {
  if (is.null(tier) || nrow(tier) == 0) return(resolved)

  candidate <- tier |> filter(usable) |>
    select(all_of(join_cols), ratio, ratio_basis, n_donor_areas,
           donor_areas, donor_ratio_cv, max_donor_area_harvest)

  resolved |>
    left_join(candidate, by = join_cols, suffix = c("", "_new")) |>
    mutate(
      ratio                  = coalesce(ratio, ratio_new),
      ratio_basis            = coalesce(ratio_basis, ratio_basis_new),
      n_donor_areas          = coalesce(n_donor_areas, n_donor_areas_new),
      donor_areas            = coalesce(donor_areas, donor_areas_new),
      donor_ratio_cv         = coalesce(donor_ratio_cv, donor_ratio_cv_new),
      max_donor_area_harvest = coalesce(max_donor_area_harvest, max_donor_area_harvest_new)
    ) |>
    select(-ends_with("_new"))
}


#' Guardrails. Failures are marked, not dropped -- the reason must reach the
#' gap register.
validate_ratios <- function(x, control = P2_CONTROL) {
  x |>
    mutate(
      fail_reason = case_when(
        n_donor_areas < control$min_donor_areas ~
          glue("only {n_donor_areas} donor area(s); need {control$min_donor_areas}"),
        donor_crc < control$min_donor_harvest ~
          glue("donor CRC harvest {round(donor_crc)} below floor {control$min_donor_harvest}"),
        !is.na(donor_ratio_cv) & donor_ratio_cv > control$max_donor_cv ~
          glue("donor ratios incoherent within block (CV {round(donor_ratio_cv, 2)})"),
        ratio < control$ratio_plausible_range[1] |
          ratio > control$ratio_plausible_range[2] ~
          glue("ratio {round(ratio, 2)} outside plausible range"),
        TRUE ~ NA_character_
      ),
      usable = is.na(fail_reason)
    )
}


# --- 4. Apply to uncovered areas ----------------------------------------------
#' Returns list(trips, gaps). Both always returned; an empty gap tibble is a
#' real result and should be written as one.
#'
#' "Already covered" MUST be checked against effort_long's real P1 rows
#' directly, not against `donors`. Confirmed as a real double-count, not a
#' theoretical one: Green-Duwamish (R4_external, area 746) has real P1
#' trips/harvest for 2022-2025, but its month column is NA (ingest_green_
#' duwamish() pools trip length across the full year, not by month -- see
#' district_creel_ingestion.R) and its total_salmon_harvest is NA (the R4
#' effort workbook has no harvest column). Both mean it can NEVER pass
#' build_p2_donors()'s month-matched-CRC-harvest join, so it never appears
#' in `donors` regardless of how much real trip data exists for it -- and a
#' `covered` check built from `donors` would therefore treat area 746 as
#' UNCOVERED and expand it via P2 on top of the real R4_external rows already
#' in the deliverable. apply_crc_projection() (pst_crc_harvest_projection.R)
#' already gets this right for the P3 tier by checking effort_long's own
#' catch_area_code directly ("ANY tier already present... not just P1 donor
#' pairs") -- apply_p2() needed the identical fix for P2.
apply_p2 <- function(crc_yr, donors, ratios, xw_area, area_system,
                     deliver_blocks, years_scope, effort_long,
                     control = P2_CONTROL, partial_years = integer(0)) {

  covered <- effort_long |>
    filter(tier == "P1", !is.na(catch_area_code)) |>
    mutate(catch_area_code = as.character(catch_area_code)) |>
    distinct(catch_area_code, year) |>
    mutate(is_covered = TRUE)

  candidates <- crc_yr |>
    transmute(catch_area_code = as.character(stream_code),
              year            = calendar_year,
              crc_harvest     = harvest) |>
    filter(year %in% years_scope, crc_harvest > 0)

  # A ratio expansion assumes crc_harvest represents a FULL year of activity
  # (that's what the donor ratios were calibrated against). A year still
  # mid-compilation - CRC card processing runs 2+ years behind, so the most
  # recent calendar year in the tidy CSV is typically Jan-Mar only, see
  # STANDING CAVEAT 2 below - has a crc_harvest that is a sliver of the true
  # annual total, and applying the real ratio to it produces a real number
  # that is nonetheless a severe, silent undercount (confirmed empirically
  # 2026-08-29: Lewis River's dominant CRC area, 615, showed ~600 trips for
  # 2025 against 64,762-67,649 in 2022-2024 - ~1% of normal, while its
  # sibling area 611, which had ZERO 2025 CRC harvest at all and therefore
  # got P3-projected instead, landed right in its own historical range).
  # Universal fix, not a per-area or hardcoded-year one: ANY area's harvest
  # for a partial-compiled year is excluded from a P2 ratio expansion here,
  # for every block, not just the Columbia tributaries where this was first
  # noticed - removing these rows from `targets` means they never reach
  # effort_long for that year, so apply_crc_projection()'s own
  # already_covered check (pst_crc_harvest_projection.R) then correctly
  # treats them as uncovered and gives them the SAME full-year projection
  # treatment an area with zero CRC data already gets, instead of leaving
  # them stuck on an uncorrected sliver. `partial_years` is computed once in
  # run_p2_extrapolation() from crc_month's own actual month coverage - nothing
  # here decides in advance which year that is.
  excluded_partial_year <- candidates |>
    filter(year %in% partial_years) |>
    left_join(xw_area, by = "catch_area_code")
  candidates <- candidates |> filter(!year %in% partial_years)

  targets <- candidates |>
    left_join(xw_area, by = "catch_area_code") |>
    left_join(area_system, by = "catch_area_code") |>
    left_join(covered, by = c("catch_area_code", "year")) |>
    filter(is.na(is_covered)) |>
    select(-is_covered)

  # CRC areas absent from the crosswalk are unassignable, not zero.
  unmapped <- targets |> filter(is.na(block))
  targets  <- targets |> filter(block %in% deliver_blocks)

  # Covered-but-unpartitioned areas (Hanford Reach 534|535|536): trips are
  # ALREADY in the deliverable via R3_external, but at fishery grain with
  # catch_area_code = NA, so the donor test above cannot see them and would
  # class these as uncovered. Expanding them would double-count ~26k Hanford
  # boat trips. Excluded here and reported, not silently dropped.
  unpartitioned <- xw_area |>
    filter(area_coverage == "covered_unpartitioned") |>
    distinct(catch_area_code)

  excluded_unpart <- targets |> semi_join(unpartitioned, by = "catch_area_code")
  targets <- targets |> anti_join(unpartitioned, by = "catch_area_code")

  if (nrow(excluded_unpart) > 0) {
    message(glue(
      "[note] p2_expansion: {nrow(excluded_unpart)} area-year(s) in areas ",
      "{paste(sort(unique(excluded_unpart$catch_area_code)), collapse = '|')} ",
      "excluded from P2 -- effort is already in the deliverable via a source ",
      "that cannot attribute it to a single CRC area. Expanding would ",
      "double-count."
    ))
  }

  # Cascade through tiers finest-first (see estimate_block_ratios()'s
  # docstring for why all five exist). Each step only fills rows still NA
  # from the previous step - a target that already resolved at system_year
  # never gets overwritten by a coarser tier.
  resolved <- targets |>
    mutate(ratio = NA_real_, ratio_basis = NA_character_,
           n_donor_areas = NA_integer_, donor_areas = NA_character_,
           donor_ratio_cv = NA_real_, max_donor_area_harvest = NA_real_)

  resolved <- resolved |> fill_ratio_tier(ratios$system_year, c("block", "system", "year"))
  resolved <- resolved |> fill_ratio_tier(ratios$block_year,  c("block", "year"))
  if (control$allow_pooled_fallback) {
    resolved <- resolved |> fill_ratio_tier(ratios$system_pooled, c("block", "system"))
    resolved <- resolved |> fill_ratio_tier(ratios$block_pooled,  c("block"))
    resolved <- resolved |> fill_ratio_tier(ratios$columbia_pooled, c("block"))
  }

  # A ratio can be well-calibrated and still not be safe to apply this far
  # outside the harvest scale it was calibrated on. See max_target_harvest_
  # multiple's comment in P2_CONTROL for the real case (area 545) this guards.
  resolved <- resolved |>
    mutate(
      out_of_scale = !is.na(ratio) &
        crc_harvest > control$max_target_harvest_multiple * max_donor_area_harvest,
      ratio = if_else(out_of_scale, NA_real_, ratio)
    )

  p2_trips <- resolved |>
    filter(!is.na(ratio)) |>
    mutate(
      angler_trips         = crc_harvest * ratio,
      total_salmon_harvest = crc_harvest,   # CRC-reported, not creel-estimated
      tier                 = "P2",
      source_id            = "p2_block_ratio",
      method = glue(
        "P2 block ratio {round(ratio, 3)} creel-trips per CRC-salmon ",
        "({ratio_basis}, {n_donor_areas} donor area(s) [{donor_areas}]) ",
        "x CRC harvest {round(crc_harvest)}. CRC-denominated: the CRC ",
        "reporting bias is inside the ratio, not applied on top of it."
      ),
      mode           = "unknown",   # R3
      location       = "unknown",   # R3 -- CRC carries no bank/boat field
      location_basis = "crc_no_split",
      mode_basis     = "not_collected",
      fishery_name   = NA_character_,
      month          = NA_integer_
    )

  p2_gaps <- bind_rows(
    unmapped |> transmute(
      block = NA_character_, catch_area_code, year, crc_harvest,
      reason = "CRC stream_code not present in pst_river_block_crosswalk crc_areas"
    ),
    resolved |> filter(is.na(ratio), !out_of_scale) |> transmute(
      block, catch_area_code, year, crc_harvest,
      reason = "no usable ratio at any tier (system_year, block_year, system_pooled, block_pooled, columbia_pooled all failed guardrails)"
    ),
    resolved |> filter(is.na(ratio), out_of_scale) |> transmute(
      block, catch_area_code, year, crc_harvest,
      reason = glue(
        "CRC harvest {round(crc_harvest)} exceeds {control$max_target_harvest_multiple}x ",
        "the largest donor area's harvest ({round(max_donor_area_harvest)}) - ratio not ",
        "extrapolated this far outside its calibration range"
      )
    ),
    excluded_unpart |> transmute(
      block, catch_area_code, year, crc_harvest,
      reason = paste("area covered by an unpartitioned source (trips already in",
                     "deliverable, no per-area split) -- excluded to avoid",
                     "double-counting")
    ),
    excluded_partial_year |> transmute(
      block, catch_area_code, year, crc_harvest,
      reason = paste("CRC harvest for this year is partial-year only (CRC",
                     "compilation has not reached month 12) -- not used for a",
                     "P2 ratio expansion, which would silently undercount;",
                     "routed to P3's full-year projection instead")
    )
  ) |>
    mutate(tier_attempted = "P2")

  list(trips = p2_trips, gaps = p2_gaps)
}


# --- 5. Leave-one-out check ---------------------------------------------------
#' Hold out each donor area, rebuild the block ratio without it, predict its
#' trips as if uncovered. This is the empirical error band on the extrapolation
#' -- the table worth showing Jim, because it answers "how wrong is this likely
#' to be?" with a number instead of an argument.
p2_loo_check <- function(donors, control = P2_CONTROL) {
  eligible <- donors |>
    group_by(block, year) |>
    filter(n_distinct(catch_area_code) > control$min_donor_areas) |>
    ungroup()

  # No block/year has more than min_donor_areas donor areas -- group_modify()
  # below would run zero times and never create observed/predicted/loo_ratio/
  # n_donors, so the mutate() after it would fail with "object 'predicted'
  # not found" instead of returning the empty result callers already expect
  # (p2_loo_summary() is only called when nrow(loo) > 0). Not hypothetical:
  # this fires for real whenever a block's donor pool shrinks to one area per
  # year (e.g. most of ColumbiaUpper's CRC_only candidates being marked
  # covered_unpartitioned once a district-supplied total covers them).
  if (nrow(eligible) == 0) {
    return(tibble(
      block = character(), year = integer(), catch_area_code = character(),
      observed = double(), predicted = double(), loo_ratio = double(),
      n_donors = integer(), abs_error = double(), pct_error = double()
    ))
  }

  eligible |>
    group_by(block, year) |>
    group_modify(function(.x, .y) {
      map_dfr(unique(.x$catch_area_code), function(held) {
        rest <- .x |> filter(catch_area_code != held)
        obs  <- .x |> filter(catch_area_code == held)
        r    <- sum(rest$creel_trips) / sum(rest$crc_harvest)
        tibble(
          catch_area_code = held,
          observed  = obs$creel_trips,
          predicted = obs$crc_harvest * r,
          loo_ratio = r,
          n_donors  = nrow(rest)
        )
      })
    }) |>
    ungroup() |>
    mutate(
      abs_error = predicted - observed,
      pct_error = 100 * (predicted - observed) / observed
    )
}

p2_loo_summary <- function(loo) {
  loo |>
    group_by(block) |>
    summarise(
      n_tests    = n(),
      median_ape = median(abs(pct_error), na.rm = TRUE),
      p90_ape    = quantile(abs(pct_error), 0.9, na.rm = TRUE),
      bias_pct   = median(pct_error, na.rm = TRUE),  # sign matters
      .groups = "drop"
    ) |>
    arrange(desc(median_ape))
}


# --- 6. Driver ----------------------------------------------------------------
#' @return list(trips, gaps, ratios, donors, loo, loo_summary) or NULL
run_p2_extrapolation <- function(effort_long, crc_yr, crc_month, crosswalk,
                                 deliver_blocks, years_scope,
                                 control = P2_CONTROL) {

  if (is.null(crc_yr) || is.null(crc_month)) {
    message("[gap] p2_expansion: no CRC harvest table; P2 skipped entirely.")
    return(NULL)
  }

  xw_area <- expand_crosswalk_areas(crosswalk)
  donors  <- build_p2_donors(effort_long, crc_month, xw_area, deliver_blocks, control)
  n_creel_areas <- attr(donors, "n_creel_areas")

  composite_donors <- build_composite_donors(effort_long, crc_month, crosswalk, deliver_blocks)
  if (nrow(composite_donors) > 0) {
    message(glue(
      "[note] p2_donors: {nrow(composite_donors)} composite-area donor ",
      "area-year(s) added ({paste(sort(unique(composite_donors$catch_area_code)), collapse = '; ')})."
    ))
    donors <- bind_rows(donors, composite_donors)
  }

  if (nrow(donors) == 0) {
    message("[blocker] p2_expansion: no donor area-years -- no CRC/creel overlap.")
    return(NULL)
  }

  message(glue(
    "[note] p2_donors: {nrow(donors)} donor area-years across ",
    "{n_distinct(donors$block)} blocks (of {n_creel_areas} ",
    "creel area-years; the remainder have no CRC counterpart)."
  ))

  # Every CRC area's system, not just donor areas' - targets need it too, to
  # try the system_year/system_pooled tiers before falling back to block.
  area_system <- crc_month |>
    distinct(stream_code, system) |>
    transmute(catch_area_code = as.character(stream_code), system)

  # Years where CRC compilation hasn't reached month 12 for ANY area -
  # dataset-wide, not per-area, since CRC card processing lag (2+ years,
  # STANDING CAVEAT 2) is a single compilation cutoff that affects every
  # area's most recent year identically. Self-adjusting: no hardcoded "2025"
  # anywhere - this recomputes from whatever crc_month actually contains, so
  # it keeps working once a 2026 partial year shows up next. See apply_p2()'s
  # own comment on why these years are excluded from ratio expansion rather
  # than expanded on a partial harvest.
  partial_years <- crc_month |>
    group_by(calendar_year) |>
    summarise(max_month = max(calendar_month), .groups = "drop") |>
    filter(max_month < 12) |>
    pull(calendar_year)

  if (length(partial_years) > 0) {
    message(glue(
      "[note] p2_expansion: year(s) {paste(sort(partial_years), collapse = ', ')} ",
      "have partial CRC compilation (max month present < 12) - excluded from ",
      "P2 ratio expansion for every area, routed to P3 projection instead."
    ))
  }

  ratios  <- estimate_block_ratios(donors, control)
  applied <- apply_p2(crc_yr, donors, ratios, xw_area, area_system,
                      deliver_blocks, years_scope, effort_long, control,
                      partial_years)
  loo     <- p2_loo_check(donors, control)

  message(glue(
    "[note] p2_expansion: {nrow(applied$trips)} uncovered CRC area-years ",
    "expanded; {nrow(applied$gaps)} unresolved."
  ))

  list(
    trips       = applied$trips,
    gaps        = applied$gaps,
    ratios      = bind_rows(ratios$system_year, ratios$block_year,
                            ratios$system_pooled, ratios$block_pooled,
                            ratios$columbia_pooled),
    donors      = donors,
    loo         = loo,
    loo_summary = if (nrow(loo) > 0) p2_loo_summary(loo) else tibble()
  )
}


# ==============================================================================
# DROP-IN FOR pst_fw_angler_trips_assembly.R
# ==============================================================================
# Keep build_block_ratios() as it stands -- it produces the creel-denominated
# diagnostic ratios and the CRC denominator table (p2$crc), both consumed here.
# Replace only its closing "P2 EXPANSION IS NOT WIRED IN" blocker with the call
# below, placed AFTER apply_track_b(): P2 rows carry no fishery_name and no
# location, so running them through Track B would do nothing but risk a fan-out.
#
#   source(here::here("R_scripts", "pst_p2_block_ratio.R"))
#
#   p2x <- run_p2_extrapolation(
#     effort_long    = effort_long,
#     crc_yr         = p2$crc,
#     crc_month      = p2$crc_month,
#     crosswalk      = crosswalk,
#     deliver_blocks = DELIVER_BLOCKS,
#     years_scope    = YEARS_SCOPE
#   )
#
#   if (!is.null(p2x)) {
#     effort_long <- bind_rows(effort_long, canon(p2x$trips))
#     purrr::walk(seq_len(nrow(p2x$gaps)), \(i) {
#       g <- p2x$gaps[i, ]
#       log_gap("p2_expansion", g$block, "gap",
#               glue("area {g$catch_area_code} {g$year}: {g$reason}"))
#     })
#     write_csv(p2x$ratios,      file.path(OUT_DIR, "pst_fw_p2_area_ratios.csv"))
#     write_csv(p2x$donors,      file.path(OUT_DIR, "pst_fw_p2_donors.csv"))
#     write_csv(p2x$loo,         file.path(OUT_DIR, "pst_fw_p2_loo_detail.csv"))
#     write_csv(p2x$loo_summary, file.path(OUT_DIR, "pst_fw_p2_loo_summary.csv"))
#   }
#
# canon() supplies river_label from xw_area and NA-fills anything absent, so the
# P2 rows slot into the existing schema. Build effort_by_mode_location and
# effort_by_area AFTER this bind, so P2 lands in both roll-ups. The tier column
# will start showing "P1|P2" on rivers where some areas were covered and others
# expanded -- correct, and exactly what R1 is for.
#
# ------------------------------------------------------------------------------
# STANDING CAVEATS -- carry into pst_fw_angler_trips.qmd, don't lose them here
# ------------------------------------------------------------------------------
# 1. SEASON ALIGNMENT. Numerator is creel trips for a surveyed season;
#    denominator is CRC harvest for the whole license year. Where the creel
#    season is materially shorter than the CRC window the ratio is biased LOW
#    and P2 understates. LOO will NOT catch this -- every donor shares the bias.
#
# 2. 2025 IS NOT EXPANDABLE. CRC publishes through license year 2024, so
#    calendar 2025 has only Jan-Mar coverage -- the same population as the 16
#    area-years already flagged comparable = FALSE in crc_vs_creel. RESOLVED
#    2026-08-29 for the ratio-expansion hazard this used to describe: apply_p2()
#    now excludes any area-year whose CRC harvest comes from a partial-compiled
#    year (computed dataset-wide from crc_month, not hardcoded to "2025") from
#    ratio expansion entirely, rather than expanding a sliver into a real-looking
#    but severely undercounted number. Those areas fall through to
#    apply_crc_projection() (pst_crc_harvest_projection.R) instead, which gives
#    them the same full-year projection treatment an area with zero CRC data
#    already got. Confirmed as a real, not theoretical, problem: before this
#    fix, Lewis River's dominant CRC area (615) showed ~600 2025 trips against
#    64,762-67,649 in 2022-2024 (~1% of normal) purely because it had SOME
#    partial CRC harvest and therefore never reached apply_crc_projection()'s
#    own "already covered" check.
#
# 3. SPECIES MIX. A block ratio built on coho-dominated donors applied to a
#    Chinook-dominated area transports the wrong catch rate. Holding species mix
#    roughly constant is much of what the block definition does, and why R4
#    forbids crossing blocks.
#
# 4. P2 IS TOTAL-ONLY. mode and location are "unknown" (R3). CRC has no
#    bank/boat field, so unlike P1 these rows cannot be split by Track B at all.
#    Expect pct_mode_unknown to RISE when P2 is switched on. That is the honest
#    number, not a regression.
#
# 5. NOT A SUBSTITUTE FOR P1. Where a creel program could be run, run it. P2
#    exists so the absence of a survey does not become an implicit zero in an
#    economic valuation.
