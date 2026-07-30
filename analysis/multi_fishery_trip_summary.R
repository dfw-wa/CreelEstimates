# ==============================================================================
# multi_fishery_trip_summary.R
#
# Purpose:
#   Produce a multi-fishery summary of total PE effort estimates and mean
#   completed trip lengths for freshwater creel fisheries whose names contain a
#   year between 2022 and 2025 (inclusive). The output data frame includes
#   estimated total angler-hours, mean completed trip length, and derived total
#   trip estimates per fishery × month × angler type.
#
# Usage:
#   Source interactively or run with Rscript from the repo root:
#     Rscript analysis/multi_fishery_trip_summary.R
#   Requires VPN / internal DB access for creelutils::fetch_data() and
#   creelutils::fishery_lut(). The public Socrata endpoint used by
#   fetch_fishery_names() must also be reachable.
#
#   Output: analysis/outputs/multi_fishery_trip_summary.rds
#
# Study-design assumptions (applied uniformly to all fisheries):
#   These come from the fw_creel.Rmd YAML defaults and are used because
#   per-fishery configuration is not stored in the DB. Review any fishery whose
#   results look suspect — it may require a non-default study design.
#   - study_design                 = "Standard"
#   - boat_type_collapse           = "Yes"
#   - fish_location_determines_type = "No"
#   - angler_type_kayak_pontoon    = "bank"
#   - period_pe                    = "week"
#   - day_length                   = "night closure"
#   - min_fishing_time             = 0.5  (hours; trips shorter than this excluded)
# ==============================================================================

# 0. Setup -------------------------------------------------------------------

library(tidyverse)
library(cli)
library(here)
library(creelutils)
library(timeDate)   # holiday calendar functions called inside prep_days()
library(suncalc)    # getSunlightTimes() called inside prep_days()
library(lubridate)  # days() called inside prep_days()

# Source all PE pipeline functions
walk(list.files(here("R_functions"), full.names = TRUE), source)

# Study-design constants — see header for rationale
STUDY_DESIGN              <- "Standard"
BOAT_TYPE_COLLAPSE        <- "Yes"
FISH_LOC_DETERMINES_TYPE  <- "No"
ANGLER_TYPE_KAYAK_PONTOON <- "bank"
PERIOD_PE                 <- "week"
DAY_LENGTH                <- "night closure"
MIN_FISHING_TIME          <- 0.5


# 1. Fishery list from public Socrata endpoint --------------------------------

cli::cli_alert_info("Fetching fishery names from public Socrata endpoint...")
all_fishery_names <- creelutils::fetch_fishery_names()

# Extract 4-digit year from each name string
year_extracted <- stringr::str_extract(all_fishery_names, "\\d{4}")
no_year_mask   <- is.na(year_extracted)

if (any(no_year_mask)) {
  cli::cli_alert_warning(
    "{sum(no_year_mask)} fishery name(s) with no extractable 4-digit year \\
     (will be excluded — check for naming inconsistencies):"
  )
  purrr::walk(all_fishery_names[no_year_mask], ~ cli::cli_bullets(c("*" = .x)))
}

fisheries <- all_fishery_names[
  !no_year_mask & dplyr::between(as.integer(year_extracted), 2022L, 2025L)
]
cli::cli_alert_success(
  "Retained {length(fisheries)} fisheries with a year in 2022\u20132025."
)


# 2. Cross-check public vs. internal (DB) name lists -------------------------
#
# fetch_fishery_names() queries the public Socrata endpoint; fetch_data() pulls
# from the internal Postgres DB. These are independent sources that should
# largely overlap for active fisheries, but name drift (typos, format changes)
# can cause mismatches. If >50 % of target fisheries are absent from the DB,
# the lists likely don't correspond and we stop rather than produce empty output.

cli::cli_alert_info(
  "Cross-checking public Socrata names vs. internal DB (fishery_lut)..."
)

db_names <- tryCatch(
  creelutils::fishery_lut() |> dplyr::pull(fishery_name),
  error = function(e) {
    cli::cli_abort(
      c("Could not query internal fishery_lut.",
        "x" = "{conditionMessage(e)}")
    )
  }
)

public_not_in_db <- dplyr::setdiff(fisheries, db_names)

if (length(public_not_in_db) > 0) {
  cli::cli_alert_warning(
    "{length(public_not_in_db)} of {length(fisheries)} target fisheries appear \\
     in the public list but not in fishery_lut. They will likely fail at \\
     fetch_data() and be skipped. Investigate if the count is unexpectedly high:"
  )
  purrr::walk(public_not_in_db, ~ cli::cli_bullets(c("!" = .x)))

  if (length(public_not_in_db) > length(fisheries) * 0.5) {
    cli::cli_abort(
      c("Stopping: more than half the target fisheries \\
         ({length(public_not_in_db)}/{length(fisheries)}) are absent from \\
         the internal DB.",
        "i" = "The public and internal name lists may not correspond. \\
               Investigate before proceeding.")
    )
  }
}


# 3–6. Per-fishery processing function --------------------------------------

process_fishery <- function(fishery_name) {

  cli::cli_alert_info("Processing: {.val {fishery_name}}")

  # Resolve estimation window from internal DB
  est_dates  <- resolve_dates(fishery_name, "", "")
  date_start <- as.Date(est_dates$est_date_start)
  date_end   <- as.Date(est_dates$est_date_end)

  # Fetch raw data from internal DB (data_source explicit: match.arg() silently
  # defaults to "internal" if omitted, which can mask config mistakes)
  dwg <- creelutils::fetch_data(
    fishery_name = fishery_name,
    data_source  = "internal"
  )

  # Minimal params list required by prep_* and est_pe_effort() functions
  params <- list(
    fishery_name = fishery_name,
    project_name = "multi_fishery_analysis"
  )

  # Patch p_census values from fishery_manager into the effort table
  # (mirrors the manual edit in fw_creel.Rmd "Shared data aggregation" chunk;
  # ensures the most up-to-date spatial-coverage fractions are used)
  dwg$effort <- dwg$effort |>
    dplyr::select(-dplyr::any_of(c("p_census_bank", "p_census_boat"))) |>
    dplyr::left_join(
      dwg$fishery_manager |>
        dplyr::filter(!is.na(p_census_bank) | !is.na(p_census_boat)) |>
        dplyr::distinct(section_num, p_census_bank, p_census_boat),
      by = "section_num"
    )

  # Build the days schedule for the estimation window
  dwg$days <- prep_days(
    params            = params,
    date_begin        = est_dates$est_date_start,
    date_end          = est_dates$est_date_end,
    weekends          = c("Saturday", "Sunday"),
    lat               = mean(dwg$ll$centroid_lat, na.rm = TRUE),
    long              = mean(dwg$ll$centroid_lon, na.rm = TRUE),
    period_pe         = PERIOD_PE,
    sections          = sort(unique(dwg$interview$section_num)),
    closures          = dwg$closures,
    day_length        = DAY_LENGTH,
    day_length_inputs = list()  # unused when day_length != "manual"
  )

  # Date-filtered slices
  eff_filt <- dwg$effort |>
    dplyr::filter(dplyr::between(event_date, date_start, date_end))
  int_filt <- dwg$interview |>
    dplyr::filter(dplyr::between(event_date, date_start, date_end))

  # --- Interview wrangling pipeline ---

  interview_fishing_time <- prep_dwg_interview_fishing_time(
    dwg_interview    = int_filt,
    min_fishing_time = MIN_FISHING_TIME,
    study_design     = STUDY_DESIGN
  )

  interview_angler_types <- prep_dwg_interview_angler_types(
    interview_fishing_time        = interview_fishing_time,
    study_design                  = STUDY_DESIGN,
    boat_type_collapse            = BOAT_TYPE_COLLAPSE,
    fish_location_determines_type = FISH_LOC_DETERMINES_TYPE,
    angler_type_kayak_pontoon     = ANGLER_TYPE_KAYAK_PONTOON
  )

  # --- Effort wrangling pipeline ---

  effort_index_summ <- prep_dwg_effort_index(
    params                        = params,
    eff                           = eff_filt,
    study_design                  = STUDY_DESIGN,
    boat_type_collapse            = BOAT_TYPE_COLLAPSE,
    fish_location_determines_type = FISH_LOC_DETERMINES_TYPE,
    angler_type_kayak_pontoon     = ANGLER_TYPE_KAYAK_PONTOON
  )

  effort_census_summ <- prep_dwg_effort_census(
    params                        = params,
    eff                           = eff_filt,
    study_design                  = STUDY_DESIGN,
    boat_type_collapse            = BOAT_TYPE_COLLAPSE,
    fish_location_determines_type = FISH_LOC_DETERMINES_TYPE,
    angler_type_kayak_pontoon     = ANGLER_TYPE_KAYAK_PONTOON
  )

  # Shared summary list (mirrors dwg_summ in fw_creel.Rmd)
  dwg_summ <- list(
    interview     = interview_angler_types,
    effort_index  = effort_index_summ$index_angler_final,
    effort_census = effort_census_summ$census_angler_final,
    census_expan  = prep_dwg_census_expan(eff = dwg$effort, days = dwg$days)
  )

  # --- PE inputs list ---

  inputs_pe <- list(
    days_total = prep_inputs_pe_days_total(days = dwg$days)
  )

  inputs_pe$interview_ang_per_object <- prep_inputs_pe_int_ang_per_object(
    dwg_summarized = dwg_summ,
    study_design   = STUDY_DESIGN
  )

  inputs_pe$paired_census_index_counts <- prep_inputs_pe_paired_census_index_counts(
    days                     = dwg$days,
    dwg_summarized           = dwg_summ,
    interview_ang_per_object = inputs_pe$interview_ang_per_object,
    census_expan             = dwg_summ$census_expan,
    study_design             = STUDY_DESIGN
  )

  inputs_pe$ang_hrs_daily_mean <- prep_inputs_pe_ang_hrs(
    days                       = dwg$days,
    dwg_summarized             = dwg_summ,
    interview_ang_per_object   = inputs_pe$interview_ang_per_object,
    paired_census_index_counts = inputs_pe$paired_census_index_counts,
    study_design               = STUDY_DESIGN
  )

  inputs_pe$df <- prep_inputs_pe_df(
    angler_hours_daily_mean = inputs_pe$ang_hrs_daily_mean
  )

  # --- PE effort estimate ---

  est_effort <- est_pe_effort(
    params         = params,
    days           = dwg$days,
    pe_inputs_list = inputs_pe,
    sections       = sort(unique(dwg$interview$section_num))
  )


  # 4. Mean trip length (monthly, per angler type) --------------------------
  #
  # Uses interview_angler_types, which is the interview data enriched by
  # prep_dwg_interview_fishing_time (adds fishing_time) and
  # prep_dwg_interview_angler_types (adds angler_final). The raw dwg$interview
  # object lacks both columns, so we cannot use it directly here.
  # year and month are derived from event_date since the DB view does not
  # pre-compute them.

  mean_trip_length_monthly <- interview_angler_types |>
    dplyr::mutate(
      year  = as.integer(format(event_date, "%Y")),
      month = as.integer(format(event_date, "%m"))
    ) |>
    dplyr::filter(trip_status == "Complete", previously_interviewed == 0) |>
    dplyr::group_by(year, month, crc_area, angler_final) |>
    dplyr::summarize(
      n_completed_angler_trips = dplyr::n(),
      mean_trip_length         = mean(fishing_time),
      mean_group_size          = mean(total_group_count),
      sd                       = sd(fishing_time),
      .groups                  = "drop"
    ) |>
    dplyr::rename(angler_type = angler_final)


  # 5. Total trip estimates -------------------------------------------------
  #
  # GRAIN NOTE: est_pe_effort() returns at grain:
  #   section_num × period × day_type × angler_final
  # where `period` is a calendar-week number (with PERIOD_PE = "week").
  #
  # mean_trip_length_monthly is at grain: year × month × crc_area × angler_final.
  #
  # These grains do not directly align because:
  #   1. Effort is weekly; trip length is monthly (finer vs. coarser time grain).
  #   2. Effort is split by day_type (weekday / weekend); trip length is not.
  #   3. Effort is split by section_num; trip length uses crc_area.
  #
  # RECONCILIATION: crc_area is joined to est_effort via a section_num → crc_area
  # lookup derived from the interview data (assumes crc_area maps consistently to
  # section_num; flag for review if a section spans multiple CRC areas).
  # We then derive year + month from each stratum's min_event_date and sum `est`
  # across all section_num, period, and day_type strata within the same
  # year × month × crc_area × angler_final cell, yielding a monthly total effort
  # estimate per crc_area and angler type. Dividing by mean trip length produces
  # estimated total completed trips.
  #
  # Consequence: section-level trip-count breakdowns are not retained. If
  # per-section trip estimates are needed, trip length would also need to be
  # computed at the section × month grain using section_num from the interview
  # data — revisit if that resolution is required.

  # crc_area is an interview-level attribute; derive a section_num → crc_area
  # lookup and join it onto est_effort so it can flow through to the final output.
  section_crc_area <- interview_angler_types |>
    dplyr::distinct(section_num, crc_area)

  total_effort_monthly <- est_effort |>
    dplyr::left_join(section_crc_area, by = "section_num") |>
    dplyr::mutate(
      year  = as.integer(format(min_event_date, "%Y")),
      month = as.integer(format(min_event_date, "%m"))
    ) |>
    dplyr::group_by(fishery_name, year, month, crc_area, angler_final) |>
    dplyr::summarize(
      total_effort_hrs = sum(est, na.rm = TRUE),
      .groups          = "drop"
    )

  total_trips <- total_effort_monthly |>
    dplyr::left_join(
      mean_trip_length_monthly,
      by = c("year", "month", "crc_area", "angler_final" = "angler_type")
    ) |>
    dplyr::mutate(
      total_trips_est = total_effort_hrs / mean_trip_length
    )

  cli::cli_alert_success("Done: {.val {fishery_name}}")
  total_trips
}


# 6. Error-isolated batch run -----------------------------------------------

results <- purrr::map(
  fisheries,
  function(fn) {
    tryCatch(
      process_fishery(fn),
      error = function(e) {
        cli::cli_alert_danger(
          "Failed [{.val {fn}}]: {conditionMessage(e)}"
        )
        NULL
      }
    )
  }
)


# 7. Combine results ---------------------------------------------------------

n_success <- sum(!purrr::map_lgl(results, is.null))
n_fail    <- length(results) - n_success
cli::cli_alert_info(
  "{n_success}/{length(fisheries)} fisheries processed successfully \\
   ({n_fail} failed)."
)

combined <- purrr::keep(results, Negate(is.null)) |>
  dplyr::bind_rows()


# 8. Save output (checkpoint before any downstream analysis) ----------------

out_dir  <- here("analysis", "outputs")
out_path <- file.path(out_dir, "multi_fishery_trip_summary.rds")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(combined, out_path)

cli::cli_alert_success(
  "Saved {nrow(combined)} rows across \\
   {dplyr::n_distinct(combined$fishery_name)} fisheries to {.path {out_path}}"
)
