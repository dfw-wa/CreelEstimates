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
# Study-design assumptions:
#   These come from the fw_creel.Rmd YAML defaults and are used because
#   per-fishery configuration is not stored in the DB.
#
#   study_design is resolved PER FISHERY from the fishery name (see section 2b),
#   NOT applied uniformly. Fisheries matching DESIGN_RULES (currently
#   "Drano Lake") run under the "Drano" branches of the PE functions; all others
#   use "Standard". This matters because the two designs read different
#   interview columns and interpret effort counts differently, and a mismatch
#   does not error — it produces a plausible-looking but wrong number.
#
#   The remaining parameters are shared across designs:
#   - boat_type_collapse           = "Yes"
#   - fish_location_determines_type = "No"
#   - angler_type_kayak_pontoon    = "bank"
#   - period_pe                    = "week" and "month" (sensitivity run)
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
library(rlang)      # .env pronoun used to pin study_design inside mutate()

# Source all PE pipeline functions
walk(list.files(here("R_functions"), full.names = TRUE), source)

# Shared design constants — see header. study_design is resolved per fishery
# in section 2b. Note fish_location_determines_type is consumed only by the
# "Standard" branches; the "Drano" branches ignore it.
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

# Known DB failures; only salmon fisheries retained for PST scope
KNOWN_FAILED <- c(
  "2024 Potholes Reservoir",
  "2025 Banks Lake",
  "Baker summer sockeye 2022",
  "Baker summer sockeye 2023"
)

fisheries <- fisheries[
  !fisheries %in% KNOWN_FAILED &
  stringr::str_detect(fisheries, stringr::regex("salmon", ignore_case = TRUE))
]
cli::cli_alert_info(
  "After excluding known failures and non-salmon: {length(fisheries)} fisheries retained."
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


# 2b. Per-fishery study design ------------------------------------------------
#
# The PE functions branch internally on study_design. "Standard" and "Drano" are
# not cosmetic variants — they consume different interview columns and assign
# angler_final from different sources:
#
#                          Standard                    Drano
#   person_count_final     total_group_count           angler_count
#   index count meaning    vehicles ("total") and      direct counts of bank
#                          trailers ("boat"), with     anglers and boats;
#                          bank derived as total-boat  angler_final is already
#                                                      bank/boat
#   ang_per_object         anglers per vehicle /       anglers per interviewed
#                          per trailer                 group (1 boat assumed)
#   census / tie-in        paired census:index ratio   every count treated as a
#                                                      census, TI_expan = 1
#
# Running a Drano fishery under the Standard branch does not error. It computes
# bank effort as (vehicle-derived total − trailer-derived boat) from counts that
# are actually direct angler and boat counts, and expands by an anglers-per-
# vehicle ratio that does not describe the data. The result is a plausible-
# looking number that is wrong.
#
# PRIOR OUTPUT WARNING: earlier runs of this script hard-coded "Standard" for
# every fishery, including the Drano Lake fisheries. Any trip estimates for
# those fisheries produced before this change should be discarded and re-run.

DESIGN_RULES <- tibble::tribble(
  ~pattern,      ~study_design,
  "Drano Lake",  "Drano"
)

DEFAULT_STUDY_DESIGN <- "Standard"
SUPPORTED_DESIGNS    <- c("Standard", "Drano")

resolve_study_design <- function(fishery_name) {
  hits <- DESIGN_RULES[
    stringr::str_detect(
      fishery_name,
      stringr::regex(DESIGN_RULES$pattern, ignore_case = TRUE)
    ), ,
    drop = FALSE
  ]

  design <- if (nrow(hits) == 0) {
    DEFAULT_STUDY_DESIGN
  } else if (dplyr::n_distinct(hits$study_design) == 1) {
    hits$study_design[1]
  } else {
    # Two rules disagreeing is a config error, not a data problem: silently
    # picking one would apply an unpredictable design.
    cli::cli_abort(
      c("Ambiguous study design for {.val {fishery_name}}.",
        "x" = "Matching rules resolve to: {.val {unique(hits$study_design)}}",
        "i" = "Fix DESIGN_RULES before re-running.")
    )
  }

  if (!design %in% SUPPORTED_DESIGNS) {
    cli::cli_abort(
      c("Unsupported study design {.val {design}} for {.val {fishery_name}}.",
        "i" = "The PE functions branch only on: {.val {SUPPORTED_DESIGNS}}.")
    )
  }
  design
}

# Interview columns each design depends on. Checked per fishery so a design
# applied to data that cannot support it fails with a clear message instead of
# an opaque error several functions downstream.
REQUIRED_INTERVIEW_COLS <- list(
  common   = c("interview_id", "event_date", "section_num", "crc_area",
               "trip_status", "previously_interviewed", "fishing_start_time",
               "interview_time", "vehicle_count", "boat_used", "boat_type"),
  Standard = c("total_group_count", "trailer_count", "fish_from_boat"),
  Drano    = c("angler_count")
)

# Report resolved designs up front so an unexpected assignment is visible
# before a long batch run starts.
design_assignment <- tibble::tibble(
  fishery_name = fisheries,
  study_design = purrr::map_chr(fisheries, resolve_study_design)
)

cli::cli_h3("Resolved study designs")
design_assignment |> dplyr::count(study_design) |> print()
if (any(design_assignment$study_design != DEFAULT_STUDY_DESIGN)) {
  design_assignment |>
    dplyr::filter(study_design != DEFAULT_STUDY_DESIGN) |>
    print(n = 30)
}


# 3–6. Per-fishery processing function --------------------------------------

process_fishery <- function(fishery_name, period_pe = PERIOD_PE) {

  study_design <- resolve_study_design(fishery_name)
  cli::cli_alert_info(
    "Processing: {.val {fishery_name}} [design: {.val {study_design}}]"
  )

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

  # Interview schema must support the resolved design. Missing angler_count
  # under "Drano", or missing total_group_count under "Standard", otherwise
  # surfaces as an opaque error inside prep_dwg_interview_fishing_time().
  missing_cols <- setdiff(
    c(REQUIRED_INTERVIEW_COLS$common, REQUIRED_INTERVIEW_COLS[[study_design]]),
    names(dwg$interview)
  )
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c("Interview table is missing column(s) required by the \
         {.val {study_design}} design.",
        "x" = "{.val {missing_cols}}")
    )
  }

  # Minimal params list required by prep_* and est_pe_effort() functions
  params <- list(
    fishery_name = fishery_name,
    project_name = "multi_fishery_analysis",
    study_design = study_design
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
    period_pe         = period_pe,
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
    study_design     = study_design
  )

  interview_angler_types <- prep_dwg_interview_angler_types(
    interview_fishing_time        = interview_fishing_time,
    study_design                  = study_design,
    boat_type_collapse            = BOAT_TYPE_COLLAPSE,
    fish_location_determines_type = FISH_LOC_DETERMINES_TYPE,
    angler_type_kayak_pontoon     = ANGLER_TYPE_KAYAK_PONTOON
  )

  # --- Effort wrangling pipeline ---

  effort_index_summ <- prep_dwg_effort_index(
    params                        = params,
    eff                           = eff_filt,
    study_design                  = study_design,
    boat_type_collapse            = BOAT_TYPE_COLLAPSE,
    fish_location_determines_type = FISH_LOC_DETERMINES_TYPE,
    angler_type_kayak_pontoon     = ANGLER_TYPE_KAYAK_PONTOON
  )

  effort_census_summ <- prep_dwg_effort_census(
    params                        = params,
    eff                           = eff_filt,
    study_design                  = study_design,
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
    study_design   = study_design
  )

  inputs_pe$paired_census_index_counts <- prep_inputs_pe_paired_census_index_counts(
    days                     = dwg$days,
    dwg_summarized           = dwg_summ,
    interview_ang_per_object = inputs_pe$interview_ang_per_object,
    census_expan             = dwg_summ$census_expan,
    study_design             = study_design
  )

  inputs_pe$ang_hrs_daily_mean <- prep_inputs_pe_ang_hrs(
    days                       = dwg$days,
    dwg_summarized             = dwg_summ,
    interview_ang_per_object   = inputs_pe$interview_ang_per_object,
    paired_census_index_counts = inputs_pe$paired_census_index_counts,
    study_design               = study_design
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
      # person_count_final, NOT total_group_count. prep_dwg_interview_fishing_time()
      # sets person_count_final = total_group_count under "Standard" and
      # angler_count under "Drano", and it is the count every downstream effort
      # calculation uses. Reading total_group_count directly would report a
      # group size inconsistent with the effort math for Drano fisheries, and
      # would return NA wherever that column is unpopulated. This is a no-op for
      # Standard fisheries.
      mean_group_size          = mean(person_count_final, na.rm = TRUE),
      sd                       = sd(fishing_time),
      .groups                  = "drop"
    ) |>
    dplyr::rename(angler_type = angler_final)


  # 5. Total trip estimates -------------------------------------------------
  #
  # GRAIN NOTE: est_pe_effort() returns at grain:
  #   section_num × period × day_type × angler_final
  # where `period` is a calendar-week or calendar-month number depending on period_pe.
  #
  # mean_trip_length_monthly is at grain: year × month × crc_area × angler_final.
  #
  # These grains do not directly align because:
  #   1. Effort periods may span month boundaries (week case); trip length is monthly.
  #   2. Effort is split by day_type (weekday / weekend); trip length is not.
  #   3. Effort is split by section_num; trip length uses crc_area.
  #
  # RECONCILIATION: each stratum's effort is prorated across calendar months by the
  # fraction of that stratum's open days falling in each month (n_days_in_month /
  # N_days_open). This correctly splits cross-month weeks and is a no-op for
  # monthly strata. crc_area is joined via a section_num lookup derived from
  # interviews (flag if a section spans multiple CRC areas).
  #
  # Consequence: section-level trip-count breakdowns are not retained.

  # crc_area is an interview-level attribute; derive a section_num → crc_area
  # lookup and join it onto est_effort so it can flow through to the final output.
  section_crc_area <- interview_angler_types |>
    dplyr::distinct(section_num, crc_area)

  # Count open days per stratum × calendar month for prorating weights
  stratum_by_month <- dwg$days |>
    dplyr::select(event_date, period, day_type, dplyr::starts_with("open_section")) |>
    tidyr::pivot_longer(
      cols      = dplyr::starts_with("open_section"),
      names_to  = "section_temp",
      values_to = "is_open"
    ) |>
    dplyr::filter(is_open) |>
    dplyr::mutate(
      section_num = as.numeric(gsub("^.*_", "", section_temp)),
      year        = as.integer(format(event_date, "%Y")),
      month       = as.integer(format(event_date, "%m"))
    ) |>
    dplyr::count(section_num, period, day_type, year, month, name = "n_days_in_month")

  total_effort_monthly <- est_effort |>
    dplyr::left_join(section_crc_area, by = "section_num") |>
    dplyr::left_join(stratum_by_month, by = c("section_num", "period", "day_type")) |>
    dplyr::mutate(effort_prorated = est * (n_days_in_month / N_days_open)) |>
    dplyr::group_by(fishery_name, year, month, crc_area, angler_final) |>
    dplyr::summarize(
      total_effort_hrs = sum(effort_prorated, na.rm = TRUE),
      .groups          = "drop"
    )

  total_trips <- total_effort_monthly |>
    dplyr::left_join(
      mean_trip_length_monthly,
      by = c("year", "month", "crc_area", "angler_final" = "angler_type")
    ) |>
    dplyr::mutate(
      total_trips_est = total_effort_hrs / mean_trip_length,
      # .env$ pin: without it dplyr resolves against the data mask first, and
      # would silently pick up a same-named column if one is added upstream.
      study_design    = .env$study_design
    ) |>
    dplyr::rename(catch_area_code = crc_area) |>
    dplyr::relocate(study_design, .after = fishery_name)

  cli::cli_alert_success(
    "Done: {.val {fishery_name}} [design: {.val {study_design}}]"
  )
  total_trips
}


# 6. Error-isolated batch run (weekly PE + monthly PE sensitivity) -----------

run_batch <- function(fisheries, period_pe) {
  cli::cli_alert_info("Running batch with period_pe = {.val {period_pe}} ...")
  purrr::map(fisheries, function(fn) {
    tryCatch(
      process_fishery(fn, period_pe = period_pe),
      error = function(e) {
        cli::cli_alert_danger("Failed [{.val {fn}}]: {conditionMessage(e)}")
        NULL
      }
    )
  })
}

results_week  <- run_batch(fisheries, "week")
results_month <- run_batch(fisheries, "month")

# Capture failures from the weekly run for fast exclusion on re-runs;
# paste into KNOWN_FAILED above to skip them without re-hitting the DB.
failed_fisheries <- fisheries[purrr::map_lgl(results_week, is.null)]
if (length(failed_fisheries) > 0) {
  cli::cli_alert_warning("Failed fisheries (add to KNOWN_FAILED to skip on re-run):")
  purrr::walk(failed_fisheries, ~ cli::cli_bullets(c("x" = .x)))
}


# 7. Combine results ---------------------------------------------------------

summarise_batch <- function(results, label, n_fisheries) {
  n_success <- sum(!purrr::map_lgl(results, is.null))
  cli::cli_alert_info(
    "pe_period={.val {label}}: {n_success}/{n_fisheries} succeeded \\
     ({n_fisheries - n_success} failed)."
  )
  purrr::keep(results, Negate(is.null)) |>
    dplyr::bind_rows() |>
    dplyr::mutate(pe_period = label)
}

combined <- dplyr::bind_rows(
  summarise_batch(results_week,  "week",  length(fisheries)),
  summarise_batch(results_month, "month", length(fisheries))
)


# 8. Save output (checkpoint before any downstream analysis) ----------------

out_dir  <- here("analysis", "outputs")
out_path <- file.path(out_dir, "multi_fishery_trip_summary.rds")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(combined, out_path)

cli::cli_alert_success(
  "Saved {nrow(combined)} rows across \\
   {dplyr::n_distinct(combined$fishery_name)} fisheries to {.path {out_path}}"
)

