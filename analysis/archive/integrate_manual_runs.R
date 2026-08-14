# ==============================================================================
# integrate_manual_runs.R
#
# Purpose:
#   Bring the manually-run fw_creel.Rmd fisheries into the batch outputs
#   produced by analysis/multi_fishery_trip_summary.R and
#   analysis/multi_fishery_harvest_summary.R.
#
#   Ten fisheries were run by hand and committed under fishery_analyses/
#   because the batch scripts could not produce usable trip expansions for
#   them (see "WHY THESE FISHERIES" below):
#
#     CRM - Roving Creel Project/  Drano Lake salmon and steelhead 2022–2025
#     District 11/                 Nisqually salmon 2022, 2023
#     District 11/                 Puyallup Carbon salmon 2024, 2025
#     District 11/                 Puyallup_Carbon salmon 2022, 2023
#
#   (Note the two spellings of Puyallup/Carbon -- "Puyallup Carbon" for
#   2024/2025 and "Puyallup_Carbon" for 2022/2023. These are distinct
#   fishery_name values in both the DB and the batch output, not a typo to
#   normalise; joining on a cleaned name would merge four fisheries into two.)
#
#   Those runs saved PE estimates but NOT the month/crc_area reconciliation --
#   the reconcile chunk was added to fw_creel.Rmd after they were rendered.
#   This script therefore does the reconciliation post hoc from the committed
#   artifacts, applying the SAME logic as the batch scripts (Sections 4-5 of
#   multi_fishery_trip_summary.R; Section 5a of multi_fishery_harvest_summary.R),
#   then binds the results into the batch tables.
#
# Usage:
#   Rscript analysis/integrate_manual_runs.R
#
#   Runs entirely off committed files. No VPN / DB access required: the raw
#   dwg object is read from each run's inputs/dwg_raw.rds rather than re-fetched,
#   and the estimation window is recovered from dwg_days.rds rather than from
#   resolve_dates(). This is deliberate -- re-fetching would risk picking up DB
#   changes made since the manual runs and silently producing numbers that no
#   longer match the committed reports.
#
# Inputs:
#   fishery_analyses/<project>/<fishery>/<RUN_ID>/params_<RUN_ID>.yml
#   fishery_analyses/<project>/<fishery>/<RUN_ID>/inputs/dwg_raw.rds
#   fishery_analyses/<project>/<fishery>/<RUN_ID>/inputs/dwg_days.rds
#   fishery_analyses/<project>/<fishery>/<RUN_ID>/inputs/inputs_pe.rds
#   fishery_analyses/<project>/<fishery>/<RUN_ID>/outputs/estimates_pe.rds
#   multi_fishery_trip_summary.csv                     (batch effort/trips)
#   analysis/outputs/multi_fishery_harvest_summary.csv (batch harvest)
#
# Outputs (analysis/outputs/):
#   integrated_trip_summary.csv     / .rds
#   integrated_harvest_summary.csv  / .rds
#   integration_ledger.csv          -- what was added, replaced, or dropped
#
# ------------------------------------------------------------------------------
# WHY THESE FISHERIES [context, not required to run the script]
#
#   In the current batch trip output, these fisheries have effort rows but
#   mean_trip_length -- and therefore total_trips_est -- is NA on EVERY row:
#
#     Drano Lake 2022/2023/2024/2025    8/8 rows blank   angler_final = "NA"
#     Nisqually salmon 2022            20/20 rows blank  angler_final = bank/boat
#     Puyallup Carbon salmon 2024      12/12 rows blank  angler_final = bank
#     Puyallup Carbon salmon 2025      10/10 rows blank
#     Puyallup_Carbon salmon 2022       8/8 rows blank
#     Puyallup_Carbon salmon 2023      12/12 rows blank
#     Lower Cowlitz 2022-23, 2023-24   48/48 blank       angler_final = "NA"
#
#   The batch trip output carries 392 NA total_trips_est rows in total. The ten
#   manual runs address 94 of them. 298 remain across 15 fisheries with no
#   manual run (listed at the end of this block); the post-integration check in
#   section 5c prints the current version of that list rather than relying on
#   these numbers, which will go stale as batch runs are repeated.
#
#   Two distinct causes are visible in that output and they should not be
#   conflated:
#
#   (a) angler_final = "NA" (Drano, Lower Cowlitz). The effort side never
#       resolved an angler type at all, so there is nothing for the trip-length
#       table to join to. For Drano this is the hard-coded-"Standard" bug: the
#       batch CSV predates DESIGN_RULES / resolve_study_design() and has no
#       study_design column, so those rows were produced under the wrong design
#       and must be DISCARDED, not merged. The manual runs used
#       study_design: Drano and resolve to Bank/Boat correctly.
#
#   (b) angler_final populated but trip length still NA (Nisqually,
#       Puyallup/Carbon). The effort side is fine; the join to
#       mean_trip_length_monthly on year x month x crc_area x angler_final is
#       what fails. Root cause is NOT diagnosed here -- this script just
#       supplies the correct values from the manual runs. Worth chasing
#       separately, because 13 further fisheries have the same symptom on SOME
#       rows and are not covered by any manual run: Quillayute fall 2022
#       (38/80), 2023 (50/80), 2024 (14/60), 2025 (32/80); Lower Chehalis 2023
#       (14/32); Hoh fall 2023 (10/16), 2025 (6/28); Upper Chehalis 2023
#       (10/24); Chehalis 2024 (8/32); Stillaguamish 2024-25 (8/32); Skagit
#       fall 2025 (6/28); Satsop 2023 (4/12); Snohomish fall 2025 (2/24).
#
#   Nisqually salmon 2023 is absent from the batch trip output entirely
#   (0 rows) while present in the harvest output -- a full failure, not a
#   partial one. It is an insert here, not a replacement.
#
# CATCH-GROUP HETEROGENEITY ACROSS THE MANUAL RUNS
#
#   The dynamic total-salmon group builds its pattern from each fishery's own
#   observed values, so est_cg differs run to run. Most of that variation is
#   the intended behaviour (species present differ by system and year):
#
#     Drano 2022, 2025            Chinook|Coho
#     Drano 2023, 2024            Chinook|Coho|Sockeye
#     Nisqually 2022              Chinook|Coho
#     Nisqually 2023              Chinook|Coho|Pink
#     Puyallup Carbon 2024        Chinook|Coho
#     Puyallup Carbon 2025        Chinook|Chum|Coho|Pink
#     Puyallup_Carbon 2022        Chinook|Coho
#     Puyallup_Carbon 2023        Chinook|Chum|Coho|Pink
#
#   Two cases are NOT routine and are checked for in section 2:
#
#   - Puyallup_Carbon salmon 2023 has life_stage levels
#     "Adult|Jack|NA|Smolt", i.e. kept SMOLTS are inside the total-salmon
#     harvest group. Whether smolt harvest belongs in a PST adult-equivalent
#     valuation is a scope decision, not a data-cleaning one. The run is not
#     rejected here; a warning names the fishery so it can be decided
#     deliberately. Nisqually 2023 and Puyallup_Carbon 2023 also carry an "NA"
#     life-stage level, which is unrecorded stage rather than a stage.
#
#   - Drano Lake 2024 has angler_final = "NA" on some strata even in the manual
#     run. Those rows cannot join to a trip length and will remain NA after
#     integration; section 2's still_na report catches them.
# ==============================================================================

# 0. Setup -------------------------------------------------------------------

library(tidyverse)
library(cli)
library(here)
library(yaml)
library(rlang)

# Source PE pipeline functions (prep_dwg_interview_fishing_time,
# prep_dwg_interview_angler_types)
walk(list.files(here("R_functions"), full.names = TRUE), source)

# Must match the batch scripts. These are NOT read from the per-run params yml
# for boat_type_collapse / kayak-pontoon / min_fishing_time, because the point
# of integration is that manual and batch rows are comparable; a per-run
# override would silently break that. Mismatches are reported in section 2.
BOAT_TYPE_COLLAPSE        <- "Yes"
FISH_LOC_DETERMINES_TYPE  <- "No"
ANGLER_TYPE_KAYAK_PONTOON <- "bank"
MIN_FISHING_TIME          <- 0.5

# Catch-group labelling, matching multi_fishery_harvest_summary.R
SALMON_SPECIES <- c("Chinook", "Coho", "Chum", "Pink", "Sockeye")
TOTAL_LABEL    <- "TotalSalmon"

RUN_ROOT <- here("fishery_analyses")
OUT_DIR  <- here("analysis", "outputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

BATCH_TRIP_PATH    <- here("multi_fishery_trip_summary.csv")
BATCH_HARVEST_PATH <- file.path(OUT_DIR, "multi_fishery_harvest_summary.csv")

# When a manual run exists for a fishery at pe_period = "week" but the batch
# also carries "month" rows for that fishery, those month rows were produced by
# the same broken path and carry NA trip estimates. TRUE drops them; FALSE keeps
# them with source = "batch_superseded_period" so they stay visible.
DROP_ORPHANED_BATCH_PERIODS <- TRUE


# 1. Discover committed manual runs ------------------------------------------

if (!dir.exists(RUN_ROOT)) {
  cli::cli_abort("No {.path fishery_analyses/} directory found at {.path {RUN_ROOT}}.")
}

run_dirs <- list.dirs(RUN_ROOT, recursive = TRUE, full.names = TRUE) |>
  purrr::keep(~ length(list.files(.x, pattern = "^params_.*\\.yml$")) == 1)

if (length(run_dirs) == 0) {
  cli::cli_abort("No run directories with a {.file params_*.yml} found under {.path {RUN_ROOT}}.")
}

run_index <- tibble::tibble(run_dir = run_dirs) |>
  dplyr::mutate(
    run_id       = basename(run_dir),
    params_path  = purrr::map_chr(run_dir, ~ list.files(.x, pattern = "^params_.*\\.yml$",
                                                        full.names = TRUE)[1]),
    params       = purrr::map(params_path, yaml::read_yaml),
    fishery_name = purrr::map_chr(params, "fishery_name"),
    project_name = purrr::map_chr(params, "project_name"),
    study_design = purrr::map_chr(params, "study_design"),
    pe_period    = purrr::map_chr(params, "period_pe"),
    # Run IDs end in a render date (…_YYYYMMDD); used to pick the latest run
    # when a fishery has been rendered more than once.
    run_date     = stringr::str_extract(run_id, "\\d{8}$")
  )

# Keep only the most recent run per fishery. Rendering twice is normal; silently
# binding both would double-count the fishery in the integrated output.
dupes <- run_index |> dplyr::count(fishery_name) |> dplyr::filter(n > 1)
if (nrow(dupes) > 0) {
  cli::cli_alert_warning(
    "{nrow(dupes)} fishery/fisheries have multiple committed runs; keeping the \\
     latest by run-date suffix:"
  )
  run_index |>
    dplyr::filter(fishery_name %in% dupes$fishery_name) |>
    dplyr::select(fishery_name, run_id, run_date) |>
    dplyr::arrange(fishery_name, dplyr::desc(run_date)) |>
    print(n = 50)
}

run_index <- run_index |>
  dplyr::arrange(fishery_name, dplyr::desc(run_date)) |>
  dplyr::distinct(fishery_name, .keep_all = TRUE)

cli::cli_h3("Manual runs discovered")
run_index |>
  dplyr::select(fishery_name, project_name, study_design, pe_period, run_id) |>
  print(n = 50)


# 2. Per-run reconciliation ---------------------------------------------------

reconcile_run <- function(run_dir, run_id, fishery_name, study_design,
                          pe_period, params_yml) {

  cli::cli_alert_info("Reconciling {.val {fishery_name}} [{.val {study_design}}]")

  # --- Load committed artifacts ---
  need <- c(
    dwg_raw   = file.path(run_dir, "inputs",  "dwg_raw.rds"),
    dwg_days  = file.path(run_dir, "inputs",  "dwg_days.rds"),
    inputs_pe = file.path(run_dir, "inputs",  "inputs_pe.rds"),
    est_pe    = file.path(run_dir, "outputs", "estimates_pe.rds")
  )
  missing <- need[!file.exists(need)]
  if (length(missing) > 0) {
    cli::cli_abort(
      c("Missing artifact(s) for {.val {fishery_name}}:",
        "x" = "{.path {unname(missing)}}")
    )
  }

  dwg          <- readRDS(need[["dwg_raw"]])
  dwg$days     <- readRDS(need[["dwg_days"]])
  inputs_pe    <- readRDS(need[["inputs_pe"]])
  estimates_pe <- readRDS(need[["est_pe"]])

  # Design-constant drift check. A manual run rendered with different
  # wrangling constants is not comparable to the batch rows it is being merged
  # into, and the difference would be invisible in the output.
  const_check <- tibble::tribble(
    ~param,                          ~batch,                     ~run,
    "boat_type_collapse",            BOAT_TYPE_COLLAPSE,         params_yml$boat_type_collapse,
    "fish_location_determines_type", FISH_LOC_DETERMINES_TYPE,   params_yml$fish_location_determines_type,
    "angler_type_kayak_pontoon",     ANGLER_TYPE_KAYAK_PONTOON,  params_yml$angler_type_kayak_pontoon,
    "min_fishing_time",              as.character(MIN_FISHING_TIME),
                                                                 as.character(params_yml$min_fishing_time)
  ) |>
    dplyr::filter(!is.na(run), batch != run)

  if (nrow(const_check) > 0) {
    cli::cli_alert_warning(
      "  {.val {fishery_name}}: run parameters differ from batch constants; \\
       merged rows may not be comparable:"
    )
    print(const_check)
  }

  # Estimation window, recovered from the committed days table rather than
  # re-resolved against the DB (see header).
  date_start <- min(dwg$days$event_date, na.rm = TRUE)
  date_end   <- max(dwg$days$event_date, na.rm = TRUE)

  # --- Rebuild interview_angler_types -------------------------------------
  #
  # Recomputed from dwg_raw rather than read from dwg_summ.rds. dwg_summ$interview
  # is interview_plus_catch, which prep_dwg_interview_catch() REPLICATES once per
  # catch group. The manual runs used a single pooled total-salmon group, so k=1
  # and the two happen to coincide today -- but that is a property of how those
  # runs were configured, not a guarantee. Recomputing is exactly what the batch
  # script does and cannot silently double-count if a future run uses more groups.

  params <- list(
    fishery_name = fishery_name,
    project_name = params_yml$project_name,
    study_design = study_design
  )

  int_filt <- dwg$interview |>
    dplyr::filter(dplyr::between(event_date, date_start, date_end))

  interview_fishing_time <- prep_dwg_interview_fishing_time(
    params           = params,
    dwg_interview    = int_filt,
    min_fishing_time = MIN_FISHING_TIME,
    study_design     = study_design
  )

  interview_angler_types <- prep_dwg_interview_angler_types(
    params                        = params,
    interview_fishing_time        = interview_fishing_time,
    study_design                  = study_design,
    boat_type_collapse            = BOAT_TYPE_COLLAPSE,
    fish_location_determines_type = FISH_LOC_DETERMINES_TYPE,
    angler_type_kayak_pontoon     = ANGLER_TYPE_KAYAK_PONTOON
  )

  # --- Section 4: mean trip length (monthly, per angler type) --------------
  # Verbatim from multi_fishery_trip_summary.R.

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
      # person_count_final, NOT total_group_count: prep_dwg_interview_fishing_time()
      # sets it to total_group_count under Standard and angler_count under Drano,
      # and it is the count the effort math uses. No-op for Standard.
      mean_group_size          = mean(person_count_final, na.rm = TRUE),
      sd                       = sd(fishing_time),
      .groups                  = "drop"
    ) |>
    dplyr::rename(angler_type = angler_final)

  if (nrow(mean_trip_length_monthly) == 0) {
    cli::cli_abort(
      c("No completed, first-time interviews for {.val {fishery_name}}.",
        "i" = "Trip expansion is impossible; this fishery cannot be integrated.")
    )
  }

  # --- Section 5: grain reconciliation -------------------------------------

  section_crc_area <- interview_angler_types |>
    dplyr::distinct(section_num, crc_area)

  multi_area <- section_crc_area |> dplyr::count(section_num) |> dplyr::filter(n > 1)
  if (nrow(multi_area) > 0) {
    cli::cli_alert_warning(
      "  Section(s) {.val {multi_area$section_num}} map to >1 crc_area in \\
       {.val {fishery_name}}; estimates will be duplicated across areas. Review."
    )
  }

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

  # Proration weights must sum to 1 within each stratum, or effort/harvest is
  # leaked (<1) or duplicated (>1) across month boundaries. Ported from the
  # harvest script's wt_check; exact by construction, so a failure means
  # stratum_by_month and days_total disagree on which days are open.
  wt_check <- inputs_pe$days_total |>
    dplyr::left_join(stratum_by_month, by = c("section_num", "period", "day_type")) |>
    dplyr::mutate(w = n_days_in_month / N_days_open) |>
    dplyr::group_by(section_num, period, day_type) |>
    dplyr::summarize(w_sum = sum(w, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(abs(w_sum - 1) > 1e-8)

  if (nrow(wt_check) > 0) {
    cli::cli_abort(
      c("Month-proration weights do not sum to 1 for {.val {fishery_name}} in \\
         {nrow(wt_check)} stratum/strata (range \\
         {round(min(wt_check$w_sum), 4)}-{round(max(wt_check$w_sum), 4)}).",
        "x" = "stratum_by_month and days_total disagree on which days are open.")
    )
  }

  # --- Trip summary rows ---------------------------------------------------

  total_effort_monthly <- estimates_pe$effort |>
    dplyr::left_join(section_crc_area, by = "section_num") |>
    dplyr::left_join(stratum_by_month, by = c("section_num", "period", "day_type")) |>
    dplyr::mutate(effort_prorated = est * (n_days_in_month / N_days_open)) |>
    dplyr::group_by(year, month, crc_area, angler_final) |>
    dplyr::summarize(total_effort_hrs = sum(effort_prorated, na.rm = TRUE),
                     .groups = "drop")

  trip_rows <- total_effort_monthly |>
    dplyr::left_join(
      mean_trip_length_monthly,
      by = c("year", "month", "crc_area", "angler_final" = "angler_type")
    ) |>
    dplyr::mutate(
      fishery_name    = .env$fishery_name,
      study_design    = .env$study_design,
      total_trips_est = total_effort_hrs / mean_trip_length,
      pe_period       = .env$pe_period,
      source          = "manual",
      run_id          = .env$run_id
    ) |>
    dplyr::select(
      fishery_name, study_design, year, month, crc_area, angler_final,
      total_effort_hrs, n_completed_angler_trips, mean_trip_length,
      mean_group_size, sd, total_trips_est, pe_period, source, run_id
    )

  # --- Harvest summary rows ------------------------------------------------
  # est_cg strings in the manual runs were produced by the dynamic
  # total-salmon group, e.g. "Chinook|Coho_Adult|Jack_AD|UM|UNK_Kept".
  # cg_lut is rebuilt by parsing that string rather than by re-deriving it from
  # dwg$catch, so the labels describe what the run actually estimated.

  cg_lut <- tibble::tibble(est_cg = unique(stats::na.omit(estimates_pe$catch$est_cg))) |>
    dplyr::mutate(
      parts             = stringr::str_split(est_cg, "_"),
      species_levels    = purrr::map_chr(parts, 1),
      life_stage_levels = purrr::map_chr(parts, 2),
      fin_mark_levels   = purrr::map_chr(parts, 3),
      fate_levels       = purrr::map_chr(parts, 4),
      spp_vec           = stringr::str_split(species_levels, "\\|"),
      is_total          = purrr::map_lgl(spp_vec, ~ length(.x) > 1),
      catch_group       = dplyr::if_else(is_total, TOTAL_LABEL, species_levels),
      species_scope     = purrr::map_chr(spp_vec, ~ paste(.x, collapse = "+")),
      fishery_name      = .env$fishery_name
    ) |>
    dplyr::select(fishery_name, est_cg, catch_group, species_scope, is_total,
                  life_stage_levels, fin_mark_levels, fate_levels)

  # Any species in the est_cg that isn't a recognised PST salmon species means
  # the manual run's catch group is not what the batch rows represent.
  unknown_spp <- cg_lut |>
    dplyr::mutate(spp = stringr::str_split(species_scope, "\\+")) |>
    tidyr::unnest(spp) |>
    dplyr::filter(!spp %in% SALMON_SPECIES) |>
    dplyr::pull(spp) |>
    unique()
  if (length(unknown_spp) > 0) {
    cli::cli_alert_warning(
      "  {.val {fishery_name}}: est_cg contains non-PST species \\
       {.val {unknown_spp}}; harvest is not comparable to the batch \\
       total-salmon group."
    )
  }

  # Life-stage scope. The dynamic catch group spans whatever stages appear on
  # kept fish, which for at least one run (Puyallup_Carbon 2023) includes
  # Smolt. Kept smolts inside a "total salmon harvest" figure is a scope
  # decision for the PST valuation, not something to silently normalise -- and
  # it is invisible downstream because the est_cg string is the only record of
  # it. "NA" is unrecorded stage and is expected; it is not flagged.
  odd_stages <- cg_lut |>
    dplyr::mutate(ls = stringr::str_split(life_stage_levels, "\\|")) |>
    tidyr::unnest(ls) |>
    dplyr::filter(!ls %in% c("Adult", "Jack", "NA", "UNK")) |>
    dplyr::pull(ls) |>
    unique()
  if (length(odd_stages) > 0) {
    cli::cli_alert_warning(
      "  {.val {fishery_name}}: kept-salmon group includes non-adult life \\
       stage{?s} {.val {odd_stages}}. Confirm these belong in the PST harvest \\
       total before delivering."
    )
  }

  harvest_rows <- estimates_pe$catch |>
    dplyr::left_join(section_crc_area, by = "section_num") |>
    dplyr::left_join(stratum_by_month, by = c("section_num", "period", "day_type")) |>
    dplyr::mutate(
      w                = n_days_in_month / N_days_open,
      harvest_prorated = est * w,
      var_prorated     = var * (w^2)
    ) |>
    dplyr::group_by(year, month, crc_area, angler_final, est_cg) |>
    dplyr::summarize(
      harvest_est = sum(harvest_prorated, na.rm = TRUE),
      harvest_var = sum(var_prorated,     na.rm = TRUE),
      n_strata    = dplyr::n(),
      .groups     = "drop"
    ) |>
    dplyr::mutate(
      fishery_name = .env$fishery_name,
      study_design = .env$study_design,
      harvest_se   = sqrt(harvest_var),
      harvest_cv   = dplyr::if_else(harvest_est > 0, harvest_se / harvest_est, NA_real_),
      # Normal approximation at the aggregated level; the stratum-level t-based
      # l95/u95 from est_pe_catch() are not summable and are not carried forward.
      harvest_l95  = harvest_est - 1.96 * harvest_se,
      harvest_u95  = harvest_est + 1.96 * harvest_se
    ) |>
    dplyr::left_join(cg_lut, by = c("fishery_name", "est_cg")) |>
    dplyr::left_join(total_effort_monthly,
                     by = c("year", "month", "crc_area", "angler_final")) |>
    dplyr::mutate(
      harvest_per_hr = dplyr::if_else(total_effort_hrs > 0,
                                      harvest_est / total_effort_hrs, NA_real_),
      pe_period      = .env$pe_period,
      source         = "manual",
      run_id         = .env$run_id
    ) |>
    dplyr::rename(catch_area_code = crc_area) |>
    dplyr::select(
      fishery_name, study_design, year, month, catch_area_code, angler_final,
      catch_group, species_scope, is_total, est_cg,
      harvest_est, harvest_var, n_strata, harvest_se, harvest_cv,
      harvest_l95, harvest_u95,
      life_stage_levels, fin_mark_levels, fate_levels,
      total_effort_hrs, harvest_per_hr, pe_period, source, run_id
    )

  cli::cli_alert_success(
    "  {.val {fishery_name}}: {nrow(trip_rows)} trip row{?s}, \\
     {nrow(harvest_rows)} harvest row{?s}"
  )

  list(trip = trip_rows, harvest = harvest_rows)
}

manual_results <- purrr::pmap(
  list(run_index$run_dir, run_index$run_id, run_index$fishery_name,
       run_index$study_design, run_index$pe_period, run_index$params),
  function(rd, rid, fn, sd_, pp, py) {
    tryCatch(
      reconcile_run(rd, rid, fn, sd_, pp, py),
      error = function(e) {
        cli::cli_alert_danger("Failed [{.val {fn}}]: {conditionMessage(e)}")
        NULL
      }
    )
  }
)

failed_runs <- run_index$fishery_name[purrr::map_lgl(manual_results, is.null)]
if (length(failed_runs) > 0) {
  cli::cli_alert_warning("Manual runs that could not be reconciled:")
  purrr::walk(failed_runs, ~ cli::cli_bullets(c("x" = .x)))
}

manual_trip <- purrr::keep(manual_results, Negate(is.null)) |>
  purrr::map("trip") |> dplyr::bind_rows()
manual_harvest <- purrr::keep(manual_results, Negate(is.null)) |>
  purrr::map("harvest") |> dplyr::bind_rows()

if (nrow(manual_trip) == 0) {
  cli::cli_abort("No manual runs reconciled successfully; nothing to integrate.")
}

# A manual run that reproduces the batch's NA trip expansion has not fixed
# anything, and merging it would look like success while changing nothing.
still_na <- manual_trip |> dplyr::filter(is.na(total_trips_est))
if (nrow(still_na) > 0) {
  cli::cli_alert_warning(
    "{nrow(still_na)} reconciled manual row{?s} still ha{?s/ve} NA \\
     total_trips_est -- the trip-length join failed in the manual run too:"
  )
  still_na |>
    dplyr::count(fishery_name, angler_final, name = "n_rows") |>
    print(n = 30)
}


# 3. Load and normalise the batch outputs ------------------------------------
#
# The two batch tables do not share a schema and neither matches the manual
# output as-produced:
#   trip:    crc_area, no study_design      (predates the DESIGN_RULES work)
#   harvest: catch_area_code, study_design
# Both are normalised to catch_area_code + study_design here so the bind is
# a real union rather than a silent column-mismatch.

batch_trip <- readr::read_csv(BATCH_TRIP_PATH, show_col_types = FALSE) |>
  dplyr::rename(dplyr::any_of(c(catch_area_code = "crc_area"))) |>
  dplyr::mutate(
    # NA_character_, not "Standard": the batch predates per-fishery design
    # resolution, so its design is unknown rather than known-Standard. Writing
    # "Standard" here would assert something false for the Drano rows.
    study_design = NA_character_,
    source       = "batch",
    run_id       = NA_character_
  )

batch_harvest <- readr::read_csv(BATCH_HARVEST_PATH, show_col_types = FALSE) |>
  dplyr::mutate(source = "batch", run_id = NA_character_)

manual_trip <- manual_trip |>
  dplyr::rename(dplyr::any_of(c(catch_area_code = "crc_area")))

# Key types must match or the anti-join below silently keeps everything.
harmonise_keys <- function(df) {
  df |> dplyr::mutate(
    fishery_name    = as.character(fishery_name),
    catch_area_code = as.character(catch_area_code),
    angler_final    = as.character(angler_final),
    pe_period       = as.character(pe_period),
    year            = as.integer(year),
    month           = as.integer(month)
  )
}

batch_trip     <- harmonise_keys(batch_trip)
batch_harvest  <- harmonise_keys(batch_harvest)
manual_trip    <- harmonise_keys(manual_trip)
manual_harvest <- harmonise_keys(manual_harvest)


# 4. Replace batch rows with manual rows -------------------------------------
#
# REPLACE, not append. Every fishery reconciled here already has batch rows
# that are either wrong (Drano: run under the wrong study design) or unusable
# (NA trip expansion). Appending would double-count the effort.
#
# Replacement is at fishery x pe_period granularity, not fishery alone. The
# manual runs are all pe_period = "week"; the batch also carries "month" rows
# for the same fisheries, produced by the same broken path. Those orphaned
# month rows are handled by DROP_ORPHANED_BATCH_PERIODS rather than left to
# sit alongside the corrected weekly rows looking equally valid.

integrate <- function(batch, manual, label) {

  manual_keys <- manual |> dplyr::distinct(fishery_name, pe_period)
  manual_fish <- unique(manual$fishery_name)

  replaced <- batch |>
    dplyr::semi_join(manual_keys, by = c("fishery_name", "pe_period"))

  orphaned <- batch |>
    dplyr::filter(fishery_name %in% manual_fish) |>
    dplyr::anti_join(manual_keys, by = c("fishery_name", "pe_period"))

  untouched <- batch |> dplyr::filter(!fishery_name %in% manual_fish)

  inserted_fish <- setdiff(manual_fish, unique(batch$fishery_name))

  if (DROP_ORPHANED_BATCH_PERIODS) {
    kept_orphans <- orphaned[0, ]
  } else {
    kept_orphans <- orphaned |> dplyr::mutate(source = "batch_superseded_period")
  }

  out <- dplyr::bind_rows(untouched, kept_orphans, manual)

  ledger <- dplyr::bind_rows(
    manual_keys |>
      dplyr::left_join(
        replaced |> dplyr::count(fishery_name, pe_period, name = "batch_rows_replaced"),
        by = c("fishery_name", "pe_period")
      ) |>
      dplyr::left_join(
        manual |> dplyr::count(fishery_name, pe_period, name = "manual_rows_added"),
        by = c("fishery_name", "pe_period")
      ) |>
      dplyr::mutate(
        table  = label,
        action = dplyr::if_else(fishery_name %in% inserted_fish, "inserted", "replaced"),
        batch_rows_replaced = tidyr::replace_na(batch_rows_replaced, 0L)
      ),
    orphaned |>
      dplyr::count(fishery_name, pe_period, name = "batch_rows_replaced") |>
      dplyr::mutate(
        table             = label,
        action            = if (DROP_ORPHANED_BATCH_PERIODS) "dropped_orphan_period"
                            else "kept_orphan_period",
        manual_rows_added = 0L
      )
  ) |>
    dplyr::select(table, fishery_name, pe_period, action,
                  batch_rows_replaced, manual_rows_added)

  cli::cli_alert_success(
    "{label}: {nrow(untouched)} batch row{?s} untouched, \\
     {nrow(replaced)} replaced, {nrow(orphaned)} orphaned period row{?s} \\
     {if (DROP_ORPHANED_BATCH_PERIODS) 'dropped' else 'kept'}, \\
     {nrow(manual)} manual row{?s} added -> {nrow(out)} total."
  )

  list(data = out, ledger = ledger)
}

trip_int    <- integrate(batch_trip,    manual_trip,    "trip_summary")
harvest_int <- integrate(batch_harvest, manual_harvest, "harvest_summary")

integrated_trip    <- trip_int$data
integrated_harvest <- harvest_int$data
ledger             <- dplyr::bind_rows(trip_int$ledger, harvest_int$ledger)


# 5. Post-integration checks --------------------------------------------------

# (a) No duplicate keys. A duplicate means a fishery was both replaced and
#     appended, i.e. its effort is counted twice.
dupe_trip <- integrated_trip |>
  dplyr::count(fishery_name, year, month, catch_area_code, angler_final, pe_period) |>
  dplyr::filter(n > 1)
if (nrow(dupe_trip) > 0) {
  cli::cli_abort(
    c("{nrow(dupe_trip)} duplicated key{?s} in the integrated trip summary.",
      "x" = "Effort is double-counted; do not use this output.")
  )
}

dupe_harv <- integrated_harvest |>
  dplyr::count(fishery_name, year, month, catch_area_code, angler_final,
               est_cg, pe_period) |>
  dplyr::filter(n > 1)
if (nrow(dupe_harv) > 0) {
  cli::cli_abort(
    c("{nrow(dupe_harv)} duplicated key{?s} in the integrated harvest summary.",
      "x" = "Harvest is double-counted; do not use this output.")
  )
}

# (b) The integration should REDUCE the count of NA trip expansions. If it
#     doesn't, the manual runs did not deliver what they were run for.
na_before <- sum(is.na(batch_trip$total_trips_est))
na_after  <- sum(is.na(integrated_trip$total_trips_est))
cli::cli_alert_info(
  "NA total_trips_est: {na_before} before integration -> {na_after} after."
)
if (na_after >= na_before) {
  cli::cli_alert_danger(
    "Integration did not reduce the number of NA trip expansions. Review the \\
     manual reconciliation before treating this output as an improvement."
  )
}

# (c) Fisheries still carrying NA trip expansions, for the follow-up list.
still_broken <- integrated_trip |>
  dplyr::filter(is.na(total_trips_est)) |>
  dplyr::count(fishery_name, source, name = "n_na_rows") |>
  dplyr::arrange(dplyr::desc(n_na_rows))
if (nrow(still_broken) > 0) {
  cli::cli_alert_warning(
    "{nrow(still_broken)} fishery/source combination{?s} still carry NA trip \\
     expansions and are NOT covered by a manual run:"
  )
  print(still_broken, n = 40)
}

# (d) Trip and harvest effort should agree where both exist. They are computed
#     from the same est_effort by the same proration, so a mismatch means the
#     two tables were built from different runs of the same fishery.
effort_check <- integrated_trip |>
  dplyr::select(fishery_name, year, month, catch_area_code, angler_final,
                pe_period, effort_trip = total_effort_hrs) |>
  dplyr::inner_join(
    integrated_harvest |>
      dplyr::distinct(fishery_name, year, month, catch_area_code, angler_final,
                      pe_period, effort_harv = total_effort_hrs),
    by = c("fishery_name", "year", "month", "catch_area_code", "angler_final",
           "pe_period")
  ) |>
  dplyr::mutate(rel_diff = abs(effort_trip - effort_harv) /
                  pmax(effort_trip, effort_harv, na.rm = TRUE)) |>
  dplyr::filter(!is.na(rel_diff), rel_diff > 1e-6)

if (nrow(effort_check) > 0) {
  cli::cli_alert_warning(
    "{nrow(effort_check)} row{?s} where trip-table and harvest-table effort \\
     disagree by >1e-6 relative. Likely the two tables reflect different runs:"
  )
  effort_check |>
    dplyr::count(fishery_name, name = "n_rows") |>
    print(n = 30)
}


# 6. Save ---------------------------------------------------------------------

readr::write_csv(integrated_trip,    file.path(OUT_DIR, "integrated_trip_summary.csv"))
readr::write_csv(integrated_harvest, file.path(OUT_DIR, "integrated_harvest_summary.csv"))
readr::write_csv(ledger,             file.path(OUT_DIR, "integration_ledger.csv"))

saveRDS(integrated_trip,    file.path(OUT_DIR, "integrated_trip_summary.rds"))
saveRDS(integrated_harvest, file.path(OUT_DIR, "integrated_harvest_summary.rds"))

cli::cli_h3("Integration ledger")
print(ledger, n = 60)

cli::cli_alert_success(
  "Wrote {nrow(integrated_trip)} trip row{?s} and {nrow(integrated_harvest)} \\
   harvest row{?s} to {.path {OUT_DIR}}"
)
