# ==============================================================================
# diagnose_trip_expansion_gaps.R
#
# Purpose:
#   Settle the remaining open question behind the 392 NA total_trips_est rows in
#   multi_fishery_trip_summary.csv, so the fix to the batch scripts is chosen on
#   evidence rather than on a plausible story.
#
#   Three causes are already established from source and from the batch CSV
#   itself (see MECHANISM below). This script tests the one that is NOT yet
#   established: why the trip-length join misses on EVERY row for
#   Puyallup/Carbon and Nisqually, when those fisheries have 2,000-3,500
#   interviews each and populated bank/boat types.
#
#   Runs entirely off files committed under fishery_analyses/. No DB access.
#
# Usage:
#   Rscript analysis/diagnose_trip_expansion_gaps.R
#
# ------------------------------------------------------------------------------
# MECHANISM (established -- not re-tested here)
#
# est_pe_effort() ends with:
#     right_join(pe_inputs_list$days_total, by = c("section_num","period","day_type"))
# days_total carries NO angler_final. So any (section, period, day_type) stratum
# present in days_total but absent from the left-hand summarise gets a row with
# angler_final = NA, n_obs = NA, ang_hrs_mean = NaN, est = NA.
#
# The left side is empty in two different situations, and the NA looks identical:
#
#   A1  WHOLE-FISHERY DESIGN MISMATCH. Under "Standard",
#       prep_dwg_effort_index() maps only count_type "Trailers Only" -> boat and
#       "Vehicle Only" -> total; everything else -> "fail". Drano count types are
#       Shore / Motor / Skiff / Pram, so all rows become "fail", the
#       ang_per_object join in prep_inputs_pe_ang_hrs() yields NA, drop_na()
#       empties the table, and every stratum comes back NA. This is exactly the
#       Drano 8/8 and Lower Cowlitz 48/48 signature (128 rows total).
#
#   A2  UNSAMPLED STRATUM. A section/period/day_type that was open but had no
#       index count. Legitimately unestimable -- but indistinguishable from A1
#       in the output.
#
# The batch CSV confirms the downstream consequences:
#   - ALL 392 NA rows also have n_completed_angler_trips = NA, so the failure is
#     always a LEFT-JOIN MISS on year x month x crc_area x angler_final, never a
#     division problem.
#   - 128 rows: angler_final = "NA"        (cause A above)
#   - 101 rows: angler_final ok, effort = 0 (trips are trivially 0, not unknown)
#   - 163 rows: angler_final ok, effort > 0 -> 1,834,801 angler-hours, 29.5% of
#     all effort in the file, with no completed interview at that grain.
#
# WHAT IS NOT ESTABLISHED
#
#   Of those 163 rows, ~1.79M of the 1.83M stranded hours are Puyallup/Carbon
#   (4 years) and Nisqually 2022. Those fisheries fail on EVERY row, not on
#   scattered months -- which rules out "no interviews that month" and points at
#   a systematic key mismatch. interview_proportions.qmd shows they have
#   2,068-2,853 interviews each with bank/boat populated (e.g. Puyallup_Carbon
#   2023 = 2,800 bank : 0 boat), so the interviews exist.
#
#   That leaves the filter in Section 4 of multi_fishery_trip_summary.R:
#       filter(trip_status == "Complete", previously_interviewed == 0)
#   If those fisheries code trip_status differently (lower case, a different
#   vocabulary, or NA), the filter empties the table and every month goes NA.
#
#   H1  trip_status vocabulary/NA  -> filter removes everything
#   H2  previously_interviewed NA  -> == 0 is NA, filter removes everything
#   H3  crc_area mismatch between the interview table and section_crc_area
#   H4  angler_final really is absent/"fail" on the interview side
#   H5  genuinely no completed interviews (would be a real data gap)
#
#   These are mutually distinguishable from the committed dwg_raw.rds. Section 3
#   below reports exactly which one holds, per fishery.
#
# WHY IT MATTERS FOR THE FIX
#
#   If H1/H2 -- the data are fine and the FILTER is wrong. Fixing it recovers
#   ~1.79M angler-hours of real, interview-backed trip expansion.
#   If H5 -- the data are absent and the only options are donor trip lengths
#   from another month/area/fishery, which imports an assumption that trip
#   length travels. Given the project's established finding that ratios do NOT
#   travel between blocks, donor-imputing 29% of total effort would be a much
#   weaker deliverable than fixing a filter.
#   So: do not choose a donor hierarchy until this script has run.
# ==============================================================================

library(tidyverse)
library(cli)
library(here)
library(yaml)

RUN_ROOT <- here("fishery_analyses")
OUT_DIR  <- here("analysis", "outputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

MIN_FISHING_TIME <- 0.5


# 1. Locate committed runs ----------------------------------------------------

run_dirs <- list.dirs(RUN_ROOT, recursive = TRUE, full.names = TRUE) |>
  purrr::keep(~ length(list.files(.x, pattern = "^params_.*\\.yml$")) == 1)

runs <- tibble::tibble(run_dir = run_dirs) |>
  dplyr::mutate(
    params       = purrr::map(run_dir, ~ yaml::read_yaml(
      list.files(.x, pattern = "^params_.*\\.yml$", full.names = TRUE)[1])),
    fishery_name = purrr::map_chr(params, "fishery_name"),
    study_design = purrr::map_chr(params, "study_design"),
    run_id       = basename(run_dir),
    run_date     = stringr::str_extract(run_id, "\\d{8}$")
  ) |>
  dplyr::arrange(fishery_name, dplyr::desc(run_date)) |>
  dplyr::distinct(fishery_name, .keep_all = TRUE)

cli::cli_h2("Runs available for diagnosis")
print(runs |> dplyr::select(fishery_name, study_design, run_id), n = 30)


# 2. Raw interview field inventory -------------------------------------------
#
# Reported BEFORE any filtering, so a filter that removes everything is visible
# as a vocabulary problem rather than as an empty result.

inspect_interviews <- function(run_dir, fishery_name, study_design) {

  dwg_path <- file.path(run_dir, "inputs", "dwg_raw.rds")
  if (!file.exists(dwg_path)) {
    cli::cli_alert_danger("No dwg_raw.rds for {.val {fishery_name}}")
    return(NULL)
  }
  dwg <- readRDS(dwg_path)
  iv  <- dwg$interview

  cli::cli_h3(fishery_name)
  cli::cli_alert_info("{nrow(iv)} raw interview row{?s}; design {.val {study_design}}")

  # --- H1: trip_status vocabulary ---
  if (!"trip_status" %in% names(iv)) {
    cli::cli_alert_danger("  trip_status column ABSENT -- filter cannot succeed (H1)")
    ts_tab <- tibble::tibble(value = "<column absent>", n = NA_integer_)
  } else {
    ts_tab <- iv |>
      dplyr::count(value = as.character(trip_status), name = "n") |>
      dplyr::arrange(dplyr::desc(n))
    cli::cli_alert_info("  trip_status values:")
    print(ts_tab, n = 20)
    if (!"Complete" %in% ts_tab$value) {
      cli::cli_alert_danger(
        "  No trip_status == 'Complete' -- the Section 4 filter empties this \\
         fishery (H1 CONFIRMED)."
      )
    }
  }

  # --- H2: previously_interviewed ---
  if (!"previously_interviewed" %in% names(iv)) {
    cli::cli_alert_danger("  previously_interviewed ABSENT (H2)")
    pi_tab <- tibble::tibble(value = "<column absent>", n = NA_integer_)
  } else {
    pi_tab <- iv |>
      dplyr::count(value = as.character(previously_interviewed), name = "n") |>
      dplyr::arrange(dplyr::desc(n))
    cli::cli_alert_info("  previously_interviewed values:")
    print(pi_tab, n = 20)
    n_na <- sum(is.na(iv$previously_interviewed))
    if (n_na == nrow(iv)) {
      cli::cli_alert_danger(
        "  previously_interviewed is ALL NA -- `== 0` yields NA and filter() \\
         drops every row (H2 CONFIRMED)."
      )
    } else if (n_na > 0) {
      cli::cli_alert_warning(
        "  {n_na} of {nrow(iv)} row{?s} have NA previously_interviewed and are \\
         silently dropped by `== 0`."
      )
    }
  }

  # --- Combined effect of the Section 4 filter ---
  n_pass <- NA_integer_
  if (all(c("trip_status", "previously_interviewed") %in% names(iv))) {
    n_pass <- iv |>
      dplyr::filter(trip_status == "Complete", previously_interviewed == 0) |>
      nrow()
    cli::cli_alert(
      "  Section 4 filter retains {n_pass} of {nrow(iv)} interview{?s} \\
       ({round(100 * n_pass / max(nrow(iv), 1), 1)}%)"
    )
    if (n_pass == 0) {
      cli::cli_alert_danger(
        "  FILTER EMPTIES THIS FISHERY -- mean_trip_length is NA for every \\
         month, which is the all-rows-NA signature in the batch output."
      )
    }
  }

  # --- H3: crc_area ---
  crc_tab <- if ("crc_area" %in% names(iv)) {
    iv |> dplyr::count(crc_area, name = "n")
  } else {
    cli::cli_alert_danger("  crc_area ABSENT from interviews (H3)")
    tibble::tibble(crc_area = NA, n = NA_integer_)
  }
  if ("crc_area" %in% names(iv)) {
    cli::cli_alert_info("  crc_area values: {.val {unique(as.character(iv$crc_area))}}")
    n_crc_na <- sum(is.na(iv$crc_area))
    if (n_crc_na > 0) {
      cli::cli_alert_warning(
        "  {n_crc_na} interview{?s} have NA crc_area; those cannot join to \\
         effort rows carrying a real area (H3)."
      )
    }
  }

  # --- H4: does angler_final resolve, and is fishing_time usable? ---
  # Re-run the two prep functions rather than trusting dwg_summ, which is the
  # catch-replicated table.
  params <- list(fishery_name = fishery_name, project_name = "diagnosis",
                 study_design = study_design)

  ang_tab <- tryCatch({
    ift <- prep_dwg_interview_fishing_time(
      params = params, dwg_interview = iv,
      min_fishing_time = MIN_FISHING_TIME, study_design = study_design
    )
    iat <- prep_dwg_interview_angler_types(
      params = params, interview_fishing_time = ift,
      study_design = study_design, boat_type_collapse = "Yes",
      fish_location_determines_type = "No", angler_type_kayak_pontoon = "bank"
    )
    tab <- iat |> dplyr::count(angler_final, name = "n")
    cli::cli_alert_info("  angler_final after prep functions:")
    print(tab)
    n_fail <- sum(iat$angler_final == "fail", na.rm = TRUE)
    if (n_fail > 0) {
      cli::cli_alert_danger(
        "  {n_fail} interview{?s} resolve to 'fail' -- these never match an \\
         effort row and are silently excluded from trip length (H4)."
      )
    }
    # fishing_time availability drives mean_trip_length even when the filter passes
    n_ft_na <- sum(is.na(iat$fishing_time))
    if (n_ft_na > 0) {
      cli::cli_alert_warning(
        "  {n_ft_na} of {nrow(iat)} row{?s} have NA fishing_time; mean() without \\
         na.rm returns NA for any month/area/type where even one is NA."
      )
    }
    tab
  }, error = function(e) {
    cli::cli_alert_danger("  prep functions failed: {conditionMessage(e)}")
    NULL
  })

  tibble::tibble(
    fishery_name        = fishery_name,
    study_design        = study_design,
    n_interviews        = nrow(iv),
    trip_status_values  = paste(ts_tab$value, collapse = " | "),
    has_Complete        = "Complete" %in% ts_tab$value,
    prev_int_all_na     = "previously_interviewed" %in% names(iv) &&
                            all(is.na(iv$previously_interviewed)),
    n_pass_filter       = n_pass,
    n_crc_area_na       = if ("crc_area" %in% names(iv)) sum(is.na(iv$crc_area)) else NA_integer_,
    n_angler_fail       = if (!is.null(ang_tab))
                            sum(ang_tab$n[ang_tab$angler_final == "fail"], na.rm = TRUE) else NA_integer_
  )
}

# The prep functions are needed for H4
walk(list.files(here("R_functions"), full.names = TRUE), source)

diagnosis <- purrr::pmap_dfr(
  list(runs$run_dir, runs$fishery_name, runs$study_design),
  function(rd, fn, sd_) {
    tryCatch(inspect_interviews(rd, fn, sd_),
             error = function(e) {
               cli::cli_alert_danger("Failed [{.val {fn}}]: {conditionMessage(e)}")
               NULL
             })
  }
)


# 3. Verdict ------------------------------------------------------------------

cli::cli_h2("Verdict")

verdict <- diagnosis |>
  dplyr::mutate(
    likely_cause = dplyr::case_when(
      !has_Complete                 ~ "H1 trip_status vocabulary",
      prev_int_all_na               ~ "H2 previously_interviewed all NA",
      n_pass_filter == 0            ~ "H1/H2 filter empties fishery",
      n_crc_area_na > 0             ~ "H3 crc_area NA on interviews",
      n_angler_fail > 0             ~ "H4 angler_final = fail",
      TRUE                          ~ "H5 data present -- gap is real, month-specific"
    )
  )

print(verdict, width = Inf)
readr::write_csv(verdict, file.path(OUT_DIR, "trip_expansion_diagnosis.csv"))

cli::cli_alert_info(
  "Wrote {.path {file.path(OUT_DIR, 'trip_expansion_diagnosis.csv')}}"
)

if (any(verdict$likely_cause != "H5 data present -- gap is real, month-specific")) {
  cli::cli_alert_success(
    "At least one fishery fails for a FIXABLE reason. Fix the batch script \\
     before considering donor trip lengths -- donors would paper over a bug."
  )
} else {
  cli::cli_alert_warning(
    "All fisheries show real data gaps. A donor hierarchy is then the only \\
     option; see the decision memo for the ranking and its cost."
  )
}
