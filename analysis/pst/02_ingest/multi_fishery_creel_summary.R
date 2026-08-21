# ==============================================================================
# multi_fishery_creel_summary.R
#
# Purpose:
#   Single-pass multi-fishery PE (point-estimate) summary of freshwater creel
#   salmon fisheries with a year in 2022-2025. Replaces and supersedes:
#
#       analysis/archive/multi_fishery_trip_summary_superseded_20260817.R     (effort + trips)
#       analysis/archive/multi_fishery_harvest_summary_superseded_20260817.R  (harvest)
#
#   Both predecessors fetched the same data, built the same days table, ran the
#   same interview and effort wrangling, and called est_pe_effort() -- twice,
#   independently, once per script. Merging them halves the DB round-trips and,
#   more importantly, makes the effort figure in the trips output and the effort
#   figure in the harvest output the SAME NUMBER BY CONSTRUCTION rather than two
#   estimates that happen to agree. The previous integration tooling had to
#   check that agreement explicitly; that check is now unnecessary.
#
#   Output grain:
#     trips    fishery_name x study_design x year x month x catch_area_code
#                x angler_final
#     harvest  ... the same, plus catch_group
#   The two join 1:1 on the six shared keys plus pe_period.
#
# Usage:
#   Rscript analysis/pst/02_ingest/multi_fishery_creel_summary.R
#   Requires VPN / internal DB access for creelutils::connect_creel_db(),
#   creelutils::fishery_lut(), and creelutils::fetch_data().
#
# Outputs (analysis/pst/outputs/):
#   multi_fishery_creel_trips.csv          / .rds
#   multi_fishery_creel_harvest.csv        / .rds
#   multi_fishery_creel_qa.csv             / .rds
#   multi_fishery_creel_run_ledger.csv     / .rds
#   multi_fishery_creel_week_vs_month.csv  / .rds
#
# Study-design assumptions:
#   study_design is resolved PER FISHERY from the fishery name (section 2b).
#   Fisheries matching DESIGN_RULES (currently "Drano Lake") run under the
#   "Drano" branches; all others use "Standard". The two designs read different
#   interview columns and interpret effort counts differently, and a mismatch
#   does not error -- it produces a plausible-looking but wrong number. Shared
#   across designs:
#     boat_type_collapse            = "Yes"
#     fish_location_determines_type = "No"
#     angler_type_kayak_pontoon     = "bank"
#     period_pe                     = "week" and "month" (sensitivity run)
#     day_length                    = "night closure"
#     min_fishing_time              = 0.5 hours
#
# ------------------------------------------------------------------------------
# SCOPE RULES
#
# [S1] PST SALMON ONLY. Chinook, Coho, Chum, Pink, Sockeye. Steelhead is out of
#      scope for the PST economic valuation and is excluded, as are trout, char,
#      and other gamefish. Matching is EXACT against dwg$catch$species, so a
#      DB-side rename surfaces as a warning rather than silently dropping fish.
#
# [S2] HARVEST = fate "Kept". Released fish excluded. Set HARVEST_FATE to
#      "Kept|Released" for total encounters instead.
#
# [S3] SMOLTS ARE EXCLUDED BY RULE. See EXCLUDE_LIFE_STAGES below. A kept smolt
#      is not an adult-equivalent harvest event and does not belong in a PST
#      valuation total; it is also rare enough to be a data-entry signal in its
#      own right. The dynamic catch-group builder would otherwise sweep it in:
#      Puyallup_Carbon salmon 2023 produced the group
#        Chinook|Chum|Coho|Pink_Adult|Jack|NA|Smolt_AD|NA|UM|UNK_Kept
#      i.e. kept smolts inside the total-salmon figure, visible only in the
#      est_cg string. Excluded stages are counted and reported per fishery
#      rather than silently dropped.
#
# ------------------------------------------------------------------------------
# METHOD NOTES -- read before interpreting output
#
# [N1] TOTAL SALMON IS ESTIMATED DIRECTLY, NOT SUMMED.
#   est_pe_catch() point estimates are exactly additive across est_cg: every
#   est_cg is built from the same replicated interview set and the same open-day
#   set, so daily ratio-of-means CPUE sums linearly and
#   est = N_days_open * mean(daily catch estimate) inherits that additivity.
#   VARIANCES ARE NOT ADDITIVE -- species co-occur within interviews, so summing
#   species-level var() ignores covariance. The "total salmon" catch group
#   therefore pools species at the per-interview level BEFORE the CPUE
#   calculation. Section 7 asserts point-estimate additivity as a QA check.
#
# [N2] CPUE USES ALL INTERVIEWS; TRIP LENGTH USES ONLY COMPLETED ONES.
#   prep_inputs_pe_daily_cpue_catch_est() computes catch / fishing_time_total
#   over every interview passing min_fishing_time. That is intentional: a
#   ratio-of-means CPUE is unbiased for incomplete trips because catch-to-date
#   and hours-to-date are both truncated. Mean trip length cannot use truncated
#   trips and is restricted to trip_status == "Complete". The asymmetry is
#   deliberate; it was previously undocumented in either script.
#
# [N3] STRATA WITH EFFORT BUT NO INTERVIEWS CONTRIBUTE ZERO HARVEST.
#   est_pe_catch() right-joins days_total then drop_na(est_cg), so a stratum
#   with estimated effort but no interview drops out rather than being imputed.
#   The QA table reports the share of estimated angler-hours lost this way.
#
# [N4] MONTH PRORATION OF VARIANCE IS APPROXIMATE.
#   Weekly strata straddling a month boundary are split by the fraction of open
#   days in each month. Point estimates split linearly (w); variance splits by
#   w^2, treating w as a fixed known constant.
#
# [N5] TRIP-LENGTH INTERVIEW SELECTION HAS TWO FIXES BAKED IN. See
#   prep_trip_length_interviews(). The previous filter silently zeroed out six
#   fisheries entirely; that is the single largest correction in this merge.
#
# [N6] MODE (guided vs. private) IS NOT PRODUCED HERE. angler_final resolves to
#   bank / boat only. The guided/private split comes from a separate interview
#   attribute and is handled downstream (interview_proportions.qmd).
# ==============================================================================

# 0. Setup -------------------------------------------------------------------

library(tidyverse)
library(cli)
library(here)
library(creelutils)
library(timeDate)   # holiday calendar functions called inside prep_days()
library(suncalc)    # getSunlightTimes() called inside prep_days()
library(lubridate)  # days() called inside prep_days()
library(rlang)      # abort()/condition classes; .env pronoun

walk(list.files(here("R_functions"), full.names = TRUE), source)

BOAT_TYPE_COLLAPSE        <- "Yes"
FISH_LOC_DETERMINES_TYPE  <- "No"
ANGLER_TYPE_KAYAK_PONTOON <- "bank"
PERIOD_PE                 <- "week"
DAY_LENGTH                <- "night closure"
MIN_FISHING_TIME          <- 0.5

# --- Scope constants (see [S1]-[S3]) ---
SALMON_SPECIES <- c("Chinook", "Coho", "Chum", "Pink", "Sockeye")
HARVEST_FATE   <- "Kept"
TOTAL_LABEL    <- "TotalSalmon"

# [S3] Life stages excluded from every catch group. Compared case-insensitively
# and by exact value against the observed levels, NOT as a regex, so a stage
# named e.g. "Smolt (hatchery)" would NOT be caught -- section 4 reports every
# retained level so an unexpected one is visible rather than assumed absent.
EXCLUDE_LIFE_STAGES <- c("Smolt")

# Regex metacharacters that would change the meaning of an assembled pattern.
REGEX_METACHARS <- "[.\\\\+*?\\[\\]^$(){}=!<>|:-]"


# 1. Fishery list from the internal DB, filtered to real salmon harvest ------
#
# Previously this pulled names from the public Socrata endpoint
# (fetch_fishery_names()) and kept only names containing the literal
# substring "salmon" -- a naming-convention guess, not a data check. That
# silently dropped every real salmon fishery whose name doesn't happen to
# contain "salmon", e.g. "Skagit spring Chinook 2024 upper" and "Skagit
# summer sockeye 2023" -- and did so BEFORE the run ledger existed for them,
# so the gap was invisible rather than logged. fishery_lut() is the internal
# DB's own name list (same call used successfully in
# analysis/pst/02_ingest/interview_proportions.qmd), and salmon-harvest
# membership below is now decided by looking at dwg$catch$species directly --
# the same species x fate predicate [S1]/[S2] use downstream in
# build_est_catch_groups() -- so every rejection is an actual data fact, not a
# string match, and every rejection is recorded.

cli::cli_alert_info("Connecting to internal DB and fetching fishery_lut()...")
conn <- creelutils::connect_creel_db()

all_fishery_names <- tryCatch(
  creelutils::fishery_lut(conn = conn) |> dplyr::pull(fishery_name) |> unique(),
  error = function(e) {
    DBI::dbDisconnect(conn)
    cli::cli_abort(c("Could not query internal fishery_lut.",
                     "x" = "{conditionMessage(e)}"))
  }
)

year_extracted <- stringr::str_extract(all_fishery_names, "\\d{4}")
no_year_mask   <- is.na(year_extracted)

if (any(no_year_mask)) {
  cli::cli_alert_warning(
    "{sum(no_year_mask)} fishery name(s) with no extractable 4-digit year \\
     (excluded -- check for naming inconsistencies):"
  )
  purrr::walk(all_fishery_names[no_year_mask], ~ cli::cli_bullets(c("*" = .x)))
}

candidate_fisheries <- all_fishery_names[
  !no_year_mask & dplyr::between(as.integer(year_extracted), 2022L, 2025L)
]
cli::cli_alert_success(
  "Retained {length(candidate_fisheries)} DB fisheries with a year in \\
   2022\u20132025."
)

# Known non-starters. Lower Cowlitz is here because it lacks the underlying data
# needed for effort estimation -- a confirmed data gap, not a fixable pipeline
# failure. Leaving it in produced 96 NA rows in every run.
KNOWN_FAILED <- c(
  "2024 Potholes Reservoir",
  "2025 Banks Lake",
  "Baker summer sockeye 2022",
  "Baker summer sockeye 2023",
  "Lower Cowlitz salmon and steelhead 2022-23",
  "Lower Cowlitz salmon and steelhead 2023-24"
)

candidate_fisheries <- candidate_fisheries[!candidate_fisheries %in% KNOWN_FAILED]
cli::cli_alert_info(
  "After excluding known failures: {length(candidate_fisheries)} candidate \\
   fisheries remain for the salmon-harvest pre-check."
)


# 1b. Exclude steelhead-primary fisheries -------------------------------------
#
# [S1] already excludes steelhead as a SPECIES from every catch group. This is
# a separate, name-based decision to drop steelhead-PRIMARY FISHERIES
# entirely -- even when they carry incidental Kept salmon that would otherwise
# pass the harvest pre-check below, e.g. "Humptulips winter steelhead 2024-25"
# turning up Kept Coho/Chinook alongside its steelhead interviews. That
# incidental catch is not what this deliverable is estimating. Fisheries
# naming BOTH salmon and steelhead (e.g. "Lower Cowlitz salmon and steelhead")
# are mixed-target surveys, not steelhead-primary, and are exempted from this
# exclusion -- they still go through the harvest pre-check like any other
# candidate.

steelhead_primary <- stringr::str_detect(
  candidate_fisheries, stringr::regex("steelhead", ignore_case = TRUE)
) & !stringr::str_detect(
  candidate_fisheries, stringr::regex("salmon", ignore_case = TRUE)
)

if (any(steelhead_primary)) {
  cli::cli_alert_warning(
    "{sum(steelhead_primary)} steelhead-primary fishery/fisheries excluded \\
     (name contains \"steelhead\" but not \"salmon\"):"
  )
  purrr::walk(candidate_fisheries[steelhead_primary], ~ cli::cli_bullets(c("!" = .x)))
}

candidate_fisheries <- candidate_fisheries[!steelhead_primary]
cli::cli_alert_info(
  "After excluding steelhead-primary fisheries: {length(candidate_fisheries)} \\
   candidate fisheries remain for the salmon-harvest pre-check."
)


# 1c. Exclude gamefish-primary fisheries ----------------------------------------
#
# Winter gamefish and summer gamefish fisheries are out of scope for the PST
# economic valuation, which focuses on PST salmon species only. Exclude
# fisheries with these names entirely.

gamefish_primary <- stringr::str_detect(
  candidate_fisheries,
  stringr::regex("(winter|summer) gamefish", ignore_case = TRUE)
)

if (any(gamefish_primary)) {
  cli::cli_alert_warning(
    "{sum(gamefish_primary)} gamefish-primary fishery/fisheries excluded \\
     (name contains \"winter gamefish\" or \"summer gamefish\"):"
  )
  purrr::walk(candidate_fisheries[gamefish_primary], ~ cli::cli_bullets(c("!" = .x)))
}

candidate_fisheries <- candidate_fisheries[!gamefish_primary]
cli::cli_alert_info(
  "After excluding gamefish-primary fisheries: {length(candidate_fisheries)} \\
   candidate fisheries remain for the salmon-harvest pre-check."
)


# 2. Harvest pre-check: keep only fisheries with real salmon harvest ---------
#
# A lightweight catch-only fetch_data() pull per candidate (mirroring the
# tables = "catch" pattern in interview_proportions.qmd), tested against the
# same species x fate predicate as [S1] (SALMON_SPECIES) and [S2]
# (HARVEST_FATE = "Kept"). This replaces the old name-substring filter. Every
# candidate's outcome -- kept, no salmon harvest, or a fetch failure -- is
# recorded in harvest_precheck so nothing can disappear before the run
# ledger the way the name filter's rejects used to.

cli::cli_alert_info(
  "Pre-checking {length(candidate_fisheries)} candidate fisheries for salmon \\
   harvest..."
)

harvest_precheck <- purrr::map(candidate_fisheries, function(fn) {
  res <- tryCatch(
    {
      dat <- creelutils::fetch_data(conn = conn, fishery_name = fn,
                                    tables = "catch", data_source = "internal")
      has_harvest <- !is.null(dat$catch) && nrow(dat$catch) > 0 && {
        dat$catch |>
          dplyr::mutate(
            species = tidyr::replace_na(as.character(species), "NA"),
            fate    = tidyr::replace_na(as.character(fate), "NA")
          ) |>
          dplyr::filter(species %in% SALMON_SPECIES,
                        stringr::str_detect(fate, HARVEST_FATE)) |>
          nrow() > 0
      }

      list(status = if (has_harvest) "ok" else "skipped",
           reason = if (has_harvest) NA_character_ else
             "No salmon harvest (species in SALMON_SPECIES with fate matching HARVEST_FATE) in catch table.")
    },
    error = function(e) {
      list(status = "error",
           reason = paste0("[harvest_precheck] ", conditionMessage(e)))
    }
  )
  tibble::tibble(fishery_name = fn, status = res$status, reason = res$reason)
}) |> dplyr::bind_rows()

DBI::dbDisconnect(conn)

precheck_excluded <- harvest_precheck |> dplyr::filter(status != "ok")
if (nrow(precheck_excluded) > 0) {
  cli::cli_alert_warning(
    "{nrow(precheck_excluded)} fishery/fisheries excluded at the harvest \\
     pre-check (no salmon harvest, or the pre-check fetch itself failed):"
  )
  print(precheck_excluded, n = 50)
}

fisheries <- harvest_precheck |>
  dplyr::filter(status == "ok") |>
  dplyr::pull(fishery_name)

cli::cli_alert_success(
  "{length(fisheries)} fishery/fisheries retained for full processing after \\
   the salmon-harvest pre-check."
)


# 2b. Per-fishery study design ------------------------------------------------
#
#                          Standard                    Drano
#   person_count_final     total_group_count           angler_count
#   index count meaning    vehicles ("total") and      direct counts of bank
#                          trailers ("boat"), bank     anglers and boats;
#                          derived as total - boat     angler_final already
#                                                      bank/boat
#   ang_per_object         anglers per vehicle /       anglers per interviewed
#                          per trailer                 group (1 boat assumed)
#   census / tie-in        paired census:index ratio   every count a census,
#                                                      TI_expan = 1
#
# Running a Drano fishery under Standard does not error. It computes bank effort
# as (vehicle-derived total - trailer-derived boat) from counts that are direct
# angler and boat counts, and expands by an anglers-per-vehicle ratio that does
# not describe the data. Section 5's effort preflight now catches this.

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
    ), , drop = FALSE
  ]

  design <- if (nrow(hits) == 0) {
    DEFAULT_STUDY_DESIGN
  } else if (dplyr::n_distinct(hits$study_design) == 1) {
    hits$study_design[1]
  } else {
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

REQUIRED_INTERVIEW_COLS <- list(
  common   = c("interview_id", "event_date", "section_num", "crc_area",
               "trip_status", "previously_interviewed", "fishing_start_time",
               "interview_time", "vehicle_count", "boat_used", "boat_type"),
  Standard = c("total_group_count", "trailer_count", "fish_from_boat"),
  Drano    = c("angler_count")
)

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


# 3. Failure handling: skips, stages, and preflight validation ---------------
#
#   SKIP  - the fishery structurally cannot be estimated. Expected, not a bug.
#   ERROR - something failed inside the pipeline. Tagged with the stage.
#
# Both are written to a ledger saved alongside the estimates, so a fishery
# missing from the output is always traceable to a reason. Silent absence from
# a deliverable going to a consultant is the failure mode worth avoiding.

skip_fishery <- function(reason, stage = "preflight") {
  rlang::abort(message = reason, class = "fishery_skip",
               stage = stage, reason = reason)
}

run_stage <- function(stage, code) {
  tryCatch(
    code,
    fishery_skip = function(cnd) stop(cnd),   # pass skips through untouched
    error = function(e) {
      rlang::abort(
        message = paste0("[", stage, "] ", conditionMessage(e)),
        class   = "fishery_error",
        stage   = stage,
        reason  = conditionMessage(e)
      )
    }
  )
}

# --- Preflight ---------------------------------------------------------------
# Runs immediately after fetch_data(), before anything expensive. Returns a
# sanitized copy of the pieces prep_days() needs, or raises a skip.

preflight_fishery <- function(dwg, fishery_name, date_start, date_end,
                              study_design) {

  need <- c(REQUIRED_INTERVIEW_COLS$common,
            REQUIRED_INTERVIEW_COLS[[study_design]])
  missing_cols <- setdiff(need, names(dwg$interview))
  if (length(missing_cols) > 0) {
    skip_fishery(
      paste0("Interview table is missing column(s) required by the '",
             study_design, "' design: ",
             paste(missing_cols, collapse = ", "), ".")
    )
  }

  if (is.na(date_start) || is.na(date_end)) {
    skip_fishery("Unresolvable estimation dates (NA from resolve_dates).")
  }
  if (date_end < date_start) {
    skip_fishery(paste0("Estimation end date (", date_end,
                        ") precedes start date (", date_start, ")."))
  }

  int_n <- dwg$interview |>
    dplyr::filter(dplyr::between(event_date, date_start, date_end)) |> nrow()
  if (int_n == 0) skip_fishery("No interviews within the estimation window.")

  eff_n <- dwg$effort |>
    dplyr::filter(dplyr::between(event_date, date_start, date_end)) |> nrow()
  if (eff_n == 0) skip_fishery("No effort count records within the estimation window.")

  # prep_days() builds one open_section_* column per section; an empty vector
  # produces a days table with no section columns, which fails obscurely inside
  # est_pe_effort()'s pivot_longer().
  sections <- sort(unique(stats::na.omit(
    dwg$interview |>
      dplyr::filter(dplyr::between(event_date, date_start, date_end)) |>
      dplyr::pull(section_num)
  )))
  if (length(sections) == 0) {
    skip_fishery("No non-missing section_num values on interviews.")
  }

  # mean(numeric(0)) is NaN and mean(c(NA)) is NA; either goes straight to
  # suncalc::getSunlightTimes(), which fails or returns all-NA day lengths that
  # propagate silently into zero effort.
  lat  <- suppressWarnings(mean(dwg$ll$centroid_lat, na.rm = TRUE))
  long <- suppressWarnings(mean(dwg$ll$centroid_lon, na.rm = TRUE))
  if (!is.finite(lat) || !is.finite(long)) {
    skip_fishery("No usable centroid coordinates in dwg$ll (needed for day length).")
  }

  if (is.null(dwg$catch) || nrow(dwg$catch) == 0) {
    skip_fishery("No catch records returned for this fishery.")
  }

  # REPAIR, NOT SKIP: closures referencing a section that never appears in the
  # interviews. prep_days() calls rows_update() against a grid built only from
  # `sections`, and rows_update() errors on any row it cannot match. A closure
  # on a section we are not estimating is harmless information.
  closures <- dwg$closures
  if (is.null(closures)) {
    closures <- tibble::tibble(section_num = double(),
                               event_date = as.Date(character()))
  }
  if (nrow(closures) > 0) {
    closures <- closures |>
      dplyr::mutate(event_date  = as.Date(event_date, format = "%Y-%m-%d"),
                    section_num = as.double(section_num))

    orphan <- closures |>
      dplyr::filter(dplyr::between(event_date, date_start, date_end),
                    !section_num %in% sections)

    if (nrow(orphan) > 0) {
      cli::cli_alert_warning(
        "  Dropping {nrow(orphan)} closure record{?s} for section{?s} \\
         {.val {sort(unique(orphan$section_num))}}, which have no interviews \\
         and are therefore not being estimated."
      )
      closures <- closures |> dplyr::filter(section_num %in% sections)
    }

    dup_n <- sum(duplicated(closures[, c("section_num", "event_date")]))
    if (dup_n > 0) {
      cli::cli_alert_warning("  Collapsing {dup_n} duplicate closure record{?s}.")
      closures <- closures |>
        dplyr::distinct(section_num, event_date, .keep_all = TRUE)
    }
  }

  list(sections = sections, lat = lat, long = long, closures = closures)
}

# --- Post-prep_days validation -----------------------------------------------

validate_days <- function(days, period_pe, fishery_name) {

  if (!any(grepl("^open_section_", names(days)))) {
    skip_fishery("prep_days() produced no open_section_* columns.", stage = "prep_days")
  }
  if (all(is.na(days$day_length))) {
    skip_fishery("All day_length values are NA.", stage = "prep_days")
  }

  # PERIOD x YEAR COLLISION. `period` is a bare calendar week (%W) or month
  # number with no year component, and every downstream join keys on period
  # alone. A fishery spanning more than one instance of the same week -- the
  # multi-season "2022-23" names -- would silently POOL those strata across
  # years, inflating N_days_open. A correctness failure that produces no error.
  collisions <- days |>
    dplyr::distinct(period, year) |>
    dplyr::count(period) |>
    dplyr::filter(n > 1)

  if (nrow(collisions) > 0) {
    skip_fishery(
      paste0("period_pe='", period_pe, "': period value(s) ",
             paste(collisions$period, collapse = ", "),
             " occur in more than one calendar year. Downstream joins key on ",
             "period alone and would pool these strata across years. Needs a ",
             "year-qualified period before this fishery can be estimated."),
      stage = "prep_days"
    )
  }
  invisible(TRUE)
}


# 4. Catch-group construction -------------------------------------------------
#
# prep_dwg_interview_catch() selects fish with str_detect() on each of species,
# life_stage, fin_mark, fate, and names the group by pasting the four patterns.
# The life_stage and fin_mark alternations are BUILT FROM THE OBSERVED VALUES in
# dwg$catch per fishery rather than hard-coded, so a new mark code appearing in
# the DB is picked up automatically instead of falling outside a fixed pattern
# and vanishing from the estimate; and the est_cg string self-documents exactly
# which levels were aggregated. The trade-off is that est_cg is not identical
# across fisheries -- downstream code should group on catch_group /
# species_scope, not on est_cg.
#
# NOTE on the "empty alternative" idiom: a trailing "|" (e.g. "Adult|Jack|") is
# a true wildcard that matches every string, not an enumeration. Avoided here.

build_est_catch_groups <- function(dwg_catch,
                                   fishery_name,
                                   species_keep  = SALMON_SPECIES,
                                   fate_regex    = HARVEST_FATE,
                                   exclude_stages = EXCLUDE_LIFE_STAGES) {

  # Mirror the NA -> "NA" coercion applied inside prep_dwg_interview_catch() so
  # the values enumerated here are the values it will match against.
  cat_std <- dwg_catch |>
    dplyr::mutate(
      dplyr::across(
        c(species, life_stage, fin_mark, fate),
        ~ tidyr::replace_na(as.character(.), "NA")
      )
    )

  observed_species <- sort(unique(cat_std$species))
  spp_present      <- sort(intersect(observed_species, species_keep))
  spp_unmatched    <- setdiff(observed_species, species_keep)

  if (length(spp_unmatched) > 0) {
    cli::cli_alert_info("  Non-PST species present and excluded: {.val {spp_unmatched}}")
  }
  if (length(spp_present) == 0) {
    skip_fishery(
      paste0("No PST salmon species in catch records. Species present: ",
             paste(observed_species, collapse = ", ")),
      stage = "catch_groups"
    )
  }

  harvest_rows <- cat_std |>
    dplyr::filter(species %in% spp_present,
                  stringr::str_detect(fate, fate_regex))

  if (nrow(harvest_rows) == 0) {
    skip_fishery(
      paste0("Salmon present (", paste(spp_present, collapse = ", "),
             ") but no records with fate matching '", fate_regex,
             "' -- catch-and-release only, or fate not recorded."),
      stage = "catch_groups"
    )
  }

  # --- [S3] Life-stage exclusion ------------------------------------------
  # Applied by omitting excluded stages from the alternation, so
  # prep_dwg_interview_catch()'s str_detect() simply never matches them. The
  # count is reported because a fishery recording kept smolts at all is a
  # data-entry signal worth seeing, not just a row to drop.
  all_stages <- sort(unique(harvest_rows$life_stage))
  dropped_stages <- all_stages[
    tolower(all_stages) %in% tolower(exclude_stages)
  ]
  ls_vals <- setdiff(all_stages, dropped_stages)

  if (length(dropped_stages) > 0) {
    n_dropped <- sum(harvest_rows$life_stage %in% dropped_stages)
    cli::cli_alert_warning(
      "  {.val {fishery_name}}: excluding life stage{?s} \\
       {.val {dropped_stages}} ({n_dropped} kept-fish record{?s}) per scope \\
       rule [S3]."
    )
  }

  if (length(ls_vals) == 0) {
    skip_fishery(
      paste0("Every kept-salmon record is an excluded life stage (",
             paste(dropped_stages, collapse = ", "), "); nothing in scope."),
      stage = "catch_groups"
    )
  }

  fm_vals <- sort(unique(
    harvest_rows$fin_mark[harvest_rows$life_stage %in% ls_vals]
  ))

  # A category value containing regex metacharacters would change the meaning of
  # the assembled alternation. Abort rather than produce a group whose
  # membership cannot be reasoned about.
  bad_vals <- c(spp_present, ls_vals, fm_vals) |>
    purrr::keep(~ stringr::str_detect(.x, REGEX_METACHARS))
  if (length(bad_vals) > 0) {
    cli::cli_abort(
      c("Category value(s) contain regex metacharacters and cannot be safely \\
         assembled into a catch-group pattern.",
        "x" = "{.val {bad_vals}}",
        "i" = "Handle these explicitly before re-running.")
    )
  }

  ls_regex <- paste(ls_vals, collapse = "|")
  fm_regex <- paste(fm_vals, collapse = "|")

  # VERIFY THE EXCLUSION ACTUALLY HOLDS.
  # str_detect() is a substring match, so an excluded stage would still be
  # captured if a retained level happened to be a substring of it (a stage
  # "Smolt" is safe against "Adult|Jack", but a hypothetical "Sub-Adult" would
  # be matched by "Adult"). Rather than assume, re-run the actual match the
  # pipeline will perform and assert no excluded stage survives. Cheap, and it
  # converts a silent scope violation into a hard stop.
  if (length(dropped_stages) > 0) {
    leaked <- harvest_rows |>
      dplyr::filter(life_stage %in% dropped_stages,
                    stringr::str_detect(life_stage, ls_regex))
    if (nrow(leaked) > 0) {
      cli::cli_abort(
        c("Life-stage exclusion leaked for {.val {fishery_name}}.",
          "x" = "Stage{?s} {.val {sort(unique(leaked$life_stage))}} still \\
                 match the retained pattern {.val {ls_regex}} by substring.",
          "i" = "The alternation cannot express this exclusion; the levels \\
                 need renaming or prep_dwg_interview_catch() needs anchoring.")
      )
    }
  }

  # (a) one row per species, aggregating over retained life stages and all marks
  species_groups <- tibble::tibble(
    species    = spp_present,
    life_stage = ls_regex,
    fin_mark   = fm_regex,
    fate       = fate_regex
  )

  # (b) one pooled row spanning all salmon species present -- the
  #     variance-correct total, see [N1]
  total_group <- tibble::tibble(
    species    = paste(spp_present, collapse = "|"),
    life_stage = ls_regex,
    fin_mark   = fm_regex,
    fate       = fate_regex
  )

  est_catch_groups <- dplyr::bind_rows(species_groups, total_group) |>
    as.data.frame(stringsAsFactors = FALSE)

  # est_cg is reconstructed exactly as prep_dwg_interview_catch() does it.
  cg_lut <- est_catch_groups |>
    dplyr::mutate(
      est_cg = purrr::pmap_chr(
        list(species, life_stage, fin_mark, fate),
        ~ paste0(c(..1, ..2, ..3, ..4), collapse = "_")
      ),
      catch_group   = c(spp_present, TOTAL_LABEL),
      species_scope = c(spp_present, paste(spp_present, collapse = "+")),
      is_total      = c(rep(FALSE, length(spp_present)), TRUE),
      fishery_name  = fishery_name
    ) |>
    dplyr::select(fishery_name, est_cg, catch_group, species_scope, is_total,
                  life_stage_levels = life_stage,
                  fin_mark_levels   = fin_mark,
                  fate_levels       = fate)

  list(
    est_catch_groups = est_catch_groups,
    cg_lut           = cg_lut,
    scope = tibble::tibble(
      fishery_name          = fishery_name,
      species_retained      = paste(spp_present, collapse = "|"),
      species_excluded      = paste(spp_unmatched, collapse = "|"),
      life_stages_retained  = ls_regex,
      life_stages_excluded  = paste(dropped_stages, collapse = "|"),
      n_records_stage_excl  = if (length(dropped_stages) > 0)
                                sum(harvest_rows$life_stage %in% dropped_stages) else 0L
    )
  )
}


# 4b. Trip-length interview selection -----------------------------------------
#
# The previous filter was:
#     filter(trip_status == "Complete", previously_interviewed == 0)
# Two defects in that line silently zeroed out entire fisheries.
#
# [D1] previously_interviewed == 0 is NA when the field is NA, and filter()
#      drops NA. Six fisheries record NOTHING in this field, so the filter
#      removed every interview and mean_trip_length came back NA for every
#      month. Confirmed by diagnose_trip_expansion_gaps.R:
#
#        Nisqually salmon 2022          2370 interviews -> 0 passed
#        Nisqually salmon 2023          2139 -> 0
#        Puyallup Carbon salmon 2024    2151 -> 0
#        Puyallup Carbon salmon 2025    2413 -> 0
#        Puyallup_Carbon salmon 2022    2068 -> 0
#        Puyallup_Carbon salmon 2023    2853 -> 0
#
#      ~1.79 million angler-hours had no trip expansion as a result. Fisheries
#      that expand cleanly (Drano: 612-862 of ~1000 passing) populate the field
#      normally, which is why the bug stayed invisible.
#
#      FIX: coalesce NA to 0. An unrecorded previously_interviewed is read as
#      "not previously interviewed" -- the only usable reading when the field is
#      not collected at all. The collection status travels with the output:
#        collected     - no NAs
#        partial       - some NAs, field IS in use
#        not_collected - entirely NA for this fishery
#        absent        - column missing
#      "partial" deserves scrutiny: there the field is in use, so an NA is
#      genuinely unknown and coalescing risks counting a re-interviewed angler
#      as a fresh trip. Still better than silently dropping the row, but flagged.
#
# [D2] angler_final == "fail" was never excluded. Those rows produced a
#      mean_trip_length keyed on angler_type = "fail", which can never match an
#      effort row and was dropped by the join anyway -- silently. Counts are
#      small (0-53 per fishery in the diagnosed set) but they are a data-quality
#      signal, so they are excluded explicitly and reported.

prep_trip_length_interviews <- function(interview_angler_types, fishery_name) {

  n_in   <- nrow(interview_angler_types)
  n_fail <- sum(interview_angler_types$angler_final == "fail", na.rm = TRUE)

  if (n_fail > 0) {
    cli::cli_alert_warning(
      "  {n_fail} interview{?s} with angler_final = 'fail' excluded from trip length."
    )
  }

  has_col   <- "previously_interviewed" %in% names(interview_angler_types)
  n_prev_na <- if (has_col) sum(is.na(interview_angler_types$previously_interviewed)) else NA_integer_

  prev_int_status <- dplyr::case_when(
    !has_col          ~ "absent",
    n_prev_na == n_in ~ "not_collected",
    n_prev_na > 0     ~ "partial",
    TRUE              ~ "collected"
  )

  if (prev_int_status == "not_collected") {
    cli::cli_alert_warning(
      "  previously_interviewed is entirely NA ({n_in} interview{?s}). Treating \\
       all as first interviews; without this the fishery produces no trip \\
       expansion at all."
    )
  } else if (prev_int_status == "partial") {
    cli::cli_alert_warning(
      "  {n_prev_na} of {n_in} interview{?s} have NA previously_interviewed \\
       while the field is otherwise in use. Coalesced to 0 -- review whether \\
       these are genuinely first interviews."
    )
  } else if (prev_int_status == "absent") {
    cli::cli_alert_warning(
      "  previously_interviewed column absent; treating all interviews as first."
    )
  }

  out <- interview_angler_types |>
    dplyr::filter(
      trip_status == "Complete",
      angler_final != "fail",
      dplyr::coalesce(
        if (has_col) as.numeric(previously_interviewed) else 0, 0
      ) == 0
    )

  if (nrow(out) == 0) {
    cli::cli_alert_danger(
      "  No interviews survive the trip-length filter even after the NA fix; \\
       trip expansion is impossible for this fishery."
    )
  }

  list(
    interviews = out,
    qa = tibble::tibble(
      fishery_name      = fishery_name,
      n_interviews_in   = n_in,
      n_angler_fail     = n_fail,
      n_prev_int_na     = n_prev_na,
      prev_int_status   = prev_int_status,
      n_interviews_kept = nrow(out),
      pct_kept          = round(100 * nrow(out) / max(n_in, 1), 1)
    )
  )
}


# 5. Per-fishery processing (effort + trips + harvest in ONE pass) ------------

process_fishery <- function(fishery_name, period_pe = PERIOD_PE) {

  study_design <- resolve_study_design(fishery_name)
  cli::cli_alert_info(
    "Processing: {.val {fishery_name}} [design: {.val {study_design}}]"
  )

  est_dates  <- run_stage("resolve_dates", resolve_dates(fishery_name, "", ""))
  date_start <- suppressWarnings(as.Date(est_dates$est_date_start))
  date_end   <- suppressWarnings(as.Date(est_dates$est_date_end))

  # ONE fetch serves effort, trips, and harvest.
  dwg <- run_stage("fetch_data", {
    creelutils::fetch_data(fishery_name = fishery_name, data_source = "internal")
  })

  pf <- run_stage("preflight", {
    preflight_fishery(dwg, fishery_name, date_start, date_end, study_design)
  })

  cg <- run_stage("catch_groups", build_est_catch_groups(dwg$catch, fishery_name))

  params <- list(
    fishery_name     = fishery_name,
    project_name     = "multi_fishery_creel",
    study_design     = study_design,
    est_catch_groups = cg$est_catch_groups
  )

  dwg$effort <- run_stage("patch_p_census", {
    dwg$effort |>
      dplyr::select(-dplyr::any_of(c("p_census_bank", "p_census_boat"))) |>
      dplyr::left_join(
        dwg$fishery_manager |>
          dplyr::filter(!is.na(p_census_bank) | !is.na(p_census_boat)) |>
          dplyr::distinct(section_num, p_census_bank, p_census_boat),
        by = "section_num"
      )
  })

  dwg$days <- run_stage("prep_days", {
    prep_days(
      params            = params,
      date_begin        = est_dates$est_date_start,
      date_end          = est_dates$est_date_end,
      weekends          = c("Saturday", "Sunday"),
      lat               = pf$lat,
      long              = pf$long,
      period_pe         = period_pe,
      sections          = pf$sections,
      closures          = pf$closures,
      day_length        = DAY_LENGTH,
      day_length_inputs = list()
    )
  })
  run_stage("prep_days", validate_days(dwg$days, period_pe, fishery_name))

  eff_filt <- dwg$effort |>
    dplyr::filter(dplyr::between(event_date, date_start, date_end))
  int_filt <- dwg$interview |>
    dplyr::filter(dplyr::between(event_date, date_start, date_end))

  # --- Interview wrangling (shared) ---------------------------------------

  interview_fishing_time <- run_stage("interview_fishing_time", {
    prep_dwg_interview_fishing_time(
      params           = params,
      dwg_interview    = int_filt,
      min_fishing_time = MIN_FISHING_TIME,
      study_design     = study_design
    )
  })

  interview_angler_types <- run_stage("interview_angler_types", {
    prep_dwg_interview_angler_types(
      params                        = params,
      interview_fishing_time        = interview_fishing_time,
      study_design                  = study_design,
      boat_type_collapse            = BOAT_TYPE_COLLAPSE,
      fish_location_determines_type = FISH_LOC_DETERMINES_TYPE,
      angler_type_kayak_pontoon     = ANGLER_TYPE_KAYAK_PONTOON
    )
  })

  # Attach catch. NOTE: this REPLICATES the interview table once per catch group
  # (n_groups x n_interviews rows) and drops crc_area. Both handled below.
  interview_plus_catch <- run_stage("interview_catch", {
    prep_dwg_interview_catch(
      params                      = params,
      interview_plus_angler_types = interview_angler_types,
      dwg_catch                   = dwg$catch,
      study_design                = study_design,
      est_catch_groups            = cg$est_catch_groups
    )
  })

  # --- Effort wrangling (shared) ------------------------------------------

  effort_index_summ <- run_stage("effort_index", {
    prep_dwg_effort_index(
      params                        = params,
      eff                           = eff_filt,
      study_design                  = study_design,
      boat_type_collapse            = BOAT_TYPE_COLLAPSE,
      fish_location_determines_type = FISH_LOC_DETERMINES_TYPE,
      angler_type_kayak_pontoon     = ANGLER_TYPE_KAYAK_PONTOON
    )
  })

  effort_census_summ <- run_stage("effort_census", {
    prep_dwg_effort_census(
      params                        = params,
      eff                           = eff_filt,
      study_design                  = study_design,
      boat_type_collapse            = BOAT_TYPE_COLLAPSE,
      fish_location_determines_type = FISH_LOC_DETERMINES_TYPE,
      angler_type_kayak_pontoon     = ANGLER_TYPE_KAYAK_PONTOON
    )
  })

  if (nrow(effort_index_summ$index_angler_final) == 0) {
    skip_fishery("No index effort counts after angler-type assignment.",
                 stage = "effort_index")
  }

  # EFFORT-SIDE DESIGN PREFLIGHT.
  # prep_dwg_effort_index() assigns "fail" to any count_type its branch does not
  # recognise. Under a wrong study design EVERY count becomes "fail"; the
  # ang_per_object join then yields NA, prep_inputs_pe_ang_hrs() drop_na()s the
  # table to nothing, and the right_join(days_total) at the end of
  # est_pe_effort() emits one row per stratum with angler_final = NA and
  # est = NA -- an entire fishery unestimated but looking like output. That is
  # how four Drano years were published under the hard-coded "Standard" design.
  # The emptiness check above does NOT catch it: an all-"fail" table is
  # non-empty.
  fail_counts <- effort_index_summ$index_angler_groups |>
    dplyr::filter(angler_final == "fail")

  # Recorded to a file (not just the console) so an unrecognised count_type
  # survives past the run that produced it. A "fail" here doesn't always clear
  # the frac_fail > 0.5 skip threshold below -- Drano Lake salmon and steelhead
  # 2024 slipped through with only 4 affected rows -- and the console warning
  # alone had already been missed once (recoded silently to "unknown" three
  # stages downstream, in pst_fw_angler_trips_assembly.R, with no trace of
  # which count_type actually caused it). This is the trace.
  fail_counts_diag <- tibble::tibble(
    fishery_name = character(),
    study_design = character(),
    count_type   = character(),
    n_rows       = integer()
  )

  if (nrow(fail_counts) > 0) {
    bad_types <- sort(unique(fail_counts$count_type))
    frac_fail <- nrow(fail_counts) / nrow(effort_index_summ$index_angler_groups)

    if (frac_fail > 0.5) {
      skip_fishery(
        paste0(round(100 * frac_fail), "% of index effort counts map to ",
               "angler_final = 'fail' under the '", study_design, "' design ",
               "(unrecognised count_type(s): ", paste(bad_types, collapse = ", "),
               "). Almost certainly the wrong study design -- check DESIGN_RULES."),
        stage = "effort_index_design"
      )
    }
    cli::cli_alert_warning(
      "  {nrow(fail_counts)} index count{?s} ({round(100 * frac_fail, 1)}%) map \\
       to 'fail' (count_type{?s} {.val {bad_types}}); their effort is excluded."
    )

    fail_counts_diag <- fail_counts |>
      dplyr::count(count_type, name = "n_rows") |>
      dplyr::mutate(fishery_name = fishery_name, study_design = study_design) |>
      dplyr::relocate(fishery_name, study_design)
  }

  dwg_summ <- list(
    interview     = interview_plus_catch,   # replicated; required for CPUE
    effort_index  = effort_index_summ$index_angler_final,
    effort_census = effort_census_summ$census_angler_final,
    census_expan  = prep_dwg_census_expan(eff = dwg$effort, days = dwg$days)
  )

  # --- PE inputs -----------------------------------------------------------

  inputs_pe <- run_stage("pe_days_total", {
    list(days_total = prep_inputs_pe_days_total(days = dwg$days))
  })

  # DELIBERATE DEVIATION FROM fw_creel.Rmd:
  # prep_inputs_pe_int_ang_per_object() is fed the UNREPLICATED interview table.
  # Under "Standard" it returns sum(person_count_final)/sum(vehicle_count), a
  # ratio of sums, so ang_per_object -- the only column consumed downstream --
  # is replication-invariant and effort is identical either way. But
  # person_count_total and object_count_total ARE inflated by the number of
  # catch groups when the replicated table is passed, which makes them useless
  # for QA and is a trap for future code. Costs nothing to do it correctly.
  inputs_pe$interview_ang_per_object <- run_stage("pe_ang_per_object", {
    prep_inputs_pe_int_ang_per_object(
      dwg_summarized = list(interview    = interview_angler_types,
                            effort_index = dwg_summ$effort_index),
      study_design   = study_design
    )
  })

  inputs_pe$paired_census_index_counts <- run_stage("pe_census_index", {
    prep_inputs_pe_paired_census_index_counts(
      days                     = dwg$days,
      dwg_summarized           = dwg_summ,
      interview_ang_per_object = inputs_pe$interview_ang_per_object,
      census_expan             = dwg_summ$census_expan,
      study_design             = study_design
    )
  })

  inputs_pe$ang_hrs_daily_mean <- run_stage("pe_ang_hrs", {
    prep_inputs_pe_ang_hrs(
      days                       = dwg$days,
      dwg_summarized             = dwg_summ,
      interview_ang_per_object   = inputs_pe$interview_ang_per_object,
      paired_census_index_counts = inputs_pe$paired_census_index_counts,
      study_design               = study_design
    )
  })

  if (nrow(inputs_pe$ang_hrs_daily_mean) == 0) {
    skip_fishery(
      "No daily angler-hour estimates survived angler-type assignment and the \\
       ang_per_object join; every stratum would return NA.",
      stage = "pe_ang_hrs"
    )
  }

  inputs_pe$daily_cpue_catch_est <- run_stage("pe_daily_cpue", {
    prep_inputs_pe_daily_cpue_catch_est(
      days                    = dwg$days,
      dwg_summarized          = dwg_summ,
      angler_hours_daily_mean = inputs_pe$ang_hrs_daily_mean
    )
  })

  inputs_pe$df <- run_stage("pe_df", {
    prep_inputs_pe_df(angler_hours_daily_mean = inputs_pe$ang_hrs_daily_mean)
  })

  # --- PE estimates. Effort estimated ONCE and reused. ---------------------

  est_effort <- run_stage("est_pe_effort", {
    est_pe_effort(
      params         = params,
      dwg            = dwg,
      days           = dwg$days,
      pe_inputs_list = inputs_pe,
      sections       = pf$sections
    )
  })

  est_catch <- run_stage("est_pe_catch", {
    est_pe_catch(
      params         = params,
      dwg            = dwg,
      days           = dwg$days,
      pe_inputs_list = inputs_pe
    )
  })


  # 5a. Grain reconciliation (shared by trips and harvest) ------------------
  #
  # est_pe_effort() / est_pe_catch() return
  #   section_num x period x day_type x angler_final [x est_cg]
  # where `period` is a calendar week or month depending on period_pe. Trip
  # length is monthly, by crc_area, and not split by day_type. The grains are
  # reconciled by prorating each stratum across calendar months by the fraction
  # of that stratum's open days in each month -- exact for cross-month weeks, a
  # no-op for monthly strata. crc_area joins via a section lookup derived from
  # interviews. Section-level breakdowns are not retained.

  # NA-SHADOWING FIX (Lower Chehalis salmon 2023 defect, surfaced downstream in
  # pst_fw_angler_trips_assembly.R): a section whose interviews carry crc_area
  # = NA on some rows and a real value on others produced TWO distinct rows
  # here (section_num, <real>) and (section_num, NA). The left_join below then
  # fanned every stratum for that section out into a coded copy and an NA
  # copy with byte-identical total_effort_hrs -- a silent doubling, not a
  # duplicate-detection false positive. NA is dropped per section whenever a
  # real crc_area is also present for that section; a section that is ONLY
  # ever NA (genuinely unmappable) is left as-is and still flows through as a
  # single NA row. The remaining multi_area check is therefore only tripped by
  # a section mapping to >1 REAL crc_area, which is a genuine ambiguity.
  section_crc_area <- interview_angler_types |>
    dplyr::distinct(section_num, crc_area) |>
    dplyr::group_by(section_num) |>
    dplyr::filter(!(is.na(crc_area) & any(!is.na(crc_area)))) |>
    dplyr::ungroup()

  multi_area <- section_crc_area |> dplyr::count(section_num) |> dplyr::filter(n > 1)
  if (nrow(multi_area) > 0) {
    cli::cli_alert_warning(
      "  Section{?s} {.val {multi_area$section_num}} map to >1 real crc_area; \\
       estimates will be duplicated across areas. Review."
    )
  }

  stratum_by_month <- dwg$days |>
    dplyr::select(event_date, period, day_type, dplyr::starts_with("open_section")) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("open_section"),
                        names_to = "section_temp", values_to = "is_open") |>
    dplyr::filter(is_open) |>
    dplyr::mutate(
      section_num = as.numeric(gsub("^.*_", "", section_temp)),
      year        = as.integer(format(event_date, "%Y")),
      month       = as.integer(format(event_date, "%m"))
    ) |>
    dplyr::count(section_num, period, day_type, year, month, name = "n_days_in_month")

  # PRORATION WEIGHTS MUST SUM TO 1 WITHIN EACH STRATUM. stratum_by_month and
  # prep_inputs_pe_days_total() both count open days by pivoting
  # open_section_*, filtering is_open, and counting -- the only difference is
  # the year/month grouping, so the weights are exact by construction. That
  # equality is the entire basis for prorating and would break silently if
  # either definition of "open" diverged, so it is checked. Below 1 leaks
  # effort/harvest; above 1 duplicates it.
  wt_check <- inputs_pe$days_total |>
    dplyr::left_join(stratum_by_month, by = c("section_num", "period", "day_type")) |>
    dplyr::mutate(w = n_days_in_month / N_days_open) |>
    dplyr::group_by(section_num, period, day_type) |>
    dplyr::summarize(w_sum = sum(w, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(abs(w_sum - 1) > 1e-8)

  if (nrow(wt_check) > 0) {
    skip_fishery(
      paste0("Month-proration weights do not sum to 1 in ", nrow(wt_check),
             " stratum/strata (range ", round(min(wt_check$w_sum), 4), "-",
             round(max(wt_check$w_sum), 4), "). stratum_by_month and days_total ",
             "disagree on which days are open; effort and harvest would be ",
             "leaked or duplicated across month boundaries."),
      stage = "prorate_weights"
    )
  }

  # THE shared effort table. Both outputs below derive from this one object, so
  # trips and harvest cannot disagree about how many angler-hours were fished.
  effort_monthly <- est_effort |>
    dplyr::left_join(section_crc_area, by = "section_num") |>
    dplyr::left_join(stratum_by_month, by = c("section_num", "period", "day_type")) |>
    dplyr::mutate(effort_prorated = est * (n_days_in_month / N_days_open)) |>
    dplyr::group_by(fishery_name, year, month, crc_area, angler_final) |>
    dplyr::summarize(
      total_effort_hrs = sum(effort_prorated, na.rm = TRUE),
      # na.rm = TRUE means an unsampled stratum contributes ZERO rather than
      # propagating NA, biasing the total downward with nothing in the output
      # saying by how much. These make the coverage explicit instead:
      # a missing dimension is not a zero.
      n_strata           = dplyr::n(),
      n_strata_estimated = sum(!is.na(effort_prorated)),
      .groups            = "drop"
    ) |>
    dplyr::mutate(prop_strata_estimated = n_strata_estimated / n_strata)


  # 5b. Trips ---------------------------------------------------------------

  trip_int <- run_stage("trip_length_interviews", {
    prep_trip_length_interviews(interview_angler_types, fishery_name)
  })

  # TRIP-LENGTH DONOR HIERARCHY. Restored from
  # analysis/archive/multi_fishery_trip_summary_superseded_20260817.R, which
  # this merge had dropped -- silently reverting to "no trip length -> row
  # dropped" for any month x area x angler-type cell with real estimated
  # effort but no completed interview that month (Yakima 2023-2025 was the
  # visible case: 13,455 effort-hrs with no total_trips_est). Downstream,
  # pst_fw_angler_trips_assembly.R also assumes every trips row carries a
  # trip_length_source label -- its absence here was a hard error, not just an
  # undercount.
  #
  # Effort is estimated for every open stratum, but interviews only happen on
  # sampled days, so a month-level trip length can be NA even though the
  # stratum has real effort. Rather than drop that effort, trip length falls
  # back to progressively coarser SEASON-LONG estimates from the SAME
  # fishery:
  #
  #   1  month        year x month x crc_area x angler_final   (primary)
  #   2  area_season  crc_area x angler_final, season-long
  #   3  type_season  angler_final, season-long (pools areas)
  #   4  fishery_season  season-long, all interviews (pools angler types)
  #   -  none         no completed interviews at all -> stays NA
  #
  # Each tier pools the underlying interviews directly rather than averaging
  # monthly means, so a month with three interviews isn't weighted the same as
  # one with three hundred. The assumption -- that mean trip length is roughly
  # stable across months within a fishery-season -- is weaker than assuming
  # rates travel between rivers or blocks (which the project has established
  # they do not), so every row records which tier produced its trip length
  # (trip_length_source) and monthly stability is measured separately (below)
  # so the assumption can be checked rather than asserted. Tier 4 pools bank
  # and boat anglers, whose trip lengths commonly differ; rows resting on it
  # are the weakest in the hierarchy.

  tl_base <- trip_int$interviews |>
    dplyr::mutate(
      year  = as.integer(format(event_date, "%Y")),
      month = as.integer(format(event_date, "%m"))
    )

  # Shared summary so every tier is computed identically.
  # person_count_final, NOT total_group_count: prep_dwg_interview_fishing_time()
  # sets person_count_final = total_group_count under "Standard" and
  # angler_count under "Drano", and it is the count every downstream effort
  # calculation uses. Reading total_group_count directly would report a group
  # size inconsistent with the effort math for Drano and return NA wherever
  # that column is unpopulated. No-op for Standard.
  #
  # na.rm on fishing_time: a single NA otherwise makes the whole cell NA, which
  # reads downstream as "no interviews" and is indistinguishable from a real
  # gap. n_trip_length_obs records how many times actually backed the mean.
  summarise_tl <- function(df, ...) {
    df |>
      dplyr::group_by(...) |>
      dplyr::summarize(
        n_completed_angler_trips = dplyr::n(),
        mean_trip_length         = mean(fishing_time, na.rm = TRUE),
        n_trip_length_obs        = sum(!is.na(fishing_time)),
        mean_group_size          = mean(person_count_final, na.rm = TRUE),
        sd                       = sd(fishing_time, na.rm = TRUE),
        .groups                  = "drop"
      ) |>
      # A cell where every fishing_time was NA yields NaN, which is not NA and
      # would be treated as a usable donor. Normalise before it propagates.
      dplyr::mutate(
        mean_trip_length = dplyr::if_else(is.finite(mean_trip_length),
                                          mean_trip_length, NA_real_)
      ) |>
      dplyr::filter(!is.na(mean_trip_length), mean_trip_length > 0)
  }

  tl_month <- summarise_tl(tl_base, year, month, crc_area, angler_final)
  tl_area  <- summarise_tl(tl_base, crc_area, angler_final)
  tl_type  <- summarise_tl(tl_base, angler_final)

  # Tier 4 is a single season-long value. Built as an explicit one-row tibble
  # so that a fishery with no usable interviews at all still yields NA columns
  # to coalesce against, rather than a zero-row table that would silently wipe
  # every effort row out of the output on the join.
  tl_fish_raw <- summarise_tl(tl_base)
  tl_fish <- if (nrow(tl_fish_raw) == 1) {
    tl_fish_raw |> dplyr::rename_with(~ paste0(.x, ".f"))
  } else {
    tibble::tibble(
      n_completed_angler_trips.f = NA_integer_,
      mean_trip_length.f         = NA_real_,
      n_trip_length_obs.f        = NA_integer_,
      mean_group_size.f          = NA_real_,
      sd.f                       = NA_real_
    )
  }

  # --- Stability diagnostic -------------------------------------------------
  # How much does monthly trip length actually vary within an area x type,
  # where months DO have data? This is the empirical support (or not) for
  # tiers 2-4. A high CV means the donor is a poor stand-in for the missing
  # months and the affected rows should be treated with caution.
  trip_length_stability <- tl_month |>
    dplyr::group_by(crc_area, angler_final) |>
    dplyr::summarize(
      n_months        = dplyr::n(),
      mean_of_months  = mean(mean_trip_length),
      sd_of_months    = sd(mean_trip_length),
      min_month       = min(mean_trip_length),
      max_month       = max(mean_trip_length),
      .groups         = "drop"
    ) |>
    dplyr::mutate(
      cv_across_months = dplyr::if_else(n_months > 1 & mean_of_months > 0,
                                        sd_of_months / mean_of_months, NA_real_),
      fishery_name     = .env$fishery_name
    ) |>
    dplyr::relocate(fishery_name)

  worst_cv <- suppressWarnings(max(trip_length_stability$cv_across_months, na.rm = TRUE))
  if (is.finite(worst_cv) && worst_cv > 0.30) {
    cli::cli_alert_warning(
      "  Monthly trip length varies by CV up to {round(worst_cv, 2)} within an \\
       area/type. Season-long donors are correspondingly weak here."
    )
  }

  # --- Apply the hierarchy --------------------------------------------------
  # Joined widest-to-narrowest with distinct suffixes, then coalesced in tier
  # order. Every tier's columns are carried so the chosen one can be labelled.

  trips_monthly <- effort_monthly |>
    dplyr::left_join(tl_month, by = c("year", "month", "crc_area", "angler_final"),
                     suffix = c("", ".m")) |>
    dplyr::left_join(tl_area,  by = c("crc_area", "angler_final"),
                     suffix = c("", ".a")) |>
    dplyr::left_join(tl_type,  by = "angler_final",
                     suffix = c("", ".t")) |>
    # tl_fish is guaranteed exactly one row (see above), so cross_join simply
    # broadcasts the season-long value onto every row.
    dplyr::cross_join(tl_fish) |>
    dplyr::mutate(
      trip_length_source = dplyr::case_when(
        !is.na(mean_trip_length)    ~ "month",
        !is.na(mean_trip_length.a)  ~ "area_season",
        !is.na(mean_trip_length.t)  ~ "type_season",
        !is.na(mean_trip_length.f)  ~ "fishery_season",
        TRUE                        ~ "none"
      ),
      mean_trip_length = dplyr::coalesce(mean_trip_length, mean_trip_length.a,
                                         mean_trip_length.t, mean_trip_length.f),
      mean_group_size  = dplyr::coalesce(mean_group_size, mean_group_size.a,
                                         mean_group_size.t, mean_group_size.f),
      sd               = dplyr::coalesce(sd, sd.a, sd.t, sd.f),
      # n_completed_angler_trips stays the MONTH-LEVEL count and is left NA on
      # donor rows. It documents the interview support for THIS cell; filling
      # it with the donor's pooled n would imply interviews that were never
      # collected in that month. The donor's own support is reported
      # separately as n_donor_obs.
      n_donor_obs = dplyr::case_when(
        trip_length_source == "month"          ~ n_trip_length_obs,
        trip_length_source == "area_season"    ~ n_trip_length_obs.a,
        trip_length_source == "type_season"    ~ n_trip_length_obs.t,
        trip_length_source == "fishery_season" ~ n_trip_length_obs.f,
        TRUE                                   ~ NA_integer_
      )
    ) |>
    dplyr::select(-dplyr::ends_with(".a"), -dplyr::ends_with(".t"),
                  -dplyr::ends_with(".f")) |>
    dplyr::mutate(
      # An estimated zero effort implies zero trips regardless of trip length,
      # so 0 / NA must not stay NA -- that reads as "unknown" when it is known
      # to be none. Guarded so a genuinely missing estimate never becomes a
      # confident zero.
      total_trips_est = dplyr::case_when(
        !is.na(total_effort_hrs) & total_effort_hrs == 0 ~ 0,
        TRUE ~ total_effort_hrs / mean_trip_length
      ),
      trip_expansion = dplyr::case_when(
        is.na(angler_final)                              ~ "no_effort_estimate",
        !is.na(total_effort_hrs) & total_effort_hrs == 0 ~ "zero_effort",
        trip_length_source == "month"                    ~ "estimated",
        trip_length_source != "none"                     ~ "estimated_donor",
        TRUE                                             ~ "no_trip_length"
      ),
      # Rows with no effort estimate at all have nothing to expand; a donor
      # trip length there would manufacture trips from an absent stratum.
      trip_length_source = dplyr::if_else(is.na(angler_final), "none",
                                          trip_length_source),
      # .env$ pin: without it dplyr resolves against the data mask first and
      # would silently pick up a same-named column if one is added upstream.
      study_design    = .env$study_design,
      prev_int_status = trip_int$qa$prev_int_status
    ) |>
    dplyr::rename(catch_area_code = crc_area) |>
    dplyr::relocate(study_design, .after = fishery_name) |>
    dplyr::relocate(trip_length_source, n_donor_obs, .after = mean_trip_length)

  n_gap <- sum(trips_monthly$trip_expansion == "no_trip_length")
  if (n_gap > 0) {
    hrs_gap <- sum(trips_monthly$total_effort_hrs[
      trips_monthly$trip_expansion == "no_trip_length"], na.rm = TRUE)
    cli::cli_alert_warning(
      "  {n_gap} row{?s} with effort but no trip length at ANY donor tier \\
       ({round(hrs_gap)} angler-hr{?s} unexpanded) -- no completed interviews \\
       anywhere in the season."
    )
  }

  n_donor <- sum(trips_monthly$trip_expansion == "estimated_donor")
  if (n_donor > 0) {
    hrs_donor <- sum(trips_monthly$total_effort_hrs[
      trips_monthly$trip_expansion == "estimated_donor"], na.rm = TRUE)
    cli::cli_alert_info(
      "  {n_donor} row{?s} expanded with a season-long donor trip length \\
       ({round(hrs_donor)} angler-hr{?s}) -- would previously have dropped out."
    )
  }


  # 5c. Harvest -------------------------------------------------------------

  harvest_monthly <- est_catch |>
    dplyr::left_join(section_crc_area, by = "section_num") |>
    dplyr::left_join(stratum_by_month, by = c("section_num", "period", "day_type")) |>
    dplyr::mutate(
      w                = n_days_in_month / N_days_open,
      harvest_prorated = est * w,
      var_prorated     = var * (w^2)          # see [N4]
    ) |>
    dplyr::group_by(fishery_name, year, month, crc_area, angler_final, est_cg) |>
    dplyr::summarize(
      harvest_est = sum(harvest_prorated, na.rm = TRUE),
      harvest_var = sum(var_prorated,     na.rm = TRUE),
      n_strata    = dplyr::n(),
      .groups     = "drop"
    ) |>
    dplyr::mutate(
      harvest_se = sqrt(harvest_var),
      harvest_cv = dplyr::if_else(harvest_est > 0, harvest_se / harvest_est, NA_real_),
      # Normal approximation at the aggregated level. The stratum-level t-based
      # l95/u95 from est_pe_catch() are not summable and are not carried forward.
      harvest_l95 = harvest_est - 1.96 * harvest_se,
      harvest_u95 = harvest_est + 1.96 * harvest_se
    ) |>
    dplyr::left_join(cg$cg_lut, by = c("fishery_name", "est_cg")) |>
    dplyr::mutate(study_design = .env$study_design) |>
    dplyr::relocate(study_design, .after = fishery_name) |>
    dplyr::relocate(catch_group, species_scope, is_total, .after = angler_final) |>
    # Same effort_monthly the trips table used -- identical by construction.
    dplyr::left_join(
      effort_monthly |>
        dplyr::select(fishery_name, year, month, crc_area, angler_final,
                      total_effort_hrs),
      by = c("fishery_name", "year", "month", "crc_area", "angler_final")
    ) |>
    dplyr::mutate(
      harvest_per_hr = dplyr::if_else(total_effort_hrs > 0,
                                      harvest_est / total_effort_hrs, NA_real_)
    ) |>
    dplyr::rename(catch_area_code = crc_area)


  # 5d. Per-fishery QA ------------------------------------------------------

  # Reported (unexpanded) harvest from interviews. The ratio expanded/reported
  # is the implied expansion factor; an implausible value flags a CPUE or
  # effort problem.
  reported <- dwg_summ$interview |>
    dplyr::group_by(est_cg) |>
    dplyr::summarize(
      reported_harvest  = sum(fish_count, na.rm = TRUE),
      n_interviews      = dplyr::n(),
      n_interviews_fish = sum(fish_count > 0, na.rm = TRUE),
      .groups           = "drop"
    )

  # Estimated angler-hours in strata with NO interview -- zero harvest by
  # construction, see [N3].
  effort_coverage <- inputs_pe$daily_cpue_catch_est |>
    dplyr::summarize(
      unsampled_effort_hrs = sum(
        ang_hrs_daily_mean_TI_expan[is.na(est_cg)], na.rm = TRUE),
      sampled_effort_hrs = sum(
        ang_hrs_daily_mean_TI_expan[!is.na(est_cg)], na.rm = TRUE
      ) / max(1, dplyr::n_distinct(stats::na.omit(inputs_pe$daily_cpue_catch_est$est_cg)))
    ) |>
    dplyr::mutate(
      prop_effort_unsampled = unsampled_effort_hrs /
        (unsampled_effort_hrs + sampled_effort_hrs)
    )

  qa <- reported |>
    dplyr::left_join(
      harvest_monthly |>
        dplyr::group_by(est_cg, catch_group, is_total) |>
        dplyr::summarize(expanded_harvest = sum(harvest_est, na.rm = TRUE),
                         .groups = "drop"),
      by = "est_cg"
    ) |>
    dplyr::mutate(
      fishery_name     = .env$fishery_name,
      study_design     = .env$study_design,
      expansion_factor = dplyr::if_else(
        reported_harvest > 0, expanded_harvest / reported_harvest, NA_real_),
      unsampled_effort_hrs  = effort_coverage$unsampled_effort_hrs,
      prop_effort_unsampled = effort_coverage$prop_effort_unsampled
    ) |>
    # Interview-selection and scope assumptions travel with the numbers rather
    # than living only in console output that scrolls past.
    dplyr::left_join(trip_int$qa, by = "fishery_name") |>
    dplyr::left_join(cg$scope,    by = "fishery_name") |>
    dplyr::relocate(fishery_name)

  cli::cli_alert_success(
    "Done: {.val {fishery_name}} ({nrow(cg$est_catch_groups) - 1} species + total)"
  )

  list(trips = trips_monthly, harvest = harvest_monthly, qa = qa,
       effort_index_fail_counts = fail_counts_diag,
       trip_length_stability    = trip_length_stability)
}


# 6. Batch run with skip/error isolation (weekly + monthly sensitivity) ------
#
# Every fishery resolves to exactly one of three outcomes, all recorded:
#   ok      - estimates produced
#   skipped - structurally not estimable; `reason` says why (expected)
#   error   - pipeline failure; `stage` says where (investigate)
#
# A fishery may succeed under one period_pe and be skipped under the other --
# the period x year collision check is period-specific -- so outcomes are
# tracked per period rather than assumed to carry over.

run_batch <- function(fisheries, period_pe) {
  cli::cli_alert_info("Running batch with period_pe = {.val {period_pe}} ...")

  purrr::map(fisheries, function(fn) {
    withCallingHandlers(
      tryCatch(
        {
          res <- process_fishery(fn, period_pe = period_pe)
          list(status = "ok", fishery_name = fn, pe_period = period_pe,
               study_design = resolve_study_design(fn),
               stage = NA_character_, reason = NA_character_, result = res)
        },
        fishery_skip = function(cnd) {
          cli::cli_alert_warning("Skipped [{.val {fn}}]: {conditionMessage(cnd)}")
          list(status = "skipped", fishery_name = fn, pe_period = period_pe,
               study_design = resolve_study_design(fn),
               stage  = cnd$stage  %||% "unknown",
               reason = cnd$reason %||% conditionMessage(cnd),
               result = NULL)
        },
        fishery_error = function(cnd) {
          cli::cli_alert_danger(
            "Failed [{.val {fn}}] at stage {.val {cnd$stage}}: {cnd$reason}")
          list(status = "error", fishery_name = fn, pe_period = period_pe,
               study_design = resolve_study_design(fn),
               stage  = cnd$stage  %||% "unknown",
               reason = cnd$reason %||% conditionMessage(cnd),
               result = NULL)
        },
        error = function(e) {   # backstop for anything outside run_stage()
          cli::cli_alert_danger("Failed [{.val {fn}}] (unstaged): {conditionMessage(e)}")
          list(status = "error", fishery_name = fn, pe_period = period_pe,
               study_design = resolve_study_design(fn),
               stage = "unstaged", reason = conditionMessage(e), result = NULL)
        }
      ),
      warning = function(w) {
        cli::cli_alert_info("  warning [{.val {fn}}]: {conditionMessage(w)}")
        invokeRestart("muffleWarning")
      }
    )
  })
}

results_week  <- run_batch(fisheries, "week")
results_month <- run_batch(fisheries, "month")


# --- Outcome ledger ----------------------------------------------------------

run_ledger <- dplyr::bind_rows(
  purrr::map_dfr(results_week,  ~ tibble::as_tibble(
    .x[c("fishery_name", "pe_period", "study_design", "status", "stage", "reason")])),
  purrr::map_dfr(results_month, ~ tibble::as_tibble(
    .x[c("fishery_name", "pe_period", "study_design", "status", "stage", "reason")]))
)

cli::cli_h2("Run outcomes")
run_ledger |>
  dplyr::count(pe_period, status) |>
  tidyr::pivot_wider(names_from = status, values_from = n, values_fill = 0) |>
  print()

if (any(run_ledger$status == "error")) {
  cli::cli_h3("Errors (pipeline failures, not data gaps -- investigate)")
  run_ledger |> dplyr::filter(status == "error") |>
    dplyr::count(stage, reason, sort = TRUE) |> print(n = 30)
}

if (any(run_ledger$status == "skipped")) {
  cli::cli_h3("Skips (expected -- fisheries lacking data required for estimation)")
  run_ledger |> dplyr::filter(status == "skipped") |>
    dplyr::count(stage, reason, sort = TRUE) |> print(n = 30)
}

dropped_entirely <- run_ledger |>
  dplyr::group_by(fishery_name) |>
  dplyr::filter(!any(status == "ok")) |>
  dplyr::slice(1) |> dplyr::ungroup() |>
  dplyr::select(fishery_name, status, stage, reason)

if (nrow(dropped_entirely) > 0) {
  cli::cli_alert_warning(
    "{nrow(dropped_entirely)} fishery/fisheries produced no estimates under \\
     either period_pe and are absent from the deliverable:"
  )
  print(dropped_entirely, n = 50)
}


# 7. Combine, verify, and save ------------------------------------------------

collect_batch <- function(results, label, element) {
  results |>
    purrr::keep(~ .x$status == "ok") |>
    purrr::map(~ .x$result[[element]]) |>
    dplyr::bind_rows() |>
    dplyr::mutate(pe_period = label)
}

trips_combined <- dplyr::bind_rows(
  collect_batch(results_week,  "week",  "trips"),
  collect_batch(results_month, "month", "trips")
)

harvest_combined <- dplyr::bind_rows(
  collect_batch(results_week,  "week",  "harvest"),
  collect_batch(results_month, "month", "harvest")
)

qa_combined <- dplyr::bind_rows(
  collect_batch(results_week,  "week",  "qa"),
  collect_batch(results_month, "month", "qa")
)

# Effort-index "fail" diagnostic (see EFFORT-SIDE DESIGN PREFLIGHT above).
# Computed before period_pe branches, so it is identical between the week and
# month runs for a given fishery -- collected from results_week only rather
# than duplicated via both batches and a meaningless pe_period tag.
effort_index_fail_counts <- collect_batch(results_week, "week", "effort_index_fail_counts") |>
  dplyr::select(-pe_period)

# Trip-length donor-hierarchy stability diagnostic (see [S4] in 5b above).
# Derived from trip_int$interviews, which does not vary with period_pe, so
# collected from results_week only for the same reason as the fail-count
# diagnostic above.
stability_combined <- collect_batch(results_week, "week", "trip_length_stability") |>
  dplyr::select(-pe_period)

# Filter out gamefish-primary fisheries from final output
gamefish_filter <- stringr::regex("(winter|summer) gamefish", ignore_case = TRUE)

trips_combined <- trips_combined |>
  dplyr::filter(!stringr::str_detect(fishery_name, gamefish_filter))

harvest_combined <- harvest_combined |>
  dplyr::filter(!stringr::str_detect(fishery_name, gamefish_filter))

qa_combined <- qa_combined |>
  dplyr::filter(!stringr::str_detect(fishery_name, gamefish_filter))

effort_index_fail_counts <- effort_index_fail_counts |>
  dplyr::filter(!stringr::str_detect(fishery_name, gamefish_filter))

stability_combined <- stability_combined |>
  dplyr::filter(!stringr::str_detect(fishery_name, gamefish_filter))

run_ledger <- run_ledger |>
  dplyr::filter(!stringr::str_detect(fishery_name, gamefish_filter))

# Fail loudly rather than writing an empty deliverable. An all-zero run almost
# always means a connectivity or credentials problem, and an empty CSV is far
# easier to hand off by mistake than to notice.
if (nrow(trips_combined) == 0 && nrow(harvest_combined) == 0) {
  cli::cli_abort(
    c("No fishery produced estimates; nothing written.",
      "i" = "Review the ledger above. If every fishery failed at \\
             {.val fetch_data}, check VPN / DB access before re-running.")
  )
}


# --- Trip expansion outcome ---------------------------------------------------
# The predecessor script wrote 392 rows with NA total_trips_est and said nothing
# about them. This makes a regression visible in the console rather than
# discovered downstream by a consultant.

cli::cli_h3("Trip expansion outcome")
trips_combined |>
  dplyr::group_by(trip_expansion) |>
  dplyr::summarize(n_rows = dplyr::n(),
                   angler_hrs = sum(total_effort_hrs, na.rm = TRUE),
                   .groups = "drop") |>
  dplyr::mutate(pct_hrs = round(100 * angler_hrs / sum(angler_hrs), 2)) |>
  print()

unexpanded <- trips_combined |>
  dplyr::filter(trip_expansion == "no_trip_length") |>
  dplyr::group_by(fishery_name) |>
  dplyr::summarize(n_rows = dplyr::n(),
                   angler_hrs = round(sum(total_effort_hrs, na.rm = TRUE)),
                   .groups = "drop") |>
  dplyr::arrange(dplyr::desc(angler_hrs))

if (nrow(unexpanded) > 0) {
  cli::cli_alert_warning(
    "{nrow(unexpanded)} fishery/fisheries have effort with no trip length at \\
     ANY donor tier -- no completed interviews anywhere in the season:"
  )
  print(unexpanded, n = 40)
} else {
  cli::cli_alert_success("Every row with effort has a trip length.")
}


# --- Donor trip lengths -------------------------------------------------------
# Effort expanded with a season-long donor rather than its own month's
# interviews. This effort would previously have dropped out of the deliverable
# entirely, so the donor is an improvement -- but it carries the assumption
# that trip length is stable across months within the fishery, and the
# consultant should be able to see which rows depend on it.

cli::cli_h3("Trip length provenance")
trips_combined |>
  dplyr::group_by(trip_length_source) |>
  dplyr::summarize(n_rows = dplyr::n(),
                   angler_hrs = round(sum(total_effort_hrs, na.rm = TRUE)),
                   trips = round(sum(total_trips_est, na.rm = TRUE)),
                   .groups = "drop") |>
  dplyr::mutate(pct_hrs = round(100 * angler_hrs / sum(angler_hrs), 2)) |>
  print()

donor_by_fishery <- trips_combined |>
  dplyr::filter(trip_length_source %in% c("area_season", "type_season",
                                          "fishery_season")) |>
  dplyr::group_by(fishery_name, trip_length_source) |>
  dplyr::summarize(n_rows = dplyr::n(),
                   angler_hrs = round(sum(total_effort_hrs, na.rm = TRUE)),
                   .groups = "drop") |>
  dplyr::arrange(dplyr::desc(angler_hrs))

if (nrow(donor_by_fishery) > 0) {
  cli::cli_alert_info(
    "{nrow(donor_by_fishery)} fishery/tier combination{?s} rely on a donor \\
     trip length:"
  )
  print(donor_by_fishery, n = 40)

  weak <- donor_by_fishery |> dplyr::filter(trip_length_source == "fishery_season")
  if (nrow(weak) > 0) {
    cli::cli_alert_warning(
      "{nrow(weak)} of those fall back to the fishery-wide mean, which pools \\
       bank and boat anglers. Treat those rows as weak."
    )
  }
}

# Empirical support for the donors: how stable is monthly trip length where
# months DO have interviews? A high CV means the season-long donor is a poor
# stand-in for the months that lack data.
unstable <- stability_combined |>
  dplyr::filter(!is.na(cv_across_months), cv_across_months > 0.30) |>
  dplyr::arrange(dplyr::desc(cv_across_months)) |>
  dplyr::distinct(fishery_name, catch_area_code = crc_area, angler_final,
                  n_months, cv_across_months, min_month, max_month)

if (nrow(unstable) > 0) {
  cli::cli_alert_warning(
    "{nrow(unstable)} fishery/area/type combination{?s} show monthly trip \\
     length varying at CV > 0.30. Donor-expanded rows in these are the least \\
     defensible in the deliverable:"
  )
  print(utils::head(unstable, 20))
}

assumed <- qa_combined |>
  dplyr::filter(prev_int_status %in% c("not_collected", "partial", "absent")) |>
  dplyr::distinct(fishery_name, prev_int_status, n_prev_int_na, n_interviews_in)

if (nrow(assumed) > 0) {
  cli::cli_alert_info(
    "{nrow(assumed)} fishery/fisheries depend on the previously_interviewed NA \\
     assumption; their trip estimates exist only because unrecorded \\
     re-interview status is read as 'first interview':"
  )
  print(assumed, n = 40)
}


# --- Scope exclusions actually applied ---------------------------------------

scope_excl <- qa_combined |>
  dplyr::filter(life_stages_excluded != "" | species_excluded != "") |>
  dplyr::distinct(fishery_name, species_excluded, life_stages_excluded,
                  n_records_stage_excl, life_stages_retained)

if (nrow(scope_excl) > 0) {
  cli::cli_h3("Scope exclusions applied ([S1] species, [S3] life stage)")
  print(scope_excl, n = 40)
}

smolt_hits <- qa_combined |>
  dplyr::filter(n_records_stage_excl > 0) |>
  dplyr::distinct(fishery_name, life_stages_excluded, n_records_stage_excl)

if (nrow(smolt_hits) > 0) {
  cli::cli_alert_warning(
    "{nrow(smolt_hits)} fishery/fisheries recorded KEPT fish at an excluded \\
     life stage. Excluded from the totals per [S3], but the records themselves \\
     are worth raising with the project lead:"
  )
  print(smolt_hits, n = 40)
}


# --- Additivity assertion (see [N1]) -----------------------------------------

additivity_check <- harvest_combined |>
  dplyr::group_by(fishery_name, pe_period, year, month, catch_area_code,
                  angler_final) |>
  dplyr::summarize(
    sum_species  = sum(harvest_est[!is_total], na.rm = TRUE),
    direct_total = sum(harvest_est[is_total],  na.rm = TRUE),
    .groups      = "drop"
  ) |>
  dplyr::mutate(
    abs_diff = abs(sum_species - direct_total),
    rel_diff = dplyr::if_else(direct_total > 0, abs_diff / direct_total, 0)
  )

bad_add <- additivity_check |> dplyr::filter(rel_diff > 1e-6, abs_diff > 1e-4)

if (nrow(bad_add) > 0) {
  cli::cli_alert_danger(
    "Additivity check FAILED for {nrow(bad_add)} stratum-month combination{?s}. \\
     Max relative difference: {round(max(bad_add$rel_diff), 4)}. Most likely a \\
     salmon species present in the data is missing from SALMON_SPECIES. \\
     Inspect `bad_add` before using the total-salmon estimates."
  )
  print(utils::head(dplyr::arrange(bad_add, dplyr::desc(rel_diff)), 10))
} else {
  cli::cli_alert_success(
    "Additivity check passed: species estimates sum to the direct total-salmon \\
     estimate across all strata."
  )
}


# --- Trips/harvest effort consistency ----------------------------------------
# Both tables now derive total_effort_hrs from the same effort_monthly object,
# so this should be exact. It is checked anyway: a mismatch would mean the two
# branches of process_fishery() diverged, which is precisely the class of bug
# that merging the scripts was meant to make impossible.

effort_check <- trips_combined |>
  dplyr::select(fishery_name, year, month, catch_area_code, angler_final,
                pe_period, effort_trips = total_effort_hrs) |>
  dplyr::inner_join(
    harvest_combined |>
      dplyr::distinct(fishery_name, year, month, catch_area_code, angler_final,
                      pe_period, effort_harvest = total_effort_hrs),
    by = c("fishery_name", "year", "month", "catch_area_code", "angler_final",
           "pe_period")
  ) |>
  dplyr::filter(!is.na(effort_trips), !is.na(effort_harvest),
                abs(effort_trips - effort_harvest) > 1e-9)

if (nrow(effort_check) > 0) {
  cli::cli_alert_danger(
    "{nrow(effort_check)} row{?s} where the trips and harvest tables disagree \\
     on total_effort_hrs. They are built from the same object, so this should \\
     be impossible -- investigate before using either output."
  )
  print(utils::head(effort_check, 10))
} else {
  cli::cli_alert_success("Trips and harvest effort agree exactly.")
}


# --- Coverage warning (see [N3]) ---------------------------------------------

low_coverage <- qa_combined |>
  dplyr::distinct(fishery_name, pe_period, prop_effort_unsampled) |>
  dplyr::filter(prop_effort_unsampled > 0.20) |>
  dplyr::arrange(dplyr::desc(prop_effort_unsampled))

if (nrow(low_coverage) > 0) {
  cli::cli_alert_warning(
    "{nrow(low_coverage)} fishery-period combination{?s} have >20% of estimated \\
     angler-hours in strata with no interviews. Harvest for these is biased low:"
  )
  print(utils::head(low_coverage, 15))
}


# --- Week vs. month sensitivity (see [N4]) -----------------------------------
# Divergence is expected and informative, not an error: weekly strata compute
# the daily mean over fewer sampled days than monthly strata, so the estimators
# genuinely differ. Large gaps point to months where a few sampled days carry
# the estimate.

period_comparison <- harvest_combined |>
  dplyr::select(fishery_name, study_design, year, month, catch_area_code,
                angler_final, catch_group, is_total, pe_period,
                harvest_est, harvest_cv) |>
  tidyr::pivot_wider(names_from = pe_period,
                     values_from = c(harvest_est, harvest_cv)) |>
  dplyr::mutate(
    harvest_diff = harvest_est_week - harvest_est_month,
    harvest_rel_diff = dplyr::if_else(
      !is.na(harvest_est_month) & harvest_est_month > 0,
      harvest_diff / harvest_est_month, NA_real_)
  )

trip_comparison <- trips_combined |>
  dplyr::select(fishery_name, year, month, catch_area_code, angler_final,
                pe_period, total_trips_est) |>
  tidyr::pivot_wider(names_from = pe_period, values_from = total_trips_est,
                     names_prefix = "trips_") |>
  dplyr::mutate(
    trips_diff = trips_week - trips_month,
    trips_rel_diff = dplyr::if_else(
      !is.na(trips_month) & trips_month > 0, trips_diff / trips_month, NA_real_)
  )

period_comparison_fishery <- period_comparison |>
  dplyr::filter(is_total) |>
  dplyr::group_by(fishery_name) |>
  dplyr::summarize(harvest_week  = sum(harvest_est_week,  na.rm = TRUE),
                   harvest_month = sum(harvest_est_month, na.rm = TRUE),
                   .groups = "drop") |>
  dplyr::left_join(
    trip_comparison |>
      dplyr::group_by(fishery_name) |>
      dplyr::summarize(trips_week  = sum(trips_week,  na.rm = TRUE),
                       trips_month = sum(trips_month, na.rm = TRUE),
                       .groups = "drop"),
    by = "fishery_name"
  ) |>
  dplyr::mutate(
    harvest_rel_diff = dplyr::if_else(
      harvest_month > 0, (harvest_week - harvest_month) / harvest_month, NA_real_),
    trips_rel_diff = dplyr::if_else(
      trips_month > 0, (trips_week - trips_month) / trips_month, NA_real_)
  ) |>
  dplyr::arrange(dplyr::desc(abs(harvest_rel_diff)))

cli::cli_h3("Week vs. month, total salmon harvest and trips, by fishery")
print(period_comparison_fishery, n = 60)

big_divergence <- period_comparison_fishery |>
  dplyr::filter(!is.na(harvest_rel_diff), abs(harvest_rel_diff) > 0.15)

if (nrow(big_divergence) > 0) {
  cli::cli_alert_warning(
    "{nrow(big_divergence)} fishery/fisheries differ by >15% between the weekly \\
     and monthly stratifications. Decide which period_pe is the deliverable \\
     rather than letting the choice default silently."
  )
}


# --- Save --------------------------------------------------------------------

out_dir <- here("analysis", "pst", "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(trips_combined,    file.path(out_dir, "multi_fishery_creel_trips.rds"))
saveRDS(harvest_combined,  file.path(out_dir, "multi_fishery_creel_harvest.rds"))
saveRDS(qa_combined,       file.path(out_dir, "multi_fishery_creel_qa.rds"))
saveRDS(run_ledger,        file.path(out_dir, "multi_fishery_creel_run_ledger.rds"))
saveRDS(period_comparison, file.path(out_dir, "multi_fishery_creel_week_vs_month.rds"))
saveRDS(effort_index_fail_counts,
        file.path(out_dir, "multi_fishery_creel_effort_index_fail_counts.rds"))
saveRDS(stability_combined,
        file.path(out_dir, "multi_fishery_creel_trip_length_stability.rds"))

readr::write_csv(trips_combined,    file.path(out_dir, "multi_fishery_creel_trips.csv"))
readr::write_csv(harvest_combined,  file.path(out_dir, "multi_fishery_creel_harvest.csv"))
readr::write_csv(qa_combined,       file.path(out_dir, "multi_fishery_creel_qa.csv"))
readr::write_csv(run_ledger,        file.path(out_dir, "multi_fishery_creel_run_ledger.csv"))
readr::write_csv(effort_index_fail_counts,
                 file.path(out_dir, "multi_fishery_creel_effort_index_fail_counts.csv"))
readr::write_csv(stability_combined,
                 file.path(out_dir, "multi_fishery_creel_trip_length_stability.csv"))
readr::write_csv(period_comparison, file.path(out_dir, "multi_fishery_creel_week_vs_month.csv"))

cli::cli_alert_info(
  "Coverage: {dplyr::n_distinct(trips_combined$fishery_name)} of \\
   {length(fisheries)} targeted fisheries produced estimates. The ledger \\
   accounts for the remainder."
)

cli::cli_alert_success(
  "Saved {nrow(trips_combined)} trip row{?s} and {nrow(harvest_combined)} \\
   harvest row{?s} across \\
   {dplyr::n_distinct(harvest_combined$fishery_name)} fisheries and \\
   {dplyr::n_distinct(harvest_combined$catch_group)} catch groups to \\
   {.path {out_dir}}"
)

if (nrow(effort_index_fail_counts) > 0) {
  cli::cli_h3("Unrecognised count_type values (angler_final = 'fail')")
  cli::cli_alert_warning(
    "{nrow(effort_index_fail_counts)} fishery/count_type combination{?s} had \\
     index counts that no branch of prep_dwg_effort_index() recognised -- \\
     see {.path multi_fishery_creel_effort_index_fail_counts.csv} for the \\
     exact count_type values to patch upstream."
  )
  print(effort_index_fail_counts, n = 30)
}
