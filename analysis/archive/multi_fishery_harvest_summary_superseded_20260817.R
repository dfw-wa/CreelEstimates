# ==============================================================================
# multi_fishery_harvest_summary.R
#
# Purpose:
#   Companion to analysis/multi_fishery_trip_summary.R. Produces multi-fishery
#   PE (point-estimate) *harvest* estimates for freshwater creel salmon
#   fisheries whose names contain a year between 2022 and 2025 (inclusive),
#   using the same time, area, and design strata as the effort/trip summary:
#
#       fishery_name x year x month x crc_area x angler_final
#
#   with an added catch-group dimension. Two levels of catch group are
#   produced for every fishery:
#
#     (a) species-specific harvest  - one group per salmon species present,
#         aggregated over ALL life stages and ALL fin-mark states, fate = Kept
#     (b) total salmon harvest      - a single group spanning all salmon
#         species present, same aggregation over life stage / fin mark
#
#   Catch-group naming follows the repo convention
#   paste0(species, "_", life_stage, "_", fin_mark, "_", fate), e.g.
#       Coho_Adult|Jack|UNK|NA_AD|UM|UNK|NA_Kept
#
# Usage:
#   Source interactively or run with Rscript from the repo root:
#     Rscript analysis/multi_fishery_harvest_summary.R
#   Requires VPN / internal DB access for creelutils::fetch_data() and
#   creelutils::fishery_lut(). The public Socrata endpoint used by
#   fetch_fishery_names() must also be reachable.
#
#   Output: analysis/outputs/multi_fishery_harvest_summary.rds  (+ .csv)
#           analysis/outputs/multi_fishery_harvest_qa.rds        (+ .csv)
#
# Study-design assumptions (applied uniformly to all fisheries):
#   Identical to multi_fishery_trip_summary.R; see that script's header for
#   rationale. Any fishery needing a non-default design must be reviewed.
#   - study_design                  = "Standard"
#   - boat_type_collapse            = "Yes"
#   - fish_location_determines_type = "No"
#   - angler_type_kayak_pontoon     = "bank"
#   - period_pe                     = "week" and "month" (sensitivity run)
#   - day_length                    = "night closure"
#   - min_fishing_time              = 0.5  (hours)
#
# ------------------------------------------------------------------------------
# METHOD NOTES  -- read before interpreting output
#
# [N1] TOTAL SALMON IS ESTIMATED DIRECTLY, NOT SUMMED.
#   est_pe_catch() point estimates are exactly additive across est_cg: every
#   est_cg is built from the same replicated interview set and the same open-day
#   set, so daily ratio-of-means CPUE sums linearly across groups, and
#   est = N_days_open * mean(daily catch estimate) inherits that additivity.
#   VARIANCES ARE NOT ADDITIVE -- species catches co-occur within interviews and
#   within strata, so summing species-level var() ignores covariance and will
#   generally understate (or, with negative covariance, overstate) uncertainty.
#   The "total salmon" catch group therefore pools species at the per-interview
#   level BEFORE the CPUE calculation, which propagates variance correctly.
#   Section 7 asserts additivity of the point estimates as a QA check; a failure
#   there means one of the assumptions above no longer holds.
#
# [N2] HARVEST = fate "Kept". Released fish are excluded. Set HARVEST_FATE to
#   "Kept|Released" to produce total encounters instead.
#
# [N3] CPUE USES ALL INTERVIEWS, NOT JUST COMPLETED TRIPS.
#   prep_inputs_pe_daily_cpue_catch_est() computes catch / fishing_time_total
#   over every interview passing min_fishing_time. This is intentional: a
#   ratio-of-means CPUE is unbiased for incomplete trips because catch-to-date
#   and hours-to-date are both truncated. This differs from the trip-length
#   calculation in multi_fishery_trip_summary.R, which correctly restricts to
#   trip_status == "Complete" & previously_interviewed == 0.
#
# [N4] STRATA WITH EFFORT BUT NO INTERVIEWS CONTRIBUTE ZERO HARVEST.
#   est_pe_catch() right-joins days_total and then drop_na(est_cg), so a
#   section x period x day_type x angler_final stratum with estimated effort but
#   no interview silently drops out rather than being imputed. Section 5b
#   quantifies the share of estimated angler-hours lost this way per fishery
#   (unsampled_effort_hrs / total_effort_hrs). Treat any fishery with a high
#   value as a downward-biased harvest estimate.
#
# [N5] WEEK-TO-MONTH PRORATION IS EXACT FOR POINT ESTIMATES, APPROXIMATE FOR
#      VARIANCE.
#   Both period_pe values are run, as in multi_fishery_trip_summary.R. Under
#   period_pe = "week" a stratum can straddle a month boundary, so each stratum
#   estimate is split by the fraction (w) of that stratum's open days falling in
#   each calendar month. Because prep_inputs_pe_days_total() and the local
#   stratum_by_month count open days identically, sum(w) = 1 within every
#   stratum and no harvest is lost or duplicated -- verified per fishery at
#   section 5a rather than assumed. Under period_pe = "month" the split is a
#   no-op (w = 1), so the same code path serves both runs.
#   What the split DOES assume is that daily harvest is uniform within a
#   stratum, since a week's estimate is allocated to months purely by day count.
#   day_type is already stratified, so this is a within-day-type assumption.
#   Variance is split by w^2, treating w as a fixed known constant; summing
#   variance across strata assumes between-stratum independence, which is
#   already the PE design assumption. The two runs are a genuine sensitivity,
#   not a reconciliation -- weekly and monthly strata produce different daily
#   means and different degrees of freedom, so the estimates are expected to
#   differ. Section 7 tabulates the divergence.
#
# [N6] EVERY FISHERY IS ACCOUNTED FOR, INCLUDING THE ONES THAT DROP OUT.
#   Fisheries lacking the data needed for estimation are bypassed rather than
#   halting the run, but never silently: each resolves to ok / skipped / error
#   in multi_fishery_harvest_run_ledger.csv, with the stage and reason. Skips
#   are expected data gaps; errors are pipeline problems worth investigating.
#   Check the ledger before treating the output as a complete fishery list --
#   a fishery absent from a consultant deliverable with no recorded reason is
#   indistinguishable from a fishery with zero harvest.
#
# [N7] STUDY DESIGN IS RESOLVED PER FISHERY, NOT FIXED GLOBALLY.
#   Fishery names matching DESIGN_RULES (currently "Drano Lake") run under the
#   "Drano" branches of the PE functions; everything else uses "Standard". The
#   two designs read different interview columns and interpret effort counts
#   differently, and the mismatch does not error -- it produces a wrong number
#   quietly. Every output row and ledger entry carries the study_design actually
#   used. Adding a design is a row in DESIGN_RULES plus its required columns in
#   REQUIRED_INTERVIEW_COLS; unsupported values abort rather than fall through.
#
# [N8] MODE (guided vs. private) IS NOT PRODUCED HERE.
#   angler_final resolves to bank / boat only. The guided/private split for the
#   PST consultant template comes from a separate interview attribute and is
#   handled downstream (see interview_proportions.qmd).
# ==============================================================================

# 0. Setup -------------------------------------------------------------------

library(tidyverse)
library(cli)
library(here)
library(creelutils)
library(timeDate)   # holiday calendar functions called inside prep_days()
library(suncalc)    # getSunlightTimes() called inside prep_days()
library(lubridate)  # days() called inside prep_days()
library(rlang)      # abort()/condition classes used by the skip handling below

# Source all PE pipeline functions
walk(list.files(here("R_functions"), full.names = TRUE), source)

# Study-design constants -- must stay in sync with multi_fishery_trip_summary.R
# Study design is resolved PER FISHERY, not fixed globally -- see section 2b.
# The remaining design parameters are shared across designs. Note that
# fish_location_determines_type is consumed only by the "Standard" branches;
# the "Drano" branches ignore it, so it is passed harmlessly either way.
BOAT_TYPE_COLLAPSE        <- "Yes"
FISH_LOC_DETERMINES_TYPE  <- "No"
ANGLER_TYPE_KAYAK_PONTOON <- "bank"
PERIOD_PE                 <- "week"
DAY_LENGTH                <- "night closure"
MIN_FISHING_TIME          <- 0.5

# Catch-group scope ----------------------------------------------------------
# Species eligible for the PST salmon rollup. Steelhead, trout, char, and
# other gamefish are excluded by design -- steelhead is explicitly out of scope
# for the PST economic valuation request. Matching is EXACT against the values
# present in dwg$catch (not regex), so a DB-side rename will show up in the
# "unmatched species" warning rather than silently dropping fish.
SALMON_SPECIES <- c("Chinook", "Coho", "Chum", "Pink", "Sockeye")

# Fate regex defining "harvest". See [N2].
HARVEST_FATE   <- "Kept"

# Label used in est_cg for the pooled across-species group.
TOTAL_LABEL    <- "TotalSalmon"


# 1. Fishery list from public Socrata endpoint --------------------------------

cli::cli_alert_info("Fetching fishery names from public Socrata endpoint...")
all_fishery_names <- creelutils::fetch_fishery_names()

year_extracted <- stringr::str_extract(all_fishery_names, "\\d{4}")
no_year_mask   <- is.na(year_extracted)

if (any(no_year_mask)) {
  cli::cli_alert_warning(
    "{sum(no_year_mask)} fishery name(s) with no extractable 4-digit year \\
     (will be excluded -- check for naming inconsistencies):"
  )
  purrr::walk(all_fishery_names[no_year_mask], ~ cli::cli_bullets(c("*" = .x)))
}

fisheries <- all_fishery_names[
  !no_year_mask & dplyr::between(as.integer(year_extracted), 2022L, 2025L)
]
cli::cli_alert_success(
  "Retained {length(fisheries)} fisheries with a year in 2022\u20132025."
)

# Known DB failures; only salmon fisheries retained for PST scope.
# Keep this list identical to multi_fishery_trip_summary.R so the harvest and
# effort outputs cover the same fishery set and can be joined without gaps.
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
     fetch_data() and be skipped:"
  )
  purrr::walk(public_not_in_db, ~ cli::cli_bullets(c("!" = .x)))

  if (length(public_not_in_db) > length(fisheries) * 0.5) {
    cli::cli_abort(
      c("Stopping: more than half the target fisheries \\
         ({length(public_not_in_db)}/{length(fisheries)}) are absent from \\
         the internal DB.",
        "i" = "The public and internal name lists may not correspond.")
    )
  }
}


# 2b. Per-fishery study design ------------------------------------------------
#
# The PE functions branch internally on study_design. "Standard" and "Drano" are
# not cosmetic variants -- they consume different interview columns and assign
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
# bank effort as (vehicle-derived total - trailer-derived boat) from counts that
# are actually direct angler and boat counts, and expands by an anglers-per-
# vehicle ratio that does not describe the data. The result is a plausible-
# looking number that is wrong, which is why the design is resolved from the
# fishery name rather than left at a global default.
#
# NOTE FOR RECONCILIATION: multi_fishery_trip_summary.R applies "Standard" to
# every fishery, including the four Drano Lake fisheries in its output. Those
# trip estimates should be regarded as suspect until that script is given the
# same resolution logic.

DESIGN_RULES <- tibble::tribble(
  ~pattern,      ~study_design,
  "Drano Lake",  "Drano"
)

DEFAULT_STUDY_DESIGN   <- "Standard"
SUPPORTED_DESIGNS      <- c("Standard", "Drano")

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
  } else if (nrow(hits) == 1) {
    hits$study_design[1]
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

# Interview columns each design depends on. Checked in preflight so a design
# applied to data that cannot support it produces a clear skip rather than a
# cryptic failure several functions downstream.
REQUIRED_INTERVIEW_COLS <- list(
  common   = c("interview_id", "event_date", "section_num", "crc_area",
               "trip_status", "previously_interviewed", "fishing_start_time",
               "interview_time", "vehicle_count", "boat_used", "boat_type"),
  Standard = c("total_group_count", "trailer_count", "fish_from_boat"),
  Drano    = c("angler_count")
)

# Report the resolved design for the whole target list up front, so an
# unexpected assignment is visible before a long batch run starts.
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
# Two failure modes are treated differently:
#
#   SKIP  - the fishery structurally cannot be estimated (no interviews, no
#           effort counts, no salmon harvest records, no usable coordinates).
#           Detected up front by preflight_fishery() and recorded with a reason.
#           These are expected outcomes, not bugs.
#
#   ERROR - something failed inside the pipeline. Caught per stage so the log
#           says WHERE it broke rather than surfacing a bare dplyr message from
#           four functions deep.
#
# Both are written to a ledger and saved alongside the estimates, so a fishery
# missing from the output is always traceable to a reason. Silent absence from
# a deliverable that goes to a consultant is the failure mode worth avoiding.

# Structured skip condition -- distinguishable from a genuine error by class.
skip_fishery <- function(reason, stage = "preflight") {
  rlang::abort(
    message = reason,
    class   = "fishery_skip",
    stage   = stage,
    reason  = reason
  )
}

# Tag an error with the stage it came from. `code` is lazily evaluated, so it
# is only forced inside the tryCatch.
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
# Runs immediately after fetch_data(), before any expensive computation. Returns
# a sanitized copy of the pieces prep_days() needs, or raises a skip.

preflight_fishery <- function(dwg, fishery_name, date_start, date_end,
                              study_design) {

  # (0) Interview schema must support the resolved design. Missing angler_count
  #     under "Drano", or missing total_group_count under "Standard", otherwise
  #     surfaces as an opaque error inside prep_dwg_interview_fishing_time().
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

  # (1) Dates must be resolvable and ordered. resolve_dates() returns NA when a
  #     fishery is absent from fishery_lut or has incomplete date fields.
  if (is.na(date_start) || is.na(date_end)) {
    skip_fishery("Unresolvable estimation dates (NA from resolve_dates).")
  }
  if (date_end < date_start) {
    skip_fishery(paste0("Estimation end date (", date_end,
                        ") precedes start date (", date_start, ")."))
  }

  # (2) Interviews are required for angler-type assignment, CPUE, the
  #     section -> crc_area lookup, and the section list itself.
  int_n <- dwg$interview |>
    dplyr::filter(dplyr::between(event_date, date_start, date_end)) |>
    nrow()
  if (int_n == 0) {
    skip_fishery("No interviews within the estimation window.")
  }

  # (3) Effort counts are required to expand CPUE to a total.
  eff_n <- dwg$effort |>
    dplyr::filter(dplyr::between(event_date, date_start, date_end)) |>
    nrow()
  if (eff_n == 0) {
    skip_fishery("No effort count records within the estimation window.")
  }

  # (4) Sections. prep_days() builds one open_section_* column per section; an
  #     empty vector produces a days table with no section columns, which then
  #     fails obscurely inside est_pe_effort()'s pivot_longer().
  sections <- sort(unique(stats::na.omit(
    dwg$interview |>
      dplyr::filter(dplyr::between(event_date, date_start, date_end)) |>
      dplyr::pull(section_num)
  )))
  if (length(sections) == 0) {
    skip_fishery("No non-missing section_num values on interviews.")
  }

  # (5) Coordinates. mean(numeric(0)) is NaN and mean(c(NA)) is NA; either is
  #     passed straight to suncalc::getSunlightTimes(), which fails or returns
  #     all-NA day lengths that propagate silently into zero effort.
  lat  <- suppressWarnings(mean(dwg$ll$centroid_lat, na.rm = TRUE))
  long <- suppressWarnings(mean(dwg$ll$centroid_lon, na.rm = TRUE))
  if (!is.finite(lat) || !is.finite(long)) {
    skip_fishery("No usable centroid coordinates in dwg$ll (needed for day length).")
  }

  # (6) Catch records. Absence of the table entirely is a skip; absence of
  #     salmon specifically is handled in build_est_catch_groups().
  if (is.null(dwg$catch) || nrow(dwg$catch) == 0) {
    skip_fishery("No catch records returned for this fishery.")
  }

  # (7) REPAIR, NOT SKIP: closures referencing a section that never appears in
  #     the interviews. prep_days() calls rows_update() with the closure table
  #     against a grid built only from `sections`, and rows_update() errors on
  #     any row it cannot match ("Attempting to update missing rows"). A closure
  #     on a section we are not estimating is harmless information, so it is
  #     dropped rather than treated as fatal. This is the most likely cause of
  #     prep_days() failures on long multi-year fisheries.
  closures <- dwg$closures
  if (is.null(closures)) {
    closures <- tibble::tibble(section_num = double(), event_date = as.Date(character()))
  }
  if (nrow(closures) > 0) {
    closures <- closures |>
      dplyr::mutate(
        event_date  = as.Date(event_date, format = "%Y-%m-%d"),
        section_num = as.double(section_num)
      )

    orphan <- closures |>
      dplyr::filter(
        dplyr::between(event_date, date_start, date_end),
        !section_num %in% sections
      )

    if (nrow(orphan) > 0) {
      cli::cli_alert_warning(
        "  Dropping {nrow(orphan)} closure record(s) for section(s) \\
         {.val {sort(unique(orphan$section_num))}}, which have no interviews \\
         and are therefore not being estimated."
      )
      closures <- closures |> dplyr::filter(section_num %in% sections)
    }

    # Duplicate closure rows also trip rows_update() ("rows must be unique").
    dup_n <- sum(duplicated(closures[, c("section_num", "event_date")]))
    if (dup_n > 0) {
      cli::cli_alert_warning(
        "  Collapsing {dup_n} duplicate closure record(s)."
      )
      closures <- closures |> dplyr::distinct(section_num, event_date, .keep_all = TRUE)
    }
  }

  list(sections = sections, lat = lat, long = long, closures = closures)
}

# --- Post-prep_days validation -----------------------------------------------
# prep_days() can return successfully but produce a days table that breaks the
# period-based joins downstream.

validate_days <- function(days, period_pe, fishery_name) {

  if (!any(grepl("^open_section_", names(days)))) {
    skip_fishery("prep_days() produced no open_section_* columns.", stage = "prep_days")
  }

  if (all(is.na(days$day_length))) {
    skip_fishery("All day_length values are NA.", stage = "prep_days")
  }

  # PERIOD x YEAR COLLISION. `period` is a bare calendar week (%W) or month
  # number with no year component, and every downstream join keys on period
  # alone. A fishery spanning more than one instance of the same week/month --
  # possible for the multi-season names like "2022-23" -- would silently POOL
  # those strata across years, inflating N_days_open and corrupting the
  # estimate. This is a correctness failure that produces no error, so it is
  # checked explicitly rather than trusted.
  collisions <- days |>
    dplyr::distinct(period, year) |>
    dplyr::count(period) |>
    dplyr::filter(n > 1)

  if (nrow(collisions) > 0) {
    skip_fishery(
      paste0(
        "period_pe='", period_pe, "': period value(s) ",
        paste(collisions$period, collapse = ", "),
        " occur in more than one calendar year. Downstream joins key on ",
        "period alone and would pool these strata across years. Needs a ",
        "year-qualified period before this fishery can be estimated."
      ),
      stage = "prep_days"
    )
  }

  invisible(TRUE)
}


# 4. Catch-group construction ------------------------------------------------
#
# prep_dwg_interview_catch() selects fish with str_detect() on each of species,
# life_stage, fin_mark, fate, and names the resulting group by pasting the four
# patterns together. To "catch all possible fish that were harvested per
# species" we need the life_stage and fin_mark patterns to span every value
# actually present in the data.
#
# Rather than hard-coding "Adult|Jack|UNK|NA", the alternation is BUILT FROM THE
# OBSERVED VALUES in dwg$catch for each fishery. Two reasons:
#   1. Nothing is silently dropped. A new life-stage or mark code appearing in
#      the DB is picked up automatically instead of falling outside a fixed
#      pattern and vanishing from the estimate.
#   2. The est_cg string becomes self-documenting -- it records exactly which
#      levels were aggregated for that fishery, which matters when comparing
#      fisheries whose coding practice differs.
# The trade-off is that est_cg strings are not identical across fisheries; the
# tidy `catch_group` / `species_scope` columns added below are what downstream
# code should group on.
#
# NOTE on the "empty alternative" idiom: a trailing "|" (e.g. "Adult|Jack|UNK|")
# makes the pattern match every string, including values you did not intend to
# include. That is a true wildcard, not an enumeration. It is avoided here so
# the pattern means what it says.

# Regex metacharacters that would break str_detect() if they appeared in a
# category value (e.g. a species recorded as "Bull Trout|Dolly Varden").
REGEX_METACHARS <- "[.\\\\+*?\\[\\]^$(){}=!<>|:-]"

build_est_catch_groups <- function(dwg_catch,
                                   fishery_name,
                                   species_keep = SALMON_SPECIES,
                                   fate_regex   = HARVEST_FATE) {

  # Mirror the NA -> "NA" coercion applied inside prep_dwg_interview_catch()
  # so the values we enumerate here are the values it will match against.
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
    cli::cli_alert_info(
      "  Non-PST species present and excluded: {.val {spp_unmatched}}"
    )
  }
  if (length(spp_present) == 0) {
    skip_fishery(
      paste0("No PST salmon species in catch records. Species present: ",
             paste(observed_species, collapse = ", ")),
      stage = "catch_groups"
    )
  }

  # Enumerate life-stage and fin-mark levels observed for the retained species
  # AND the harvest fate. Scoping to harvested fish keeps the pattern tight;
  # levels that only ever occur on released fish are irrelevant here.
  harvest_rows <- cat_std |>
    dplyr::filter(
      species %in% spp_present,
      stringr::str_detect(fate, fate_regex)
    )

  if (nrow(harvest_rows) == 0) {
    skip_fishery(
      paste0("Salmon present (", paste(spp_present, collapse = ", "),
             ") but no records with fate matching '", fate_regex,
             "' -- catch-and-release only, or fate not recorded."),
      stage = "catch_groups"
    )
  }

  ls_vals <- sort(unique(harvest_rows$life_stage))
  fm_vals <- sort(unique(harvest_rows$fin_mark))

  # Guard: a category value containing regex metacharacters would change the
  # meaning of the assembled alternation. Abort rather than produce a group
  # whose membership cannot be reasoned about.
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

  # (a) one row per species, aggregating over all life stages and fin marks
  species_groups <- tibble::tibble(
    species    = spp_present,
    life_stage = ls_regex,
    fin_mark   = fm_regex,
    fate       = fate_regex
  )

  # (b) one pooled row spanning all salmon species present. This is the
  #     variance-correct total -- see [N1].
  total_group <- tibble::tibble(
    species    = paste(spp_present, collapse = "|"),
    life_stage = ls_regex,
    fin_mark   = fm_regex,
    fate       = fate_regex
  )

  est_catch_groups <- dplyr::bind_rows(species_groups, total_group) |>
    as.data.frame(stringsAsFactors = FALSE)

  # Lookup translating the machine-generated est_cg into tidy labels for
  # grouping and reporting. est_cg is reconstructed exactly as
  # prep_dwg_interview_catch() does it: paste of the four fields, "_" collapsed.
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

  list(est_catch_groups = est_catch_groups, cg_lut = cg_lut)
}


# 5. Per-fishery processing function -----------------------------------------

process_fishery_harvest <- function(fishery_name, period_pe = PERIOD_PE) {

  study_design <- resolve_study_design(fishery_name)
  cli::cli_alert_info(
    "Processing: {.val {fishery_name}} [design: {.val {study_design}}]"
  )

  est_dates <- run_stage("resolve_dates", {
    resolve_dates(fishery_name, "", "")
  })
  date_start <- suppressWarnings(as.Date(est_dates$est_date_start))
  date_end   <- suppressWarnings(as.Date(est_dates$est_date_end))

  dwg <- run_stage("fetch_data", {
    creelutils::fetch_data(
      fishery_name = fishery_name,
      data_source  = "internal"
    )
  })

  # Validate inputs and repair the closure table before anything expensive runs
  pf <- run_stage("preflight", {
    preflight_fishery(dwg, fishery_name, date_start, date_end, study_design)
  })

  # Build catch groups from this fishery's own catch composition (section 4)
  cg <- run_stage("catch_groups", {
    build_est_catch_groups(dwg$catch, fishery_name)
  })

  params <- list(
    fishery_name     = fishery_name,
    project_name     = "multi_fishery_harvest",
    study_design     = study_design,
    est_catch_groups = cg$est_catch_groups
  )

  # Patch p_census values from fishery_manager into the effort table
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

  # prep_days() uses the preflight-sanitized sections, coordinates, and
  # closures rather than reading them raw off dwg.
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

  run_stage("validate_days", {
    validate_days(dwg$days, period_pe, fishery_name)
  })

  eff_filt <- dwg$effort |>
    dplyr::filter(dplyr::between(event_date, date_start, date_end))
  int_filt <- dwg$interview |>
    dplyr::filter(dplyr::between(event_date, date_start, date_end))

  # --- Interview wrangling pipeline ---

  interview_fishing_time <- run_stage("interview_fishing_time", {
    prep_dwg_interview_fishing_time(
      dwg_interview    = int_filt,
      min_fishing_time = MIN_FISHING_TIME,
      study_design     = study_design
    )
  })

  # min_fishing_time can eliminate every interview in a fishery of very short
  # trips; downstream failures would otherwise be cryptic.
  if (nrow(interview_fishing_time) == 0) {
    skip_fishery(
      paste0("No interviews survive the min_fishing_time filter (",
             MIN_FISHING_TIME, " hr)."),
      stage = "interview_fishing_time"
    )
  }

  interview_angler_types <- run_stage("interview_angler_types", {
    prep_dwg_interview_angler_types(
      interview_fishing_time        = interview_fishing_time,
      study_design                  = study_design,
      boat_type_collapse            = BOAT_TYPE_COLLAPSE,
      fish_location_determines_type = FISH_LOC_DETERMINES_TYPE,
      angler_type_kayak_pontoon     = ANGLER_TYPE_KAYAK_PONTOON
    )
  })

  # Attach catch. NOTE: this REPLICATES the interview table once per catch
  # group (n_groups x n_interviews rows) and drops crc_area. Both facts are
  # handled explicitly below.
  interview_plus_catch <- run_stage("interview_catch", {
    prep_dwg_interview_catch(
      params                      = params,
      interview_plus_angler_types = interview_angler_types,
      dwg_catch                   = dwg$catch,
      study_design                = study_design,
      est_catch_groups            = cg$est_catch_groups
    )
  })

  # --- Effort wrangling pipeline (unchanged from the effort script) ---

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

  # Index effort counts are what CPUE is expanded by; without them there is
  # nothing to scale to a total.
  if (nrow(effort_index_summ$index_angler_final) == 0) {
    skip_fishery("No index effort counts after angler-type assignment.",
                 stage = "effort_index")
  }

  # Effort-side preflight [A-i]. prep_dwg_effort_index() assigns "fail" to any
  # count_type its branch does not recognise. Under a wrong study design EVERY
  # count becomes "fail"; ang_per_object then joins to NA, drop_na() empties
  # ang_hrs_daily_mean, and the right_join(days_total) at the end of
  # est_pe_effort() emits one row per stratum with angler_final = NA and
  # est = NA -- an entire fishery unestimated but looking like output. This is
  # how four Drano years were published under the hard-coded "Standard" design.
  # The check above only catches a fully EMPTY index table; an all-"fail" table
  # is non-empty and slips past it.
  fail_counts <- effort_index_summ$index_angler_groups |>
    dplyr::filter(angler_final == "fail")

  if (nrow(fail_counts) > 0) {
    bad_types <- sort(unique(fail_counts$count_type))
    frac_fail <- nrow(fail_counts) / nrow(effort_index_summ$index_angler_groups)

    if (frac_fail > 0.5) {
      skip_fishery(
        paste0(
          round(100 * frac_fail), "% of index effort counts map to ",
          "angler_final = 'fail' under the '", study_design, "' design ",
          "(unrecognised count_type(s): ", paste(bad_types, collapse = ", "),
          "). Almost certainly the wrong study design -- check DESIGN_RULES."
        ),
        stage = "effort_index_design"
      )
    }
    cli::cli_alert_warning(
      "  {.val {fishery_name}}: {nrow(fail_counts)} index count{?s} \\
       ({round(100 * frac_fail, 1)}%) map to 'fail' \\
       (count_type{?s} {.val {bad_types}}); their effort is excluded."
    )
  }

  dwg_summ <- list(
    interview     = interview_plus_catch,   # replicated; required for CPUE
    effort_index  = effort_index_summ$index_angler_final,
    effort_census = effort_census_summ$census_angler_final,
    census_expan  = prep_dwg_census_expan(eff = dwg$effort, days = dwg$days)
  )

  # --- PE inputs list ---

  inputs_pe <- run_stage("pe_days_total", {
    list(days_total = prep_inputs_pe_days_total(days = dwg$days))
  })

  # DELIBERATE DEVIATION FROM fw_creel.Rmd:
  # prep_inputs_pe_int_ang_per_object() is fed the UNREPLICATED interview table.
  # Under "Standard" design it returns sum(person_count_final)/sum(vehicle_count),
  # a ratio of sums, so ang_per_object -- the only column consumed downstream --
  # is invariant to replication and the resulting effort estimates are identical
  # to multi_fishery_trip_summary.R either way. But the companion columns
  # person_count_total and object_count_total ARE inflated by the number of catch
  # groups when the replicated table is passed, which makes them useless for QA
  # and is a trap for any future code that reads them. Computing from the
  # unreplicated table costs nothing and keeps every column meaningful.
  inputs_pe$interview_ang_per_object <- run_stage("pe_ang_per_object", {
    prep_inputs_pe_int_ang_per_object(
      dwg_summarized = list(
        interview    = interview_angler_types,
        effort_index = dwg_summ$effort_index
      ),
      study_design   = study_design
    )
  })

  inputs_pe$paired_census_index_counts <- run_stage("pe_paired_counts", {
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
    skip_fishery("No daily angler-hour estimates produced.", stage = "pe_ang_hrs")
  }

  # Daily ratio-of-means CPUE and daily catch estimate, by est_cg. See [N3].
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

  # --- PE estimates ---

  est_effort <- run_stage("est_pe_effort", {
    est_pe_effort(
      params         = params,
      days           = dwg$days,
      pe_inputs_list = inputs_pe,
      sections       = pf$sections
    )
  })

  est_catch <- run_stage("est_pe_catch", {
    est_pe_catch(
      params         = params,
      dwg            = dwg,        # est_pe_catch() reads dwg$days internally
      days           = dwg$days,
      pe_inputs_list = inputs_pe
    )
  })

  # drop_na(est_cg) inside est_pe_catch() can empty the table entirely if no
  # stratum ever had both effort and an interview. See [N4].
  if (nrow(est_catch) == 0) {
    skip_fishery(
      "est_pe_catch() returned no rows -- no stratum has both estimated effort and interviews.",
      stage = "est_pe_catch"
    )
  }


  # 5a. Reconcile to calendar months and CRC area ---------------------------
  #
  # Identical proration logic to multi_fishery_trip_summary.R. est_pe_catch()
  # returns section_num x period x day_type x angler_final x est_cg; `period` is
  # a calendar week or month depending on period_pe. Each stratum estimate is
  # split across calendar months by the fraction of that stratum's open days in
  # each month (a no-op for monthly strata). Variance is split by the square of
  # that fraction -- see [N5]. crc_area joins via a section lookup derived from
  # interviews. Section-level breakdowns are not retained.

  section_crc_area <- interview_angler_types |>
    dplyr::distinct(section_num, crc_area)

  # Flag sections spanning multiple CRC areas -- the join below would duplicate
  # rows and inflate totals if this ever occurs.
  multi_area <- section_crc_area |>
    dplyr::count(section_num) |>
    dplyr::filter(n > 1)
  if (nrow(multi_area) > 0) {
    cli::cli_alert_warning(
      "  Section(s) {.val {multi_area$section_num}} map to >1 crc_area in \\
       {.val {fishery_name}}; harvest will be duplicated across areas. Review."
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

  # PRORATION WEIGHTS MUST SUM TO 1 WITHIN EACH STRATUM.
  # stratum_by_month and prep_inputs_pe_days_total() both count open days by
  # pivoting open_section_*, filtering is_open, and counting -- the only
  # difference is the year/month grouping. So the weights are exact by
  # construction, and the week-to-month split loses nothing. That equality is
  # the entire basis for prorating, and it would break silently if either
  # function's definition of "open" ever diverged, so it is checked rather than
  # assumed. A weight sum below 1 leaks harvest; above 1 duplicates it.
  wt_check <- inputs_pe$days_total |>
    dplyr::left_join(stratum_by_month, by = c("section_num", "period", "day_type")) |>
    dplyr::mutate(w = n_days_in_month / N_days_open) |>
    dplyr::group_by(section_num, period, day_type) |>
    dplyr::summarize(w_sum = sum(w, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(abs(w_sum - 1) > 1e-8)

  if (nrow(wt_check) > 0) {
    skip_fishery(
      paste0(
        "Month-proration weights do not sum to 1 in ", nrow(wt_check),
        " stratum/strata (range ", round(min(wt_check$w_sum), 4), "-",
        round(max(wt_check$w_sum), 4), "). stratum_by_month and days_total ",
        "disagree on which days are open; harvest would be leaked or duplicated ",
        "across month boundaries."
      ),
      stage = "prorate_weights"
    )
  }

  harvest_monthly <- est_catch |>
    dplyr::left_join(section_crc_area, by = "section_num") |>
    dplyr::left_join(stratum_by_month, by = c("section_num", "period", "day_type")) |>
    dplyr::mutate(
      w                 = n_days_in_month / N_days_open,
      harvest_prorated  = est * w,
      var_prorated      = var * (w^2)
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
      # Normal-approximation interval at the aggregated level. The stratum-level
      # l95/u95 from est_pe_catch() use a t quantile on stratum df and are not
      # summable, so they are intentionally not carried forward.
      harvest_l95 = harvest_est - 1.96 * harvest_se,
      harvest_u95 = harvest_est + 1.96 * harvest_se
    ) |>
    dplyr::left_join(cg$cg_lut, by = c("fishery_name", "est_cg")) |>
    # .env$ pin: without it, dplyr would resolve `study_design` against the
    # data mask first, silently picking up a same-named column if one is ever
    # added upstream.
    dplyr::mutate(study_design = .env$study_design) |>
    dplyr::relocate(study_design, .after = fishery_name) |>
    dplyr::relocate(catch_group, species_scope, is_total, .after = angler_final)

  # Effort, prorated the same way, so harvest can be expressed per angler-hour
  # and cross-checked against multi_fishery_trip_summary.R.
  effort_monthly <- est_effort |>
    dplyr::left_join(section_crc_area, by = "section_num") |>
    dplyr::left_join(stratum_by_month, by = c("section_num", "period", "day_type")) |>
    dplyr::mutate(effort_prorated = est * (n_days_in_month / N_days_open)) |>
    dplyr::group_by(fishery_name, year, month, crc_area, angler_final) |>
    dplyr::summarize(
      total_effort_hrs = sum(effort_prorated, na.rm = TRUE),
      .groups          = "drop"
    )

  harvest_monthly <- harvest_monthly |>
    dplyr::left_join(
      effort_monthly,
      by = c("fishery_name", "year", "month", "crc_area", "angler_final")
    ) |>
    dplyr::mutate(
      harvest_per_hr = dplyr::if_else(
        total_effort_hrs > 0, harvest_est / total_effort_hrs, NA_real_
      )
    ) |>
    dplyr::rename(catch_area_code = crc_area)


  # 5b. Per-fishery QA ------------------------------------------------------

  # (a) Reported (unexpanded) harvest from interviews, per catch group. The
  #     ratio expanded/reported is the implied expansion factor -- an
  #     implausible value flags a CPUE or effort problem.
  reported <- dwg_summ$interview |>
    dplyr::group_by(est_cg) |>
    dplyr::summarize(
      reported_harvest   = sum(fish_count, na.rm = TRUE),
      n_interviews       = dplyr::n(),
      n_interviews_fish  = sum(fish_count > 0, na.rm = TRUE),
      .groups            = "drop"
    )

  # (b) Estimated angler-hours in strata with NO interview for a given day.
  #     These contribute zero harvest by construction -- see [N4].
  effort_coverage <- inputs_pe$daily_cpue_catch_est |>
    dplyr::summarize(
      unsampled_effort_hrs = sum(
        ang_hrs_daily_mean_TI_expan[is.na(est_cg)], na.rm = TRUE
      ),
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
        reported_harvest > 0, expanded_harvest / reported_harvest, NA_real_
      ),
      unsampled_effort_hrs  = effort_coverage$unsampled_effort_hrs,
      prop_effort_unsampled = effort_coverage$prop_effort_unsampled
    ) |>
    dplyr::relocate(fishery_name)

  cli::cli_alert_success(
    "Done: {.val {fishery_name}} \\
     ({nrow(cg$est_catch_groups) - 1} species + total)"
  )

  list(harvest = harvest_monthly, qa = qa)
}


# 6. Batch run with skip/error isolation (weekly + monthly sensitivity) ------
#
# Every fishery resolves to exactly one of three outcomes, all recorded:
#   ok      - estimates produced
#   skipped - structurally not estimable; `reason` says why (expected)
#   error   - pipeline failure; `stage` says where (investigate)
#
# A fishery may succeed under one period_pe and be skipped under the other --
# the period x year collision check in validate_days() is period-specific --
# so outcomes are tracked per period rather than assumed to carry over.

run_batch <- function(fisheries, period_pe) {
  cli::cli_alert_info("Running harvest batch with period_pe = {.val {period_pe}} ...")

  purrr::map(fisheries, function(fn) {
    withCallingHandlers(
      tryCatch(
        {
          res <- process_fishery_harvest(fn, period_pe = period_pe)
          list(status = "ok", fishery_name = fn, pe_period = period_pe,
               study_design = resolve_study_design(fn),
               stage = NA_character_, reason = NA_character_, result = res)
        },
        fishery_skip = function(cnd) {
          cli::cli_alert_warning(
            "Skipped [{.val {fn}}]: {conditionMessage(cnd)}"
          )
          list(status = "skipped", fishery_name = fn, pe_period = period_pe,
               study_design = resolve_study_design(fn),
               stage  = cnd$stage %||% "unknown",
               reason = cnd$reason %||% conditionMessage(cnd),
               result = NULL)
        },
        fishery_error = function(cnd) {
          cli::cli_alert_danger(
            "Failed [{.val {fn}}] at stage {.val {cnd$stage}}: {cnd$reason}"
          )
          list(status = "error", fishery_name = fn, pe_period = period_pe,
               study_design = resolve_study_design(fn),
               stage  = cnd$stage %||% "unknown",
               reason = cnd$reason %||% conditionMessage(cnd),
               result = NULL)
        },
        # Backstop for anything raised outside a run_stage() wrapper
        error = function(e) {
          cli::cli_alert_danger(
            "Failed [{.val {fn}}] (unstaged): {conditionMessage(e)}"
          )
          list(status = "error", fishery_name = fn, pe_period = period_pe,
               study_design = resolve_study_design(fn),
               stage = "unstaged", reason = conditionMessage(e), result = NULL)
        }
      ),
      # Surface warnings with the fishery attached instead of letting them
      # accumulate anonymously until the end of the run.
      warning = function(w) {
        cli::cli_alert_info("  warning [{.val {fn}}]: {conditionMessage(w)}")
        invokeRestart("muffleWarning")
      }
    )
  })
}

results_week  <- run_batch(fisheries, "week")
results_month <- run_batch(fisheries, "month")

# --- Outcome ledger --------------------------------------------------------

run_ledger <- dplyr::bind_rows(
  purrr::map_dfr(results_week,  ~ tibble::as_tibble(.x[c("fishery_name", "pe_period", "study_design",
                                                         "status", "stage", "reason")])),
  purrr::map_dfr(results_month, ~ tibble::as_tibble(.x[c("fishery_name", "pe_period", "study_design",
                                                         "status", "stage", "reason")]))
)

ledger_summary <- run_ledger |>
  dplyr::count(pe_period, status) |>
  tidyr::pivot_wider(names_from = status, values_from = n, values_fill = 0)

cli::cli_h2("Run outcomes")
print(ledger_summary)

if (any(run_ledger$status == "error")) {
  cli::cli_h3("Errors (investigate -- these are pipeline failures, not data gaps)")
  run_ledger |>
    dplyr::filter(status == "error") |>
    dplyr::count(stage, reason, sort = TRUE) |>
    print(n = 30)
}

if (any(run_ledger$status == "skipped")) {
  cli::cli_h3("Skips (expected -- fisheries lacking data required for estimation)")
  run_ledger |>
    dplyr::filter(status == "skipped") |>
    dplyr::count(stage, reason, sort = TRUE) |>
    print(n = 30)
}

# Fisheries producing nothing under EITHER period -- the set genuinely absent
# from the deliverable.
dropped_entirely <- run_ledger |>
  dplyr::group_by(fishery_name) |>
  dplyr::filter(!any(status == "ok")) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::select(fishery_name, status, stage, reason)

if (nrow(dropped_entirely) > 0) {
  cli::cli_alert_warning(
    "{nrow(dropped_entirely)} fishery/fisheries produced no estimates under \\
     either period_pe. These are absent from the deliverable:"
  )
  print(dropped_entirely, n = 50)
  cli::cli_alert_info(
    "Fisheries whose status is {.val error} at a stage you do not intend to \\
     fix can be added to KNOWN_FAILED to skip the DB round-trip on re-runs."
  )
}


# 7. Combine, verify, and save ----------------------------------------------

collect_batch <- function(results, label, element) {
  results |>
    purrr::keep(~ .x$status == "ok") |>
    purrr::map(~ .x$result[[element]]) |>
    dplyr::bind_rows() |>
    dplyr::mutate(pe_period = label)
}

harvest_combined <- dplyr::bind_rows(
  collect_batch(results_week,  "week",  "harvest"),
  collect_batch(results_month, "month", "harvest")
)

qa_combined <- dplyr::bind_rows(
  collect_batch(results_week,  "week",  "qa"),
  collect_batch(results_month, "month", "qa")
)

# Fail loudly rather than writing an empty deliverable. An all-zero run almost
# always means a connectivity or credentials problem, not that no fishery had
# harvest, and an empty CSV is far easier to hand off by mistake than to notice.
if (nrow(harvest_combined) == 0) {
  cli::cli_abort(
    c("No fishery produced harvest estimates; nothing written.",
      "i" = "Review the outcome ledger above. If every fishery failed at \\
             {.val fetch_data}, check VPN / DB access before re-running.")
  )
}


# --- Additivity assertion (see [N1]) ---------------------------------------
# Sum of species-specific point estimates should equal the directly-estimated
# total-salmon point estimate within floating-point tolerance. A mismatch means
# a species present in the data fell outside SALMON_SPECIES, or the equal-day-
# set assumption underpinning additivity has broken.

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
    "Additivity check FAILED for {nrow(bad_add)} stratum-month combination(s). \\
     Max relative difference: {round(max(bad_add$rel_diff), 4)}. \\
     Most likely a salmon species present in the data is missing from \\
     SALMON_SPECIES. Inspect `bad_add` before using the total-salmon estimates."
  )
  print(utils::head(dplyr::arrange(bad_add, dplyr::desc(rel_diff)), 10))
} else {
  cli::cli_alert_success(
    "Additivity check passed: species estimates sum to the direct total-salmon \\
     estimate across all strata."
  )
}

# --- Coverage warning (see [N4]) -------------------------------------------
low_coverage <- qa_combined |>
  dplyr::distinct(fishery_name, pe_period, prop_effort_unsampled) |>
  dplyr::filter(prop_effort_unsampled > 0.20) |>
  dplyr::arrange(dplyr::desc(prop_effort_unsampled))

if (nrow(low_coverage) > 0) {
  cli::cli_alert_warning(
    "{nrow(low_coverage)} fishery-period combination(s) have >20% of estimated \\
     angler-hours in strata with no interviews. Harvest for these is biased low:"
  )
  print(utils::head(low_coverage, 15))
}


# --- Week vs. month sensitivity (see [N5]) ---------------------------------
# Both runs are stacked in harvest_combined via pe_period, matching the effort
# script's output shape. This block additionally pivots them side by side so the
# sensitivity is readable. Divergence is expected and informative, not an error:
# weekly strata compute the daily mean over fewer sampled days than monthly
# strata, so the two estimators genuinely differ. Large gaps point to months
# where a few sampled days are carrying the estimate.

period_comparison <- harvest_combined |>
  dplyr::select(fishery_name, study_design, year, month, catch_area_code,
                angler_final, catch_group, is_total, pe_period,
                harvest_est, harvest_cv) |>
  tidyr::pivot_wider(
    names_from  = pe_period,
    values_from = c(harvest_est, harvest_cv)
  ) |>
  dplyr::mutate(
    harvest_diff = harvest_est_week - harvest_est_month,
    harvest_rel_diff = dplyr::if_else(
      !is.na(harvest_est_month) & harvest_est_month > 0,
      harvest_diff / harvest_est_month,
      NA_real_
    )
  )

# Fishery-level rollup of total salmon only -- the number most likely to be
# quoted, and the one worth eyeballing before it leaves the building.
period_comparison_fishery <- period_comparison |>
  dplyr::filter(is_total) |>
  dplyr::group_by(fishery_name) |>
  dplyr::summarize(
    harvest_week  = sum(harvest_est_week,  na.rm = TRUE),
    harvest_month = sum(harvest_est_month, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    abs_diff = harvest_week - harvest_month,
    rel_diff = dplyr::if_else(harvest_month > 0, abs_diff / harvest_month, NA_real_)
  ) |>
  dplyr::arrange(dplyr::desc(abs(rel_diff)))

cli::cli_h3("Week vs. month total-salmon harvest, by fishery")
print(period_comparison_fishery, n = 60)

big_divergence <- period_comparison_fishery |>
  dplyr::filter(!is.na(rel_diff), abs(rel_diff) > 0.15)

if (nrow(big_divergence) > 0) {
  cli::cli_alert_warning(
    "{nrow(big_divergence)} fishery/fisheries differ by >15% between the weekly \\
     and monthly stratifications. Decide which period_pe is the deliverable \\
     rather than letting the choice default silently."
  )
}

# Rows present under one stratification but not the other -- usually a stratum
# that had interviews under one period grouping and none under the other.
asymmetric <- period_comparison |>
  dplyr::filter(xor(is.na(harvest_est_week), is.na(harvest_est_month)))

if (nrow(asymmetric) > 0) {
  cli::cli_alert_info(
    "{nrow(asymmetric)} fishery-month-stratum row(s) appear under only one \\
     period_pe. See period_comparison for detail."
  )
}


# --- Save ------------------------------------------------------------------

out_dir <- here("analysis", "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(harvest_combined, file.path(out_dir, "multi_fishery_harvest_summary.rds"))
saveRDS(qa_combined,      file.path(out_dir, "multi_fishery_harvest_qa.rds"))
saveRDS(run_ledger,       file.path(out_dir, "multi_fishery_harvest_run_ledger.rds"))
saveRDS(period_comparison, file.path(out_dir, "multi_fishery_harvest_week_vs_month.rds"))

readr::write_csv(harvest_combined, file.path(out_dir, "multi_fishery_harvest_summary.csv"))
readr::write_csv(qa_combined,      file.path(out_dir, "multi_fishery_harvest_qa.csv"))
readr::write_csv(run_ledger,       file.path(out_dir, "multi_fishery_harvest_run_ledger.csv"))
readr::write_csv(period_comparison,
                 file.path(out_dir, "multi_fishery_harvest_week_vs_month.csv"))

cli::cli_alert_info(
  "Coverage: {dplyr::n_distinct(harvest_combined$fishery_name)} of \\
   {length(fisheries)} targeted fisheries produced estimates. The ledger \\
   accounts for the remainder."
)

cli::cli_alert_success(
  "Saved {nrow(harvest_combined)} harvest rows across \\
   {dplyr::n_distinct(harvest_combined$fishery_name)} fisheries and \\
   {dplyr::n_distinct(harvest_combined$catch_group)} catch groups to \\
   {.path {out_dir}}"
)
