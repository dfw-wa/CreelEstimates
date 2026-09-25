# ==============================================================================
# parse_guide_logbook.R
#
# Purpose:
#   Summarize WDFW's mandatory guide logbook extract into CRC (catch record
#   code / catch area) x year x trip-type angler-trip counts, for use as a
#   MINIMUM-BOUND signal on the proportion of guided angler trips per CRC.
#
#   This data is a numerator only: it captures trips a *licensed* guide
#   logged, not the total population of guided + unguided angler trips on a
#   river. It has no denominator, and reporting compliance is imperfect (see
#   the `v_compliance` table in the source extract), so any proportion
#   computed downstream from this output (guided angler-trips here / total
#   angler-trips from some other source) is a floor on the true guided
#   share, not a point estimate. This script does not compute that
#   proportion itself - it only produces the clean numerator table.
#
# Source (input_files/pst/guide_logbook/guide_logbook_data_2026-09-02.rds):
#   A 22-table relational extract. Tables used here:
#     trip             - one row per logged trip: trip_type_id, trip_date,
#                         water_body_id, is_void
#     trip_type_lut     - 2 rows: "Guided" vs "Non-guided" (the latter is the
#                         guide's OWN trip - personal/comped use of their own
#                         license - not an unguided angler elsewhere)
#     trip_angler       - one row per angler on a trip, typed via
#                         trip_angler_type_lut: Paying / Comped / Crew
#     trip_angler_type_lut
#     water_body_lut    - has a `crc_code` column, the join key to
#                         input_files/pst/lookup_tables/crc_area_lut.csv
#                         (catch_area_code). Confirmed matching format
#                         (e.g. "561", "690", "536") against a sample pull.
#
# Angler-trip unit:
#   "Guided angler trips" here means trip_angler rows (one per client on a
#   trip), not `trip` rows (one per guide launch, which can carry multiple
#   anglers). Angler type "Crew" is excluded (guide staff, not clients);
#   "Paying" and "Comped" are both counted as clients.
#
# Filters applied before aggregation:
#   - is_void == FALSE (819 trips flagged void as of the 2026-09-02 extract)
#   - trip_date year in 2020:2026 (a couple of garbage rows carry years like
#     9 or 23 - clearly data entry errors, not real trips)
#   - water_body_id resolves to a non-NA crc_code (about 79% of non-void
#     trips do; the rest are water bodies never coded to a CRC in
#     water_body_lut and cannot be attributed - counted and reported as
#     "unresolved", not silently dropped)
#
# Output:
#   analysis/pst/outputs/07_guide_logbook/guide_logbook_angler_trips_by_crc_year.csv
#     One row per catch_area_code x calendar_year (Guided trips only -
#     Non-guided rows are dropped here since this table is meant to feed a
#     guided-share proportion), with the angler-trip count and the
#     crc_area_lut.csv name/region attached.
#   analysis/pst/outputs/07_guide_logbook/guide_logbook_angler_trips_by_crc_year_month.csv
#     Same, one row finer: catch_area_code x calendar_year x calendar_month.
#     Added 2026-09-11 because the annual table CANNOT be compared against
#     this repo's own creel-based estimates without it. The logbook is
#     calendar-year and species-agnostic; the creel estimates are
#     salmon-directed effort inside a season window (e.g. Hoh fall salmon
#     2023 surveyed September-October only). Comparing the two annually
#     charges a river's entire winter steelhead guide season against a
#     two-month fall salmon creel - on the Hoh that produced a "minimum"
#     guided count 2.8x our whole boat-trip estimate, which is a scope
#     mismatch, not a bound violation. Any comparison or floor built off
#     this data must restrict to the months the creel actually operated;
#     see guide_logbook_vs_interview_diagnostic.R (03_analysis).
#   analysis/pst/outputs/07_guide_logbook/guide_logbook_coverage_summary.csv
#     Row counts for each exclusion (void, bad year, unresolved CRC) so
#     downstream users can see how much of the raw extract the summary
#     table actually represents.
#
# Usage:
#   Rscript analysis/pst/02_ingest/parse_guide_logbook.R
#   Requires no DB access - reads only the committed RDS extract.
# ==============================================================================

library(tidyverse)
library(glue)
library(here)
library(cli)

RDS_PATH <- here("input_files", "pst", "guide_logbook",
                  "guide_logbook_data_2026-09-02.rds")
CRC_LUT_PATH <- here("input_files", "pst", "lookup_tables", "crc_area_lut.csv")

out_dir <- here("analysis", "pst", "outputs", "07_guide_logbook")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

OUT_SUMMARY_CSV  <- file.path(out_dir, "guide_logbook_angler_trips_by_crc_year.csv")
OUT_MONTH_CSV    <- file.path(out_dir, "guide_logbook_angler_trips_by_crc_year_month.csv")
OUT_COVERAGE_CSV <- file.path(out_dir, "guide_logbook_coverage_summary.csv")

YEARS_SCOPE <- 2020:2026

cli::cli_h1("Parsing guide logbook extract")

# 1. Load tables ---------------------------------------------------------------

gl <- readRDS(RDS_PATH)

trip           <- gl$trip
trip_type_lut  <- gl$trip_type_lut
water_body_lut <- gl$water_body_lut
trip_angler    <- gl$trip_angler
angler_type_lut <- gl$trip_angler_type_lut

# 2. Build a clean trip-level lookup (type + crc_code + year) -----------------

trip_clean <- trip |>
  mutate(
    trip_type_name = trip_type_lut$name[match(trip_type_id, trip_type_lut$id)],
    crc_code       = water_body_lut$crc_code[match(water_body_id, water_body_lut$id)],
    trip_year      = as.integer(format(trip_date, "%Y")),
    trip_month     = as.integer(format(trip_date, "%m"))
  ) |>
  select(id, trip_type_name, crc_code, trip_year, trip_month, is_void)

# 3. Coverage accounting - count exclusions before applying them --------------

n_total     <- nrow(trip_clean)
n_void      <- sum(trip_clean$is_void, na.rm = TRUE)
n_bad_year  <- sum(!trip_clean$is_void & !(trip_clean$trip_year %in% YEARS_SCOPE))
n_unresolved_crc <- sum(!trip_clean$is_void & trip_clean$trip_year %in% YEARS_SCOPE &
                          is.na(trip_clean$crc_code))
n_kept      <- n_total - n_void - n_bad_year - n_unresolved_crc

coverage <- tibble(
  stage = c("total_trips_in_extract", "excluded_void", "excluded_bad_year",
            "excluded_unresolved_crc", "trips_retained"),
  n_trips = c(n_total, n_void, n_bad_year, n_unresolved_crc, n_kept)
)

cli::cli_alert_info(
  "{n_kept} / {n_total} trips retained ({n_void} void, {n_bad_year} outside {min(YEARS_SCOPE)}-{max(YEARS_SCOPE)}, {n_unresolved_crc} unresolved CRC)."
)

# 4. Join angler-level rows to the clean trip table, excluding crew -----------
# "Angler trip" = client-level (Paying/Comped); Crew are guide staff, not
# clients, and are excluded so a multi-crew trip isn't counted as multiple
# client angler-trips.

trip_angler_clean <- trip_angler |>
  mutate(angler_type_name = angler_type_lut$name[match(trip_angler_type_id, angler_type_lut$id)]) |>
  filter(angler_type_name %in% c("Paying", "Comped")) |>
  left_join(trip_clean, by = c("trip_id" = "id")) |>
  filter(!is_void, trip_year %in% YEARS_SCOPE, !is.na(crc_code))

# 5. Aggregate to CRC x year x trip_type --------------------------------------

# A handful of catch_area_code values (559, 566, 618) legitimately cover two
# distinct named water bodies in crc_area_lut.csv (e.g. 559 = both "Lake
# Scanewa" and "Cowlitz R. above Cowlitz Falls Dam") - collapsed to one row
# per code here so the join below stays one-to-one, not many-to-many.
crc_lut <- suppressMessages(read_csv(CRC_LUT_PATH, show_col_types = FALSE)) |>
  mutate(catch_area_code = as.character(catch_area_code)) |>
  group_by(catch_area_code) |>
  summarise(
    catch_area_description = paste(unique(catch_area_description), collapse = " / "),
    catch_area_region      = paste(unique(catch_area_region), collapse = " / "),
    .groups = "drop"
  )

# Non-guided trips (the guide's own personal/comped use of their license,
# per trip_type_lut) are dropped here - this table is meant to feed a
# guided-share proportion, so only Guided rows are relevant.
angler_trips_by_crc_year <- trip_angler_clean |>
  filter(trip_type_name == "Guided") |>
  count(crc_code, trip_year, trip_type_name, name = "angler_trips") |>
  left_join(crc_lut, by = c("crc_code" = "catch_area_code")) |>
  arrange(crc_code, trip_year, trip_type_name)

# Month-level companion of the same table. Same filters, same Guided-only
# and Paying/Comped-only definitions - the ONLY difference is that
# trip_month survives the aggregation, so a downstream comparison can
# restrict to the months a creel survey actually ran. See this file's
# header for why an annual-only table is not comparable to the creel
# estimates at all.
angler_trips_by_crc_year_month <- trip_angler_clean |>
  filter(trip_type_name == "Guided") |>
  count(crc_code, trip_year, trip_month, trip_type_name, name = "angler_trips") |>
  left_join(crc_lut, by = c("crc_code" = "catch_area_code")) |>
  arrange(crc_code, trip_year, trip_month)

n_crc_unmatched <- angler_trips_by_crc_year |>
  filter(is.na(catch_area_description)) |>
  distinct(crc_code) |>
  nrow()

if (n_crc_unmatched > 0L) {
  cli::cli_alert_warning(
    "{n_crc_unmatched} crc_code value(s) in the logbook have no match in crc_area_lut.csv - kept in output with a blank name/region."
  )
}

# 5b. Salmon-directed guided angler trips -------------------------------------
# The table the assembly consumes as the guided-trip floor. The logbook has no
# target-species field, so a trip counts as salmon-directed by these rules
# (agreed 2026-09-25; sizes of each in guide_logbook_species_classification.R):
#
#   COUNT  salmon caught, with or without steelhead. A salmon+steelhead trip
#          counts only inside the salmon window - outside it the steelhead is
#          taken as the target.
#   COUNT  nothing caught, inside the salmon window.
#   WEIGHT steelhead caught (no salmon), inside the salmon window: counted at
#          p = P(salmon-directed | caught only steelhead), from creel interviews
#          with the same catch outcome (guided, CRC area x month where the
#          sample allows, else pooled) - salmon_directed_share.R writes it.
#          Replaces the earlier rule that counted these only on four rivers
#          picked by creel NAME, which was an artifact: every creel records all
#          catch. If that file is absent the old name rule is used and warned.
#   DROP   everything else: trout / warmwater / sturgeon / other catch; nothing
#          caught outside the window; steelhead outside the window.
#
# WEIGHT_NO_CATCH = TRUE would weight the no-catch-in-window trips the same way
# (P(salmon-directed | caught nothing)); off by default - counting them in full
# was the agreed rule. The proportion is written either way for sensitivity.
#
# Salmon window = months with CRC salmon harvest for that area (same year, or
# pooled years when CRC has not compiled that year yet), EXCEPT where creel
# interviews show guided effort is not salmon-directed across the open season:
# the Cowlitz below Mayfield (561) is open year-round, but guided interviews
# there name salmon only in Sep-Nov (93% of guided Cowlitz effort is steelhead
# - guided_target_mix_by_river.R).

SALMON_SPECIES <- c("Chinook", "Coho", "Chum", "Pink", "Sockeye")
# Legacy fallback only (used when the conditional-proportion file is missing).
STEELHEAD_INCLUSIVE_RIVERS <- c("Drano Lake", "Skykomish", "Stillaguamish", "Wallace")
WEIGHT_NO_CATCH <- FALSE
COND_PROP_CSV <- here("analysis", "pst", "outputs", "04_interview_proportions",
                      "logbook_catch_conditional_salmon_target_prop.csv")
SALMON_WINDOW_OVERRIDE <- tibble(crc_code = "561", trip_month = 9:11)

CRC_HARVEST_FILES <- here("analysis", "pst", "outputs", "01_crc_harvest",
                          c("crc_freshwater_harvest_2010_2024_tidy.csv",
                            "crc_freshwater_harvest_final_creel_subs_tidy.csv"))
CROSSWALK_PATH <- here("input_files", "pst", "lookup_tables",
                       "pst_river_block_crosswalk.csv")
OUT_SALMON_CSV <- file.path(out_dir,
                            "guide_logbook_salmon_angler_trips_by_crc_year_month.csv")

if (!any(file.exists(CRC_HARVEST_FILES))) {
  stop("No CRC freshwater harvest file in analysis/pst/outputs/01_crc_harvest/ - ",
       "run parse_crc_freshwater_harvest.R first; the salmon window comes from it.",
       call. = FALSE)
}

crc_months <- purrr::map_dfr(CRC_HARVEST_FILES[file.exists(CRC_HARVEST_FILES)],
                             ~ suppressMessages(read_csv(.x, show_col_types = FALSE))) |>
  filter(!is.na(calendar_month), harvest_count > 0) |>
  transmute(crc_code = as.character(stream_code),
            trip_year = as.integer(calendar_year),
            trip_month = as.integer(calendar_month)) |>
  distinct()
crc_years <- crc_months |> distinct(crc_code, trip_year)
crc_months_pooled <- crc_months |> distinct(crc_code, trip_month)

sthd_codes <- read_csv(CROSSWALK_PATH, show_col_types = FALSE) |>
  filter(river_label %in% STEELHEAD_INCLUSIVE_RIVERS, !is.na(crc_areas)) |>
  pull(crc_areas) |> strsplit("\\|") |> unlist() |> unique()

species_name <- gl$species_lut$name[match(gl$encounter$species_id, gl$species_lut$id)]
trip_catch <- gl$encounter |>
  mutate(species = species_name, fish_count = as.numeric(fish_count)) |>
  filter(fish_count > 0) |>
  group_by(trip_id) |>
  summarise(n_salmon    = sum(fish_count[species %in% SALMON_SPECIES]),
            n_steelhead = sum(fish_count[species %in% "Steelhead"]),
            n_other     = sum(fish_count[!species %in% c(SALMON_SPECIES, "Steelhead")]),
            .groups = "drop")

guided_trips <- trip_clean |>
  filter(!is_void, trip_type_name == "Guided", trip_year %in% YEARS_SCOPE,
         !is.na(crc_code)) |>
  mutate(crc_code = as.character(crc_code)) |>
  left_join(trip_catch, by = c("id" = "trip_id")) |>
  mutate(across(c(n_salmon, n_steelhead, n_other), ~ coalesce(.x, 0)))

in_window <- function(code, yr, mo) {
  k      <- paste(code, yr, mo)
  has_yr <- paste(code, yr) %in% paste(crc_years$crc_code, crc_years$trip_year)
  win <- if_else(has_yr,
                 k %in% paste(crc_months$crc_code, crc_months$trip_year, crc_months$trip_month),
                 paste(code, mo) %in% paste(crc_months_pooled$crc_code, crc_months_pooled$trip_month))
  override <- code %in% SALMON_WINDOW_OVERRIDE$crc_code
  if_else(override,
          paste(code, mo) %in% paste(SALMON_WINDOW_OVERRIDE$crc_code,
                                     SALMON_WINDOW_OVERRIDE$trip_month),
          win)
}

guided_trips <- guided_trips |>
  mutate(
    salmon_window = in_window(crc_code, trip_year, trip_month),
    salmon_rule = case_when(
      n_salmon > 0 & n_steelhead == 0                       ~ "salmon_caught",
      n_salmon > 0 & salmon_window                          ~ "salmon_and_steelhead_in_window",
      n_salmon > 0                                          ~ "DROP_salmon_and_steelhead_off_window",
      n_steelhead > 0 & salmon_window                       ~ "steelhead_only_in_window",
      n_steelhead > 0                                       ~ "DROP_steelhead_off_window",
      n_other > 0                                           ~ "DROP_other_species",
      salmon_window                                         ~ "no_catch_in_window",
      TRUE                                                  ~ "DROP_no_catch_off_window"
    )
  )

# Per-trip weight: 1 for counted rules, 0 for DROP, the creel conditional
# proportion for steelhead-only-in-window (and no-catch, if WEIGHT_NO_CATCH).
if (file.exists(COND_PROP_CSV)) {
  cond <- read_csv(COND_PROP_CSV, show_col_types = FALSE,
                   col_types = cols(crc_code = "c")) |>
    select(crc_code, month, condition, p_salmon_target, prop_tier)
  cond_look <- function(code, mo, cond_name) {
    k <- tibble(crc_code = code, month = as.integer(mo), condition = cond_name)
    own <- k |> left_join(cond, by = c("crc_code", "month", "condition"))
    pooled <- k |> mutate(crc_code = "*") |>
      left_join(cond, by = c("crc_code", "month", "condition"))
    list(p    = coalesce(own$p_salmon_target, pooled$p_salmon_target),
         tier = coalesce(own$prop_tier, pooled$prop_tier))
  }
  sh <- cond_look(guided_trips$crc_code, guided_trips$trip_month, "steelhead_only")
  nc <- cond_look(guided_trips$crc_code, guided_trips$trip_month, "no_catch")
  guided_trips <- guided_trips |>
    mutate(p_sthd = sh$p, p_sthd_tier = sh$tier, p_nocatch = nc$p)
  n_miss <- sum(guided_trips$salmon_rule == "steelhead_only_in_window" &
                  is.na(guided_trips$p_sthd))
  if (n_miss > 0) cli::cli_alert_warning(
    "{n_miss} steelhead-only in-window trips have no conditional proportion (not even pooled) - weighted 0.")
} else {
  cli::cli_alert_warning(paste(
    "{COND_PROP_CSV} not found - steelhead-only in-window trips fall back to the",
    "legacy name-based rule (counted only on {toString(STEELHEAD_INCLUSIVE_RIVERS)}).",
    "Run analysis/pst/03_analysis/salmon_directed_share.R first."))
  guided_trips <- guided_trips |>
    mutate(p_sthd = if_else(crc_code %in% sthd_codes, 1, 0),
           p_sthd_tier = "legacy name-based rule", p_nocatch = NA_real_)
}

guided_trips <- guided_trips |>
  mutate(weight = case_when(
    str_starts(salmon_rule, "DROP") ~ 0,
    salmon_rule == "steelhead_only_in_window" ~ coalesce(p_sthd, 0),
    salmon_rule == "no_catch_in_window" & WEIGHT_NO_CATCH ~ coalesce(p_nocatch, 1),
    TRUE ~ 1))

cli::cli_h2("Steelhead-only in-window guided trips: creel targeting weight")
print(as.data.frame(guided_trips |>
  filter(salmon_rule == "steelhead_only_in_window") |>
  group_by(p_sthd_tier) |>
  summarise(trips = n(), mean_weight = round(mean(weight), 3), .groups = "drop")),
  row.names = FALSE)

salmon_anglers <- trip_angler_clean |>
  filter(trip_type_name == "Guided") |>
  mutate(crc_code = as.character(crc_code)) |>
  inner_join(guided_trips |> select(id, salmon_rule, weight), by = c("trip_id" = "id"))

cli::cli_h2("Guided angler-trips by salmon rule")
print(as.data.frame(salmon_anglers |>
                      group_by(salmon_rule) |>
                      summarise(angler_trips = n(), counted = round(sum(weight), 1),
                                .groups = "drop") |>
                      mutate(pct = round(100 * angler_trips / sum(angler_trips), 1))),
      row.names = FALSE)

# Counted (weighted) angler-trips per rule, plus raw steelhead-only counts in
# and out of the window so downstream checks can see what was weighted away.
counted_wide <- salmon_anglers |>
  filter(!str_starts(salmon_rule, "DROP")) |>
  group_by(crc_code, trip_year, trip_month, salmon_rule) |>
  summarise(n = sum(weight), .groups = "drop") |>
  pivot_wider(names_from = salmon_rule, values_from = n, values_fill = 0)
raw_sthd <- salmon_anglers |>
  filter(salmon_rule %in% c("steelhead_only_in_window", "DROP_steelhead_off_window")) |>
  mutate(salmon_rule = if_else(salmon_rule == "steelhead_only_in_window",
                               "steelhead_only_in_window_raw",
                               "steelhead_only_off_window_raw")) |>
  count(crc_code, trip_year, trip_month, salmon_rule, name = "n") |>
  pivot_wider(names_from = salmon_rule, values_from = n, values_fill = 0)
salmon_by_crc_year_month <- counted_wide |>
  mutate(angler_trips = rowSums(across(-c(crc_code, trip_year, trip_month)))) |>
  full_join(raw_sthd, by = c("crc_code", "trip_year", "trip_month")) |>
  mutate(across(-c(crc_code, trip_year, trip_month), ~ coalesce(.x, 0))) |>
  left_join(crc_lut, by = c("crc_code" = "catch_area_code")) |>
  arrange(crc_code, trip_year, trip_month)

# 6. Write output --------------------------------------------------------------

cli::cli_h1("Writing output")

readr::write_csv(angler_trips_by_crc_year, OUT_SUMMARY_CSV)
readr::write_csv(angler_trips_by_crc_year_month, OUT_MONTH_CSV)
readr::write_csv(coverage, OUT_COVERAGE_CSV)
readr::write_csv(salmon_by_crc_year_month, OUT_SALMON_CSV)

cli::cli_alert_success("Wrote {nrow(angler_trips_by_crc_year)} rows to {OUT_SUMMARY_CSV}")
cli::cli_alert_success("Wrote {nrow(angler_trips_by_crc_year_month)} rows to {OUT_MONTH_CSV}")
cli::cli_alert_success("Wrote {nrow(coverage)} rows to {OUT_COVERAGE_CSV}")
cli::cli_alert_success("Wrote {nrow(salmon_by_crc_year_month)} rows to {OUT_SALMON_CSV}")
