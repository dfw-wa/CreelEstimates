# ==============================================================================
# guide_logbook_species_classification.R
#
# Purpose:
#   Size the assumption before making it. The guide logbook has NO target
#   species field - only what was CAUGHT (`encounter`, resolved via
#   `species_lut`). So "was this a salmon trip?" has to be inferred, and this
#   script measures how much of the logbook rests on inference rather than
#   evidence, BEFORE any of it is wired into the guided proportion.
#
#   Two axes, crossed:
#
#     CATCH   salmon_catch      >=1 Pacific salmon landed/released
#             other_catch_only  encounters, but only steelhead/trout/other
#             no_catch          no encounter recorded (skunked, or unreported)
#
#     MONTH   salmon_month      CRC reports salmon harvest in that area+month
#             non_salmon_month  CRC reports none
#             no_crc_reference  no CRC record for that area-year at all
#
#   giving the cells that matter:
#
#     salmon_catch        + any month        -> VERIFIED salmon trip
#     other_catch_only    + non_salmon_month -> CLEAR exclude (the easy call)
#     other_catch_only    + salmon_month     -> AMBIGUOUS (trout in season)
#     no_catch            + salmon_month     -> AMBIGUOUS (skunked in season)
#     no_catch            + non_salmon_month -> likely exclude
#     salmon_catch        + non_salmon_month -> verified, but the CRC month
#                                               reference disagrees - worth a look
#
#   Everything is reported in ANGLER-trips (Paying/Comped `trip_angler` rows),
#   not trips, because angler-trips is the quantity the guided proportion is
#   built from. Trip counts are shown alongside.
#
# Scope filters match parse_guide_logbook.R exactly, so the totals here
# reconcile with guide_logbook_angler_trips_by_crc_year.csv: Guided trips only,
# non-void, trip_year in 2020:2026, crc_code resolvable.
#
# NOT a producer - writes a per-trip classification for reuse, but nothing
# downstream reads it yet. Nothing in the pipeline changes until the magnitudes
# below are judged acceptable.
#
# Inputs:
#   input_files/pst/guide_logbook/guide_logbook_data_2026-09-02.rds  (not committed)
#   input_files/pst/lookup_tables/crc_area_lut.csv
#   analysis/pst/outputs/01_crc_harvest/crc_freshwater_harvest_2010_2024_tidy.csv
#     (+ ..._final_creel_subs_tidy.csv if present - months are unioned)
#
# Outputs:
#   analysis/pst/outputs/08_guide_logbook_diagnostic/
#     guide_logbook_trip_classification.csv       one row per guided trip
#     guide_logbook_classification_summary.csv    the cross-tab
#
# Usage:
#   Rscript analysis/pst/03_analysis/guide_logbook_species_classification.R
# ==============================================================================

suppressMessages({
  library(tidyverse)
  library(here)
  library(glue)
})

options(width = 200)

RDS_PATH  <- here("input_files", "pst", "guide_logbook",
                  "guide_logbook_data_2026-09-02.rds")
CRC_LUT   <- here("input_files", "pst", "lookup_tables", "crc_area_lut.csv")
CRC_DIR   <- here("analysis", "pst", "outputs", "01_crc_harvest")
OUT_DIR   <- here("analysis", "pst", "outputs", "08_guide_logbook_diagnostic")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

YEARS_SCOPE <- 2020:2026

# Pacific salmon as CRC counts them. Steelhead is deliberately NOT here - it is
# the species this whole exercise exists to separate out. Atlantic Salmon is
# excluded too: an escapee encounter is not evidence of a salmon-directed trip.
SALMON_SPECIES <- c("Chinook", "Coho", "Chum", "Pink", "Sockeye")

# The "ambiguous" cell - non-salmon catch in a month salmon were open - is only
# ambiguous if you ignore WHICH non-salmon fish were caught. A trip that boated
# eight walleye is a walleye trip whether or not salmon were open. Grouping the
# non-salmon catch turns most of that cell back into evidence.
#
# Rainbow Trout (74,085 fish) and Walleye (53,430) are the two most-caught
# species in the whole logbook, both above Chinook (38,830) - this logbook is
# dominated by non-salmon guiding, so defaulting ambiguity toward "salmon"
# pushes hard in the wrong direction.
SPECIES_GROUPS <- list(
  salmon    = SALMON_SPECIES,
  steelhead = c("Steelhead"),
  trout_char = c("Rainbow Trout", "Westslope Cutthroat", "Coastal Cutthroat",
                 "Dolly/Bull Trout", "Kokanee", "Brown Trout", "Cutbow Trout",
                 "Lake Trout", "Brook Trout", "Golden Trout", "Tiger Trout",
                 "Mountain Whitefish", "Lake Whitefish", "Pygmy Whitefish"),
  warmwater = c("Walleye", "Smallmouth Bass", "Largemouth Bass", "Yellow Perch",
                "Black Crappie", "White Crappie", "Channel Catfish",
                "Blue Catfish", "Brown Bullhead", "Black Bullhead", "Bluegill",
                "Burbot", "Tiger Musky", "Northern Pike", "Carp"),
  sturgeon  = c("White Sturgeon", "Green Sturgeon")
)

species_group <- function(x) {
  out <- rep("other", length(x))
  for (g in names(SPECIES_GROUPS)) out[x %in% SPECIES_GROUPS[[g]]] <- g
  out[is.na(x)] <- "unrecorded"
  out
}

req <- function(p, who) {
  if (!file.exists(p)) stop(glue("Required input missing:\n  {p}\nProduced by: {who}"),
                            call. = FALSE)
  p
}

req(RDS_PATH, "WDFW guide logbook extract (not committed - run where it exists)")
req(CRC_LUT,  "committed lookup table")

gl <- readRDS(RDS_PATH)

# ---- 1. Guided angler-trips, same filters as parse_guide_logbook.R -----------

trip_clean <- gl$trip |>
  mutate(
    trip_type_name = gl$trip_type_lut$name[match(trip_type_id, gl$trip_type_lut$id)],
    crc_code       = gl$water_body_lut$crc_code[match(water_body_id, gl$water_body_lut$id)],
    water_body     = gl$water_body_lut$name[match(water_body_id, gl$water_body_lut$id)],
    trip_year      = as.integer(format(trip_date, "%Y")),
    trip_month     = as.integer(format(trip_date, "%m"))
  ) |>
  filter(!is_void, trip_type_name == "Guided",
         trip_year %in% YEARS_SCOPE, !is.na(crc_code)) |>
  select(trip_id = id, crc_code, water_body, trip_year, trip_month)

anglers_per_trip <- gl$trip_angler |>
  mutate(angler_type = gl$trip_angler_type_lut$name[match(trip_angler_type_id,
                                                          gl$trip_angler_type_lut$id)]) |>
  filter(angler_type %in% c("Paying", "Comped")) |>
  count(trip_id, name = "angler_trips")

trips <- trip_clean |>
  left_join(anglers_per_trip, by = "trip_id") |>
  mutate(angler_trips = coalesce(angler_trips, 0L))

# ---- 2. Catch class ----------------------------------------------------------
# fish_count > 0 is the test, not merely the presence of an encounter row: rows
# with a zero count exist and are not evidence of a catch.

enc <- gl$encounter |>
  mutate(species = gl$species_lut$name[match(species_id, gl$species_lut$id)]) |>
  filter(trip_id %in% trips$trip_id)

cat("\n================ SPECIES ENCOUNTERED (guided, in scope) ================\n")
cat("Confirm the salmon list below is right before trusting anything else.\n\n")
enc |>
  group_by(species) |>
  summarise(encounters = n(), fish = sum(fish_count, na.rm = TRUE), .groups = "drop") |>
  mutate(counted_as_salmon = species %in% SALMON_SPECIES) |>
  arrange(desc(fish)) |> head(30) |> as.data.frame() |> print(row.names = FALSE)

enc <- enc |> mutate(sp_group = species_group(species))

# Dominant non-salmon group per trip, by fish count - what the trip was
# evidently after when no salmon were landed.
dominant_other <- enc |>
  filter(sp_group != "salmon", fish_count > 0) |>
  group_by(trip_id, sp_group) |>
  summarise(fish = sum(fish_count, na.rm = TRUE), .groups = "drop_last") |>
  slice_max(fish, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(trip_id, other_group = sp_group)

trip_catch <- enc |>
  group_by(trip_id) |>
  summarise(
    n_salmon      = sum(fish_count[species %in% SALMON_SPECIES], na.rm = TRUE),
    n_other       = sum(fish_count[!species %in% SALMON_SPECIES], na.rm = TRUE),
    species_seen  = paste(sort(unique(species)), collapse = "; "),
    .groups = "drop"
  ) |>
  left_join(dominant_other, by = "trip_id")

trips <- trips |>
  left_join(trip_catch, by = "trip_id") |>
  mutate(
    across(c(n_salmon, n_other), ~ coalesce(.x, 0)),
    catch_class = case_when(
      n_salmon > 0 ~ "salmon_catch",
      n_other  > 0 ~ "other_catch_only",
      TRUE         ~ "no_catch"
    )
  )

# ---- 3. Month class, from CRC salmon harvest --------------------------------
# CRC freshwater harvest is salmon-only (Chinook/Coho/Jack/Sockeye/Pink/Chum -
# no steelhead), so a month appearing there IS a month salmon were being caught
# in that area. That is the reference the logbook month is tested against.

crc_files <- c(file.path(CRC_DIR, "crc_freshwater_harvest_2010_2024_tidy.csv"),
               file.path(CRC_DIR, "crc_freshwater_harvest_final_creel_subs_tidy.csv"))
crc_files <- crc_files[file.exists(crc_files)]
if (length(crc_files) == 0) {
  stop(glue("No CRC harvest file in {CRC_DIR} - run parse_crc_freshwater_harvest.R"),
       call. = FALSE)
}

crc <- map_dfr(crc_files, ~ suppressMessages(read_csv(.x, show_col_types = FALSE))) |>
  filter(!is.na(calendar_month), harvest_count > 0) |>
  mutate(stream_code = as.character(stream_code))

salmon_months_yr <- crc |> distinct(stream_code, calendar_year, calendar_month)
# Pooled across years, for areas whose specific year CRC has not compiled yet
# (2025-2026 logbook trips mostly). Used only as a second tier, and reported.
salmon_months_any <- crc |> distinct(stream_code, calendar_month)
crc_years_by_area <- crc |> distinct(stream_code, calendar_year)

trips <- trips |>
  mutate(
    .has_crc_year = paste(crc_code, trip_year) %in%
      paste(crc_years_by_area$stream_code, crc_years_by_area$calendar_year),
    .in_month_yr = paste(crc_code, trip_year, trip_month) %in%
      paste(salmon_months_yr$stream_code, salmon_months_yr$calendar_year,
            salmon_months_yr$calendar_month),
    .in_month_any = paste(crc_code, trip_month) %in%
      paste(salmon_months_any$stream_code, salmon_months_any$calendar_month),
    month_class = case_when(
      .has_crc_year &  .in_month_yr  ~ "salmon_month",
      .has_crc_year & !.in_month_yr  ~ "non_salmon_month",
      .in_month_any                  ~ "salmon_month (pooled yrs)",
      TRUE                           ~ "non_salmon_month (pooled yrs)"
    ),
    month_basis = if_else(.has_crc_year, "crc_same_year", "crc_pooled_years")
  ) |>
  select(-starts_with("."))

# ---- 4. Final class ----------------------------------------------------------

trips <- trips |>
  mutate(
    .salmon_mo = str_starts(month_class, "salmon_month"),
    final_class = case_when(
      catch_class == "salmon_catch" &  .salmon_mo ~ "1 VERIFIED salmon",
      catch_class == "salmon_catch" & !.salmon_mo ~ "2 VERIFIED salmon, outside CRC salmon months",
      catch_class == "other_catch_only" & !.salmon_mo ~ "3 CLEAR exclude (non-salmon catch, non-salmon month)",
      catch_class == "other_catch_only" &  .salmon_mo ~ "4 AMBIGUOUS (non-salmon catch, salmon month)",
      catch_class == "no_catch" &  .salmon_mo ~ "5 AMBIGUOUS (no catch, salmon month)",
      TRUE                                    ~ "6 likely exclude (no catch, non-salmon month)"
    )
  ) |>
  select(-.salmon_mo)

# ---- 5. Report ---------------------------------------------------------------

tot_trips <- nrow(trips)
tot_ang   <- sum(trips$angler_trips)

pct <- function(x, tot) round(100 * x / tot, 1)

cat("\n================ THE CROSS-TAB (angler-trips) ================\n")
xt <- trips |>
  group_by(catch_class, month_class) |>
  summarise(trips = n(), angler_trips = sum(angler_trips), .groups = "drop") |>
  mutate(pct_angler_trips = pct(angler_trips, tot_ang)) |>
  arrange(catch_class, month_class)
print(as.data.frame(xt), row.names = FALSE)

cat("\n================ WHAT IT COMES TO ================\n")
summ <- trips |>
  group_by(final_class) |>
  summarise(trips = n(), angler_trips = sum(angler_trips), .groups = "drop") |>
  mutate(pct_angler_trips = pct(angler_trips, tot_ang)) |>
  arrange(final_class)
print(as.data.frame(summ), row.names = FALSE)

verified  <- sum(summ$angler_trips[str_starts(summ$final_class, "[12]")])
ambiguous <- sum(summ$angler_trips[str_starts(summ$final_class, "[45]")])
excluded  <- sum(summ$angler_trips[str_starts(summ$final_class, "[36]")])

cat(glue(
  "\nOf {format(tot_ang, big.mark = ',')} guided angler-trips in scope:\n",
  "  VERIFIED salmon (salmon caught)     {format(verified, big.mark = ',')}  ({pct(verified, tot_ang)}%)\n",
  "  AMBIGUOUS (inference required)      {format(ambiguous, big.mark = ',')}  ({pct(ambiguous, tot_ang)}%)\n",
  "  EXCLUDED (non-salmon evidence)      {format(excluded, big.mark = ',')}  ({pct(excluded, tot_ang)}%)\n\n",
  "The ambiguous share is the size of the assumption. Everything in it gets a\n",
  "guided salmon trip only because we decide it does.\n"
), "\n")

# ---- 5b. What the ambiguous cell is actually made of -------------------------
# Cell 4 is the bulk of the ambiguity, and most of it is not ambiguous once the
# non-salmon catch is named. Only cell 5 - nothing caught, in a salmon month -
# needs a genuine assumption.

cat("\n================ CELL 4 BROKEN OUT: what was caught instead ================\n")
cat("Non-salmon catch in a salmon-open month, by dominant species group.\n",
    "Anything but steelhead here is a different fishery, not an ambiguous one.\n\n", sep = "")
cell4 <- trips |>
  filter(str_starts(final_class, "4")) |>
  group_by(other_group) |>
  summarise(trips = n(), angler_trips = sum(angler_trips), .groups = "drop") |>
  mutate(pct_of_all = pct(angler_trips, tot_ang)) |>
  arrange(desc(angler_trips))
print(as.data.frame(cell4), row.names = FALSE)

irreducible <- sum(trips$angler_trips[str_starts(trips$final_class, "5")])
steelhead_amb <- sum(cell4$angler_trips[cell4$other_group == "steelhead"])
cat(glue(
  "\nIf a non-salmon catch is taken as evidence of a non-salmon trip, the only\n",
  "genuinely undecidable group left is 'no catch in a salmon month':\n",
  "  {format(irreducible, big.mark = ',')} angler-trips ({pct(irreducible, tot_ang)}% of the logbook)\n",
  "Steelhead-only trips in a salmon month are arguable either way and add\n",
  "  {format(steelhead_amb, big.mark = ',')} more ({pct(steelhead_amb, tot_ang)}%).\n"
), "\n")

cat("\n================ BY CRC REGION ================\n")
lut <- suppressMessages(read_csv(CRC_LUT, show_col_types = FALSE)) |>
  mutate(catch_area_code = as.character(catch_area_code)) |>
  group_by(catch_area_code) |>
  summarise(region = first(catch_area_region),
            area   = first(catch_area_description), .groups = "drop")

# The conditional sums MUST come before angler_trips is reassigned. summarise()
# evaluates in order, so `angler_trips = sum(angler_trips)` first would replace
# the per-row vector with a scalar and every later angler_trips[<logical>] would
# index that scalar and return NA. This exact trap has bitten this codebase
# four times now; it is silent every time.
by_region <- trips |>
  left_join(lut, by = c("crc_code" = "catch_area_code")) |>
  group_by(region) |>
  summarise(
    verified  = sum(angler_trips[str_starts(final_class, "[12]")]),
    ambiguous = sum(angler_trips[str_starts(final_class, "[45]")]),
    excluded  = sum(angler_trips[str_starts(final_class, "[36]")]),
    angler_trips = sum(angler_trips),
    .groups = "drop"
  ) |>
  mutate(pct_verified = pct(verified, angler_trips),
         pct_ambiguous = pct(ambiguous, angler_trips)) |>
  select(region, angler_trips, verified, ambiguous, excluded,
         pct_verified, pct_ambiguous) |>
  arrange(desc(angler_trips))
print(as.data.frame(by_region), row.names = FALSE)

cat("\n================ WORST AREAS FOR AMBIGUITY (top 20 by ambiguous angler-trips) ================\n")
by_area <- trips |>
  left_join(lut, by = c("crc_code" = "catch_area_code")) |>
  group_by(crc_code, area) |>
  summarise(
    verified  = sum(angler_trips[str_starts(final_class, "[12]")]),
    ambiguous = sum(angler_trips[str_starts(final_class, "[45]")]),
    excluded  = sum(angler_trips[str_starts(final_class, "[36]")]),
    angler_trips = sum(angler_trips),
    .groups = "drop"
  ) |>
  mutate(pct_ambiguous = pct(ambiguous, angler_trips)) |>
  select(crc_code, area, angler_trips, verified, ambiguous, excluded, pct_ambiguous) |>
  arrange(desc(ambiguous))
print(as.data.frame(head(by_area, 20)), row.names = FALSE)

cat("\n================ MONTH REFERENCE BASIS ================\n")
cat("How often the CRC month test used the trip's own year vs. pooled years:\n")
trips |> group_by(month_basis) |>
  summarise(angler_trips = sum(angler_trips), .groups = "drop") |>
  mutate(pct = pct(angler_trips, tot_ang)) |> as.data.frame() |> print(row.names = FALSE)

# ---- 6. Write ----------------------------------------------------------------

cls_path <- file.path(OUT_DIR, "guide_logbook_trip_classification.csv")
sum_path <- file.path(OUT_DIR, "guide_logbook_classification_summary.csv")
write_csv(trips, cls_path)
write_csv(by_area, sum_path)

cat(glue("\nWrote {cls_path}\nWrote {sum_path}\n"), "\n")
