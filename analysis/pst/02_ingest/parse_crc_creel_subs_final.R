# ==============================================================================
# parse_crc_creel_subs_final.R
# Location: analysis/pst/02_ingest/parse_crc_creel_subs_final.R
#
# Purpose:
#   Parse the "final", creel-substituted CRC freshwater harvest workbooks
#   (input_files/pst/CRC/crc_edits_creel_subs/, pushed by Evan 2026-08-31 -
#   these have creel-based estimates substituted in for CRC-only card counts
#   where a design-based creel program exists, and are labeled "Final" for
#   2023/2024) into the same tidy schema as the main CRC extract, THEN
#   synthesize a ground-truth lookup: for every (catch_area_code, year), did
#   this final/corrected data report ANY salmon harvest at all?
#
#   Ground-truthing logic, extended from the P3-only, hand-verified season-
#   status mechanism (pst_crc_harvest_projection.R's PART 3) to run
#   automatically across every year this final data covers (2022-2024) and
#   across BOTH P2 and P3, not just P3/2025:
#     - Real creel survey presence (P1) already proves a fishery ran - this
#       lookup is not consulted for P1 rows.
#     - A CRC-only (P2/P3) area-year with ZERO final harvest reported is
#       treated as evidence the fishery did not occur that year (closed, or
#       no fish available to catch) - NOT as "harvest happened to be zero in
#       an open season." This is a deliberate simplification (Evan's call,
#       2026-08-31): a real, ongoing freshwater fishery drawing any legal
#       effort across a full season/area essentially always produces SOME
#       reported catch, so a clean zero across every species and month is a
#       strong signal of closure, not bad luck - the same asymmetry the
#       season-status lookup's own "restricted" vs "closed" distinction
#       already leans on.
#
# This is purely a lookup-building step. It does NOT feed the main CRC
# harvest file (crc_freshwater_harvest_2010_2024_tidy.csv) that P2's ratio
# construction and P3's projection actually run on - those stay on the
# original workbooks. apply_final_harvest_occurrence_filter() (pst_fw_
# angler_trips_assembly.R) is what actually zeroes out any P2/P3 trip row
# this lookup flags as not-occurred.
#
# Input files (input_files/pst/CRC/crc_edits_creel_subs/):
#   Salmon Freshwater Estimates 2022.xlsx            -> sheet "FW 2022-2023"
#   Salmon Freshwater Estimates 2023 Final.xlsx       -> sheet "FW 2023-2024"
#   Salmon Freshwater Estimates 2024 Final.xlsx       -> sheet "FW 2024-2025"
#
# Outputs:
#   analysis/pst/outputs/01_crc_harvest/crc_freshwater_harvest_final_creel_subs_tidy.csv
#     Same leaf-level schema as the main CRC extract (license_year, region,
#     system, stream, stream_code, species, calendar_year, calendar_month,
#     harvest_count) - kept for audit/comparison, not consumed elsewhere.
#   analysis/pst/outputs/01_crc_harvest/pst_crc_final_harvest_occurrence.csv
#     One row per (catch_area_code, year): final_harvest_total, season_
#     occurred (TRUE/FALSE). THE lookup apply_final_harvest_occurrence_
#     filter() reads.
# ==============================================================================

library(tidyverse)
library(here)
library(glue)
library(cli)

source(here("analysis", "pst", "02_ingest", "pst_crc_fw_parser.R"))

out_dir <- here("analysis", "pst", "outputs", "01_crc_harvest")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

OUT_TIDY_CSV      <- file.path(out_dir, "crc_freshwater_harvest_final_creel_subs_tidy.csv")
OUT_OCCURRENCE_CSV <- file.path(out_dir, "pst_crc_final_harvest_occurrence.csv")

FINAL_MANIFEST <- tribble(
  ~license_year, ~filename,                                    ~sheet,
  2022L,         "Salmon Freshwater Estimates 2022.xlsx",       "FW 2022-2023",
  2023L,         "Salmon Freshwater Estimates 2023 Final.xlsx", "FW 2023-2024",
  2024L,         "Salmon Freshwater Estimates 2024 Final.xlsx", "FW 2024-2025"
)

# 1. Parse ---------------------------------------------------------------------

cli::cli_h1("Parsing final/creel-substituted CRC workbooks")

parsed_list <- pmap(FINAL_MANIFEST, parse_fw_workbook, subdir = "crc_edits_creel_subs")
names(parsed_list) <- as.character(FINAL_MANIFEST$license_year)

all_leaf <- map_dfr(parsed_list, "leaf_long")

# 2. Light validation ------------------------------------------------------------
# Lighter than parse_crc_freshwater_harvest.R's 6-check suite on purpose: these
# are 3 files, already labeled "Final"/reconciled by WDFW, not the 15-year
# historical set with its own catalogued Draft-status discrepancies. One
# check - leaf sums reconcile to each stream's own subtotal row - is enough to
# catch a genuine parse error without re-deriving that whole apparatus.

cli::cli_h2("Check: leaf sums vs stream-species Total rows")

leaf_sums <- map_dfr(parsed_list, function(p) {
  p$leaf |>
    select(license_year, stream, stream_code, species, all_of(p$month_col_names)) |>
    rowwise() |>
    mutate(leaf_sum = sum(c_across(all_of(p$month_col_names)), na.rm = TRUE)) |>
    ungroup() |>
    select(license_year, stream, stream_code, species, leaf_sum)
})

leaf_stream_sums <- leaf_sums |>
  group_by(license_year, stream, stream_code) |>
  summarise(leaf_stream_sum = sum(leaf_sum, na.rm = TRUE), .groups = "drop")

subtot_sums <- map_dfr(parsed_list, function(p) {
  p$subtotals |>
    select(license_year, stream, stream_code, all_of(p$month_col_names)) |>
    rowwise() |>
    mutate(subtot_sum = sum(c_across(all_of(p$month_col_names)), na.rm = TRUE)) |>
    ungroup() |>
    select(license_year, stream, stream_code, subtot_sum)
})

check <- subtot_sums |>
  left_join(leaf_stream_sums, by = c("license_year", "stream", "stream_code")) |>
  mutate(diff = subtot_sum - coalesce(leaf_stream_sum, 0L), match = abs(diff) < 0.5)

check_fail <- filter(check, !match)
if (nrow(check_fail) == 0L) {
  cli::cli_alert_success("PASS: All {nrow(check)} stream subtotal rows reconcile with leaf sums.")
} else {
  cli::cli_alert_warning("{nrow(check_fail)} stream subtotal(s) do not match leaf sums (not blocking):")
  print(select(check_fail, license_year, stream, stream_code, leaf_stream_sum, subtot_sum, diff))
}

# 3. Write tidy leaf-level CSV ---------------------------------------------------

readr::write_csv(all_leaf, OUT_TIDY_CSV)
cli::cli_alert_success(glue("Wrote {nrow(all_leaf)} rows to {OUT_TIDY_CSV}"))

# 4. Synthesize the occurrence lookup --------------------------------------------
# Salmon only (VALID_SPECIES excludes Steelhead already, matching the main
# extract's own PST-scope convention) - sum every species/month per
# (stream_code, calendar_year). A stream_code with NO rows at all in a given
# year (the fishery isn't even listed that year) is exactly as strong a
# signal as a listed-but-all-zero row, so both collapse to the same
# final_harvest_total = 0 / season_occurred = FALSE outcome - achieved by
# reading straight from all_leaf's actual rows rather than a pre-scaffolded
# area x year grid, since an absent row and a zero row mean the same thing
# here (unlike pst_season_status_lookup.csv's month-level UNVERIFIED, where
# "no row yet" and "verified open" are deliberately different states).

# EXCLUDE calendar_year 2025 (added 2026-08-31, confirmed empirically before
# this exclusion): the 3-file manifest above only supplies 2025's Jan-Mar
# months (the tail of the 2024 license-year file, which runs Apr 2024-Mar
# 2025) - Apr-Dec 2025 isn't in a "final" file yet. Almost every fall-run
# salmon fishery is dormant Jan-Mar, so treating that 3-month sliver as the
# WHOLE year produced 117 of 127 (92%) area-years flagged FALSE for 2025 -
# an artifact of partial coverage, not evidence of 117 closures. calendar_
# year 2022 is kept despite also being partial (Apr-Dec only, since license_
# year 2021 isn't in this manifest to supply its Jan-Mar) - Jan-Mar is
# off-season for salmon broadly, so a 9-month Apr-Dec window is a reasonable
# proxy for "did the season run," unlike 2025's Jan-Mar-only window.
occurrence <- all_leaf |>
  filter(species %in% VALID_SPECIES, calendar_year != 2025L) |>
  group_by(catch_area_code = stream_code, year = calendar_year) |>
  summarise(final_harvest_total = sum(harvest_count, na.rm = TRUE), .groups = "drop") |>
  mutate(season_occurred = final_harvest_total > 0) |>
  arrange(catch_area_code, year)

readr::write_csv(occurrence, OUT_OCCURRENCE_CSV)

n_not_occurred <- sum(!occurrence$season_occurred)
cli::cli_alert_success(glue(
  "Wrote {nrow(occurrence)} (area, year) rows to {OUT_OCCURRENCE_CSV} - ",
  "{n_not_occurred} flagged season_occurred = FALSE."
))
