# ==============================================================================
# check_composite_double_count.R
#
# Purpose:
#   Verify the composite creel rows ("Cascade + Skagit", "Skykomish +
#   Snohomish") do not double count against their single-river rows. The
#   labels themselves cannot (every creel fishery's trips are counted once,
#   under one river_label); what can is AREA COVERAGE: P2/P3 decide whether to
#   expand an area from the catch_area_code the P1 rows actually carry. If a
#   composite creel's rows are all coded to one of its areas, the other looks
#   uncovered and is expanded on top of the creel.
#
# Checks, on the final categorised output (pst_fw_categorization_audit.csv):
#   1. Area-year with P1 creel trips AND a full-year P2/P3 expansion (not a
#      month-gap row) - a double count.
#   2. Month-gap rows in an area-month that has P1 creel trips - a double
#      count (should be impossible by construction).
#   3. For each composite creel fishery-year, which of its crosswalk CRC
#      areas its P1 rows actually carry - an area listed but not carried is
#      where check 1 would bite.
#   4. Area-months with trips from more than one creel fishery (e.g. Skagit
#      sockeye and Cascade + Skagit spring Chinook on 830) - listed for
#      review; separate creel programs can legitimately overlap in an area.
#   5. Trip totals by river label for the four labels, by year.
#
# Usage:
#   Rscript analysis/pst/03_analysis/check_composite_double_count.R
# ==============================================================================

suppressMessages({
  library(tidyverse)
  library(here)
  library(glue)
})
options(width = 220)

ASM  <- here("analysis", "pst", "outputs", "05_assembly")
CW   <- here("input_files", "pst", "lookup_tables", "pst_river_block_crosswalk.csv")
FOCUS_AREAS  <- c("826", "830", "844", "850", "852")
FOCUS_RIVERS <- c("Cascade", "Skagit", "Cascade + Skagit",
                  "Skykomish", "Snohomish", "Skykomish + Snohomish")

a <- read_csv(file.path(ASM, "pst_fw_categorization_audit.csv"), show_col_types = FALSE,
              col_types = cols(catch_area_code = "c", .default = col_guess())) |>
  mutate(catch_area_code = as.character(catch_area_code),
         month_gap = source_id %in% c("p2_month_gap", "p3_month_gap"))

# One row per unit (the audit has one row per unit already); trips by area.
p1 <- a |> filter(tier == "P1", total_trips > 0)
exp_full <- a |> filter(tier %in% c("P2", "P3"), !month_gap, total_trips > 0)
exp_gap  <- a |> filter(month_gap, total_trips > 0)

cat("=== 1. P1 creel AND full-year P2/P3 expansion in the same area-year (double count) ===\n")
c1 <- p1 |> distinct(catch_area_code, year) |> filter(!is.na(catch_area_code)) |>
  inner_join(exp_full |> group_by(catch_area_code, year, river_label, tier, source_id) |>
               summarise(expansion_trips = round(sum(total_trips)), .groups = "drop"),
             by = c("catch_area_code", "year"))
if (nrow(c1) == 0) cat("  none\n") else print(as.data.frame(c1), row.names = FALSE)
cat("  (focus areas", paste(FOCUS_AREAS, collapse = "/"), "only):",
    nrow(filter(c1, catch_area_code %in% FOCUS_AREAS)), "\n\n")

cat("=== 2. Month-gap rows in an area-month that has P1 creel trips (double count) ===\n")
c2 <- exp_gap |> semi_join(p1 |> filter(!is.na(month)), by = c("catch_area_code", "year", "month"))
if (nrow(c2) == 0) cat("  none\n\n") else {
  print(as.data.frame(c2 |> select(catch_area_code, year, month, river_label, total_trips)), row.names = FALSE)
  cat("\n")
}

cat("=== 3. Composite creel fisheries: crosswalk CRC areas vs areas their P1 rows carry ===\n")
cw <- read_csv(CW, show_col_types = FALSE) |>
  filter(source_id == "creel_pe", str_detect(coalesce(crc_areas, ""), fixed("|"))) |>
  select(fishery_name, river_label, crc_areas)
c3 <- cw |>
  left_join(p1 |> group_by(fishery_name) |>
              summarise(carried = paste(sort(unique(na.omit(catch_area_code))), collapse = "|"),
                        na_code_trips = round(sum(total_trips[is.na(catch_area_code)])),
                        trips = round(sum(total_trips)), .groups = "drop"),
            by = "fishery_name") |>
  mutate(listed_not_carried = map2_chr(crc_areas, carried, \(l, c)
           paste(setdiff(strsplit(l, "|", fixed = TRUE)[[1]],
                         strsplit(coalesce(c, ""), "|", fixed = TRUE)[[1]]), collapse = "|")))
print(as.data.frame(c3), row.names = FALSE)
cat("  listed_not_carried non-empty = that area's coverage by this creel is invisible to P2/P3.\n\n")

cat("=== 3b. Full-year P2/P3 expansions on an area a same-year composite creel lists but does not carry (double count) ===\n")
yr_of <- function(fn) as.integer(str_extract(fn, "\\b(20\\d{2})\\b"))
c3b <- c3 |> filter(listed_not_carried != "", !is.na(trips)) |>
  mutate(year = yr_of(fishery_name)) |>
  separate_longer_delim(listed_not_carried, "|") |>
  rename(catch_area_code = listed_not_carried) |>
  inner_join(exp_full |> group_by(catch_area_code, year, source_id) |>
               summarise(expansion_trips = round(sum(total_trips)), .groups = "drop"),
             by = c("catch_area_code", "year"))
if (nrow(c3b) == 0) cat("  none\n\n") else {
  print(as.data.frame(c3b |> select(fishery_name, catch_area_code, year, source_id, expansion_trips)), row.names = FALSE)
  cat("\n")
}

cat("=== 4. Area-months with trips from more than one creel fishery (review) ===\n")
c4 <- p1 |> filter(!is.na(catch_area_code), !is.na(month)) |>
  group_by(catch_area_code, year, month) |>
  filter(n_distinct(fishery_name) > 1) |>
  summarise(fisheries = paste(sort(unique(fishery_name)), collapse = " ; "),
            trips = round(sum(total_trips)), .groups = "drop")
if (nrow(c4) == 0) cat("  none\n\n") else {
  print(as.data.frame(c4 |> filter(catch_area_code %in% FOCUS_AREAS)), row.names = FALSE)
  cat(glue("  ({nrow(c4)} area-months statewide; focus areas shown)\n\n"))
}

cat("=== 5. Trips by river label and year (focus rivers) ===\n")
a |> filter(river_label %in% FOCUS_RIVERS) |>
  group_by(river_label, year) |>
  summarise(trips = round(sum(total_trips)),
            tiers = paste(sort(unique(tier)), collapse = "+"),
            areas = paste(sort(unique(na.omit(catch_area_code))), collapse = "|"), .groups = "drop") |>
  arrange(river_label, year) |> as.data.frame() |> print(row.names = FALSE)
