# ==============================================================================
# skagit_cascade_fall_monthly.R
#
# Purpose:
#   Monthly angler-trip estimates for the Skagit (CRC 830) and Cascade (CRC
#   826) fall salmon fisheries, 2022-2025, with the basis of every month:
#     - Creel (P1): the fall salmon creel's own design-based estimate.
#     - CRC infill (P2 / P3): a month in the fall window with CRC harvest but
#       no creel on that catch area - CRC harvest (actual, or projected for
#       2025 months not yet compiled) x block trips-per-fish ratio.
#
#   The assembly finds infill months by CATCH AREA, not by fishery: its
#   p2_month_gap / p3_month_gap rows carry no fishery_name. Here an infill
#   month is assigned to the area-year's fall salmon fishery when it falls in
#   the fall window (Aug-Dec, per Evan 2026-10-21); infill outside the window
#   (e.g. April) is left out and reported.
#
#   2025 is a composite creel: "Skagit fall salmon 2025" covered 826 and 830
#   together (river label "Skagit + Cascade"), so 2025 has no separate
#   Cascade fall fishery. 2022-2024 have separate Skagit and Cascade creels.
#
#   Fall-window months with no output row get a row with 0 trips and the
#   reason: marked closed in pst_month_gap_review.csv, another creel (e.g.
#   Skagit summer sockeye) covered that area-month so no infill was made, or
#   no CRC harvest.
#
# Inputs:
#   analysis/pst/outputs/05_assembly/pst_fw_effort_long.csv (assembly output)
#   input_files/pst/lookup_tables/pst_river_block_crosswalk.csv
#   input_files/pst/lookup_tables/pst_month_gap_review.csv
#
# Output:
#   analysis/pst/outputs/05_assembly/pst_fw_skagit_cascade_fall_monthly.csv
#
# Usage (after the assembly):
#   Rscript analysis/pst/03_analysis/skagit_cascade_fall_monthly.R
# ==============================================================================

suppressMessages({
  library(tidyverse)
  library(here)
  library(glue)
})
options(width = 220)

ASM    <- here("analysis", "pst", "outputs", "05_assembly")
LUT    <- here("input_files", "pst", "lookup_tables")
OUT    <- file.path(ASM, "pst_fw_skagit_cascade_fall_monthly.csv")
AREAS  <- c("826", "830")
FALL_MONTHS <- 8:12
GAP_SOURCES <- c("p2_month_gap", "p3_month_gap")

# --- Fall salmon fishery for each catch area x year ---------------------------
fall_xw <- read_csv(file.path(LUT, "pst_river_block_crosswalk.csv"), show_col_types = FALSE,
                    col_types = cols(.default = "c")) |>
  filter(source_id == "creel_pe",
         str_detect(fishery_name, "^(Skagit|Cascade) fall salmon \\d{4}$")) |>
  mutate(year = as.integer(str_extract(fishery_name, "\\d{4}$"))) |>
  separate_longer_delim(crc_areas, "|") |>
  transmute(fall_fishery = fishery_name, catch_area_code = crc_areas, year)
dup <- fall_xw |> count(catch_area_code, year) |> filter(n > 1)
if (nrow(dup) > 0) stop("More than one fall salmon fishery on an area-year in the crosswalk: ",
                        paste(glue("{dup$catch_area_code} {dup$year}"), collapse = ", "))

# --- Assembly output for these areas ------------------------------------------
el <- read_csv(file.path(ASM, "pst_fw_effort_long.csv"), show_col_types = FALSE,
               col_types = cols(catch_area_code = "c", fishery_name = "c",
                                river_label = "c", method = "c", .default = col_guess())) |>
  mutate(catch_area_code = str_remove(as.character(catch_area_code), "\\.0+$"),
         year = as.integer(year), month = as.integer(month)) |>
  filter(catch_area_code %in% AREAS | fishery_name %in% fall_xw$fall_fishery)

rows <- el |>
  mutate(part = case_when(
    fishery_name %in% fall_xw$fall_fishery ~ "fall creel",
    source_id %in% GAP_SOURCES & month %in% FALL_MONTHS ~ "infill, fall window",
    source_id %in% GAP_SOURCES ~ "infill, outside fall window",
    tier == "P1" ~ "other creel",
    TRUE ~ paste("other:", tier, source_id)))

# Every trip on these areas, by part - the fall table plus the rest adds up.
cat("=== Trips on CRC 826/830 (and the fall fisheries) by part ===\n")
rows |> group_by(year, part) |> summarise(trips = round(sum(angler_trips)), .groups = "drop") |>
  pivot_wider(names_from = part, values_from = trips, values_fill = 0) |>
  arrange(year) |> as.data.frame() |> print(row.names = FALSE)
cat("\n")

# --- Monthly fall table -------------------------------------------------------
fall_rows <- rows |>
  filter(part %in% c("fall creel", "infill, fall window")) |>
  left_join(fall_xw |> rename(.xw_fishery = fall_fishery), by = c("catch_area_code", "year")) |>
  mutate(fall_fishery = if_else(part == "fall creel", fishery_name, .xw_fishery))
unassigned <- fall_rows |> filter(is.na(fall_fishery))
if (nrow(unassigned) > 0) {
  warning(glue("{nrow(unassigned)} infill row(s) on an area-year with no fall salmon creel in ",
               "the crosswalk - left out: ",
               "{paste(unique(glue('{unassigned$catch_area_code} {unassigned$year}-{unassigned$month}')), collapse = ', ')}"),
          call. = FALSE)
  fall_rows <- fall_rows |> filter(!is.na(fall_fishery))
}

other_creel <- rows |>
  filter(part == "other creel", !is.na(month), angler_trips > 0) |>
  group_by(catch_area_code, year, month) |>
  summarise(other_creel_same_area_month = paste(sort(unique(fishery_name)), collapse = " ; "),
            other_creel_trips = round(sum(angler_trips)), .groups = "drop")

monthly <- fall_rows |>
  mutate(basis = case_when(
    part == "fall creel"          ~ "Creel (P1)",
    source_id == "p2_month_gap"   ~ "CRC infill - actual CRC harvest (P2)",
    source_id == "p3_month_gap"   ~ "CRC infill - projected CRC harvest (P3)")) |>
  group_by(year, fall_fishery, river_label, catch_area_code, month, basis, source_id) |>
  summarise(
    trips          = sum(angler_trips),
    boat_trips     = sum(angler_trips[location == "boat"]),
    bank_trips     = sum(angler_trips[location == "bank"]),
    guided_trips   = sum(angler_trips[mode == "guided"]),
    unguided_trips = sum(angler_trips[mode == "unguided"]),
    salmon_harvest = sum(total_salmon_harvest, na.rm = TRUE),
    method         = first(na.omit(method)),
    .groups = "drop") |>
  mutate(note = case_when(
    basis == "Creel (P1)" & is.na(month) ~ "Creel row with no month (annual total)",
    basis == "Creel (P1)" ~ "",
    TRUE ~ as.character(glue("No creel on CRC {catch_area_code} this month; CRC harvest x ",
                             "ratio, assigned to the fall fishery by the Aug-Dec window"))))

# --- Fall-window months with nothing, and why ---------------------------------
review <- {
  p <- file.path(LUT, "pst_month_gap_review.csv")
  if (file.exists(p)) read_csv(p, show_col_types = FALSE, col_types = cols(.default = "c")) |>
    transmute(catch_area_code, year = as.integer(year), month = as.integer(month),
              closed = str_to_lower(str_squish(coalesce(decision, ""))) == "closed") |>
    filter(closed, !is.na(month)) |> distinct(catch_area_code, year, month)
  else tibble(catch_area_code = character(), year = integer(), month = integer())
}
# A composite fall creel (2025) covers a month on every one of its areas when
# it has a row for that month on any of them.
fishery_months <- monthly |> filter(basis == "Creel (P1)", !is.na(month)) |>
  distinct(fall_fishery, month)
empty <- fall_xw |>
  filter(year %in% unique(el$year)) |>
  crossing(month = FALL_MONTHS) |>
  anti_join(monthly, by = c("fall_fishery", "catch_area_code", "year", "month")) |>
  anti_join(fishery_months, by = c("fall_fishery", "month")) |>
  left_join(other_creel, by = c("catch_area_code", "year", "month")) |>
  left_join(review |> mutate(.closed = TRUE), by = c("catch_area_code", "year", "month")) |>
  left_join(rows |> distinct(catch_area_code, year, river_label) |>
              group_by(catch_area_code, year) |> summarise(river_label = first(river_label), .groups = "drop"),
            by = c("catch_area_code", "year")) |>
  transmute(year, fall_fishery, river_label, catch_area_code, month,
            basis = "No trips", source_id = NA_character_,
            trips = 0, boat_trips = 0, bank_trips = 0, guided_trips = 0, unguided_trips = 0,
            salmon_harvest = 0, method = NA_character_,
            note = case_when(
              coalesce(.closed, FALSE) ~ "CRC infill dropped: marked closed in pst_month_gap_review.csv",
              !is.na(other_creel_same_area_month) ~ as.character(glue(
                "Another creel ran on CRC {catch_area_code} this month, so no CRC infill was ",
                "made; its trips stay with that fishery")),
              TRUE ~ "No CRC harvest reported (or projected month closed), so no infill"))

out <- bind_rows(monthly, empty) |>
  left_join(other_creel, by = c("catch_area_code", "year", "month")) |>
  mutate(across(c(trips, boat_trips, bank_trips, guided_trips, unguided_trips, salmon_harvest),
                ~ round(.x, 1))) |>
  select(year, fall_fishery, river_label, catch_area_code, month, basis, source_id,
         trips, boat_trips, bank_trips, guided_trips, unguided_trips, salmon_harvest,
         other_creel_same_area_month, other_creel_trips, note, method) |>
  arrange(year, fall_fishery, catch_area_code, month)

write_csv(out, OUT, na = "")

cat("=== Monthly fall salmon trips (salmon_harvest = creel estimate for P1, CRC harvest for infill) ===\n")
out |> select(year, fall_fishery, catch_area_code, month, basis, trips, boat_trips, bank_trips,
              guided_trips, salmon_harvest, other_creel_same_area_month) |>
  mutate(trips = round(trips), across(c(boat_trips, bank_trips, guided_trips, salmon_harvest), round)) |>
  as.data.frame() |> print(row.names = FALSE)

cat("\n=== Fall fishery totals: creel vs CRC infill ===\n")
out |> group_by(year, fall_fishery) |>
  summarise(creel_trips  = round(sum(trips[basis == "Creel (P1)"])),
            infill_trips = round(sum(trips[str_detect(basis, "^CRC infill")])),
            total_trips  = round(sum(trips)),
            infill_months = {
              i <- str_detect(basis, "^CRC infill")
              if (any(i)) paste(sort(unique(paste0(catch_area_code[i], ":", month[i]))),
                                collapse = " ") else ""
            },
            .groups = "drop") |>
  mutate(pct_infill = round(100 * infill_trips / pmax(total_trips, 1), 1)) |>
  as.data.frame() |> print(row.names = FALSE)
cat(glue("\nWrote {OUT}\n"))
