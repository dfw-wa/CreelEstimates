# ==============================================================================
# columbia_bank_boat_interview_scan.R
#
# Purpose:
#   Lower Columbia P2/P3 rivers (Cowlitz, Lewis, Kalama, ...) get bank/boat from
#   the all-creel pooled ratio (0.337 boat) because no Columbia creel below Drano
#   carries a design-based bank/boat split. Before deciding what to borrow, find
#   every creel interview in the database that could inform bank/boat on
#   Columbia systems - including interviews with NO fishery_name, or a
#   fishery_name absent from fishery_lut / the PST crosswalk, which the old
#   per-fishery pull in interview_proportions.qmd never saw.
#
# Needs no DB: reads the unfiltered interview pull that interview_proportions.qmd
# persists (render it first after deleting any old .cache/all_interviews*.rds).
#
# Columbia = interview crc_area whose CRC region names the Columbia or Snake
# (crc_area_lut.csv), or, where crc_area is blank, a water_body matching a
# Columbia-system name list. Bank/boat derived exactly as the qmd does.
#
# Output (analysis/pst/outputs/04_interview_proportions/):
#   columbia_bank_boat_interview_scan.csv        - water body x fishery x year
#   columbia_bank_boat_interview_scan_month.csv  - water body x year x month
#
# Usage:
#   Rscript analysis/pst/03_analysis/columbia_bank_boat_interview_scan.R
# ==============================================================================

suppressMessages({
  library(tidyverse)
  library(here)
  library(glue)
})
options(width = 220)

IN_DIR  <- here("analysis", "pst", "outputs", "04_interview_proportions")
LUT     <- here("input_files", "pst", "lookup_tables", "crc_area_lut.csv")
CW_PATH <- here("input_files", "pst", "lookup_tables", "pst_river_block_crosswalk.csv")
MIN_LOCATED <- 20

COLUMBIA_WB <- regex(paste(
  "columbia|cowlitz|cispus|toutle|coweeman|kalama|lewis|washougal|elochoman",
  "grays|abernathy|germany|mill cr|deep r|chinook r|salmon cr|cedar cr",
  "wind r|drano|little white|white salmon|klickitat|rock cr|camas|blue cr",
  "tilton|mayfield|riffe|merwin|yale|swift|snake|tucannon|grande ronde",
  "walla|touchet|yakima|naches|wenatchee|icicle|entiat|methow|okanogan",
  "similkameen|hanford|ringold|priest|mcnary|bonneville|dalles",
  sep = "|"), ignore_case = TRUE)

rds <- file.path(IN_DIR, "all_interviews.rds")
if (!file.exists(rds)) stop(glue("{rds} not found - render interview_proportions.qmd first."), call. = FALSE)
int <- readRDS(rds) |> mutate(across(everything(), as.character))

col_or_na <- function(d, nm) if (nm %in% names(d)) d[[nm]] else NA_character_
cat("=== interview pull ===\n")
cat(glue("{nrow(int)} rows; {sum(is.na(int$fishery_name))} with no fishery_name\n\n"))

lut <- read_csv(LUT, show_col_types = FALSE, name_repair = "unique_quiet") |>
  transmute(crc_area = as.character(catch_area_code),
            crc_desc = catch_area_description, crc_region = catch_area_region) |>
  distinct(crc_area, .keep_all = TRUE)
cw_fish <- read_csv(CW_PATH, show_col_types = FALSE) |>
  filter(!is.na(fishery_name)) |> pull(fishery_name) |> unique()

na_str <- function(x) if_else(x %in% c("NA", "", " "), NA_character_, x)

ints <- int |>
  mutate(
    across(everything(), na_str),
    crc_area    = col_or_na(int, "crc_area"),
    water_body  = col_or_na(int, "water_body"),
    angler_type = col_or_na(int, "angler_type"),
    boat_used   = col_or_na(int, "boat_used"),
    fish_from_boat = col_or_na(int, "fish_from_boat"),
    trip_guided = col_or_na(int, "trip_guided"),
    target_species = col_or_na(int, "target_species"),
    date  = suppressWarnings(as.Date(event_date)),
    year  = lubridate::year(date),
    month = lubridate::month(date),
    angler_final = case_when(
      angler_type == "Bank" ~ "Bank",
      angler_type == "Boat" ~ "Boat",
      boat_used == "No" ~ "Bank",
      boat_used == "Yes" & fish_from_boat == "Bank" ~ "Bank",
      boat_used == "Yes" ~ "Boat",
      TRUE ~ NA_character_),
    anglers = suppressWarnings(as.numeric(angler_count)),
    anglers = if_else(is.na(anglers) | anglers <= 0, 1, anglers)
  ) |>
  left_join(lut, by = "crc_area") |>
  mutate(
    columbia = coalesce(str_detect(crc_region, regex("columbia|snake", ignore_case = TRUE)), FALSE) |
               (is.na(crc_region) & coalesce(str_detect(water_body, COLUMBIA_WB), FALSE)),
    fishery = coalesce(fishery_name, "(no fishery_name)"),
    fishery_status = case_when(
      is.na(fishery_name)          ~ "no fishery_name",
      fishery_name %in% cw_fish    ~ "in PST crosswalk",
      TRUE                         ~ "not in PST crosswalk")
  )

col <- ints |> filter(columbia)
cat(glue("Columbia-system interviews: {nrow(col)} ",
         "({sum(!is.na(col$angler_final))} with bank/boat derivable)\n\n"))

cat("=== by fishery status (Columbia) ===\n")
col |> group_by(fishery_status) |>
  summarise(interviews = n(), located = sum(!is.na(angler_final)),
            water_bodies = n_distinct(water_body), years = paste(sort(unique(year)), collapse = ","),
            .groups = "drop") |>
  as.data.frame() |> print(row.names = FALSE)

summ <- function(d, ...) {
  d |> group_by(...) |>
    summarise(
      interviews = n(),
      located    = sum(!is.na(angler_final)),
      pct_boat_angler = round(100 * sum(anglers[angler_final %in% "Boat"]) /
                                sum(anglers[!is.na(angler_final)]), 1),
      pct_boat_party  = round(100 * mean(angler_final[!is.na(angler_final)] == "Boat"), 1),
      guided_flagged  = sum(trip_guided %in% c("Guided", "Non-guided")),
      pct_guided      = round(100 * sum(trip_guided %in% "Guided") /
                                max(sum(trip_guided %in% c("Guided", "Non-guided")), 1), 1),
      target_answered = sum(!is.na(target_species) & target_species != "Target species not asked"),
      months = paste(sort(unique(month)), collapse = ","),
      crc_areas = paste(sort(unique(na.omit(crc_area))), collapse = "|"),
      .groups = "drop")
}

by_fy <- summ(col, crc_region, water_body, fishery, fishery_status, year) |>
  arrange(crc_region, water_body, year, fishery)
by_m  <- summ(col, crc_region, water_body, year, month) |>
  arrange(crc_region, water_body, year, month)
write_csv(by_fy, file.path(IN_DIR, "columbia_bank_boat_interview_scan.csv"))
write_csv(by_m,  file.path(IN_DIR, "columbia_bank_boat_interview_scan_month.csv"))

cat("\n=== Columbia water bodies with bank/boat data, all years (located >= ", MIN_LOCATED, ") ===\n", sep = "")
summ(col, crc_region, water_body) |>
  filter(located >= MIN_LOCATED) |>
  arrange(crc_region, desc(located)) |>
  select(crc_region, water_body, interviews, located, pct_boat_angler, pct_boat_party,
         pct_guided, guided_flagged, crc_areas) |>
  as.data.frame() |> print(row.names = FALSE)

cat("\n=== the NEW data: Columbia interviews outside the PST crosswalk / with no fishery_name ===\n")
by_fy |> filter(fishery_status != "in PST crosswalk", located > 0) |>
  select(crc_region, water_body, fishery, year, interviews, located, pct_boat_angler, months) |>
  as.data.frame() |> print(row.names = FALSE)

cat(glue("\nWrote {file.path(IN_DIR, 'columbia_bank_boat_interview_scan.csv')} and _month.csv\n"))
