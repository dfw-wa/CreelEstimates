# ==============================================================================
# logbook_guided_on_override_rivers.R
#
# Purpose:
#   Rivers in pst_location_override.csv are forced to bank (not known to be
#   boatable). Guide activity is evidence the other way: a river with
#   substantial logged guided trips is very likely boat-fished (guided salmon
#   anglers fish from boats ~96% of the time in creel interviews). This lists,
#   per override river, the guide logbook trips on its CRC area(s):
#     - all guided angler-trips (any catch), per year 2022-2025
#     - salmon-directed guided angler-trips (the rules used for the guided
#       floor), per year
#     - the deliverable's total trips and guided trips on that river
#       (pst_fw_categorization_audit.csv), for scale
#
# Inputs:
#   input_files/pst/lookup_tables/{pst_location_override,pst_river_block_crosswalk}.csv
#   analysis/pst/outputs/07_guide_logbook/
#     guide_logbook_angler_trips_by_crc_year.csv
#     guide_logbook_salmon_angler_trips_by_crc_year_month.csv
#   analysis/pst/outputs/05_assembly/pst_fw_categorization_audit.csv (optional)
#
# Output:
#   analysis/pst/outputs/05_assembly/pst_fw_override_rivers_logbook.csv
#
# Usage:
#   Rscript analysis/pst/03_analysis/logbook_guided_on_override_rivers.R
# ==============================================================================

suppressMessages({
  library(tidyverse)
  library(here)
  library(glue)
})
options(width = 220)

LUT <- here("input_files", "pst", "lookup_tables")
LOG <- here("analysis", "pst", "outputs", "07_guide_logbook")
ASM <- here("analysis", "pst", "outputs", "05_assembly")

ov <- read_csv(file.path(LUT, "pst_location_override.csv"), show_col_types = FALSE) |>
  transmute(river_label, p_boat_override = p_boat)
codes <- read_csv(file.path(LUT, "pst_river_block_crosswalk.csv"), show_col_types = FALSE) |>
  filter(river_label %in% ov$river_label, !is.na(crc_areas), crc_areas != "") |>
  distinct(river_label, block, crc_areas) |>
  separate_longer_delim(crc_areas, "|") |>
  transmute(river_label, block, crc_code = as.character(crc_areas)) |>
  distinct()

all_g <- read_csv(file.path(LOG, "guide_logbook_angler_trips_by_crc_year.csv"),
                  show_col_types = FALSE, col_types = cols(crc_code = "c")) |>
  filter(trip_type_name == "Guided") |>
  transmute(crc_code, year = as.integer(trip_year), guided_all = angler_trips)
sal_g <- read_csv(file.path(LOG, "guide_logbook_salmon_angler_trips_by_crc_year_month.csv"),
                  show_col_types = FALSE, col_types = cols(crc_code = "c")) |>
  group_by(crc_code, year = as.integer(trip_year)) |>
  summarise(guided_salmon = sum(angler_trips), .groups = "drop")

per_year <- codes |>
  inner_join(full_join(all_g, sal_g, by = c("crc_code", "year")), by = "crc_code",
             relationship = "many-to-many") |>
  filter(year %in% 2022:2025) |>
  group_by(block, river_label, year) |>
  summarise(guided_all = sum(guided_all, na.rm = TRUE),
            guided_salmon = round(sum(guided_salmon, na.rm = TRUE)), .groups = "drop")

audit_path <- file.path(ASM, "pst_fw_categorization_audit.csv")
deliv <- if (file.exists(audit_path)) {
  read_csv(audit_path, show_col_types = FALSE) |>
    filter(river_label %in% ov$river_label) |>
    group_by(river_label) |>
    summarise(deliverable_trips = round(sum(total_trips)),
              deliverable_guided = round(sum(guided)), .groups = "drop")
} else tibble(river_label = character(), deliverable_trips = double(), deliverable_guided = double())

summ <- ov |>
  left_join(per_year |> group_by(river_label, block) |>
              summarise(guided_all_2022_25 = sum(guided_all),
                        guided_salmon_2022_25 = sum(guided_salmon),
                        max_year_guided_salmon = max(c(0, guided_salmon)),
                        years_with_guides = sum(guided_all > 0), .groups = "drop"),
            by = "river_label") |>
  left_join(deliv, by = "river_label") |>
  mutate(across(c(guided_all_2022_25, guided_salmon_2022_25, max_year_guided_salmon, years_with_guides),
                ~ coalesce(.x, 0)),
         guided_salmon_pct_of_trips = round(100 * guided_salmon_2022_25 / deliverable_trips, 1)) |>
  arrange(desc(guided_salmon_2022_25), desc(guided_all_2022_25))

write_csv(summ, file.path(ASM, "pst_fw_override_rivers_logbook.csv"))

cat("=== guide logbook angler-trips on rivers forced to bank, 2022-2025 ===\n")
cat("(guided_all = any catch; guided_salmon = salmon-directed under the floor rules;\n",
    " deliverable_* = this river's total and guided trips in the deliverable)\n\n", sep = "")
summ |> select(block, river_label, guided_all_2022_25, guided_salmon_2022_25,
               max_year_guided_salmon, years_with_guides, deliverable_trips,
               deliverable_guided, guided_salmon_pct_of_trips) |>
  as.data.frame() |> print(row.names = FALSE)

cat("\n=== by year, rivers with any guided trips ===\n")
per_year |> filter(guided_all > 0) |>
  pivot_wider(names_from = year, values_from = c(guided_all, guided_salmon), values_fill = 0) |>
  arrange(river_label) |> as.data.frame() |> print(row.names = FALSE)
cat(glue("\nWrote {file.path(ASM, 'pst_fw_override_rivers_logbook.csv')}\n"))
