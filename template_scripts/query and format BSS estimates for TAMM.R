# Pull BSS daily catch estimates from the creel database and scale the daily means so they sum to the season-total median.

# To run:
# 1. Enter the analysis_id(s) and the species, life stage, fin mark and fate you want under USER ENTRY A and B.
# 2. Run the whole script. Check the summary near the end, where diff should be close to 0 for each catch group.
# 3. The csv saves to the project folder. Change the file name on the last line if needed.

# Set analysis_ids -----------------------------------------------------------------------------------------------------

# ---- USER ENTRY A ----
skagit_fall_salmon_2023 <- "" # analysis_id
skagit_fall_salmon_2024 <- "" # analysis_id

ids <- c(
  skagit_fall_salmon_2023,
  skagit_fall_salmon_2024
  )

# ---- USER ENTRY B ----
species <- c("Chinook")
life_stage <- c("Adult", "Jack")
fin_mark <- c("AD", "UM")
fate <- c("Released")

est_category <- "catch"
est_type <- c("quantile_50", "mean")

# Load packages
library(creelutils)
library(dplyr)
library(tidyr)

# Connect to Postgres database -----------------------------------------------------------------------------------------
con <- connect_creel_db()

# Query database lookup tables -----------------------------------------------------------------------------------------
project_lut <- fetch_db_table(conn=con, schema="creel", table = "project_lut") |> select(project_id, project_name)
fishery_lut <- fetch_db_table(conn=con, schema="creel", table = "fishery_lut")
fishery_lut_names <- fishery_lut |> select(fishery_id, fishery_name)

# fisheries names, sections, and associated CRC area code
fishery_manager <- fetch_db_table(conn=con, schema="creel", table = "vw_fishery_manager")
crc_sections <- fishery_manager |> 
  select(
    project_name, fishery_name, location_code, location_type, 
    section_num, catch_area_description, catch_area_code
  ) |> 
  filter(location_type == "Section") |> 
  # Drop out of scope projects
  filter_out(project_name %in% c("R5 Inland Fish", "Cowlitz", "CRM - Roving Creel Project", "District 12")) |> 
  filter_out(catch_area_code == "2-2") # Chehalis section without section num (Marine Area CRC 2-2)

# retain minimum columns for join, non-trim object has descriptive names for sections
crc_sections_trim <- crc_sections |> 
  select(project_name, fishery_name, section_num, catch_area_code)


# Query model estimates ------------------------------------------------------------------------------------------------
## Metadata table ----
analysis_lut <- fetch_db_table(
  conn=con, 
  schema="creel",
  table = "model_analysis_lut",
  filter = "analysis_id %in% ids"
  )

analysis_lut <- analysis_lut |> 
  left_join(project_lut, by = "project_id") |> 
  left_join(fishery_lut_names, by = "fishery_id") |> 
  relocate(analysis_id, project_name, fishery_name, data_grade, model_run_type)

## Total-scale ----
total <- fetch_db_table(
  conn=con,
  schema="creel",
  table = "vw_model_estimates_total",
  filter = "analysis_id %in% analysis_lut$analysis_id"
  )

### Filter catch ---- 
total <- total |> 
  filter(
    estimate_category %in% est_category,
    estimate_type %in% est_type,
    species_name %in% species,
    fate_name %in% fate,
    life_stage_name %in% life_stage,
    fin_mark_desc %in% fin_mark
  )

total$estimate_value <- round(total$estimate_value, 1)

## Stratum-scale ----
stratum <- fetch_db_table(
  conn=con,
  schema="creel",
  table = "vw_model_estimates_stratum",
  filter = "analysis_id %in% analysis_lut$analysis_id"
  )

stratum <- stratum |> 
  relocate(project_id, fishery_id, model_catch_group_id, .after = last_col())

### Filter catch ---- 
stratum <- stratum |> 
  filter(
    estimate_category %in% est_category,
    estimate_type %in% est_type,
    species_name %in% species,
    fate_name %in% fate,
    life_stage_name %in% life_stage,
    fin_mark_desc %in% fin_mark
  )

# Join CRC area code to each section
stratum <- stratum |> 
  left_join(crc_sections_trim, by = c("project_name", "fishery_name", "section_num"))

# Isolate each model type ----------------------------------------------------------------------------------------------
# formatting process is specific to each

## PE 
# pe_estimates <- stratum |> filter(model_type == "PE")
# pe_estimates <- pe_estimates |> filter(estimate_type == "total_catch") # PE value

## BSS 

bss_estimates <- stratum |> filter(model_type == "BSS")

# Format BSS -------------------------------------------------------------------------------------------

## get season-total median ----
keys <- c("project_name", "fishery_name", "species_name", "life_stage_name",
          "fin_mark_desc", "fate_name", "model_type", "estimate_category")

total_median <- total |>
  filter(model_type == "BSS", estimate_type == "quantile_50") |>
  select(all_of(keys), total_median = estimate_value)

## k-adjust daily means to season-total median ----
bss_estimates_fmt <- bss_estimates |>
  filter(estimate_type == "mean") |>
  left_join(total_median, by = keys) |>
  group_by(across(all_of(keys))) |>
  mutate(
    k = total_median / sum(estimate_value),
    adjusted_mean = estimate_value * k
  ) |>
  ungroup() |>
  select(
    fishery_name, species_name, life_stage_name, fin_mark_desc, fate_name,
    estimate_category, event_date = min_event_date, period, day_type,
    section_num, catch_area_code, angler_type = angler_type_name,
    daily_mean = estimate_value, k, adjusted_mean, total_median
  ) |>
  arrange(fishery_name, life_stage_name, fin_mark_desc, event_date, section_num, angler_type)

# check: should be ~0 for every catch group
bss_estimates_fmt |>
  group_by(species_name, life_stage_name, fin_mark_desc) |>
  summarise(diff = sum(adjusted_mean) - first(total_median), k = first(k), .groups = "drop")

# Save out csv file ----------------------------------------------------------------------------------------------------
bss_estimates_fmt |>
  mutate(across(c(daily_mean, adjusted_mean), \(x) round(x, 2)), k = round(k, 4)) |> # final rounding to tidy
  write.csv(file = here::here("d14_tamm_request_2026-11-03.csv"), row.names = FALSE)

# Format PE - weekly by CRC -------------------------------------------------------------------------------------------
# pe_estimates_fmt <- pe_estimates |>
#   group_by(
#     project_name,
#     fishery_name,
#     min_event_date,
#     max_event_date,
#     period,
#     species_name,
#     life_stage_name,
#     fin_mark_desc,
#     fate_name,
#     catch_area_code
#   ) |>
#   summarise(
#     estimated_catch = round(sum(estimate_value, na.rm = TRUE), 1),
#     .groups = "drop"
#   )
# 
# validate_total <- pe_estimates_fmt |> 
#   group_by(
#     project_name,
#     fishery_name,
#     species_name,
#     life_stage_name,
#     fin_mark_desc,
#     fate_name,
#   ) |>
#   summarise(
#     estimated_catch = sum(estimated_catch, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# # compare to total 
# total |> select(
#   project_name, fishery_name, species_name, life_stage_name, 
#   fate_name, fin_mark_desc, estimate_type, estimate_value
#   ) |> 
#   print(n=50)
# 
# print(validate_total, n = 50)
