# ==============================================================================
# interview_source_compare.R
#
# Purpose:
#   explore_creel_db_2022_2025.R showed creel.vw_interview carries 171,216
#   interviews for 2022-2025 against 82,482 in vw_analysis_interview - the only
#   view the pipeline has ever read. Lewis River (Mainstem / North Fork / East
#   Fork) exists ONLY in vw_interview. This script sizes what the extra ~89k
#   interviews are and whether they can inform bank/boat and guided splits on
#   P2 rivers - step 1-2 of the P2 interview plan:
#     1. what vw_interview has that the analysis view lacks (project, water
#        body, year), matched on interview_id;
#     2. whether the bank/boat, guided, angler count and target fields are
#        populated in those extra rows;
#     3. per water body (Columbia first), how many salmon-season interviews
#        carry a usable bank/boat answer - the eligibility screen.
#
#   Field names differ between views (e.g. water_body vs water_body_desc), so
#   every field is resolved from a list of candidates and the choice printed.
#
# Needs no DB: reads the cache explore_creel_db_2022_2025.R wrote.
#
# Output (analysis/pst/outputs/04_interview_proportions/):
#   interview_source_extra_by_project.csv   - rows only in vw_interview
#   interview_source_water_body_coverage.csv - water body x year field coverage
#
# Usage:
#   Rscript analysis/pst/03_analysis/interview_source_compare.R
# ==============================================================================

suppressMessages({
  library(tidyverse)
  library(here)
  library(glue)
})
options(width = 220)

CACHE   <- here(".cache", "creel_db_2022_2025")
OUT_DIR <- here("analysis", "pst", "outputs", "04_interview_proportions")
LUT     <- here("input_files", "pst", "lookup_tables", "crc_area_lut.csv")
MIN_LOCATED <- 20

rd <- function(n) {
  f <- file.path(CACHE, paste0(n, ".rds"))
  if (!file.exists(f)) stop(glue("{f} missing - run explore_creel_db_2022_2025.R first"), call. = FALSE)
  readRDS(f)
}
full <- rd("vw_interview")
ana  <- rd("vw_analysis_interview")

pick <- function(d, cands, label) {
  hit <- intersect(cands, names(d))[1]
  cat(sprintf("  %-16s -> %s\n", label, coalesce(hit, "(none)")))
  if (is.na(hit)) rep(NA_character_, nrow(d)) else as.character(d[[hit]])
}
na_str <- function(x) if_else(str_squish(coalesce(x, "")) %in% c("", "NA"), NA_character_, str_squish(x))

cat("=== vw_interview columns ===\n", paste(names(full), collapse = ", "), "\n\n")
cat("=== field resolution (vw_interview) ===\n")
f <- tibble(
  interview_id  = pick(full, c("interview_id", "id"), "interview_id"),
  event_date    = pick(full, c("event_date", "fishing_start_datetime"), "date"),
  project       = pick(full, c("project_name", "project_desc"), "project"),
  fishery       = pick(full, c("fishery_name"), "fishery"),
  water_body    = pick(full, c("water_body_desc", "water_body"), "water_body"),
  crc_area      = pick(full, c("crc_area", "catch_area_code", "crc_area_code"), "crc_area"),
  angler_type   = pick(full, c("angler_type", "angler_type_desc", "angler_type_code"), "angler_type"),
  boat_used     = pick(full, c("boat_used", "boat_used_desc"), "boat_used"),
  fish_from_boat= pick(full, c("fish_from_boat", "fish_from_boat_desc"), "fish_from_boat"),
  trip_guided   = pick(full, c("trip_guided", "trip_guided_desc"), "trip_guided"),
  angler_count  = pick(full, c("angler_count"), "angler_count"),
  target        = pick(full, c("target_species", "target_species_desc"), "target_species")
) |>
  mutate(across(everything(), na_str))

# Value vocabularies - the bank/boat derivation below assumes the analysis
# view's wording; check it holds for vw_interview before trusting the split.
cat("\n=== value vocabularies (vw_interview, 2022-2025) ===\n")
for (cl in c("angler_type", "boat_used", "fish_from_boat", "trip_guided")) {
  v <- f |> count(.data[[cl]], sort = TRUE) |> head(8)
  cat(sprintf("  %-15s %s\n", cl, paste0(coalesce(v[[1]], "NA"), " (", v$n, ")", collapse = " | ")))
}

lut <- read_csv(LUT, show_col_types = FALSE, name_repair = "unique_quiet") |>
  transmute(crc_area = as.character(catch_area_code), crc_region = catch_area_region) |>
  distinct(crc_area, .keep_all = TRUE)

yn <- function(x) case_when(str_detect(coalesce(x, ""), regex("^(y|yes|true|1)$", TRUE)) ~ "Yes",
                            str_detect(coalesce(x, ""), regex("^(n|no|false|0)$", TRUE)) ~ "No")
ints <- f |>
  mutate(
    date   = suppressWarnings(as.Date(substr(event_date, 1, 10))),
    year   = lubridate::year(date), month = lubridate::month(date),
    boat_yn = yn(boat_used),
    angler_final = case_when(
      str_detect(coalesce(angler_type, ""), regex("^bank", TRUE)) ~ "Bank",
      str_detect(coalesce(angler_type, ""), regex("^boat", TRUE)) ~ "Boat",
      boat_yn == "No" ~ "Bank",
      boat_yn == "Yes" & str_detect(coalesce(fish_from_boat, ""), regex("bank", TRUE)) ~ "Bank",
      boat_yn == "Yes" ~ "Boat"),
    guided = case_when(str_detect(coalesce(trip_guided, ""), regex("^guided|^y", TRUE)) ~ "Guided",
                       str_detect(coalesce(trip_guided, ""), regex("non|^n", TRUE)) ~ "Unguided"),
    anglers = suppressWarnings(as.numeric(angler_count)),
    anglers = if_else(is.na(anglers) | anglers <= 0, 1, anglers),
    in_analysis_view = interview_id %in% as.character(ana$interview_id)
  ) |>
  left_join(lut, by = "crc_area") |>
  filter(year %in% 2022:2025)

cat(glue("\n{nrow(ints)} vw_interview rows 2022-2025; {sum(ints$in_analysis_view)} also in ",
         "vw_analysis_interview, {sum(!ints$in_analysis_view)} only in vw_interview.\n\n"))

# ---- 1. What the analysis view is missing ------------------------------------
extra <- ints |> filter(!in_analysis_view)
by_proj <- extra |>
  group_by(project, has_fishery = !is.na(fishery)) |>
  summarise(interviews = n(), located = sum(!is.na(angler_final)),
            guided_flag = sum(!is.na(guided)), target = sum(!is.na(target)),
            water_bodies = n_distinct(water_body),
            top_water_bodies = paste(head(names(sort(table(water_body), decreasing = TRUE)), 4), collapse = " | "),
            years = paste(sort(unique(year)), collapse = ","), .groups = "drop") |>
  arrange(desc(interviews))
write_csv(by_proj, file.path(OUT_DIR, "interview_source_extra_by_project.csv"))
cat("=== interviews ONLY in vw_interview, by project ===\n")
print(as.data.frame(head(by_proj, 30)), row.names = FALSE)

# ---- 2/3. Coverage by water body ----------------------------------------------
cov <- ints |>
  group_by(crc_region, water_body, year) |>
  summarise(interviews = n(), only_in_vw_interview = sum(!in_analysis_view),
            located = sum(!is.na(angler_final)),
            pct_boat_angler = round(100 * sum(anglers[angler_final %in% "Boat"]) /
                                      max(sum(anglers[!is.na(angler_final)]), 1), 1),
            guided_flag = sum(!is.na(guided)),
            pct_guided = round(100 * sum(guided %in% "Guided") / max(sum(!is.na(guided)), 1), 1),
            target_answered = sum(!is.na(target) & target != "Target species not asked"),
            months = paste(sort(unique(month)), collapse = ","),
            crc_areas = paste(sort(unique(na.omit(crc_area))), collapse = "|"),
            .groups = "drop") |>
  arrange(crc_region, water_body, year)
write_csv(cov, file.path(OUT_DIR, "interview_source_water_body_coverage.csv"))

cat("\n=== Columbia/Snake water bodies, all years (located >= ", MIN_LOCATED, ") ===\n", sep = "")
col_rx <- regex("columbia|snake", ignore_case = TRUE)
cov |> filter(str_detect(coalesce(crc_region, ""), col_rx) |
                (is.na(crc_region) & str_detect(coalesce(water_body, ""),
                  regex("lewis river|kalama|washougal|cowlitz|toutle|elochoman|grays|wind r|klickitat|white salmon|drano|columbia", TRUE)))) |>
  group_by(crc_region, water_body) |>
  # pct first: summarise() evaluates in order, so `located` must not be
  # reassigned to its sum before the weighted mean reads it.
  summarise(pct_boat_angler = round(weighted.mean(pct_boat_angler, located), 1),
            interviews = sum(interviews), only_new = sum(only_in_vw_interview),
            located = sum(located),
            guided_flag = sum(guided_flag), target_answered = sum(target_answered),
            years = paste(sort(unique(year)), collapse = ","),
            crc_areas = paste(unique(crc_areas[crc_areas != ""]), collapse = "|"),
            .groups = "drop") |>
  filter(located >= MIN_LOCATED) |>
  arrange(desc(located)) |>
  as.data.frame() |> print(row.names = FALSE)

cat("\n=== Lewis River (water body match only - excludes Lewis St./Fort Lewis) by year x month ===\n")
ints |> filter(str_detect(coalesce(water_body, ""), regex("lewis river", TRUE))) |>
  group_by(water_body, year, month) |>
  summarise(n = n(), located = sum(!is.na(angler_final)),
            pct_boat = round(100 * sum(anglers[angler_final %in% "Boat"]) /
                               max(sum(anglers[!is.na(angler_final)]), 1)),
            guided_flag = sum(!is.na(guided)), target = sum(!is.na(target)),
            crc = paste(unique(na.omit(crc_area)), collapse = "|"), .groups = "drop") |>
  as.data.frame() |> print(row.names = FALSE)
