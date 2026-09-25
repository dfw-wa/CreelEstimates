# ==============================================================================
# interview_river_boat_share.R
#
# Purpose:
#   P2/P3 rows are annual CRC-harvest expansions with no bank/boat field. Until
#   now they took a creel design ratio from their river if it had a P1 creel,
#   else their block, else all creels (0.337 for most Lower Columbia rivers).
#   creel.vw_interview holds 2022-2025 interviews on rivers with no P1 creel
#   in the pipeline - Lewis (3,648 NF + 632 mainstem), Kalama, Wind, Klickitat
#   (the CRM - Tribs project) - that never reach vw_analysis_interview.
#   This turns them into a per-river bank/boat share for the categorize stage.
#
# Method:
#   1. Interviews = vw_analysis_interview (explorer cache) plus the
#      vw_interview rows it lacks, 2022-2025, deduplicated on interview_id.
#      The analysis-view record wins: vw_interview has no boat_used field.
#   2. water body -> PST river_label via interview_water_body_river_map.csv
#      (editable), then exact name match against the crosswalk.
#   3. Bank/boat: angler_type_code Boat/Bank (fish_from_boat BK -> Bank); the
#      analysis view's boat_used route for rows only there. Angler-weighted.
#   4. Monthly boat share per river: river x year x month where >= MIN_MONTH
#      located interviews, else river x month pooled over 2022-2025.
#   5. One share per river-year = those monthly shares weighted by the
#      river's CRC salmon harvest by month (2022-2024 pooled profile - 2025
#      CRC is Jan-Mar only). The P2/P3 rows it applies to are CRC-harvest
#      expansions, so the months salmon are caught set the mix - which also
#      does the salmon-vs-steelhead filtering (a winter-steelhead boat month
#      with no CRC salmon gets no weight).
#   6. Kept only where interviews cover >= MIN_WEIGHT_COVERAGE of that CRC
#      harvest weight and >= MIN_LOCATED located interviews sit in those
#      months. River-year first; river (years pooled) as fallback.
#
# Inputs:
#   .cache/creel_db_2022_2025/vw_interview.rds   (explore_creel_db_2022_2025.R)
#   .cache/creel_db_2022_2025/vw_analysis_interview.rds (optional; the explorer's
#     copy - NOT all_interviews.rds, whose creelutils-pulled columns segfaulted
#     R on read here, 2026-09-25)
#   analysis/pst/outputs/01_crc_harvest/crc_freshwater_harvest_2010_2024_tidy.csv
#   input_files/pst/lookup_tables/{pst_river_block_crosswalk,
#     interview_water_body_river_map}.csv
#
# Output (analysis/pst/outputs/04_interview_proportions/):
#   interview_boat_share_river_year.csv   - read by pst_categorize_mode_location.R
#   interview_boat_share_river_month.csv  - the monthly shares behind it
#
# Usage:
#   Rscript analysis/pst/03_analysis/interview_river_boat_share.R
# ==============================================================================

suppressMessages({
  library(tidyverse)
  library(here)
  library(glue)
})
options(width = 220)

YEARS               <- 2022:2025
CRC_PROFILE_YEARS   <- 2022:2024
MIN_MONTH           <- 10     # located interviews for a river-year-month share
MIN_LOCATED         <- 100    # located interviews behind a river(-year) share.
                              # Raised from 20 (2026-09-25). (The 41-interview
                              # Nisqually share that prompted it was a bug - see
                              # section 1 - not a real thin sample.)
MIN_WEIGHT_COVERAGE <- 0.5    # share of CRC salmon weight with a monthly share

CACHE   <- here(".cache", "creel_db_2022_2025")
OUT_DIR <- here("analysis", "pst", "outputs", "04_interview_proportions")
LUTDIR  <- here("input_files", "pst", "lookup_tables")
CRC_CSV <- here("analysis", "pst", "outputs", "01_crc_harvest",
                "crc_freshwater_harvest_2010_2024_tidy.csv")

step <- function(msg) { message(format(Sys.time(), "%H:%M:%S"), "  ", msg); flush.console() }

na_str <- function(x) if_else(str_squish(coalesce(as.character(x), "")) %in% c("", "NA"),
                              NA_character_, str_squish(as.character(x)))
getcol <- function(d, nm) if (nm %in% names(d)) na_str(d[[nm]]) else rep(NA_character_, nrow(d))

# ---- 1. Interviews -----------------------------------------------------------
vw_path <- file.path(CACHE, "vw_interview.rds")
if (!file.exists(vw_path)) stop(glue("{vw_path} missing - run explore_creel_db_2022_2025.R"), call. = FALSE)
step("reading vw_interview cache")
vw <- readRDS(vw_path)
step(glue("vw_interview: {nrow(vw)} rows"))
vw_int <- tibble(
  interview_id = getcol(vw, "interview_id"),
  event_date   = getcol(vw, "event_date"),
  water_body   = getcol(vw, "water_body_desc"),
  angler_type  = getcol(vw, "angler_type_code"),
  fish_from_boat = getcol(vw, "fish_from_boat"),
  boat_used    = NA_character_,
  angler_count = getcol(vw, "angler_count"),
  source       = "vw_interview")

ana_path <- file.path(CACHE, "vw_analysis_interview.rds")
ana_int <- if (file.exists(ana_path)) {
  step("reading vw_analysis_interview cache")
  a <- readRDS(ana_path)
  tibble(interview_id = getcol(a, "interview_id"), event_date = getcol(a, "event_date"),
         water_body = getcol(a, "water_body"), angler_type = getcol(a, "angler_type"),
         fish_from_boat = getcol(a, "fish_from_boat"), boat_used = getcol(a, "boat_used"),
         angler_count = getcol(a, "angler_count"), source = "vw_analysis_interview")
} else NULL
# An interview in both views keeps its ANALYSIS-view record: vw_interview has
# no boat_used column, and newer creels (Nisqually, most Puget Sound) record
# bank/boat only there - taking the vw_interview copy dropped them (Nisqually
# fell from 1k+ interviews to the 41 carrying angler_type_code, 2026-09-25).
# vw_interview contributes only the interviews the analysis view lacks
# (CRM - Tribs, Lewis, Kalama, Wind, Klickitat...).
if (!is.null(ana_int)) vw_int <- vw_int |> filter(!interview_id %in% ana_int$interview_id)

rm(vw); if (exists("a")) rm(a); invisible(gc())
step("deriving bank/boat")
ints <- bind_rows(vw_int, ana_int) |>
  mutate(
    date  = suppressWarnings(as.Date(substr(event_date, 1, 10))),
    year  = lubridate::year(date), month = lubridate::month(date),
    location = case_when(
      str_detect(coalesce(angler_type, ""), regex("^boat", TRUE)) &
        coalesce(fish_from_boat, "") %in% c("BK", "Bank") ~ "Bank",
      str_detect(coalesce(angler_type, ""), regex("^boat", TRUE)) ~ "Boat",
      str_detect(coalesce(angler_type, ""), regex("^bank", TRUE)) ~ "Bank",
      boat_used == "No" ~ "Bank",
      boat_used == "Yes" & coalesce(fish_from_boat, "") %in% c("BK", "Bank") ~ "Bank",
      boat_used == "Yes" ~ "Boat"),
    anglers = suppressWarnings(as.numeric(angler_count)),
    anglers = if_else(is.na(anglers) | anglers <= 0, 1, anglers)
  ) |>
  filter(year %in% YEARS, !is.na(month), !is.na(location))
cat(glue("{nrow(ints)} located interviews 2022-2025 ",
         "({sum(ints$source != 'vw_interview')} analysis view, ",
         "{sum(ints$source == 'vw_interview')} vw_interview only)\n\n"))

# ---- 2. Water body -> river_label --------------------------------------------
step("mapping water bodies to rivers")
cw <- read_csv(file.path(LUTDIR, "pst_river_block_crosswalk.csv"), show_col_types = FALSE)
wb_map <- read_csv(file.path(LUTDIR, "interview_water_body_river_map.csv"),
                   show_col_types = FALSE) |> select(water_body, river_label)
# Automatic matches beyond the hand-kept map: same name once "River"/"R."/
# punctuation are dropped and word order ignored ("Skagit River" -> Skagit,
# "North Fork Nooksack River" -> "Nooksack River, North Fork"). Fork/reach
# words are kept, so a mainstem never lands on a fork. Only unique matches.
norm_name <- function(x) {
  x |> str_to_lower() |> str_remove_all("\\([^)]*\\)") |>
    str_replace_all("\\br\\.|\\briver\\b|[^a-z ]", " ") |> str_squish() |>
    map_chr(\(t) paste(sort(unique(strsplit(t, " ")[[1]])), collapse = " "))
}
cw_norm <- tibble(river_label = unique(cw$river_label)) |>
  filter(!is.na(river_label)) |> mutate(key = norm_name(river_label)) |>
  group_by(key) |> filter(n() == 1) |> ungroup()
auto <- tibble(water_body = unique(na.omit(ints$water_body))) |>
  filter(!water_body %in% wb_map$water_body) |>
  mutate(key = norm_name(water_body)) |>
  inner_join(cw_norm, by = "key") |> select(water_body, river_label)
cat("=== automatic water body -> river matches ===\n")
print(as.data.frame(auto), row.names = FALSE)
wb_map <- bind_rows(wb_map, auto) |> distinct()

mapped <- ints |> inner_join(wb_map, by = "water_body", relationship = "many-to-many")
cat("=== interviews mapped to PST rivers ===\n")
mapped |> count(river_label, name = "located") |> arrange(desc(located)) |>
  as.data.frame() |> print(row.names = FALSE)
unmapped <- ints |> anti_join(wb_map, by = "water_body") |> count(water_body, sort = TRUE)
cat(glue("\n{sum(unmapped$n)} located interviews on {nrow(unmapped)} water bodies not mapped ",
         "(mainstem Columbia, lakes, rivers outside the crosswalk). Largest: ",
         "{paste(head(glue('{unmapped$water_body} ({unmapped$n})'), 8), collapse = '; ')}\n\n"))

# ---- 3. Monthly shares --------------------------------------------------------
step("monthly shares")
share_at <- function(d, ...) {
  d |> group_by(...) |>
    summarise(located = n(),
              p_boat = sum(anglers[location == "Boat"]) / sum(anglers), .groups = "drop")
}
m_ym <- share_at(mapped, river_label, year, month) |> filter(located >= MIN_MONTH)
m_m  <- share_at(mapped, river_label, month)       |> filter(located >= MIN_MONTH)

# ---- 4. CRC salmon profile by river x month ------------------------------------
step("CRC salmon profile")
if (!file.exists(CRC_CSV)) stop(glue("{CRC_CSV} missing - run parse_crc_freshwater_harvest.R"), call. = FALSE)
river_codes <- cw |> filter(!is.na(crc_areas), crc_areas != "") |>
  distinct(river_label, crc_areas) |>
  separate_longer_delim(crc_areas, "|") |>
  transmute(river_label, stream_code = as.character(crc_areas)) |> distinct()
crc_prof <- read_csv(CRC_CSV, show_col_types = FALSE) |>
  filter(calendar_year %in% CRC_PROFILE_YEARS, !is.na(calendar_month)) |>
  transmute(stream_code = as.character(stream_code), month = as.integer(calendar_month),
            harvest = as.numeric(harvest_count)) |>
  inner_join(river_codes, by = "stream_code", relationship = "many-to-many") |>
  group_by(river_label, month) |>
  summarise(w = sum(harvest, na.rm = TRUE), .groups = "drop") |>
  filter(w > 0)

# ---- 5. River-year and river shares --------------------------------------------
step("river-year shares")
rivers <- intersect(unique(mapped$river_label), unique(crc_prof$river_label))
grid_y <- crc_prof |> filter(river_label %in% rivers) |> crossing(year = YEARS)

ry <- grid_y |>
  left_join(m_ym |> rename(p_ym = p_boat, n_ym = located), by = c("river_label", "year", "month")) |>
  left_join(m_m  |> rename(p_m = p_boat,  n_m = located),  by = c("river_label", "month")) |>
  mutate(p = coalesce(p_ym, p_m), n = if_else(!is.na(p_ym), n_ym, n_m),
         src = case_when(!is.na(p_ym) ~ "ym", !is.na(p_m) ~ "m")) |>
  group_by(river_label, year) |>
  summarise(p_boat = sum(w[!is.na(p)] * p[!is.na(p)]) / sum(w[!is.na(p)]),
            weight_coverage = sum(w[!is.na(p)]) / sum(w),
            n_located = sum(n, na.rm = TRUE),
            months_own_year = paste(month[src %in% "ym"], collapse = ","),
            months_pooled = paste(month[src %in% "m"], collapse = ","),
            .groups = "drop") |>
  mutate(level = "river-year")

rp <- crc_prof |> filter(river_label %in% rivers) |>
  left_join(m_m, by = c("river_label", "month")) |>
  group_by(river_label) |>
  summarise(weight_coverage = sum(w[!is.na(p_boat)]) / sum(w),
            n_located = sum(located, na.rm = TRUE),
            months_pooled = paste(month[!is.na(p_boat)], collapse = ","),
            p_boat = sum(w[!is.na(p_boat)] * p_boat[!is.na(p_boat)]) / sum(w[!is.na(p_boat)]),
            .groups = "drop") |>
  mutate(level = "river", year = NA_integer_)

out <- bind_rows(ry, rp) |>
  mutate(usable = weight_coverage >= MIN_WEIGHT_COVERAGE & n_located >= MIN_LOCATED &
           !is.nan(p_boat),
         across(c(p_boat, weight_coverage), ~ round(.x, 4))) |>
  select(river_label, level, year, p_boat, usable, n_located, weight_coverage,
         months_own_year, months_pooled) |>
  arrange(river_label, level, year)

write_csv(out, file.path(OUT_DIR, "interview_boat_share_river_year.csv"))
write_csv(bind_rows(m_ym |> mutate(level = "river-year-month"),
                    m_m |> mutate(level = "river-month (2022-2025 pooled)")) |>
            mutate(p_boat = round(p_boat, 4)),
          file.path(OUT_DIR, "interview_boat_share_river_month.csv"))

cat("=== rivers mapped but with no CRC salmon profile (no share possible) ===\n")
cat(" ", paste(setdiff(unique(mapped$river_label), unique(crc_prof$river_label)), collapse = "; "), "\n\n")
cat("=== interview boat share, CRC-salmon-weighted (usable rows are applied) ===\n")
out |> select(-months_own_year) |> as.data.frame() |> print(row.names = FALSE)
cat(glue("\nWrote {file.path(OUT_DIR, 'interview_boat_share_river_year.csv')}\n"))
