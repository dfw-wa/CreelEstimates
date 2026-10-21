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
#   analysis/pst/outputs/02_multi_fishery_creel/multi_fishery_creel_trips.csv (PE angler hours)
#   analysis/pst/outputs/04_interview_proportions/all_interviews.rds (creel interview counts)
#   input_files/pst/lookup_tables/pst_river_block_crosswalk.csv
#   input_files/pst/lookup_tables/pst_month_gap_review.csv
#
# Output:
#   analysis/pst/outputs/05_assembly/pst_fw_skagit_cascade_fall_monthly.csv
#   analysis/pst/outputs/05_assembly/pst_fw_skagit_cascade_fall_monthly.png
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
                             "ratio, assigned to the fall fishery by the Aug-Dec window; guided trips ",
                             "are guide logbook reports on this area-month, not CRC"))))

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

# --- Context: creel interviews and PE angler hours per fall fishery-year ------
# Interviews: the fall salmon creels' interview count (sample size behind the
# creel estimates). PE hours: the same design-based PE run as the creel trips
# (multi_fishery_creel_trips.csv, monthly PE period, NA-area shadow rows
# dropped as in the assembly) - all anglers in the creel, whereas the trips
# are scaled to salmon-directed effort, so hours / trips is not a trip length.
int_path <- here("analysis", "pst", "outputs", "04_interview_proportions", "all_interviews.rds")
n_int <- NULL
if (file.exists(int_path)) {
  ints <- readRDS(int_path) |>
    filter(fishery_name %in% fall_xw$fall_fishery) |>
    mutate(date = suppressWarnings(as.Date(as.character(event_date))),
           year = as.integer(lubridate::year(date))) |>
    filter(!is.na(year))
  n_int <- if ("interview_id" %in% names(ints)) {
    ints |> group_by(fall_fishery = fishery_name, year) |>
      summarise(creel_interviews = n_distinct(interview_id), .groups = "drop")
  } else ints |> count(fall_fishery = fishery_name, year, name = "creel_interviews")
} else message(glue("{int_path} not found - no interview counts. ",
                    "Render analysis/pst/02_ingest/interview_proportions.qmd first."))

pe_path <- here("analysis", "pst", "outputs", "02_multi_fishery_creel", "multi_fishery_creel_trips.csv")
pe_hrs <- NULL
if (file.exists(pe_path)) {
  pe_hrs <- read_csv(pe_path, show_col_types = FALSE, col_types = cols(catch_area_code = "c")) |>
    filter(pe_period == "month", fishery_name %in% fall_xw$fall_fishery, !is.na(total_trips_est)) |>
    group_by(fishery_name, year, month, angler_final) |>
    filter(!(is.na(catch_area_code) & any(!is.na(catch_area_code)))) |>
    ungroup() |>
    group_by(fall_fishery = fishery_name, year = as.integer(year)) |>
    summarise(pe_angler_hours = round(sum(total_effort_hrs, na.rm = TRUE)), .groups = "drop")
} else message(glue("{pe_path} not found - no PE angler hours."))

fishery_totals <- out |> group_by(year, fall_fishery) |>
  summarise(creel_trips  = round(sum(trips[basis == "Creel (P1)"])),
            infill_trips = round(sum(trips[str_detect(basis, "^CRC infill")])),
            total_trips  = round(sum(trips)),
            infill_months = {
              i <- str_detect(basis, "^CRC infill")
              if (any(i)) paste(sort(unique(paste0(catch_area_code[i], ":", month[i]))),
                                collapse = " ") else ""
            },
            .groups = "drop") |>
  mutate(pct_infill = round(100 * infill_trips / pmax(total_trips, 1), 1))
fishery_totals <- if (is.null(pe_hrs)) mutate(fishery_totals, pe_angler_hours = NA_real_) else
  left_join(fishery_totals, pe_hrs, by = c("fall_fishery", "year"))
fishery_totals <- if (is.null(n_int)) mutate(fishery_totals, creel_interviews = NA_integer_) else
  left_join(fishery_totals, n_int, by = c("fall_fishery", "year"))

cat("\n=== Fall fishery totals: creel vs CRC infill, with PE angler hours and creel interviews ===\n")
fishery_totals |> as.data.frame() |> print(row.names = FALSE)
cat(glue("\nWrote {OUT}\n"))

# --- Figure -------------------------------------------------------------------
# Small multiples: columns = year; rows = angler trips, guided trips, salmon
# harvest, each on its own y-scale (trips and fish never share an axis). One
# bar per month, Skagit and Cascade stacked, creel at the bottom and CRC
# infill on top. Each bar carries its value; a CRC-infill-only bar is labelled
# "CRC <value>". Each panel shows its season total and CRC part; the
# angler-trips panels add the season's PE angler hours. Guided trips are
# guide logbook reports in every month, so the guided row is split by river
# only. The CSV and the fishery totals above are the table view. Palette:
# categorical slots 1-4 of the dataviz reference palette, validated (CVD
# ΔE 9.1; aqua and yellow under 3:1 on white, so infill bars are labelled
# and outlined).
PNG <- file.path(ASM, "pst_fw_skagit_cascade_fall_monthly.png")
RIVER <- c("830" = "Skagit", "826" = "Cascade")
FILL_COLS <- c("Skagit"                   = "#2a78d6",
               "Cascade"                  = "#eb6834",
               "Skagit, CRC infill month"  = "#1baf7a",
               "Cascade, CRC infill month" = "#eda100")
MEASURES <- c(trips = "Angler trips", guided_trips = "Guided trips", salmon_harvest = "Salmon harvest")

no_month <- out |> filter(is.na(month), trips > 0)
if (nrow(no_month) > 0)
  message(glue("[plot] {nrow(no_month)} creel row(s) with no month left out of the figure ",
               "({round(sum(no_month$trips))} trips)."))
plot_months <- sort(union(FALL_MONTHS, out$month[!is.na(out$month) & out$trips > 0]))

plot_df <- out |>
  filter(!is.na(month), basis != "No trips", catch_area_code %in% names(RIVER)) |>
  pivot_longer(all_of(names(MEASURES)), names_to = "measure", values_to = "value") |>
  mutate(infill  = str_detect(basis, "^CRC infill"),
         projected = basis == "CRC infill - projected CRC harvest (P3)",
         # Guided trips come from the guide logbook in every month (creel and
         # infill months alike), never from CRC, so that row is split by river only.
         infill  = infill & measure != "guided_trips",
         projected = projected & infill,
         segment = factor(if_else(infill, paste0(RIVER[catch_area_code], ", CRC infill month"),
                                  unname(RIVER[catch_area_code])), levels = names(FILL_COLS)),
         measure = factor(MEASURES[measure], levels = MEASURES),
         month   = factor(month.abb[month], levels = month.abb[plot_months])) |>
  group_by(year, measure, month, segment) |>
  summarise(value = sum(value), projected = any(projected), .groups = "drop")

# One label per bar stack: its value. A bar that is all CRC infill reads
# "CRC <value>" ("(proj.)" on a second line for projected 2025 harvest); a
# bar mixing creel and infill adds its CRC part in brackets.
stack_lab <- plot_df |>
  group_by(year, measure, month) |>
  summarise(bar_top = sum(value),
            crc = sum(value[str_detect(as.character(segment), "CRC")]),
            proj = any(projected[str_detect(as.character(segment), "CRC")]),
            .groups = "drop") |>
  filter(bar_top >= 0.5) |>
  mutate(label = case_when(
    crc >= bar_top - 0.5 ~ paste0("CRC ", scales::comma(bar_top, accuracy = 1),
                                  if_else(proj, "\n(proj.)", "")),
    crc >= 0.5 ~ paste0(scales::comma(bar_top, accuracy = 1), "\n(CRC ",
                        scales::comma(crc, accuracy = 1), ")"),
    TRUE ~ scales::comma(bar_top, accuracy = 1)))

# Season totals per panel; the angler-trips row adds PE angler hours for the
# fall creels that year (both rivers, as the bars stack both).
pe_year <- if (is.null(pe_hrs)) NULL else
  pe_hrs |> group_by(year) |> summarise(pe = sum(pe_angler_hours), .groups = "drop")
panel_tot <- plot_df |>
  group_by(measure, year) |>
  summarise(total = sum(value), crc = sum(value[str_detect(as.character(segment), "CRC")]), .groups = "drop")
panel_tot <- (if (is.null(pe_year)) mutate(panel_tot, pe = NA_real_) else
  left_join(panel_tot, pe_year, by = "year")) |>
  mutate(label = paste0("Total ", scales::comma(total, accuracy = 1),
                        if_else(crc >= 0.5, paste0("  (CRC infill ", scales::comma(crc, accuracy = 1), ")"), ""),
                        if_else(measure == MEASURES[["trips"]] & coalesce(pe, 0) > 0,
                                paste0("\n", scales::comma(pe, accuracy = 1), " angler hours (PE)"), "")))

fig <- ggplot(plot_df, aes(month, value, fill = segment)) +
  geom_col(width = 0.75, colour = "white", linewidth = 0.4, position = position_stack(reverse = TRUE)) +
  geom_text(data = stack_lab, aes(month, bar_top, label = label), inherit.aes = FALSE,
            vjust = -0.3, size = 2.5, lineheight = 0.9, colour = "grey15") +
  geom_text(data = panel_tot, aes(x = -Inf, y = Inf, label = label), inherit.aes = FALSE,
            hjust = -0.05, vjust = 1.3, size = 2.8, lineheight = 0.95, colour = "grey25") +
  facet_grid(measure ~ year, scales = "free_y", switch = "y") +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.4))) +
  scale_fill_manual(values = FILL_COLS, name = NULL, drop = TRUE) +
  labs(title = "Skagit and Cascade fall salmon: monthly estimates, 2022-2025",
       subtitle = paste0(
         "Fall window Aug-Dec. CRC infill = a month with CRC harvest but no creel on that catch area: ",
         "CRC harvest x trips-per-fish ratio.\n",
         "2025 is one composite creel (Skagit + Cascade), split here by catch area (830 Skagit, 826 Cascade). ",
         "Guided trips are guide logbook reports (a minimum).\n",
         "PE angler hours: all anglers in the fall creels; trips are scaled to salmon-directed effort."),
       x = NULL, y = NULL) +
  theme_classic(base_size = 10) +
  theme(legend.position = "top", legend.justification = "left",
        strip.placement = "outside", strip.background = element_blank(),
        strip.text.y.left = element_text(angle = 0, hjust = 1, colour = "grey20"),
        strip.text.x = element_text(face = "bold", colour = "grey20"),
        axis.text = element_text(colour = "grey35"),
        axis.line = element_line(colour = "grey60", linewidth = 0.3),
        axis.ticks = element_line(colour = "grey60", linewidth = 0.3),
        panel.spacing.y = unit(0.8, "lines"),
        plot.title.position = "plot",
        plot.subtitle = element_text(colour = "grey35", size = 8.5))
ggsave(PNG, fig, width = 11, height = 7.5, dpi = 200, bg = "white")
cat(glue("Wrote {PNG}\n"))
