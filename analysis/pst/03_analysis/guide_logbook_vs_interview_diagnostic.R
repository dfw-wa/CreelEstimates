# ==============================================================================
# guide_logbook_vs_interview_diagnostic.R
# Location: analysis/pst/03_analysis/guide_logbook_vs_interview_diagnostic.R
#
# Purpose: a standalone DIAGNOSTIC - not part of the pipeline, not feeding any
# deliverable - answering one question before the guide logbook is wired into
# the analysis as a minimum bound on guided share:
#
#   Do the two independent measures of guided angler trips actually track each
#   other, river-year by river-year?
#
#     y = guide logbook guided angler trips (WDFW mandatory guide logbook,
#         a census of what licensed guides reported)
#     x = our own interview-proportion estimate of guided trips (design-based
#         creel trips x guided share from creel interviews)
#
# NEITHER IS TRUTH. Both carry their own bias, in opposite directions, and
# this script is a correspondence check, not a validation of either:
#   - the logbook is a NUMERATOR ONLY with imperfect reporting compliance, so
#     it under-counts guided trips by an unknown amount;
#   - the interview proportion is a sample-based share applied to a modeled
#     trip total, so it carries both sampling error and whatever selection
#     bias governs who gets interviewed (guides launching early and fishing
#     hard are plausibly under-sampled at an access point).
#
# One known unit bias on the x side has since been FIXED and is noted here so
# a stale run of this script is not misread: the interview proportion was
# originally the share of interview PARTIES that were guided, applied to an
# ANGLER-trip total. Because guided parties run larger than unguided ones,
# that understated guided trips - the bias ran in the opposite direction from
# the overshoot the first regression appeared to show. interview_proportions.qmd
# now exports an angler-weighted share (`prop_basis == "angler_weighted"`), so
# x is larger than it was in any run predating that change; re-render the
# producer before reading a slope off this script.
# A slope near 1 would be reassuring; a slope far from 1, or no relationship
# at all, means the logbook cannot be used as a floor without understanding
# why first.
#
# ------------------------------------------------------------------------------
# THE MONTH RESTRICTION IS THE WHOLE BALLGAME
#
# The logbook is calendar-year and species-agnostic: every trip a guide
# logged, salmon or steelhead, January to December. Our estimates are
# salmon-directed effort inside whatever season window the creel actually
# operated - Hoh fall salmon 2023 surveyed September and October, nothing
# else. Compared annually, a coastal river charges its entire winter
# steelhead guide season against a two-month fall salmon creel. Measured on
# the 2026-09-02 extract that produced a logbook "minimum" of 818 guided
# trips against a whole-season Hoh 2023 boat estimate of 292 - a 2.8x
# "violation" of a supposed floor that is really just two different
# fisheries being differenced.
#
# So this script compares ONLY within the months each river-year's creel
# actually ran, and requires the month-level logbook table to do it. There
# is deliberately no annual fallback: an annual comparison is not a weaker
# version of this diagnostic, it is a misleading one. The unrestricted
# annual figure IS carried through as a context column (lb_guided_annual) so
# the size of the restriction is visible, but nothing is fit to it.
#
# ------------------------------------------------------------------------------
# SCOPE: WHICH RIVER-YEARS CAN APPEAR AT ALL
#
# Only `creel_pe` rows carry mode_basis = "pending_track_b" - i.e. only
# rivers with this repo's own creel interview program can produce a guided
# share. Every district-external source (R1 Snake, R2 Upper Columbia, R3
# Hanford/Yakima/McNary, R4 Green-Duwamish) is mode_basis = "not_collected":
# those programs never collected a guided field, so they have no x-value and
# are correctly absent here, not missing data to chase.
#
# Inputs (all required - this script stops rather than half-run):
#   analysis/pst/outputs/07_guide_logbook/
#     guide_logbook_angler_trips_by_crc_year_month.csv   (parse_guide_logbook.R)
#   analysis/pst/outputs/05_assembly/pst_fw_trips_by_mode_location.csv
#     (pst_fw_angler_trips_assembly.R - needs interview_mode_location_props.csv
#      present, which needs interview_proportions.qmd rendered against the DB;
#      without it every row is mode = "unknown" and there is no x-axis)
#   analysis/pst/outputs/02_multi_fishery_creel/multi_fishery_creel_trips.csv
#     (multi_fishery_creel_summary.R - supplies the per-CRC month coverage
#      that defines each river-year's season window)
#
# Outputs:
#   analysis/pst/outputs/08_guide_logbook_diagnostic/
#     guide_logbook_vs_interview_river_year.csv  - the paired points + residuals
#     guide_logbook_vs_interview_fit.csv         - slope/intercept/R2/n per fit
#     guide_logbook_vs_interview_scatter.png     - the plot
#
# How to run (after the three inputs above exist):
#   Rscript analysis/pst/03_analysis/guide_logbook_vs_interview_diagnostic.R
# ==============================================================================

library(tidyverse)
library(here)
library(glue)

GUIDE_LOGBOOK_DIR <- here("analysis", "pst", "outputs", "07_guide_logbook")
ASSEMBLY_DIR      <- here("analysis", "pst", "outputs", "05_assembly")
CREEL_DIR         <- here("analysis", "pst", "outputs", "02_multi_fishery_creel")
OUT_DIR           <- here("analysis", "pst", "outputs", "08_guide_logbook_diagnostic")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

LOGBOOK_MONTH_PATH <- file.path(GUIDE_LOGBOOK_DIR,
                                "guide_logbook_angler_trips_by_crc_year_month.csv")
LOGBOOK_YEAR_PATH  <- file.path(GUIDE_LOGBOOK_DIR,
                                "guide_logbook_angler_trips_by_crc_year.csv")
EFFORT_PATH        <- file.path(ASSEMBLY_DIR, "pst_fw_trips_by_mode_location.csv")
CREEL_PATH         <- file.path(CREEL_DIR, "multi_fishery_creel_trips.csv")

# ---- 0. Required inputs -------------------------------------------------------
# Stop, don't degrade. A diagnostic that silently runs on two of three inputs
# produces a fit nobody can interpret.

require_input <- function(path, produced_by) {
  if (!file.exists(path)) {
    stop(glue(
      "Required input not found:\n  {path}\n",
      "Produced by: {produced_by}\n",
      "This diagnostic does not run without it."
    ), call. = FALSE)
  }
  path
}

require_input(LOGBOOK_MONTH_PATH, paste(
  "analysis/pst/02_ingest/parse_guide_logbook.R (needs the guide logbook RDS",
  "extract, which is not committed - run on a machine that has it)"))
require_input(EFFORT_PATH, paste(
  "analysis/pst/03_analysis/pst_fw_angler_trips_assembly.R, which in turn needs",
  "interview_mode_location_props.csv from interview_proportions.qmd (DB access)"))
require_input(CREEL_PATH, "analysis/pst/02_ingest/multi_fishery_creel_summary.R (DB access)")

logbook_month <- read_csv(LOGBOOK_MONTH_PATH, show_col_types = FALSE) |>
  mutate(crc_code = as.character(crc_code))
effort <- read_csv(EFFORT_PATH, show_col_types = FALSE)
creel  <- read_csv(CREEL_PATH, show_col_types = FALSE) |>
  mutate(catch_area_code = as.character(catch_area_code))

logbook_year <- if (file.exists(LOGBOOK_YEAR_PATH)) {
  read_csv(LOGBOOK_YEAR_PATH, show_col_types = FALSE) |>
    mutate(crc_code = as.character(crc_code))
} else NULL

# ---- 1. Our side: interview-proportion guided trips, per river-year -----------
# river_label is the grain our estimates are actually published at, and it is
# the grain the logbook has to be aggregated UP to - several rivers are a
# single creel over many CRC areas (Quillayute = 398|400|402|404|406).
#
# catch_area_codes is read per river-YEAR, not per river: the CRC set behind
# one river genuinely moves between years as creel coverage changes (Skagit is
# 830 in 2022-23 and 826|830 in 2024-25; Chehalis is 317 then 315|317;
# Snohomish moves across 844|850|852). Keying the logbook aggregation off a
# fixed river->CRC map would silently mis-attribute those years.

split_codes <- function(x) {
  sort(unique(unlist(strsplit(x[!is.na(x) & x != ""], "\\|"))))
}

ours <- effort |>
  filter(tier == "P1", str_detect(source_id, "creel_pe")) |>
  group_by(block, river_label, year) |>
  summarise(
    our_guided   = sum(angler_trips[mode == "guided"], na.rm = TRUE),
    our_boat     = sum(angler_trips[location == "boat"], na.rm = TRUE),
    our_total    = sum(angler_trips, na.rm = TRUE),
    crc_set      = paste(split_codes(catch_area_codes), collapse = "|"),
    .groups = "drop"
  ) |>
  # A river-year with no guided rows at all resolved to mode = "unknown"
  # rather than to a guided share of zero - that is an absent x-value, not a
  # measured zero, and fitting to it would be inventing data. [R3]
  filter(our_guided > 0)

if (nrow(ours) == 0) {
  stop(paste(
    "No creel_pe river-years carry mode = 'guided'. Every row resolved to",
    "mode = 'unknown', which means apply_track_b() never matched",
    "interview_mode_location_props.csv. Re-render interview_proportions.qmd",
    "and re-run the assembly before this diagnostic."
  ), call. = FALSE)
}

# ---- 2. Season window: the months each river-year's creel actually ran -------
# Taken from the creel table's own catch_area_code x month coverage, not from a
# season assumption. A river-year's window is the union of months surveyed
# across any CRC area in that river-year's own set.

creel_months <- creel |>
  filter(!is.na(month), !is.na(catch_area_code)) |>
  distinct(catch_area_code, year, month)

season_window <- ours |>
  select(block, river_label, year, crc_set) |>
  rowwise() |>
  mutate(
    months = list(sort(unique(
      creel_months$month[creel_months$catch_area_code %in% strsplit(crc_set, "\\|")[[1]] &
                           creel_months$year == year]
    )))
  ) |>
  ungroup()

n_no_window <- sum(lengths(season_window$months) == 0)
if (n_no_window > 0) {
  message(glue(
    "[note] {n_no_window} river-year(s) have a guided estimate but no month ",
    "coverage in multi_fishery_creel_trips.csv - they cannot be month-restricted ",
    "and are dropped from the fit rather than compared annually."
  ))
}

# ---- 3. Logbook side, restricted to that window ------------------------------

paired <- season_window |>
  rowwise() |>
  mutate(
    codes            = list(strsplit(crc_set, "\\|")[[1]]),
    season_months    = paste(months, collapse = ","),
    n_season_months  = length(months),
    lb_guided = sum(logbook_month$angler_trips[
      logbook_month$crc_code %in% codes &
        logbook_month$trip_year == year &
        logbook_month$trip_month %in% months
    ], na.rm = TRUE),
    lb_guided_annual = if (is.null(logbook_year)) NA_real_ else sum(
      logbook_year$angler_trips[
        logbook_year$crc_code %in% codes & logbook_year$trip_year == year
      ], na.rm = TRUE)
  ) |>
  ungroup() |>
  select(-codes, -months) |>
  filter(n_season_months > 0) |>
  left_join(ours, by = c("block", "river_label", "year", "crc_set")) |>
  mutate(
    across(c(our_guided, our_boat, our_total), ~ round(.x)),
    # Ratio of the two guided measures. > 1 means the logbook - a
    # numerator-only census with imperfect compliance - still exceeded our
    # interview-based estimate, which a floor interpretation cannot explain
    # away and is the single most informative column here.
    lb_over_ours     = round(lb_guided / pmax(our_guided, 1), 3),
    our_guided_share = round(our_guided / pmax(our_total, 1), 3),
    lb_share_of_total = round(lb_guided / pmax(our_total, 1), 3),
    pct_of_annual_kept = if_else(
      !is.na(lb_guided_annual) & lb_guided_annual > 0,
      round(100 * lb_guided / lb_guided_annual, 1), NA_real_
    )
  ) |>
  arrange(block, river_label, year)

# ---- 4. Fits ------------------------------------------------------------------
# Two denominators, because "guided" means different things on each side. The
# logbook counts client angler-trips on guide boats; our guided estimate is a
# share of ALL trips, bank included. Guides are boat-based, so the boat-only
# comparison is the closer like-for-like - it is reported alongside, not
# instead of, the all-mode fit.

fit_one <- function(df, xcol, label) {
  x <- df[[xcol]]; y <- df$lb_guided
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3 || length(unique(x)) < 2) {
    return(tibble(fit = label, n = length(x), slope = NA_real_,
                  intercept = NA_real_, r_squared = NA_real_, p_value = NA_real_,
                  note = "too few distinct points to fit"))
  }
  m <- lm(y ~ x)
  s <- summary(m)
  tibble(
    fit       = label,
    n         = length(x),
    slope     = round(unname(coef(m)[2]), 3),
    intercept = round(unname(coef(m)[1]), 1),
    r_squared = round(s$r.squared, 3),
    p_value   = signif(unname(s$coefficients[2, 4]), 3),
    note      = if (length(x) < 10) "n < 10 - treat as indicative only" else NA_character_
  )
}

fits <- bind_rows(
  fit_one(paired, "our_guided", "logbook_guided ~ interview_guided (all modes)"),
  fit_one(paired, "our_boat",   "logbook_guided ~ our_boat_trips (boat-only denominator)")
)

# ---- 5. Report ----------------------------------------------------------------

cat("\n=== Guide logbook vs. interview-proportion guided trips ===\n")
cat(glue("Month-restricted to each river-year's own creel season window.\n",
         "{nrow(paired)} paired river-years.\n\n"), "\n")

print(as.data.frame(paired |> select(
  block, river_label, year, crc_set, season_months,
  our_guided, lb_guided, lb_over_ours, lb_guided_annual, pct_of_annual_kept
)), row.names = FALSE)

cat("\n=== Fits ===\n")
print(as.data.frame(fits), row.names = FALSE)

cat("\n=== lb_guided / our_guided ===\n")
print(summary(paired$lb_over_ours))

violations <- paired |> filter(lb_guided > our_guided)
cat(glue("\n{nrow(violations)} of {nrow(paired)} river-years have logbook guided ",
        "trips ABOVE our interview-based estimate.\n"))
if (nrow(violations) > 0) {
  cat("A floor cannot exceed the quantity it bounds - these are the rows to\n",
      "understand before the logbook is used as a minimum:\n", sep = "")
  print(as.data.frame(violations |> select(river_label, year, our_guided,
                                           lb_guided, lb_over_ours)),
        row.names = FALSE)
}

# ---- 6. Plot ------------------------------------------------------------------

p <- ggplot(paired, aes(x = our_guided, y = lb_guided)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "#2c7fb8", fill = "#2c7fb8", alpha = 0.15) +
  geom_point(aes(colour = block), size = 2.5) +
  ggrepel::geom_text_repel(aes(label = paste0(river_label, " ", year)),
                           size = 2.6, max.overlaps = 20, seed = 1) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Guide logbook vs. interview-proportion guided angler trips",
    subtitle = glue("Month-restricted to each river-year's creel season window | ",
                    "n = {nrow(paired)} | dashed line = 1:1"),
    x = "Our estimate: creel trips x interview guided share",
    y = "Guide logbook guided angler trips",
    colour = "Block",
    caption = "Neither axis is truth: the logbook under-counts (compliance), the interview share carries sampling and access-point selection bias."
  ) +
  theme_minimal(base_size = 11)

plot_path <- file.path(OUT_DIR, "guide_logbook_vs_interview_scatter.png")
suppressWarnings(ggsave(plot_path, p, width = 9, height = 6.5, dpi = 150))

# ---- 7. Write -----------------------------------------------------------------

paired_path <- file.path(OUT_DIR, "guide_logbook_vs_interview_river_year.csv")
fits_path   <- file.path(OUT_DIR, "guide_logbook_vs_interview_fit.csv")
write_csv(paired, paired_path)
write_csv(fits, fits_path)

cat(glue("\nWrote {paired_path}\n",
         "Wrote {fits_path}\n",
         "Wrote {plot_path}\n"), "\n")
