# ==============================================================================
# inspect_track_b.R
#
# Purpose:
#   Read-only inspection of what Track B did, after interview_proportions.qmd
#   and pst_fw_angler_trips_assembly.R have both been re-run. Computes nothing
#   the pipeline does not already compute - it only reads the committed outputs
#   and answers the four questions worth asking after the angler-weighting and
#   month-grain changes:
#
#     1. Did the angler weighting apply, or did cells fall back to party counts?
#     2. Which tier actually served the trips (month / fishery-year / block)?
#     3. Did the month grain move the guided totals, and for which rivers?
#     4. Do the trips still conserve, and did the previously deleted guided
#        rows come back?
#
# Usage:
#   Rscript analysis/pst/03_analysis/inspect_track_b.R
#   No DB access needed - reads only CSVs under analysis/pst/outputs/.
# ==============================================================================

suppressMessages({
  library(tidyverse)
  library(here)
  library(glue)
})

options(width = 200)

PROPS_DIR <- here("analysis", "pst", "outputs", "04_interview_proportions")
ASM_DIR   <- here("analysis", "pst", "outputs", "05_assembly")

rd <- function(path, what) {
  if (!file.exists(path)) {
    cat(sprintf("\n!! MISSING: %s\n   expected at %s\n", what, path))
    return(NULL)
  }
  suppressMessages(read_csv(path, show_col_types = FALSE))
}

props   <- rd(file.path(PROPS_DIR, "interview_mode_location_props.csv"), "annual proportions")
pmonth  <- rd(file.path(PROPS_DIR, "interview_mode_location_props_month.csv"), "month proportions")
effort  <- rd(file.path(ASM_DIR, "pst_fw_effort_long.csv"), "assembled effort")
mva     <- rd(file.path(ASM_DIR, "pst_fw_track_b_month_vs_annual.csv"), "month vs annual diagnostic")
gaps    <- rd(file.path(ASM_DIR, "pst_fw_gap_register.csv"), "gap register")

# ---- 1. Did the angler weighting apply? --------------------------------------

cat("\n================ 1. PROPORTION BASIS ================\n")
if (!is.null(props)) {
  if (!"prop_basis" %in% names(props)) {
    cat("!! props table has no prop_basis column - it predates the angler-weighted\n",
        "   fix. Re-render analysis/pst/02_ingest/interview_proportions.qmd.\n", sep = "")
  } else {
    props |>
      filter(!is.na(prop_basis)) |>
      distinct(fishery_name, year, location, prop_basis) |>
      count(prop_basis, name = "cells") |>
      mutate(pct = round(100 * cells / sum(cells), 1)) |>
      as.data.frame() |> print()

    cat("\n-- Largest angler-vs-party shifts on guided rows (prop_party_delta) --\n")
    props |>
      filter(mode == "guided", n_location >= 30, !is.na(prop_party_delta)) |>
      transmute(fishery_name, year, location,
                n_boat = n_location,
                party = round(prop_parties, 3),
                angler = round(prop, 3),
                shift = round(prop_party_delta, 3),
                ratio = round(prop / prop_parties, 2)) |>
      arrange(desc(abs(shift))) |> head(12) |> as.data.frame() |> print()
  }
}

# ---- 2. Which tier served the trips? -----------------------------------------

cat("\n================ 2. TIER MIX ================\n")
if (!is.null(effort)) {
  effort |>
    filter(tier == "P1") |>
    group_by(mode_basis) |>
    summarise(angler_trips = round(sum(angler_trips, na.rm = TRUE)), .groups = "drop") |>
    mutate(pct = round(100 * angler_trips / sum(angler_trips), 1)) |>
    arrange(desc(angler_trips)) |> as.data.frame() |> print()
}
if (!is.null(pmonth)) {
  cat(glue("\nMonth-grain cells at or above n_location 30: ",
           "{sum(distinct(pmonth, fishery_name, year, month_num, location, n_location)$n_location >= 30)}",
           " of {nrow(distinct(pmonth, fishery_name, year, month_num, location))}\n"))
}

# ---- 3. Did the month grain move anything? -----------------------------------

cat("\n================ 3. MONTH VS FLAT ANNUAL (guided trips) ================\n")
if (!is.null(mva)) {
  cat(glue("Strata where the month tier changed the guided total: ",
           "{sum(abs(mva$guided_delta) > 1, na.rm = TRUE)} of {nrow(mva)}\n"))
  cat(glue("Net change in guided angler trips: ",
           "{round(sum(mva$guided_delta, na.rm = TRUE))}\n\n"))
  mva |>
    transmute(fishery_name, year, location,
              flat_annual = round(guided_flat_annual),
              applied     = round(guided_applied),
              delta       = round(guided_delta),
              pct         = round(pct_change, 1),
              tiers) |>
    head(15) |> as.data.frame() |> print()
}

# ---- 4. Conservation, and the guided rows that used to vanish ----------------

cat("\n================ 4. CONSERVATION & RECOVERED GUIDED ROWS ================\n")
if (!is.null(gaps)) {
  blockers <- gaps |> filter(severity == "blocker")
  if (nrow(blockers) > 0) {
    cat("!! BLOCKERS in the gap register:\n")
    blockers |> select(source_id, detail) |> as.data.frame() |> print()
  } else {
    cat("No blockers in the gap register (the Track B conservation check passed).\n")
  }
  tb <- gaps |> filter(str_detect(source_id, "track_b"))
  if (nrow(tb) > 0) {
    cat("\n-- Track B notes --\n")
    tb |> select(severity, detail) |> as.data.frame() |> print()
  }
}

# The cells the old per-mode threshold deleted: a well-sampled cell whose guided
# mode sat below MIN_INTERVIEWS. Every one of these should now carry a guided row.
if (!is.null(props) && !is.null(effort)) {
  cat("\n-- Cells that previously lost their guided row (guided n < 30, cell n >= 30) --\n")
  at_risk <- props |>
    filter(mode == "guided", n_interviews < 30, n_interviews > 0, n_location >= 30) |>
    select(fishery_name, year, location, n_guided = n_interviews, n_cell = n_location)

  recovered <- at_risk |>
    left_join(
      effort |>
        filter(tier == "P1", mode == "guided") |>
        mutate(location = tolower(location)) |>
        group_by(fishery_name, year, location) |>
        summarise(guided_trips = sum(angler_trips, na.rm = TRUE), .groups = "drop"),
      by = c("fishery_name", "year", "location")
    ) |>
    mutate(guided_trips = round(coalesce(guided_trips, 0)))

  print(as.data.frame(recovered))
  n_missing <- sum(recovered$guided_trips == 0)
  if (n_missing > 0) {
    cat(glue("\n!! {n_missing} of these still have no guided trips - investigate.\n"))
  } else {
    cat(glue("\nAll {nrow(recovered)} carry guided trips. Total recovered: ",
             "{round(sum(recovered$guided_trips))} angler trips.\n"))
  }
}

cat("\nDone.\n")
