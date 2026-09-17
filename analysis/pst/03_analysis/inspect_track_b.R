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

  # p1_trips and guided_trips are both read off the raw angler_trips vector
  # before anything reassigns it - see the note in the assembly about the
  # summarise() evaluation-order hazard.
  eff_cells <- effort |>
    filter(tier == "P1") |>
    mutate(location = tolower(location)) |>
    group_by(fishery_name, year, location) |>
    summarise(p1_trips     = sum(angler_trips, na.rm = TRUE),
              guided_trips = sum(angler_trips[mode == "guided"], na.rm = TRUE),
              .groups = "drop")

  # A props cell with no P1 rows at all is not a Track B failure - the fishery
  # simply is not in the salmon deliverable (steelhead and gamefish fisheries,
  # and years outside YEARS_SCOPE, are interviewed but never assembled). Only a
  # cell that HAS P1 trips and still shows no guided rows is a real problem.
  recovered <- at_risk |>
    left_join(eff_cells, by = c("fishery_name", "year", "location")) |>
    mutate(
      p1_trips     = round(coalesce(p1_trips, 0)),
      guided_trips = round(coalesce(guided_trips, 0)),
      status = case_when(
        p1_trips == 0    ~ "n/a - not in the salmon deliverable",
        guided_trips > 0 ~ "recovered",
        TRUE             ~ "STILL ZERO - investigate"
      )
    )

  recovered |> count(status, name = "cells") |> as.data.frame() |> print()

  cat(glue("\nGuided angler trips recovered: ",
           "{round(sum(recovered$guided_trips))}\n\n"))

  still_zero <- recovered |> filter(status == "STILL ZERO - investigate")
  if (nrow(still_zero) > 0) {
    cat("-- Cells with P1 trips but no guided rows (real, investigate) --\n")
    still_zero |>
      select(fishery_name, year, location, n_guided, n_cell, p1_trips) |>
      arrange(desc(p1_trips)) |> as.data.frame() |> print()
  } else {
    cat("Every at-risk cell that reaches the deliverable now carries guided trips.\n")
  }
}

# ---- 5. Month-by-month picture for anything still stuck ----------------------
# A cell can legitimately resolve to zero guided trips if the months carrying
# the effort are months with no guided interviews - the guided sample landing
# in a month the creel barely fished. That is a real (if uncomfortable) answer,
# not a join failure, and the only way to tell them apart is to line the two up.

if (exists("still_zero") && !is.null(still_zero) && nrow(still_zero) > 0 &&
    !is.null(pmonth) && !is.null(effort)) {

  cat("\n================ 5. STUCK CELLS, MONTH BY MONTH ================\n")

  eff_month <- effort |>
    filter(tier == "P1") |>
    mutate(location = tolower(location)) |>
    group_by(fishery_name, year, location, month) |>
    summarise(trips = sum(angler_trips, na.rm = TRUE),
              modes = paste(sort(unique(mode)), collapse = "/"),
              basis = paste(sort(unique(mode_basis)), collapse = "/"),
              .groups = "drop")

  props_month_guided <- pmonth |>
    filter(mode == "guided") |>
    transmute(fishery_name, year, month = month_num, location,
              n_int_month = n_location, prop_guided = round(prop, 4))

  for (i in seq_len(nrow(still_zero))) {
    r <- still_zero[i, ]
    cat(glue("\n-- {r$fishery_name} | {r$location} | ",
             "annual guided interviews {r$n_guided} of {r$n_cell} --\n\n"))
    eff_month |>
      filter(fishery_name == r$fishery_name, year == r$year,
             location == r$location) |>
      left_join(props_month_guided,
                by = c("fishery_name", "year", "month", "location")) |>
      transmute(month, trips = round(trips), modes, basis,
                n_int_month, prop_guided) |>
      arrange(month) |> as.data.frame() |> print()
  }

  cat("\nRead this as: a month with trips but prop_guided = 0 means the creel\n",
      "fished that month and interviewed no guides in it. A month with trips\n",
      "but n_int_month = NA means the month tier had no cell at all and the\n",
      "row fell back - check `basis` for which tier actually served it.\n", sep = "")
}

cat("\nDone.\n")
