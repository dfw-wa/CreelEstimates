# ==============================================================================
# creel_guided_species_seasonality.R
#
# Purpose:
#   The guide logbook has no target-species field, so classifying a logged trip
#   as salmon-directed rests on what it caught - and on the Cowlitz, the single
#   largest ambiguous block in the logbook (8,209 guided angler-trips), that
#   inference has no P1 creel to check against: Cowlitz reaches the deliverable
#   only through P2/P3, which is CRC-salmon-harvest-derived by construction.
#
#   But the creel DOES interview the Cowlitz ("Lower Cowlitz salmon and
#   steelhead"). Those interviews carry BOTH a guided flag and species-level
#   catch, so they can say directly what guided anglers were catching, month by
#   month. That is the independent read on seasonality the logbook cannot give:
#
#     - in which months are guided Cowlitz interviews landing salmon vs steelhead?
#     - how sharply do the two separate, or do they overlap?
#     - how many guided interviews caught NOTHING - the same irreducible cell
#       the logbook has, but here with a known denominator?
#
#   Generalises beyond the Cowlitz: pass any fishery regex.
#
# WHY A SEPARATE PULL. analysis/pst/outputs/04_interview_proportions/
# all_interviews.csv is the `interview` table only. Species lives in the creel
# DB's `catch` table (species / life_stage / fin_mark / fate), which
# interview_proportions.qmd never pulls. This script pulls both and joins them.
#
# Needs DB/VPN access, like multi_fishery_creel_summary.R. Results cache to
# .cache/ so a re-run is cheap; delete the cache to force a fresh pull.
#
# Usage:
#   Rscript analysis/pst/03_analysis/creel_guided_species_seasonality.R
#   Rscript analysis/pst/03_analysis/creel_guided_species_seasonality.R "yakima"
#   Rscript analysis/pst/03_analysis/creel_guided_species_seasonality.R "cowlitz|drano"
#   Rscript analysis/pst/03_analysis/creel_guided_species_seasonality.R "."
#     - "." matches every fishery. Use it to build the target-vs-catch
#       calibration (section 5) from whichever creels DO record a target
#       species, then carry that error rate onto fisheries that do not - and
#       onto the guide logbook, which records no target at all.
#
# Output:
#   analysis/pst/outputs/08_guide_logbook_diagnostic/
#     creel_guided_species_by_month.csv
# ==============================================================================

suppressMessages({
  library(tidyverse)
  library(here)
  library(glue)
})
# creelutils is loaded ONLY when a fresh DB pull is needed - see section 1. A
# cached re-run touches no database, so it should not require the database
# package either (and cannot be run at all on a machine without it otherwise).

options(width = 200)

args <- commandArgs(trailingOnly = TRUE)
FISHERY_PATTERN <- if (length(args) >= 1 && nzchar(args[1])) args[1] else "cowlitz"

OUT_DIR <- here("analysis", "pst", "outputs", "08_guide_logbook_diagnostic")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
CACHE   <- here(".cache")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)
cache_file <- file.path(CACHE, glue("creel_int_catch_{make.names(FISHERY_PATTERN)}.rds"))

# Same species set the creel summary uses for its salmon catch groups, so this
# diagnostic and the estimates agree on what counts as salmon.
SALMON_SPECIES <- c("Chinook", "Coho", "Chum", "Pink", "Sockeye")

# ---- 1. Pull interview + catch for the matching fisheries --------------------

if (file.exists(cache_file)) {
  cat(glue("Reading cached pull: {cache_file}\n(delete it to refresh)\n\n"))
  pulled <- readRDS(cache_file)
} else {
  if (!requireNamespace("creelutils", quietly = TRUE)) {
    stop(glue("No cache at {cache_file} and creelutils is not installed, so a ",
              "fresh pull is impossible. Run this on a machine with DB/VPN ",
              "access and the creelutils package."), call. = FALSE)
  }
  library(creelutils)
  conn <- connect_creel_db()
  fisheries <- fishery_lut(conn = conn) |>
    pull(fishery_name) |> unique()
  hits <- fisheries[str_detect(fisheries, regex(FISHERY_PATTERN, ignore_case = TRUE))]

  if (length(hits) == 0) {
    DBI::dbDisconnect(conn)
    stop(glue("No fishery_name matches /{FISHERY_PATTERN}/. ",
              "Run with a different pattern."), call. = FALSE)
  }
  cat(glue("Matched {length(hits)} fisheries:\n"),
      paste0("  ", hits, collapse = "\n"), "\n\n")

  grab <- function(fn, tbl) {
    tryCatch({
      d <- fetch_data(conn = conn, fishery_name = fn, tables = tbl,
                      data_source = "internal")[[tbl]]
      if (is.null(d) || nrow(d) == 0) return(NULL)
      d |> mutate(across(everything(), as.character), .fishery_name = fn)
    }, error = function(e) {
      cli::cli_alert_warning("{tbl} pull failed for {fn}: {e$message}"); NULL
    })
  }

  pulled <- list(
    interview = map(hits, grab, tbl = "interview") |> compact() |> bind_rows(),
    catch     = map(hits, grab, tbl = "catch")     |> compact() |> bind_rows()
  )
  DBI::dbDisconnect(conn)
  saveRDS(pulled, cache_file)
  cat(glue("Cached to {cache_file}\n\n"))
}

int <- pulled$interview
cat_tbl <- pulled$catch

if (is.null(int) || nrow(int) == 0) {
  stop("No interview rows returned for that pattern.", call. = FALSE)
}

cat("=== interview columns ===\n"); cat(" ", paste(names(int), collapse = ", "), "\n\n")
cat("=== catch columns ===\n")
cat(" ", if (is.null(cat_tbl)) "(none returned)" else paste(names(cat_tbl), collapse = ", "), "\n\n")

# ---- 1b. Is target species recorded directly? --------------------------------
# Nothing in this repo references a target-species field, but the qmd pulls the
# whole interview table, so an unused one would already be sitting there. If it
# exists it beats catch-based inference outright - and more importantly it lets
# the catch-based inference be CALIBRATED (section 5), which is the only way to
# put an error bar on the same inference applied to the logbook.

TARGET_PAT <- "target|fishing_for|sought|directed|intent|pursu|fishery_type|trip_type"

# Target-species taxonomy is shared with guided_target_mix_by_river.R.
source(here("analysis", "pst", "03_analysis", "_target_species_classes.R"))

target_col <- NULL
tcands <- names(int)[str_detect(names(int), regex(TARGET_PAT, ignore_case = TRUE))]
cat("=== candidate target-species fields in the interview table ===\n")
if (length(tcands) == 0) {
  cat("  none found by name. Catch-based inference is the only route.\n\n")
} else {
  for (cl in tcands) {
    v <- int[[cl]]
    u <- unique(v[!is.na(v) & nzchar(v)])
    cat("  ", cl, " - ", sum(!is.na(v) & nzchar(v)), " non-blank of ", length(v),
        ", ", length(u), " distinct\n", sep = "")
    if (length(u) <= 25) cat("      ", paste(sort(u), collapse = " | "), "\n", sep = "")
    else cat("      first 15: ", paste(head(sort(u), 15), collapse = " | "), "\n", sep = "")
    # Deliberately NO minimum coverage: target species is asked by some creel
    # programmes and not others, so a field that is blank on this fishery but
    # populated elsewhere is still the field we want - it just means the
    # calibration has to be built from the creels that do ask it (see below).
    # trip_guided matches the pattern but is the guided flag, not a target.
    if (is.null(target_col) && cl != "trip_guided" &&
        length(u) >= 2 && length(u) <= 30 && length(u) > 0) target_col <- cl
  }
  # The field name is known, so prefer it outright rather than whatever the
  # pattern happened to hit first.
  if ("target_species" %in% names(int)) target_col <- "target_species"
  cat("\n  -> using: ", target_col %||% "none usable", "\n\n", sep = "")

  # Where the field is actually answered. If it is blank for the fishery in
  # question but populated for others, re-run with a wider pattern (e.g. "." for
  # every fishery) to build the calibration from the creels that do ask it, then
  # carry that error rate across.
  if (!is.null(target_col)) {
    cat("=== coverage of ", target_col, " by fishery ===\n", sep = "")
    int |>
      group_by(.fishery_name) |>
      summarise(interviews = n(),
                # "Target species not asked" is a recorded value meaning the
                # question was never put - counting it as answered would
                # overstate coverage.
                answered = sum(classify_target(.data[[target_col]]) %in%
                                 c("salmon", "salmon_or_steelhead", "steelhead",
                                   "other_species", "nonspecific", "unmapped")),
                .groups = "drop") |>
      mutate(pct_answered = round(100 * answered / interviews, 1)) |>
      arrange(desc(pct_answered)) |> as.data.frame() |> print(row.names = FALSE)
    cat("\n")
  }
}

# ---- 2. Find the interview <-> catch key -------------------------------------
# Not assumed: the key is discovered by intersecting column names and preferring
# something interview-ish, then verified by match rate. A silently wrong key
# would make every interview look like it caught nothing.

key <- NULL
if (!is.null(cat_tbl) && nrow(cat_tbl) > 0) {
  shared <- intersect(names(int), names(cat_tbl))
  prefer <- shared[str_detect(shared, regex("interview.*id|^id$", ignore_case = TRUE))]
  cand   <- c(prefer, setdiff(shared, prefer))
  for (k in cand) {
    if (all(is.na(int[[k]]))) next
    rate <- mean(cat_tbl[[k]] %in% int[[k]], na.rm = TRUE)
    if (!is.na(rate) && rate > 0.9) { key <- k; break }
  }
  cat(glue("Join key: {key %||% 'NONE FOUND'}",
           if (!is.null(key)) glue(" ({round(100 * mean(cat_tbl[[key]] %in% int[[key]]), 1)}% of catch rows match an interview)") else ""),
      "\n\n")
}

if (is.null(key)) {
  stop(paste("Could not find a reliable interview<->catch key. Shared columns:",
             paste(intersect(names(int), names(cat_tbl)), collapse = ", "),
             "\nInspect the two column lists above and set the key by hand."),
       call. = FALSE)
}

# ---- 3. Classify each interview by what it caught ----------------------------

catch_class <- cat_tbl |>
  mutate(
    species = str_squish(species),
    is_salmon = species %in% SALMON_SPECIES,
    is_sthd   = str_detect(coalesce(species, ""), regex("steelhead", ignore_case = TRUE))
    # Deliberately no fish_count: the question is what a trip was AFTER, which
    # presence answers - and referencing a column this table may not carry under
    # that name would fail the run for nothing.
  ) |>
  group_by(.int_key = .data[[key]]) |>
  summarise(
    salmon_rows = sum(is_salmon, na.rm = TRUE),
    sthd_rows   = sum(is_sthd, na.rm = TRUE),
    other_rows  = sum(!is_salmon & !is_sthd, na.rm = TRUE),
    species_seen = paste(sort(unique(species)), collapse = "; "),
    .groups = "drop"
  )

ints <- int |>
  mutate(
    .int_key = .data[[key]],
    event_date = suppressWarnings(as.Date(event_date)),
    year  = lubridate::year(event_date),
    month = lubridate::month(event_date),
    guided = case_when(
      trip_guided == "Guided"     ~ "Guided",
      trip_guided == "Non-guided" ~ "Unguided",
      TRUE                        ~ NA_character_
    )
  ) |>
  left_join(catch_class, by = ".int_key") |>
  mutate(
    across(c(salmon_rows, sthd_rows, other_rows), ~ coalesce(.x, 0)),
    caught = case_when(
      salmon_rows > 0 & sthd_rows > 0 ~ "both salmon & steelhead",
      salmon_rows > 0                 ~ "salmon",
      sthd_rows   > 0                 ~ "steelhead only",
      other_rows  > 0                 ~ "other species only",
      TRUE                            ~ "nothing"
    )
  ) |>
  filter(!is.na(month))

# ---- 4. Report ---------------------------------------------------------------

cat("=== interviews by guided status ===\n")
ints |> count(guided, name = "interviews") |> as.data.frame() |> print(row.names = FALSE)

g <- ints |> filter(guided == "Guided")
if (nrow(g) == 0) stop("No interviews carry trip_guided == 'Guided'.", call. = FALSE)

cat(glue("\n=== GUIDED interviews: what was caught, by month (n = {nrow(g)}) ===\n"), "\n")
by_month <- g |>
  count(month, caught, name = "interviews") |>
  group_by(month) |>
  mutate(pct = round(100 * interviews / sum(interviews), 1)) |>
  ungroup()
by_month |>
  select(-pct) |>
  pivot_wider(names_from = caught, values_from = interviews, values_fill = 0) |>
  arrange(month) |> as.data.frame() |> print(row.names = FALSE)

cat("\n=== same, as row percentages ===\n")
by_month |>
  select(-interviews) |>
  pivot_wider(names_from = caught, values_from = pct, values_fill = 0) |>
  arrange(month) |> as.data.frame() |> print(row.names = FALSE)

cat("\n=== guided vs unguided, salmon share of catch-bearing interviews ===\n")
ints |>
  filter(!is.na(guided), caught != "nothing") |>
  group_by(guided, month) |>
  summarise(
    salmon_ints = sum(caught %in% c("salmon", "both salmon & steelhead")),
    n = n(), .groups = "drop"
  ) |>
  mutate(pct_salmon = round(100 * salmon_ints / n, 1)) |>
  select(guided, month, n, pct_salmon) |>
  pivot_wider(names_from = guided, values_from = c(n, pct_salmon)) |>
  arrange(month) |> as.data.frame() |> print(row.names = FALSE)

cat("\n=== guided interviews that caught NOTHING, by month ===\n")
cat("This is the logbook's irreducible cell, here with a denominator.\n\n")
g |>
  group_by(month) |>
  summarise(interviews = n(), nothing = sum(caught == "nothing"), .groups = "drop") |>
  mutate(pct_nothing = round(100 * nothing / interviews, 1)) |>
  arrange(month) |> as.data.frame() |> print(row.names = FALSE)

# ---- 5. Calibration: how well does catch recover stated target? -------------
# THE POINT OF THIS SECTION. The logbook has no target field, so every
# ambiguous trip there is classified from catch alone. Here both are present,
# so the error rate of that exact inference can be measured instead of assumed -
# and the numbers below are what should be applied as a correction, or a
# caveat, to the logbook's ambiguous cells.

if (!is.null(target_col)) {
  g2 <- g |>
    mutate(target = str_squish(.data[[target_col]]),
           target_class = classify_target(.data[[target_col]])) |>
    filter(!target_class %in% c("blank", "not_asked"))

  if (nrow(g2) > 0) {
    # The direct answer first. Everything else in this section is a check on
    # the catch-based rule; this is the measurement it is trying to imitate.
    cat(glue("\n=== GUIDED interviews by STATED TARGET and month ",
             "(n = {nrow(g2)}) ===\n"), "\n")
    cat("Counts:\n")
    g2 |> count(month, target_class, name = "interviews") |>
      pivot_wider(names_from = target_class, values_from = interviews,
                  values_fill = 0) |>
      arrange(month) |> as.data.frame() |> print(row.names = FALSE)
    cat("\nRow percentages:\n")
    g2 |> count(month, target_class, name = "n") |>
      group_by(month) |> mutate(pct = round(100 * n / sum(n), 1)) |>
      ungroup() |> select(-n) |>
      pivot_wider(names_from = target_class, values_from = pct, values_fill = 0) |>
      arrange(month) |> as.data.frame() |> print(row.names = FALSE)

    cat("\n=== guided vs unguided target mix (whole period) ===\n")
    ints |>
      filter(!is.na(guided)) |>
      mutate(target_class = classify_target(.data[[target_col]])) |>
      filter(!target_class %in% c("blank", "not_asked")) |>
      count(guided, target_class, name = "n") |>
      group_by(guided) |> mutate(pct = round(100 * n / sum(n), 1)) |>
      ungroup() |> select(-n) |>
      pivot_wider(names_from = guided, values_from = pct, values_fill = 0) |>
      as.data.frame() |> print(row.names = FALSE)

    cat("\n=== raw target_species values, guided only ===\n")
    g2 |> count(target, target_class, name = "interviews") |>
      arrange(desc(interviews)) |> as.data.frame() |> print(row.names = FALSE)

    cat(glue("\n=== CALIBRATION: stated target vs. what was caught ",
             "(guided, n = {nrow(g2)}) ===\n"), "\n")
    cat("Rows = what the angler said they were after; columns = what the\n",
        "catch-based rule would have concluded. Off-diagonal mass is the error\n",
        "rate of applying that same rule to the logbook.\n",
        "If this fishery leaves the target blank, re-run with pattern \".\" to\n",
        "build the calibration from creels that do ask it.\n\n", sep = "")
    conf <- g2 |> count(target_class, caught, name = "n") |>
      group_by(target_class) |> mutate(pct = round(100 * n / sum(n), 1)) |> ungroup()
    conf |> select(-pct) |>
      pivot_wider(names_from = caught, values_from = n, values_fill = 0) |>
      as.data.frame() |> print(row.names = FALSE)
    cat("\n-- row percentages --\n")
    conf |> select(-n) |>
      pivot_wider(names_from = caught, values_from = pct, values_fill = 0) |>
      as.data.frame() |> print(row.names = FALSE)

    # The two logbook cells that actually need this: what were anglers really
    # after when they caught nothing, or caught only steelhead?
    cat("\n-- the two ambiguous logbook cells, resolved against stated target --\n")
    g2 |>
      filter(caught %in% c("nothing", "steelhead only")) |>
      count(caught, target_class, name = "interviews") |>
      group_by(caught) |> mutate(pct = round(100 * interviews / sum(interviews), 1)) |>
      ungroup() |> arrange(caught, desc(interviews)) |>
      as.data.frame() |> print(row.names = FALSE)

  }
} else {
  cat("\n=== CALIBRATION skipped: no usable target field ===\n")
  cat("Without a stated target there is no way to measure the error rate of\n",
      "catch-based classification, here or in the logbook. The classification\n",
      "stands as an assumption rather than a measured one.\n", sep = "")
}

cat("\n=== by fishery and year, for context ===\n")
g |> count(.fishery_name, year, name = "guided_interviews") |>
  arrange(.fishery_name, year) |> as.data.frame() |> print(row.names = FALSE)

out <- file.path(OUT_DIR, "creel_guided_species_by_month.csv")
write_csv(by_month, out)
cat(glue("\nWrote {out}\n"), "\n")
