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

cat("\n=== by fishery and year, for context ===\n")
g |> count(.fishery_name, year, name = "guided_interviews") |>
  arrange(.fishery_name, year) |> as.data.frame() |> print(row.names = FALSE)

out <- file.path(OUT_DIR, "creel_guided_species_by_month.csv")
write_csv(by_month, out)
cat(glue("\nWrote {out}\n"), "\n")
