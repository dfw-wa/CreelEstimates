# ==============================================================================
# cowlitz_salmon_trip_review.R
#
# Meeting aid: which guided Cowlitz trips should count as salmon trips?
# Puts the guide logbook (what guided trips CAUGHT) next to the creel
# interviews (what guided anglers SAID they were targeting, and what they
# caught), month by month, for the Cowlitz below Mayfield Dam (CRC 561).
#
# Reads only files already on disk - no DB:
#   input_files/pst/guide_logbook/guide_logbook_data_2026-09-02.rds
#   .cache/creel_int_catch_cowlitz.rds   (from creel_guided_species_seasonality.R)
#   analysis/pst/outputs/01_crc_harvest/crc_freshwater_harvest_*_tidy.csv
# Any missing source is skipped with a message.
#
# Output: analysis/pst/outputs/08_guide_logbook_diagnostic/cowlitz_review/
#   cowlitz_salmon_trip_review.pdf  - all plots, one per page
#   *.csv                           - the tables printed below
#
# Usage: Rscript analysis/pst/03_analysis/cowlitz_salmon_trip_review.R
# ==============================================================================

suppressMessages({ library(tidyverse); library(here); library(glue) })
options(width = 160)

CRC_CODES   <- c("561")          # Cowlitz R. below Mayfield Dam
YEARS       <- 2020:2026
SALMON      <- c("Chinook", "Coho", "Chum", "Pink", "Sockeye")
CATCH_LEVELS <- c("salmon only", "salmon + steelhead", "steelhead only",
                  "other species only", "nothing")
CATCH_COLS  <- c("salmon only" = "#1b7837", "salmon + steelhead" = "#7fbf7b",
                 "steelhead only" = "#2166ac", "other species only" = "#bababa",
                 "nothing" = "#f4a582")

OUT <- here("analysis", "pst", "outputs", "08_guide_logbook_diagnostic", "cowlitz_review")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
source(here("analysis", "pst", "03_analysis", "_target_species_classes.R"))

classify_catch <- function(n_salmon, n_sthd, n_other) {
  factor(case_when(
    n_salmon > 0 & n_sthd > 0 ~ "salmon + steelhead",
    n_salmon > 0              ~ "salmon only",
    n_sthd > 0                ~ "steelhead only",
    n_other > 0               ~ "other species only",
    TRUE                      ~ "nothing"), levels = CATCH_LEVELS)
}
mon <- function(m) factor(month.abb[m], levels = month.abb)
# When sourced from cowlitz_salmon_trip_review.qmd, tables are kept in
# `tables` for the report instead of printed, and no PDF is written.
QUIET <- exists("QUIET") && isTRUE(QUIET)
tables <- list()
show <- function(df, title, file) {
  tables[[title]] <<- df
  if (!QUIET) {
    cat("\n==== ", title, " ====\n", sep = "")
    print(as.data.frame(df), row.names = FALSE)
  }
  write_csv(df, file.path(OUT, file))
}
plots <- list()

# ---- 1. Guide logbook ---------------------------------------------------------
rds <- here("input_files", "pst", "guide_logbook", "guide_logbook_data_2026-09-02.rds")
lb_trips <- NULL
if (file.exists(rds)) {
  gl <- readRDS(rds)
  trips <- gl$trip |>
    mutate(type  = gl$trip_type_lut$name[match(trip_type_id, gl$trip_type_lut$id)],
           crc   = as.character(gl$water_body_lut$crc_code[match(water_body_id, gl$water_body_lut$id)]),
           year  = as.integer(format(trip_date, "%Y")),
           month = as.integer(format(trip_date, "%m"))) |>
    filter(!is_void, type == "Guided", crc %in% CRC_CODES, year %in% YEARS)
  anglers <- gl$trip_angler |>
    mutate(atype = gl$trip_angler_type_lut$name[match(trip_angler_type_id,
                                                      gl$trip_angler_type_lut$id)]) |>
    filter(atype %in% c("Paying", "Comped")) |>
    count(trip_id, name = "anglers")
  catch <- gl$encounter |>
    mutate(sp = gl$species_lut$name[match(species_id, gl$species_lut$id)],
           n = as.numeric(fish_count)) |>
    filter(n > 0) |>
    group_by(trip_id) |>
    summarise(n_salmon = sum(n[sp %in% SALMON]), n_sthd = sum(n[sp %in% "Steelhead"]),
              n_other = sum(n[!sp %in% c(SALMON, "Steelhead")]), .groups = "drop")
  lb_trips <- trips |>
    left_join(anglers, by = c("id" = "trip_id")) |>
    left_join(catch, by = c("id" = "trip_id")) |>
    mutate(across(c(anglers, n_salmon, n_sthd, n_other), ~ coalesce(.x, 0)),
           caught = classify_catch(n_salmon, n_sthd, n_other))

  lb_month <- lb_trips |> count(month, caught, wt = anglers, name = "angler_trips")
  plots$lb <- ggplot(lb_month, aes(mon(month), angler_trips, fill = caught)) +
    geom_col() + scale_fill_manual(values = CATCH_COLS, drop = FALSE) +
    labs(title = "Guide logbook: Cowlitz guided angler-trips by month and what was caught",
         subtitle = glue("CRC {toString(CRC_CODES)}, {min(YEARS)}-{max(YEARS)}, all years pooled"),
         x = NULL, y = "Guided angler-trips (clients)", fill = "Caught") +
    theme_minimal(base_size = 12)

  plots$lb_pct <- ggplot(lb_month, aes(mon(month), angler_trips, fill = caught)) +
    geom_col(position = "fill") + scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = CATCH_COLS, drop = FALSE) +
    labs(title = "Guide logbook: share of guided angler-trips by what was caught",
         x = NULL, y = NULL, fill = "Caught") + theme_minimal(base_size = 12)

  show(lb_month |> pivot_wider(names_from = caught, values_from = angler_trips, values_fill = 0) |>
         arrange(month) |> mutate(month = month.abb[month]),
       "LOGBOOK: guided angler-trips by month x catch (all years)", "logbook_month_by_catch.csv")
  show(lb_trips |> count(year, caught, wt = anglers, name = "angler_trips") |>
         pivot_wider(names_from = caught, values_from = angler_trips, values_fill = 0),
       "LOGBOOK: guided angler-trips by year x catch", "logbook_year_by_catch.csv")
} else message("Logbook RDS not found - logbook sections skipped.")

# ---- 2. Creel interviews ------------------------------------------------------
cache <- here(".cache", "creel_int_catch_cowlitz.rds")
cr <- NULL
if (file.exists(cache)) {
  pulled <- readRDS(cache)
  ccatch <- pulled$catch |>
    mutate(sp = str_squish(species)) |>
    group_by(interview_id) |>
    summarise(n_salmon = sum(sp %in% SALMON), n_sthd = sum(sp %in% "Steelhead"),
              n_other = sum(!sp %in% c(SALMON, "Steelhead")), .groups = "drop")
  cr <- pulled$interview |>
    mutate(date = as.Date(event_date), month = lubridate::month(date),
           guided = case_when(trip_guided == "Guided" ~ "Guided",
                              trip_guided == "Non-guided" ~ "Unguided"),
           target = classify_target(target_species)) |>
    left_join(ccatch, by = "interview_id") |>
    mutate(across(c(n_salmon, n_sthd, n_other), ~ coalesce(.x, 0L)),
           caught = classify_catch(n_salmon, n_sthd, n_other)) |>
    filter(!is.na(month), !is.na(guided), target %in% ANSWERED_CLASSES)

  tgt_cols <- c(salmon = "#1b7837", steelhead = "#2166ac", salmon_or_steelhead = "#80cdc1",
                other_species = "#bababa", nonspecific = "#e0e0e0", unmapped = "#000000")
  plots$cr_target <- ggplot(cr, aes(mon(month), fill = target)) +
    geom_bar() + facet_wrap(~ guided, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = tgt_cols) +
    labs(title = "Creel interviews: stated target species by month",
         subtitle = "Cowlitz creel, guided vs unguided parties",
         x = NULL, y = "Interviews (parties)", fill = "Stated target") +
    theme_minimal(base_size = 12)

  g <- cr |> filter(guided == "Guided")
  plots$cr_caught <- ggplot(g, aes(mon(month), fill = caught)) +
    geom_bar() + scale_fill_manual(values = CATCH_COLS, drop = FALSE) +
    labs(title = "Creel interviews: what GUIDED parties caught, by month",
         subtitle = "Same catch classes as the logbook plot - compare the two",
         x = NULL, y = "Guided interviews", fill = "Caught") + theme_minimal(base_size = 12)

  # The key table: given what a guided party caught, what did it say it targeted?
  calib <- g |> count(caught, target) |> group_by(caught) |>
    mutate(pct = round(100 * n / sum(n), 1)) |> ungroup()
  plots$calib <- ggplot(calib, aes(target, caught, fill = pct)) +
    geom_tile(colour = "white") + geom_text(aes(label = glue("{pct}%\n(n={n})")), size = 3.5) +
    scale_fill_gradient(low = "#f7f7f7", high = "#2166ac", limits = c(0, 100)) +
    labs(title = "Guided creel parties: what they caught vs what they said they targeted",
         subtitle = "Row % - e.g. of parties that caught only steelhead, the share that targeted salmon",
         x = "Stated target", y = "Caught", fill = "Row %") +
    theme_minimal(base_size = 12)

  show(calib |> select(-n) |> pivot_wider(names_from = target, values_from = pct, values_fill = 0),
       "CREEL (guided): row % stated target, given what was caught", "creel_caught_vs_target.csv")
  show(g |> filter(caught %in% c("steelhead only", "nothing")) |>
         count(caught, month, target) |> group_by(caught, month) |>
         mutate(pct_salmon_target = round(100 * sum(n[target == "salmon"]) / sum(n), 1),
                interviews = sum(n)) |>
         ungroup() |> distinct(caught, month, interviews, pct_salmon_target) |>
         arrange(caught, month) |> mutate(month = month.abb[month]),
       "CREEL (guided): steelhead-only and no-catch parties - % that targeted salmon, by month",
       "creel_ambiguous_by_month.csv")
  show(cr |> count(guided, month, target) |> group_by(guided, month) |>
         mutate(pct = round(100 * n / sum(n), 1)) |> ungroup() |> select(-n) |>
         pivot_wider(names_from = target, values_from = pct, values_fill = 0) |>
         arrange(guided, month) |> mutate(month = month.abb[month]),
       "CREEL: row % stated target by month, guided vs unguided", "creel_target_by_month.csv")
} else message("No cached creel pull at ", cache,
               " - run creel_guided_species_seasonality.R first. Creel sections skipped.")

# ---- 3. CRC salmon harvest by month (season context) --------------------------
crc_files <- here("analysis", "pst", "outputs", "01_crc_harvest",
                  c("crc_freshwater_harvest_2010_2024_tidy.csv",
                    "crc_freshwater_harvest_final_creel_subs_tidy.csv"))
crc_files <- crc_files[file.exists(crc_files)]
if (length(crc_files) > 0) {
  crc <- map_dfr(crc_files, ~ suppressMessages(read_csv(.x, show_col_types = FALSE))) |>
    filter(as.character(stream_code) %in% CRC_CODES, calendar_year >= 2015) |>
    group_by(species, calendar_month) |>
    summarise(harvest = sum(harvest_count, na.rm = TRUE), .groups = "drop")
  plots$crc <- ggplot(crc, aes(mon(calendar_month), harvest, fill = species)) +
    geom_col() +
    labs(title = "CRC salmon harvest by month, Cowlitz below Mayfield (2015+)",
         subtitle = "When salmon are actually being kept - the season the logbook is matched against",
         x = NULL, y = "Reported harvest", fill = NULL) + theme_minimal(base_size = 12)
} else message("CRC harvest files not found - season context plot skipped.")

# ---- 4. What each counting rule would give ------------------------------------
# Guided salmon angler-trips on the Cowlitz under alternative rules, by year.
if (!is.null(lb_trips)) {
  sep_nov <- lb_trips$month %in% 9:11
  rules <- lb_trips |>
    mutate(
      A_salmon_caught_only      = caught == "salmon only",
      B_plus_salmon_and_sthd    = caught %in% c("salmon only", "salmon + steelhead"),
      C_plus_nothing_Sep_Nov    = B_plus_salmon_and_sthd | (caught == "nothing" & sep_nov),
      D_plus_sthd_only_Sep_Nov  = C_plus_nothing_Sep_Nov | (caught == "steelhead only" & sep_nov),
      E_everything_any_month    = TRUE
    ) |>
    group_by(year) |>
    summarise(across(A_salmon_caught_only:E_everything_any_month, ~ sum(anglers[.x])),
              .groups = "drop")
  show(rules, paste("RULE OPTIONS: guided salmon angler-trips by year",
                    "(current pipeline = C; D adds steelhead-only Sep-Nov)"),
       "rule_options_by_year.csv")
}

# ---- 5. Write the PDF ---------------------------------------------------------
if (length(plots) > 0 && !QUIET) {
  pdf_path <- file.path(OUT, "cowlitz_salmon_trip_review.pdf")
  pdf(pdf_path, width = 11, height = 7.5)
  for (p in plots) print(p)
  dev.off()
  cat(glue("\nWrote {pdf_path} ({length(plots)} pages) and tables to {OUT}\n"), "\n")
}
