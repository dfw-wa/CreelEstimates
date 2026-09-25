# ==============================================================================
# salmon_directed_share.R
#
# Purpose:
#   P1 creel trips are effort / trip length - target-agnostic. On water open to
#   both salmon and steelhead (Columbia tribs above all), every steelhead angler
#   the creel counted is sitting in the salmon trip total, and P2/P3 inherit the
#   inflation through the donor numerator (trips / CRC salmon harvest). Drano is
#   the sharpest case: it is one of the few Columbia creels feeding the P2
#   ratios, and ~all of its interviews answer target_species "Multiple salmon
#   and/or steelhead".
#
#   This builds, per fishery_name x year x month, the share of creel effort that
#   is salmon-directed, for the assembly to scale creel_pe trips by BEFORE the
#   P2 ratios are built.
#
# Method (agreed 2026-09-25, Evan/Kale):
#   share = (salmon + f_mixed * salmon_or_steelhead) / answered_specific
#     - classes from _target_species_classes.R; weighted by angler_count where
#       recorded (a party is not an angler), else by party.
#     - answered_specific = salmon + salmon_or_steelhead + steelhead + other.
#       "nonspecific" (Any species / Other / Unknown) is left out of the
#       denominator, i.e. assumed to split like the anglers who did name a
#       target. blank / not_asked are unanswered.
#     - f_mixed: the salmon fraction of the "salmon or steelhead" anglers, from
#       their own monthly catch - salmon encounters / (salmon + steelhead
#       encounters), all fates. That is the salmon:steelhead CPUE ratio on the
#       same interviews (the effort denominator cancels). Assumes equal
#       catchability per unit of directed effort (Kale's caveat) - a mixed
#       angler catching 3 salmon per steelhead is read as 75% salmon-directed.
#       Needs the creel catch table: pulled by creel_guided_species_seasonality.R
#       into .cache/creel_int_catch_*.rds ("drano", or "." for everything).
#       Without it f_mixed = 1 (mixed counted as salmon - no reduction) and the
#       basis says so. [R3]
#   Tiers (first with >= MIN_ANSWERED answered anglers-weighted interviews):
#     fishery-year-month -> fishery-month (years pooled) -> fishery (all months)
#     -> none (share = 1, no adjustment, logged).
#
# Guide logbook cross-check (NOT applied): guided trips that caught salmon vs
# steelhead-only, by CRC area x month, from parse_guide_logbook.R's salmon-rule
# counts. Trip-level, so closer to a trip share than a fish ratio, but guided
# only and self-reported - it sits beside the creel share as a comparison.
#
# Inputs:
#   analysis/pst/outputs/04_interview_proportions/all_interviews.rds (or .csv)
#   .cache/creel_int_catch_*.rds                          (optional, f_mixed)
#   analysis/pst/outputs/07_guide_logbook/
#     guide_logbook_salmon_angler_trips_by_crc_year_month.csv (optional, check)
#   input_files/pst/lookup_tables/pst_river_block_crosswalk.csv
#
# Output (analysis/pst/outputs/04_interview_proportions/):
#   salmon_directed_share_month.csv   - applied by the assembly
#   logbook_catch_conditional_salmon_target_prop.csv - read by parse_guide_logbook.R
#   salmon_directed_share_vs_logbook.csv
#
# Run order: this script, then parse_guide_logbook.R (which reads the
# conditional proportions); re-run this one afterwards to refresh the
# logbook cross-check.
#
# Usage:
#   Rscript analysis/pst/03_analysis/salmon_directed_share.R
# ==============================================================================

suppressMessages({
  library(tidyverse)
  library(here)
  library(glue)
})
options(width = 200)

source(here("analysis", "pst", "03_analysis", "_target_species_classes.R"))

IN_DIR   <- here("analysis", "pst", "outputs", "04_interview_proportions")
OUT_DIR  <- IN_DIR
LOG_DIR  <- here("analysis", "pst", "outputs", "07_guide_logbook")
CW_PATH  <- here("input_files", "pst", "lookup_tables", "pst_river_block_crosswalk.csv")
CACHE    <- here(".cache")

SALMON_SPECIES <- c("Chinook", "Coho", "Chum", "Pink", "Sockeye")
MIN_ANSWERED   <- 20   # interviews answering a specific target, per tier cell
MIN_MIXED_FISH <- 10   # salmon + steelhead encounters behind an f_mixed value

# ---- 1. Interviews -----------------------------------------------------------

rds <- file.path(IN_DIR, "all_interviews.rds")
csv <- file.path(IN_DIR, "all_interviews.csv")
int <- if (file.exists(rds)) readRDS(rds) else if (file.exists(csv)) {
  read_csv(csv, show_col_types = FALSE, col_types = cols(.default = "c"))
} else {
  stop(glue("No interview pull in {IN_DIR}. Render ",
            "analysis/pst/02_ingest/interview_proportions.qmd first."), call. = FALSE)
}
need <- c("fishery_name", "event_date", "target_species")
miss <- setdiff(need, names(int))
if (length(miss) > 0) stop(glue("all_interviews is missing: {toString(miss)}"), call. = FALSE)

ints <- int |>
  mutate(across(everything(), as.character)) |>
  mutate(
    date  = suppressWarnings(as.Date(event_date)),
    year  = as.integer(lubridate::year(date)),
    month = as.integer(lubridate::month(date)),
    cls   = classify_target(target_species),
    # Angler-weighted, same convention as interview_proportions.qmd; a party
    # with no usable angler_count counts once.
    w = if ("angler_count" %in% names(int)) {
      a <- suppressWarnings(as.numeric(angler_count))
      if_else(is.na(a) | a <= 0, 1, a)
    } else 1
  ) |>
  filter(!is.na(month))

# ---- 2. f_mixed from the catch of "salmon or steelhead" anglers ---------------

caches <- list.files(CACHE, pattern = "^creel_int_catch_.*\\.rds$", full.names = TRUE)
mixed_catch <- NULL
ci_catch <- NULL
if (length(caches) > 0) {
  pulls <- map(caches, readRDS)
  ci <- map(pulls, "interview") |> compact() |> bind_rows() |> distinct()
  cc <- map(pulls, "catch")     |> compact() |> bind_rows() |> distinct()
  key <- "interview_id"
  if (nrow(ci) > 0 && nrow(cc) > 0 && key %in% names(ci) && key %in% names(cc) &&
      "species" %in% names(cc) && "target_species" %in% names(ci)) {
    cc <- cc |>
      mutate(species = str_squish(species),
             n = if ("fish_count" %in% names(cc))
                   coalesce(suppressWarnings(as.numeric(fish_count)), 1) else 1)
    if (!"fishery_name" %in% names(ci)) ci$fishery_name <- ci$.fishery_name
    fish <- cc |>
      group_by(across(all_of(key))) |>
      summarise(salmon = sum(n[species %in% SALMON_SPECIES]),
                sthd   = sum(n[str_detect(coalesce(species, ""),
                                          regex("steelhead", ignore_case = TRUE))]),
                any_catch = sum(n),
                .groups = "drop")
    # Every interview with its catch, for the catch-conditional targeting
    # proportions in section 2b (guide logbook steelhead-only / no-catch trips).
    ci_catch <- ci |>
      mutate(date  = suppressWarnings(as.Date(event_date)),
             month = as.integer(lubridate::month(date)),
             fishery_name = as.character(fishery_name),
             cls = classify_target(target_species),
             guided = if ("trip_guided" %in% names(ci)) trip_guided == "Guided" else NA,
             w = if ("angler_count" %in% names(ci)) {
               a <- suppressWarnings(as.numeric(angler_count))
               if_else(is.na(a) | a <= 0, 1, a)
             } else 1) |>
      filter(!is.na(month)) |>
      distinct(across(all_of(c(key, "fishery_name", "month", "cls", "guided", "w")))) |>
      left_join(fish, by = key) |>
      mutate(across(c(salmon, sthd, any_catch), ~ coalesce(.x, 0)),
             caught = case_when(salmon > 0 ~ "salmon",
                                sthd > 0 ~ "steelhead_only",
                                any_catch > 0 ~ "other_only",
                                TRUE ~ "no_catch"))
    mixed_catch <- ci |>
      mutate(date  = suppressWarnings(as.Date(event_date)),
             year  = as.integer(lubridate::year(date)),
             month = as.integer(lubridate::month(date)),
             fishery_name = as.character(fishery_name)) |>
      filter(classify_target(target_species) == "salmon_or_steelhead", !is.na(month)) |>
      distinct(across(all_of(c(key, "fishery_name", "year", "month")))) |>
      left_join(fish, by = key) |>
      mutate(across(c(salmon, sthd), ~ coalesce(.x, 0)))
    cat(glue("Catch cache: {length(caches)} file(s), ",
             "{n_distinct(mixed_catch$fishery_name)} fisheries with 'salmon or ",
             "steelhead' interviews, {nrow(mixed_catch)} such interviews.\n\n"))
  } else {
    cat("Catch cache present but missing interview_id / species / target_species - f_mixed unavailable.\n\n")
  }
} else {
  cat(glue("No catch cache in {CACHE} - f_mixed unavailable; 'salmon or steelhead' ",
           "counts as salmon (no reduction). Run creel_guided_species_seasonality.R ",
           "\"drano\" (or \".\") to build it.\n\n"))
}

# f_mixed by tier: fishery-year-month -> fishery-month -> fishery.
fmix_tier <- function(keys, tier) {
  if (is.null(mixed_catch)) return(NULL)
  mixed_catch |>
    group_by(across(all_of(keys))) |>
    summarise(mix_salmon = sum(salmon), mix_sthd = sum(sthd), .groups = "drop") |>
    filter(mix_salmon + mix_sthd >= MIN_MIXED_FISH) |>
    mutate(f_mixed = mix_salmon / (mix_salmon + mix_sthd), f_mixed_tier = tier)
}
fm_ym <- fmix_tier(c("fishery_name", "year", "month"), "fishery-year-month")
fm_m  <- fmix_tier(c("fishery_name", "month"), "fishery-month (years pooled)")
fm_f  <- fmix_tier("fishery_name", "fishery (all months)")

# ---- 2b. Catch-conditional targeting proportions (for the guide logbook) -----
# The logbook records catch, never target. A guided trip that caught only
# steelhead, or nothing, inside a salmon-open month may or may not have been
# after salmon. The creel answers that directly: among interviews with the SAME
# catch outcome, what share named a salmon target? parse_guide_logbook.R
# weights those logbook trips by it, replacing the old name-based
# "steelhead-inclusive rivers" rule.
#   p = (salmon + f * salmon_or_steelhead) / answered_specific, angler-weighted,
#   f = the cell's mixed-target salmon:steelhead encounter ratio (as above; 1 if
#       too few fish - flagged in the tier label).
# Keyed on CRC code x month (logbook grain), years pooled (conditional samples
# are thin). Tiers, first with >= MIN_ANSWERED answered interviews:
#   guided crc-month -> guided crc -> all-mode crc-month -> all-mode crc
#   -> guided pooled month -> guided pooled -> all-mode pooled.
cond_prop <- NULL
if (!is.null(ci_catch) && file.exists(CW_PATH)) {
  cw_codes0 <- read_csv(CW_PATH, show_col_types = FALSE) |>
    filter(!is.na(fishery_name), !is.na(crc_areas), crc_areas != "") |>
    distinct(fishery_name, crc_areas) |>
    mutate(crc_code = strsplit(as.character(crc_areas), "\\|")) |>
    unnest(crc_code) |> distinct(fishery_name, crc_code)

  cc_int <- ci_catch |>
    filter(caught %in% c("steelhead_only", "no_catch")) |>
    rename(condition = caught) |>
    left_join(cw_codes0, by = "fishery_name", relationship = "many-to-many")
  mixed_all <- ci_catch |>
    filter(cls == "salmon_or_steelhead") |>
    left_join(cw_codes0, by = "fishery_name", relationship = "many-to-many")

  cprop <- function(keys, guided_only, tier) {
    d <- if (guided_only) filter(cc_int, guided %in% TRUE) else cc_int
    m <- if (guided_only) filter(mixed_all, guided %in% TRUE) else mixed_all
    fk <- setdiff(keys, "condition")
    # Pooled tiers: a fishery spanning several CRC codes was fanned out by the
    # crosswalk join - collapse back to one row per interview.
    if (!"crc_code" %in% keys) {
      d <- d |> select(-crc_code) |> distinct()
      m <- m |> select(-crc_code) |> distinct()
    }
    fm <- m |> group_by(across(all_of(fk))) |>
      summarise(ms = sum(salmon), mt = sum(sthd), .groups = "drop") |>
      mutate(f = if_else(ms + mt >= MIN_MIXED_FISH, ms / (ms + mt), NA_real_))
    d |>
      group_by(across(all_of(keys))) |>
      summarise(
        n_answered = sum(cls %in% c("salmon", "salmon_or_steelhead", "steelhead", "other_species")),
        w_salmon = sum(w[cls == "salmon"]), w_mixed = sum(w[cls == "salmon_or_steelhead"]),
        w_denom  = sum(w[cls %in% c("salmon", "salmon_or_steelhead", "steelhead",
                                    "other_species", "unmapped")]),
        .groups = "drop") |>
      filter(n_answered >= MIN_ANSWERED) |>
      (\(x) if (length(fk) == 0) cross_join(x, fm |> select(f))
             else left_join(x, fm |> select(all_of(fk), f), by = fk))() |>
      mutate(p_salmon_target = (w_salmon + coalesce(f, 1) * w_mixed) / w_denom,
             prop_tier = paste0(tier, if_else(w_mixed > 0 & is.na(f),
                                              "; mixed counted as salmon", "")))
  }
  grid <- bind_rows(
    cc_int |> filter(!is.na(crc_code)) |> distinct(crc_code),
    tibble(crc_code = "*")) |>
    crossing(month = 1:12, condition = c("steelhead_only", "no_catch"))
  tiers <- list(
    list(c("crc_code", "month", "condition"), TRUE,  "guided, crc-month"),
    list(c("crc_code", "condition"),          TRUE,  "guided, crc"),
    list(c("crc_code", "month", "condition"), FALSE, "all modes, crc-month"),
    list(c("crc_code", "condition"),          FALSE, "all modes, crc"),
    list(c("month", "condition"),             TRUE,  "guided, pooled month"),
    list(c("condition"),                      TRUE,  "guided, pooled"),
    list(c("condition"),                      FALSE, "all modes, pooled"))
  cond_prop <- grid
  for (t in tiers) {
    v <- cprop(t[[1]], t[[2]], t[[3]]) |>
      select(all_of(t[[1]]), p_new = p_salmon_target, n_new = n_answered, t_new = prop_tier)
    cond_prop <- cond_prop |> left_join(v, by = t[[1]])
    if (!"p_salmon_target" %in% names(cond_prop)) {
      cond_prop <- cond_prop |> mutate(p_salmon_target = p_new, n_answered = n_new, prop_tier = t_new)
    } else {
      fill <- is.na(cond_prop$p_salmon_target)
      cond_prop$p_salmon_target[fill] <- cond_prop$p_new[fill]
      cond_prop$n_answered[fill]      <- cond_prop$n_new[fill]
      cond_prop$prop_tier[fill]       <- cond_prop$t_new[fill]
    }
    cond_prop <- cond_prop |> select(-p_new, -n_new, -t_new)
  }
  cond_prop <- cond_prop |>
    mutate(p_salmon_target = round(p_salmon_target, 4)) |>
    arrange(condition, crc_code, month)
  write_csv(cond_prop, file.path(OUT_DIR, "logbook_catch_conditional_salmon_target_prop.csv"))

  cat("=== P(salmon-directed | catch outcome), creel interviews, pooled ===\n")
  cond_prop |> filter(crc_code == "*") |>
    group_by(condition, prop_tier) |>
    summarise(months = n(), p = round(mean(p_salmon_target), 3), .groups = "drop") |>
    as.data.frame() |> print(row.names = FALSE)
  cat("\n=== by CRC code (crc-specific tiers only) ===\n")
  cond_prop |> filter(crc_code != "*", str_detect(prop_tier, "crc")) |>
    group_by(condition, crc_code) |>
    summarise(p_mean = round(mean(p_salmon_target), 3), tiers = paste(unique(prop_tier), collapse = " / "),
              .groups = "drop") |>
    as.data.frame() |> print(row.names = FALSE)
  cat("\n")
} else {
  cat("No catch cache - logbook catch-conditional proportions NOT written; ",
      "parse_guide_logbook.R will fall back to its unweighted rules.\n\n")
}

# ---- 3. Target shares by tier ------------------------------------------------

tally <- function(keys, tier) {
  ints |>
    group_by(across(all_of(keys))) |>
    summarise(
      n_interviews = n(),
      n_answered   = sum(cls %in% c("salmon", "salmon_or_steelhead", "steelhead", "other_species")),
      w_salmon     = sum(w[cls == "salmon"]),
      w_mixed      = sum(w[cls == "salmon_or_steelhead"]),
      w_sthd       = sum(w[cls == "steelhead"]),
      w_other      = sum(w[cls %in% c("other_species", "unmapped")]),
      .groups = "drop") |>
    filter(n_answered >= MIN_ANSWERED) |>
    mutate(target_tier = tier)
}
t_ym <- tally(c("fishery_name", "year", "month"), "fishery-year-month")
t_m  <- tally(c("fishery_name", "month"), "fishery-month (years pooled)")
t_f  <- tally("fishery_name", "fishery (all months)")

# Every fishery x year x month the interviews cover. The assembly falls back to
# share = 1 for creel strata with no row here.
cells <- ints |> distinct(fishery_name, year, month)

pick <- function(cells, a, b, c, keys_a, keys_b, keys_c, cols) {
  pa <- if (is.null(a)) NULL else a |> select(all_of(c(keys_a, cols)))
  pb <- if (is.null(b)) NULL else b |> select(all_of(c(keys_b, cols)))
  pc <- if (is.null(c)) NULL else c |> select(all_of(c(keys_c, cols)))
  out <- cells
  for (p in list(list(pa, keys_a), list(pb, keys_b), list(pc, keys_c))) {
    if (is.null(p[[1]])) next
    out <- out |>
      left_join(p[[1]] |> rename_with(~ paste0(.x, ".new"), all_of(cols)), by = p[[2]])
    for (cl in cols) {
      out[[cl]] <- if (cl %in% names(out)) coalesce(out[[cl]], out[[paste0(cl, ".new")]])
                   else out[[paste0(cl, ".new")]]
    }
    out <- out |> select(-ends_with(".new"))
  }
  for (cl in cols) if (!cl %in% names(out)) out[[cl]] <- NA
  out
}

tcols <- c("n_interviews", "n_answered", "w_salmon", "w_mixed", "w_sthd", "w_other", "target_tier")
fcols <- c("mix_salmon", "mix_sthd", "f_mixed", "f_mixed_tier")
K3 <- c("fishery_name", "year", "month"); K2 <- c("fishery_name", "month"); K1 <- "fishery_name"

share <- cells |>
  pick(t_ym, t_m, t_f, K3, K2, K1, tcols) |>
  pick(fm_ym, fm_m, fm_f, K3, K2, K1, fcols) |>
  mutate(
    denom      = w_salmon + w_mixed + w_sthd + w_other,
    has_mixed  = coalesce(w_mixed, 0) > 0,
    f_used     = case_when(!has_mixed ~ NA_real_,
                           !is.na(f_mixed) ~ f_mixed,
                           TRUE ~ 1),
    salmon_directed_share = case_when(
      is.na(denom) | denom == 0 ~ 1,
      TRUE ~ (w_salmon + coalesce(f_used, 0) * w_mixed) / denom),
    share_basis = case_when(
      is.na(denom) | denom == 0 ~ "none: target not answered (share = 1, unadjusted)",
      !has_mixed ~ paste0("target_species, ", target_tier),
      !is.na(f_mixed) ~ paste0("target_species, ", target_tier,
                               "; mixed split by catch, ", f_mixed_tier),
      TRUE ~ paste0("target_species, ", target_tier,
                    "; mixed counted as salmon (no catch split available)"))
  ) |>
  mutate(across(c(w_salmon, w_mixed, w_sthd, w_other, denom), ~ round(.x, 1)),
         across(c(f_mixed, f_used, salmon_directed_share), ~ round(.x, 4))) |>
  select(fishery_name, year, month, salmon_directed_share, share_basis,
         n_interviews, n_answered, w_salmon, w_mixed, w_sthd, w_other,
         f_mixed = f_used, mix_salmon, mix_sthd, target_tier, f_mixed_tier) |>
  arrange(fishery_name, year, month)

write_csv(share, file.path(OUT_DIR, "salmon_directed_share_month.csv"))

cat("=== salmon-directed share, fishery summary (interview-weighted mean of cells) ===\n")
share |>
  group_by(fishery_name) |>
  summarise(months = n(),
            mean_share = round(weighted.mean(salmon_directed_share,
                                             coalesce(n_interviews, 1)), 3),
            min_share = min(salmon_directed_share), max_share = max(salmon_directed_share),
            unadjusted_months = sum(str_starts(share_basis, "none")),
            mixed_split_months = sum(str_detect(share_basis, "mixed split")),
            .groups = "drop") |>
  arrange(mean_share) |> as.data.frame() |> print(row.names = FALSE)

cat("\n=== Drano detail ===\n")
share |> filter(str_detect(fishery_name, regex("drano", ignore_case = TRUE))) |>
  select(fishery_name, year, month, salmon_directed_share, f_mixed, mix_salmon,
         mix_sthd, n_answered, share_basis) |>
  as.data.frame() |> print(row.names = FALSE)

# ---- 4. Guide logbook cross-check (comparison only) ---------------------------

log_csv <- file.path(LOG_DIR, "guide_logbook_salmon_angler_trips_by_crc_year_month.csv")
if (file.exists(log_csv) && file.exists(CW_PATH)) {
  lg <- read_csv(log_csv, show_col_types = FALSE, col_types = cols(crc_code = "c"))
  col_or0 <- function(d, nm) if (nm %in% names(d)) coalesce(d[[nm]], 0) else 0
  lg <- lg |>
    mutate(log_salmon_trips = col_or0(lg, "salmon_caught") +
                              col_or0(lg, "salmon_and_steelhead_in_window"),
           log_sthd_only_trips = col_or0(lg, "steelhead_only_in_window_raw") +
                                 col_or0(lg, "steelhead_only_off_window_raw")) |>
    select(crc_code, year = trip_year, month = trip_month,
           log_salmon_trips, log_sthd_only_trips)

  cw_codes <- read_csv(CW_PATH, show_col_types = FALSE) |>
    filter(!is.na(fishery_name), !is.na(crc_areas), crc_areas != "") |>
    distinct(fishery_name, crc_areas) |>
    mutate(crc_code = strsplit(as.character(crc_areas), "\\|")) |>
    unnest(crc_code) |> distinct(fishery_name, crc_code)

  cmp <- share |>
    select(fishery_name, year, month, salmon_directed_share, share_basis) |>
    inner_join(cw_codes, by = "fishery_name", relationship = "many-to-many") |>
    inner_join(lg, by = c("crc_code", "year", "month")) |>
    group_by(fishery_name, year, month, salmon_directed_share, share_basis) |>
    summarise(crc_codes = paste(sort(unique(crc_code)), collapse = "|"),
              log_salmon_trips = sum(log_salmon_trips),
              log_sthd_only_trips = sum(log_sthd_only_trips), .groups = "drop") |>
    mutate(logbook_salmon_trip_share = if_else(
      log_salmon_trips + log_sthd_only_trips > 0,
      round(log_salmon_trips / (log_salmon_trips + log_sthd_only_trips), 4), NA_real_))
  write_csv(cmp, file.path(OUT_DIR, "salmon_directed_share_vs_logbook.csv"))

  cat("\n=== creel salmon-directed share vs guide logbook salmon-trip share ===\n")
  cat("(logbook = guided trips that caught salmon / (those + steelhead-only trips);",
      "guided, self-reported, catch-conditional - a check, not applied)\n")
  cmp |>
    group_by(fishery_name) |>
    summarise(months = n(),
              creel_share = round(mean(salmon_directed_share), 3),
              logbook_share = round(weighted.mean(logbook_salmon_trip_share,
                                                  log_salmon_trips + log_sthd_only_trips,
                                                  na.rm = TRUE), 3),
              logbook_trips = sum(log_salmon_trips + log_sthd_only_trips),
              .groups = "drop") |>
    arrange(desc(logbook_trips)) |> as.data.frame() |> print(row.names = FALSE)
} else {
  cat("\n(guide logbook salmon-rule file or crosswalk absent - cross-check skipped)\n")
}

cat(glue("\nWrote {file.path(OUT_DIR, 'salmon_directed_share_month.csv')}\n"))
