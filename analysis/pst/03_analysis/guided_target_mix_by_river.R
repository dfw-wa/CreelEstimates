# ==============================================================================
# guided_target_mix_by_river.R
#
# Purpose:
#   What do guided anglers say they are fishing for, river by river and month by
#   month? The creel asks `target_species` directly, and the answer decides how
#   the guide logbook - which records only catch, never target - should be read
#   on each river. The Cowlitz run showed guided effort there is ~93% steelhead;
#   this checks which other rivers look like that and which look like salmon
#   guide fisheries.
#
#   Needs no DB and no catch table. target_species and trip_guided are both in
#   the interview pull that interview_proportions.qmd already persists.
#
# Reports, per river (river_label via the crosswalk, else fishery_name):
#   - coverage: interviews, share with a guided flag, share answering target
#   - guided vs unguided target mix (salmon / steelhead / salmon_or_steelhead /
#     other / nonspecific)
#   - guided share of salmon-targeted interviews - a direct creel figure to set
#     against the logbook floor on that river. Counts PARTIES, and only
#     interviews carrying a guided flag.
#   and, guided only, the target mix by river x month.
#
# Input:
#   analysis/pst/outputs/04_interview_proportions/all_interviews.rds
#   input_files/pst/lookup_tables/pst_river_block_crosswalk.csv
#
# Output (analysis/pst/outputs/08_guide_logbook_diagnostic/):
#   guided_target_mix_by_river.csv
#   guided_target_mix_by_river_month.csv
#
# Usage:
#   Rscript analysis/pst/03_analysis/guided_target_mix_by_river.R
# ==============================================================================

suppressMessages({
  library(tidyverse)
  library(here)
  library(glue)
})
options(width = 200)

source(here("analysis", "pst", "03_analysis", "_target_species_classes.R"))

IN_DIR  <- here("analysis", "pst", "outputs", "04_interview_proportions")
CW_PATH <- here("input_files", "pst", "lookup_tables", "pst_river_block_crosswalk.csv")
OUT_DIR <- here("analysis", "pst", "outputs", "08_guide_logbook_diagnostic")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

rds <- file.path(IN_DIR, "all_interviews.rds")
csv <- file.path(IN_DIR, "all_interviews.csv")
int <- if (file.exists(rds)) readRDS(rds) else if (file.exists(csv)) {
  read_csv(csv, show_col_types = FALSE, col_types = cols(.default = "c"))
} else {
  stop(glue("No interview pull in {IN_DIR}. Render ",
            "analysis/pst/02_ingest/interview_proportions.qmd first."), call. = FALSE)
}

need <- c("fishery_name", "event_date", "trip_guided", "target_species")
miss <- setdiff(need, names(int))
if (length(miss) > 0) stop(glue("all_interviews is missing: {toString(miss)}"), call. = FALSE)

# fishery_name -> river. Fisheries not in the crosswalk (steelhead-only creels,
# out-of-scope years) keep their own name rather than being dropped - they are
# still informative about guide behaviour on that water.
cw <- read_csv(CW_PATH, show_col_types = FALSE) |>
  filter(!is.na(fishery_name)) |>
  distinct(fishery_name, river_label, block)

ints <- int |>
  mutate(across(everything(), as.character)) |>
  left_join(cw, by = "fishery_name") |>
  mutate(
    river  = coalesce(river_label, fishery_name),
    block  = coalesce(block, "(not in crosswalk)"),
    date   = suppressWarnings(as.Date(event_date)),
    month  = lubridate::month(date),
    guided = case_when(trip_guided == "Guided"     ~ "Guided",
                       trip_guided == "Non-guided" ~ "Unguided",
                       TRUE                        ~ NA_character_),
    target_class = classify_target(target_species)
  )

# ---- 1. Per-river summary -----------------------------------------------------

share <- function(cls, which) {
  a <- cls %in% ANSWERED_CLASSES
  if (!any(a)) return(NA_real_)
  round(100 * sum(cls[a] == which) / sum(a), 1)
}

by_river <- ints |>
  group_by(block, river) |>
  summarise(
    interviews    = n(),
    pct_guided_flag = round(100 * mean(!is.na(guided)), 1),
    pct_target_answered = round(100 * mean(target_class %in% ANSWERED_CLASSES), 1),
    guided_n      = sum(guided == "Guided" & target_class %in% ANSWERED_CLASSES, na.rm = TRUE),
    unguided_n    = sum(guided == "Unguided" & target_class %in% ANSWERED_CLASSES, na.rm = TRUE),
    g_salmon      = share(target_class[guided %in% "Guided"], "salmon"),
    g_steelhead   = share(target_class[guided %in% "Guided"], "steelhead"),
    g_salm_or_sthd = share(target_class[guided %in% "Guided"], "salmon_or_steelhead"),
    g_other       = share(target_class[guided %in% "Guided"], "other_species"),
    u_salmon      = share(target_class[guided %in% "Unguided"], "salmon"),
    u_steelhead   = share(target_class[guided %in% "Unguided"], "steelhead"),
    # Direct creel figure for the logbook cross-check: of interviews that named
    # salmon, what share were guided? Parties, not anglers.
    guided_salmon_ints   = sum(guided %in% "Guided" & target_class == "salmon"),
    unguided_salmon_ints = sum(guided %in% "Unguided" & target_class == "salmon"),
    .groups = "drop"
  ) |>
  mutate(
    guided_share_of_salmon = if_else(
      guided_salmon_ints + unguided_salmon_ints > 0,
      round(100 * guided_salmon_ints / (guided_salmon_ints + unguided_salmon_ints), 1),
      NA_real_)
  ) |>
  arrange(block, desc(guided_n))

cat("\n================ GUIDED TARGET MIX BY RIVER ================\n")
cat("g_* = % of guided interviews naming that target; u_* = unguided.\n",
    "Shares are of interviews that answered (blank / 'not asked' excluded).\n",
    "guided_share_of_salmon counts parties, and only interviews with a guided flag.\n\n",
    sep = "")
by_river |>
  select(block, river, interviews, pct_guided_flag, pct_target_answered,
         guided_n, g_salmon, g_steelhead, g_salm_or_sthd, g_other,
         unguided_n, u_salmon, u_steelhead, guided_share_of_salmon) |>
  as.data.frame() |> print(row.names = FALSE)

# Rivers where guides lean differently from everyone else - where applying one
# fishery-wide rule to the logbook would be most wrong.
cat("\n================ WHERE GUIDES DIFFER FROM THE FISHERY (>= 20 guided interviews) ================\n")
by_river |>
  filter(guided_n >= 20, !is.na(g_salmon), !is.na(u_salmon)) |>
  mutate(salmon_gap = g_salmon - u_salmon) |>
  arrange(salmon_gap) |>
  select(block, river, guided_n, g_salmon, u_salmon, salmon_gap, g_steelhead) |>
  as.data.frame() |> print(row.names = FALSE)

# ---- 2. Guided target mix by river x month -----------------------------------

by_month <- ints |>
  filter(guided == "Guided", target_class %in% ANSWERED_CLASSES, !is.na(month)) |>
  count(block, river, month, target_class, name = "n") |>
  group_by(block, river, month) |>
  mutate(guided_interviews = sum(n), pct = round(100 * n / guided_interviews, 1)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = target_class, values_from = pct, values_fill = 0) |>
  arrange(block, river, month)

cat("\n================ GUIDED TARGET MIX BY RIVER AND MONTH (row %) ================\n")
cat("Rivers with fewer than 20 guided interviews overall are omitted here (still in the CSV).\n\n")
keep <- by_river$river[by_river$guided_n >= 20]
by_month |> filter(river %in% keep) |> as.data.frame() |> print(row.names = FALSE)

# ---- 3. Bank vs boat for guided trips ----------------------------------------
# The logbook has no boat/bank field, so assigning guided trips "boat first" is
# an assumption. The creel records location, so it can be checked: what share of
# guided parties - and guided ANGLERS, the logbook's unit - fished from a boat?
#
# Location is derived exactly as in interview_proportions.qmd (angler_type where
# recorded, else boat_used / fish_from_boat), so the two cannot disagree.

has <- function(x) x %in% names(ints)
ints <- ints |>
  mutate(
    .atype = if (has("angler_type"))    angler_type    else NA_character_,
    .bused = if (has("boat_used"))      boat_used      else NA_character_,
    .ffb   = if (has("fish_from_boat")) fish_from_boat else NA_character_,
    location = case_when(
      .atype == "Bank"                    ~ "Bank",
      .atype == "Boat"                    ~ "Boat",
      .bused == "No"                      ~ "Bank",
      .bused == "Yes" & .ffb == "Bank"    ~ "Bank",
      .bused == "Yes"                     ~ "Boat",
      TRUE                                ~ NA_character_
    ),
    anglers = suppressWarnings(as.numeric(if (has("angler_count")) angler_count else NA)),
    anglers = if_else(!is.na(anglers) & anglers > 0, anglers, NA_real_)
  ) |>
  select(-.atype, -.bused, -.ffb)

# Boat share for a subset, by parties and by anglers. Angler weighting uses only
# interviews that recorded a party size.
boat_shares <- function(df) {
  df |>
    filter(!is.na(location)) |>
    summarise(
      located_n      = n(),
      pct_boat_party = round(100 * mean(location == "Boat"), 1),
      pct_boat_angler = if (any(!is.na(anglers)))
        round(100 * sum(anglers[location == "Boat"], na.rm = TRUE) /
                sum(anglers, na.rm = TRUE), 1) else NA_real_,
      .groups = "drop"
    )
}

loc_by_river <- bind_rows(
  ints |> filter(guided %in% "Guided") |>
    group_by(block, river) |> boat_shares() |> mutate(set = "guided, all targets"),
  ints |> filter(guided %in% "Guided", target_class == "salmon") |>
    group_by(block, river) |> boat_shares() |> mutate(set = "guided, salmon target"),
  ints |> filter(guided %in% "Unguided") |>
    group_by(block, river) |> boat_shares() |> mutate(set = "unguided, all targets")
) |>
  select(block, river, set, located_n, pct_boat_party, pct_boat_angler) |>
  arrange(block, river, set)

cat("\n================ GUIDED TRIPS: BANK VS BOAT ================\n")
cat("pct_boat_angler weights by party size (the logbook counts client anglers).\n",
    "Shown only where the set has >= 10 located interviews; all rows are in the CSV.\n\n",
    sep = "")
loc_wide <- loc_by_river |>
  filter(located_n >= 10) |>
  pivot_wider(names_from = set,
              values_from = c(located_n, pct_boat_party, pct_boat_angler),
              names_glue = "{set} | {.value}")
loc_wide |>
  select(block, river,
         any_of(c("guided, all targets | located_n",
                  "guided, all targets | pct_boat_angler",
                  "guided, salmon target | located_n",
                  "guided, salmon target | pct_boat_angler",
                  "unguided, all targets | pct_boat_angler"))) |>
  # "unguided" first - otherwise the "guided" pattern matches inside it.
  rename_with(~ str_replace_all(.x, c("^unguided, all targets \\| " = "u_",
                                       "^guided, all targets \\| " = "g_",
                                       "^guided, salmon target \\| " = "gsalm_"))) |>
  filter(!is.na(g_located_n)) |>
  arrange(desc(g_located_n)) |>
  as.data.frame() |> print(row.names = FALSE)

# The single number the "guided -> boat first" decision rests on.
pooled <- ints |> filter(guided %in% "Guided") |> boat_shares()
pooled_s <- ints |> filter(guided %in% "Guided", target_class == "salmon") |> boat_shares()
cat(glue(
  "\nAll rivers pooled - guided anglers fishing from a boat: {pooled$pct_boat_angler}% ",
  "(n = {pooled$located_n} located guided interviews);\n",
  "salmon-targeted guided only: {pooled_s$pct_boat_angler}% (n = {pooled_s$located_n}).\n"
), "\n")

# ---- 4. Write -----------------------------------------------------------------

p1 <- file.path(OUT_DIR, "guided_target_mix_by_river.csv")
p2 <- file.path(OUT_DIR, "guided_target_mix_by_river_month.csv")
p3 <- file.path(OUT_DIR, "guided_bank_boat_by_river.csv")
write_csv(by_river, p1)
write_csv(by_month, p2)
write_csv(loc_by_river, p3)
cat(glue("\nWrote {p1}\nWrote {p2}\nWrote {p3}\n"), "\n")
