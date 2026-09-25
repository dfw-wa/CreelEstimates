# ==============================================================================
# pst_categorize_mode_location.R
#
# Sourced by pst_fw_angler_trips_assembly.R. Defines categorize_mode_location(),
# the terminal stage that gives EVERY row of effort_long a location (bank/boat)
# and a mode (guided/unguided), with the basis for each recorded. The
# consultant needs all trips categorised, so this runs after every tier (P1,
# P2, P3) exists and after every zeroing correction has run.
#
# Order matters, and follows from the creel interviews
# (guided_target_mix_by_river.R): guided salmon anglers fish from a boat ~96%
# of the time, unguided anglers mostly from the bank. So one bank/boat ratio
# per river would put guided trips on the bank and unguided trips in boats.
#
#   1. LOCATION. Rows already split by the creel design keep that split. Rows
#      with location unknown/combined are split by a creel bank/boat ratio,
#      first available of: river x year x month (LOCATION_USE_MONTH_TIER),
#      river x year, river, block x year, block, all. The month-vs-annual
#      comparison is written to pst_fw_location_month_sensitivity.csv.
#   2. GUIDED TOTAL. Guide logbook salmon-directed angler-trips
#      (parse_guide_logbook.R) are matched to each unit by CRC code x year x
#      month - a P1 row's own month, all months for P2/P3 rows (which are
#      annual; the logbook table is already restricted to salmon windows).
#      Rows with no CRC code use the crosswalk's codes for their river. When
#      several units claim the same logbook cell (shared CRC codes, bank and
#      boat of one stratum), it is split in proportion to trips - never counted
#      twice. Guided = min(logbook, unit total): a minimum, with reporting bias
#      acknowledged. No logbook record -> guided = 0, as a strict floor.
#   3. GUIDED BY LOCATION. Guided trips go to boat at the creel's measured
#      guided-salmon boat share (river-specific where >= 20 interviews, else
#      pooled 95.8%). If that exceeds the boat stratum, the excess moves to
#      bank and is flagged. Unguided = what is left in each stratum.
#
# Trip and harvest totals are conserved exactly; the assembly logs a blocker
# otherwise. Everything is written to pst_fw_categorization_audit.csv.
# ==============================================================================

GUIDED_BOAT_SHARE_POOLED <- 0.958   # creel, guided salmon-targeted, all rivers
GUIDED_BOAT_MIN_N        <- 20      # interviews needed for a river-specific share

LOGBOOK_SALMON_PATH <- here("analysis", "pst", "outputs", "07_guide_logbook",
                            "guide_logbook_salmon_angler_trips_by_crc_year_month.csv")
GUIDED_BOAT_PATH    <- here("analysis", "pst", "outputs", "08_guide_logbook_diagnostic",
                            "guided_bank_boat_by_river.csv")

UNIT_KEYS <- c("block", "river_label", "fishery_name", "catch_area_code",
               "year", "month", "tier", "source_id")

# Location split, as a standalone step so it can be run with and without the
# month level for the sensitivity check. Rows already bank/boat are kept; the
# rest are split by the first available creel boat share (trip-weighted, from
# the kept rows):
#   [river x year x month, if use_month] -> river x year -> river
#   -> [river x year, river: creel INTERVIEW boat share, CRC-salmon-weighted
#       months - interview_river_boat_share.R; rivers with no P1 design split
#       of their own, e.g. Lewis, Kalama, Wind, Klickitat]
#   -> block x year -> block -> all creels.
# Month only matches rows that carry one (P1 creel strata); P2/P3 rows are
# annual and start at river x year either way.
LOCATION_USE_MONTH_TIER <- TRUE
USE_INTERVIEW_RIVER_SHARE <- TRUE
INTERVIEW_SHARE_PATH <- here("analysis", "pst", "outputs", "04_interview_proportions",
                             "interview_boat_share_river_year.csv")

read_interview_share <- function() {
  if (!USE_INTERVIEW_RIVER_SHARE || !file.exists(INTERVIEW_SHARE_PATH)) return(NULL)
  read_csv(INTERVIEW_SHARE_PATH, show_col_types = FALSE) |> filter(usable)
}

split_location <- function(el, use_month) {
  known <- el |> filter(location %in% c("bank", "boat"), angler_trips > 0)
  ratio_at <- function(...) {
    known |> group_by(...) |>
      summarise(.boat = sum(angler_trips[location == "boat"]) / sum(angler_trips),
                .groups = "drop")
  }
  r_rym <- ratio_at(river_label, year, month) |> filter(!is.na(month)) |> rename(p_rym = .boat)
  r_ry  <- ratio_at(river_label, year) |> rename(p_ry = .boat)
  r_r   <- ratio_at(river_label)       |> rename(p_r  = .boat)
  r_by  <- ratio_at(block, year)       |> rename(p_by = .boat)
  r_b   <- ratio_at(block)             |> rename(p_b  = .boat)
  p_all <- if (nrow(known) > 0)
    sum(known$angler_trips[known$location == "boat"]) / sum(known$angler_trips) else 0.5
  ish <- read_interview_share()
  r_iy <- if (is.null(ish)) tibble(river_label = character(), year = integer(), p_iy = double()) else
    ish |> filter(level == "river-year") |> transmute(river_label, year = as.integer(year), p_iy = p_boat)
  r_i  <- if (is.null(ish)) tibble(river_label = character(), p_i = double()) else
    ish |> filter(level == "river") |> transmute(river_label, p_i = p_boat)

  to_split <- el |> filter(!location %in% c("bank", "boat"))
  kept     <- el |> filter(location %in% c("bank", "boat"))
  if (nrow(to_split) == 0) return(el)

  to_split <- to_split |>
    left_join(r_rym, by = c("river_label", "year", "month")) |>
    left_join(r_ry,  by = c("river_label", "year")) |>
    left_join(r_r,   by = "river_label") |>
    left_join(r_iy,  by = c("river_label", "year")) |>
    left_join(r_i,   by = "river_label") |>
    left_join(r_by,  by = c("block", "year")) |>
    left_join(r_b,   by = "block") |>
    mutate(
      p_rym = if (use_month) p_rym else NA_real_,
      .p = coalesce(p_rym, p_ry, p_r, p_iy, p_i, p_by, p_b, p_all),
      location_basis = case_when(
        !is.na(p_rym) ~ "imputed: river-year-month creel ratio",
        !is.na(p_ry)  ~ "imputed: river-year creel ratio",
        !is.na(p_r)   ~ "imputed: river creel ratio (all years)",
        !is.na(p_iy)  ~ "imputed: river-year interview boat share (CRC-weighted months)",
        !is.na(p_i)   ~ "imputed: river interview boat share (CRC-weighted months, all years)",
        !is.na(p_by)  ~ "imputed: block-year creel ratio",
        !is.na(p_b)   ~ "imputed: block creel ratio (all years)",
        TRUE          ~ "imputed: all-creel ratio"
      ),
      location_basis = paste0(location_basis,
                              if_else(location == "combined", " [source combined bank/boat]", ""))
    ) |>
    select(-p_rym, -p_ry, -p_r, -p_iy, -p_i, -p_by, -p_b)
  split_rows <- bind_rows(
    to_split |> mutate(location = "boat",
                       angler_trips = angler_trips * .p,
                       total_salmon_harvest = total_salmon_harvest * .p),
    to_split |> mutate(location = "bank",
                       angler_trips = angler_trips * (1 - .p),
                       total_salmon_harvest = total_salmon_harvest * (1 - .p))
  ) |> select(-.p)
  bind_rows(kept, split_rows)
}

# Interview share vs creel design split, for rivers that have both - the
# check on whether interviews can stand in for a design split at all
# (interviews over- or under-sample boat anglers depending on access).
write_interview_vs_design <- function(el) {
  ish <- read_interview_share()
  if (is.null(ish)) {
    log_gap("categorize", NA, "note", glue(
      "interview river boat shares not applied - {INTERVIEW_SHARE_PATH} ",
      "{if (file.exists(INTERVIEW_SHARE_PATH)) 'has no usable rows' else 'not found'}."))
    return(invisible(NULL))
  }
  design <- el |> filter(location %in% c("bank", "boat"), angler_trips > 0) |>
    group_by(river_label, year) |>
    summarise(design_trips = sum(angler_trips),
              p_design = sum(angler_trips[location == "boat"]) / sum(angler_trips),
              .groups = "drop")
  cmp <- ish |> filter(level == "river-year") |>
    transmute(river_label, year = as.integer(year), p_interview = p_boat, n_located) |>
    inner_join(design, by = c("river_label", "year")) |>
    mutate(diff_pts = round(100 * (p_interview - p_design), 1),
           across(c(p_interview, p_design), ~ round(.x, 3)))
  write_csv(cmp, file.path(OUT_DIR, "pst_fw_location_interview_vs_design.csv"))
  iv_rivers <- sort(unique(ish$river_label))
  log_gap("categorize", NA, "note", glue(
    "interview river boat shares usable for {length(iv_rivers)} rivers: ",
    "{paste(iv_rivers, collapse = '; ')}. Rivers with a P1 design split: ",
    "{paste(sort(unique(design$river_label)), collapse = '; ')}."))
  if (nrow(cmp) == 0) {
    log_gap("categorize", NA, "note",
            "interview vs design: no river-year has both - validation not possible this run.")
  }
  if (nrow(cmp) > 0) {
    log_gap("categorize", NA, "note", glue(
      "interview vs design boat share on {nrow(cmp)} river-years with both: ",
      "median difference {median(cmp$diff_pts)} pts (interview - design), range ",
      "{min(cmp$diff_pts)} to {max(cmp$diff_pts)}. See pst_fw_location_interview_vs_design.csv."))
    cat("\n=== interview vs design boat share (river-years with both) ===\n")
    print(as.data.frame(cmp), row.names = FALSE)
  }
  invisible(cmp)
}

# Boat trips with vs without the month level, by block x river x tier x year,
# over the IMPUTED rows only (measured rows are identical by construction).
write_location_month_sensitivity <- function(a, b) {
  roll <- function(d, tag) {
    d |> filter(grepl("^imputed", coalesce(location_basis, ""))) |>
      group_by(block, river_label, tier, year) |>
      summarise(imputed_trips = sum(angler_trips),
                "boat_{tag}" := sum(angler_trips[location == "boat"]),
                # Exact label: the interview labels also contain "months".
                "month_level_trips_{tag}" := sum(angler_trips[grepl("river-year-month creel", location_basis, fixed = TRUE)]),
                .groups = "drop")
  }
  sens <- full_join(roll(a, "annual"), roll(b, "month") |> select(-imputed_trips),
                    by = c("block", "river_label", "tier", "year")) |>
    mutate(across(where(is.numeric) & !year, ~ coalesce(.x, 0)),
           boat_share_annual = if_else(imputed_trips > 0, boat_annual / imputed_trips, NA_real_),
           boat_share_month  = if_else(imputed_trips > 0, boat_month / imputed_trips, NA_real_),
           boat_trips_shift  = boat_month - boat_annual,
           pct_imputed_on_month_level = if_else(imputed_trips > 0,
                                                100 * month_level_trips_month / imputed_trips, NA_real_)) |>
    select(-month_level_trips_annual) |>
    mutate(across(c(boat_share_annual, boat_share_month), ~ round(.x, 4)),
           across(c(imputed_trips, boat_annual, boat_month, boat_trips_shift,
                    month_level_trips_month, pct_imputed_on_month_level), ~ round(.x, 1))) |>
    arrange(desc(abs(boat_trips_shift)))
  write_csv(sens, file.path(OUT_DIR, "pst_fw_location_month_sensitivity.csv"))

  tot_imp <- sum(sens$imputed_trips)
  shift   <- sum(sens$boat_trips_shift)
  gross   <- sum(abs(sens$boat_trips_shift))
  on_m    <- sum(sens$month_level_trips_month)
  log_gap("categorize", NA, "note", glue(
    "location month sensitivity: {round(on_m)} of {round(tot_imp)} imputed trips ",
    "reach the river-year-month level; boat trips shift by {round(shift)} net ",
    "({round(gross)} gross) vs annual ratios; applied = ",
    "{if (LOCATION_USE_MONTH_TIER) 'month' else 'annual'}. ",
    "Detail: pst_fw_location_month_sensitivity.csv"))
  cat("\n=== Location: river-year-month level vs annual (imputed rows) ===\n")
  cat(glue("  imputed trips {format(round(tot_imp), big.mark = ',')}; on month level ",
           "{format(round(on_m), big.mark = ',')}; boat shift net {round(shift)}, ",
           "gross {round(gross)}\n\n"))
  print(as.data.frame(head(sens |> filter(boat_trips_shift != 0), 15)), row.names = FALSE)
  invisible(sens)
}

categorize_mode_location <- function(effort_long, crosswalk) {

  el <- effort_long |>
    mutate(location     = tolower(coalesce(location, "unknown")),
           angler_trips = coalesce(angler_trips, 0),
           total_salmon_harvest = coalesce(total_salmon_harvest, 0),
           .unit = paste(block, river_label, fishery_name, catch_area_code,
                         year, month, tier, source_id, sep = "\u001f"))

  trips_in   <- sum(el$angler_trips)
  harvest_in <- sum(el$total_salmon_harvest)

  # ---- 1. Location ----------------------------------------------------------
  # Run both ways - with and without the river x year x month level - and keep
  # the comparison, so the month sensitivity is on file whichever is applied.
  write_interview_vs_design(el)
  el_nomonth <- split_location(el, use_month = FALSE)
  el_month   <- split_location(el, use_month = TRUE)
  write_location_month_sensitivity(el_nomonth, el_month)
  el <- if (LOCATION_USE_MONTH_TIER) el_month else el_nomonth

  # One row per unit, bank and boat side by side.
  units <- el |>
    group_by(.unit, across(all_of(UNIT_KEYS))) |>
    summarise(
      B  = sum(angler_trips[location == "boat"]),
      K  = sum(angler_trips[location == "bank"]),
      HB = sum(total_salmon_harvest[location == "boat"]),
      HK = sum(total_salmon_harvest[location == "bank"]),
      location_basis = paste(sort(unique(coalesce(location_basis, "design_stratum"))),
                             collapse = "; "),
      method = paste(sort(unique(na.omit(method))), collapse = "; "),
      n_strata = suppressWarnings(max(n_strata, na.rm = TRUE)),
      n_strata_estimated = suppressWarnings(max(n_strata_estimated, na.rm = TRUE)),
      prop_strata_estimated = suppressWarnings(min(prop_strata_estimated, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    mutate(across(c(n_strata, n_strata_estimated, prop_strata_estimated),
                  ~ if_else(is.finite(.x), .x, NA_real_)),
           Tt = B + K)

  # ---- 2. Guided total from the logbook -------------------------------------
  lb <- if (file.exists(LOGBOOK_SALMON_PATH)) {
    read_csv(LOGBOOK_SALMON_PATH, show_col_types = FALSE) |>
      transmute(code = as.character(crc_code), year = as.integer(trip_year),
                month = as.integer(trip_month), L = angler_trips)
  } else NULL

  if (is.null(lb)) {
    log_gap("guide_logbook", NA, "blocker",
            glue("{LOGBOOK_SALMON_PATH} not found - run parse_guide_logbook.R. ",
                 "Without it guided trips cannot be assigned; every row is left ",
                 "mode = 'unknown'."))
  }

  river_codes <- crosswalk |>
    filter(!is.na(crc_areas), crc_areas != "") |>
    distinct(river_label, crc_areas) |>
    mutate(code = strsplit(crc_areas, "\\|")) |>
    select(-crc_areas) |> unnest(code) |> distinct()

  cells <- units |>
    select(.unit, river_label, catch_area_code, year, month, Tt) |>
    mutate(own_code = !is.na(catch_area_code) & catch_area_code != "NA")
  cells <- bind_rows(
    cells |> filter(own_code) |> mutate(code = catch_area_code),
    cells |> filter(!own_code) |> select(-catch_area_code) |>
      inner_join(river_codes, by = "river_label", relationship = "many-to-many")
  ) |>
    mutate(month = as.integer(month)) |>
    group_by(.unit) |>
    mutate(n_codes = n_distinct(code)) |>
    ungroup()
  cells <- bind_rows(
    cells |> filter(!is.na(month)),
    cells |> filter(is.na(month)) |> select(-month) |> crossing(month = 1:12)
  ) |>
    group_by(.unit) |>
    mutate(w = Tt / n()) |>
    ungroup()

  units_with_codes <- unique(cells$.unit)

  alloc <- tibble(.unit = character(), G_raw = double())
  lb_unallocated <- 0
  if (!is.null(lb)) {
    claimed <- cells |>
      inner_join(lb, by = c("code", "year", "month")) |>
      group_by(code, year, month) |>
      mutate(share = if (sum(w) > 0) w / sum(w) else 0) |>
      ungroup() |>
      mutate(alloc = L * share)
    alloc <- claimed |> group_by(.unit) |>
      summarise(G_raw = sum(alloc), .groups = "drop")
    in_scope_lb <- lb |> filter(year %in% unique(units$year))
    lb_unallocated <- sum(in_scope_lb$L) - sum(alloc$G_raw)
  }

  # ---- 3. Guided by location -------------------------------------------------
  pg <- if (file.exists(GUIDED_BOAT_PATH)) {
    read_csv(GUIDED_BOAT_PATH, show_col_types = FALSE) |>
      filter(set == "guided, salmon target", located_n >= GUIDED_BOAT_MIN_N,
             !is.na(pct_boat_angler)) |>
      transmute(river_label = river, p_g = pct_boat_angler / 100) |>
      distinct(river_label, .keep_all = TRUE)
  } else {
    log_gap("guided_boat_share", NA, "note",
            glue("{GUIDED_BOAT_PATH} not found - pooled guided boat share ",
                 "{GUIDED_BOAT_SHARE_POOLED} used for every river. Run ",
                 "guided_target_mix_by_river.R for river-specific shares."))
    tibble(river_label = character(), p_g = double())
  }

  units <- units |>
    left_join(alloc, by = ".unit") |>
    left_join(pg, by = "river_label") |>
    mutate(
      p_g_basis = if_else(is.na(p_g), "pooled", "river"),
      p_g   = coalesce(p_g, GUIDED_BOAT_SHARE_POOLED),
      G_raw = coalesce(G_raw, 0),
      capped = G_raw > Tt + 1e-9,
      G  = pmin(G_raw, Tt),
      gb = pmin(G * p_g, B),
      gk = G - gb,
      gb = if_else(gk > K, G - K, gb),
      gk = G - gb,
      spill = abs(gb - G * p_g) > 1e-6 & G > 0,
      mode_basis = case_when(
        is.null(lb)                     ~ "guide logbook unavailable",
        G > 0                           ~ "guide logbook (minimum)",
        .unit %in% units_with_codes     ~ "no logbook record (floor = 0)",
        TRUE                            ~ "no CRC link to logbook (floor = 0)"
      ),
      mode_basis = paste0(mode_basis,
                          if_else(capped, "; logbook capped at trip total", ""),
                          if_else(spill, "; guided spill between bank/boat", ""))
    )

  shape <- function(loc, md, trips, harvest_stratum, trips_stratum) {
    units |> transmute(
      across(all_of(UNIT_KEYS)), location = loc, mode = md,
      angler_trips = trips,
      total_salmon_harvest = if_else(trips_stratum > 0,
                                     harvest_stratum * trips / trips_stratum, 0),
      location_basis, mode_basis, method,
      n_strata, n_strata_estimated, prop_strata_estimated
    )
  }
  out <- if (is.null(lb)) {
    bind_rows(shape("boat", "unknown", units$B, units$HB, units$B),
              shape("bank", "unknown", units$K, units$HK, units$K))
  } else {
    bind_rows(
      shape("boat", "guided",   units$gb,            units$HB, units$B),
      shape("boat", "unguided", units$B - units$gb,  units$HB, units$B),
      shape("bank", "guided",   units$gk,            units$HK, units$K),
      shape("bank", "unguided", units$K - units$gk,  units$HK, units$K)
    )
  }
  # Zero-trip units keep a single row so zeroing corrections stay traceable.
  out <- out |>
    group_by(across(all_of(UNIT_KEYS))) |>
    filter(angler_trips > 0 | (sum(angler_trips) == 0 & row_number() == 1)) |>
    ungroup() |>
    canon()

  # ---- Checks and audit ------------------------------------------------------
  trips_out   <- sum(out$angler_trips)
  harvest_out <- sum(out$total_salmon_harvest)
  if (abs(trips_out - trips_in) > 1e-6 * max(1, trips_in) ||
      abs(harvest_out - harvest_in) > 1e-6 * max(1, harvest_in)) {
    log_gap("categorize", NA, "blocker",
            glue("categorization changed totals: trips {round(trips_in)} -> ",
                 "{round(trips_out)}, harvest {round(harvest_in)} -> {round(harvest_out)}"))
  }
  n_unknown <- sum(out$angler_trips[out$mode == "unknown" | out$location == "unknown"])
  if (n_unknown > 0 && !is.null(lb)) {
    log_gap("categorize", NA, "blocker",
            glue("{round(n_unknown)} angler trips still uncategorized after the terminal stage."))
  }

  pct <- function(x) round(100 * x / max(trips_in, 1), 1)
  lb_note <- if (is.null(lb)) "logbook missing" else glue(
    "logbook salmon angler-trips allocated {round(sum(units$G_raw))}, ",
    "capped to {round(sum(units$G))} after the trip-total cap; ",
    "{round(lb_unallocated)} in-scope logbook trips matched no estimate row")
  log_gap("categorize", NA, "note", glue(
    "all rows categorized. Guided {round(sum(units$G))} trips ",
    "({pct(sum(units$G))}% of {round(trips_in)}); ",
    "location imputed for {pct(sum(el$angler_trips[!is.na(el$location_basis) & grepl('^imputed', el$location_basis)]))}% ",
    "of trips; {sum(units$spill)} units needed a bank/boat guided spill; ",
    "{sum(units$capped)} units had logbook > trip total. {lb_note}."))

  audit <- units |>
    transmute(across(all_of(UNIT_KEYS)), total_trips = Tt, boat_trips = B,
              bank_trips = K, location_basis, logbook_allocated = G_raw,
              guided = G, guided_boat = gb, guided_bank = gk,
              guided_boat_share_used = p_g, guided_boat_share_basis = p_g_basis,
              capped, spill, mode_basis)
  write_csv(audit, file.path(OUT_DIR, "pst_fw_categorization_audit.csv"))

  out
}
