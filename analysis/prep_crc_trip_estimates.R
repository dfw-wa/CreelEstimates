# ==============================================================================
# prep_crc_trip_estimates.R
#
# Purpose:
#   Extract Historic Salmon Effort (CRC angler trips) from the NEPA PS
#   Recreational Effort Estimates Excel workbook and produce a tidy data frame
#   joinable to analysis/outputs/multi_fishery_trip_summary.rds on the keys
#   year × month × crc_area.
#
#   NOTE: The data with rivers in column A is in the "2026 FW Effort Projection"
#   tab, not the "2026 Marine Effort Projection" tab (which contains marine
#   areas 5-13 with no river-level identifiers). The note here is flagged in
#   case the user intended the marine areas — see the Marine tab read below.
#
# Excel sheet structure ("2026 FW Effort Projection"):
#   Row 1  : Section headers — "Historic Salmon Catch" / "Historic Salmon Effort"
#   Row 3  : Year labels (2005-2024 repeated for catch block, then effort block)
#   Row 4+ : River name in col A (only on first month row; NA otherwise), col B
#            = month (1-12), followed by data columns
#
# Inputs:
#   input_files/NEPA PS_Recreational Effort Estimates ... .xlsx
#   input_files/crc_area_lut.csv
#
# Outputs:
#   analysis/outputs/crc_trip_estimates.rds          — tidy trip data
#   analysis/outputs/crc_trip_estimates_match_review.csv — fuzzy match audit
# ==============================================================================

library(tidyverse)
library(readxl)
library(here)
library(cli)

# 0. Paths -------------------------------------------------------------------

xlsx_path <- here(
  "input_files",
  "NEPA PS_Recreational Effort Estimates 2025_2026_for 2026_2027 Projections_4_21_2026.xlsx"
)
lut_path  <- here("input_files", "crc_area_lut.csv")
out_dir   <- here("analysis", "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# 1. Read raw sheet and identify effort-year column positions ----------------

SHEET <- "2026 FW Effort Projection"
raw   <- read_excel(xlsx_path, sheet = SHEET, col_names = FALSE)

# Row 3 holds year labels; effort block follows the catch block
year_labels <- as.character(unlist(raw[3, ]))
all_year_positions <- which(year_labels %in% as.character(2005:2024))

if (length(all_year_positions) < 40L) {
  cli::cli_abort(
    "Expected 40 year-labelled columns (20 catch + 20 effort); \\
     found {length(all_year_positions)}. Check the sheet structure."
  )
}

effort_year_positions <- all_year_positions[21:40]   # second block = effort
effort_years          <- as.integer(year_labels[effort_year_positions])

cli::cli_alert_info(
  "Effort columns: cols {min(effort_year_positions)}-{max(effort_year_positions)}, \\
   years {min(effort_years)}-{max(effort_years)}."
)


# 2. Extract and tidy the data block ----------------------------------------

# Select only river-name, month, and the 20 effort-year columns.
# Coerce all columns to character first to handle mixed-type cells
# (some effort cells contain "closed" text alongside numeric values).
data_raw <- raw[4:nrow(raw), c(1L, 2L, effort_year_positions)] |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
  dplyr::rename_with(~ c("river_raw", "month_raw", as.character(effort_years)))

# Forward-fill river names (each river name appears only on its month-1 row)
data_raw <- data_raw |>
  tidyr::fill(river_raw, .direction = "down")

# Keep only rows where month_raw is strictly a numeric month 1-12
salmon_effort <- data_raw |>
  dplyr::mutate(month = suppressWarnings(as.integer(month_raw))) |>
  dplyr::filter(!is.na(month), dplyr::between(month, 1L, 12L)) |>
  dplyr::select(-month_raw) |>
  # Drop summary / footer rows that survived the fill (Grand Total etc.)
  dplyr::filter(!grepl("grand total|summari|methodology|region|total$",
                        river_raw, ignore.case = TRUE))

# Pivot to long: one row per river × month × year
salmon_effort_long <- salmon_effort |>
  tidyr::pivot_longer(
    cols      = as.character(effort_years),
    names_to  = "year",
    values_to = "crc_trips"
  ) |>
  dplyr::mutate(
    year      = as.integer(year),
    crc_trips = suppressWarnings(as.numeric(crc_trips))
  )

cli::cli_alert_success(
  "Parsed {dplyr::n_distinct(salmon_effort_long$river_raw)} rivers, \\
   {nrow(salmon_effort_long)} river × month × year rows."
)


# 3. Fuzzy match river names to crc_area_lut --------------------------------
# Uses base-R adist() (Levenshtein edit distance) normalised by max string
# length → similarity in [0, 1].  Matches with similarity < THRESHOLD are
# flagged for manual review.

lut <- readr::read_csv(lut_path, show_col_types = FALSE)

# Search the full LUT — Puget Sound rivers may be missing from the PS-only
# subset if they have different region codings in the table.
lut_search <- lut

# Normalise helper: lowercase, expand abbreviations, strip non-alpha-numeric.
# Parenthetical qualifiers ARE stripped so "Skokomish River (Mason Co.)"
# normalises to "skokomish river" and can match cleanly against "SKOKOMISH R.".
# Disambiguation between same-name rivers in different regions is handled by
# the Puget Sound region bonus applied after computing similarities (see below).
normalise <- function(x) {
  x |>
    tolower() |>
    stringr::str_replace_all(c(
      "(?<![a-z])r[.]"      = "river",   # "R." -> "river"
      "(?<![a-z])cr[.]"     = "creek",   # "CR." -> "creek"
      "(?<![a-z])lk[.]?"    = "lake",    # "LK." or bare "LK" -> "lake"
      "r's\\b"              = "rivers",
      "[/\\-]"              = " "
    )) |>
    # strip parenthetical qualifiers AFTER abbreviation expansion
    stringr::str_remove_all("\\s*\\([^)]*\\)") |>
    stringr::str_remove_all("[^a-z0-9 ]") |>
    stringr::str_squish()
}

river_names_unique <- unique(salmon_effort_long$river_raw)
rivers_norm <- normalise(river_names_unique)
lut_norm    <- normalise(lut_search$catch_area_description)

# Levenshtein distance matrix (rows = Excel rivers, cols = LUT entries)
dist_mat <- utils::adist(rivers_norm, lut_norm, ignore.case = TRUE)
rownames(dist_mat) <- river_names_unique
colnames(dist_mat) <- lut_search$catch_area_description

# Normalised similarity: 1 − dist / max(nchar(a), nchar(b))
len_mat <- outer(nchar(rivers_norm), nchar(lut_norm), FUN = pmax)
sim_mat <- 1 - dist_mat / len_mat
sim_mat[!is.finite(sim_mat)] <- 0

# Puget Sound region bonus: PS entries get a 20 % similarity boost so that
# a PS-specific entry (e.g. "Skokomish River (Mason Co.)" → "skokomish river")
# reliably outscores a shorter wrong match (e.g. "Snohomish River") that would
# otherwise win on raw edit distance alone.
# The bonus is NOT capped at 1.0 during selection so that the exact PS match
# (raw 1.0 → boosted 1.2) reliably beats a near PS match (raw 0.93 → boosted
# 1.12) when both would otherwise cap at 1.0 and tie-break by index.
# The stored `similarity` value is always the pre-bonus raw score.
PS_BONUS <- 1.20
is_ps <- lut_search$catch_area_region == "Puget Sound" & !is.na(lut_search$catch_area_region)
sim_mat_boosted          <- sim_mat
sim_mat_boosted[, is_ps] <- sim_mat[, is_ps] * PS_BONUS   # no upper cap for selection

best_idx        <- apply(sim_mat_boosted, 1, which.max)
best_similarity <- sim_mat[cbind(seq_len(nrow(sim_mat)), best_idx)]   # raw score

SIMILARITY_THRESHOLD <- 0.70   # below this, flag for manual review

match_table <- tibble::tibble(
  river_name_crc      = river_names_unique,
  river_norm          = rivers_norm,
  matched_description = lut_search$catch_area_description[best_idx],
  crc_area_id         = lut_search$crc_area_id[best_idx],
  catch_area_code     = lut_search$catch_area_code[best_idx],
  catch_area_region   = lut_search$catch_area_region[best_idx],
  similarity          = round(best_similarity, 3),
  # Flag if similarity is below threshold OR the matched region is not Puget
  # Sound (the FW effort data covers Puget Sound salmon rivers).
  region_mismatch     = !is.na(lut_search$catch_area_region[best_idx]) &
                          lut_search$catch_area_region[best_idx] != "Puget Sound",
  review_needed       = best_similarity < SIMILARITY_THRESHOLD |
                          (!is.na(lut_search$catch_area_region[best_idx]) &
                           lut_search$catch_area_region[best_idx] != "Puget Sound")
)

n_flagged <- sum(match_table$review_needed)
cli::cli_alert_info(
  "Fuzzy match complete. {n_flagged}/{nrow(match_table)} rivers flagged \\
   (similarity < {SIMILARITY_THRESHOLD}):"
)
if (n_flagged > 0) {
  match_table |>
    dplyr::filter(review_needed) |>
    dplyr::select(river_name_crc, matched_description, similarity) |>
    dplyr::arrange(similarity) |>
    print(n = Inf)
}


# 4. Join match results onto effort data ------------------------------------

crc_trips <- salmon_effort_long |>
  dplyr::left_join(match_table, by = c("river_raw" = "river_name_crc")) |>
  dplyr::transmute(
    river_name_crc      = river_raw,
    crc_area            = matched_description,   # aligns with crc_area in creel output
    crc_area_id,
    catch_area_code,
    catch_area_region,
    year,
    month,
    crc_trips,
    match_similarity    = similarity,
    region_mismatch,
    review_needed
  ) |>
  dplyr::arrange(crc_area, year, month)


# 5. Save outputs ------------------------------------------------------------

out_rds <- file.path(out_dir, "crc_trip_estimates.rds")
out_csv <- file.path(out_dir, "crc_trip_estimates_match_review.csv")

saveRDS(crc_trips, out_rds)
cli::cli_alert_success(
  "Saved {nrow(crc_trips)} rows ({dplyr::n_distinct(crc_trips$river_name_crc)} \\
   rivers) to {.path {out_rds}}"
)

readr::write_csv(
  dplyr::arrange(match_table, dplyr::desc(review_needed), similarity),
  out_csv
)
cli::cli_alert_success("Match review table saved to {.path {out_csv}}")


# 6. Usage note --------------------------------------------------------------
#
# Join to creel-based trip estimates:
#
  creel <- readRDS(here("analysis", "outputs", "multi_fishery_trip_summary.rds"))
  crc   <- readRDS(here("analysis", "outputs", "crc_trip_estimates.rds"))

  # Aggregate creel totals across angler type before joining
  creel_agg <- creel |>
    mutate(crc_area = as.character(crc_area)) |> 
    dplyr::group_by(year, month, crc_area, pe_period) |>
    dplyr::summarise(
      creel_trips_est = sum(total_trips_est, na.rm = TRUE),
      .groups = "drop"
    )

  comparison <- creel_agg |>
    dplyr::left_join(
      dplyr::select(crc, crc_area, year, month, crc_trips, review_needed),
      by = c("crc_area", "year", "month")
    )
