# ==============================================================================
# build_columbia_basin_trip_summary.R
# Location: analysis/columbia_basin_trip_summary/build_columbia_basin_trip_summary.R
#
# Purpose: compile and standardize Columbia basin recreational salmon angler
# trip estimates for Raquel Crosier (WDFW Fish Program Deputy Director of
# Regions). Combines TWO genuinely different kinds of estimate that have
# never been put side by side before:
#
#   MAINSTEM (Buoy 10, Lower Columbia below Bonneville, Bonneville-McNary) -
#     joint ODFW & WDFW design-based creel estimates, read directly from the
#     two workbooks ODFW supplied. Not part of the PST tributary pipeline at
#     all - this is the water that pipeline's own crosswalk explicitly
#     excludes as "ColumbiaMainstem, OUT_OF_SCOPE" (dam-to-dam reach
#     segments, not tributaries).
#
#   TRIBUTARY (Columbia Lower/Middle/Upper blocks, Snake) - this repo's own
#     WDFW-only PST freshwater pipeline output (P1/P2/P3 tiered), read from
#     analysis/pst/outputs/05_assembly/pst_fw_trips_by_mode_location.csv.
#     Real creel data where it exists, CRC-ratio expansion/projection where
#     it doesn't - see the P1/P2/P3 plain-language mapping below and the
#     "Methods & Data Sources" tab this script writes.
#
# Every row in the output is tagged with its Data Source so the two are
# never silently blended: "ODFW & WDFW (joint creel estimate)" for the
# mainstem rows, "WDFW" for the tributary rows.
#
# SCOPE CAVEAT (surfaced in the Methods tab, not just here): the mainstem
# trip counts are combined salmon+steelhead effort, matching how ODFW/WDFW
# jointly run those fisheries. The tributary side is salmon-only by the PST
# pipeline's own design (steelhead-primary fisheries excluded there). A
# mainstem row is therefore NOT apples-to-apples with a tributary row for
# species scope, even though both are "angler trips" - flagged explicitly so
# a reader doesn't sum across scope boundaries without knowing that.
#
# Inputs:
#   input_files/pst/external_data/Copy of Buoy 10 Angler Trips by Mode and
#     Catch 20222025.xlsx (sheet "Buoy 10") - ODFW & WDFW, Aug-Dec each year,
#     Bank/Private Boat/Guided Boat/Charter Boat/Boat Total.
#   input_files/pst/external_data/LCR Angler Trips by Mode 20222025.xlsx
#     (sheet "Lower Columbia" - ODFW & WDFW, Jan-Dec, Bank/Private Boat/
#     Guided Boat/Boat Total; sheet "Bonneville-McNary" - ODFW & WDFW, month
#     range not stated in the source workbook, total trips only, no mode
#     split available)
#   analysis/pst/outputs/05_assembly/pst_fw_trips_by_mode_location.csv -
#     WDFW, written by pst_fw_angler_trips_assembly.R. MUST be current; this
#     script does not run that pipeline itself. Filtered here to
#     ColumbiaLower/ColumbiaMiddle/ColumbiaUpper/ColumbiaSnake.
#
# Output:
#   analysis/columbia_basin_trip_summary/outputs/
#     Columbia_Basin_Angler_Trip_Estimates.xlsx
#
# How to run:
#   Rscript analysis/columbia_basin_trip_summary/build_columbia_basin_trip_summary.R
# ==============================================================================

library(tidyverse)
library(here)
library(glue)
library(readxl)
library(openxlsx)

EXTERNAL_DIR <- here("input_files", "pst", "external_data")
PST_ASSEMBLY_DIR <- here("analysis", "pst", "outputs", "05_assembly")
OUT_DIR <- here("analysis", "columbia_basin_trip_summary", "outputs")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

ODFW_WDFW_SOURCE <- "ODFW & WDFW (joint creel estimate)"
WDFW_SOURCE      <- "WDFW"

# ---- 1. Mainstem: ODFW & WDFW joint estimates --------------------------------
# All three sheets share the same quirk: every real column is followed by a
# blank spacer column, which readxl turns into an auto-named "...N" column -
# selected past by position rather than by name, since the header row leaves
# them unlabeled. Rows with no Year are the sheets' own blank spacer rows,
# not real data - filtered out rather than sliced by row number so a future
# version of these workbooks with a different blank-row layout doesn't
# silently misalign the data.

read_buoy10 <- function(path) {
  raw <- suppressMessages(read_excel(path, sheet = "Buoy 10", skip = 8))
  raw |>
    filter(!is.na(Year)) |>
    transmute(
      year          = as.integer(Year),
      region        = "Buoy 10 (Columbia mainstem, river mouth)",
      season_covered = "Aug-Dec",
      angler_trips  = TOTAL,
      bank          = Bank,
      private_boat  = `Private Boat`,
      guided_boat   = `Guided Boat`,
      charter_boat  = `Charter Boat`,
      boat_total    = `Boat Total`
    )
}

read_lower_columbia_mainstem <- function(path) {
  raw <- suppressMessages(read_excel(path, sheet = "Lower Columbia", skip = 8))
  raw |>
    filter(!is.na(Year)) |>
    transmute(
      year          = as.integer(Year),
      region        = "Lower Columbia mainstem (below Bonneville Dam)",
      season_covered = "Jan-Dec",
      angler_trips  = TOTAL,
      bank          = Bank,
      private_boat  = `Private Boat`,
      guided_boat   = `Guided Boat`,
      charter_boat  = NA_real_,   # not a column in this sheet - no charter fleet reported here
      boat_total    = `Boat Total`
    )
}

read_bonneville_mcnary <- function(path) {
  raw <- suppressMessages(read_excel(path, sheet = "Bonneville-McNary", skip = 3))
  raw |>
    filter(!is.na(Year)) |>
    transmute(
      year          = as.integer(Year),
      region        = "Bonneville-McNary mainstem",
      # Source workbook has no State:/Method:/Month: header block like the
      # other two sheets do - month coverage is genuinely not stated, not
      # assumed to match the others. [R3]: a missing dimension stays
      # "unknown," never guessed.
      season_covered = "not stated in source",
      angler_trips  = `Salmonid Anglers`,
      bank          = NA_real_,  # no mode split at all in this sheet
      private_boat  = NA_real_,
      guided_boat   = NA_real_,
      charter_boat  = NA_real_,
      boat_total    = NA_real_
    )
}

mainstem_detail <- bind_rows(
  read_buoy10(file.path(EXTERNAL_DIR, "Copy of Buoy 10 Angler Trips by Mode and Catch 20222025.xlsx")),
  read_lower_columbia_mainstem(file.path(EXTERNAL_DIR, "LCR Angler Trips by Mode 20222025.xlsx")),
  read_bonneville_mcnary(file.path(EXTERNAL_DIR, "LCR Angler Trips by Mode 20222025.xlsx"))
) |>
  mutate(data_source = ODFW_WDFW_SOURCE) |>
  arrange(region, year)

# ---- 2. Tributary: WDFW PST pipeline output ----------------------------------
# Read as-is from the pipeline's own detail table - this script does not
# recompute anything, it only re-labels tier codes into plain language and
# rolls rivers up to the same region grain as the mainstem side above.

COLUMBIA_TRIB_BLOCKS <- c("ColumbiaLower", "ColumbiaMiddle", "ColumbiaUpper", "ColumbiaSnake")

TRIB_REGION_LABEL <- c(
  ColumbiaLower  = "Columbia Lower tributaries (WDFW)",
  ColumbiaMiddle = "Columbia Middle tributaries (WDFW)",
  ColumbiaUpper  = "Columbia Upper tributaries (WDFW)",
  ColumbiaSnake  = "Snake River tributaries (WDFW)"
)

# Plain-language mapping for the tier codes this repo uses everywhere else -
# see analysis/pst/03_analysis/pst_fw_angler_trips_assembly.R and
# pst_p2_block_ratio.R/pst_crc_harvest_projection.R for the actual mechanics
# this is translating.
TIER_PLAIN_LANGUAGE <- c(
  P1 = paste(
    "Creel-based (P1): a real, design-based creel survey directly measured",
    "trips for this river and year."
  ),
  P2 = paste(
    "CRC expansion (P2): no creel survey covered this river/year, so trips",
    "are estimated by applying a trips-per-salmon ratio - derived from",
    "rivers in the same region and year that DO have both a real creel",
    "survey and CRC harvest data - to this river's own real, already",
    "published CRC harvest count."
  ),
  P3 = paste(
    "Projected (P3): CRC has not yet published harvest for this year, so",
    "harvest itself is first projected from that river's recent history,",
    "then expanded into trips the same way P2 expands a real harvest figure."
  )
)

tributary_path <- file.path(PST_ASSEMBLY_DIR, "pst_fw_trips_by_mode_location.csv")
if (!file.exists(tributary_path)) {
  stop(glue(
    "{tributary_path} not found - run analysis/pst/run_pst_pipeline.R (through ",
    "05_effort_assembly) before this script. This script reads that output, ",
    "it does not regenerate it."
  ))
}

tributary_raw <- read_csv(tributary_path, show_col_types = FALSE) |>
  filter(block %in% COLUMBIA_TRIB_BLOCKS)

tributary_detail <- tributary_raw |>
  group_by(block, river_label, year, tier) |>
  summarise(angler_trips = sum(angler_trips, na.rm = TRUE), .groups = "drop") |>
  filter(angler_trips > 0) |>
  transmute(
    region       = TRIB_REGION_LABEL[block],
    river        = river_label,
    year,
    angler_trips = round(angler_trips),
    tier,
    method_plain_language = TIER_PLAIN_LANGUAGE[tier]
  ) |>
  arrange(region, river, year)

# One sentence per region-year summarizing the tier mix by trip-weighted
# share, e.g. "79% creel-based (P1), 21% CRC expansion (P2)" - this is what
# lets the combined summary tab show ONE method description per region-year
# even though a region is usually a blend of rivers on different tiers.
tier_share_sentence <- function(tier, trips) {
  totals <- tapply(trips, tier, sum)
  totals <- totals[totals > 0]
  if (length(totals) == 0) return(NA_character_)
  pct <- round(100 * totals / sum(totals))
  label <- c(P1 = "creel-based (P1)", P2 = "CRC expansion (P2)", P3 = "projected (P3)")[names(totals)]
  ord <- order(-pct)
  paste(glue("{pct[ord]}% {label[ord]}"), collapse = ", ")
}

tributary_summary <- tributary_raw |>
  group_by(block, year) |>
  summarise(
    method       = tier_share_sentence(tier, angler_trips),
    angler_trips = round(sum(angler_trips, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  transmute(
    year,
    region         = TRIB_REGION_LABEL[block],
    data_source    = WDFW_SOURCE,
    season_covered = "Jan-Dec (salmon-directed effort only - see Methods tab)",
    angler_trips,
    method
  )

# ---- 3. Combined summary -----------------------------------------------------

mainstem_summary <- mainstem_detail |>
  transmute(
    year, region, data_source,
    season_covered,
    angler_trips = round(angler_trips),
    method = "Design-based creel survey (joint ODFW/WDFW program) - see Methods tab"
  )

combined_summary <- bind_rows(mainstem_summary, tributary_summary) |>
  arrange(match(region, c(
    "Buoy 10 (Columbia mainstem, river mouth)",
    "Lower Columbia mainstem (below Bonneville Dam)",
    "Bonneville-McNary mainstem",
    TRIB_REGION_LABEL
  )), year)

# ---- 4. Methods & data sources tab -------------------------------------------

methods_notes <- tribble(
  ~Topic, ~Explanation,
  "Two data sources",
  paste(
    "\"ODFW & WDFW (joint creel estimate)\" = the Columbia mainstem (Buoy 10,",
    "below Bonneville, Bonneville-McNary) is fished and managed jointly by",
    "Oregon and Washington under the Columbia River Compact; these are",
    "design-based creel estimates both agencies produce together, taken",
    "directly from the workbooks ODFW supplied. \"WDFW\" = the Columbia",
    "tributaries (Lower/Middle/Upper blocks) and Snake River are WDFW's own",
    "estimates, produced entirely within this analysis."
  ),
  "P1 - Creel-based",
  TIER_PLAIN_LANGUAGE[["P1"]],
  "P2 - CRC expansion",
  TIER_PLAIN_LANGUAGE[["P2"]],
  "P3 - Projected",
  TIER_PLAIN_LANGUAGE[["P3"]],
  "Mainstem vs. tributary scope",
  paste(
    "Mainstem trip counts are COMBINED salmon + steelhead effort - that's how",
    "ODFW/WDFW jointly report those fisheries. Tributary trip counts are",
    "SALMON-DIRECTED EFFORT ONLY, by this analysis's own design",
    "(steelhead-primary fisheries are excluded). A mainstem row is not apples-to-apples",
    "with a tributary row on species scope, even though both are labeled",
    "\"angler trips\" - do not sum across this boundary without accounting",
    "for it."
  ),
  "Buoy 10 season coverage",
  "Aug-Dec only, per the source workbook - not a full calendar year.",
  "Bonneville-McNary season coverage",
  paste(
    "Not stated in the source workbook (no State/Method/Month header block",
    "like the other two ODFW sheets carry). Assume nothing about the season",
    "window for this region without checking with ODFW/WDFW directly."
  ),
  "Tributary region groupings",
  paste(
    "\"Columbia Lower/Middle/Upper\" and \"Snake River\" tributary blocks are",
    "this analysis's own grouping, based on CRC's own region field (not a",
    "hand-assigned label) - see analysis/pst/01_intro_methods/",
    "_03_pipeline_and_registry.qmd for the full derivation."
  ),
  "Undocumented secondary columns",
  paste(
    "The Lower Columbia mainstem sheet's mode columns (Bank, Private Boat,",
    "Guided Boat, Boat Total) each have an adjacent, unlabeled decimal-valued",
    "column in the source workbook that this compilation does NOT include -",
    "its meaning isn't documented in the sheet itself. Flagging its",
    "existence here rather than silently dropping it without a record."
  )
)

# ---- 5. Write workbook --------------------------------------------------------

hdr_style   <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2",
                           border = "Bottom", wrapText = TRUE)
title_style <- createStyle(textDecoration = "bold", fontSize = 13)
num_style   <- createStyle(numFmt = "#,##0")

data_col_width <- function(x, header) {
  x_chr <- if (is.numeric(x)) format(x, big.mark = ",", trim = TRUE) else as.character(x)
  max(c(4, nchar(x_chr, type = "chars"), nchar(header, type = "chars") + 1), na.rm = TRUE) + 2
}

add_sheet <- function(wb, sheet_name, df, title = NULL, freeze = TRUE, wrap_cols = NULL) {
  addWorksheet(wb, sheet_name)
  start_row <- if (!is.null(title)) 3 else 1
  writeData(wb, sheet_name, df, startRow = start_row, headerStyle = hdr_style)
  if (freeze) freezePane(wb, sheet_name, firstActiveRow = start_row + 1)
  widths <- mapply(data_col_width, df, names(df))
  if (!is.null(wrap_cols)) {
    wrap_idx <- which(names(df) %in% wrap_cols)
    widths[wrap_idx] <- 60
    addStyle(wb, sheet_name, createStyle(wrapText = TRUE),
            rows = (start_row + 1):(start_row + nrow(df)), cols = wrap_idx,
            gridExpand = TRUE, stack = TRUE)
  }
  setColWidths(wb, sheet_name, cols = seq_along(df), widths = widths)
  if (!is.null(title)) {
    writeData(wb, sheet_name, title, startRow = 1, startCol = 1)
    addStyle(wb, sheet_name, title_style, rows = 1, cols = 1)
  }
  num_cols <- which(vapply(df, is.numeric, logical(1)))
  if (length(num_cols) > 0 && nrow(df) > 0) {
    addStyle(wb, sheet_name, num_style,
            rows = (start_row + 1):(start_row + nrow(df)), cols = num_cols,
            gridExpand = TRUE, stack = TRUE)
  }
  invisible(NULL)
}

wb <- createWorkbook()

add_sheet(wb, "Combined Summary", combined_summary,
         title = "Columbia Basin Recreational Salmon Angler Trips - Combined Summary (2022-2025)",
         wrap_cols = "method")

add_sheet(wb, "Mainstem Mode Detail", mainstem_detail |>
           rename(`Angler Trips` = angler_trips, Region = region, Year = year,
                  `Season Covered` = season_covered, Bank = bank,
                  `Private Boat` = private_boat, `Guided Boat` = guided_boat,
                  `Charter Boat` = charter_boat, `Boat Total` = boat_total),
         title = "Mainstem detail by mode - as reported by ODFW/WDFW, trips only")

add_sheet(wb, "Tributary Detail", tributary_detail |>
           rename(Region = region, River = river, Year = year,
                  `Angler Trips` = angler_trips, Tier = tier,
                  `Method (plain language)` = method_plain_language),
         title = "Tributary detail by river and tier - WDFW PST pipeline output",
         wrap_cols = "Method (plain language)")

add_sheet(wb, "Methods & Data Sources", methods_notes,
         title = "Methods & Data Sources", freeze = FALSE, wrap_cols = "Explanation")

out_path <- file.path(OUT_DIR, "Columbia_Basin_Angler_Trip_Estimates.xlsx")
saveWorkbook(wb, out_path, overwrite = TRUE)

cli_ok <- tryCatch({ cli::cli_alert_success(glue("Wrote {out_path}")); TRUE },
                   error = function(e) FALSE)
if (!cli_ok) message(glue("Wrote {out_path}"))
