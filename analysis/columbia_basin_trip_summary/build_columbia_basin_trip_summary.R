# ==============================================================================
# build_columbia_basin_trip_summary.R
# Location: analysis/columbia_basin_trip_summary/build_columbia_basin_trip_summary.R
#
# Purpose: compile and standardize Columbia basin recreational salmon angler
# trip estimates for Raquel Crosier (WDFW Fish Program Deputy Director of
# Regions), organized by REGION (Lower / Middle / Upper Columbia, Snake) -
# each region showing its MAINSTEM component and its TRIBUTARY component
# side by side, since those are two independent things a first draft of
# this script conflated:
#
#   WATER TYPE (mainstem vs. tributary) is NOT the same axis as DATA SOURCE
#   (who produced the estimate). Getting this right matters here specifically
#   because Upper Columbia's mainstem water (Hanford Reach, McNary Reservoir,
#   and part of Chad Jackson's combined Upper Columbia total) is WDFW's OWN
#   data, not ODFW's - mainstem is emphatically NOT exclusively ODFW-supplied.
#   Only the Lower and Middle Columbia mainstem reaches (Buoy 10 through
#   Bonneville-McNary) come from the joint ODFW/WDFW workbooks; everything
#   upstream of McNary Dam that this compilation carries is WDFW's alone.
#
#   Per region:
#     Lower Columbia  - mainstem = Buoy 10 + below Bonneville Dam (ODFW & WDFW
#                        joint). tributary = Cowlitz/Lewis/Kalama/Elochoman/
#                        Washougal/etc. (WDFW, this repo's own PST pipeline).
#     Middle Columbia - mainstem = Bonneville-McNary (ODFW & WDFW joint).
#                        tributary = Drano Lake/Klickitat/Wind/Big White
#                        Salmon (WDFW).
#     Upper Columbia  - mainstem = Hanford Reach + McNary Reservoir (WDFW,
#                        R3_external/Todd Miller) - water ABOVE McNary Dam,
#                        not covered by either ODFW sheet, which stop AT
#                        McNary Dam. tributary = Yakima River (WDFW,
#                        R3_external). A third component, Chad Jackson's
#                        (R2) combined Upper Columbia total, bundles MORE
#                        mainstem (Priest Rapids-Chief Joseph reaches) together
#                        with real tributaries (Entiat/Okanogan/Similkameen/
#                        Wenatchee/Icicle Creek) in ONE bundled number that
#                        cannot be split into mainstem vs. tributary - kept as
#                        its own "Mixed" row rather than forced into either
#                        bucket.
#     Snake           - the only water in this compilation's Snake block is
#                        the Snake River itself (WDFW, R1_external/Jeremy
#                        Trump's combined total) - that's mainstem Snake
#                        River, not a small tributary stream, so it is
#                        labeled Mainstem here, not Tributary.
#
# This is the water the PST tributary pipeline's own crosswalk otherwise
# excludes as "ColumbiaMainstem, OUT_OF_SCOPE" for the Lower/Middle reaches,
# PLUS the mainstem water this repo's PST pipeline actually does carry
# further upstream (Hanford Reach/McNary Reservoir/part of the R2 total/
# Snake River) but had never previously been called out AS mainstem.
#
# Every row also carries a Data Source column so that's never confused with
# water type: "ODFW & WDFW (joint creel estimate)" only for the two ODFW-
# supplied workbooks (Lower and Middle Columbia mainstem); "WDFW" for
# everything else in this compilation, mainstem or tributary alike.
#
# SCOPE CAVEAT (surfaced in the Methods tab, not just here): the ODFW/WDFW
# joint mainstem trip counts are combined salmon+steelhead effort, matching
# how those fisheries are jointly run. The WDFW tributary side is salmon-only
# by the PST pipeline's own design (steelhead-primary fisheries excluded
# there). A mainstem row is therefore NOT apples-to-apples with a tributary
# row for species scope, even though both are "angler trips."
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
#     ColumbiaLower/ColumbiaMiddle/ColumbiaUpper/ColumbiaSnake, then
#     reclassified by river into the mainstem/tributary/mixed split above -
#     see RIVER_WATER_TYPE.
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
CRC_LUT_PATH <- here("input_files", "pst", "lookup_tables", "crc_area_lut.csv")
OUT_DIR <- here("analysis", "columbia_basin_trip_summary", "outputs")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

ODFW_WDFW_SOURCE <- "ODFW & WDFW (joint creel estimate)"
WDFW_SOURCE      <- "WDFW"

REGION_ORDER <- c("Lower Columbia", "Middle Columbia", "Upper Columbia", "Snake River")
WATER_TYPE_ORDER <- c("Mainstem", "Tributary", "Mixed (mainstem + tributary, not separable)")

# ---- 1. Mainstem: ODFW & WDFW joint estimates (Lower + Middle Columbia only) -
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
      region        = "Lower Columbia",
      water_type    = "Mainstem",
      area          = "Buoy 10 (river mouth)",
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
      region        = "Lower Columbia",
      water_type    = "Mainstem",
      area          = "Below Bonneville Dam",
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
      region        = "Middle Columbia",
      water_type    = "Mainstem",
      area          = "Bonneville Dam to McNary Dam",
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

odfw_wdfw_detail <- bind_rows(
  read_buoy10(file.path(EXTERNAL_DIR, "Copy of Buoy 10 Angler Trips by Mode and Catch 20222025.xlsx")),
  read_lower_columbia_mainstem(file.path(EXTERNAL_DIR, "LCR Angler Trips by Mode 20222025.xlsx")),
  read_bonneville_mcnary(file.path(EXTERNAL_DIR, "LCR Angler Trips by Mode 20222025.xlsx"))
) |>
  mutate(data_source = ODFW_WDFW_SOURCE) |>
  arrange(match(region, REGION_ORDER), area, year)

# ---- 1b. Which CRC mainstem area codes each ODFW/WDFW area corresponds to ---
# These three areas are defined by river landmarks (dams, bridges, buoy
# lines), not creel/interview strata - matched here directly against
# crc_area_lut.csv by the landmark descriptions CRC itself uses for its
# mainstem catch areas. Confirmed against each sheet's own boundary text:
# Buoy 10's title says "Buoy 10 line to Tongue Point/Rocky Point line" (= CRC
# 519 exactly); the Lower Columbia sheet's own cell comment says "Below
# Bonneville Dam - Does not include Buoy 10" (= CRC 521/523/525, the three
# reaches from that same Rocky Pt/Tongue Pt line up to Bonneville Dam);
# Bonneville-McNary's title is literal (= CRC 527/529/531, Bonneville Dam to
# McNary Dam in three reaches). Also documented as ColumbiaMainstem/
# OUT_OF_SCOPE rows in pst_river_block_crosswalk.csv (2026-09-08) - kept
# hardcoded here too rather than read from there, since this script's own
# job is to be independently checkable against crc_area_lut.csv without
# depending on the PST crosswalk's own filtering conventions.
MAINSTEM_AREA_CRC <- c(
  "Buoy 10 (river mouth)"       = "519",
  "Below Bonneville Dam"        = "521|523|525",
  "Bonneville Dam to McNary Dam" = "527|529|531"
)

crc_lut <- read_csv(CRC_LUT_PATH, show_col_types = FALSE) |>
  transmute(catch_area_code = as.character(catch_area_code),
           catch_area_description, catch_area_region)

mainstem_crc_lookup <- tibble(area = names(MAINSTEM_AREA_CRC), crc_areas = MAINSTEM_AREA_CRC) |>
  separate_longer_delim(crc_areas, delim = "|") |>
  rename(catch_area_code = crc_areas) |>
  left_join(crc_lut, by = "catch_area_code") |>
  transmute(
    Area = area,
    `CRC Area Code` = catch_area_code,
    `CRC Area Description` = catch_area_description,
    `CRC Region` = catch_area_region
  ) |>
  arrange(match(Area, names(MAINSTEM_AREA_CRC)), `CRC Area Code`)

odfw_wdfw_detail <- odfw_wdfw_detail |>
  mutate(crc_areas = MAINSTEM_AREA_CRC[area])

# ---- 2. WDFW: this repo's own PST pipeline output ----------------------------
# Read as-is from the pipeline's own detail table - this script does not
# recompute anything, it only re-labels tier codes into plain language and
# reclassifies each river by region + water type (see the header comment).

WDFW_BLOCKS <- c("ColumbiaLower", "ColumbiaMiddle", "ColumbiaUpper", "ColumbiaSnake")

BLOCK_REGION <- c(
  ColumbiaLower  = "Lower Columbia",
  ColumbiaMiddle = "Middle Columbia",
  ColumbiaUpper  = "Upper Columbia",
  ColumbiaSnake  = "Snake River"
)

# Everything not listed here defaults to "Tributary" (river_label is unique
# enough within a block for this; verified against pst_river_block_
# crosswalk.csv's ColumbiaUpper/ColumbiaSnake rows, 2026-09-08). "Upper
# Columbia" as a river_label is Chad Jackson's (R2) own combined district
# total - genuinely bundles mainstem and tributary CRC areas into one number
# neither this script nor the pipeline itself can split further; see MD6 in
# analysis/pst/03_analysis/_22_status_and_gaps.qmd for the full area list.
RIVER_WATER_TYPE <- c(
  "Hanford Reach"                 = "Mainstem",
  "McNary Reservoir"              = "Mainstem",
  "Columbia River (above McNary)" = "Mainstem",  # Priest Rapids-Chief Joseph
                                     # reaches (CRC 537-545/547|549) - one row
                                     # is superseded by the R2 bundle below,
                                     # the other (547|549, Roosevelt Lake) is
                                     # NOT covered by R2 and can still surface
                                     # here on its own; both are mainstem
                                     # either way.
  "Upper Columbia"   = "Mixed (mainstem + tributary, not separable)",
  "Snake River"      = "Mainstem"
)

# Plain-language mapping for the tier codes this repo uses everywhere else -
# see analysis/pst/03_analysis/pst_fw_angler_trips_assembly.R and
# pst_p2_block_ratio.R/pst_crc_harvest_projection.R for the actual mechanics
# this is translating.
TIER_PLAIN_LANGUAGE <- c(
  P1 = paste(
    "Creel-based (P1): a design-based creel survey directly measured trips",
    "for this river and year."
  ),
  P2 = paste(
    "CRC expansion (P2): no creel survey covered this river/year, so trips",
    "are estimated by applying a trips-per-salmon ratio - derived from",
    "rivers in the same region and year that DO have both a creel survey",
    "and CRC harvest data - to this river's own already published CRC",
    "harvest count."
  ),
  P3 = paste(
    "Projected (P3): CRC has not yet published harvest for this year, so",
    "harvest itself is first projected from that river's recent history,",
    "then expanded into trips the same way P2 expands a published harvest",
    "figure."
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

wdfw_raw <- read_csv(tributary_path, show_col_types = FALSE) |>
  filter(block %in% WDFW_BLOCKS) |>
  mutate(
    region     = BLOCK_REGION[block],
    water_type = coalesce(RIVER_WATER_TYPE[river_label], "Tributary")
  )

# Sorted, deduplicated union of every catch_area_code behind a group of rows
# - pst_fw_trips_by_mode_location.csv already carries this pipe-delimited
# per river/year (a composite river like the R2-sourced "Upper Columbia" row
# is itself multiple codes), so rolling up to river or region just means
# re-exploding and re-collapsing rather than a fresh lookup.
crc_areas_union <- function(x) {
  codes <- unlist(strsplit(x[!is.na(x) & x != ""], "\\|"))
  if (length(codes) == 0) return(NA_character_)
  paste(sort(unique(codes)), collapse = "|")
}

wdfw_detail <- wdfw_raw |>
  group_by(region, water_type, river_label, year, tier) |>
  summarise(angler_trips = sum(angler_trips, na.rm = TRUE),
           crc_areas    = crc_areas_union(catch_area_codes), .groups = "drop") |>
  filter(angler_trips > 0) |>
  transmute(
    region, water_type,
    river        = river_label,
    year,
    angler_trips = round(angler_trips),
    tier,
    crc_areas,
    method_plain_language = TIER_PLAIN_LANGUAGE[tier]
  ) |>
  arrange(match(region, REGION_ORDER), match(water_type, WATER_TYPE_ORDER), river, year)

# One sentence per region/water-type/year summarizing the tier mix by trip-
# weighted share, e.g. "79% creel-based (P1), 21% CRC expansion (P2)" - this
# is what lets the combined summary tab show ONE method description per row
# even though a region/water-type is usually a blend of rivers on different
# tiers.
tier_share_sentence <- function(tier, trips) {
  totals <- tapply(trips, tier, sum)
  totals <- totals[totals > 0]
  if (length(totals) == 0) return(NA_character_)
  pct <- round(100 * totals / sum(totals))
  label <- c(P1 = "creel-based (P1)", P2 = "CRC expansion (P2)", P3 = "projected (P3)")[names(totals)]
  ord <- order(-pct)
  paste(glue("{pct[ord]}% {label[ord]}"), collapse = ", ")
}

wdfw_summary <- wdfw_raw |>
  group_by(region, water_type, year) |>
  summarise(
    method       = tier_share_sentence(tier, angler_trips),
    angler_trips = round(sum(angler_trips, na.rm = TRUE)),
    crc_areas    = crc_areas_union(catch_area_codes),
    .groups = "drop"
  ) |>
  transmute(
    year, region, water_type,
    data_source    = WDFW_SOURCE,
    season_covered = "Jan-Dec (salmon-directed effort only - see Methods tab)",
    angler_trips,
    method,
    crc_areas
  )

# ---- 3. Combined summary -----------------------------------------------------

odfw_wdfw_summary <- odfw_wdfw_detail |>
  transmute(
    year, region, water_type, data_source,
    season_covered,
    angler_trips = round(angler_trips),
    method = "Design-based creel survey (joint ODFW/WDFW program) - see Methods tab",
    crc_areas
  )

combined_summary <- bind_rows(odfw_wdfw_summary, wdfw_summary) |>
  arrange(match(region, REGION_ORDER), match(water_type, WATER_TYPE_ORDER), data_source, year)

# ---- 4. Methods & data sources tab -------------------------------------------

methods_notes <- tribble(
  ~Topic, ~Explanation,
  "Region layout",
  paste(
    "Every region (Lower/Middle/Upper Columbia, Snake) is shown with its",
    "mainstem component and its tributary component as SEPARATE rows -",
    "these are independent things: which water it is (mainstem vs.",
    "tributary) is not the same question as who produced the estimate",
    "(see \"Two data sources\" below). Do not sum a region's mainstem and",
    "tributary rows into one \"region total\" without noting the two also",
    "differ in species scope - see \"Mainstem vs. tributary scope\" below."
  ),
  "Two data sources",
  paste(
    "\"ODFW & WDFW (joint creel estimate)\" = ONLY the Lower and Middle",
    "Columbia mainstem (Buoy 10, below Bonneville Dam, Bonneville-McNary) -",
    "water fished and managed jointly by Oregon and Washington under the",
    "Columbia River Compact, taken directly from the workbooks ODFW",
    "supplied. \"WDFW\" = everything else in this compilation, mainstem or",
    "tributary alike: the Columbia tributaries (Lower/Middle/Upper blocks),",
    "Snake River, AND the Upper Columbia mainstem (Hanford Reach, McNary",
    "Reservoir, and the mainstem reaches bundled into Chad Jackson's",
    "combined Upper Columbia total) - all WDFW's own estimates. Mainstem is",
    "NOT exclusively ODFW-supplied; it is only the two reaches below McNary",
    "Dam that are joint."
  ),
  "Upper Columbia mainstem, specifically",
  paste(
    "Hanford Reach and McNary Reservoir sit ABOVE McNary Dam - outside the",
    "ODFW/WDFW joint sheets, which stop AT McNary Dam - and are reported by",
    "WDFW district staff (Todd Miller, R3_external) through this repo's own",
    "PST pipeline. Chad Jackson's (R2) combined Upper Columbia total adds a",
    "further mainstem stretch (Priest Rapids to Chief Joseph Dam) bundled",
    "into ONE number together with several tributaries (Entiat,",
    "Okanogan, Similkameen, Wenatchee River, Icicle Creek) - that bundle",
    "cannot be split into mainstem vs. tributary, so it is shown here as its",
    "own \"Mixed\" row rather than forced into either bucket."
  ),
  "Snake River",
  paste(
    "The only water this compilation carries for the Snake block is the",
    "Snake River itself (WDFW, Jeremy Trump's/R1_external's combined total",
    "near the WA/ID border) - that is mainstem Snake River, not a small",
    "tributary stream, and is labeled Mainstem here accordingly."
  ),
  "P1 - Creel-based",
  TIER_PLAIN_LANGUAGE[["P1"]],
  "P2 - CRC expansion",
  TIER_PLAIN_LANGUAGE[["P2"]],
  "P3 - Projected",
  TIER_PLAIN_LANGUAGE[["P3"]],
  "Mainstem vs. tributary scope",
  paste(
    "ODFW/WDFW joint mainstem trip counts are COMBINED salmon + steelhead",
    "effort - that's how those fisheries are jointly reported. WDFW",
    "tributary trip counts are SALMON-DIRECTED EFFORT ONLY, by this",
    "analysis's own design (steelhead-primary fisheries are excluded). A",
    "mainstem row is not apples-to-apples with a tributary row on species",
    "scope, even though both are labeled \"angler trips\" - do not sum",
    "across this boundary without accounting for it."
  ),
  "Buoy 10 season coverage",
  "Aug-Dec only, per the source workbook - not a full calendar year.",
  "Bonneville-McNary season coverage",
  paste(
    "Not stated in the source workbook (no State/Method/Month header block",
    "like the other two ODFW sheets carry). Assume nothing about the season",
    "window for this region without checking with ODFW/WDFW directly."
  ),
  "Undocumented secondary columns",
  paste(
    "The Lower Columbia mainstem sheet's mode columns (Bank, Private Boat,",
    "Guided Boat, Boat Total) each have an adjacent, unlabeled decimal-valued",
    "column in the source workbook that this compilation does NOT include -",
    "its meaning isn't documented in the sheet itself. Flagging its",
    "existence here rather than silently dropping it without a record."
  ),
  "Mainstem CRC area codes",
  paste(
    "The three ODFW/WDFW mainstem areas (Buoy 10, Below Bonneville Dam,",
    "Bonneville Dam to McNary Dam) are defined by river landmarks, not by",
    "creel/interview strata - matched here directly against crc_area_lut.csv",
    "by the landmark descriptions CRC uses for its own mainstem catch areas.",
    "See the \"Mainstem CRC Area Lookup\" tab for the exact codes. CRC's own",
    "\"catch_area_region\" field (in crc_area_lut.csv) is \"Columbia River\"",
    "for every one of them - it does not distinguish Lower/Middle/Upper, so",
    "it is carried through as-is rather than treated as a finer region",
    "label. These same codes are ALSO now documented as ColumbiaMainstem/",
    "OUT_OF_SCOPE rows in pst_river_block_crosswalk.csv (2026-09-08), cross-",
    "checked there against the ACTUAL region field the Salmon Freshwater",
    "Estimates CRC files (input_files/pst/CRC/) carry: CRC 519/521 verified",
    "'Columbia - Lower', 527/529/531 verified 'Columbia - Middle'. CRC 523",
    "and 525 do not appear in that CRC series at all, so their region could",
    "not be verified the same way - see that crosswalk row's own note."
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
  num_cols <- which(vapply(df, is.numeric, logical(1)) & !names(df) %in% c("Year", "year"))
  if (length(num_cols) > 0 && nrow(df) > 0) {
    addStyle(wb, sheet_name, num_style,
            rows = (start_row + 1):(start_row + nrow(df)), cols = num_cols,
            gridExpand = TRUE, stack = TRUE)
  }
  invisible(NULL)
}

wb <- createWorkbook()

add_sheet(wb, "Combined Summary", combined_summary |>
           rename(Year = year, Region = region, `Water Type` = water_type,
                  `Data Source` = data_source, `Season Covered` = season_covered,
                  `Angler Trips` = angler_trips, Method = method,
                  `CRC Areas` = crc_areas),
         title = "Columbia Basin Recreational Salmon Angler Trips - Combined Summary (2022-2025)",
         wrap_cols = "Method")

add_sheet(wb, "Mainstem Mode Detail", odfw_wdfw_detail |>
           rename(Year = year, Region = region, `Water Type` = water_type,
                  Area = area, `Season Covered` = season_covered,
                  `Angler Trips` = angler_trips, Bank = bank,
                  `Private Boat` = private_boat, `Guided Boat` = guided_boat,
                  `Charter Boat` = charter_boat, `Boat Total` = boat_total,
                  `Data Source` = data_source, `CRC Areas` = crc_areas),
         title = "ODFW/WDFW joint mainstem detail by mode - Lower & Middle Columbia only")

add_sheet(wb, "Mainstem CRC Area Lookup", mainstem_crc_lookup,
         title = "CRC catch area codes represented by each Buoy 10/LCR mainstem area (crc_area_lut.csv)")

add_sheet(wb, "WDFW Detail", wdfw_detail |>
           rename(Region = region, `Water Type` = water_type, River = river,
                  Year = year, `Angler Trips` = angler_trips, Tier = tier,
                  `CRC Areas` = crc_areas,
                  `Method (plain language)` = method_plain_language),
         title = "WDFW detail by river, region, water type, and tier - PST pipeline output",
         wrap_cols = "Method (plain language)")

add_sheet(wb, "Methods & Data Sources", methods_notes,
         title = "Methods & Data Sources", freeze = FALSE, wrap_cols = "Explanation")

out_path <- file.path(OUT_DIR, "Columbia_Basin_Angler_Trip_Estimates.xlsx")
saveWorkbook(wb, out_path, overwrite = TRUE)

cli_ok <- tryCatch({ cli::cli_alert_success(glue("Wrote {out_path}")); TRUE },
                   error = function(e) FALSE)
if (!cli_ok) message(glue("Wrote {out_path}"))
