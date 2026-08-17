# ==============================================================================
# patch_crosswalk_areas.R
# Location: R_scripts/patch_crosswalk_areas.R
#
# One-off, idempotent patch to pst_river_block_crosswalk.csv:
#
#   1. Fill crc_areas for the three mid-Columbia R3_external fisheries.
#      Hanford Reach = 534|535|536, McNary = 533, Yakima = 690.
#      Source: CRC_AREA_LUT / HANFORD_CRC_AREAS in
#      mid_columbia_yakima_creel_ingestion.R, confirmed by Todd Miller
#      2026-08-11 (OQ1). Hanford 536 is independently corroborated by the CRC
#      harvest file, where stream_code 536 is "Columbia Old Hanford
#      townsite-Priest Rapids Dam".
#
#   2. Drop the ColumbiaMainstem rows. Buoy 10 / LCR / Bonneville-McNary were
#      transmitted to us BY the consultant via ODFW; they are not ours to
#      redeliver and nothing ingests them.
#
#   3. Mark Hanford's areas as covered-but-unpartitioned so P2 does not
#      re-expand them (see the note at the bottom -- this is the part that
#      would otherwise silently double-count).
#
# Writes a timestamped backup first. Run once, review the diff, commit.
# ==============================================================================

library(tidyverse)
library(here)
library(glue)

PST_DIR <- here("input_files", "pst")
XW_PATH <- file.path(PST_DIR, "pst_river_block_crosswalk.csv")

stopifnot(file.exists(XW_PATH))

xw <- read_csv(XW_PATH, show_col_types = FALSE)
cli::cli_alert_info("Read {nrow(xw)} crosswalk rows.")

backup <- file.path(PST_DIR,
                    glue("pst_river_block_crosswalk_{format(Sys.time(), '%Y%m%d_%H%M%S')}.bak.csv"))
write_csv(xw, backup)
cli::cli_alert_info("Backup written to {basename(backup)}")


# --- 1. Mid-Columbia CRC areas -------------------------------------------------
# Matched on the fishery_name stem so all four year-variants pick up the same
# areas without hard-coding every year string.

MID_COL_AREAS <- tribble(
  ~name_pattern,        ~crc_areas_new,   ~area_note,
  "^Hanford Reach",     "534|535|536",    "composite: fishery spans three CRC areas, no section-level split available",
  "^McNary Reservoir",  "533",            NA_character_,
  "^Yakima",            "690",            NA_character_
)

xw_patched <- xw |>
  mutate(crc_areas_before = crc_areas)

for (i in seq_len(nrow(MID_COL_AREAS))) {
  pat <- MID_COL_AREAS$name_pattern[i]
  val <- MID_COL_AREAS$crc_areas_new[i]
  hits <- str_detect(xw_patched$fishery_name, pat)
  if (!any(hits)) {
    cli::cli_alert_warning("No crosswalk rows match {.val {pat}} -- check fishery_name spellings.")
    next
  }
  xw_patched$crc_areas[hits] <- val
  cli::cli_alert_success("{sum(hits)} row{?s} matching {.val {pat}} -> crc_areas = {val}")
}

# Report anything we overwrote that wasn't blank -- a silent overwrite of a
# previously-correct value is exactly the kind of thing to catch here.
overwritten <- xw_patched |>
  filter(!is.na(crc_areas_before), crc_areas_before != "",
         crc_areas_before != crc_areas) |>
  select(fishery_name, crc_areas_before, crc_areas)

if (nrow(overwritten) > 0) {
  cli::cli_alert_warning("{nrow(overwritten)} row{?s} had a PRE-EXISTING crc_areas value that this patch changed:")
  print(overwritten)
}


# --- 2. Drop ColumbiaMainstem --------------------------------------------------

mainstem <- xw_patched |> filter(block == "ColumbiaMainstem")

if (nrow(mainstem) > 0) {
  cli::cli_alert_info("Removing {nrow(mainstem)} ColumbiaMainstem row{?s}:")
  print(mainstem |> select(fishery_name, river_label, source_id))
  # Keep a record outside the active crosswalk rather than only in git history.
  write_csv(mainstem |> select(-crc_areas_before),
            file.path(PST_DIR, "pst_river_block_crosswalk_mainstem_removed.csv"))
  cli::cli_alert_info("Removed rows archived to pst_river_block_crosswalk_mainstem_removed.csv")
} else {
  cli::cli_alert_info("No ColumbiaMainstem rows present -- already removed.")
}

xw_patched <- xw_patched |> filter(block != "ColumbiaMainstem")


# --- 3. Flag covered-but-unpartitioned areas -----------------------------------
# See the note below. Hanford's trips exist at fishery grain but carry
# catch_area_code = NA, so its three CRC areas look "uncovered" to any
# area-keyed coverage test. This column is what stops P2 expanding them.

if (!"area_coverage" %in% names(xw_patched)) {
  xw_patched <- xw_patched |> mutate(area_coverage = NA_character_)
}

xw_patched <- xw_patched |>
  mutate(
    area_coverage = case_when(
      str_detect(fishery_name, "^Hanford Reach") ~ "covered_unpartitioned",
      !is.na(area_coverage)                      ~ area_coverage,
      TRUE                                       ~ "standard"
    )
  )

cli::cli_alert_success(
  "{sum(xw_patched$area_coverage == 'covered_unpartitioned')} row{?s} marked covered_unpartitioned."
)


# --- 4. Validate and write ------------------------------------------------------

DELIVER_BLOCKS <- c("PugetSound", "WACoast", "ColumbiaTrib")
stopifnot(all(xw_patched$block %in% DELIVER_BLOCKS))

still_blank <- xw_patched |>
  filter(is.na(crc_areas) | crc_areas == "") |>
  count(block, source_id, name = "n_rows")

if (nrow(still_blank) > 0) {
  cli::cli_alert_warning("Rows still lacking crc_areas (P2 cannot reach these):")
  print(still_blank)
}

write_csv(xw_patched |> select(-crc_areas_before), XW_PATH)
cli::cli_alert_success("Wrote {nrow(xw_patched)} rows to {basename(XW_PATH)}")

cli::cli_h3("Areas now mapped, by block")
xw_patched |>
  separate_longer_delim(crc_areas, delim = "|") |>
  filter(!is.na(crc_areas), crc_areas != "") |>
  distinct(block, crc_areas) |>
  count(block, name = "n_areas") |>
  print()


# ==============================================================================
# FOLLOW-ON: two things this patch changes downstream
# ==============================================================================
#
# A. ALL_BLOCKS can collapse into DELIVER_BLOCKS.
#    In pst_fw_effort_assembly.R, ALL_BLOCKS existed only so the crosswalk
#    validation would tolerate the documented-but-excluded mainstem rows. With
#    those rows gone:
#
#      DELIVER_BLOCKS <- c("PugetSound", "WACoast", "ColumbiaTrib")
#      stopifnot(all(crosswalk$block %in% DELIVER_BLOCKS))
#
#    Keep the log_gap("ColumbiaMainstem", ...) note -- the scope decision should
#    stay visible in the gap register even though the rows are gone. Losing the
#    note would make it look like mainstem was never considered.
#
# B. HANFORD WOULD OTHERWISE BE DOUBLE-COUNTED BY P2.  *** read this one ***
#
#    Hanford's trips arrive via R3_external at fishery grain with
#    catch_area_code = NA -- the Summary sheet gives one fleet-wide weekly total
#    with no section breakdown, so the effort genuinely cannot be partitioned
#    across 534/535/536.
#
#    Giving those areas a crc_areas mapping (which is correct and necessary for
#    coverage reporting) has a side effect: P2's donor test asks "does this
#    area-year have creel trips?", finds none for 534/535/536 because the trips
#    carry a NULL area, and classifies all three as UNCOVERED. It would then
#    expand their CRC harvest into trips -- on top of the ~26,000 Hanford boat
#    trips already in the deliverable from R3_external.
#
#    The area_coverage column set above is the guard. In
#    R_scripts/pst_p2_block_ratio.R, inside apply_p2(), exclude these areas from
#    the target set before the donor anti-join:
#
#      unpartitioned <- xw_area_full |>
#        filter(area_coverage == "covered_unpartitioned") |>
#        distinct(catch_area_code)
#
#      targets <- targets |>
#        anti_join(unpartitioned, by = "catch_area_code")
#
#    and log them so the exclusion is visible rather than assumed:
#
#      log_gap("p2_expansion", "ColumbiaTrib", "note",
#              glue("areas {paste(unpartitioned$catch_area_code, collapse = '|')} ",
#                   "excluded from P2: effort is already in the deliverable via ",
#                   "R3_external but cannot be attributed to a single CRC area. ",
#                   "Expanding them would double-count Hanford Reach."))
#
#    expand_crosswalk_areas() must carry area_coverage through for this to work
#    -- add it to the transmute().
#
#    The same hazard applies to any future fishery whose trips exist without an
#    area code. The column, not the Hanford special case, is the fix.
# ==============================================================================
