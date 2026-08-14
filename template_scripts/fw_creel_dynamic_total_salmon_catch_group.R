# ==============================================================================
# Dynamic "all kept salmon" catch group for fw_creel.Rmd
#
# Purpose:
#   Ported from build_est_catch_groups() in multi_fishery_harvest_summary.R,
#   trimmed to produce ONLY the pooled total-salmon group (no per-species
#   rows), for use in manual fw_creel.Rmd runs on Puyallup/Carbon, Nisqually,
#   and Drano Lake.
#
#   Rather than hard-coding life_stage/fin_mark values into est_catch_groups,
#   this builds the regex alternation from the values actually observed in
#   dwg$catch for the fishery being run, so nothing present in the data is
#   silently excluded from "all kept salmon."
#
# Placement:
#   fw_creel.Rmd currently defines est_catch_groups as a hard-coded params
#   value BEFORE dwg is fetched. This dynamic version needs dwg$catch, so it
#   must run AFTER fetch_data() and BEFORE prep_dwg_interview_catch(). Two
#   chunks below:
#     Chunk 1 -> add near the top of the Rmd (setup/constants area), once.
#     Chunk 2 -> add immediately after the fetch_data() call that produces
#                `dwg`, replacing the hard-coded est_catch_groups assignment
#                that currently feeds `params$est_catch_groups` /
#                prep_dwg_interview_catch(). Confirm the exact line against
#                the current Rmd before deleting the old assignment — grep
#                for "est_catch_groups" to find every use site, since it may
#                also be referenced in a params list passed to est_pe_catch().
# ==============================================================================


# ---- Chunk 1: constants + helper function (setup area, once) ---------------

# Species eligible for the PST salmon rollup. Steelhead, trout, char, and
# other gamefish are excluded by design. Matching is EXACT against the values
# in dwg$catch$species (not regex) so a DB-side rename surfaces as a warning
# rather than silently dropping fish.
SALMON_SPECIES <- c("Chinook", "Coho", "Chum", "Pink", "Sockeye")

# Fate regex defining "harvest" (kept fish only; excludes Released).
HARVEST_FATE <- "Kept"

# Regex metacharacters that would break str_detect() if present in an
# observed category value (guards against a value like "Bull Trout|Dolly Varden"
# silently changing the meaning of the assembled pattern).
REGEX_METACHARS <- "[.\\\\+*?\\[\\]^$(){}=!<>|:-]"

#' Build a single "all kept salmon" est_catch_groups row from observed data
#'
#' @param dwg_catch  dwg$catch as returned by fetch_data()
#' @param fishery_name  character, for logging only
#' @param species_keep  species eligible for the pooled group (default
#'   SALMON_SPECIES)
#' @param fate_regex  fate to treat as harvest (default HARVEST_FATE)
#' @return a one-row data.frame in the shape prep_dwg_interview_catch() /
#'   est_pe_catch() expect for est_catch_groups (species, life_stage,
#'   fin_mark, fate), or NULL (with a warning) if no PST salmon species /
#'   no harvested records are present.
build_total_salmon_catch_group <- function(dwg_catch,
                                            fishery_name,
                                            species_keep = SALMON_SPECIES,
                                            fate_regex   = HARVEST_FATE) {

  # Mirror the NA -> "NA" coercion prep_dwg_interview_catch() applies
  # internally, so the values enumerated here match what it will str_detect()
  # against.
  cat_std <- dwg_catch |>
    dplyr::mutate(
      dplyr::across(
        c(species, life_stage, fin_mark, fate),
        ~ tidyr::replace_na(as.character(.), "NA")
      )
    )

  observed_species <- sort(unique(cat_std$species))
  spp_present       <- sort(intersect(observed_species, species_keep))
  spp_unmatched      <- setdiff(observed_species, species_keep)

  if (length(spp_unmatched) > 0) {
    cli::cli_alert_info(
      "Non-PST species present and excluded from total-salmon group: \\
       {.val {spp_unmatched}}"
    )
  }
  if (length(spp_present) == 0) {
    cli::cli_alert_warning(
      "No PST salmon species found in catch for {.val {fishery_name}}."
    )
    return(NULL)
  }

  harvest_rows <- cat_std |>
    dplyr::filter(
      species %in% spp_present,
      stringr::str_detect(fate, fate_regex)
    )

  if (nrow(harvest_rows) == 0) {
    cli::cli_alert_warning(
      "No records with fate matching {.val {fate_regex}} for \\
       {.val {fishery_name}}."
    )
    return(NULL)
  }

  ls_vals <- sort(unique(harvest_rows$life_stage))
  fm_vals <- sort(unique(harvest_rows$fin_mark))

  bad_vals <- c(spp_present, ls_vals, fm_vals) |>
    purrr::keep(~ stringr::str_detect(.x, REGEX_METACHARS))
  if (length(bad_vals) > 0) {
    cli::cli_abort(
      c("Category value(s) contain regex metacharacters and cannot be safely \\
         assembled into the catch-group pattern.",
        "x" = "{.val {bad_vals}}",
        "i" = "Handle these explicitly before re-running.")
    )
  }

  cli::cli_alert_success(
    "Total-salmon catch group for {.val {fishery_name}}: species = \\
     {paste(spp_present, collapse = '|')}"
  )

  data.frame(
    species    = paste(spp_present, collapse = "|"),
    life_stage = paste(ls_vals, collapse = "|"),
    fin_mark   = paste(fm_vals, collapse = "|"),
    fate       = fate_regex,
    stringsAsFactors = FALSE
  )
}


# ---- Chunk 2: build the group for this run (after fetch_data(), before -----
# ---- prep_dwg_interview_catch() / est_pe_catch()) ---------------------------

# Replaces the hard-coded est_catch_groups assignment for this fishery.
# `dwg` must already exist (i.e. this runs after the fetch_data() call).
est_catch_groups <- build_total_salmon_catch_group(
  dwg_catch    = dwg$catch,
  fishery_name = params$fishery_name   # or the literal fishery name string
                                        # if fw_creel.Rmd doesn't use params
)

if (is.null(est_catch_groups)) {
  stop(
    "No total-salmon catch group could be built for this fishery -- see \\
     warnings above (no PST species, or no Kept records, in dwg$catch)."
  )
}

# If est_catch_groups feeds a `params` list consumed later (e.g.
# params$est_catch_groups used inside prep_dwg_interview_catch() /
# est_pe_catch() calls), update that list too:
# params$est_catch_groups <- est_catch_groups
