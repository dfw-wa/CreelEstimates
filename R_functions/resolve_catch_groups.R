#' Resolve catch groups for estimation
#'
#' Dynamically source fishery catch groups from the Postgres creel database.
#' By default the script param `est_catch_groups` is blank quotes ("") and the
#' model catch group view (vw_model_catch_group) is queried via
#' `creelutils::fishery_catchgroups()`, returning the component fields alongside
#' `combined_catch_group` and `model_catch_group_id`. Users can optionally enter
#' catch groups manually as a data frame of the four component fields (atomic or
#' combined with `|`), which always overrides the database and is passed through
#' verbatim with no database contact — correct formatting of manual entries is
#' the user's responsibility. Aborts when catch groups are blank and the
#' database lookup fails or returns no defined groups.
#'
#' @details
#' A fully specified manual entry requires no database connectivity, mirroring
#' `resolve_dates()`. Catch group ids are joined at upload time in
#' `creelutils::prep_export()` (where a live connection is required regardless); 
#' manually entered groups not previously defined in the Creel App at
#' <https://apps.wdfw-fish.us/> are warned and skipped at upload, but estimate
#' normally.
#'
#' @param fishery character; must match a fishery_lut row when catch groups are blank
#' @param catch_groups "" (query the database) or a data frame with columns
#'   species, life_stage, fin_mark, fate
#' @param observed_only logical; passed to `creelutils::fishery_catchgroups()` on the 
#' blank path to restrict to catch groups with observed catch. Default FALSE.
#' @param conn A valid database connection from `creelutils::connect_creel_db()`.
#'   Optional with NULL default; `creelutils::fishery_catchgroups()` opens and closes 
#'   a lazy connection internally when NULL. Unused on the manual path.
#' @return data frame of catch groups; database-sourced rows carry
#'   `combined_catch_group` and `model_catch_group_id`, manual rows do not
resolve_catch_groups <- function(
    fishery,
    catch_groups,
    observed_only = FALSE,
    conn = NULL
) {
  components <- c("species", "life_stage", "fin_mark", "fate")
  
  # Validate 'observed_only' as single logical
  if (!is.logical(observed_only) || length(observed_only) != 1L || is.na(observed_only)) {
    cli::cli_abort("{.arg observed_only} must be a single logical TRUE or FALSE.")
  }
  
  # If catch groups are manually entered as a data frame -> pass through unchanged,
  # no database contact (keeps fully specified runs offline-reproducible)
  if (is.data.frame(catch_groups)) {
    missing <- setdiff(components, colnames(catch_groups))
    if (length(missing)) {
      cli::cli_abort(c(
        "{.arg catch_groups} is missing column{?s} {.field {missing}}.",
        "i" = "Script YAML `params$est_catch_groups` must be entered as:",
        " " = "!r data.frame(rbind(c(species = '', life_stage = '', fin_mark = '', fate = '')))"
      ))
    }
    
    # Add cols to match fishery_catchgroups() output
    if (!"fishery_name" %in% names(catch_groups)) {
      catch_groups$fishery_name <- fishery
    }
    if (!"combined_catch_group" %in% names(catch_groups)) { # combined created from components
      catch_groups$combined_catch_group <- creelutils::combine_catch_group(catch_groups)
    }
    if (!"model_catch_group_id" %in% names(catch_groups)) { # uuid col as NA, unknown with manual entry
      catch_groups$model_catch_group_id <- NA_character_
    }
    
    catch_groups <- dplyr::relocate(
      catch_groups,
      dplyr::any_of(c("fishery_name", "combined_catch_group", "model_catch_group_id")),
      .before = 1
    )
    
    return(as.data.frame(catch_groups))
  }
  
  # If catch groups are not entered (param as NULL, NA, length-0, or empty quotes "") -> normalize to ""
  coerce_blank <- \(x) if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(x)) "" else as.character(x)
  if (nzchar(coerce_blank(catch_groups))) {
    cli::cli_abort(
      "{.arg catch_groups} must be blank (\"\") to query the database, or a data frame with columns {.field {components}}."
    )
  }

  # Source fishery catch groups from the database
  cg_rows <- tryCatch(
    creelutils::fishery_catchgroups(conn = conn, fishery_name = fishery, observed_only = observed_only),
    error = function(e) e
  )
  
  if (inherits(cg_rows, "error")) {
    cli::cli_abort(c(
      "Cannot resolve catch groups for {.val {fishery}}.",
      "x" = "Lookup failed: {conditionMessage(cg_rows)}",
      "i" = "and no manual est_catch_groups were supplied."
    ))
  }
  
  if (nrow(cg_rows) == 0) {
    reason <- if (observed_only) {
      "no catch groups with observed catch found in vw_model_catch_group"
    } else {
      "no catch groups found in vw_model_catch_group"
    }
    cli::cli_abort(c(
      "Cannot resolve catch groups for {.val {fishery}}.",
      "x" = "{reason}",
      "i" = "and no manual est_catch_groups were supplied."
    ))
  }
  
  return(as.data.frame(cg_rows))
}
