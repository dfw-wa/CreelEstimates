#' Resolve the estimation window for a fishery
#'
#' Dynamically source fishery start and end dates from the Postgres creel database.
#' By default the script parameters est_date_start and est_date_end are blank
#' quotes (""). Users can optionally enter one or both dates manually. Manually
#' entered dates always override and validation of manual dates is up to the user. 
#' If at least one date parameter is left blank, the creel database fishery lookup
#' table (fishery_lut) is queried. See @details for more information about 
#' database connectivity. For in-season estimates, the end date is capped at
#' `today - 1` (i.e., yesterday, the last completed day). Aborts on a 
#' failed/ambiguous lookup or an inverted timeframe window.
#' 
#' @details
#' The `conn` argument may optionally be used to pass through an open connection
#' to the Postgres creel database. If left as NULL (default), a connection to the
#' database is opened internally by `creelutils::fishery_lut(conn)` and closed on exit.
#'
#' @param fishery character; must match a fishery_lut row when a date is blank
#' @param est_date_start,est_date_end "YYYY-MM-DD" or "" (blank -> from the lookup table)
#' @param conn A valid database connection from `creelutils::connect_creel_db()`. Optional argument with NULL default. If a connection is not passed through, `creelutils::fishery_lut()` internally opens a lazy connection and closes on exit.
#' @return list(est_date_start, est_date_end), character "YYYY-MM-DD"
#' @examples
#' \dontrun{
#' # standard use
#' est_dates <- resolve_dates(params$fishery_name, params$est_date_start, params$est_date_end)
#' 
#' # pass open connection; for example, to the 'test' server environment
#' con <- creelutils::connect_creel_db(db_env = "test")
#' est_dates <- resolve_dates(params$fishery_name, params$est_date_start, params$est_date_end, conn = con)
#' }
resolve_dates <- function(
    fishery,
    est_date_start,
    est_date_end,
    conn = NULL
) {
  # If fishery dates are manually entered -> pass through unchanged
  # If fishery dates are not entered (param as NULL, NA, length-0, or empty quotes "") -> normalize each to ""
  coerce_blank <- \(x) if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(x)) "" else as.character(x)
  est_start_input <- coerce_blank(est_date_start)
  est_end_input   <- coerce_blank(est_date_end)
  
  # Source fishery dates from the database if either param was left blank (default: "")
  needs_db <- !nzchar(est_start_input) || !nzchar(est_end_input)
  
  if (needs_db) {
    fishery_row <- tryCatch(
      creelutils::fishery_lut(conn = conn) |> # query and format
        dplyr::filter(fishery_name == fishery) |>
        dplyr::select(fishery_name, fishery_start_date, fishery_end_date),
      error = \(e) NULL
    )
    
    # NULL returned means a connection issue
    # row count != 1 means fishery name either absent from, or duplicated in, fishery_lut
    if (is.null(fishery_row) || nrow(fishery_row) != 1) {
      reason <- if (is.null(fishery_row)) "lookup failed (DB connection or query error)"
      else if (nrow(fishery_row) == 0) "fishery_name not found in fishery_lut"
      else "fishery_name matched multiple rows in fishery_lut"
      cli::cli_abort(c(
        "Cannot resolve estimation window for {.val {fishery}}.",
        "x" = "{reason},",
        "x" = "and no manual est_date_start / est_date_end were supplied."
      ))
    }
    
    # Resolve fishery dates
    # If the YAML param est_date_start is provided use that date (default is ""),
    # otherwise use the start date from the fishery lookup table.
    resolved_start <- if (nzchar(est_start_input)) as.Date(est_start_input) else as.Date(fishery_row$fishery_start_date)
    
    # If the YAML param est_date_end is provided use that date (default is "").
    # If a manual est_date_end is not provided, use whichever comes first:
    #   1) the end date from the fishery lookup table, or 2) the system date minus one (yesterday).
    #
    #   This accounts for in-season versus post-season runs of the script:
    #   - run within the fishery date range (in-season)  -> uses Sys.Date() - 1
    #   - run after the fishery has closed (post-season) -> uses the lookup table end date
    resolved_end <- if (nzchar(est_end_input)) as.Date(est_end_input) else min(as.Date(fishery_row$fishery_end_date), Sys.Date() - 1)  
  
    # Abort when the resolved window is inverted
    if (resolved_end < resolved_start) {
      cli::cli_abort("Invalid date range for {.val {fishery}}: end ({resolved_end}) is before start ({resolved_start}).")
    }

    # Derive model run type from the resolved window relative to the true fishery end date:
    # - estimate window ends before the fishery end date -> "In-season"
    # - estimate window reaches the fishery end date -> "Post-season"
    model_run_type <- if (resolved_end < as.Date(fishery_row$fishery_end_date)) {
      "In-season"
    } else {
      "Post-season"
      }
      
  } else {
    # Both supplied = verbatim, convert to date to validate format and mirror needs_db date resolution
    resolved_start <- as.Date(est_start_input)
    resolved_end   <- as.Date(est_end_input)
    
    # Abort when the resolved window is inverted
    if (resolved_end < resolved_start) {
      cli::cli_abort("Invalid date range for {.val {fishery}}: end ({resolved_end}) is before start ({resolved_start}).")
    }
    
    # If both dates supplied manually, no database lookup
    # model run type set as "Unknown" without database fishery_lut validation
    model_run_type <- "Unknown"
  }

  # Return list of resolved dates and run type
  list(
    est_date_start = as.character(resolved_start),
    est_date_end = as.character(resolved_end),
    model_run_type = model_run_type
  )
}
