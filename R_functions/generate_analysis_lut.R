# Function to generate or load analysis lookup table with session-specific uuid
#
# `resolved_params` is built in fw_creel.Rmd after resolve_dates() and
# resolve_catch_groups() run: the YAML params list overlaid with resolver
# outputs (est_date_start, est_date_end, model_run_type, est_catch_groups).
# The same object is later passed through the export pipeline
# (finalize_analysis_lut() / export_estimates()) in place of raw params.
generate_analysis_lut <- function(resolved_params, current_file_path = NULL) {
  
  # Try to get current file path if not provided
  if (is.null(current_file_path)) {
    current_file_path <- tryCatch({
      if (!is.null(knitr::current_input()) && knitr::current_input() != "") {
        # knitr::current_input(dir = TRUE) returns the full path to the file
        # actually being knit — i.e. the analysis-folder copy of the script —
        # rather than a path reconstructed under template_scripts/, which is
        # always wrong once a session is running from its own analysis folder.
        knitr::current_input(dir = TRUE)
      } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        rstudioapi::getSourceEditorContext()$path
      } else {
        # Headless (Rscript / cron / CI) invocation: no reliable script-path
        # API exists here. Left as NULL intentionally — making this branch
        # work is separate follow-up work, not part of this fix.
        NULL
      }
    }, error = function(e) NULL)
  }
  
  # Check if we're running from an existing analysis folder
  existing_analysis_lut <- NULL
  if (!is.null(current_file_path) && current_file_path != "" && file.exists(current_file_path)) {
    current_dir <- dirname(current_file_path)
    
    # Check if current directory matches analysis folder pattern
    # Pattern: IDENTIFIER_XXXX_YYYYMMDD (e.g., SFS25_ABCD_20241222)
    folder_name <- basename(current_dir)
    analysis_pattern <- "^[A-Z0-9]+_[0-9a-f]{4}_\\d{8}$"
    
    if (grepl(analysis_pattern, folder_name)) {
      # Look for existing analysis_lut RDS file
      lut_file <- file.path(current_dir, paste0("analysis_lut_", folder_name, ".rds"))
      
      if (file.exists(lut_file)) {
        existing_analysis_lut <- readRDS(lut_file)
        
        # Legacy (ETL 1.0) luts carry analysis_name; still usable for local
        # work, but finalize_analysis_lut() will abort at export time
        if ("analysis_name" %in% names(existing_analysis_lut)) {
          cli::cli_alert_warning(
            "Legacy (ETL 1.0) analysis_lut loaded; database export will fail. Start a new session to export."
          )
        }
        
        cli::cli_alert_success("Loaded existing analysis_lut from {.path {lut_file}}")
        cli::cli_alert_info("Using analysis_id: {.val {existing_analysis_lut$analysis_id}}")
        assign("analysis_lut", existing_analysis_lut, envir = .GlobalEnv)
        return(invisible(existing_analysis_lut))
      }
    }
  }
  
  # If no existing analysis found, create new one
  if (!exists("analysis_lut", envir = .GlobalEnv)) {
    
    # Generate session-specific analysis uuid
    analysis_id <- uuid::UUIDgenerate()
    analysis_id <- tolower(analysis_id)  # force lowercase (PostgreSQL standard)
    cli::cli_alert_success("New analysis_id created: {.val {substr(analysis_id, 1, 8)}...}")
    
    # Create fishery identifier
    fishery_name <- resolved_params$fishery_name
    year_format <- dplyr::case_when(
      stringr::str_detect(fishery_name, "\\d{4}-\\d{2}") ~ "yyyy-yy",
      stringr::str_detect(fishery_name, "\\b\\d{4}\\b") ~ "yyyy",
      TRUE ~ "other"
    )
    
    last_two_digits <- dplyr::case_when(
      year_format == "yyyy-yy" ~ stringr::str_extract(fishery_name, "(?<=-)\\d{2}"),
      year_format == "yyyy" ~ stringr::str_sub(stringr::str_extract(fishery_name, "\\b\\d{4}\\b"), -2),
      TRUE ~ NA_character_
    )
    
    # Extract initials from fishery name
    initials <- fishery_name |>
      stringr::str_replace_all("\\band\\b|\\d{4}-\\d{2}|\\b\\d{4}\\b", "") |>
      stringr::str_extract_all("\\b\\w") |>
      purrr::map_chr(~ paste(.x, collapse = "")) |>
      stringr::str_to_upper()
    
    identifier <- paste0(initials, last_two_digits)
    
    # Create analysis folder name: IDENTIFIER_LAST4_YYYYMMDD
    analysis_folder_name <- paste(
      identifier,
      stringr::str_sub(analysis_id, -4),
      format(Sys.Date(), "%Y%m%d"),
      sep = "_"
    )
    
    # Git metadata: SHA and tag only when HEAD on a tag
    git_sha <- tryCatch(
      system("git rev-parse HEAD", intern = TRUE),
      error = function(e) NA_character_
    )
    git_tag <- tryCatch({
      tag <- system("git tag --points-at HEAD", intern = TRUE)
      if (length(tag) == 0) NA_character_ else tag[1]
    }, error = function(e) NA_character_)

    params_json <- as.character(
      jsonlite::toJSON(resolved_params, auto_unbox = TRUE, pretty = TRUE)
    )
    
    # Create analysis_lut data frame
    analysis_lut <- data.frame(
      analysis_id = analysis_id,
      project_name = resolved_params$project_name,
      fishery_name = fishery_name,
      analysis_folder_name = analysis_folder_name,
      model_run_type = resolved_params$model_run_type,
      git_sha = git_sha,
      git_tag = git_tag,
      params_json = params_json,
      stringsAsFactors = FALSE
    )
    
    assign("analysis_lut", analysis_lut, envir = .GlobalEnv)
    
  } else {
    cli::cli_alert_info("analysis_lut with unique 'analysis_id' already exists in this R session.")
    analysis_lut <- get("analysis_lut", envir = .GlobalEnv)
  }
  
  return(invisible(analysis_lut))
}
