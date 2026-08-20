# Create folder structure for analysis outputs and configure knitr settings

#' Set up analysis folder structure and knitr cache settings
#' 
#' @param params List of parameters from Rmd YAML header
#' @param analysis_lut Analysis lookup table with folder information
#' @param est_dates Estimate dates resolved from the database fishery lookup table or manually entered by a user
#' @param mapped_drive Drive letter (with colon) mapped to the OneDrive root,
#'   used by `here_short()` to shorten output paths and avoid Windows
#'   MAX_PATH issues. Defaults to `"O:"`. Set to `NULL` to skip shortening
#'   and always use the full `here::here()` path.
#' 
#' @return List with paths to outputs folders
setup_analysis_structure <- function(params, analysis_lut, est_dates, mapped_drive = "O:") {
  
  # Define analysis folder path (shortened via mapped drive when available --
  # see here_short(); falls back to here::here() if mapped_drive is NULL or
  # the drive can't be confirmed)
  analysis_folder_path <- if (!is.null(mapped_drive)) {
    here_short(
      "fishery_analyses", 
      params$project_name, 
      params$fishery_name, 
      analysis_lut$analysis_folder,
      mapped_drive = mapped_drive
    )
  } else {
    here::here(
      "fishery_analyses", 
      params$project_name, 
      params$fishery_name, 
      analysis_lut$analysis_folder
    )
  }
  
  # Create analysis folder
  if (!dir.exists(analysis_folder_path)) {
    dir.create(analysis_folder_path, recursive = TRUE)
    cli::cli_alert_success("Analysis folder created at {.path {analysis_folder_path}}")
  } else {
    cli::cli_alert_info("Using existing analysis folder at {.path {analysis_folder_path}}")
  }
  
  # Create organized subfolders for outputs
  outputs_folders <- list(
    inputs = file.path(analysis_folder_path, "inputs"),
    outputs = file.path(analysis_folder_path, "outputs"),
    figures = file.path(analysis_folder_path, "figures")
  )
  
  purrr::walk(outputs_folders, ~ {
    if (!dir.exists(.x)) {
      dir.create(.x, recursive = TRUE)
    }
  })
  
  # Define cache location (only create folder if caching is enabled)
  cache_path <- file.path(analysis_folder_path, ".cache")
  
  if (isTRUE(params$enable_cache)) {
    if (!dir.exists(cache_path)) {
      dir.create(cache_path, recursive = TRUE)
    }
    
    # Set knitr cache to SEPARATE location from analysis outputs
    knitr::opts_chunk$set(
      cache = TRUE,
      cache.path = paste0(cache_path, .Platform$file.sep),
      fig.path = paste0(outputs_folders$figures, .Platform$file.sep),
      fig.keep = "all",
      dev = c("png", "pdf"),
      dpi = 300,
      cache.extra = list(params, analysis_lut$analysis_id, est_dates),
      cache.lazy = FALSE
    )
  } else {
    # Caching disabled - no .cache folder created, no cache options set
    knitr::opts_chunk$set(
      cache = FALSE,
      fig.path = paste0(outputs_folders$figures, .Platform$file.sep),
      fig.keep = "all",
      dev = c("png", "pdf"),
      dpi = 300
    )
  }
  
  # Set output directory for knitted HTML
  knitr::opts_chunk$set(output_dir = analysis_folder_path)
  
  cli::cli_alert_success("Output structure created:")
  cli::cli_alert_info("  Inputs: {.path {outputs_folders$inputs}}")
  cli::cli_alert_info("  Outputs: {.path {outputs_folders$outputs}}")
  cli::cli_alert_info("  Figures: {.path {outputs_folders$figures}}")
  if (isTRUE(params$enable_cache)) {
    cli::cli_alert_info("  Cache: {.path {cache_path}}")
  } else {
    cli::cli_alert_warning("  Cache: DISABLED (no .cache folder created)")
  }
  
  # Return outputs folders for use in subsequent code
  return(list(
    analysis_folder_path = analysis_folder_path,
    outputs_folders = outputs_folders,
    cache_path = cache_path
  ))
}
