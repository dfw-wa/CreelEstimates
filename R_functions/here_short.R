#' Project-relative path, shortened via a mapped network drive when available
#'
#' Drop-in replacement for `here::here()`. If a mapped drive (default `"O:"`)
#' points at the same location as the user's OneDrive sync root, the returned
#' path is rewritten to start with the drive letter instead of the full
#' `"C:/Users/<username>/OneDrive - ..."` prefix. This is what actually fixes
#' Windows MAX_PATH (260-character) failures -- Windows resolves a mapped/
#' substituted drive letter to a short path at the OS level, so tools that
#' aren't long-path-aware (Explorer, many image viewers, older Office apps)
#' can open the file even though the "real" underlying path is long.
#'
#' The OneDrive root is read from the `OneDriveCommercial` / `OneDrive`
#' environment variables that OneDrive itself sets on every user's machine,
#' so this works for any username without hardcoding one -- as long as
#' everyone has mapped the same drive letter to their OneDrive root (as your
#' team's setup script already does).
#'
#' Falls back to plain `here::here()` (no error, one-time warning) if:
#'   - not running on Windows,
#'   - the OneDrive env var can't be found, or
#'   - the mapped drive isn't currently connected in this R session.
#'
#' @param ... path components, passed straight through to `here::here()`
#' @param mapped_drive drive letter (with colon) mapped to the OneDrive root.
#'   Defaults to `"O:"`.
#' @return character; a single absolute path
#'
#' @examples
#' \dontrun{
#' here_short("fishery_analyses", params$project_name)
#' # "O:/Projects/Creel/GitHub/CreelEstimates/fishery_analyses/CRM-Roving Creel Project"
#' # instead of
#' # "C:/Users/bentlktb/OneDrive - Washington State Executive Branch Agencies/Projects/Creel/GitHub/CreelEstimates/fishery_analyses/CRM-Roving Creel Project"
#' }
here_short <- function(..., mapped_drive = "O:") {

  full_path <- here::here(...)

  # Only relevant on Windows -- this whole problem is a Windows path-length thing
  if (.Platform$OS.type != "windows") {
    return(full_path)
  }

  # OneDrive sets one of these env vars to the sync root for the signed-in account.
  # OneDriveCommercial = work/school account (this is the one you'll want here);
  # OneDrive = personal account, kept as a fallback.
  onedrive_root <- Sys.getenv("OneDriveCommercial")
  if (!nzchar(onedrive_root)) onedrive_root <- Sys.getenv("OneDrive")

  if (!nzchar(onedrive_root)) {
    cli::cli_alert_warning(
      "here_short(): could not detect a OneDrive root from environment variables; using the full path."
    )
    return(full_path)
  }

  onedrive_root_norm <- normalizePath(onedrive_root, winslash = "/", mustWork = FALSE)
  full_path_norm      <- normalizePath(full_path, winslash = "/", mustWork = FALSE)

  # Confirm the mapped drive is actually connected in *this* R session before
  # rewriting anything -- mapped drives are per-login-session, so this can be
  # FALSE if RStudio was launched before the drive was (re)mapped.
  mapped_drive_connected <- dir.exists(paste0(mapped_drive, "/"))

  if (!mapped_drive_connected) {
    cli::cli_alert_warning(
      "here_short(): {mapped_drive} is not currently connected; using the full path. \\
       Map the drive, then restart R/RStudio."
    )
    return(full_path)
  }

  if (!startsWith(full_path_norm, onedrive_root_norm)) {
    # Project isn't actually under the detected OneDrive root -- nothing to shorten
    return(full_path)
  }

  candidate_path <- sub(onedrive_root_norm, mapped_drive, full_path_norm, fixed = TRUE)

  # mapped_drive existing isn't proof it points at the OneDrive root -- it could
  # be mapped to something unrelated (a stale mapping, a different share, etc.).
  # Verify by checking that the *project root* -- which is guaranteed to already
  # exist, since R is running from it -- actually exists at the candidate path too.
  project_root_candidate <- sub(onedrive_root_norm, mapped_drive, here::here(), fixed = TRUE)

  if (!dir.exists(project_root_candidate)) {
    cli::cli_alert_warning(
      "here_short(): {mapped_drive} exists but doesn't appear to point at the \\
       OneDrive root (expected to find the project at {.path {project_root_candidate}}); \\
       using the full path instead."
    )
    return(full_path)
  }

  candidate_path
}
