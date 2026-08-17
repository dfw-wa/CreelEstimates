# ==============================================================================
# run_pst_pipeline.R
# Location: analysis/pst/run_pst_pipeline.R
#
# Purpose:
#   Orchestrator for the PST freshwater angler-trips pipeline. Runs the
#   producer scripts under 02_ingest/ and the two assembly scripts under
#   03_analysis/ in dependency order, with every step guarded so a failure
#   in one step never stops the others from being attempted.
#
# Usage:
#   Rscript analysis/pst/run_pst_pipeline.R
#   (run from anywhere inside the repo; here::here() resolves the root)
#
# What needs DB/VPN:
#   Step 2 (multi_fishery_creel_summary.R) hits the internal creel DB via
#   creelutils::fetch_data(). Step 4 (interview_proportions.qmd) also hits
#   the DB and is a Quarto document, not an Rscript step - see "Step 4" below
#   for how this orchestrator handles it. Steps 1, 3, 5, 6 read only local
#   files and need no network access.
#
# What this script does NOT do:
#   - It does not render 02_ingest/interview_proportions.qmd (step 4). That
#     document is a standalone producer, documented in its own header as
#     "run manually to refresh Track B" - this orchestrator honors that and
#     only checks whether its outputs are already on disk, reporting a clear
#     manual-run instruction if not. Rendering it here would silently start
#     a DB-dependent Quarto render inside an Rscript batch job, which is a
#     worse failure mode than telling the user to run it themselves.
#   - It does not render the parent doc, pst_fw_angler_trips.qmd (step 7).
#     That render is the last manual step in the workflow and is reported as
#     such in the final summary, with the exact command to run.
#   - It does not run 02_ingest/patch_crosswalk_areas.R. That is a one-shot,
#     idempotent maintenance script that rewrites
#     input_files/pst/lookup_tables/pst_river_block_crosswalk.csv in place; it is not part
#     of the routine pipeline and must be run and reviewed by hand. See its
#     own header for what it does.
#   - It does not install or check for packages. If a step's own library()
#     calls fail, that surfaces as a normal step failure in the summary.
#
# Step isolation: system2("Rscript", ...), not source().
#   Every producer script under 02_ingest/ and 03_analysis/ defines its own
#   top-level objects with the SAME NAMES - OUT_DIR, PST_DIR, read_if(),
#   log_gap(), canon(), GRAIN, and so on (compare
#   pst_fw_angler_trips_assembly.R and pst_fw_build_jim_workbook.R, which both
#   define read_if()/log_note() at file scope). source()-ing several of
#   these into one session would let a later script's definitions silently
#   shadow an earlier script's, or trip on a leftover object from a prior
#   step - exactly the kind of bug that would be invisible until the numbers
#   came out wrong. Each step below therefore runs as its own `Rscript`
#   subprocess (matching the "Usage: Rscript ..." line already documented in
#   every script's own header), so every step starts from a clean R session
#   and a failure in one cannot corrupt the environment of the next. The
#   cost is one process start per step (a few seconds of overhead) and no
#   shared in-memory objects between steps - acceptable here because every
#   step already communicates with the next exclusively through files in
#   analysis/pst/outputs/, never through shared R objects. callr::r() would
#   give the same isolation with a slightly cleaner captured-output API, but
#   it is not a dependency used anywhere else in this repo (render.R and
#   every script here use base library()/Rscript); system2() avoids adding
#   one just for this.
#
# Design rule this file follows (from pst_fw_angler_trips_assembly.R):
#   [R2] "A missing input is logged as a gap, never silently dropped and
#   never fatal. Partial assembly is the expected state until all providers
#   return." Applied here at the step level: every step is wrapped so an
#   error is caught, logged with its condition message, and does NOT stop
#   the run. Steps 5 and 6 depend on upstream outputs but are still
#   ATTEMPTED even when known to be missing inputs, because
#   pst_fw_angler_trips_assembly.R degrades gracefully (empty/partial tables, not
#   an error) when its inputs are absent - skipping them pre-emptively here
#   would contradict that design and hide what the assembly script is
#   actually able to produce from what's on disk right now.
# ==============================================================================

library(here)
library(cli)
library(glue)

# ---- 0. Steps to run ---------------------------------------------------------
# Edit this vector to run a subset. Names must match STEP_REGISTRY below.
# Default is every automated step, in dependency order. Step 4 and step 7 are
# not in this vector - see the header for why (interview_proportions.qmd and
# the parent render are reported as manual steps instead).

STEPS <- c(
  "01_crc_freshwater_harvest",
  "02_multi_fishery_creel_summary",
  "03_mid_columbia_yakima",
  "05_effort_assembly",
  "06_jim_workbook"
)

# ---- 1. Step registry ---------------------------------------------------------
# path       : script run via Rscript, relative to the repo root
# needs_db   : DB/VPN required (informational only, not enforced)
# depends_on : upstream step names this one reads outputs from (informational;
#              see header - dependents are attempted regardless per [R2])

STEP_REGISTRY <- list(
  "01_crc_freshwater_harvest" = list(
    path       = "analysis/pst/02_ingest/parse_crc_freshwater_harvest.R",
    needs_db   = FALSE,
    depends_on = character(0)
  ),
  "02_multi_fishery_creel_summary" = list(
    path       = "analysis/pst/02_ingest/multi_fishery_creel_summary.R",
    needs_db   = TRUE,
    depends_on = character(0)
  ),
  "03_mid_columbia_yakima" = list(
    path       = "analysis/pst/02_ingest/mid_columbia_yakima_creel_ingestion.R",
    needs_db   = FALSE,
    depends_on = character(0)
  ),
  "05_effort_assembly" = list(
    path       = "analysis/pst/03_analysis/pst_fw_angler_trips_assembly.R",
    needs_db   = FALSE,
    depends_on = c("01_crc_freshwater_harvest", "02_multi_fishery_creel_summary",
                    "03_mid_columbia_yakima", "04_interview_proportions")
  ),
  "06_jim_workbook" = list(
    path       = "analysis/pst/03_analysis/pst_fw_build_jim_workbook.R",
    needs_db   = FALSE,
    depends_on = c("05_effort_assembly")
  )
)

# ---- 2. Step 4 status check (not run here; see header) ------------------------
# interview_proportions.qmd is a standalone producer with its own header
# saying it is "run manually to refresh Track B" and is deliberately not
# included by the parent Quarto doc. This orchestrator does not render it -
# it only checks whether the outputs it feeds to step 5 are present, and
# tells the user how to produce them if not.

check_step4_outputs <- function() {
  out_dir <- here::here("analysis", "pst", "outputs")
  required <- c("interview_mode_location_props.csv", "interview_batch_crosscheck.csv",
                "all_interviews.csv", "all_interviews.rds")
  present <- file.exists(file.path(out_dir, required))
  list(all_present = all(present),
       missing     = required[!present])
}

# ---- 3. Guarded runner --------------------------------------------------------
# [R2]: a failing step is logged and does not abort the run. Each step is a
# fresh Rscript subprocess (see header, "Step isolation").

run_step <- function(name) {
  step <- STEP_REGISTRY[[name]]
  script_path <- here::here(step$path)
  t0 <- Sys.time()

  cli::cli_h2("Step {name}: {step$path}")
  if (step$needs_db) {
    cli::cli_alert_info("Requires DB/VPN access.")
  }

  if (!file.exists(script_path)) {
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    cli::cli_alert_danger("Script not found at {script_path} - skipping.")
    return(list(step = name, status = "failed", elapsed = elapsed,
                message = glue("script not found at {script_path}")))
  }

  result <- tryCatch({
    proc <- system2("Rscript", args = shQuote(script_path),
                     stdout = "", stderr = "")
    if (!identical(proc, 0L)) {
      stop(glue("Rscript exited with status {proc}"))
    }
    list(status = "ok", message = NA_character_)
  }, error = function(e) {
    list(status = "failed", message = conditionMessage(e))
  })

  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  if (result$status == "ok") {
    cli::cli_alert_success("{name} completed in {round(elapsed, 1)}s.")
  } else {
    cli::cli_alert_danger("{name} FAILED after {round(elapsed, 1)}s: {result$message}")
  }

  list(step = name, status = result$status, elapsed = elapsed,
       message = result$message)
}

# ---- 4. Run --------------------------------------------------------------------

cli::cli_h1("PST freshwater angler-trips pipeline")
cli::cli_alert_info("Steps queued: {paste(STEPS, collapse = ', ')}")

step4 <- check_step4_outputs()
if (step4$all_present) {
  cli::cli_alert_success(paste(
    "Step 4 (interview_proportions.qmd) outputs already present in",
    "analysis/pst/outputs/ - not re-rendered by this orchestrator."
  ))
} else {
  cli::cli_alert_warning(glue(
    "Step 4 (interview_proportions.qmd) outputs missing: ",
    "{paste(step4$missing, collapse = ', ')}. This script does NOT render ",
    "that document (it needs DB/VPN and is meant to be run manually). ",
    "Render it yourself: quarto render analysis/pst/02_ingest/",
    "interview_proportions.qmd -- step 5 below will still run and will log ",
    "the missing interview_prop input as a gap rather than fail."
  ))
}

results <- lapply(STEPS, run_step)

# ---- 5. Summary ------------------------------------------------------------

cli::cli_h1("Summary")

summary_tbl <- data.frame(
  step    = vapply(results, `[[`, character(1), "step"),
  status  = vapply(results, `[[`, character(1), "status"),
  elapsed_sec = round(vapply(results, `[[`, numeric(1), "elapsed"), 1),
  message = vapply(results, function(r) {
    if (is.na(r$message)) "" else r$message
  }, character(1)),
  stringsAsFactors = FALSE
)

skipped <- setdiff(names(STEP_REGISTRY), STEPS)
if (length(skipped) > 0) {
  summary_tbl <- rbind(
    summary_tbl,
    data.frame(step = skipped, status = "skipped", elapsed_sec = 0,
               message = "not in STEPS", stringsAsFactors = FALSE)
  )
}

print(summary_tbl, row.names = FALSE)

n_ok      <- sum(summary_tbl$status == "ok")
n_failed  <- sum(summary_tbl$status == "failed")
n_skipped <- sum(summary_tbl$status == "skipped")
cli::cli_alert_info(glue(
  "{n_ok} ok, {n_failed} failed, {n_skipped} skipped."
))

if (n_failed > 0) {
  cli::cli_alert_warning(paste(
    "One or more steps failed. Per design rule [R2], later steps still ran.",
    "Check analysis/pst/outputs/pst_fw_gap_register.csv (written by step",
    "05_effort_assembly, if it ran) for what it was missing, and the",
    "messages above for why each failed step did not complete."
  ))
}

cli::cli_h2("Manual steps not run by this script")
cli::cli_ul(c(
  "Step 4, if outputs are stale or missing: quarto render analysis/pst/02_ingest/interview_proportions.qmd  (needs DB/VPN)",
  "Step 7, the parent document: quarto render analysis/pst/pst_fw_angler_trips.qmd  (reads everything written above; renders from whatever is on disk and reports the rest as gaps per [R2])"
))
