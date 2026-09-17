# ==============================================================================
# inspect_guide_logbook_schema.R
#
# Purpose:
#   Read-only reconnaissance of the guide logbook RDS extract. parse_guide_logbook.R
#   uses 5 of the extract's ~22 tables; this prints what is in ALL of them so two
#   open questions can be settled before the logbook is wired in as the source of
#   the guided proportion:
#
#     Q1 BOAT USE. Is the logbook exclusively boat trips, or does it carry a
#        field that distinguishes boat from bank? This decides whether guided
#        trips can be assigned to the boat stratum as a rule, or whether the
#        bank/boat split has to be imputed for them like every other unknown.
#
#     Q2 SPECIES. Does any table record catch or target species per trip? The
#        logbook is otherwise species-agnostic, so a Hoh winter steelhead guide
#        trip counts the same as a fall Chinook trip. Restricting to months with
#        CRC salmon harvest removes the seasonal part of that contamination but
#        not the within-month part. A species field would remove both.
#
#   Writes nothing and changes nothing - it only prints.
#
# Usage:
#   Rscript analysis/pst/02_ingest/inspect_guide_logbook_schema.R
#   Requires input_files/pst/guide_logbook/guide_logbook_data_2026-09-02.rds,
#   which is NOT committed - run on a machine that has it.
# ==============================================================================

suppressMessages({
  library(tidyverse)
  library(here)
  library(glue)
})

options(width = 200)

RDS_PATH <- here("input_files", "pst", "guide_logbook",
                 "guide_logbook_data_2026-09-02.rds")

if (!file.exists(RDS_PATH)) {
  stop(glue("Guide logbook extract not found at:\n  {RDS_PATH}\n",
            "This extract is not committed - run this on a machine that has it."),
       call. = FALSE)
}

gl <- readRDS(RDS_PATH)

# Tables parse_guide_logbook.R already uses, so the unexplored ones stand out.
USED <- c("trip", "trip_type_lut", "trip_angler", "trip_angler_type_lut",
          "water_body_lut")

cat("\n================ ALL TABLES ================\n")
tbl_summary <- tibble(
  table = names(gl),
  rows  = vapply(gl, function(x) if (is.data.frame(x)) nrow(x) else NA_integer_, integer(1)),
  cols  = vapply(gl, function(x) if (is.data.frame(x)) ncol(x) else NA_integer_, integer(1)),
  used_by_parser = names(gl) %in% USED
) |> arrange(desc(used_by_parser), desc(rows))
print(as.data.frame(tbl_summary), row.names = FALSE)

cat("\n================ COLUMNS PER TABLE ================\n")
for (nm in names(gl)) {
  x <- gl[[nm]]
  if (!is.data.frame(x)) {
    cat("\n-- ", nm, ": not a data frame (", class(x)[1], ")\n", sep = ""); next
  }
  cat("\n-- ", nm, " (", nrow(x), " rows)",
      if (nm %in% USED) "  [already used]" else "", "\n", sep = "")
  cat("   ", paste(names(x), collapse = ", "), "\n", sep = "")
}

# ---- Targeted search for the two questions ----------------------------------
# Column NAMES are matched rather than contents, then any hit has its distinct
# values printed - a lookup table's values are what actually answer the
# question ("Drift Boat"/"Sled"/"Bank" vs. a meaningless integer key).

# Several columns in this extract are integer64, and some (`version`) are
# integer64 bit patterns being read as denormal doubles - printing those as
# characters yields hundreds of digits per value and segfaulted the first
# version of this script. Coerce to something printable and cap the width.
safe_chr <- function(v) {
  if (inherits(v, "integer64")) v <- suppressWarnings(as.numeric(v))
  if (is.numeric(v)) {
    v <- suppressWarnings(as.numeric(v))
    fin <- v[is.finite(v) & v != 0]
    # Only reach for scientific notation when a value would otherwise print
    # hundreds of digits (the denormal `version` columns). Ordinary counts and
    # hours stay readable.
    pathological <- length(fin) > 0 &&
      (max(abs(fin)) >= 1e9 || min(abs(fin)) < 1e-4)
    return(if (pathological) format(signif(v, 4), scientific = TRUE, trim = TRUE)
           else format(signif(v, 6), scientific = FALSE, trim = TRUE))
  }
  substr(as.character(v), 1, 60)
}

# Sort numerics as numbers, everything else as text, then format. Sorting the
# formatted strings instead would order 100 before 2.
safe_sorted <- function(v) {
  if (inherits(v, "integer64")) v <- suppressWarnings(as.numeric(v))
  if (is.numeric(v)) v <- sort(v) else v <- sort(as.character(v))
  safe_chr(v)
}

# Columns that are audit plumbing rather than content - never informative here,
# and several are the pathological ones above.
NOISE <- paste0("create_datetime|modify_datetime|obsolete_datetime|receive_datetime",
                "|client_create|client_modify|^version$|platform|operating_system",
                "|client_version|app_version|sort_order|client_trip_id")

drop_noise <- function(x) x[, !str_detect(names(x), regex(NOISE, ignore_case = TRUE)),
                            drop = FALSE]

show_values <- function(v, indent = "   ") {
  u <- unique(v[!is.na(v)])
  out <- tryCatch(safe_sorted(u), error = function(e) safe_chr(u))
  if (length(out) <= 25) {
    cat(indent, "values: ", paste(out, collapse = " | "), "\n", sep = "")
  } else {
    cat(indent, length(out), " distinct; first 12: ",
        paste(head(out, 12), collapse = " | "), "\n", sep = "")
  }
}

probe <- function(pattern, label) {
  cat("\n================ ", label, " ================\n", sep = "")
  hits <- 0L

  # Matching TABLE names matters as much as column names. A `boat_type_id`
  # column holds integer keys that say nothing on their own - the answer is in
  # boat_type_lut, whose own column is just called `name` and would never match
  # the pattern. Lookup tables are small, so print them whole.
  for (nm in names(gl)) {
    x <- gl[[nm]]
    if (!is.data.frame(x)) next
    if (!str_detect(nm, regex(pattern, ignore_case = TRUE))) next
    hits <- hits + 1L
    xs <- drop_noise(x)
    cat("\n-- TABLE ", nm, " (", nrow(x), " rows, ",
        ncol(x), " cols; showing <=15 rows, plumbing columns dropped):\n", sep = "")
    tryCatch({
      xs <- xs |> mutate(across(everything(), safe_chr))
      print(as.data.frame(head(xs, 15)), row.names = FALSE)
      if (nrow(x) > 15) cat("   ... ", nrow(x) - 15, " more rows\n", sep = "")
    }, error = function(e) cat("   (could not print: ", conditionMessage(e), ")\n", sep = ""))
  }

  for (nm in names(gl)) {
    x <- gl[[nm]]
    if (!is.data.frame(x)) next
    cols <- names(x)[str_detect(names(x), regex(pattern, ignore_case = TRUE))]
    for (cl in cols) {
      hits <- hits + 1L
      v <- x[[cl]]
      cat("\n-- ", nm, "$", cl, "  (", class(v)[1], ", ",
          sum(!is.na(v)), " non-NA of ", length(v), ")\n", sep = "")
      show_values(v)
      # An integer key is only meaningful once resolved - point at the lookup
      # that resolves it if one is present under the obvious name.
      lut <- str_replace(cl, "_id$", "_lut")
      if (str_ends(cl, "_id") && lut %in% names(gl)) {
        cat("   -> resolves via ", lut, "\n", sep = "")
      }
    }
  }

  if (hits == 0L) {
    cat("\nNo table or column name matches /", pattern, "/ anywhere.\n", sep = "")
  }
  invisible(hits)
}

n_boat <- probe("boat|vessel|craft|launch|ramp|bank|shore|access|moor|motor|kayak|raft|drift",
                "Q1  BOAT / BANK FIELDS")
n_spp  <- probe("species|fish|target|catch|salmon|steelhead|retain|kept|harvest",
                "Q2  SPECIES / CATCH FIELDS")

# ---- Verdict -----------------------------------------------------------------

cat("\n================ READ THIS ================\n")
cat(
  "Q1 BOAT USE:\n",
  if (n_boat > 0)
    "   Candidate field(s) found above - check whether the values actually separate\n   boat from bank, or are only vessel DETAIL on trips that are all boat trips.\n"
  else
    "   No boat/bank field. Either every logged trip is a boat trip (in which case\n   guided trips can be assigned to the boat stratum as a rule), or boat use is\n   simply not recorded (in which case that assignment is an assumption to state).\n   The licence rules, not this extract, settle which - ask the logbook programme.\n",
  "\nQ2 SPECIES:\n",
  if (n_spp > 0)
    "   Candidate field(s) found above - if any gives catch or target species per\n   trip, the logbook can be restricted to salmon trips directly and the Hoh\n   steelhead contamination largely goes away.\n"
  else
    "   No species field. Month-matching against CRC salmon harvest is the only\n   available filter, and within-month steelhead contamination stays as a known,\n   unquantified bias - which has to be stated wherever the floor is reported.\n",
  sep = ""
)

cat("\nDone. Nothing was written.\n")
