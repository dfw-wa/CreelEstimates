# ==============================================================================
# explore_creel_db_2022_2025.R
#
# Purpose:
#   Everything the PST pipeline reads from the creel database goes through
#   creelutils' analysis views (vw_analysis_interview, ..._catch, ..._effort),
#   filtered by fishery_name. Data that never made it into a fishery - or into
#   the analysis views at all - is invisible. Known case: Lewis River creel data
#   should exist in FISH.creel but no Lewis interview reaches the pipeline.
#
#   This pulls every INTERVIEW table and view in schema `creel` of the FISH
#   database (relation name matching RELATION_PATTERN - effort and catch are
#   deliberately left out), scoped only by date (2022-01-01 to 2025-12-31)
#   where the relation has a date column, whole where it has none (up to
#   MAX_UNDATED_ROWS), and caches each one locally so it can be explored
#   without re-querying. No fishery, water body or project filter anywhere.
#
#   Then it searches every text column of every pulled relation for SEARCH
#   (default "lewis") and reports where it turns up and when.
#
# Also lists (does not pull) every other relation in the schema, so a
# differently-named interview table would still show up by name.
#
# Needs DB/VPN access and creelutils (for connect_creel_db()).
#
# Usage:
#   Rscript analysis/pst/02_ingest/explore_creel_db_2022_2025.R
#   Rscript analysis/pst/02_ingest/explore_creel_db_2022_2025.R "lewis|kalama"
#
# Output:
#   .cache/creel_db_2022_2025/<relation>.rds            - one per relation
#   analysis/pst/outputs/04_interview_proportions/
#     creel_db_inventory_2022_2025.csv   - relation, type, rows in DB, rows
#                                          pulled, date column used, status
#     creel_db_search_hits.csv           - relation x column x value, n rows
#     creel_db_search_hits_by_month.csv  - relation x value x year x month
#
# Re-running reads the cache; delete .cache/creel_db_2022_2025/ to re-pull.
# ==============================================================================

suppressMessages({
  library(tidyverse)
  library(here)
  library(glue)
})
options(width = 220)

args <- commandArgs(trailingOnly = TRUE)
SEARCH <- if (length(args) >= 1 && nzchar(args[1])) args[1] else "lewis"

SCHEMA           <- "creel"
DATE_FROM        <- "2022-01-01"
DATE_TO          <- "2025-12-31"
MAX_UNDATED_ROWS <- 2e6
RELATION_PATTERN <- "interview"   # interviews only - no effort, no catch
# Preferred date column when a relation has several.
DATE_COL_PREF    <- c("event_date", "survey_date", "interview_date",
                      "sample_date", "trip_date", "date")

CACHE   <- here(".cache", "creel_db_2022_2025")
OUT_DIR <- here("analysis", "pst", "outputs", "04_interview_proportions")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Flatten anything DBI hands back that won't bind or save cleanly (geometry,
# blobs, integer64) to character; list columns become NA.
flatten <- function(d) {
  d |> mutate(across(everything(), \(x) {
    if (is.list(x)) NA_character_ else as.character(x)
  }))
}

# ---- 1. Inventory + pull -----------------------------------------------------

inv_path <- file.path(OUT_DIR, "creel_db_inventory_2022_2025.csv")
cached <- list.files(CACHE, pattern = "\\.rds$")

if (length(cached) == 0) {
  if (!requireNamespace("creelutils", quietly = TRUE)) {
    stop("No cache and creelutils not installed - run on a machine with DB access.",
         call. = FALSE)
  }
  conn <- creelutils::connect_creel_db()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  rels <- DBI::dbGetQuery(conn, glue("
    SELECT table_name, table_type FROM information_schema.tables
    WHERE table_schema = '{SCHEMA}' AND table_name ~* '{RELATION_PATTERN}'
    ORDER BY table_type, table_name"))
  cols <- DBI::dbGetQuery(conn, glue("
    SELECT table_name, column_name, data_type FROM information_schema.columns
    WHERE table_schema = '{SCHEMA}'"))
  all_rels <- DBI::dbGetQuery(conn, glue("
    SELECT table_name, table_type FROM information_schema.tables
    WHERE table_schema = '{SCHEMA}' ORDER BY table_name"))
  cat(glue("Other relations in {SCHEMA} (listed, not pulled): ",
           "{paste(setdiff(all_rels$table_name, rels$table_name), collapse = ', ')}\n\n"))
  cat(glue("{nrow(rels)} interview relations in {SCHEMA} ",
           "({sum(rels$table_type == 'BASE TABLE')} tables, ",
           "{sum(rels$table_type == 'VIEW')} views)\n\n"))

  inv <- map_dfr(seq_len(nrow(rels)), \(i) {
    t    <- rels$table_name[i]
    tc   <- cols |> filter(table_name == t)
    dcol <- tc |> filter(data_type %in% c("date", "timestamp without time zone",
                                          "timestamp with time zone"))
    date_col <- intersect(DATE_COL_PREF, dcol$column_name)[1]
    if (is.na(date_col) && nrow(dcol) > 0) {
      # Any other date column except audit stamps.
      other <- setdiff(dcol$column_name[!str_detect(dcol$column_name,
                        "created|modified|updated|obsolete|loaded")], character(0))
      date_col <- other[1]
    }
    q_t <- glue('{SCHEMA}."{t}"')
    n_db <- tryCatch(as.numeric(DBI::dbGetQuery(conn, glue("SELECT count(*) AS n FROM {q_t}"))$n),
                     error = function(e) NA_real_)

    sql <- if (!is.na(date_col)) {
      glue('SELECT * FROM {q_t} WHERE "{date_col}" >= \'{DATE_FROM}\' AND "{date_col}" < \'{as.Date(DATE_TO) + 1}\'')
    } else if (!is.na(n_db) && n_db <= MAX_UNDATED_ROWS) {
      glue("SELECT * FROM {q_t}")
    } else NA_character_

    status <- "pulled"
    n_pulled <- NA_real_
    if (is.na(sql)) {
      status <- glue("skipped: no date column and {n_db} rows > MAX_UNDATED_ROWS")
    } else {
      d <- tryCatch(DBI::dbGetQuery(conn, sql), error = function(e) e)
      if (inherits(d, "error")) {
        status <- paste("failed:", conditionMessage(d))
      } else {
        d <- flatten(as_tibble(d))
        n_pulled <- nrow(d)
        saveRDS(d, file.path(CACHE, paste0(make.names(t), ".rds")))
      }
    }
    cat(sprintf("  %-45s %-10s db=%-9s pulled=%-9s date=%-18s %s\n", t,
                rels$table_type[i], format(n_db), format(n_pulled),
                coalesce(date_col, "-"), status))
    tibble(relation = t, type = rels$table_type[i], rows_in_db = n_db,
           rows_pulled = n_pulled, date_col = date_col, status = status,
           file = paste0(make.names(t), ".rds"))
  })
  write_csv(inv, inv_path)
  cat(glue("\nInventory -> {inv_path}\n\n"))
} else {
  cat(glue("Reading {length(cached)} cached relations from {CACHE} (delete it to re-pull)\n\n"))
  inv <- if (file.exists(inv_path)) read_csv(inv_path, show_col_types = FALSE) else
    tibble(relation = sub("\\.rds$", "", cached), file = cached, status = "pulled")
}

# ---- 2. Search every text column for SEARCH ----------------------------------

cat(glue("=== searching all pulled relations for /{SEARCH}/ (case-insensitive) ===\n\n"))
pat <- regex(SEARCH, ignore_case = TRUE)

hits <- list(); by_month <- list()
for (i in seq_len(nrow(inv))) {
  f <- file.path(CACHE, inv$file[i])
  if (!file.exists(f)) next
  d <- readRDS(f)
  if (nrow(d) == 0) next
  hit_cols <- names(d)[map_lgl(d, \(x) any(str_detect(coalesce(x, ""), pat)))]
  if (length(hit_cols) == 0) next

  for (cl in hit_cols) {
    hits[[length(hits) + 1]] <- d |>
      filter(str_detect(coalesce(.data[[cl]], ""), pat)) |>
      count(value = .data[[cl]], name = "n_rows") |>
      mutate(relation = inv$relation[i], column = cl, .before = 1)
  }

  dcol <- inv$date_col[i]
  if (!is.na(dcol) && dcol %in% names(d)) {
    any_hit <- reduce(map(hit_cols, \(cl) str_detect(coalesce(d[[cl]], ""), pat)), `|`)
    sub <- d[any_hit, ]
    first_hit <- do.call(coalesce, map(hit_cols, \(cl)
      if_else(str_detect(coalesce(sub[[cl]], ""), pat), sub[[cl]], NA_character_)))
    key <- sub |>
      mutate(.date = suppressWarnings(as.Date(substr(.data[[dcol]], 1, 10))),
             .value = first_hit)
    by_month[[length(by_month) + 1]] <- key |>
      count(value = .value, year = lubridate::year(.date),
            month = lubridate::month(.date), name = "n_rows") |>
      mutate(relation = inv$relation[i], .before = 1)
  }
}

hits <- bind_rows(hits)
by_month <- bind_rows(by_month)
if (nrow(hits) == 0) {
  cat("No matches in any pulled relation.\n")
} else {
  write_csv(hits, file.path(OUT_DIR, "creel_db_search_hits.csv"))
  write_csv(by_month, file.path(OUT_DIR, "creel_db_search_hits_by_month.csv"))

  cat("--- relations x columns with matches ---\n")
  hits |> group_by(relation, column) |>
    summarise(rows = sum(n_rows), values = n_distinct(value),
              examples = paste(head(unique(value), 4), collapse = " | "), .groups = "drop") |>
    arrange(desc(rows)) |> as.data.frame() |> print(row.names = FALSE)

  if (nrow(by_month) > 0) {
    cat("\n--- dated matches by relation x year (rows) ---\n")
    by_month |> group_by(relation, year) |>
      summarise(rows = sum(n_rows), months = paste(sort(unique(month)), collapse = ","),
                .groups = "drop") |>
      pivot_wider(names_from = year, values_from = c(rows, months)) |>
      as.data.frame() |> print(row.names = FALSE)
  }
  cat(glue("\nDetail -> {file.path(OUT_DIR, 'creel_db_search_hits.csv')} and _by_month.csv\n"))
}

cat("\n=== pull status ===\n")
inv |> count(status = str_extract(status, "^[a-z]+")) |> print()
cat("\nTo explore a relation:  d <- readRDS('.cache/creel_db_2022_2025/<name>.rds')\n")
