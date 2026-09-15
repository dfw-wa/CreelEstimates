# ==============================================================================
# Post-upload inspection — "what actually landed in the database?"
#
# Run after a successful fw_creel.Rmd render with params$export == "database".
# Set `analysis_id` below; everything else derives from it. Run top to bottom.
#
# PREREQUISITE — this script reads ETL 2.0 contracts. In a FRESH R session,
# before anything else:
#   remove.packages("creelutils")
#   remotes::install_github("dfw-wa/creelutils@feature/ETL-catch-groups",
#                           force = TRUE, upgrade = "never")
#   # restart R again, then confirm:
#   pkgload::is_dev_package("creelutils")             # FALSE
#   packageDescription("creelutils")$RemoteRef        # feature/ETL-catch-groups
#   args(creelutils::fishery_catchgroups)             # must show observed_only
#
# Scope: creel.model_analysis_lut, creel.model_estimates_total,
#        creel.model_estimates_stratum, and their vw_ reporting views.
#
# Section 5 additionally diffs the database against the local `creel_estimates`
# object from the render session, if it is still in scope. Everything else runs
# from the analysis_id alone, so a reviewer can inspect someone else's upload.
#
# ------------------------------------------------------------------------------
# WHAT IS ON EACH TABLE  (ETL 2.0 — creelutils@feature/ETL-catch-groups)
# ------------------------------------------------------------------------------
# All three tables additionally carry four DATABASE-OWNED audit columns that R
# never writes and must never send: created_datetime, created_by,
# modified_datetime, modified_by. They are excluded from every contract check
# below. modified_* stay NULL until a row is updated in place.
#
# model_analysis_lut  — exactly ONE row per analysis run. The reproducibility
#   record. Column contract is asserted by finalize_analysis_lut():
#     analysis_id ............ uuid PK, lowercased, generated per session
#     project_id, fishery_id . uuids resolved from project_lut / fishery_lut at
#                              finalize time; project_name/fishery_name are
#                              dropped on the database branch
#     data_grade ............. "Provisional" | "Approved" (title-cased, validated)
#     model_run_type ......... "In-season" | "Post-season" | "Unknown"
#                              ("Unknown" = both dates entered manually)
#     git_sha, git_tag ....... repo state at run time; git_tag is NA unless HEAD
#                              points at a tag
#     analysis_folder_name ... IDENTIFIER_LAST4_YYYYMMDD
#     params_json ............ resolved_params snapshot: YAML params overlaid
#                              with resolver output (dates, run type, and the
#                              catch groups WITH their model_catch_group_ids)
#     r_session_json ......... sessionInfo capture, added at finalize
#     analysis_json,
#     fishery_regulation_json  currently omitted -> NULL on the database side
#     comment_txt ............ NA unless set
#
# model_estimates_stratum — fine grain. One row per
#   section x time stratum x day_type x angler_type x catch group x model x
#   estimate_type. Written by prep_export() -> write_stratum():
#     * est_cg / project_name / fishery_name are DROPPED (ids only)
#     * angler_final is DROPPED in favor of angler_type_id (joined from
#       angler_type_lut)
#     * NaN estimate_value is coerced to 0 (stratum only)
#     * diagnostics are FILTERED OUT: n_eff, r_hat, n_div, standard_error,
#       standard_deviation — these are total-scale only by design
#
# model_estimates_total — season//window totals. Same standardized long shape,
#   RETAINS all estimate_types including diagnostics. No angler_type_id.
#
# Shared vocabulary (transform_estimates()):
#   estimate_category : catch | effort | cpue    (cpue is stratum-only)
#   estimate_type     : 18 valid values, see `valid_types` below
#   model_catch_group_id : NA on EFFORT rows by design (effort is not a
#                          fish-attribute quantity). Non-NA on catch and cpue.
#   estimate_time_period : params$period_pe or params$period_bss, by model_type
#
# vw_model_estimates_* — the base table joined out to human-readable names
#   (species / life_stage / fin_mark / fate, angler type, project, fishery)
#   plus model_run_type and data_grade denormalized down from the lut. Drops the
#   surrogate PK and the audit columns. Row counts MUST equal the base table; a
#   difference means join fan-out.
# ==============================================================================

library(dplyr)
library(tidyr)
library(creelutils)

# check that pre-export object is loaded for comparison
creel_estimates <- creel_estimates 

# ---- 0. Target -------------------------------------------------------------

analysis_id <- "18a8cf99-a514-40b3-8628-80369c22268f"

f <- glue::glue("analysis_id == '{analysis_id}'")

lut        <- fetch_db_table(conn = con, "creel", "model_analysis_lut",         filter = f)
total      <- fetch_db_table(conn = con, "creel", "model_estimates_total",      filter = f)
stratum    <- fetch_db_table(conn = con, "creel", "model_estimates_stratum",    filter = f)
total_vw   <- fetch_db_table(conn = con, "creel", "vw_model_estimates_total",   filter = f)
stratum_vw <- fetch_db_table(conn = con, "creel", "vw_model_estimates_stratum", filter = f)

# Reference vocabularies, mirrored from transform_estimates()
valid_types    <- c("total_catch", "total_effort", "daily_mean", "daily_variance",
                    "number_observations", "days_open", "mean", "standard_deviation",
                    "standard_error", "quantile_2_5", "quantile_25", "quantile_50",
                    "quantile_75", "quantile_97_5", "r_hat", "n_eff", "n_div",
                    "degrees_freedom")
stratum_reject <- c("n_eff", "r_hat", "n_div", "standard_error", "standard_deviation")
lut_contract   <- c("analysis_id", "project_id", "fishery_id", "data_grade",
                    "model_run_type", "git_sha", "git_tag", "analysis_folder_name",
                    "params_json", "analysis_json", "r_session_json",
                    "fishery_regulation_json", "comment_txt")

# Database-owned, added on insert by the table defaults. R never writes these.
db_audit_cols <- c("created_datetime", "created_by", "modified_datetime", "modified_by")

# ---- 1. Did anything land? -------------------------------------------------

tibble::tibble(
  object = c("lut", "total", "stratum", "total_vw", "stratum_vw"),
  nrow   = c(nrow(lut), nrow(total), nrow(stratum), nrow(total_vw), nrow(stratum_vw)),
  ncol   = c(ncol(lut), ncol(total), ncol(stratum), ncol(total_vw), ncol(stratum_vw))
)
# Expect: lut = 1 row. views match their base tables row-for-row.

# ---- 2. analysis_lut against the schema contract ---------------------------

list(
  missing_from_lut = setdiff(lut_contract, names(lut)),
  not_in_contract  = setdiff(names(lut), c(lut_contract, db_audit_cols))
)
# Both expect character(0). The audit columns are whitelisted — they are added
# by the database on insert, so their presence is correct, not a contract break.

lut |>
  select(-ends_with("_json")) |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(everything(), names_to = "field", values_to = "value") |>
  print(n = Inf)
# Read off: data_grade, model_run_type, git_sha, git_tag, analysis_folder_name.
# git_sha absent means this run is not reproducible from the repo. git_sha
# present still does not prove the working tree was clean at run time — that is
# a known gap until a code_clean flag is added to the lut.

# Which json snapshots are populated vs NULL
lut |>
  select(ends_with("_json")) |>
  summarise(across(everything(), ~ifelse(is.na(.), "NULL", paste0(nchar(.), " chars")))) |>
  pivot_longer(everything(), names_to = "snapshot", values_to = "state")

# ---- 3. params_json: the reproducibility snapshot --------------------------

lut$params_json |> jsonlite::prettify()

pj <- jsonlite::fromJSON(lut$params_json)

# Headline run parameters
pj[c("project_name", "fishery_name", "est_date_start", "est_date_end",
     "model_run_type", "model_used", "data_grade", "export", "export_tables")] |>
  purrr::map_chr(~paste(as.character(.x), collapse = ", ")) |>
  tibble::enframe(name = "param", value = "value") |>
  print(n = Inf)
# `export` should read "database" for a run that reached these tables. The lut
# is built once per session by generate_analysis_lut(), so a params value edited
# after that point will not be reflected here.

# The catch groups the RUN resolved, with the ids it captured
run_cg <- tibble::as_tibble(pj$est_catch_groups)
run_cg

# ---- 4. Catch group identity: captured vs written --------------------------
# The central ETL 2.0 question. Ids in params_json are the run's snapshot;
# ids in the estimate tables are what was actually keyed on. They must agree.

written_ids <- union(total$model_catch_group_id, stratum$model_catch_group_id)
written_ids <- written_ids[!is.na(written_ids)]

list(
  captured_not_written = setdiff(run_cg$model_catch_group_id, written_ids),
  written_not_captured = setdiff(written_ids, run_cg$model_catch_group_id)
)
# Both empty = the crosswalk held end to end.
# captured_not_written is expected & benign if a defined group had zero catch.

# Human-readable crosswalk behind the UUIDs (names live on the VIEW)
cg_cols <- intersect(
  c("model_catch_group_id", "combined_catch_group",
    "species_name", "life_stage_name", "fin_mark_desc", "fate_name",
    "species", "life_stage", "fin_mark", "fate"),
  names(stratum_vw)
)
cg_label_cols <- setdiff(cg_cols, "model_catch_group_id")

stratum_vw |>
  distinct(across(all_of(cg_cols))) |>
  filter(!is.na(model_catch_group_id)) |>
  arrange(across(all_of(cg_label_cols)))

# Staleness: are these ids still defined in the database today?
current_cg <- creelutils::fishery_catchgroups(conn = con, fishery_name = pj$fishery_name)
setdiff(written_ids, current_cg$model_catch_group_id)
# Non-empty = a definition was edited or removed since this run. Re-run before
# treating these estimates as current.

# ---- 5. Local pre-export object vs what landed -----------------------------
# Requires `creel_estimates` from the render session — the output of
# transform_estimates(), i.e. BEFORE prep_export(). Skip ahead if not in scope.
#
# prep_export() is the ONLY thing between that object and the database:
#   total   : + model_catch_group_id  (joined est_cg = combined_catch_group)
#             - est_cg, project_name, fishery_name
#   stratum : the same, plus
#             + angler_type_id        (joined angler_final = angler_type_code)
#             - angler_final
#             NaN estimate_value coerced to 0
# Nothing else is added, dropped, filtered, or recomputed — the stratum
# diagnostic filter already happened inside transform_estimates(). So replaying
# those steps locally and diffing is a complete audit of the write. The database
# additionally attaches its surrogate PK and the four audit columns on insert.

cg_xwalk   <- run_cg |> select(combined_catch_group, model_catch_group_id)
angler_lut <- fetch_db_table(conn = con, "creel", "angler_type_lut") |>
  select(angler_type_code, angler_type_id)

local_total <- creel_estimates$total |>
  left_join(cg_xwalk, by = c("est_cg" = "combined_catch_group")) |>
  select(-any_of(c("est_cg", "project_name", "fishery_name")))

local_stratum <- creel_estimates$stratum |>
  left_join(cg_xwalk, by = c("est_cg" = "combined_catch_group")) |>
  left_join(angler_lut, by = c("angler_final" = "angler_type_code")) |>
  select(-any_of(c("est_cg", "project_name", "fishery_name", "angler_final"))) |>
  mutate(estimate_value = ifelse(is.nan(estimate_value), 0, estimate_value))

# 5a. Row counts must survive the export unchanged
tibble::tibble(
  table = c("total", "stratum"),
  local = c(nrow(creel_estimates$total), nrow(creel_estimates$stratum)),
  db    = c(nrow(total), nrow(stratum)),
  view  = c(nrow(total_vw), nrow(stratum_vw))
)
# Any inequality = join fan-out in prep_export(), or a partial write.

# 5b. Column mapping — the *_added lists should be the prep_export() additions
# plus the surrogate PK and the four database audit columns; nothing else.
list(
  total_added     = setdiff(names(total), names(creel_estimates$total)),
  total_dropped   = setdiff(names(creel_estimates$total), names(total)),
  stratum_added   = setdiff(names(stratum), names(creel_estimates$stratum)),
  stratum_dropped = setdiff(names(creel_estimates$stratum), names(stratum))
)

# 5c. Label resolution — every local est_cg should find an id
creel_estimates$total |>
  distinct(est_cg) |>
  left_join(cg_xwalk, by = c("est_cg" = "combined_catch_group")) |>
  mutate(expected_unkeyed = is.na(est_cg)) |>
  arrange(is.na(model_catch_group_id), est_cg)
# EXPECTED: one row with est_cg = NA and model_catch_group_id = NA. That is the
# effort rows, which carry no catch group by design (see 8b).
# FAILURE: any row with a non-NA est_cg and an NA id — the label built by
# combine_catch_group() did not match the database-built combined_catch_group.
# prep_export() aborts on this, so it should be impossible here, but it is
# precisely the failure the catch group migration exists to prevent.

# 5d. Value-level diff, keyed on every shared non-value column
key_total <- setdiff(intersect(names(local_total), names(total)), "estimate_value")
key_total

c(local_dupes = anyDuplicated(local_total[key_total]),
  db_dupes    = anyDuplicated(total[key_total]))
# Both 0 = the key is unique on each side and the join below is strictly 1:1.
# Non-zero means the grain is coarser than these columns describe; add the
# missing key column before trusting the diff.

diff_total <- full_join(local_total, total, by = key_total,
                        suffix = c("_local", "_db")) |>
  mutate(delta = estimate_value_db - estimate_value_local)

diff_total |>
  summarise(n_rows        = n(),
            only_in_db    = sum(is.na(estimate_value_local)),
            only_in_local = sum(is.na(estimate_value_db)),
            n_differing   = sum(abs(delta) > 1e-9, na.rm = TRUE),
            max_abs_delta = max(abs(delta), na.rm = TRUE))

diff_total |> filter(abs(delta) > 1e-9) |> print(n = 20)   # expect zero rows

key_stratum <- setdiff(intersect(names(local_stratum), names(stratum)), "estimate_value")

diff_stratum <- full_join(local_stratum, stratum, by = key_stratum,
                          suffix = c("_local", "_db")) |>
  mutate(delta = estimate_value_db - estimate_value_local)

diff_stratum |>
  summarise(n_rows        = n(),
            only_in_db    = sum(is.na(estimate_value_local)),
            only_in_local = sum(is.na(estimate_value_db)),
            n_differing   = sum(abs(delta) > 1e-9, na.rm = TRUE),
            max_abs_delta = max(abs(delta), na.rm = TRUE))

# How many stratum values the NaN -> 0 coercion silently rewrote
sum(is.nan(creel_estimates$stratum$estimate_value))

# 5e. Type drift across the write (Date vs POSIXct, integer vs numeric)
tibble::tibble(column = key_total,
               local  = purrr::map_chr(local_total[key_total], ~class(.x)[1]),
               db     = purrr::map_chr(total[key_total],       ~class(.x)[1])) |>
  filter(local != db)

# 5f. Three-way identity: local label -> id -> view names -> label again.
# The view returns fin_mark as codes (AD|UM|UNK), matching est_cg, so this is a
# hard equality check. Rebuilding with combine_catch_group() means the view and
# the run label are compared under the identical join rule used upstream.
# Needs the ETL 2.0 view column names; adjust the transmute if they change.
cg_rebuilt <- total_vw |>
  filter(!is.na(model_catch_group_id)) |>
  distinct(model_catch_group_id, species_name, life_stage_name,
           fin_mark_desc, fate_name) |>
  transmute(model_catch_group_id,
            species    = species_name,
            life_stage = life_stage_name,
            fin_mark   = fin_mark_desc,
            fate       = fate_name) |>
  mutate(rebuilt = creelutils::combine_catch_group(
    pick(species, life_stage, fin_mark, fate)
  ))

cg_identity <- run_cg |>
  left_join(select(cg_rebuilt, model_catch_group_id, rebuilt),
            by = "model_catch_group_id") |>
  mutate(matches = !is.na(rebuilt) & rebuilt == combined_catch_group)

cg_identity |> print(n = Inf)
# All TRUE = the label the run built, the id it wrote, and the components the
# view exposes are the same object. A FALSE here is the silent-zeroing failure
# mode: estimates compute fine but key to the wrong group.

# ---- 6. What the views add over the base tables ----------------------------

list(
  total_view_adds    = setdiff(names(total_vw),   names(total)),
  stratum_view_adds  = setdiff(names(stratum_vw), names(stratum)),
  total_view_drops   = setdiff(names(total),   names(total_vw)),
  stratum_view_drops = setdiff(names(stratum), names(stratum_vw))
)
# Expected drops: the surrogate PK, the four audit columns, and angler_type_id
# (replaced by angler_type_name on the stratum view).
# NOTE: the views currently expose `period_timestep` where the base tables carry
# `estimate_time_period`. The rename in transform_estimates() has not propagated
# to the view definitions — tracked in section 13.

names(stratum)   # base grain / key columns
names(stratum_vw)

# ---- 7. Vocabulary inventory: what was written, by model x category --------

stratum |>
  count(model_type, estimate_category, estimate_type) |>
  arrange(model_type, estimate_category, estimate_type) |>
  print(n = Inf)

total |>
  count(model_type, estimate_category, estimate_type) |>
  arrange(model_type, estimate_category, estimate_type) |>
  print(n = Inf)

# Anything outside the standardized set means a mapping was missed upstream
list(
  unstandardized_stratum = setdiff(unique(stratum$estimate_type), valid_types),
  unstandardized_total   = setdiff(unique(total$estimate_type),   valid_types)
)

list(
  stratum_categories = sort(unique(stratum$estimate_category)),  # catch, cpue, effort
  total_categories   = sort(unique(total$estimate_category))     # catch, effort
)

# Which estimate_types each model writes, side by side. PE and BSS currently
# share no point-estimate label at total scale — see section 13.
bind_rows(stratum = stratum, total = total, .id = "table") |>
  distinct(table, model_type, estimate_type) |>
  mutate(present = TRUE) |>
  pivot_wider(names_from = model_type, values_from = present, values_fill = FALSE) |>
  arrange(table, estimate_type) |>
  print(n = Inf)

# ---- 8. Grain and keying invariants ----------------------------------------

# 8a. Diagnostics must be absent from stratum, present on total (BSS runs)
list(
  leaked_into_stratum = intersect(unique(stratum$estimate_type), stratum_reject),
  present_on_total    = intersect(unique(total$estimate_type),   stratum_reject)
)

# 8b. model_catch_group_id: NA on effort, non-NA on catch/cpue
bind_rows(stratum = stratum, total = total, .id = "table") |>
  group_by(table, estimate_category) |>
  summarise(n_rows   = n(),
            n_groups = n_distinct(model_catch_group_id, na.rm = TRUE),
            pct_na   = round(100 * mean(is.na(model_catch_group_id))),
            .groups  = "drop")
# Expect pct_na = 100 for effort, 0 for catch and cpue.

# 8c. estimate_time_period should be period_pe for PE, period_bss for BSS
bind_rows(stratum = stratum, total = total, .id = "table") |>
  count(table, model_type, estimate_time_period)

# ---- 9. Model coverage per catch group -------------------------------------

stratum_vw |>
  filter(!is.na(model_catch_group_id)) |>
  distinct(across(all_of(cg_label_cols)), model_type) |>
  mutate(present = TRUE) |>
  pivot_wider(names_from = model_type, values_from = present, values_fill = FALSE)
# Gaps here mean one model didn't write for a group that the other did.

# ---- 10. Headline point estimates, PE vs BSS -------------------------------
# PE writes `total_catch`; BSS writes `mean` and quantiles and has no
# total_catch row, so a naive pivot on estimate_type silently returns PE only.
# Pull all three labels explicitly.

total_vw |>
  filter(estimate_category == "catch",
         (model_type == "PE"  & estimate_type == "total_catch") |
           (model_type == "BSS" & estimate_type %in% c("mean", "quantile_50"))) |>
  mutate(label = if_else(model_type == "PE", "PE_total",
                         paste0("BSS_", estimate_type))) |>
  select(all_of(cg_label_cols), label, estimate_value) |>
  pivot_wider(names_from = label, values_from = estimate_value)

total_vw |>
  filter(estimate_category == "effort",
         (model_type == "PE"  & estimate_type == "total_effort") |
           (model_type == "BSS" & estimate_type %in% c("mean", "quantile_50"))) |>
  mutate(label = if_else(model_type == "PE", "PE_total",
                         paste0("BSS_", estimate_type))) |>
  select(label, estimate_value) |>
  pivot_wider(names_from = label, values_from = estimate_value)

# BSS posterior shape tripwire.
# mean_vs_median far from 1 = skewed posterior. mean_above_upper is the harder
# failure: a posterior mean lying outside its own 97.5th percentile is not a
# usable point estimate at any chain length.
bss_skew <- total_vw |>
  filter(model_type == "BSS",
         estimate_type %in% c("mean", "quantile_50", "quantile_97_5")) |>
  select(all_of(cg_label_cols), estimate_category, estimate_type, estimate_value) |>
  pivot_wider(names_from = estimate_type, values_from = estimate_value) |>
  mutate(mean_vs_median   = mean / quantile_50,
         mean_above_upper = mean > quantile_97_5) |>
  arrange(desc(mean_vs_median))

bss_skew |> print(n = Inf)

# BSS convergence, total scale only. Short test chains will trip these; they are
# here to be read, not to gate a plumbing review.
total |>
  filter(estimate_type %in% c("r_hat", "n_eff", "n_div")) |>
  select(model_catch_group_id, estimate_category, estimate_type, estimate_value) |>
  pivot_wider(names_from = estimate_type, values_from = estimate_value)

# ---- 11. Value sanity ------------------------------------------------------

bind_rows(stratum = stratum, total = total, .id = "table") |>
  group_by(table, estimate_category, estimate_type) |>
  summarise(n     = n(),
            n_na  = sum(is.na(estimate_value)),
            n_nan = sum(is.nan(estimate_value)),
            n_neg = sum(estimate_value < 0, na.rm = TRUE),
            min   = min(estimate_value, na.rm = TRUE),
            max   = max(estimate_value, na.rm = TRUE),
            .groups = "drop") |>
  arrange(table, estimate_category, estimate_type) |>
  print(n = Inf)
# stratum n_nan must be 0 (prep_export coerces NaN -> 0). Negative catch or
# effort is never valid; negatives on daily_variance / diagnostics are.

# days_open cannot exceed the length of the estimate window. Currently expected
# to FAIL — process_estimates_pe() builds the total from a distinct() over
# period/day_type/est_cg/n_obs/N_days_open, which does not collapse sections
# whose n_obs differ, so section-level values are summed. number_observations
# is shown alongside because it is likely inflated by the same mechanism.
days_open_check <- total |>
  filter(estimate_type %in% c("days_open", "number_observations")) |>
  mutate(window_days   = as.numeric(max_event_date - min_event_date) + 1,
         within_window = estimate_value <= window_days) |>
  select(model_catch_group_id, estimate_category, estimate_type,
         estimate_value, window_days, within_window)

days_open_check |> print(n = Inf)

# ---- 12. Reviewer summary --------------------------------------------------
# All TRUE on a healthy upload. Known upstream issues live in section 13 so
# they do not dilute this signal.

tibble::tribble(
  ~check,                                ~pass,
  "lut is exactly one row",              nrow(lut) == 1L,
  "lut matches schema contract",         length(setdiff(lut_contract, names(lut))) == 0L &&
    length(setdiff(names(lut),
                   c(lut_contract, db_audit_cols))) == 0L,
  "git_sha captured",                    !is.na(lut$git_sha),
  "views match base row counts",         nrow(total_vw) == nrow(total) &&
    nrow(stratum_vw) == nrow(stratum),
  "no unstandardized estimate_types",    length(setdiff(unique(c(stratum$estimate_type,
                                                                 total$estimate_type)),
                                                        valid_types)) == 0L,
  "no diagnostics leaked to stratum",    length(intersect(unique(stratum$estimate_type),
                                                          stratum_reject)) == 0L,
  "all written ids were captured in run", length(setdiff(written_ids,
                                                         run_cg$model_catch_group_id)) == 0L,
  "written ids still defined in db",     length(setdiff(written_ids,
                                                        current_cg$model_catch_group_id)) == 0L,
  "label -> id -> view names round-trip", all(cg_identity$matches),
  "catch/cpue rows all keyed",           !any(is.na(bind_rows(stratum, total) |>
                                                      filter(estimate_category != "effort") |>
                                                      pull(model_catch_group_id))),
  "no NaN in stratum values",            sum(is.nan(stratum$estimate_value)) == 0L,
  "no negative catch or effort",         !any(bind_rows(stratum, total) |>
                                                filter(estimate_category %in% c("catch", "effort"),
                                                       estimate_type %in% c("total_catch", "total_effort")) |>
                                                pull(estimate_value) < 0, na.rm = TRUE),
  # the four below require section 5 to have run
  "row counts survived export",          nrow(creel_estimates$total) == nrow(total) &&
    nrow(creel_estimates$stratum) == nrow(stratum),
  "export join key is 1:1",              anyDuplicated(local_total[key_total]) == 0L &&
    anyDuplicated(local_stratum[key_stratum]) == 0L,
  "total values round-trip exactly",     sum(abs(diff_total$delta) > 1e-9, na.rm = TRUE) == 0L &&
    !any(is.na(diff_total$estimate_value_db)),
  "stratum values round-trip exactly",   sum(abs(diff_stratum$delta) > 1e-9, na.rm = TRUE) == 0L &&
    !any(is.na(diff_stratum$estimate_value_db))
)

# ---- 13. Known issues watchlist --------------------------------------------
# Expected FALSE today. Tracked here rather than in section 12 so that a
# reviewer can tell "this PR broke something" apart from "this was already
# broken". Delete a row once its fix lands.

tibble::tribble(
  ~watch,                                              ~resolved,
  
  "days_open <= estimate window length",
  all(days_open_check$within_window[days_open_check$estimate_type == "days_open"]),
  
  "BSS posterior mean within its own 97.5% quantile",
  !any(bss_skew$mean_above_upper, na.rm = TRUE),
  
  "BSS writes a shared point-estimate label",
  "total_catch" %in% (total |> filter(model_type == "BSS") |> pull(estimate_type)),
  
  "views expose estimate_time_period, not period_timestep",
  !"period_timestep" %in% c(names(total_vw), names(stratum_vw))
)
# days_open ......... process_estimates_pe() sums across sections
# BSS mean .......... fat-tailed posterior on sparse catch groups; also trips on
#                     deliberately short test chains
# shared label ...... consumers asking for "the total catch estimate" silently
#                     get PE only; needs a mapping or a coalescing view
# period_timestep ... view definitions still on the pre-rename column