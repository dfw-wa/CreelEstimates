#' Reassign unknown fin marks (UNK) to AD or UM before estimation
#'
#' Pre-processing step for interview catch data. Each catch row with an unknown
#' fin mark is split into AD and UM fish by a binomial draw, using the mark rate
#' observed among known-mark fish (AD + UM) in the most specific stratum that
#' has enough known-mark fish. Counts stay integers, so the reassigned data can
#' be passed straight to the PE and BSS pipelines.
#'
#' @details
#' **Strata and fallback.** Mark rates are always computed within species and
#' fate. In mark-selective fisheries kept fish are mostly AD and released fish
#' mostly UM, so a pooled rate would misassign UNK fish. In non-selective
#' fisheries the rates may be similar, but retention can still depend on mark
#' (voluntary release of wild fish, size, bag limits, mid-season rule changes),
#' so fate is kept as a stratum; it costs precision, not accuracy. `strata` lists grouping sets from most to least
#' specific. Each UNK row uses the first set whose matching stratum has at least
#' `min_known` interviews (or fish, see `min_known_unit`) with known-mark fish.
#' The pseudo-column `week_window` pools the row's week with `window_weeks`
#' weeks on either side, so sparse weeks borrow from neighbours before falling
#' back to the whole season. Rows that no set can resolve are left as UNK and
#' reported.
#'
#' **Sampling unit.** Fish caught by one angler group are not independent, so by
#' default `min_known` counts interviews, not fish.
#'
#' **Mark-rate uncertainty.** With `rate_draw = "posterior"` (default), one mark
#' rate per stratum is drawn from Beta(n_UM + prior[1], n_AD + prior[2]) before
#' the binomial draws, i.e., a proper single imputation. The default prior is
#' Jeffreys, Beta(0.5, 0.5); `c(1, 1)` is uniform. Mark-rate uncertainty is still
#' NOT carried into downstream credible intervals by a single run: imputed fish
#' are treated as observed. To propagate it, render M times with different seeds
#' and pool the draws. `rate_draw = "point"` uses n_UM / (n_UM + n_AD).
#'
#' **Kept fish.** In mark-selective fisheries, set `kept_unk_as_ad = TRUE` to
#' assign kept UNK fish to AD by regulation rather than from a mark rate
#' (`unk_rate_level` 0). Leave `FALSE` where UM fish may be retained.
#'
#' **Assumption.** Within a stratum, UNK fish have the same mark rate as
#' known-mark fish (missing at random). Review this for released fish, which
#' may go unexamined for reasons related to mark status.
#'
#' **RNG and record-level seeding.** Each random draw gets its own seed, built
#' from `seed` plus a stable key: the stratum for a mark-rate draw, and the
#' catch record (`record_id`, `catch_id` by default) for a binomial split. A
#' UNK record's split therefore depends only on its own record and its
#' stratum's known-mark counts, not on row order or on other records. Adding,
#' deleting or editing one record changes only that record's split, plus the
#' splits in any stratum whose known-mark counts changed; every other
#' assignment is unchanged. Changing `seed` gives a new, independent
#' imputation. The global RNG state is restored on exit, so the function does
#' not change later random draws (e.g., Stan seeds). If `seed` is `NULL`, a
#' seed is drawn at random and reported in `settings$seed`.
#'
#' **Run once.** Calling this on catch data that were already reassigned is an
#' error, because the audit columns would be overwritten. Start from the raw
#' catch (e.g., `inputs/dwg_raw.rds`) when re-running.
#'
#' @param catch Interview catch table (e.g., `dwg$catch`) with columns
#'   `interview_id`, `species`, `life_stage`, `fin_mark`, `fate`, `fish_count`.
#' @param interview Interview table (e.g., `dwg$interview`) with columns
#'   `interview_id`, `section_num`, `event_date`. Used only to attach strata.
#' @param species Character vector of species to reassign. `NULL` (default)
#'   reassigns UNK rows of every species that has known-mark fish.
#' @param unk_codes Values of `fin_mark` treated as unknown.
#' @param ad_code,um_code Values of `fin_mark` for adipose-clipped and unmarked.
#' @param strata List of character vectors, most to least specific. Available
#'   columns: any in `catch`, plus `section_num`, `event_date`, `week`
#'   (Monday start of the week of `event_date`), and `week_window` (see Details).
#' @param window_weeks Weeks on either side of the row's week pooled by `week_window`.
#' @param min_known Minimum sample a stratum needs before its rate is used.
#' @param min_known_unit `"interviews"` (default) counts interviews with
#'   known-mark fish; `"fish"` counts known-mark fish.
#' @param rate_draw `"posterior"` or `"point"`; see Details.
#' @param prior Beta prior shapes `c(UM, AD)` for `rate_draw = "posterior"`.
#' @param kept_unk_as_ad Assign kept UNK fish to AD by rule; see Details.
#' @param kept_code Value of `fate` for retained fish.
#' @param record_id Column in `catch` that uniquely identifies each catch
#'   record (`catch_id` from `creelutils::fetch_data()`). Used to key each
#'   record's random split. If the column is missing, a composite key
#'   (interview, species, life stage, fate, mark and order within the
#'   interview) is used instead, which is stable unless rows within an
#'   interview are reordered.
#' @param seed Integer seed for reproducible reassignment; see Details.
#'   `NULL` draws one at random.
#'
#' @return A list with
#'   * `catch`: `catch` with UNK rows split into AD/UM rows. Adds `fin_mark_raw`,
#'     `mark_imputed`, `unk_p_um`, `unk_rate_level`, `unk_stratum`. If
#'     `catch_group` exists it is rebuilt from the new `fin_mark`.
#'   * `settings`: arguments used.
#'
#' @examples
#' \dontrun{
#' unk <- reassign_unk_marks(dwg$catch, dwg$interview, species = "Chinook", seed = 1)
#' dwg$catch <- unk$catch
#' }
reassign_unk_marks <- function(
    catch,
    interview,
    species = NULL,
    unk_codes = "UNK",
    ad_code = "AD",
    um_code = "UM",
    strata = list(
      c("species", "fate", "life_stage", "section_num", "week"),
      c("species", "fate", "life_stage", "week"),
      c("species", "fate", "life_stage", "week_window"),
      c("species", "fate", "life_stage"),
      c("species", "fate")
    ),
    window_weeks = 1,
    min_known = 10,
    min_known_unit = c("interviews", "fish"),
    rate_draw = c("posterior", "point"),
    prior = c(0.5, 0.5),
    kept_unk_as_ad = FALSE,
    kept_code = "Kept",
    record_id = "catch_id",
    seed = NULL
) {
  rate_draw <- match.arg(rate_draw)
  min_known_unit <- match.arg(min_known_unit)
  new_cols  <- c("fin_mark_raw", "mark_imputed", "unk_p_um", "unk_rate_level", "unk_stratum")

  # ---- 1. Validate inputs ----
  need_catch <- c("interview_id", "species", "life_stage", "fin_mark", "fate", "fish_count")
  need_int   <- c("interview_id", "section_num", "event_date")
  miss_c <- setdiff(need_catch, names(catch))
  miss_i <- setdiff(need_int, names(interview))
  if (length(miss_c)) cli::cli_abort("{.arg catch} is missing column{?s} {.field {miss_c}}.")
  if (length(miss_i)) cli::cli_abort("{.arg interview} is missing column{?s} {.field {miss_i}}.")
  if (any(new_cols %in% names(catch))) {
    cli::cli_abort(c(
      "{.arg catch} has already been through reassign_unk_marks().",
      i = "Reload the raw catch (re-run the dwg_fetch and manual_edits chunks) before reassigning again."
    ))
  }
  if (!is.list(strata) || !length(strata)) cli::cli_abort("{.arg strata} must be a non-empty list of character vectors.")
  has_species_fate <- purrr::map_lgl(strata, \(s) all(c("species", "fate") %in% s))
  if (!all(has_species_fate)) {
    cli::cli_abort("Every set in {.arg strata} must include {.val species} and {.val fate}.")
  }
  if (!is.numeric(min_known) || length(min_known) != 1 || min_known < 1) {
    cli::cli_abort("{.arg min_known} must be a single number >= 1.")
  }
  if (!is.numeric(window_weeks) || length(window_weeks) != 1 || window_weeks < 0 || window_weeks %% 1 != 0) {
    cli::cli_abort("{.arg window_weeks} must be a single whole number >= 0.")
  }
  if (!is.numeric(prior) || length(prior) != 2 || any(prior <= 0)) {
    cli::cli_abort("{.arg prior} must be two positive numbers, c(UM, AD).")
  }

  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || is.na(seed))) {
    cli::cli_abort("{.arg seed} must be a single number or NULL.")
  }

  # ---- 2. Record-level seeding; the global RNG state is restored on exit ----
  # Each draw is seeded from `seed` plus a stable key (stratum or catch record),
  # so one record's split does not depend on row order or on other records.
  if (is.null(seed)) seed <- sample.int(.Machine$integer.max, 1L)
  old_seed <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (!is.null(old_seed)) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  # Deterministic string -> integer seed (polynomial hash mod 2^31 - 1, base R only)
  key_seed <- function(...) {
    keys <- paste(seed, ..., sep = "|")
    vapply(keys, function(k) {
      h <- 0
      for (b in utf8ToInt(k)) h <- (h * 31 + b) %% 2147483647
      as.integer(h)
    }, integer(1), USE.NAMES = FALSE)
  }

  # ---- 3. Attach section, date and week from the interview table ----
  int_keys <- interview |>
    dplyr::distinct(interview_id, section_num, event_date)
  if (anyDuplicated(int_keys$interview_id)) {
    cli::cli_abort("{.arg interview} has interview_id values with more than one section_num/event_date.")
  }
  clash <- intersect(c("section_num", "week"), names(catch))
  if (length(clash)) cli::cli_abort("{.arg catch} already has column{?s} {.field {clash}}; rename before calling.")

  orig_cols <- names(catch)
  # .row tracks each original catch row so split rows can be put back in order
  cj <- catch |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    dplyr::left_join(
      int_keys |> dplyr::rename(.section_num = section_num, .event_date = event_date),
      by = "interview_id"
    ) |>
    dplyr::mutate(
      section_num = .section_num,
      event_date  = if ("event_date" %in% orig_cols) event_date else .event_date,
      week        = lubridate::floor_date(as.Date(event_date), "week", week_start = 1)  # Monday start; NA-safe
    ) |>
    dplyr::select(-".section_num", -".event_date")

  # Stable key for each catch record, used to seed its binomial split
  if (!is.null(record_id) && record_id %in% orig_cols) {
    if (anyNA(catch[[record_id]]) || anyDuplicated(catch[[record_id]])) {
      cli::cli_abort("{.field {record_id}} must be unique and non-missing to key the random draws.")
    }
    cj$.key <- as.character(catch[[record_id]])
  } else {
    cli::cli_alert_warning(paste0(
      "No ", if (is.null(record_id)) "record_id" else record_id,
      " column in catch; keying draws on interview, species, ",
      "life stage, fate, mark and row order within the interview."
    ))
    cj <- cj |>
      dplyr::mutate(
        .key = paste(interview_id, species, life_stage, fate, fin_mark,
                     dplyr::row_number(), sep = "_"),
        .by = c(interview_id, species, life_stage, fate, fin_mark)
      )
  }

  strata_cols <- setdiff(unique(unlist(strata)), "week_window")
  bad_strata <- setdiff(strata_cols, names(cj))
  if (length(bad_strata)) cli::cli_abort("Unknown strata column{?s}: {.field {bad_strata}}.")

  # ---- 4. Flag the rows to reassign (UNK) and the rows that inform the rate ----
  # fin_mark NA is neither UNK nor known, so those rows are left untouched.
  in_species <- if (is.null(species)) rep(TRUE, nrow(cj)) else cj$species %in% species
  is_unk   <- cj$fin_mark %in% unk_codes & in_species
  is_known <- cj$fin_mark %in% c(ad_code, um_code) & in_species

  if (any(is_unk & is.na(cj$fish_count))) {
    cli::cli_alert_warning("UNK rows with missing fish_count are left unchanged.")
  }
  if (any(is_unk & !is.na(cj$fish_count) & cj$fish_count != round(cj$fish_count))) {
    cli::cli_abort("{.field fish_count} must be whole numbers for UNK rows.")
  }
  is_unk <- is_unk & !is.na(cj$fish_count) & cj$fish_count > 0

  # ---- 5. Count known-mark fish and interviews in every stratum, one table per level ----
  known <- cj[is_known, , drop = FALSE]
  count_known <- function(x, keys) {
    x |>
      dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
      dplyr::summarise(
        n_AD  = sum(fish_count[fin_mark == ad_code], na.rm = TRUE),
        n_UM  = sum(fish_count[fin_mark == um_code], na.rm = TRUE),
        n_int = dplyr::n_distinct(interview_id),
        .groups = "drop"
      )
  }
  # week_window is matched on week; each week's counts are added to every week within +/- window_weeks
  join_keys <- purrr::map(strata, \(s) replace(s, s == "week_window", "week"))
  rate_tables <- purrr::map2(strata, join_keys, function(s, keys) {
    if (!"week_window" %in% s) return(count_known(known, keys))
    count_known(dplyr::filter(known, !is.na(week)), keys) |>
      tidyr::expand_grid(.offset = -window_weeks:window_weeks) |>
      dplyr::mutate(week = week + 7 * .offset) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
      dplyr::summarise(dplyr::across(c(n_AD, n_UM, n_int), sum), .groups = "drop")
  })

  # ---- 6. Give each UNK row the most specific stratum with enough known-mark data ----
  # Rows still unassigned after level k move on to the coarser level k + 1.
  unk <- cj[is_unk, , drop = FALSE] |>
    dplyr::mutate(unk_rate_level = NA_integer_, unk_stratum = NA_character_,
                  n_AD = NA_real_, n_UM = NA_real_)

  # Level 0: kept UNK fish are AD by regulation, no rate needed
  kept_rule_label <- paste("Kept UNK ->", ad_code, "(rule)")
  if (kept_unk_as_ad) {
    is_kept <- unk$fate %in% kept_code
    unk$unk_rate_level[is_kept] <- 0L
    unk$unk_stratum[is_kept] <- kept_rule_label
  }

  for (k in seq_along(strata)) {
    todo <- which(is.na(unk$unk_rate_level))
    if (!length(todo)) break
    keys <- join_keys[[k]]
    hit <- unk[todo, keys, drop = FALSE] |>
      dplyr::left_join(rate_tables[[k]], by = keys)  # dplyr matches NA keys to NA
    n_basis <- if (min_known_unit == "interviews") hit$n_int else hit$n_AD + hit$n_UM
    # n_basis is NA when the stratum has no known-mark fish at all
    ok <- !is.na(n_basis) & n_basis >= min_known
    idx <- todo[ok]
    unk$unk_rate_level[idx] <- k
    unk$n_AD[idx] <- hit$n_AD[ok]
    unk$n_UM[idx] <- hit$n_UM[ok]
    unk$unk_stratum[idx] <- do.call(
      paste, c(lapply(unk[idx, keys, drop = FALSE], as.character), sep = " | ")
    )
    if ("week_window" %in% strata[[k]]) {
      unk$unk_stratum[idx] <- paste0(unk$unk_stratum[idx], " +/-", window_weeks, "wk")
    }
  }

  unresolved <- is.na(unk$unk_rate_level)
  if (any(unresolved)) {
    cli::cli_alert_warning(paste0(
      sum(unk$fish_count[unresolved]), " UNK fish in ", sum(unresolved),
      " row(s) had no stratum with >= ", min_known, " ", min_known_unit,
      " with known-mark fish and were left as UNK."
    ))
  }

  # ---- 7. Draw one mark rate per stratum, then split each UNK row binomially ----
  # All rows in a stratum share one rate draw, so they are imputed consistently.
  # Each stratum's draw is seeded from its own label, so it does not depend on
  # which other strata are in use.
  strata_used <- unk[!unresolved & unk$unk_rate_level > 0, , drop = FALSE] |>
    dplyr::distinct(unk_rate_level, unk_stratum, n_AD, n_UM)
  strata_used$unk_p_um <- if (rate_draw == "posterior") {
    rate_seeds <- key_seed("rate", strata_used$unk_rate_level, strata_used$unk_stratum)
    vapply(seq_len(nrow(strata_used)), function(i) {
      set.seed(rate_seeds[i])
      stats::rbeta(1, strata_used$n_UM[i] + prior[1], strata_used$n_AD[i] + prior[2])
    }, numeric(1))
  } else {
    strata_used$n_UM / (strata_used$n_AD + strata_used$n_UM)
  }
  rates <- dplyr::bind_rows(
    strata_used |> dplyr::select(unk_rate_level, unk_stratum, unk_p_um),
    if (kept_unk_as_ad) dplyr::tibble(unk_rate_level = 0L, unk_stratum = kept_rule_label, unk_p_um = 0)
  )

  res <- unk[!unresolved, , drop = FALSE] |>
    dplyr::left_join(rates, by = c("unk_rate_level", "unk_stratum"))
  # Each record's split is seeded from its own key (record_id), not its row position
  split_seeds <- key_seed("split", res$.key)
  res$n_to_um <- vapply(seq_len(nrow(res)), function(i) {
    set.seed(split_seeds[i])
    as.numeric(stats::rbinom(1, size = res$fish_count[i], prob = res$unk_p_um[i]))
  }, numeric(1))
  res$n_to_ad <- res$fish_count - res$n_to_um

  # ---- 8. Rebuild the catch table: each UNK row becomes up to two rows (UM, AD) ----
  # .ord keeps the original row first, then its UM and AD pieces; zero-count pieces are dropped.
  split_rows <- dplyr::bind_rows(
    res |> dplyr::mutate(fin_mark_raw = fin_mark, fin_mark = um_code,
                         fish_count = n_to_um, .ord = 1L),
    res |> dplyr::mutate(fin_mark_raw = fin_mark, fin_mark = ad_code,
                         fish_count = n_to_ad, .ord = 2L)
  ) |>
    dplyr::filter(fish_count > 0) |>
    dplyr::mutate(mark_imputed = TRUE)

  # Everything not split (known marks, unresolved UNK, other species) passes through unchanged
  pass_rows <- cj |>
    dplyr::filter(!(.row %in% res$.row)) |>
    dplyr::mutate(fin_mark_raw = fin_mark, mark_imputed = FALSE,
                  unk_p_um = NA_real_, unk_rate_level = NA_integer_,
                  unk_stratum = NA_character_, .ord = 0L)

  catch_out <- dplyr::bind_rows(pass_rows, split_rows) |>
    dplyr::arrange(.row, .ord) |>
    dplyr::select(dplyr::all_of(c(orig_cols, new_cols)))

  if ("catch_group" %in% orig_cols) {
    # mirror creelutils::fetch_data(), which builds catch_group with paste()
    catch_out$catch_group <- paste(catch_out$species, catch_out$life_stage,
                                   catch_out$fin_mark, catch_out$fate, sep = "_")
  }

  # ---- 9. Safety check: fish totals must not change within species x life stage x fate ----
  group_totals <- function(x) {
    x |>
      dplyr::summarise(n = sum(fish_count, na.rm = TRUE), .by = c(species, life_stage, fate)) |>
      dplyr::arrange(species, life_stage, fate)
  }
  if (!isTRUE(all.equal(as.data.frame(group_totals(catch)), as.data.frame(group_totals(catch_out)),
                        check.attributes = FALSE))) {
    cli::cli_abort("Internal error: fish_count totals changed within species/life_stage/fate.")
  }

  n_rule <- sum(res$fish_count[res$unk_rate_level == 0])
  cli::cli_alert_success(paste0(
    "Reassigned ", sum(res$fish_count), " UNK fish (", sum(res$n_to_um), " to ",
    um_code, ", ", sum(res$n_to_ad), " to ", ad_code, ") using ",
    nrow(strata_used), " strata",
    if (n_rule > 0) paste0("; ", n_rule, " kept fish set to ", ad_code, " by rule"), "."
  ))

  list(
    catch = catch_out,
    settings = list(
      species = species, unk_codes = unk_codes, ad_code = ad_code, um_code = um_code,
      strata = strata, window_weeks = window_weeks, min_known = min_known,
      min_known_unit = min_known_unit, rate_draw = rate_draw, prior = prior,
      kept_unk_as_ad = kept_unk_as_ad, kept_code = kept_code,
      record_id = record_id, seed = seed
    )
  )
}
