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
#' fate (anglers keep AD fish and release UM fish, so a pooled rate would
#' misassign released UNK fish). `strata` lists grouping sets from most to least
#' specific. Each UNK row uses the first set whose matching stratum has at least
#' `min_known` known-mark fish. Rows that no set can resolve are left as UNK and
#' reported.
#'
#' **Mark-rate uncertainty.** With `rate_draw = "posterior"` (default), one mark
#' rate per stratum is drawn from Beta(n_UM + 1, n_AD + 1) before the binomial
#' draws, i.e., a proper single imputation. Mark-rate uncertainty is still NOT
#' carried into downstream credible intervals by a single run: imputed fish are
#' treated as observed. To propagate it, render M times with different seeds and
#' pool the draws. `rate_draw = "point"` uses n_UM / (n_UM + n_AD).
#'
#' **Assumption.** Within a stratum, UNK fish have the same mark rate as
#' known-mark fish (missing at random). Review this for released fish, which
#' may go unexamined for reasons related to mark status.
#'
#' **RNG.** When `seed` is supplied, the global RNG state is restored on exit,
#' so the seed does not change later random draws (e.g., Stan seeds).
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
#'   columns: any in `catch`, plus `section_num`, `event_date`, and `week`
#'   (Monday start of the week of `event_date`).
#' @param min_known Minimum known-mark fish (AD + UM) a stratum needs to be used.
#' @param rate_draw `"posterior"` or `"point"`; see Details.
#' @param seed Optional integer seed for reproducible reassignment.
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
      c("species", "fate", "life_stage"),
      c("species", "fate")
    ),
    min_known = 10,
    rate_draw = c("posterior", "point"),
    seed = NULL
) {
  rate_draw <- match.arg(rate_draw)

  # ---- Validate inputs ----
  need_catch <- c("interview_id", "species", "life_stage", "fin_mark", "fate", "fish_count")
  need_int   <- c("interview_id", "section_num", "event_date")
  miss_c <- setdiff(need_catch, names(catch))
  miss_i <- setdiff(need_int, names(interview))
  if (length(miss_c)) cli::cli_abort("{.arg catch} is missing column{?s} {.field {miss_c}}.")
  if (length(miss_i)) cli::cli_abort("{.arg interview} is missing column{?s} {.field {miss_i}}.")
  if (!is.list(strata) || !length(strata)) cli::cli_abort("{.arg strata} must be a non-empty list of character vectors.")
  if (!all(vapply(strata, function(s) all(c("species", "fate") %in% s), logical(1)))) {
    cli::cli_abort("Every set in {.arg strata} must include {.val species} and {.val fate}.")
  }
  if (!is.numeric(min_known) || length(min_known) != 1 || min_known < 1) {
    cli::cli_abort("{.arg min_known} must be a single number >= 1.")
  }

  # ---- RNG: seed locally, restore global state on exit ----
  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
    on.exit({
      if (had_seed) assign(".Random.seed", old_seed, envir = globalenv())
      else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
    }, add = TRUE)
    set.seed(seed)
  }

  # ---- Attach strata columns from interviews ----
  int_keys <- interview |>
    dplyr::distinct(interview_id, section_num, event_date)
  if (anyDuplicated(int_keys$interview_id)) {
    cli::cli_abort("{.arg interview} has interview_id values with more than one section_num/event_date.")
  }
  added_cols <- c("section_num", "event_date", "week")
  clash <- intersect(setdiff(added_cols, "event_date"), names(catch))
  if (length(clash)) cli::cli_abort("{.arg catch} already has column{?s} {.field {clash}}; rename before calling.")

  orig_cols <- names(catch)
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

  strata_cols <- unique(unlist(strata))
  bad_strata <- setdiff(strata_cols, names(cj))
  if (length(bad_strata)) cli::cli_abort("Unknown strata column{?s}: {.field {bad_strata}}.")

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

  # ---- Known-mark counts at each stratum level ----
  known <- cj[is_known, , drop = FALSE]
  rate_tables <- purrr::map(strata, function(s) {
    known |>
      dplyr::group_by(dplyr::across(dplyr::all_of(s))) |>
      dplyr::summarise(
        n_AD = sum(fish_count[fin_mark == ad_code], na.rm = TRUE),
        n_UM = sum(fish_count[fin_mark == um_code], na.rm = TRUE),
        .groups = "drop"
      )
  })

  # ---- Resolve each UNK row to the most specific usable stratum ----
  unk <- cj[is_unk, , drop = FALSE] |>
    dplyr::mutate(unk_rate_level = NA_integer_, unk_stratum = NA_character_,
                  n_AD = NA_real_, n_UM = NA_real_)

  for (k in seq_along(strata)) {
    todo <- which(is.na(unk$unk_rate_level))
    if (!length(todo)) break
    s <- strata[[k]]
    hit <- unk[todo, s, drop = FALSE] |>
      dplyr::left_join(rate_tables[[k]], by = s)  # dplyr matches NA keys to NA
    ok <- !is.na(hit$n_AD) & (hit$n_AD + hit$n_UM) >= min_known
    idx <- todo[ok]
    unk$unk_rate_level[idx] <- k
    unk$n_AD[idx] <- hit$n_AD[ok]
    unk$n_UM[idx] <- hit$n_UM[ok]
    unk$unk_stratum[idx] <- do.call(
      paste, c(lapply(unk[idx, s, drop = FALSE], as.character), sep = " | ")
    )
  }

  unresolved <- is.na(unk$unk_rate_level)
  if (any(unresolved)) {
    cli::cli_alert_warning(paste0(
      sum(unk$fish_count[unresolved]), " UNK fish in ", sum(unresolved),
      " row(s) had no stratum with >= ", min_known,
      " known-mark fish and were left as UNK."
    ))
  }

  # ---- One mark rate per stratum (shared by all rows in it), then binomial ----
  strata_used <- unk[!unresolved, , drop = FALSE] |>
    dplyr::distinct(unk_rate_level, unk_stratum, n_AD, n_UM)
  strata_used$unk_p_um <- if (rate_draw == "posterior") {
    stats::rbeta(nrow(strata_used), strata_used$n_UM + 1, strata_used$n_AD + 1)
  } else {
    strata_used$n_UM / (strata_used$n_AD + strata_used$n_UM)
  }

  res <- unk[!unresolved, , drop = FALSE] |>
    dplyr::left_join(
      strata_used |> dplyr::select(unk_rate_level, unk_stratum, unk_p_um),
      by = c("unk_rate_level", "unk_stratum")
    )
  res$n_to_um <- stats::rbinom(nrow(res), size = res$fish_count, prob = res$unk_p_um)
  res$n_to_ad <- res$fish_count - res$n_to_um

  # ---- Build the reassigned catch table ----
  split_rows <- dplyr::bind_rows(
    res |> dplyr::mutate(fin_mark_raw = fin_mark, fin_mark = um_code,
                         fish_count = n_to_um, .ord = 1L),
    res |> dplyr::mutate(fin_mark_raw = fin_mark, fin_mark = ad_code,
                         fish_count = n_to_ad, .ord = 2L)
  ) |>
    dplyr::filter(fish_count > 0) |>
    dplyr::mutate(mark_imputed = TRUE)

  kept_rows <- cj |>
    dplyr::filter(!(.row %in% res$.row)) |>
    dplyr::mutate(fin_mark_raw = fin_mark, mark_imputed = FALSE,
                  unk_p_um = NA_real_, unk_rate_level = NA_integer_,
                  unk_stratum = NA_character_, .ord = 0L)

  new_cols <- c("fin_mark_raw", "mark_imputed", "unk_p_um", "unk_rate_level", "unk_stratum")
  catch_out <- dplyr::bind_rows(kept_rows, split_rows) |>
    dplyr::arrange(.row, .ord) |>
    dplyr::select(dplyr::all_of(c(orig_cols, new_cols)))

  if ("catch_group" %in% orig_cols) {
    # mirror creelutils::fetch_data(), which builds catch_group with paste()
    catch_out$catch_group <- paste(catch_out$species, catch_out$life_stage,
                                   catch_out$fin_mark, catch_out$fate, sep = "_")
  }

  # ---- Check: fish totals are preserved ----
  tot_in  <- sum(catch$fish_count, na.rm = TRUE)
  tot_out <- sum(catch_out$fish_count, na.rm = TRUE)
  if (!isTRUE(all.equal(tot_in, tot_out))) {
    cli::cli_abort("Internal error: fish_count total changed ({tot_in} -> {tot_out}).")
  }

  cli::cli_alert_success(paste0(
    "Reassigned ", sum(res$fish_count), " UNK fish (", sum(res$n_to_um), " to ",
    um_code, ", ", sum(res$n_to_ad), " to ", ad_code, ") using ",
    nrow(strata_used), " strata."
  ))

  list(
    catch = catch_out,
    settings = list(
      species = species, unk_codes = unk_codes, ad_code = ad_code, um_code = um_code,
      strata = strata, min_known = min_known, rate_draw = rate_draw, seed = seed
    )
  )
}
