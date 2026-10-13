#' Reconcile and bound UNK fin-mark reassignment in the interview sample
#'
#' One row per species x life stage x fate that had UNK fish. Shows where every
#' sampled fish ended up, and the AD mark rate, AD / (AD + UM), under three
#' cases: all UNK fish as UM (lower bound), as reassigned (among fish with an
#' assigned mark), and all UNK as AD (upper bound). Wide bounds mean the
#' estimates are sensitive to the UNK assumption. These are sample proportions,
#' not expanded catch estimates.
#'
#' @param catch Output of `reassign_unk_marks()$catch`.
#' @param species Species to include; `NULL` includes every species with UNK fish.
#' @param catch_groups Optional data frame of selected catch groups; when given,
#'   only catch rows inside these groups are summarised (see [unk_in_catch_groups()]).
#' @param interview,date_start,date_end Optional. With all three, only catch from
#'   interviews inside the estimation window is summarised (see [catch_in_window()]),
#'   matching the reported-catch table.
#' @param unk_codes,ad_code,um_code Same as in [reassign_unk_marks()].
#'
#' @return A tibble, or `NULL` if there are no UNK fish.
summarise_unk_reassignment <- function(
    catch,
    species = NULL,
    catch_groups = NULL,
    interview = NULL,
    date_start = NULL,
    date_end = NULL,
    unk_codes = "UNK",
    ad_code = "AD",
    um_code = "UM"
) {
  keep_species <- species %||% unique(catch$species)
  if (!is.null(date_start) && is.null(interview)) {
    cli::cli_abort("{.arg interview} is required to apply an estimation window.")
  }
  if (!is.null(interview)) {
    catch <- catch[catch_in_window(catch, interview, date_start, date_end), , drop = FALSE]
  }
  if (!is.null(catch_groups)) {
    catch <- catch[unk_in_catch_groups(catch, catch_groups, unk_codes, ad_code, um_code), , drop = FALSE]
  }

  out <- catch |>
    dplyr::filter(species %in% keep_species, !is.na(fish_count)) |>
    dplyr::summarise(
      obs_AD     = sum(fish_count[!mark_imputed & fin_mark == ad_code]),
      obs_UM     = sum(fish_count[!mark_imputed & fin_mark == um_code]),
      unk_raw    = sum(fish_count[fin_mark_raw %in% unk_codes]),
      unk_to_AD  = sum(fish_count[mark_imputed & fin_mark == ad_code]),
      unk_to_UM  = sum(fish_count[mark_imputed & fin_mark == um_code]),
      unk_left   = sum(fish_count[fin_mark %in% unk_codes]),
      .by = c(species, life_stage, fate)
    ) |>
    dplyr::filter(unk_raw > 0) |>
    dplyr::mutate(
      n_total      = obs_AD + obs_UM + unk_raw,
      # bounds use every fish with a mark or UNK status; the reassigned rate uses fish with an assigned mark
      ad_rate_low  = obs_AD / n_total,
      ad_rate_imp  = dplyr::if_else(n_total > unk_left, (obs_AD + unk_to_AD) / (n_total - unk_left), NA_real_),
      ad_rate_high = (obs_AD + unk_raw) / n_total
    ) |>
    dplyr::arrange(species, fate, life_stage)

  if (!nrow(out)) return(NULL)
  out
}

#' Reshape the UNK summary into Known / UNK reassigned / Final rows
#'
#' Long layout of [summarise_unk_reassignment()] output: per species x life
#' stage x fate, one row for known-mark fish, one for reassigned UNK fish and
#' one for the final sample, so the final total matches the reported catch.
#' `ci_low`/`ci_high` are a 95% Wilson interval on the AD mark rate of
#' known-mark fish (sampling uncertainty), filled for the known row only.
#' `bound_low`/`bound_high` are the AD mark rate if all UNK fish were UM / AD
#' (a sensitivity range, not a statistical interval), filled for the final row
#' only. All four are `NA` elsewhere.
#'
#' @param summ Output of [summarise_unk_reassignment()].
#'
#' @return A tibble with `group`, `row`, `AD`, `UM`, `UNK`, `total`,
#'   `pct_sample`, `ad_rate`, `ci_low`, `ci_high`, `bound_low`, `bound_high`;
#'   `NULL` if `summ` is `NULL`.
format_unk_summary <- function(summ) {
  if (is.null(summ)) return(NULL)
  share <- \(a, b) dplyr::if_else(b > 0, a / b, NA_real_)
  n_known <- summ$obs_AD + summ$obs_UM
  n_imp   <- summ$unk_to_AD + summ$unk_to_UM
  ci <- lapply(seq_len(nrow(summ)), \(i) {
    if (n_known[i] > 0) suppressWarnings(stats::prop.test(summ$obs_AD[i], n_known[i], correct = FALSE)$conf.int) else c(NA_real_, NA_real_)
  })
  group <- paste(summ$species, summ$life_stage, summ$fate, sep = " \u00b7 ")
  n_all <- summ$n_total

  dplyr::bind_rows(
    dplyr::tibble(
      group, row = "Known mark (observed)", AD = summ$obs_AD, UM = summ$obs_UM, UNK = 0,
      total = n_known, ad_rate = share(summ$obs_AD, n_known),
      ci_low = purrr::map_dbl(ci, 1), ci_high = purrr::map_dbl(ci, 2),
      bound_low = NA_real_, bound_high = NA_real_, n_all
    ),
    dplyr::tibble(
      group, row = "UNK reassigned", AD = summ$unk_to_AD, UM = summ$unk_to_UM, UNK = summ$unk_left,
      total = summ$unk_raw, ad_rate = share(summ$unk_to_AD, n_imp),
      ci_low = NA_real_, ci_high = NA_real_, bound_low = NA_real_, bound_high = NA_real_, n_all
    ),
    dplyr::tibble(
      group, row = "Final sample", AD = summ$obs_AD + summ$unk_to_AD, UM = summ$obs_UM + summ$unk_to_UM,
      UNK = summ$unk_left, total = n_all, ad_rate = summ$ad_rate_imp,
      ci_low = NA_real_, ci_high = NA_real_,
      bound_low = summ$ad_rate_low, bound_high = summ$ad_rate_high, n_all
    ),
    .id = ".ord"
  ) |>
    dplyr::mutate(pct_sample = total / n_all, .g = match(group, unique(group))) |>
    dplyr::arrange(.g, .ord) |>
    dplyr::select(group, row, AD, UM, UNK, total, pct_sample, ad_rate, ci_low, ci_high, bound_low, bound_high)
}

#' Check that in-window fish totals are unchanged by reassignment
#'
#' Compares in-window fish totals by species x life stage x fate before and
#' after [reassign_unk_marks()], using the same window and interview match as
#' the reported-catch table. Aborts if any group differs. Warns about catch
#' rows with no interview date, which the reported-catch table silently drops.
#'
#' @param catch_before,catch_after Catch before and after reassignment.
#' @param interview Interview table with `interview_id`, `event_date`.
#' @param date_start,date_end Inclusive estimation window.
#'
#' @return Invisibly, a tibble of in-window totals (`before`, `after`) by group.
check_unk_window_totals <- function(catch_before, catch_after, interview, date_start, date_end) {
  totals <- function(x) {
    x[catch_in_window(x, interview, date_start, date_end), , drop = FALSE] |>
      dplyr::summarise(n = sum(fish_count, na.rm = TRUE), .by = c(species, life_stage, fate))
  }
  cmp <- dplyr::full_join(totals(catch_before), totals(catch_after),
                          by = c("species", "life_stage", "fate"), suffix = c("_before", "_after")) |>
    dplyr::mutate(dplyr::across(c(n_before, n_after), ~ tidyr::replace_na(.x, 0))) |>
    dplyr::rename(before = n_before, after = n_after) |>
    dplyr::arrange(species, life_stage, fate)

  bad <- cmp[cmp$before != cmp$after, , drop = FALSE]
  if (nrow(bad)) {
    cli::cli_abort(c(
      "In-window fish totals changed during UNK reassignment for {nrow(bad)} group{?s}.",
      i = "First mismatch: {bad$species[1]} {bad$life_stage[1]} {bad$fate[1]} ({bad$before[1]} before, {bad$after[1]} after)."
    ))
  }

  undated <- is.na(interview$event_date[match(catch_before$interview_id, interview$interview_id)])
  if (any(undated)) {
    cli::cli_alert_warning(
      "{sum(catch_before$fish_count[undated], na.rm = TRUE)} fish in {sum(undated)} catch row{?s} have no interview date and are excluded from the window."
    )
  }
  invisible(cmp)
}
