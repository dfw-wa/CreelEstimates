#' Reconcile and bound UNK fin-mark reassignment in the interview sample
#'
#' One row per species x life stage x fate that had UNK fish. Shows where every
#' sampled fish ended up, and the AD share of the sample under three cases:
#' all UNK fish as UM (lower bound), as reassigned, and all UNK as AD (upper
#' bound). Wide bounds mean the estimates are sensitive to the UNK assumption.
#' These are sample proportions, not expanded catch estimates.
#'
#' @param catch Output of `reassign_unk_marks()$catch`.
#' @param species Species to include; `NULL` includes every species with UNK fish.
#' @param unk_codes,ad_code,um_code Same as in [reassign_unk_marks()].
#'
#' @return A tibble, or `NULL` if there are no UNK fish.
summarise_unk_reassignment <- function(
    catch,
    species = NULL,
    unk_codes = "UNK",
    ad_code = "AD",
    um_code = "UM"
) {
  keep_species <- species %||% unique(catch$species)

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
      # denominator: every fish with a mark or UNK status, so the three shares are comparable
      n_total       = obs_AD + obs_UM + unk_raw,
      ad_share_low  = obs_AD / n_total,
      ad_share_imp  = (obs_AD + unk_to_AD) / n_total,
      ad_share_high = (obs_AD + unk_raw) / n_total
    ) |>
    dplyr::arrange(species, fate, life_stage)

  if (!nrow(out)) return(NULL)
  out
}
