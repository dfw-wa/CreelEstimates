#' Drop catch groups emptied by UNK fin-mark reassignment
#'
#' After `reassign_unk_marks()`, a catch group defined on `fin_mark = "UNK"`
#' matches no fish, so PE and BSS would still estimate it (as 0). This finds
#' groups that matched fish in the estimation window before reassignment and
#' match none after, so they can be removed before estimation. Groups that were
#' already empty (e.g., a manually entered group with no catch) are kept.
#'
#' Matching mirrors `prep_dwg_interview_catch()`: each component of a catch
#' group is a regex applied with `str_detect()`, with NA treated as "NA".
#'
#' @param catch_before,catch_after Catch before and after reassignment.
#' @param interview Interview table with `interview_id` and `event_date`.
#' @param est_catch_groups Data frame with `species`, `life_stage`, `fin_mark`, `fate`.
#' @param date_start,date_end Estimation window (inclusive).
#'
#' @return A list with `kept` and `dropped` (rows of `est_catch_groups`).
drop_emptied_catch_groups <- function(
    catch_before,
    catch_after,
    interview,
    est_catch_groups,
    date_start,
    date_end
) {
  comps <- c("species", "life_stage", "fin_mark", "fate")

  in_window <- interview$interview_id[
    !is.na(interview$event_date) &
      interview$event_date >= as.Date(date_start) &
      interview$event_date <= as.Date(date_end)
  ]
  as_text <- function(x) {
    x |>
      dplyr::filter(interview_id %in% in_window) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(comps), ~ tidyr::replace_na(as.character(.x), "NA")))
  }
  before <- as_text(catch_before)
  after  <- as_text(catch_after)
  groups <- dplyr::mutate(
    est_catch_groups,
    dplyr::across(dplyr::all_of(comps), ~ tidyr::replace_na(as.character(.x), "NA"))
  )

  fish_matched <- function(catch, i) {
    hit <- stringr::str_detect(catch$species, groups$species[i]) &
      stringr::str_detect(catch$life_stage, groups$life_stage[i]) &
      stringr::str_detect(catch$fin_mark, groups$fin_mark[i]) &
      stringr::str_detect(catch$fate, groups$fate[i])
    sum(catch$fish_count[hit], na.rm = TRUE)
  }
  n_before <- vapply(seq_len(nrow(groups)), \(i) fish_matched(before, i), numeric(1))
  n_after  <- vapply(seq_len(nrow(groups)), \(i) fish_matched(after, i), numeric(1))
  emptied <- n_before > 0 & n_after == 0

  list(
    kept    = est_catch_groups[!emptied, , drop = FALSE],
    dropped = est_catch_groups[emptied, , drop = FALSE]
  )
}
