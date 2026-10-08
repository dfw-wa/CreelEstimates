# ==============================================================================
# Tests for R_functions/reassign_unk_marks.R
# Run from the project root:  testthat::test_file("tests/test-reassign_unk_marks.R")
# Uses synthetic data only; no database or network access.
# ==============================================================================
library(testthat)
library(dplyr)
source(here::here("R_functions", "reassign_unk_marks.R"))

make_fixture <- function() {
  # 2 sections x 2 weeks, one interview per section-day, 14 days
  dates <- seq(as.Date("2026-09-07"), by = "day", length.out = 14)  # Monday start
  interview <- tidyr::expand_grid(section_num = 1:2, event_date = dates) |>
    mutate(interview_id = row_number())

  set.seed(99)
  known <- interview |>
    slice_sample(n = 200, replace = TRUE) |>
    mutate(
      species = "Chinook",
      life_stage = sample(c("Adult", "Jack"), n(), replace = TRUE, prob = c(0.8, 0.2)),
      fate = sample(c("Released", "Kept"), n(), replace = TRUE, prob = c(0.7, 0.3)),
      # released fish mostly UM, kept fish mostly AD (selective fishery)
      fin_mark = ifelse(fate == "Released",
                        sample(c("UM", "AD"), n(), TRUE, c(0.8, 0.2)),
                        sample(c("UM", "AD"), n(), TRUE, c(0.05, 0.95))),
      fish_count = 1
    )
  unk <- interview |>
    slice_sample(n = 40, replace = TRUE) |>
    mutate(species = "Chinook", life_stage = "Adult", fate = "Released",
           fin_mark = "UNK", fish_count = sample(1:3, n(), TRUE))
  coho_unk <- tibble(interview_id = 1L, species = "Coho", life_stage = "Adult",
                     fate = "Released", fin_mark = "UNK", fish_count = 2)

  catch <- bind_rows(known, unk, coho_unk) |>
    select(interview_id, species, life_stage, fin_mark, fate, fish_count) |>
    mutate(catch_id = row_number(),
           catch_group = paste(species, life_stage, fin_mark, fate, sep = "_"))
  list(catch = catch, interview = interview)
}

test_that("fish totals are preserved and only UNK rows change", {
  fx <- make_fixture()
  out <- reassign_unk_marks(fx$catch, fx$interview, species = "Chinook", seed = 1)
  expect_equal(sum(out$catch$fish_count), sum(fx$catch$fish_count))
  # known-mark rows untouched
  known_in  <- fx$catch |> filter(fin_mark != "UNK") |> arrange(catch_id)
  known_out <- out$catch |> filter(!mark_imputed, fin_mark_raw != "UNK") |> arrange(catch_id)
  expect_equal(known_out$fish_count, known_in$fish_count)
  expect_equal(known_out$fin_mark, known_in$fin_mark)
  # every Chinook UNK fish reassigned; Coho left alone (species filter)
  expect_false(any(out$catch$species == "Chinook" & out$catch$fin_mark == "UNK"))
  expect_equal(sum(out$catch$fish_count[out$catch$species == "Coho" & out$catch$fin_mark == "UNK"]), 2)
  # per-row split sums back to the original UNK count
  split_totals <- out$catch |> filter(mark_imputed) |> count(catch_id, wt = fish_count)
  orig_unk <- fx$catch |> filter(species == "Chinook", fin_mark == "UNK") |> select(catch_id, fish_count)
  expect_equal(split_totals$n[order(split_totals$catch_id)], orig_unk$fish_count[order(orig_unk$catch_id)])
})

test_that("catch_group is rebuilt and audit columns are populated", {
  fx <- make_fixture()
  out <- reassign_unk_marks(fx$catch, fx$interview, species = "Chinook", seed = 1)
  expect_equal(out$catch$catch_group,
               paste(out$catch$species, out$catch$life_stage, out$catch$fin_mark, out$catch$fate, sep = "_"))
  imp <- out$catch |> filter(mark_imputed)
  expect_true(all(imp$fin_mark_raw == "UNK"))
  expect_true(all(imp$fin_mark %in% c("AD", "UM")))
  expect_true(all(!is.na(imp$unk_p_um) & !is.na(imp$unk_rate_level)))
})

test_that("seed gives reproducible results and does not change the global RNG", {
  fx <- make_fixture()
  set.seed(123); before <- runif(1)
  set.seed(123)
  a <- reassign_unk_marks(fx$catch, fx$interview, species = "Chinook", seed = 7)
  after <- runif(1)
  expect_equal(before, after)
  b <- reassign_unk_marks(fx$catch, fx$interview, species = "Chinook", seed = 7)
  expect_identical(a$catch, b$catch)
})

test_that("draws are keyed to records: removing or reordering records leaves other splits unchanged", {
  fx <- make_fixture()
  full <- reassign_unk_marks(fx$catch, fx$interview, species = "Chinook", seed = 7)$catch
  # Remove one UNK record; known-mark counts (and so every rate) are unchanged
  drop_id <- fx$catch |> filter(fin_mark == "UNK", species == "Chinook") |> slice(1) |> pull(catch_id)
  edited <- reassign_unk_marks(filter(fx$catch, catch_id != drop_id), fx$interview,
                               species = "Chinook", seed = 7)$catch
  # Reverse the row order
  shuffled <- reassign_unk_marks(arrange(fx$catch, desc(catch_id)), fx$interview,
                                 species = "Chinook", seed = 7)$catch
  key_cols <- function(x) x |> select(catch_id, fin_mark, fish_count, unk_p_um) |> arrange(catch_id, fin_mark)
  expect_equal(key_cols(filter(full, catch_id != drop_id)), key_cols(edited))
  expect_equal(key_cols(full), key_cols(shuffled))
  # A different seed still gives a different imputation
  other <- reassign_unk_marks(fx$catch, fx$interview, species = "Chinook", seed = 8)$catch
  expect_false(identical(key_cols(full), key_cols(other)))
})

test_that("a composite key is used when catch_id is missing, and record_id must be unique", {
  fx <- make_fixture()
  expect_message(
    out <- reassign_unk_marks(select(fx$catch, -catch_id), fx$interview, species = "Chinook", seed = 7),
    "catch_id"
  )
  expect_equal(sum(out$catch$fish_count), sum(fx$catch$fish_count))
  dup <- fx$catch |> mutate(catch_id = 1L)
  expect_error(reassign_unk_marks(dup, fx$interview, species = "Chinook", seed = 7), "unique")
})

test_that("falls back to coarser strata when fine strata are sparse", {
  fx <- make_fixture()
  out_fine   <- reassign_unk_marks(fx$catch, fx$interview, species = "Chinook", min_known = 1, seed = 1)
  out_coarse <- reassign_unk_marks(fx$catch, fx$interview, species = "Chinook", min_known = 1e3, seed = 1)
  expect_true(min(out_fine$catch$unk_rate_level, na.rm = TRUE) == 1)
  # nothing has 1000 known fish -> all Chinook UNK rows left unchanged
  expect_false(any(out_coarse$catch$mark_imputed))
  expect_equal(sum(out_coarse$catch$fish_count[out_coarse$catch$species == "Chinook" & out_coarse$catch$fin_mark == "UNK"]),
               sum(fx$catch$fish_count[fx$catch$species == "Chinook" & fx$catch$fin_mark == "UNK"]))
  out_mid <- reassign_unk_marks(fx$catch, fx$interview, species = "Chinook", min_known = 60, seed = 1)
  expect_true(all(out_mid$catch$unk_rate_level[out_mid$catch$mark_imputed] >= 2))  # section x week cells hold ~30 fish
})

test_that("rates are computed within fate and recover the simulated mark rate", {
  fx <- make_fixture()
  # large UNK released sample, point rates, pooled strata
  big <- fx$catch |> filter(fin_mark == "UNK", species == "Chinook") |> mutate(fish_count = 500)
  catch <- bind_rows(fx$catch |> filter(fin_mark != "UNK"), big)
  out <- reassign_unk_marks(catch, fx$interview, species = "Chinook",
                            strata = list(c("species", "fate")), rate_draw = "point", seed = 1)
  kn <- fx$catch |> filter(species == "Chinook", fate == "Released", fin_mark %in% c("AD", "UM"))
  p_hat <- sum(kn$fish_count[kn$fin_mark == "UM"]) / sum(kn$fish_count)
  imp <- out$catch |> filter(mark_imputed)
  expect_equal(length(unique(imp$unk_stratum)), 1)
  expect_equal(unique(imp$unk_p_um), p_hat)
  expect_equal(sum(imp$fish_count[imp$fin_mark == "UM"]) / sum(imp$fish_count), p_hat, tolerance = 0.02)
  expect_gt(p_hat, 0.6)  # released pool is mostly UM, not the pooled rate
})

test_that("no UNK rows is a no-op", {
  fx <- make_fixture()
  catch <- fx$catch |> filter(fin_mark != "UNK")
  out <- reassign_unk_marks(catch, fx$interview, seed = 1)
  expect_equal(nrow(out$catch), nrow(catch))
  expect_false(any(out$catch$mark_imputed))
})

test_that("input validation", {
  fx <- make_fixture()
  expect_error(reassign_unk_marks(select(fx$catch, -fate), fx$interview), "missing column")
  expect_error(reassign_unk_marks(fx$catch, fx$interview, strata = list(c("species", "week"))), "species")
  bad <- fx$catch; bad$fish_count[bad$fin_mark == "UNK"][1] <- 1.5
  expect_error(reassign_unk_marks(bad, fx$interview), "whole numbers")
  once <- reassign_unk_marks(fx$catch, fx$interview, seed = 1)
  expect_error(reassign_unk_marks(once$catch, fx$interview), "already been through")
})


# Coho = data rich (2 sections x 3 weeks x 2 life stages, many fish); Chinook = data poor (1 section, sparse)
make_rich_poor_fixture <- function() {
  dates <- seq(as.Date("2026-09-07"), by = "day", length.out = 21)
  interview <- tidyr::expand_grid(section_num = 1:2, event_date = dates) |>
    mutate(interview_id = row_number())
  set.seed(5)
  coho <- interview |>
    slice_sample(n = 600, replace = TRUE) |>
    mutate(species = "Coho", life_stage = sample(c("Adult", "Jack"), n(), TRUE, c(0.8, 0.2)),
           fate = "Released", fin_mark = sample(c("UM", "AD", "UNK"), n(), TRUE, c(0.55, 0.3, 0.15)),
           fish_count = sample(1:3, n(), TRUE))
  chin <- interview |>
    filter(section_num == 1, event_date < as.Date("2026-09-21")) |>
    slice_sample(n = 30) |>
    mutate(species = "Chinook", life_stage = "Adult", fate = "Released",
           fin_mark = sample(c("UM", "AD", "UNK"), n(), TRUE, c(0.3, 0.1, 0.6)), fish_count = 1)
  catch <- bind_rows(coho, chin) |>
    select(interview_id, species, life_stage, fin_mark, fate, fish_count) |>
    mutate(catch_id = row_number(), catch_group = paste(species, life_stage, fin_mark, fate, sep = "_"))
  list(catch = catch, interview = interview)
}

test_that("plot_unk_mark_reassignment adapts to data-rich (Coho) and data-poor (Chinook) species", {
  source(here::here("R_functions", "plot_unk_mark_reassignment.R"))
  library(patchwork)
  fx <- make_rich_poor_fixture()
  out <- reassign_unk_marks(fx$catch, fx$interview, seed = 1)
  plots <- plot_unk_mark_reassignment(out$catch, fx$interview)
  expect_setequal(names(plots), c("Coho_Released", "Chinook_Released"))
  expect_equal(attr(plots, "n_sections"), 2)
  only <- plot_unk_mark_reassignment(out$catch, fx$interview, species = "Coho")
  expect_equal(names(only), "Coho_Released")
  for (nm in names(plots)) {
    expect_no_error(ggplot2::ggsave(tempfile(fileext = ".png"), plots[[nm]], width = 10, height = 9))
  }
  # Chinook has one section and one life stage: no facet rows
  expect_equal(dplyr::n_distinct(plots[["Chinook_Released"]][[1]]$data$section), 1)
  expect_null(plot_unk_mark_reassignment(dplyr::filter(out$catch, FALSE), fx$interview))
})

# Small hand-built data: one section, weeks of 2026-09-07 / 14 / 21
tiny <- function(known_rows, unk_rows) {
  rows <- bind_rows(known_rows, unk_rows)
  interview <- distinct(rows, interview_id, event_date) |> mutate(section_num = 1L)
  catch <- rows |> mutate(species = "Coho", life_stage = "Adult") |> select(-event_date)
  list(catch = catch, interview = interview)
}
known_in_week <- function(start, n_int, fish_each, id0, mark = "UM", fate = "Released") {
  tibble(interview_id = id0 + seq_len(n_int), event_date = as.Date(start),
         fin_mark = mark, fate = fate, fish_count = fish_each)
}

test_that("min_known counts interviews by default, fish on request", {
  fx <- tiny(known_in_week("2026-09-07", 1, 12, 0),
             tibble(interview_id = 100L, event_date = as.Date("2026-09-08"), fin_mark = "UNK", fate = "Released", fish_count = 2))
  by_int  <- reassign_unk_marks(fx$catch, fx$interview, seed = 1)
  by_fish <- reassign_unk_marks(fx$catch, fx$interview, seed = 1, min_known_unit = "fish")
  expect_false(any(by_int$catch$mark_imputed))          # 1 interview < 10
  expect_true(all(by_fish$catch$unk_rate_level[by_fish$catch$mark_imputed] == 1))
})

test_that("sparse weeks borrow from adjacent weeks before the season rate", {
  known <- bind_rows(
    known_in_week("2026-09-07", 6, 1, 0,  "UM"),
    known_in_week("2026-09-14", 2, 1, 10, "AD"),
    known_in_week("2026-09-21", 6, 1, 20, "UM"),
    known_in_week("2026-10-26", 20, 1, 40, "AD")        # far-away weeks: season fallback would pull toward AD
  )
  fx <- tiny(known, tibble(interview_id = 100L, event_date = as.Date("2026-09-15"),
                           fin_mark = "UNK", fate = "Released", fish_count = 3))
  out <- reassign_unk_marks(fx$catch, fx$interview, seed = 1, rate_draw = "point")
  imp <- filter(out$catch, mark_imputed)
  expect_true(all(imp$unk_rate_level == 3))
  expect_match(imp$unk_stratum[1], "2026-09-14 \\+/-1wk")
  expect_equal(unique(imp$unk_p_um), 12 / 14)           # 12 UM, 2 AD across the 3-week window
  none <- reassign_unk_marks(fx$catch, fx$interview, seed = 1, rate_draw = "point", window_weeks = 0)
  expect_true(all(filter(none$catch, mark_imputed)$unk_rate_level == 4))
})

test_that("Jeffreys prior is the default and prior is validated", {
  fx <- tiny(known_in_week("2026-09-07", 10, 1, 0, "AD"),
             tibble(interview_id = 100L, event_date = as.Date("2026-09-08"), fin_mark = "UNK", fate = "Released", fish_count = 1))
  draw_p <- function(prior) purrr::map_dbl(1:300, \(s) {
    suppressMessages(reassign_unk_marks(fx$catch, fx$interview, seed = s, prior = prior))$catch |>
      filter(mark_imputed) |> pull(unk_p_um)
  })
  expect_equal(mean(draw_p(c(0.5, 0.5))), 0.5 / 11, tolerance = 0.25)
  expect_equal(mean(draw_p(c(1, 1))), 1 / 12, tolerance = 0.25)
  expect_error(reassign_unk_marks(fx$catch, fx$interview, prior = c(0, 1)), "positive")
})

test_that("kept UNK fish go to AD by rule only when requested", {
  known <- bind_rows(known_in_week("2026-09-07", 10, 1, 0, "UM", "Kept"),
                     known_in_week("2026-09-07", 10, 1, 20, "UM", "Released"))
  unk <- tibble(interview_id = 100:101, event_date = as.Date("2026-09-08"), fin_mark = "UNK",
                fate = c("Kept", "Released"), fish_count = 5)
  fx <- tiny(known, unk)
  out <- reassign_unk_marks(fx$catch, fx$interview, seed = 1, kept_unk_as_ad = TRUE)
  kept <- filter(out$catch, mark_imputed, fate == "Kept")
  expect_equal(unique(kept$fin_mark), "AD")
  expect_equal(unique(kept$unk_rate_level), 0L)
  expect_equal(sum(kept$fish_count), 5)
  expect_true(all(filter(out$catch, mark_imputed, fate == "Released")$unk_rate_level > 0))
  off <- reassign_unk_marks(fx$catch, fx$interview, seed = 1)
  expect_true(all(filter(off$catch, mark_imputed)$unk_rate_level > 0))
})

test_that("summarise_unk_reassignment reconciles fish and orders the bounds", {
  source(here::here("R_functions", "summarise_unk_reassignment.R"))
  fx <- make_rich_poor_fixture()
  out <- suppressMessages(reassign_unk_marks(fx$catch, fx$interview, seed = 1))
  s <- summarise_unk_reassignment(out$catch)
  expect_equal(s$unk_raw, s$unk_to_AD + s$unk_to_UM + s$unk_left)
  expect_equal(sum(s$n_total), sum(fx$catch$fish_count))
  expect_true(all(s$ad_share_low <= s$ad_share_imp & s$ad_share_imp <= s$ad_share_high))
  expect_equal(unique(summarise_unk_reassignment(out$catch, species = "Coho")$species), "Coho")
})

test_that("drop_emptied_catch_groups removes only groups that reassignment emptied", {
  source(here::here("R_functions", "drop_emptied_catch_groups.R"))
  fx <- make_fixture()
  out <- reassign_unk_marks(fx$catch, fx$interview, species = "Chinook", seed = 1)
  grp <- function(...) data.frame(species = c(...)[1], life_stage = c(...)[2], fin_mark = c(...)[3], fate = c(...)[4])
  cg <- rbind(
    grp("Chinook", "Adult", "UNK", "Released"),     # emptied
    grp("Chinook", "Adult", "UM", "Released"),      # gains fish
    grp("Chinook", "Adult", "UM|UNK", "Released"),  # regex still matches fish
    grp("Coho", "Adult", "UNK", "Released"),        # UNK left (species not reassigned)
    grp("Steelhead", "Adult", "UM", "Kept")         # empty before and after
  )
  win <- c(min(fx$interview$event_date), max(fx$interview$event_date))
  res <- drop_emptied_catch_groups(fx$catch, out$catch, fx$interview, cg, win[1], win[2])
  expect_equal(res$dropped$fin_mark, "UNK")
  expect_equal(res$dropped$species, "Chinook")
  expect_equal(nrow(res$kept), 4)
  # UNK fish only outside the window do not count as emptied
  early <- drop_emptied_catch_groups(fx$catch, out$catch, fx$interview, cg, win[1] - 30, win[1] - 20)
  expect_equal(nrow(early$dropped), 0)
  # reassignment off: catch unchanged, nothing dropped
  none <- drop_emptied_catch_groups(fx$catch, fx$catch, fx$interview, cg, win[1], win[2])
  expect_equal(nrow(none$dropped), 0)
})

test_that("catch_groups limits reporting only: scope helper, summary, plots and warning", {
  source(here::here("R_functions", "summarise_unk_reassignment.R"))
  source(here::here("R_functions", "plot_unk_mark_reassignment.R"))
  library(patchwork)
  fx <- make_fixture()
  grp <- function(sp, ls, fm, fa) data.frame(species = sp, life_stage = ls, fin_mark = fm, fate = fa)
  cg <- grp("Chinook", "Adult", "UM", "Released")

  # helper: species/life stage/fate must match; mark pattern must match AD, UM or UNK
  hit <- unk_in_catch_groups(fx$catch, cg)
  expect_true(all(fx$catch$species[hit] == "Chinook" & fx$catch$life_stage[hit] == "Adult" & fx$catch$fate[hit] == "Released"))
  expect_equal(sum(hit), sum(fx$catch$species == "Chinook" & fx$catch$life_stage == "Adult" & fx$catch$fate == "Released"))
  expect_false(any(unk_in_catch_groups(fx$catch, grp("Chinook", "Adult", "XX", "Released"))))
  expect_true(all(unk_in_catch_groups(fx$catch, grp("Chinook|Coho", "Adult", "UM|UNK", "Released"))[fx$catch$species == "Coho"]))

  # reassignment itself is unchanged by catch_groups
  a <- reassign_unk_marks(fx$catch, fx$interview, seed = 7)
  b <- reassign_unk_marks(fx$catch, fx$interview, seed = 7, catch_groups = cg)
  expect_identical(a$catch, b$catch)

  # summary and plots only show the selected catch groups
  s_all <- summarise_unk_reassignment(a$catch)
  s_cg  <- summarise_unk_reassignment(a$catch, catch_groups = cg)
  expect_true(nrow(s_all) > nrow(s_cg))
  expect_equal(s_cg$species, "Chinook")
  expect_equal(s_cg$life_stage, "Adult")
  expect_equal(s_cg$fate, "Released")
  p_cg <- plot_unk_mark_reassignment(a$catch, fx$interview, catch_groups = cg)
  expect_equal(names(p_cg), "Chinook_Released")

  # the left-as-UNK warning only counts fish inside the selected groups
  msgs_for <- function(groups) {
    msgs <- character()
    withCallingHandlers(
      reassign_unk_marks(fx$catch, fx$interview, min_known = 1000, seed = 7, catch_groups = groups),
      message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }
    )
    msgs
  }
  expect_true(any(grepl("left as UNK", msgs_for(cg))))
  expect_false(any(grepl("left as UNK", msgs_for(grp("Steelhead", "Adult", "UM", "Released")))))
})
