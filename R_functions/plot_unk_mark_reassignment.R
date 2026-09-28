#' Plot observed mark rates and UNK fin-mark reassignment by time and section
#'
#' Diagnostic for [reassign_unk_marks()]. For each species x fate that had UNK
#' fish, returns a two-panel figure faceted by section (rows) and life stage
#' (columns). Facets, time grain, and groups adapt to the data: a single section
#' or life stage is not faceted, and the x axis is weekly unless the data span
#' fewer than two weeks (then daily).
#'
#' * Top panel: observed mark rate (AD / (AD + UM), known-mark fish only; point
#'   size = known-mark sample size) and the mark rate after reassignment
#'   (includes UNK fish assigned to AD or UM).
#' * Bottom panel: fish counts by status: observed AD, observed UM, UNK
#'   reassigned to AD, UNK reassigned to UM, and UNK left unresolved.
#'
#' @param catch Reassigned catch table, i.e., `reassign_unk_marks(...)$catch`
#'   (needs `fin_mark_raw` and `mark_imputed`).
#' @param interview Interview table with `interview_id`, `section_num`, `event_date`.
#' @param species Species to plot; `NULL` plots every species with UNK fish.
#'   Use the same value passed to [reassign_unk_marks()].
#' @param unk_codes,ad_code,um_code Same as in [reassign_unk_marks()].
#'
#' @return A named list of patchwork objects (one per species x fate), with
#'   attribute `n_sections` (max number of section rows across groups), or
#'   `NULL` if there are no UNK fish to show.
plot_unk_mark_reassignment <- function(
    catch,
    interview,
    species = NULL,
    unk_codes = "UNK",
    ad_code = "AD",
    um_code = "UM"
) {
  # ---- 1. Validate input ----
  need <- c("interview_id", "species", "life_stage", "fate", "fin_mark",
            "fin_mark_raw", "mark_imputed", "fish_count")
  miss <- setdiff(need, names(catch))
  if (length(miss)) cli::cli_abort("{.arg catch} is missing column{?s} {.field {miss}}; pass the output of reassign_unk_marks().")

  # ---- 2. Legend order and colours: dark = observed, light = reassigned ----
  status_levels <- c("Observed AD", "Observed UM", "UNK -> AD", "UNK -> UM", "UNK (unresolved)")
  status_cols <- c(
    "Observed AD" = "#1b7837", "Observed UM" = "#762a83",
    "UNK -> AD" = "#a6dba0", "UNK -> UM" = "#c2a5cf", "UNK (unresolved)" = "grey60"
  )

  # ---- 3. Label each catch row with its section, date and mark status ----
  int_keys <- interview |>
    dplyr::distinct(interview_id, section_num, event_date) |>
    dplyr::rename(.section_num = section_num, .event_date = event_date)

  plot_species <- species %||% unique(catch$species)

  d <- catch |>
    dplyr::select(dplyr::all_of(need)) |>
    dplyr::filter(species %in% plot_species, !is.na(fish_count), fish_count > 0) |>
    dplyr::left_join(int_keys, by = "interview_id") |>
    dplyr::mutate(
      status = dplyr::case_when(
        mark_imputed & fin_mark == ad_code ~ "UNK -> AD",
        mark_imputed & fin_mark == um_code ~ "UNK -> UM",
        fin_mark %in% unk_codes            ~ "UNK (unresolved)",
        fin_mark == ad_code                ~ "Observed AD",
        fin_mark == um_code                ~ "Observed UM",
        TRUE                               ~ NA_character_   # e.g. fin_mark NA: not plotted
      ),
      section = paste("Section", .section_num)
    ) |>
    dplyr::filter(!is.na(status))

  # Rows without a matching interview date can't be placed in time; report them so totals reconcile
  no_date <- is.na(d$.event_date)
  if (any(no_date)) {
    cli::cli_alert_warning("{sum(d$fish_count[no_date])} fish in {sum(no_date)} catch row{?s} have no interview date and are not plotted.")
    d <- d[!no_date, , drop = FALSE]
  }

  # ---- 4. Keep only species x fate groups that had UNK fish ----
  groups <- d |>
    dplyr::filter(fin_mark_raw %in% unk_codes) |>
    dplyr::distinct(species, fate)
  if (!nrow(groups)) {
    cli::cli_alert_info("No UNK fish found; nothing to plot.")
    return(NULL)
  }
  d <- dplyr::semi_join(d, groups, by = c("species", "fate"))

  # ---- 5. Time grain: weekly bins, or daily if the data span < 2 weeks ----
  span_days <- as.numeric(diff(range(d$.event_date)))
  by_week <- span_days >= 13
  d <- d |>
    dplyr::mutate(
      time = if (by_week) lubridate::floor_date(.event_date, "week", week_start = 1) else as.Date(.event_date)
    )
  bar_width <- if (by_week) 5.5 else 0.9
  x_lab <- if (by_week) "Week (Monday start)" else "Date"

  # ---- 6. One two-panel figure per species x fate ----
  plots <- purrr::pmap(groups, function(species, fate) {
    g <- d |> dplyr::filter(.data$species == !!species, .data$fate == !!fate)

    # Fish per section x life stage x time bin x status (bottom panel)
    counts <- g |>
      dplyr::group_by(section, life_stage, time, status) |>
      dplyr::summarise(n = sum(fish_count), .groups = "drop") |>
      dplyr::mutate(status = factor(status, levels = status_levels))

    # AD share per cell (top panel). "Observed" uses known-mark fish only;
    # "Reassigned" adds imputed fish, whose rate may come from a coarser stratum.
    rates <- counts |>
      tidyr::pivot_wider(names_from = status, values_from = n, values_fill = 0,
                         names_expand = TRUE) |>
      dplyr::mutate(
        n_known    = `Observed AD` + `Observed UM`,
        n_all      = n_known + `UNK -> AD` + `UNK -> UM`,
        Observed   = dplyr::if_else(n_known > 0, `Observed AD` / n_known, NA_real_),
        Reassigned = dplyr::if_else(n_all > 0, (`Observed AD` + `UNK -> AD`) / n_all, NA_real_)
      ) |>
      dplyr::select(section, life_stage, time, n_known, Observed, Reassigned) |>
      tidyr::pivot_longer(c(Observed, Reassigned), names_to = "basis", values_to = "rate")

    # Shared x axis for both panels so bars and points line up
    x_scale <- ggplot2::scale_x_date(
      limits = range(counts$time) + c(-bar_width / 2, bar_width / 2),
      breaks = if (dplyr::n_distinct(counts$time) <= 10) sort(unique(counts$time)) else scales::breaks_pretty(8),
      date_labels = "%b %d", guide = ggplot2::guide_axis(check.overlap = TRUE)
    )

    # Facet only by dimensions that vary (sections = rows, life stages = columns).
    # Scales are fixed so panes can be compared directly.
    n_sec <- dplyr::n_distinct(counts$section)
    n_ls  <- dplyr::n_distinct(counts$life_stage)
    facet <- if (n_sec > 1 && n_ls > 1) {
      ggplot2::facet_grid(section ~ life_stage)
    } else if (n_sec > 1) {
      ggplot2::facet_grid(section ~ .)
    } else if (n_ls > 1) {
      ggplot2::facet_grid(. ~ life_stage)
    }

    p_rate <- ggplot2::ggplot(
      rates |> dplyr::filter(!is.na(rate)),
      ggplot2::aes(time, rate, colour = basis, linetype = basis)
    ) +
      ggplot2::geom_line(linewidth = 0.5, alpha = 0.7) +
      ggplot2::geom_point(
        data = ~ dplyr::filter(.x, basis == "Observed"),
        ggplot2::aes(size = n_known), alpha = 0.8
      ) +
      ggplot2::geom_point(
        data = ~ dplyr::filter(.x, basis == "Reassigned"), size = 1, shape = 17
      ) +
      facet +
      x_scale +
      ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      ggplot2::scale_colour_manual(values = c(Observed = "black", Reassigned = "#d95f02")) +
      ggplot2::scale_linetype_manual(values = c(Observed = "solid", Reassigned = "dashed")) +
      ggplot2::scale_size_area(max_size = 4, name = "Known-mark\nsample size") +
      ggplot2::labs(x = NULL, y = "Mark rate (AD share)", colour = NULL, linetype = NULL) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "right")

    p_n <- ggplot2::ggplot(counts, ggplot2::aes(time, n, fill = status)) +
      ggplot2::geom_col(width = bar_width) +
      facet +
      x_scale +
      ggplot2::scale_fill_manual(values = status_cols, drop = TRUE, name = "Fish status") +
      ggplot2::labs(x = x_lab, y = "Fish sampled (count)") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "right")

    (p_rate / p_n) +
      patchwork::plot_layout(heights = c(1, 1.2)) +
      patchwork::plot_annotation(
        title = paste(species, "-", fate),
        subtitle = "Observed mark rates and UNK fin marks reassigned to AD/UM"
      )
  })
  names(plots) <- paste(groups$species, groups$fate, sep = "_")

  attr(plots, "n_sections") <- max(1L, max(purrr::map_int(plots, \(p) {
    dplyr::n_distinct(p[[1]]$data$section)
  })))
  plots
}
