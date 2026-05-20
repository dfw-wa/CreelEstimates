generate_ppc_plots_v2 <- function(bss_fit, inputs_bss, ecg, dwg, params) {
  
  extracted_data <- extract(bss_fit)
  ecg_inputs     <- inputs_bss[[ecg]]
  
  # Parameter configuration lookup. Each entry encodes everything needed to
  # build PPC data and plots for one _rep parameter. Extend as model evolves.
  # NOTE: B_s_rep index column names (section_B, day_B, countnum_B) are assumed
  # by analogy with other parameters — verify these match your inputs_bss structure.
  param_config <- list(
    V_I_rep = list(
      obs_name     = "V_I",
      index_cols   = c(section = "section_V", day = "day_V", countnum = "countnum_V"),
      has_section  = TRUE,
      has_gear     = FALSE,
      summary_grps = "section",
      x_label      = "Observed Vehicle Counts",
      plot_title   = "Vehicle Counts PPC",
      plus_one     = FALSE
    ),
    T_I_rep = list(
      obs_name     = "T_I",
      index_cols   = c(section = "section_T", day = "day_T", countnum = "countnum_T"),
      has_section  = TRUE,
      has_gear     = FALSE,
      summary_grps = "section",
      x_label      = "Observed Trailer Counts",
      plot_title   = "Trailer Counts PPC",
      plus_one     = FALSE
    ),
    A_I_rep = list(
      obs_name     = "A_I",
      index_cols   = c(section = "section_A", day = "day_A", countnum = "countnum_A"),
      has_section  = TRUE,
      has_gear     = FALSE,
      summary_grps = "section",
      x_label      = "Observed Angler Index Counts",
      plot_title   = "Angler Index Counts PPC",
      plus_one     = FALSE
    ),
    B_s_rep = list(
      obs_name     = "B_s",
      index_cols   = c(section = "section_B", day = "day_B", countnum = "countnum_B"),
      has_section  = TRUE,
      has_gear     = FALSE,
      summary_grps = "section",
      x_label      = "Observed Boat Census Counts",
      plot_title   = "Boat Census Counts PPC (Drano)",
      plus_one     = FALSE
    ),
    E_s_rep = list(
      obs_name     = "E_s",
      index_cols   = c(section = "section_E", day = "day_E", gear = "gear_E", countnum = "countnum_E"),
      has_section  = TRUE,
      has_gear     = TRUE,
      summary_grps = c("section", "gear"),
      x_label      = "Observed Angler Counts",
      plot_title   = "Angler Census Counts PPC",
      plus_one     = FALSE
    ),
    c_rep = list(
      obs_name     = "c",
      index_cols   = c(section = "section_IntC", day = "day_IntC", gear = "gear_IntC"),
      has_section  = TRUE,
      has_gear     = TRUE,
      summary_grps = c("section", "gear"),
      x_label      = "Observed Catch",
      plot_title   = "Catch PPC",
      plus_one     = TRUE   # +1 offset for display
    ),
    V_A_rep = list(
      obs_name     = "V_A",
      index_cols   = c(gear = "gear_IntA"),
      has_section  = FALSE,
      has_gear     = TRUE,
      summary_grps = "gear",
      x_label      = "Observed Angler Group Vehicles",
      plot_title   = "Angler Group Vehicles PPC",
      plus_one     = FALSE
    ),
    T_A_rep = list(
      obs_name     = "T_A",
      index_cols   = c(gear = "gear_IntA"),
      has_section  = FALSE,
      has_gear     = TRUE,
      summary_grps = "gear",
      x_label      = "Observed Angler Group Trailers",
      plot_title   = "Angler Group Trailers PPC",
      plus_one     = FALSE
    ),
    B_A_rep = list(
      obs_name     = "B_A",
      index_cols   = c(gear = "gear_IntA"),
      has_section  = FALSE,
      has_gear     = TRUE,
      summary_grps = "gear",
      x_label      = "Observed Boat Interview Expansions",
      plot_title   = "Boat Interview Expansions PPC (Drano)",
      plus_one     = FALSE
    )
  )
  
  # Retain only parameters present in both the fit and the input data
  # Retain only parameters present in both the fit and the input data,
  # with non-empty observations and draws
  active_params <- param_config[
    names(param_config) %in% names(extracted_data) &
      map_lgl(param_config, \(cfg) cfg$obs_name %in% names(ecg_inputs)) &
      map_lgl(param_config, \(cfg) {
        obs_vals <- ecg_inputs[[cfg$obs_name]]
        !is.null(obs_vals) && length(obs_vals) > 0 && sum(obs_vals) > 0
      }) &
      map_lgl(names(param_config), \(rep_name) {
        rep_mat <- extracted_data[[rep_name]]
        !is.null(rep_mat) && length(rep_mat) > 0
      })
  ]
  
  if (length(active_params) == 0) {
    warning(sprintf(
      "No recognized PPC parameters found for catch group '%s'. Check that _rep arrays and observed inputs are named consistently.",
      ecg
    ))
    return(NULL)
  }
  
  # Check for missing/NaN values across all active parameters
  missing_params <- names(active_params)[
    map_lgl(names(active_params), \(rep_name)
            any(is.na(extracted_data[[rep_name]])) || any(is.nan(extracted_data[[rep_name]]))
    )
  ]
  
  if (length(missing_params) > 0) {
    warning(sprintf(
      paste0(
        "\n=== MISSING VALUES DETECTED ===\n",
        "Catch group: %s\n",
        "Parameters with missing/NaN values: %s\n",
        "Skipping PPC plot generation for this catch group.\n",
        "RECOMMENDATION: Check convergence diagnostics (Rhat, ESS, trace plots) for this model fit.\n",
        "Missing values may indicate:\n",
        "  - Model convergence issues\n",
        "  - Numerical instability in sampling\n",
        "  - Problematic parameter combinations\n",
        "================================\n"
      ),
      ecg,
      paste(missing_params, collapse = ", ")
    ))
    return(NULL)
  }
  
  # Section reference table
  sections <- dwg$effort |>
    filter(location_type == "Section") |>
    distinct(water_body, section_num) |>
    arrange(section_num) |>
    rename(section = section_num)
  
  # Helper: p_fit summary with totals row
  add_summary_totals <- function(data, group_vars) {
    group_vars <- intersect(group_vars, names(data))  # guard against missing cols
    summary_data <- data |>
      group_by(across(all_of(group_vars))) |>
      summarise(
        fit_count    = sum(fit == "fit"),
        no_fit_count = sum(fit == "no fit"),
        total_count  = fit_count + no_fit_count,
        p_fit        = if_else(no_fit_count == 0, 1, fit_count / total_count),
        .groups = "drop"
      )
    total_row <- summary_data |>
      summarise(
        across(all_of(group_vars), ~"Total"),
        fit_count    = sum(fit_count),
        no_fit_count = sum(no_fit_count),
        total_count  = sum(total_count),
        p_fit        = if_else(sum(no_fit_count) == 0, 1, sum(fit_count) / sum(total_count))
      )
    bind_rows(summary_data, total_row)
  }
  
  # Helper: decode gear integer to label
  decode_gear <- function(x) {
    as.factor(case_match(
      x,
      1 ~ "Bank Anglers",
      2 ~ "Boat Anglers",
      .default = as.character(x)
    ))
  }
  
  # Helper: build PPC data tibble for one parameter
  # NOTE: imap passes (value, name) so cfg comes first, rep_name second
  build_ppc_data <- function(cfg, rep_name) {
    bounds <- extracted_data[[rep_name]] |>
      apply(2, \(x) quantile(x, c(0.025, 0.975))) |>
      t() |>
      as_tibble(.name_repair = ~ paste0("V", seq_along(.))) |>
      rename(`2.5%` = 1, `97.5%` = 2)
    
    # Observed values + index columns from inputs_bss
    obs_data <- tibble(!!cfg$obs_name := ecg_inputs[[cfg$obs_name]])
    for (std_name in names(cfg$index_cols)) {
      input_col <- cfg$index_cols[[std_name]]
      if (input_col %in% names(ecg_inputs)) {
        obs_data[[std_name]] <- ecg_inputs[[input_col]]
      }
    }
    
    ppc_data <- bind_cols(bounds, obs_data)
    
    # Join section water body labels
    if (cfg$has_section && "section" %in% names(ppc_data)) {
      ppc_data <- ppc_data |>
        left_join(sections, by = "section") |>
        mutate(section = as.factor(water_body))
    }
    
    # Standard factor conversions
    ppc_data <- ppc_data |>
      mutate(
        across(any_of(c("day", "countnum")), as.factor),
        across(any_of("gear"), decode_gear)
      )
    
    # Fit flag
    ppc_data |>
      mutate(
        fit = factor(
          if_else(
            .data[[cfg$obs_name]] >= `2.5%` & .data[[cfg$obs_name]] <= `97.5%`,
            "fit", "no fit"
          ),
          levels = c("fit", "no fit")
        )
      )
  }
  
  # Helper: build PPC plot
  build_ppc_plot <- function(ppc_data, cfg) {
    offset   <- if (isTRUE(cfg$plus_one)) 1 else 0
    jitter_w <- if (cfg$has_section) 0.5 else 0.2
    
    p <- ggplot(ppc_data, aes(
      x     = .data[[cfg$obs_name]] + offset,
      y     = .data[[cfg$obs_name]] + offset,
      color = fit
    )) +
      geom_errorbar(
        aes(ymin = `2.5%` + offset, ymax = `97.5%` + offset),
        width    = 0,
        position = position_jitter(width = jitter_w, height = 0)
      ) +
      geom_line(color = "black") +
      labs(
        x     = cfg$x_label,
        y     = "95% PPI",
        title = paste0(cfg$plot_title, ": ", ecg)
      ) +
      theme_bw() +
      scale_colour_manual(
        values = c("#F8766D", "#00BFC4"),
        labels = c("fit", "no fit"),
        drop   = FALSE
      )
    
    # Faceting by parameter structure
    if (cfg$has_section && cfg$has_gear) {
      p <- p + facet_grid(rows = vars(gear), cols = vars(section))
    } else if (cfg$has_section) {
      p <- p + facet_grid(cols = vars(section))
    } else if (cfg$has_gear) {
      p <- p + facet_grid(cols = vars(gear))
    }
    
    # Axis limits/scales
    if (isTRUE(cfg$plus_one)) {
      p <- p +
        scale_y_continuous(breaks = seq(1, 10, by = 1)) +
        scale_x_continuous(breaks = seq(1, 10, by = 1))
    } else {
      max_val <- max(ppc_data$`97.5%`)
      p <- p + coord_cartesian(xlim = c(0, max_val), ylim = c(0, max_val))
    }
    
    p
  }
  
  # Build all PPC data and plots for active parameters
  ppc_data_list <- imap(active_params, build_ppc_data)
  
  plot_list <- imap(active_params, \(cfg, rep_name) {
    build_ppc_plot(ppc_data_list[[rep_name]], cfg)
  })
  names(plot_list) <- paste0(map_chr(active_params, "obs_name"), "_PPC_plt")
  
  # Combined p_fit summary across all active parameters
  p_fit <- imap_dfr(active_params, \(cfg, rep_name) {
    add_summary_totals(ppc_data_list[[rep_name]], cfg$summary_grps) |>
      mutate(Parameter = paste0(cfg$obs_name, "_PPC"))
  })
  
  # Combined plot groups:
  #   p_combined  = section-only index count plots (V_I, T_I, A_I, B_s)
  #   p_combined2 = gear-only interview expansion plots (V_A, T_A, B_A)
  index_plots <- plot_list[
    names(plot_list) %in% paste0(
      map_chr(keep(active_params, \(cfg) cfg$has_section && !cfg$has_gear), "obs_name"),
      "_PPC_plt"
    )
  ]
  interview_plots <- plot_list[
    names(plot_list) %in% paste0(
      map_chr(keep(active_params, \(cfg) cfg$has_gear && !cfg$has_section), "obs_name"),
      "_PPC_plt"
    )
  ]
  
  p_combined  <- if (length(index_plots) > 0)     gridExtra::arrangeGrob(grobs = index_plots,     ncol = 1) else NULL
  p_combined2 <- if (length(interview_plots) > 0) gridExtra::arrangeGrob(grobs = interview_plots, ncol = 1) else NULL
  
  list(
    p_fit       = p_fit,
    plots       = plot_list,
    p_combined  = p_combined,
    p_combined2 = p_combined2
  )
}