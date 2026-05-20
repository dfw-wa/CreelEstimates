calculate_ppc_pvalues_v2 <- function(bss_fit, inputs_bss, ecg) {
  
  extracted_data <- extract(bss_fit)
  ecg_inputs <- inputs_bss[[ecg]]
  
  # Lookup table mapping rep parameter names to their observed counterpart
  # in inputs_bss[[ecg]]. Extend this list as new model configurations arise.
  param_map <- list(
    V_I_rep = "V_I",   # Vehicle index counts
    T_I_rep = "T_I",   # Trailer index counts
    A_I_rep = "A_I",   # Angler index counts
    B_s_rep = "B_s",   # Boat census counts (Drano)
    E_s_rep = "E_s",   # Angler census counts
    c_rep   = "c",     # Catch counts
    V_A_rep = "V_A",   # Vehicle interview expansions
    T_A_rep = "T_A",   # Trailer interview expansions
    B_A_rep = "B_A"    # Boat interview expansions (Drano)
  )
  # Retain only parameters present in both the fit and the input data
  active_params <- param_map[
    names(param_map) %in% names(extracted_data) &
      unlist(param_map) %in% names(ecg_inputs)
  ]
  
  if (length(active_params) == 0) {
    warning(sprintf(
      "No recognized PPC parameters found for catch group '%s'. Check that rep arrays and observed inputs are named consistently.", 
      ecg
    ))
    return(NULL)
  }
  
  # Internal helper: compute posterior predictive p-values for one parameter
  compute_pvalue <- function(rep_draws, obs_vals) {
    rep_draws |>
      as_tibble(.name_repair = ~ paste0("V", seq_along(.))) |>
      mutate(iteration = row_number()) |>
      pivot_longer(
        cols      = -iteration,
        names_to  = "idx",
        values_to = "value"
      ) |>
      mutate(idx = as.integer(str_remove(idx, "V"))) |>
      left_join(
        tibble(obs = obs_vals, idx = seq_along(obs_vals)),
        by = "idx"
      ) |>
      group_by(iteration) |>
      summarise(
        mean_gt = if_else(mean(value) > mean(obs), 1, 0),
        sd_gt   = if_else(sd(value)   > sd(obs),   1, 0),
        .groups = "drop"
      ) |>
      summarise(
        mean_gt = sum(mean_gt) / n(),
        sd_gt   = sum(sd_gt)   / n()
      )
  }
  
  # Compute p-values for all active parameters and return as named list
  # Output names follow the convention: <obs_name>_pvalue (e.g., B_I_pvalue)
  pvalue_list <- imap(active_params, \(obs_name, rep_name) {
    compute_pvalue(
      rep_draws = extracted_data[[rep_name]],
      obs_vals  = ecg_inputs[[obs_name]]
    )
  })
  
  names(pvalue_list) <- paste0(unlist(active_params), "_pvalue")
  
  pvalue_list
}