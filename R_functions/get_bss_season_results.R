#' Prepare season-level results for a single catch group
#'
#' @param ecg_draws Draws object from extract(ecg_fit)
#' @param ecg_name Name of the catch group
#'
#' @return List containing catch and effort results and quantiles, or NULL if error occurs
get_bss_season_results <- function(ecg_draws, ecg_name) {
  
  tryCatch({
    # Prepare catch data
    catch_results <- ecg_draws$C_sum |> 
      as.data.frame() |> 
      mutate(iter = row_number()) |> 
      pivot_longer(cols = -iter, names_to = "est_cg", values_to = "C_sum") |> 
      select(-"est_cg") |>
      mutate(ecg = ecg_name)
    
    # Check for missing values in catch
    if (any(is.na(catch_results$C_sum)) || any(is.nan(catch_results$C_sum))) {
      n_nan_c <- sum(is.na(catch_results$C_sum) | is.nan(catch_results$C_sum))
      n_total_c <- length(catch_results$C_sum)
      pct_nan_c <- round(100 * n_nan_c / n_total_c, 1)
      cli::cli_alert_warning("Missing or NaN values detected in C_sum for catch group: {ecg_name} ({n_nan_c} of {n_total_c} draws, {pct_nan_c}%)")
    }
    
    catch_quantiles <- catch_results |>
      reframe(C_sum = quantile(C_sum, c(0.025, 0.5, 0.975), na.rm = TRUE), 
              q = c(0.025, 0.5, 0.975))
    
    # Prepare effort data
    effort_results <- ecg_draws$E_sum |> 
      as.data.frame() |> 
      mutate(iter = row_number()) |> 
      pivot_longer(cols = -iter, names_to = "est_cg", values_to = "E_sum") |> 
      select(-"est_cg") |>
      mutate(ecg = ecg_name)
    
    # Check for missing values in effort
    if (any(is.na(effort_results$E_sum)) || any(is.nan(effort_results$E_sum))) {
      n_nan_e <- sum(is.na(effort_results$E_sum) | is.nan(effort_results$E_sum))
      n_total_e <- length(effort_results$E_sum)
      pct_nan_e <- round(100 * n_nan_e / n_total_e, 1)
      cli::cli_alert_warning("Missing or NaN values detected in E_sum for catch group: {ecg_name} ({n_nan_e} of {n_total_e} draws, {pct_nan_e}%)")
    }
    
    effort_quantiles <- effort_results |>
      reframe(E_sum = quantile(E_sum, c(0.025, 0.5, 0.975), na.rm = TRUE), 
              q = c(0.025, 0.5, 0.975))
    
    return(list(
      catch_results = catch_results,
      catch_quantiles = catch_quantiles,
      effort_results = effort_results,
      effort_quantiles = effort_quantiles
    ))
    
  }, error = function(e) {
    cli::cli_alert_danger("Error processing season results for catch group {ecg_name}: {e$message}")
    cli::cli_alert_warning("Skipping density plot generation for catch group: {ecg_name}")
    return(NULL)
  })
}
