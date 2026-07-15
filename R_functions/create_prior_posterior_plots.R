# Approximate likelihood as posterior / prior density on a common grid
approx_likelihood <- function(post_vals, prior_vals, n_grid = 512) {
  x_min <- 0
  x_max <- quantile(c(post_vals, prior_vals), 0.995)
  grid  <- seq(x_min, x_max, length.out = n_grid)
  
  post_kde  <- density(post_vals, from = x_min, to = x_max, n = n_grid)
  prior_kde <- density(prior_vals, from = x_min, to = x_max, n = n_grid)
  
  # Ratio approximates unnormalised likelihood
  lik <- ifelse(prior_kde$y > 1e-6, post_kde$y / prior_kde$y, NA_real_)
  
  # Rescale to match the peak density of the posterior for visual comparability
  post_peak <- max(post_kde$y, na.rm = TRUE)
  lik_peak  <- max(lik, na.rm = TRUE)
  if (!is.na(lik_peak) && lik_peak > 0) {
    lik <- lik * post_peak / lik_peak
  }
  
  tibble::tibble(x = grid, y = lik)
}


create_prior_posterior_plots <- function(draws_df, stan_data, ecg) {
  
  variance_priors <- tibble::tibble(
    param = c("sigma_eps_C", "sigma_eps_E", "sigma_r_E", "sigma_r_C", "sigma_mu_C", "sigma_mu_E"),
    df = c(
      stan_data$value_student_t_df_sigma_eps_C,
      stan_data$value_student_t_df_sigma_eps_E,
      stan_data$value_student_t_df_sigma_r_E,
      stan_data$value_student_t_df_sigma_r_C,
      stan_data$value_student_t_df_sigma_mu_C,
      stan_data$value_student_t_df_sigma_mu_E
    ),
    scale = c(
      stan_data$value_student_t_scale_sigma_eps_C,
      stan_data$value_student_t_scale_sigma_eps_E,
      stan_data$value_student_t_scale_sigma_r_E,
      stan_data$value_student_t_scale_sigma_r_C,
      stan_data$value_student_t_scale_sigma_mu_C,
      stan_data$value_student_t_scale_sigma_mu_E
    ),
    label = c(
      "Catch process error",
      "Effort process error",
      "Effort overdispersion",
      "Catch overdispersion",
      "Catch spatial heterogeneity",
      "Effort spatial heterogeneity"
    )
  )
  
  safe_name <- stringr::str_replace_all(ecg, "[^[:alnum:]]", "_")
  
  plots_list <- purrr::pmap(variance_priors, function(param, df, scale, label) {
    prior_vals <- scale * abs(rt(5000, df = df))
    post_vals  <- as.numeric(draws_df[[param]])
    lik_df     <- approx_likelihood(post_vals, prior_vals)
    x_max      <- quantile(c(prior_vals, post_vals), 0.995)
    
    density_df <- dplyr::bind_rows(
      tibble::tibble(value = prior_vals, Distribution = "Prior"),
      tibble::tibble(value = post_vals,  Distribution = "Posterior")
    )
    
    ggplot() +
      geom_density(
        data = density_df,
        aes(x = value, fill = Distribution, colour = Distribution),
        alpha = 0.4
      ) +
      geom_line(
        data = lik_df,
        aes(x = x, y = y, linetype = "Likelihood"),
        colour = "black",
        linewidth = 0.8,
        na.rm = TRUE
      ) +
      scale_linetype_manual(
        name = NULL,
        values = c("Likelihood" = "dashed")
      ) +
      coord_cartesian(xlim = c(0, x_max)) +
      labs(
        title    = label,
        subtitle = glue::glue("Half-Student-t(df = {df}, scale = {scale})"),
        x = param,
        y = "Density"
      ) +
      theme_bw() +
      theme(
        legend.position = "top",
        plot.title      = element_text(size = 10, face = "bold"),
        plot.subtitle   = element_text(size = 8)
      )
  })
  
  list(
    plots_list = plots_list,
    safe_name  = safe_name,
    ecg        = ecg
  )
}