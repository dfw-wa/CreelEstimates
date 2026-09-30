# Half-Student-t density (half-Cauchy when df = 1)
dhalft <- function(x, df, scale) {
  ifelse(x >= 0, 2 * stats::dt(x / scale, df = df) / scale, 0)
}

# Reflection-corrected KDE for a strictly nonnegative parameter
half_kde <- function(x, x_max, n_grid = 1024) {
  x  <- x[is.finite(x) & x >= 0]
  bw <- tryCatch(stats::bw.SJ(x), error = function(e) stats::bw.nrd0(x))
  kd <- stats::density(c(-x, x), bw = bw, from = 0, to = x_max, n = n_grid)
  tibble::tibble(x = kd$x, y = 2 * kd$y)
}

# Prior, posterior and approximate likelihood on one common grid
make_variance_density_data <- function(post_vals, df, scale, n_grid = 1024) {
  post_vals <- post_vals[is.finite(post_vals) & post_vals >= 0]
  
  post_q995 <- unname(stats::quantile(post_vals, 0.995))
  post_q99  <- unname(stats::quantile(post_vals, 0.99))
  prior_q80 <- scale * stats::qt(0.99, df = df)  #truncated to 99%
  x_max     <- max(post_q995 * 1.25, prior_q80)
  
  grid    <- seq(0, x_max, length.out = n_grid)
  prior_y <- dhalft(grid, df = df, scale = scale)
  post_y  <- half_kde(post_vals, x_max = x_max, n_grid = n_grid)$y
  
  # likelihood ∝ posterior / prior, only where posterior is well estimated
  lik_y <- post_y / prior_y
  lik_y[!is.finite(lik_y) | grid > post_q99] <- NA_real_
  if (any(is.finite(lik_y)) && max(lik_y, na.rm = TRUE) > 0) {
    lik_y <- lik_y * max(post_y) / max(lik_y, na.rm = TRUE)
  }
  
  list(
    density_df = dplyr::bind_rows(
      tibble::tibble(x = grid, y = post_y, Distribution = "Posterior"),
      tibble::tibble(x = grid, y = prior_y, Distribution = "Prior")
    ),
    lik_df = tibble::tibble(x = grid, y = lik_y),
    x_max  = x_max
  )
}

create_prior_posterior_plots <- function(draws_df, stan_data, ecg) {
  
  variance_priors <- tibble::tibble(
    param = c("sigma_eps_C", "sigma_eps_E", "sigma_r_E",
              "sigma_r_C", "sigma_mu_C", "sigma_mu_E"),
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
      "Catch process error", "Effort process error",
      "Effort overdispersion", "Catch overdispersion",
      "Catch spatial heterogeneity", "Effort spatial heterogeneity"
    )
  )
  
  safe_name <- stringr::str_replace_all(ecg, "[^[:alnum:]]", "_")
  
  plots_list <- purrr::pmap(variance_priors, function(param, df, scale, label) {
    
    pd <- make_variance_density_data(
      post_vals = as.numeric(draws_df[[param]]),
      df = df, scale = scale
    )
    
    ggplot2::ggplot(pd$density_df,
                    ggplot2::aes(x = x, y = y, fill = Distribution, colour = Distribution)) +
      ggplot2::geom_area(position = "identity", alpha = 0.4, linewidth = 0.5) +
      ggplot2::geom_line(
        data = pd$lik_df,
        ggplot2::aes(x = x, y = y, linetype = "Likelihood"),
        inherit.aes = FALSE, colour = "black", linewidth = 0.8, na.rm = TRUE
      ) +
      ggplot2::scale_linetype_manual(name = NULL, values = c("Likelihood" = "dashed")) +
      ggplot2::scale_x_continuous(
        limits = c(0, pd$x_max),
        expand = ggplot2::expansion(mult = c(0, 0.01))
      ) +
      ggplot2::labs(
        title    = label,
        subtitle = glue::glue("Half-Student-t(df = {df}, scale = {scale})"),
        x = param, y = "Density"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "top",
        plot.title      = ggplot2::element_text(size = 10, face = "bold"),
        plot.subtitle   = ggplot2::element_text(size = 8)
      )
  })
  
  list(plots_list = plots_list, safe_name = safe_name, ecg = ecg)
}