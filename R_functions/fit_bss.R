#thin wrapper on stan()
fit_bss <- function(
  # model_file = here::here("stan_models/BSS_creel_model_02_2021-01-22.stan"),
  #model_file_name = here::here(paste0("stan_models/", model_file_name)), #BSS_creel_model_02_2021-01-22.stan"),
  # model_file_name = here::here("stan_models/BSS_creel_model_02_2021-01-22_ppc.stan"),
  model_file_name,
  bss_inputs_list,
  n_chain = n_chain,  
  n_cores = n_cores,
  n_iter = n_iter,
  n_warmup = n_warmup,
  n_thin = n_thin,
  adapt_delta = adapt_delta,
  max_treedepth = max_treedepth,
  init = "0",
  ...){
  
  model_path <- here::here("stan_models", model_file_name)
  
  if(!file.exists(model_path)){
    stop(
      "Stan model file not found: ", model_path, 
      "\nCheck params$bss_model_file_name matches a file in stan_models/"
    )
  }
  
  # naming convention: single-gear (G == 1) models end in "_1G.stan"
  is_1g_model <- grepl("_1G\\.stan$", model_file_name)
  
  if(bss_inputs_list$G == 1 && !is_1g_model){
    stop(
      "Catch group '", bss_inputs_list$est_cg, "' has a single gear/angler type (G = 1), ",
      "but the selected model '", model_file_name, "' assumes 2 gear/angler types.",
      "\nSet params$bss_model_file_name to \"BSS_creel_model_02_2021-01-22_ppc_1G.stan\"."
    )
  }
  
  if(bss_inputs_list$G > 1 && is_1g_model){
    stop(
      "Catch group '", bss_inputs_list$est_cg, "' has ", bss_inputs_list$G, 
      " gear/angler types, but the selected model '", model_file_name, 
      "' is a single-gear (G = 1) model.",
      "\nSet params$bss_model_file_name to a multi-gear model (e.g., \"BSS_creel_model_02_2021-01-22_ppc.stan\")."
    )
  }
  
  if(is_1g_model){
    gear_elements <- c("gear_A", "gear_E", "gear_IntC", "gear_IntA")
    bad_gear <- gear_elements[vapply(gear_elements, function(x){
      v <- bss_inputs_list[[x]]
      length(v) > 0 && any(v != 1)
    }, logical(1))]
    if(length(bad_gear) > 0){
      stop(
        "Single-gear model requires all gear indices to equal 1, but found other values in: ",
        paste(bad_gear, collapse = ", "),
        "\n(e.g., census/index counts contain boat anglers while interviews are bank-only)."
      )
    }
    if(nrow(bss_inputs_list$p_TI) != 1){
      stop(
        "Single-gear model requires p_TI to have 1 row, but it has ", 
        nrow(bss_inputs_list$p_TI), "."
      )
    }
  }
  
  stan(
    file = model_path,
    data = bss_inputs_list,
    chains = n_chain,
    cores = n_cores,
    iter = n_iter,
    warmup = n_warmup,
    thin = n_thin, init = init, include = T,
    control = list(
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth
    )
  )
  
}