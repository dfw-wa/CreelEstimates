get_bss_cpue_daily <- function(bss_fit, ecg, dwg, gear_labels = c("bank", "boat"), ...){

  bss_fit |> 
    summary(pars = c("lambda_C_S")) |> 
    pluck("summary") |> #only want the combined-chains version
    as.data.frame() |> 
    rownames_to_column("estimate_index") |> 
    as_tibble() |> 
    mutate(indices = str_sub(estimate_index, 12, 20) |> str_remove("\\]")) |> 
    separate(
      col = indices,
      into = c("section_num", "day_index", "angler_final")
    ) |> 
    mutate(
      across(c(section_num, day_index), as.integer),
      angler_final = gear_labels[as.integer(angler_final)],
      est_cg = ecg,
      estimate = "CPUE_daily"
    ) |> 
    left_join(dwg$days |> 
                select(event_date, day_index, week, month),
              by = "day_index") |> 
    relocate(estimate, estimate_index, est_cg, day_index, event_date, week, month, section_num, angler_final) |> 
    arrange(event_date)
}