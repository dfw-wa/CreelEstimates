# creates i.) est_cg field using species, life_stage, fin_mark, and fate, ii.) summarizes the number each angler group caught, and...
# ...iii.) creates output based on study_design and corresponding interview fields needed for angler effort calculations  

prep_dwg_interview_catch <- function(
    params,
    interview_plus_angler_types, # output from preceding functions that created calculated fishing time and defined angler_final 
    dwg_catch,                   # catch data from dwg 
    study_design,                # string passed from params denoting which study design was followed during data collection
    est_catch_groups,            # data.frame passed from params of aggregated catch groups of interest to estimate
    ...){

  #coerce missing values to actual strings to allow params$est_catch_groups to include NAs alongside non-NA
  #to allow 'run' specification in params, add 'run' within across()
  dwg_catch_group <- 
    dwg_catch |> 
    mutate(across(c(species, life_stage, fin_mark, fate), ~replace_na(as.character(.), "NA")))
  
  est_catch_groups <- est_catch_groups |>
    mutate(across(c(species, life_stage, fin_mark, fate), ~replace_na(as.character(.), "NA")))
  
  catches <- map_df(
    1:nrow(est_catch_groups),
    ~dwg_catch_group |>
      filter(
        str_detect(species, est_catch_groups$species[.x]),
        str_detect(life_stage, est_catch_groups$life_stage[.x]),
        str_detect(fin_mark, est_catch_groups$fin_mark[.x]),
        str_detect(fate, est_catch_groups$fate[.x])
      ) |>
      mutate(est_cg = combine_catch_group(est_catch_groups[.x, ])) |>
      group_by(est_cg, interview_id) |>
      summarise(fish_count = sum(fish_count, na.rm = T), .groups = "drop")
  )

  #replicate wrangled interviews n-many of catch_groups to estimate
  int_cat <- map_df(
    1:nrow(est_catch_groups), 
    ~interview_plus_angler_types |>   
      mutate(est_cg = combine_catch_group(est_catch_groups[.x, ]))
    ) |>
    left_join(catches, by = c("est_cg", "interview_id")) |>
    mutate(fish_count = replace_na(fish_count, 0)) |>
    mutate(
      fishery_name = params$fishery_name # add back fishery_name
    ) |>
    relocate(fishery_name)

  if(str_detect(study_design, "tandard" )){
    interview_final<-
      int_cat |> 
      dplyr::select(
        interview_id,
        section_num, event_date, angler_final, angler_final_int,
        vehicle_count, trailer_count,
        fishing_time, person_count_final, fishing_time_total,
        trip_status, previously_interviewed,
        est_cg, fish_count
      ) |>
      dplyr::arrange(section_num, event_date, angler_final) 
    
  }else if(study_design == "Drano"){
    
    interview_final<-
      int_cat |> 
      dplyr::select(
        interview_id,
        section_num, event_date, angler_final, angler_final_int,
        fishing_time, person_count_final, fishing_time_total,
        trip_status, previously_interviewed,
        est_cg, fish_count
      ) |>
      dplyr::arrange(section_num, event_date, angler_final) 
  }
  return(interview_final)
}
