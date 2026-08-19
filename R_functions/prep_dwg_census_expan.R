# creates summary table of census expansion factors based on values in data base

#NOTE: "p_census is the same as "p_TI" in the BSS model, which represent a known or assumed proportion of each section covered by tie-in counts for each final angler-type; this parameter is used to directly expand/modify the census count data prior to the estimate of the bias parameter/expansion factor
#NOTE: values of "p_census/p_TI" are entered and stored in creel db
#NOTE: currently, values of "p_census/p_TI" are limited to two "angler_final" groupings (bank, boat)

prep_dwg_census_expan <- function(
    eff, # effort data from dwg filtered using start & end dates passed from params
    days,
    ...){
  
  eff |> 
    dplyr::filter( # filter to only dates specified in params and described in dwg$days object
      location_type == "Section", 
      event_date >= min(days$event_date),
      event_date <= max(days$event_date)
    ) |> 
    dplyr::distinct(section_num, p_census_bank, p_census_boat) |>
    tidyr::pivot_longer(
      cols = starts_with("p_census_"), 
      names_prefix = "p_census_",
      names_to = "angler_final",
      values_to = "p_census"
    ) |> 
    dplyr::mutate(
      p_census = replace_na(p_census, 1)
    ) |> 
    dplyr::arrange(angler_final, section_num)
  
}
