# Freshwater Creel Estimates

This repository supports estimation of freshwater fishery catches and angler effort using roving-roving creel designs.

The main template script *fw_creel.Rmd* provide a workflow to fetch raw observed data, generate intermediate summaries, produce expanded point estimates and associated uncertainty measures, and output tables and figures. It builds on previous scripts associated with a rapid, established method ("PE") and a more recently developed Bayesian hierarchical state space method ("BSS").

These standard procedures are modified through user inputs in the uppermost `params` block of the script, defining the fishery of interest (`fishery_name`), the start and end dates of the period to be assessed, the focal species and encounter types of interest (`est_catch_groups`), and various other controls related to both the sampling design/protocol (e.g., what quantities were counted during index surveys; which days of the week are considered 'weekend' vs 'weekday') and the particular analysis (e.g., a minimum 'fishing time' duration threshold to filter interview).

## Functions 

This parameterized Rmarkdown workflow calls a sequence of *R_functions/*

  - `fetch_dwg` brings into memory a set of electronically collected observations from the statewide freshwater creel database [published at data.wa.gov](https://data.wa.gov/browse?q=creel)); these consist of counts at index sites and along census survey sections as well as angler interviews and associated catch information
  - `prep_days` associates time-strata information to the period of interest (e.g., week/month index, potential fishing hours, section-specific closures)

Next, the raw data are summarized as a list of objects shared by both the PE and BSS estimation methods

  - `prep_dwg_interview` filters and reorganizes interview records, calculating times and joining catch values conditioned on user inputs in the `params` block
  - `prep_dwg_effort_census` filters, reorganizes and aggregates survey counts, in particular associating the "count_sequence" index of the nearest-in-time index observations
  - `prep_dwg_effort_index` similarly filters, reorganizes and aggregates counts at specific index sites

These objects are further processed to form the inputs for a "classic PE" method  

  - `prep_inputs_pe_census_expan` 
  - `prep_inputs_pe_days_total` 
  - `prep_inputs_pe_ang_hrs_bank_boat` 
  - `prep_inputs_pe_ang_hrs_vhcl_trlr` 
  - `prep_inputs_pe_daily_cpue_catch_est`
  - `prep_inputs_pe_df`
  - `est_pe_effort`
  - `est_pe_catch`

The initial summary objects also form the basis for the state space method

  - `prep_inputs_bss` translates the prepared tabular data into a list of the vector and matrix formats required by the **stan model** code and adds values for several priors; this function is iterated as a list of lists, with element of the outer list associated with each desired "catch group"
  
Each catch-group-specific list of state space inputs is then processed

  - `fit_bss` wraps the `rstan::stan()` function, passing in the data list and various arguments controlling the MCMC process (e.g., number of chains, iterations, etc.)
    - the default BSS model assumes 2 gear/angler types (bank & boat); for single-gear fisheries (e.g., bank-only) set `params$bss_model_file_name` to `"BSS_creel_model_02_2021-01-22_ppc_1G.stan"` (supports 1+ sections); `fit_bss` stops with an informative error if the model and the number of gear types in the catch group don't match; daily outputs and PPC plots label gears from the interview angler types (`gear_labels` in the BSS inputs), so bank-only and boat-only fisheries are labelled correctly
  - `get_bss_overview`
  - `get_bss_catch_daily`
  - `get_bss_effort_daily`
  
Finally, the calculated estimates are presented as tables, plots, and standalone workbooks...

  **PENDING FUNCTIONALIZED OUTPUTS**

 