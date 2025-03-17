# Description: Add future emissions to the ceds Hector time series. 

# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Load the historical emissions we would like to combine with 
file.path(DIRS$TABLES, "hector_ceds_historical_emiss.csv") %>% 
  read.csv(comment.char = ";") -> 
  ceds_historical_wide

ceds_historical_wide %>% 
  pivot_longer(-Date) %>%
  select(year = Date, variable = name, value) %>% 
  mutate(scenario = "historical") -> 
  ceds_historical

# 1. Main Chunk ----------------------------------------------------------------
# Load the future emissions.
list.files(system.file("input/tables", package = "hector"), 
           pattern = "ssp", full.names = TRUE) %>% 
  lapply(function(f){
    
    # Define the scenario name
    scn <- gsub(pattern = "_emiss-constraints_rf.csv", x = basename(f), replacement = "")
    
    # Read the csv file and change from wide to long.
    read.csv(file = f, comment.char = ";") %>%  
      select(names(ceds_historical_wide)) %>% 
      filter(Date > CEDS_FINAL_YEAR) -> 
      wide_results
    
    # For only the emissions change the formatting of Hector form wide to long,
    # for easy plotting comparisons
    wide_results %>% 
      pivot_longer(-Date) %>%
      select(year = Date, variable = name, value) %>%  
      bind_rows(ceds_historical) %>% 
      mutate(scenario = scn) -> 
      out
    
    return(out)
    
  }) %>% 
  do.call(what = "rbind") -> 
  future_emiss

# Define the transition period to make the transition from the historical period 
# to the future period. 
transition_period <- 10 
trans_start <- CEDS_FINAL_YEAR+1
trans_end <- trans_start + transition_period
transition_years <- trans_start:trans_end

# Write replace the transition years with NA values. 
future_emiss %>% 
  mutate(value = if_else(year %in% transition_years, NA, value)) %>% 
  arrange(scenario, variable, year) %>% 
  group_by(variable, scenario) %>% 
  mutate(value = na.approx(value, year)) %>% 
  ungroup() ->
  hist_future_rslts

# 2. Write Output --------------------------------------------------------------

# Save the results for QAQC
write.csv(hist_future_rslts, 
          file = file.path(DIRS$L3, "L3.hist_future.csv"),
          row.names = FALSE)

# Write the input tables and the ini files out 
hist_future_rslts %>% 
  distinct() %>% 
  # TODO the get units function will throw an error related halocarbon 
  # emissions, it is fine and there is a github issue related to that. 
  mutate(units = getunits(variable)) %>%  
  split(f = .$scenario) %>% 
  lapply(FUN = write_hector_csv, write_to = DIRS$TABLES) -> 
  tables

# Write the ini files out!
tables %>%
  lapply(FUN = write_hector_ini) ->
  inis
