# Description: Combine the CEDs data with additional data sources in order 
# to get the total global emissions required for hector inputs. The csv file 
# returned here still needs to be formatted. 

# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Import the L0 data sets 
list.files(DIRS$L0, pattern = "L0.*.csv", full.names = TRUE) %>% 
  lapply(read.csv, comment.char = "#") %>% 
  do.call(what = "rbind") -> 
  emissions

# Load the mapping file. 
file.path(DIRS$MAPPING, "L1.hector_sector_mapping.csv") %>% 
  read.csv(comment.char = "#") -> 
  mapping_file 

# 1. Main Chunk ----------------------------------------------------------------
# Keep only the relevant emissions from the respective sources and aggregate to 
# the global total values. 
emissions %>%  
  inner_join(mapping_file, by = join_by(sector, variable, source)) -> 
  emissions_both_sources
  
emissions_both_sources %>% 
  summarise(value = sum(value), .by = c("year", "variable", "units")) -> 
  out

# TODO I am not sure about this file name....
write.csv(out, file = file.path(DIRS$L1, "ceds_burnning_emiss.csv"), row.names = FALSE)


# Z. Diagnostic plots ---------------------------------------------
if(FALSE){
  

  
  file.path(DIRS$L1, "ceds_burnning_emiss-og.csv") %>% 
    read.csv %>% 
    mutate(source = "cmip6") -> 
    cmip6_data

  
  file.path(DIRS$L1, "ceds_burnning_emiss.csv") %>% 
    read.csv %>% 
    mutate(source = "cmip7") -> 
    cmip7_data
  
  
  system.file(package = "hector", "input/tables/ssp245_emiss-constraints_rf.csv") %>% 
    read.csv(comment.char = ";") %>% 
    select(year = Date, value = NOX_emissions) -> 
    d
  
  # Problem 
  # "NOX_emissions"
  
  
  # [1] "BC_emissions"    "CH4_emissions"   "CO_emissions"    "N2O_emissions"   "NH3_emissions"  
  # [6] "NMVOC_emissions" "NOX_emissions"   "OC_emissions"    "SO2_emissions"   "ffi_emissions" 
  
  em <- "NOX_emissions" 
  
  cmip6_data %>% 
    bind_rows(cmip7_data) %>% 
    filter(variable == em) -> 
    dd
  

    ggplot() + 
    geom_line(data = dd, aes(year, value, color = source)) + 
      geom_line(data = d, aes(year, value)) + 
      
    labs(title = em)
  
}
  
