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
  
  em <- EMISSIONS_N2O()
  
  
  emissions_both_sources %>% 
    filter(variable == em) %>% 
    ggplot(aes(year, value, fill = source)) + 
    geom_area() + 
    theme_bw() +
    labs(title = em, y = getunits(em), x = NULL)
  
  
  
}
  





