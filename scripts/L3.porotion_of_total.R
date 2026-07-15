# Description:  Requested by R. Hoesly the portion of CEDS emissions out of global total emissions. 

# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

library(dplyr)
library(ggplot2)


# 1. Load Data -----------------------------------------------------------------

here::here("data/L0/L0.CEDS_emissions.csv") %>% 
  read.csv() %>% 
  select(year, variable, ceds_value = value) %>%  
  mutate(variable = if_else(variable == FFI_EMISSIONS(), "CO2", variable)) -> 
  ceds


here::here("data/L2/hector_ceds.csv") %>% 
  read.csv() %>% 
  select(year, variable, global_value = value) -> 
  global

# Combine the CO2 emissions into a single category 
global %>% 
  filter(variable %in% c(LUC_EMISSIONS(), FFI_EMISSIONS())) %>% 
  summarise(global_value = sum(global_value), .by = ("year")) %>% 
  mutate(variable = "CO2") %>% 
  rbind(global) -> 
  global

# 2. Get Ratio -----------------------------------------------------------------

ceds %>% 
  left_join(global, by = join_by(year, variable)) %>% 
  mutate(ratio = ceds_value/global_value) %>% 
  filter(year %in% c(1850, 2000)) %>% 
  mutate(percent = round(ratio * 100, 3)) -> 
  out

# 3. Save Output ---------------------------------------------------------------

fname <- file.path(DIRS$L3, "percent_ceds_1850_2000.csv")
write.csv(x = out, file = fname, row.names = FALSE)

