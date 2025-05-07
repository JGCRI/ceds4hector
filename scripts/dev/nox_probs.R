library(dplyr)
library(ggplot2)


"data/L3/hector_CEDS_rslts_data.csv" %>% 
  read.csv() %>% 
  filter(variable == EMISSIONS_NOX()) -> 
  d1 
  

system.file(package = "hector", "input/tables/ssp245_emiss-constraints_rf.csv") %>% 
  read.csv(comment.char = ";") %>% 
  select(year = Date, value = "NOX_emissions") %>% 
  mutate(scenario = "rcmip") -> 
  d2

ggplot() + 
  geom_line(data = d1, aes(year, value, color = scenario)) + 
  geom_line(data = d2, aes(year, value, color = scenario))




