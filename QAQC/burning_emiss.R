# comparing RCMIP & CMIP7 open burning emissions... 

# 0. Set Up ---------------
library(dplyr)
library(ggplot2)



# 1. RCMIP -------------------

# let's revisit the rcmip NOx emissions and see if there is a trend that we 
# can follow from the sub categories... 
rcmip_emissions_file <- file.path("data/raw-data/", 
                                  pattern = "rcmip-emissions-annual-means-v5-1-0.csv")

read.csv(rcmip_emissions_file) %>% 
  filter(Scenario == "historical") %>% 
  filter(Region == "World") %>% 
  pivot_longer(starts_with("X"), names_to = "year") %>% 
  mutate(year = as.integer(gsub("X", "", year))) %>% 
  na.omit -> 
  rcmip_long

rcmip_long %>% 
  #filter(grepl(pattern = "MAGICC AFOLU$", x = Variable)) %>%  
  na.omit -> 
  rcmip_burning

# 2. CMIP7 Burning -------------------

file.path("data/L0", pattern = "L0.BB4CMIP_emissions-raw.csv") %>% 
  read.csv() -> 
  cmip7_burning


# 2B. CMIP5 era history ---- 

"~/Documents/Hector-WD/hector-rcmip-tier1/inst/input/emissions/RCP45_emissions.csv" %>% 
  read.csv(comment.char = ";") %>% 
  select(year = Date, N2O_emissions, NOX_emissions) %>% 
  filter(year <= 2015) -> 
  rcp_emissions



# 3. Comparison -------------------

rcmip_burning$Variable %>% unique()


em <- "Emissions\\|OC\\|"
em <- "N2O"


rcmip_burning %>% 
  filter(grepl(x = Variable, pattern = em)) %>% 
  # filter(Variable %in% c("Emissions|N2O|MAGICC AFOLU|Agricultural Waste Burning", 
  #                        "Emissions|N2O|MAGICC AFOLU|Forest Burning", 
  #                        "Emissions|N2O|MAGICC AFOLU|Grassland Burning", 
  #                        "Emissions|N2O|MAGICC AFOLU|Peat Burning")) %>% 
  # summarise(value = sum(value), .by = "year") %>% 
  mutate(source = "rcmip" ) -> 
  rcmip_d

cmip7_burning %>% 
  filter(grepl(x = variable, pattern = em)) %>% 
  mutate(source = "cmip7") -> 
  cmip7_d

# 
ggplot() + 
  #geom_line(data = cmip7_d, aes(year, value, color = source)) + 
  #geom_line(data = rcmip_d, aes(year, value, color = Variable)) + 
  geom_line(data = rcp_emissions, aes(year, N2O_emissions))



# - how differenet are teh emissions in teh same units



"data/L0/L0.BB4CMIP_emissions.csv" %>% 
  read.csv %>% 
  mutate(source = "my burning") -> 
  my_brunning

"data/L0/L0.CEDS_emissions.csv" %>% 
  read.csv %>% 
  mutate(source = "my ceds") -> 
  my_ceds

em_var <- EMISSIONS_N2O()

bind_rows(my_brunning, my_ceds) %>%  
  filter(variable == em_var) %>% 
  ggplot() + 
  geom_line(aes(year, value, color = source)) 




