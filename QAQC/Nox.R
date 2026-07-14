# Compare NOx Emissions! 

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(hector)


# 1. Read in global NOx data --------------------------------------------------------------

em <- EMISSIONS_N2O()


# The default values included in the V3 release
system.file(package = "hector", "input/tables") %>% 
  list.files("ssp245_emiss-constraints_rf.csv", full.names = TRUE) %>% 
  read.csv(comment.char = ";") %>% 
  select(year = Date, em) %>% 
  mutate(variable = em, source = "v3.2") %>%  
  filter(year <= 2023) -> 
  d1

here::here("inputs", "tables", "cmip6_corrected_ssp245_emiss.csv") %>% 
  read.csv(comment.char = ";") %>%  
  select(year = Date,  em) %>% 
  mutate(variable = em, source = "corrected cmip6") %>%  
  filter(year <= 2023) -> 
  d2


here::here("inputs", "tables", "hector_ceds_historical_emiss.csv") %>% 
  read.csv(comment.char = ";") %>% 
  select(year = Date, em) %>% 
  mutate(variable = em, source = "cmip7") %>%  
  filter(year <= 2023) -> 
  d3

# 2B. CMIP5 era history ---- 

"~/Documents/Hector-WD/hector-rcmip-tier1/inst/input/emissions/RCP45_emissions.csv" %>% 
  read.csv(comment.char = ";") %>% 
  select(year = Date, em) %>% 
  filter(year <= 2015) %>% 
  mutate(source = "cmip5") -> 
  d4


d <- bind_rows(d1, d2, d3, d4)

d %>% 
  rename(value = em) %>% 
  ggplot(aes(year, value, color = source)) + 
  geom_line() + 
  labs(title = em)




# 2. Revisit RCMIP NOx ---------------------------------------------------------


# let's revisit the rcmip NOx emissions and see if there is a trend that we 
# can follow from the sub categories... 
rcmip_emissions_file <- file.path("data/raw-data/", 
                                  pattern = "rcmip-emissions-annual-means-v5-1-0.csv")


read.csv(rcmip_emissions_file) %>% 
  filter(Scenario == "historical") %>% 
  filter(Region == "World") %>% 
  filter(grepl(pattern = "NOx", x = Variable))  %>% 
  pivot_longer(starts_with("X"), names_to = "year") %>% 
  mutate(year = as.integer(gsub("X", "", year))) %>% 
  na.omit -> 
  rcmip_nox


# According to RCMIP these are all in the same units..... 
rcmip_nox %>% 
  select(Variable, Unit) %>% distinct()

rcmip_nox %>% 
  filter(Variable == "Emissions|NOx") %>%
  mutate(source = "rcmip total") -> 
  rcmip_total

rcmip_nox %>% 
  filter(Variable %in% c("Emissions|NOx|MAGICC AFOLU", 
                         "Emissions|NOx|MAGICC Fossil and Industrial")) %>%
  summarise(value = sum(value), .by = "year") %>% 
  mutate(source = "my total") -> 
  my_total

# Okay yes the total NOx emissions from RCMIP is the sum of the FFI and BURN
mean(rcmip_total$value - my_total$value)

ggplot() + 
  geom_line(data = rcmip_total, aes(year, value)) + 
  geom_line(data = my_total, aes(year, value))



rcmip_nox %>% 
  filter(Variable %in% c("Emissions|NOx|MAGICC AFOLU", 
                         "Emissions|NOx|MAGICC Fossil and Industrial", 
                         "Emissions|NOx")) %>%
  ggplot() + 
  geom_line(aes(year, value, color = Variable))


rcmip_nox %>% 
  filter(Variable %in% c("Emissions|NOx|MAGICC AFOLU")) %>% 
  filter(year <= 2013) -> 
  rcmip_burning



"data/L0/L0.BB4CMIP_emissions-raw.csv" %>% 
  read.csv() %>% 
  filter(variable == "Emissions|NOx|Biomass Burning") %>% 
  filter(year <= 2013) -> 
  cmip7_burning



ggplot() + 
  geom_line(data = rcmip_burning, aes(year, value, color = "rcmip")) + 
  geom_line(data = cmip7_burning, aes(year, value, color = "cmip7")) 
  

rcmip_burning %>% 
  filter(year <= 1850) -> 
  rcmip_pre_1850

cmip7_burning %>% 
  filter(year <= 1850) -> 
  cmip7_pre_1850


mean(cmip7_pre_1850$value/rcmip_pre_1850$value)

sd(cmip7_burning$value/rcmip_burning$value)
