# The purpose of this QAQC script it is to take a look at the differences between 
# the historical [CO2], [CH4], and [N2O] with observations. I think the goal 
# here is to get a sense of how much changing the antro. emissions  

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(tidyr)

# 1. Benchmark Data ---------------------------------------------------------

# Load the historical concentrations that are going to be used in the comparisons. 
here::here("data", "raw-data", "Supplementary_Table_UoM_GHGConcentrations-1-1-0_annualmeans_v23March2017.csv") %>% 
  read.csv(skip = 21) %>% 
  na.omit %>% 
  lapply(., function(x) gsub(",", "", x)) %>% 
  lapply(., function(x) gsub(" ", "", x)) %>% 
  bind_rows() -> 
  conc_df

names(conc_df)[1] <- "year"  

conc_df %>% 
  pivot_longer(cols = -year, names_to = "variable") %>% 
  mutate(variable = paste0(variable, "_concentration")) %>% 
  filter(variable %in% c(CONCENTRATIONS_CO2(), CONCENTRATIONS_N2O(), CONCENTRATIONS_CH4())) %>% 
  mutate(source = "observations") %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(value = as.double(value)) -> 
  observations_df


# 2. Load Hector Output --------------------------------------------------------


write.csv(old, file = file.path(DIRS$L3, "hector_CEDS_rslts_old.csv"), row.names = FALSE)
write.csv(new, file = file.path(DIRS$L3, "hector_CEDS_rslts_new.csv"), row.names = FALSE)

c(file.path(DIRS$L3, "hector_CEDS_rslts_old.csv"), 
  file.path(DIRS$L3, "hector_CEDS_rslts_new.csv")) %>% 
  lapply(read.csv) %>% 
  bind_rows() %>% 
  filter(variable %in% c(CONCENTRATIONS_CO2(), CONCENTRATIONS_N2O(), CONCENTRATIONS_CH4()))  -> 
  hector_rslts 


# 3. Comparisons  --------------------------------------------------------------

observations_df %>% 
  bind_rows(hector_rslts) -> 
  to_plot

VAR <- CONCENTRATIONS_CO2()
to_plot %>% 
  filter(variable == VAR) %>% 
  ggplot() +
  geom_line(aes(year, value, color = source)) + 
  labs(title = VAR, subtitle = "hector vs. observations", 
       y = getunits(CO2_CONSTRAIN()))

VAR <- CONCENTRATIONS_N2O()
to_plot %>% 
  filter(variable == VAR) %>% 
  ggplot() +
  geom_line(aes(year, value, color = source)) + 
  labs(title = VAR, subtitle = "hector vs. observations", 
       y = getunits(N2O_CONSTRAIN()))

VAR <- CONCENTRATIONS_CH4()
to_plot %>% 
  filter(variable == VAR) %>% 
  ggplot() +
  geom_line(aes(year, value, color = source)) + 
  labs(title = VAR, subtitle = "hector vs. observations", 
       y = getunits(CH4_CONSTRAIN()))




