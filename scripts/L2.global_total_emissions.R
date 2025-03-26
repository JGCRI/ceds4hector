# Description: Read in the CEDS L1 emissions and add add in the missing 
# historical emissions from the default V3.2 table. 

# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# 1. Load Data -----------------------------------------------------------------
# Load the L1 emissions which combine CEDS and open burning emissions. 
L1_emiss_data <- read.csv(file.path(DIRS$L1, "ceds_burnning_emiss.csv"))

# Read in the default V3.2 emissions and limit until the year 2023. 
read.csv(file.path(DIRS$L0, "L0.V32_emissions.csv")) %>% 
  filter(year <=  max(L1_emiss_data$year)) -> 
  default_emiss

# 2. Input emissions missing from CEDS -----------------------------------------
# Subset the default emissions into the types missing from the L1 data set. 
default_emiss %>% 
  filter(!variable %in% L1_emiss_data$variable) -> 
  missing_emissions

# 2. Combine Emissions ---------------------------------------------------------

# Replace the L1 CH4 and N2O emissions with the complete time series 
# created from the blended CEDS and default emissions. 
L1_emiss_data %>% 
  # Add in the missing values.
  bind_rows(missing_emissions) %>% 
  select(year, variable, value, units) %>% 
  mutate(scenario = "historical") %>% 
  extend_to_1745 -> 
  complete_historical_emissions

write.csv(complete_historical_emissions, 
          file = file.path(DIRS$L2, "hector_ceds.csv"), row.names = FALSE)


# 6. Save Output Table ---------------------------------------------------------
input_table <- write_hector_csv(x = complete_historical_emissions,
                                write_to = DIRS$TABLES, 
                                info_source = "hector_ceds")

write_hector_ini(input_table)
