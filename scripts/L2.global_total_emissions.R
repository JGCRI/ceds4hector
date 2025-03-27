# Description: Read in the CEDS L1 emissions and add add in the missing 
# historical emissions from the default V3.2 table. 

# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# 1. Load Data -----------------------------------------------------------------
# Load the L1 emissions which combine CEDS and open burning emissions. 
L1_emiss_data <- read.csv(file.path(DIRS$L1, "ceds_burnning_emiss.csv"))

<<<<<<< HEAD
# Read in the default V3.2 emissions and limit until the year 2023. 
=======
# Read in the default V3.2 emissions and limit until the year 2022. 
>>>>>>> main
read.csv(file.path(DIRS$L0, "L0.V32_emissions.csv")) %>% 
  filter(year <=  max(L1_emiss_data$year)) -> 
  default_emiss

# 2. Input emissions missing from CEDS -----------------------------------------
# Subset the default emissions into the types missing from the L1 data set. 
default_emiss %>% 
  filter(!variable %in% L1_emiss_data$variable) -> 
  missing_emissions

<<<<<<< HEAD
# 2. Combine Emissions ---------------------------------------------------------
=======
# 3. CH4 Emissions ------------------------------------------------------------
# There are several emissions that are missing results from the early years.
# Aka N2O and CH4 emissions from CEDS stop earlier than 1750. In this section 
# we will blend the default historical emissions with the CEDS time series. 
L1_emiss_data %>% 
  filter(variable == EMISSIONS_CH4()) %>% 
  mutate(source = "ceds") %>% 
  na.omit -> 
  ceds_ch4_incomplete

ceds_first_yr <- min(ceds_ch4_incomplete$year)
NA_yrs <- ceds_first_yr - 5

default_emiss %>% 
  filter(variable == EMISSIONS_CH4()) %>% 
  mutate(source = "default") %>% 
  # Only keep the values less than that of the 
  # first year of CEDS observations
  filter(year < ceds_first_yr)  %>% 
  # Replace the years we would like to interpolate 
  # over with NAs. This is going to be 5 years. 
  mutate(value = if_else(year >= NA_yrs, NA, value)) -> 
  default_ch4

# Combine the methane data frame together and use linear interpolation 
# to replace the NA values. 
bind_rows(ceds_ch4_incomplete, default_ch4) %>% 
  mutate(value = na.approx(value, year)) -> 
  complete_ch4_timeseries

# 4. N2O Emissions -------------------------------------------------------------
L1_emiss_data %>% 
  filter(variable == EMISSIONS_N2O()) %>% 
  mutate(source = "ceds") %>% 
  na.omit -> 
  ceds_n2o_incomplete

ceds_first_yr <- min(ceds_n2o_incomplete$year)
NA_yrs <- ceds_first_yr - 10

default_emiss %>% 
  filter(variable == EMISSIONS_N2O()) %>% 
  mutate(source = "default") %>% 
  # Only keep the values less than that of the 
  # first year of CEDS observations
  filter(year < ceds_first_yr) %>% 
  # Replace the years we would like to interpolate 
  # over with NAs. This is going to be 10 years because 
  # of how different the CEDS and the historical emissions were.
  mutate(value = if_else(year >= NA_yrs, NA, value)) -> 
  default_n2o

# Combine the methane data frame together and use linear interpolation 
# to replace the NA values. 
bind_rows(ceds_n2o_incomplete, default_n2o) %>% 
  mutate(value = na.approx(value, year)) -> 
  complete_n2o_timeseries

# 5. Combine Emissions ---------------------------------------------------------
>>>>>>> main

# Replace the L1 CH4 and N2O emissions with the complete time series 
# created from the blended CEDS and default emissions. 
L1_emiss_data %>% 
<<<<<<< HEAD
=======
  filter(!variable %in% c(EMISSIONS_CH4(), EMISSIONS_N2O())) %>% 
  bind_rows(complete_ch4_timeseries, 
            complete_n2o_timeseries) %>% 
>>>>>>> main
  # Add in the missing values.
  bind_rows(missing_emissions) %>% 
  select(year, variable, value, units) %>% 
  mutate(scenario = "historical") %>% 
  extend_to_1745 -> 
  complete_historical_emissions

<<<<<<< HEAD
write.csv(complete_historical_emissions, 
          file = file.path(DIRS$L2, "hector_ceds.csv"), row.names = FALSE)


=======
>>>>>>> main
# 6. Save Output Table ---------------------------------------------------------
input_table <- write_hector_csv(x = complete_historical_emissions,
                                write_to = DIRS$TABLES, 
                                info_source = "hector_ceds")

write_hector_ini(input_table)
