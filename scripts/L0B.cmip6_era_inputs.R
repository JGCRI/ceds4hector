# Description: Correct the CMIP6 era NOx emissions! 
# The CMIP6 era CEDS NOx Emissions are in the correct units. Replace rcmip global
# NOX emissions with the rcmip anthropocentric (ffi) NOx + the open burning 
# emissions data set for the CMIP6 historical years. This script does have to be 
# run after the L0.BB4CMIP-emiss.R script is run. 


# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# CMIP6 era future period starts in 2015 
FUTURE_START_YR <- "2015"

# Write a hector emissions input table. 
# Args
#   x: data table containing Hector input values (wide)
#   scn: str name indicating the ssp scenario
#   write_to: str directory to write the hector csv output to
#   info_source: string name indicating the source part of the module name
#   end_tag: string used at the end of of the table file, in most cases will be default name "_emiss-constraints_rf"
#   str: file name
# Return:
save_updated_tables <- function(x, 
                                scn,
                                write_to = DIRS$TABLES, 
                                info_source = "cmip6_corrected", 
                                end_tag = "_emiss"){
  
  # Create the file name
  assert_that(length(scn) == 1)
  fname <- file.path(write_to, paste0(info_source, '_', scn, end_tag, '.csv'))
  
  # Add the header information to the csv table.
  # TODO look into a more efficient way to do this, one that does not
  # require intermediate products to be written out to the disk.
  readr::write_csv(x, fname, append = FALSE, col_names = TRUE)
  lines <- readLines(fname)
  
  # Format a list of units that will be used in the header.
  vars <- names(x)
  var_units <- getunits(vars[-1])
  units_list <- paste(c('; UNITS:', var_units), collapse = ', ')
  
  git_tag <- substr(system("git rev-parse HEAD", intern=TRUE), start = 1, stop = 15)
  create_info <-  c(paste0('; created by ceds4hector on ', date(),
                           " commit ", git_tag))
  final_lines <- append(c(paste0('; ', scn, " from ", info_source),
                          paste0("; commit ", git_tag),
                          paste0("; date ", date()),
                          units_list),
                        lines)
  writeLines(final_lines, fname)
  return(fname)
  
}



# 1. Data ----------------------------------------------------------------------

# Start by making sure the required data files are available. 

# The RCMIP emissions that will provide the FFI NOx emissions. 
rcmip_emissions_file <- file.path(DIRS$RAW_DATA, pattern = "rcmip-emissions-annual-means-v5-1-0.csv")
stopifnot(file.exists(rcmip_emissions_file)) # If this throws an error you need to install the raw-data 

# The open burning emissions. 
open_burning_file <- file.path(DIRS$L0, "L0.BB4CMIP_emissions.csv")
stopifnot(file.exists(open_burning_file)) # If this throws an error you need to run the L0.BB4CMIP-emiss.R script

# Now load the data. 
read.csv(rcmip_emissions_file) %>% 
  filter(Scenario == "historical") %>%
  filter(Variable ==  "Emissions|NOx|MAGICC Fossil and Industrial") %>% 
  filter(Region == "World") %>% 
  pivot_longer(starts_with("X"), names_to = "year") %>% 
  mutate(year = as.integer(gsub("X", "", year))) %>% 
  # Convert from units of N2O to units of N
  mutate(value = value * 14/44) %>% 
  select(year, value) %>% 
  mutate(variable = EMISSIONS_NOX()) -> 
  cmip6_era_antro_nox

# Since the only difference between the CMIP6 and CMIp6 releases of the 
# open burnding emissions is the extension from 2015 - 2023, if we 
# subset the CMIP7 open burning emissions data set to only include 
# the values pre 2015 we essentially have the CMIP6 values we need. 
read.csv(open_burning_file) %>% 
  filter(variable == EMISSIONS_NOX()) %>% 
  filter(year < FUTURE_START_YR) %>% 
  select(year, value, variable) -> 
  cmip6_era_burning


# Get the total NOx emissions for the CMIP6 historical period, these are the 
# values we want to use! 
cmip6_era_antro_nox %>% 
  bind_rows(cmip6_era_burning) %>%  
  summarise(value = sum(value), .by = c("year", "variable")) %>%  
  extend_to_1745 %>%  
  na.omit -> 
  correct_nox_emissions


# Identify the emission tables that we need to correct.
system.file(package = "hector", "input/tables") %>%
  list.files(pattern = "ssp", full.names = TRUE) -> 
  cmip6_emission_files


# For each of the emissions data file replace the historical NOx emissions. 
lapply(cmip6_emission_files, function(f){
  
  d <- read.csv(f, comment.char = ";")
  
  # This might be overkill but confirm that we are replacing 
  # the correct years of data. 
  yrs_to_change <- intersect(correct_nox_emissions$year, d$Date)
  indexs <- which(d$Date %in% yrs_to_change)
  stopifnot(all(d$Date[indexs] == correct_nox_emissions$year))
  
  # Replace the NOx emissions! 
  d$NOX_emissions[indexs] <- correct_nox_emissions$value
  
  # Return the corrected emissions data.table
  return(d)
  
}) -> 
  corrected_emission_data_tables

# Extract the scenario names. 
scn_names <- gsub(pattern = "_emiss-constraints_rf.csv", 
                  replacement = "", 
                  x = basename(cmip6_emission_files))

# Save all emission input tables. 
emissions_tables <- mapply(save_updated_tables, 
                           scn = scn_names,
                           x = corrected_emission_data_tables)

# Make new ini files
lapply(FUN = write_hector_ini, emissions_tables)


# Z. Checking ------------------------------------------------------------------
if(FALSE){
  
  # Compare the cmip6 and ceds historical NOx emissions 
  file.path(DIRS$TABLES, "hector_ceds_historical_emiss.csv") %>% 
    read.csv(comment.char = ";") %>%  
    select(Date, NOX_emissions) %>% 
    mutate(source = "new") -> 
    ceds_nox

  file.path(DIRS$TABLES, "cmip6_corrected_ssp245_emiss.csv") %>% 
    read.csv(comment.char = ";") %>%  
    select(Date, NOX_emissions) %>% 
    mutate(source = "corrected") -> 
    corrected_nox
  
  corrected_nox %>% 
    bind_rows(ceds_nox) %>% 
    ggplot(aes(Date, NOX_emissions, color = source)) + 
    geom_line()
  
}








