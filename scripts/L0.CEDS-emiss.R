# Description: Read in the CEDS data and convert to Hector units and naming
# conventions. Some of the CEDS emissions are for only certain emissions
# sources (exclude things like biomass burning) and must be combined with 
# other data sources before they can be passed into hector. 
# 
# TODO add citation 



# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Define Helper Functions
# Find all the global sector ceds emissions for a specific release version! Error
# will be thrown if the data has not already been downloaded.
# Args
#   DIR: str path to where the ceds data files live
#   ceds_v: str of the release version, I think it might typically be a date
# Returns the str of the full ceds path
find_my_ceds_files <- function(DIR, ceds_v = "v_2025_03_18"){
  
  assert_that(dir.exists(DIR))
  
  # Find all the possible ceds files
  all_ceds_files <- list.files(DIR, pattern = "CEDS")
  assert_that(length(all_ceds_files) >= 1, msg = "no CEDS files found")
  
  # Now check to see if there are global emissions by sector... it
  # does seem like the CEDS versions have different file naming patterns
  ceds_global_files <- all_ceds_files[grepl(x = all_ceds_files, pattern = "global")]
  ceds_sector <- ceds_global_files[grepl(x = ceds_global_files, pattern = "sector")]
  
  # I do not want to be messing around with the fuel types!
  ceds_sector_global <- ceds_sector[!grepl(x = ceds_sector, pattern = "fuel")]
  files <- ceds_sector_global[grepl(x = ceds_sector_global, pattern = ceds_v)]
  
  assert_that(length(files) >= 1,
              msg = paste0("no global sector emission files found for ", ceds_v))
  
  # Append the file names on to the directory path
  out <- file.path(DIR, files)
  return(out)
}


# 1. Import Data ---------------------------------------------------------------
# Load the mapping file
mapping <- read.csv(list.files(DIRS$MAPPING, pattern = "L0.CEDS_hector_mapping.csv",
                               full.names = TRUE), comment.char = "#")

# Read in all the raw data
find_my_ceds_files(DIRS$RAW_DATA) %>%
  lapply(read.csv) %>%
  do.call(what = "bind_rows") ->
  ceds_raw_data

# 3. Total Global Emissions ----------------------------------------------------
ceds_raw_data %>%
  # subset the data so that it only includes the emissions we are interested.
  filter(em %in% mapping$ceds_variable) %>%
  # Change from wide to long format and make sure that the years are integers
  # in preparation for joining with the mapping file.
  pivot_longer(starts_with("X"), names_to = "year") %>%
  mutate(year = as.integer(gsub(replacement = "", x = year, pattern = "X"))) %>%
  rename(ceds_variable = em, ceds_units = units) %>% 
  inner_join(mapping, by = join_by(ceds_variable)) %>% 
  # Convert to Hector units and aggregate!
  mutate(value = value * cf) %>% 
  summarise(value = sum(value), .by = c("hector_variable", "year", "hector_units",
                                        "hector_sector")) %>%
  select(year, sector = hector_sector,
         variable = hector_variable, value, units = hector_units) %>%
  mutate(source = "CEDS") ->
  out

write.csv(out, file = file.path(DIRS$L0, "L0.CEDS_emissions.csv"), row.names = FALSE)


# Z. Comparison with Hector inputs ---------------------------------------------
# TODO remove this eventually after feel good about things but this
# line will load hector_comp which is the default set of emissions
# that is useful for comparing results with
if(FALSE){
  
  library(ggplot2)
  source(here::here("scripts", "dev", "hector_comp_data.R"))
  
  em_name <- EMISSIONS_CH4()
    
  out %>%
   # filter(grepl(x = ceds_variable, pattern = "CH4")) %>%
    filter(variable == em_name) %>% 
    summarise(value = sum(value), .by = c("variable", "year", "units")) ->
    ceds_emiss
  
  hector_comp %>%
    filter(variable == em_name) ->
    default_emiss
  
  ggplot() +
    geom_line(data = ceds_emiss, aes(year, value, color = "ceds")) +
    geom_line(data = default_emiss, aes(year, value, color = "default"))
}

