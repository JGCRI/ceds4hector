# Description: Read in the global biomass burning emissions for CMIP& (BB4CMIP)
# these emissions will be combined with other CEDS and other anthropogenic
# time series to get the total global emissions that will be used in Hector. 
#
# The time series were extended to 2023 by repeating the mean 2012:2022 emissions.

# TODO need to add the CMIP7 ciatation here
# 
# Data Source: van Marle, M. J. E., Kloster, S., Magi, B. I., Marlon, J. R., 
# Daniau, A.-L., Field, R. D., Arneth, A., Forrest, M., Hantson, S., Kehrwald, 
# N. M., Knorr, W., Lasslop, G., Li, F., Mangeon, S., Yue, C., Kaiser, J. W., 
# and van der Werf, G. R.: Historic global biomass burning emissions for CMIP6 
# (BB4CMIP) based on merging satellite observations with proxies and fire models (1750–2015), Geosci. Model Dev., 10, 3329-3357, https://doi.org/10.5194/gmd-10-3329-2017, 2017.
# Data extracted by country using tiled, gridded country masks as used in CEDS:
#  http://www.globalchange.umd.edu/ceds/
#  


# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# Load the BB4CMIP data files
# Args
#   f: string path to the data file to read in
# Returns: data.frame of global emission per sector
load_BB4CMIP_fxn <- function(f){

    # Each of the files corresponded to a type of emissions single sector.
    d <- read.csv(f)

    # Make sure we have the global values 
    stopifnot(unique(d$region) == "World")
   

    
    # Determine emission species
    em <- unlist(strsplit(basename(f), split = "_|-"))[[1]]

    # Extract the global values
    inds <- which(grepl(x = colnames(d), pattern = "X"))
    
    # Format the output 
    pivot_longer(d, names_to = "year", values_to = "value", cols = inds) %>% 
      mutate(year = as.numeric(gsub(replacement = "", pattern = "X", x = year))) -> 
      out
    
    return(out)


}

# 1. Main Chunk ----------------------------------------------------------------
# Import raw data files
file.path(DIRS$RAW_DATA) %>%
  list.files(pattern = "gfed-bb4cmip_cmip7_global_0011", full.names = TRUE) -> 
  files

# Assert that all the files exists, if this throws an error 
# see the read me in the data-raw directory. 
stopifnot(all(file.exists(files)))

files %>% 
    lapply(load_BB4CMIP_fxn) %>%
    do.call(what = "rbind") ->
    emissions


if(DEBUG){
  write.csv(emissions, file = file.path(DIRS$L0, "L0.BB4CMIP_emissions-raw.csv"), row.names = FALSE)
}




# Rad in the mapping file
file.path(DIRS$MAPPING, "L0.BB4CMIP_hector_mapping.csv") %>%
    read.csv(comment.char = "#") ->
    mapping

# Use the mapping file to aggregate the open burning emissions and
# convert to Hector emissions.
emissions %>%
    inner_join(mapping, by = join_by(variable)) %>%
    mutate(value = value * cf) %>%
    summarise(value = sum(value), .by = c("year", "hector_variable",
                                          "hector_units", 
                                          "hector_sector")) %>%
    select(year, sector = hector_sector,
           variable = hector_variable, value, units = hector_units) %>%
    mutate(source = "BB4CMIP") ->
  emissions_till_2022


out <- uniform_extend_df(emissions_till_2022, 2012:2022, extend_to = CEDS_FINAL_YEAR)


# Save the emissions from the global carbon project
write.csv(out, file = file.path(DIRS$L0, "L0.BB4CMIP_emissions.csv"), row.names = FALSE)

# Z. Comparison with Hector inputs ---------------------------------------------
if(FALSE){

    # TODO remove this eventually after feel good about things
    source(here::here("scripts", "dev", "hector_comp_data.R"))
    hector_comp$source <- "hector"

    em <- EMISSIONS_CH4()


    emissions %>% 
      mutate(value = (28/44) * value) %>% 
      filter(variable == "Emissions|NOx|Biomass Burning") %>% 
      ggplot(aes(year, value)) + 
      geom_line()
    
    # Mt NO2/yr 
      
      
      
      

}

