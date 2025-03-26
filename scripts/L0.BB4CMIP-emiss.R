# Description: Read in the global biomass burning emissions for CMIP6 (BB4CMIP)
# these emissions will be combined with other CEDS and other anthropogenic
# time series to get the total global emissions that will be used in Hector. 
# The time series were extended to 2023 by repeating the 2015 values. 
#
# 
# Data Source: van Marle, M. J. E., Kloster, S., Magi, B. I., Marlon, J. R., 
# Daniau, A.-L., Field, R. D., Arneth, A., Forrest, M., Hantson, S., Kehrwald, 
# N. M., Knorr, W., Lasslop, G., Li, F., Mangeon, S., Yue, C., Kaiser, J. W., 
# and van der Werf, G. R.: Historic global biomass burning emissions for CMIP6 
# (BB4CMIP) based on merging satellite observations with proxies and fire models (1750–2015), Geosci. Model Dev., 10, 3329-3357, https://doi.org/10.5194/gmd-10-3329-2017, 2017.
# Data extracted by country using tiled, gridded country masks as used in CEDS:
#  http://www.globalchange.umd.edu/ceds/
#  
#  Extraction from version 1.2 of open burning emissions downloaded from:
#  https://esgf-node.llnl.gov/projects/input4mips/ by L. Feng, Oct 20, 2017.

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

    # assert that the data only contains one type of data
    ver <- unique(d$ver)
    stopifnot(length(ver) == 1)

    sector <- unique(d$sector)
    stopifnot(length(sector) == 1)

    # Determine emission species
    em <- unlist(strsplit(basename(f), split = "_|-"))[[1]]

    # Extract the global values
    inds <- which(grepl(x = colnames(d), pattern = "X"))
    value <- as.vector(colSums(d[, inds]))
    years <- as.vector(as.numeric(gsub(x = colnames(d)[inds], pattern = "X", replacement = "")))
    units <- "Tg"

    # Format output
    out <- data.frame(year = years, sector, variable = em, value, units)
    return(out)


}

# 1. Main Chunk ----------------------------------------------------------------
# Import raw data files
file.path(DIRS$RAW_DATA, "1750-2015_v1.2_Emissions_bulk_em") %>%
  list.files(pattern = "csv", full.names = TRUE) -> 
  files

# Assert that all the files exists, if this throws an error 
# see the read me in the data-raw directory. 
stopifnot(all(file.exists(files)))

files %>% 
    lapply(load_BB4CMIP_fxn) %>%
    do.call(what = "rbind") ->
    emissions

# Rad in the mapping file
file.path(DIRS$MAPPING, "L0.BB4CMIP_hector_mapping.csv") %>%
    read.csv(comment.char = "#") ->
    mapping

# Use the mapping file to aggregate the open burning emissions and
# convert to Hector emissions.
emissions %>%
    inner_join(mapping, by = join_by(sector, variable)) %>%
    mutate(value = value * cf) %>%
    summarise(value = sum(value), .by = c("year", "hector_variable",
                                          "hector_units", 
                                          "hector_sector")) %>%
    select(year, sector = hector_sector,
           variable = hector_variable, value, units = hector_units) %>%
    mutate(source = "BB4CMIP") ->
  emissions_till_2015

out <- uniform_extend_df(emissions_till_2015, 2010:2015, extend_to = CEDS_FINAL_YEAR)

# Save the emissions from the global carbon project
write.csv(out, file = file.path(DIRS$L0, "L0.BB4CMIP_emissions.csv"), row.names = FALSE)

# Z. Comparison with Hector inputs ---------------------------------------------
if(FALSE){

    # TODO remove this eventually after feel good about things
    source(here::here("scripts", "dev", "hector_comp_data.R"))
    hector_comp$source <- "hector"

    em <- EMISSIONS_CH4()

    file.path(DIRS$L0, "L0.CEDS_emissions.csv") %>%
        read.csv() ->
        ceds
    
    out %>%  
      filter(variable == em) %>% 
      ggplot(aes(year, value)) + 
      geom_line() + 
      labs(title = em, y = getunits(em))
    
    out %>%
        bind_rows(ceds) %>%
        filter(variable == em) %>%
        summarise(value = sum(value, na.rm = TRUE), .by = c("year", "variable")) %>%
        mutate(source = "CEDS + BB4CMIP") ->
        out2

    hector_comp %>%
    bind_rows(out2) %>%
        filter(variable == em) %>%
      # filter(year <= 2015) %>%
        ggplot() +
        geom_line(aes(year, value, color = source)) +       
      labs(title = em, y = getunits(em))


}

