# Objective here is to read in the CEDS data for a single version change the formatting
# from wide to long and convert to the proper units.
# TODO?
# might want to consider making it easier to process different CEDS versions?
# Would we want to make some sort of mapping files for combining the different
# data sources or do it by hand? For the sake of expediency I am going to try
# and select by hand....

# 0. Set Up -------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "L0.constants.R"))

# Find all the global sector ceds emissions for a specific release version! Error
# will be thrown if the data has not already been downloaded.
# Args
#   DIR: str path to where the ceds data files live
#   ceds_v: str of the release version, I think it might typically be a date
# Returns the str of the full ceds path
find_my_ceds_files <- function(DIR, ceds_v = "v2024_07_08"){

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

# get the total global emissions per emissions species from a global sector emissions file
# Args
#   f: str full file path to the global sector emissions file
# Returns: the ceds aggregate emissions values (the sector information has been removed)
get_ceds_global_emissions <- function(f){

    d <- read.csv(f)

    em <-  unique(d[["em"]])
    units <- unique(d[["units"]])

    vals_df <-  select(d, -em, -sector, -units)
    total_values <- apply(X = vals_df, MARGIN = 2, FUN = sum)
    Xyrs <- names(total_values)
    yrs <- as.integer(gsub(pattern = "X", replacement = "", x = Xyrs))

    out <- data.frame("value" = total_values,
                      "year" = yrs,
                      "variable" = em,
                      "units" = units,
                      "row.names" = NULL)
    return(out)
}


# 1. Main Chunk ----------------------------------------------------------------
lapply(find_my_ceds_files(DIRS$RAW_DATA),
       FUN = get_ceds_global_emissions) %>%
    do.call(what = "rbind") %>%
    rename(em = variable, ceds_units = units) ->
    global_values

write.csv(global_values, file = file.path(DIRS$L0, "L0.ceds_global.csv"), row.names = FALSE)


#
# global_values %>%
#     select(ceds_variable, ceds_units) %>%
#     distinct() %>%
#     write.csv("ceds_hector_mapping.csv", row.names = FALSE)
#
#
#
# ini <- system.file(package = "hector", "input/hector_ssp245.ini")
# hc <- newcore(ini)
# run(hc)
# vars <- c(EMISSIONS_BC(), EMISSIONS_CH4(), EMISSIONS_CO(), FFI_EMISSIONS(),
#           EMISSIONS_N2O(), EMISSIONS_NH3(), EMISSIONS_NMVOC(), EMISSIONS_NOX(),
#           EMISSIONS_OC(), EMISSIONS_SO2())
# fetchvars(hc, 1990, vars = vars) %>%
#     select(variable, units) %>%
#     distinct() %>%
#     mutate(em = gsub(pattern = "_emissions", x = variable, replacement = ""),
#            hector_units = units) %>%
#     select(em, hector_units) ->
#     hector_units_df
#
# global_values %>%
#     filter(em %in% c("BC", "CH4", "CO", "N2O", "NH3", "NMVOC", "OC")) %>%
#     mutate(value = value * 0.001) ->
#     converted_ceds
#
# global_values %>%
#     select(em, ced_units = units) %>%
#     distinct ->
#     ceds_units
#
# hector_units_df %>%
#     inner_join(ceds_units)
#
#
#
# vars <- c(EMISSIONS_BC(), EMISSIONS_CH4(), EMISSIONS_CO(), EMISSIONS_N2O(),
#           EMISSIONS_NH3(), EMISSIONS_NMVOC(), EMISSIONS_OC())
# fetchvars(hc, 1750:2025, vars = vars) %>%
#     mutate(em = gsub(pattern = "_emissions", x = variable, replacement = "")) ->
#     hector_emiss
#
# converted_ceds %>%
#     mutate(value = if_else(em == "CO", value /28.01, value)) ->
#     converted_ceds
#
# ggplot() +
#     geom_line(data = converted_ceds, aes(year, value, color = "ceds")) +
#     geom_line(data = hector_emiss, aes(year, value, color = "hector")) +
#     facet_wrap("em", scales = "free")
