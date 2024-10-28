# Import and format the CEDS data into total CEDS global values by
# emission type. Emissions remain in the original units.
# This script assumes that the raw-data has already been downloaded.
#
# TODO
# A) I think this code might depended on CEDS version which is unideal...

# 0. Set Up -------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

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

write.csv(global_values, file = file.path(DIRS$L0, "L0.ceds_emiss.csv"), row.names = FALSE)

