# Import and format the RCMIP sectoral data set.
# TODO: How do I want to handle the fact that we are going to need to aggregate
# across sources of information?? do I want to do a mapping file? I don't particuarlly
# love that the accregation is unclear.... does everythign that


# 0. Set Up -------------------------------------------------------------------
library(assertthat)
library(dplyr)
library(tidyr)


# TODO probably use a package manager but for now this is probably good enough
stopifnot(packageVersion("dplyr") == "1.1.4")
stopifnot(packageVersion("tidyr") == "1.3.1")


# packages that are probably not going to be required but could be helpful during
# the developmental stage.
library(ggplot2)
library(hector)


# TODO move all the relative paths and the lib loads to a the constants file? perhaps
DATA_DIR <- here::here("data")
RD_DIR <- file.path(DATA_DIR, "raw-data")
MAPPING_DIR <- file.path(DATA_DIR, "mapping")

# Load the project constants and basic functions
source(here::here("scripts", "L0.constants.R"))
conversion_table <- read.csv(file.path(MAPPING_DIR, "RCMIP_variable-conversion.csv"), comment.char = "#")


# TODO idk about the name of this intermediate data file but it is probably fine
# for now...
L0_DIR <- file.path(DATA_DIR, "L0_data")
dir.create(L0_DIR, showWarnings = FALSE)






# 1. Main Chunk -------------------------------------------------------------------

# Load the RCMIP emission files
rcmip_file <- list.files(path = DATA_DIR, pattern = "rcmip-emissions-annual-means-v5-1-0.csv",
           full.names = TRUE, recursive = TRUE)
assert_that(file.exists(rcmip_file), msg = "Missing RCMIP emissions file see set up instructions!")

read.csv(rcmip_file) %>%
    filter(Scenario == "historical") ->
    raw_rcmip_emissions


# Split the RCMIP emissions that will need to be combined with the CEDS data
# and the other emissions that will be RCMIP only.

# Save a copy of the CEDS emission species that can be used in grepl search,
# and because RCMIP has some different notation when it comes to SO2 and NMVOC
# add those strings to the search pattern manually.
ceds_emiss_serach_pattern <- paste0(c(CEDS_EMISS, "Sulfur", "VOC"), collapse = "|")

# Gather all of the non CEDS emission species in a data frame and change from
# wide to long orientation, apply the conversion factor and save output.
raw_rcmip_emissions %>%
    filter(!grepl(x = Variable, pattern = ceds_emiss_serach_pattern)) %>%
    filter(Region == "World") %>%
    gather(key = "year", value = "value", starts_with("X")) %>%
    mutate(year = as.integer(gsub(replacement = "", x = year, pattern = "X"))) %>%
    rename(rcmip_variable = Variable) %>%
    # TODO unclear if this is the correct join to do here or not...
    inner_join(conversion_table, by = join_by(rcmip_variable)) %>%
    mutate(value = value * cf) %>%
    select(scenario = Scenario, year = year, value,
           variable = hector_variable, units = hector_unit) ->
    hector_rcmip_nonceds_emiss

write.csv(hector_rcmip_nonceds_emiss, file = file.path(L0_DIR, "rcmip_nonceds_emiss.csv"), row.names = FALSE)






