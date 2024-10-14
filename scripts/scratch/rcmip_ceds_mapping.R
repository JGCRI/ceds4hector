# Devloping the mapping file for the RCMIP CEDS mapping file. Some of the stuff
# this code should not be re run and may not be commited to the repo but is potenitally
# helpful, basically I had to do some work to figure out which emissions were subcategories
# of other emission species... okay actually that might be too hard so I think
# I might skill that which is pobably not good

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
#conversion_table <- read.csv(file.path(MAPPING_DIR, "RCMIP_variable-conversion.csv"), comment.char = "#")


# TODO idk about the name of this intermediate data file but it is probably fine
# for now...
L0_DIR <- file.path(DATA_DIR, "L0_data")
dir.create(L0_DIR, showWarnings = FALSE)

# Load the RCMIP emission files
rcmip_file <- list.files(path = DATA_DIR, pattern = "rcmip-emissions-annual-means-v5-1-0.csv",
                         full.names = TRUE, recursive = TRUE)
assert_that(file.exists(rcmip_file), msg = "Missing RCMIP emissions file see set up instructions!")

read.csv(rcmip_file) %>%
    filter(Scenario == "historical") ->
    raw_rcmip_emissions

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

# BC ---------------------------------------------------------------------------

# For now let's start working with the BC emissions.
raw_rcmip_emissions %>%
    # TOD this will evntaully be replaced with a patttern of full CEDS emissions.
    filter(grepl(x = Variable, pattern = "BC")) %>%
    filter(Region == "World") %>%
    gather(key = "year", value = "value", starts_with("X")) %>%
    mutate(year = as.integer(gsub(replacement = "", x = year, pattern = "X"))) ->
    rcmip_emissions_long

rcmip_emissions_long$Variable %>% unique()



# Let's check to see what the specific

rcmip_emissions_long %>%
    filter(Variable == "Emissions|BC") ->
    bc_total

rcmip_emissions_long %>%
    filter(!Variable %in% c("Emissions|BC", "Emissions|BC|MAGICC Fossil and Industrial", "Emissions|BC|MAGICC AFOLU")) %>%
group_by(year) %>%
    summarise(my_value = sum(value)) ->
    my_total

# Check to make sure that we have only the dissagreated values
bc_total %>%
    inner_join(my_total) %>%
    mutate(dif = value - my_value) %>%
    pull(dif) %>% summary()


# Here is the list of all the RCMIP emissions that we determined is all the
# disaggregated sectors.
# Emissions|BC|MAGICC AFOLU|Agricultural Waste Burning
# Emissions|BC|MAGICC AFOLU|Agriculture
# Emissions|BC|MAGICC AFOLU|Forest Burning
# Emissions|BC|MAGICC AFOLU|Grassland Burning
# Emissions|BC|MAGICC AFOLU|Peat Burning
# Emissions|BC|MAGICC Fossil and Industrial|Aircraft
# Emissions|BC|MAGICC Fossil and Industrial|Energy Sector
# Emissions|BC|MAGICC Fossil and Industrial|Industrial Sector
# Emissions|BC|MAGICC Fossil and Industrial|International Shipping
# Emissions|BC|MAGICC Fossil and Industrial|Residential Commercial Other
# Emissions|BC|MAGICC Fossil and Industrial|Solvents Production and Application
# Emissions|BC|MAGICC Fossil and Industrial|Transportation Sector
# Emissions|BC|MAGICC Fossil and Industrial|Waste


system.file(package = "hector", "input/tables/ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    select(year = Date, hector_value = BC_emissions) ->
    hector_vals

# Being paranoid and checking to make sure that the total emissions match the
# hector totals
bc_total %>%
    inner_join(hector_vals) %>%
    mutate(dif = value - hector_value) %>%
    pull(dif) %>% summary()

all_ceds_files <- find_my_ceds_files(DIR = "data/raw-data")
bc_emiss <- read.csv(all_ceds_files[grepl(pattern = "BC_", x = all_ceds_files)])

bc_emiss %>%
  #  filter(grepl(x = sector, pattern = "3")) %>%
    pull(sector) %>% unique()
all_ceds_files %>% basename()


# CO ---------------------------------------------------------------------------

raw_rcmip_emissions %>%
    #mutate(Variable = gsub(replacement = "", x = Variable, pattern = "\\|")) %>%
    filter(grepl(x = Variable, pattern = "EmissionsOCMAGICC")) %>%
    pull(Variable) %>% unique()

# NH3 ---------------------------------------------------------------------------

raw_rcmip_emissions %>%
    filter(grepl(x = Variable, pattern = "NH3")) %>%
    select(Variable, Unit) %>% distinct()

# NMVOC ---------------------------------------------------------------------------

raw_rcmip_emissions %>%
    filter(grepl(x = Variable, pattern = "VOC")) %>%
    select(Variable, Unit) %>% distinct()

global_values$em %>% unique()

# CH4 ---------------------------------------------------------------------------
raw_rcmip_emissions %>%
    filter(grepl(x = Variable, pattern = "CH4")) %>%
    select(Variable, Unit) %>% distinct()

# OC --------------------------
raw_rcmip_emissions %>%
    mutate(Variable = gsub(replacement = "", x = Variable, pattern = "\\|")) %>%
    filter(grepl(x = Variable, pattern = "EmissionsCO")) %>%
    select(Variable, Unit) %>% distinct()

# N2O --------------------------
raw_rcmip_emissions %>%
    filter(grepl(x = Variable, pattern = "N2O")) %>%
    select(Variable, Unit) %>% distinct()

# NOx  --------------------------
raw_rcmip_emissions %>%
    filter(grepl(x = Variable, pattern = "NOx")) %>%
    select(Variable, Unit) %>% distinct()

# SO2 --------------------------
raw_rcmip_emissions %>%
    filter(grepl(x = Variable, pattern = "Sulfur")) %>%
    select(Variable, Unit) %>% distinct()

