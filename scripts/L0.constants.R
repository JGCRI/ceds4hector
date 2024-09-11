# Define project constants, TBD if these should only be for the L0 or global project
# constants. Could also include functions here if that would be helpful but that
# it TBD.

# 0. Load packages -------------------------------------------------------------
library(assertthat)
library(dplyr)
library(tidyr)
library(zoo)


# TODO probably use a package manager but for now this is probably good enough
stopifnot(packageVersion("dplyr") == "1.1.4")
stopifnot(packageVersion("tidyr") == "1.3.1")

# packages that are probably not going to be required but could be helpful during
# the developmental stage.
library(ggplot2)
library(hector)

# 1. Set Up Directories --------------------------------------------------------
BASE <-  here::here()
DIRS <- list(DATA = file.path(BASE, "data"),
             RAW_DATA = file.path(BASE, "data", "raw-data"),
             MAPPING = file.path(BASE, "data", "mapping"),
             L0 = file.path(BASE, "data", "L0"),
             L1 = file.path(BASE, "data", "L1"))

dir.create(DIRS$L0, showWarnings = FALSE)
dir.create(DIRS$L1, showWarnings = FALSE)

# 2. Define Constants ----------------------------------------------------------

# All of the CEDS species, these are the emissions that will have to be blended
# with the RCMIP species
CEDS_EMISS <- c("BC", "CH4", "CO", "CO2", "N2O", "NH3", "NMVOC", "NOx", "OC", "SO2")

# This is the final year of the the CEDS data!
CEDS_FINAL_YEAR <- 2022


