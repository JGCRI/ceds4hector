# Read in R packages and define project constants
# 0. Load packages -------------------------------------------------------------
library(assertthat)
library(data.table)
library(dplyr)
library(here)
library(tidyr)
library(zoo)
library(hector)

# TODO probably use a package manager but for now this is probably good enough
stopifnot(packageVersion("dplyr") == "1.1.4")
stopifnot(packageVersion("tidyr") == "1.3.1")
stopifnot(packageVersion("here") == "1.0.1")
stopifnot(packageVersion("zoo") == "1.8.12")

zoo
# packages that are probably not going to be required but could be helpful during
# the developmental stage.
library(ggplot2)

# 1. Set Up Directories --------------------------------------------------------
BASE <-  here::here()
DIRS <- list(DATA = file.path(BASE, "data"),
             RAW_DATA = file.path(BASE, "data", "raw-data"),
             MAPPING = file.path(BASE, "data", "mapping"),
             L0 = file.path(BASE, "data", "L0"),
             L1 = file.path(BASE, "data", "L1"),
             TABLES = file.path(BASE, "inputs", "tables"))

sapply(DIRS, dir.create, showWarnings = FALSE, recursive = TRUE)


# # 2. Define Constants ----------------------------------------------------------
#
# # All of the CEDS species, these are the emissions that will have to be blended
# # with the RCMIP species
# CEDS_EMISS <- c("BC", "CH4", "CO", "CO2", "N2O", "NH3", "NMVOC", "NOx", "OC", "SO2")
#
# # This is the final year of the the CEDS data!
# CEDS_FINAL_YEAR <- 2022
#
#
#
# # 3. Common Functions ----------------------------------------------------------
#
# # TODO should generalize to make more useful
# # Extend the emissions until 1745
# # Args
# #   x: data.frame emissions that start in 1750
# # Return: data.frame that has extended the emissions until 1745
# extend_to_1745 <- function(x){
#
#     new_start_yr <- 1745
#     to_copy <- filter(x, year == 1750)
#
#     scn <- rep(x = to_copy$scenario, each = 5)
#     yr <-  rep(1745:1749, length.out = length(scn))
#     val <- rep(x = to_copy$value, each = 5)
#     var <- rep(x = to_copy$variable, each = 5)
#     u <- rep(x = to_copy$units, each = 5)
#
#     data.frame(scenario = scn,
#                year = yr,
#                value = val,
#                variable = var,
#                units = u) ->
#         early_eimss
#
#     rbind(x, early_eimss) %>%
#         arrange(scenario, variable, year) ->
#         out
#
#     return(out)
#
# }
