# Run Hector
# This script does all the Hector runs for the CEDS manuscript.

# 0. Set Up --------------------------------------------------------------------
# Make sure that the correct version of Hector is being installed (not one
# of the devlopmental versions)
remotes::install_github("jgcri/hector@v3.2.0")
library(hector)

# Load extra packages that will be helpful
library(dplyr)
library(ggplot2)

# Define the color scheme, using the IPCC SSP color schemes
SSP_COLORS <- c("ssp119" = "#00a9cf", "ssp126" = "#003466", "ssp245" = "#f69320",
                "ssp370" = "#df0000", "ssp434" = "#2274ae","ssp460" = "#b0724e",
                "ssp585"= "#980002", "CEDS" = "#000000", "historical"="#92397a")

# Set the plot theme
theme_set(theme_bw())


emiss <- c("BC_emissions", "CO_emissions", "NH3_emissions", "NMVOC_emissions",
           "NOX_emissions", "OC_emissions", "SO2_emissions", "ffi_emissions")
conc  <- c(CONCENTRATIONS_CH4(), CONCENTRATIONS_N2O(), CONCENTRATIONS_CO2())
rf <- c(RF_ALBEDO(), RF_CO2(), RF_N2O(), RF_H2O_STRAT(), RF_O3_TROP(), RF_BC(),
        RF_OC(), RF_SO2(), RF_VOL(), RF_CH4(), RF_CF4(), RF_C2F6(),
        RF_HFC23(), RF_HFC32(), RF_HFC4310(), RF_HFC125(), RF_HFC134A(),
        RF_HFC143A(), RF_HFC227EA(), RF_HFC245FA(), RF_SF6(), RF_CFC11(),
        RF_CFC12(), RF_CFC113(), RF_CFC114(), RF_CFC115(), RF_CCL4(), RF_CH3CCL3(),
        RF_HCFC22(), RF_HCFC141B(), RF_HCFC142B(), RF_HALON1211(), RF_HALON1301(),
        RF_HALON2402(), RF_CH3CL(), RF_CH3BR(), RF_NH3(), RF_ACI(), RF_MISC())
temp  <- c(GLOBAL_TAS(), RF_TOTAL())
varlist <- c(emiss, conc, rf, temp)

# My helper function that run Hector and format all data of interest
# Args
#   ini: str path to the ini file
#   name: character string for the scenario name
#   varlist: character vector of Hector variables to report, potentially a long
#       vector which is why it is defined by default
my_run_hector <- function(ini, name, vars = varlist){

    system.file(package = "hector", "input/tables") %>%
        list.files(pattern = name, full.names = TRUE) %>%
        read.csv(comment.char = ";") ->
        inputs

    hc <- newcore(ini, name = name)

    run(hc, runtodate = 2022)
    out <- fetchvars(hc, dates = 1745:2022, vars = varlist)
    return(out)

}

# 1. Default Hector Runs -------------------------------------------------------
inis <- list.files(system.file(package = "hector", "input"),
                   pattern = "ini", full.names = TRUE)
names <- gsub(x = basename(inis), pattern = "hector_|.ini", replacement = "")

mapply(my_run_hector, ini = inis, name = names, SIMPLIFY = FALSE) %>%
    bind_rows() ->
    default_out


# 2. CEDS driven Hector run ----------------------------------------------------

# TODO this path should be updated to where the ceds ini file lives
ini <- here::here("inputs", "hector_ceds_historical.ini")

ceds_out <- my_run_hector(ini, name = "CEDS")



