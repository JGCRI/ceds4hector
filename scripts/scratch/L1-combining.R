# Combine CEDS and the RCMIP emissions, this is just a draft! Not very well
# coded
# OC emissions are looking weird... Why ugh!
# Same with NOx... I think the issue is may be related to the fact that the
# not all the time series go all the way back to 1750 which then causes some
# problems...

# Okay I am not feeling good about the NOX, N2O, and what not fml


#  [1] "BC"    "CH4"   "CO"    "CO2"   "N2O"   "NH3"   "NMVOC" "NOx"   "OC"    "SO2"

# Things that I think look good
## BC, OC, NH3, CO, CH4, SO2, CO2


# Things that I am unsure about
## NMVOC, N2O, NOx

# Still left todo
## figure out a better way to handle the NAs pre...
## need to get the LUC emissions from the RCMIP

# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "L0.constants.R"))

# Read in data!
ceds_emiss <- read.csv(file.path(DIRS$L0, "L0.ceds_global.csv"))
rcmip_emiss <- read.csv(file.path(DIRS$L0, "L0.rcmip_ceds_emiss.csv"))

# 1. Main Chunk ----------------------------------------------------------------
rcmip_emiss %>%
    select(scenario) %>%
    distinct() %>%
    mutate(id = 1) ->
    scns_to_copy

ceds_emiss %>%
    mutate(id = 1) %>%
    full_join(scns_to_copy,
              relationship = "many-to-many",
              by = join_by(id)) %>%
    select(-id) ->
    ceds_emiss_scenarios

# This might be a tad excessive but until there is the full mapping set up
# lets check to make sure that we are only working the RCMIP values
# that correspond to the CEDS values.
ceds_emiss_scenarios %>%
    select(scenario, em, year) %>%
    distinct() %>%
    mutate(id = 1) ->
    ceds_emiss_yrs

# Subset the rcmip values so that it only includes values
# for the years that we also have CEDS data for.
rcmip_emiss %>%
    mutate(id = 1) %>%
    # This is not the right kind of join I don't think... here
    left_join(ceds_emiss_yrs) %>%
    select(-id) ->
    rcmip_emiss_subset

# Aggregate the CEDS and RCMIP data together!
ceds_emiss_scenarios %>%
    bind_rows(rcmip_emiss_subset) %>%
    group_by(scenario, em, year) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup %>%
    # TODO this should be a mapping file
    mutate(value = value * 0.001) %>%
    na.omit %>%
    filter(year <= 2020)  ->
    total_emissions

# What are the species that do not need to be blended with RCMIP
ceds_emiss %>%
    filter(em %in% c("N2O")) %>%
    mutate(value = value * (28/44) * 0.001) ->
    n2o_ceds_emiss


# BC EMISSIONS ---------------------------------------
system.file(package = "hector", "input/tables/ssp119_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    select(year = Date, value = BC_emissions) %>%
    filter(year %in% 1750:2016) ->
    hector_bc

ggplot() +
    geom_line(data = filter(total_emissions, em == "BC" & scenario == "ssp245"),
              aes(year, value, color = "ceds update")) +
    geom_line(data = hector_bc, aes(year, value, color = "hector")) +
    labs(title = EMISSIONS_BC())



# OC EMISSIONS  ---------------------------------------
system.file(package = "hector", "input/tables/ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    select(year = Date, value = OC_emissions) %>%
    filter(year %in% 1750:2016) ->
    hector_oc

ggplot() +
    geom_line(data = filter(total_emissions, em == "OC" & scenario == "ssp245"),
              aes(year,  value, color = "update ceds")) +
    geom_line(data = hector_oc, aes(year, value, color = "hector")) +
    labs(title = EMISSIONS_OC())


# NH3 EMISSIONS  ---------------------------------------
system.file(package = "hector", "input/tables/ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    select(year = Date, value = NH3_emissions) %>%
    filter(year %in% 1750:2016) ->
    hector_nh3

ggplot() +
    geom_line(data = filter(total_emissions, em == "NH3" & scenario == "ssp245"),
              aes(year,  value, color = "ceds update")) +
    geom_line(data = hector_nh3, aes(year, value, color = "hector")) +
    labs(title = EMISSIONS_NH3())

# NMVOC EMISSIONS  ---------------------------------------
system.file(package = "hector", "input/tables/ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    select(year = Date, value = NMVOC_emissions) %>%
    filter(year %in% 1750:2016) ->
    hector_nmvoc

plt_yrs <- 1900:2022

ggplot() +
    geom_line(data = filter(total_emissions, em == "NMVOC" & scenario == "ssp245" & year %in% plt_yrs),
              aes(year,  value, color = "ceds update")) +
    geom_line(data = filter(hector_nmvoc, year %in% plt_yrs), aes(year, value, color = "hector")) +
    labs(title = EMISSIONS_NMVOC())


# CO EMISSIONS ---------------------------------------
system.file(package = "hector", "input/tables/ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    select(year = Date, value = CO_emissions) %>%
    filter(year %in% 1750:2016) ->
    hector_co

ggplot() +
    geom_line(data = filter(total_emissions, em == "CO" & scenario == "ssp245"),
              aes(year, value, color = "ceds update")) +
    geom_line(data = hector_co, aes(year, value, color = "hector")) +
    labs(title = EMISSIONS_CO())

# CH4 EMISSIONS ---------------------------------------
system.file(package = "hector", "input/tables/ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    select(year = Date, value = CH4_emissions) %>%
    filter(year %in% 1750:2016) ->
    hector_ch4

ggplot() +
    geom_line(data = filter(total_emissions, em == "CH4" & scenario == "ssp245"),
              aes(year, value, color = "ceds update")) +
    geom_line(data = hector_ch4, aes(year, value, color = "hector")) +
    labs(title = EMISSIONS_CH4())
NULL

# N2O EMISSIONS ------------------------------------------------

system.file(package = "hector", "input/tables/ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    select(year = Date, value = N2O_emissions) %>%
    filter(year %in% 1750:2016) ->
    hector_n2o


ggplot() +
    geom_line(data = filter(n2o_ceds_emiss, em == "N2O"),
              aes(year, value, color = "ceds update")) +
    geom_line(data = hector_n2o, aes(year, value, color = "hector")) +
    labs(title = EMISSIONS_N2O())
NULL

# NOX EMISSIONS ------------------------------------------------
system.file(package = "hector", "input/tables/ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    select(year = Date, value = NOX_emissions) %>%
    filter(year %in% 1750:2016) ->
    hector_nox

ggplot() +
    geom_line(data = filter(global_values, em == "NOx"),
              aes(year, (value/478), color = "ceds update")) +
    geom_line(data = hector_nox, aes(year, value, color = "hector")) +
    labs(title = EMISSIONS_NOX())
NULL



# SO2 EMISSIONS ------------------------------------------------

system.file(package = "hector", "input/tables/ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    select(year = Date, value = SO2_emissions) %>%
    filter(year %in% 1750:2025) ->
    hector_so2


ggplot() +
    geom_line(data = filter(total_emissions, em == "SO2"),
              aes(year, (value*500), color = "ceds update")) +
    geom_line(data = hector_so2, aes(year, value, color = "hector")) +
    labs(title = EMISSIONS_SO2())


# CO2 EMISSIONS ---------------------------------------
# kt CO2 to "Pg C/yr"

cf <- 0.000000273
system.file(package = "hector", "input/tables/ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    select(year = Date, value = ffi_emissions) %>%
    filter(year %in% 1750:2025) ->
    hector_co2

ceds_emiss %>%
    filter(em == "CO2")


ggplot() +
   geom_line(data = filter(ceds_emiss, em == "CO2"),
             aes(year, value * cf, color = "ceds update")) +
    geom_line(data = hector_co2, aes(year, value, color = "hector")) +
    labs(title = "CO2 Emissions")
NULL

