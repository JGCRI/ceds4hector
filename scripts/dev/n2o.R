# Description: Trying to understand what is going on with the N2O emissions...

# 0. Set Up --------------------------------------------------------------------
# Start from a clean environment
# TODO this would be dropped if written as a function like gcamdata
remove(list = ls())

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# TODO this is temporary but right now the rcmip-emissions are not being pulled
# in atuomatically
"~/Documents/2024/hector-CEDS/data/raw-data/rcmip-emissions-annual-means-v5-1-0.csv" %>%
    read.csv() %>%
    filter(Scenario == "ssp245") %>%
    filter(Region == "World") %>%
    pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "value") %>%
    mutate(year = as.integer(gsub(replacement = "", x = year, pattern = "X"))) %>%
    rename(rcmip_variable = Variable) %>%
    filter(year <= 2020) ->
    all_rcmip_emissions

# Let's confirm that the total emissions are the sum of the two n2o sectoral emisisons.
# yes it is!
all_rcmip_emissions %>%
    filter(rcmip_variable == "Emissions|N2O") %>%
    mutate(value = value * cf) ->
    total_n2o

all_rcmip_emissions %>%
    filter(rcmip_variable %in% c("Emissions|N2O|MAGICC AFOLU",
                                 "Emissions|N2O|MAGICC Fossil and Industrial")) %>%
    mutate(value = value * cf) ->
    # summarise(value = sum(value), .by = year) %>%
    # mutate(rcmip_variable = "sectoral") ->
    sectoral_n2o


cf <- (28.014/ 44.014) * 0.001
ceds %>%
    filter(variable == EMISSIONS_N2O()) %>%
    na.omit ->
    ceds_n2o


ggplot() +
    geom_line(data = total_n2o, aes(year, value)) +
    geom_line(data = sectoral_n2o, aes(year, value, color = rcmip_variable)) +
    geom_line(data = ceds_n2o, aes(year, value, color = "ceds")) +
    theme(legend.position = "bottom")



# Let's confirm that the conversion is working as expected
total_n2o %>%
    mutate(value = value * (28.014/ 44.014) * 0.001) ->
    total_n2o2

system.file(package = "hector", "input/tables/ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";")  %>%
    select(year = Date, value = N2O_emissions) ->
    hector_n2o

ggplot() +
    geom_line(data = total_n2o2, aes(year, value)) +
    geom_line(data = hector_n2o, aes(year, value, color = "hector"), linetype = 2) +
    geom_line(data = ceds_n2o, aes(year, value, color = "ceds"))
