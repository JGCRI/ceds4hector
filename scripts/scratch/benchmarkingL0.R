# Benchmarking the RCMIP L0 script outputs, basically I want to check to see
# if the units are correct/that we have pulled all the emissions to match old hector versions...

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(hector)
library(ggplot2)


BASE_DIR <- here::here()


# 1. the non CEDS RCMIP Emisions --------------------------------------------------
# The rcmip non ceds emissions should be equal to the historical hector emissions
file.path("./data/L0_data/rcmip_nonceds_emiss.csv") %>%
    read.csv() %>%
    select(year, value, variable) ->
    rcmip_data

# Quick run Hector to see what the historical emissions are for these!
hc <- newcore(system.file(package = "hector", "input/hector_ssp245.ini"))
run(hc, runtodate = 2020)
fetchvars(hc, dates = 1750:2020, vars = unique(rcmip_data$variable)) %>%
    rename(hector_value = value) ->
    hector_data

hector_data %>%
    left_join(rcmip_data) %>% head() %>%
    mutate(dif = abs(hector_value - value)) ->
    difference_df

assert_that(sum(difference_df$dif) == 0, msg = "Problem with the non CEDS RCMIP Emiss")
assert_that(mean(difference_df$dif) == 0, msg = "Problem with the non CEDS RCMIP Emiss")


