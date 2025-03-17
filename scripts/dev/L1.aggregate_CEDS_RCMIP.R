# When necessary combine the add the ag waste and open burning data from
# RCMIP emissions to CEDS emissions. The goal here is to have a complete
# global emissions for the CEDS periods and convert to Hector units.
# The next step will be to blend with the full time series so the emissions
# time series are long enough to us in a Hector run.
# TODO
# A) May want to rethink the mapping file
# B) May want to revisit the interpolation/extrapolation method
# C) How do we want to deal with the scenarios? This might be different
#       for gcamdata we probably do not care as much about the blending with
#       the future scenario parts since those emissions will be passed from
#       GCAM but there will still be some emissions that will have to be set
#       to default for the future period if they are not produced by GCAM.
# The remaining TODOs are related to potential areas where code could be automated.

# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Read in data!
ceds_emiss <- read.csv(file.path(DIRS$L0, "L0.ceds_emiss.csv"))
rcmip_emiss <- read.csv(file.path(DIRS$L0, "L0.rcmip_ceds_emiss.csv"))
rcmip_ch4n2o_emiss <- read.csv(file.path(DIRS$L1, "L1.hector_rcmip_emiss.csv")) %>%
    filter(variable %in% c(EMISSIONS_CH4(), EMISSIONS_N2O()))

# Load mapping file
mapping <- read.csv(file.path(DIRS$MAPPING, "CEDS_Hector_mapping.csv"),
                    comment.char = "#")

# 1. Main Chunk ----------------------------------------------------------------
# One of the differences between the CEDS and RCMIP data is that CEDS lacks
# scenario information, since we want to use CEDS data with all of the different
# scenarios we will need to add the scenario information to the CEDS data frame.

# 1.A. Aggregate CEDS & RCMIP emissions together if needed ---------------------
# Save a copy of all the RCMIP scenario names.
rcmip_emiss %>%
    select(scenario) %>%
    distinct() %>%
    mutate(id = 1) ->
    scns_to_copy

# Make a copy of the ceds data for each of the scenarios.
ceds_emiss %>%
    mutate(id = 1) %>%
    full_join(scns_to_copy,
              relationship = "many-to-many",
              by = join_by(id)) %>%
    select(-id) ->
    ceds_emiss_scenarios

# We only want to be interpolating/extrapolating emissions for NAs that
# are near present day, not early on in the historical record.
ceds_emiss_scenarios %>%
    filter(!(year <= 2000 & is.na(value))) ->
    ceds_data

# Now subset the RCMIP data so that it only includes values for
# the years we have the CEDS data for.
ceds_data %>%
    select(em, year) %>%
    distinct() %>%
    mutate(join_on = 1) ->
    ceds_meta_df

rcmip_emiss %>%
    mutate(join_on = 1) %>%
    inner_join(ceds_meta_df, by = c("em", "year", "join_on")) %>%
    select(-join_on) ->
    rcmip_emiss_subset

# Aggregate the CEDS and RCMIP data together!
ceds_data %>%
    bind_rows(rcmip_emiss_subset) %>%
    group_by(scenario, em, year) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup %>%
    na.omit %>%
    filter(year <= 2022) %>%
    rename(ceds_variable = em, ceds_value = value) ->
    combo_emissions

# 1.B. Convert to Hector units --------------------------------------------------
# Map the CEDS to Hector emission names and units
# TODO should need to add some better notes about why this is incomplete.
combo_emissions %>%
    inner_join(mapping, by = "ceds_variable") %>%
    mutate(value = ceds_value * cf) %>%
    select(scenario, year, value, variable = hector_variable, units = hector_units) ->
    ceds_hector_impcomplete

# We need to extrapolate the early year values, but not for the CH4 and N2O
# emissions, those are special cases.
# TODO this is not the most robust way to handle this but we are assuming that
# the first CEDS year is 1750.
ceds_hector_impcomplete %>%
    filter(!variable %in% c(EMISSIONS_N2O(), EMISSIONS_CH4())) %>%
    filter(year == 1750)  ->
    emissions_1750

scn <- rep(x = emissions_1750$scenario, each = 5)
yr <-  rep(1745:1749, length.out = length(scn))
val <- rep(x = emissions_1750$value, each = 5)
var <- rep(x = emissions_1750$variable, each = 5)
u <- rep(x = emissions_1750$units, each = 5)

data.frame(scenario = scn,
           year = yr,
           value = val,
           variable = var,
           units = u) ->
    early_eimss

# Right now we are not using CEDS emissions for CH4 and N2O pre 1970.
rbind(early_eimss, ceds_hector_impcomplete) %>%
    arrange(scenario, variable) ->
    ceds_hector_ch4_n2o_incomplete

# 1.C. Complete CH4 & N2O Emissions --------------------------------------------
# Since we are not using the CEDS emissions for CH4 and N2O before 1970 we
# need to insure a smooth transition between the default values and the CEDS
# values.

# Based on expert opinion from S. Smith we want to use 10 years to transition
# between default Hector and CEDS CH4 emissions.
ch4_trans_yrs <- 10

# Figure out the max year of the default Hector emissions we want to keep.
ceds_hector_ch4_n2o_incomplete %>%
    filter(variable == EMISSIONS_CH4()) %>%
    pull(year) %>%
    min ->
    max_ch4_yr

srt_ch4_transtion <- max_ch4_yr - ch4_trans_yrs

# Subset the RCMIP emissions to only include the CH4 emissions.
rcmip_ch4n2o_emiss %>%
    filter(variable == EMISSIONS_CH4()) %>%
    filter(year < max_ch4_yr) %>%
    # Replace the transition years with NAs
    mutate(value = if_else(year >= srt_ch4_transtion, NA, value)) %>%
    # Add the CEDS data and arrange
    bind_rows(filter(ceds_hector_ch4_n2o_incomplete,
                     variable == EMISSIONS_CH4())) %>%
    arrange(year) %>%
    mutate(value = na.approx(value, na.rm = FALSE)) ->
    complete_CH4

# N2O Emissions
# Based on expert opinion from S. Smith we want to start the transition at 1940

# Figure out the max year of the default Hector emissions we want to keep.
ceds_hector_ch4_n2o_incomplete %>%
    filter(variable == EMISSIONS_N2O()) %>%
    pull(year) %>%
    min ->
    max_n2o_yr

srt_n2o_transtion <- 1940

# Subset the RCMIP emissions to only include the N2O emissions.
rcmip_ch4n2o_emiss %>%
    filter(variable == EMISSIONS_N2O()) %>%
    filter(year < max_n2o_yr) %>%
    # Replace the transition years with NAs
    mutate(value = if_else(year >= srt_n2o_transtion, NA, value)) %>%
    # Add the CEDS data and arrange
    bind_rows(filter(ceds_hector_ch4_n2o_incomplete,
                     variable == EMISSIONS_N2O())) %>%
    arrange(year) %>%
    mutate(value = na.approx(value, na.rm = FALSE)) ->
    complete_N2O

# 1.D. Save Output -------------------------------------------------------------
bind_rows(complete_N2O,
          complete_CH4,
          ceds_hector_ch4_n2o_incomplete) ->
    ceds_hector_emiss

write.csv(ceds_hector_emiss,
          file = file.path(DIRS$L1, "L1.ceds_hector.csv"),
          row.names = FALSE)


