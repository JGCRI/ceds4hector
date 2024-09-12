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

# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Read in data!
ceds_emiss <- read.csv(file.path(DIRS$L0, "L0.ceds_emiss.csv"))
rcmip_emiss <- read.csv(file.path(DIRS$L0, "L0.rcmip_ceds_emiss.csv"))

# Load mapping file
mapping <- read.csv(file.path(DIRS$MAPPING, "CEDS_Hector_mapping.csv"),
                    comment.char = "#")

# 1. Main Chunk ----------------------------------------------------------------
# One of the differences between the CEDS and RCMIP data is that CEDS lacks
# scenario information, since we want to use CEDS data with all of the different
# scenarios we will need to add the scenario information to the CEDS data frame.

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
    filter(year <= 2020) %>%
    rename(ceds_variable = em, ceds_value = value) ->
    combo_emissions

# Map the CEDS to Hector emission names and units
combo_emissions %>%
    inner_join(mapping, by = "ceds_variable") %>%
    mutate(value = ceds_value * cf) %>%
    select(scenario, year, value, variable = hector_variable, units = hector_units) ->
    ceds_hector_impcomplete

write.csv(ceds_hector_impcomplete,
          file = file.path(DIRS$L1, "L1.incomplete_ceds_hector.csv"),
          row.names = FALSE)


