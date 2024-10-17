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
obs_conc <-  read.csv(file.path(DIRS$L0, "L0.conc_obs.csv"))

# Load mapping file
mapping <- read.csv(file.path(DIRS$MAPPING, "CEDS_Hector_mapping.csv"),
                    comment.char = "#")

# Function that uses CH4 concentrations and back calculates the emissions
# Args
#   input_ch4: data.frame of the CH4 concentrations
#   input_nox: data.frame of the NOX emissions
#   input_co: data.frame of the CO emissions
#   input_nmvoc: data.frame of the NMVOC emissions
#   NOX_0: numeric, default set 3.877282 of the 1745 NOX emissions, may want to change to CEDS
#   CO_0: numeric, default set to 348.5274 of the 1745 CO emissions, may want to change to CEDS
#   NMVOC_0: numeric, default set to 60.02183 the 1745 NMVOC emissions, may want to change to CEDS
# Returns: data.frame of emissions that would produce the [CH4] in Hector
get_ch4_emissions <- function(input_ch4, input_nox, input_co, input_nmvoc,
                              NOX_0 =  3.877282, CO_0 = 348.5274, NMVOC_0 = 60.02183){

    # Quality check the inputs, make sure we are working with the right units and
    # that the time series is continuous! over kill but whatever
    assert_that(has_name(x = input_ch4, which = c("year", "variable", "value", "units")))
    # assert_that(are_equal(unique(input_ch4$units), "pppbv CH4"))
    assert_that(are_equal(unique(diff(input_ch4$year)), 1))
    assert_that(has_name(x = input_nox, which = c("year", "variable", "value", "units")))
    assert_that(are_equal(unique(input_nox$units), "Tg N"))
    assert_that(are_equal(unique(diff(input_nox$year)), 1))
    assert_that(has_name(x = input_co, which = c("year", "variable", "value", "units")))
    assert_that(are_equal(unique(input_co$units), "Tg CO"))
    assert_that(are_equal(unique(diff(input_co$year)), 1))
    assert_that(has_name(x = input_nmvoc, which = c("year", "variable", "value", "units")))
    assert_that(are_equal(unique(input_nmvoc$units), "Tg NMVOC"))
    assert_that(are_equal(unique(diff(input_nmvoc$year)), 1))
    assert_that(are_equal(nrow(input_co), nrow(input_ch4)))

    # After making sure that all the inputs match what we need extract the values.
    ch4_conc <- input_ch4$value
    nox_vals <- input_nox$value
    nmvoc_vals <- input_nmvoc$value
    co_vals <- input_co$value

    # Default values from the ini files.
    # CH4 component constants
    M0= 731.41  		# 731.41 ppb preindustrial methane IPCC AR6 Table 7.SM.1, the CH4 forcing equations is calibrated to a M0 of 731.41 ppb
    Tsoil=160 			# lifetime of soil sink (years)
    Tstrat=120          # lifetime of tropospheric sink (years)
    UC_CH4=2.78			# Tg(CH4)/ppb unit conversion between emissions and concentrations
    CH4N=335	        # Natural CH4 emissions (Tgrams)

    # OH component constants
    TOH0=6.6			# inital OH lifetime (years)
    CNOX=0.0042			# coefficent for NOX
    CCO=-0.000105		# coefficent for CO
    CNMVOC=-0.000315	# coefficent for NMVOC (non methane VOC)
    CCH4=-0.32			# coefficent for CH4


    # Modify the concentrations time series to account for
    # for the time lag.
    ch4_conc <- c(NA, ch4_conc)[1:length(nmvoc_vals)]

    # OH lifetime calculations
    a <- CCH4 * log(ch4_conc/M0)
    b <- CNOX*(nox_vals - NOX_0)
    c <- CCO * (co_vals - CO_0)
    d <- CNMVOC*(nmvoc_vals - NMVOC_0)
    toh <- (a + b + c + d)
    tau <- TOH0 * exp( -1 * toh )

    # Calculate the change in CH4 concentrations and pad to make sure
    # it is the correct length.
    dch4 <- c(diff(ch4_conc), NA)
    # Use (S5) to calculate the emissions.
    emiss <- UC_CH4 * (dch4 + (ch4_conc/tau) + (ch4_conc/Tstrat) + (ch4_conc/Tsoil))
    emiss <- emiss - CH4N # remove the natural emissions

    data.frame(year = input_ch4$year,
               value = emiss,
               variable = EMISSIONS_CH4(),
               units = getunits(EMISSIONS_CH4())) %>%
        na.omit() ->
        out

    return(out)

}

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

# Right now the N2O and CH4 emissions are incomplete, we are working on
# some sort of method to add in those emissions.
rbind(early_eimss, ceds_hector_impcomplete) %>%
    arrange(scenario, variable) ->
    ceds_hector_ch4_n2o_incomplete

# TODO
# Need to add chunk of code that extends the CH4 and N2O emissions until 1745.
# Extend CH4 emissions until 1745.
# So the CEDS CH4 emissions start in 1970 and we need them to start in 1745.
# We are able to do this inverting Hector's CH4 concentration calculations to
# figure out what the CH4 emissions are and will use the helper function
# defined above.
ceds_hector_ch4_n2o_incomplete %>%
    filter(variable == EMISSIONS_CH4()) %>%
    pull(year) %>%
    min ->
    ch4_start_yr

# Extract the CH4 concentrations from the observations.
ch4_conc <- filter(obs_conc, (variable == CONCENTRATIONS_CH4() & year < ch4_start_yr))

# Extract the non-methane emissions that are required for this back calculation.
scn <- "historical"
nmvoc_emiss <- filter(ceds_hector_ch4_n2o_incomplete, (variable == EMISSIONS_NMVOC() & scenario == scn & year < ch4_start_yr))
co_emiss <- filter(ceds_hector_ch4_n2o_incomplete, (variable == EMISSIONS_CO() & scenario == scn & year < ch4_start_yr))
nox_emiss <- filter(ceds_hector_ch4_n2o_incomplete, (variable == EMISSIONS_NOX() & scenario == scn & year < ch4_start_yr))

# Save a copy of the initial emissions.
NOX_0 <- nox_emiss$value[[1]]
CO_0 <- co_emiss$value[[1]]
NMVOC_0 <- nmvoc_emiss$value[[1]]

# Calculate the required emissions
get_ch4_emissions(input_ch4 = ch4_conc, input_nox = nox_emiss,
                  input_co = co_emiss, input_nmvoc = nmvoc_emiss,
                  NOX_0 = NOX_0, CO_0 = CO_0, NMVOC_0 = NMVOC_0) ->
    ch4_emissions

ch4_emissions %>%
    filter(year == min(year)) %>%
    mutate(year = 1745) %>%
    rbind(ch4_emissions) ->
    ch4_emissions

# Repeat the emissions data table for X number of scenarios
scns <- unique(ceds_hector_ch4_n2o_incomplete$scenario)
early_ch4_emissions_scns <- do.call(rbind, replicate(length(scns), ch4_emissions, simplify = FALSE))
early_ch4_emissions_scns$scenario <- rep(scns, each = nrow(ch4_emissions))

# Add the early emissions to the other ceds emissions.
ceds_hector_ch4_n2o_incomplete %>%
    bind_rows(early_ch4_emissions_scns) %>%
    arrange(scenario, variable, year) ->
    ceds_hector_n2o_incomplete

# TODO need to figure out what do to about the n2o emissions.
write.csv(ceds_hector_ch4_n2o_incomplete,
          file = file.path(DIRS$L1, "L1.incomplete_ceds_hector.csv"),
          row.names = FALSE)


