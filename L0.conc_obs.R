# Import and format the CH4 and N2O concentration observations that are used
# to help extend the CEDS emissions to 1745.
# TODO
# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# 1. Main Chunk ----------------------------------------------------------------

# Import the data and prep for formatting.
file.path(DIRS$RAW_DATA, "Supplementary_Table_UoM_GHGConcentrations-1-1-0_annualmeans_v23March2017.csv") %>%
    read.csv(skip = 23, header = FALSE) %>%
    na.omit() ->
    conc_obs

# Due the the native format of the file manually add the column names here.
# TODO figure out an automatic way to do this?
colnames(conc_obs) <- c("year", "CO2", "CH4", "N2O",
                        "CFC-12a", "HFC-134a", "CFC-11", "CFC-12")

# Since the data set is missing the data starting in 1745, let's use
# the preindustrial conc values used in Hector.
ini <- system.file(package = "hector", "input", "hector_ssp245.ini")
hc  <- newcore(ini)
N2O_0 <- fetchvars(hc, NA, PREINDUSTRIAL_N2O())[["value"]]
CH4_0 <- fetchvars(hc, NA, PREINDUSTRIAL_CH4())[["value"]]
data.frame(year = 1745, CH4 = CH4_0, N2O = N2O_0) %>%
    mutate(year = as.integer(year),
           CH4 = as.double(CH4),
           N2O = as.double(N2O)) ->
    init

# We are also missing data for a few years add them as NAs into the
# obs data frame.
expected_years <- 1746:2015
missing_yrs <- setdiff(expected_years, unique(conc_obs$year))
missing_df <- data.frame(year = missing_yrs,
                         CH4 = NA,
                         N2O = NA) %>%
    mutate(CH4 = as.numeric(CH4),
           N2O = as.numeric(N2O))

# Add the preindustrial values to the observations data frame. Now
# we will have the data frame with the NAs.
conc_obs %>%
    select(year, CH4, N2O)  %>%
    mutate(CH4 = gsub(x = CH4, replacement = "", pattern = ","),
           N2O = gsub(x = N2O, replacement = "", pattern = ,"")) %>%
    mutate(year = as.integer(year),
           CH4 = as.numeric(CH4),
           N2O = as.numeric(N2O)) %>%
    bind_rows(init) %>%
    bind_rows(missing_df) %>%
    arrange(year) ->
    obs_df_wNAs

CH4_noNA <- na.approx(obs_df_wNAs$CH4, na.rm = FALSE)
N2O_noNA <- na.approx(obs_df_wNAs$N2O, na.rm = FALSE)

data.frame(year = obs_df_wNAs$year,
           CH4 = CH4_noNA,
           N2O = N2O_noNA) %>%
    na.omit ->
    full_obs

# Final formatting the observations so that are in the correct format for our helper functions.
names(full_obs) <- c("year", CONCENTRATIONS_CH4(), CONCENTRATIONS_N2O())

# Change from wide to long
full_obs %>%
    pivot_longer(cols = c("CH4_concentration", "N2O_concentration"),
                 names_to = "variable") %>%
    select(year, variable, value) %>%
    mutate(units = if_else(variable == CONCENTRATIONS_CH4(),
                           getunits(CH4_CONSTRAIN()),
                           getunits(N2O_CONSTRAIN()))) ->
    conc_obs

write.csv(conc_obs, file = file.path(DIRS$L0, "L0.conc_obs.csv"), row.names = FALSE)

