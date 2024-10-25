# Import and format the RCMIP data set. We will need a copy of full RCMIP
# global emissions and copies of the ag waste, forest, peatland, and grassland
# burning emissions.
# This script assumes that the raw-data has already been downloaded.
# TODO:
# A) A better CEDS-RCMIP mapping might be better but would take more time for
#       now the mapping file only includes the emissions we know that we
#       need to copy over.
# B) Consider a better way to deal with the recent years data

# 0. Set Up -------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# TODO figure out if these two helper function are redundant of if they do
# different things...

# Use linear interpolation to fill in missing data
# Args
#   d: dataframe for a single emission species
# Returns: data frame ideally with no NAs
# TODO might consider a different way to do this, could do a pad end with constants
# and also interpolate between end points instead of doing linear fit
my_replace_na_fxn <- function(d){
    req <- c("scenario", "em", "year", "units", "value")
    assert_that(all(req %in% colnames(d)), msg = "missing require column")

    # Make sure there is only one scenario, em being processed
    d %>%
        select(scenario, em, units) %>%
        distinct() ->
        meta_data

    assert_that(nrow(meta_data) == 1, msg = "mult emissions or scenarios identified")

    # Make sure that there is only year per data
    assert_that(!any(duplicated(d$year)), msg = "mulitple obs for the same year")

    # Saving some information about the
    na_indx <- which(is.na(d$value))
    na_yrs <- d$year[na_indx]

    fit <- lm(data = tail(d, n = 40), value ~ year)
    new_vals <- predict(object = fit, newdata = data.frame(year = na_yrs))
    d$value[na_indx] <- new_vals

    if(any(is.na(d))){
        warning("Still NAs in ", meta_data[["em"]])
    }

    return(d)

}

# Helper function that will fill in emission emissions
# Args
#   d: data.frame, long format of Hector emissions, year/variable/value/unit/scenario
#   expected_yrs: vector, of the years of data that need the data set to have
# Returns: data.frame, long format of Hector emissions, year/variable/value/unit/scenario for
# all the years defined in the expected_yrs without any NAs.
fill_in_missing_emiss <- function(d, expected_yrs = 1800:2500){

    # Make sure that we are only working with a single scenario
    scn <- unique(d$scenario)

    # Make sure that the length is 1
    assert_that(length(scn) == 1)

    # Change the orientation of the data from wide to long, if one of the variables
    # has all the required years of data this will force NAs into the missing
    # information for the other variables. If not then this function will need
    # to be modified to add in those NAs before interpolation. Errors will be
    # triggered if this is necessary.
    d %>%
        select(year, value, variable) %>%
        tidyr::spread(variable, value) %>%
        mutate(year = as.integer(year)) ->
        wide_data

    missing_yrs <- setdiff(expected_yrs, wide_data$year)
    if(length(missing_yrs) > 0){
        stop("TODO: add code to add the missing required years")
    }

    if(sum(is.na(wide_data)) == 0){
        warning("no NAs to detected, nothing to be interpolated")
    }

    wide_data %>%
        mutate(across(where(is.numeric), ~na.approx(.x, wide_data$year, na.rm = FALSE))) %>%
        pivot_longer(-year, names_to = "variable") %>%
        mutate(units = getunits(variable)) %>%
        arrange(variable, year) %>%
        mutate(scenario = scn) ->
        out

    return(out)

}

# 1. Main Chunk ----------------------------------------------------------------
# Load the RCMIP emission files
rcmip_file_emiss <- list.files(path = DIRS$DATA, pattern = "rcmip-emissions-annual-means-v5-1-0.csv",
                               full.names = TRUE, recursive = TRUE)
assert_that(file.exists(rcmip_file_emiss), msg = "Missing RCMIP emissions file see set up instructions!")

rcmip_file_rf <- list.files(path = DIRS$DATA, pattern = "rcmip-radiative-forcing-annual-means-v5-1-0.csv",
                            full.names = TRUE, recursive = TRUE)

read.csv(rcmip_file_emiss) %>%
    rbind(read.csv(rcmip_file_rf)) %>%
    filter(Scenario == "ssp245") ->
    raw_rcmip_emissions


# 1A. All RCMIP to Hector  -----------------------------------------------------
# Start out by converting all RCMIP emissions to Hector inputs, since we will
# need to blend together potentially some of the older emissions as well and
# blend with the near future values we will want total emissions for each of the
# spieces readily available.
# TODO add the SF, land use albedo and so on into the mix?

# Load the RCMIP to Hector mapping file.
rcmip_hector_mapping <- read.csv(file = file.path(DIRS$MAPPING, "RCMIP_Hector_mapping.csv"),
                                 comment.char = "#") %>%
    select(-cf_notes)

# Gather all of the non CEDS emission species in a data frame and change from
# wide to long orientation, apply the conversion factor and save output.
raw_rcmip_emissions %>%
    filter(Region == "World") %>%
    pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "value") %>%
    mutate(year = as.integer(gsub(replacement = "", x = year, pattern = "X"))) %>%
    rename(rcmip_variable = Variable) ->
    rcmip_long

rcmip_long %>%
    inner_join(rcmip_hector_mapping, by = join_by(rcmip_variable)) %>%
    mutate(value = value * cf) %>%
    select(scenario = Scenario, year = year, value,
           variable = hector_variable, units = hector_unit) %>%
    na.omit ->
    hector_rcmip_nonceds_emiss_some_neg

# Since Hector requires strictly positive emissions the CO2 emissions and uptake
# will need to be modified.
hector_rcmip_nonceds_emiss_some_neg %>%
    filter(variable %in% c(FFI_EMISSIONS(), LUC_EMISSIONS())) %>%
    select(scenario, year, value, variable) %>%
    pivot_wider(id_cols = c(scenario, year), names_from = variable) %>%
    mutate("daccs_uptake" = 0,
           "luc_uptake" = 0) %>%
    mutate(daccs_uptake = ifelse(ffi_emissions < 0, -1 * ffi_emissions, 0),
           luc_uptake = ifelse(luc_emissions < 0, -1 * luc_emissions, 0)) %>%
    mutate(luc_emissions = ifelse(luc_emissions < 0, 0, luc_emissions),
           ffi_emissions = ifelse(ffi_emissions < 0, 0, ffi_emissions)) %>%
    pivot_longer(cols = -c(year, scenario), names_to = "variable") %>%
    mutate(units = getunits(FFI_EMISSIONS())) ->
    hector_rcmip_nonceds_emiss_cc_emiss


# Add the processed co2 emissions to the emissions df.
hector_rcmip_nonceds_emiss_some_neg %>%
    filter(!variable %in% hector_rcmip_nonceds_emiss_cc_emiss$variable) %>%
    rbind(hector_rcmip_nonceds_emiss_cc_emiss) ->
    hector_rcmip_nonceds_emiss_df1


# The non historical emissions need to have the information be
# interpolated, NA values will throw errors, this is more of
# a problem in the future scenarios.
hector_rcmip_nonceds_emiss_df1 %>%
    filter(scenario != "historical") %>%
    split(interaction(.$scenario)) %>%
    lapply(FUN = fill_in_missing_emiss, expected_yrs = 1950:2500) %>%
    bind_rows %>%
    bind_rows(filter(hector_rcmip_nonceds_emiss_df1, scenario == "historical")) ->
    hector_rcmip_nonceds_emiss_df1

# Extend the rcmip emissions until 1745.
split(hector_rcmip_nonceds_emiss_df1,
      hector_rcmip_nonceds_emiss_df1$scenario) %>%
    lapply(FUN = extend_to_1745) %>%
    bind_rows ->
    hector_rcmip_nonceds_emiss_df1

# L1 is where all the emissions in Hector units are saved.
write.csv(hector_rcmip_nonceds_emiss_df1,
          file = file.path(DIRS$L1, "L1.hector_rcmip_emiss.csv"), row.names = FALSE)

# 1B. Select Emissions RCMIP to CEDS ------------------------------------------------------------
# Load the RCMIP to CEDS mapping file
rcmip_ceds_mapping <- read.csv(file.path(DIRS$MAPPING, "RCMIP_CEDS_mapping.csv"), comment.char = "#")

# For now let's start working with the BC emissions, which is the only values
# included in the mapping file.
raw_rcmip_emissions %>%
    filter(Region == "World") %>%
    gather(key = "year", value = "value", starts_with("X")) %>%
    mutate(year = as.integer(gsub(replacement = "", x = year, pattern = "X"))) %>%
    filter(year <= CEDS_FINAL_YEAR) %>%
    rename(rcmip_variable = Variable) ->
    long_rcmip

# Map from RCMIP to CEDs units, aggregate to total emissions and replace
# NAs with values.
long_rcmip %>%
    left_join(rcmip_ceds_mapping, by = "rcmip_variable") %>%
    mutate(value = value * cf) ->
    rcmip_ceds_units

rcmip_ceds_units %>%
    select(scenario = Scenario, em, year, value, units = ceds_units) %>%
    group_by(scenario, em, year, units) %>%
    summarise(value = sum(value), .groups = "keep") %>%
    ungroup() %>%
    split(f = interaction(.$scenario, .$em)) %>%
    lapply(my_replace_na_fxn) %>%
    do.call(what = "rbind") ->
    rcmip_ceds_emiss

# Extend the emissions until 1745.
rcmip_ceds_emiss %>%
    # Because of the way that the extend_to_1745 function was written the
    # rename the em column so that it will work with this function.
    rename(variable = em) %>%
    split(., .$scenario) %>%
    lapply(FUN = extend_to_1745) %>%
    bind_rows %>%
    rename(em = variable) ->
    rcmip_ceds_emiss

write.csv(rcmip_ceds_emiss, file = file.path(DIRS$L0, "L0.rcmip_ceds_emiss.csv"), row.names = FALSE)
