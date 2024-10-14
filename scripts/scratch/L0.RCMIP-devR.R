# Import and format the RCMIP data set. We will need a copy of full RCMIP
# global emissions and copies of the ag waste, forest, peatland, and grassland
# burning emissions.
# TODO:
# A) A better CEDS-RCMIP mapping might be better but would take more time for
#       now the mapping file only includes the emissions we know that we
#       need to copy over.
# B) Probably need to flesh out the RCMIP to Hector portion of the code so that
#       the full scenarios are copied over.
# C) Need to change the list of scenarios that are being processed by this script.


# 0. Set Up -------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "L0.constants.R"))


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


# 1. Main Chunk ----------------------------------------------------------------
# Load the RCMIP emission files
rcmip_file <- list.files(path = DIRS$DATA, pattern = "rcmip-emissions-annual-means-v5-1-0.csv",
                         full.names = TRUE, recursive = TRUE)
assert_that(file.exists(rcmip_file), msg = "Missing RCMIP emissions file see set up instructions!")

# For now let's limit to a handful of scenarios
scenarios <- c("ssp119", "ssp245", "ssp585", "historical")

read.csv(rcmip_file) %>%
    filter(Scenario %in% scenarios) ->
    raw_rcmip_emissions

# 1A. All RCMIP to Hector  -----------------------------------------------------
# Start out by converting all RCMIP emissions to Hector inputs, since we will
# need to blend together potentially some of the older emissions as well and
# blend with the near future values we will want total emissions for each of the
# speices readily available.
# TODO add the SF, land use albedo and so on into the mix?

# Load the RCMIP to Hector mapping file.
rcmip_hector_mapping <- read.csv(file = file.path(DIRS$MAPPING, "RCMIP_Hector_mapping.csv"),
                                 comment.char = "#")

# Gather all of the non CEDS emission species in a data frame and change from
# wide to long orientation, apply the conversion factor and save output.
raw_rcmip_emissions %>%
    filter(Region == "World") %>%
    pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "value") %>%
    mutate(year = as.integer(gsub(replacement = "", x = year, pattern = "X"))) %>%
    rename(rcmip_variable = Variable) %>%
    # TODO unclear if this is the correct join to do here or not...
    inner_join(rcmip_hector_mapping, by = join_by(rcmip_variable)) %>%
    mutate(value = value * cf) %>%
    select(scenario = Scenario, year = year, value,
           variable = hector_variable, units = hector_unit) %>%
    na.omit ->
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

write.csv(rcmip_ceds_emiss, file = file.path(DIRS$L0, "L0.rcmip_ceds_emiss.csv"), row.names = FALSE)
