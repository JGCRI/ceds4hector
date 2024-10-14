# Objective: Playing around trying to get the emissions based on the concentrations,
# this a a proof oc concept as for how to calculate total n20 emissions for Hector
# based on a concentration pathway

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(hector)

# 1. Run Default Hector  -------------------------------------------------------
# Default values from the ini files.
N0=273.87
UC_N2O=4.8
TN2O0=132

# Running Hector in emission driven model to get the corresponding concentraitons
# we need emission driven concentrations of N2O to check our wokr.
path <- system.file(package = "hector", "input/hector_ssp245.ini")
hc <- newcore(path, loglevel = LL_DEBUG(), suppresslogging = FALSE)
run(hc)
out <- fetchvars(hc, dates = 1740:2100, vars = c(EMISSIONS_N2O(),
                                                 NAT_EMISSIONS_N2O(),
                                          CONCENTRATIONS_N2O()))

# Saving a copy of the n2o concentrations and and other important Hector
# N2O information, although this is some what redundent since it is
# read in via the ini file.
out %>%
    filter(variable == CONCENTRATIONS_N2O()) %>%
    pull(value)->
    n2o_conc_vals
yrs <- unique(out$year)
preind_n2o <- fetchvars(hc, NA, PREINDUSTRIAL_N2O())[["value"]]
nat_n2o <- fetchvars(hc, 1800, NAT_EMISSIONS_N2O())[["value"]]

# Determine the N2O lifetime using (S2) from Dorheim et al. 2024 supplemental
# We are seeing some small numerical differences between the Hector acutal
# and the proof of concept, I wonder if it is due to a time lag here...
tau_n2o <- TN2O0 * (n2o_conc_vals/N0) ^ (-0.05) # the first year of tau is for 1746 looking at the Hector n2o logs output streams
#tau_n2o <- TN2O0 * (n2o_conc_vals/N0) ^ (-0.05)
# If we want to start at 1745 we need to add another year of the initial tau
# and then drop the tail observation to make sure we have the correct length
tau_n2o <- c(TN2O0, tau_n2o)[1:length(yrs)]

# This starts with the change between 1745 and 1746
d_co2 <- diff(c(n2o_conc_vals))[1:length(yrs)]

# Calculate my emissions aka the back calculate emissions
4.8 * (d_co2 + n2o_conc_vals/tau_n2o) -> my_emiss

# format into a data frame
my_emiss <- c(my_emiss)[1:length(yrs)]

yrs <- 1745:2100

# change the years due to the lagg from the hector solver
my_emiss_df <- data.frame(year = 1745:2100,
                          value = my_emiss,
                          source = "mine")

out %>%
    filter(variable != CONCENTRATIONS_N2O()) %>%
    group_by(year) %>%
    summarise(value = sum(value)) %>%
    mutate(source = "hector") ->
    hector_emiss

# Compare the actual reported Hector emissions with the ones back caculated
# to show that this was implemented properly.
rbind(my_emiss_df, hector_emiss) %>%
    #filter(year <= 1760) %>%
    ggplot() +
    geom_line(aes(year, value, color = source))
    #geom_line(data = out, aes(year, value))

rbind(my_emiss_df, hector_emiss) %>%
    tidyr::spread(source, value) %>%
    mutate(dif = abs(hector - mine)/mine) ->
    diff_df


diff_df %>% head()
diff_df$dif %>% summary()




# 2. Functionalize get hector n2o emissions  -----------------------------------
# Back calculate total N2O emissions for Hector based on a time series of N2O concentrations
get_hector_n2o_emissions <- function(input_df){

    # Quality check the inputs, make sure we are working with the right units and
    # that the time series is continuous!
    assertthat::has_name(x = input_df, which = c("year", "variable", "value", "units"))
    assertthat::are_equal(unique(input_df$units), "ppbv N2O")
    assertthat::are_equal(unique(diff(input_df$year)), 1)


    # Default Hector values defined in Hector ini files.
    N0=273.87
    UC_N2O=4.8
    TN2O0=132

    # Extract the concentrations and years data.
    yrs <- input_df$year
    n <- length(yrs)
    conc_vals <- input_df$value


    # Determine the N2O lifetime using (S2) from Dorheim et al. 2024, there is
    # a time lag so padd the front with the original lifetime.
    tau_n2o <- TN2O0 * (conc_vals/N0) ^ (-0.05)
    tau_n2o <- c(TN2O0, tau_n2o)[1:n]

    # Calculate the change [N2O] over time
    d_co2 <- diff(c(conc_vals))[1:n]

    # Calculate the emissions based on (S1) from Dorheim et al. 2024
    emiss <- 4.8 * (d_co2 + conc_vals/tau_n2o)


    data.frame(year = yrs,
               value = emiss) %>%
        # filter(year %in% yrs) %>%
        na.omit ->
        out
    return(out)
}



n2o_conc <- fetchvars(hc, 1740:2100, CONCENTRATIONS_N2O())
out <- get_hector_n2o_emissions(input_df = n2o_conc)

my_emiss_df %>%
    na.omit ->
    to_compare
# confirm that we are getting the same results
out$value - to_compare$value
out$year - to_compare$year

