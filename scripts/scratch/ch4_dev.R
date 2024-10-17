# Developing the CH4 function, again we want to use the [CH4] and inputs from
# a Hector run to make sure that it is being implemented properly.

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(hector)
library(hector)

# 1. Experimenting with set up  ------------------------------------------------
# Default values from the ini files.
# CH4 component constants
M0= 731.41  		# 731.41 ppb preindustrial methane IPCC AR6 Table 7.SM.1, the CH4 forcing equations is calibrated to a M0 of 731.41 ppb
Tsoil=160 			# lifetime of soil sink (years)
Tstrat=120          # lifetime of tropospheric sink (years)
UC_CH4=2.78			# Tg(CH4)/ppb unit conversion between emissions and concentrations
CH4N=335	        # Natural CH4 emissions (Tgrams)

# OH component constants
TOH0=6.6			#; inital OH lifetime (years)
CNOX=0.0042			#; coefficent for NOX
CCO=-0.000105		#; coefficent for CO
CNMVOC=-0.000315	#; coefficent for NMVOC (non methane VOC)
CCH4=-0.32			#; coefficent for CH4

# Running Hector in emission driven model to get the corresponding concentrations
# we need emission driven concentrations of CH4 to check our work to confirm
# that we have the correct function set up.
path <- here::here("inputs", "hector-input", "hector_ssp245_noperm.ini")
hc <- newcore(path, loglevel = LL_DEBUG(), suppresslogging = FALSE)
run(hc)
hout <- fetchvars(hc, dates = 1740:2100, vars = c(EMISSIONS_CH4(),
                                                  EMISSIONS_NOX(),
                                                  EMISSIONS_CO(),
                                                  EMISSIONS_NMVOC(),
                                                  CONCENTRATIONS_CH4()))

hout %>%
    filter(variable == CONCENTRATIONS_CH4())  ->
    input_ch4
    ch4_conc

hout %>%
    filter(variable == EMISSIONS_NOX())  ->
    input_nox

hout %>%
    filter(variable == EMISSIONS_CO()) ->
    input_co

hout %>%
    filter(variable == EMISSIONS_NMVOC()) ->
    input_nmvoc

hout %>%
    filter(variable == EMISSIONS_CH4()) %>%
    pull(value) ->
    ch4_emiss

# Initial emissions
NOX_0 <- 3.877282
CO_0 <- 348.5274
NMVOC_0 <- 60.02183

ch4_conc <- c(NA, ch4_conc)[1:length(ch4_emiss)]

a <- CCH4 * log(ch4_conc/M0)
b <- CNOX*(nox_vals - NOX_0)
c <- CCO * (co_vals - CO_0)
d <- CNMVOC*(nmvoc_vals - NMVOC_0)

toh <- (a + b + c + d)
tau <- TOH0 * exp( -1 * toh )

d_ch4 <- ((ch4_emiss+CH4N)/UC_CH4)  - (ch4_conc/tau) - (ch4_conc/Tstrat) - (ch4_conc/Tsoil)
out <- diff(ch4_conc)


rbind(data.frame(value = out,
                 year = 1745:2099,
                 source = "output"),
      data.frame(value = d_ch4,
                 year = 1745:2100,
                 source = "mine")) %>%
    filter(year < 1850) %>%
    ggplot(aes(year, value, color = source, linetype = source)) +
    geom_line()

# Now let's see if we can back calculate the emissions from the stuff
dch4 <- c(diff(ch4_conc), NA)
emiss <-  UC_CH4 * (dch4 + (ch4_conc/tau) + (ch4_conc/Tstrat) + (ch4_conc/Tsoil))

hout %>%
    filter(variable == EMISSIONS_CH4()) %>%
    mutate(value = value + CH4N) %>%
    bind_rows(data.frame(value = emiss,
                         year = 1745:2100,
                         variable = "mine")) ->
    x
x %>%
    ggplot(aes(year, value, color = variable, linetype = variable)) +
    geom_line()

# Okay now that we got workflow working! Let's functionalize it!!!

# 2. Functionalize & Testing  --------------------------------------------------
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
    assertthat::has_name(x = input_ch4, which = c("year", "variable", "value", "units"))
    assertthat::are_equal(unique(input_ch4$units), "pppbv CH4")
    assertthat::are_equal(unique(diff(input_ch4$year)), 1)
    assertthat::has_name(x = input_nox, which = c("year", "variable", "value", "units"))
    assertthat::are_equal(unique(input_nox$units), "Tg N")
    assertthat::are_equal(unique(diff(input_nox$year)), 1)
    assertthat::has_name(x = input_co, which = c("year", "variable", "value", "units"))
    assertthat::are_equal(unique(input_co$units), "Tg CO")
    assertthat::are_equal(unique(diff(input_co$year)), 1)
    assertthat::has_name(x = input_nmvoc, which = c("year", "variable", "value", "units"))
    assertthat::are_equal(unique(input_nmvoc$units), "Tg NMVOC")
    assertthat::are_equal(unique(diff(input_nmvoc$year)), 1)

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
    TOH0=6.6			#; inital OH lifetime (years)
    CNOX=0.0042			#; coefficent for NOX
    CCO=-0.000105		#; coefficent for CO
    CNMVOC=-0.000315	#; coefficent for NMVOC (non methane VOC)
    CCH4=-0.32			#; coefficent for CH4


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

    data.frame(year = input_ch4$year,
               value = emiss,
               variable = EMISSIONS_CH4(),
               units = getunits(EMISSIONS_CH4())) %>%
        na.omit() ->
        out

    return(out)


}




# Now let's test it, basically doing what we did above but double checking to
# make the function works as expected.
hout %>%
    filter(variable == CONCENTRATIONS_CH4())  ->
    input_ch4

hout %>%
    filter(variable == EMISSIONS_NOX())  ->
    input_nox

hout %>%
    filter(variable == EMISSIONS_CO()) ->
    input_co

hout %>%
    filter(variable == EMISSIONS_NMVOC()) ->
    input_nmvoc

hout %>%
    filter(variable == EMISSIONS_CH4()) %>%
    mutate(value = value + CH4N) ->
    ch4_emiss

out <- get_ch4_emissions(input_ch4, input_nox, input_co, input_nmvoc)
out$variable <- "mine"

ch4_emiss %>%
    bind_rows(out) %>%
    ggplot(aes(year, value, color = variable, linetype = variable)) +
    geom_line()
