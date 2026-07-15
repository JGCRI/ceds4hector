# The natural CH4 and N2O emissions need to be updated to account for the new 
# antro emissions. 


# 0. Set Up --------------------------------------------------------------------
source(here::here("scripts", "constants.R"))

# 0.A. Define helper functions -------------------------------------------------

# Using observations of N2O concentrations and the anthropocentric N2O emissions
# back calculate the natural N2O emissions for Hector
# Args
#   n2o_conc: data frame of observations of global N2O concentrations
#   total_emiss: data frame of Hector's N2O_emissions
#   TAU0: double initial life time of N2O defined in ini
#   N2O0: double initial concentration of N2O also defined in the ini 
# Returns: data frame of the N2O natural emissions for Hector
get_natural_N2O <- function(n2o_conc, n2o_emiss, TAU0, N2O0){
  
  # Confirm that we are only working with the correct variables.
  stopifnot(unique(n2o_emiss$variable) == EMISSIONS_N2O())
  stopifnot(unique(n2o_conc$variable) == CONCENTRATIONS_N2O())
  
  
  # Make sure there are no more emissions relative to the concentrations
  n2o_emiss %>%
    filter(year <= max(n2o_conc$year)) ->
    n2o_emiss
  
  # Save the values for 1745 as the starting point.
  N2O_conc_0 <- N2O0
  tau_0 <- TAU0
  my_n2o_conc <- c(N2O_conc_0)
  my_tau      <- c(tau_0)
  
  total_E <- (N2O_conc_0/tau_0) * 4.8
  new_nat_n2o <- total_E - n2o_emiss$value[1]
  
  my_nat_n2o  <- c(new_nat_n2o)
  
  for(t in n2o_emiss$year[-1]){
    
    # Extract the information that we need in our calculation
    # for the present time step.
    antro_emiss <- n2o_emiss$value[n2o_emiss$year == t]
    lag_n2o <- my_n2o_conc[t-1745]
    current_n2o <- n2o_conc$value[n2o_conc$year == t]
    
    # Calculate the elements of the N2O concentration equation
    delta_n2o <- current_n2o - lag_n2o
    tau <- tau_0 * (lag_n2o/N2O_conc_0)^(-0.05)
    total_emiss <- 4.8 * (delta_n2o + lag_n2o/tau)
    new_nat_emiss <- total_emiss - antro_emiss
    
    # Make sure that the natural emissions strictly positive
    # if not then assume 0 natural emissions and update the
    # concentrations accordingly.
    if(new_nat_emiss < 0){
      update_delta_n2o <- antro_emiss/4.8 - lag_n2o/tau
      my_n2o_conc <- c(my_n2o_conc, lag_n2o + update_delta_n2o)
      my_nat_n2o <- c(my_nat_n2o, 0)
    } else {
      my_nat_n2o <- c(my_nat_n2o, new_nat_emiss)
      my_n2o_conc <- c(my_n2o_conc, lag_n2o + delta_n2o)
    }
    
  }
  
  
  data.frame(year = n2o_emiss$year,
             value = my_nat_n2o,
             variable = NAT_EMISSIONS_N2O(),
             units = getunits(NAT_EMISSIONS_N2O())) ->
    out
  
  return(out)
  
}


# Helper function that will fill in emission emissions
# Args
#   d: data.frame, long format of Hector emissions, year/variable/value/unit/scenario
#   expected_yrs: vector, of the years of data that need the data set to have
# Returns: data.frame, long format of Hector emissions, year/variable/value/unit/scenario for
# all the years defined in the expected_yrs without any NAs.
fill_in_missing_emiss <- function(d, expected_yrs = 1745:2014){
  
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
    
    
    wide_data %>% 
      select(-year) %>% 
      .[1, ] -> 
      df
    
    df[] <- NA
    
    df %>% cbind(year = missing_yrs) %>% 
      bind_rows(wide_data) %>% 
      arrange(year) -> 
      wide_data
    
    
  }
  
  if(sum(is.na(wide_data)) == 0){
    warning("no NAs to detected, nothing to be interpolated")
  }
  
  wide_data %>%
    mutate(across(where(is.numeric), ~na.approx(.x, wide_data$year, na.rm = FALSE))) %>%
    pivot_longer(-year, names_to = "variable") %>%
    arrange(variable, year) ->
    out
  
  return(out)
  
}

# Set up a new Hector core with constrained CH4 and N2O concentrations
# Args
#   ini: path to the hector ini
#   name: "no name" by default but will name the hector results if a
#           different string is provided
#   ghg_constraints: data frame of the ghg constraints aka observations... 
# Returns: active hector core ready to run in constraint mode
newcore_CH4_N2O <- function(ini, name = "no name", ghg_constraints){
  
  stopifnot(file.exists(ini))
  
  # Instantiate the hector core
  hc <- newcore(inifile = ini, name = name)
  
  # Subset the GHG's that should be used as constraints.
  ch4 <- filter(ghg_constraints, variable == CONCENTRATIONS_CH4())
  n2o <- filter(ghg_constraints, variable == CONCENTRATIONS_N2O())
  
  # Apply the constraints to the Hector core
  setvar(core = hc, dates = ch4$year, var = CH4_CONSTRAIN(),
         values = ch4$value, unit = getunits(CH4_CONSTRAIN()))
  reset(hc)
  
  setvar(core = hc, dates = n2o$year, var = N2O_CONSTRAIN(),
         values = n2o$value, unit = getunits(N2O_CONSTRAIN()))
  reset(hc)
  
  return(hc)
  
}


# Get a variable from the ini file for those not available to fetch via R
# Args
#   pnames: vector of parameter names to extract from the ini file
#   ini_lines: lines from the hector ini
# Returns: vector of parameter and parameter values
fetch_vars_ini <- function(pnames, ini_lines){
  
  val <- c()
  
  for(p in pnames){
    
    # Identify the ini line where the parameter is set.
    pattern <- paste0("^", p, "=.*;")
    indx    <- which(grepl(pattern = pattern, x = ini_lines))
    
    # There should have only been one ini line identified.
    stopifnot(length(indx) == 1)
    
    single_line <- ini_lines[indx]
    
    single_val <- as.numeric(unlist(strsplit(single_line, split = "=|.;"))[2])
    val <- c(val, single_val)
  }
  
  names(val) <- pnames
  return(val)
}



# 0.B Import Data --------------------------------------------------------------

# Load the historical concentrations that are going to be used in the comparisons. 
here::here("data", "raw-data", "Supplementary_Table_UoM_GHGConcentrations-1-1-0_annualmeans_v23March2017.csv") %>% 
  read.csv(skip = 21) %>% 
  na.omit %>% 
  lapply(., function(x) gsub(",", "", x)) %>% 
  lapply(., function(x) gsub(" ", "", x)) %>% 
  bind_rows() -> 
  conc_df

names(conc_df)[1] <- "year"  

conc_df %>% 
  pivot_longer(cols = -year, names_to = "variable") %>% 
  mutate(variable = paste0(variable, "_concentration")) %>% 
  filter(variable %in% c(CONCENTRATIONS_CO2(), CONCENTRATIONS_N2O(), CONCENTRATIONS_CH4())) %>% 
  mutate(source = "observations") %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(value = as.double(value)) %>%  
  extend_to_1745 %>% 
  fill_in_missing_emiss -> 
  observations_df


# Load the ceds hector emissions 
file.path(DIRS$L2, "hector_ceds.csv") %>% 
  read.csv() -> 
  antro_emiss

# 1. N2O Natural Emissions -------------------------------------------------
# Starting with the N2O emissions. 
observations_df %>%  
  filter(variable == CONCENTRATIONS_N2O()) -> 
  n2o_conc

antro_emiss %>% 
  dplyr::filter(variable == EMISSIONS_N2O()) %>% 
  filter(year %in% n2o_conc$year) -> 
  n2o_emiss

# TODO make this something that can extract with the R helper 
TN2O0 <- 132
N2O0 <- 273.87

natural_n2o <- get_natural_N2O(n2o_conc = n2o_conc, 
                               n2o_emiss = n2o_emiss, 
                               TAU0 = TN2O0, 
                               N2O0 = N2O0)

natural_n2o$value %>% 
  mean -> 
  natural_n2o_emiss



# 2. CH4 Natural Emissions -------------------------------------------------

# Emissions to be used to calculate the CH4 emissions
emiss_vars <- c(EMISSIONS_CO(), EMISSIONS_NMVOC(),
                EMISSIONS_NOX(), EMISSIONS_CH4())

# Because of the permafrost feedback calculating the methane natural emissions
# is slightly more complicated 
ini_file <- file.path("inputs", "hector_ceds_historical.ini")
hc <- newcore_CH4_N2O(ini_file, ghg_constraints = observations_df)     # Make we are using the constrained CH4 run
run(hc, runtodate = max(observations_df$year))


# Fetch Hector output that will be used to calculate the natural CH4 emissions
out <- fetchvars(hc, dates = 1745:max(observations_df$year),
                 vars = c(CONCENTRATIONS_CH4(), "rh_ch4", emiss_vars))

# Initial Emissions
out %>%
  filter(year == min(out$year)) %>%
  filter(variable %in% emiss_vars) %>%
  select(variable, value) %>%
  tidyr::spread(variable, value) ->
  E0

# Full Emission Input Time Series
out %>%
  filter(variable %in% emiss_vars) %>%
  select(year, variable, value) %>%
  tidyr::spread(variable, value) ->
  E_inputs

# Rh CH4
out %>%
  filter(variable == "rh_ch4") %>%
  mutate(value = value * (1000.0 * 16.04 / 12.01), # convert from Pg C to Tg CH4
         units = "Tg CH4") ->
  rh_ch4_emiss

# CH4 concentrations to fit to
out %>%
  filter(variable == CONCENTRATIONS_CH4()) ->
  ch4_conc

# Extract constants/parameters/coefficients from a Hector ini file
pnames   <- c("TOH0", "CNOX", "CCO", "CNMVOC", "CCH4", "M0",
              "Tsoil", "Tstrat", "UC_CH4")

ini_lines  <- readLines(ini_file)
p_values   <- fetch_vars_ini(pnames, ini_lines)


# During the preindustrial time period we assume that there is no change
# in emissions or concentrations which simplifies some of the calculations.
with(as.list(p_values), {
  
  delta_CH4 <- 0
  total_emiss   <- (delta_CH4 + M0/TOH0 + M0/Tstrat + M0/Tsoil) * 2.78
  new_nat_emiss <- total_emiss - E0$CH4_emissions - rh_ch4_emiss$value[1]
  
  return(new_nat_emiss)
}) ->
  NCH40

with(as.list(p_values), {
  
  yrs <- E_inputs$year
  natural_emiss_ts <- c(NCH40)
  tau_oh_ts        <- c(TOH0)
  ch4_conc_ts      <- c(M0)
  
  
  for (i in 2:nrow(ch4_conc)){
    
    a <- CCH4 * (log(ch4_conc_ts[i-1]) - log(M0))
    b <- CNOX * (E_inputs$NOX_emissions[i] - E0$NOX_emissions)
    c <- CCO * (E_inputs$CO_emissions[i] - E0$CO_emissions)
    d <- CNMVOC * (E_inputs$NMVOC_emissions[i] - E0$NMVOC_emissions)
    toh_exp <- a + b + c + d
    
    new_TOH <- TOH0 * exp(-1 * toh_exp)
    
    delta_CH4 <- ch4_conc$value[i] - ch4_conc_ts[i-1]
    
    total_emiss   <- (delta_CH4 + ch4_conc_ts[i-1]/new_TOH + ch4_conc_ts[i-1]/Tstrat + ch4_conc_ts[i-1]/Tsoil) * 2.78
    new_nat_emiss <- total_emiss - E_inputs$CH4_emissions[i] - rh_ch4_emiss$value[i-1]
    new_ch4_conc  <- delta_CH4 + ch4_conc_ts[i-1]
    
    # update results
    natural_emiss_ts <- c(natural_emiss_ts, new_nat_emiss)
    tau_oh_ts <- c(tau_oh_ts, new_TOH)
    ch4_conc_ts <- c(ch4_conc_ts, new_ch4_conc)
  }
  
  
  # Format results as a data frame
  rbind( data.frame(year = yrs,
                    value = natural_emiss_ts,
                    variable = NATURAL_CH4()),
         data.frame(year = yrs,
                    value = tau_oh_ts,
                    variable = "TAU_OH"),
         data.frame(year = yrs,
                    value = ch4_conc_ts,
                    variable = CONCENTRATIONS_CH4())) ->
    my_rslts
  return(my_rslts)
}) %>%
  filter(variable == NATURAL_CH4()) ->
  natural_ch4_emissions

natural_ch4_emissions %>% 
  pull(value) %>% 
  mean -> 
  natural_ch4_emiss

# 3. Update Input Files --------------------------------------------------------

# Write to input csv table, start by creating a data frame 
bind_rows(data.frame(year = 1745:2100, 
                     value = natural_ch4_emiss,
                     variable = NATURAL_CH4()), 
          data.frame(year = 1745:2100, 
                     value = natural_n2o_emiss,
                     variable = NAT_EMISSIONS_N2O())) %>% 
  mutate(scenario = "NA", 
         units = getunits(variable)) %>% 
  distinct -> 
  natural_emiss_df

write_hector_csv(x = natural_emiss_df,
                 write_to = DIRS$TABLES, 
                 required = c(NATURAL_CH4(), NAT_EMISSIONS_N2O()), 
                 info_source = "natural_emiss_cmip7CEDS") -> 
  input_table


# Update the ini file so that it is using the new natural emissions we calculated here. 
ini_file <- here::here("inputs", "hector_ceds_historical.ini")
ini_lines <- readLines(ini_file)

natural_ch4_indx <- which(grepl(x = ini_lines, pattern = NATURAL_CH4()))
new_nat_ch4 <- gsub(x = ini_lines[natural_ch4_indx], pattern = "core_inputs.csv", replacement = basename(input_table))
ini_lines[natural_ch4_indx] <- new_nat_ch4


natural_n2o_indx <- which(grepl(x = ini_lines, pattern = NAT_EMISSIONS_N2O()))
new_nat_n2o <- gsub(x = ini_lines[natural_n2o_indx], pattern = "core_inputs.csv", replacement = basename(input_table))
ini_lines[natural_n2o_indx] <- new_nat_n2o

writeLines(text = ini_lines, con = ini_file)



