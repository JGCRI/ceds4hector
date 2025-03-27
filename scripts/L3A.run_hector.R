# Description: Run Hector with the new CEDS release and the CMIP6 release. 
# Save a copy of all the output and then selected variables where there 
# are differences in the variables.


# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Save a copy of all the variables, don't include the tracking data or any 
# constraints since hector won't be run with constraints, or is a constant 
# parameter. 
params <- c(hector::inputstable$parameter[hector::inputstable$time.variant  == "n"], "lo_warming_ratio")
all_vars <- hector::fxntable$string
all_vars <- all_vars[!grepl(pattern = "tracking|constrain|biome|warmingfactor", x = tolower(all_vars))]
all_vars <- c(setdiff(all_vars, c(params, "FCH4")), "RF_CH4")

yrs <- 1750:2023


# 1. Run CMIP6 Historical ------------------------------------------------------
# Run Hector with SSP2-45 scenario as the CMIP6 historical pathway. 
ini <- system.file(package = "hector", "input/hector_ssp245.ini")
hc <- newcore(ini)
run(hc, runtodate = 2023)
out1 <- fetchvars(hc, yrs, all_vars)
out1$scenario <- "CMIP6 CEDS"


# 2. Run CMIP6 Historical ------------------------------------------------------
# Run Hector with the CMIP7 CEDS
ini <- file.path("inputs", "hector_ceds_historical.ini")
hc <- newcore(ini)
run(hc, runtodate = 2023)
out2 <- fetchvars(hc, yrs, all_vars)
out2$scenario <- "CMIP7 CEDS"


# 3. Calculate the differences between the output ------------------------------

difference_df <- out1
difference_df$value <- out2$value - out1$value
difference_df$scenario <- "CMIP7 CEDS - CMIP6 CEDS"


# 4. Save results for all variables --------------------------------------------
# Save the results for all of the hector variables 
write.csv(out1, file = file.path(DIRS$L3, "hector_CMIP6_CEDS_all_data.csv"), row.names = FALSE)
write.csv(out2, file = file.path(DIRS$L3, "hector_CMIP7_CEDS_all_data.csv"), row.names = FALSE)
write.csv(difference_df, file = file.path(DIRS$L3, "hector_difference_CEDS_all_data.csv"), row.names = FALSE)


# 5. Identify the variables of interest ----------------------------------------

# How we will define 0 in terms of sqrt(MSE)
zero_def <- 1e-9

# Determine which variables have a difference in the MSE. 
difference_df %>% 
  summarise(dif = sqrt(mean(value^2)), .by = c(variable)) %>% 
  filter(dif > zero_def) %>% 
  pull(variable) -> 
  vars_to_keep


# Subset the data sets so that they only include variables that contain different 
# results. 
out1 %>% 
  rbind(out2) %>% 
  filter(variable %in% vars_to_keep) -> 
  raw_hector_vars 

difference_df %>% 
  filter(variable %in% vars_to_keep) -> 
  difference_in_hector_vars

# Save the results for all of the hector variables 
write.csv(raw_hector_vars, file = file.path(DIRS$L3, "select_hector_results.csv"), row.names = FALSE)
write.csv(difference_in_hector_vars, file = file.path(DIRS$L3, "select_hector_difference_in_results.csv"), row.names = FALSE)


# 6.Plot Results ---------------------------------------------------------------

# Create the figure directories and general them 
FIG_DIR_DIFF <- file.path(DIRS$L3, "figs_diff")
FIG_DIR <- file.path(DIRS$L3, "figs")

dir.create(FIG_DIR, showWarnings = FALSE)
dir.create(FIG_DIR_DIFF, showWarnings = FALSE)

# Set up some general plot features 
theme_set(theme_bw() + 
            theme(legend.title = element_blank()))

FIG_WIDTH <- 10
FIG_HEIGHT <- 8

# Vector of variables to use in the plotting 
vars_to_plot <- unique(difference_in_hector_vars$variable)


lapply(vars_to_plot, function(var){
  
  difference_in_hector_vars %>% 
    filter(variable == var) -> 
    to_plot1
  
  raw_hector_vars %>% 
    filter(variable == var) -> 
    to_plot2
  
  nice_var <- gsub(pattern = "_", replacement = " ", x = var)
  
  units <- unique(to_plot1$units)
  
  # Plot the difference in the variable 
  to_plot1 %>% 
    #filter(year <= 2014) %>% 
    ggplot(aes(year, value)) + 
    geom_line() + 
    labs(subtitle = "CMIP7 CEDS - CMIP6 CEDS", 
         y = units, 
         title = paste0("Difference in ", nice_var), 
         x = NULL) -> 
    plot1
  
  
  # Plot the raw results 
  to_plot2 %>% 
    ggplot(aes(year, value, color = scenario)) + 
    geom_line() + 
    labs(subtitle = "CMIP7 CEDS - CMIP6 CEDS", 
         y = units, 
         title = nice_var, 
         x = NULL) -> 
    plot2
  
  ggsave(filename = file.path(FIG_DIR_DIFF, paste0("difference_in", var, ".png")),
         plot = plot1, width = FIG_WIDTH , height = FIG_HEIGHT)
  
  ggsave(filename = file.path(FIG_DIR, paste0(var, ".png")),
         plot = plot2, width = FIG_WIDTH , height = FIG_HEIGHT)
  
})





