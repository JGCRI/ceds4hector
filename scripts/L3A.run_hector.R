# Description: Run Hector with the new CEDS release and the CMIP6 release. 
# Save a copy of all the output and then selected variables where there 
# are differences in the variables.


# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Save a copy of all the variables, don't include the tracking data or any 
# constraints since hector won't be run with constraints, or is a constant 
# parameter. 
vars_to_save <- c("BC_emissions", "CH4_emissions", "CO_emissions", "ffi_emissions", 
                  "N2O_emissions", "NH3_emissions", "NMVOC_emissions", "NOX_emissions", 
                  "OC_emissions", "SO2_emissions", "RF_tot", "RF_CO2", 
                  "RF_N2O", "RF_H2O_strat", "RF_O3_trop", "RF_BC", "RF_OC", 
                  "RF_NH3", "RF_SO2", "RF_aci", "FCH4", "global_tas", "O3_concentration", 
                  "CO2_concentration", "N2O_concentration", "CH4_concentration")



# 1. Run CMIP6 Historical ------------------------------------------------------
# Run Hector with the CMIP6 scenarios but make sure that we are
# using the corrected NOx historical emissions! So not the emissions included 
# in the R package. 
all_ssp_inis <- list.files("inputs", full.names = TRUE, pattern = "ssp.{,3}ini" )


lapply(all_ssp_inis, function(ini){
  
  ssp_name <- gsub(basename(ini), pattern = "cmip6_corrected_|.ini", replace = "")
  
  hc <- newcore(ini)
  run(hc, runtodate = 2023)
  out <- fetchvars(hc, 1750:2023, vars_to_save)
  out$scenario <- "CMIP6 CEDS"  
  out$ssp <- ssp_name
  return(out)
  
}) %>% 
  do.call(what = "rbind") -> 
  out1




# 2. Run CMIP6 Historical ------------------------------------------------------
# Run Hector with the CMIP7 CEDS
ini <- file.path("inputs", "hector_ceds_historical.ini")
hc <- newcore(ini)
run(hc, runtodate = 2023)
out2 <- fetchvars(hc, 1750:2023, vars_to_save)
out2$scenario <- "CMIP7 CEDS"


# 3. Calculate the differences between the output ------------------------------

# Combine into a data frame. 
out <- bind_rows(out1, out2)

out %>% 
  filter(variable %in% c(RF_ACI(), RF_BC(), RF_OC(), RF_SO2(), RF_NH3())) %>% 
  summarise(value = sum(value), .by = c("scenario", "year", "units", "ssp")) %>% 
  mutate(variable = "total aerosol RF") -> 
  total_aerosol_rf

out %>% 
  rbind(total_aerosol_rf) -> 
  out

out %>% 
  filter(ssp %in% c("ssp245", NA)) %>% 
  select(scenario, year, variable, value) %>% 
  spread(scenario, value) %>% 
  na.omit %>% 
  mutate(difference = `CMIP7 CEDS` - `CMIP6 CEDS`) -> 
  diff_out
  

# Some summary results 
diff_out %>% 
  filter(variable %in% c(GLOBAL_TAS(), RF_TOTAL(), "total aerosol RF")) %>% 
  summarise(mean_diff = sqrt(mean(difference^2)), .by = c("variable")) %>% 
  knitr::kable()


# 4. Save results for all variables --------------------------------------------
# Save the results for all of the hector variables 
write.csv(out, file = file.path(DIRS$L3, "hector_CEDS_rslts_data.csv"), row.names = FALSE)
write.csv(diff_out, file = file.path(DIRS$L3, "hector_difference.csv"), row.names = FALSE)


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
vars_to_plot <- unique(diff_out$variable)


lapply(vars_to_plot, function(var){
  
  diff_out %>% 
    filter(variable == var) -> 
    to_plot1
  
  out %>% 
    filter(variable == var) -> 
    to_plot2
  
  nice_var <- gsub(pattern = "_", replacement = " ", x = var)
  
  units <- unique(to_plot1$units)
  
  # Plot the difference in the variable 
  to_plot1 %>% 
    filter(year <= 2014) %>% 
    ggplot(aes(year, difference)) + 
    geom_line() + 
    labs(subtitle = "CMIP7 CEDS - CMIP6 CEDS", 
         y = units, 
         title = paste0("Difference in ", nice_var), 
         x = NULL) -> 
    plot1
  
  
  # Plot the raw results 
  to_plot2 %>% 
    ggplot(aes(year, value, color = scenario, by = ssp)) + 
    geom_line() + 
    labs(y = units, 
         title = nice_var, 
         x = NULL) + 
    scale_color_manual(values = c("CMIP6 CEDS" = "grey", "CMIP7 CEDS" = "red")) -> 
    plot2
  
  ggsave(filename = file.path(FIG_DIR_DIFF, paste0("difference_in_", var, ".png")),
         plot = plot1, width = FIG_WIDTH , height = FIG_HEIGHT)
  
  ggsave(filename = file.path(FIG_DIR, paste0(var, ".png")),
         plot = plot2, width = FIG_WIDTH , height = FIG_HEIGHT)
  
})





