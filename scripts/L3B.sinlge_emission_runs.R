# Description: Run Hector with new CEDS emissions but only change one emission 
# species at a time, which will allow for better comparisons of effects on 
# total RF changes. 

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(hector)
theme_set(theme_bw())

BASE_DIR <- here::here()

FINAL_HIST_YEAR <- 2022
YRS <- 1750:FINAL_HIST_YEAR


# The variables to save.
vars <- c("BC_emissions", "CH4_emissions", "CO_emissions", "ffi_emissions",
          "N2O_emissions", "NH3_emissions", "NMVOC_emissions", "NOX_emissions",
          "OC_emissions", "SO2_emissions", "RF_tot", "RF_CO2",
          "RF_N2O", "RF_H2O_strat", "RF_O3_trop", "RF_BC", "RF_OC", "RF_CH4",
          "RF_NH3", "RF_SO2", "RF_aci", "global_tas", "O3_concentration",
          "CO2_concentration", "N2O_concentration", "CH4_concentration", NBP(), NPP(), VEG_C())

# 1. "Default" Hector Run ------------------------------------------------------

# Run default hector
ini <- list.files("inputs", full.names = TRUE, pattern = "ssp245")
hc  <- newcore(ini, name  ="default hector")
run(hc, runtodate = FINAL_HIST_YEAR)
fetchvars(hc, dates = YRS, vars = vars) %>% 
  filter(year <= 2014) -> 
  out1

# 2. Single Emission Runs ------------------------------------------------------

# Use the mapping file to determine which variables we even want to look at. 
read.csv(file.path(DIRS$MAPPING, "L1.hector_sector_mapping.csv"), comment.char = "#") %>% 
  filter(source == "CEDS") %>% 
  pull(variable) %>% 
  unique() -> 
  ceds_emiss

# Load the data 
read.csv(file.path(DIRS$L2, "hector_ceds.csv")) %>% 
  filter(variable %in% ceds_emiss) -> 
  input_data


# Run Hector and change only one CEDS emissions time series at a time. 
lapply(unique(input_data$variable), function(var){
  
  input_data %>%
    filter(variable == var) ->
    use_this
  
  vars_to_keep <- c(GLOBAL_TAS(), RF_TOTAL(), var)
  
  if(grepl(pattern = "emissions", var)){
    
    if(var == EMISSIONS_CH4()){
      vars_to_keep <- c(vars_to_keep, "RF_CH4", CONCENTRATIONS_CH4())
    } else if(var %in% c(FFI_EMISSIONS(), DACCS_UPTAKE(), LUC_EMISSIONS(), LUC_UPTAKE())) {
      vars_to_keep <- c(vars_to_keep, RF_CO2(), NBP(), CONCENTRATIONS_CO2())
    } else if(var %in% c(EMISSIONS_CO(), EMISSIONS_NMVOC(), EMISSIONS_NOX(), NAT_EMISSIONS_N2O())) {
      
    } else {
      vars_to_keep <- c(vars_to_keep, paste0("RF_", gsub(x = var, pattern = "_emissions", replacement = "")))
    }
    
  }
  
  if(grepl(pattern = "HFC|CFC|halo|HFC365_emissions", var)){
    use_units <- "Gg"
  } else {
    use_units <- getunits(var)
  }
  
  print(var)
  
  hc <- newcore(ini, name = var)
  setvar(core = hc,
         dates = use_this$year,
         var = var,
         values = use_this$value,
         unit = use_units)
  reset(hc)
  run(hc, runtodate = max(YRS))
  out <- fetchvars(hc, dates = YRS, vars = vars_to_keep)
  return(out)
}) %>%
  do.call(what = "rbind") ->
  rslts

rslts %>% 
  bind_rows(out1) -> 
  single_var_runs

write.csv(single_var_runs, file = file.path(DIRS$L3, "single_var_runs.csv"), row.names = FALSE)
single_var_runs <- read.csv(file = file.path(DIRS$L3, "single_var_runs.csv"))


# Let's rank the MSE between the total RF 
rslts %>% 
 # filter(variable %in% c(RF_TOTAL(), GLOBAL_TAS())) %>% 
  rename(new_value = value) %>% 
  left_join(out1 %>% 
              select(year, variable, value), by = join_by(year, variable)) %>% 
  mutate(SE = (value - new_value)^2) %>% 
  summarise(RMSE = sqrt(mean(SE, na.rm = TRUE)), .by = c("scenario", "variable")) %>% 
  arrange(variable, desc(RMSE)) -> 
  MSE_table


MSE_table %>% 
  filter(variable %in% c(RF_TOTAL(), GLOBAL_TAS()))  -> 
  RMSE_table

write.csv(RMSE_table, file = file.path(DIRS$L3, "single_var_runs-RMSE.csv"), row.names = FALSE)


# Single Variable Runs 
scn <- EMISSIONS_BC()


lapply(unique(rslts$scenario), function(scn){
  
  vars_to_plot <- c(scn, RF_TOTAL(), GLOBAL_TAS())
  
  single_var_runs %>% 
    filter(scenario == scn) %>% 
    filter(variable %in% vars_to_plot) %>% 
    mutate(variable = factor(variable, levels = vars_to_plot, ordered = TRUE)) -> 
    single_emiss_to_plot
  
  out1 %>% 
    filter(variable %in% vars_to_plot) %>% 
    mutate(variable = factor(variable, levels = vars_to_plot, ordered = TRUE)) -> 
    default_to_plot
  
  MSE_table %>%
    filter(scenario == scn) %>% 
    filter(variable %in% vars_to_plot) %>% 
     mutate(RMSE = signif(RMSE, digits = 3)) ->
    tb
  
  tbs <- lapply(split(tb, tb$variable), "[", -1)
  
  df <- tibble(x = rep(-Inf, length(tbs)), 
               y = rep(Inf, length(tbs)), 
               variable = factor(vars_to_plot, levels = vars_to_plot, ordered = TRUE),
               tbl = tbs)
  
  ggplot() + 
    geom_line(data = default_to_plot, aes(year, value, color = "default")) + 
    geom_line(data = single_emiss_to_plot, aes(year, value, color = "CEDS")) + 
    facet_wrap("variable", scales = "free") + 
    scale_color_manual(values = c("default" = "darkgrey", CEDS = "red")) + 
    labs(y = NULL, x = NULL) + 
    theme(legend.position = "bottom", legend.title = element_blank()) + 
    labs(title = paste0("Deafult Emissions + CEDS ", scn, " only")) + 
    geom_table(data = df, aes(x = x, y = y, label = tbl),
               hjust = 0, vjust = 1) -> 
    plot 
  fname <- file.path(DIRS$L3, "single_emissions", paste0(scn, "_only.png"))
  ggsave(plot, filename = fname, width = 10, height = 5.5)
  
})





