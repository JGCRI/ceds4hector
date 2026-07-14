# Description: Load the default ssp245 hector emissions included in 
# the Hector V3.2 release which originally came from RCMIP I. 


# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Import the emissions
system.file("input/tables/ssp245_emiss-constraints_rf.csv", package = "hector") %>%
  read.csv(comment.char = ";") %>% 
  as.data.table -> 
  ssp_inputs

# Load the core inputs (SV & natural emissions that are not included) 
# in the specific SSP scenarios. 
system.file("input/tables/core_inputs.csv", package = "hector") %>%
  read.csv(comment.char = ";") %>% 
  summarise(CH4N = mean(CH4N), SV = mean(SV), N2O_natural_emissions = mean(N2O_natural_emissions), .by = c(Date)) %>% 
  as.data.table -> 
  core_inputs 

# Combine all the inputs into a single input data table. 
ssp_inputs[core_inputs, on =.(Date)] -> 
  wide_results

# 1. Main Chunk ---------------------------------------------------------------

# Save only a copy of the emissions and the dates
emiss_to_keep <- c("Date", colnames(wide_results)[grepl(x = colnames(wide_results), 
                                                  pattern = "emissions")])
# Add in the additional input variables needed by Hector but 
# do not include the pattern "emissions".
to_keep <- c(emiss_to_keep, "daccs_uptake", "luc_uptake", "RF_albedo", "SV", 
             "CH4N")

# For only the emissions change the formatting of Hector form wide to long,
# for easy plotting comparisons
wide_results %>% 
  select(all_of(to_keep)) %>% 
  pivot_longer(-Date) %>%
  select(year = Date, variable = name, value) %>%
  filter(year <= 2023) ->
  hector

# Since these are full total global emission the sector information will be 
# coded as 0 and we will set the source as the approporate hector version.
hector %>%  
  # TODO the getunits call will return a warning message and must 
  # be resolved with a if_else statement to replace NAs, this can be 
  # addressed with https://github.com/JGCRI/hector/issues/774 
  mutate(source = paste0("V", HECTOR_V), 
         sector = 0, 
         units = getunits(variable)) %>% 
  mutate(units = if_else(is.na(units), "ppt", units)) -> 
  out

write.csv(out, file = file.path(DIRS$L0, paste0("L0.hectorV",HECTOR_V, "_emissions.csv")), row.names = FALSE)
