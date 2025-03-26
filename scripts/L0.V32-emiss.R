# Description: Load the default ssp245 hector emissions included in 
# the Hector V3.2 release which originally came from RCMIP I. 


# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Import the emissions
system.file("input/tables/ssp245_emiss-constraints_rf.csv", package = "hector") %>%
  read.csv(comment.char = ";") ->
  wide_results


# 1. Main Chunk ---------------------------------------------------------------

# Save only a copy of the emissions and the dates
# TODO would it also be helpful to pull a copy of the RF albeldo and SV? 
emiss_to_keep <- c("Date", colnames(wide_results)[grepl(x = colnames(wide_results), 
                                                  pattern = "emissions")])
# Add in the additional input variables needed by Hector but 
# do not include the pattern "emissions".
to_keep <- c(emiss_to_keep, "daccs_uptake", "luc_uptake", "RF_albedo", "SV")

# For only the emissions change the formatting of Hector form wide to long,
# for easy plotting comparisons
wide_results[, to_keep] %>%
  pivot_longer(-Date) %>%
  select(year = Date, variable = name, value) %>%
  filter(year <= 2023) ->
  hector

# Since these are full total global emission the sector information will be 
# coded as 0 and we will set the source as V3.2 
hector %>%  
  # TODO the getunits call will return a warning message and must 
  # be resolved with a if_else statement to replace NAs, this can be 
  # addressed with https://github.com/JGCRI/hector/issues/774 
  mutate(source = "V3.2", 
         sector = 0, 
         units = getunits(variable)) %>% 
  mutate(units = if_else(is.na(units), "ppt", units)) -> 
  out

write.csv(out, file = file.path(DIRS$L0, "L0.V32_emissions.csv"), row.names = FALSE)




