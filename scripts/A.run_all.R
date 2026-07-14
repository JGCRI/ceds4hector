# Run all of the files at once. 


BASE_DIR <- here::here()
scripts <- c("L0.BB4CMIP-emiss.R", 
             "L0.CEDS-emiss.R", 
             "L0.hectorV-emiss.R", 
             "L0B.cmip6_era_inputs.R",
             "L1.aggregate_CEDS_emissions.R", 
             "L2.global_total_emissions.R", 
             "L3A.run_hector.R")#, 
             #"L3B.sinlge_emission_runs.R", 
             #"L3.porotion_of_total.R")

files <- file.path(BASE_DIR, "scripts", scripts)

for(f in files){
  print(f)
  source(f)
}
