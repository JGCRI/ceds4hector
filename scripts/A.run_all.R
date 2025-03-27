# Run all of the files at once. 


BASE_DIR <- here::here()
scripts <- c("L0.BB4CMIP-emiss.R", 
             "L0.CEDS-emiss.R", 
             "L0.V32-emiss.R", 
             "L1.aggregate_CEDS_emissions.R", 
             "L2.global_total_emissions.R", 
             "L3A.run_hector.R")

files <- file.path(BASE_DIR, "scripts", scripts)

for(f in files){
  source(f)
}
