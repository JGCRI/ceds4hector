# Objective: launch the entire workflow from this single script, just
# source this file.
# TODO
# Ideas
#   - use drake?
#   - use renv to gaurentee the package versions
# ------------------------------------------------------------------------------
# List of R scripts to run in order
scripts <- c("./scripts/L0.CEDS-emiss.R",
             "./scripts/L0.conc_obs.R",
             "./scripts/L0.RCMIP_emiss.R",
             "./scripts/L1.aggregate_CEDS_RCMIP.R",
             "./scripts/L2.hector_tables.R")

# Run each script
for (s in scripts){

    source(s)
}


# Save a copy of the R session information
ofile <- here::here("scripts", "sessionInfo.txt")
sink(ofile)
sessionInfo()
sink()

