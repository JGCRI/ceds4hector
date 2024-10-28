The CEDS data set does not provide all the emission spieces needed to complete a Hector run, this workflow combines the CEDS emissions with emissions from RCMIP. 


* `constants.R`: contains helper functions and the packages required by other project scripts
* `L0`: the scripts that are used to import raw-data and format (all names are the same and everything is in CEDS units)
* `L1`: combine the CEDS and RCMIP emissions into the global time series Hector will use (we need global emissions across countries/sectors)
* `L2`: save the emissions as the properly formatted csv table for hector! and write out the ini file 
