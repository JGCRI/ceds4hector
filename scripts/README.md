The CEDS data set does not provide all the emission species needed to complete a Hector run, this workflow combines the CEDS emissions with emissions from RCMIP. 


* `constants.R`: contains helper functions and the packages required by other project scripts
* `L0`: the scripts that are used to import raw-data and format (all names are the same and everything is in CEDS units)
* `L1`: combine the CEDS and RCMIP emissions into the global time series that can be used as Hector inputs, since Hector needs global total emissions across all countries, sectors, and sources
* `L2`: add in the missing emissions species that are not included in CEDS output 
