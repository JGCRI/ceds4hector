This is project converts the CEDS data into the hector expected inputs, also because 
CEDS does not include all of the input time series required by hector (halocarbons or 
the contributions from open burning) additional data must be processed and formatted 
to set up the complete historical hector input files. 


## Dependencies 

### raw-data 

This workflow relies on CEDS data and also some additional data sources. To download 
and install all the raw-data run the following command from terminal or download 
the files from the `data/raw-data/1.raw-data.urls.txt` file into the `data/raw-data`
directory. 


```
cd data/raw-data
bash 1.get-raw-data.sh
```


### R 

This workflow depends on your standard R packages and a JGCRI package. 


I've set up a renv file to help with setup, `renv::status()` and/or `renv::restore()` to load the package 
environment.

But I've found renv to sometimes be finicky, so here are some explicit notes on my R version 
the JGCRI package used in ceds4hector. 

```
R version 4.3.3 (2024-02-29)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS 26.5.2
```

* hector branch halocarb_fix - TODO this should probably be updated/clarified 
* matilda V1.1.0






* `constants.R`: contains helper functions and the packages required by other project scripts
* `L0`: the scripts that are used to import raw-data and format (all names are the same and everything is in CEDS units)
* `L1`: combine the CEDS and RCMIP emissions into the global time series that can be used as Hector inputs, since Hector needs global total emissions across all countries, sectors, and sources
* `L2`: add in the missing emissions species that are not included in CEDS output, this will complete a full historical emissions input table and ini file 
* `L3`: run hector with the CMIP7 vs CMIP6 era CEDS and make some plots. 




