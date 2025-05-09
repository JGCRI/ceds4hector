# hector-CEDS


This project can be used to prepare the Hector `.ini` file and emissions `.csv` table from the 2024-07-08 [CEDS](https://github.com/jGCRI/ceds) release. The tentative plan is for this workflow to eventually be incorporated into gcam-data, but for now, it is a standalone workflow. Setup instructions are written for macOS users; Windows users may need to make some adjustments. 


## Getting started

1. **Clone the Repository** Clone a copy of this repository to your local machine.
2. **Download Raw Data Files** The raw data files are too large to be committed to the Git repository. You can download them from Zenodo by running the following command in your terminal from the `hector-CEDS/data/raw-data` directory:

```
sh get-raw-data.sh
```

3. **Install R Packages** Install the required packages listed in `hector-CEDS/scripts/constants.R`. 


Now you should be set up to run the workflow!

## Running the full workflow

To run the entire workflow at once, execute the following command in R. This will run all scripts in the correct order, generating Hector inputs in the `hector-CEDS/inputs` directory: 

```
source("scripts/A.run_all.R")
```

Otherwise users can source the scripts individually, in increase L order. 

## Using CEDS emissions in Hector

The Hector inputs generated by this workflow are included in this repository. Most users can use the default `hector-CEDS/inputs/hector_ceds_historical.ini` file after cloning the repository without running `scripts/A.run_all.R`. 

Here’s an example of how to use this `.ini` created by the `scripts/A.run_all.R` workflow or included in the repository:

```
# Load Hector package
library(hector)

# Define the path to the hector_ceds_historical ini file
# Replace PATH/TO with the path to where you installed hector-CEDS on your local machine
ini <- "PATH/TO/hector-CEDS/inputs/hector_ceds_historical.ini"

# Create a new Hector core with the specified ini file
hc <- newcore(ini)

# Run Hector up to the end of CEDS emissions data in 2023
run(hc, runtodate = 2023)

# Use fetchvars to examine Hector results
# For more examples, visit https://github.com/JGCRI/hector
```










