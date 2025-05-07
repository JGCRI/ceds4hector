# Read in R packages and define project constants

# 0. Environment Set Up --------------------------------------------------------
# Start from a clean environment
remove(list = ls())

DEBUG <- FALSE
#remotes::install_github("jgcri/hector", force = TRUE)

# Load the packages
library(assertthat)
library(data.table)
library(dplyr)
library(here)
library(tidyr)
library(zoo)
library(hector)
library(readxl)


# TODO probably use a package manager but for now this is probably good enough
stopifnot(packageVersion("assertthat") == "0.2.1")
stopifnot(packageVersion("data.table") == "1.17.0")
stopifnot(packageVersion("dplyr") == "1.1.4")
stopifnot(packageVersion("here") == "1.0.1")
stopifnot(packageVersion("tidyr") == "1.3.1")
stopifnot(packageVersion("zoo") == "1.8.14")
stopifnot(packageVersion("hector") == "3.2.0")
stopifnot(packageVersion("readxl") == "1.4.5")


# TODO packages that are probably not going to be required but could be helpful during
# the developmental stage.
library(ggplot2)

# 1. Set Up Directories --------------------------------------------------------
BASE <-  here::here()
DIRS <- list(DATA = file.path(BASE, "data"),
             RAW_DATA = file.path(BASE, "data", "raw-data"),
             MAPPING = file.path(BASE, "data", "mapping"),
             L0 = file.path(BASE, "data", "L0"),
             L1 = file.path(BASE, "data", "L1"),
             L2 = file.path(BASE, "data", "L2"),
             L3 = file.path(BASE, "data", "L3"),
             TABLES = file.path(BASE, "inputs", "tables"))

sapply(DIRS, dir.create, showWarnings = FALSE, recursive = TRUE)


# # 2. Define Constants ----------------------------------------------------------

UNITS <- list(MTonne_to_Pg = 1e-3, # Convert from million tonne of carbon  to Pg
              GtC_to_PgC = 1) # gigaton of carbon (GtC) and petagram of carbon (PgC) are equivalent



# # All of the CEDS species, these are the emissions that will have to be blended
# # with the RCMIP species
# CEDS_EMISS <- c("BC", "CH4", "CO", "CO2", "N2O", "NH3", "NMVOC", "NOx", "OC", "SO2")
#
# This is the final year of the the CEDS data!
CEDS_FINAL_YEAR <- 2023



# 3. Common Functions ----------------------------------------------------------

# TODO should generalize to make more useful
# Extend the emissions until 1745
# Args
#   df: data.frame emissions that start in 1750 and need to be extended until 1745
# Return: data.frame that has extended the emissions until 1745
extend_to_1745 <- function(df){
  
  # Throw an error if any of the required columns are missing 
  req_cols <- c("year", "value")
  stopifnot(all(req_cols %in% names(df)))

  # Determine which of the columns that need to be replicated
  group_cols <- setdiff(names(df), req_cols)
  
  # Determine which variables do not need to be extended 
  df %>% 
    filter(year < 1750) %>% 
    distinct(variable) -> 
    complete_early_yrs
  
  df %>% 
    filter(year == 1750) %>% 
    filter(!variable %in% complete_early_yrs$variable) -> 
    values_1750
  
  current_start_yr <- 1750
  yrs <- 1745:(current_start_yr-1)
  n <- length(yrs) 
  rows <- nrow(values_1750)
  
  
  # Duplicate the data frame contents and add the years. 
  do.call(rbind, replicate(n, values_1750, simplify = FALSE))  %>% 
    mutate(year = rep(yrs, each = rows)) %>% 
    select(names(df)) -> 
    missing_data 
  
  # Add the missing data to the data frame. 
  missing_data %>% 
    rbind(df) %>% 
    arrange(variable, year) -> 
    out

    return(out)

}

# TODO might need to generalize or something 
# Args 
#   df: data.frame of emissions that are missing data
#   mean_years: vector of years to use the mean to replicate over
#   extend_to: the year to extend/replicated data to
# Returns: data.frame of emissions
uniform_extend_df <- function(df, mean_yrs, extend_to){
  
  # Throw an error if any of the required columns are missing 
  req_cols <- c("year", "value")
  stopifnot(all(req_cols %in% names(df)))
  
  # Determine which of the columns that need to be replicated
  group_cols <- setdiff(names(df), req_cols)
  
  # Get the mean values that are going to be 
  # replicated over the missing years. 
  df %>% 
    filter(year %in% mean_yrs) %>%
    summarise(value = mean(value), .by = all_of(group_cols)) -> 
    mean_vals 
  
  # TODO is the assumption that the data is all ends the 
  # same year too simplistic? 
  # 
  # Determine the number of years to replicate the data over 
  # and create a vector of the years to be added to the data frame.
  current_end_yr <- max(df$year)
  yrs <- (current_end_yr+1):extend_to
  n <- length(yrs) 
  rows <- nrow(mean_vals)
  
  # Duplicate the data frame contents and add the years. 
  do.call(rbind, replicate(n, mean_vals, simplify = FALSE))  %>% 
    mutate(year = rep(yrs, each = rows)) %>% 
    select(names(df)) -> 
    missing_data 
  
  # Add the missing data to the data frame. 
  missing_data %>% 
    rbind(df) %>% 
    arrange(variable, year) -> 
    out
  
  return(out)
  
}



# Write a hector input table Save the hector csv files into the proper hector format
# Args
#   x: data table containing Hector input values
#   required: str vector of the required variables that must be included in the table
#   write_to: str directory to write the hector csv output to
#   info_source: string name indicating the source part of the module name
#   end_tag: string used at the end of of the table file, in most cases will be default name "_emiss-constraints_rf"
#   str: file name
# Return:
write_hector_csv <- function(x, 
                             required=NULL, 
                             write_to, 
                             info_source = "hector_ceds", 
                             end_tag = "_emiss"){
  
  # Format and save the emissions and concentration constraints in the csv files
  # in the proper Hector table input file.
  assert_that(dir.exists(write_to))
  assert_that(has_name(x, c("scenario", "year", "variable", "units", "value")))
  x <- as.data.table(x)
  
  # Create the file name
  scn   <- unique(x[['scenario']])
  assert_that(length(scn) == 1)
  fname <- file.path(write_to, paste0(info_source, '_', scn, end_tag, '.csv'))
  
  if(!is.null(required)){
    missing <- !required %in% unique(x[["variable"]])
    assert_that(all(!missing), msg = paste("Missing required variable(s):", paste0(required[missing], collapse = ", ")))
  }
  
  # Transform the data frame into the wide format that Hector expects.
  input_data <- dcast(as.data.table(x)[, list(Date = year, variable, value)], Date ~ variable)
  
  # Add the header information to the csv table.
  # TODO look into a more efficient way to do this, one that does not
  # require intermediate products to be written out to the disk.
  readr::write_csv(input_data, fname, append = FALSE, col_names = TRUE)
  lines <- readLines(fname)
  
  # Format a list of units that will be used in the header.
  vars <- names(input_data)
  var_units <- getunits(vars[-1])
  units_list <- paste(c('; UNITS:', var_units), collapse = ', ')
  
  git_tag <- substr(system("git rev-parse HEAD", intern=TRUE), start = 1, stop = 15)
  create_info <-  c(paste0('; created by ceds4hector on ', date(),
                           " commit ", git_tag))
  final_lines <- append(c(paste0('; ', scn, " from ", info_source),
                          paste0("; commit ", git_tag),
                          paste0("; date ", date()),
                          units_list),
                        lines)
  writeLines(final_lines, fname)
  return(fname)
  
}




# Args
#   ini: lines of a Hector ini file.
#   replacement_path: the file path to the hector input csv table.
#   run_name: character name of run name.
#   pattern: regular expression pattern of the csv paths to replace with `replacement_path`
#            the default is set to replace all emission and concentration constraints.
# Returns: lines of a Hector ini file.
replace_csv_string <- function(ini, replacement_path, run_name, pattern = "=csv:.*TEMPLATE.csv"){
  
  # Make sure the pattern exists.
  assert_that(any(grepl(pattern = pattern, x = ini)))
  
  # Replace the path in the ini to the template csv file with a path to the input table.
  new_ini <- gsub(pattern = pattern, replacement = paste0('=csv:', replacement_path), x = ini)
  new_ini <- gsub(pattern = 'TEMPLATE', replacement = run_name, x = new_ini)
  
  return(new_ini)
}

# Write a ini file for a particular csv table
# Args
#   path: str pathway to the csv table of emisisons & inputs
# Returns: str path where the ini file is written out to
write_hector_ini <- function(path){
  
  # Read in the ini template.
  template_ini <- readLines(file.path(DIRS$DATA, "hector_TEMPLATE.ini"))
  
  name <- gsub(x = basename(path), pattern = "_emiss.csv", replacement = "")
  csv <- file.path(".", basename(DIRS$TABLES), basename(path))
  
  new_ini <- replace_csv_string(ini = template_ini, replacement_path = csv, run_name = name)
  
  out_file <- file.path(DIRS$TABLES, "..", paste0(name, ".ini"))
  writeLines(new_ini, con = out_file)
  
  return(out_file)
}
