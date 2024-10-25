# Combine the CEDS and RCMIP emissions into the format required by Hector.
# This level will write the output tables out ready for use by Hector.

# 0. Set Up -------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Load the data! Right now everything is in the hector units.
rcmip_emiss <- read.csv(file.path(DIRS$L1, "L1.hector_rcmip_emiss.csv"))
ceds_emiss <- read.csv(file.path(DIRS$L1, "L1.ceds_hector.csv"))

# Read in the ini template.
template_ini <- readLines(file.path(DIRS$DATA, "hector_TEMPLATE.ini"))


# Write a hector input table Save the hector csv files into the proper hector format
# Args
#   x: data table containing Hector input values
#   required: str vector of the required variables that must be included in the table
#   write_to: str directory to write the hector csv output to
#   info_source: string name indicating the source part of the module name
#   end_tag: string used at the end of of the table file, in most cases will be default name "_emiss-constraints_rf"
#   str: file name
# Return:
write_hector_csv <- function(x, required=NULL, write_to, info_source = "hector_ceds", end_tag = "_emiss"){

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
    create_info <-  c(paste0('; created by hector-ceds on ', date(),
                             " commit ", git_tag))
    final_lines <- append(c(paste0('; ', scn, " from ", info_source),
                            paste0("; commit ", git_tag),
                            paste0("; date ", date()),
                            units_list),
                          lines)
    writeLines(final_lines, fname)
    return(fname)

}


# Replace the emissions csv file strings with the path to the new csv table
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

    name <- gsub(x = basename(path), pattern = "_emiss.csv", replacement = "")
    csv <- file.path(".", basename(DIRS$TABLES), basename(path))

    new_ini <- replace_csv_string(ini = template_ini, replacement_path = csv, run_name = name)

    out_file <- file.path(DIRS$TABLES, "..", paste0(name, ".ini"))
    writeLines(new_ini, con = out_file)

    return(out_file)
}


# 1. Main Chunk ----------------------------------------------------------------

# Replace the default emissions with the ceds emissions.
ceds_emiss %>%
    select(scenario, year, variable) %>%
    distinct() %>%
    mutate(drop = 1) ->
    ceds_meta_df

# Replace the RCMIP emissions with the CEDS emissions.
rcmip_emiss %>%
    left_join(ceds_meta_df, by = join_by(scenario, year, variable)) %>%
    filter(is.na(drop)) %>%
    select(-drop) %>%
    rbind(ceds_emiss) %>%
    mutate(scenario = "historical") %>%
    filter(year <= 2022) ->
    hector_emissions

# Write the tables out!
hector_emissions %>%
    distinct() %>%
    split(f = .$scenario) %>%
    lapply(FUN = write_hector_csv, write_to = DIRS$TABLES) ->
    tables

# Write the ini files out!
tables %>%
    lapply(FUN = write_hector_ini) ->
    inis
