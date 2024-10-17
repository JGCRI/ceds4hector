# how differen

scns <- c("ssp119", "ssp245", "ssp585")

list.files(system.file(package = "hector", "input/tables"),
           pattern = paste0(scns, collapse = "|"), full.names = TRUE) %>%
    lapply(function(l){

        scn <- gsub(x = basename(l), pattern = "_emiss-constraints_rf.csv", replacement = "")
        d <- read.csv(l, comment.char = ";")
        dd <- d[ , c("Date", "SV", "RF_albedo")]
        dd$scn <- scn
        return(dd)

    }) %>%
    bind_rows() ->
    rf


rf %>%
    ggplot(aes(Date, SV, color = scn)) +
    geom_line()


rf %>%
    ggplot(aes(Date, RF_albedo, color = scn)) +
    geom_line()
