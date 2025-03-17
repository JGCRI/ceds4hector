# Helper script that loads the Hector emission inputs as a sort of
# comparison.

# 1. Import the emissions data -------------------------------------------------
system.file("input/tables/ssp245_emiss-constraints_rf.csv", package = "hector") %>%
    read.csv(comment.char = ";") ->
    wide_results

# Save only a copy of the emissions and the dates
to_keep <- c("Date", colnames(wide_results)[grepl(x = colnames(wide_results), pattern = "emissions")])

# For only the emissions change the formatting of Hector form wide to long,
# for easy plotting comparisons
wide_results[, to_keep] %>%
    pivot_longer(-Date) %>%
    select(year = Date, variable = name, value) %>%
    filter(year <= 2014) ->
    hector_comp

remove( list = c("wide_results", "to_keep"))
