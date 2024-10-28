# What is the difference between historical run with and without the permafrost.
library(hector)


ini <- here::here("inputs", "hector-input", "hector_ssp245_noperm.ini")
hc <- newcore(ini)
run(hc)
fetchvars(hc, 1745:2022, c(PERMAFROST_C(), CONCENTRATIONS_CH4())) %>%
    mutate(scenario = "no perm") ->
    out1


ini <- system.file(package = "hector", "input", "hector_ssp245.ini")
hc <- newcore(ini)
run(hc)
fetchvars(hc, 1745:2022, c(PERMAFROST_C(), CONCENTRATIONS_CH4())) %>%
    mutate(scenario = "perm") ->
    out2

# Looks like there is pretty small differences in the concentrations starting in
# 2000, so I don't htink that is enough cause for concern...
rbind(out1, out2) %>%
    ggplot(aes(year, value, color = scenario)) +
    geom_line() +
    facet_wrap("variable", scales = "free")
