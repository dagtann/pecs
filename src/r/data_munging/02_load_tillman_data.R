# Load Tillman's publication data and assign unique panel key
# Author: Dag Tanneberg
# Last update: 04/17/2019
# Version info:
#   04/17/2019: Formatting, removed a filter statement that dropped
#        Australia, Canada, and Luxembourg from the data.
# =============================================================================
rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- c("haven", "countrycode")
missing <- which(!(packs %in% rownames(installed.packages())))
if (any(missing)) {
    cat("Installing missing packages: ", packs[missing], "\n")
    install.packages(packs[missing], dependencies = TRUE)
}
lapply(packs, library, character.only = TRUE)


# Load Tillman's panel
tillman <- read_dta(
    file.path(path_project, "dta", "raw", "pec_elections_apr13.dta")
)
tillman <- select(tillman,
    country, year, enep, disprop, pr, plurality, closeness, growth, lnincome,
    year, pec1, pec20, vote_pec, smallpec, largepec, turnout
) %>%
    mutate(year2 = year * 10 + duplicated(
        # Panel country X year !unique. Early elections in Greece & UK.
        paste(tillman$country, tillman$year, sep = ":"))
    ) %>%
    filter(
        !is.na(country) & (country != "")
    ) %>%
    mutate(iso3c = countrycode(country, "country.name", "iso3c", warn = TRUE)) %>%
    mutate(in_tillman = TRUE)


# Execute fixed effects transformation
bw <- select(tillman, -country, -year, -year2, -in_tillman) %>%
    group_by(iso3c) %>%
    summarize_all(list(bw = mean)) %>%
    ungroup()
tillman <- left_join(tillman, bw, by = "iso3c")
vars <- sapply( # stemmed varnames for w/i transformation
    str_split(
        names(tillman)[grep("bw", names(tillman), fixed = TRUE)], "(\\_bw)"
    ), "[", 1
)
for (i in vars) {
    tillman[, paste(i, "wi", sep = "_")] <-
        tillman[, i] - tillman[, paste(i, "bw", sep = "_")]
}


# merge into country_panel
country_panel <- full_join(
    country_panel, select(tillman, -country, -year), by = c("iso3c", "year2")
) %>%
    mutate(in_tillman = ifelse(is.na(in_tillman), 0, in_tillman))
# housekeeping ===================================================================
write_csv2(tillman, path = file.path(path_project, "out", "tillman_raw.csv"))
clean_workspace <- append(clean_workspace, "tillman")
rm(list = ls()[!(ls() %in% clean_workspace)])
## END
