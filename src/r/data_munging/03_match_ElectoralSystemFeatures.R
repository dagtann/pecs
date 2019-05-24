# Merge information on the electoral system from VDEM and Gallagher/Gandrud into
# the panel.
# Author: Dag Tanneberg
# Last update: 04/17/2019
# Version info:
#   05/22/2019 Added turnout to vdem import, fixed year2 bug in disproportionality
#       NOTE: Also added gdp per capita from VDEM which is obviously not an
#       institution
#   04/17/2019 Rewrote & -factored after data loss
# ================================================================================
rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- c("data.table", "countrycode")
missing <- which(!(packs %in% rownames(installed.packages())))
if(any(missing)) {
    cat("Installing missing packages: ", packs[missing], "\n")
    install.packages(packs[missing], dependencies = TRUE)
}
for (p in packs) library(p, character.only = TRUE, quietly = TRUE)


# vdem data
electoral_system <- fread(
    file.path(path_archive, "vdem", "v9", "V-Dem-CY-Full+Others-v9.csv"),
    select = c("country_id", "year", "v2elloeldm", "v2eltrnout",
        "v2elcomvot", "v2elparlel", "v2ellovtlg", "v2ellovtsm", "e_migdppc"
    )
)
electoral_system <- rename(electoral_system, vdem = country_id)
# Sanity check: Reasonable ranges on each variable?
# apply(electoral_system, 2, summary)
# Sanity check: Is each panel entry identified?
# with(electoral_system, table(duplicated(paste(vdem, year, sep = ":"))))
# NOTE: Each panel entry is unique. Consequently, VDEM has a secret(?) method to
#   deal with multiple elections within the same country-year.
country_panel <- left_join(country_panel, electoral_system, by = c("vdem", "year"))


# Gandrud disproportionality
disproportionality <- fread(
    file.path(path_archive, "disproportionality", "Disproportionality.csv")
) %>%
    mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c", warn = TRUE))
# Sanity check: Are all country entries known?
# table(is.na(disproportionality$iso3c))
# disproportionality[is.na(disproportionality$iso3c), ]
# Unidentified entries in the original
disproportionality <- filter(disproportionality, !is.na(iso3c))
# Sanity check: Are all panel entries unique?
# filter <- with(disproportionality, duplicated(paste(iso3c, year, sep = ":")))
# table(filter)
# disproportionality[filter, ] # Where is Great Britian 1974? -> Uses October
disproportionality <- mutate(disproportionality, year2 = year * 10) %>%
    mutate(year2 = ifelse(
        (iso3c == "IRL" & disproportionality > 2 & year == 1982) |
        (iso3c == "GBR" & disproportionality > 15 & year == 1974) |
        (iso3c == "GRC" & disproportionality > 4 & year == 1989),
        year2 + 1, year2)
    )
# filter <- with(disproportionality, duplicated(paste(iso3c, year2, sep = ":")))
# table(filter) ## All unique
country_panel <- left_join(
    country_panel, select(disproportionality, -country, -iso2c, -year),
    by = c("iso3c", "year2")
)


# Housekeeping
rm(list = ls()[!(ls() %in% clean_workspace)])
detach(package:data.table)
## END