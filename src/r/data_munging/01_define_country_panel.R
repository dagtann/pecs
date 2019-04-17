# This script generates a country-panel which includes numerous identifier
# codes.
# Author: Dag Tanneberg
# Last update: 04/17/2019
# Version info:
#   04/17/2019: Script setup.
# =============================================================================
rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- "countrycode"
missing <- which(!(packs %in% rownames(installed.packages())))
if(any(missing)) {
    cat("Installing missing packages: ", packs[missing], "\n")
    install.packages(packs[missing], dependencies = TRUE)
}
lapply(packs, library, character.only = TRUE)


# Get panel from countrycode data
country_panel <- codelist_panel
names(country_panel) <- str_replace_all(names(country_panel), "\\.", "_")


# Sanity check: What are the dimensions of the data?
# dim(country_panel) # 30806 entries
# length(unique(country_panel$country)) # 280 countries
# range(country_panel$year) # 1789 - 2018
# # Sanity check: Are all panel entries unique?
# duplicated <- with(country_panel,
#     duplicated(paste(country_name_en, year, sep = ":"))
# )
# table(duplicated)
# duplicated <- with(country_panel,
#     duplicated(paste(iso3c, year, sep = ":"))
# )
# table(duplicated)
# table(country_panel[as.logical(duplicated), "country_name_en"])
# duplicates for countries not in chair data, e.g., German principalities, Yemen
# Sanity check: gaps in panel?
# unique(country_panel[is.na(country_panel$iso3c), "country_name_en"])
# iso3c is NA for numerous historical countries
country_panel <- filter(country_panel, !is.na(iso3c))
# Define 2nd composite key element -> NOTE 1
country_panel[, "year2"] <- country_panel$year * 10

# housekeeping
clean_workspace <- append(clean_workspace, "country_panel")
detach(package:countrycode)
rm(list = ls()[!(ls() %in% clean_workspace)])
## END
# NOTE 1: Some countries hold two or more elections in the same year. Therefore,
#     panel entries are not unique. Year2 is used to encode the first, second, etc.
#     election in each year.