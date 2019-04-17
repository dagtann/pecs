
# rm(list = ls()[!(ls() %in% clean_workspace)])
# packs <- "countrycode"
# missing <- which(!(packs %in% rownames(installed.packages())))
# if(any(missing)) {
#     cat("Installing missing packages: ", packs[missing], "\n")
#     install.packages(packs[missing], dependencies = TRUE)
# }
# lapply(packs, library, character.only = TRUE)

# # vdem data
# vdem9 <- read_csv(
#     unz(
#         file.path(path_archive, "vdem", "v9", "Country_Year_V-Dem_Full+others_CSV_v9.zip"),
#         "Country_Year_V-Dem_Full+others_CSV_v9/V-Dem-CY-Full+Others-v9.csv"
#     )
# )
# electoral_system <- select(vdem9,
#     country_name, country_id, year, COWcode,
#     v2elloeldm, v2elcomvot, v2elparlel, v2ellovtlg, v2ellovtsm
# ) %>%
# rename(ccode = COWcode)
# # Sanity check: Reasonable ranges on each variable?
# # apply(electoral_system[, -1], 2, summary)

# # Gandrud disproportionality
# disproportionality <- read_csv(
#     file.path(path_archive, "disproportionality", "Disproportionality.csv")
# ) %>%
#     mutate(ccode = countrycode(iso2c, "iso2c", "cown")) %>%
#     filter(!is.na(ccode))
#  filter <- duplicated(paste(disproportionality$ccode, disproportionality$year))
# disproportionality[filter, ]

# tmp <- left_join(electoral_system, disproportionality, by = c("ccode", "year"))