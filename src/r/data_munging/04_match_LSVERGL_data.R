# This script merges all data provided by the chair and not previously included
# into country_panel.
#
# Author: Dag Tanneberg
# Last update: 2019/04/25
# Version info:
#   2019/04/25 Switched from haven::read_dta to foreign::read.dta. Primary key
#       now imports as string and can be matched with country_panel. Finished
#       import and merge. Implemented consistentcy checks.
#   2019/04/25 Aborted b/c primary key in lsvergl could not be identified
# =============================================================================
rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- c("foreign")
missing <- which(!(packs %in% rownames(installed.packages())))
if (any(missing)) {
    cat("Installing missing packages: ", packs[missing], "\n")
    install.packages(packs[missing], dependencies = TRUE)
}
lapply(packs, library, character.only = TRUE)

# Declare Constants
panel_id <- c("iso3c", "year2")
new_content <- c("twobiggest_parties", "blocvotes",
    "additional_identifiability", "pec_neu", "pectotal_neu", "pectotal_inc",
    "pectotal_other", "pec10_neu", "pec20_neu", "pec30_neu",
    "enp_votes", "enp_seats", "advantage_ratio", "polarization", "dm_eff",
    paste("any", c(paste0("type", 1:6), "progr", "incumbent"), sep = "_")
)

# Define Data Objects
lsvergl <- read.dta(
    file.path(path_project, "dta", "raw", "Tillman_LSVERGL.dta")
)
# Check Data Integrity
# Sanity check: Are panel entries uniquely identified?
duplicates <- with(lsvergl, duplicated(paste(c, year2, sep = ":")))
table(duplicates); rm(duplicates)
# (c, year2) is the primary key. What format is c?
class(lsvergl$c) # Expected character. Is factor b/c STATA numlabel applied
lsvergl <- lsvergl %>% mutate(iso3c = as.character(c))
# Sanity check: Type conversion successful? Any NA entries?
summary(lsvergl$iso3c); table(lsvergl$iso3c, exclude = NULL)
lsvergl <- select(lsvergl, c(panel_id, new_content))

# Sanity check: plausible ranges on content?
summary(lsvergl[, new_content])


# Merge Datasets
country_panel <- left_join(country_panel, lsvergl, by = c("iso3c", "year2"))
tillman <- left_join(tillman, lsvergl, by = c("iso3c", "year2"))  # Trainingset


# Execute fixed effects transformation for Tillman
polarization_bw <- tillman %>%
    select(iso3c, polarization) %>%
    group_by(iso3c) %>%
    summarize_all(list(bw = mean), na.rm = TRUE) %>%
    ungroup() %>%
    rename(polarization_bw = bw)
tillman <- left_join(tillman, polarization_bw, by = "iso3c")
tillman <- mutate(tillman, polarization_wi = polarization - polarization_bw)

# Housekeeping
lapply(paste("package", packs, sep = ":"), detach, character.only = TRUE)
rm(list = ls()[!(ls() %in% clean_workspace)])