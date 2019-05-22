# This script merges all data provided by the chair and not previously included
# into country_panel.
#
# Author: Dag Tanneberg
# Last update: 2019/04/25
# Version info:
#   2019/05/18 Removed TUR/CYP from source. Deleted redundant code.
#   2019/05/15 Found country entries TUR and CYP during EDA, which are not included
#       in the original raw data. Panel entries are dropped for the time being.
#   2019/05/14 Commented any_fooBar indicator variables b/c not included in
#       latest lsvergl data set version. Added election date variable to
#       new_content
#   2019/05/01 Switched back to foreign::read.dta b/c STATA labeled values
#       caused problems. Added new PEC information to object new_content.
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
for (p in packs) library(p, character.only = TRUE, quietly = TRUE)

# Declare Constants
panel_id <- c("iso3c", "year2")
new_content <- c("election_date", "election_id", "twobiggest_parties", "blocvotes",
    "additional_identifiability", "nupec_neu", "pec_neu", "pectotal_neu",
    "pectotal_inc",
    "pectotal_other", "pec10_neu", "pec20_neu", "pec30_neu",
    "enp_votes", "enp_seats", "advantage_ratio", "polarization", "dm_eff",
    "in_lsvergl"
    # paste("any", c(paste0("type", 1:6), "progr", "incumbent"), sep = "_")
)

# Define Data Objects
lsvergl <- read.dta(
    file.path(path_project, "dta", "raw", "PEC_LSVERGL_1.dta")
) %>%
    mutate(in_lsvergl = 1) # flag for country_panel
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
country_panel <- full_join(country_panel, lsvergl, by = c("iso3c", "year2"))
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
for (p in packs) detach(paste("package", p, sep = ":"), character.only = TRUE)
rm(list = ls()[!(ls() %in% clean_workspace)])