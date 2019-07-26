# Merge information on the electoral system from VDEM and Gallagher/Gandrud into
# the panel.
# Author: Dag Tanneberg
# Last update: 04/17/2019
# Version info:
#   07/16/2019: Dropped Gandrud dispropotionality data b/c matching problems
#       country_panel
#   07/15/2019: Added economic growth calculation
#   05/06/2019 Added log gdp variable
#   05/22/2019 Added turnout to vdem import, fixed year2 bug in disproportionality
#       NOTE: Also added gdp per capita from VDEM which is obviously not an
#       institution
#   04/17/2019 Rewrote & -factored after data loss
# ================================================================================
rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- c("data.table", "countrycode", "lubridate")
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
country_panel <- mutate(country_panel, ln_e_migdppc = log(e_migdppc)) %>%
    group_by(iso3c) %>%
    mutate(growth_neu = (lag(e_migdppc, order_by = year) - e_migdppc) / e_migdppc * 100) %>%
    ungroup()
# summary(country_panel[, c("e_migdppc", "ln_e_migdppc", "growth_neu")])
# golder <- read_dta(file.path(path_archive, "electoralSystems", "v3",
#     "es_data-v3", "es_data-v3.dta"), encoding = "latin1"
# )
# tmp <- golder %>% select(ccode, year, elec_id, tier1_avemag) %>%
#     mutate(year2 = year * 10) %>%
#     mutate(year2 = ifelse())
# mask <- with(country_panel, year2 %% year != 0)
# dmy(golder$date) %in% ymd(country_panel[mask, c("election_date", "iso3c")])

# Housekeeping
rm(list = ls()[!(ls() %in% clean_workspace)])
detach(package:data.table)
## END