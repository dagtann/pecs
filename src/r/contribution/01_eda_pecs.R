# EDA on full PECs data
library("haven")

pecs <- read_dta(
    file.path(path_project, "dta", "raw", "PEC_all.dta"), encoding = "latin1"
)

# Extract basic descriptives
length(unique(pecs[["country_name_short"]])) # 37 ctries
length(unique(pecs[["election_id"]])) # 297 elections
summary(pecs$eyear) # 1945 - 2015, 9 NAs
pecs$edate[is.na(pecs$eyear)] # edate also missing
pecs$country_name_short[is.na(pecs$eyear)] # all EST?
# Coding Error

tmp <- aggregate(
    election_id ~ country_name_short, data = pecs,
    FUN = function(x) length(unique(x))
)
summary(tmp$election_id) # 1 to 27 elections in which PECs formed
# Lists SA, VIC, WA which are Australian Federal States -> Find and remove
# from the data file