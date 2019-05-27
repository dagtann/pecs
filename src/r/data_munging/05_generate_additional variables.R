rm(list = ls()[!(ls() %in% clean_workspace)])
# Generate smallpec / largepec indicators
pecs <- read_dta(
    file.path(path_project, "dta", "raw", "PEC_raw_stable15_2.dta"),
    encoding = "latin1"
)

pec_indicators <- grep("^[pec1-9]{4}$", names(pecs), perl = TRUE, value = TRUE)
pec_shares <- paste(pec_indicators, "share", sep = "_")

# calculate vote share for each pec
for (i in pec_indicators) {
    # cat("Processing on:", i, "\n")
    mask <- pecs[, i] == 1
    assign("tmp",
        aggregate(vote_share ~ election_id, data = pecs[mask, ],
            FUN = sum, na.rm = TRUE
        )
    )
    # cat("Result has shape", nrow(tmp), "entries.\n")
    names(tmp)[2] <- paste(i, "share", sep = "_")
    # print(head(tmp))
    country_panel <- left_join(country_panel, tmp, by = "election_id")
}
summary(country_panel[, pec_shares])
country_panel[, "smallpec_neu"] <- apply(
    country_panel[, pec_shares], 1, function(r) any(r < .2, na.rm = TRUE)
)
country_panel[, "largepec_neu"] <- apply(
    country_panel[, pec_shares], 1, function(r) any(r >= .4, na.rm = TRUE)
)
# Sanity checks: Are there gross deviations?
with(country_panel, table(smallpec, smallpec_neu))
with(country_panel, table(largepec, largepec_neu))

# Create cabinet member indicator
cabinet <- readxl::read_excel(
    file.path(path_project, "dta", "raw", "parlgov-stable.xlsx"), sheet = "cabinet"
)

rm(list = ls()[!(ls() %in% clean_workspace)])