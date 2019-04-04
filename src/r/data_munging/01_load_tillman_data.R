# Load Tillman's publication data and assign unique panel key ====================
rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- "haven"
missing <- which(!(packs %in% rownames(installed.packages())))
if(any(missing)) {
    cat("Installing missing packages: ", packs[missing], "\n")
    install.packages(packs[missing], dependencies = TRUE)
}
tillman <- haven::read_dta(
    file.path(path_project, "dta", "raw", "pec_elections_apr13.dta")
)
tillman <- select(tillman,
    country, year, enep, disprop, pr, plurality, closeness, growth, lnincome,
    year, pec1, pec20, vote_pec, smallpec, largepec, turnout
) %>% 
    mutate(turnout = turnout / 100, vote_pec = vote_pec / 100) %>%
    mutate(year2 = year * 10 + duplicated(
        paste(tillman$country, tillman$year, sep = ":"))
    ) %>%
    filter(!is.na(country) & (country != ""))
bw <- select(tillman, -year, -year2) %>%
    group_by(country) %>%
    summarize_all(list(bw = mean)) %>%
    ungroup()
tillman <- left_join(tillman, bw, by = "country")
# generate group means and demeaned variables
vars <- sapply(
    str_split(names(tillman)[grep("bw", names(tillman), fixed = TRUE)], "(\\_bw)"),
    "[", 1
)
for (i in vars) {
    tillman[, paste(i, "wi", sep = "_")] <-
        tillman[, i] - tillman[, paste(i, "bw", sep = "_")]
}
write_csv(x = tillman, path = file.path(path_project, "out", "tillman_raw.csv"))
# NOTE: Panel country X year is not unique. Early elections in Greece & UK.
# housekeeping ===================================================================
clean_workspace <- c(clean_workspace, "tillman")
rm(list = ls()[!(ls() %in% clean_workspace)])
## END
