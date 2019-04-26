
rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- c("haven", "countrycode")
missing <- which(!(packs %in% rownames(installed.packages())))
if(any(missing)) {
    cat("Installing missing packages: ", packs[missing], "\n")
    install.packages(packs[missing], dependencies = TRUE)
}
lapply(packs, library, character.only = TRUE)

lsvergl <- read_dta(file.path(path_project, "dta", "raw", "Tillman_LSVERGL.dta"))
lsvergl$country_id

vars_to_keep <- (1:ncol(lsvergl))[names(lsvergl) %in% c("cid", "year2", "enpv", "enps", "dm_eff", "country_name")]
loc_nupec_neu <- grep(pattern = "nupec_neu", x = names(lsvergl), fixed = TRUE)
loc_polarization <- grep(pattern = "polarization", x = names(lsvergl), fixed = TRUE)
vars_to_keep <- append(vars_to_keep, loc_nupec_neu:loc_polarization)
lsvergl <- lsvergl[, sort(vars_to_keep)]

lsvergl <- lsvergl %>%
    mutate(iso3c = countrycode(cid, "iso3n", "iso3c", warn = TRUE))

any(is.na(lsvergl$iso3c)) & any(is.na(lsvergl$cid))
table(is.na(lsvergl$cid))
unique(lsvergl$country_name[is.na(lsvergl$cid)])


tmp <- left_join(country_panel, lsvergl, by = c("iso3c", "year2"))



str(tmp)
table(lsvergl$c)