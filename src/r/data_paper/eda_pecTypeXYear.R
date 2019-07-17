# Preamble
rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- "lubridate"
if (!all(packs %in% installed.packages())) {
    mask <- packs %in% installed.packages()
    cat("Installing packages:", packs[!mask], "\n")
    install.packages(packs[!mask], repos = "https://cloud.r-project.org")
    rm(mask)
}
for (p in packs) library(p, character.only = TRUE)


# Raw data & Hooks
pecs <- read_dta(
    file.path(path_project, "dta", "raw", "PEC_raw_stable15_2.dta"),
    encoding = "latin1"
) %>%
    rename(iso3c = country_name_short)
indicators <- list(
    types = grep("^pec[0-9]_type$", names(pecs)),
    incumbents = grep("^pec[0-9]_incumbent$", names(pecs)),
    programs = grep("^pec[0-9]_prog$", names(pecs))
)


# Convert pecs*_type/incumbent/prog from wide to long format
aggregated_information <- lapply(
    seq(indicators),
    function(l) {
        val_label <- names(indicators)[l]
        out <- aggregate(
            pecs[, indicators[[l]]],
            list(iso3c = pecs[["iso3c"]], election_id = pecs[["election_id"]]),
            FUN = function (row) {
                ifelse(all(is.na(row)), NA, max(row, na.rm = TRUE))
            }
        )
        gather_cols <- names(out)[3:ncol(out)]
        out <- gather_(out, "pec_no", val_label, gather_cols = gather_cols)
        out <- mutate(out, pec_no = as.integer(str_sub(pec_no, 4, 4)))
        return(out)
    }
)
pecs_long <- plyr::join_all(aggregated_information,
    by = c("iso3c", "election_id", "pec_no")
)
mask <- apply(pecs_long[, c("types", "incumbents", "programs")], 1,
              function(r) all(is.na(r)))
pecs_long <- pecs_long[!mask, ]
tmp <- select(pecs, iso3c, election_id, election_date) %>%
    mutate(year = year(election_date)) %>%
    select(-election_date) %>%
    group_by(election_id) %>%
    mutate(count = seq(n())) %>%
    ungroup() %>%
    filter(count == 1) %>%
    select(-count)
pecs_long <- left_join(pecs_long, tmp, by = c("iso3c", "election_id"))


pecs_long[, "freq"] <- 1
pdta <- aggregate(freq ~ programs + year, data = pecs_long, FUN = sum)
ggplot(data = pdta, aes(x = year, y = freq)) + geom_bar(stat = "identity")
ggplot(data = pdta, aes(x = freq, fill = factor(types))) +
    geom_histogram() +
    facet_wrap(~year)


     +
    coord_flip()


ggplot(data = pecs_long, aes(x = year, fill = factor(incumbents))) + geom_bar()
ggplot(data = pecs_long, aes(x = year, fill = factor(programs))) + geom_bar()

str(pecs_long)
