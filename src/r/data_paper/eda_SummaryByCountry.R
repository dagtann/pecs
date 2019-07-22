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

# functions
find_mode <- function(x){
    tbl <- table(x)
    max_freq <- max(tbl)
    names(which(tbl == max_freq))
}

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
lsvergl <- read_dta(
    file.path(path_project, "dta", "raw", "PEC_LSVERGL_2.dta"), encoding = "latin1"
)


# Data dimensions
length(unique(pecs$iso3c))  # 35
length(unique(pecs$election_id))  # 562
range(year(pecs$election_date))  # 1945 - 2015
sum(lsvergl$nupec_neu)  # 493


# Detailed summary table by country
table_data <- lsvergl %>%
    rename(iso3c = country_name_short) %>%
    group_by(country_name, iso3c) %>%
    mutate(first_year = min(year(election_date)),
        last_year = max(year(election_date)), no_elections = length(election_id),
        no_pecs = sum(nupec_neu), perc_elec = mean(pec_neu),
        mean_pectotal = mean(pectotal_neu), no_inc = sum(pec_inc)) %>%
    select(country_name, iso3c, first_year, last_year, no_elections, no_pecs,
        perc_elec, mean_pectotal, no_inc) %>%
    summarize_all(mean)

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
                            by = c("iso3c", "election_id", "pec_no"))
mode_type <- aggregate(types ~ iso3c, data = pecs_long, FUN = find_mode)
names(mode_type)[2] <- "mode_type"
no_prog <- pecs_long %>%
    group_by(iso3c) %>%
    mutate(no_prog = sum(programs, na.rm = TRUE)) %>%
    select(iso3c, no_prog) %>%
    summarize_all(mean) %>%
    ungroup()

table_data <- left_join(table_data, mode_type, by = "iso3c")
table_data <- left_join(table_data, no_prog, by = "iso3c")
table_data <- table_data %>% select(-iso3c)
write_csv2(table_data, file.path(path_project, "out", "eda_summaryByCountry.csv"))