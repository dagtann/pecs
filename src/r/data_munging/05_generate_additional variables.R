# Version info:
    # 07/15/2019: Script now calculates closeness of elections
rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- c("lubridate")
if (!all(packs %in% installed.packages())) {
    mask <- packs %in% installed.packages()
    cat("Installing packages:", packs[!mask], "\n")
    install.packages(packs[!mask], repos = "https://cloud.r-project.org")
    rm(mask)
}
for (p in packs) library(p, character.only = TRUE)


# functions
calc_closeness <- function(votes) {
    # Closeness of elections: Distance between 2 strongest parties.
    sorted <- sort(votes, decreasing = TRUE)
    return(sorted[1] - sorted[2])
}


# data objects
pecs <- read_dta(
    file.path(path_project, "dta", "raw", "PEC_raw_stable15_2.dta"),
    encoding = "latin1"
)
pec_indicators <- grep("^[pec1-9]{4}$", names(pecs), perl = TRUE, value = TRUE)


# create by election smallpec / largepec indicators
pec_shares <- paste(pec_indicators, "share", sep = "_")
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
    country_panel[, pec_shares], 1, function(r) as.numeric(any(r < .2, na.rm = TRUE))
)
country_panel[, "largepec_neu"] <- apply(
    country_panel[, pec_shares], 1, function(r) as.numeric(any(r >= .4, na.rm = TRUE))
)
# Sanity checks: Are there gross deviations?
with(country_panel, table(smallpec, smallpec_neu))
with(country_panel, table(largepec, largepec_neu))


# create by election pec type indicators
tmp <- aggregate(
    pecs[, paste(pec_indicators, "type", sep = "_")],
    list("election_id" = pecs$election_id),
    FUN = function(x) ifelse(all(is.na(x)), 0, max(x, na.rm = TRUE))
)
type_lookup <- sort(unique(unlist(tmp[, -1])))
type_lookup <- type_lookup[-which.min(type_lookup)]
tmp <- gather(tmp, "pec", "type", 2:ncol(tmp))
res <- outer(tmp[["type"]], type_lookup, "==")
res <- as.data.frame(cbind(res, tmp[["election_id"]]))
colnames(res) <- c(paste0("any_type", type_lookup), "election_id")
res <- aggregate(
    res[, paste0("any_type", type_lookup)],
    list(election_id = res$election_id),
    FUN = max
)
country_panel <- left_join(country_panel, res, by = "election_id")

# create by election incumb & program indicators
to_aggregate <- paste(
    pec_indicators,
    rep(c("prog", "incumbent"), each = length(pec_indicators)), sep = "_"
)
tmp <- aggregate(
    pecs[, to_aggregate], list("election_id" = pecs$election_id),
    FUN = function(x) ifelse(all(is.na(x)), 0, max(x, na.rm = TRUE))
)
res <- vapply(
    c("prog", "incumbent"),
    FUN = function(suffix) {
        to_compare <- subset(tmp,  select = paste(pec_indicators, suffix, sep = "_"))
        apply(to_compare, 1, FUN = function(r) {
            ifelse(all(is.na(r)), 0, any(r == 1, na.rm = TRUE))})
    },
    FUN.VALUE = numeric(nrow(tmp))
)
res <- as.data.frame(cbind(res, tmp[["election_id"]]))
colnames(res) <- c(paste("any", c("prog", "incumbent"), sep = "_"), "election_id")
country_panel <- left_join(country_panel, res, by = "election_id")


# Compute closeness of elections
tmp <- select(pecs, election_id, vote_share) %>%
    group_by(election_id) %>%
    mutate(closeness_neu = calc_closeness(vote_share)) %>%
    ungroup()
tmp <- aggregate(closeness_neu ~ election_id, data = tmp, FUN = mean, na.rm = TRUE)
country_panel <- left_join(country_panel, tmp, by = "election_id")

# Add new econ data to tillman
tmp <- select(country_panel, iso3c, year2, e_migdppc)
tillman <- left_join(tillman, tmp, by = c("iso3c", "year2")) %>%
    group_by(iso3c) %>%
    mutate(
           growth_neu = (lag(e_migdppc, order_by = year) - e_migdppc) /
                         e_migdppc * 100,
           ln_e_migdppc = log(e_migdppc))


if (FALSE) {  # Commented b/c massive problems on join
# Create cabinet member indicator
pecs <- read_dta(
    file.path(path_project, "dta", "raw", "PEC_raw_stable15_2.dta"),
    encoding = "latin1"
)
elections_pool <- unique(pecs$election_id)
cabinets <- readxl::read_excel(
    file.path(path_project, "dta", "raw", "parlgov-stable.xlsx"),
    sheet = "cabinet"
) %>%
    mutate(
        start_date = ymd(as.character(start_date)),
        election_date = ymd(as.character(election_date))
    ) %>%
    select(previous_cabinet_id, election_id, party_id, cabinet_party
    ) %>%
    filter(election_id %in% elections_pool) %>%
    filter(!is.na(previous_cabinet_id))
mask <- with(cabinets,
    duplicated(paste(election_id, previous_cabinet_id, party_id))
)
cabinets[mask, ] # election_id == 293, party_id == 947
# entries not in lsvergl data -> drop
cabinets <- filter(cabinets, election_id != 293 & party_id != 947)
with(cabinets,
    any(duplicated(paste(election_id, previous_cabinet_id, party_id)))
)  # now unique?
tmp2 <- left_join(pecs, cabinets,
    # keep all entries in pecs, add only matching entries from cabinets
    by = c("election_id", "previous_cabinet_id", "party_id")
)
write.csv2(tmp2, file.path(path_project, "out", "pecs_wi_NA_cabParty.csv"))


tmp2 <- mutate(
    tmp2, cabinet_party = ifelse(is.na(cabinet_party), -999, cabinet_party)
)

for (i in pec_indicators) {
    cat("Processing on:", i, "\n")
    mask <- pecs[, i] == 1
    assign("tmp2",
        aggregate(cabinet_party ~ election_id, data = tmp[mask, ],
            FUN = function(x) all(x, na.rm = TRUE)
        )
    )
    print(table(tmp2[, 2], exclude = NULL))
    print(table(pecs[mask, paste(i, "incumbent", sep = "_")]))
    # cat("Result has shape", nrow(tmp), "entries.\n")
    # names(tmp)[2] <- paste(i, "share", sep = "_")
    # print(head(tmp))
    # country_panel <- left_join(country_panel, tmp, by = "election_id")
}

table(is.na(cabinets$cabinet_party))
nrow(pecs)
nrow(tmp)
table(is.na(tmp$cabinet_party))
tmp[is.na(tmp$cabinet_party), c("country_name_short", "election_date", "party_name_short")]
}

for (p in packs) detach(paste("package", p, sep = ":"), character.only = TRUE)
rm(list = ls()[!(ls() %in% clean_workspace)])
