rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- c("lubridate")
if (!all(packs %in% installed.packages())) {
    mask <- packs %in% installed.packages()
    cat("Installing packages:", packs[!mask], "\n")
    install.packages(packs[!mask], repos = "https://cloud.r-project.org")
    rm(mask)
}
for (p in packs) library(p, character.only = TRUE)
rm(p)


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
    country_panel[, pec_shares], 1, function(r) any(r < .2, na.rm = TRUE)
)
country_panel[, "largepec_neu"] <- apply(
    country_panel[, pec_shares], 1, function(r) any(r >= .4, na.rm = TRUE)
)
# Sanity checks: Are there gross deviations?
# with(country_panel, table(smallpec, smallpec_neu))
# with(country_panel, table(largepec, largepec_neu))


# create by election pec type indicators
tmp <- aggregate(
    pecs[, paste(pec_indicators, "type", sep = "_")],
    list("election_id" = pecs$election_id),
    FUN = function(x) ifelse(all(is.na(x)), 0, max(x, na.rm = TRUE))
)
type_lookup <- sort(unique(unlist(tmp[, -1])))
tmp <- gather(tmp, "pec", "type", 2:ncol(tmp))
res <- outer(tmp[["type"]], type_lookup, "==")
res <- as.data.frame(cbind(res, tmp[["election_id"]]))
colnames(res) <- c(paste0("any_type", type_lookup), "election_id")
res <- na.omit(res)  # either all or no NA, all NA = not covered by pecs
res <- within(res, count <- ave(seq(1, nrow(res)), election_id, FUN = seq_along))
res <- subset(res, count == 1)
res <- res[, -ncol(res)]
country_panel <- left_join(country_panel, res, by = "election_id")


# create by election incumb & program indicators
to_aggregate <- paste(
    pec_indicators,
    rep(c("prog", "incumbent"), each = length(pec_indicators)), sep = "_")
tmp <- aggregate(
    pecs[, to_aggregate], list("election_id" = pecs$election_id),
    FUN = function(x) ifelse(all(is.na(x)), 0, max(x, na.rm = TRUE)))
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



if (FALSE) {
# Create cabinet member indicator
cabinets <- readxl::read_excel(
    file.path(path_project, "dta", "raw", "parlgov-stable.xlsx"), sheet = "cabinet"
) %>%
mutate(
    start_date = ymd(as.character(start_date)),
    election_date = ymd(as.character(election_date))
)
tmp <- select(cabinets, cabinet_id, party_id, cabinet_party) %>%
    rename(previous_cabinet_id = cabinet_id)

tmp2 <- left_join(pecs, tmp, by = c("previous_cabinet_id", "party_id"))
for (i in pec_indicators) {
    cat("Processing on:", i, "\n")
    mask <- ifelse(tmp2[, i] == 1, TRUE, FALSE)
    assign("tmp",
        aggregate(cabinet_party ~ election_id, data = tmp2[mask, ],
            FUN = print#function(x) all(x == 1, na.rm = TRUE)
        )
    )
    aggregate(cabinet_party ~ election_id, data = tmp2[mask, ],
            FUN = print#function(x) all(x == 1, na.rm = TRUE)
        )
    # cat("Result has shape", nrow(tmp), "entries.\n")
    names(tmp)[2] <- paste(i, "incumbent2", sep = "_")
    print(head(tmp))
    tmp3 <- left_join(country_panel, tmp, by = "election_id")
}
aggregate(cabinet_party ~ election_id, data = tmp2[mask, ],
            FUN = function(x) all(x == 1, na.rm = TRUE)
        )

write_csv2(tmp2, file.path(path_project, "out", "pecs_cabs"))
dim(tmp2)
table(cab_party = is.na(tmp2$cabinet_party), prev_cab = is.na(tmp2$previous_cabinet_id))



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