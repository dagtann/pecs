# This script compiles frequency distributions for each type of pre-electoral
# coaltion, the existence of a joint program and their incumbency status.
# Frequency distributions are augmented by information on the median electoral
# system which each type of pre-electoral coalition operates in. The script
# returns an Excel ready csv-file.
# Author: Dag Tanneberg, dag.tanneberg@uni-potsdam.de
# Version info:
#     07/12/2019: Refactored data aggregation process, added table output routine.
# =============================================================================
# Preamble
rm(list = ls()[!(ls() %in% clean_workspace)])

# Functions
calc_closeness <- function(votes) {
    # Closeness of elections: Distance between 2 strongest parties.
    sorted <- sort(votes, decreasing = TRUE)
    return(sorted[1] - sorted[2])
}

# Raw data & Hooks
pecs <- read_dta(
    file.path(path_project, "dta", "raw", "PEC_raw_stable15_2.dta"),
    encoding = "latin1"
) %>%
    rename(iso3c = country_name_short) %>%
    group_by(election_id) %>%
    mutate(closeness_neu = calc_closeness(vote_share)) %>%
    ungroup()
indicators <- list(
    types = grep("^pec[0-9]_type$", names(pecs)),
    incumbents = grep("^pec[0-9]_incumbent$", names(pecs)),
    programs = grep("^pec[0-9]_prog$", names(pecs))
)
esystem_properties <- c("dm_eff", "disproportionality", "enp_votes")

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
mask <- apply(pecs_long[, names(indicators)], 1, function(r) all(is.na(r)))
pecs_long <- pecs_long[!mask, ]


# Prep table output
pec_freqs <- apply(pecs_long[, c("types", "programs", "incumbents")], 2,
    function (col) cbind(
        abs_freq = table(col), rel_freq = prop.table(table(col))
    )
)
tmp <- select(country_panel, iso3c, election_id, esystem_properties)
pecs_long <- left_join(pecs_long, tmp, by = c("iso3c", "election_id"))
esystem_data <- lapply(
    names(indicators),
    function (x) {
        aggregate(pecs_long[, esystem_properties],
            list(types = pecs_long[[x]]), FUN = median, na.rm = TRUE
        )
    }
)
esystem_data <- do.call(rbind, esystem_data)

# Return table
attribute_labels <- c("Type", "", "", "", "", "", "Joint program?", "",
    "Incumbent coalition?", ""
)
value_labels <- c("Nomination Agreement", "Joint List",
    "Dual-Ballot Instructions", "Vote Transfer Instructions",
    "Public Commitment", "Other", "No", "Yes", "No", "Yes"
)
tbl <- cbind(do.call(rbind, pec_freqs), t(do.call(rbind, esystem_data)))
tbl <- tbl[, c(1, 2, 4:ncol(tbl))]
tbl <- data.frame(Attribute = attribute_labels, Value = value_labels, tbl)
write_csv2(tbl,
    file.path(path_project, "out", "eda_pecTypeXelectoralSystem.csv")
)
tmp <- readLines(
    file.path(path_project, "out", "eda_pecTypeXelectoralSystem.csv")
)
tmp[1] <- paste0("Attribute;Value;Abs. Freq.;Rel. Freq;District Magnitude;",
    "Disproportionality;Electoral Parties"
)
writeLines(tmp, file.path(path_project, "out", "eda_pecTypeXelectoralSystem.csv"))

# Housekeeping
rm(list = ls()[!(ls() %in% clean_workspace)])
## End