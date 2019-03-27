# This script decomposes the variance in the key treatment indicators into its
# between and within components. It return to graphical files.
# Prepare data -------------------------------------------------------------------
btw_dta <- select(tillman, country, pec1, vote_pec) %>%
    group_by(country) %>%
    summarise_all(.funs = mean, na.rm = TRUE) %>%
    rename(bw_pec1 = pec1, bw_vote_pec = vote_pec)
tillman <- left_join(tillman, btw_dta, by = "country")
for(v in c("pec1", "vote_pec")){
    tillman[, paste("wi", v, sep = "_")] <- tillman[, v] - 
        tillman[, paste("bw", v, sep = "_")]
}

# (a) vote_pec -------------------------------------------------------------------
label_data <- select(btw_dta, country, bw_vote_pec) %>%
    mutate(
        country_label = paste(
            country, paste0("(", round(bw_vote_pec, 2), ")"), sep = "\n")
    ) %>%
    arrange(bw_vote_pec)
p1 <- ggplot(
    data = tillman,
    aes(x = reorder(country, bw_vote_pec), y = wi_vote_pec)) +
geom_boxplot() +
scale_x_discrete(labels = label_data[["country_label"]]) +
labs(
    x = "Country\n(Country mean)", y = "Within-differences in percentage of vote",
    title = "Variance Decomposition: How well did PECs do in national elections?"
)
ggsave(
    plot = p1,
    file = file.path(path_project, "out", "boxplot_varianceDecomposition_vote_pec.png")
)

# (b) pec1 -----------------------------------------------------------------------
label_data <- select(btw_dta, country, bw_pec1) %>%
    mutate(
        country_label = paste(
            country, paste0("(", round(bw_pec1, 2), ")"), sep = "\n"
        )
    ) %>%
    arrange(bw_pec1)
p2 <- ggplot(
    data = tillman,
    aes(x = reorder(country, bw_pec1), y = wi_pec1)) +
geom_boxplot() +
scale_x_discrete(labels = label_data[["country_label"]]) +
labs(
    x = "Country\n(Country mean)", y = "Within-differences in percentage of vote",
    title = "Variance Decomposition: Did a PEC compete in national elections?"
)
ggsave(
    plot = p2,
    file = file.path(path_project, "out", "boxplot_varianceDecomposition_pec1.png")
)

# Housekeeping -------------------------------------------------------------------
filter <- as.logical(
    grepl("bw_", names(tillman), fixed = TRUE) +
    grepl("wi_", names(tillman), fixed = TRUE)
)
tillman[, !filter]
rm(list = ls()[!(ls() %in% cleanWorkspace)])
## END

