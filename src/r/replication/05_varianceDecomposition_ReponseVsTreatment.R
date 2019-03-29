library("gridExtra")
btw_dta <- select(tillman, country, pec1, vote_pec, turnout) %>%
    group_by(country) %>%
    summarise_all(.funs = mean, na.rm = TRUE) %>%
    rename(bw_pec1 = pec1, bw_vote_pec = vote_pec, bw_turnout = turnout)
tillman <- left_join(tillman, btw_dta, by = "country")
for(v in c("pec1", "vote_pec", "turnout")){
    tillman[, paste("wi", v, sep = "_")] <-
        tillman[, v] - tillman[, paste("bw", v, sep = "_")]
}

# (a) pec1 -----------------------------------------------------------------------
countries <- unique(tillman[["country"]])
r <- vector(mode = "numeric", length = length(countries))
names(r) <- countries
for(i in countries){
    filter <- which(tillman[, "country"] == i)
    r[i] <- cor(
        tillman[filter, "wi_turnout"], tillman[filter, "wi_pec1"],
        use = "complete.obs"
    )
}
r[is.na(r)] <- 999
tillman <- within(tillman,
    country_f <- factor(country, levels = countries[order(r)])
)
p1 <- ggplot(data = tillman, aes(x = wi_pec1, y = wi_turnout)) + 
    geom_smooth(method = "lm", se = FALSE) +
    geom_point(alpha = .6) +
    facet_wrap(~ country_f, ncol = 3) +
    labs(x = "PEC in election?", y = "Turnout")

# (b) vote_pec
for(i in countries){
    filter <- which(tillman[, "country"] == i)
    r[i] <- cor(
        tillman[filter, "wi_turnout"], tillman[filter, "wi_vote_pec"],
        use = "complete.obs"
    )
}
tillman <- within(tillman,
    country_f <- factor(country, levels = countries[order(r)])
)
p2 <- ggplot(data = tillman, aes(x = wi_vote_pec, y = wi_turnout)) + 
    geom_smooth(method = "lm", se = FALSE) +
    geom_point(alpha = .6) +
    facet_wrap(~ country_f, ncol = 3) +
    labs(x = "Percent of vote for PEC", y = "Turnout")
p_total <- grid.arrange(p1, p2, ncol = 2)
ggsave(
    file.path(path_project, "out", "scatter_VarianceDecomposition_TreatmentVsOutcome.png"),
    p_total
)

## END