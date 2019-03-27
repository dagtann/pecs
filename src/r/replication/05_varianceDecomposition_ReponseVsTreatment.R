
btw_dta <- select(tillman, country, pec1, vote_pec, turnout) %>%
    group_by(country) %>%
    summarise_all(.funs = mean, na.rm = TRUE) %>%
    rename(bw_pec1 = pec1, bw_vote_pec = vote_pec, bw_turnout = turnout)
tillman <- left_join(tillman, btw_dta, by = "country")
for(v in c("pec1", "vote_pec", "turnout")){
    tillman[, paste("wi", v, sep = "_")] <- tillman[, v] - 
        tillman[, paste("bw", v, sep = "_")]
}

# (a) vote_pec -------------------------------------------------------------------
ggplot(data = tillman, aes(x = wi_pec1, y = wi_turnout)) + 
    geom_point() +
    geom_smooth(se = FALSE) +
    geom_smooth(method = "lm", col = "red") +
    facet_wrap(~ country)

ggplot(data = tillman, aes(x = wi_vote_pec, y = wi_turnout)) + 
    geom_point() +
    geom_smooth(se = FALSE) +
    geom_smooth(method = "lm", col = "red") +
    facet_wrap(~ country)
## END