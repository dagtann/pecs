library("plm")
library("car")

data_to_fit <- filter(
    country_panel, in_tillman == 1 & !(iso3c %in% c("LUX", "CAN"))
)
data_to_fit <- pdata.frame(data_to_fit, index = c("iso3c", "year2"))

ctry_entries <- unique(data_to_fit$iso3c)
dw_statistics <- matrix(
    FALSE, nrow=length(ctry_entries), ncol=2,
    dimnames=list(ctry_entries, c("dw", "p"))
)
names(dw_statistics) <- ctry_entries

fit1 <- plm(turnout ~ pec1 + enep + disprop * pr + plurality * closeness +
    growth + lnincome, model="within", data = data_to_fit
)
pbnftest(fit1, "bnf")
fit2 <- plm(turnout ~ vote_pec + enep + disprop * pr + plurality * closeness +
    growth + lnincome, model="within", data = data_to_fit
)
pbnftest(fit2, "bnf")
pdwtest(fit2)

for (ctry in ctry_entries) {
    fit <- lm(
        turnout ~ vote_pec + enep + disprop * pr + plurality * closeness + growth + lnincome,
        data = subset(data_to_fit, iso3c == ctry)
    )
    dw_statistics[ctry, "dw"] <- durbinWatsonTest(fit)[["dw"]]
    dw_statistics[ctry, "p"] <- durbinWatsonTest(fit)[["p"]]
}
dw_statistics

fit <- lm(
        turnout ~ vote_pec + enep + disprop * pr + plurality * closeness + growth + lnincome + iso3c,
        data = data_to_fit)
    )
durbinWatsonTest(fit)