rm(list = ls()[!(ls() %in% clean_workspace)])
library(foreign)
tillman <- read.dta(
    file.path( path_project, "prestudies", "ellger", "tilmann_merger.dta" )
)
detach(package:foreign)
data_to_fit <- subset(tillman,
    select = c("country", "year2", "turnout", "pec1", "enep", "disprop", "pr",
        "plurality", "closeness", "growth", "lnincome"
    )
)

library("plm")
data_to_fit <- pdata.frame(data_to_fit, index = c("country", "year2"))
summary(data_to_fit)


head(data_to_fit)
fit <- plm(
    turnout ~ pec1 +
        enep + disprop * pr + plurality * closeness + growth + lnincome,
    model = "within",
    data = data_to_fit
)
summary(fit)
detach(package:plm)
