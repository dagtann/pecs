# This script computes between-versions of Model 1 through 4 from Table 4.
# Author: Dag Tanneberg
# Date: 03/22/2019
# ================================================================================
# Prepare workspace
rm(list = ls()[!(ls() %in% clean_workspace)])
packages <- c("foreign", "plm")
for(i in packages){
    if(!(i %in% rownames(installed.packages()))) {
        cat("Now installing required package:\\t", i)
        install.packages(i, dependencies = TRUE)
    }
    library(i, character.only = TRUE)
}

# Declare string constants
panel_id <- c("country", "year2")
control <- paste(
    c("enep", "disprop * pr", "plurality * closeness", "growth", "lnincome"),
    collapse = " + "
)
treatment <- c("pec1", "pec20", "vote_pec", "smallpec+largepec")
response <- "turnout"

# Load and prepare data
tillman <- read.dta(
    file.path(path_project, "prestudies", "ellger", "tilmann_merger.dta")
)
tillman <- aggregate(
    tillman, by = list(country = tillman$country), FUN = mean, na.rm = TRUE
)
between_models <- list()
for(i in 1:length(treatment)) {
    assign(
        "frm",
        paste0(response, " ~ ", paste(treatment[i], control, sep = " + "))
    )
    between_models[[i]] <- lm(as.formula(frm), data = tillman)
}
names(between_models) <- treatment
lapply(between_models, summary)
## END
# Coefficient on PECs flips sign in every model. However, the effect is regularly
# swamped by its standard error. There is little to no evidence in favor of a
# between-effect.