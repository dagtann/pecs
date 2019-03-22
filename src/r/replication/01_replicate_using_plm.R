# This script replicates Model 1 through 4 from Table 4 in the original publication.
# Author: Dag Tanneberg
# Date: 03/21/2019
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
data_to_fit <- pdata.frame(tillman, index = c("country", "year2"))

fitted_models <- list()
for(i in 1:length(treatment)) {
    assign(
        "frm",
        paste0(response, " ~ ", paste(treatment[i], control, sep = " + "))
    )
    fitted_models[[i]] <- plm(
        as.formula(frm), model = "within", data = data_to_fit
    )
}
names(fitted_models) <- treatment
lapply(fitted_models, coef)
## END
## Coefficients replicate exactly.