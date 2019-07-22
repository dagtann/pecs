# This script replicates Model 1 through 4 from Table 4 in the original publication.
# Author: Dag Tanneberg
# Date: 03/21/2019
# ================================================================================
# Prepare workspace
rm(list = ls()[!(ls() %in% clean_workspace)])
packages <- c("foreign", "plm", "sandwich", "lmtest")
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

# Prepare data
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
lapply(fitted_models, function(m){
    lmtest::coeftest(m, vcov=function(x){vcovBK(x, cluster = "group")})
})

vcovBK(fitted_models[[1]], cluster = c("group"))
coeftest(fitted_models[[1]], vcov = vcovBK(fitted_models[[1]], cluster = c("group")))
coeftest(fitted_models[[3]], vcov = vcovBK(fitted_models[[3]], cluster = c("group")))
## END
## Coefficients replicate exactly.