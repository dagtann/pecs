# This script replicates Tillman's analysis using simple trend models, including
# year fixed effects and polynomials up to degree 3.
# Author: Dag Tanneberg
# Last update: 04/16/2019
# Version info:
#     04/16/2019 Implements model fitting and tests in loops.
# =============================================================================
rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- c("plm", "lmtest")
lapply(packs, library, character.only = TRUE)

# Constants
panel_id <- c("iso3c", "year2")
control <- paste(
    c("enep", "disprop * pr", "plurality * closeness", "growth", "lnincome"),
    collapse = " + "
)
treatment <- c("pec1", "pec20", "vote_pec", "smallpec+largepec")
response <- "turnout"

# Data
t_poly <- poly(tillman$year - 1990, degree = 3)
colnames(t_poly) <- t_labels <- paste0("t", 1:3)
data_to_fit <- cbind.data.frame(tillman, t_poly)
data_to_fit <- pdata.frame(data_to_fit, index = panel_id)

# Fit year fixed effects for each treatment
fitted_dummies <- list()
for(i in 1:length(treatment)){
    fitted_dummies[[i]] <- plm(
        as.formula(
            paste0(response, " ~ ", paste(treatment[i], control, "factor(year)", sep = " + ") )
        ), data = data_to_fit, model = "within"
    )
}
lapply(fitted_dummies, function(x) coeftest(x, vcov = vcovBK(x, cluster = "group")))

# Fit year polynomials for each treatment, returns for each degree
fitted_polynomials <- list()

for (i in 1:length(treatment)) {
    fitted_polynomials[[treatment[i]]] <- list()
    for(t in 1:length(t_labels)){
        assign(
            "frm",
            paste0(response, " ~ ", paste(treatment[i], control,
                paste(t_labels[1:t], collapse = " + "), sep = " + ")
            )
        )
        fitted_polynomials[[i]][[t]] <- plm(
            as.formula(frm), model = "within", data = data_to_fit
        )
    }
}
lapply(treatment, function(t) {
    lapply(fitted_polynomials[[t]], function(x) {
        bk_se <- vcovBK(x, cluster = c("group"))
        coeftest(x, vcov = bk_se)
    })
})
# Treatment effects that turn on the vote percentage gained by pre-electoral
# coalitions are robust to detrending. The association between pre-electoral
# coalitions and turnout is not.

# housekeeping
detach(package:plm)
detach(package:lmtest)
rm(list = ls()[!(ls() %in% clean_workspace)])
## End