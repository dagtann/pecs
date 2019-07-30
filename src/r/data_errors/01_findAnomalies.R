packages <- c("car", "plm")
for (package in packages) {
    if(!(package %in% row.names(installed.packages()))) {
        install.packages(package, "https://cloud.r-project.org")
    }
    library(package, character.only=TRUE)
}

variables <- paste0(
                    c("turnout", "enep", "disprop" , "pr", "plurality",
                      "closeness", "growth", "lnincome", "pec1", "pec20",
                      "vote_pec", "smallpec", "largepec"),
                    "_wi")
pdta <- subset(tillman, select = c("iso3c", variables))
pdta <- within(pdta, pec1_sign <- factor(sign(pec1_wi)))
# for (variable in c("pr", "pec1", "pec20", "smallpec", "largepec")) {
#     pdta[, variable] <- factor(pdta[["variable"]], levels = 0:1,
#                                labels = c("N", "Y"))
# }
scatterplotMatrix(pdta[, variables]) # bimodal distribution in lnincome
ggplot(data = pdta, aes(x = lnincome_wi)) +
    geom_histogram(binwidth = .5) +
    facet_wrap(~iso3c)
# Ireland & NZL have very large deviations from the mean

# Declare string constants
panel_id <- c("country", "year2")
control <- paste(
    c("enep", "disprop * pr", "plurality * closeness", "growth", "lnincome", "factor(iso3c)"),
    collapse = " + "
)
treatment <- c("pec1", "pec20", "vote_pec", "smallpec+largepec")
response <- "turnout"

# Prepare data
data_to_fit <- tillman

fitted_models <- list()
for(i in 1:length(treatment)) {
    assign(
        "frm",
        paste0(response, " ~ ", paste(treatment[i], control, sep = " + "))
    )
    fitted_models[[i]] <- lm(
        as.formula(frm), data = data_to_fit
    )
}
names(fitted_models) <- treatment
residualPlots(fitted_models[["pec1"]], terms = ~ lnincome)
residualPlots(fitted_models[["vote_pec"]], terms = ~ lnincome)
marginalModelPlots(fitted_models[["pec1"]])
compareCoefs(fitted_models[["pec1"]],
    update(fitted_models[["pec1"]], . ~ . - lnincome + e_migdppc_log),
    update(fitted_models[["pec1"]], . ~ . - lnincome + rgdpec_log),
    update(fitted_models[["pec1"]], . ~ . - lnincome + NY.GDP.PCAP.KD_log),
    update(fitted_models[["pec1"]], . ~ . - lnincome + NY.GDP.MKTP.PP.KD_log)
)
pdta <- gather(tillman, key = "indicator", value = "value",  lnincome,
               e_migdppc_log, rgdpec_log, NY.GDP.PCAP.KD_log)
ggplot(pdta, aes(x = value)) + geom_histogram(binwidth = .5) + facet_grid(iso3c ~ indicator)

ggplot(data = tillman, aes(x = pec1_wi, y = turnout_wi, shape = factor(iso3c == "IRL"))) + geom_point()

