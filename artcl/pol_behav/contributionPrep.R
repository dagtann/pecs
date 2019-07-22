rm(list = ls()[!(ls() %in% clean_workspace)])
packages <- c("plm", "sandwich", "lmtest", "texreg")
for(i in packages){
    if(!(i %in% rownames(installed.packages()))) {
        cat("Now installing required package:\t", i)
        install.packages(i, dependencies = TRUE)
    }
    library(i, character.only = TRUE)
}

data_to_fit <- filter(country_panel, (in_tillman == 1)) %>%
    mutate(pectotal_neu = pectotal_neu * 100)
data_to_fit <- pdata.frame(data_to_fit, index = c("iso3c", "year2"))
fit_type <- plm(
    turnout ~
        any_type1 + any_type2 + any_type3 + any_type4 + any_type5 + any_type6 +
        enep + disprop * pr + plurality * closeness + growth + lnincome,
    data = data_to_fit, effect = "twoways", model = "within")
summary(fit_type)
fit_prog <- plm(
    turnout ~
        any_prog +
        enep + disprop * pr + plurality * closeness + growth + lnincome,
    data = data_to_fit, effect = "twoways", model = "within")
summary(fit_prog)
fit_incumbent <- plm(
    turnout ~
        any_incumbent +
        enep + disprop * pr + plurality * closeness + growth + lnincome,
    data = data_to_fit, effect = "twoways", model = "within")
summary(fit_incumbent)
fit_baseline <- plm(
    turnout ~
        pectotal_neu +
        enep + disprop * pr + plurality * closeness + growth + lnincome,
    data = data_to_fit, effect = "twoways", model = "within")
fit_interInc <- plm(
    turnout ~
        any_incumbent + any_type1 + any_type2 + any_type3 + any_type4 +
        any_type5 + any_type6 +
        any_incumbent:any_type1 +
        any_incumbent:any_type2 +
        any_incumbent:any_type3 +
        any_incumbent:any_type4 +
        any_incumbent:any_type5 +
        any_incumbent:any_type6 +
        enep + disprop * pr + plurality * closeness + growth + lnincome,
    data = data_to_fit, effect = "twoways", model = "within"
)
fit_interProg <- plm(
    turnout ~
        any_prog + any_type1 + any_type2 + any_type3 + any_type4 +
        any_type5 + any_type6 +
        any_prog:any_type1 +
        any_prog:any_type2 +
        any_prog:any_type3 +
        any_prog:any_type4 +
        any_prog:any_type5 +
        any_prog:any_type6 +
        enep + disprop * pr + plurality * closeness + growth + lnincome,
    data = data_to_fit, effect = "twoways", model = "within"
)
screenreg(list(fit_baseline, fit_type, fit_prog, fit_incumbent, fit_interInc, fit_interProg))