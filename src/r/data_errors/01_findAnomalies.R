# This script identifies anomalous data points in Tillman's analysis data. It
# returns several diagnostics plots.
# Author: Dag Tanneberg
# Version info:
#   07/31/2019 Feature complete v1.0
# Preamble ---------------------------------------------------------------------
rm(list = ls()[!(ls() %in% clean_workspace)])
packages <- c("car", "plm")
for (package in packages) {
    if(!(require(package, character.only = TRUE))) {
        install.packages(package, "https://cloud.r-project.org")
        library(package, character.only=TRUE)
    }
}

# String constants
control <- paste(
    c("enep", "disprop * pr", "plurality * closeness", "growth", "lnincome",
      "factor(iso3c)"),
    collapse = " + "
)
treatments <- c("pec1", "pec20", "vote_pec", "smallpec+largepec")
response <- "turnout"
gdp_indicators <- c("lnincome", "e_migdppc_log", "rgdpec_log",
                    "NY.GDP.PCAP.KD_log")

# Replicate original analysis
data_to_fit <- subset(tillman, !(iso3c %in% c("LUX", "CAN")))
fitted_models <- list()
for(i in seq(treatments)) {
    assign("frm",
            paste0(response, " ~ ", paste(treatments[i], control, sep = " + ")))
    fitted_models[[i]] <- lm(as.formula(frm), data = data_to_fit)
}
names(fitted_models) <- treatments
# basic diagnostic plot (residualPlots()) show a bimodal distribution in lnincome
# Output diagnostic plot for all treatments
pdta <- lapply(
    fitted_models, function(m) {
        cbind.data.frame(rstd = rstandard(m),
                         lnincome = model.matrix(m)[, "lnincome"])
    }
)
pdta <- do.call(rbind.data.frame, pdta)
pdta[, "treatment"] <- sapply(str_split(rownames(pdta), "\\."), "[", 1)
pdta <- within(pdta,
    treatment_label <- factor(treatment, levels = treatments,
                              labels = c("PEC", "PEC > 20", "Vote PEC",
                                         "Small or large PEC")))
p <- ggplot(data = pdta, aes(x = lnincome, y = rstd)) + geom_point() + 
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
    labs(y = "Pearson Residual", x = "log(Income)",
         title = "Residual vs. Predictor Plots") +
    facet_wrap(~treatment_label)
ggsave(file.path(path_project, "out", "data_error_residualVSincome.png"))

# Check by-country distribution of income
pdta <- gather(data_to_fit, key = "indicator", value = "value",  lnincome,
               e_migdppc_log, rgdpec_log, NY.GDP.PCAP.KD_log)
pdta <- within(pdta, indicator_label <- factor(indicator,
                                               levels = c("lnincome",
                                                          "e_migdppc_log",
                                                          "rgdpec_log",
                                                          "NY.GDP.PCAP.KD_log"),
                                               labels = c("Tillman",
                                                          "Maddison Project",
                                                          "Penn World Tables v9",
                                                          "WDI")))
p <- ggplot(pdta, aes(x = value, fill = indicator_label)) +
    geom_density(alpha = .4) + 
    labs(y = "Density", x = "log(GDP per capita)", fill = "Source",
         title = "Anomalies in GDP") +
    facet_wrap(~iso3c)
ggsave(file.path(path_project, "out", "density_anomaliesInGdp.png"), p)

# Repeat analysis using different gdp measures
data_to_fit <- pdata.frame(data_to_fit, index = c("iso3c", "year2"))
fitted_models <- vector("list", length(treatments))
pdta <- data.frame(treatment = "", gdp = "", beta = 0, se = 0)[0, ]
for (i in seq(treatments)) {
    for (j in seq(gdp_indicators)) {
        assign("frm",
                paste0(response, " ~ ", paste(treatments[i],
                                              control[!(control == "lnincome")],
                                              gdp_indicators[j], sep = " + ")))
        fitted_models[[i]][[j]] <- plm(as.formula(frm), data = data_to_fit,
                                       model = "within", effect = "individual")
        beta <- coef(fitted_models[[i]][[j]])[str_split(treatments, "\\+")[[i]]]
        se <- sqrt(diag(vcovBK(fitted_models[[i]][[j]],
                               cluster = "group"))[str_split(treatments,
                                                             "\\+")[[i]]])
        pdta <- rbind.data.frame(pdta, 
                                 data.frame(treatment = str_split(treatments,
                                                                  "\\+")[[i]],
                                            gdp = gdp_indicators[j], beta = beta,
                                            se = se, stringsAsFactors =FALSE),
                                 stringsAsFactors = FALSE)
    }
}
pdta <- within(pdta, {
    treatment_label <- factor(treatment,
                              levels = c("pec1", "pec20", "vote_pec",
                                         "smallpec", "largepec"),
                              labels = c("PEC", "PEC > 20", "Vote PEC",
                                         "Small PEC", "Large PEC"))
    gdp_label <- factor(gdp,
                        levels = c("lnincome", "e_migdppc_log", "rgdpec_log",
                                   "NY.GDP.PCAP.KD_log"),
                        labels = c("Tillman", "Maddison Project",
                                   "Penn World Tables v9", "WDI"))
    lower <- beta - qnorm(.975) * se
    upper <- beta + qnorm(.975) * se})
p <- ggplot(data = pdta, aes(x = gdp_label, y = beta, ymin = lower, 
                             ymax = upper)) +
    geom_pointrange() +
    labs(x = "Source of GDP per capita",
         y = "Coefficient estimate\nCIs at 95 percent, Panel robust SE",
         title = "Sensitivity of results to GDP per capita") +
    coord_flip() +
    facet_wrap(~treatment_label, scales = "free_x")
ggsave(file.path(path_project, "out", "coefplot_SensitivityToGDP.png"))

# Simple eda plot
pdta <- filter(tillman, !(iso3c %in% c("LUX", "CAN")))
p <- ggplot(pdta, aes(x = lnincome, y = turnout,
                      colour = factor(pec1, 0:1, c("No", "Yes")))) +
    geom_point() +
    labs(x = "log GDP per capita", y = "Turnout", colour = "PEC",
         title = "Joint distribution of turnout, GDP and pre-electoral coalitions") +
    facet_wrap( ~ iso3c) +
    theme_grey(base_size = 18)
ggsave(file.path(path_project, "out", "point_TurnoutGdpPecXcountry.png"), p,
       width = 22, height = 22 / 1.618)

# housekeeping -----------------------------------------------------------------
for (package in packackages) {
    detach(paste0("package:", packackage), character.only = TRUE)
}
rm(list = ls()[!(ls() in clean_workspace)])
## END