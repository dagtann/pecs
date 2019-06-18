rm(list = ls()[!(ls() %in% clean_workspace)])
packages <- c("plm", "sandwich", "lmtest", "texreg")
for(i in packages){
    if(!(i %in% rownames(installed.packages()))) {
        cat("Now installing required package:\t", i)
        install.packages(i, dependencies = TRUE)
    }
    library(i, character.only = TRUE)
}

# Declare Functions
extract_results <- function (model, of.interest = NULL, pcse = NULL) {
    out <- cbind(beta = coef(model), se = sqrt(diag(vcov(model))))
    if(!is.null(pcse)) {
        out[, "se"] <- sqrt(diag(plm::vcovBK(model, cluster = pcse)))
    }
    if (!is.null(of.interest)) {
        out <- out[grep(of.interest, rownames(out), perl=TRUE), ]
    }
    return(out)
}


# Declare string constants
panel_id <- c("country", "year2")
control <- paste(
    c("enep", "disprop * pr", "plurality * closeness", "growth", "lnincome"),
    collapse = " + "
)
treatments <- c("PEC contested" = "pec_neu", "Vote PEC" = "pectotal_neu")
response <- "turnout"
trends <- c("None"="None", "Two-way Error\nComponent"="twoways",
    "First\nDifference"="fd", "Common\nTrend"="t",
    "Unit-specific\nTrend"="t*iso3c", "Lagged DV"="turnout_l1"
)


country_panel[, "any_type346"] <- apply(
    country_panel[, c("any_type3", "any_type4", "any_type6")], 1,
    function(r) {
        as.numeric(ifelse(all(is.na(r)), NA, any(r == 1, na.rm = TRUE)))
    }
)
# Prepare data
data_to_fit <- filter(country_panel, (in_tillman == 1) &
    !(iso3c %in% c("LUX", "CAN"))
) %>%
    group_by(iso3c) %>%
    mutate(turnout_l1 = dplyr::lag(turnout, n = 1, order_by = year2)) %>%
    ungroup() %>%
    mutate(pectotal_neu = pectotal_neu * 100)
data_to_fit <- pdata.frame(data_to_fit, index = c("iso3c", "year2"))

# Fit models
fitted_models <- list()
for (trend in trends){
    for(treatment in treatments) {
        assign(
            "frm",
            paste0(response, " ~ ", paste(treatment, control, trend, sep=" + "))
        )
        if (trend == "None") {
            assign(
                "frm",
                paste0(response, " ~ ", paste(treatment, control, sep=" + "))
            )
        }
        if (trend == c("twoways")) {
            assign(
                "frm",
                paste0(response, " ~ ", paste(treatment, control, sep=" + "))
            )
            fitted_models[[paste(trend, treatment, sep = "_")]] <- plm(
                as.formula(frm), effect="twoways", model="within", data=data_to_fit
            )
            next
        }
        if (trend == c("fd")) {
            assign(
                "frm",
                paste0(response, " ~ ", paste(treatment, control, sep = " + "))
            )
            fitted_models[[paste(trend, treatment, sep = "_")]] <- plm(
                as.formula(frm), effect="individual", model="fd", data=data_to_fit
            )
            next
        }
        fitted_models[[paste(trend, treatment, sep = "_")]] <- plm(
            as.formula(frm), model = "within", data = data_to_fit
        )
    }
}
# Generate coefficient plot
to_plot <- vapply(fitted_models[c(1:4, 7:12)],
    FUN=extract_results, FUN.VALUE=numeric(2), pcse="group", of.interest="pec"
)
to_plot <- cbind(
    to_plot,
    vapply(fitted_models[c("fd_pec_neu", "fd_pectotal_neu")],
        FUN = function(model){
            out <- cbind(beta = coef(model), se = sqrt(diag(vcov(model))))
            out <- out[grep("pec", rownames(out), perl=TRUE), ]
            return(out)
        },
        FUN.VALUE = numeric(2)
    )
)
to_plot <- t(to_plot)
treatment_locations <- str_locate(rownames(to_plot), "pectotal_neu$|pec_neu$")
treatment_labels <- str_sub(rownames(to_plot), treatment_locations)
treatment_labels <- factor(treatment_labels, treatments, names(treatments))
trend_labels <- str_split_fixed(rownames(to_plot), "pectotal_neu|pec_neu", 2)[, 1]
trend_labels <- str_sub(trend_labels, 1, str_locate(trend_labels, "_$")[, 1]-1)
trend_labels <- factor(trend_labels, trends, names(trends))
z <- qnorm(c(.1, .05) / 2, lower = FALSE)
pdta <- tibble(beta=to_plot[, "beta"], se=to_plot[, "se"],
    trend_label=trend_labels, treatment_label=treatment_labels
) %>%
    mutate(lower90 = beta - z[1] * se, upper90 = beta + z[1] * se) %>%
    mutate(lower95 = beta - z[2] * se, upper95 = beta + z[2] * se)
ggplot(data = pdta, aes(x = trend_label, y = beta)) +
    geom_linerange(aes(ymin = lower95, ymax = upper95)) +
    geom_linerange(aes(ymin = lower90, ymax = upper90), size=1.1) +
    geom_point(size = 2, shape = 21, fill = "white") +
    labs(x = "Detrending Strategy", y = expression(hat(beta)),
        caption = "Note: CIs at 90 and 95 per cent."
    ) +
    facet_grid(treatment_label ~ ., scales = "free") +
    ggthemes::theme_fivethirtyeight() +
    theme(axis.title = element_text(), axis.title.x = element_blank())


apply(country_panel[, c("smallpec_neu", "largepec_neu", "any_type1",
    "any_type2", "any_type3", "any_type4", "any_type5", "any_type6",
    "any_prog", "any_incumbent")], 2, table)

country_panel[, "any_type346"] <- apply(
    country_panel[, c("any_type3", "any_type4", "any_type6")], 1,
    function(r) {
        as.numeric(ifelse(all(is.na(r)), NA, any(r == 1, na.rm = TRUE)))
    }
)
summary(country_panel$any_type346)

fit_type <- plm(
    turnout ~
        pectotal_neu + any_type1 + any_type2 + any_type5 +
        enep + disprop * pr + plurality * closeness + growth + lnincome,
    data = data_to_fit, effect = "twoways", model = "within")
summary(fit_type)
fit_prog <- plm(
    turnout ~
        pectotal_neu + any_prog +
        enep + disprop * pr + plurality * closeness + growth + lnincome,
    data = data_to_fit, effect = "twoways", model = "within")
summary(fit_prog)
fit_incumbent <- plm(
    turnout ~
        pectotal_neu + any_incumbent +
        enep + disprop * pr + plurality * closeness + growth + lnincome,
    data = data_to_fit, effect = "twoways", model = "within")
summary(fit_incumbent)
fit_baseline <- plm(
    turnout ~
        pectotal_neu +
        enep + disprop * pr + plurality * closeness + growth + lnincome,
    data = data_to_fit, effect = "twoways", model = "within")

screenreg(list(fit_baseline, fit_type, fit_prog, fit_incumbent), fstatistic = TRUE)