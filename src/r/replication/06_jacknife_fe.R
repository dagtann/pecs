library("lmtest")

execute_jackknife <- function(drop, from, formula, data){
    # drop = "France"
    # from = "country"
    # data = data_to_fit
    model_data <- data[data[[from]] != drop, ]
    fit <- plm(as.formula(formula), data = model_data, model = "within")
    cbind(beta = coef(fit), se = sqrt(diag(vcovBK(fit, "HC3", "group"))))
}

# Declare string constants
panel_id <- c("country", "year2")
control <- paste(
    c("enep", "disprop * pr", "plurality * closeness", "growth", "lnincome"),
    collapse = " + "
)
treatment <- c("pec1", "vote_pec")
response <- "turnout"
country_entries <- sort(unique(tillman$country))

# Prepare data
data_to_fit <- pdata.frame(tillman, index = c("country", "year2"))
fit <- plm(
    as.formula(paste0(response, " ~ ", paste(treatment[1], control, sep = " + "))),
    model = "within",
    data = data_to_fit
)

# execute jacknife
for(i in treatment){
    assign(paste("results", i, sep = "_"),  lapply(
        country_entries, function(c) {
            execute_jackknife(
                drop = c, from = "country",
                formula = paste0(response, " ~ ", paste(i, control, sep = " + ")),
                data = data_to_fit
            )
            }
        )
    )
}
jackknife_results <- list(pec1 = results_pec1, vote_pec = results_vote_pec)
rm(list = ls()[grep("results_", ls(), fixed = TRUE)])
pdta <- cbind(
    sapply(jackknife_results[[1]], function(x){x["pec1", ]}),
    sapply(jackknife_results[[2]], function(x){x["vote_pec", ]})
)
pdta <- t(pdta)
pdta <- cbind.data.frame(
    pdta, country = rep(country_entries, 2),
    treatment = rep(treatment, each = length(country_entries))
)
pdta <- mutate(pdta, lower = beta - qnorm(.975) * se) %>%
    mutate(upper = beta + qnorm(.975) * se)

coef_reference <- vector("numeric", length(treatment))
names(coef_reference) <- treatment
for(i in treatment){
    coef_reference[i] <- coef(
        plm(
            as.formula(paste0(response, " ~ ", paste(i, control, sep = " + "))),
            data = data_to_fit,
            model = "within"
        )
    )[i]
}
coef_reference <- data.frame(
    treatment = names(coef_reference), reference = coef_reference
)
pdta <- left_join(pdta, coef_reference, by = "treatment")
pdta <- mutate(pdta, treatment_f = factor(treatment, levels = c("pec1", "vote_pec"), labels = c("PEC in election?", "Percent of vote for PEC")))
p <- ggplot(data = pdta,
    aes(x = country, y = beta, ymin = lower, ymax = upper)
) +
    geom_hline(
        aes(yintercept = reference, color = "Published estimate")
    ) +
    geom_pointrange() +
    facet_wrap(~treatment_f, scales = "free_y", ncol = 1) +
    labs(y = "Estimated effect & CI95\nwhen country is dropped") +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()
    )
ggsave(file.path(path_project, "out", "pointrange_jackknife_treatments.png"))
# housekeeping
detach(package:plm)