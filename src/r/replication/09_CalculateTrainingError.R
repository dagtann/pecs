# This calulcate the training RMSE for pec1 and vote_pec treatments.
# Different RMSEs are calculated for (a) the entire sample, (b) each country.
# The script returns a plot.
# Author: Dag Tanneberg
# Date: 04/02/2019
# ================================================================================
# Prepare workspace
rm(list = ls()[!(ls() %in% clean_workspace)])

# Declare constants
panel_id <- c("country", "year2")
control <- paste(
    c("enep", "disprop * pr", "plurality * closeness", "growth", "lnincome", "country"),
    collapse = " + "
)
treatment <- c("pec1", "vote_pec")
response <- "turnout"

# Prepare data
data_to_fit <- select(tillman,
    enep, disprop, pr, plurality, closeness, growth, lnincome, country, turnout,
    pec1, vote_pec
) %>%
    group_by(country) %>%
    mutate(bw_turnout = mean(turnout), wi_turnout = turnout - bw_turnout) %>%
    ungroup()
data_to_fit <- as.data.frame(data_to_fit)
# Fit Dummy Variable FE (b/c predict on class("plm") returns an error)
fitted_models <- list()
for(i in 1:length(treatment)) {
    assign(
        "frm", paste0(response, " ~ ", paste(treatment[i], control, sep = " + "))
    )
    fitted_models[[i]] <- lm(
        as.formula(frm), data = data_to_fit
    )
}
names(fitted_models) <- treatment

# Calulate sample based RMSE
rmse <- vapply(
    fitted_models, function(m){sqrt(mean(resid(m) ^ 2))},
    FUN.VALUE = numeric(1)
)
rmse; sd(data_to_fit$wi_turnout)
(sd(data_to_fit$wi_turnout) / rmse) - 1

# Calculate country based RMSEs
countries <- unique(data_to_fit$country)
country_rmse <- matrix(FALSE, ncol = 2, nrow = length(countries),
    dimnames = list(countries, treatment)
)
for (t in treatment) {
    data_to_fit[, paste("hat", t, sep = "_")] <-
        predict(fitted_models[[t]], newdata = data_to_fit) - data_to_fit$bw_turnout
    data_to_fit[, paste("e", t, sep = "_")] <-
        data_to_fit[, paste("hat", t, sep = "_")] - data_to_fit$wi_turnout
    for (i in countries){
        country_rmse[i, t] <- sqrt(
            mean(data_to_fit[data_to_fit$country == i, paste("e", t, sep = "_")] ^ 2, na.rm = TRUE)
        )
    }
}

# plot data
pdta <- data.frame(country_rmse, country = countries)
pdta <- gather(pdta, key = "treatment", value = "rmse", pec1, vote_pec)
pdta[, "sample_rmse"] <- rep(rmse, each = length(countries))
pdta <- mutate(pdta, treatment_f = factor(treatment, levels = c("pec1", "vote_pec"), labels = c("PEC contested?", "PEC vote percent")))
p <- ggplot(
    data = pdta, aes(x = reorder(country, rmse), y = rmse, colour = treatment_f)
) +
    geom_point(size = 3) +
    geom_hline(aes(yintercept = sample_rmse, colour = treatment_f)) +
    labs(y = "Root Mean Square Error", colour = "Treatment", subtitle = "Straight lines show the RMSE for the entire data.") +
    theme_grey(base_size = base_size) +
    theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom"
    )
print(p)
ggsave(
    file = file.path(path_project, "out", "scatter_trainingRmse.png"),
    plot = p, width = 12, height = 8
)
## END
