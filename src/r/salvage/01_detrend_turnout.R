# This script vizualizes common trends in turnout and the formation of
# pre-electoral coalitions. It returns several png files.
# Author: Dag Tanneberg
# Date: 04/11/2019
# ================================================================================
rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- c("car", "plm", "splines")
missing <- which(!(packs %in% rownames(installed.packages())))
if(any(missing)) {
    cat("Installing missing packages: ", packs[missing], "\n")
    install.packages(packs[missing], dependencies = TRUE)
}
lapply(packs, library, character.only = TRUE, quietly = TRUE)

# Plot panel unit specific trends in turnout
# (a) Save population level trend
population_fit <- lm(turnout_wi ~ bs(year, 3), data = tillman)
yhat <- predict(
    population_fit,
    newdata = data.frame(year = seq(min(tillman$year), max(tillman$year))),
    level = .95, interval = "confidence"
)
yhat <- cbind.data.frame(yhat, year = seq(min(tillman$year), max(tillman$year)))
tmp <- tillman %>% mutate(country = "All") # Add multiple for entire sample
pdta <- rbind.data.frame(tillman, tmp)
# (b) Generate plot
p <- ggplot(data = pdta, aes(x = year, y = turnout_wi)) +
    geom_point() +
    geom_ribbon(
        data = yhat, aes(y = NULL, ymin = lwr, ymax = upr, x = year), alpha = .4
    ) +
    geom_line(
        data = yhat, aes(y = fit, x = year, colour = "All countries")
    ) +
    geom_smooth(
        data = subset(pdta, country != "All"),
        method = "lm", formula = y ~ bs(x, degree = 3), se = FALSE,
        aes(col = "Single country")
    ) +
    facet_wrap(vars(country)) +
    scale_colour_brewer(type = "qual", palette = 3) +
    labs(
        colour = "Cubic base spline for", y = "Turnout\n(demeaned)"
    ) +
    theme_gray(base_size = base_size * .75) +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(), legend.position = "bottom"
    )
ggsave(
    file = file.path(path_project, "out", "scatter_smoother_detrendTurnout.png"),
    plot = p
)

# Plot panel unit specific trends in pec1
# (a) Save population level trend
population_fit <- lm(pec1_wi ~ bs(year, 3), data = tillman)
yhat <- predict(
    population_fit,
    newdata = data.frame(year = seq(min(tillman$year), max(tillman$year))),
    level = .95, interval = "confidence"
)
yhat <- cbind.data.frame(yhat, year = seq(min(tillman$year), max(tillman$year)))
tmp <- tillman %>% mutate(country = "All") # Add multiple for entire sample
pdta <- rbind.data.frame(tillman, tmp)
# (b) Generate plot
p <- ggplot(data = pdta, aes(x = year, y = pec1_wi)) +
    geom_point() +
    geom_ribbon(
        data = yhat, aes(y = NULL, ymin = lwr, ymax = upr, x = year), alpha = .4
    ) +
    geom_line(
        data = yhat, aes(y = fit, x = year, colour = "All countries")
    ) +
    geom_smooth(
        data = subset(pdta, country != "All"),
        method = "lm", formula = y ~ bs(x, degree = 3), se = FALSE,
        aes(col = "Single country")
    ) +
    facet_wrap(vars(country)) +
    scale_colour_brewer(type = "qual", palette = 3) +
    labs(
        colour = "Cubic base spline for",
        y = "Pre-electoral coaliation formed?\n(demeaned)"
    ) +
    theme_gray(base_size = base_size * .75) +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(), legend.position = "bottom"
    )
ggsave(
    file = file.path(path_project, "out", "scatter_smoother_detrendPec1.png"),
    plot = p
)

# Plot parallel trend in turnout & pec1
## (a) z-standardize treatment & outcome
tmp <- group_by(tillman, country) %>%
    summarise_at(.vars = vars(turnout_wi, pec1_wi), .funs = list(sigma = sd))
tillman <- left_join(tillman, tmp, by = "country") %>%
    mutate(turnout_wi_z = turnout_wi / turnout_wi_sigma) %>%
    mutate(pec1_wi_z = pec1_wi / pec1_wi_sigma)
tmp <- tillman %>% mutate(country = "All")
pdta <- rbind.data.frame(tillman, tmp)
## Plot both trends
p <- ggplot(data = pdta, aes(x = year)) +
    geom_smooth(aes(y = turnout_wi_z, colour = "Turnout"), se = FALSE,
        method = "lm", formula = y ~ bs(x, 3)
    ) +
    geom_smooth(aes(y = pec1_wi_z, colour = "Pre-electoral coalition"), se = FALSE,
        span = .9, method = "lm", formula = y ~ bs(x, 3)
    ) +
    facet_wrap(vars(country)) +
    scale_colour_brewer(type = "qual", palette = 3) +
    labs(y = "Z-Standardized Within-Differences", colour = "Trend in") +
    theme_gray(base_size = base_size * .75) +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(), legend.position = "bottom"
    )
ggsave(
    file = file.path(path_project, "out", "line_commonTrend_Pec1_Turnout.png"),
    plot = p
)
# Housekeeping ===================================================================
drop <- c(
    grep("sigma", names(tillman), fixed = TRUE),
    grep("wi_z", names(tillman), fixed = TRUE)
)
tillman <- tillman[, -drop]
rm(list = ls()[!(ls() %in% clean_workspace)])