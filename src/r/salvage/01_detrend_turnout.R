packs <- c("car", "plm", "splines")
missing <- which(!(packs %in% rownames(installed.packages())))
if(any(missing)) {
    cat("Installing missing packages: ", packs[missing], "\n")
    install.packages(packs[missing], dependencies = TRUE)
}
lapply(packs, library, character.only = TRUE)

# Population level trend
# ggplot(data = tillman, aes(x = year, y = turnout_wi)) + geom_point() +
#     geom_smooth(method = "lm") +
#     geom_smooth(method = "lm", formula = y ~ poly(x, 2), col = "red") +
#     geom_smooth(method = "lm", formula = y ~ poly(x, 3), col = "orange")

# ggplot(data = tillman, aes(x = year, y = turnout_wi)) + geom_point() +
#     geom_smooth(method = "lm", aes(colour = "Linear")) +
#     geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), aes(colour = "B Spline, 3")) +
#     geom_smooth(method = "lm", formula = y ~ splines::bs(x, 6), aes(colour = "B Spline, 5"))
## Degrees higher than 3 do not noticeably change predictions.

# Plot panel unit specific trends
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
    scale_y_continuous(breaks = seq(-.1, .1, by = .05)) +
    scale_colour_brewer(type = "qual", palette = 2) +
    labs(
        colour = "Cubic base spline for", y = "Turnout\n(demeaned)",
        title = "Trends in Turnout"
    ) +
    theme_gray(base_size = base_size * .75) +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(), legend.position = "bottom"
    )
ggsave(file = file.path(path_project, "out", "scatter_smoother_detrendTurnout.png"))
print(p)
# In all but Denmark, Australia, Belgium, and Luxembourg turnout declines over time.
# Individual countries may exhibit sizeable non-linearities, e.g. UK and Israel.




# fit <- lm(turnout ~ year + factor(country), data = tillman)
# residualPlots(fit)
# # Tukey test: non-linearity in the residuals
# year_bs <- bs(tillman$year, degree = 3)
# names(year_bs) <- paste0("bs", names(bs))
# str(year_bs)
# fit <- lm(turnout ~ bs(year, degree = 3) * factor(country), data = tillman)
# pred_data <- expand.grid(country = unique(tillman$country), year = seq(1970, 2011))
# pred_data[, "yhat"] <- predict(fit, newdata = pred_data)
# plot(pred_data$year, pred_data$yhat, type = "n")
# for(i in pred_data$country){
#     lines(pred_data$year[pred_data[, "country"] == i], pred_data$yhat[pred_data[, "country"] == i], type = "l")
# }
# plot(pred_data$year, yhat)

# residualPlots(fit)
# plot(density(tillman$turnout_wi))
# curve(
#     dnorm(x, mean = mean(tillman$turnout_wi), sd = sd(tillman$turnout_wi)),
#     from = min(tillman$turnout_wi), to = max(tillman$turnout_wi),
#     add = TRUE, col = "red"
# )
# rug(tillman$turnout_wi)