# This script vizualizes common trends in turnout and the formation of
# pre-electoral coalitions. It returns several pdf files.
# Author: Dag Tanneberg
# Date: 04/11/2019
# ================================================================================
rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- c("car", "plm", "splines", "texreg", "knitr")
missing <- which(!(packs %in% rownames(installed.packages())))
if(any(missing)) {
    cat("Installing missing packages: ", packs[missing], "\n")
    install.packages(packs[missing], dependencies = TRUE)
}
lapply(packs, library, character.only = TRUE, quietly = TRUE)

# Constants
panel_id <- c("country", "year2")
control <- paste(
    c("enep", "disprop * pr", "plurality * closeness", "growth", "lnincome"),
    collapse = " + "
)
treatment <- c("pec1", "pec20", "vote_pec", "smallpec+largepec")
response <- "turnout"
yr_range <- as.numeric(min(tillman$year):max(tillman$year))
knots_num <- 3
knots_loc <- quantile(yr_range, probs = seq(0, 1, length.out = knots_num))
yr_spline_labs <- paste0(paste0("yr_spline", 1:(knots_num+1)))

# Data objects
yr_splines <- bs(yr_range, knots = knots_loc[-c(1, knots_num)], degree = 3)
yr_splines <- cbind.data.frame(yr_splines, yr_range)
colnames(yr_splines) <- c(yr_spline_labs, "year")
tillman <- left_join(tillman, yr_splines, by = "year")
# Throws a warning about different attributes, STATA import legacy
data_to_fit <- pdata.frame(tillman, index = c("country", "year2"))
fitted_models <- list()

# Plot panel unit specific trends in turnout
# (a) Save population level trend
population_fit <- lm(
    as.formula(
        paste0("turnout_wi", " ~ ", paste(yr_spline_labs, collapse = " + "))
    ),
    data = tillman
)
yhat <- predict(
    population_fit,
    newdata = yr_splines[, -ncol(yr_splines)],
    level = .95, interval = "confidence"
)
yhat <- cbind.data.frame(yhat, yr_range)
tmp <- tillman %>% mutate(country = "All") # Add multiple for entire sample
pdta <- rbind.data.frame(tillman, tmp)
# (b) Generate plot
p <- ggplot(data = pdta, aes(x = year, y = turnout_wi)) +
    geom_point() +
    geom_ribbon(
        data = yhat, aes(y = NULL, ymin = lwr, ymax = upr, x = yr_range), alpha = .4
    ) +
    geom_line(
        data = yhat, aes(y = fit, x = yr_range, colour = "All countries")
    ) +
    geom_smooth(
        data = subset(pdta, country != "All"),
        method = "lm", formula = y ~ bs(x, knots = knots_loc[-c(1, knots_num)], degree = 3), se = FALSE,
        aes(colour = "Single country")
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
    file = file.path(path_project, "out", "scatter_smoother_detrendTurnout.pdf"),
    plot = p
)

# Plot panel unit specific trends in pec1
# (a) Save population level trend
population_fit <- lm(
    as.formula(
        paste0("pec1_wi", " ~ ", paste(yr_spline_labs, collapse = " + "))
    ),
    data = tillman
)
yhat <- predict(
    population_fit,
    newdata = yr_splines[, -ncol(yr_splines)],
    level = .95, interval = "confidence"
)
yhat <- cbind.data.frame(yhat, year = yr_range)
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
        method = "lm", formula = y ~ bs(x, knots = knots_loc[-c(1, knots_num)], degree = 3), se = FALSE,
        aes(colour = "Single country")
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
    file = file.path(path_project, "out", "scatter_smoother_detrendPec1.pdf"),
    plot = p
)

# Plot parallel trend in turnout & pec1
## (a) z-standardize treatment & outcome
tmp <- group_by(tillman, country) %>%
    summarise_at(.vars = vars(turnout_wi, pec1_wi, vote_pec_wi),
        .funs = list(sigma = sd)
    )
tillman <- left_join(tillman, tmp, by = "country") %>%
    mutate(turnout_wi_z = turnout_wi / turnout_wi_sigma) %>%
    mutate(pec1_wi_z = pec1_wi / pec1_wi_sigma) %>%
    mutate(vote_pec_wi_z = vote_pec_wi / vote_pec_wi_sigma)
tmp <- tillman %>% mutate(country = "All")
pdta <- rbind.data.frame(tillman, tmp)
## Plot both trends
p <- ggplot(data = pdta, aes(x = year)) +
    geom_smooth(aes(y = turnout_wi_z, colour = "Turnout"), se = FALSE,
        method = "lm", formula = y ~ bs(x, 3)
    ) +
    geom_smooth(aes(y = pec1_wi_z, colour = "Pre-Electoral Coalition"),
        method = "lm", formula = y ~ bs(x, knots = knots_loc[-c(1, knots_num)], degree = 3),
        se = FALSE
    ) +
    geom_smooth(aes(y = vote_pec_wi_z, colour = "Pre-Electoral Coalition Result"),
        method = "lm", formula = y ~ bs(x, knots = knots_loc[-c(1, knots_num)], degree = 3),
        se = FALSE
    ) +
    facet_wrap(vars(country)) +
    scale_colour_brewer(type = "qual", palette = 3) +
    labs(y = "Z-Standardized Within-Differences", colour = "Cubic Base Spline for") +
    theme_gray(base_size = base_size * .75) +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(), legend.position = "bottom"
    )
ggsave(
    file = file.path(path_project, "out", "line_commonTrend_Pec1_Turnout.pdf"),
    plot = p
)

# fit detrended models
for (i in 1:length(treatment)) {
    assign(
        "frm",
        paste0(response, " ~ ", paste(treatment[i], control,
            paste(yr_spline_labs, collapse = " + "), sep = " + ")
        )
    )
    fitted_models[[i]] <- plm(
        as.formula(frm), model = "within", data = data_to_fit
    )
}
bk_se <- lapply(fitted_models, vcovBK, cluster = c("group"))
# Housekeeping ===================================================================
drop <- c(
    grep("sigma", names(tillman), fixed = TRUE),
    grep("wi_z", names(tillman), fixed = TRUE),
    grep("spline", names(tillman), fixed = TRUE)
)
tillman <- tillman[, -drop]
# rm(list = ls()[!(ls() %in% clean_workspace)])