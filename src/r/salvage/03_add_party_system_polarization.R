 summary(country_panel$polarization)
 plot(density(country_panel$polarization, na.rm = TRUE))

tillman <- mutate(tillman, polarization = polarization * 100)

ggplot(data = tillman,
   aes(x = year, y = polarization)) + geom_point() + geom_smooth() + facet_wrap(vars(iso3c))

polarization_bw <- tillman %>%
    select(iso3c, polarization) %>%
    group_by(iso3c) %>%
    summarize_all(list(bw = mean), na.rm = TRUE) %>%
    ungroup() %>%
    rename(polarization_bw = bw)
tillman <- left_join(tillman, polarization_bw, by = "iso3c")
tillman <- mutate(tillman, polarization_wi = polarization - polarization_bw)

ggplot(data = tillman,
    aes(x = year, y = polarization_wi)) + geom_point() + geom_smooth() + facet_wrap(vars(iso3c))

ggplot(data = tillman, aes(x = turnout_wi, y = polarization_wi)) +
    geom_point() + geom_smooth() + facet_wrap(vars(iso3c))

packs <- c("plm", "splines")
missing <- which(!(packs %in% rownames(installed.packages())))
if (any(missing)) {
    cat("Installing missing packages: ", packs[missing], "\n")
    install.packages(packs[missing], dependencies = TRUE)
}
lapply(packs, library, character.only = TRUE, quietly = TRUE)

# Constants
panel_id <- c("country", "year2")
control <- paste(
    c("polarization", "enep", "disprop", "closeness", "growth", "lnincome"), collapse = " + "
)
treatment <- c("pec1", "pec20", "vote_pec", "smallpec+largepec")
response <- "turnout"
yr_range <- as.numeric(min(tillman$year):max(tillman$year))
knots_num <- 3
knots_loc <- quantile(yr_range, probs = seq(0, 1, length.out = knots_num))
yr_spline_labs <- paste0(paste0("yr_spline", 1:(knots_num + 1)))

# Data objects
yr_splines <- bs(yr_range, knots = knots_loc[-c(1, knots_num)], degree = 3)
yr_splines <- cbind.data.frame(yr_splines, yr_range)
colnames(yr_splines) <- c(yr_spline_labs, "year")
data_to_fit <- left_join(tillman, yr_splines, by = "year")
# Throws a warning about different attributes, STATA import legacy
data_to_fit <- pdata.frame(data_to_fit, index = c("country", "year2"))
fitted_models <- list()

# refit models
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
bk_pval <- lapply(fitted_models,
    function(x){
        lmtest::coeftest(x, vcov = vcovBK(x, cluster = "group"))[, 4]
    }
)
lmtest::coeftest(
    fitted_models[[3]], vcov = function(m){vcovBK(m, cluster = "group")}
)
with(tillman, cor(polarization, closeness, use = "complete.obs"))
ggplot(data = tillman, aes(x = polarization_wi, y = closeness_wi)) + geom_point() + geom_smooth() + facet_wrap(~iso3c)
car::residualPlots(fitted_models[[3]])
})