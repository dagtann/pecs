pec_features <- paste(
    "any", c(paste0("type", 1:6), "progr", "incumbent"), sep = "_"
)
apply(tillman[pec_features], 2, summary) # single NA on all obs
# All indicator variables, all sensible ranges.
# type3 or type6 never observed


tmp <- select(tillman, pec_features[c(-3, -6)]) %>%
    gather(key = "type", value = "value", any_type1, any_type2, any_type4, any_type5, any_progr, any_incumbent)
ggplot(tmp, aes(x = type, y = value)) + stat_sum()

head(tmp)
ggplot(data = tillman)
tillman


library(plm)

# Constants
panel_id <- c("iso3c", "year2")
control <- paste(
    c("enep", "disprop * pr", "plurality * closeness", "growth", "lnincome"),
    collapse = " + "
)
pec_feature <- paste("any", c(paste0("type", 1:6), "progr", "incumbent"), sep = "_")
treatment <- c("pec1", "pec20", "vote_pec", "smallpec+largepec")
response <- "turnout"
yr_range <- as.numeric( # type conversion b/c left_join() fails on integer seq
    seq(min(tillman$year), max(tillman$year))
)
max_degree <- 3
yr_poly_labs <- paste0(paste0("yr_poly", seq(max_degree)))

yr_poly <- poly(yr_range, degree = 3)
yr_poly <- cbind.data.frame(yr_poly, yr_range)
colnames(yr_poly) <- c(yr_poly_labs, "year")
data_to_fit <- left_join(tillman, yr_poly, by = "year")
# Throws a warning about different attributes, STATA import legacy

data_to_fit <- pdata.frame(data_to_fit, index = c("country", "year2"))
fitted_models <- list()
for (i in 1:length(treatment)) {
    assign(
        "frm",
        paste0(response, " ~ ",
            paste(
                treatment[i],
                #paste(pec_feature[grepl("type", pec_feature)], collapse = " + "),
                "any_progr",
                control,
                paste(yr_poly_labs, collapse = " + "), sep = " + "
            )
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
lapply(fitted_models, function (f) {lmtest::coeftest(f, vcov = vcovBK(f, cluster = "group"))})


assign(
        "frm",
        paste0(response, " ~ ",
            paste(
                "pectotal_inc", "pectotal_other",
                control,
                paste(yr_poly_labs, collapse = " + "), sep = " + "
            )
        )
    )
fit <- plm(as.formula(frm), model = "within", data = data_to_fit)
lmtest::coeftest(fit, vcov = vcovBK(fit, cluster = "group"))
