# This script vizualizes common trends in turnout and the formation of
# pre-electoral coalitions. It returns several pdf files.
# Author: Dag Tanneberg
# Date: 04/11/2019
# ================================================================================
rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- c("car", "plm", "splines", "texreg", "knitr")
missing <- which(!(packs %in% rownames(installed.packages())))
if (any(missing)) {
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
treatment <- c("pec_neu", "pec20_neu", "pectotal_neu")
response <- "turnout"
yr_range <- as.numeric(min(tillman$year):max(tillman$year))
knots_num <- 3
knots_loc <- quantile(yr_range, probs = seq(0, 1, length.out = knots_num))
yr_spline_labs <- paste0(paste0("yr_spline", 1:(knots_num + 1)))


# Data objects
yr_splines <- bs(yr_range, knots = knots_loc[-c(1, knots_num)], degree = 3)
yr_splines <- cbind.data.frame(yr_splines, yr_range)
colnames(yr_splines) <- c(yr_spline_labs, "year")
tillman <- left_join(tillman, yr_splines, by = "year")
# Throws a warning about different attributes, STATA import legacy
data_to_fit <- pdata.frame(tillman, index = c("country", "year2"))
fitted_models <- list()



# fit detrended models
for (i in 1:length(treatment)) {
    assign(
        "frm",
        paste0(response, " ~ ", paste(treatment[i], control, sep = " + ")
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
lapply(
    fitted_models,
    FUN = function(m) {
        lmtest::coeftest(
            m, vcov = vcovBK(m, cluster = "group")
        )
    }
)


# Housekeeping ===================================================================
drop <- c(
    grep("sigma", names(tillman), fixed = TRUE),
    grep("wi_z", names(tillman), fixed = TRUE),
    grep("spline", names(tillman), fixed = TRUE)
)
tillman <- tillman[, -drop]
# lapply(packs,
#     function(p){detach(name = paste0("package:", p), character.only = TRUE)}
# )
# rm(list = ls()[!(ls() %in% clean_workspace)])
## END