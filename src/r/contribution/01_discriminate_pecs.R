rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- c("entropy")
missing <- which(!(packs %in% rownames(installed.packages())))
if(any(missing)) {
    cat("Installing missing packages: ", packs[missing], "\n")
    install.packages(packs[missing], dependencies = TRUE)
}
lapply(packs, library, character.only = TRUE)

# Does lsvergl identify the same PECs?
mytable <- with(tillman, table(tillman = pec1, lsvergl = pec_neu, exclude = NULL)
addmargins(mytable)
       # lsvergl
# tillman   0   1 <NA> Sum
    # 0   115  30    1 146
    # 1    29  89    0 118
    # Sum 144 119    1 264
1 - sum(diag(mytable)) / sum(mytable) # Discrepancy
# Sources disagree on 22 percent of all observations

# On what cases do both sources differ?
tmp <- filter(tillman, pec1 != pec_neu) %>%
    select(iso3c, year2, pec1, pec_neu)
length(unique(tmp$iso3c)) / length(unique(tillman$iso3c)) # 68% ctrys affected
length(unique(paste(tmp$iso3c, tmp$year2))) / # N_elections affected
    length(unique(paste(tillman$iso3c, tillman$year2))) # N all elections
# 22% elections affected

tmp <- group_by(tmp, iso3c) %>%
    summarise_if(is.numeric, sum) %>%
    gather("source", "value", pec1, pec_neu)
ggplot(data = tmp, aes(x = reorder(iso3c, value), y = value, fill = source)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(breaks = seq(0, 20, 2)) +
    labs(y = "Number of elections in which pre-electoral coalitions formed")



as.data.frame(tmp)


tmp <- group_by(tillman, iso3c) %>%
    select(paste("any", paste0("type", 1:6), sep = "_")) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)
by_ctry_entropy <- apply(tmp[, -1], 1, entropy, na.rm = TRUE)
names(by_ctry_entropy) <- tmp[["iso3c"]]

grep("any", names(tillman))
tmp <- gather(tillman, "any", "value", grep("any", names(tillman)))
tmp <- aggregate(value ~ any, data = tmp, FUN = sum, na.rm = TRUE)
sum(tmp$value)
with(tillman, length(unique(paste(iso3c, year2))))
sum(apply(tillman[, grep("any", names(tillman))], 1, FUN = sum, na.rm = TRUE))
tmp
table(tillman$pec1)
ggplot(data = tmp, aes(x = any, y = value)) + geom_bar(stat = "identity")

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
