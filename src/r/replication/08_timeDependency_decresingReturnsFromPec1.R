# Include obs period dummies and check decreasing returns
panel_id <- c("country", "year2")
control <- paste(
    c("enep", "disprop * pr", "plurality * closeness", "growth", "lnincome", "year_f"),
    collapse = " + "
)
treatment <- c("pec1", "vote_pec")
response <- "turnout"

# Prepare data
data_to_fit <- pdata.frame(tillman, index = c("country", "year2"))
data_to_fit <- within(data_to_fit, year_f <- factor(year))

frm <- as.formula(
    paste(response, paste(treatment[1], control, sep = " + "), sep = "~")
)
fit_0 <- plm(frm, data = data_to_fit, model = "within")
fit_1 <- update(fit_1, . ~ . - year_f)
fit_2 <- update(fit_1, . ~ . + pec1:year_f)
waldtest(fit_0, fit_1) # Model equivalence highly rejected
waldtest(fit_0, fit_1,
    vcov = function(x) vcovHC(x, method = "white2", type = "HC3"))
) # Model equivalence highly rejected, year dummies required

waldtest(fit_1, fit_2) # Model equivalence highly rejected
waldtest(fit_1, fit_2,
    vcov = function(x) vcovHC(x, method = "white2", type = "HC3"))
) # Model equivalence highly rejected, returns from pec1 change

fit_3 <- update(fit_0, . ~ . - year_f + year)
fit_4 <- update(fit_3, . ~ . + pec1:year)
summary(fit_4)