library("plm")
library("lmtest")
library("stargazer")

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
data_to_fit <- within(data_to_fit, t <- year - 1990)

frm <- as.formula(
    paste(response, paste(treatment[1], control, sep = " + "), sep = "~")
)
fit_0 <- plm(frm, data = data_to_fit, model = "within")
fit_1 <- update(fit_0, . ~ . - year_f)
fit_2 <- update(fit_0, . ~ . + pec1:year_f)
waldtest(fit_0, fit_1) # Model equivalence highly rejected
waldtest(fit_0, fit_1,
    vcov = function(x) vcovBK(x, method = "HC3", cluster = "group"))
) # Model equivalence highly rejected
# Year dummies are required
coeftest(fit_0, vcov = function(x) vcovBK(x, method = "HC3", cluster = "group"))
# pec1 turns statistically indistinguishable from 0

waldtest(fit_0, fit_2) # Model equivalence not rejected
waldtest(fit_0, fit_2,
    vcov = function(x) vcovBK(x, type = "HC3", cluster = "group"))
) # Model equivalence not rejected.
# There are no decreasing returns from pec1

fit_3 <- update(fit_0, . ~ . - year_f + poly(t, 3))
summary(fit_3)
fit_4 <- update(fit_3, . ~ . + pec1 * poly(t, 3))
summary(fit_4)
waldtest(fit_3, fit_4) # Interaction rejected
waldtest(fit_3, fit_4,
    vcov = function(x) vcovBK(x, type = "HC3", cluster = "group")
)

frm <- as.formula(
    paste(response, paste(treatment[2], control, sep = " + "), sep = "~")
)
fit_vote_pec_0 <- plm(frm, data = data_to_fit, model = "within")
fit_vote_pec_1 <- update(fit_vote_pec_0, . ~ . - year_f)
fit_vote_pec_2 <- update(fit_vote_pec_0, . ~ . + vote_pec:year_f)
waldtest(fit_vote_pec_0, fit_vote_pec_1) # Model equivalence highly rejected
waldtest(fit_vote_pec_0, fit_vote_pec_1,
    vcov = function(x) vcovBK(x, type = "HC3", cluster = "group")
) # Model equivalence highly rejected, year dummies required
waldtest(fit_vote_pec_0, fit_vote_pec_2)
waldtest(fit_vote_pec_0, fit_vote_pec_2,
    vcov = function(x) vcovHC(x, method = "white2", type = "HC3"))
) # Model equivalence not rejected, no changing returns

fit_vote_pec_3 <- update(fit_vote_pec_0, . ~ . - year_f + poly(t, 3))
summary(fit_vote_pec_3)
fit_vote_pec_4 <- update(fit_vote_pec_3, . ~ . + vote_pec * poly(t, 3))
summary(fit_vote_pec_4)
waldtest(fit_3, fit_4) # Interaction rejected
waldtest(fit_3, fit_4,
    vcov = function(x) vcovBK(x, type = "HC3", cluster = "group")
)

model_list <- mget(c(paste("fit", c(1, 0, 2), sep = "_"), paste("fit_vote_pec", c(1, 0, 2), sep = "_")))
robust_se <- lapply(
    model_list,
    function(x) sqrt(diag(vcovBK(x, type = "HC3", cluster = "group")))
)
stargazer(model_list, se = robust_se, type = "html", out = file.path(path_project, "out", "regression_tables.html"))



