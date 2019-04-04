library("car")
institutions <- c("pr", "plurality", "disprop", "enep")
apply(tillman[, paste(institutions, "wi", sep = "_")], 2, fivenum)
apply(tillman[, paste(institutions, "wi", sep = "_")], 2, sd)

scatterplotMatrix(tillman[, paste(institutions, "wi", sep = "_")], smooth = FALSE)
cor(tillman[, paste(institutions, "wi", sep = "_")])

fit_pec1 <- lm(turnout_wi ~ pec1_wi + enep_wi + disprop_wi + pr_wi + 
    plurality_wi + closeness_wi + growth_wi + lnincome_wi,
    data = tillman
)
summary(fit_pec1)
vif(fit_pec1)
# Plurality and pr confidence intervals affected most
residualPlots(fit_pec1)
marginalModelPlots(fit_pec1)
ggplot(data = tillman, aes(x = disprop_wi, y = turnout_wi)) +
    geom_point() + geom_smooth()

residualPlots(
    update(fit_pec1, . ~ . - pr_wi - disprop_wi + poly(disprop_wi, 3))
)


    (tillman$disprop_wi, tillman$turnout_wi)
aggregate(disprop_wi ~ )

ggplot(data = tillman, aes(x = disprop_wi))
apply(till)
cor(as.matrix(tillman[]))


# Declare string constants
panel_id <- c("country", "year2")
control <- paste(
    c("enep", "disprop * pr", "plurality * closeness", "growth", "lnincome"),
    collapse = " + "
)
treatment <- c("pec1", "pec20", "vote_pec", "smallpec+largepec")
response <- "turnout"


fit_pec1_constrained <- update(fit_pec1, . ~ . - disprop_wi:pr_wi - pr_wi)
anova(fit_pec1, fit_pec1_constrained)
fit_pec1_constrained2 <- update(fit_pec1_constrained, . ~ . - plurality_wi:closeness_wi - plurality_wi)
fit_pec1_constrained2
anova(fit_pec1, fit_pec1_constrained, fit_pec1_constrained2)
fit_pec1_constrained3 <- update(fit_pec1_constrained2, . ~ . + disprop_wi * closeness_wi)
anova(fit_pec1, fit_pec1_constrained, fit_pec1_constrained2, fit_pec1_constrained3)
summary(fit_pec1_constrained3)
# HO of equal fit not rejected
data_to_fit <- pdata.frame(tillman, index = panel_id)
plm_pec1 <- plm( 
    turnout ~ pec1 + log(disprop) + plurality * closeness + enep + growth + lnincome,
    model = "within", data = data_to_fit
)
summary(plm_pec1)
with(tillman, table(pr, plurality))
filter <- with(tillman, which(pr == plurality))
as.data.frame(tillman[filter, c("country", "year", "disprop", "pr", "plurality")])
aggregate(disprop ~ pr + plurality, data = tillman, FUN = mean)
aggregate(disprop ~ pr, data = tillman, FUN = mean)
cor(as.matrix(tillman[, c("pec1", "plurality", "turnout")]))
summary(update(plm_pec1, . ~ . + splines::bs(year, degree = 3)))
