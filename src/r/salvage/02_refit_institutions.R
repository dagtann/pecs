
rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- c("car", "splines", "plm")
missing <- which(!(packs %in% rownames(installed.packages())))
if(any(missing)) {
    cat("Installing missing packages: ", packs[missing], "\n")
    install.packages(packs[missing], dependencies = TRUE)
}
lapply(packs, library, character.only = TRUE)

# institution lookup table
institution_label <- paste(c("pr", "plurality", "disprop", "enep"), "wi", sep ="_")
institution_location <- which(names(tillman) %in% institution_label)
names(institution_location) <- institution_label

# time trend
yr_range <- as.numeric(min(tillman$year):max(tillman$year))
knots_num <- 3
knots_loc <- quantile(yr_range, probs = seq(0, 1, length.out = knots_num))
yr_spline_labs <- paste0(paste0("yr_spline", 1:(knots_num + 1)))

# Data objects
yr_splines <- bs(yr_range, knots = knots_loc[-c(1, knots_num)], degree = 3)
yr_splines <- cbind.data.frame(yr_splines, yr_range)
colnames(yr_splines) <- c(yr_spline_labs, "year")
tillman <- left_join(tillman, yr_splines, by = "year")
data_to_fit <- subset(tillman, !(country %in% c("Australia", "Luxemburg", "Canada")))

# univariate assessment
apply(tillman[, institution_location], 2, summary)
apply(tillman[, institution_location], 2, sd)
boxplot(tillman[, institution_location])
# very little variation in plurality / pr
# what countries contribute variation?
pdta <- gather(tillman, key = "variable", value = "value", institution_location)
ggplot(data = pdta, aes(x = country, y = value)) +
    geom_boxplot() + facet_wrap(~variable) +
    theme(axis.text.x = element_text(angle = 90))
# sensitivity analysis using simplified model:
drop <- c("France", "Italy", "Japan", "New Zealand")
fit <- lm(turnout_wi ~ pec1_wi + enep_wi + disprop_wi + pr_wi +
    plurality_wi + closeness_wi + growth_wi + lnincome_wi,
    data = data_to_fit
)
beta_hat <- matrix(FALSE, nrow = length(coef(fit)), ncol = length(drop) + 1,
    dimnames = list(names(coef(fit)), c(drop, "original"))
)
for(i in seq(length(drop))) {
    beta_hat[, i] <- coef(
        update(
            fit, . ~ .,
            data = subset(
                tillman, !(
                    country %in% append(
                       c("Australia", "Luxemburg", "Canada"), drop[i]
                    )
                )
            )
        )
    )
}
beta_hat[, ncol(beta_hat)] <- coef(fit)
beta_hat
# pr & plurality extremely sensitive to France, Italy, Japan + strongly correlated
as.data.frame(tillman[tillman$country %in% drop, c("country", "year2", "pr", "plurality")])
# France: plurality == 1 only in 2007!? All but 1986 (0, 0)
# Italy: Switches from PR to all Plurality in 1994
# Japan: (0, 0) until 1993, all plurality afterwards
# These codings are fishy.

scatterplotMatrix(tillman[, institution_location], smooth = FALSE)
cor(tillman[, institution_location])
# plurality & pr strong negative correlation
# disprop_wi & pr_wi strong negative correlation
fit_1 <- update(fit, . ~ . - pr_wi - plurality_wi,
    data = subset(tillman, !(country %in% c("Australia", "Luxemburg", "Canada")))
)
summary(fit_1)
vif(fit_1) # multicollinearity not an issue anymore
residualPlots(fit_1)
avPlots(fit_1, )
data_to_fit[225, ]
fit_2 <- update(fit_1, data = data_to_fit[-c(225), ])
compareCoefs(fit_1, fit_2) # coef only lightly reduced
avPlots(fit_2)
ceresPlot(fit_2, "closeness_wi")
influenceIndexPlot(fit_2)

fit_3 <- update(fit_1, data = data_to_fit[-c(106:108), ])
compareCoefs(fit_1, fit_3) # coef only lightly reduced
plot(data_to_fit$closeness)
abline(v = 190)
ggplot(data = data_to_fit, aes(x = country, y = closeness_wi)) + geom_boxplot()
data_to_fit[, "flag"] <- 0
data_to_fit[106:108, "flag"] <- 1
data_to_fit[106:108, ]
data_to_fit[101:112, ]
# Fishy data: Ireland has >= 10.0 lnincome b/w 2002 and 2011,
# i.e. ~= 22026.47 $ / capita. Figures are not entirely untrustworthy. However,
# there is no other country in that range, numbers do not nosedive after 2008,
# and Irish lnincome is in the range of 2.4 to 3 before 2002. Hence, numbers are
# weird for both, Ireland and the sample.




vif(fit_1) # multicollinearity not an issue anymore
residualPlots(fit_1)
avPlots(fit_1, )




marginalModelPlots(fit_1)

# closeness


fit_2 <- update(fit_1, . ~ . + enep_wi * disprop_wi * closeness_wi)
summary(fit_2)
anova(fit_1, fit_2)

fit_3 <- update(fit_1, . ~ . + yr_spline1 + yr_spline2 + yr_spline3 + yr_spline4)
summary(fit_3)

fit_4 <- update(fit_3, . ~ . - pec1_wi + vote_pec_wi)
summary(fit_4)

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
