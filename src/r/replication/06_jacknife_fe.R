library("plm")


execute_jackknife <- function(drop, from, formula, data){
    # drop = "France"
    # from = "country"
    # data = data_to_fit
    model_data <- data[data[[from]] != drop, ]
    fit <- plm(as.formula(formula), data = model_data, model = "within")
    cbind(beta = coef(fit), se = sqrt(diag(vcov(fit))))
}

# Declare string constants
panel_id <- c("country", "year2")
control <- paste(
    c("enep", "disprop * pr", "plurality * closeness", "growth", "lnincome"),
    collapse = " + "
)
treatment <- c("pec1", "vote_pec")
response <- "turnout"
country_entries <- sort(unique(tillman$country))

# Prepare data
data_to_fit <- pdata.frame(tillman, index = c("country", "year2"))

# execute jacknife
jackknife_pec1 <- lapply(country_entries, function(c) {
    execute_jackknife(
        drop = c,
        from = "country",
        formula = paste0(response, " ~ ", paste(treatment[2], control, sep = " + ")),
        data = data_to_fit
    )
    }
)
names(jackknife_pec1) <- country_entries
plot_data <- do.call(rbind.data.frame, jackknife_pec1)
split_pos <- str_locate(rownames(plot_data), "\\.")[, 1]
plot_data[, "country"] <- str_sub(
    rownames(plot_data),
    end = split_pos - 1
)
plot_data[, "term"] <- str_sub(
    rownames(plot_data),
    start = split_pos + 1
)
rownames(plot_data) <- NULL
plot_data <- mutate(plot_data,
    upper = beta + qnorm(1 - .05 / 2) * se,
    lower = beta - qnorm(1 - .05 / 2) * se
)
coef_reference <- coef(
    plm(
        as.formula(paste0(response, " ~ ", paste(treatment[2], control, sep = " + "))),
        data = data_to_fit,
        model = "within"
    )
)
coef_reference <- data.frame(
    term = names(coef_reference), reference = coef_reference
)
plot_data <- left_join(plot_data, coef_reference, by = "term")

ggplot(data = plot_data,
    aes(x = country, y = beta, ymin = lower, ymax = upper)
) +
    geom_hline(aes(yintercept = reference), col = "red") +
    geom_pointrange() +
    facet_wrap(~term, scales = "free_y") +
    theme(
        axis.text.x = element_text(angle = 45)
    )

