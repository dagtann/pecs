rm(list = ls()[!(ls() %in% clean_workspace)])
packages <- c("foreign", "plm", "sandwich", "lmtest")
for(i in packages){
    if(!(i %in% rownames(installed.packages()))) {
        cat("Now installing required package:\\t", i)
        install.packages(i, dependencies = TRUE)
    }
    library(i, character.only = TRUE)
}

# Declare string constants
panel_id <- c("country", "year2")
control <- paste(
    c("enep", "disprop * pr", "plurality * closeness", "growth", "lnincome"),
    collapse = " + "
)
treatment <- c("pec1", "vote_pec")
response <- "turnout"

# Prepare data
data_to_fit <- pdata.frame(tillman, index = c("country", "year2"))

fitted_models <- lapply(treatment,
    function(t){
        frm <- as.formula(
            paste0(response, " ~ ", paste(t, control, sep = " + "))
        )
        pooled <- lm(frm, data = data_to_fit)
        fe <- plm(frm, model = "within", data = data_to_fit)
        re <- plm(frm, model = "random", data = data_to_fit)
        return(list(pooled = pooled, fe = fe, re = re))
    }
)
names(fitted_models) <- treatment
sapply(fitted_models$pec1, coef)

coef_names <- names(coef(fitted_models$pec1$pooled))
coef_matrix <- matrix(
    FALSE, ncol = 3, nrow = length(coef_names),
    dimnames = list(coef_names, c("pooled", "fe", "re"))
)
for(i in seq(ncol(coef_matrix))){
    coef_order <- match(names(coef(fitted_models$pec1[[i]])), coef_names)
    coef_matrix[coef_order, i] <- coef(fitted_models$pec1[[i]])
}
coef_matrix
phtest(fitted_models$pec1$fe, fitted_models$pec1$re)

coef_names <- names(coef(fitted_models$vote_pec$pooled))
coef_matrix <- matrix(
    FALSE, ncol = 3, nrow = length(coef_names),
    dimnames = list(coef_names, c("pooled", "fe", "re"))
)
for(i in seq(ncol(coef_matrix))){
    coef_order <- match(names(coef(fitted_models$vote_pec[[i]])), coef_names)
    coef_matrix[coef_order, i] <- coef(fitted_models$vote_pec[[i]])
}
coef_matrix
phtest(fitted_models$vote_pec$fe, fitted_models$vote_pec$re)
