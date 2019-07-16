library("plm")


# Functions
construct_formula <- function(response = "turnout_neu", treatment, control) {
    formula(paste0(response, "~", treatment, "+", control))
}


# Data
treatments <- c("pec_neu", "pec20_neu", "pectotal_neu", "smallpec_neu",
                "largepec_neu")
controls <- paste(
                  c("enp_votes", "disproportionality", 
                    "plurality_neu * closeness_neu", "growth_neu", 
                    "ln_e_migdppc"),
                  collapse = " + "
)
datasets <- list(
    tillman = filter(country_panel, in_tillman == 1) %>%
        select(
            iso3c, year2, election_id,
            turnout_neu, pec1, pec20, vote_pec, smallpec, largepec,
            enp_votes, disproportionality, plurality_neu, closeness_neu,
            growth_neu, ln_e_migdppc
        ) %>% 
        rename(pec_neu = pec1, pec20_neu = pec20, pectotal_neu = vote_pec,
            smallpec_neu = smallpec, largepec_neu = largepec
        ), 
    intersection = filter(country_panel, in_tillman == 1 & in_lsvergl == 1) %>%
        select(
            iso3c, year2, election_id,
            turnout_neu, pec_neu, pec20_neu, pectotal_neu, smallpec_neu, largepec_neu,
            enp_votes, disproportionality, plurality_neu, closeness_neu,
            growth_neu, ln_e_migdppc
        ),
    lsvergl = filter(country_panel, in_lsvergl == 1) %>%
        select(
            iso3c, year2, election_id,
            turnout_neu, pec_neu, pec20_neu, pectotal_neu, smallpec_neu, largepec_neu,
            enp_votes, disproportionality, plurality_neu, closeness_neu,
            growth_neu, ln_e_migdppc
        )
)


# Execute replication
model_results <- lapply(datasets, function(d){
    # d <- datasets[[1]]
    out <- vector("list", length = length(treatments))
    names(out) <- treatments
    for (t in treatments) {
        out[[t]] <- plm(
            construct_formula(treatment = t, control = controls),
            data = pdata.frame(d, index = c("iso3c", "year2")),
            effects = "twoway", model = "within"
        )
    }
    return(out)
    }
)
retrieve_estimate <- function(model, estimates, cluster){
    mask <- names(coef(model)) %in% estimates
    out <- matrix(0, nrow = sum(mask), ncol = 2,
        dimnames = list(NULL, c("beta", "se"))
    )
    rownames(out) <- names(coef(model)[mask])
    out[, "beta"] <- coef(model)[mask]
    out[, "se"] <- sqrt(diag(vcovBK(model, cluster = cluster))[mask])
    return(out)
}


retrieve_estimates <- function(model.list){
    vapply(model.list, FUN = retrieve_estimate)
}