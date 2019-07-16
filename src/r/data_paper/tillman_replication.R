rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- c("plm")
if (!all(packs %in% installed.packages())) {
    mask <- packs %in% installed.packages()
    cat("Installing packages:", packs[!mask], "\n")
    install.packages(packs[!mask], repos = "https://cloud.r-project.org")
    rm(mask)
}
for (p in packs) library(p, character.only = TRUE)


# Functions
construct_formula <- function(response = "turnout_neu", treatment, control) {
    formula(paste0(response, "~", treatment, "+", control))
}


retrieve_estimate <- function(model, estimates, cluster = "group"){
    mask <- names(coef(model)) %in% estimates
    out <- matrix(0, nrow = sum(mask), ncol = 2,
                  dimnames = list(names(coef(model))[mask], c("beta", "se")))
    rownames(out) <- names(coef(model)[mask])
    out[, "beta"] <- coef(model)[mask]
    out[, "se"] <- sqrt(diag(plm::vcovBK(model, cluster = cluster))[mask])
    return(out)
}


retrieve_estimates <- function(model.list, ...){
    vapply(model.list, FUN = retrieve_estimate, FUN.VALUE = numeric(2), ...)
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
        ) %>%
        mutate(pectotal_neu = pectotal_neu / 100),
    intersection = filter(country_panel, in_tillman == 1 & in_lsvergl == 1) %>%
        select(
            iso3c, year2, election_id,
            turnout_neu, pec_neu, pec20_neu, pectotal_neu, smallpec_neu,
            largepec_neu, enp_votes, disproportionality, plurality_neu,
            closeness_neu, growth_neu, ln_e_migdppc
        ),
    lsvergl = filter(country_panel, in_lsvergl == 1) %>%
        select(
            iso3c, year2, election_id,
            turnout_neu, pec_neu, pec20_neu, pectotal_neu, smallpec_neu,
            largepec_neu, enp_votes, disproportionality, plurality_neu,
            closeness_neu, growth_neu, ln_e_migdppc
        )
)


# Execute replication
replication_results <- lapply(datasets, function(d){
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


# generate plot
pdta <- do.call(rbind.data.frame, lapply(replication_results,
                retrieve_estimates, estimates = treatments))
pdta[, "data"] <- str_split(rownames(pdta), pattern = "[:punct:]",
                            simplify = TRUE)[, 1]
pdta[, "type"] <- rep(c("beta", "sigma"), length(replication_results))
pdta <- gather(pdta, "treatment", "estimate", pec_neu, pec20_neu, pectotal_neu,
               smallpec_neu, largepec_neu) %>%
        spread(type, estimate)
ggplot(data = pdta, aes(x = data, y = beta)) +
    geom_linerange(aes(ymin = beta - qnorm(.975) * sigma,
                   ymax = beta + qnorm(.975) * sigma)) +
    geom_linerange(aes(ymin = beta - qnorm(.95) * sigma,
                   ymax = beta + qnorm(.95) * sigma), size = 1.1) +
    geom_point() +
    facet_wrap(~treatment)


# housekeeping
for (p in packs) detach(paste0("package:", p), character.only = TRUE)
rm(list = ls()[!(ls() %in% clean_workspace)])