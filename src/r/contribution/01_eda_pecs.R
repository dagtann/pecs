rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- c("stringdist", "cluster")
missing <- which(!(packs %in% rownames(installed.packages())))
if(any(missing)) {
    cat("Installing missing packages: ", packs[missing], "\n")
    install.packages(packs[missing], dependencies = TRUE,
        repos = "https://cloud.r-project.org"
    )
}
lapply(packs, library, character.only = TRUE)


# Basic descriptives on lsvergl pec data =========================================
filter <- is.na(country_panel$pec_neu) # obs in lsvergl?
any(filter) # sanity check: at least some NA -> pec count not brute forced to 0
length(unique(country_panel$iso3c[!filter])) # 35 democracies
range(country_panel$year2[!filter]) / 10 # 1945 to 2015
tmp <- aggregate(
    election_id ~ iso3c, subset(country_panel, !is.na(pec_neu)),
    function(x) max(seq_along(x))
)
any(is.na(tmp)) # sanity check: any election unidentified?
length(unique(tmp$iso3c)) # sanity check: still 37 democracies?
sum(tmp$election_id) # 564 elections
summary(tmp) # 5 to 27 elections per country
tmp[tmp$election_id >= median(tmp$election_id), ] # old democracies
tmp[tmp$election_id < median(tmp$election_id), ] # 3rd wave democracies + LUX (5yr legislature)
hist(tmp$election_id, breaks = 15) # apparent gap b/w 10 and 15 elections


# How many unique PECs coded? ====================================================
pec_all <- read_dta( # load complete chair data
    file.path(path_project, "dta", "raw", "PEC_all.dta"), encoding = "latin1"
)
unique_countries <- sort(unique(pec_all$country_name_short))
setdiff(unique(country_panel$iso3c[!filter]), unique_countries)
sort(intersect(country_panel$iso3c[!filter], unique_countries)) == unique_countries
# ERROR: country_panel lists more country than there are in pec_all

# pec_all data nests parties in elections in countries.
# PECs have been coded by party & election. Therefore, the same PEC entry occurs
# once for each party.
pec_strings <- names(pec_all)[grep("_string", names(pec_all), fixed = TRUE)]
tmp <- select(pec_all, c("country_name_short", "edate", pec_strings)) %>%
    rename("iso3c" = country_name_short) %>%
    gather("pec", "name", pec_strings) %>%
    filter(name != "") %>%
    group_by(iso3c, edate, name) %>%
    mutate(n = seq_along(edate)) %>%
    filter(n == 1) %>%
    select(-n)
length(unique(tmp$name)) # presumably 288 PECs

# Sanity check: The prior count of unique PECs increases for each, possibly
# wrong entry in <name>. More precisely, "LPA-CP", "LPA - CP", and "CP-LPA" will
# all be counted separetely even though all refer to the same PEC. In the following,
# string distances are checked to find and eliminate such problems.
pec_name_distances <- stringdistmatrix(
    unique(tmp$name), method = "cosine", useNames = "strings"
)
dist_density <- density(pec_name_distances); plot(dist_density)
(modal_distance <- dist_density$x[which.max(dist_density$y)]) # 0.9987673
mean(pec_name_distances < modal_distance) # ~ p90
# Note: bimodal distribution of distances
plot(ecdf(pec_name_distances))
abline(v = quantile(pec_name_distances, probs = seq(0, .1, .01)))
# Slope increases after 2 percent quantile

# create distance lookup table
dist_matrix <- as.matrix(pec_name_distances)
rnames <- rownames(dist_matrix); cnames <- colnames(dist_matrix)
dist_frame <- data.frame(p1 = FALSE, p2 = FALSE, d = FALSE)[0, ]
for (r in rnames[-1]) {
    current_location <- which(r == rnames)
    dist_frame <- rbind.data.frame(
        dist_frame, data.frame(
            p1 = r,
            p2 = cnames[1:(current_location-1)],
            d = dist_matrix[current_location, 1:(current_location-1)],
            stringsAsFactors = FALSE
        ),
        stringsAsFactors = FALSE
    )
}
rownames(dist_frame) <- NULL; str(dist_frame)
dist_frame <- dist_frame[with(dist_frame, order(d)), ]
dist_frame[1:20, ]
# Found very few candidates.

n_unique_pecs_by_country <- aggregate(
    name ~ iso3c, data = tmp, FUN = function(x){length(unique(x))}
)
summary(n_unique_pecs_by_country$name) # 1 to 24, median 6.5, mean 9
n_pec_tillman <- aggregate(pec1 ~ iso3c, data = tillman, FUN = sum)
tmp2 <- left_join(n_pec_tillman, n_unique_pecs_by_country, by = "iso3c") %>%
    rename("tillman" = pec1, "lsvergl" = name) %>%
    gather("source", "count", tillman, lsvergl)
ggplot(data = tmp2, aes(x = iso3c, y = count, fill = source)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "", y = "Count of PECs")

tmp3 <- select(tillman, iso3c, pec1, pec_neu) %>%
    rename("tillman" = pec1, "lsvergl" = pec_neu) %>%
    gather("source", "count", tillman, lsvergl) %>%
    group_by(iso3c, source) %>%
    summarise_at("count", sum, na.rm = TRUE)
ggplot(data = tmp3, aes(x = iso3c, y = count, fill = source)) + geom_bar(stat = "identity", position = "dodge")
