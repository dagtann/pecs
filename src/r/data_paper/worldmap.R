packs <- c("cowplot", "googleway", "ggplot2", "ggrepel",  "ggspatial", "sf", "rnaturalearth", "rnaturalearthdata")
for (p in packs){
    if(!(p %in% rownames(installed.packages()))) {
        cat("Installing required package: ", p, "\n")
        install.packages(p, repos = "https://cloud.r-project.org")
    }
    library(p, character.only = TRUE)
}


library("sf")
library("countrycode")
data(iso3166)
countries <- map_data("world") %>%
    rename(mapname = region)
countries <- left_join(countries, iso3166, by = "mapname")
countries <- within(countries,
    iso3c <- countrycode(ISOname, "country.name", "iso3c", warn = TRUE))

table(country_panel$in_lsvergl)
tmp <- select(country_panel, iso3c, in_lsvergl) %>%
    mutate(in_lsvergl = ifelse(is.na(in_lsvergl), 0, 1)) %>%
    group_by(iso3c) %>%
    summarise(in_lsvergl = max(in_lsvergl, na.rm = TRUE))
countries <- left_join(countries, tmp, by = "iso3c")
p <- ggplot(data = countries,
            aes(x = long, y = lat, group = group)
    ) +
    geom_polygon(colour = "white")
p
with(countries, table(iso3c == a3, exclude = NULL))
with(tmp, table(iso3c))

