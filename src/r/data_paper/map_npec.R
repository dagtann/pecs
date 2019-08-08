# Create a chloroplast world map that shows sample coverage and total number of
# pecs. Returns a png.
# Author: Dag Tanneberg
# Version info:
#     08/05/2019: v1.0
# Preamble =====================================================================
rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- c("sf", "rnaturalearth", "rnaturalearthdata")
for (p in packs){
    if(!require(p, character.only = TRUE)) {
        cat("Installing required package: ", p, "\n")
        install.packages(p, repos = "https://cloud.r-project.org")
        library(p, character.only = TRUE)
    }
}
theme_set(ggthemes::theme_fivethirtyeight())


# Map data
coastlines <- ne_coastline(scale = "medium", returnclass = "sf")
world <- ne_countries(scale = "medium", returnclass = "sf")


# Generate aesthetics
npec_data <- country_panel %>%
    filter(in_lsvergl == 1) %>%
    select(iso3c, year, nupec_neu) %>%
    group_by(iso3c) %>%
    mutate(
           min_year = min(year), max_year = max(year), npec = sum(nupec_neu)) %>%
    select(-year, -nupec_neu) %>%
    summarize_all(mean) %>%
    ungroup() %>%
    rename(iso_a3 = iso3c)
world <- left_join(world, npec_data, by = "iso_a3")


# Generate plot
p <- ggplot() +
    geom_sf(data = coastlines, size = 0.1) + 
    geom_sf(data = world, aes(fill = npec), colour = "#F0F0F0", size = 0.1) +
    scale_fill_gradient(low = "#fcfbfd", high = "#3f007d", na.value = "#F0F0F0") +
    coord_sf(
             crs = paste0("+proj=robin"),
             expand = FALSE) +
    labs(fill = "Total number\nof PECs") +
    theme(
        axis.text = element_blank(),
        legend.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = .3),
        plot.background = element_rect(fill = "white"),
        plot.margin = grid::unit(rep(0, 4), "lines"))
ggsave(file.path(path_project, "out", "map_npec.png"))


# housekeeping =================================================================
for (p in packs) detach(paste0("package:", p), character.only = TRUE)
rm(list = ls()[!(ls() %in% clean_workspace)])