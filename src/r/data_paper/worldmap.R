packs <- c("cowplot", "googleway", "ggplot2", "ggrepel",  "ggspatial", "sf", "rnaturalearth", "rnaturalearthdata")
for (p in packs){
    if(!require(p, character.only = TRUE)) {
        cat("Installing required package: ", p, "\n")
        install.packages(p, repos = "https://cloud.r-project.org")
        library(p, character.only = TRUE)
    }
}

theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
head(world)

# create a basic mao
world_map <- ggplot(data = world) + geom_sf()
world_map + labs(x = "Longitude", y = "Latitude", title = "World map",
                 subtitle = paste0(
                                   "(",
                                   length(unique(world$name)),
                                   " countries)"))

# layer attributes can be manipulated using standard ggplot2 arguments
ggplot(data = world) + geom_sf(fill = "green", colour = "white")
ggplot(data = world, aes(fill = pop_est)) + geom_sf() +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# projection and extent
# defaul coordinate sytems: cs of 1st layer to define a cs
# fall back to WGS84 (GPS reference system) if non provided
    # using argument crs and a valid PROJ4 string, world maps can be projected
    # on the fly
ggplot(data = world) + 
    geom_sf() + 
    coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")


# zooming
ggplot(data = world) +
    geom_sf() +
    coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)

# add country and other names
# world data contains country names and centroid coordinates
world_points < st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
ggplot(data = world) +
    geom_sf() +
    geom_text(data = world_points, aes(x = X, y = Y, label = name)) +
    coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) +
    annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico",
             fontface = "italic", color = "grey22", size = 6)



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

