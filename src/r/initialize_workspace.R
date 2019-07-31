# Prep workspace
rm(list = ls())
options(help_type = "html")


packs <- c("tidyverse")
for ( i in packs ) {
    if (!(i %in% rownames(installed.packages()))) {
        install.packages(i, dependencies = TRUE)
    }
    library(i, character.only = TRUE)
}
rm(packs, i)


# Constants
path_project <- file.path("~", "github", "pecs")
path_archive <- file.path("~", "OneDrive", "data", "archive")
base_size <- 18 # plot character size
plot_width <- 10
clean_workspace <- c(ls(), "clean_workspace")

# Data munging
munging_files <- list.files(
    file.path(path_project, "src", "r", "data_munging"), full.names = TRUE
)
lapply(munging_files, source)


# housekeeping
rm(list = ls()[!(ls() %in% clean_workspace)])