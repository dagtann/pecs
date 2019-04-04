# Prepare workspace
rm(list = ls())
options(help_type = "html")

path_project <- file.path("~", "github", "pecs")
base_size <- 18 # plot character size

packs <- c("tidyverse")
for ( i in packs ) {
    if (!(i %in% rownames( installed.packages())))
        installed.packages(i, dependencies = TRUE)
    library(i, character.only = TRUE)
}
rm(packs, i)

clean_workspace <- c(ls(), "clean_workspace")

# Data munging
munging_files <- list.files(
    file.path(path_project, "src", "r", "data_munging"), full.names = TRUE
)
lapply(munging_files, source)
# housekeeping
clean_workspace <- c(ls(), "clean_workspace")