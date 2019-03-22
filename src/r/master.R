rm(list = ls())
options(help_type = "html")
path_project <- file.path("~", "github", "pecs")

packs <- c("tidyverse")
for( i in packs ) {
    if(!(i %in% rownames( installed.packages())))
        installed.packages(i, dependencies = TRUE)
    library(i, character.only = TRUE)
}
rm(packs)
clean_workspace <- c(ls(), "clean_workspace")