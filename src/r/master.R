rm(list = ls())
options(help_type = "html")
path_project <- file.path("~", "github", "pecs")

packs <- c("tidyverse", "foreign")
for( i in packs ) {
    if(!(i %in% rownames( installed.packages())))
        installed.packages(i, dependencies = TRUE)
    library(i, character.only = TRUE)
}
rm(packs)

tillman <- read.dta(
    file.path(path_project, "prestudies", "ellger", "tilmann_merger.dta")
)
detach(package:foreign)
cleanWorkspace <- c(ls(), "cleanWorkspace")