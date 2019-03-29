rm(list = ls())
options(help_type = "html")
path_project <- file.path("~", "github", "pecs")

packs <- c("tidyverse", "foreign")
for ( i in packs ) {
    if (!(i %in% rownames( installed.packages())))
        installed.packages(i, dependencies = TRUE)
    library(i, character.only = TRUE)
}
rm(packs)

tillman <- read.dta(
    file.path(path_project, "prestudies", "ellger", "tilmann_merger.dta")
)
tillman <- mutate(tillman, turnout = turnout / 100, vote_pec = vote_pec / 100)
detach(package:foreign)

clean_workspace <- c(ls(), "clean_workspace")