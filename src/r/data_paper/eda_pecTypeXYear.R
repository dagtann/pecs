# This script generate a facetted bar chart. Each facet show one of the three
# descriptive properties, type, joint program, and incumbency, of PECs against
# time. The script returns a png file.
# Author: Dag Tanneberg
# Version info:
#   07/20/2019: Fixed fill colour bug, reformatted, added housekeeping.
#   07/20/2019: First version.
# ==============================================================================
# Preamble
rm(list = ls()[!(ls() %in% clean_workspace)])
packs <- "lubridate"
if (!all(packs %in% installed.packages())) {
    mask <- packs %in% installed.packages()
    cat("Installing packages:", packs[!mask], "\n")
    install.packages(packs[!mask], repos = "https://cloud.r-project.org")
    rm(mask)
}
for (p in packs) library(p, character.only = TRUE)


# Constants
fill_colors <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0",
                 "#f0027f")


# Raw data & Hooks
pecs <- read_dta(
                 file.path(path_project, "dta", "raw", "PEC_raw_stable15_2.dta"),
                 encoding = "latin1") %>%
    rename(iso3c = country_name_short)
indicators <- list(
                   pecs = grep("^pec[0-9]$", names(pecs)),
                   types = grep("^pec[0-9]_type$", names(pecs)),
                   incumbents = grep("^pec[0-9]_incumbent$", names(pecs)),
                   programs = grep("^pec[0-9]_prog$", names(pecs)))


# Convert pecs*_type/incumbent/prog from wide to long format
aggregated_information <- lapply(seq(indicators), function(l) {
        val_label <- names(indicators)[l]
        out <- aggregate(pecs[, indicators[[l]]],
                         list(iso3c = pecs[["iso3c"]],
                              election_id = pecs[["election_id"]]),
                         FUN = function (row) {
                                               ifelse(all(is.na(row)), NA,
                                                      max(row, na.rm = TRUE))})
        gather_cols <- names(out)[3:ncol(out)]
        out <- gather_(out, "pec_no", val_label, gather_cols = gather_cols)
        out <- mutate(out, pec_no = as.integer(str_sub(pec_no, 4, 4)))
        return(out)
    }
)
pecs_long <- plyr::join_all(aggregated_information,
                            by = c("iso3c", "election_id", "pec_no"))
pecs_long <- filter(pecs_long, pecs == 1)
tmp <- select(pecs, election_id, election_date) %>%
    mutate(year = year(election_date)) %>%
    select(-election_date) %>%
    group_by(election_id) %>%
    summarize_all(mean)
pecs_long <- left_join(pecs_long, tmp, by = c("election_id"))
rm(tmp)


# Prepare plot data
freq_pecs <- aggregate(pecs ~ year + types, data = pecs_long, FUN = sum)
names(freq_pecs)[3] <- c("freq")
freq_pecs[, "facet"] <- "Type"
freq_incumbents <- aggregate(pecs ~ year + incumbents, data = pecs_long,
                             FUN = sum, na.rm = TRUE)
freq_incumbents <- subset(freq_incumbents, incumbents == 1)
names(freq_incumbents)[2:3] <- c("facet", "freq")
freq_incumbents[, "facet"] <- "Incumbency"
freq_incumbents[, "types"] <- -99
freq_programs <- aggregate(pecs ~ year + programs, data = pecs_long,
                           FUN = sum, na.rm = TRUE)
freq_programs <- subset(freq_programs, programs == 1)
names(freq_programs)[2:3] <- c("facet", "freq")
freq_programs[, "facet"] <- "Joint program"
freq_programs[, "types"] <- -99
pdta <- rbind.data.frame(freq_pecs, freq_incumbents, freq_programs)
pdta <- within(pdta, {
              types_label <- factor(types, levels = 1:6,
                                    labels = c("Nomination agreements",
                                               "Joint lists",
                                               "Dual-ballot instructions",
                                               "Vote transfer instructions",
                                               "Public commitment", "Other"))
              facet_label <- factor(facet, levels = c("Type", "Incumbency",
                                                      "Joint program"))})


# Generate plot
p <- ggplot(data = pdta, aes(x = year, y = freq)) + 
    geom_point(aes(y = freq - .5, colour = types_label), size = 0) +
    geom_bar(fill = "#3C3C3C", size = 0.1, width = 1, stat = "identity") +
    geom_bar(aes(fill = types_label), colour = "white", size = 0.1, width = 1,
                 stat = "identity", show.legend = FALSE) +
    facet_wrap(~ facet_label) +
    scale_color_manual(values = fill_colors, na.translate = FALSE) +
    scale_fill_manual(values = fill_colors, na.translate = FALSE) + 
    labs(y = "Count") +
    guides(colour = guide_legend(override.aes = list(size = 2))) +
    ggthemes::theme_fivethirtyeight(base_size = base_size) +
    theme(axis.title = element_text(),axis.title.x = element_blank(), 
          legend.key = element_blank(), legend.title = element_blank())
ggsave(file.path(path_project, "out", "bar_pecTypeYear.png"), p,
       width = plot_width, height = plot_width / 1.618)


# House keeping
for (p in packs) detach(paste0("package:", p), character.only = TRUE)
rm(list = ls()[!(ls() %in% clean_workspace)])
