rm(list = ls()[!(ls() %in% clean_workspace)])
packages <- c("foreign")
for (i in packages) {
    if (!(i %in% rownames(installed.packages()))) {
        cat("Now installing required package:\\t", i)
        install.packages(i, dependencies = TRUE)
    }
    library(i, character.only = TRUE)
}

# Load and prepare data
tillman <- read.dta(
    file.path(path_project, "prestudies", "ellger", "tilmann_merger.dta")
)

# === Basic information on the panel ==========================================
dim (tillman) # 227 obs
length(unique(tillman[["country"]])) # 19 countries
sort(unique(tillman[["country"]]))
#  [1] "Austria"     "Belgium"     "Denmark"     "Finland"     "France"
#  [6] "Germany"     "Greece"      "Iceland"     "Ireland"     "Israel"
# [11] "Italy"       "Japan"       "Netherlands" "New Zealand" "Norway"
# [16] "Portugal"    "Spain"       "Sweden"      "UK"
# Western Europe + Japan, New Zealand, Israel
summary(tillman[["year"]]) # captures 1970 - 2011
pdta <- group_by(tillman, country) %>%
    count(year)
ggplot(data = pdta, aes(x = year, y = n)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    facet_wrap(~ country)
filter(pdta, country == "Germany")
# Panel includes only election years
filter(pdta, n != 1)
# Panel entries are not unique
#   -> Greece, Ireland, and the UK have two elections in one year
pdta <- mutate(pdta,
    l1_year = lag(year, by_sort = year),
    delta = year - l1_year
)
ggplot(data = pdta, aes(x = country, y = delta)) + geom_boxplot()
# Panel entries are not equally spaced in time
summary(
    group_by(tillman, country) %>%
    count(country)
)
rm(pdta)
# Number of panel entries differs for each country
# Tillman uses an unbalanced panel of countries in Western Europe plus Japan,
# New Zealand, and Israel. Countries enter the panel following their first
# democratic election after 1970. The observation period ends in 2011. The
# number of elections covered ranges from 9 to 16 with a median of 12.
# The length of election periods differs tremendously between and
# occasionally within countries, ranging from 1 to 5 with median 4. Finally,
# some observations in the panel are not uniquely identified.

# === Basic information on PECs ===============================================
apply(tillman[, c("pec1", "nupec", "vote_pec")], 2, summary)
# PECs entered more than half of all elections and won between 0 and 100 per cent
# of the vote. The number of PECs per election ranges between 0 and 4.
pdta <- group_by(tillman, country) %>%
    summarise(pec1 = mean(pec1, na.rm = TRUE))
ggplot(data = pdta, aes(x = reorder(country, pec1), y = pec1)) + geom_col() +
    scale_y_continuous(breaks = seq(0, 1, .1))
rm(pdta)
# PECs entered every French and Israelian election! PECs formed only in 8 per
# cent of all Japanese elections. Almost 2/3 of all German elections saw PECs.
ggplot(data = subset(tillman, pec1 == 1), aes(x = nupec)) + stat_ecdf()
round(prop.table(table(tillman[["nupec"]])), 2)
# Most often 1 or at most 2 PECs form.

ggplot(subset(tillman, pec1 == 1), aes(x = year, y = vote_pec)) +
    geom_point(alpha = .8) + geom_smooth()

by(tillman$vote_pec, INDICES = tillman$pec1, FUN = summary)
pdta <- select(tillman, year, turnout, pec1, vote_pec) %>%
    mutate(turnout = turnout / 100) %>%
    mutate(vote_pec = vote_pec / 100) %>%
    gather(key = "variable", value = "value", turnout:vote_pec)
pdta <- within(pdta, {
    variable <- factor(
        variable,
        levels = c("turnout", "pec1", "vote_pec"),
        labels = c("Wahlbeteiligung", "PEC?", "Ergebnis der PEC")
    )
    }
)
ggplot(pdta, aes(x = year, y = value)) +
    geom_jitter() + geom_smooth() + facet_grid(variable ~ .) + labs(x = "Jahr", y = "Relativer Anteil")
ggplot(data = tillman, aes(x = factor(nupec), y = vote_pec)) + geom_boxplot() + geom_point()
select(tillman, country, year, vote_pec) %>%
filter(vote_pec > 90)
