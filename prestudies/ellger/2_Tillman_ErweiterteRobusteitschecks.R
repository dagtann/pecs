# The Goal is to Replace Tilmans' Analyses with our data

rm(list=ls())
options(scipen=999)

library("dplyr")
library("readxl")
library("foreign")
library("haven")
library('lubridate')
library ('countrycode')
library("RCurl")
library("sandwich")
library("stargazer")
library("car")
library("ggplot2")
library("gridExtra")
library("ggrepel")
library(RCurl)
library(gdata) 
library(zoo)

getwd()

# Information on cabinets and if they resulted/ originated from a PEC (Dummy)
till.data <- read_dta("tilmann_merger.dta")
## Countries in dataset
unique(till.data$country)


new.pec <- read.csv("PEC_DFG.csv")
new.pec <- new.pec %>% filter(election_id != 462 & election_id != 158) 
new.pec <- new.pec

##### Comparison of datasets
# 19 countries
length(unique(till.data$country))
# 37 countries
length( unique(new.pec$country_name))
# Different time spans:
till.years <- (unique(till.data$year))
length(till.years)
our.years <- ( unique(new.pec$year))
length(our.years)


### Importing the Golder data
golder.data <-  read.csv( "golder_data-v3.csv" )
golder.data$country <- as.character(golder.data$country)
golder.data$country[golder.data$country == "Greek Cyprus"] <- "Cyprus"
golder.data$country[golder.data$country == "West Germany"] <- "Germany"
golder.data$country[golder.data$country == "Czechoslovakia"] <- "Czech Republic"
golder.data$country[golder.data$ccode == 380] <- "Sweden"
golder.data$country[golder.data$ccode == 338] <- "Malta"
golder.data$country <- as.factor(golder.data$country)

golder.subset <- golder.data %>% 
  filter(presidential == 0 )  %>% 
  filter(country %in% new.pec$country_name) %>%
  select(country, date, aclp_code, ccode, ccode2, 
         legislative_type, elecrule, seats, 
         tier1_avemag, enep, enep_others, enep1,
         enpp, enpp1, year, month, day, regime ) %>%
  rename( country_name = country)

## Changing wrong values befor the merge
new.pec <- new.pec %>% mutate(month = ifelse(country_name == "Czech Republic" & year == 1996 , 5, month))
new.pec <- new.pec %>% mutate(month = ifelse(country_name == "France" & year == 1997 , 5, month))
new.pec <- new.pec %>% mutate(month = ifelse(country_name == "Ireland" & year == 1954 , 5, month))
new.pec <- new.pec %>% mutate(month = ifelse(country_name == "Lithuania" & year == 1992 , 10, month))
new.pec <- new.pec %>% mutate(month = ifelse(country_name == "Lithuania" & year == 1996 , 11, month))
new.pec <- new.pec %>% mutate(month = ifelse(country_name == "Latvia" & year == 1995 , 9, month))
new.pec <- new.pec %>% mutate(month = ifelse(country_name == "Netherlands" & year == 1971 , 4, month))
new.pec <- new.pec %>% mutate(month = ifelse(country_name == "Portugal" & year == 1979 , 12, month))
new.pec <- new.pec %>% mutate(month = ifelse(country_name == "Slovakia" & year == 1994 , 9, month))
new.pec <- new.pec %>% mutate(month = ifelse(country_name == "Turkey" & year == 1987 , 11, month))
new.pec$month <- as.factor(new.pec$month)

merge.pec.golder <-  left_join(new.pec, golder.subset, by = c("country_name", "year", "month" ))

### Adding disprop and polarization data from parlgov
parlgov.stats <- read.csv("parlgov_disprop.csv")
parlgov.subset <- parlgov.stats %>% 
  select(election_id, disproportionality, turnout, enp_votes, enp_seats, polarization_vote)
parlgov.pec.joint <-  left_join(merge.pec.golder, parlgov.subset, by = c("election_id"))

### Adding Info from ParlGov View Election Stable 15
parlgov.election.15 <- read.csv("view_election_stable15.csv")
parlgov.election.15 <- parlgov.election.15 %>%  filter(! is.na(vote_share))  %>%  filter(election_type == "parliament")

# Merging Cdu/Csu  and Lib/Nat to one party
parlgov.election.15 <- parlgov.election.15 %>% group_by(election_id) %>%
  mutate(vote_share = ifelse(party_id == 808, (vote_share[party_id==1180] + vote_share[party_id==808]), vote_share),
         seats =    ifelse(party_id == 808, (seats[party_id==1180] + seats[party_id==808]), seats)   ) %>%
  filter(party_id != 1180) %>%
  mutate(vote_share = ifelse(party_id == 1411, (vote_share[party_id==1411] + vote_share[party_id==184]), vote_share),
         seats =    ifelse(party_id == 1411, (seats[party_id==1411] + seats[party_id==184]), seats)   ) %>%
  filter(party_id != 184)  %>% ungroup()
# saving the altered ParlGov Election Version:
write.csv(parlgov.election.15, "3_parlgov_election.csv")

closeness <- parlgov.election.15  %>%  group_by(election_id)  %>%
  mutate(vote_share = vote_share / sum(vote_share)) %>% 
  arrange(desc(vote_share)) %>%
  mutate(closeness = vote_share - dplyr::lag(vote_share)) %>% 
  summarise(closeness = nth(closeness,2))

closeness.added <-  left_join(parlgov.pec.joint, closeness, by = c("election_id"))

### Economic Variables from Penn Economic Data
## using Penn economic data
penn.econ <- read_excel("pwt90.xlsx", sheet= 3)
names(penn.econ)
penn.econ.gdp <- penn.econ %>%  mutate(gdpe_pc = rgdpe/pop, gdpo_pc = rgdpo/pop) %>%
  group_by(country) %>%
  mutate(gdpe_growth =  rgdpe - dplyr::lag(rgdpe)) %>%
  mutate(gdpo_growth =  rgdpo - dplyr::lag(rgdpo)) %>%
  ungroup(country) %>%
  select( -c(country, pop, rgdpe, rgdpo))

penngdp.added <- left_join(closeness.added, penn.econ.gdp, by = c("country_name_short" = "iso3c", "year")) # problematic: Worldbank data starts at 1970s for some countries

# Creating PR and Plurality-Dummies
final.variables <- penngdp.added %>%
  mutate( pr = ifelse(penngdp.added$legislative_type != 1, 1,0),
          plurality = ifelse(penngdp.added$legislative_type != 2, 1,0),
          majoritarian = ifelse(penngdp.added$legislative_type == 1, 1,0))

# Creating the Interactions
final.variables2 <- final.variables  %>%
  mutate( pluralityXcloseness = (final.variables$plurality * final.variables$closeness),
          prXdisprop =(final.variables$pr * final.variables$disproportionality),
          majoritarianXcloseness = (final.variables$majoritarian * final.variables$closeness))  %>%
  mutate( compulsory = 0)

# Creating Compulsory-Voting Dummy:
comp.countries <- c("Australia", "Belgium", "Brazil",
                    "Cyprus", "Liechtenstein", "Luxembourg",
                    "Turkey", "Greece", "Netherlands")
final.variables2$compulsory <- ifelse( final.variables2$country_name %in% comp.countries, 1, 0) 
# Specifically fro Greece (-2000) and Netherlands (-1967)
final.variables2$compulsory <- ifelse( final.variables2$country_name == "Greece" & final.variables2$year >= 2000, 0, final.variables2$compulsory) 
final.variables2$compulsory <- ifelse( final.variables2$country_name == "Netherlands" & final.variables2$year >= 1967, 0, final.variables2$compulsory) 

enhanced.data <- final.variables2 %>% 
  select(c(election_id, country_id, country_name, country_name_short, 
           election_date, year, turnout, compulsory,starts_with("pec"),
           enep, enpp, pr, disproportionality, prXdisprop,
           plurality, closeness, pluralityXcloseness, gdpe_growth, gdpe_pc, majoritarian, majoritarianXcloseness))




##### Overview over the enhanced dataset:
length(unique(enhanced.data$country_id))   # 37 countries
length(unique(enhanced.data$year))         # 71 years
length(enhanced.data$election_id)          # 580 election
sum (enhanced.data$pec_neu)                # 258 elections with at least one PEC
#### Summary:
# 580 elections of 37 democratic countries over 71 years. 258 contained PECs
  



##########################################
# Reanalysis
##########################################
### Creating Fixed Effects Models
full.m1 <-lm(turnout ~ enep + disproportionality +
               pr + prXdisprop + plurality + 
               closeness + pluralityXcloseness +
               gdpe_growth + gdpe_pc  + factor(country_id) , data= enhanced.data)
# M2
full.m2 <-lm(turnout ~ pec_neu + enep + disproportionality +
               pr + prXdisprop + plurality + 
               closeness + pluralityXcloseness +
               gdpe_growth + gdpe_pc  + factor(country_id) , data=enhanced.data)
# M4
full.m4 <-lm(turnout ~ pectotal_neu + enep + disproportionality +
               pr + prXdisprop + plurality + 
               closeness + pluralityXcloseness +
               gdpe_growth + gdpe_pc  + factor(country_name_short) , data=enhanced.data)


##################################################
# OLS Regressions -  Clustered SEs (see script 1)
full.m4.linear <-lm(turnout ~ pectotal_neu + enep + disproportionality +
            pr + prXdisprop + plurality + 
            closeness + pluralityXcloseness +
            gdpe_growth + gdpe_pc + compulsory + year, data=enhanced.data)

full.m1.linear <-lm(turnout ~ enep + disproportionality +
                      pr + prXdisprop + plurality + 
                      closeness + pluralityXcloseness +
                      gdpe_growth + gdpe_pc + compulsory + year , data=enhanced.data)

full.m2.linear <-lm(turnout ~ pec_neu + enep + disproportionality +
                      pr + prXdisprop + plurality + 
                      closeness + pluralityXcloseness +
                      gdpe_growth + gdpe_pc + compulsory + year , data=enhanced.data)


###########
#### Creating final OLS-Regression Table 3


var_names2 <- c("PEC-Dummy", 
               "PEC-Stimmenanteil",
               "ENP",
               "Gallagher-Index",
               "PR-Dummy",
               "PR X Gallagher",
               "Mehrheitswahl-Dummy",
               "Closeness",
               "Mehrheitswahl X Closeness",
               "BIP-Wachstum",
               "BIP-PK (log.)",
               "Wahlpflicht",
               "Wahljahr")

stargazer(full.m1.linear, full.m2.linear,full.m4.linear,
          single.row = TRUE, omit.stat = c("rsq", "f"), type = "html",
          se=list( coef(summary(full.m1.linear,cluster = c("country_id")))[, 2], 
                                    coef(summary(full.m2.linear,cluster = c("country_id")))[, 2],
                                   coef(summary(full.m4.linear,cluster = c("country_id")))[, 2]),
          add.lines = list(c("Fixed effects?", "Nein", "Nein", "Nein")),
          covariate.labels = var_names2,
          dep.var.labels=c("Wahlbeteiligung"), omit = "factor" , out="Tabelle3_final.html")

 write.csv(enhanced.data, "enhanced_data.csv")

 
##########################
#Diagnostics
###########################
 
#########################
#### 1: Einflussreiche Fälle:
data.variance <- enhanced.data %>% group_by(country_id) %>% 
  mutate( variance_maj = var(majoritarian),
          variance_pr = var (pr),
          variance_plural = var(plurality),
          sd_pectotal_neu = sd(pectotal_neu)) %>% 
  filter( election_id != 538 ) %>% ungroup()

# Calculating standard deviation of critical variables over time
sum.variance <- data.variance %>% group_by(country_id) %>% 
  summarise( sd_turnout = sd(turnout),
             sd_pectotal_neu = sd(pectotal_neu),
             sd_pec_neu = sd(pec_neu),
             sd_maj = sd(majoritarian),
             sd_pr = sd (pr),
             sd_plural = sd(plurality),
             country_name = first(country_name)) %>%
  ungroup %>% arrange(  desc(sd_pectotal_neu), desc(sd_turnout))

#########################
# Abbildung 3: Visualisierung d. einflussreichen Faelle:
#########################
plot1 <-  ggplot2::ggplot(data = sum.variance, aes(x= sd_pectotal_neu, y = sd_turnout )) +
  geom_point(shape=19, alpha=1/3)  + 
  theme_bw() +
  geom_text_repel(aes(label= country_name) ) +
  xlim(0, 50) + ylim(0,20) +
  labs(x = "Standardabw. PEC-Stimmenanteil",
       y = "Standardabw. Wahlbeteiligung") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

plot2 <- ggplot2::ggplot(data = sum.variance, aes(x= sd_pec_neu, y = sd_turnout )) +
  geom_point(shape=19, alpha=1/3)  +
  xlim(0, 0.65) + ylim(0, 20) +
  theme_bw() +
  geom_text_repel(aes(label= country_name) ) +
  labs(x = "Standardabw. PEC-Dummy",
       y = "Standardabw. Wahlbeteiligung") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

grid.arrange(plot1, plot2, ncol = 1)




#################################
# Further Robustness tests
###################################
# creating necessary interaction:
diagnostic.data <- enhanced.data %>%  mutate( prXcloseness = pr* closeness) 


##### Dropping Specific Country_groups:
# Without countries with high volatility in pectotal
diagnostic.data1 <- diagnostic.data %>%  filter(!(country_name_short %in% c("FRA", "AUT", "ITA", "HUN" )))
diag1.fe <-lm(turnout ~ pectotal_neu + 
                enep + disproportionality +
              pr + prXdisprop + plurality + 
              closeness + pluralityXcloseness +
                gdpe_growth + gdpe_pc + 
                factor(country_name_short), data=diagnostic.data1)

# Without countries with low volatility in pectotal
diagnostic.data2 <- diagnostic.data %>%  filter(!(country_name_short %in% c("CAN", "MLT", "CYP", "TUR" )))
diag2.fe <-lm(turnout ~ pectotal_neu + 
                enep + disproportionality +
                pr + prXdisprop + plurality + 
                closeness + pluralityXcloseness +
                gdpe_growth + gdpe_pc + 
                factor(country_name_short), data=diagnostic.data2)

# Without misleading eastern European election
diagnostic.data3 <- diagnostic.data %>%  mutate(east_europe = 0) %>% 
  mutate( east_europe = ifelse( country_name_short %in% c("CZE", "BGR", "EST", "HRV", 'HUN',
                                            "LTU", "LVA", 'ROU', "SVK", 'SVN', 'POL'), 1, 0)) %>% 
  filter(east_europe != 1 | year >= 1998)

diag3.fe <-lm(turnout ~ pectotal_neu + 
                enep + disproportionality +
                pr + prXdisprop + plurality + 
                closeness + pluralityXcloseness +
                gdpe_growth + gdpe_pc + 
                factor(country_name_short), data=diagnostic.data3)


#######################
# Replication without problematic assumptions and electoral system dummies:
#######################

# Ohne Annahme, dass Closeness nur in Plurality-Systemen wirkt:
diag4.fe <-lm(turnout ~ pectotal_neu + 
                enep + 
                disproportionality +
                pr + 
                prXdisprop + # nur Kein pluralityXcloseness
                closeness + 
                gdpe_growth + gdpe_pc + 
                factor(country_name_short), data=diagnostic.data)
diag4b.fe <-lm(turnout ~ pec_neu + 
                enep + 
                disproportionality +
                pr + 
                prXdisprop + # nur Kein pluralityXcloseness
                closeness + 
                gdpe_growth + gdpe_pc + 
                factor(country_name_short), data=diagnostic.data)

# Annahme, dass Disproportionalität nur in PR Systemen wirkt:
diag5.fe <-lm(turnout ~ pectotal_neu + 
                enep + 
                disproportionality +
                closeness + 
                gdpe_growth + gdpe_pc + 
                factor(country_name_short), data=diagnostic.data)

diag5b.fe <-lm(turnout ~ pec_neu + 
                enep + 
                disproportionality +
                closeness + 
                gdpe_growth + gdpe_pc + 
                factor(country_name_short), data=diagnostic.data)

#############################################
# Tabelle 4: Robustheitschecks:
#############################################
var_names <- c("PEC-Stimmenanteil",
               "PEC-Dummy",
               "ENP",
               "Gallagher-Index",
               "PR-Dummy",
               "PR X Gallagher",
               "Mehrheitswahl-Dummy",
               "Closeness",
               "Mehrheitswahl X Closeness",
               "BIP-Wachstum",
               "BIP-PK (log.)")

stargazer(full.m4, diag1.fe, diag2.fe, diag3.fe, diag5.fe, diag5b.fe,
          column.labels   = c("Referenz","Einflussreiche Beobachtungen (2)-(4)", "Test der Annahmen (5)-(6)"),
          column.separate = c(1, 3, 2),
          covariate.labels = var_names,
          add.lines = list(c("Fixed-Effects?","Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"),
                           c("Gesamtsample?", "Ja", "Ohne AUT, FRA, ITA, HUN", "Ohne TUR, MLT, CAN, CYP",
                             "Ohne Osteuropa vor '98", "Ja", "Ja")),
          type="html", single.row = TRUE, omit.stat = c("rsq", "f"),
          dep.var.labels=c("Wahlbeteiligung"), omit = "factor", out="Tabelle4_final.html")





##########################
# Did not make it into the thesis:
##########################
# Plots for enhanced sample
##########################
p1 <- ggplot2::ggplot(data = enhanced.data, aes(x= country_name_short, y = turnout )) +
  geom_point(shape = 1) + ylim(50,100) +
  stat_summary(color="blue") +
  labs(x = "Land",
       y = "Wahlbeteiligung") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90))
p1
p2<- ggplot2::ggplot(data = enhanced.data, aes(x= year, y = turnout )) +
  geom_point(aes(colour = factor(country_id), shape = 1)) + scale_shape_identity() + 
  geom_smooth() + 
  ylim(50,100) +
  scale_color_hue(l=40, c=35) +
  guides(color=FALSE) +
  labs(x = "Jahr",
       y = "Wahlbeteiligung") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
##
p3 <- ggplot2::ggplot(data = enhanced.data, aes(x= country_name_short, y = pec_neu )) +
  geom_jitter(width = 0.2, height = 0.05, shape=1) +
  stat_summary( color = "blue") +
  labs(x = "Land",
       y = "PEC-Auftreten (Dummy)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90))

p4 <-  ggplot2::ggplot(data = enhanced.data,aes(x= year, y = pec_neu )) +
  scale_shape_identity() + 
  stat_summary( shape = 1, size = 0.3)+
  geom_smooth() + 
  guides(color=FALSE) +
  labs(x = "Jahr",
       y = "PEC-Auftreten (Dummy)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
##
p5 <-  ggplot2::ggplot(data = enhanced.data, aes(x= country_name_short, y = pectotal_neu )) +
  geom_jitter(width = 0.2, height = 0.05, shape=1) +
  stat_summary(color="blue") +
  labs(x = "Land",
       y = "PEC-Stimmenanteil") +
  theme_bw()  +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90))

p6 <-  ggplot2::ggplot(data = enhanced.data, aes(x= year, y = pectotal_neu )) +
  geom_jitter(width = 0.05, height = 0.01, shape=1, aes(color = factor(country_id))) +
  geom_smooth() + 
  scale_color_hue(l=40, c=35) +
  guides(color=FALSE) +
  labs(x = "Jahr",
       y = "PEC-Stimmenanteil") +
  theme_bw( ) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)


