rm(list=ls())

# install.packages(c("dplyr", "foreign", "haven", "ggplot2", "lubridate", "countrycode")) # rest omitted
options(scipen=999)

# install.packages(c("dplyr", "foreign", "haven", "ggplot2", "lubridate", "countrycode")) # rest omitted

library("dplyr")
library("foreign")
library("haven")
library('lubridate')
library ('countrycode')
library ('stargazer')
library('plm') 
library("gplots")
library('estimatr')
library("ggplot2")
library("gridExtra")
library("readxl")
library("RCurl")
library("sandwich")
library("car")


# Check Working Directory
getwd()

### Load Tillman-PEC Data
# Information on cabinets and if they resulted/ originated from a PEC (Dummy)
till.data <- read_dta("tilmann_merger.dta") # Originaldaten (nur ein paar changes in seb.do-file)

## Countries in dataset
length(unique(till.data$country))
length(unique(till.data$year))

### Variable Overview:
 # Turnout            > turnout
 # PEC                > 1 (after German recode of CDU/CSU)
 # PEC>20%            > pec20
 # PEC-Voteshare      > vote_pec
 # Small PEC          > smallpec
 # Large PEC          > largepec
 # Parties            > enep 
 # PR                 > pr
 # Disproportionality > disprop
 # Plurality          > plurality
 # Closeness          > closeness
 # Economic Growth    > growth
 # Income             > lnincome

########################
## Creating "Abbildung 2"
p1 <- ggplot2::ggplot(data = till.data, aes(x= country_name_short, y = turnout )) +
  geom_point(shape = 1) + ylim(50,100) +
  stat_summary(color="blue") +
  labs(x = "Land",
       y = "Wahlbeteiligung") +
  theme_bw() +
  theme(axis.text=element_text(size=12, angle = 90),
        axis.title=element_text(size=14,face="bold"))

p2 <- ggplot2::ggplot(data = till.data, aes(x= year, y = turnout )) +
  geom_point(aes(colour = factor(id), shape = 1)) + scale_shape_identity() + 
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
p3b <- ggplot2::ggplot(data = till.data, aes(x= country_name_short, y = pec1 )) +
  geom_jitter(width = 0.2, height = 0.05, shape=1) +
  stat_summary( color = "blue") +
  labs(x = "Land",
       y = "PEC-Auftreten (Dummy)") +
  theme_bw() +
  theme(axis.text=element_text(size=12, angle = 90),
        axis.title=element_text(size=14,face="bold"))

p4b <- ggplot2::ggplot(data = till.data,aes(x= year, y = pec1 )) +
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
p5b <- ggplot2::ggplot(data = till.data, aes(x= country_name_short, y = vote_pec )) +
  geom_jitter(width = 0.2, height = 0.05, shape=1) +
  stat_summary(color="blue") +
  labs(x = "Land",
       y = "PEC-Stimmenanteil") +
  theme_bw()  +
  theme(axis.text=element_text(size=14, angle = 90),
        axis.title=element_text(size=14,face="bold"))


p6b <- ggplot2::ggplot(data = till.data, aes(x= year, y = turnout )) +
  geom_point(aes(colour = factor(id), shape = 1)) + scale_shape_identity() + 
  geom_smooth() + 
  scale_color_hue(l=40, c=35) +
  guides(color=FALSE) +
  labs(x = "Jahr",
       y = "PEC-Stimmenanteil") +
  theme_bw( ) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))

grid.arrange(p1, p2, p3b, p4b, p5b, p6b, ncol = 2) # Abbildung 2





#################################################################
# Replication:
till.data <- till.data %>% mutate(pluralityXcloseness = pr * closeness)

### Replikation der Modelle aus d. Artikel ###
##M1: 
orig.1 <-lm(turnout ~ enep + disprop +
             pr + prXdisprop + plurality + 
             closeness + pluralityXcloseness +
             growth + lnincome  + factor(id) , data=till.data)

summary(orig.1)
nobs(orig.1) # number of obs fits

orig.2 <-lm(turnout ~ pec1 + enep  + disprop +
             pr + prXdisprop + plurality + 
             closeness + pluralityXcloseness +
             growth + lnincome  + factor(id) , data=till.data)
summary(orig.2)
nobs(orig.2)


orig.4 <-lm(turnout ~ vote_pec + enep  + disprop +
             pr + prXdisprop + plurality + 
             closeness + pluralityXcloseness +
             growth + lnincome  + factor(id) , data=till.data)
summary(orig.4)
nobs(orig.4)




##############################################
##### Enhancing the dataset:
# DFG Data
new.pec <- read.csv("PEC_DFG.csv")
new.pec <- new.pec %>% filter(election_id != 462 & election_id != 158)

###### Golder Data
# Importing Matt Golder data on electoral systems
golder.data <-  read.csv( "golder_data-v3.csv" )
golder.data$country <- as.character(golder.data$country)

# Small fixes for merging
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

# Changing wrong values befor the merge
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

######## Parlgov Stats, Closeness and Transformation (CDU-CSU and LIB-Nat)
parlgov.stats <- read.csv("parlgov_disprop.csv")
parlgov.subset <- parlgov.stats %>% 
  select(election_id, disproportionality, turnout, enp_votes, enp_seats, polarization_vote)
parlgov.pec.joint <-  left_join(merge.pec.golder, parlgov.subset, by = c("election_id"))

### Loading Parlgov View Election Stable 15
parlgov.election.15 <- read.csv("view_election_stable15.csv")
parlgov.election.15 <- parlgov.election.15 %>%  filter(! is.na(vote_share))  %>%  filter(election_type == "parliament")

# Merging Cdu/Csu and Lib/Nat to one party
parlgov.election.15 <- parlgov.election.15 %>% group_by(election_id) %>%
  mutate(vote_share = ifelse(party_id == 808, (vote_share[party_id==1180] + vote_share[party_id==808]), vote_share),
         seats =    ifelse(party_id == 808, (seats[party_id==1180] + seats[party_id==808]), seats)   ) %>%
  filter(party_id != 1180) %>%
  mutate(vote_share = ifelse(party_id == 1411, (vote_share[party_id==1411] + vote_share[party_id==184]), vote_share),
         seats =    ifelse(party_id == 1411, (seats[party_id==1411] + seats[party_id==184]), seats)   ) %>%
  filter(party_id != 184)  %>% ungroup()

# Save adaptions of parlgov
write.csv(parlgov.election.15, "3_parlgov_election.csv")


#### Closeness calculation 
closeness <- parlgov.election.15  %>%  group_by(election_id)  %>%
  mutate(vote_share = vote_share / sum(vote_share)) %>% 
  arrange(desc(vote_share)) %>%
  mutate(closeness = vote_share - dplyr::lag(vote_share)) %>% 
  summarise(closeness = nth(closeness,2))

closeness.added <-  left_join(parlgov.pec.joint, closeness, by = c("election_id"))
closeness.added2 <-  inner_join(parlgov.pec.joint, closeness, by = c("election_id"))
#### Finished ParlGOV

#### Economic Variables from PENN World Table
penn.econ <- read_excel("pwt90.xlsx", sheet= 3)
names(penn.econ)
penn.econ.gdp <- penn.econ %>%  mutate(gdpe_pc = rgdpe/pop, gdpo_pc = rgdpo/pop) %>%
  group_by(country) %>%
  mutate(gdpe_growth =  rgdpe - dplyr::lag(rgdpe)) %>%
  mutate(gdpo_growth =  rgdpo - dplyr::lag(rgdpo)) %>%
  ungroup(country) %>%
  select( -c(country, pop, rgdpe, rgdpo))
penngdp.added <- left_join(closeness.added, penn.econ.gdp, by = c("country_name_short" = "iso3c", "year")) # problematic: Worldbank data starts at 1970s for some countries

### Adding it all together
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



#######################
## This is the full enhanced dataset with 
## all necessary controls for replication:

enhanced.data <- final.variables2 %>% 
  select(c(election_id, country_id, country_name, country_name_short, 
           election_date, year, turnout, compulsory,starts_with("pec"),
           enep, enpp, pr, disproportionality, prXdisprop,
           plurality, closeness, pluralityXcloseness, gdpe_growth, gdpe_pc, majoritarian, majoritarianXcloseness)) %>% 
  rename( lnincome = gdpe_pc,
           growth = gdpe_growth)

till.data <- till.data %>% rename(pec_neu = pec1,
                                  pectotal_neu = vote_pec,
                                  disproportionality = disprop)

############################################
### REPLICATION
#Reestimation on enhanced data

full.m1 <-lm(turnout ~ enep + disproportionality +
               pr + prXdisprop + plurality + 
               closeness + pluralityXcloseness +
               growth + lnincome  + factor(country_id) , data= enhanced.data)

# M2
full.m2 <-lm(turnout ~ pec_neu + enep + disproportionality +
               pr + prXdisprop + plurality + 
               closeness + pluralityXcloseness +
               growth + lnincome  + factor(country_id) , data=enhanced.data)

# M4
full.m4 <-lm(turnout ~ pectotal_neu + enep + disproportionality +
               pr + prXdisprop + plurality + 
               closeness + pluralityXcloseness +
               growth + lnincome  + factor(country_name_short) , data=enhanced.data)


# Original Models
orig.1 <-lm(turnout ~ enep + disproportionality +
              pr + prXdisprop + plurality + 
              closeness + pluralityXcloseness +
              growth + lnincome  + factor(id) , data=till.data)

orig.2 <-lm(turnout ~ pec_neu + enep  + disproportionality +
              pr + prXdisprop + plurality + 
              closeness + pluralityXcloseness +
              growth + lnincome  + factor(id) , data=till.data)

orig.4 <-lm(turnout ~ pectotal_neu + enep  + disproportionality +
              pr + prXdisprop + plurality + 
              closeness + pluralityXcloseness +
              growth + lnincome  + factor(id) , data=till.data)




##### TABELLE 2 DER ARBEIT (1. Regression) 
# Vergleicht ursprüngliche Modelle (inkl. Baseline) mit Daten zu unserer Replikation
var_names <- c("PEC-Dummy", 
               "PEC-Stimmenanteil",
               "ENP",
               "Gallagher-Index",
               "PR-Dummy",
               "PR X Gallagher",
               "Mehrheitswahl-Dummy",
               "Closeness",
               "Mehrheitswahl X Closeness",
               "BIP-Wachstum",
               "BIP-PK (log.)")

stargazer(orig.1, orig.2, orig.4, 
          full.m1, full.m2, full.m4,
          column.labels   = c("Replikationsdaten (1) - (3)", "Erweiterte Daten (4) - (6)"),
          column.separate = c(3, 6),
          covariate.labels = var_names,
          add.lines = list(c("Fixed effects?", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")),
          type="html", single.row = TRUE, omit.stat = c("rsq", "f"),
          dep.var.labels=c("Wahlbeteiligung"), omit = "factor", out="Tabelle_2.html")



till.summary <- till.data %>% group_by(country) %>% 
                              summarise (min_year = min(year),
                                         max_year = max(year))
write.csv(till.summary, "tillman_dataoverview.csv")

enhanced.summary <- enhanced.data %>% group_by(country_name) %>% 
  summarise (min_year = min(year),
             max_year = max(year))
write.csv(enhanced.summary, "enhanced_dataoverview.csv")





# ######## CODE DER NICHT GENUTZT WIRD:
# # U.A. Hausmann-Test; Random-Effects, etc.
# 
# 
# #### Expanding the scope: Cross country variance.
# # hausman test: fixed vs random effects
#  re.m1 <- plm(turnout ~ enep + disprop +
#                 pr + prXdisprop + plurality + 
#                 closeness + pluralityXcloseness +
#                 growth + lnincome, data=till.data, index=c("id"), model="random")
#  summary(re.m1)
#  
#  re.m2 <- plm(turnout ~ pec1 + enep + disprop +
#                 pr + prXdisprop + plurality + 
#                 closeness + pluralityXcloseness +
#                 growth + lnincome, data=till.data, index=c("id"), model="random")
#  summary(re.m2)
#  
#  # null hypothesis is that the preferred model is random effects vs. the alternative the fixed           effects (see Green, 2008, chapter 9). It basically tests whether the unique errors (ui) are          correlated with the regressors, the null hypothesis is they are not.
#  # have to specify fixed effects within plm package (as plm type)
# fe.m1.test <- plm(turnout ~ enep + disprop +
#                pr + prXdisprop + plurality + 
#                closeness + pluralityXcloseness +
#                growth + lnincome, data=till.data, index=c("id"), model="within")
# fe.m2.test <- plm(turnout ~ pec1 + enep + disprop +
#                pr + prXdisprop + plurality + 
#                closeness + pluralityXcloseness +
#                growth + lnincome, data=till.data, index=c("id"), model="within")
# 
# summary(fe.m1.test) 
# summary(fe.m2.test)#works, identical results
# 
# 
# phtest(fe.m1.test, re.m1) # use fixed effects
# phtest(fe.m2.test, re.m2) # use random effects
# 
# ### 2nd step: Estimate cross country for every year
# # Checking for cross country variance
# m1.yearfix <- plm(turnout ~ enep + disprop +
#                     pr + prXdisprop + plurality + 
#                     closeness + pluralityXcloseness +
#                     growth + lnincome, data=till.data, index=c("year"), model="within")
# summary(m1.yearfix)
# m2.yearfix <- plm(turnout ~ pec1 + enep + disprop +
#                     pr + prXdisprop + plurality + 
#                     closeness + pluralityXcloseness +
#                     growth + lnincome, data=till.data, index=c("year"), model="within")
# summary(m2.yearfix)
# m4.yearfix <- plm(turnout ~ vote_pec + enep + disprop +
#                     pr + prXdisprop + plurality + 
#                     closeness + pluralityXcloseness +
#                     growth + lnincome, data=till.data, index=c("year"), model="within")
# summary(m4.yearfix)
# 
# #####################
# ### Alternative specifictaion: OLS with White-clustered SE's (too weak) ######
# # and year variable to control for time
# m1.ols <- lm(turnout ~ enep + disprop +
#                 pr + prXdisprop + plurality + 
#                 closeness + pluralityXcloseness +
#                 growth + lnincome + compulsory + year, data=merge.clean)
# summary(m1.ols)
# 
# 
# m2.ols <- lm(turnout ~ pec1 + enep + disprop +
#                pr + prXdisprop + plurality + 
#                closeness + pluralityXcloseness +
#                growth + lnincome + compulsory + year, data=merge.clean)
# summary(m2.ols)
# 
# 
# m4.ols <- lm(turnout ~ vote_pec + enep + disprop +
#                pr + prXdisprop + plurality + 
#                closeness + pluralityXcloseness +
#                growth + lnincome + compulsory + year, data=merge.clean)
# summary(m4.ols)
# 
# # load necessary packages for importing the function
# library(RCurl)
# # load necessary packages for the example
# library(gdata) 
# library(zoo)
# 
# # import the function
# url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
# eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
#      envir=.GlobalEnv)
# 
# 
# summary(m1.ols, cluster = c("id"))# Clustered Standard errors
# summary(m2.ols, cluster = c("id")) # Clustered Standard errors
# summary(m4.ols, cluster = c("id")) # Clustered Standard errors
# 
# # save cluster robust standard errors
# cluster_m1 <- as.vector(summary(m1.ols,cluster = c("id"))$coefficients[,"Std. Error"])
# cluster_m2 <- as.vector(summary(m2.ols,cluster = c("id"))$coefficients[,"Std. Error"])
# cluster_m4 <- as.vector(summary(m4.ols,cluster = c("id"))$coefficients[,"Std. Error"])
# 
# # print stargazer output with robust standard errors
# stargazer(m1.ols, m2.ols, m4.ols, 
#           se = list(cluster_m1, cluster_m2, cluster_m4),
#           single.row = TRUE, omit.stat = c("rsq", "f"), type = "html",
#           dep.var.labels=c("Turnout"), out="cluster.html")
