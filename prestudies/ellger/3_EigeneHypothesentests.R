# 3. Skript der Masterarbeit
# Ziel: Berechnung der neuen Variablen 
# Block-CLoseness und Effektive Anzahl d. Blöcke


rm(list=ls())

# install.packages(c("dplyr", "foreign", "haven", "ggplot2", "lubridate", "countrycode", "stargazer", 'plm', 'ggplot2', 'car')) # rest omitted
options(scipen=999)

library("dplyr")
library("foreign")
library("haven")
library('lubridate')
library ('countrycode')
library ('stargazer')
library('plm') 
library("gplots")
library("ggplot2")
library("car")
library("tidyr")


getwd

# Laden des erweiterten Datensatzes
data <- read.csv("enhanced_data.csv") # Erweiterter Datensatz: Aus den Replikationsanalysen

# Laden der ParlGov Daten (v.a. View Election Stable 15)
polarization.parlgov <- read.csv("parlgov_disprop.csv")
polarization.parlgov <- polarization.parlgov %>% select(election_id, polarization_vote, enp_votes, enp_seats)

election.parlgov <- read.csv("3_parlgov_election.csv")
election.parlgov <- election.parlgov %>% select("election_type", "vote_share", "seats","seats_total", 
                                                "left_right", "country_id",
                                                "election_id", "party_id") %>% mutate( seat_share = seats/seats_total,
                                                                                       vote_share = vote_share/100)

# Laden der PEC-Daten (jede Beobachtung entspr. einer Partei)
a <- read_dta("PEC_partylevel.dta")
names(a)
a <- a %>% select("election_id","party_id","country_name_short", "edate","eyear",
                  "pec1", "pec2", "pec3", "pec4", "pec5","pec6", "pec7", "pec8" )


# Merge ParlGov und party-level PEC Daten:
b <- right_join(a, election.parlgov, by = c("election_id", "party_id"))

b$pec1[is.na(b$pec1)] <- 0
b$pec2[is.na(b$pec2)] <- 0
b$pec3[is.na(b$pec3)] <- 0
b$pec4[is.na(b$pec4)] <- 0
b$pec5[is.na(b$pec5)] <- 0
b$pec6[is.na(b$pec6)] <- 0
b$pec7[is.na(b$pec7)] <- 0
b$pec8[is.na(b$pec8)] <- 0
b$seats[is.na(b$seats)] <- 0
b$vote_share[is.na(b$vote_share)] <- 0

# Umcodieren von CDU/CSU und Lib/Nat in keine PEC!
 b$pec1[b$country_name_short == "DEU" | b$country_name_short == "AUS"] <- 0
 
# Ausschluss von fehlerhaften Wahlen:
b <-  filter(b, election_id != 1001) # dropping measuerement error (Kroatian Election)
b <- filter(b, election_id != 158) # dropping Poland 91
b <- filter(b, election_id != 462) # dropping Lithuania 92


###################################
#### Calculating ENB by vote share:
c = b %>% 
  select(election_id, party_id, starts_with("pec"), vote_share) %>% 
  gather(pec, pec_member, -election_id, -party_id, -vote_share)


c2 = c %>% filter(pec_member == 1) %>% 
  group_by(election_id, pec) %>% 
  mutate(vote_share = ifelse(is.na(vote_share), 0, vote_share)) %>% 
  mutate(pec_vote_share = sum(vote_share)) %>%
  ungroup() %>%
  group_by(election_id, party_id) %>% 
  filter(pec_vote_share == max(pec_vote_share)) %>% 
  slice(1L) %>% 
  ungroup() %>% 
  group_by(election_id, pec) %>% 
  slice(1L) %>% 
  ungroup() %>% 
  bind_rows(c %>% group_by(election_id, party_id) %>% filter(sum(pec_member) == 0.0) %>% slice(1L) %>%
              ungroup() %>% mutate(pec = "single party", pec_member = 1, pec_vote_share = vote_share)) %>% 
  arrange(election_id, party_id, pec) %>% 
  select(-vote_share, -pec_member)

c3  <- c2 %>% group_by(election_id) %>% 
  mutate(election_vote_share = sum(pec_vote_share)) %>%
  mutate(pec_vote_share = pec_vote_share / sum(pec_vote_share)) %>%    # standardisieren!
  summarise( enb_vote = 1 / sum( (pec_vote_share)^2))  %>% ungroup()

c4 <- c3 %>% left_join( data, by = "election_id") %>% 
              select(election_id, election_date, country_name_short, enpp, enep, enb_vote)


###### Calculating ENB by seat-share:
d = b %>% 
  select(election_id, party_id, starts_with("pec"), seat_share) %>% 
  gather(pec, pec_member, -election_id, -party_id, -seat_share)


d2 = d %>% filter(pec_member == 1) %>% 
  group_by(election_id, pec) %>% 
    mutate(seat_share = ifelse(is.na(seat_share), 0, seat_share)) %>% 
    mutate(pec_seat_share = sum(seat_share)) %>%
  ungroup() %>%
  group_by(election_id, party_id) %>% 
    filter(pec_seat_share == max(pec_seat_share)) %>% 
    slice(1L) %>% 
  ungroup() %>% 
  group_by(election_id, pec) %>% 
    slice(1L) %>% 
  ungroup() %>% 
  bind_rows(d %>% group_by(election_id, party_id) %>% filter(sum(pec_member) == 0.0) %>% slice(1L) %>% ungroup() %>% mutate(pec = "single party", pec_member = 1, pec_seat_share = seat_share)) %>% 
  arrange(election_id, party_id, pec) %>% 
  select(-seat_share, -pec_member)
  
d3  <- d2 %>% group_by(election_id) %>% 
  mutate(election_seat_share = sum(pec_seat_share)) %>%
  mutate(pec_seat_share = pec_seat_share / sum(pec_seat_share)) %>% 
  summarise( enb_seat = 1 / sum( (pec_seat_share)^2))  %>% ungroup()

 d4 <- left_join(d3, data, by = "election_id")
 d5 <- d4  %>% select(election_id, election_date, country_name_short, enpp, enep, enb_seat)
 
 
#### Calculating the effective number of parties:
# ENP VOTES:
e <- b %>% group_by(election_id) %>%  
      summarise( enp_vote = 1 / sum((vote_share)^2 ) ) %>% ungroup()
e$election_id <- as.integer(e$election_id)
 
e2 <- left_join(e, data, by = "election_id")
 
e3 <- e2 %>% select(election_id, election_date, country_name_short, enp_vote, enep)

# ENP SEATS:
f <- b %>% group_by(election_id) %>%  
  summarise( enp_seat = 1 / sum((seat_share)^2 ) ) %>% ungroup()
f$election_id <- as.integer(f$election_id)

f2 <- left_join(f, data, by = "election_id")

f3 <- f2 %>% select(election_id, election_date, country_name_short, enp_seat, enpp)


#########################################
## Calculating Block Closeness
close.calc = b %>% 
  select(election_id, party_id, starts_with("pec"), vote_share) %>% 
  gather(pec, pec_member, -election_id, -party_id, -vote_share)

close.calc.2 = close.calc %>% filter(pec_member == 1) %>% 
  group_by(election_id, pec) %>% 
  mutate(vote_share = ifelse(is.na(vote_share), 0, vote_share)) %>% 
  mutate(pec_vote_share = sum(vote_share)) %>%
  ungroup() %>%
  group_by(election_id, party_id) %>% 
  filter(pec_vote_share == max(pec_vote_share)) %>% 
  slice(1L) %>% 
  ungroup() %>% 
  group_by(election_id, pec) %>% 
  slice(1L) %>% 
  ungroup() %>% 
  bind_rows(close.calc %>% group_by(election_id, party_id) %>% filter(sum(pec_member) == 0) %>% slice(1L) %>% ungroup() %>%
  mutate(pec = "single party", pec_member = 1, pec_vote_share = vote_share)) %>% 
  arrange(election_id, party_id, pec) %>% 
  select(-vote_share, -pec_member)

bloc.closeness  <- close.calc.2 %>% 
  arrange(election_id, desc(pec_vote_share)) %>% 
  group_by(election_id) %>% 
  mutate(pec_vote_share = pec_vote_share / sum(pec_vote_share)) %>% 
  mutate(bloc_closeness =   pec_vote_share - dplyr::lag(pec_vote_share) ) %>% 
  summarise(bloc_closeness = nth(bloc_closeness, 2))


### Calculating absolute number of parties
parties.abs <- election.parlgov %>% 
                   group_by(election_id) %>% 
                    mutate( n_parties = length(party_id)) %>% 
                    summarise( n_parties = first(n_parties)) %>%  ungroup()



########
# Done with Calculations
# Now Merging with original data

# joining: e;f; d3, c3, by election ID
enb.data.1 <- full_join(e, f, by= "election_id")
enb.data.2 <- full_join(enb.data.1, d3, by= "election_id")
enb.data <- full_join(enb.data.2, c3, by= "election_id")
close.data <- full_join(enb.data, bloc.closeness, by= "election_id")
full.data <- left_join(close.data, parties.abs, by = "election_id")

### final join with enhanced data from previous script:
data.2 <- left_join(data, full.data, by = "election_id")
data.3 <- data.2 %>% select(-enep, -enpp)

data.4 <- data.3 %>% mutate( delta_enb_seat = enp_seat - enb_seat,
                             delta_enb_vote = enp_vote - enb_vote, 
                             delta_closeness = closeness - bloc_closeness)
data.4$delta_enb_vote[data.4$delta_enb_vote <= 0] <- 0

data.4 <- data.4 %>% mutate(east_europe = 0) %>% 
                    mutate( east_europe = ifelse( country_name_short %in% c("CZE", "BGR", "EST", "HRV", 'HUN',
                                                                            "LTU", "LVA", 'ROU', "SVK", 'SVN', 'POL'), 1, 0))




######################
# Modelle der Tabellen 5 und 6

# M1
# H1 Vorwahlkoalitionen haben lediglich in Verhältniswahlsystemen einen positiven Effekt
#   auf die Wahlbeteiligung
data.4 <- data.4 %>% mutate(pectotal_neuXpr =  pectotal_neu*pr,
                            pec_neuXpr = pec_neu*pr)

h1.a <- lm(turnout ~ pectotal_neu + pr +
              pectotal_neuXpr +
              enp_vote +
              disproportionality +
              closeness +
              gdpe_growth + gdpe_pc  +
             year + 
             east_europe +
             compulsory,
            data=data.4)

h1.b <-  lm(turnout ~ pec_neu + pr +
                pec_neuXpr +
                enp_vote +
                disproportionality +
                closeness +
                gdpe_growth + gdpe_pc  +
                year + 
                east_europe +
                compulsory, data=data.4)

## M2 ##
# H2: Wenn die absolute Parteianzahl hoch (und damit besonders unübersichtlich) ist,
#     ist dann wirken Vorwahlkoalitionen stärker auf die Wahlbeteiligung.
data.4 <- data.4 %>% mutate(pectotal_neuXn_parties =  pectotal_neu*n_parties,
                            pec_neuXn_parties      =  pec_neu*n_parties)

h2.a <- lm(turnout ~ pectotal_neu + n_parties +
              pectotal_neuXn_parties +
              enp_vote +
              disproportionality +
              closeness +
              gdpe_growth + gdpe_pc  +
              factor(country_id) , data=data.4)

h2.b <- lm(turnout ~ pec_neu + n_parties +
              pec_neuXn_parties +
              enp_vote +
              disproportionality +
              closeness +
              gdpe_growth + gdpe_pc  +
              factor(country_id) , data=data.4)

# M3
# H3: Wenn durch Vorwahlkoalitionen die Identifiability stark zunimmt, dann steigt der Turnout
# m3.fe <- lm(turnout ~ delta_enb_vote + 
#               disproportionality +
#               closeness +
#               gdpe_growth + gdpe_pc  +
#               factor(country_id) , data=data.4)
# summary(m3.fe)
# 
# m3.ols <- lm(turnout ~ delta_enb_vote +
#                pr +
#                disproportionality +
#                closeness +
#                gdpe_growth + gdpe_pc  +
#                east_europe +
#                year+
#                compulsory, data=data.4)
# summary(m3.ols) 

### enb + enp
h3.a <- lm(turnout ~ enb_vote +
               enp_vote +
               disproportionality +
               closeness +
               gdpe_growth + gdpe_pc  +
              factor(country_id)  , data=data.4)

h3.b <- lm(turnout ~ enb_vote +
              enp_vote +
              pr +
              disproportionality +
              closeness +
              gdpe_growth + gdpe_pc  +
               east_europe +
              year + 
              compulsory  , data=data.4)


##### Decisiveness #####
# H4: Geringere Block-Closeness führt zu höherer Wahlbeteiligung
 
h4.a <- lm(turnout ~  bloc_closeness +
             closeness +
              enp_vote + 
              disproportionality +
              gdpe_growth + gdpe_pc  +
              factor(country_id) , data=data.4)

h4.b <- lm(turnout ~  bloc_closeness +
             closeness + 
               enp_vote + 
               disproportionality +
               gdpe_growth + gdpe_pc  +
             pr +
               east_europe +
               year +
               compulsory,data=data.4)


##########
# Kitchen-Sink/ Kombinierte Modelle (Tabelle 6 M4+M5):

comb.1 <- lm(turnout ~ bloc_closeness +
               enb_vote +
               closeness +
              enp_vote + 
              disproportionality +
              gdpe_growth + gdpe_pc  +
              factor(country_id) , data=data.4)

comb.2 <- lm(turnout ~ bloc_closeness +
               closeness +
               enb_vote +
               enp_vote + 
               disproportionality +
               gdpe_growth + gdpe_pc  +
               east_europe +
               year + 
               compulsory,
             data=data.4)

##############################
#### Tillmans Modell (Tabelle 6 M5):
data.4 <- data.4 %>% mutate(pluralityXcloseness = plurality *closeness,
                            prXdisprop = pr * disproportionality)

till.1 <-lm(turnout ~ bloc_closeness +
                    enb_vote +
                     pec_neu +
                    enp_vote + 
                     gdpe_growth +
                   gdpe_pc  +  
                    disproportionality +
                    pr +
                    prXdisprop +
                    plurality + 
                    closeness + 
                    pluralityXcloseness +
                    factor(country_name_short), data=data.4)

####### Erstellen d. Tabelle:

# Tabelle 5
 var_names1 <- c("PEC-Stimmenanteil", 
                "PEC-Dummy",
                "PR-Dummy",
                "PEC-Stimmenanteil X PR",
                "PR-Dummy X PR",
                "Absolute Parteienzahl",
                "PEC-Stimmenanteil X absol. Parteienzahl",
                "PEC-Dummy X absol. Parteienzahl",
                "Effektive Blockzahl (ENB)",
                "Effektive Parteienzahl (ENP)",
                  "Disproportionalitaet",
                  "Closeness",
                "BIP-Wachstum",
                "BIP-PK (log.)",
                  "Wahljahr",
                  "Osteuropa-Dummy",
                  "Wahlpflicht-Dummy")


stargazer(h1.a, h1.b, h2.a, h2.b, h3.a, h3.b,
          column.labels   = c("Hypothese 1", "Hypothese 2", "Hypothese 3"),
          column.separate = c( 2,2,2),
          covariate.labels = var_names1,
          add.lines = list(c("Fixed effects?", "Nein", "Nein", "Ja", "Ja", "Ja", "Nein")),
          single.row = T, omit.stat = c("rsq", "f"), type = "html",
          dep.var.labels=c("Wahlbeteiligung"), omit = "factor" , out="Tabelle5.html")

# Tabelle 6:
var_names2 <- c("Block-Closeness", 
                "Effektive Blockzahl (ENB)",
                "PEC-Dummy",
                "Closeness",
                "Mehreitswahl X Closeness",
                "Effektive Parteienzahl (ENP)",
                "Disproportionalitaet",
                "BIP-Wachstum",
                "BIP-PK (log.)",
                "PR-Dummy",
                "Osteuropa-Dummy",
                "Wahljahr",
                "Wahlpflicht-Dummy",
                "PR X Disprop",
                "Mehrheitswahl-Dummy")

stargazer(h4.a, h4.b, comb.1, comb.2, till.1, 
          column.labels   = c("Hypothese 4", "Kombiniertes Modell", "Tillman-Modell"),
          column.separate = c( 2,2,1),
          covariate.labels = var_names2,
          single.row = T, omit.stat = c("rsq", "f"), type = "html",
          dep.var.labels=c("Wahlbeteiligung"), omit = "factor" ,
          add.lines = list(c("Fixed effects?", "Ja", "Nein", "Ja", "Nein", "Ja")),
          out="Tabelle6.html")


