
*Open the Parlgov Stable 2015 file (election version)
use source_files/view_election_stable15, clear
drop if election_type=="ep"
drop election_type

gen seat_share = seats/seats_total
replace vote_share = vote_share/100

*Merge the PEC data into it using the election_id
merge 1:1 election_id party_id using PEC_all, keepusing(pec*) nogen //(pec1 pec2 pec3 pec4 pec5 pec6 pec7 pec8 pec1_incumbent pec1_ea pec2_incumbent pec3_incumbent pec4_incumbent pec5_incumbent pec6_incumbent pec7_incumbent pec8_incumbent)

gen k=0
local PECarticle = "AUS AUT BEL CAN CHE CZE DEU DNK ESP FIN FRA GBR GRC ITA ISR ISL IRL JPN LUX NOR NLD PRT NZL SWE BGR HRV SVN SVK POL EST LTU LVA HUN ROU MLT"
foreach c in `PECarticle' {
	replace k = 1 if country_name_short=="`c'"
	}
keep if k
drop k

saveold PEC_raw_stable15_2, version(12) replace

**we dont use this anymore
forval i=1/7 {
	drop pec`i'_string
	}

*zero missing PECs [important for the later rowsumming]
forval i=1/7 {
	replace pec`i'=0 if pec`i'==.
	}

*fill in missing election_id and country names (those of merge=using only)
gsort election_id - country_name_short
replace country_name_short = country_name_short[_n-1] if country_name_short=="" & election_id==election_id[_n-1]

*we want to count PECs as analog as possible to Tillman, who does not count CDU/CSU and Lib/NAt as respective PECS
*Therefore, we merge those "coalitions" to single_parties
*First German Union
    gen collapser = 111111 if party_name_short=="CDU"
replace collapser = 111111 if party_name_short=="CSU"
*Then Australian Lib/Nat
replace collapser = 222222 if party_name_short=="LPA"
replace collapser = 222222 if party_name_short=="NCP|NPA"

replace collapser = _n if collapser == .
count
collapse (first) country_name_short country_name election_date seats_total party_name* country_id party_id ///
           (sum) vote_share seats seat_share ///
		   (mean) left_right pec*  ,by(collapser election_id)
count

foreach string in party_name_short party_name party_name_english {
	replace `string' = "CDU/CSU" if party_name_short=="CDU" | party_name_short=="CSU"
	replace `string' = "Lib/Nat" if party_name_short=="LPA" | party_name_short=="NCP|NPA"
	}
replace party_id = 111111 if party_name_short=="CDU/CSU" 
replace party_id = 222222 if party_name_short=="Lib/Nat" 

*Once, in 1955, the LibNats had no PEC (following our data)
replace pec2 = 1 if party_name_short=="Lib/Nat" & year(date(election_date,"YMD"))==1955

*Until here we merged both parties. If we want to get rid of their respective pecs, we need to set them 0
replace pec1 = 0 if country_name_short=="DEU" //CDU/CSU PEC wollen wir nicht als PEC zählen
replace pec1 = 0 if country_name_short=="AUS" //Lib-Nat sollen nicht als PEC gezählt werden
*replace pec2 = 0 if country_name_short=="AUS" & year(date(election_date,"YMD"))>= 1975 Achtung... there are a number of PECS the Lib/Nats have with other parties, which are nothing but regional versions of themselves... we should set them 0 too!

*************************START TO BUILD INDICATORS

*PECTOTAL_NEU should sum up the number as well as percentage of PECS in any election
*PECTOTAL_INC and PECTOTAL_OTHER should give the percentage of PECS that are incumbent as well as those that do not involve incumbents

forval i=1/7 {
rename pec`i' pecneu`i' //*this renaming is just helping later
gen pecinc`i' = pecneu`i'*pec`i'_incumbent //*only PECs with incumbent==1 stay 1
gen pec`i'_rest = pec`i'_incumbent==0 
gen pecother`i' = pecneu`i'*pec`i'_rest //*only PECs with incumbent==0 stay 1
}

foreach var in neu inc other {
egen pectotal_`var' = rowtotal(pec`var'*) //*number of PECs (later incumbentPECs .. otherPECS) for each party in each election
replace pectotal_`var'= 1 if pectotal_`var'>1 & pectotal_`var' !=. //*transform the number to a dummy, 0 = no pec, 1=at least one pec
replace pectotal_`var' = vote_share*pectotal_`var' //*vote share of party instead of the 1, 0 if not in Pec (can be summed later for every election!)
}

**NUPEC_NEU gives the number of PECs for each election
forval i=1/7 {
bysort election_id: egen pecnum`i' = max(pecneu`i')  // checks each column for its max value, is 1 if there is a PEC
}
egen nupec_neu = rowtotal(pecnum*) //totalling the columns gives us the number of PECs
drop pecnum* //was just a temp variable

**Identifizierbarket der Parties (votes of biggest 2 parties)
gsort election_id - seat_share  //sort biggest 2 show up on top
by election_id: gen n=_n //just number cases for each election
gen twobiggest_parties = seat_share if n==1 | n==2 //write vote share only for the biggest two parties (will be summed in collapse)

**Collapse the Elections, taking several sums
collapse (first) country_name* country_id election_date nupec_neu (sum) pectotal_neu pectotal_inc pectotal_other twobiggest_parties, by(election_id)


**here we just build some handy dummies for later analysis
foreach var in neu inc other {
gen pec_`var' = pectotal_`var'>0 & pectotal_`var'!=.
gen pec10_`var' = pectotal_`var'>.1 & pectotal_`var'!=.
gen pec20_`var' = pectotal_`var'>.2 & pectotal_`var'!=.
gen pec30_`var' = pectotal_`var'>.3 & pectotal_`var'!=.
}

**************************************************************************merge with Tilmann
*merging with Tilman is complicated because we need a identifier variable  
*reason: his election dates are not unique because they are just years and some countries voted twice per year
*i choose to build a new ID 
so country_name_s election_date
gen year = year(date(election_date,"YMD"))
gen year2 = year*10
**there are 3 countries/cases with 2 elections per year
replace year2 = year2+1 if year==1982 & country_name_s=="IRL" & month(date(election_date, "YMD"))==11
replace year2 = year2+1 if year==1974 & country_name_s=="GBR" & month(date(election_date, "YMD"))==10
replace year2 = year2+1 if year==1989 & country_name_s=="GRC" & month(date(election_date, "YMD"))==11
replace year2 = year2+1 if year2==year2[_n-1]
drop if year2==.

*i put this data on the side to work on the Tilmann data set before i merge both
*saveold temp/PEC_LSVERGL, replace v(12)


/*Tilmann dataset manipulation
use source_files/tilmann, clear
drop if country==""
replace country="New Zealand" if country=="NewZealand"

*these 4 lines are just to build country codes:
kountry country, from(other) stuck
rename _IS cid
kountry cid, from(iso3n) to(iso3c)
rename _IS country_name_short


*again, working on the identifier
replace year = 2008 if year==2006 & country=="Spain" //evtl Tilmann data error??

gen year2 = year*10
replace year2 = year2+1 if country_name_short=="IRL" & disprop>2 & year==1982
replace year2 = year2+1 if country_name_short=="GBR" & disprop<15 & year==1974
replace year2 = year2+1 if country_name_short=="GRC" & disprop<4 & year==1989

*Finally, merging both datasets
merge 1:1 country_name_short year2 using temp/PEC_LSVERGL, gen(merge_Tilmann)
rename turnout turnout_tillman
*/

************************************FURTHER MERGES

**merge our blocvotes data (this is the percentage of votes for the biggest 2 blocs as opposed to parties, its calculated in another do-file because it involves collapsing te dataset 8 times)
merge 1:1 election_id using source_files/data_blocvotes, keepusing(blocvotes) keep(1 3) nogen
*with our blocvote data, we can compare blocvotes with the vote share of the 2 biggest parties!
gen additional_identifiability = blocvotes - twobiggest_parties*100 if twobiggest_parties!=0 & blocvotes!=0
replace additional_identifiability = 0 if additional_identifiability >-0.0001 & additional_identifiability<0.0001

*now we also merge PARLGOV data on elections
merge 1:1 election_id using source_files/election_parameter, nogen keep(3 1)

*now we merge our data on effective thresholds / electoral system proportionality
merge 1:1 country_name_short year2 using source_files/Best_Zhirnov_erweitert9_LH , nogen keep(3 1) keepusing(dm_eff)

*cleaning the dataset
encode country_name_short, gen(c) //lets have a numeric country variable
order  country_name country_name_short c //put country variables up front
sort country_name election_date //sort it
xtset c year2 //set it as panel/time-series data, gives the option for some easy graphics


saveold PEC_LSVERGL_2, replace v(12)

exit

