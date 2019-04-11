use view_election_stable15, clear

merge 1:1 election_id party_id using PEC_all, keepusing(pec1 pec2 pec3 pec4 pec5 pec6 pec7 pec8)

gsort election_id - country_name_short
replace country_name_short = country_name_short[_n-1] if country_name_short=="" & election_id==election_id[_n-1]

replace pec1 = 0 if country_name_short=="DEU" //CDU/CSU PEC wollen wir nicht als PEC zählen
replace pec1 = 0 if country_name_short=="AUS" //Lib-Nat sollen nicht als PEC gezählt werden
*drop if 

foreach var of varlist pec* {
replace `var'=0 if `var'==.
}

*keep if election_id==64
*compress

egen pectotal_neu = rowtotal(pec*)
replace pectotal_neu=1 if pectotal_neu>1 & pectotal_neu!=.
replace pectotal_neu = vs1*pectotal_neu

forval i=1/8 {
bysort election_id: egen pecnum`i' = max(pec`i') 
}
egen nupec_neu = rowtotal(pecnum*)
drop pecnum*

**Identifizierbarket der Parties
gsort election_id - vs1
by election_id: gen n=_n
gen twobiggest_parties = vs1 if n==1 | n==2

collapse (first) country_name* country_id election_date nupec_neu (sum) pectotal_neu twobiggest_parties, by(election_id)
merge 1:1 election_id using data_blocvotes, keepusing(blocvotes) keep(1 3) nogen
gen additional_identifiability = blocvotes - twobiggest_parties*100 if twobiggest_parties!=0 & blocvotes!=0
replace additional_identifiability = 0 if additional_identifiability >-0.0001 & additional_identifiability<0.0001

gen pec_neu = pectotal_neu>0 & pectotal_neu!=.
gen pec10_neu = pectotal_neu>.1 & pectotal_neu!=.
gen pec20_neu = pectotal_neu>.2 & pectotal_neu!=.
gen pec30_neu = pectotal_neu>.3 & pectotal_neu!=.

so country_name_s election_date
gen year = year(election_date)
gen year2 = year*10
replace year2 = year2+1 if year==1982 & country_name_s=="IRL" & month(election_date)==11
replace year2 = year2+1 if year==1974 & country_name_s=="GBR" & month(election_date)==10
replace year2 = year2+1 if year==1989 & country_name_s=="GRC" & month(election_date)==11
replace year2 = year2+1 if year2==year2[_n-1]
drop if year2==.
format election_date %d

saveold temp/PEC_LSVERGL, replace v(12)


use tilmann, clear
drop if country==""
replace country="New Zealand" if country=="NewZealand"
replace year = 2008 if year==2006 & country=="Spain" //evtl Tilmann Fehler??
kountry country, from(other) stuck
rename _IS cid
kountry cid, from(iso3n) to(iso3c)
rename _IS country_name_short

gen year2 = year*10
replace year2 = year2+1 if country_name_short=="IRL" & disprop>2 & year==1982
replace year2 = year2+1 if country_name_short=="GBR" & disprop<15 & year==1974
replace year2 = year2+1 if country_name_short=="GRC" & disprop<4 & year==1989

merge 1:1 country_name_short year2 using temp/PEC_LSVERGL
rename turnout turnout_tillman
merge 1:1 election_id using election_parameter, nogen keep(3 1)

drop _merge
merge 1:1 country_name_short year2 using Best_Zhirnov_erweitert9_LH , nogen keep(3 1)

encode country_name_short, gen(c)
order country country_name country_name_short c
sort country_name election_date
xtset c year2


saveold Tillman_LSVERGL, replace v(12)


exit
local control "enep disprop pr prXdisprop smd closeness smdXcloseness growth lnincome"
xtreg turnout_tillman pec1  `control' dm_eff polarization, fe cluster(id)

insheet using viewcalc_election_parameter.csv, delimiter(";") clear
save election_parameter, replace


browse if country_name_s=="DEU" & year(election_date)==1965

browse
