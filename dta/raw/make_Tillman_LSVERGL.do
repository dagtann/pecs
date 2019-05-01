cd "~/github/pecs/dta/raw"
use view_election_stable15_v12, clear

merge 1:1 election_id party_id using PEC_all_v12, ///
	keepusing( ///
		pec1 pec2 pec3 pec4 pec5 pec6 pec7 pec8 ///
		*_type *_progr *_incumbent ///
	)
drop pec1_unsicher_type

* Process incumbent information
capture: generate pec8_incumbent = 0 if pec8!=.
capture: destring pec*_incumbent, replace
replace pec1_incumbent = . if pec1_incumbent == 9
// see documentation, should be recoded in original files

* Fill country information
gsort election_id - country_name_short
replace country_name_short = country_name_short[_n-1] ///
	if country_name_short == "" & election_id == election_id[_n-1]

* special codings
replace pec1 = 0 if country_name_short=="DEU" //CDU/CSU PEC wollen wir nicht als PEC zählen
replace pec1 = 0 if country_name_short=="AUS" //Lib-Nat sollen nicht als PEC gezählt werden

* pec strength
foreach var of varlist pec* {
	replace `var' = 0 if `var' == .
}
egen pectotal_neu = rowtotal(pec*)
replace pectotal_neu=1 if pectotal_neu > 1 & pectotal_neu != .
replace pectotal_neu = vs1 * pectotal_neu
forval i=1/8 {
	gen pecinc`i' = pec`i'*pec`i'_incumbent
	gen pec`i'_rest = pec`i'_incumbent==0
	gen pecother`i' = pec`i'*pec`i'_rest
}
egen pectotal_inc = rowtotal(pecinc*)
egen pectotal_other = rowtotal(pecother*)
replace pectotal_inc= 1 if pectotal_inc > 1 & pectotal_inc != .
replace pectotal_other = 1 if pectotal_other > 1 & pectotal_other != .
replace pectotal_inc = vs1 * pectotal_inc
replace pectotal_other = vs1 * pectotal_other

* pec number
forval i = 1/8 {
	bysort election_id: egen pecnum`i' = max(pec`i') 
}
egen nupec_neu = rowtotal(pecnum*)
drop pecnum*

* pec type
forval v = 1/6 {
	egen any_type`v' = anymatch(pec*_type), values(`v')
}
egen any_progr = anymatch(pec*_progr), values(1)
egen any_incumbent = anymatch(pec*_incumbent), values(1)

**Identifizierbarkeit der Parties
gsort election_id - vs1
by election_id: gen n = _n
gen twobiggest_parties = vs1 if n == 1 | n == 2

#delim ;
collapse
    (first) country_name* country_id election_date nupec_neu any_*
    (sum) pectotal_neu pectotal_inc pectotal_other twobiggest_parties,
    by(election_id)
;
merge 1:1 election_id using data_blocvotes_v12,
	keepusing(blocvotes) keep(1 3) nogen
;
gen additional_identifiability = blocvotes - twobiggest_parties * 100 if
	twobiggest_parties != 0 & blocvotes != 0
;
replace additional_identifiability = 0 if
	additional_identifiability > -0.0001 & additional_identifiability < 0.0001
;
#delim cr

foreach var in neu inc other {
	gen pec_`var' = pectotal_`var'>0 & pectotal_`var'!=.
	gen pec10_`var' = pectotal_`var'>.1 & pectotal_`var'!=.
	gen pec20_`var' = pectotal_`var'>.2 & pectotal_`var'!=.
	gen pec30_`var' = pectotal_`var'>.3 & pectotal_`var'!=.
}

so country_name_s election_date
gen year = year(election_date)
gen year2 = year*10
replace year2 = year2+1 if year==1982 & country_name_s=="IRL" & month(election_date)==11
replace year2 = year2+1 if year==1974 & country_name_s=="GBR" & month(election_date)==10
replace year2 = year2+1 if year==1989 & country_name_s=="GRC" & month(election_date)==11
replace year2 = year2+1 if year2==year2[_n-1]
drop if year2==.
format election_date %d

save PEC_LSVERGL, replace

use tilmann_v12, clear
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

merge 1:1 country_name_short year2 using PEC_LSVERGL
rename turnout turnout_tillman
merge 1:1 election_id using election_parameter_v12, nogen keep(3 1)

drop _merge
merge 1:1 country_name_short year2 using Best_Zhirnov_erweitert9_LH , nogen keep(3 1) keepusing(dm_eff)

encode country_name_short, gen(c)
order country country_name country_name_short c
sort country_name election_date
xtset c year2


save Tillman_LSVERGL, replace
exit

local control "enep disprop pr prXdisprop smd closeness smdXcloseness growth lnincome"
xtreg turnout_tillman pec1  `control' dm_eff polarization, fe cluster(id)

insheet using viewcalc_election_parameter.csv, delimiter(";") clear
save election_parameter, replace


browse if country_name_s=="DEU" & year(election_date)==1965

browse
