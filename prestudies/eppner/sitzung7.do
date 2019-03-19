use pec_elections_apr13, clear
keep if variation==1
xtset, clear
save tilmann, replace

use tilmann, clear
tab country,gen(cdummy)

reg turnout pec
reg turnout pec cdummy*
*wir benötigen eine numerische country variable
encode country, gen(country_num)
xtreg turnout pec, fe i(country_num)


bysort country: egen countrymean = mean(turnout)
gen turnout_dev = turnout-countrymean
bysort country: egen countrymeanpec = mean(pec)
gen pec_dev = pec-countrymeanpec
reg turnout_dev pec_dev

local control "enep disprop pr prXdisprop smd closeness smdXcloseness growth lnincome"
xtreg turnout pec1 `control', fe cluster(id)

tab country, gen(cdummy)
reg turnout pec1 `control' cdummy* , cluster(id)



*Übung
sum turnout
reg turnout
reg turnout cdummy*

sum turnout if country=="Austria"
sum turnout if country=="UK"
****Ergo: die Länderdummys schätzen den Average in dem jeweiligen Land (bzw wie stark der abweicht von der Referenzkategorie)

*soweit eigentlich NICHTS NEUES

reg turnout pec1
reg turnout pec1 cdummy*
xtset country
*Interpretieren Sie beide Modelle mit Blick auf PEC1!

**BEide sagen ja:
[1] Das Vorhandensein einer Vorwahlkoalition erhöht die Wahlbeteiligung um 1.65%
*ABER WO IST DER UNTERSCHIED?
*das zweite Modell vergleich die Länder nicht untereinander, sondern nur die Fälle in EINEM LAND
*dahinter die Idee: In 
reg turnout pec1
avplots, mlabel(country) name(normal, replace)
reg turnout pec1 cdummy*
avplots, mlabel(country) name(fixedeffect, replace)
graph combine normal fixedeffect


xtreg turnout pec, fe i(country_num)
estimates store FE
xtreg turnout pec, re i(country_num)
estimates store RE
hausman FE RE


reg turnout pec1 cdummy*

avplots, mlabel(country) name(fixedeffect, replace)
graph combine normal fixedeffect
