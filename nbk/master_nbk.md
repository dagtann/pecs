## 03/21/2019

Package plm's within estimator exactly reproduces the results published in Table 4.
The next step should be to assess model fit using the most informative 
formulation, i.e. Model 4. Note that the standard errors could be reproduced 
exactly as the text is somewhat vague on what robust standard errors were used.
Anyhow, the different versions supplied by plm did not lead to substantively
different conclusions.

Proposed EDA:
    1. Frequency and electoral success of PECs per country (over time)
    2. Jackknife: Do the results depend on particular countries?
    3. Increase precision: Replace PR/Plurality by District Magnitude

## 03/28/2019

Concluded replication. Results can consistently be reproduced, but turn out not to be particularly robust.

1. Plotting the within-variance in turnout against pec1 shows that most countries do not sensibly inform the estimated causal effect because the within variance in pec1 is to small. For instance, France and Israel do not at all contribute to the estimated effect.
2. The inclusion of period dummies shows that Tillman falsely attributes a trent in the response to PECs.
3. An interaction of pec1 with time shows decreasing returns from PECs.
4. Jacknife: 

### ToDo:
- Generate the within plot and sort panels by effect estimate √
- Generate output tables for pec1 and vote_pec with time dummies and interactios √
- draw a marginal effect plot for the decreasing return from PECs √
- Clean up code files √
- Implement permutation tests

## 04/09/2019

Prepped inclusion of information on the electoral system. Found duplicated entries in Gandruds panel series. Must develop a system to identify unique elections in Tillman, then forward that information to VDEM and Gandrud.

## 04/17/2019

Lost code yesterday on push to github. Rewrote almost the entire data_munging stage. Checked through electoral system indicators. Notices severe flaws in PR & Plurality:
1. Italy, France, Japan, New Zealand account for all within-variance;
2. With the exception of New Zealand dropping any of these countries dramatically shifts the coefficients around;
3. There seem to be coding mistakes, e.g., France has plurality in 2007 only.
Dropped both indicators to clean up the analysis. Higher levels of dispropotionality and more parties continue to decrease turnout. After detrending vote_pec_wi continues to boost turnout whereas pec1_wi does not.

Next steps:

- Write up instituions; √
- Throw in polarization.

## 04/25/2019

Finished institutions write up. Tried to merge polarization from lsvergl into the data. Could not identify unique country id. Return to issue!

## 04/26/2019

Epiphany: Tillman drops countries which do not contribute information on the causal effect of PECs, e.g., Canada, Luxembourg, and Australia. However, these countries may provide information on the strategic incentives to form PECs. We are possibly dealing with endogenous selection bias.

Unintuitive results on adding polarization:

1. Polarization does not increase turnout. The marginal effect is negative and statistically indistinguishable from 0.
2. PEC voting percentages continue to boost turnout even after controlling for party system polarization.

Results merit closer inspection of the estimated models.

## 05/01/2019

Added new lsvergl data. Data now includes information on PEC type,
joint program and incumbency status. Also, voting percentages for
incumbent PECs vs. others has been added. First results hint to
mobilizing effect of non-incumbent PECs, i.e. coordinated opposition
establishes a clearly identifiable alternative to the government. This
hint merits further investigation.

## 05/07/2019

Investigate overlap between PEC codings. Codings disagree on 22 per cent of all observations. Disagreements vary stronger by country than they do by elections. 
