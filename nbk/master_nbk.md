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
- Clean up code files
- Implement permutation tests
