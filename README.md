# BDA_project
A Bayesian analysis of vibrations over time from power plant components. All code and other files related to the project can be found in this repo. 

## Conclusion of the analysis
We looked at the vibrations from the cooling water pumps from a large Danish power plant, with the initial motivation that we were told that the Spring months were supposed to have unusually high vibration levels. Based on the Bayesian analysis using Hamiltonian Markov Chain Monte Carlo, we did not find any reason to believe that this was actually the case. We did find, however, that the month of September showed higher level of vibrations, but this was simply due to parts being replaced during the summer, causing a burn-in period for the pump, before stability was regained due to adjusted parts.


## Software
The analysis was done using R 4.1.0 and the Bayesian MCMC modelling framework Stan (via the RStan package).
