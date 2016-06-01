# Background

This package contains the code used in the simulations of Barkoczi, Analytis and Wu (2016).

# Usage

## Generating the environments used in the simulations

1. Fitness matrices for Function-based environments are generated using '/environments/fitnessMatrix.R'

2. The Mason and Watts (2012) environment is generated on the fly in the simulation code. The payoff function can be found in '/environments/functions.R' at the bottom

3.  NK environments with custom N and K parameters can be generated using the file '/environments/Generate_NK/generate_NK.R'. For our purposes, we generated 100 replications of N=20,K=5 and for N=20,K=10.

## Social Networks

1. Social network data is generated using '/environments/Networks/networks.R'

## Agent Simulations
1. The simulation can be run using the file '/agents/simulation.R'. Model strategies can be found in '/agents/strategies.R'


# Citation

Barkoczi,D. Analytis,P.P., \& Wu, M.C. (2016). Collective search on rugged landscapes: a cross-environmental analysis. Proceedings of the 38th Annual Conference of the Cognitive Science Society.
