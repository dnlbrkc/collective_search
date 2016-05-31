## Background

This package contains the code used in the simulations of Barkoczi, Analytis \& Wu (2016) Collective search on rugged landscapes: a cross-environmental analysis. \textit{Proceedings of the 38th Annual Conference of the Cognitive Science Society}

## Folders and Files

The code is stored in separate folders.

1. Agents: this contains code for implementing the simulation and the functions used in the simulations

strategies.R: Contains the functions that implement different individual and/or social learning strategies used in the main simulation.
simulation.R: Contains the code for running the simulation.


2. Analysis: contains code for analyzing the results and plotting the graphs.

all_environments.R: Produces the main graph.

combine_results.R: Combines the output of different simulation runs.

modalityPayoffs.R: Calculates modality of the landscapes.


3.Environments:

environmentalStatistics.R:

fitnessMatrix.R

functions.R

MasonWattsStatistics.R: Calculates environmental properties for the Mason and Watts environment.

maxMinEnv.R:

payoffHistograms.R:

- Generate_NK: contains code for generating NK landscapes

- Networks: contains the files storing the different networks

- NK landscapes: contains 100 pre-generated NK landscapes

## Usage

- To run the simulation just open the file '/agents/simulation.R'
- To generate the NK landscapes open '/environments/Generate_NK/generate_NK.R'

