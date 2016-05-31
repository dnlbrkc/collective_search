## Background

This package contains the code used in the simulations of Barkoczi, Analytis \& Wu (2016) Collective search on rugged landscapes: a cross-environmental analysis. \textit{Proceedings of the 38th Annual Conference of the Cognitive Science Society}

## Folders and Files

The code is stored in separate folders.

- Agents: this contains code for implementing the simulation and the functions used in the simulations

1. strategies.R: Contains the functions that implement different individual and/or social learning strategies used in the main simulation.

2. simulation.R: Contains the code for running the simulation.


- Analysis: contains code for analyzing the results and plotting the graphs.

1. all_environments.R: Produces the main graph.

2. combine_results.R: Combines the output of different simulation runs.

3. modalityPayoffs.R: Calculates modality of the landscapes.


- Environments:

1. environmentalStatistics.R:

2. fitnessMatrix.R

3. functions.R

4. MasonWattsStatistics.R: Calculates environmental properties for the Mason and Watts environment.

5. maxMinEnv.R:

6. payoffHistograms.R:

- Generate_NK: contains code for generating NK landscapes

- Networks: contains the files storing the different networks

- NK landscapes: contains 100 pre-generated NK landscapes

## Usage

- To run the simulation just open the file '/agents/simulation.R'
- To generate the NK landscapes open '/environments/Generate_NK/generate_NK.R'

