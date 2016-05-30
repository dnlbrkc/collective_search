## 

This package contains the code used in the simulations of Barkoczi, Analytis \& Wu (2016) Collective search on rugged landscapes: a cross-environmental analysis. \textit{Proceedings of the 38th Annual Conference of the Cognitive Science Society}

## Structure

The code is stored in separate folders.

- Agents: this contains code for implementing the simulation.

strategies.R: contains the functions that implement different individual and/or social learning strategies.
simulation.R: contains the code for running the simulation


- Analysis: contains code for analizing the results and plotting the graphs.

all_environments.R: produces the main graph

combine_results.R: combines the output of different simulation rungs

modalityPayoffs.R: calculates modality of the landscapes


- Environments:

environmentalStatistics.R:

fitnessMatrix.R

functions.R

MasonWattsStatistics.R: Calculates environmental properties for the Mason and Watts environment.

maxMinEnv.R:

payoffHistograms.R:

- Generate_NK: contains code for generating NK landscapes

- Networks: contains the files storing the different networks

- NK landscapes: contains 100 pre-generated NK landscapes

## Example

To run the simulation just open the file /agents/simulation.R.