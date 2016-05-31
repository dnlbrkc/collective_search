## Background

This package contains the code used in the simulations of Barkoczi, Analytis and Wu (2016).

## Usage

- To run the simulation just open the file '/agents/simulation.R'
- To generate the NK landscapes open '/environments/Generate_NK/generate_NK.R'


## Network files

- Social network are stored in /environments/Networks

1. fullNet.Rdata : Fully connected networks with 100 nodes

2. localNet.Rdata: Locally connected lattice network with 100 nodes and a degree of 4.

##Landscape files

- Nk Landscapes can be generated using the file /environments/Generate_NK/Generate_NK.R
- Functions for the 2D landscapes are stored in /environments/function.R
- Pre-generated payoffs for function-based 2D environments for a 1001x1001 grid are stored in /environments/environments.Rdata

##Citation

Barkoczi,D. Analytis,P.P., \& Wu, C (2016). Collective search on rugged landscapes: a cross-environmental analysis. Proceedings of the 38th Annual Conference of the Cognitive Science Society.
