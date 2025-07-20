# Dynamic Bayesian Network (DBN) Structure Learning Simulation

This repository provides an R implementation of a simulation study for learning the structure of Dynamic Bayesian Networks (DBNs) using different scoring criteria.  
It evaluates both model fit and structural accuracy across 30 replications.

# Features
- Supports multivariate time series with `k = 5`, `k = 7`, or `k = 10` variables.
- Computes model fit scores and Structural Hamming Distance (SHD) for each scoring criterion.
- Outputs summary statistics: mean and standard error (SE) of scores, and mean of SHDs.

# Requirements
- R (≥ 4.0.0 recommended)
- The following R packages:
  - `dbnR`
  - `dplyr`

Install dependencies in R if not already installed:
install.packages("dbnR")
install.packages("dplyr")

# Usage
git clone https://github.com/asliyamann/dbn_simulation.git
