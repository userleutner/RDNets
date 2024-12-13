# Resource Distribution Networks (RDNets)

## Overview

The `RDNets` repository contains code for the study titled **"The Impact of a 10% Tax Rate and Redistribution on Resource Concentration in Networks."** The study simulates the redistribution of resources (tokens) among agents under various conditions, including a fixed 10% tax rate, and tracks the resulting inequality metrics over multiple iterations.

## Key Features

### Simulation Models

Two primary models are implemented:

- **Null Model**: Random interactions without a structured win/lose dynamic.
- **Winner-Loser Model**: Incorporates structured interactions where some agents gain and others lose based on predefined rules.

### Tax Strategy

- A fixed 10% tax rate is applied.
- Collected taxes are redistributed equally among all agents.
- Any remainders from redistribution are carried forward to subsequent iterations to avoid loss of resources.

### Metrics

The simulation tracks:

- **Gini Coefficient**: Measures inequality in resource distribution.
- **Lambda-Factor**: Evaluates resource dynamics and redistribution effects.
- **Triad Structures**: Examines patterns of interactions between agents to provide insight into individual relationships and their impact on resource distribution.

### Flexibility

The number of agents, tax rates, models, and iterations can be customized. Simulations are reproducible by setting an initial random seed.

## Repository Structure

### Scripts

#### `functions/hnl.R`

This is the core simulation script. Key components include:

- ``: Initializes agents with a specified number of tokens (default: 50 per agent).
- ``: Applies tax and redistributes resources, carrying forward remainders.
- ``: Runs simulations for a specified number of iterations, with adjustable models and tax rates.

#### `functions/add_row.R`

A helper script used to append results during simulations.

#### `systematic_runs.R`

Defines flexible and reproducible parameters for running systematic simulations. Key parameters include:

- `tax_rates`: A vector of tax rates (e.g., 0.1, 0.0).
- `models`: A list of models (`"null"` or `"winlo"`).
- `individuals`: A range of agent counts (e.g., 10, 20, 50, 100, 200).

### Output

#### Data Files

- Stored in `output/data/`.
- Includes plots showing resource distribution trends, Gini coefficient dynamics, lambda-factor trajectories, and triad structures.

#### Graphical Results

- Stored in `output/images/`.
- Includes plots showing resource distribution trends, Gini coefficient dynamics, and lambda-factor trajectories.


### Prerequisite
The statisical Analysis was performed using R 4.3.0\
R Core Team (2023). _R: A Language and Environment for Statistical
  Computing_. R Foundation for Statistical Computing, Vienna, Austria.
  <https://www.R-project.org/>.
  
The follwing R packages where used:

- `ggplot2`   H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.
- `dplyr`  Wickham H, François R, Henry L, Müller K, Vaughan D (2023). _dplyr: A Grammar of Data Manipulation_. R package version 1.1.4, <https://CRAN.R-project.org/package=dplyr>.
- `ineq`    Zeileis A (2014). _ineq: Measuring Inequality, Concentration, and Poverty_. R package version 0.2-13, <https://CRAN.R-project.org/package=ineq>.
- `fmsb`  Nakazawa M (2024). _fmsb: Functions for Medical Statistics Book with some Demographic Data_. R package version 0.7.6, <https://CRAN.R-project.org/package=fmsb>.


## License

This repository is open-source and available under the MIT License. See the `LICENSE` file for more details.
