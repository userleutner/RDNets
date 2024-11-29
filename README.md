# **R**esource **D**istribution **Net**works

`RDNets` contains code for the study titled "The Impact of a 10% Tax Rate and Redistribution on Resource Concentration in Networks". The study simulates the redistribution of resources (tokens) among agents with (and without) a fixed 10% tax rate and tracks the resulting inequality over multiple iterations.

## Key Scripts
### `functions/hnl.R`
This is the core script of the repository and contains the simulation model. It defines the following main components:

- **`hnl$new()`**: Initializes the environment and defines agent tokens.
- **`hnl$tax_reinvest()`**: Handles the collection of tax and the redistribution of resources among agents, including the handling of carried-forward remainders.
- **`hnl$iter()`**: Runs the simulation for a specified number of games (iterations), utilizing different models (Null or Winner-Loser) and adjusting for tax rate and agent resource levels.

### `functions/add_row.R`
This helper function is used to append new data rows to the results of each simulation.

## Handling Simulations

The simulations are executed by running the main function in `hnl.R`, which simulates the interaction of agents within a network. The primary steps are as follows:

1. **Initialize Simulation**: Set the initial number of agents and their token amounts using `hnl$new()`. Each agent starts with a set amount of tokens (default of 50 tokens).

2. **Run Simulation**: Call the `hnl$iter()` function to start the simulation. The number of iterations (games) and the model type (Null or Winner-Loser) can be customized as arguments to this function.

   - The `iter()` function simulates agent interactions and resource transfers based on the model.
   - Tax is applied at the end of each iteration with the `tax_reinvest()` function, adjusting each agent's tokens based on the specified tax rate.

3. **Tax Strategy**: The model applies a 10% tax rate (or other values as specified) and redistributes the collected tax evenly among all agents. The remainder from this redistribution is carried forward to the next iteration, ensuring that no tax is lost.

4. **Run Multiple Simulations**: The process is repeated across multiple runs (5 runs by default). Each run results in different distributions of resources, which are stored in both tabular (CSV) and graphical formats (images).

## Output

### Data Files
The results of each simulation are summarized in CSV files located in the `output/data/` folder. These files contain key metrics for each iteration, including:
- The total number of tokens for each agent at the end of each run.
- Calculations for the Gini coefficient and lambda-factor.
- Occurrence freqeuncies of the measured triad structures.

### Images
Graphical representations of the simulation data are stored in the `output/images/` directory. These include plots of resource distribution over time, Gini coefficient visualizations, and lambda-factor trend charts.

## Running the Simulations

To run the simulations, follow these steps:

1. **Install Required Packages**: Ensure you have the necessary R packages installed to run the scripts. You may need packages such as `ggplot2`, `hanna`, `fmsb` for plotting and `dplyr` and `ineq` for data manipulation.

2. **Load the Script to Build the Environment**: Open and run the `hnl.R` script in an R environment. You can adjust the parameters (number of agents, tax rates, and model type) by modifying the function calls within the script.

3. **Start the Simulation**: Call the `hnl$iter()` function, specifying the number of iterations, tax rate, model type, and any other relevant parameters. Example:

   ```r
   hnl$iter(games = 10, model = "winlo", region = 9, progress = TRUE, tax_rate = 0.1)


## License