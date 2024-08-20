# to create triad dataframe in simulation_1_20.rmd and systematic_runs.rmd

add_row <- function(df, new_data, iteration, method_name, agents, neighbour, tax_rate, gini_coefficient) {
  new_row <- data.frame(
    iteration = iteration,
    method = method_name,
    agents = agents,
    neighbour = neighbour,
    tax_rate = tax_rate,
    gini_coefficient = gini_coefficient,
    dd = as.numeric(new_data["dd"]),
    ds = as.numeric(new_data["ds"]),
    pa = as.numeric(new_data["pa"]),
    tr = as.numeric(new_data["tr"]),
    cy = as.numeric(new_data["cy"]),
    stringsAsFactors = FALSE
  )
  df <- rbind(df, new_row)
  return(df)
}