library(ggplot2)
library(hanna)
library(ineq)
# -------------------
source("hnl.R")
source("functions/add_row.R")
# ------------------------------

# Flexible parameters for simulation
initial_seed <- 123  # Initial seed for reproducibility
tax_rates <- c(0.0, 0.1) 
models <- c("null", "winlo") # Null Model and Winner Loser Model
individuals <- c(10, 20, 50, 100, 200)
base_path <- "output/images" # Path to save generated image data
num_runs <- 5

# Loop through all combinations of parameters
for (run_num in 1:num_runs) {
  run_seed <- initial_seed + run_num  # Adjust seed for each run
  set.seed(run_seed)
  
  # Define the run directory
  run_dir <- file.path(base_path, sprintf("run%d", run_num))
  
  # Create the run directory if it does not exist
  if (!dir.exists(run_dir)) {
    dir.create(run_dir, recursive = TRUE)
  }
  
  for (tax_rate in tax_rates) {
    tax_str <- sprintf("tax%02d", as.integer(tax_rate * 100))
    
    # Directory for the current tax rate
    tax_dir <- file.path(run_dir, tax_str)
    data_dir <- file.path("output/data")
    lambda_dir <- file.path(tax_dir, "lambda")  # New lambda directory

    # Create directories if they do not exist
    if (!dir.exists(tax_dir)) {
      dir.create(tax_dir, recursive = TRUE)
    }
    if (!dir.exists(lambda_dir)) {
      dir.create(lambda_dir, recursive = TRUE)
    }
    if (!dir.exists(data_dir)) {
      dir.create(data_dir, recursive = TRUE)
    }
    
    # Initialize collect_data for each tax rate run
    collect_data <- data.frame()  # Reset collect_data for each tax rate
    
    for (model in models) {
      for (num_individuals in individuals) {
        
        num_neighbours <- ifelse(num_individuals == 10, 10, 20)
        
        # Generate the file path for the main plot
        file_path <- file.path(
          tax_dir, 
          sprintf("%s_%dind_%dnei_%s.png", model, num_individuals, num_neighbours, tax_str)
        )
        
        # PNG device for the main plot
        png(filename = file_path, width = 1400, height = 1000)
        
        # PNG layout
        par(mfcol = c(4, 4), mai = c(0.5, 0.5, 0.3, 0.3) + 0.03) 
        
        # Initialize the hnl environment
        hnl$new(n = num_individuals)
        
        initial_tokens <- hnl$token
        prev_tokens <- initial_tokens # Start with the initial tokens as the previous state
        resource_changes <- numeric(0)
        
        # Iterate and plot
        for (i in 1:20) {
          A <- hnl$iter(region = num_neighbours, tax_rate = tax_rate, model = model)
          A <- ifelse(is.na(A) | A == -1, 0, A)
          num_digits <- nchar(nrow(A))
          rownames(A) <- colnames(A) <- sprintf(paste0("%0", num_digits, "i"), 1:nrow(A)) 
          
          # Calculate resource changes for lambda calculation
          token_changes <- mean(abs(hnl$token - prev_tokens))
          lambda <- if (i == 1) token_changes else mean(c(resource_changes, token_changes))
          resource_changes <- c(resource_changes, token_changes) # Keep track of all changes
          prev_tokens <- hnl$token # Update prev_tokens to the current state
          
          if (i %in% c(1, 5, 10, 20)) {
            cols <- rep("chartreuse3", nrow(A))
            cols[hnl$token > 1] <- "orange"
            cols[hnl$token > 9] <- "firebrick3"
            
            # Plotting via external hgraph function
            hgraph$plot(A, vertex.color = cols, main = paste("iter =", i), layout = "sam")
            
            # Barplot
            barplot(table(hnl$token), main = paste("Gini =", round(hanna::simul$gini(hnl$token), 2)), 
                    xlab = "Number of Token", ylab= "Number of Agents")

            # Lorenz curve plot
            plot(Lc(hnl$token), main = "Lorenz Curve", xlab = "Cumulative Share of Agents", ylab = "Cumulative Share of Tokens", col = "blue", lwd = 2)
            abline(0, 1, col = "red", lty = 2)

            # Dotchart
            dotchart(unlist(hgraph$triads(A)), 
                     xlab = "Occurrence", ylab = "Triad Structure")
            
            # Calculate Gini coefficient
            gini <- round(hanna::simul$gini(hnl$token), 2)

            # Add collected data to collect_data
            collect_data <- add_row(collect_data, 
                                    unlist(hgraph$triads(A)), 
                                    iteration = i,
                                    method_name = model, 
                                    agents = num_individuals,
                                    neighbour = num_neighbours, 
                                    tax_rate = tax_rate, 
                                    gini_coefficient = gini,
                                    lambda = lambda
                                    )
          }
        }
        
        # Close PNG device for the main plot
        dev.off()
        
        # ---- Visualization Code ---- #
        # Time Series Line Plot for Lambda
        time_series_file <- file.path(lambda_dir, sprintf("timeseries_run%d_tax%02d_%s_%dind.png", 
                                                        run_num, as.integer(tax_rate * 100), model, num_individuals))
        png(filename = time_series_file, width = 800, height = 600)
        plot(1:20, resource_changes, type="o", col="blue", xlab="Iteration", ylab="Lambda",
             main=paste("Lambda over Time for", model, "Model with", num_individuals, "Agents"))
        dev.off()
        
      }
    }
    
    # Save collected data as CSV
    summary_file <- file.path(data_dir, sprintf("data_summary_run%d_%s.csv", run_num, tax_str))
    write.csv(collect_data, summary_file, row.names = FALSE)
    
    # Reset collect_data for the next tax rate or run, if necessary
    collect_data <- data.frame()
  }
}

cat("All runs completed and files saved.\n")
