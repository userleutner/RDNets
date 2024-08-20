library(ggplot2)
library(hanna) 
# -------------------
source("hnl.R")
source("functions/add_row.R")
# ------------------------------


# flexible parameters for simulation
initial_seed <- 123  
tax_rates <- c(0.0, 0.05, 0.1, 0.3)  # tax rates to apply
models <- c("null", "winlo") # Null Model an Winner Loser Model
individuals <- c(10, 20, 50, 100, 200)
base_path <- "/images/systematic/" # path to safe generated data
num_runs <- 5  
# -----------------------------------------------------------

# collecting triad data
triad1 <- data.frame()

# loop through all combinations of parameters
for (run_num in 1:num_runs) {
  run_seed <- initial_seed + run_num  # Adjust seed for each run
  set.seed(run_seed)
  
  for (tax_rate in tax_rates) {
    tax_str <- sprintf("tax%02d", as.integer(tax_rate * 100))
    
    for (model in models) {
      
      for (num_individuals in individuals) {
        
        num_neighbours <- ifelse(num_individuals == 10, 10, 20)
        
        # create directory structure if it doesn't exist
        run_dir <- file.path(base_path, sprintf("run%d", run_num), tax_str)
        if (!dir.exists(run_dir)) {
          dir.create(run_dir, recursive = TRUE)
        }
        
        # generate the file path
        file_path <- file.path(
          run_dir, 
          sprintf("%s_%dind_%dnei_%s.png", model, num_individuals, num_neighbours, tax_str)
        )
        
        # png device
        png(filename = file_path, width = 1400, height = 1000)
        
        # plot layout
        par(mfcol = c(3, 4), mai = rep(0.3, 4))
        
        # hnl environment
        hnl$new(n = num_individuals)
        
        # iterate and plot
        for (i in 1:20) {
          A = hnl$iter(region = num_neighbours, tax_rate = tax_rate, model = model)
          A = ifelse(is.na(A) | A == -1, 0, A)
          num_digits = nchar(nrow(A))
          rownames(A) = colnames(A) = sprintf(paste0("%0", num_digits, "i"), 1:nrow(A)) 
          
          if (i %in% c(1, 5, 10, 20)) {
            cols = rep("chartreuse3", nrow(A))
            cols[hnl$token > 1] = "orange"
            cols[hnl$token > 9] = "firebrick3"
            
            # plotting via external hgraph function
            hgraph$plot(A, vertex.color = cols, main = paste("iter =", i), layout = "sam")
            
            # barplot
            barplot(table(hnl$token), main = paste("Gini =", round(hanna::simul$gini(hnl$token), 2)))
            
            # dotchart
            dotchart(unlist(hgraph$triads(A)))
            
            # calculate Gini coefficient
            gini <- round(hanna::simul$gini(hnl$token), 2)
            
            # add collected data to triad1
            triad1 <- add_row(triad1, 
                              unlist(hgraph$triads(A)), 
                              method_name = model, 
                              iteration = i,
                              agents = num_individuals,
                              neighbour = num_neighbours, 
                              tax_rate = tax_rate, 
                              gini_coefficient = gini)
          }
        }
        
        dev.off()
        
        # save collected data to csv file
        summary_file <- file.path(run_dir, sprintf("triad_summary_%s_%dind_%dnei_%s.csv", 
                                                   model, num_individuals, num_neighbours, tax_str))
        write.csv(triad1, summary_file, row.names = FALSE)
        
      }
    }
  }
}

cat("All runs completed and files saved.\n")
