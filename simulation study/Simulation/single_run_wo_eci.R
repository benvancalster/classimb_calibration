################################################
############## single run script ###############
################################################

# In this script a function is defined to do a full simulation run of a specified
# scenario for a specified number of iterations. The results are stored and saved
# in an RDS file. The function takes the list with all simulations, the validation data 
# for a scenario, the number of the scenario, the number of the iteration and a vector
# with seeds for the iteration as input. The output is a matrix containing estimations
# for all performance metrics, all warnings, true AUC, true prevalence and the time it took
# for each model to be estimated.

single_run <- function(simlist_total, val_data, scenario, iter, seed){
  simlist <-  simlist_total[[scenario]]$development
  set.seed(seed[iter])  
    
  # Generating development data
  data <- tryCatch.W.E(gen_TOTAL(simlist = simlist))
  true_AUC <- data$value[[2]]
  true_prevalence <- mean(data$value[[1]][,1])
  
  # Handeling imbalance
  ALL_data <- bal_data(data$value[[1]])
  
  # Fitting models
  ALL_models <- fit_mod_ALL(ALL_data)
  
  # Model validation
  performance_matrix <- val_ALL(val_data, ALL_models, simlist, run = iter)
  
  # Error saving
  data_warnings <- data$warning
  model_warnings <- c()
  for (i in 1:4){
    
      w <- sapply(ALL_models[[i]], '[', 'warning')
      model_warnings_temp <- c(rep(w[1],2), rep(w[2],2), rep(w[3],2))
    
    model_warnings <- append(model_warnings, model_warnings_temp)
    
  }
  
  # Add errors to results
  results <- data.frame(cbind(performance_matrix, data_warnings, sapply(model_warnings, '[[', 1), true_AUC, true_prevalence)) %>% 
    rename(imbalance = V1, model = V4, prob_0_occurence = probs_0, prob_1_occurence = probs_1,
           performance_warnings = V15, system_time = V16, model_warnings = V18) %>% 
    arrange(desc(imbalance))
    
  rownames(results) <- NULL  
  
  # Create directory and store results
  
  dir.create("Output/OUT", showWarnings = FALSE)
  path <- file.path("Output/OUT",scenario)
  dir.create(path,showWarnings = FALSE)
  saveRDS(results,file=file.path(path,paste(paste(scenario,sprintf("%05.0f",iter),sep="_"),".rds",sep="")))
}





