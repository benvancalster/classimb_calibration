
#### Check for separation function ####

sep_check_all <- function(simlist_total, scenario, iter, seed){

  simlist <-  simlist_total[[scenario]]$development
  set.seed(seed[iter])  

  # Generating development data
  data <- gen_TOTAL(simlist = simlist)

  # Handeling imbalance
  ALL_data <- bal_data(data[[1]])

  ## Fit GLM on data and check for separation
  separation <- rep(NA, 4)

    for (i in 1:4){
      mod_ML <- glm(formula = y~., family = 'binomial', data = ALL_data[[i]])

      if (fastAUC(p = mod_ML$fitted.values, y = ALL_data[[i]]$y) == 1){
        separation[i] <- 1
            } else {
              separation[i] <- 0
            }
          }

  # fit all models
  ALL_models <- fit_mod_ALL(ALL_data)

  # get probs from RF and check separation
  separation_recal <- rep(NA, 4)

  unad_data <- ALL_data$unadjusted

    for (i in 1:4){
      probs <- predict(ALL_models[[i]]$rf_mod$value[[1]][[1]], unad_data, type = 'prob')[,2]

      if (fastAUC(p = probs, y = ALL_data$unadjusted$y) == 1){
          separation_recal[i] <- 1
          } else {
           separation_recal[i] <- 0
        }
      }

    results <- cbind(iter, 
                 c("unadjusted", "ROS", "RUS", "SMOTE", "RF_recal_unad", "RF_recal_ROS", "RF_recal_RUS", "RF_recal_SMOTE"),
                 c(separation, separation_recal))
              
      results <- data.frame(results)
      colnames(results) <- c("iter", "imbalance", "separation")

      # Create directory and store results
      
      dir.create("Output/OUT_separation", showWarnings = FALSE)
      path <- file.path("Output/OUT_separation",scenario)
      dir.create(path,showWarnings = FALSE)
      saveRDS(results, file=file.path(path,paste(paste(scenario,sprintf("%05.0f",iter),sep="_"),".rds",sep="")))
}





