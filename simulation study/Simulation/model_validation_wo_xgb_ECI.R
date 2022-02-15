################################################
######## Model validation functions ############
################################################

# In this script the functions the evaluate the performance of all models on
# a validation data set are defined. For the unadjusted data (with imbalance),
# performance is evaluated for a decision threshold of both 0.5 and the prevalence. 

################################################
######## Model recalibrated models  ############
################################################

# A function to estimate the performance of the recalibrated models, taking the validation
# data set, the probabilities estimated by the uncalibrated model and the calibrated model as input.

val_RECAL <- function(val_data, probs, model) {
  recal_data <- data.frame(cbind(val_data[,1], probs)) %>% 
    rename(outcome = V1)
  probs_2 <- predict(model[[1]], newdata = recal_data, type = "response")
  probs_0 <- sum(probs_2 == 0)
  probs_1 <- sum(probs_2 == 1)
  pred_2 <- rep(0, length(probs_2)) # empty vector for class prediction
  pred_2[probs_2 > .5] = 1  # predict class with 0.5 threshold
  res <- tryCatch.W.E(performance(pred_2, probs_2, val_data))
  res$value <- c(res$value, probs_0, probs_1)
  return(res)
  
}

################################################
######## Model validation ML-model  ############
################################################
# A function to esimtate the performance of the ML models.
# The output consists of a matrix with 2 rows displaying the results of two models.
# When dealing with an adjusted development set, these two models are the original and recalibrated model.
# When dealing with an unadjusted development data set, these row represent the original model with either 
# a decision threshold set at 0.5 or at the prevalence. The output matrix contains the different performance 
# measures (see performance_measures.R) and possible warnings that occured in the estimation
# of the performance measures.

val_ML <- function(val_data, model, simlist, unadjusted = FALSE){

  ### Estimating probabilities and predicting class membership
    probs <- as.vector(predict(model[[1]][[1]], newdata = val_data, type = 'response'))
    probs_0 <- sum(probs == 0)
    probs_1 <- sum(probs == 1)
    pred <- rep(0, length(probs)) # empty vector for class prediction
    pred[probs > .5] = 1  # predict class with 0.5 threshold


    ### Estimating performance
    
    res <- tryCatch.W.E(performance(pred, probs, val_data))
    if (class(res$warning) == "character"){
      res_warning <- res$warning
    } else {
      res_warning <- res$warning$message
    }
    
    ### Performance with alternative cut_off  
    if (unadjusted == TRUE) {
      pred_prev<- rep(0, length(probs)) # empty vector for class prediction
      pred_prev[probs > simlist$prev] <-  1  # predict class with prevalence threshold
      res_temp <- tryCatch.W.E(performance(pred_prev, probs, val_data))
      
      if (class(res$warning) == "character"){
        res_temp_warning <- res_temp$warning
      } else {
        res_temp_warning <- res_temp$warning$message
      }
      
      res <- rbind(cbind(t(res$value), probs_0, probs_1, res$warning,  model[[1]][[2]]), 
                   cbind(t(res_temp$value), probs_0, probs_1, res_temp$warning,model[[1]][[2]])) 
    }
    
    ### Performance recalibrated model 
    if (unadjusted == FALSE) {
      res_recal <- val_RECAL(val_data, probs, model[[2]])
      
      if (class(res_recal$warning) == "character"){
        res_recal_warning <- res_recal$warning
      } else {
        res_recal_warning <- res_recal$warning$message
      }
      
      res <- rbind(cbind(t(res$value), probs_0, probs_1, res_warning,  model[[1]][[2]]),
                   cbind(t(res_recal$value), res_recal_warning, model[[2]][[2]]))
    }
    return(res)
}
#debugonce(val_ML)

# # 
 #p <- val_ML(val_data[[1]], ALL_models$unadjusted$lr_mod$value, simlist = simlist, unadjusted = TRUE)


################################################
######## Model validation Rid-model ############
################################################
# A function to esimtate the performance of the ridge models.
# The output consists of a matrix with 2 rows displaying the results of two models.
# When dealing with an adjusted development set, these two models are the original and recalibrated model.
# When dealing with an unadjusted development data set, these row represent the original model with either 
# a decision threshold set at 0.5 or at the prevalence. The output matrix contains the different performance 
# measures (see performance_measures.R) and possible warnings that occured in the estimation
# of the performance measures.
val_RID <- function(val_data, model, simlist, unadjusted = FALSE){
  
  ### Estimating probabilities and predicting class membership
  probs <- as.vector(predict(model[[1]][[1]] , newx = as.matrix(val_data)[,-1], type = 'response'))
  probs_0 <- sum(probs == 0)
  probs_1 <- sum(probs == 1)
  pred<- rep(0, length(probs)) # empty vector for class prediction
  pred[probs > .5] = 1  # predict class with 0.5 threshold
  

  ### Estimating performance
  
  res <- tryCatch.W.E(performance(pred, probs, val_data))
  if (class(res$warning) == "character"){
    res_warning <- res$warning
  } else {
    res_warning <- res$warning$message
  }
  
  ### Performance with alternative cut_off  
  if (unadjusted == TRUE) {
    pred_prev<- rep(0, length(probs)) # empty vector for class prediction
    pred_prev[probs > simlist$prev] <-  1  # predict class with prevalence threshold
    res_temp <- tryCatch.W.E(performance(pred_prev, probs, val_data))
    
    if (class(res$warning) == "character"){
      res_temp_warning <- res_temp$warning
    } else {
      res_temp_warning <- res_temp$warning$message
    }
    
    res <- rbind(cbind(t(res$value), probs_0, probs_1, res$warning, model[[1]][[2]]),
                 cbind(t(res_temp$value), probs_0, probs_1,res_temp$warning,model[[1]][[2]])) 
  }
  
  ### Performance recalibrated model 
  if (unadjusted == FALSE) {
    res_recal <- val_RECAL(val_data, probs, model[[2]])
    
    if (class(res_recal$warning) == "character"){
      res_recal_warning <- res_recal$warning
    } else {
      res_recal_warning <- res_recal$warning$message
    }
    
    res <- rbind(cbind(t(res$value),probs_0, probs_1, res_warning, model[[1]][[2]]), 
                 cbind(t(res_recal$value), res_recal_warning, model[[2]][[2]]))
  }
  return(res)

}   

# p <- val_RID(val_data[[1]], ALL_models$ROS$rid_mod$value, simlist = simlist, unadjusted = FALSE)

################################################
######## Model validation RF-model  ############
################################################
# A function to esimtate the performance of the RF models.
# The output consists of a matrix with 2 rows displaying the results of two models.
# When dealing with an adjusted development set, these two models are the original and recalibrated model.
# When dealing with an unadjusted development data set, these row represent the original model with either 
# a decision threshold set at 0.5 or at the prevalence. The output matrix contains the different performance 
# measures (see performance_measures.R) and possible warnings that occured in the estimation
# of the performance measures.
val_RF <- function(val_data, model, simlist, unadjusted = FALSE){
  

  ### Estimating probabilities and predicting class membership
  probs <- predict(model[[1]][[1]] , val_data, type = 'prob')[,2]
  probs_0 <- sum(probs == 0)
  probs_1 <- sum(probs == 1)
  pred <- rep(0, length(probs)) # empty vector for class prediction
  pred[probs > .5] = 1  # predict class with 0.5 threshold
  
  pred_prev<- rep(0, length(probs)) # empty vector for class prediction
  pred_prev[probs > simlist$prev] <-  1  # predict class with prevalence threshold
  
  ### Estimating performance
  
  res <- tryCatch.W.E(performance(pred, probs, val_data))
  if (class(res$warning) == "character"){
    res_warning <- res$warning
  } else {
    res_warning <- res$warning$message
  }
  
  ### Performance with alternative cut_off  
  if (unadjusted == TRUE) {
    pred_prev<- rep(0, length(probs)) # empty vector for class prediction
    pred_prev[probs > simlist$prev] <-  1  # predict class with prevalence threshold
    res_temp <- tryCatch.W.E(performance(pred_prev, probs, val_data))
    
    if (class(res$warning) == "character"){
      res_temp_warning <- res_temp$warning
    } else {
      res_temp_warning <- res_temp$warning$message
    }
    
    res <- rbind(cbind(t(res$value),probs_0, probs_1, res$warning, model[[1]][[2]]),
                 cbind(t(res_temp$value),probs_0, probs_1, res_temp$warning,model[[1]][[2]])) 
  }
  
  ### Performance recalibrated model 
  if (unadjusted == FALSE) {
    res_recal <- val_RECAL(val_data, probs, model[[2]])
    
    if (class(res_recal$warning) == "character"){
      res_recal_warning <- res_recal$warning
    } else {
      res_recal_warning <- res_recal$warning$message
    }
    
    res <- rbind(cbind(t(res$value),probs_0, probs_1, res_warning, model[[1]][[2]]),
                 cbind(t(res_recal$value), res_recal_warning, model[[2]][[2]]))
  }
  return(res)
}    
# debugonce(val_RF)
# p <- val_RF(val_data[[1]], model = ALL_models$unadjusted$rf_mod$value, simlist = simlist, unadjusted = TRUE)

################################################
######## Model validation XGboost model ########
################################################
# A function to esimtate the performance of the XGB models.
# The output consists of a matrix with 2 rows displaying the results of two models.
# When dealing with an adjusted development set, these two models are the original and recalibrated model.
# When dealing with an unadjusted development data set, these row represent the original model with either 
# a decision threshold set at 0.5 or at the prevalence. The output matrix contains the different performance 
# measures (see performance_measures.R) and possible warnings that occured in the estimation
# of the performance measures.
# val_XGB <- function(val_data, model, simlist, unadjusted = FALSE){
#   
#   # putting validation data in matrix format (THINK ABOUT HOW TO DO THIS ONCE FOR ALL IMBALANCE SOLUTIONS?)
#   test <- sparse.model.matrix(y ~., data = val_data)[,-1]
#   output_vector <- val_data[,1]
#   dtest <- xgb.DMatrix(data = test, label = output_vector) # create xgb.DMatrix object to train model
#   
#   
#   ### Estimating probabilities and predicting class membership
#   probs <- predict(model[[1]][[1]], dtest)
#   pred <- rep(0, length(probs)) # empty vector for class prediction
#   pred[probs > .5] = 1  # predict class with 0.5 threshold
#   
#   pred_prev<- rep(0, length(probs)) # empty vector for class prediction
#   pred_prev[probs > simlist$prev] <-  1  # predict class with prevalence threshold
#   
#   ### Estimating performance
#   
#   res <- tryCatch.W.E(performance(pred, probs, val_data))
#   if (class(res$warning) == "character"){
#     res_warning <- res$warning
#   } else {
#     res_warning <- res$warning$message
#   }
#   
#   ### Performance with alternative cut_off  
#   if (unadjusted == TRUE) {
#     pred_prev<- rep(0, length(probs)) # empty vector for class prediction
#     pred_prev[probs > simlist$prev] <-  1  # predict class with prevalence threshold
#     res_temp <- tryCatch.W.E(performance(pred_prev, probs, val_data))
#     
#     if (class(res$warning) == "character"){
#       res_temp_warning <- res_temp$warning
#     } else {
#       res_temp_warning <- res_temp$warning$message
#     }
#     
#     res <- rbind(cbind(t(res$value), res$warning, model[[1]][[2]]), cbind(t(res_temp$value), res_temp$warning,model[[1]][[2]])) 
#   }
#   
#   ### Performance recalibrated model 
#   if (unadjusted == FALSE) {
#     res_recal <- val_RECAL(val_data, probs, model[[2]])
#     
#     if (class(res_recal$warning) == "character"){
#       res_recal_warning <- res_recal$warning
#     } else {
#       res_recal_warning <- res_recal$warning$message
#     }
#     
#     res <- rbind(cbind(t(res$value), res_warning, model[[1]][[2]]), cbind(t(res_recal$value), res_recal_warning, model[[2]][[2]]))
#   }
#   return(res)
# }    

 # p <- val_XGB(val_data[[1]], model = ALL_models$ROS$xg_mod$value, simlist = simlist, unadjusted = FALSE)
################################################
########   All models combined    ##############
################################################
# A function to estimate the performance of all models of a particular adjustment method.
# Takes the validation data of a scenario, the list containing all models of a particular 
# adjustment method and the list containing the simulation factors as input.
# Puts out a matrix with the results of all models for a particular adjustment method.

val_mods <- function(val_data, models, simlist, run, unadjusted = FALSE){
  condition <- names(simlist_total[1])

  
  res_ML <- cbind(condition, run, "ML", rbind(val_ML(val_data, model = models$lr_mod$value, simlist, unadjusted = unadjusted)))
  res_RID <- cbind(condition, run, "RID", rbind(val_RID(val_data, model = models$rid_mod$value, simlist, unadjusted = unadjusted)))
  res_RF <- cbind(condition, run, "RF", rbind(val_RF(val_data, model = models$rf_mod$value, simlist, unadjusted = unadjusted)))
  #res_XGB <- cbind(condition, run, "XGB", rbind(val_XGB(val_data, model = models$xg_mod$value,simlist, unadjusted = unadjusted)))
  
  return(rbind(res_ML, res_RID, res_RF))#, res_XGB))
  
}


################################################
#### All imbalance solutions combined ##########
################################################
# A function taking the validation data for a scenario, a list with all models and the 
# list containing the simulation factors as input. THe output is a matrix containing the
# results of all models on the rows. 
val_ALL <- function(val_data, ALL_models, simlist, run){
  res <- matrix(nrow = 0, ncol = 16) #ncol = 17 when ECI is included, 15 when it is not
  for (i in 1:4){
    ifelse(names(ALL_models[i])=='unadjusted', unadjusted <- TRUE, unadjusted <-  FALSE)
    models <- ALL_models[[i]]
    
    if (unadjusted == FALSE) {
      model_names <- c(paste0(rep(names(ALL_models[i]),3),'_recal'), rep(names(ALL_models[i]),3))
      index <- c(2,4,6,1,3,5)
    res_temp <- cbind(model_names[order(index)], val_mods(val_data, models, simlist, run, unadjusted = unadjusted))
    } else {
      res_temp <- cbind(rep(c(names(ALL_models[1]), 'threshold_move'), 3), val_mods(val_data, models, simlist, run, unadjusted = unadjusted))
    }
    res <- rbind(res, res_temp)
  }
  
  return(res)
}

#debugonce(val_ALL)
#p <- val_ALL(val_data[[1]], ALL_models, simlist = simlist_total$`scenario 1`$validation, run = 1)
# 
 
# ALL_models$unadjusted$lr_mod$value[[length(ALL_models$unadjusted$lr_mod$value)]]
# 
# res_temp <- cbind(rep(names(ALL_models[i]), 4), val_mods(val_data, models, simlist, unadjusted = unadjusted))

