
################################################
########## Model fitting functions #############
################################################

# In this script the functions to fit all models are defined. In the ML logistic 
# regression function a test for data separation is integrated.

################################################
########## Re-calibration model ################
################################################
# This function takes the probabilities estimated by a model as imput and re-estimates
# the intercept to re-calibrate the model

recal_function <- function(probs, outcome) {
  
  if (sum(probs == 1) != 0){
    probs[probs == 1] <- 1 - ((1-max(probs[probs != 1]))/2)
  } 
  
  if (sum(probs == 0) != 0) {
    probs[probs == 0] <- min(probs[probs != 0]) /2 
  }
  recal_mod <- glm(outcome ~ 1, offset = log(probs/(1-probs)), family = "binomial")
}

################################################
########## ML logistic regression  #############
################################################
# This function fits a ML logistic regression model. If the model is fit on an
# adjusted data set, a second, recalibrated model is fit. The output of this function
# consists of a list containing a list with the ML model and the system time and, 
# if appropiate, a list with the recalibrated model and its system time.

ML_reg <- function(data, unad_data){
  sys_time <- system.time(mod_ML <- glm(formula = y~., family = 'binomial', data = data))
  
  if (fastAUC(p = mod_ML$fitted.values, y = data[,1]) == 1){
    warning("Apperant AUC = 1, seperation is assumed")
  }
  
  if (nrow(data) != nrow(unad_data)){
    probs <- predict(mod_ML, unad_data, type = 'response')
    sys_time_recal <- system.time(recal_mod <- recal_function(probs = probs, outcome = unad_data[,1]))
    
    return(list(list(mod_ML, sys_time[3]), list(recal_mod, sys_time_recal[3])))# getting list of ML model, recalibrated ML model and system time
  } else {
    return(list(list(mod_ML, sys_time[3]))) # getting list of ML model and system time
  }
  
}


################################################
######## ridge logistic regression  ############
################################################
# This function fits a ridge logistic regression model. Line 62-65 create a vector
# with lambdas. If the model is fit on an adjusted data set, a second, recalibrated model is fit. The output of this function
# consists of a list containing a list with the ridge model and the system time and, 
# if appropiate, a list with the recalibrated model and its system time.

lseq <- function(from=0.001, to=64, length.out=251) {
  exp(seq(log(from), log(to), length.out = length.out))
}
lambdas <-  c(0, lseq())

RID_reg <- function(data, unad_data, lambdas){
  x <- model.matrix(y ~., data)[,-1]
  y <- data[,1]
  
  # Get hyper parameter
  nfolds <- ifelse(any(table(data$y)<8),nrow(data),10) # If data is near degenerate nfolds for LOOCV
  
  cv_out <- cv.glmnet(x = x , y = y, alpha = 0, lambda = lambdas, 
                      family = 'binomial', nfolds = nfolds)
  
  # Fit model
  sys_time <- system.time(mod_RID <- glmnet(x = x, y = y, alpha = 0, 
                                            family = 'binomial',
                                            lambda = cv_out$lambda.min))
  
  # Fit recalibration model
  if (nrow(data) != nrow(unad_data)){
    probs <- predict(mod_RID, as.matrix(unad_data)[,-1], type = 'response')
    sys_time_recal <- system.time(recal_mod <- recal_function(probs = probs, outcome = unad_data[,1]))
    
    return(list(list(mod_RID, sys_time[3]), list(recal_mod, sys_time_recal[3])))
  } else {
    return(list(list(mod_RID,sys_time[3])))
  }
  
}



################################################
############### Random forest ##################
################################################
# This function fits a random forest model. If the model is fit on an
# adjusted data set, a second, recalibrated model is fit. The output of this function
# consists of a list containing a list with the RF model and the system time and, 
# if appropiate, a list with the recalibrated model and its system time.

RF <- function(data, unad_data){
  data[,1] <- as.factor(data[,1]) # set outcome to factor to work for RF-function
  
  # Fit model
  sys_time <- system.time(mod_RF <- randomForest(formula = y~., data = data))
  
  # Recalibration
  if (nrow(data) != nrow(unad_data)){
    probs <- predict(mod_RF, unad_data, type = 'prob')[,2]
    sys_time_recal <- system.time(recal_mod <- recal_function(probs = probs, outcome = unad_data[,1]))
    
    return(list(list(mod_RF,sys_time[3]), list(recal_mod, sys_time_recal[3])))
  } else {
    return(list(list(mod_RF, sys_time[3])))
  }
}



################################################
################## XGboost #####################
################################################
# This function fits a gradient boosting model using the xgboost algorithm. If the model is fit on an
# adjusted data set, a second, recalibrated model is fit. The output of this function
# consists of a list containing a list with the XGB model and the system time and, 
# if appropiate, a list with the recalibrated model and its system time.
# XGB <- function(data, unad_data){
#   train <- sparse.model.matrix(y ~., data = data)[,-1]
#   output_vector <- data[,1]
#   dtrain <- xgb.DMatrix(data = train, label = output_vector) # create xgb.DMatrix object to train model
# 
#   sys_time <- system.time(mod_XGB <-  xgboost(data = dtrain, nrounds = 5, objective = "binary:logistic"))
# 
#   # Recalibration
#   if (nrow(data) != nrow(unad_data)){ # Check if dealing with an adjusted data set
# 
#     cal <- sparse.model.matrix(y ~., data = unad_data)[,-1]
#     output_vector <- unad_data[,1]
#     dcal <- xgb.DMatrix(data = cal, label = output_vector) # create xgb.DMatrix object
# 
#     probs <- predict(mod_XGB, dcal)
#     sys_time_recal <- system.time(recal_mod <- recal_function(probs = probs, outcome = unad_data[,1]))
# 
#     return(list(list(mod_XGB, sys_time[3]), list(recal_mod, sys_time_recal[3])))
#   } else {
#     return(list(list(mod_XGB, sys_time[3])))
#   }
# }



################################################
######## Function to fit all models ############
################################################
# This function takes the data and adjusted data set as input (these may be the same),
# and gives all four models as output in the form of a list.
# The custom tryCatch.W.E.function is used to
# prevent the code from stopping when an error occurs and to save warnings. 
fit_mod <- function(data, unad_data, lambdas){
  lr_mod <- tryCatch.W.E(ML_reg(data, unad_data))
  rid_mod <- tryCatch.W.E(RID_reg(data,unad_data, lambdas))
  rf_mod <- tryCatch.W.E(RF(data,unad_data))
  #xg_mod <- tryCatch.W.E(XGB(data,unad_data))
  models <- list(lr_mod, rid_mod, rf_mod)#, xg_mod)
  names(models) <- c("lr_mod", "rid_mod", "rf_mod")#, "xg_mod")
  models
}



############################################################
######## Function to fit all models on all data ############
############################################################
# This function takes as input the list of all development data sets (adjusted and unadjusted)
# and fits all models on those data sets. The output is a list containing 4 lists (1 for each adjustment method)
# which in their turn contain lists containing the different models fit on the 
# particular development set


# Fitting all models on all data sets (adjusted & non-adjusted). The order of the
# models is: unadjusted, ROS, RUS, SMOTE
fit_mod_ALL <- function(ALL_data){
  models <- list()
  for (i in 1:4){
    models[i] <- list(fit_mod(ALL_data[[i]], ALL_data[[1]], lambdas))
  }
  names(models) <- names(ALL_data)
  return(models)
}


#########################################
############# Test script ###############
#########################################
#ALL_models <- fit_mod_ALL(ALL_data)


