# Evaluation functions

#### Discrimination ####


# accuracy
accuracy <- function(pred, outcome){
  correct_predictions <- sum(pred == outcome)
  total_predictions <- length(pred)
  correct_predictions / total_predictions
}

# sensitivity 
sensitivity <- function(pred, outcome) {
  true_positives <- sum(pred == 1 & outcome == 1)
  false_negatives <-  sum(pred == 0 & outcome == 1)
  true_positives/(true_positives+false_negatives)
}

# specificity
specificity <- function(pred, outcome) {
  true_negatives <- sum(pred == 0 & outcome == 0)
  false_positives <-  sum(pred == 1 & outcome == 0)
  true_negatives/(true_negatives+false_positives)
}

# All combined
F1 <- function(pred, outcome){
  true_positives <- sum(pred == 1 & outcome == 1)
  false_negatives <-  sum(pred == 0 & outcome == 1)
  true_negatives <- sum(pred == 0 & outcome == 0)
  false_positives <-  sum(pred == 1 & outcome == 0)
  
  accuracy <- (true_negatives + true_positives)/
              (true_negatives + true_positives + false_positives + false_negatives)
  sensitivity <-  true_positives/(true_positives+false_negatives)
  specificity <- true_negatives/(true_negatives+false_positives)
  f1 <- (2*true_positives)/((2*true_positives) + false_positives + false_negatives)
  
  c(accuracy, sensitivity, specificity, f1)
}



# All combined
# c-statistic
# c.stat2 <- function(probs, outcome){
#   probs <- as.matrix(probs)
#   cats <- sort(unique(outcome))
#   n_cat <- length(cats)
#   n0   <- sum(outcome == cats[2])
#   n1   <- length(outcome) - n0
#   r <- rank(probs[,1])
#   S0 <- sum(as.numeric(r[outcome == cats[2]]))
#   (S0 - n0 * (n0 + 1)/2)/(as.numeric(n0) * as.numeric(n1))
# }

fastAUC <- function(p, y) {
  x1 = p[y==1]; n1 = length(x1); 
  x2 = p[y==0]; n2 = length(x2);
  r = rank(c(x1,x2))  
  auc = (sum(r[1:n1]) - n1*(n1+1)/2) / n1 / n2
  return(auc)
}


# with CI
# auc <- matrix(nrow = ncol(probs_table), ncol = 3)
# 
# for (i in 1:ncol(probs_table)){
#   mat <- cbind(probs_table[,i], test_outcome)
#   x <- data.frame(mat) %>% 
#     filter(test_outcome == 1)
#   y <- data.frame(mat) %>% 
#     filter(test_outcome == 0)
#   auc[i,] <- auRoc::auc.nonpara.mw(x = x[,1], 
#                                    y = y[,1],
#                                    method = 'pepe')
# }

#### Calibration ####


# Calibration curves + intercepts (with CI)
# calibration_ci <- function(probs,outcome){
#   slope_model <- glm(outcome ~ log(probs/(1-probs)), family = "binomial")
#   slope <- coef(slope_model)[2]
#   slope_ci <- confint(slope_model)[2,]
#   intercept_model <- glm(outcome ~ 1, 
#                          offset = log(probs/(1-probs)), 
#                          family = "binomial")
#   intercept <- coef(intercept_model)
#   intercept_ci <-  confint(intercept_model)
#   return(c(intercept, intercept_ci, slope, slope_ci))
# }

# Brier score
brier <- function(probs,outcome) {
  mean((outcome - probs)^2)
}

# Calibration curves + intercepts (without CI)
calibration <- function(probs,outcome){
  brier_score <- brier(probs, outcome)
  
  if (sum(probs == 1) != 0){
    probs[probs == 1] <- 1 - ((1-max(probs[probs != 1]))/2)
  } 
  
  if (sum(probs == 0) != 0) {
    probs[probs == 0] <- min(probs[probs != 0]) /2 
  }
  
  slope_model <- glm(outcome ~ log(probs/(1-probs)), family = "binomial")
  slope <- coef(slope_model)[2]
  attributes(slope) <- NULL
  intercept_model <- glm(outcome ~ 1, 
                         offset = log(probs/(1-probs)), 
                         family = "binomial")
  intercept <- coef(intercept_model)
  attributes(intercept) <- NULL
  cal_results <- c(intercept, slope, brier_score)
  
  return(cal_results)
}



# ECI
eci_bvc <- function(calout,pred){
  (mean((pred-fitted(calout))*(pred-fitted(calout))))*100
}

calout_f <- function(outcome, probs){ 
  loess(outcome ~ log(probs/(1-probs)), span = 1)
}

eci <- function(probs, pred, outcome){
  if (sum(probs == 1) != 0){
    probs[probs == 1] <- 1 - ((1-max(probs[probs != 1]))/2)
  } 
  
  if (sum(probs == 0) != 0) {
    probs[probs == 0] <- min(probs[probs != 0]) /2 
  }
  
  calout <- calout_f(outcome, probs)
  eci <- eci_bvc(calout, pred)
  
  eci
}


# system.time(calout_f(val_data[,1], probs))

################################################
######## All meseasures combined  ##############
################################################

performance <- function(pred, probs, val_data){

## Accuracy measures
F1 <- F1(pred, val_data[,1])


## Discrimination
c_statistic <- fastAUC(probs, val_data[,1])

# Calibration

res_cal <-  calibration(probs, val_data[,1])

#eci <- eci(probs, pred, val_data[,1]) # Takes a long time to run due to LOESS fitting


results <- c(F1, c_statistic, res_cal)#, eci)
names(results) <- c("accuracy", "sensitivity", "specificity", "F1",
                    "c_statistic", "cal_intercept", "cal_slope", "Brier")#, "ECI")
return(results)
}

