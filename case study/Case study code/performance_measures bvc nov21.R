#########################################
############ Performance measures #######
#########################################

# In this script, the functions to compute the performance measures used in
# the case study are defined.

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




#### Calibration ####


 #Calibration curves + intercepts (with CI)
 calibration_ci <- function(probs,outcome){
   slope_model <- glm(outcome ~ log(probs/(1-probs)), family = "binomial")
   slope <- coef(slope_model)[2]
 slope_ci <- confint(slope_model)[2,]
   intercept_model <- glm(outcome ~ 1, 
                          offset = log(probs/(1-probs)), 
                          family = "binomial")
   intercept <- coef(intercept_model)
   intercept_ci <-  confint(intercept_model)
   return(c(intercept, intercept_ci, slope, slope_ci))
 }

# Brier score
brier <- function(probs,outcome) {
  mean((outcome - probs)^2)
}





