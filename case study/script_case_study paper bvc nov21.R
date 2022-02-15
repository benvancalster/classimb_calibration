#####################################
#####################################
#### CLASS IMBALANCE: CASE STUDY ####
#####################################
#####################################

# Authors: Ruben van den Goorbergh, Ben Van Calster, Maarten van Smeden
# Date: Nov 25, 2021


###############################
#### Set working directory ####
###############################

setwd("C:\\Ben\\MStat thesis proposals\\Utrecht 2020\\")


################################
#### Load required packages ####
################################

# In this part of the code, all required packages are loaded. If package is not
# installed on your computer, you may install it using: install.packages("<PACKAGENAME>")

#data cleaning
library(tidyverse) 
library(summarytools)
library(DescTools)

# models
library(glmnet)
library(glmnetUtils)
library(data.table)

#imbalance
library(caret) 
library(smotefamily) 

# Performance
library(xtable) 
library(CalibrationCurves)
library(rmda)

# Source custom written performance measure functions
source('Ruben van den Goorbergh/Case study code/performance_measures bvc nov21.R')


#####################
#### Data import ####
#####################

data <- read.delim('Data/iota_utrecht.txt', header = TRUE)

# select only premenopausal patients (to create more imbalance)
# Restrict to adults, and people below 60
data <- data %>% filter(menoyn != 1) 
data <- data %>% filter(Age <60 & Age >=18) 

# select variables used for model 
data2 <- data %>% 
  select(outcome1, Age, lesdmax, papnr) 


#########################
#### Check imbalance ####
#########################

summary(data2)

dfSummary(data2)

# Check imbalance
data2 %>% 
  ggplot() +
  geom_bar(mapping = aes(outcome1))

n_minor <- data %>% 
  filter(outcome1 == 1) %>% 
  nrow()

n_major <- data %>% 
  filter(outcome1 == 0) %>% 
  nrow()

n_minor/n_major


#####################################
####   DATA SPLITTING 0.8:0.2    ####
#####################################

# In this part of the code, the data is split into train and test parts.

#### train-test data ####
set.seed(1333) # set seed for reproducibility 

# Sample indexes to select data for train set
train_index <- sample(x = nrow(data2), 
                      size = round(0.8 * length(data2$outcome1)), 
                      replace = FALSE)

# Split data into train and test set
train_set <- data2[train_index,] 
test_set <- data2[-train_index,]
test_set$papnr = as.factor(test_set$papnr)

# select outcome test set used in performance measures
test_outcome <- test_set$outcome1 


##################################
#### CREATE BALANCED DATASETS ####
##################################

# In this part different data sets are created using different class imbalance approaches

# creating separate predictors and outcome matrices to use in caret package functions
x <- train_set[,2:ncol(train_set)] 
y <- as.factor(train_set[,1])


## Random undersampling ##
##########################

train_down <- downSample(x = x, y = y, yname = 'outcome1')
train_down %>% 
  ggplot()+
  geom_bar(mapping = aes(outcome1))


## Random oversampling ##
#########################

train_up <- upSample(x = x, y = y, yname = 'outcome1')
train_up %>% 
  ggplot()+
  geom_bar(mapping = aes(outcome1))


## SMOTE ##
###########

train_smote <- SMOTE(X= train_set[,2:length(train_set)], target = train_set[,1], dup_size = 0)

train_smote <- train_smote$data

train_smote <- train_smote %>% 
  rename(outcome1 = class) 

train_smote$outcome1  <- as.factor(train_smote$outcome1)

train_smote %>% 
  ggplot()+
  geom_bar(mapping = aes(outcome1))



# Make papnr a factor
train_set$papnr = as.factor(train_set$papnr)
train_up$papnr = as.factor(train_up$papnr)
train_down$papnr = as.factor(train_down$papnr)
train_smote$papnr = as.factor(round(train_smote$papnr, digits=0))



######################################
#### Standard logistic regression ####
######################################

# In this part the logistic regression models are fitted.

## no adjustment##
##################
 
log_model <-glm(formula = outcome1~rcs(Age,3) + rcs(lesdmax,3) + papnr, family = 'binomial', data = train_set)
summary(log_model)
log_probs <- predict(log_model, test_set, type = 'response') # get probabilities
log_pred <- rep(0, length(log_probs)) # empty vector for class prediction
log_pred[log_probs > .5] = 1  # predict class with 0.5 threshold
log_pred2 <- rep(0, length(log_probs)) # empty vector for class prediction
log_pred2[log_probs > .192] = 1  # predict class with 0.5 threshold


## Undersampling ##
###################

log_model_down <-glm(formula = outcome1~rcs(Age,3) + rcs(lesdmax,3) + papnr, family = 'binomial', data = train_down)
summary(log_model_down)
log_probs_down <- predict(log_model_down, test_set, type = 'response') # get probabilities
log_pred_down <- rep(0, length(log_probs_down)) # empty vector for class prediction
log_pred_down[log_probs_down > .5] = 1 # predict class with 0.5 threshold
log_pred2_down <- rep(0, length(log_probs_down)) # empty vector for class prediction
log_pred2_down[log_probs_down > .192] = 1 # predict class with 0.5 threshold


## Oversampling ##
##################

log_model_up <-glm(formula = outcome1~rcs(Age,3) + rcs(lesdmax,3) + papnr, family = 'binomial', data = train_up)
summary(log_model_up)
log_probs_up <- predict(log_model_up, test_set, type = 'response') # get probabilities
log_pred_up <- rep(0, length(log_probs_up)) # empty vector for class prediction
log_pred_up[log_probs_up > .5] = 1 # predict class with 0.5 threshold
log_pred2_up <- rep(0, length(log_probs_up)) # empty vector for class prediction
log_pred2_up[log_probs_up > .192] = 1 # predict class with 0.5 threshold


## SMOTE ##
###########

log_model_smote <-glm(formula = outcome1~rcs(Age,3) + rcs(lesdmax,3) + papnr, family = 'binomial', data = train_smote)
summary(log_model_smote)
log_probs_smote <- predict(log_model_smote, test_set, type = 'response') # get probabilities
log_pred_smote <- rep(0, length(log_probs_smote)) # empty vector for class prediction
log_pred_smote[log_probs_smote > .5] = 1 # predict class with 0.5 threshold
log_pred2_smote <- rep(0, length(log_probs_smote)) # empty vector for class prediction
log_pred2_smote[log_probs_smote > .192] = 1 # predict class with 0.5 threshold


###################################
#### Ridge logistic regression ####
###################################

# In this part the ridge logistic regression models are fitted.

# Function to create a grid of 250 non 0 lambda values on a logarithmic scale
lseq <- function(from=0.001, to=64, length.out=251) {
  exp(seq(log(from), log(to), length.out = length.out))
}

lambdas <-  c(0, lseq()) # create grid by adding 0 to the 250 non-zero values for lambda


## no adjustment ##
###################

x_train <- model.matrix(outcome1 ~rcs(Age,3) + rcs(lesdmax,3) + papnr, train_set)[,-1] # create matrix with predictors
y_train <- train_set$outcome1 # create vector with outcome

# Get hyperparameter
cv_out <- glmnet::cv.glmnet(x = x_train, y= y_train, alpha = 0, lambda = lambdas, 
                            family = 'binomial')

# Fit model
rid_model <- glmnet(x = x_train, y = y_train, alpha = 0, 
                    family = 'binomial',
                    lambda = cv_out$lambda.min)

x_test <- model.matrix(outcome1 ~rcs(Age,3) + rcs(lesdmax,3) + papnr, test_set)[,-1] # create matrix with predictors
y_test <- test_set$outcome1 # create vector with outcome
rid_probs <- predict(rid_model, newx = x_test, type = 'response') # get probabilities

rid_pred <- rep(0, length(rid_probs)) # vector for class prediction
rid_pred[rid_probs > .5] = 1  # predict classes with 0.5 threshold
rid_pred2 <- rep(0, length(rid_probs)) # vector for class prediction
rid_pred2[rid_probs > .192] = 1  # predict classes with 0.5 threshold


## random undersampling ##
##########################

x_train_down <- model.matrix(outcome1 ~rcs(Age,3) + rcs(lesdmax,3) + papnr, train_down)[,-1]# create matrix with predictors as model input
y_train_down <- train_down$outcome1 # create vector with outcome as model input

# Tune hyperparameter
cv_out_down <- glmnet::cv.glmnet(x = x_train_down, y= y_train_down, alpha = 0, 
                                 lambda = lambdas, family = 'binomial')

# Fit model
rid_model_down <- glmnet(x = x_train_down, y = y_train_down, alpha = 0, 
                         family = 'binomial',
                         lambda = cv_out_down$lambda.min)

x_test <- model.matrix(outcome1 ~rcs(Age,3) + rcs(lesdmax,3) + papnr, test_set)[,-1] # create matrix with predictors for predict function
y_test <- test_set$outcome1 # create matrix with predictors for predict function
rid_probs_down <- predict(rid_model_down, newx = x_test, type = 'response') # get probabilities

rid_pred_down <- rep(0, length(rid_probs_down)) # Create vector for class predictions
rid_pred_down[rid_probs_down > .5] = 1 # predict classes with threshold 0.5
rid_pred2_down <- rep(0, length(rid_probs_down)) # Create vector for class predictions
rid_pred2_down[rid_probs_down > .192] = 1 # predict classes with threshold 0.5


## Random oversampling ##
#########################

x_train_up <- model.matrix(outcome1 ~rcs(Age,3) + rcs(lesdmax,3) + papnr, train_up)[,-1]# create matrix with predictors as model input
y_train_up <- train_up$outcome1 # create vector with outcome as model input

# Tune hyperparameter
cv_out_up <- glmnet::cv.glmnet(x = x_train_up, y= y_train_up, alpha = 0, lambda = lambdas, 
                               family = 'binomial')

# Fit model
rid_model_up <- glmnet(x = x_train_up, y = y_train_up, alpha = 0, 
                       family = 'binomial',
                       lambda = cv_out_up$lambda.min)

x_test <- model.matrix(outcome1 ~rcs(Age,3) + rcs(lesdmax,3) + papnr, test_set)[,-1]# create matrix with predictors for predict funtion
y_test <- test_set$outcome1 # create vector with outcome for predict function

rid_probs_up <- predict(rid_model_up, newx = x_test, type = 'response') # get probabilities

rid_pred_up <- rep(0, length(rid_probs_up)) # vector for class predictions
rid_pred_up[rid_probs_up > .5] = 1 # predict class with threshold 0.5
rid_pred2_up <- rep(0, length(rid_probs_up)) # vector for class predictions
rid_pred2_up[rid_probs_up > .192] = 1 # predict class with threshold 0.5


## SMOTE ##
###########

x_train_smote <- model.matrix(outcome1 ~rcs(Age,3) + rcs(lesdmax,3) + papnr, train_smote)[,-1] # create matrix with predictors as model input
y_train_smote <- train_smote$outcome1 # create vector with outcome as model input

# Hyperparameter tuning
cv_out_smote <- glmnet::cv.glmnet(x = x_train_up, y= y_train_up, alpha = 0, 
                                  lambda = lambdas, family = 'binomial')

# Fit model
rid_model_smote <- glmnet(x = x_train_smote, y = y_train_smote, alpha = 0, 
                          family = 'binomial',
                          lambda = cv_out_smote$lambda.min)

x_test <- model.matrix(outcome1 ~rcs(Age,3) + rcs(lesdmax,3) + papnr, test_set)[,-1] # create matrix with predictors for predict function
y_test <- test_set$outcome1 # create vector with output for predict function

rid_probs_smote <- predict(rid_model_smote, newx = x_test, type = 'response')

rid_pred_smote <- rep(0, length(rid_probs_smote)) # Create vector for class prediction
rid_pred_smote[rid_probs_smote > .5] = 1 # predict classes with threshold 0.5
rid_pred2_smote <- rep(0, length(rid_probs_smote)) # Create vector for class prediction
rid_pred2_smote[rid_probs_smote > .192] = 1 # predict classes with threshold 0.5



##############################
#### Test set performance ####
##############################

# In this part of the code, all performance measures are computed and placed in a 
# table. Also, calibration plots are generated to visualize the performance.

# Creating table with all estimated probabilities
probs_table <- cbind(log_probs, log_probs_down, log_probs_up, log_probs_smote,
                     rid_probs, rid_probs_down, rid_probs_up, rid_probs_smote)

# Creating table with all predicted classes
pred_table <- cbind(log_pred, log_pred_down, log_pred_up, log_pred_smote,
                    rid_pred, rid_pred_down, rid_pred_up, rid_pred_smote)
pred2_table <- cbind(log_pred2, log_pred2_down, log_pred2_up, log_pred2_smote,
                    rid_pred2, rid_pred2_down, rid_pred2_up, rid_pred2_smote)

# Creating empty vectors for performance measures
accuracy_vector <- rep(NA, ncol(pred_table))
sensitivity_vector <- rep(NA, ncol(pred_table))
specificity_vector <- rep(NA, ncol(pred_table))
accuracy2_vector <- rep(NA, ncol(pred2_table))
sensitivity2_vector <- rep(NA, ncol(pred2_table))
specificity2_vector <- rep(NA, ncol(pred2_table))

# Loop over all models and imbalance approaches to get performance measures
for (i in 1:ncol(pred_table)){
  accuracy_vector[i] <- accuracy(pred_table[,i], test_outcome)
  sensitivity_vector[i] <- sensitivity(pred_table[,i], test_outcome)
  specificity_vector[i] <- specificity(pred_table[,i], test_outcome)
}
for (i in 1:ncol(pred2_table)){
  accuracy2_vector[i] <- accuracy(pred2_table[,i], test_outcome)
  sensitivity2_vector[i] <- sensitivity(pred2_table[,i], test_outcome)
  specificity2_vector[i] <- specificity(pred2_table[,i], test_outcome)
}

# Create matrix to store calibration measures (intercept & slope + CI)
calibration_matrix <- matrix(ncol = 6, nrow = ncol(probs_table))

# Calculate calibration measures for all models
for (i in 1:ncol(probs_table)) {
  calibration_matrix[i,] <- calibration_ci(probs = probs_table[,i], outcome = test_outcome)
}

# Create matrix to store c statistics + CI
cstat_matrix <- matrix(nrow = ncol(probs_table), ncol = 3)

# Calculate c-statistic + CI
for (i in 1:ncol(probs_table)){
  mat <- cbind(probs_table[,i], test_outcome)
  x <- data.frame(mat) %>% 
    filter(test_outcome == 1)
  y <- data.frame(mat) %>% 
    filter(test_outcome == 0)
  cstat_matrix[i,] <- auRoc::auc.nonpara.mw(x = x[,1], 
                                   y = y[,1],
                                   method = 'pepe')
}

# Bind all results together
results <- cbind(accuracy_vector, sensitivity_vector, specificity_vector,
                 accuracy2_vector, sensitivity2_vector, specificity2_vector,
                 cstat_matrix, calibration_matrix)

# Round results to 2 digits
results <- format(round(results, digits = 2), nsmall = 2)

# Name rows and columns of the results object
colnames(results) <- c("Accuracy (0.5)", "Sensitivity (0.5)", "Specificity (0.5)",
                       "Accuracy (EF)", "Sensitivity (EF)", "Specificity (EF)", "C-statistic",
                       "C-statistic lower", "C-statistic upper", "CIL", 
                       "Lower CIL", "Upper CIL", "Calibration slope", 
                       "Lower slope", "Upper slope")
rownames(results) <- c("LR", "LR down", "LR up", "LR smote", 
                       "Ridge", "Ridge down", "Ridge up", "Ridge smote")

results <- data.frame(results) %>% 
  apply(2, as.character) %>% 
  apply(2, as.numeric)

results <- data.frame(results)

# Loop over results object to get all CI's in parentheses
for (i in 1:nrow(results)){
  results$C.statistic[i] <- str_c(results$C.statistic[i],
                   " (", 
                   results$C.statistic.lower[i],
                   " to ",
                   results$C.statistic.upper[i],
                   ")")
  results$CIL[i] <- str_c(results$CIL[i],
                               " (", 
                               results$Lower.CIL[i],
                               " to ",
                               results$Upper.CIL[i],
                               ")")
  results$Calibration.slope[i] <- str_c(results$Calibration.slope[i],
                               " (", 
                               results$Lower.slope[i],
                               " to ",
                               results$Upper.slope[i],
                               ")")
}

# Remove old CI measure columns
  results <- results %>% 
  select(!c(C.statistic.lower, C.statistic.upper, 
            Upper.CIL, Lower.CIL, 
            Upper.slope, Lower.slope))

# Save results
saveRDS(results, "Ruben van den Goorbergh/cases_study_results.RDS")


## Calibration plots ##
#######################

# using val.prob.ci.2
par(mfrow = c(4,2),
    oma=c(5,5,2,9) + 0.0,
    mar=c(0,0,1,1) + 0.0)
val.prob.ci.2(p = probs_table[,1], y = test_set$outcome1, smooth="loess", legendloc=F, lwd.smooth=2, lwd.ideal=2, lty.ideal=2, dostats = F, axes = F)
axis(side=2,at=c(0,0.2,0.4,0.6,0.8,1))
box(which="plot")
mtext("SLR", side=3, line=1.3)
val.prob.ci.2(p = probs_table[,5], y = test_set$outcome1, smooth="loess", legendloc=F, lwd.smooth=2, lwd.ideal=2, lty.ideal=2, dostats = F, axes = F)
box(which="plot")
mtext("Uncorrected", side=4, las=1, line=1.3)
mtext("Ridge", side=3, line=1.3)
val.prob.ci.2(p = probs_table[,2], y = test_set$outcome1, smooth="loess", legendloc=F, lwd.smooth=2, lwd.ideal=2, lty.ideal=2, dostats = F, axes = F)
axis(side=2,at=c(0,0.2,0.4,0.6,0.8,1))
box(which="plot")
val.prob.ci.2(p = probs_table[,6], y = test_set$outcome1, smooth="loess", legendloc=F, lwd.smooth=2, lwd.ideal=2, lty.ideal=2, dostats = F, axes = F)
box(which="plot")
mtext("RUS", side=4, las=1, line=1.3)
val.prob.ci.2(p = probs_table[,3], y = test_set$outcome1, smooth="loess", legendloc=F, lwd.smooth=2, lwd.ideal=2, lty.ideal=2, dostats = F, axes = F)
axis(side=2,at=c(0,0.2,0.4,0.6,0.8,1))
box(which="plot")
val.prob.ci.2(p = probs_table[,7], y = test_set$outcome1, smooth="loess", legendloc=F, lwd.smooth=2, lwd.ideal=2, lty.ideal=2, dostats = F, axes = F)
box(which="plot")
mtext("ROS", side=4, las=1, line=1.3)
val.prob.ci.2(p = probs_table[,4], y = test_set$outcome1, smooth="loess", legendloc=F, lwd.smooth=2, lwd.ideal=2, lty.ideal=2, dostats = F, axes = F)
axis(side=2,at=c(0,0.2,0.4,0.6,0.8,1))
axis(side=1,at=c(0,0.2,0.4,0.6,0.8,1))
box(which="plot")
val.prob.ci.2(p = probs_table[,8], y = test_set$outcome1, smooth="loess", legendloc=F, lwd.smooth=2, lwd.ideal=2, lty.ideal=2, dostats = F, axes = F)
axis(side=1,at=c(0,0.2,0.4,0.6,0.8,1))
box(which="plot")
mtext("SMOTE", side=4, las=1, line=1.3)
title(xlab = list("Estimated probability",cex=2),
      ylab = list("Observed proportion",cex=2), 
      outer = TRUE)#, line=3)
dev.off()

# using val.prob.ci.2; L2 LR only
par(mfrow = c(2,2),
    oma=c(5,5,0,0) + 0.0,
    mar=c(0,0,1,1) + 0.0)
val.prob.ci.2(p = probs_table[,5], y = test_set$outcome1, smooth="loess", legendloc=F, lwd.smooth=2, lwd.ideal=2, lty.ideal=2, dostats = F, axes = F)
axis(side=2,at=c(0,0.2,0.4,0.6,0.8,1))
box(which="plot")
text(x=0, y=1, adj=0, "Uncorrected", cex=1.3)
val.prob.ci.2(p = probs_table[,6], y = test_set$outcome1, smooth="loess", legendloc=F, lwd.smooth=2, lwd.ideal=2, lty.ideal=2, dostats = F, axes = F)
box(which="plot")
text(x=0, y=1, adj=0, "RUS", cex=1.3)
val.prob.ci.2(p = probs_table[,7], y = test_set$outcome1, smooth="loess", legendloc=F, lwd.smooth=2, lwd.ideal=2, lty.ideal=2, dostats = F, axes = F)
axis(side=1,at=c(0,0.2,0.4,0.6,0.8,1))
axis(side=2,at=c(0,0.2,0.4,0.6,0.8,1))
box(which="plot")
text(x=0, y=1, adj=0, "ROS", cex=1.3)
val.prob.ci.2(p = probs_table[,8], y = test_set$outcome1, smooth="loess", legendloc=F, lwd.smooth=2, lwd.ideal=2, lty.ideal=2, dostats = F, axes = F)
axis(side=1,at=c(0,0.2,0.4,0.6,0.8,1))
box(which="plot")
text(x=0, y=1, adj=0, "SMOTE", cex=1.3)
title(xlab = list("Estimated probability",cex=1.5),
      ylab = list("Observed proportion",cex=1.5), 
      outer = TRUE)#, line=3)
dev.off()

dcdataLR = as.data.frame(cbind(test_set$outcome1,probs_table[,1:4]))
dcdataL2 = as.data.frame(cbind(test_set$outcome1,probs_table[,5:8]))
colnames(dcdataLR) = c("Y", "No", "RUS", "ROS", "SMOTE")
colnames(dcdataL2) = c("Y", "No", "RUS", "ROS", "SMOTE")

LRund <- decision_curve(Y~No, data = dcdataLR, fitted.risk = TRUE, thresholds = seq(0, 1, by = .05), bootstraps = 200) 
LRrusd <- decision_curve(Y~RUS, data = dcdataLR, fitted.risk = TRUE, thresholds = seq(0, 1, by = .05), bootstraps = 200) 
LRrosd <- decision_curve(Y~ROS, data = dcdataLR, fitted.risk = TRUE, thresholds = seq(0, 1, by = .05), bootstraps = 200) 
LRsmoted <- decision_curve(Y~SMOTE, data = dcdataLR, fitted.risk = TRUE, thresholds = seq(0, 1, by = .05), bootstraps = 200) 
L2und <- decision_curve(Y~No, data = dcdataL2, fitted.risk = TRUE, thresholds = seq(0, 1, by = .05), bootstraps = 200) 
L2rusd <- decision_curve(Y~RUS, data = dcdataL2, fitted.risk = TRUE, thresholds = seq(0, 1, by = .05), bootstraps = 200) 
L2rosd <- decision_curve(Y~ROS, data = dcdataL2, fitted.risk = TRUE, thresholds = seq(0, 1, by = .05), bootstraps = 200) 
L2smoted <- decision_curve(Y~SMOTE, data = dcdataL2, fitted.risk = TRUE, thresholds = seq(0, 1, by = .05), bootstraps = 200) 

dev.off()
plot_decision_curve( list(LRund, LRrusd, LRrosd, LRsmoted), 
                     curve.names = c("Uncorrected", "RUS", "ROS", "SMOTE"),
                     col = c("black", "red", "green", "blue"), 
                     ylim = c(-0.1, 0.2), #set ylim
                     xlim = c(0,1),
                     lty = c(1,1,1,1), lwd = c(2,2,2,2), confidence.intervals = FALSE,
                     standardize = FALSE, #plot Net benefit instead of standardized net benefit
                     legend.position = "topright",xlab="Risk threshold",ylab = "Net Benefit", cost.benefit.axis=F) 
plot_decision_curve( list(L2und, L2rusd, L2rosd, L2smoted), 
                     curve.names = c("Uncorrected", "RUS", "ROS", "SMOTE"),
                     col = c("black", "red", "green", "blue"), 
                     ylim = c(-0.1,0.2), #set ylim
                     xlim = c(0,1),
                     lty = c(1,1,1,1), lwd = c(2,2,2,2), confidence.intervals = FALSE,
                     standardize = FALSE, #plot Net benefit instead of standardized net benefit
                     legend.position = "topright",xlab="Probability threshold",ylab = "Net Benefit", cost.benefit.axis=F) 

