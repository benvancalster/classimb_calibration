####################################
####### Imbalance solutions ########
####################################

# In this script the functions to handle the imbalance in the data are defined.

# First I write my own ROS and RUS functions. Reason for this is both the capability
# to handle numerical outcome variables and that these functions are fast than
# the upSample and downSample from the caret package.


#### RANDOM OVERSAMPLING FUNCTION ####
# Random oversampling function that upsamples the minority class to the point
# where both classes have the same size. Handles datasets in which all variables
# are numerical.
ros <- function(data) {
  if (mean(data$y) > 0.5) { # check which class is the majority class
    maxClass <- 1
  } else {
    maxClass <- 0
  }
  
  top <- sum(data$y == maxClass) # determine size majority class (to which size the minority class needs to be 'inflated')
  min <- data[data$y != maxClass,] # select minority class data
  ind <- seq(from = 1, to = nrow(min), by = 1) # create vector with row indices minority class to sample from
  x <- sample(ind, size = top - nrow(min), replace = TRUE) # sample from row indices with replacement 
                                                            #with the size being the difference between the minority and majority class
  data <- rbind(data, min[x,]) # combine old data with duplicated of the sampled minority cases
  data # return over sampled data set
}

#### RANDOM UNDERSAMPLING FUNCTION ####
# Random undersampling function that downsamples the majority class to the point
# where both classes have the same size. Handles datasets in which all variables
# are numerical.
rus <- function(data) {
  if (mean(data$y) < 0.5) { # determine which class is the minority class
    minClass <- 1
  } else {
    minClass <- 0
  }
  
  top <- sum(data$y == minClass) # Determine size minority class
  maj <- data[data$y != minClass,] # Select majority class data
  ind <- seq(from = 1, to = nrow(maj), by = 1) # Create vector with indices for the majority class
  x <- sample(ind, size = nrow(maj) - top, replace = FALSE) # sample indices that need to be removed from the original data set
  data <- rbind(data[data$y == minClass,], maj[-x,]) # bind minority class data with remaining majority class data
  data # return undersampled data
}


#### CREATING ALL DATASETS FUNCTION ####
# A function that takes the simulated developmentdata set as an input and returns 4 data sets:
# 3 balanced ones (ROS, RUS & SMOTE) and the original data set. The output is returned in
# the form of a list.

bal_data <- function(data){
  # determine which class is the minority class, used in rus function and to determine
  # k in the smote function.
  
  if (mean(data$y) < 0.5) { 
    minClass <- 1
  } else {
    minClass <- 0
  }
  
  # ROS and RUS
  ros_data <- ros(data)
  rus_data <- rus(data)
  
  # SMOTE 
  x <- data[,-data$y] # store predictors separate from outcome
  y <- data$y # store outcome separate from predictors
  n_minClass <- sum(data$y == minClass)
  if (n_minClass < 6) {
    smote_data <- SMOTE(X = x, target = y, K = (n_minClass-1))
  } else {
    smote_data <- SMOTE(X = x, target = y, K = 5)
  }
  smote_data <- smote_data$data %>% 
    rename(y = class) %>% 
    dplyr::select(y, dplyr::everything()) %>% 
    mutate(y = as.numeric(y))
  
  ALL_data <- list(data, ros_data, rus_data, smote_data)
  names(ALL_data) <- c("unadjusted", "ROS", "RUS", "SMOTE")
  ALL_data
}
#debugonce(bal_data)
 #ALL_data <- bal_data(data[[1]])





