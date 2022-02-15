#########################################
######### DATA GENERATION ###############
#########################################

# In this script the functions a defined that will be used to generate the 
# development and the validation data sets using the parameters specified in the 
# list with simulation scenarios.

# Function for catching errors and warnings. Saves the output in the form of a list of which
# the last element contains the warning.
tryCatch.W.E <- function(expr){
  W <- "NULL"
  w.handler <- function(w){
    W <<- w
    invokeRestart("muffleWarning")}
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e), warning = w.handler),warning = W)
}



## Function to create a vector of coefficients
makeCoefs <- function(coef, n_pred){
  alpha <- coef[1]
  beta <- coef[2]
  coef <- c(alpha, rep(beta, n_pred))
  coef
}

# Function to create argument for data generation from a MVN-distribution (mu & sigma)
MVN_arg <- function(n_pred) {
  mu <- rep(0, n_pred)
  sigma <- matrix(0, nrow = n_pred, ncol = n_pred) # create var-covar matrix for predictor variables
  diag(sigma) <- 1 
  arg <- list(mu, sigma)
  return(arg)
}
  

# Function to generate X
gen_MVN_X <- function(n, mu, sigma){
  gen_X <- mvrnorm(n = n, mu = mu, Sigma = sigma)
  return(gen_X)
}

# Function to generate Y, check for (near) degenerate outcome and bind X and Y
gen_BINOM_Y <- function(gen_X, coef){
  coef <- coef
  dm <- cbind(1, gen_X) # create data matrix including 1 for intercept
 
   ### Generate probabilities and outcomes based on optimized parameters. Check for
  # (near) degenerate outcome distributions.
  p <- plogis(dm %*% coef)
  y <- rbinom(length(p), 1, p)
  
  if (var(y) != 0){
  gen_TOTAl <- cbind(y, gen_X)

  } else {
    warning("Degenerate outcome distribution detected, simulation run aborted")
    
  }
  
  if (any(table(y)<8)) {
    warning("Near degenerate outcome distribution detected, LOOCV is used tuning lambda")
  } 
  
  true_AUC <- fastAUC(p, y)
  return(list(data.frame(gen_TOTAl), true_AUC))
}

# Function to using functions above to get from simulation scenario to data set
gen_TOTAL <- function(simlist){
  n <- simlist$n
  n_pred <- simlist$n_pred
  coef <- makeCoefs(simlist$coef, n_pred)
  
  arg <- MVN_arg(n_pred)
  
  gen_X <- gen_MVN_X(n = n, mu = arg[[1]], sigma = arg[[2]])
  
  gen_BINOM_Y(gen_X, coef)
  
}


#########################################
############# Test script ###############
#########################################

# simlist <- simlist_total$`scenario 24`$development
# data <- gen_TOTAL(simlist_total$`scenario 24`$development)
# val_data <- gen_TOTAL(simlist_total$`scenario 24`$validation)


