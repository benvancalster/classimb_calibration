#############################
### try-out simulate data ###
#############################

################### Setting up #############################
library(MASS)
library(tidyverse)


#### Functions #####

###############
# C-statistic #
###############


fastAUC <- function(p, y) {
  x1 = p[y==1]; n1 = length(x1); 
  x2 = p[y==0]; n2 = length(x2);
  r = rank(c(x1,x2))  
  auc = (sum(r[1:n1]) - n1*(n1+1)/2) / n1 / n2
  return(auc)
}



##########################################
##########################################
##########################################
## Defining a function to get the sum of absolute 
## differences between the preferred and observed 
## values of both the C-statistic and prevalence

func_c <- function(par, dm, n_pred, pref_cstat, pref_prev){
  # par is a vector with initial guesses for both
  # the intercept and the n_pred beta coefficients
  # However since all betas have been restricted to be equal
  # only one value is necessary.
  
  # Providing the beta0 and n_pred beta as specified in the par object. 
  # Again, betas are restricted to be equal.
  dgm_par <- c(par[1], rep(par[2], n_pred)) 
  
  # Obtain values for y based on Bernoulli distribution, with input p
  #system.time(p <- exp(dm %*% dgm_par)/(1+exp(dm %*% dgm_par)))
  p <- 1/(1+exp(-dm %*% dgm_par)) # SAME THING, JUST SLIGHTLY LESS COMPUTATION
  
  #system.time(y <- rbinom(length(p),1,p))
  y <-as.numeric(p>runif(length(p))) # SAME THING, JUST SLIGHTLY FASTER
  
  # Obtain observed values of c-statistic and 
  # average predicted probability of an event
  
  # obs_cstat <- c_stat2(preds = p, outcome = y) # obtain c-statistic based on p and y
  obs_cstat <- fastAUC(p = p, y = y)
  obs_prev <- mean(y) # KEEP IT SIMPLE ;)
  
  # Sum of absolute differences of both values:
  #abs(obs_cstat-pref_cstat) + abs(obs_prev-pref_prev)
  (obs_cstat-pref_cstat)^2 + (obs_prev-pref_prev)^2 # alternative, not sure which one is better
  
}

###################
## check results ##
###################

## A function that checks whether the determined coefficients return
## The preferred C-statistic and prevalence

checking <- function(par){
  ## What do the observed prevalence and c-statistic look like?
  dgm_par <- c(par[1], rep(par[2], n_pred)) 
  
  # Obtain values for y based on Bernoulli distribution, with input p
  p <- plogis(dm %*% dgm_par)
  y <- rbinom(length(p),1,p)
  
  # Obtain observed values
  #obs_cstat <- c_stat2(preds = p, outcome = y) # obtain c-statistic based on p and y
  obs_cstat <- fastAUC(p = p, y = y)
  obs_prev <- mean(y) # THE OBSERVED PREVALENCE IS NOT A FUNCTION OF JUST THE INTERCEPT
  c("cstat" = obs_cstat, "prev" = obs_prev)
  
}


#######################
### data generation ###
#######################

n_pred <- 3
sigma <- matrix(0, ncol = n_pred, nrow = n_pred) # create covariance matrix to be used as input, no covariance, n_pred predictors
diag(sigma) <- 1 # set the diagonal to 1
mu <- rep(0, n_pred) # provide a vector of values for mu
n <- 10000 # setting n
X <- mvrnorm(n = n, mu = mu, Sigma = sigma) # create 3 predictor columns
dm <- cbind(1, X) # Putting the above in a data matrix, including intercept

## Only one run
results_c <- optim(c(-5, 0.2), func_c, dm = dm, pref_cstat = 0.75, pref_prev = 0.01, n_pred = 3, method = "BFGS")
par <- results_c$par
#### 20 repetitions #####
### Comparing on computational time ###


## C-statistic
system.time(reps_c <- replicate(n = 50, optim(c(-5, 0.5), func_c, pref_cstat = 0.75, pref_prev = 0.1, n_pred = 12, method = "BFGS"), simplify = F))

## C-statistic
results_check_c <- apply(sapply(reps_c, '[[', 1), 2, checking)
apply(results_check_c, 1, summary)
# Here the C-statistic varies more than above, the prevalence also shows some deviations. 

# Both have some variation in both estimates, but what 
# So maybe we should use the median coefficients?
par_c <- apply(sapply(reps_c, '[[', 1), 1, median)


#####################################################################
######### Validate results on independent validation set ############
#####################################################################
## Create validation dataset ##
set.seed(111)
n_val <- 100000 # setting n
X_val <- mvrnorm(n = n_val, mu = mu, Sigma = sigma) # create 3 predictor columns
dm_val <- cbind(1, X_val) # Putting the above in a data matrix, including intercept

## Use the function as defined here to check results
checking_val <- function(par, n_pred){
  dgm_par_val <- c(par[1], rep(par[2], n_pred)) 
  
  p_val <- plogis(dm_val %*% dgm_par_val)
  y_val <- rbinom(length(p_val),1,p_val)
  
  # Obtain observed values
  #obs_cstat <- c_stat2(preds = p_val, outcome = y_val) # obtain c-statistic based on p and y
  obs_cstat <- fastAUC(p = p_val, y = y_val)
  obs_prev <- mean(y_val) # THE OBSERVED PREVALENCE IS NOT A FUNCTION OF JUST THE INTERCEPT
  c("cstat" = obs_cstat, "prev" = obs_prev)
}


checking_val(par_c)

## So when taking the median of all coefficients, the results in the validation set are good
## for both approaches. The main difference is that using R^2 is faster, 
## but leads to a bit more variation in the prevalence




####################################
####################################
####################################
## END SCRIPT pt. 1 ##

# To make sure the coefficients are well esitmated, not only do we need different runs,
# but also different data sets to base the estimation on. Because initial parameters
# that are really off may lead to non-finite finite-difference values, also the 
# initial parameters should be estimated.


#####################################
#INSERT ITITIAL PARAMETER ESTIMATION#
#####################################






###############################################################
############## OPTIMIZING BETA FINAL FUNCTION #################
###############################################################

# Create function to optimize coefficients given scenario
optimB <- function(n_pred, pref_prev, par_init, n){
  sigma <- matrix(0, ncol = n_pred, nrow = n_pred) # create covariance matrix to be used as input, no covariance, n_pred predictors
  diag(sigma) <- 1 # set the diagonal to 1
  mu <- rep(0, n_pred) # provide a vector of values for mu
  X <- mvrnorm(n = n, mu = mu, Sigma = sigma) # Sample data for n_pred predictors
  dm <- cbind(1, X) # Putting the above in a data matrix, including intercept
  
  reps_c <- replicate(n = 20, optim(par = par_init, 
                                    fn = func_c,
                                    dm = dm,
                                    pref_cstat = 0.75, 
                                    pref_prev = pref_prev,
                                    n_pred = n_pred,
                                    method = "BFGS", 
                                    hessian = FALSE,
                                    control = list(maxit = 50)),
                                    simplify = F)
  par_c <- apply(sapply(reps_c, '[[', 1), 1, median)
  par_c
}



##########################################################
### Estimating coefficients for different prevalences ####
##########################################################

#### Create matrix with optimized coefficients over multiple data sets
n_pred <- c(3,6,12,24)
pref_prev <- c(0.3, 0.1, 0.01)
scen_temp <- expand.grid(pref_prev, n_pred)
par_all <- matrix(NA, nrow = nrow(scen_temp), ncol = 2)


par_init <- matrix(data = c(-1, 0.5,
                            -2.5, 0.5,
                            -5, 0.5,
                            -1, 0.4,
                            -2.5, 0.4,
                            -5, 0.4,
                            -1, 0.3,
                            -2.5, 0.3,
                            -5, 0.3,
                            -1, 0.2,
                            -2.5, 0.2,
                            -5, 0.2),
                   nrow = 12, ncol = 2, byrow = TRUE)

set.seed(123)
for (j in 1:nrow(par_all)){

  par_sce <- matrix(NA, 20, 2)

    for (i in 1:20){
      par_sce[i,] <- optimB(n_pred = scen_temp[j,2], pref_prev = scen_temp[j,1], par_init =par_init[j,], n = 5000)
    }
par_all[j,] <- apply(par_sce, 2, median)
}

##### 6 pred
for (j in 4:6){
  
  par_sce <- matrix(NA, 20, 2)
  
  for (i in 1:20){
    par_sce[i,] <- optimB(n_pred = scen_temp[j,2], pref_prev = scen_temp[j,1], par_init =par_init[j,], n = 5000)
  }
  par_all[j,] <- apply(par_sce, 2, median)
}

### 12 pred
for (j in 7:9){
  
  par_sce <- matrix(NA, 20, 2)
  
  for (i in 1:20){
    par_sce[i,] <- optimB(n_pred = scen_temp[j,2], pref_prev = scen_temp[j,1], par_init =par_init[j,], n = 5000)
  }
  par_all[j,] <- apply(par_sce, 2, median)
}

save(par_all, file = "betas_2.rData")

load("betas_2.rData")
betas <- par_all
## Check on validation set
## Create validation dataset ##
set.seed(111)
p <- c(3,3,3,6,6,6,12,12,12,24,24,24)

for(i in 1:length(p)){
  sigma <- matrix(0, ncol = p[i], nrow = p[i]) # create covariance matrix to be used as input, no covariance, n_pred predictors
  diag(sigma) <- 1 # set the diagonal to 1
  mu <- rep(0, p[i]) # provide a vector of values for mu
  n_val <- 100000 # setting n
  X_val <- mvrnorm(n = n_val, mu = mu, Sigma = sigma) # create 3 predictor columns
  dm_val <- cbind(1, X_val) # Putting the above in a data matrix, including intercept
  print(checking_val(par_all[i,], n_pred = p[i]))
}





