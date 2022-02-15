#### Generation of simlists ####

# In this code a list of lists is created containing all simulation scenarios,
# split up in a list for the development sets and a list for the validation set
# per scenario.

setwd('/<YOURLOCALFILEPATH>/Simulation_study/Pre_simulation')
load('/<YOURLOCALFILEPATH>/Simulation_study/Pre_simulation/Output/betas_2.rData') # Loading regression coefficients for different scenarios

############################################### 
#### Simlutation scenarios devlopment sets ####
############################################### 
n <- c(2500, 5000) # sample size
n_pred <- c(3,6,12, 24) # number of predictors
prev <- c(0.3, 0.1, 0.01) # prevalence
development<- cross(list(n = n, n_pred = n_pred, prev = prev)) # Create list with all development scenarios


# Add coefficients to development
for(i in 1:length(development)){
if (development[[i]]$n_pred == 3 & development[[i]]$prev == 0.3){
  development[[i]]$coef <-  par_all[1,]
} else if (development[[i]]$n_pred == 3 & development[[i]]$prev == 0.1){
  development[[i]]$coef <-  par_all[2,]
    }else if (development[[i]]$n_pred == 3 & development[[i]]$prev == 0.01){
  development[[i]]$coef <-  par_all[3,]
      } else if (development[[i]]$n_pred == 6 & development[[i]]$prev == 0.3){
    development[[i]]$coef <-  par_all[4,]
        } else if (development[[i]]$n_pred == 6 & development[[i]]$prev == 0.1){
     development[[i]]$coef <-  par_all[5,] 
          } else if (development[[i]]$n_pred == 6 & development[[i]]$prev == 0.01){
      development[[i]]$coef <-  par_all[6,]
           }else if (development[[i]]$n_pred == 12 & development[[i]]$prev == 0.3){
        development[[i]]$coef <-  par_all[7,]
            }else if (development[[i]]$n_pred == 12 & development[[i]]$prev == 0.1){
          development[[i]]$coef <-  par_all[8,]
              }else if (development[[i]]$n_pred == 12 & development[[i]]$prev == 0.01){
             development[[i]]$coef <-  par_all[9,]
                } else if (development[[i]]$n_pred == 24 & development[[i]]$prev == 0.3){
               development[[i]]$coef <-  par_all[10,]
                  }else if (development[[i]]$n_pred == 24 & development[[i]]$prev == 0.1){
               development[[i]]$coef <-  par_all[11,]
                    }else if (development[[i]]$n_pred == 24 & development[[i]]$prev == 0.01){
               development[[i]]$coef <-  par_all[12,]
}}


## Naming simulation scenarios
list_names_dev <- rep(NA, length(development))
  
for(i in 1:length(development)){
  list_names_dev[i] <- (paste('development'))
}
names(development) <- list_names_dev

############################################### 
#### Simlutation scenarios validation sets ####
############################################### 

n <- c(rep(5000, length(n))) # N
n_pred <- c(3,6,12, 24) # number of predictors
prev <- c(0.3, 0.1, 0.01) # prevalence
validation <- cross(list(n = n, n_pred = n_pred, prev = prev)) # Create list for all validation scenarios

# Add coefficients to validation
for(i in 1:length(validation)){
  if (validation[[i]]$n_pred == 3 & validation[[i]]$prev == 0.3){
    validation[[i]]$coef <-  par_all[1,]
    } else if (validation[[i]]$n_pred == 3 & validation[[i]]$prev == 0.1){
       validation[[i]]$coef <-  par_all[2,]
      }else if (validation[[i]]$n_pred == 3 & validation[[i]]$prev == 0.01){
         validation[[i]]$coef <-  par_all[3,]
        } else if (validation[[i]]$n_pred == 6 & validation[[i]]$prev == 0.3){
          validation[[i]]$coef <-  par_all[4,]
          } else if (validation[[i]]$n_pred == 6 & validation[[i]]$prev == 0.1){
           validation[[i]]$coef <-  par_all[5,] 
            } else if (validation[[i]]$n_pred == 6 & validation[[i]]$prev == 0.01){
           validation[[i]]$coef <-  par_all[6,]
              }else if (validation[[i]]$n_pred == 12 & validation[[i]]$prev == 0.3){
           validation[[i]]$coef <-  par_all[7,]
                }else if (validation[[i]]$n_pred == 12 & validation[[i]]$prev == 0.1){
            validation[[i]]$coef <-  par_all[8,]
                  }else if (validation[[i]]$n_pred == 12 & validation[[i]]$prev == 0.01){
              validation[[i]]$coef <-  par_all[9,]
                    }else if (validation[[i]]$n_pred == 24 & validation[[i]]$prev == 0.3){
                validation[[i]]$coef <-  par_all[10,]
                      }else if (validation[[i]]$n_pred == 24 & validation[[i]]$prev == 0.1){
                  validation[[i]]$coef <-  par_all[11,]
                        }else if (validation[[i]]$n_pred == 24 & validation[[i]]$prev == 0.01){
                    validation[[i]]$coef <-  par_all[12,]
  }}

## Naming simulation scenarios
list_names_val <- rep(NA, length(validation))

for(i in 1:length(validation)){
  list_names_val[i] <- (paste('validation'))
}
names(validation) <- list_names_val

############################################### 
################ Merging lists ################
###############################################
simlist_total <- rep(list(NA), 24)

for (i in 1:length(development)){
  simlist_total[[i]] <- c(development[i], validation[i])
}

## Naming simulation scenarios
list_names <- rep(NA, length(simlist_total))

for(i in 1:length(validation)){
  list_names[i] <- (paste('scenario', i))
}
names(simlist_total) <- list_names
saveRDS(simlist_total, "Output/simlist_total.RDS")


