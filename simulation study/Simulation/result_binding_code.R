#####################################
####### Binding the results #########
#####################################

# Set working directory
setwd("<YOURLOCALFILEPATH>/Research_archive/Simulaiton_study/Simulation")

library(tidyverse)

# Creating a list of all combinations of scenarios with the same sample size and number of predictors
scenarios <- list(c(1,9,17), 
                      c(2,10,18),
                      c(3,11,19),
                      c(4,12,20),
                      c(5,13,21),
                      c(6,14,22),
                      c(7,15,23),
                      c(8,16,24))


for (k in 1:8){ # Iterate of combination simulation scenarios
  res <- matrix(NA, nrow = 0, ncol = 21) # Empty matrix to bind results
for (j in c(scenarios[[k]])){ # Iterate within simulation scenario combination
  scenario <- paste0("scenario ",j) # 
for (i in 1:2000){ #Iterate over all simulation itteration results within a scenario
  res <- rbind(res, cbind(readRDS(paste("Output/OUT/", j, "/", j,"_", sprintf("%05.0f",i), ".rds", sep = "")), scenario)) # Bind results
    }
}
  saveRDS(res, paste0("Output/res_", scenarios[[k]][1], "_",scenarios[[k]][2], "_", scenarios[[k]][3],".RDS")) # save results as RDS file
}
