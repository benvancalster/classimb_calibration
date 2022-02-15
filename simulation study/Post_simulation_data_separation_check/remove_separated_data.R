# This code is used to bind all separation check results and remove separated
# data sets from the data.

library(tidyverse)
setwd("/<YOURLOCALFILEPATH/Research_archive/Simulation_study/Post_simulation_data_separation_check")

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

# Bind separation results
for (k in 1:8){ # Iterate of combination simulation scenarios
  res <- matrix(NA, nrow = 0, ncol = 4) # Empty matrix to bind results
  for (j in c(scenarios[[k]])){ # Iterate within simulation scenario combination
    scenario <- paste0("scenario ",j) # 
    for (i in 1:2000){ #Iterate over all simulation itteration results within a scenario
      res <- rbind(res, cbind(readRDS(paste("Output/OUT_separation/", j, "/", j,"_", sprintf("%05.0f",i), ".rds", sep = "")), scenario)) # Bind results
    }
  }
  saveRDS(res, paste0("Output/sep_", scenarios[[k]][1], "_",scenarios[[k]][2], "_", scenarios[[k]][3],".RDS")) # save results as RDS file
}

# Check separation, only data separation in RUS data (scenarios 19, 21, 23, 24)
for(i in 1:8){
readRDS(paste0("Output/sep_", scenarios[[i]][1], "_",scenarios[[i]][2], "_", scenarios[[i]][3],".RDS")) %>% 
  group_by(scenario, imbalance) %>% 
  print(summarise(sep = sum(separation == "1")))
}

# Remove iterations with separated RUS data

sep_rus <- matrix(NA, nrow = 0, ncol = 2)

for(i in 1:8){ 
  # Read in separation files and select iterations and scenarios with separated data
  sep_rus <- readRDS(paste0("Output/sep_", scenarios[[i]][1], "_",scenarios[[i]][2], "_", scenarios[[i]][3],".RDS")) %>% 
    filter(imbalance == "RUS") %>% 
    filter(scenario == "scenario 19" | scenario == "scenario 21" | scenario == "scenario 23" | scenario == "scenario 24") %>% 
    filter(separation == "1") %>% 
    select(scenario,iter) 
  
  # Read in original results and filter out separated data
  sep_rm <- readRDS(paste0("Data/res_", scenarios[[i]][1], "_",scenarios[[i]][2], "_", scenarios[[i]][3],".RDS")) %>% 
    filter(!((scenario %in% sep_rus$scenario) & (run %in% sep_rus$iter)))

  # Save new data file with separated data removed
  saveRDS(sep_rm, paste0("Output/data", scenarios[[i]][1], "_",scenarios[[i]][2], "_", scenarios[[i]][3],".RDS"))

  }




        