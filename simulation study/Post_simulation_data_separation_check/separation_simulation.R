####################################################
############## Simulation run script ###############
####################################################

# In this script a function is defined and called to run the full simulation using a loop.

setwd("/<YOURLOCALFILEPATH/Research_archive/Simulation_study/Post_simulation_data_separation_check")

#####################################################
############ Sourcing all files needed #############
####################################################
# Source all files needed for successfully running the code

source("Script/dependencies.R")
source("Script/performance_measures_wo_eci.R")
source("Script/data_generation.R")
source("Script/imbalance_functions.R")
source("Script/fitting_models_wo_xgb.R")
source("Script/model_validation_wo_xgb_ECI.R")
source("Script/single_run_wo_eci.R")
source("Script/separation_detection.R")


####################################################
###### loading seeds and simulation scenarios ######
####################################################
# Load seeds and list with all simulation scenarios
scen_seed <- readRDS("scen_seed.RDS")
seed <- readRDS("seed.RDS")
simlist_total <- readRDS('simlist_total.RDS')



c_args <- commandArgs(trailingOnly = T)
scenario <- as.numeric(c_args[1]) # scenario
n <- as.numeric(c_args[2]) # Specify number of iterations 


# This function runs the whole simulation. It takes the list with all simulation scenarios,
# the number of the scenario, the number of iterations (n), vector of scenario seeds (for simulating the validation set),
# and the matrix of seeds for all iterations of all scenarios as input.
# The output is saved per scenario per iteration (see single_run.R).

sim_func <- function(simlist_total, scenario, seed, n = n){
  
  for (i in 1:n){
    sep_check_all(simlist_total = simlist_total, scenario = scenario, iter = i, seed = seed)
  }
}

####################################################
############### Run the simulation #################
####################################################


sim_func(simlist_total = simlist_total, scenario = scenario, seed = seed[,scenario], n = n)



