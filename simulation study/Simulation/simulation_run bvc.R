####################################################
############## Simulation run script ###############
####################################################

# In this script a function is defined and called to run the full simulation using a loop.

 setwd("c:\\Ben\\MStat thesis proposals\\Utrecht 2020\\Ruben van den Goorbergh\\Simulation code")

####################################################
############ Sourcing all files needed #############
####################################################
# Source all files needed for successfully running the code

source("dependencies.R")
source("performance_measures_wo_eci.R")
source("data_generation.R")
source("imbalance_functions.R")
source("fitting_models_wo_xgb.R")
source("model_validation_wo_xgb_ECI.R")
source("single_run_wo_eci.R")


####################################################
###### loading seeds and simulation scenarios ######
####################################################
# Load seeds and list with all simulation scenarios
scen_seed <- readRDS("scen_seed.RDS")
seed <- readRDS("seed.RDS")
simlist_total <- readRDS('simlist_total.RDS')



c_args <- commandArgs(trailingOnly = T)
scenario <- as.numeric(c_args[1]) # scenario
#n <- as.numeric(c_args[2]) # Specify number of iterations 


# This function runs the whole simulation. It takes the list with all simulation scenarios,
# the number of the scenario, the number of iterations (n), vector of scenario seeds (for simulating the validation set),
# and the matrix of seeds for all iterations of all scenarios as input.
# The output is saved per scenario per iteration (see single_run.R).

sim_func <- function(simlist_total, scenario, scen_seed, seed){
set.seed(scen_seed)  
simlist_val <- simlist_total[[scenario]]$validation
val_data <- gen_TOTAL(simlist = simlist_val)[[1]]

for (i in 1:2000){
single_run(simlist_total, val_data, scenario, iter  = i, seed = seed)
  }
}

####################################################
############### Run the simulation #################
####################################################



scen_seed_current <- scen_seed[scenario]  
system.time(sim_func(simlist_total, scenario = scenario, scen_seed = scen_seed_current, seed = seed[,scenario]))




