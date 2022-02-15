#########################################
########## Packages used  ###############
#########################################

# Place hashtags/remove lines that are not applicable to your situation

#################################################################
########## Commands for running on local machine  ###############
#################################################################
# require(MASS)
# require(crayon)
# require(pillar)
# require(dplyr)
# require(magrittr)
# require(randomForest)
# require(glmnet)
# require(smotefamily)

#################################################################
########## Commands for running on HPC ##########################
#################################################################

require("MASS", lib.loc = "/hpc/local/CentOS7/julius_te/R_libs")
require("crayon", lib.loc = "/hpc/local/CentOS7/julius_te/R_libs")
require("pillar", lib.loc = "/hpc/local/CentOS7/julius_te/R_libs")
require("dplyr", lib.loc = "/hpc/local/CentOS7/julius_te/R_libs")
require("magrittr", lib.loc = "/hpc/local/CentOS7/julius_te/R_libs")
require("randomForest", lib.loc = "/hpc/local/CentOS7/julius_te/R_libs")
require("glmnet", lib.loc = "/hpc/local/CentOS7/julius_te/R_libs")
require("FNN", lib.loc = "/hpc/local/CentOS7/julius_te/R_libs")
require("smotefamily", lib.loc = "/hpc/local/CentOS7/julius_te/R_libs")

# Function needed for SMOTE
n_dup_max <-function(size_input,size_P,size_N,dup_size=0)
  {   #Size_P is the number of positive used for generating not actual size of P
    if(is.vector(dup_size)&&length(dup_size)>1)
    {
      if(length(which(dup_size==0))>0)
      {
        sizeM=floor((2*size_N-size_input)/size_P)
      }
      if(length(which(dup_size==0))==0)
      { 
        sizeM=max(dup_size)
      }
    }
    if(!is.vector(dup_size)||length(dup_size)==1)
    {
      if(dup_size==0)
      {
        sizeM=floor((2*size_N-size_input)/size_P)
      }
      if(dup_size!=0)
      {
        sizeM=dup_size
      }
    }
    return(sizeM)	
}
