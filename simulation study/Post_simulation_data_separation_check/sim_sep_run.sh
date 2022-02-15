#!/bin/sh
#!/bin/bash
#SBATCH --time=60:00:00
#SBATCH --mem=10G
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=r.vandengoorbergh@gmail.com
#SBATCH --array=1-24



/hpc/local/CentOS7/julius_te/R-3.6.2/bin/bin/Rscript /home/julius_te/rvandengoorbergh/simulation_separation/separation_simulation.R $SLURM_ARRAY_TASK_ID 3






