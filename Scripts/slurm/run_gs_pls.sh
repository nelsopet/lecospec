#!/usr/bin/env bash
#SBATCH --job-name=gs-rf                        # Job name
#SBATCH --mail-type=ALL                         # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=kenneth.bundy@maine.edu     # Where to send mail
#SBATCH --nodes=1                               # Number of Nodes
#SBATCH --ntasks=1                              # Number of copies of job to run
#SBATCH --cpus-per-task=2	                    # Number of cores
#SBATCH --mem=32gb                              # Job memory request
#SBATCH --time=999:00:00                        # Time limit hrs:min:sec
#SBATCH --output=logs/ls_rf_%j.log              # Standard output and error log
#SBATCH --partition=skylake                     # Run on something other than debug

echo "Setting up"
date

module load apptainer
apptainer exec --writable ~/ls/ Rscript Scripts/gs3_pls.R

echo "Grid Search Complete"
date