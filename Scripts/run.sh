#!/usr/bin/env bash
#SBATCH --job-name=lecospec                     # Job name
#SBATCH --mail-type=ALL                         # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=kenneth.bundy@maine.edu     # Where to send mail
#SBATCH --nodes=1                               # Number of Nodes
#SBATCH --ntasks=1                              # Number of copies of job to run
#SBATCH --cpus-per-task=12	                    # Number of cores
#SBATCH --mem-per-cpu=8gb                       # Job memory request
#SBATCH --exclusive                             # hog the node for this job
#SBATCH --time=12:00:00                        # Time limit hrs:min:sec
#SBATCH --output=lecospec_%j.log                # Standard output and error log
#SBATCH --partition=haswell                     # Run on something other than debug

echo "Initializing"
pwd; hostname; date

module load singularity/3.7.1
singularity instance start --writable /home/kbundy/lecospec/ lsContainer


echo "Container Intitialized, Running Lecospec Prediction Pipeline"
srun singularity exec --writable /home/kbundy/lecospec/ cd /lecospec-master && Rscript --verbose ./Scripts/5_Classify_Image1.R
