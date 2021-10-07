#!/usr/bin/env bash
#SBATCH --job-name=lecospec                      # Job name
#SBATCH --mail-type=ALL                         # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=kenneth.bundy@maine.edu     # Where to send mail
#SBATCH --nodes=1                               # Number of Nodes
#SBATCH --ntasks=1                              # Number of copies of job to run
#SBATCH --time=120:00:00                        # Time limit hrs:min:sec
#SBATCH --exclusive                             # steal the entire node for more power
#SBATCH --output=lecospec_%j.log               # Standard output and error log
#SBATCH --partition=haswell                     # Run on something other than debug



echo "Setting up..."
module load singularity/3.7.1

echo "Processing..."
srun singularity exec --writable /home/kbundy/lecospec Rscript --verbose /home/kbundy/lecospec-master/runs/run_template.R