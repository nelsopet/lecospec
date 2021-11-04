#!/usr/bin/env bash
#SBATCH --job-name=convert                    # Job name
#SBATCH --mail-type=ALL                         # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=kenneth.bundy@maine.edu     # Where to send mail
#SBATCH --nodes=1                               # Number of Nodes
#SBATCH --ntasks=1                              # Number of copies of job to run
#SBATCH --exclusive                             # hog the node for this job
#SBATCH --time=24:00:00                        # Time limit hrs:min:sec
#SBATCH --output=lecospec_%j.log                # Standard output and error log
#SBATCH --partition=haswell                     # Run on something other than debug

echo "Initializing"
pwd; hostname; date
module load singularity/3.7.1
echo "Running Lecospec Prediction Pipeline"
srun singularity exec --writable /home/kbundy/lecospec/ Rscript --verbose /home/kbundy/lecospec-master/mle/conversion_test.R
echo "Execution Completed"