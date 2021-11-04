#!/usr/bin/env bash
#SBATCH --job-name=lecospec                      # Job name
#SBATCH --mail-type=ALL                         # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=kenneth.bundy@maine.edu     # Where to send mail
#SBATCH --nodes=6                               # Number of Nodes
#SBATCH --ntasks=1                              # Number of copies of job to run
#SBATCH --time=120:00:00                        # Time limit hrs:min:sec
#SBATCH --exclusive                             # steal the entire node for more power
#SBATCH --output=lecospec_%j.log               # Standard output and error log
#SBATCH --partition=haswell                     # Run on something other than debug


echo "Setting up..."
module load singularity/3.7.1

DATAPATH=/home/kbundy/lsData/
CODEPATH=/home/kbundy/lecospec-master/

echo "Loading .tif files from $DATAPATH"
echo "Outputting files to $CODEPATH"
export SINGULARITY_BIND="$DATAPATH,$CODEPATH"
echo "Exporting Environment variables: $SINGULARITY_BIND"

echo "Processing..."
srun singularity exec /home/kbundy/lecospec Rscript --verbose /home/kbundy/lecospec-master/runs/run_chatnika.R
srun singularity exec /home/kbundy/lecospec Rscript --verbose /home/kbundy/lecospec-master/runs/run_EagleSummit.R
srun singularity exec /home/kbundy/lecospec Rscript --verbose /home/kbundy/lecospec-master/runs/run_EightMile.R
srun singularity exec /home/kbundy/lecospec Rscript --verbose /home/kbundy/lecospec-master/runs/run_LittleLake.R
srun singularity exec /home/kbundy/lecospec Rscript --verbose /home/kbundy/lecospec-master/runs/run_MaskedLittleLake.R
srun singularity exec /home/kbundy/lecospec Rscript --verbose /home/kbundy/lecospec-master/runs/run_MurphyDome.R
