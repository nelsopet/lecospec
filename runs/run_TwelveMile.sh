#!/usr/bin/env bash
#SBATCH --job-name=lecospec                       # Job name
#SBATCH --mail-type=ALL                         # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=kenneth.bundy@maine.edu     # Where to send mail
#SBATCH --nodes=1                               # Number of Nodes
#SBATCH --ntasks=1                              # Number of copies of job to run
#SBATCH --time=120:00:00                        # Time limit hrs:min:sec
#SBATCH --exclusive                             # steal the entire node for more power
#SBATCH --output=bison_gulch_%j.log               # Standard output and error log
#SBATCH --partition=haswell                     # Run on something other than debug



echo "Setting up..."
module load singularity/3.7.1
INPUTPATH=/home/kbundy/lsData/
OUTPUTPATH=/home/kbundy/lecospec-master/

echo "Loading .tif files from $INPUTPATH"
echo "Outputting files to $OUTPUTPATH"
export SINGULARITY_BIND="$INPUTPATH,$OUTPUTPATH,/home/kbundy/"
echo "Exporting Environment variables: $SINGULARITY_BIND"
echo "Processing..."
srun singularity exec --bind $SINGULARITY_BIND /home/kbundy/lecospec Rscript --verbose /home/kbundy/lecospec-master/runs/run_TwelveMile.R