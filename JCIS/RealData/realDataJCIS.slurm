#!/bin/bash
#SBATCH --job-name=AllChroms
#SBATCH --time=1-09:00:00
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=6

#SBATCH -o out.%j
#SBATCH -e err.%j

#SBATCH --mail-type=BEGIN,FAIL,END
#SBATCH --mail-user=rreese531@gmail.com

#SBATCH --account=usu-em
#SBATCH --partition=usu-em

module load R/3.3.2
srun --multi-prog allChroms.conf
