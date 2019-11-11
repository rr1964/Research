#!/bin/bash
#SBATCH --job-name=MarginalSims1_2
#SBATCH --time=2-00:00:00
#SBATCH --node
#SBATCH --ntasks-per-node=4

#SBATCH -o out.%j
#SBATCH -e err.%j

#SBATCH --mail-type=BEGIN,FAIL,END
#SBATCH --mail-user=rreese531@gmail.com

#SBATCH --account=usu-em
#SBATCH --partition=usu-em

module load R/3.3.2
srun --multi-prog marginalSims1_2.comf
