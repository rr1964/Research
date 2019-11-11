#!/bin/bash
#SBATCH --job-name=RealData
#SBATCH --time=2-00:00:00
#SBATCH --node = 1
#SBATCH --ntasks-per-node=3

#SBATCH -o out.%j
#SBATCH -e err.%j

#SBATCH --mail-type=BEGIN,FAIL,END
#SBATCH --mail-user=rreese531@gmail.com

#SBATCH --account=usu-em
#SBATCH --partition=usu-em

module load R/3.3.2
srun --multi-prog RealData.comf
