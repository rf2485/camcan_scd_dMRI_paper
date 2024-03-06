#!/bin/bash
#SBATCH --mail-user=rf2485@nyulangone.org
#SBATCH --mail-type=ALL
#SBATCH --ntasks=1
#SBATCH --tasks-per-node=1
#SBATCH --mem=32G
#SBATCH --time=12:00:00
#SBATCH -o ./slurm_output/11_tbss_postreg/slurm-%j.out

module load fsl/6.0.4
cd tbss
tbss_3_postreg -S

