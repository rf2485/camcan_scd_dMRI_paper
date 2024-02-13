#!/bin/bash

#SBATCH --mail-type=ALL
#SBATCH --mail-user=ryn.flaherty@nyulangone.org
#SBATCH -N1 -n8 --mem=16G
#SBATCH --time=3-00:00:00
#SBATCH -o ./slurm_output/10_tbss_reg/slurm-%j.out

module load condaenvs/gpu/fsl
module load fsl/6.0.4

# export FSLSUB_CONF=/gpfs/home/rf2485/fsl_sub.yml
cd tbss
tbss_2_reg -T
