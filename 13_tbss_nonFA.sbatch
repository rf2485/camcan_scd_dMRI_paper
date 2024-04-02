#!/bin/bash
 
#SBATCH --mail-type=ALL
#SBATCH --mail-user=ryn.flaherty@nyulangone.org
#SBATCH -N1 -n1 --mem=64G
#SBATCH --time=12:00:00
#SBATCH --array=1-3
#SBATCH -o ./slurm_output/13_tbss_nonFA/slurm-%A_%a.out

metric_list=(MD RD L1)
metric_num=$(($SLURM_ARRAY_TASK_ID-1))
metric=${metric_list[$metric_num]}

module load fsl/6.0.4
cd tbss
tbss_non_FA ${metric}

