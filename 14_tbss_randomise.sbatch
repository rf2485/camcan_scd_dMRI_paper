#!/bin/bash

#SBATCH --mail-user=ryn.flaherty@nyulangone.org
#SBATCH --mail-type=ALL
#SBATCH --mem=64G
#SBATCH --time=12:00:00
#SBATCH --array=1-7
#SBATCH -o ./slurm_output/14_tbss_randomise/slurm-%A_%a.out

metric_list=(FA MD RD L1)
metric_num=$(($SLURM_ARRAY_TASK_ID-1))
metric=${metric_list[$metric_num]}

module load fsl/6.0.4
cd tbss/stats
mkdir $metric/

test_list=(scd_story scd_age age_story)
for test in "${test_list[@]}"; do
  # Text2Vest ${test}_con.txt ${test}.con
  # Text2Vest ${test}_mat.txt ${test}.mat
  randomise_parallel -i all_${metric} -o $metric/${metric}_lc_${test} -m lower_cingulum_mask -d ${test}.mat -t ${test}.con -n 500 -T
  randomise_parallel -i all_${metric} -o $metric/${metric}_rlc_${test} -m r_lower_cingulum_mask -d ${test}.mat -t ${test}.con -n 500 -T
  randomise_parallel -i all_${metric} -o $metric/${metric}_llc_${test} -m l_lower_cingulum_mask -d ${test}.mat -t ${test}.con -n 500 -T
done


