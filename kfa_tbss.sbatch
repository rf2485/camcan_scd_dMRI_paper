#!/bin/bash
 
#SBATCH --mail-type=ALL
#SBATCH --mail-user=ryn.flaherty@nyulangone.org
#SBATCH -N1 -n1 --mem=64G
#SBATCH --time=12:00:00
#SBATCH -o ./slurm_output/kfa_tbss/slurm-%j.out

metric=KFA
module load fsl/6.0.4
cd tbss
tbss_non_FA ${metric}

test_list=(scd_story scd_age age_story)
for test in "${test_list[@]}"; do
randomise_parallel -i all_${metric} -o $metric/${metric}_lc_${test} -m lower_cingulum_mask -d ${test}.mat -t ${test}.con -n 500 -T
  randomise_parallel -i all_${metric} -o $metric/${metric}_rlc_${test} -m r_lower_cingulum_mask -d ${test}.mat -t ${test}.con -n 500 -T
  randomise_parallel -i all_${metric} -o $metric/${metric}_llc_${test} -m l_lower_cingulum_mask -d ${test}.mat -t ${test}.con -n 500 -T
done
