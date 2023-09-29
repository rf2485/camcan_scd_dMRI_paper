#!/bin/bash
#SBATCH --mail-user=rf2485@nyulangone.org
#SBATCH --mail-type=ALL
#SBATCH --ntasks=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=3-00:00:00

cd tbss
module load fsl/.6.0.6
export LD_LIBRARY_PATH=/lib

tbss_2_reg -T
tbss_3_postreg -S
tbss_4_prestats 0.3

non_FA_list=( MD RD L1 ICVF ISOVF OD )
for metric in ${non_FA_list[@]}; do
  tbss_non_FA ${metric}
done

# cd stats/
# design_ttest2 design 198 127
# cp $FSL_DIR/data/atlases/JHU/JHU-ICBM-labels-1mm.nii.gz .
# fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 37 -uthr 38 -bin lower_cingulum_mask.nii.gz

# metric_list=( FA MD RD L1 ICVF ISOVF OD )
# for metric in ${metric_list[@]}; do
#   mkdir -p ../../roi_diff_means/$metric
#   cp all_${metric}.nii.gz ../../roi_diff_means
#   cd ../../roi_diff_means/
#   fslsplit all_${metric}.nii.gz ${metric}_ -t
#   find . -name ${metric}_0*.nii.gz | head -n 198 | xargs -d $'\n' mv -t $metric/
#   #mv -- ${metric}_0*.nii.gz([1,198]) ${metric}/
#   cd $metric
#
#   for f in ${metric}_0*.nii.gz; do
#     mv "$f" "ctl_$f"
#   done
#
#   cd ../
#   for f in ${metric}_0*.nii.gz; do
#     mv "$f" "${metric}/scd_${f}"
#   done
#   cd ../tbss/stats/
# done