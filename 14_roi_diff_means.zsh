#!/bin/zsh
basedir=/Volumes/Research/lazarm03lab/labspace/AD/camcan995/derivatives/camcan_scd_dMRI_paper
# cd $basedir/tbss/stats/
# metric_list=( FA MD RD L1 ICVF ISOVF OD )
# for metric in ${metric_list[@]}; do
#   mkdir -p $basedir/roi_diff_means/$metric
#   cp all_${metric}.nii.gz $basedir/roi_diff_means
#   cd $basedir/roi_diff_means/
#   fslsplit all_${metric}.nii.gz ${metric}_ -t
#   #find . -name ${metric}_0*.nii.gz | head -n 198 | xargs -d $'\n' mv -t $metric/
#   mv -- ${metric}_0*.nii.gz([1,198]) ${metric}/
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
#   cd $basedir/tbss/stats/
# done

cd $basedir/roi_diff_means

cp $FSL_DIR/data/atlases/JHU/JHU-ICBM-labels-1mm.nii.gz .
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 37 -uthr 38 -bin lower_cingulum_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 37 -uthr 37 -bin r_lower_cingulum_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 38 -uthr 38 -bin l_lower_cingulum_mask.nii.gz
rm JHU-ICBM-labels-1mm.nii.gz

matlab -sd $basedir -batch "roi_diff_means"
