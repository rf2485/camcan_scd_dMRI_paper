#!/bin/zsh
# srun --mem=16G --time=03:00:00 --pty zsh
# module load fsl/6.0.4
# module load matlab/R2023a

basedir=/gpfs/data/lazarlab/CamCan995/derivatives/camcan_scd_dMRI_paper
cd $basedir/tbss/stats/
metric_list=( FA MD RD L1 )

for metric in ${metric_list[@]}; do
  mkdir -p $basedir/roi_diff_means/$metric
  cp all_${metric}.nii.gz $basedir/roi_diff_means
  cd $basedir/roi_diff_means/
  fslsplit all_${metric}.nii.gz ${metric}_ -t
  mv -- ${metric}_0*.nii.gz([1,198]) ${metric}/
  cd $metric

  for f in ${metric}_0*.nii.gz; do
    mv "$f" "ctl_$f"
  done

  cd ../
  for f in ${metric}_0*.nii.gz; do
    mv "$f" "${metric}/scd_${f}"
  done
  cd $basedir/tbss/stats/
done

cd $basedir/roi_diff_means

cp $basedir/tbss/stats/*_mask.nii.gz .

matlab -sd $basedir -batch "roi_diff_means"
