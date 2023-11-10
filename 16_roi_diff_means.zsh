#!/bin/zsh
# srun --mem=16G --time=03:00:00 --pty zsh
basedir=/gpfs/data/lazarlab/CamCan995/derivatives/camcan_scd_dMRI_paper
cd $basedir/tbss/stats/
metric_list=( FA MD RD L1 ICVF ISOVF OD )
# module load fsl/6.0.4

for metric in ${metric_list[@]}; do
  mkdir -p $basedir/roi_diff_means/$metric
  cp all_${metric}.nii.gz $basedir/roi_diff_means
  cd $basedir/roi_diff_means/
  fslsplit all_${metric}.nii.gz ${metric}_ -t
  #find . -name ${metric}_0*.nii.gz | head -n 198 | xargs -d $'\n' mv -t $metric/
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

# module load matlab/R2023a
matlab -sd $basedir -batch "roi_diff_means"
