#!/bin/bash

dirS=/Volumes/Research/lazarm03lab/labspace/AD/camcan995/raw

#subject_list='sub-CC110037 sub-CC110045'

for j in $(cut -f1 dwi_over_55.tsv); do
	mkdir dwi_preprocessing/${j}
  cp $dirS/${j}/dwi/${j}_dwi.* dwi_preprocessing/${j}/
  cd dwi_preprocessing/${j}/
	dwidenoise ${j}_dwi.nii.gz dwi_denoised.nii.gz
	
	mrcalc ${j}_dwi.nii.gz dwi_denoised.nii.gz -subtract dwi_residuals.nii.gz
	mrdegibbs dwi_denoised.nii.gz dwi_degibbs.nii.gz

	mcflirt -in dwi_degibbs.nii.gz -out dwi_corr.nii.gz
  fslroi dwi_corr.nii.gz b0 0 1 #pull b0 image from corrected dwi
  fslmaths b0.nii.gz -Tmean b0_mean.nii.gz #mean across time
  bet b0_mean b0_brain -f 0.2 -g 0 -n -m #generate brain mask
  cd ../..
done

