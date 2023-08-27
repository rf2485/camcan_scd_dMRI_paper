#!/bin/bash

dirS=/Volumes/Research/lazarm03lab/labspace/AD/camcan995/
cd ${dirS}
#subject_list='sub-CC110037 sub-CC110045'

for j in $(cut -f1 source_materials/imaging/dwi/participants.tsv); do
	mkdir derivatives/dwi_preprocessing/${j}
  cp raw/${j}/dwi/${j}_dwi.* derivatives/dwi_preprocessing/${j}/
  cd derivatives/dwi_preprocessing/${j}/
	dwidenoise ${j}_dwi.nii.gz dwi_denoised.nii.gz
	
	mrcalc ${j}_dwi.nii.gz dwi_denoised.nii.gz -subtract dwi_residuals.nii.gz
	mrdegibbs dwi_denoised.nii.gz dwi_degibbs.nii.gz

	mcflirt -in dwi_degibbs.nii.gz -out dwi_corr.nii.gz
  cd ${dirS}
done

