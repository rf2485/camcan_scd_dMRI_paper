#!/bin/bash
#SBATCH --mail-user=rf2485@nyulangone.org
#SBATCH --mail-type=ALL
#SBATCH --time=03:00:00
#SBATCH --mem=16G
#SBATCH --array=1-325
#SBATCH -o ./slurm_output/7_dwi_preproc/slurm-%A_%a.out

dirS=/gpfs/data/lazarlab/CamCan995/raw/
#
# mkdir -p dwi_preprocessing/
# cut -f1 dwi_over_55.tsv > dwi_preprocessing/subjectsfile.txt
# sed -i '1d' dwi_preprocessing/subjectsfile.txt

subj_list=$(cut -f1 dwi_preprocessing/subjectsfile.txt)
subj_list=($subj_list)
subj_num=$(($SLURM_ARRAY_TASK_ID-1))
j=${subj_list[$subj_num]}

module load fsl/6.0.4
module load mrtrix3/3.0
# module load cuda/9.1
# module load freesurfer/7.4.1

mkdir -p dwi_preprocessing/${j}
cp $dirS/${j}/dwi/${j}_dwi.* dwi_preprocessing/${j}/
cd dwi_preprocessing/${j}/

if ! [ -f dwi_degibbs.nii ]; then
	dwidenoise ${j}_dwi.nii.gz dwi_denoised.nii

	mrcalc ${j}_dwi.nii.gz dwi_denoised.nii -subtract dwi_residuals.nii
	mrdegibbs dwi_denoised.nii dwi_degibbs.nii
fi

mcflirt -in dwi_degibbs.nii -out dwi_corr.nii.gz
# dwipreproc dwi_degibbs.nii dwi_eddy.nii -rpe_header -fslgrad ${j}_dwi.bvec ${j}_dwi.bval -json_import ${j}_dwi.json -eddy_options " --slm=linear "
# eddy_correct dwi_degibbs dwi_eddy_correct 0

fslroi dwi_corr.nii.gz b0 0 2 	#pull b0 images from corrected dwi
# fslroi dwi_eddy b0 0 2 
# fslroi dwi_eddy_correct b0 0 2

fslmaths b0 -Tmean b0_mean #mean across time

bet b0_mean b0_brain -f 0.2 -g 0 -n -m #generate brain mask
# mri_synthstrip -i b0_mean.nii.gz -o b0_brain.nii.gz -m b0_brain_mask.nii.gz -g -b 2

fslmaths b0_brain_mask -dilD -fillh -bin b0_brain_mask

cd ../..