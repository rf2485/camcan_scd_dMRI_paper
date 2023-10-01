#!/bin/bash
#SBATCH --mail-user=rf2485@nyulangone.org
#SBATCH --mail-type=ALL
#SBATCH --time=1:00:00
#SBATCH --mem=8G
#SBATCH --array=1-325

dirS=/gpfs/data/lazarlab/CamCan995/raw/
#
# mkdir -p dwi_preprocessing/
# cut -f1 dwi_over_55.tsv > dwi_preprocessing/subjectsfile.txt
# sed -i '1d' dwi_preprocessing/subjectsfile.txt

module load fsl/6.0.4
module load mrtrix3/3.0
subj_list=$(cut -f1 dwi_preprocessing/subjectsfile.txt)
subj_list=($subj_list)
subj_num=$(($SLURM_ARRAY_TASK_ID-1))
j=${subj_list[$subj_num]}


mkdir -p dwi_preprocessing/${j}
cp $dirS/${j}/dwi/${j}_dwi.* dwi_preprocessing/${j}/
cd dwi_preprocessing/${j}/
dwidenoise ${j}_dwi.nii.gz dwi_denoised.nii

mrcalc ${j}_dwi.nii.gz dwi_denoised.nii -subtract dwi_residuals.nii
mrdegibbs dwi_denoised.nii dwi_degibbs.nii

mcflirt -in dwi_degibbs.nii -out dwi_corr.nii.gz
fslroi dwi_corr.nii.gz b0 0 1 #pull b0 image from corrected dwi
fslmaths b0.nii.gz -Tmean b0_mean.nii.gz #mean across time
bet b0_mean b0_brain -f 0.2 -g 0 -n -m #generate brain mask
cd ../..
