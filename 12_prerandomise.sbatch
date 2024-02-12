#!/bin/bash
#SBATCH --mail-type=ALL
#SBATCH --mail-user=ryn.flaherty@nyulangone.org
#SBATCH -N1 -n1 --mem=32G
#SBATCH --time=12:00:00
#SBATCH -o ./slurm_output/12_prerandomise/slurm-%j.out

#using R
# Rscript fsl_glm_matrices.R

#using FSL
module load fsl/6.0.4
FSL_DIR=/gpfs/share/apps/fsl/6.0.4/
cd tbss
tbss_4_prestats 0.3
cd stats/
Text2Vest scd_age_con.txt scd_age.con
Text2Vest scd_age_mat.txt scd_age.mat
Text2Vest scd_story_con.txt scd_story.con
Text2Vest scd_story_mat.txt scd_story.mat
Text2Vest age_story_con.txt age_story.con
Text2Vest age_story_mat.txt age_story.mat
design_ttest2 design 198 127
# cp $FSL_DIR/data/atlases/JHU/JHU-ICBM-labels-1mm.nii.gz .
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 37 -uthr 38 -bin lower_cingulum_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 37 -uthr 37 -bin r_lower_cingulum_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 38 -uthr 38 -bin l_lower_cingulum_mask.nii.gz
cd ../..
