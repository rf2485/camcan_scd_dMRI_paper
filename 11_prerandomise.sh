Rscript fsl_glm_matrices.R 
cd tbss/stats/
Text2Vest scd_age_con.txt scd_age.con
Text2Vest scd_age_mat.txt scd_age.mat
Text2Vest scd_story_con.txt scd_story.con
Text2Vest scd_story_mat.txt scd_story.mat
Text2Vest age_story_con.txt age_story.con
Text2Vest age_story_mat.txt age_story.mat
design_ttest2 design 198 127
cp $FSL_DIR/data/atlases/JHU/JHU-ICBM-labels-1mm.nii.gz .
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 37 -uthr 38 -bin lower_cingulum_mask.nii.gz