Rscript fsl_glm_matrices.R 
cd tbss/stats/
design_ttest2 design 198 127
cp $FSL_DIR/data/atlases/JHU/JHU-ICBM-labels-1mm.nii.gz .
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 37 -uthr 38 -bin lower_cingulum_mask.nii.gz