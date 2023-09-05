cd roi_diff_means

cp $FSL_DIR/data/atlases/JHU/JHU-ICBM-labels-1mm.nii.gz .
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 37 -uthr 38 -bin lower_cingulum_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 37 -uthr 37 -bin r_lower_cingulum_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 38 -uthr 38 -bin l_lower_cingulum_mask.nii.gz

matlab -sd . -batch "roi_diff_means"
