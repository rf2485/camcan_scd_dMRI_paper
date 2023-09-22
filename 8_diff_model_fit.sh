dirS=/Volumes/Research/lazarm03lab/labspace/AD/camcan995

for j in $(cut -f1 dwi_preprocessing/subjectsfile.txt); do
	mkdir -p diff_model_fit/${j}
  if [ ! -f diff_model_fit/$j/dti_RD.nii.gz ]; then
  	dtifit --data=dwi_preprocessing/${j}/dwi_corr.nii.gz \
      --mask=dwi_preprocessing/${j}/b0_brain_mask.nii.gz \
      --bvecs=dwi_preprocessing/${j}/${j}_dwi.bvec \
      --bvals=dwi_preprocessing/${j}/${j}_dwi.bval \
      --out=diff_model_fit/$j/dti
    fslmaths diff_model_fit/$j/dti_L2 -add diff_model_fit/$j/dti_L3 -div 2 diff_model_fit/$j/dti_RD
  fi
  if [ ! -f diff_model_fit/${j}/fit_ODI.nii.gz ]; then
    python3 amico_noddi.py ${j}
    mv dwi_preprocessing/${j}/AMICO/NODDI/* diff_model_fit/${j}/
    rm -r dwi_preprocessing/${j}/AMICO
  fi
done