dirS=/Volumes/Research/lazarm03lab/labspace/AD/camcan995

for j in $(cut -f1 dwi_preprocessing/subjectsfile.txt); do
	mkdir -p dtifit/${j}
  if [ ! -f dtifit/$j/dti_RD ]; then
  	dtifit --data=dwi_preprocessing/${j}/dwi_corr.nii.gz \
      --mask=dwi_preprocessing/${j}/b0_brain_mask.nii.gz \
      --bvecs=dwi_preprocessing/${j}/${j}_dwi.bvec \
      --bvals=dwi_preprocessing/${j}/${j}_dwi.bval \
      --out=dtifit/$j/dti
    fslmaths dtifit/$j/dti_L2 -add dtifit/$j/dti_L3 -div 2 dtifit/$j/dti_RD
  fi
  if [ ! -f dtifit/${j}/FIT_OD.nii.gz ]; then
    python3 amico_noddi.py ${j}
    mv dwi_preprocessing/${j}/AMICO/NODDI/* dtifit/${j}/
    rm -r dwi_preprocessing/${j}/AMICO
  fi
done