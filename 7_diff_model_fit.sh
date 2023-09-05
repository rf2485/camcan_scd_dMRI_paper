dirS=/Volumes/Research/lazarm03lab/labspace/AD/camcan995

for j in $(cut -f1 dwi_over_55.tsv); do
	mkdir dtifit/${j}
	dtifit --data=dwi_preprocessing/${j}/dwi_corr.nii.gz \
    --mask=dwi_preprocessing/${j}/b0_brain_mask.nii.gz \
    --bvecs=dwi_preprocessing/${j}/${j}_dwi.bvec \
    --bvals=dwi_preprocessing/${j}/${j}_dwi.bval \
    --out=dtifit/$j/dti
  fslmaths dtifit/$j/dti_L2 -add dtifit/$j/dti_L3 -div 2 dtifit/$j/dti_RD
  python3 amico_noddi.py ${j}
done