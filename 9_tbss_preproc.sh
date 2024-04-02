mkdir tbss
mkdir tbss/MD
mkdir tbss/RD
mkdir tbss/L1
  
for j in $(cut -f1 dwi_over_55_ctl.tsv); do
  cp diff_model_fit/$j/dti_FA.nii.gz tbss/ctl_${j}.nii.gz
  cp diff_model_fit/$j/dti_MD.nii.gz tbss/MD/ctl_${j}.nii.gz
  cp diff_model_fit/$j/dti_RD.nii.gz tbss/RD/ctl_${j}.nii.gz
  cp diff_model_fit/$j/dti_L1.nii.gz tbss/L1/ctl_${j}.nii.gz
done

for j in $(cut -f1 dwi_over_55_scd.tsv); do
  cp diff_model_fit/$j/dti_FA.nii.gz tbss/scd_${j}.nii.gz
  cp diff_model_fit/$j/dti_MD.nii.gz tbss/MD/scd_${j}.nii.gz
  cp diff_model_fit/$j/dti_RD.nii.gz tbss/RD/scd_${j}.nii.gz
  cp diff_model_fit/$j/dti_L1.nii.gz tbss/L1/scd_${j}.nii.gz
done

cd tbss
# using fsl
module load fsl/6.0.4
tbss_1_preproc *.nii.gz
firefox FA/slicesdir/index.html
