mkdir tbss
mkdir tbss/MD
mkdir tbss/RD
mkdir tbss/L1
mkdir tbss/ICVF
mkdir tbss/ISOVF
mkdir tbss/OD
  
for j in $(cut -f1 dwi_over_55_ctl.tsv); do
  cp dtifit/$j/dti_FA.nii.gz tbss/ctl_${j}.nii.gz
  cp dtifit/$j/dti_MD.nii.gz tbss/MD/ctl_${j}.nii.gz
  cp dtifit/$j/dti_RD.nii.gz tbss/RD/ctl_${j}.nii.gz
  cp dtifit/$j/dti_L1.nii.gz tbss/L1/ctl_${j}.nii.gz
  cp AMICO/$j/NODDI/FIT_ICVF.nii.gz tbss/ICVF/ctl_${j}.nii.gz
  cp AMICO/$j/NODDI/FIT_ISOVF.nii.gz tbss/ISOVF/ctl_${j}.nii.gz
  cp AMICO/$j/NODDI/FIT_OD.nii.gz tbss/OD/ctl_${j}.nii.gz
done

for j in $(cut -f1 dwi_over_55_scd.tsv); do
  cp dtifit/$j/dti_FA.nii.gz tbss/scd_${j}.nii.gz
  cp dtifit/$j/dti_MD.nii.gz tbss/MD/scd_${j}.nii.gz
  cp dtifit/$j/dti_RD.nii.gz tbss/RD/scd_${j}.nii.gz
  cp dtifit/$j/dti_L1.nii.gz tbss/L1/scd_${j}.nii.gz
  cp AMICO/$j/NODDI/FIT_ICVF.nii.gz tbss/ICVF/scd_${j}.nii.gz
  cp AMICO/$j/NODDI/FIT_ISOVF.nii.gz tbss/ISOVF/scd_${j}.nii.gz
  cp AMICO/$j/NODDI/FIT_OD.nii.gz tbss/OD/scd_${j}.nii.gz
done

cd tbss
# using fsl
tbss_1_preproc *.nii.gz
cd slicesdir
open ./index.html
# quality control processed FA images
#if any images fail QC (ie large parts of brain missing)
#on local
# cd ..
# rm -r sliicesdir
# cp origdata/*.nii.gz .
# rm -r origdata
# rm [problem image].nii.gz
#repeat step 1 as necessary until there are no problem images