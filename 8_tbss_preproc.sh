mkdir tbss
for j in $(cut -f1 dwi_over_55_ctl.tsv); do
  cp dtifit/$j/dti_FA.nii.gz tbss/ctl_${j}.nii.gz
done

for j in $(cut -f1 dwi_over_55_scd.tsv); do
  cp dtifit/$j/dti_FA.nii.gz tbss/scd_${j}.nii.gz
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