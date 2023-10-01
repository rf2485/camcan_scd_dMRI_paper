#SBATCH --mail-user=rf2485@nyulangone.org
#SBATCH --mail-type=ALL
#SBATCH --ntasks=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=3-00:00:00

module load fsl/6.0.4
mkdir tbss
mkdir tbss/MD
mkdir tbss/RD
mkdir tbss/L1
mkdir tbss/ICVF
mkdir tbss/ISOVF
mkdir tbss/OD
  
for j in $(cut -f1 dwi_over_55_ctl.tsv); do
  cp diff_model_fit/$j/dti_FA.nii.gz tbss/ctl_${j}.nii.gz
  cp diff_model_fit/$j/dti_MD.nii.gz tbss/MD/ctl_${j}.nii.gz
  cp diff_model_fit/$j/dti_RD.nii.gz tbss/RD/ctl_${j}.nii.gz
  cp diff_model_fit/$j/dti_L1.nii.gz tbss/L1/ctl_${j}.nii.gz
  cp diff_model_fit/$j/FIT_ICVF.nii.gz tbss/ICVF/ctl_${j}.nii.gz
  cp diff_model_fit/$j/FIT_ISOVF.nii.gz tbss/ISOVF/ctl_${j}.nii.gz
  cp diff_model_fit/$j/FIT_OD.nii.gz tbss/OD/ctl_${j}.nii.gz
done

for j in $(cut -f1 dwi_over_55_scd.tsv); do
  cp diff_model_fit/$j/dti_FA.nii.gz tbss/scd_${j}.nii.gz
  cp diff_model_fit/$j/dti_MD.nii.gz tbss/MD/scd_${j}.nii.gz
  cp diff_model_fit/$j/dti_RD.nii.gz tbss/RD/scd_${j}.nii.gz
  cp diff_model_fit/$j/dti_L1.nii.gz tbss/L1/scd_${j}.nii.gz
  cp diff_model_fit/$j/FIT_ICVF.nii.gz tbss/ICVF/scd_${j}.nii.gz
  cp diff_model_fit/$j/FIT_ISOVF.nii.gz tbss/ISOVF/scd_${j}.nii.gz
  cp diff_model_fit/$j/FIT_OD.nii.gz tbss/OD/scd_${j}.nii.gz
done

cd tbss
# using fsl
module load fsl/6.0.4
tbss_1_preproc *.nii.gz

tbss_2_reg -T
tbss_3_postreg -S
tbss_4_prestats 0.3

non_FA_list=( MD RD L1 ICVF ISOVF OD )
for metric in ${non_FA_list[@]}; do
  tbss_non_FA ${metric}
done