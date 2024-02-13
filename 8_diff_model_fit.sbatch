#!/bin/bash
#SBATCH --mail-user=rf2485@nyulangone.org
#SBATCH --mail-type=ALL
#SBATCH --time=1:00:00
#SBATCH --mem=8G
#SBATCH --array=1-325
#SBATCH -o ./slurm_output/8_diff_model_fit/slurm-%A_%a.out

module load miniconda3/gpu/4.9.2
# conda create -n amico python=3.7
conda activate amico
# pip install packaging
# pip install numpy
# conda install openblas
# pip install dmri-amico==1.5.4
module load fsl/6.0.4

subj_list=$(cut -f1 dwi_preprocessing/subjectsfile.txt)
subj_list=($subj_list)
subj_num=$(($SLURM_ARRAY_TASK_ID-1))
j=${subj_list[$subj_num]}

basedir=/gpfs/data/lazarlab/CamCan995/derivatives/camcan_scd_dMRI_paper

mkdir -p $basedir/diff_model_fit/${j}

if ! [ -f $basedir/diff_model_fit/$j/dti_RD.nii.gz ]; then
	dtifit --data=$basedir/dwi_preprocessing/${j}/dwi_eddy.nii.gz \
	  --mask=$basedir/dwi_preprocessing/${j}/b0_brain_mask.nii.gz \
	  --bvecs=$basedir/dwi_preprocessing/${j}/${j}_dwi.bvec \
	  --bvals=$basedir/dwi_preprocessing/${j}/${j}_dwi.bval \
	  --out=$basedir/diff_model_fit/$j/dti
	fslmaths $basedir/diff_model_fit/$j/dti_L2 -add $basedir/diff_model_fit/$j/dti_L3 -div 2 $basedir/diff_model_fit/$j/dti_RD
fi

if ! [ -f $basedir/diff_model_fit/${j}/FIT_ISOVF.nii.gz ]; then
	python3 $basedir/amico_noddi.py ${j}
	mv $basedir/dwi_preprocessing/${j}/AMICO/NODDI/* $basedir/diff_model_fit/${j}/
	rm -r $basedir/dwi_preprocessing/${j}/AMICO
fi

