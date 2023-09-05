#!/bin/bash
#SBATCH --mail-user=rf2485@nyulangone.org
#SBATCH --mail-type=ALL
#SBATCH --time=12:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --array=1-327

dirS=/gpfs/data/lazarlab/CamCan995/raw

subj_list=$(cut -f1 anat_over_55.tsv)
subj_list=($subj_list)
subj_num=$(($SLURM_ARRAY_TASK_ID-1))
subj=${subj_list[$subj_num]}

module load freesurfer/7.4.1
export SUBJECTS_DIR=recon-all-clinical/
recon-all-clinical.sh ${dirS}/${subj}/anat/${subj}_T1w.nii.gz $subj 4 recon-all-clinical
recon-all -segstats -wmparc -balabels -subjid ${subj}