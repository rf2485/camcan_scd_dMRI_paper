#!/bin/bash

#SBATCH --mail-type=ALL
#SBATCH --mail-user=ryn.flaherty@nyulangone.org
#SBATCH --mem=32G
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --time=7-00:00:00
#SBATCH --gres=gpu:a100:4

module load cuda/11.8
module load freesurfer/7.3.2


for j in $(cut -f1 ../anat_over_55.tsv)
mri_synthseg --i $dirS/raw --o $dirS/segmentations --parc --vol $dirS/volumes.csv --qc $dirS/qc.csv --post $dirS/posteriors

