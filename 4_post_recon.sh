export SUBJECTS_DIR=./

cut -f1 anat_over_55.tsv > recon-all-clinical/subjectsfile.txt
cd recon-all-clinical
sed -i '' '1d' subjectsfile.txt

#generate stats tables with Freesurfer
aparcstats2table --subjectsfile=subjectsfile.txt --hemi lh --tablefile=lh_aparctable.tsv --measure=thickness --common-parcs
aparcstats2table --subjectsfile=subjectsfile.txt --hemi rh --tablefile=rh_aparctable.tsv --measure=thickness --common-parcs
asegstats2table --subjectsfile=subjectsfile.txt --tablefile=asegtable.tsv --common-segs
asegstats2table --subjectsfile=subjectsfile.txt --stats=wmparc.stats --tablefile=wmparctable.tsv --common-segs

qatools.py --subjects_dir ./ --output_dir ../recon_qc --screenshots --fornix --outlier