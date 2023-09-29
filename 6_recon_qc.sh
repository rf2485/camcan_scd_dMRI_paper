cd recon-all-clinical/

for subj in $(cut -f1 subjectsfile.txt); do
  echo "--------------${subj}--------------------"
  cd $subj
  freeview -v mri/T1.mgz mri/aparc+aseg.mgz:colormap=lut:opacity=0.2
  cd ..
done