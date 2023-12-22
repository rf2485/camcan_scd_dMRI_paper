README
================

This is the code used for data processing and statistical analyses in
Flaherty, R., et al. (2023). *Diffusion Imaging Markers of Accelerated
Neuronal Aging of the Lower Cingulum in Subjective Cognitive Decline*
\[Manuscript submitted for publication\]. Department of Radiology, New
York University School of Medicine.

## Directions

Running this code requires the following dependencies:

- RStudio \>= 2023.09.1+494 with the following packages:
  - base \>= 4.3.2
  - tidyverse \>= 2.0.0
  - arsenal \>= 3.6.3
  - ggpmisc \>= 0.5.4-1
  - interactions \>= 1.1.5
  - ggtext \>= 0.1.2
- FreeSurfer == 7.4.1
- FSL == 6.0.4
- MRTrix3 == 3.0
- python == 3.7 with package dmri-amico == 1.5.4 (more detailed
  installation instructions for AMICO are available on [their
  github](https://github.com/daducci/AMICO/wiki/How-to-install-AMICO))
- zsh
- MatLab \>= 2023a with Image Processing Toolbox

Scripts with .sbatch extensions are written for submission to a SLURM
batch processing system on a HPC. It is highly recommended to conduct
this analysis on an HPC. Scripts with .sh extensions are written for
Mate Desktop and can be run with either bash or zsh. Scripts with .zsh 
extensions can only be run with zsh.

Run each numbered script in order. Unnumbered scripts are called by the
numbered scripts and do not need to be called manually. Wait until the
script finishes before starting the next numbered script.

The original analysis was conducted on Red Hat Enterprise Linux Server
release 7.4.

## License

Shield: [![CC BY-SA
4.0](https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg)](http://creativecommons.org/licenses/by-sa/4.0/)

This work is licensed under a [Creative Commons Attribution-ShareAlike
4.0 International
License](http://creativecommons.org/licenses/by-sa/4.0/).

[![CC BY-SA
4.0](https://licensebuttons.net/l/by-sa/4.0/88x31.png)](http://creativecommons.org/licenses/by-sa/4.0/)
