import os
os.environ['KMP_DUPLICATE_LIB_OK']='True'
import amico
import sys
study="dwi_preprocessing/"
subject=sys.argv[1]
amico.core.setup()
ae = amico.Evaluation(study, subject)
bvals = os.path.join(study, subject, subject+"_dwi.bval")
bvecs = os.path.join(study, subject, subject+"_dwi.bvec")
amico.util.fsl2scheme(bvals, bvecs)

ae.load_data(dwi_filename = "dwi_corr.nii.gz", scheme_filename = subject+"_dwi.scheme", mask_filename = "b0_brain_mask.nii.gz", b0_thr = 0)
ae.set_model("NODDI")
ae.generate_kernels(regenerate=True)
ae.load_kernels()
ae.fit()
ae.save_results()