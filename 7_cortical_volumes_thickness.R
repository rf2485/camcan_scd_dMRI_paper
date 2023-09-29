#run synthseg, recon-all-clinical, and aparcstats2table first
library(tidyverse)
library(arsenal)

#import aseg stats table (gray matter volumes)
aseg = read_tsv("recon-all-clinical/asegtable.tsv") %>%
  rename(participant_id=`Measure:volume`) %>%
  mutate(across(c(2:ncol(.)), .fns = ~.*1000/EstimatedTotalIntraCranialVol)) #normalize by intracranial volume
#import left aparc stats table (cortical thickness)
lh_aparc = read_tsv("recon-all-clinical/lh_aparctable.tsv") %>%
  rename(participant_id=lh.aparc.thickness)
#import right aparc stats table (cortical thickness)
rh_aparc = read_tsv("recon-all-clinical/rh_aparctable.tsv") %>%
  rename(participant_id=rh.aparc.thickness)
#import wmparc stats table (white matter volumes)
wmparc = read_tsv("recon-all-clinical/wmparctable.tsv") %>%
  rename(participant_id=`Measure:volume`) %>%
  mutate(across(c(2:ncol(.)), .fns = ~.*1000/EstimatedTotalIntraCranialVol)) #normalize by intracranial volume
#import the demographics and testing data of participants over the age of 55 with anatomical MRI scans
anat_over_55 = read.delim("anat_over_55.tsv", tryLogical = FALSE) %>%
  select(participant_id, SCD) #select participant ID and SCD status

#create volumes table
volumes <- left_join(anat_over_55, aseg) %>% #join gray matter volumes with SCD status
  select(participant_id, SCD, contains("Hippocampus")) %>% #select all columns that contain "Hippocampus" in the name
  left_join(., wmparc) %>% #add white matter volumes
  select(participant_id, SCD, contains("Hippocampus"), #select all prior columns
         contains("parahippo"), #select all columns that contain "parahippo" in name
         contains("CerebralWhiteMatter")) %>% #select all columns that contain "CerebralWhiteMatter" in name
  drop_na() #drop rows with any NA values

#create thickness table
thickness <- left_join(anat_over_55, lh_aparc) %>% #join right cortical thickness with SCD stats
  left_join(., rh_aparc) %>% #add left cortical thickness
  drop_na() %>% #drop rows with any NA values
  #select columns who's names contain the structures of interest
  select(participant_id, contains("parahippocampal"), contains("entorhinal"),
         contains("temporalpole"), contains("inferiortemporal"), contains("middletemporal"), 
         contains("inferiorparietal"), contains("middletemporal"), contains("inferiorparietal"), 
         contains("superiorparietal"), contains("precuneus"), contains("posteriorcingulate")) %>%
  #remove "_thickness" from column names
  rename_with(~ str_remove(., "_thickness"))

failed_qc <- c('sub-CC510255', #abnormality in left temporal pole
               'sub-CC620821', #segmentation errors from large ventricles
               'sub-CC621011', #segmentation errors from large ventricles
               'sub-CC711027', #severe motion artifacts
               'sub-CC721434' #segmentation errors from large ventricles
)
#join volumes and thickness tables
volumes_thickness <- left_join(thickness, volumes) %>%
  filter(!participant_id %in% failed_qc) #remove rows that failed QC
volumes_thickness$SCD <- factor(volumes_thickness$SCD, #change SCD column into factor type
                                levels = c(1, 0),
                                labels = c('SCD', 'Control'))

#generate thickness table
results <- "asis"
thickness_table <- tableby(SCD ~ lh_parahippocampal + 
                             rh_parahippocampal +
                             lh_entorhinal + rh_entorhinal +
                             lh_temporalpole + rh_temporalpole +
                             lh_inferiortemporal + rh_inferiortemporal +
                             lh_middletemporal + rh_middletemporal +
                             lh_inferiorparietal + rh_inferiorparietal +
                             lh_superiorparietal + rh_superiorparietal +
                             lh_precuneus + rh_precuneus +
                             lh_posteriorcingulate + rh_posteriorcingulate,
                      data = volumes_thickness, total = FALSE)
summary(thickness_table, text = TRUE) #view
write2word(thickness_table, "thickness_table.docx") #save thickness table to file

#generate volumes table
volumes_table <- tableby(SCD ~ `Left-Hippocampus` + `Right-Hippocampus` + 
                           `wm-lh-parahippocampal` + `wm-rh-parahippocampal` +
                           lhCerebralWhiteMatterVol + rhCerebralWhiteMatterVol,
                         data = volumes_thickness, total = FALSE)
summary(volumes_table, text = TRUE) #view
write2word(volumes_table, "volumes_table.docx") #save volume table to file
