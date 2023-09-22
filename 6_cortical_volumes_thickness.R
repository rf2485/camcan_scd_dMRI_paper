#run synthseg, recon-all-clinical, and aparcstats2table first
library(tidyverse)
library(arsenal)

aseg = read_tsv("recon-all-clinical/asegtable.tsv") %>%
  rename(participant_id=`Measure:volume`) %>%
  mutate(across(c(2:ncol(.)), .fns = ~.*1000/EstimatedTotalIntraCranialVol)) #normalize by intracranial volume
lh_aparc = read_tsv("recon-all-clinical/lh_aparctable.tsv") %>%
  rename(participant_id=lh.aparc.thickness)
rh_aparc = read_tsv("recon-all-clinical/rh_aparctable.tsv") %>%
  rename(participant_id=rh.aparc.thickness)
wmparc = read_tsv("recon-all-clinical/wmparctable.tsv") %>%
  rename(participant_id=`Measure:volume`) %>%
  mutate(across(c(2:ncol(.)), .fns = ~.*1000/EstimatedTotalIntraCranialVol))
anat_over_55 = read.delim("anat_over_55.tsv", tryLogical = FALSE) %>%
  select(participant_id, SCD)

volumes <- left_join(anat_over_55, aseg) %>%
  select(participant_id, SCD, contains("Hippocampus")) %>%
  left_join(., wmparc) %>%
  select(participant_id, SCD, contains("Hippocampus"), contains("parahippo"), 
         contains("CerebralWhiteMatter")) %>%
  drop_na()
thickness <- left_join(anat_over_55, lh_aparc) %>% left_join(., rh_aparc) %>% 
  drop_na() %>%
  select(participant_id, contains("parahippocampal"), contains("entorhinal"), 
         contains("temporalpole"), contains("inferiortemporal"), contains("middletemporal"), 
         contains("inferiorparietal"), contains("middletemporal"), contains("inferiorparietal"), 
         contains("superiorparietal"), contains("precuneus"), contains("posteriorcingulate")) %>%
  rename_with(~ str_remove(., "_thickness"))
volumes_thickness <- left_join(thickness, volumes)

#prepare table
volumes_thickness$SCD <- factor(volumes_thickness$SCD,
                                levels = c(1, 0),
                                labels = c('SCD', 'Control'))
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
summary(thickness_table, text = TRUE)
write2word(thickness_table, "thickness_table.docx")
volumes_table <- tableby(SCD ~ `Left-Hippocampus` + `Right-Hippocampus` + 
                           `wm-lh-parahippocampal` + `wm-rh-parahippocampal` +
                           lhCerebralWhiteMatterVol + rhCerebralWhiteMatterVol,
                         data = volumes_thickness, total = FALSE)
summary(volumes_table, text = TRUE)
write2word(volumes_table, "volumes_table.docx")
