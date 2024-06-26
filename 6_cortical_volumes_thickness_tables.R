#run synthseg, recon-all-clinical, and aparcstats2table first
library(tidyverse)
library(arsenal)
library(effectsize)

#define remove_outliers function
remove_outliers <- function(x, na.rm = TRUE) 
{
  ## Find 25% and 75% Quantiles using inbuild function
  quant <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  
  ## Find Interquantile range and multiply it by 1.5 
  ## to derive factor for range calculation
  H <- 1.5 * IQR(x, na.rm = na.rm)
  
  y <- x
  
  ## fill the outlier elements with NA
  y[x < (quant[1] - H)] <- NA
  y[x > (quant[2] + H)] <- NA
  
  y
}

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
  filter(!participant_id %in% failed_qc) %>% #remove rows that failed QC
  mutate(across(where(is.double), remove_outliers)) #remove outliers (change to NA)
volumes_thickness_all <- left_join(thickness, volumes) %>%
  filter(!participant_id %in% failed_qc)
  
#save to file
write_csv(volumes_thickness, 'volumes_thickness.csv')
write_csv(volumes_thickness_all, 'volumes_thickness_all.csv')

volumes_thickness$SCD <- factor(volumes_thickness$SCD, #change SCD column into factor type
                                levels = c(1, 0),
                                labels = c('SCD', 'Control'))
volumes_thickness_all$SCD <- factor(volumes_thickness_all$SCD, #change SCD column into factor type
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
thickness_table_all <- tableby(SCD ~ lh_parahippocampal + 
                             rh_parahippocampal +
                             lh_entorhinal + rh_entorhinal +
                             lh_temporalpole + rh_temporalpole +
                             lh_inferiortemporal + rh_inferiortemporal +
                             lh_middletemporal + rh_middletemporal +
                             lh_inferiorparietal + rh_inferiorparietal +
                             lh_superiorparietal + rh_superiorparietal +
                             lh_precuneus + rh_precuneus +
                             lh_posteriorcingulate + rh_posteriorcingulate,
                           data = volumes_thickness_all, total = FALSE)
summary(thickness_table_all, text = TRUE) #view

#generate volumes table
volumes_table <- tableby(SCD ~ `Left-Hippocampus` + `Right-Hippocampus` + 
                           `wm-lh-parahippocampal` + `wm-rh-parahippocampal` +
                           lhCerebralWhiteMatterVol + rhCerebralWhiteMatterVol,
                         data = volumes_thickness, total = FALSE)
summary(volumes_table, text = TRUE) #view
write2word(volumes_table, "volumes_table.docx") #save volume table to file
volumes_table_all <- tableby(SCD ~ `Left-Hippocampus` + `Right-Hippocampus` + 
                           `wm-lh-parahippocampal` + `wm-rh-parahippocampal` +
                           lhCerebralWhiteMatterVol + rhCerebralWhiteMatterVol,
                         data = volumes_thickness_all, total = FALSE)
summary(volumes_table_all, text = TRUE) #view

#t values have to be inserted manually in word
t.test(lh_entorhinal ~ SCD, data=volumes_thickness)
t.test(lh_entorhinal ~ SCD, data=volumes_thickness_all)
cohens_d(lh_entorhinal ~ SCD, data=volumes_thickness)
cohens_d(lh_entorhinal ~ SCD, data=volumes_thickness_all)

t.test(rh_entorhinal ~ SCD, data=volumes_thickness)
t.test(rh_entorhinal ~ SCD, data=volumes_thickness_all)
cohens_d(rh_entorhinal ~ SCD, data=volumes_thickness)
cohens_d(rh_entorhinal ~ SCD, data=volumes_thickness_all)

t.test(lh_temporalpole ~ SCD, data=volumes_thickness)
t.test(lh_temporalpole ~ SCD, data=volumes_thickness_all)
cohens_d(lh_temporalpole ~ SCD, data=volumes_thickness)
cohens_d(lh_temporalpole ~ SCD, data=volumes_thickness_all)

t.test(rh_temporalpole ~ SCD, data=volumes_thickness)
t.test(rh_temporalpole ~ SCD, data=volumes_thickness_all)
cohens_d(rh_temporalpole ~ SCD, data=volumes_thickness)
cohens_d(rh_temporalpole ~ SCD, data=volumes_thickness_all)

t.test(lh_parahippocampal ~ SCD, data = volumes_thickness)
cohens_d(lh_parahippocampal ~ SCD, data = volumes_thickness)

t.test(rh_parahippocampal ~ SCD, data = volumes_thickness)
cohens_d(rh_parahippocampal ~ SCD, data = volumes_thickness)

t.test(lh_inferiortemporal ~ SCD, data = volumes_thickness)
cohens_d(lh_inferiortemporal ~ SCD, data = volumes_thickness)

t.test(rh_inferiortemporal ~ SCD, data = volumes_thickness)
cohens_d(rh_inferiortemporal ~ SCD, data = volumes_thickness)

t.test(lh_middletemporal ~ SCD, data = volumes_thickness)
cohens_d(lh_middletemporal ~ SCD, data = volumes_thickness)

t.test(rh_middletemporal ~ SCD, data = volumes_thickness)
cohens_d(rh_middletemporal ~ SCD, data = volumes_thickness)

t.test(lh_inferiorparietal ~ SCD, data = volumes_thickness)
cohens_d(lh_inferiorparietal ~ SCD, data = volumes_thickness)

t.test(rh_inferiorparietal ~ SCD, data = volumes_thickness)
cohens_d(rh_inferiorparietal ~ SCD, data = volumes_thickness)

t.test(lh_superiorparietal ~ SCD, data = volumes_thickness)
cohens_d(lh_superiorparietal ~ SCD, data = volumes_thickness)

t.test(rh_superiorparietal ~ SCD, data = volumes_thickness)
cohens_d(rh_superiorparietal ~ SCD, data = volumes_thickness)

t.test(lh_precuneus ~ SCD, data = volumes_thickness)
cohens_d(lh_precuneus ~ SCD, data = volumes_thickness)

t.test(rh_precuneus ~ SCD, data = volumes_thickness)
cohens_d(rh_precuneus ~ SCD, data = volumes_thickness)

t.test(lh_posteriorcingulate ~ SCD, data = volumes_thickness)
cohens_d(lh_posteriorcingulate ~ SCD, data = volumes_thickness)

t.test(rh_posteriorcingulate ~ SCD, data = volumes_thickness)
cohens_d(rh_posteriorcingulate ~ SCD, data = volumes_thickness)

t.test(`Left-Hippocampus` ~ SCD, data = volumes_thickness)
cohens_d(`Left-Hippocampus` ~ SCD, data = volumes_thickness)

t.test(`Right-Hippocampus` ~ SCD, data = volumes_thickness)
cohens_d(`Right-Hippocampus` ~ SCD, data = volumes_thickness)

t.test(`wm-lh-parahippocampal` ~ SCD, data = volumes_thickness)
cohens_d(`wm-lh-parahippocampal` ~ SCD, data = volumes_thickness)

t.test(`wm-rh-parahippocampal` ~ SCD, data = volumes_thickness)
cohens_d(`wm-rh-parahippocampal` ~ SCD, data = volumes_thickness)
