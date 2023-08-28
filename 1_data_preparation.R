library(tidyverse)
#replace with location for your source data from the CamCan data repository
data_dir = "/Volumes/Research/lazarm03lab/labspace/AD/camcan995/source_materials"

### pulling all spreadsheets into BIDS compliant participants.tsv ###

standard_data = read_csv(file.path(data_dir, "standard_data.csv"))
approved_data = read_tsv(file.path(data_dir, "behavioral/approved_data.tsv"))

#summary txt files had header and footer removed and were converted to tsv in a text editor
vstm = read_tsv(file.path(data_dir, "behavioral/VSTMcolour_summary.tsv")) %>%
  rename_at(vars(-CCID), ~ paste0(., "_vstm"))
rtsimple = read_tsv(file.path(data_dir, "behavioral/RTsimple_summary.tsv")) %>%
  rename_at(vars(-CCID), ~ paste0(., "_rtsimple"))
rtchoice = read_tsv(file.path(data_dir, "behavioral/RTchoice_summary.tsv")) %>%
  rename_at(vars(-CCID), ~ paste0(., "_rtchoice"))
emotional_memory = read_tsv(file.path(data_dir, "behavioral/EmotionalMemory_summary.tsv")) %>%
  rename_at(vars(-CCID), ~ paste0(., "_emotional_mem"))
cattell = read_tsv(file.path(data_dir, "behavioral/Cattell_summary.tsv")) %>%
  rename_at(vars(-CCID), ~ paste0(., "_cattell"))
cardio_measures = read_tsv(file.path(data_dir, "behavioral/cardio_measures.tsv")) %>%
  rename(CCID = Observations) %>%   rename_at(vars(-CCID), ~ paste0(., "_cardio"))

participants = full_join(standard_data, approved_data, by='CCID') %>%
  left_join(., vstm, by='CCID') %>%
  left_join(., rtsimple, by='CCID') %>% left_join(., rtchoice, by='CCID') %>%
  left_join(., emotional_memory, by='CCID') %>% full_join(., cattell, by='CCID') %>%
  left_join(., cardio_measures, by='CCID') %>%
  mutate(CCID = str_replace(CCID, "CC", "sub-CC")) %>% rename(participant_id=CCID)
names(participants) <- tolower(names(participants))
write_tsv(participants, "/Volumes/Research/lazarm03lab/labspace/AD/camcan995/raw/participants.tsv")

#replace with location of your participants.tsv
participants = read.delim("/Volumes/Research/lazarm03lab/labspace/AD/camcan995/raw/participants.tsv", tryLogical = FALSE)
participants$SCD <- participants$homeint_v230
participants$SCD[participants$SCD == 1] <- FALSE
participants$SCD[participants$SCD == 2] <- TRUE
participants$SCD[is.na(participants$SCD)] <- FALSE

### filter by age and if DWI is available ###
participants_over_55 <- participants %>% filter(age > 55)
#replace with location of your dwi participants.tsv
dwi_participants = read_tsv(file.path(data_dir, "imaging/dwi/participants.tsv")) %>%
  select(participant_id) %>%
  left_join(., participants, by='participant_id')
dwi_over_55 = dwi_participants %>% filter(age > 55)
write_tsv(dwi_over_55, "dwi_over_55.tsv")

### filter by age and if anat is available ###
anat_participants = read_tsv(file.path(data_dir, "imaging/anat/participants.tsv")) %>%
  select(participant_id) %>%
  left_join(., participants, by='participant_id')
anat_over_55 = anat_participants %>% filter(age > 55)
