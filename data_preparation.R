library(tidyverse)
data_dir = "/Volumes/Research/lazarm03lab/labspace/AD/camcan995/source_materials"

#### pulling all spreadsheets into BIDS compliant participants.tsv ###

# standard_data = read_csv(file.path(data_dir, "standard_data.csv"))
# approved_data = read_tsv(file.path(data_dir, "behavioral/approved_data.tsv"))
# 
# #summary txt files had header and footer removed and were converted to tsv in a text editor
# rtsimple = read_tsv(file.path(data_dir, "behavioral/RTsimple_summary.tsv")) %>%
#   mutate(CCID = str_trim(CCID))
# rtchoice = read_tsv(file.path(data_dir, "behavioral/RTchoice_summary.tsv"))
# emotional_memory = read_tsv(file.path(data_dir, "behavioral/EmotionalMemory_summary.tsv"))
# cattell = read_tsv(file.path(data_dir, "behavioral/Cattell_summary.tsv"))
# cardio_measures = read_tsv(file.path(data_dir, "behavioral/cardio_measures.tsv")) %>%
#   rename(CCID = Observations)
# 
# participants = full_join(standard_data, approved_data, by='CCID') %>% 
#   left_join(., rtsimple, by='CCID') %>% left_join(., rtchoice, by='CCID') %>%
#   full_join(., emotional_memory, by='CCID') %>% full_join(., cattell, by='CCID') %>%
#   full_join(., cardio_measures, by='CCID') %>%
#   mutate(CCID = str_replace(CCID, "CC", "sub-CC")) %>% rename(participant_id=CCID)
# write_tsv(participants, "/Volumes/Research/lazarm03lab/labspace/AD/camcan995/raw/participants.tsv")

participants = read.delim("/Volumes/Research/lazarm03lab/labspace/AD/camcan995/raw/participants.tsv", tryLogical = FALSE)
participants$SCD <- participants$homeint_v230
participants$SCD[participants$SCD == 1] <- FALSE
participants$SCD[participants$SCD == 2] <- TRUE
participants$SCD[is.na(participants$SCD)] <- FALSE

### filter by age and DWI available ###
participants_over_55 <- participants %>% filter(Age > 55)
dwi_participants = read_tsv(file.path(data_dir, "imaging/dwi/participants.tsv")) %>%
  left_join(., participants)
dwi_over_55 = dwi_participants %>% filter(Age > 55)
