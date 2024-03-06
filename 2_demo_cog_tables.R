library(tidyverse)
library(arsenal)

anat_over_55_mem = read.delim("anat_over_55.tsv", tryLogical = FALSE) %>%
  select(participant_id, SCD, age, sex, homeint_v15, homeint_v24, homeint_v74, 
         homeint_mmse_cal, homeint_storyrecall_i, homeint_storyrecall_d, 
         objprpos_emotional_mem, objprneu_emotional_mem,objprneg_emotional_mem, 
         prcsn_ss1_vstm, prcsn_ss2_vstm, prcsn_ss3_vstm, prcsn_ss4_vstm) %>%
  mutate(across(objprpos_emotional_mem:prcsn_ss4_vstm, function(x) x*100)) %>%
  mutate(homeint_v24 = replace_na(homeint_v24, 8)) %>%
  rename(Age=age, Sex=sex)

anat_over_55_mem$SCD <- factor(anat_over_55_mem$SCD,
                              levels = c(1,0),
                              labels = c('SCD', 'Control'))
anat_over_55_mem$homeint_v15 <- factor(anat_over_55_mem$homeint_v15,
                                       levels = c('D', 'B', 'C', 'A', 'F', 'E'),
                                       labels = c('Less than £18000', 
                                                  '£18000 to 30999',
                                                  '£31000 to 51999',
                                                  '£52000 to 100000',
                                                  'Greater than £100000',
                                                  'Prefer not to answer'))


anat_over_55_mem$homeint_v24 <- factor(anat_over_55_mem$homeint_v24,
                                       levels = c(1,2,3,4,5,6,7,8),
                                       labels = c('White',
                                                  'Mixed',
                                                  'Asian or Asian British',
                                                  'Black or Black British',
                                                  'Chinese',
                                                  'Other',
                                                  "Don't know",
                                                  "No answer"))

attr(anat_over_55_mem$SCD, 'label') <- 'Subjective Cognitive Decline'
attr(anat_over_55_mem$homeint_v15, 'label') <- 'Income'
attr(anat_over_55_mem$homeint_v24, 'label') <- 'Ethnicity'
attr(anat_over_55_mem$homeint_v74, 'label') <- 'Years of Education'
attr(anat_over_55_mem$homeint_mmse_cal, 'label') <- 'MMSE'
attr(anat_over_55_mem$homeint_storyrecall_i, 'label') <- 'Immediate Story Recall'
attr(anat_over_55_mem$homeint_storyrecall_d, 'label') <- 'Delayed Story Recall'
attr(anat_over_55_mem$objprpos_emotional_mem, 'label') <- 'EM Positive Recognition (%)'
attr(anat_over_55_mem$objprneu_emotional_mem, 'label') <- 'EM Neutral Recognition (%)'
attr(anat_over_55_mem$objprneg_emotional_mem, 'label') <- 'EM Negative Recognition (%)'
attr(anat_over_55_mem$prcsn_ss1_vstm, 'label') <- 'VSTM Set Size 1 Recall (%)'
attr(anat_over_55_mem$prcsn_ss2_vstm, 'label') <- 'VSTM Set Size 2 Recall (%)'
attr(anat_over_55_mem$prcsn_ss3_vstm, 'label') <- 'VSTM Set Size 3 Recall (%)'
attr(anat_over_55_mem$prcsn_ss4_vstm, 'label') <- 'VSTM Set Size 4 Recall (%)'
results <- 'asis'

demo_table <- tableby(SCD ~ Sex + Age + homeint_v15 + homeint_v24 + homeint_v74, 
                      data = anat_over_55_mem, cat.test = "chisq")
summary(demo_table, text = TRUE)
write2word(demo_table, "demo_table.docx")
#t values and chi2 values have to be inserted manually in word
chisq.test(anat_over_55_mem$SCD, anat_over_55_mem$Sex, correct=F)
t.test(Age ~ SCD, data=anat_over_55_mem)
chisq.test(anat_over_55_mem$SCD, anat_over_55_mem$homeint_v15, correct = F)
chisq.test(anat_over_55_mem$SCD, anat_over_55_mem$homeint_v24, correct = F,
           simulate.p.value = T)
t.test(homeint_v74 ~ SCD, data = anat_over_55_mem)

cog_table <- tableby(SCD ~ homeint_mmse_cal + homeint_storyrecall_i + 
                       homeint_storyrecall_d + objprpos_emotional_mem + 
                       objprneu_emotional_mem + objprneg_emotional_mem + 
                       prcsn_ss1_vstm + prcsn_ss2_vstm + prcsn_ss3_vstm + 
                       prcsn_ss4_vstm, 
                     data = anat_over_55_mem, 
                     numeric.stats = c("N", "meansd", "range"))
summary(cog_table, text = TRUE)
write2word(cog_table, "cog_table.docx")
#t values have to be inserted manually in word
t.test(homeint_mmse_cal ~ SCD, data=anat_over_55_mem)
t.test(homeint_storyrecall_i ~ SCD, data=anat_over_55_mem)
t.test(homeint_storyrecall_d ~ SCD, data=anat_over_55_mem)
