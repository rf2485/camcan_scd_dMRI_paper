library(tidyverse)
library(arsenal)

dwi_over_55_mem = read.delim("dwi_over_55.tsv", tryLogical = FALSE) %>%
  select(participant_id, SCD, age, sex, homeint_mmse_cal, homeint_storyrecall_i, 
         homeint_storyrecall_d, objprpos_emotional_mem, objprneu_emotional_mem,
         objprneg_emotional_mem, prcsn_ss1_vstm, prcsn_ss2_vstm, prcsn_ss3_vstm,
         prcsn_ss4_vstm) %>%
  mutate(across(objprpos_emotional_mem:prcsn_ss4_vstm, function(x) x*100)) %>%
  rename(Age=age, Sex=sex)
dwi_over_55_mem$SCD <- factor(dwi_over_55_mem$SCD,
                              levels = c(1,0),
                              labels = c('SCD', 'Control'))
attr(dwi_over_55_mem$SCD, 'label') <- 'Subjective Cognitive Decline'
attr(dwi_over_55_mem$homeint_mmse_cal, 'label') <- 'MMSE'
attr(dwi_over_55_mem$homeint_storyrecall_i, 'label') <- 'Immediate Story Recall'
attr(dwi_over_55_mem$homeint_storyrecall_d, 'label') <- 'Delayed Story Recall'
attr(dwi_over_55_mem$objprpos_emotional_mem, 'label') <- 'EM Positive Recognition (%)'
attr(dwi_over_55_mem$objprneu_emotional_mem, 'label') <- 'EM Neutral Recognition (%)'
attr(dwi_over_55_mem$objprneg_emotional_mem, 'label') <- 'EM Negative Recognition (%)'
attr(dwi_over_55_mem$prcsn_ss1_vstm, 'label') <- 'VSTM Set Size 1 Recall (%)'
attr(dwi_over_55_mem$prcsn_ss2_vstm, 'label') <- 'VSTM Set Size 2 Recall (%)'
attr(dwi_over_55_mem$prcsn_ss3_vstm, 'label') <- 'VSTM Set Size 3 Recall (%)'
attr(dwi_over_55_mem$prcsn_ss4_vstm, 'label') <- 'VSTM Set Size 4 Recall (%)'
results <- 'asis'

demo_table <- tableby(SCD ~ Sex + Age, data = dwi_over_55_mem)
summary(demo_table, text = TRUE)
write2word(demo_table, "demo_table.docx")
#t values and chi2 values have to be inserted manually in word
chisq.test(dwi_over_55_mem$SCD, dwi_over_55_mem$Sex, correct=F)
t.test(Age ~ SCD, data=dwi_over_55_mem)

cog_table <- tableby(SCD ~ homeint_mmse_cal + homeint_storyrecall_i + 
                       homeint_storyrecall_d + objprpos_emotional_mem + 
                       objprneu_emotional_mem + objprneg_emotional_mem + 
                       prcsn_ss1_vstm + prcsn_ss2_vstm + prcsn_ss3_vstm + 
                       prcsn_ss4_vstm, 
                     data = dwi_over_55_mem, 
                     numeric.stats = c("N", "meansd", "range"))
summary(cog_table, text = TRUE)
write2word(cog_table, "cog_table.docx")
#t values have to be inserted manually in word
t.test(homeint_mmse_cal ~ SCD, data=dwi_over_55_mem)
t.test(homeint_storyrecall_i ~ SCD, data=dwi_over_55_mem)
t.test(homeint_storyrecall_d ~ SCD, data=dwi_over_55_mem)
