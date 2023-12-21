library(tidyverse)
library(arsenal)


#read in ROI means
scd_table <- read_csv('roi_diff_means/scd_means_table.csv')
scd_table$cohort <- 'scd'
names(scd_table) <- sub('scd_', '', names(scd_table))
ctl_table <- read_csv("roi_diff_means/ctl_means_table.csv")
ctl_table$cohort <- 'ctl'
names(ctl_table) <- sub('ctl_', '', names(ctl_table))
df <- rbind(ctl_table, scd_table)
duplicated_columns <- duplicated(colnames(df))
df <- df[!duplicated_columns]
df <- df %>% relocate(cohort, .after = last_col())

results <- "asis"
diff_table <- tableby(cohort ~ mean_FA_l_lower_cingulum_mask + 
                        mean_MD_l_lower_cingulum_mask +
                        mean_L1_l_lower_cingulum_mask +
                        mean_RD_l_lower_cingulum_mask +
                        mean_FA_r_lower_cingulum_mask + 
                        mean_MD_r_lower_cingulum_mask +
                        mean_L1_r_lower_cingulum_mask +
                        mean_RD_r_lower_cingulum_mask,
                        data = df, total = FALSE)
summary(diff_table, text = TRUE)
write2word(diff_table, "diff_table.docx")
