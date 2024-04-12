library(tidyverse)
library(arsenal)
library(effectsize)

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
df_remove_outliers <- df %>% mutate(across(where(is.double), remove_outliers))

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

diff_table_remove_outliers <- tableby(cohort ~ mean_FA_l_lower_cingulum_mask + 
                        mean_MD_l_lower_cingulum_mask +
                        mean_L1_l_lower_cingulum_mask +
                        mean_RD_l_lower_cingulum_mask +
                        mean_FA_r_lower_cingulum_mask + 
                        mean_MD_r_lower_cingulum_mask +
                        mean_L1_r_lower_cingulum_mask +
                        mean_RD_r_lower_cingulum_mask,
                      data = df_remove_outliers, total = FALSE)
summary(diff_table_remove_outliers, text = TRUE)

means <- df %>% group_by(cohort) %>% 
  summarize(across(ends_with("lower_cingulum_mask"), 
                   list(mean = mean, sd = sd, min = min, max = max)))
#t values have to be inserted manually in word
t.test(mean_FA_l_lower_cingulum_mask ~ cohort, data = df)
t.test(mean_FA_l_lower_cingulum_mask ~ cohort, data = df_remove_outliers)
cohens_d(mean_FA_l_lower_cingulum_mask ~ cohort, data = df)
cohens_d(mean_FA_l_lower_cingulum_mask ~ cohort, data = df_remove_outliers)

t.test(mean_MD_l_lower_cingulum_mask ~ cohort, data = df)
t.test(mean_MD_l_lower_cingulum_mask ~ cohort, data = df_remove_outliers)
cohens_d(mean_MD_l_lower_cingulum_mask ~ cohort, data = df)
cohens_d(mean_MD_l_lower_cingulum_mask ~ cohort, data = df_remove_outliers)

t.test(mean_L1_l_lower_cingulum_mask ~ cohort, data = df)
t.test(mean_L1_l_lower_cingulum_mask ~ cohort, data = df_remove_outliers)
cohens_d(mean_L1_l_lower_cingulum_mask ~ cohort, data = df)
cohens_d(mean_L1_l_lower_cingulum_mask ~ cohort, data = df_remove_outliers)

t.test(mean_RD_l_lower_cingulum_mask ~ cohort, data = df)
t.test(mean_RD_l_lower_cingulum_mask ~ cohort, data = df_remove_outliers)
cohens_d(mean_RD_l_lower_cingulum_mask ~ cohort, data = df)
cohens_d(mean_RD_l_lower_cingulum_mask ~ cohort, data = df_remove_outliers)

t.test(mean_FA_r_lower_cingulum_mask ~ cohort, data = df)
cohens_d(mean_FA_r_lower_cingulum_mask ~ cohort, data = df)

t.test(mean_MD_r_lower_cingulum_mask ~ cohort, data = df)
t.test(mean_MD_r_lower_cingulum_mask ~ cohort, data = df_remove_outliers)
cohens_d(mean_MD_r_lower_cingulum_mask ~ cohort, data = df)
cohens_d(mean_MD_r_lower_cingulum_mask ~ cohort, data = df_remove_outliers)

t.test(mean_L1_r_lower_cingulum_mask ~ cohort, data = df)
t.test(mean_L1_r_lower_cingulum_mask ~ cohort, data = df_remove_outliers)
cohens_d(mean_L1_r_lower_cingulum_mask ~ cohort, data = df)
cohens_d(mean_L1_r_lower_cingulum_mask ~ cohort, data = df_remove_outliers)

t.test(mean_RD_r_lower_cingulum_mask ~ cohort, data = df)
t.test(mean_RD_r_lower_cingulum_mask ~ cohort, data = df_remove_outliers)
cohens_d(mean_RD_r_lower_cingulum_mask ~ cohort, data = df)
cohens_d(mean_RD_r_lower_cingulum_mask ~ cohort, data = df_remove_outliers)
