library(tidyverse)

design_matrix <- read.delim("dwi_over_55.tsv", tryLogical = F) %>%
  mutate(EV1 = ifelse(SCD == 0, 1, 0),
         EV2 = ifelse(SCD == 1, 1, 0)) %>%
  rename(EV3 = age, EV4 = homeint_storyrecall_d) %>%
  select(EV1, EV2, EV3, EV4)

#mean center covariates
design_matrix$EV3 <- scale(design_matrix$EV3, scale = F)
design_matrix$EV4 <- scale(design_matrix$EV4, scale = F)

#model 1: scd and story interaction
scd_story_mat <- design_matrix %>% select(EV1, EV2, EV4) %>%
  mutate(EV3 = ifelse(EV1 == 1, EV4, 0),
         EV4 = ifelse(EV1 == 0, EV4, 0)) %>%
  select(order(colnames(.))) #order columns alphabetically

scd_story_mat <- unname(as.matrix(scd_story_mat))
write.table(scd_story_mat, file = "tbss/stats/scd_story_mat.txt", sep = "\t", 
            row.names = F, col.names = F)
scd_story_con <- rbind(c(0,0,1,-1), c(0,0,-1,1))
write.table(scd_story_con, file = "tbss/stats/scd_story_con.txt", sep = "\t",
            row.names = F, col.names = F)

#model 2: scd and age interaction
scd_age_mat <- design_matrix %>% select(EV1, EV2, EV3) %>%
  mutate(EV4 = ifelse(EV1 == 1, EV3, 0),
         EV3 = ifelse(EV1 == 0, EV3, 0))

scd_age_mat <- unname(as.matrix(scd_age_mat))
write.table(scd_age_mat, file = "tbss/stats/scd_age_mat.txt", sep = "\t", 
            row.names = F, col.names = F)
scd_age_con <- rbind(c(0,0,1,-1), c(0,0,-1,1))
write.table(scd_age_con, file = "tbss/stats/scd_age_con.txt", sep = "\t",
            row.names = F, col.names = F)

# model 3: age and story interaction
age_story_mat <- design_matrix %>% select(EV3, EV4) %>%
  rename(EV2 = EV3, EV3 = EV4) %>%
  mutate(EV1 = 1, #intercept
         EV4 = EV2*EV3) %>% #interaction
  select(order(colnames(.))) #order columns alphabetically

age_story_mat <- unname(as.matrix(age_story_mat))
write.table(age_story_mat, file = "tbss/stats/age_story_mat.txt", sep = "\t",
            row.names = F, col.names = F)
age_story_con <- rbind(c(0,0,0,1), c(0,0,0,-1))
write.table(age_story_con, file = "tbss/stats/age_story_con.txt", sep = "\t",
            row.names = F, col.names = F)