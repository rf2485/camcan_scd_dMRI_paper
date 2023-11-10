library(tidyverse)
library(ggpmisc)
library(interactions)
library(officer)
library(ggtext)

######################### data preparation #####################################
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

#read in data for SCD
scd_diff_means <- read_csv('roi_diff_means/scd_means_table.csv')
scd_cog_demo <- read.delim('dwi_over_55_scd.tsv', tryLogical=F, na.strings = c("NA", "NaN"))
#combine diffusion means with cognitive and demographics data
scd_table <- cbind(scd_diff_means, scd_cog_demo)
#remove 'scd_' suffix from column names
names(scd_table) <- sub('scd_', '', names(scd_table))

#read in data for controls
ctl_diff_means <- read_csv("roi_diff_means/ctl_means_table.csv")
ctl_cog_demo <- read.delim('dwi_over_55_ctl.tsv', tryLogical = F, na.strings = c("NA", "NaN"))
#combine diffusion means with cognitive and demographics data
ctl_table <- cbind(ctl_diff_means, ctl_cog_demo)
#remove 'ctl_' suffix from column names
names(ctl_table) <- sub('ctl_', '', names(ctl_table))

#combine SCD and control rows
df <- rbind(ctl_table, scd_table)

#add cortical volumes and thickness
volumes_thickness <- read_csv("volumes_thickness.csv") %>%
  select(-SCD) #remove duplicate non-id column
df <- left_join(df, volumes_thickness, by='participant_id')
#generate cohort factor column from SCD column
df <- df %>%
  mutate(cohort = ifelse(SCD == 1, 'scd', 'ctl'), #makes data more readable
         story_d = scale(homeint_storyrecall_d, scale = F), #mean center for consistency with TBSS
         age = scale(age, scale = F)) #mean center for consistency with TBSS
#move participant_id and cohort to beginning of df
df <- df %>% select(participant_id, cohort, SCD, age, story_d, everything())
write.csv(df, "final_dataframe.csv")

#drop diffusion outliers (assume this is an image quality issue, already done for anatomical)
df_FA <- df 
df_FA$mean_FA_lower_cingulum_mask <- remove_outliers(df$mean_FA_lower_cingulum_mask)
df_FA <- df_FA %>% drop_na(mean_FA_lower_cingulum_mask)
#split into groups of interest
ctl_FA <- df_FA %>% filter(cohort == 'ctl')
scd_FA <- df_FA %>% filter(cohort == 'scd')
upper_story_FA <- df_FA %>% filter(story_d > 0)
lower_story_FA <- df_FA %>% filter(story_d < 0)
df_MD <- df 
df_MD$mean_MD_lower_cingulum_mask <- remove_outliers(df$mean_MD_lower_cingulum_mask)
df_MD <- df_MD %>% drop_na(mean_MD_lower_cingulum_mask)
ctl_MD <- df_MD %>% filter(cohort == 'ctl')
scd_MD <- df_MD %>% filter(cohort == 'scd')
upper_story_MD <- df_MD %>% filter(story_d > 0)
lower_story_MD <- df_MD %>% filter(story_d < 0)
df_L1 <- df 
df_L1$mean_L1_lower_cingulum_mask <- remove_outliers(df$mean_L1_lower_cingulum_mask)
df_L1 <- df_L1 %>% drop_na(mean_L1_lower_cingulum_mask)
ctl_L1 <- df_L1 %>% filter(cohort == 'ctl')
scd_L1 <- df_L1 %>% filter(cohort == 'scd')
upper_story_L1 <- df_L1 %>% filter(story_d > 0)
lower_story_L1 <- df_L1 %>% filter(story_d < 0)
df_RD <- df 
df_RD$mean_RD_lower_cingulum_mask <- remove_outliers(df$mean_RD_lower_cingulum_mask)
df_RD <- df_RD %>% drop_na(mean_RD_lower_cingulum_mask)
ctl_RD <- df_RD %>% filter(cohort == 'ctl')
scd_RD <- df_RD %>% filter(cohort == 'scd')
upper_story_RD <- df_RD %>% filter(story_d > 0)
lower_story_RD <- df_RD %>% filter(story_d < 0)
df_ISOVF <- df 
df_ISOVF$mean_ISOVF_lower_cingulum_mask <- remove_outliers(df$mean_ISOVF_lower_cingulum_mask)
df_ISOVF <- df_ISOVF %>% drop_na(mean_ISOVF_lower_cingulum_mask)
ctl_ISOVF <- df_ISOVF %>% filter(cohort == 'ctl')
scd_ISOVF <- df_ISOVF %>% filter(cohort == 'scd')
upper_story_ISOVF <- df_ISOVF %>% filter(story_d > 0)
lower_story_ISOVF <- df_ISOVF %>% filter(story_d < 0)
df_ICVF <- df 
df_ICVF$mean_ICVF_lower_cingulum_mask <- remove_outliers(df$mean_ICVF_lower_cingulum_mask)
df_ICVF <- df_ICVF %>% drop_na(mean_ICVF_lower_cingulum_mask)
ctl_ICVF <- df_ICVF %>% filter(cohort == 'ctl')
scd_ICVF <- df_ICVF %>% filter(cohort == 'scd')
upper_story_ICVF <- df_ICVF %>% filter(story_d > 0)
lower_story_ICVF <- df_ICVF %>% filter(story_d < 0)
df_OD <- df 
df_OD$mean_OD_lower_cingulum_mask <- remove_outliers(df$mean_OD_lower_cingulum_mask)
df_OD <- df_OD %>% drop_na(mean_OD_lower_cingulum_mask)
ctl_OD <- df_OD %>% filter(cohort == 'ctl')
scd_OD <- df_OD %>% filter(cohort == 'scd')
upper_story_OD <- df_OD %>% filter(story_d > 0)
lower_story_OD <- df_OD %>% filter(story_d < 0)

######################## scd-thickness interaction scatterplots ##################
FA_scd_l_ento <- lm(mean_FA_l_lower_cingulum_mask ~ lh_entorhinal * cohort, df_FA)
summary(FA_scd_l_ento)
# FA_l_ento <- lm(mean_FA_l_lower_cingulum_mask ~ lh_entorhinal, df_FA)
# summary(FA_l_ento)
ggplot(df_FA, aes(lh_entorhinal, mean_FA_l_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "left") +
  labs(title = "Left Mean FA", x = "Left Entorhinal Cortical Thickness", y = "Mean FA") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
MD_scd_l_ento <- lm(mean_MD_l_lower_cingulum_mask ~ lh_entorhinal * cohort, df_MD)
summary(MD_scd_l_ento)
# MD_l_ento <- lm(mean_MD_l_lower_cingulum_mask ~ lh_entorhinal, df_MD)
# summary(MD_l_ento)
# ggplot(df_MD, aes(lh_entorhinal, mean_MD_l_lower_cingulum_mask)) +
#   geom_point() +
#   geom_smooth(method = 'lm', formula = y ~ x) +
#   stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right")
L1_scd_l_ento <- lm(mean_L1_l_lower_cingulum_mask ~ lh_entorhinal * cohort, df_L1)
summary(L1_scd_l_ento)
# L1_l_ento <- lm(mean_L1_l_lower_cingulum_mask ~ lh_entorhinal, df_L1)
# summary(L1_l_ento)
RD_scd_l_ento <- lm(mean_RD_l_lower_cingulum_mask ~ lh_entorhinal * cohort, df_RD)
summary(RD_scd_l_ento)
# RD_l_ento <- lm(mean_RD_l_lower_cingulum_mask ~ lh_entorhinal, df_RD)
# summary(RD_l_ento)
ggplot(df_RD, aes(lh_entorhinal, mean_RD_l_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "right") +
  labs(title = "Left Mean RD", x = "Left Entorhinal Cortical Thickness", y = "Mean RD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
OD_scd_l_ento <- lm(mean_OD_l_lower_cingulum_mask ~ lh_entorhinal * cohort, df_OD)
summary(OD_scd_l_ento)
# OD_l_ento <- lm(mean_OD_l_lower_cingulum_mask ~ lh_entorhinal, df_OD)
# summary(OD_l_ento)
# ggplot(df_OD, aes(lh_entorhinal, mean_OD_l_lower_cingulum_mask)) +
#   geom_point() +
#   geom_smooth(method = 'lm', formula = y ~ x) +
#   stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right")
ICVF_scd_l_ento <- lm(mean_ICVF_l_lower_cingulum_mask ~ lh_entorhinal * cohort, df_ICVF)
summary(ICVF_scd_l_ento)
# ICVF_l_ento <- lm(mean_ICVF_l_lower_cingulum_mask ~ lh_entorhinal, df_ICVF)
# summary(ICVF_l_ento)
ISOVF_scd_l_ento <- lm(mean_ISOVF_l_lower_cingulum_mask ~ lh_entorhinal * cohort, df_ISOVF)
summary(ISOVF_scd_l_ento)
# ISOVF_l_ento <- lm(mean_ISOVF_l_lower_cingulum_mask ~ lh_entorhinal, df_ISOVF)
# summary(ISOVF_l_ento)

FA_scd_r_ento <- lm(mean_FA_r_lower_cingulum_mask ~ rh_entorhinal * cohort, df_FA)
summary(FA_scd_r_ento)
# FA_r_ento <- lm(mean_FA_r_lower_cingulum_mask ~ rh_entorhinal, df_FA)
# summary(FA_r_ento)
ggplot(df_FA, aes(rh_entorhinal, mean_FA_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "left") +
  labs(title = "Right Mean FA", x = "Right Entorhinal Cortical Thickness", y = "Mean FA") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
MD_scd_r_ento <- lm(mean_MD_r_lower_cingulum_mask ~ rh_entorhinal * cohort, df_MD)
summary(MD_scd_r_ento)
# MD_r_ento <- lm(mean_MD_r_lower_cingulum_mask ~ rh_entorhinal, df_MD)
# summary(MD_r_ento)
ggplot(df_MD, aes(rh_entorhinal, mean_MD_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "left") +
  labs(title = "Right Mean MD", x = "Right Entorhinal Cortical Thickness", y = "Mean MD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
L1_scd_r_ento <- lm(mean_L1_r_lower_cingulum_mask ~ rh_entorhinal * cohort, df_L1)
summary(L1_scd_r_ento)
# L1_r_ento <- lm(mean_L1_r_lower_cingulum_mask ~ rh_entorhinal, df_L1)
# summary(L1_r_ento)
ggplot(df_L1, aes(rh_entorhinal, mean_L1_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "left") +
  labs(title = "Right Mean AxD", x = "Right Entorhinal Cortical Thickness", y = "Mean AxD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
    
RD_scd_r_ento <- lm(mean_RD_r_lower_cingulum_mask ~ rh_entorhinal * cohort, df_RD)
summary(RD_scd_r_ento)
# RD_r_ento <- lm(mean_RD_r_lower_cingulum_mask ~ rh_entorhinal, df_RD)
# summary(RD_r_ento)
ggplot(df_RD, aes(rh_entorhinal, mean_RD_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "left") +
  labs(title = "Right Mean RD", x = "Right Entorhinal Cortical Thickness", y = "Mean RD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
OD_scd_r_ento <- lm(mean_OD_r_lower_cingulum_mask ~ rh_entorhinal * cohort, df_OD)
summary(OD_scd_r_ento)
# OD_r_ento <- lm(mean_OD_r_lower_cingulum_mask ~ rh_entorhinal, df_OD)
# summary(OD_r_ento)
ggplot(df_OD, aes(rh_entorhinal, mean_OD_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right") +
  theme_bw()
ICVF_scd_r_ento <- lm(mean_ICVF_r_lower_cingulum_mask ~ rh_entorhinal * cohort, df_ICVF)
summary(ICVF_scd_r_ento)
# ICVF_r_ento <- lm(mean_ICVF_r_lower_cingulum_mask ~ rh_entorhinal, df_ICVF)
# summary(ICVF_r_ento)
ISOVF_scd_r_ento <- lm(mean_ISOVF_r_lower_cingulum_mask ~ rh_entorhinal * cohort, df_ISOVF)
summary(ISOVF_scd_r_ento)
# ISOVF_r_ento <- lm(mean_ISOVF_r_lower_cingulum_mask ~ rh_entorhinal, df_ISOVF)
# summary(ISOVF_r_ento)
ggplot(df_ISOVF, aes(rh_entorhinal, mean_ISOVF_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right") +
  theme_bw()

FA_scd_r_temppole <- lm(mean_FA_r_lower_cingulum_mask ~ rh_temporalpole * cohort, df_FA)
summary(FA_scd_r_temppole)
# FA_r_temppole <- lm(mean_FA_r_lower_cingulum_mask ~ rh_temporalpole, df_FA)
# summary(FA_r_temppole)
ggplot(df_FA, aes(rh_temporalpole, mean_FA_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "left") +
  labs(title = "Right Mean FA", x = "Right Temporal Pole Cortical Thickness", y = "Mean FA") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
MD_scd_r_temppole <- lm(mean_MD_r_lower_cingulum_mask ~ rh_temporalpole * cohort, df_MD)
summary(MD_scd_r_temppole)
# MD_r_temppole <- lm(mean_MD_r_lower_cingulum_mask ~ rh_temporalpole, df_MD)
# summary(MD_r_temppole)
ggplot(df_MD, aes(rh_temporalpole, mean_MD_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "left") +
  labs(title = "Right Mean MD", x = "Right Temporal Pole Cortical Thickness", y = "Mean MD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
L1_scd_r_temppole <- lm(mean_L1_r_lower_cingulum_mask ~ rh_temporalpole * cohort, df_L1)
summary(L1_scd_r_temppole)
# L1_r_temppole <- lm(mean_L1_r_lower_cingulum_mask ~ rh_temporalpole, df_L1)
# summary(L1_r_temppole)
ggplot(df_L1, aes(rh_temporalpole, mean_L1_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "left") +
  labs(title = "Right Mean AxD", x = "Right Temporal Pole Cortical Thickness", y = "Mean AxD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
RD_scd_r_temppole <- lm(mean_RD_r_lower_cingulum_mask ~ rh_temporalpole * cohort, df_RD)
summary(RD_scd_r_temppole)
# RD_r_temppole <- lm(mean_RD_r_lower_cingulum_mask ~ rh_temporalpole, df_RD)
# summary(RD_r_temppole)
ggplot(df_RD, aes(rh_temporalpole, mean_RD_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "left") +
  labs(title = "Right Mean RD", x = "Right Temporal Pole Cortical Thickness", y = "Mean RD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
OD_scd_r_temppole <- lm(mean_OD_r_lower_cingulum_mask ~ rh_temporalpole * cohort, df_OD)
summary(OD_scd_r_temppole)
# OD_r_temppole <- lm(mean_OD_r_lower_cingulum_mask ~ rh_temporalpole, df_OD)
# summary(OD_r_temppole)
# ggplot(df_OD, aes(rh_temporalpole, mean_OD_r_lower_cingulum_mask)) +
#   geom_point() +
#   geom_smooth(method = 'lm', formula = y ~ x) +
#   stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right")
ICVF_scd_r_temppole <- lm(mean_ICVF_r_lower_cingulum_mask ~ rh_temporalpole * cohort, df_ICVF)
summary(ICVF_scd_r_temppole)
ISOVF_scd_r_temppole <- lm(mean_ISOVF_r_lower_cingulum_mask ~ rh_temporalpole * cohort, df_ISOVF)
summary(ISOVF_scd_r_temppole)

######################## scd-story interaction scatterplots #################################################
l_ento_scd_story <- lm(lh_entorhinal ~ story_d * cohort, df)
summary(l_ento_scd_story)
l_ento_story <- lm(lh_entorhinal ~ story_d, df)
summary(l_ento_story)
r_ento_scd_story <- lm(rh_entorhinal ~ story_d * cohort, df)
summary(r_ento_scd_story)
r_ento_story <- lm(rh_entorhinal ~ story_d, df)
summary(r_ento_story)
r_temppole_scd_story <- lm(rh_temporalpole ~ story_d * cohort, df)
summary(r_temppole_scd_story)
r_temppole_story <- lm(rh_temporalpole ~ story_d, df)
summary(r_temppole_story)

fig1 <- read_pptx()
layout_summary(fig1)
#device size 6.80x4.86 inches
#width 653 height 466

FA_scd_story <- lm(mean_FA_r_lower_cingulum_mask ~ story_d * cohort, df_FA)
summary(FA_scd_story)
ctl_FA_story <- lm(mean_FA_r_lower_cingulum_mask ~ story_d, ctl_FA)
scd_FA_story <- lm(mean_FA_r_lower_cingulum_mask ~ story_d, scd_FA)
fig1FA <- interact_plot(FA_scd_story, pred = story_d, modx = cohort, 
                        plot.points = T, interval = T, point.alpha = 1, vary.lty = F,
                        modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Story Delayed Recall", y = "Mean FA", title = "Right Mean FA",
       subtitle = paste0(
         "interaction p = ",
         signif(summary(FA_scd_story)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(ctl_FA_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_FA_story)$adj.r.squared, 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(scd_FA_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_FA_story)$adj.r.squared, 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig1FA
fig1 <- add_slide(fig1)
fig1 <- ph_with(x = fig1, fig1FA, location = ph_location_type(type = "body"))

MD_scd_story <- lm(mean_MD_r_lower_cingulum_mask ~ story_d * cohort, df_MD)
summary(MD_scd_story)
ctl_MD_story <- lm(mean_MD_r_lower_cingulum_mask ~ story_d, ctl_MD)
scd_MD_story <- lm(mean_MD_r_lower_cingulum_mask ~ story_d, scd_MD)
fig1MD <- interact_plot(MD_scd_story, pred = story_d, modx = cohort, 
                        plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                        modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Story Delayed Recall", y = "Mean MD", title = "Right Mean MD",
       subtitle = paste0(
         "interaction p = ",
         signif(summary(MD_scd_story)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      "p = ", signif(summary(ctl_MD_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_MD_story)$adj.r.squared, 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 2.5, hjust = 1.01,
                    label = paste0(
                      # "p = ", signif(summary(scd_MD_story)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_MD_story)$adj.r.squared, 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig1MD
fig1 <- add_slide(fig1)
fig1 <- ph_with(x = fig1, fig1MD, location = ph_location_type(type = "body"))

L1_scd_story <- lm(mean_L1_r_lower_cingulum_mask ~ story_d * cohort, df_L1)
summary(L1_scd_story)
L1_story <- lm(mean_L1_r_lower_cingulum_mask ~ story_d, df_L1)
summary(L1_story)
ctl_L1_story <- lm(mean_L1_r_lower_cingulum_mask ~ story_d, ctl_L1)
scd_L1_story <- lm(mean_L1_r_lower_cingulum_mask ~ story_d, scd_L1)
fig1L1 <- interact_plot(L1_scd_story, pred = story_d, modx = cohort, 
                        plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                        modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Story Delayed Recall", y = "Mean AxD", title = "Right Mean AxD",
       subtitle = paste0(
         "interaction p = ", signif(summary(L1_scd_story)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      "p = ", signif(summary(ctl_L1_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_L1_story)$adj.r.squared, 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 2.5, hjust = 1.01,
                    label = paste0(
                      # "p = ", signif(summary(scd_L1_story)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_L1_story)$adj.r.squared, 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig1L1
fig1 <- add_slide(fig1)
fig1 <- ph_with(x = fig1, fig1L1, location = ph_location_type(type = "body"))

RD_scd_story <- lm(mean_RD_r_lower_cingulum_mask ~ story_d * cohort, df_RD)
summary(RD_scd_story)
ctl_RD_story <- lm(mean_RD_r_lower_cingulum_mask ~ story_d, ctl_RD)
scd_RD_story <- lm(mean_RD_r_lower_cingulum_mask ~ story_d, scd_RD)
fig1RD <- interact_plot(RD_scd_story, pred = story_d, modx = cohort, 
                        plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                        modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Story Delayed Recall", y = "Mean RD", title = "Right Mean RD",
       subtitle = paste0(
         "interaction p = ",
         signif(summary(RD_scd_story)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      "p = ", signif(summary(ctl_RD_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_RD_story)$adj.r.squared, 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 2.5, hjust = 1.01,
                    label = paste0(
                      # "p = ", signif(summary(scd_RD_story)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_RD_story)$adj.r.squared, 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig1RD
fig1 <- add_slide(fig1)
fig1 <- ph_with(x = fig1, fig1RD, location = ph_location_type(type = "body"))

ISOVF_scd_story <- lm(mean_ISOVF_r_lower_cingulum_mask ~ story_d * cohort, df_ISOVF)
summary(ISOVF_scd_story)
ctl_ISOVF_story <- lm(mean_ISOVF_r_lower_cingulum_mask ~ story_d, ctl_ISOVF)
scd_ISOVF_story <- lm(mean_ISOVF_r_lower_cingulum_mask ~ story_d, scd_ISOVF)
fig1ISOVF <- interact_plot(ISOVF_scd_story, pred = story_d, modx = cohort, 
                           plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                           modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Story Delayed Recall", y = "Mean FWVF", title = "Right Mean FWVF",
       subtitle = paste0(
         "interaction p = ",
         signif(summary(ISOVF_scd_story)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      "p = ", signif(summary(ctl_ISOVF_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_ISOVF_story)$adj.r.squared, 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 2.5, hjust = 1.01,
                    label = paste0(
                      "p = ", signif(summary(scd_ISOVF_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_ISOVF_story)$adj.r.squared, 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig1ISOVF
fig1 <- add_slide(fig1)
fig1 <- ph_with(x = fig1, fig1ISOVF, location = ph_location_type(type = "body"))

ICVF_scd_story <- lm(mean_ICVF_r_lower_cingulum_mask ~ story_d * cohort, df_ICVF)
summary(ICVF_scd_story)
ctl_ICVF_story <- lm(mean_ICVF_r_lower_cingulum_mask ~ story_d, ctl_ICVF)
scd_ICVF_story <- lm(mean_ICVF_r_lower_cingulum_mask ~ story_d, scd_ICVF)
fig1ICVF <- interact_plot(ICVF_scd_story, pred = story_d, modx = cohort, 
                          plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                          modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Story Delayed Recall", y = "Mean IC", title = "Right Mean IC",
       subtitle = paste0(
         "interaction p = ",
         signif(summary(ICVF_scd_story)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(ctl_ICVF_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_ICVF_story)$adj.r.squared, 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(scd_ICVF_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_ICVF_story)$adj.r.squared, 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig1ICVF
fig1 <- add_slide(fig1)
fig1 <- ph_with(x = fig1, fig1ICVF, location = ph_location_type(type = "body"))

OD_scd_story <- lm(mean_OD_r_lower_cingulum_mask ~ story_d * cohort, df_OD)
summary(OD_scd_story)
ctl_OD_story <- lm(mean_OD_r_lower_cingulum_mask ~ story_d, ctl_OD)
scd_OD_story <- lm(mean_OD_r_lower_cingulum_mask ~ story_d, scd_OD)
fig1OD <- interact_plot(OD_scd_story, pred = story_d, modx = cohort, 
                        plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                        modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Story Delayed Recall", y = "Mean OD", title = "Right Mean OD",
       subtitle = paste0(
         "interaction p = ",
         signif(summary(OD_scd_story)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(ctl_OD_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_OD_story)$adj.r.squared, 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(scd_OD_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_OD_story)$adj.r.squared, 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig1OD
fig1 <- add_slide(fig1)
fig1 <- ph_with(x = fig1, fig1OD, location = ph_location_type(type = "body"))

print(fig1, target='fig1.pptx')

################################### scd-age interaction scatterplots ########################################## 
l_ento_scd_age <- lm(lh_entorhinal ~ age * cohort, df)
summary(l_ento_scd_age)
l_ento_age <- lm(lh_entorhinal ~ age, df)
summary(l_ento_age)
ggplot(df, aes(age, rh_entorhinal, color=cohort)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right")
r_ento_scd_age <- lm(rh_entorhinal ~ age * cohort, df)
summary(r_ento_scd_age)
r_ento_age <- lm(rh_entorhinal ~ age, df)
summary(r_ento_age)
ggplot(df, aes(age, rh_entorhinal, color = cohort)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right")
r_temppole_scd_age <- lm(rh_temporalpole ~ age * cohort, df)
summary(r_temppole_scd_age)
r_temppole_age <- lm(rh_temporalpole ~ age, df)
summary(r_temppole_age)
ggplot(df, aes(age, rh_temporalpole, color = cohort)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right")

fig3 <- read_pptx()
layout_summary(fig3)

FA_scd_age <- lm(mean_FA_r_lower_cingulum_mask ~ age * cohort, df_FA)
summary(FA_scd_age)
# ctl_FA_age <- lm(mean_FA_lower_cingulum_mask ~ age, ctl_FA)
# scd_FA_age <- lm(mean_FA_lower_cingulum_mask ~ age, scd_FA)
# fig3FA <- interact_plot(FA_scd_age, pred = age, modx = cohort, 
#                         plot.points = T, interval = T, point.alpha = 1, vary.lty = F,
#                         modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
#   theme(legend.position = 'none') +
#   labs(x = "Mean Centered Age", y = "Mean FA", title = "Bilateral Mean FA",
#        subtitle = paste0(
#          "interaction p = ",
#          signif(summary(FA_scd_age)$coefficients[4,4], 2)
#        )
#   ) +
#   geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
#                     label = paste0(
#                       # "p = ", signif(summary(ctl_FA_age)$coefficients[2,4], 2),
#                       "p < 0.001",
#                       ", adj-R<sup>2</sup> = ", signif(summary(ctl_FA_age)$adj.r.squared, 2)),
#                     color = "Control"), show.legend = F,
#                 fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
#   geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
#                     label = paste0(
#                       # "p = ", signif(summary(scd_FA_age)$coefficients[2,4], 2),
#                       "p < 0.001",
#                       ", adj-R<sup>2</sup> = ", signif(summary(scd_FA_age)$adj.r.squared, 2)),
#                     color = "SCD"), show.legend = F,
#                 fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5))
# fig3FA
# fig3 <- add_slide(fig3)
# fig3 <- ph_with(x = fig3, fig3FA, location = ph_location_type(type = "body"))

MD_scd_age <- lm(mean_MD_r_lower_cingulum_mask ~ age * cohort, df_MD)
summary(MD_scd_age)
ctl_MD_age <- lm(mean_MD_r_lower_cingulum_mask ~ age, ctl_MD)
scd_MD_age <- lm(mean_MD_r_lower_cingulum_mask ~ age, scd_MD)
fig3MD <- interact_plot(MD_scd_age, pred = age, modx = cohort, 
                        plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                        modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean MD", title = "Right Mean MD",
       subtitle = paste0(
         "interaction p = ",
         signif(summary(MD_scd_age)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(ctl_MD_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_MD_age)$adj.r.squared, 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(scd_MD_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_MD_age)$adj.r.squared, 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig3MD
fig3 <- add_slide(fig3)
fig3 <- ph_with(x = fig3, fig3MD, location = ph_location_type(type = "body"))

L1_scd_age <- lm(mean_L1_r_lower_cingulum_mask ~ age * cohort, df_L1)
summary(L1_scd_age)
ctl_L1_age <- lm(mean_L1_r_lower_cingulum_mask ~ age, ctl_L1)
scd_L1_age <- lm(mean_L1_r_lower_cingulum_mask ~ age, scd_L1)
fig3L1 <- interact_plot(L1_scd_age, pred = age, modx = cohort, 
                        plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                        modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean AxD", title = "Right Mean AxD",
       subtitle = paste0(
         # "p < 0.001"
         "interaction p = ", signif(summary(L1_scd_age)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(ctl_L1_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_L1_age)$adj.r.squared, 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(scd_MD_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_L1_age)$adj.r.squared, 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig3L1
fig3 <- add_slide(fig3)
fig3 <- ph_with(x = fig3, fig3L1, location = ph_location_type(type = "body"))

RD_scd_age <- lm(mean_RD_r_lower_cingulum_mask ~ age * cohort, df_RD)
summary(RD_scd_age)
ctl_RD_age <- lm(mean_RD_r_lower_cingulum_mask ~ age, ctl_RD)
scd_RD_age <- lm(mean_RD_r_lower_cingulum_mask ~ age, scd_RD)
fig3RD <- interact_plot(RD_scd_age, pred = age, modx = cohort, 
                        plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                        modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean RD", title = "Right Mean RD",
       subtitle = paste0(
         "interaction p = ",
         signif(summary(RD_scd_age)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(ctl_RD_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_RD_age)$adj.r.squared, 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(scd_RD_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_RD_age)$adj.r.squared, 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig3RD
fig3 <- add_slide(fig3)
fig3 <- ph_with(x = fig3, fig3RD, location = ph_location_type(type = "body"))

ISOVF_scd_age <- lm(mean_ISOVF_r_lower_cingulum_mask ~ age * cohort, df_ISOVF)
summary(ISOVF_scd_age)
ctl_ISOVF_age <- lm(mean_ISOVF_r_lower_cingulum_mask ~ age, ctl_ISOVF)
scd_ISOVF_age <- lm(mean_ISOVF_r_lower_cingulum_mask ~ age, scd_ISOVF)
fig3ISOVF <- interact_plot(ISOVF_scd_age, pred = age, modx = cohort, 
                           plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                           modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean FWVF", title = "Bilateral Mean FWVF",
       subtitle = paste0(
         "interaction p = ",
         signif(summary(ISOVF_scd_age)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(ctl_ISOVF_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_ISOVF_age)$adj.r.squared, 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(scd_ISOVF_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_ISOVF_age)$adj.r.squared, 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig3ISOVF
fig3 <- add_slide(fig3)
fig3 <- ph_with(x = fig3, fig3ISOVF, location = ph_location_type(type = "body"))

ICVF_scd_age <- lm(mean_ICVF_lower_cingulum_mask ~ age * cohort, df_ICVF)
summary(ICVF_scd_age)
ctl_ICVF_age <- lm(mean_ICVF_lower_cingulum_mask ~ age, ctl_ICVF)
scd_ICVF_age <- lm(mean_ICVF_lower_cingulum_mask ~ age, scd_ICVF)
fig3ICVF <- interact_plot(ICVF_scd_age, pred = age, modx = cohort, 
                          plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                          modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean IC", title = "Bilateral Mean IC",
       subtitle = paste0(
         "interaction p = ",
         signif(summary(ICVF_scd_age)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(ctl_ICVF_age)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_ICVF_age)$adj.r.squared, 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(scd_ICVF_age)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_ICVF_age)$adj.r.squared, 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig3ICVF
fig3 <- add_slide(fig3)
fig3 <- ph_with(x = fig3, fig3ICVF, location = ph_location_type(type = "body"))

OD_scd_age <- lm(mean_OD_lower_cingulum_mask ~ age * cohort, df_OD)
summary(OD_scd_age)
ctl_OD_age <- lm(mean_OD_lower_cingulum_mask ~ age, ctl_OD)
scd_OD_age <- lm(mean_OD_lower_cingulum_mask ~ age, scd_OD)
fig3OD <- interact_plot(OD_scd_age, pred = age, modx = cohort, 
                        plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                        modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean OD", title = "Bilateral Mean OD",
       subtitle = paste0(
         "interaction p = ",
         signif(summary(OD_scd_age)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(ctl_OD_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_OD_age)$adj.r.squared, 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(scd_OD_age)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_OD_age)$adj.r.squared, 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig3OD
fig3 <- add_slide(fig3)
fig3 <- ph_with(x = fig3, fig3OD, location = ph_location_type(type = "body"))

print(fig3, target='fig3.pptx')

########################################################### age-story interaction plots ##############################################
l_ento_story_age <- lm(lh_entorhinal ~ story_d * age, df)
summary(l_ento_story_age)
r_ento_story_age <- lm(rh_entorhinal ~ story_d * age, df)
summary(r_ento_story_age)
r_temppole_story_age <- lm(rh_temporalpole ~ story_d * age, df)
summary(r_temppole_story_age)

fig5 <- read_pptx()
layout_summary(fig5)

FA_story_age <- lm(mean_FA_lower_cingulum_mask ~ story_d * age, df_FA)
summary(FA_story_age)
upper_story_FA_age <- lm(mean_FA_lower_cingulum_mask ~ story_d, upper_story_FA)
lower_story_FA_age <- lm(mean_FA_lower_cingulum_mask ~ story_d, lower_story_FA)

fig5FA <- interact_plot(FA_story_age, pred = age, modx = story_d, modx.values = "plus-minus", 
                        plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                        legend.main = 'Delayed Story Recall Score'
) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean FA", title = "Bilateral Mean FA",
       subtitle = paste0(
         "interaction p = ", signif(summary(FA_story_age)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(upper_story_FA_age)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(upper_story_FA_age)$adj.r.squared, 2))),
                color = "darkblue", show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(lower_story_FA_age)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(lower_story_FA_age)$adj.r.squared, 2))), 
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig5FA
fig5 <- add_slide(fig5)
fig5 <- ph_with(x = fig5, fig5FA, location = ph_location_type(type = "body"))

MD_story_age <- lm(mean_MD_lower_cingulum_mask ~ story_d * age, df_MD)
summary(MD_story_age) 
upper_story_MD_age <- lm(mean_MD_lower_cingulum_mask ~ story_d, upper_story_MD)
lower_story_MD_age <- lm(mean_MD_lower_cingulum_mask ~ story_d, lower_story_MD)

fig5MD <- interact_plot(MD_story_age, pred = age, modx = story_d, modx.values = "plus-minus", 
                        plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                        legend.main = 'Delayed Story Recall Score'
) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean MD", title = "Bilateral Mean MD",
       subtitle = paste0(
         "interaction p = ", signif(summary(MD_story_age)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(upper_story_MD_age)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(upper_story_MD_age)$adj.r.squared, 2))),
                color = "darkblue", show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(lower_story_MD_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(lower_story_MD_age)$adj.r.squared, 2))), 
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig5MD
fig5 <- add_slide(fig5)
fig5 <- ph_with(x = fig5, fig5MD, location = ph_location_type(type = "body"))

L1_story_age <- lm(mean_L1_lower_cingulum_mask ~ story_d * age, df_L1)
summary(L1_story_age) #yes
upper_story_L1_age <- lm(mean_L1_lower_cingulum_mask ~ story_d, upper_story_L1)
lower_story_L1_age <- lm(mean_L1_lower_cingulum_mask ~ story_d, lower_story_L1)

fig5L1 <- interact_plot(L1_story_age, pred = age, modx = story_d, modx.values = "plus-minus", 
                        plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                        legend.main = 'Delayed Story Recall Score'
) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean AxD", title = "Bilateral Mean AxD",
       subtitle = paste0(
         "interaction p = ", signif(summary(L1_story_age)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(upper_story_L1_age)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(upper_story_L1_age)$adj.r.squared, 2))),
                color = "darkblue", show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(lower_story_L1_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(lower_story_L1_age)$adj.r.squared, 2))), 
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig5L1
fig5 <- add_slide(fig5)
fig5 <- ph_with(x = fig5, fig5L1, location = ph_location_type(type = "body"))

RD_story_age <- lm(mean_RD_lower_cingulum_mask ~ story_d * age, df_RD)
summary(RD_story_age) #yes
upper_story_RD_age <- lm(mean_RD_lower_cingulum_mask ~ story_d, upper_story_RD)
lower_story_RD_age <- lm(mean_RD_lower_cingulum_mask ~ story_d, lower_story_RD)

fig5RD <- interact_plot(RD_story_age, pred = age, modx = story_d, modx.values = "plus-minus", 
                        plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                        legend.main = 'Delayed Story Recall Score'
) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean RD", title = "Bilateral Mean RD",
       subtitle = paste0(
         "interaction p = ", signif(summary(RD_story_age)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(upper_story_RD_age)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(upper_story_RD_age)$adj.r.squared, 2))),
                color = "darkblue", show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(lower_story_RD_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(lower_story_RD_age)$adj.r.squared, 2))), 
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig5RD
fig5 <- add_slide(fig5)
fig5 <- ph_with(x = fig5, fig5RD, location = ph_location_type(type = "body"))

ISOVF_story_age <- lm(mean_ISOVF_lower_cingulum_mask ~ story_d * age, df_ISOVF)
summary(ISOVF_story_age) #yes
upper_story_ISOVF_age <- lm(mean_ISOVF_lower_cingulum_mask ~ story_d, upper_story_ISOVF)
lower_story_ISOVF_age <- lm(mean_ISOVF_lower_cingulum_mask ~ story_d, lower_story_ISOVF)

fig5ISOVF <- interact_plot(ISOVF_story_age, pred = age, modx = story_d, modx.values = "plus-minus", 
                           plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                           legend.main = 'Delayed Story Recall Score'
) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean FWVF", title = "Bilateral Mean FWVF",
       subtitle = paste0(
         "interaction p = ", signif(summary(ISOVF_story_age)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(upper_story_ISOVF_age)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(upper_story_ISOVF_age)$adj.r.squared, 2))),
                color = "darkblue", show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(lower_story_ISOVF_age)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(lower_story_ISOVF_age)$adj.r.squared, 2))), 
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig5ISOVF
fig5 <- add_slide(fig5)
fig5 <- ph_with(x = fig5, fig5ISOVF, location = ph_location_type(type = "body"))

ICVF_story_age <- lm(mean_ICVF_lower_cingulum_mask ~ story_d * age, df_ICVF)
summary(ICVF_story_age) #yes
upper_story_ICVF_age <- lm(mean_ICVF_lower_cingulum_mask ~ story_d, upper_story_ICVF)
lower_story_ICVF_age <- lm(mean_ICVF_lower_cingulum_mask ~ story_d, lower_story_ICVF)

fig5ICVF <- interact_plot(ICVF_story_age, pred = age, modx = story_d, modx.values = "plus-minus", 
                          plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                          legend.main = 'Delayed Story Recall Score'
) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean ICVF", title = "Bilateral Mean ICVF",
       subtitle = paste0(
         "interaction p = ", signif(summary(ICVF_story_age)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(upper_story_ICVF_age)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(upper_story_ICVF_age)$adj.r.squared, 2))),
                color = "darkblue", show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(lower_story_ICVF_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(lower_story_ICVF_age)$adj.r.squared, 2))), 
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig5ICVF
fig5 <- add_slide(fig5)
fig5 <- ph_with(x = fig5, fig5ICVF, location = ph_location_type(type = "body"))

OD_story_age <- lm(mean_OD_lower_cingulum_mask ~ story_d * age, df_OD)
summary(OD_story_age) #yes
upper_story_OD_age <- lm(mean_OD_lower_cingulum_mask ~ story_d, upper_story_OD)
lower_story_OD_age <- lm(mean_OD_lower_cingulum_mask ~ story_d, lower_story_OD)

fig5OD <- interact_plot(OD_story_age, pred = age, modx = story_d, modx.values = "plus-minus", 
                        plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                        legend.main = 'Delayed Story Recall Score'
) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean OD", title = "Bilateral Mean OD",
       subtitle = paste0(
         "interaction p = ", signif(summary(OD_story_age)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(upper_story_OD_age)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(upper_story_OD_age)$adj.r.squared, 2))),
                color = "darkblue", show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(lower_story_OD_age)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(lower_story_OD_age)$adj.r.squared, 2))), 
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig5OD
fig5 <- add_slide(fig5)
fig5 <- ph_with(x = fig5, fig5OD, location = ph_location_type(type = "body"))

print(fig5, target='fig5.pptx')