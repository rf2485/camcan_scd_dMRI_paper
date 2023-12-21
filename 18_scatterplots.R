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
df <- rbind(ctl_table, scd_table) %>%
  #remove diffusion outliers (already done for thickness/volume)
  mutate(across(ends_with("_mask"), remove_outliers))

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

#split into groups of interest
ctl_df <- df %>% filter(cohort == 'ctl')
scd_df <- df %>% filter(cohort == 'scd')
upper_story_df <- df %>% filter(story_d > 0)
lower_story_df <- df %>% filter(story_d < 0)

######################## scd-story interaction scatterplots #################################################
#device size 6.80x4.86 inches
#width 693 height 495

FA_scd_story <- lm(mean_FA_r_lower_cingulum_mask ~ story_d * cohort, df)
summary(FA_scd_story)
ctl_FA_story <- lm(mean_FA_r_lower_cingulum_mask ~ story_d, ctl_df)
scd_FA_story <- lm(mean_FA_r_lower_cingulum_mask ~ story_d, scd_df)
interact_plot(FA_scd_story, pred = story_d, modx = cohort, 
              plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Story Delayed Recall", y = "Mean FA", title = "Right Mean FA",
       subtitle = paste0(
         "interaction p = ",
         signif(summary(FA_scd_story)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      "p = ", signif(summary(ctl_FA_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_FA_story)$adj.r.squared, 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 2.5, hjust = 1.01,
                    label = paste0(
                      "p = ", signif(summary(scd_FA_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_FA_story)$adj.r.squared, 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

MD_scd_story <- lm(mean_MD_r_lower_cingulum_mask ~ story_d * cohort, df)
summary(MD_scd_story)
ctl_MD_story <- lm(mean_MD_r_lower_cingulum_mask ~ story_d, ctl_df)
scd_MD_story <- lm(mean_MD_r_lower_cingulum_mask ~ story_d, scd_df)
interact_plot(MD_scd_story, pred = story_d, modx = cohort, 
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

L1_scd_story <- lm(mean_L1_r_lower_cingulum_mask ~ story_d * cohort, df)
summary(L1_scd_story)
ctl_L1_story <- lm(mean_L1_r_lower_cingulum_mask ~ story_d, ctl_df)
scd_L1_story <- lm(mean_L1_r_lower_cingulum_mask ~ story_d, scd_df)
interact_plot(L1_scd_story, pred = story_d, modx = cohort, 
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

RD_scd_story <- lm(mean_RD_r_lower_cingulum_mask ~ story_d * cohort, df)
summary(RD_scd_story)
ctl_RD_story <- lm(mean_RD_r_lower_cingulum_mask ~ story_d, ctl_df)
scd_RD_story <- lm(mean_RD_r_lower_cingulum_mask ~ story_d, scd_df)
interact_plot(RD_scd_story, pred = story_d, modx = cohort, 
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

### thickness group-story interaction
l_ento_scd_story <- lm(lh_entorhinal ~ story_d * cohort, df)
summary(l_ento_scd_story)
r_ento_scd_story <- lm(rh_entorhinal ~ story_d * cohort, df)
summary(r_ento_scd_story)
r_temppole_scd_story <- lm(rh_temporalpole ~ story_d * cohort, df)
summary(r_temppole_scd_story)


#left lower cingulum
FA_scd_story <- lm(mean_FA_l_lower_cingulum_mask ~ story_d * cohort, df)
summary(FA_scd_story)
MD_scd_story <- lm(mean_MD_l_lower_cingulum_mask ~ story_d * cohort, df)
summary(MD_scd_story)
L1_scd_story <- lm(mean_L1_l_lower_cingulum_mask ~ story_d * cohort, df)
summary(L1_scd_story)
RD_scd_story <- lm(mean_RD_l_lower_cingulum_mask ~ story_d * cohort, df)
summary(RD_scd_story)

################################### scd-age interaction scatterplots ########################################## 
FA_scd_age <- lm(mean_FA_r_lower_cingulum_mask ~ age * cohort, df)
summary(FA_scd_age)
ctl_FA_age <- lm(mean_FA_r_lower_cingulum_mask ~ age, ctl_df)
scd_FA_age <- lm(mean_FA_r_lower_cingulum_mask ~ age, scd_df)
interact_plot(FA_scd_age, pred = age, modx = cohort, 
              plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean FA", title = "Right Mean FA",
       subtitle = paste0(
         "interaction p = ",
         signif(summary(FA_scd_age)$coefficients[4,4], 2)
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(ctl_FA_age)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_FA_age)$adj.r.squared, 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(scd_FA_age)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_FA_age)$adj.r.squared, 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


MD_scd_age <- lm(mean_MD_r_lower_cingulum_mask ~ age * cohort, df)
summary(MD_scd_age)
ctl_MD_age <- lm(mean_MD_r_lower_cingulum_mask ~ age, ctl_df)
scd_MD_age <- lm(mean_MD_r_lower_cingulum_mask ~ age, scd_df)
interact_plot(MD_scd_age, pred = age, modx = cohort, 
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

L1_scd_age <- lm(mean_L1_r_lower_cingulum_mask ~ age * cohort, df)
summary(L1_scd_age)
ctl_L1_age <- lm(mean_L1_r_lower_cingulum_mask ~ age, ctl_df)
scd_L1_age <- lm(mean_L1_r_lower_cingulum_mask ~ age, scd_df)
interact_plot(L1_scd_age, pred = age, modx = cohort, 
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

RD_scd_age <- lm(mean_RD_r_lower_cingulum_mask ~ age * cohort, df)
summary(RD_scd_age)
ctl_RD_age <- lm(mean_RD_r_lower_cingulum_mask ~ age, ctl_df)
scd_RD_age <- lm(mean_RD_r_lower_cingulum_mask ~ age, scd_df)
interact_plot(RD_scd_age, pred = age, modx = cohort,
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

l_ento_scd_age <- lm(lh_entorhinal ~ age * cohort, df)
summary(l_ento_scd_age)
r_ento_scd_age <- lm(rh_entorhinal ~ age * cohort, df)
summary(r_ento_scd_age)
r_temppole_scd_age <- lm(rh_temporalpole ~ age * cohort, df)
summary(r_temppole_scd_age)

########################################################### age-story interaction plots ##############################################
FA_story_age <- lm(mean_FA_r_lower_cingulum_mask ~ story_d * age, df)
summary(FA_story_age)
# upper_story_FA_age <- lm(mean_FA_r_lower_cingulum_mask ~ story_d, upper_story_df)
# lower_story_FA_age <- lm(mean_FA_r_lower_cingulum_mask ~ story_d, lower_story_df)
# 
# interact_plot(FA_story_age, pred = age, modx = story_d, modx.values = "plus-minus",
#               plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
#               legend.main = 'Delayed Story Recall Score'
# ) +
#   theme(legend.position = 'none') +
#   labs(x = "Mean Centered Age", y = "Mean FA", title = "Bilateral Mean FA",
#        subtitle = paste0(
#          "interaction p = ", signif(summary(FA_story_age)$coefficients[4,4], 2)
#        )
#   ) +
#   geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
#                     label = paste0(
#                       "p = ", signif(summary(upper_story_FA_age)$coefficients[2,4], 2),
#                       # "p < 0.001",
#                       ", adj-R<sup>2</sup> = ", signif(summary(upper_story_FA_age)$adj.r.squared, 2))),
#                 color = "darkblue", show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
#   geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
#                     label = paste0(
#                       "p = ", signif(summary(lower_story_FA_age)$coefficients[2,4], 2),
#                       # "p < 0.001",
#                       ", adj-R<sup>2</sup> = ", signif(summary(lower_story_FA_age)$adj.r.squared, 2))),
#                 show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5))

MD_story_age <- lm(mean_MD_r_lower_cingulum_mask ~ story_d * age, df)
summary(MD_story_age) 

L1_story_age <- lm(mean_L1_lower_cingulum_mask ~ story_d * age, df)
summary(L1_story_age) 

RD_story_age <- lm(mean_RD_lower_cingulum_mask ~ story_d * age, df)
summary(RD_story_age) 

l_ento_story_age <- lm(lh_entorhinal ~ story_d * age, df)
summary(l_ento_story_age)
r_ento_story_age <- lm(rh_entorhinal ~ story_d * age, df)
summary(r_ento_story_age)
r_temppole_story_age <- lm(rh_temporalpole ~ story_d * age, df)
summary(r_temppole_story_age)

######################## diffusion-thickness scatterplots ##################
#check for correlation
FA_l_ento <- lm(mean_FA_l_lower_cingulum_mask ~ lh_entorhinal, df)
summary(FA_l_ento)
#with age
FA_l_ento_age <- lm(mean_FA_l_lower_cingulum_mask ~ lh_entorhinal + age , df)
summary(FA_l_ento_age)
#check for interaction
FA_scd_l_ento <- lm(mean_FA_l_lower_cingulum_mask ~ lh_entorhinal * cohort, df)
summary(FA_scd_l_ento)
#plot best model
ggplot(df, aes(lh_entorhinal, mean_FA_l_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "left") +
  labs(title = "Left Mean FA", x = "Left Entorhinal Cortical Thickness", y = "Mean FA") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

MD_l_ento <- lm(mean_MD_l_lower_cingulum_mask ~ lh_entorhinal, df)
summary(MD_l_ento)
MD_l_ento_age <- lm(mean_MD_l_lower_cingulum_mask ~ lh_entorhinal + age, df)
summary(MD_l_ento_age)
MD_scd_l_ento <- lm(mean_MD_l_lower_cingulum_mask ~ lh_entorhinal * cohort, df)
summary(MD_scd_l_ento)
ggplot(df, aes(lh_entorhinal, mean_MD_l_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right") +
  labs(title = "Left Mean MD", x = "Left Entorhinal Cortical Thickness", y = "Mean MD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

L1_l_ento <- lm(mean_L1_l_lower_cingulum_mask ~ lh_entorhinal, df)
summary(L1_l_ento)
L1_l_ento_age <- lm(mean_L1_l_lower_cingulum_mask ~ lh_entorhinal + age, df)
summary(L1_l_ento_age)
L1_scd_l_ento <- lm(mean_L1_l_lower_cingulum_mask ~ lh_entorhinal * cohort, df)
summary(L1_scd_l_ento)
ggplot(df, aes(lh_entorhinal, mean_L1_l_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right") +
  labs(title = "Left Mean AxD", x = "Left Entorhinal Cortical Thickness", y = "Mean AxD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

RD_l_ento <- lm(mean_RD_l_lower_cingulum_mask ~ lh_entorhinal, df)
summary(RD_l_ento)
RD_l_ento_age <- lm(mean_RD_l_lower_cingulum_mask ~ lh_entorhinal + age, df)
summary(RD_l_ento_age)
RD_scd_l_ento <- lm(mean_RD_l_lower_cingulum_mask ~ lh_entorhinal * cohort, df)
summary(RD_scd_l_ento)
ggplot(df, aes(lh_entorhinal, mean_RD_l_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "right") +
  labs(title = "Left Mean RD", x = "Left Entorhinal Cortical Thickness", y = "Mean RD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

FA_r_ento <- lm(mean_FA_r_lower_cingulum_mask ~ rh_entorhinal, df)
summary(FA_r_ento)
FA_r_ento_age <- lm(mean_FA_r_lower_cingulum_mask ~ rh_entorhinal + age, df)
summary(FA_r_ento_age)
FA_scd_r_ento <- lm(mean_FA_r_lower_cingulum_mask ~ rh_entorhinal * cohort, df)
summary(FA_scd_r_ento)
ggplot(df, aes(rh_entorhinal, mean_FA_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "right") +
  labs(title = "Right Mean FA", x = "Right Entorhinal Cortical Thickness", y = "Mean FA") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

MD_r_ento <- lm(mean_MD_r_lower_cingulum_mask ~ rh_entorhinal, df)
summary(MD_r_ento)
MD_r_ento_age <- lm(mean_MD_r_lower_cingulum_mask ~ rh_entorhinal + age, df)
summary(MD_r_ento_age)
MD_scd_r_ento <- lm(mean_MD_r_lower_cingulum_mask ~ rh_entorhinal * cohort, df)
summary(MD_scd_r_ento)
ggplot(df, aes(rh_entorhinal, mean_MD_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "right") +
  labs(title = "Right Mean MD", x = "Right Entorhinal Cortical Thickness", y = "Mean MD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

L1_r_ento <- lm(mean_L1_r_lower_cingulum_mask ~ rh_entorhinal, df)
summary(L1_r_ento)
L1_r_ento_age <- lm(mean_L1_r_lower_cingulum_mask ~ rh_entorhinal + age, df)
summary(L1_r_ento_age)
L1_scd_r_ento <- lm(mean_L1_r_lower_cingulum_mask ~ rh_entorhinal * cohort, df)
summary(L1_scd_r_ento)
ggplot(df, aes(rh_entorhinal, mean_L1_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "right") +
  labs(title = "Right Mean AxD", x = "Right Entorhinal Cortical Thickness", y = "Mean AxD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
    
RD_r_ento <- lm(mean_RD_r_lower_cingulum_mask ~ rh_entorhinal, df)
summary(RD_r_ento)
RD_r_ento_age <- lm(mean_RD_r_lower_cingulum_mask ~ rh_entorhinal +age, df)
summary(RD_r_ento_age)
RD_scd_r_ento <- lm(mean_RD_r_lower_cingulum_mask ~ rh_entorhinal * cohort, df)
summary(RD_scd_r_ento)
ggplot(df, aes(rh_entorhinal, mean_RD_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "right") +
  labs(title = "Right Mean RD", x = "Right Entorhinal Cortical Thickness", y = "Mean RD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

FA_r_temppole <- lm(mean_FA_r_lower_cingulum_mask ~ rh_temporalpole, df)
summary(FA_r_temppole)
FA_r_temppole_age <- lm(mean_FA_r_lower_cingulum_mask ~ rh_temporalpole + age, df)
summary(FA_r_temppole_age)
FA_scd_r_temppole <- lm(mean_FA_r_lower_cingulum_mask ~ rh_temporalpole * cohort, df)
summary(FA_scd_r_temppole)
ggplot(df, aes(rh_temporalpole, mean_FA_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "right") +
  labs(title = "Right Mean FA", x = "Right Temporal Pole Cortical Thickness", y = "Mean FA") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

MD_r_temppole <- lm(mean_MD_r_lower_cingulum_mask ~ rh_temporalpole, df)
summary(MD_r_temppole)
MD_r_temppole_age <- lm(mean_MD_r_lower_cingulum_mask ~ rh_temporalpole + age, df)
summary(MD_r_temppole_age)
MD_scd_r_temppole <- lm(mean_MD_r_lower_cingulum_mask ~ rh_temporalpole * cohort, df)
summary(MD_scd_r_temppole)
ggplot(df, aes(rh_temporalpole, mean_MD_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "right", vjust = 0.3) +
  labs(title = "Right Mean MD", x = "Right Temporal Pole Cortical Thickness", y = "Mean MD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

L1_r_temppole <- lm(mean_L1_r_lower_cingulum_mask ~ rh_temporalpole, df)
summary(L1_r_temppole)
L1_r_temppole_age <- lm(mean_L1_r_lower_cingulum_mask ~ rh_temporalpole + age, df)
summary(L1_r_temppole_age)
L1_scd_r_temppole <- lm(mean_L1_r_lower_cingulum_mask ~ rh_temporalpole * cohort, df)
summary(L1_scd_r_temppole)
ggplot(df, aes(rh_temporalpole, mean_L1_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "right", vjust = 0.3) +
  labs(title = "Right Mean AxD", x = "Right Temporal Pole Cortical Thickness", y = "Mean AxD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

RD_r_temppole <- lm(mean_RD_r_lower_cingulum_mask ~ rh_temporalpole, df)
summary(RD_r_temppole)
RD_r_temppole_age <- lm(mean_RD_r_lower_cingulum_mask ~ rh_temporalpole + age, df)
summary(RD_r_temppole_age)
RD_scd_r_temppole <- lm(mean_RD_r_lower_cingulum_mask ~ rh_temporalpole * cohort, df)
summary(RD_scd_r_temppole)
ggplot(df, aes(rh_temporalpole, mean_RD_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label(c("P", "adj.R2")), small.p = T, formula = y ~ x, label.x = "right", vjust=0.3) +
  labs(title = "Right Mean RD", x = "Right Temporal Pole Cortical Thickness", y = "Mean RD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

