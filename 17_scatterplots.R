library(tidyverse)
library(ggpmisc)
library(interactions)
library(ggtext)
library(modelsummary)

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
df_all <- rbind(ctl_table, scd_table)

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
volumes_thickness_all <- read_csv("volumes_thickness_all.csv") %>%
  select(-SCD)
df_all <- left_join(df_all, volumes_thickness_all, by='participant_id')
df_all <- df_all %>%
  mutate(cohort = ifelse(SCD == 1, 'scd', 'ctl'), 
         story_d = scale(homeint_storyrecall_d, scale = F), 
         age = scale(age, scale = F)) 
df_all <- df_all %>% select(participant_id, cohort, SCD, age, story_d, everything())
write.csv(df_all, "final_dataframe_all.csv")

df$mean_MD_r_lower_cingulum_mask <- df$mean_MD_r_lower_cingulum_mask * 10^3
df$mean_L1_r_lower_cingulum_mask <- df$mean_L1_r_lower_cingulum_mask * 10^3
df$mean_RD_r_lower_cingulum_mask <- df$mean_RD_r_lower_cingulum_mask * 10^3
df$mean_MD_l_lower_cingulum_mask <- df$mean_MD_l_lower_cingulum_mask * 10^3
df$mean_L1_l_lower_cingulum_mask <- df$mean_L1_l_lower_cingulum_mask * 10^3
df$mean_RD_l_lower_cingulum_mask <- df$mean_RD_l_lower_cingulum_mask * 10^3

#split into groups of interest
ctl_df <- df %>% filter(cohort == 'ctl')
scd_df <- df %>% filter(cohort == 'scd')
upper_story_df <- df %>% filter(story_d > 0)
lower_story_df <- df %>% filter(story_d < 0)
ctl_df_all <- df_all %>% filter(cohort == 'ctl')
scd_df_all <- df_all %>% filter(cohort == 'scd')
upper_story_df_all <- df_all %>% filter(story_d > 0)
lower_story_df_all <- df_all %>% filter(story_d < 0)

######################## scd-story interaction scatterplots #################################################
#graph width 693 length 495

FA_scd_story <- lm(mean_FA_r_lower_cingulum_mask ~ story_d * cohort, df)
summary(FA_scd_story)
ctl_FA_story <- lm(mean_FA_r_lower_cingulum_mask ~ story_d, ctl_df)
scd_FA_story <- lm(mean_FA_r_lower_cingulum_mask ~ story_d, scd_df)
interact_plot(FA_scd_story, pred = story_d, modx = cohort, 
              plot.points = T, interval = T, point.alpha = 1, vary.lty = F,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Story Delayed Recall", y = "Mean FA", title = "Right Mean FA",
       subtitle = bquote(
         "interaction p = " * .(signif(summary(FA_scd_story)$coefficients[4,4], 2)) *
         ", adj-R"^2 * " = " * .(signif(summary(FA_scd_story)$adj.r.squared, 2)) *
         ", \u03B2 = " * .(signif(summary(FA_scd_story)$coefficients[4,1], 2))
       )
  ) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      "p = ", signif(summary(ctl_FA_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_FA_story)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(ctl_FA_story)$coefficients[2,1], 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 2.5, hjust = 1.01,
                    label = paste0(
                      "p = ", signif(summary(scd_FA_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_FA_story)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(scd_FA_story)$coefficients[2,1], 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  scale_y_continuous(expand = expansion(mult = c(0.02,0.1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

MD_scd_story <- lm(mean_MD_r_lower_cingulum_mask ~ story_d * cohort, df)
summary(MD_scd_story)
MD_scd_story_all <- lm(mean_MD_r_lower_cingulum_mask ~ story_d * cohort, df_all)
summary(MD_scd_story_all)
ctl_MD_story <- lm(mean_MD_r_lower_cingulum_mask ~ story_d, ctl_df)
scd_MD_story <- lm(mean_MD_r_lower_cingulum_mask ~ story_d, scd_df)
interact_plot(MD_scd_story, pred = story_d, modx = cohort, 
              plot.points = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Story Delayed Recall", y = "Mean MD", title = "Right Mean MD",
       subtitle = bquote(
         "interaction p = " * .(signif(summary(MD_scd_story)$coefficients[4,4], 2)) *
           ", adj-R"^2 * " = " * .(signif(summary(MD_scd_story)$adj.r.squared, 2)) *
           ", \u03B2 = " * .(signif(summary(MD_scd_story)$coefficients[4,1], 2))
       )
  ) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      "p = ", signif(summary(ctl_MD_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_MD_story)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(ctl_MD_story)$coefficients[2,1], 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 2.5, hjust = 1.01,
                    label = paste0(
                      # "p = ", signif(summary(scd_MD_story)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_MD_story)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(scd_MD_story)$coefficients[2,1], 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  scale_y_continuous(expand = expansion(mult = c(0.02,0.1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

L1_scd_story <- lm(mean_L1_r_lower_cingulum_mask ~ story_d * cohort, df)
summary(L1_scd_story)
L1_scd_story_all <- lm(mean_L1_r_lower_cingulum_mask ~ story_d * cohort, df_all)
summary(L1_scd_story_all)
ctl_L1_story <- lm(mean_L1_r_lower_cingulum_mask ~ story_d, ctl_df)
scd_L1_story <- lm(mean_L1_r_lower_cingulum_mask ~ story_d, scd_df)
interact_plot(L1_scd_story, pred = story_d, modx = cohort, 
              plot.points = T, interval = T, point.alpha = 1, vary.lty = F,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Story Delayed Recall", y = "Mean AxD", title = "Right Mean AxD",
       subtitle = bquote(
         "interaction p = " * .(signif(summary(L1_scd_story)$coefficients[4,4], 2)) *
           ", adj-R"^2 * " = " * .(signif(summary(L1_scd_story)$adj.r.squared, 2)) *
           ", \u03B2 = " * .(signif(summary(L1_scd_story)$coefficients[4,1], 2))
       )
  ) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      "p = ", signif(summary(ctl_L1_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_L1_story)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(ctl_L1_story)$coefficients[2,1], 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 2.5, hjust = 1.01,
                    label = paste0(
                      # "p = ", signif(summary(scd_L1_story)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_L1_story)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(scd_L1_story)$coefficients[2,1], 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  scale_y_continuous(expand = expansion(mult = c(0.02,0.1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

RD_scd_story <- lm(mean_RD_r_lower_cingulum_mask ~ story_d * cohort, df)
summary(RD_scd_story)
RD_scd_story_all <- lm(mean_RD_r_lower_cingulum_mask ~ story_d * cohort, df_all)
summary(RD_scd_story_all)
ctl_RD_story <- lm(mean_RD_r_lower_cingulum_mask ~ story_d, ctl_df)
scd_RD_story <- lm(mean_RD_r_lower_cingulum_mask ~ story_d, scd_df)
interact_plot(RD_scd_story, pred = story_d, modx = cohort, 
              plot.points = T, interval = T, point.alpha = 1, vary.lty = F,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Story Delayed Recall", y = "Mean RD", title = "Right Mean RD",
       subtitle = bquote(
         "interaction p = " * .(signif(summary(RD_scd_story)$coefficients[4,4], 2)) *
           ", adj-R"^2 * " = " * .(signif(summary(RD_scd_story)$adj.r.squared, 2)) *
           ", \u03B2 = " * .(signif(summary(RD_scd_story)$coefficients[4,1], 2))
       )
  ) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      "p = ", signif(summary(ctl_RD_story)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_RD_story)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(ctl_RD_story)$coefficients[2,1], 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 2.5, hjust = 1.01,
                    label = paste0(
                      # "p = ", signif(summary(scd_RD_story)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_RD_story)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(scd_RD_story)$coefficients[2,1], 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  scale_y_continuous(expand = expansion(mult = c(0.02,0.1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#left lower cingulum
l_FA_scd_story <- lm(mean_FA_l_lower_cingulum_mask ~ story_d * cohort, df)
summary(l_FA_scd_story)
l_FA_scd_story_all <- lm(mean_FA_l_lower_cingulum_mask ~ story_d * cohort, df_all)
summary(l_FA_scd_story_all)
l_MD_scd_story <- lm(mean_MD_l_lower_cingulum_mask ~ story_d * cohort, df)
summary(l_MD_scd_story)
l_MD_scd_story_all <- lm(mean_MD_l_lower_cingulum_mask ~ story_d * cohort, df_all)
summary(l_MD_scd_story_all)
l_L1_scd_story <- lm(mean_L1_l_lower_cingulum_mask ~ story_d * cohort, df)
summary(l_L1_scd_story)
l_L1_scd_story_all <- lm(mean_L1_l_lower_cingulum_mask ~ story_d * cohort, df_all)
summary(l_L1_scd_story_all)
l_RD_scd_story <- lm(mean_RD_l_lower_cingulum_mask ~ story_d * cohort, df)
summary(l_RD_scd_story)
l_RD_scd_story_all <- lm(mean_RD_l_lower_cingulum_mask ~ story_d * cohort, df_all)
summary(l_RD_scd_story_all)

l_diff_story_models <- list(
  "Left FA" = l_FA_scd_story,
  "Left MD" = l_MD_scd_story,
  "Left AxD" = l_L1_scd_story,
  "Left RD" = l_RD_scd_story
)
modelsummary(l_diff_story_models,
             statistic = c("SE = {std.error}",
                           "t = {statistic}",
                           "p = {p.value}"),
             gof_map = "adj.r.squared",
             coef_map = c("story_d" = "Delayed Story Recall \u03B2",
                          "cohortscd" = "Group (SCD vs Control) \u03B2",
                          "story_d:cohortscd" = "Story x Group \u03B2"),
             fmt = fmt_statistic(estimate = fmt_sprintf("%.2e"),
                                 std.error = fmt_sprintf("%.2e")),
             output = "l_diff_story_models.docx")

### thickness group-story interaction
l_ento_scd_story <- lm(lh_entorhinal ~ story_d * cohort, df)
summary(l_ento_scd_story)
l_ento_scd_story_all <- lm(lh_entorhinal ~ story_d * cohort, df_all)
summary(l_ento_scd_story_all)
r_ento_scd_story <- lm(rh_entorhinal ~ story_d * cohort, df)
summary(r_ento_scd_story)
r_ento_scd_story_all <- lm(rh_entorhinal ~ story_d * cohort, df_all)
summary(r_ento_scd_story_all)
r_temppole_scd_story <- lm(rh_temporalpole ~ story_d * cohort, df)
summary(r_temppole_scd_story)
r_temppole_scd_story_all <- lm(rh_temporalpole ~ story_d * cohort, df_all)
summary(r_temppole_scd_story_all)

thickness_story_models <- list(
  "Left Entorhinal" = l_ento_scd_story,
  "Right Entorhinal" = r_ento_scd_story,
  "Right Temporal Pole" = r_temppole_scd_story
)
modelsummary(thickness_story_models,
             statistic = c("SE = {std.error}",
                           "t = {statistic}",
                           "p = {p.value}"),
             gof_map = "adj.r.squared",
             coef_map = c("story_d" = "Delayed Story Recall \u03B2",
                          "cohortscd" = "Group (SCD vs Control) \u03B2",
                          "story_d:cohortscd" = "Story x Group \u03B2"),
             fmt = fmt_statistic(estimate = fmt_sprintf("%.2e"),
                                 std.error = fmt_sprintf("%.2e")),
             output = "thickness_story_models.docx"
             )
################################### scd-age interaction scatterplots ########################################## 
FA_scd_age <- lm(mean_FA_r_lower_cingulum_mask ~ age * cohort, df)
summary(FA_scd_age)
ctl_FA_age <- lm(mean_FA_r_lower_cingulum_mask ~ age, ctl_df)
scd_FA_age <- lm(mean_FA_r_lower_cingulum_mask ~ age, scd_df)
interact_plot(FA_scd_age, pred = age, modx = cohort, 
              plot.points = T, interval = T, point.alpha = 1, vary.lty = F,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean FA", title = "Right Mean FA",
       subtitle = bquote(
         "interaction p = " * .(signif(summary(FA_scd_age)$coefficients[4,4], 2)) *
           ", adj-R"^2 * " = " * .(signif(summary(FA_scd_age)$adj.r.squared, 2)) *
           ", \u03B2 = " * .(signif(summary(FA_scd_age)$coefficients[4,1], 2))
       )
  ) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      # "p = ", signif(summary(ctl_FA_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_FA_age)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(ctl_FA_age)$coefficients[2,1], 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 2.5, hjust = 1.01,
                    label = paste0(
                      # "p = ", signif(summary(scd_FA_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_FA_age)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(scd_FA_age)$coefficients[2,1], 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

MD_scd_age <- lm(mean_MD_r_lower_cingulum_mask ~ age * cohort, df)
summary(MD_scd_age)
MD_scd_age_all <- lm(mean_MD_r_lower_cingulum_mask ~ age * cohort, df_all)
summary(MD_scd_age_all)
ctl_MD_age <- lm(mean_MD_r_lower_cingulum_mask ~ age, ctl_df)
scd_MD_age <- lm(mean_MD_r_lower_cingulum_mask ~ age, scd_df)
interact_plot(MD_scd_age, pred = age, modx = cohort, 
              plot.points = T, interval = T, point.alpha = 1, vary.lty = F,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean MD", title = "Right Mean MD",
       subtitle = bquote(
         "interaction p = " * .(signif(summary(MD_scd_age)$coefficients[4,4], 2)) *
           ", adj-R"^2 * " = " * .(signif(summary(MD_scd_age)$adj.r.squared, 2)) *
           ", \u03B2 = " * .(signif(summary(MD_scd_age)$coefficients[4,1], 2))
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(ctl_MD_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_MD_age)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(ctl_MD_age)$coefficients[2,1], 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(scd_MD_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_MD_age)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(scd_MD_age)$coefficients[2,1], 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

L1_scd_age <- lm(mean_L1_r_lower_cingulum_mask ~ age * cohort, df)
summary(L1_scd_age)
L1_scd_age_all <- lm(mean_L1_r_lower_cingulum_mask ~ age * cohort, df_all)
summary(L1_scd_age_all)
ctl_L1_age <- lm(mean_L1_r_lower_cingulum_mask ~ age, ctl_df)
scd_L1_age <- lm(mean_L1_r_lower_cingulum_mask ~ age, scd_df)
interact_plot(L1_scd_age, pred = age, modx = cohort, 
              plot.points = T, interval = T, point.alpha = 1, vary.lty = F,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean AxD", title = "Right Mean AxD",
       subtitle = bquote(
         "interaction p = " * .(signif(summary(L1_scd_age)$coefficients[4,4], 2)) *
           ", adj-R"^2 * " = " * .(signif(summary(L1_scd_age)$adj.r.squared, 2)) *
           ", \u03B2 = " * .(signif(summary(L1_scd_age)$coefficients[4,1], 2))
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(ctl_L1_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_L1_age)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(ctl_L1_age)$coefficients[2,1], 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(scd_L1_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_L1_age)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(scd_L1_age)$coefficients[2,1], 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

RD_scd_age <- lm(mean_RD_r_lower_cingulum_mask ~ age * cohort, df)
summary(RD_scd_age)
RD_scd_age_all <- lm(mean_RD_r_lower_cingulum_mask ~ age * cohort, df_all)
summary(RD_scd_age_all)
ctl_RD_age <- lm(mean_RD_r_lower_cingulum_mask ~ age, ctl_df)
scd_RD_age <- lm(mean_RD_r_lower_cingulum_mask ~ age, scd_df)
interact_plot(RD_scd_age, pred = age, modx = cohort, 
              plot.points = T, interval = T, point.alpha = 1, vary.lty = F,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean RD", title = "Right Mean RD",
       subtitle = bquote(
         "interaction p = " * .(signif(summary(RD_scd_age)$coefficients[4,4], 2)) *
           ", adj-R"^2 * " = " * .(signif(summary(RD_scd_age)$adj.r.squared, 2)) *
           ", \u03B2 = " * .(signif(summary(RD_scd_age)$coefficients[4,1], 2))
       )
  ) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(ctl_RD_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(ctl_RD_age)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(ctl_RD_age)$coefficients[2,1], 2)),
                    color = "Control"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 2.5, hjust = -0.01,
                    label = paste0(
                      # "p = ", signif(summary(scd_RD_age)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(scd_RD_age)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(scd_RD_age)$coefficients[2,1], 2)),
                    color = "SCD"), show.legend = F,
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  scale_y_continuous(expand = expansion(mult = c(0.02,0.1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Left cingulum
l_FA_scd_age <- lm(mean_FA_l_lower_cingulum_mask ~ age * cohort, df)
summary(l_FA_scd_age)
l_FA_scd_age_all <- lm(mean_FA_l_lower_cingulum_mask ~ age * cohort, df_all)
summary(l_FA_scd_age_all)
l_MD_scd_age <- lm(mean_MD_l_lower_cingulum_mask ~ age * cohort, df)
summary(l_MD_scd_age)
l_MD_scd_age_all <- lm(mean_MD_l_lower_cingulum_mask ~ age * cohort, df_all)
summary(l_MD_scd_age_all)
l_L1_scd_age <- lm(mean_L1_l_lower_cingulum_mask ~ age * cohort, df)
summary(l_L1_scd_age)
l_L1_scd_age_all <- lm(mean_L1_l_lower_cingulum_mask ~ age * cohort, df_all)
summary(l_L1_scd_age_all)
l_RD_scd_age <- lm(mean_RD_l_lower_cingulum_mask ~ age * cohort, df)
summary(l_RD_scd_age)
l_RD_scd_age_all <- lm(mean_RD_l_lower_cingulum_mask ~ age * cohort, df_all)
summary(l_RD_scd_age_all)

l_diff_age_models <- list(
  "Left FA" = l_FA_scd_age,
  "Left MD" = l_MD_scd_age,
  "Left AxD" = l_L1_scd_age,
  "Left RD" = l_RD_scd_age
)
modelsummary(l_diff_age_models,
             statistic = c("SE = {std.error}",
                           "t = {statistic}",
                           "p = {p.value}"),
             gof_map = "adj.r.squared",
             coef_map = c("age" = "Age \u03B2",
                          "cohortscd" = "Group (SCD vs Control) \u03B2",
                          "age:cohortscd" = "age x Group \u03B2"),
             fmt = fmt_statistic(estimate = fmt_sprintf("%.2e"),
                                 std.error = fmt_sprintf("%.2e")),
             output = "l_diff_age_models.docx")

l_ento_scd_age <- lm(lh_entorhinal ~ age * cohort, df)
summary(l_ento_scd_age)
l_ento_scd_age_all <- lm(lh_entorhinal ~ age * cohort, df_all)
summary(l_ento_scd_age_all)
r_ento_scd_age <- lm(rh_entorhinal ~ age * cohort, df)
summary(r_ento_scd_age)
r_ento_scd_age_all <- lm(rh_entorhinal ~ age * cohort, df_all)
summary(r_ento_scd_age_all)
r_temppole_scd_age <- lm(rh_temporalpole ~ age * cohort, df)
summary(r_temppole_scd_age)
r_temppole_scd_age_all <- lm(rh_temporalpole ~ age * cohort, df_all)
summary(r_temppole_scd_age_all)

thick_age_models <- list(
  "Left Entorhinal" = l_ento_scd_age,
  "Right Entorhinal" = r_ento_scd_age,
  "Right Temporal Pole" = r_temppole_scd_age
)
modelsummary(thick_age_models,
             statistic = c("SE = {std.error}",
                           "t = {statistic}",
                           "p = {p.value}"),
             gof_map = "adj.r.squared",
             coef_map = c("age" = "Age \u03B2",
                          "cohortscd" = "Group (SCD vs Control) \u03B2",
                          "age:cohortscd" = "age x Group \u03B2"),
             fmt = fmt_statistic(estimate = fmt_sprintf("%.2e"),
                                 std.error = fmt_sprintf("%.2e")),
             output = "thick_age_models.docx"
)

######################## age-story recall ###########################
age_story_r_diff_models <- list(
  "Right FA" = lm(mean_FA_r_lower_cingulum_mask ~ story_d * age, df),
  "Right MD" = lm(mean_MD_r_lower_cingulum_mask ~ story_d * age, df),
  "Right AxD" = lm(mean_L1_r_lower_cingulum_mask ~ story_d * age, df),
  "Right RD" = lm(mean_RD_r_lower_cingulum_mask ~ story_d * age, df)
)
modelsummary(age_story_r_diff_models,
             statistic = c("SE = {std.error}",
                           "t = {statistic}",
                           "p = {p.value}"),
             gof_map = "adj.r.squared",
             coef_map = c("story_d" = "Delayed Story Recall \u03B2",
                          "age" = "Age \u03B2",
                          "story_d:age" = "Age x Story \u03B2"),
             fmt = fmt_statistic(estimate = fmt_sprintf("%.2e"),
                                 std.error = fmt_sprintf("%.2e")),
             output = "age_story_r_diff_models.docx"
)
age_story_l_diff_models <- list(
  "Left FA" = lm(mean_FA_l_lower_cingulum_mask ~ story_d * age, df),
  "Left MD" = lm(mean_MD_l_lower_cingulum_mask ~ story_d * age, df),
  "Left AxD" = lm(mean_L1_l_lower_cingulum_mask ~ story_d * age, df),
  "Left RD" = lm(mean_RD_l_lower_cingulum_mask ~ story_d * age, df)
)
modelsummary(age_story_l_diff_models,
             statistic = c("SE = {std.error}",
                           "t = {statistic}",
                           "p = {p.value}"),
             gof_map = "adj.r.squared",
             coef_map = c("story_d" = "Delayed Story Recall \u03B2",
                          "age" = "Age \u03B2",
                          "story_d:age" = "Age x Story \u03B2"),
             fmt = fmt_statistic(estimate = fmt_sprintf("%.2e"),
                                 std.error = fmt_sprintf("%.2e")),
             output = "age_story_l_diff_models.docx"
)
age_story_thick_models <- list(
  "Left Entorhinal" = lm(lh_entorhinal ~ story_d * age, df),
  "Right Entorhinal" = lm(rh_entorhinal ~ story_d * age, df),
  "Right Temporal Pole" = lm(rh_entorhinal ~ story_d * age, df)
)
modelsummary(age_story_thick_models,
             statistic = c("SE = {std.error}",
                           "t = {statistic}",
                           "p = {p.value}"),
             gof_map = "adj.r.squared",
             coef_map = c("story_d" = "Delayed Story Recall \u03B2",
                          "age" = "Age \u03B2",
                          "story_d:age" = "Age x Story \u03B2"),
             fmt = fmt_statistic(estimate = fmt_sprintf("%.2e"),
                                 std.error = fmt_sprintf("%.2e")),
             output = "age_story_thick_models.docx"
)
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
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(FA_l_ento)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(FA_l_ento)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(FA_l_ento)$coefficients[2,1], 2))), 
                fill = NA, label.color = NA) +
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
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(MD_l_ento)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(MD_l_ento)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(MD_l_ento)$coefficients[2,1], 2))), 
                fill = NA, label.color = NA) +
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
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(L1_l_ento)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(L1_l_ento)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(L1_l_ento)$coefficients[2,1], 2))), 
                fill = NA, label.color = NA) +
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
  geom_richtext(aes(x = -Inf, y = Inf, vjust = 1.1, hjust = -0.01,
                    label = paste0(
                      "p = ", signif(summary(RD_l_ento)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(RD_l_ento)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(RD_l_ento)$coefficients[2,1], 2))), 
                fill = NA, label.color = NA) +
  labs(title = "Left Mean RD", x = "Left Entorhinal Cortical Thickness", y = "Mean RD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

l_ento_diff_models <- list(
  "Left FA" = FA_scd_l_ento,
  "Left MD" = MD_scd_l_ento,
  "Left AxD" = L1_scd_l_ento,
  "Left RD" = RD_scd_l_ento
)
modelsummary(l_ento_diff_models,
             statistic = c("SE = {std.error}",
                           "t = {statistic}",
                           "p = {p.value}"),
             gof_map = "adj.r.squared",
             coef_map = c("lh_entorhinal" = "Thickness \u03B2",
                          "cohortscd" = "Group \u03B2",
                          "lh_entorhinal:cohortscd" = "Thickness x Group \u03B2"),
             fmt = fmt_statistic(estimate = fmt_sprintf("%.2e"),
                                 std.error = fmt_sprintf("%.2e")),
             output = "l_ento_diff_models.docx"
)
l_ento_age_models <- list(
  "Left FA" = FA_l_ento_age,
  "Left MD" = MD_l_ento_age,
  "Left AxD" = L1_l_ento_age,
  "Left RD" = RD_l_ento_age
)
modelsummary(l_ento_age_models,
             statistic = c("SE = {std.error}",
                           "t = {statistic}",
                           "p = {p.value}"),
             gof_map = "adj.r.squared",
             coef_map = c("lh_entorhinal" = "Thickness \u03B2",
                          "age" = "Age \u03B2",
                          "lh_entorhinal:age" = "Thickness x Age \u03B2"),
             fmt = fmt_statistic(estimate = fmt_sprintf("%.2e"),
                                 std.error = fmt_sprintf("%.2e")),
             output = "l_ento_age_models.docx"
)

FA_r_ento <- lm(mean_FA_r_lower_cingulum_mask ~ rh_entorhinal, df)
summary(FA_r_ento)
FA_r_ento_age <- lm(mean_FA_r_lower_cingulum_mask ~ rh_entorhinal + age, df)
summary(FA_r_ento_age)
FA_scd_r_ento <- lm(mean_FA_r_lower_cingulum_mask ~ rh_entorhinal * cohort, df)
summary(FA_scd_r_ento)
ggplot(df, aes(rh_entorhinal, mean_FA_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      # "p = ", signif(summary(FA_r_ento)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(FA_r_ento)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(FA_r_ento)$coefficients[2,1], 2))), 
                fill = NA, label.color = NA) +
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
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      # "p = ", signif(summary(MD_r_ento)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(MD_r_ento)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(MD_r_ento)$coefficients[2,1], 2))), 
                fill = NA, label.color = NA) +
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
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      # "p = ", signif(summary(L1_r_ento)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(L1_r_ento)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(L1_r_ento)$coefficients[2,1], 2))), 
                fill = NA, label.color = NA) +
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
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      # "p = ", signif(summary(RD_r_ento)$coefficients[2,4], 2),
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(RD_r_ento)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(RD_r_ento)$coefficients[2,1], 2))), 
                fill = NA, label.color = NA) +
  labs(title = "Right Mean RD", x = "Right Entorhinal Cortical Thickness", y = "Mean RD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

r_ento_diff_models <- list(
  "Right FA" = FA_scd_r_ento,
  "Right MD" = MD_scd_r_ento,
  "Right AxD" = L1_scd_r_ento,
  "Right RD" = RD_scd_r_ento
)
modelsummary(r_ento_diff_models,
             statistic = c("SE = {std.error}",
                           "t = {statistic}",
                           "p = {p.value}"),
             gof_map = "adj.r.squared",
             coef_map = c("rh_entorhinal" = "Thickness \u03B2",
                          "cohortscd" = "Group \u03B2",
                          "rh_entorhinal:cohortscd" = "Thickness x Group \u03B2"),
             fmt = fmt_statistic(estimate = fmt_sprintf("%.2e"),
                                 std.error = fmt_sprintf("%.2e")),
             output = "r_ento_diff_models.docx"
)
r_ento_age_models <- list(
  "Right FA" = FA_r_ento_age,
  "Right MD" = MD_r_ento_age,
  "Right AxD" = L1_r_ento_age,
  "Right RD" = RD_r_ento_age
)
modelsummary(r_ento_age_models,
             statistic = c("SE = {std.error}",
                           "t = {statistic}",
                           "p = {p.value}"),
             gof_map = "adj.r.squared",
             coef_map = c("rh_entorhinal" = "Thickness \u03B2",
                          "age" = "Age \u03B2",
                          "rh_entorhinal:age" = "Thickness x Age \u03B2"),
             fmt = fmt_statistic(estimate = fmt_sprintf("%.2e"),
                                 std.error = fmt_sprintf("%.2e")),
             output = "r_ento_age_models.docx"
)

FA_r_temppole <- lm(mean_FA_r_lower_cingulum_mask ~ rh_temporalpole, df)
summary(FA_r_temppole)
FA_r_temppole_age <- lm(mean_FA_r_lower_cingulum_mask ~ rh_temporalpole + age, df)
summary(FA_r_temppole_age)
FA_scd_r_temppole <- lm(mean_FA_r_lower_cingulum_mask ~ rh_temporalpole * cohort, df)
summary(FA_scd_r_temppole)
ggplot(df, aes(rh_temporalpole, mean_FA_r_lower_cingulum_mask)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      "p = ", signif(summary(FA_r_temppole)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(FA_r_temppole)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(FA_r_temppole)$coefficients[2,1], 2))), 
                fill = NA, label.color = NA) +
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
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      "p = ", signif(summary(MD_r_temppole)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(MD_r_temppole)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(MD_r_temppole)$coefficients[2,1], 2))), 
                fill = NA, label.color = NA) +
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
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      "p = ", signif(summary(L1_r_temppole)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(L1_r_temppole)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(L1_r_temppole)$coefficients[2,1], 2))), 
                fill = NA, label.color = NA) +
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
  geom_richtext(aes(x = Inf, y = Inf, vjust = 1.1, hjust = 1.01,
                    label = paste0(
                      "p = ", signif(summary(RD_r_temppole)$coefficients[2,4], 2),
                      # "p < 0.001",
                      ", adj-R<sup>2</sup> = ", signif(summary(RD_r_temppole)$adj.r.squared, 2),
                      ", \u03B2 = ", signif(summary(RD_r_temppole)$coefficients[2,1], 2))), 
                fill = NA, label.color = NA) +
  labs(title = "Right Mean RD", x = "Right Temporal Pole Cortical Thickness", y = "Mean RD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

r_temp_diff_models <- list(
  "Right FA" = FA_scd_r_temppole,
  "Right MD" = MD_scd_r_temppole,
  "Right AxD" = L1_scd_r_temppole,
  "Right RD" = RD_scd_r_temppole
)
modelsummary(r_temp_diff_models,
             statistic = c("SE = {std.error}",
                           "t = {statistic}",
                           "p = {p.value}"),
             gof_map = "adj.r.squared",
             coef_map = c("rh_temporalpole" = "Thickness \u03B2",
                          "cohortscd" = "Group \u03B2",
                          "rh_temporalpole:cohortscd" = "Thickness x Group \u03B2"),
             fmt = fmt_statistic(estimate = fmt_sprintf("%.2e"),
                                 std.error = fmt_sprintf("%.2e")),
             output = "r_temp_diff_models.docx"
)
r_temp_age_models <- list(
  "Right FA" = FA_r_temppole_age,
  "Right MD" = MD_r_temppole_age,
  "Right AxD" = L1_r_temppole_age,
  "Right RD" = RD_r_temppole_age
)
modelsummary(r_temp_age_models,
             statistic = c("SE = {std.error}",
                           "t = {statistic}",
                           "p = {p.value}"),
             gof_map = "adj.r.squared",
             coef_map = c("rh_temporalpole" = "Thickness \u03B2",
                          "age" = "Age \u03B2",
                          "rh_temporalpole:age" = "Thickness x Age \u03B2"),
             fmt = fmt_statistic(estimate = fmt_sprintf("%.2e"),
                                 std.error = fmt_sprintf("%.2e")),
             output = "r_temp_age_models.docx"
)

#### cognition - atrophy - diffusion anaylyses ######
## story-atrophy
story_l_ento <- lm(story_d ~ lh_entorhinal, df)
summary(story_l_ento)
story_r_ento <- lm(story_d ~ rh_entorhinal, df)
summary(story_r_ento)
story_r_temppole <- lm(story_d ~ rh_temporalpole, df)
summary(story_r_temppole) #no significant correlations

##story-diffusion
story_r_FA <- lm(story_d ~ mean_FA_r_lower_cingulum_mask, df)
summary(story_r_FA)
story_l_FA <- lm(story_d ~ mean_FA_l_lower_cingulum_mask, df)
summary(story_l_FA)
story_r_MD <- lm(story_d ~ mean_MD_r_lower_cingulum_mask, df) 
summary(story_r_MD) #significant
story_l_MD <- lm(story_d ~ mean_MD_l_lower_cingulum_mask, df) 
summary(story_l_MD)
story_r_L1 <- lm(story_d ~ mean_L1_r_lower_cingulum_mask, df)
summary(story_r_L1) #significant
story_l_L1 <- lm(story_d ~ mean_L1_l_lower_cingulum_mask, df)
summary(story_l_L1)
story_r_RD <- lm(story_d ~ mean_RD_r_lower_cingulum_mask, df)
summary(story_r_RD) #significant
story_l_RD <- lm(story_d ~ mean_RD_l_lower_cingulum_mask, df)
summary(story_l_RD)

##story-diffusion-atrophy
story_l_ento_FA <- lm(story_d ~ lh_entorhinal + mean_FA_r_lower_cingulum_mask, df)
summary(story_l_ento_FA)
story_l_ento_MD <- lm(story_d ~ lh_entorhinal + mean_MD_r_lower_cingulum_mask, df)
summary(story_l_ento_MD)
story_l_ento_L1 <- lm(story_d ~ lh_entorhinal + mean_L1_r_lower_cingulum_mask, df)
summary(story_l_ento_L1)
story_l_ento_RD <- lm(story_d ~ lh_entorhinal + mean_RD_r_lower_cingulum_mask, df)
summary(story_l_ento_RD)
story_r_ento_FA <- lm(story_d ~ rh_entorhinal + mean_FA_r_lower_cingulum_mask, df)
summary(story_r_ento_FA)
story_r_ento_MD <- lm(story_d ~ rh_entorhinal + mean_MD_r_lower_cingulum_mask, df)
summary(story_r_ento_MD)
story_r_ento_L1 <- lm(story_d ~ rh_entorhinal + mean_L1_r_lower_cingulum_mask, df)
summary(story_r_ento_L1)
story_r_ento_RD <- lm(story_d ~ rh_entorhinal + mean_RD_r_lower_cingulum_mask, df)
summary(story_r_ento_RD)
story_r_temppole_FA <- lm(story_d ~ rh_temporalpole + mean_FA_r_lower_cingulum_mask, df)
summary(story_r_temppole_FA)
story_r_temppole_MD <- lm(story_d ~ rh_temporalpole + mean_MD_r_lower_cingulum_mask, df)
summary(story_r_temppole_MD)
story_r_temppole_L1 <- lm(story_d ~ rh_temporalpole + mean_L1_r_lower_cingulum_mask, df)
summary(story_r_temppole_L1)
story_r_temppole_RD <- lm(story_d ~ rh_temporalpole + mean_RD_r_lower_cingulum_mask, df)
summary(story_r_temppole_RD)

#all metrics together
story_diffusion <- lm(story_d ~ mean_FA_r_lower_cingulum_mask + 
                        mean_MD_r_lower_cingulum_mask +
                        mean_L1_r_lower_cingulum_mask +
                        mean_RD_r_lower_cingulum_mask, 
                      df)
summary(story_diffusion)
story_diffusion_atrophy <- lm(story_d ~ mean_FA_r_lower_cingulum_mask + 
                                mean_MD_r_lower_cingulum_mask +
                                mean_L1_r_lower_cingulum_mask +
                                mean_RD_r_lower_cingulum_mask +
                                lh_entorhinal + rh_entorhinal + rh_temporalpole,
                              df)
summary(story_diffusion_atrophy)
