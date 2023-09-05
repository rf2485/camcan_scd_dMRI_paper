library(tidyverse)
library(jtools)
library(ggpmisc)
library(interactions)
library(officer)
library(ggtext)

ismrm_plots_ppts <- read_pptx()
layout_summary(scatterplots_ppts)

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

#add age and story
age_story_mat <- read_tsv("tbss/stats/age_story_mat.txt", col_names = F)
df$age <- age_story_mat$X2
df$story_d <- age_story_mat$X3

### scd-story interaction scatterplots ###

FA_r_lower_cingulum_scd_story_d <- lm(mean_FA_r_lower_cingulum_mask ~ story_d * cohort, df)
fig1FA <- ggplot(df, aes(story_d, mean_FA_r_lower_cingulum_mask, color = cohort)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right") +
  labs(x = "Story Delayed Recall", y = "Mean FA",
       color = "Cohort", title = "Right Mean FA",
       subtitle = paste0(
         "interaction p = ", 
         signif(summary(FA_r_lower_cingulum_scd_story_d)$coefficients[4,4], 2))) +
  scale_color_hue(labels = c("Control", "SCD")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig1FA
ismrm_plots_ppts <- add_slide(ismrm_plots_ppts)
ismrm_plots_ppts <- ph_with(x = ismrm_plots_ppts, fig1FA, location = ph_location_type(type = "body"))

MD_r_lower_cingulum_scd_story_d <- lm(mean_MD_r_lower_cingulum_mask ~ story_d + cohort + cohort * story_d, df)
summary(MD_r_lower_cingulum_scd_story_d)
fig1MD <- ggplot(df, aes(story_d, mean_MD_r_lower_cingulum_mask, color = cohort)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right") +
  labs(x = "Story Delayed Recall", y = "Mean MD",
       color = "Cohort", title = "Right Mean MD",
       subtitle = paste0(
         "interaction p = ", 
         signif(summary(MD_r_lower_cingulum_scd_story_d)$coefficients[4,4], 2))) +
  scale_color_hue(labels = c("Control", "SCD")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig1MD
ismrm_plots_ppts <- add_slide(ismrm_plots_ppts)
ismrm_plots_ppts <- ph_with(x = ismrm_plots_ppts, fig1MD, location = ph_location_type(type = "body"))

L1_r_lower_cingulum_scd_story_d <- lm(mean_L1_r_lower_cingulum_mask ~ story_d + cohort + cohort * story_d, df)
summary(L1_r_lower_cingulum_scd_story_d)
fig1AD <- ggplot(df, aes(story_d, mean_L1_r_lower_cingulum_mask, color = cohort)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right") +
  labs(x = "Story Delayed Recall", y = "Mean AxD",
       color = "Cohort", title = "Right Mean AxD",
       subtitle = paste0(
         "interaction p = ", 
         signif(summary(L1_r_lower_cingulum_scd_story_d)$coefficients[4,4], 2))) +
  scale_color_hue(labels = c("Control", "SCD")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig1AD
ismrm_plots_ppts <- add_slide(ismrm_plots_ppts)
ismrm_plots_ppts <- ph_with(x = ismrm_plots_ppts, fig1AD, location = ph_location_type(type = "body"))

RD_r_lower_cingulum_scd_story_d <- lm(mean_RD_r_lower_cingulum_mask ~ story_d * cohort, df)
summary(RD_r_lower_cingulum_scd_story_d)
fig1RD <- ggplot(df, aes(story_d, mean_RD_r_lower_cingulum_mask, color = cohort)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right") +
  labs(x = "Story Delayed Recall", y = "Mean RD",
       color = "Cohort", title = "Right Mean RD ",
       subtitle = paste0(
         "interaction p = ", 
         signif(summary(RD_r_lower_cingulum_scd_story_d)$coefficients[4,4], 2))) +
  scale_color_hue(labels = c("Control", "SCD")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

fig1RD
ismrm_plots_ppts <- add_slide(ismrm_plots_ppts)
ismrm_plots_ppts <- ph_with(x = ismrm_plots_ppts, fig1RD, location = ph_location_type(type = "body"))

ISOVF_r_lower_cingulum_scd_story_d <- lm(mean_ISOVF_r_lower_cingulum_mask ~ story_d * cohort, df)
summary(ISOVF_r_lower_cingulum_scd_story_d)

fig1FWVF <- ggplot(df, aes(story_d, mean_ISOVF_r_lower_cingulum_mask, color = cohort)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right") +
  labs(x = "Story Delayed Recall", y = "Mean FW",
       color = "Cohort", title = "Right Mean FW",
       subtitle = paste0(
         "interaction p = ", 
         signif(summary(ISOVF_r_lower_cingulum_scd_story_d)$coefficients[4,4], 2))) +
  scale_color_hue(labels = c("Control", "SCD")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

fig1FWVF
ismrm_plots_ppts <- add_slide(ismrm_plots_ppts)
ismrm_plots_ppts <- ph_with(x = ismrm_plots_ppts, fig1FWVF, location = ph_location_type(type = "body"))

print(ismrm_plots_ppts, target='scd-story scatterplots.pptx')

### scd-age interaction scatterplots ### 
lc_interaction_pptx <- read_pptx()
layout_summary(lc_interaction_pptx)

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

df_MD <- df 
df_MD$mean_MD_lower_cingulum_mask <- remove_outliers(df$mean_MD_lower_cingulum_mask)
df_MD <- df_MD %>% drop_na(mean_MD_lower_cingulum_mask)

MD_cohort_int_age_gender <- lm(mean_MD_lower_cingulum_mask ~ cohort * age + gender, df_MD)
summary(MD_cohort_int_age_gender) #yes
ctl_MD <- df_MD %>% filter(cohort == 'ctl')
scd_MD <- df_MD %>% filter(cohort == 'scd')
ctl_MD_age_gender <- lm(mean_MD_lower_cingulum_mask ~ age + gender, ctl_MD)
scd_MD_age_gender <- lm(mean_MD_lower_cingulum_mask ~ age + gender, scd_MD)

MD_cohort_int_age_gender_plot <- interact_plot(MD_cohort_int_age_gender, pred = age, modx = cohort, 
                                               partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                                               modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean MD", title = "Bilateral Mean MD",
       subtitle = paste0("Corrected for Gender \n",
                         "interaction p = ",
                         signif(summary(MD_cohort_int_age_gender)$coefficients[5,4], 2)
       )
  ) +
  geom_richtext(aes(x = -10, y = 7e-04, vjust = -1, 
                    # label = paste0("p = ", signif(summary(ctl_MD_age_gender)$coefficients[2,4], 2)), 
                    label = paste0("p < 0.001, adj-R<sup>2</sup> = ",
                                   signif(summary(ctl_MD_age_gender)$adj.r.squared, 2)),
                    color = "Control"), show.legend = F, 
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = -10, y = 7e-04, vjust = 1, 
                    # label = paste0("p = ", signif(summary(scd_MD_age_gender)$coefficients[2,4], 2)), 
                    label = paste0("p < 0.001, adj-R<sup>2</sup> = ",
                                   signif(summary(scd_MD_age_gender)$adj.r.squared, 2)),
                    color = "SCD"), show.legend = F, 
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
MD_cohort_int_age_gender_plot

lc_interaction_pptx <- add_slide(lc_interaction_pptx)
lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, MD_cohort_int_age_gender_plot, location = ph_location_type(type = "body"))

df_L1 <- df 
df_L1$mean_L1_lower_cingulum_mask <- remove_outliers(df$mean_L1_lower_cingulum_mask)
df_L1 <- df_L1 %>% drop_na(mean_L1_lower_cingulum_mask)
L1_cohort_int_age_gender <- lm(mean_L1_lower_cingulum_mask ~ cohort * age + gender, df_L1)
summary(L1_cohort_int_age_gender) #yes
ctl_L1 <- df_L1 %>% filter(cohort == 'ctl')
scd_L1 <- df_L1 %>% filter(cohort == 'scd')
ctl_L1_age_gender <- lm(mean_L1_lower_cingulum_mask ~ age + gender, ctl_L1)
scd_L1_age_gender <- lm(mean_L1_lower_cingulum_mask ~ age + gender, scd_L1)

L1_cohort_int_age_gender_plot <- interact_plot(L1_cohort_int_age_gender, pred = age, modx = cohort, 
                                               partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                                               modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean AD", title = "Bilateral Mean AD",
       subtitle = paste0("Corrected for Gender \n",
                         "interaction p < 0.001"
                         # "interaction p = ",
                         # signif(summary(L1_cohort_int_age_gender)$coefficients[5,4], 2)
       )
  ) +
  geom_richtext(aes(x = -10, y = 9.5e-04, vjust = -1, 
                    label = paste0(
                      "p = ", signif(summary(ctl_L1_age_gender)$coefficients[2,4], 2), 
                      ", adj-R<sup>2</sup> = ",signif(summary(ctl_L1_age_gender)$adj.r.squared, 2)),
                    color = "Control"), 
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = -10, y = 9.5e-04, vjust = 1, 
                    label = paste0(
                      "p = ", signif(summary(scd_L1_age_gender)$coefficients[2,4], 2), 
                      ", adj-R<sup>2</sup> = ",signif(summary(scd_L1_age_gender)$adj.r.squared, 2)),
                    color = "SCD"), 
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
L1_cohort_int_age_gender_plot

lc_interaction_pptx <- add_slide(lc_interaction_pptx)
lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, L1_cohort_int_age_gender_plot, location = ph_location_type(type = "body"))

df_RD <- df 
df_RD$mean_RD_lower_cingulum_mask <- remove_outliers(df$mean_RD_lower_cingulum_mask)
df_RD <- df_RD %>% drop_na(mean_RD_lower_cingulum_mask)
RD_cohort_int_age_gender <- lm(mean_RD_lower_cingulum_mask ~ cohort * age + gender, df_RD)
summary(RD_cohort_int_age_gender) #yes
ctl_RD <- df_RD %>% filter(cohort == 'ctl')
scd_RD <- df_RD %>% filter(cohort == 'scd')
ctl_RD_age_gender <- lm(mean_RD_lower_cingulum_mask ~ age + gender, ctl_RD)
scd_RD_age_gender <- lm(mean_RD_lower_cingulum_mask ~ age + gender, scd_RD)

RD_cohort_int_age_gender_plot <- interact_plot(RD_cohort_int_age_gender, pred = age, modx = cohort, 
                                               partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                                               modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean RD", title = "Bilateral Mean RD",
       subtitle = paste0("Corrected for Gender \n",
                         "interaction p = ",
                         signif(summary(RD_cohort_int_age_gender)$coefficients[5,4], 2)
       )
  ) +
  geom_richtext(aes(x = -10, y = 6e-04, vjust = -1, 
                    label = paste0(
                      # "p = ", signif(summary(ctl_RD_age_gender)$coefficients[2,4], 2), 
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ",signif(summary(ctl_RD_age_gender)$adj.r.squared, 2)),
                    color = "Control"), 
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = -10, y = 6e-04, vjust = 1, 
                    label = paste0(
                      # "p = ", signif(summary(scd_RD_age_gender)$coefficients[2,4], 2), 
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ",signif(summary(scd_RD_age_gender)$adj.r.squared, 2)),
                    color = "SCD"), 
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
RD_cohort_int_age_gender_plot

lc_interaction_pptx <- add_slide(lc_interaction_pptx)
lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, RD_cohort_int_age_gender_plot, location = ph_location_type(type = "body"))

print(lc_interaction_pptx, target = "lc_interaction.pptx")

### age-story interaction plots ### 
L1_story_d_int_age_gender <- lm(mean_L1_lower_cingulum_mask ~ story_d * age + gender, df_L1)
summary(L1_story_d_int_age_gender) #yes

L1_story_d_int_age_gender_plot_1 <- interact_plot(L1_story_d_int_age_gender, pred = age, modx = story_d, modx.values = "plus-minus", 
                                                  partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                                                  legend.main = 'Delayed Story Recall Score'
) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean AD", title = "Bilateral Mean AD",
       subtitle = paste0("Corrected for Gender \n",
                         "interaction p = ",
                         signif(summary(L1_story_d_int_age_gender)$coefficients[5,4], 2)
       )
  ) +
  geom_richtext(aes(x = -10, y = 9.5e-04, vjust = -1, 
                    label = paste0(
                      "p < 0.001", 
                      ", adj-R<sup>2</sup> = ",signif(summary(upper_story_L1_age_gender)$adj.r.squared, 2))),
                color = "darkblue", show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = -10, y = 9.5e-04, vjust = 1, 
                    label = paste0(
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ",signif(summary(lower_story_L1_age_gender)$adj.r.squared, 2)
                    )),
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
L1_story_d_int_age_gender_plot_1

lc_interaction_pptx <- add_slide(lc_interaction_pptx)
lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, L1_story_d_int_age_gender_plot_1, location = ph_location_type(type = "body"))

upper_age_L1 <- df_L1 %>% filter(age > 0)
lower_age_L1 <- df_L1 %>% filter(age < 0)
upper_age_L1_story_gender <- lm(mean_L1_lower_cingulum_mask ~ story_d + gender, upper_age_L1)
lower_age_L1_story_gender <- lm(mean_L1_lower_cingulum_mask ~ story_d + gender, lower_age_L1)

L1_story_d_int_age_gender_plot_2 <- interact_plot(L1_story_d_int_age_gender, pred = story_d, modx = age, modx.values = "plus-minus", 
                                                  partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                                                  legend.main = 'Age'
) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Story Delayed Recall Score", y = "Mean AD", title = "Bilateral Mean AD",
       subtitle = paste0("Corrected for Gender \n",
                         "interaction p = ",
                         signif(summary(L1_story_d_int_age_gender)$coefficients[5,4], 2)
       )
  ) +
  geom_richtext(aes(x = 8, y = 9.5e-04, vjust = -1, 
                    label = paste0(
                      "p = ", signif(summary(upper_age_L1_story_gender)$coefficients[2,4], 2), 
                      ", adj-R<sup>2</sup> = ",signif(summary(upper_age_L1_story_gender)$adj.r.squared, 2))),
                color = "darkblue", show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = 8, y = 9.5e-04, vjust = 1, 
                    label = paste0(
                      "p = ", signif(summary(lower_age_L1_story_gender)$coefficients[2,4], 2) 
                      # ", adj-R<sup>2</sup> = ",signif(summary(lower_age_L1_story_gender)$adj.r.squared, 2)
                    )),
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
L1_story_d_int_age_gender_plot_2

lc_interaction_pptx <- add_slide(lc_interaction_pptx)
lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, L1_story_d_int_age_gender_plot_2, location = ph_location_type(type = "body"))
