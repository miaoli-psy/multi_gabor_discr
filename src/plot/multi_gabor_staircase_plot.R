# libraires ---------------------------------------------------------------
# install.packages("readxl",
#                  "dplyr",
#                  "tidyverse",
#                  "sjPlot",
#                  "glmmTMB",
#                  "rstatix",
#                  "emmeans",
#                  "sjstats",
#                  "MuMIn",
#                  "multcomp",
#                  "nlme",
#                  "r2glmm",
#                  "ggplot2",
#                  "ggthemes",
#                  "svglite",
#                  "ggpubr")
# 

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(svglite)
library(ggpubr)

# set working path---------------------------------------------------------
getwd()
setwd("d:/OneDrive/projects/multi_gabor_discr/src/plot/")

# --------------------Exp1 & Exp2 Threshold--------------------------------
# read data

# gbr_sc_threshold1.xlsx and gbr_sc_threshold2.xlsx

data_exp1 <- readxl::read_excel(path = file.choose())
data_exp2 <- readxl::read_excel(path = file.choose())

# data_preprocessed <- data_exp1
data_preprocessed <- data_exp2

# check threshold single gabor

data <- data_preprocessed %>% 
  group_by(trials.setsize) %>% 
  summarise(
    threshold_mean = mean(trials.intensity),
    threshold_std = sd(trials.intensity)
  )

# all condition
data_by_subject <- data_preprocessed %>%
  group_by(participant,
           trials.setsize,
           gabor_arrangment,
           gabor_type,
           full_condition) %>%
  summarise(
    threshold_mean = mean(trials.intensity),
    threshold_std = sd(trials.intensity),
    n = n() 
  ) %>%
  mutate(
    threshold_SEM = threshold_std / sqrt(n),
    threshold_CI = threshold_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )



data_across_subject <- data_preprocessed %>%
  group_by(trials.setsize,
           gabor_arrangment,
           gabor_type,
           full_condition) %>%
  summarise(
    threshold_mean = mean(trials.intensity),
    threshold_std = sd(trials.intensity),
    n = n() 
  ) %>%
  mutate(
    threshold_SEM = threshold_std / sqrt(n),
    threshold_CI = threshold_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


my_plot <-  ggplot() +
  
  geom_point(
    data = data_across_subject,
    aes(
      x = trials.setsize,
      y = threshold_mean,
      group = gabor_arrangment,
      color = gabor_arrangment,
      size = 0.5
    ),
    
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.6
  ) +
  
  geom_point(
    data = data_by_subject,
    aes(
      x = trials.setsize,
      y = threshold_mean,
      group = gabor_arrangment,
      color = gabor_arrangment,
      size = 0.5
    ),
    alpha = 0.05,
    position = position_dodge(0.5)
  ) +
  
  
  geom_errorbar(
    data = data_across_subject,
    aes(
      x = trials.setsize,
      y = threshold_mean,
      ymin = threshold_mean - threshold_SEM,
      ymax = threshold_mean + threshold_SEM,
      group = gabor_arrangment
    ),
    color = "black",
    size  = 0.8,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  
  labs(y = "Threshold", x = "Set size") +
  
  
  # scale_color_manual(
  #   labels = c("radial", "tangential", "setsize1"),
  #   values = c("#BB5566", "#004488", "grey"),
  #   name = "anisotropy"
  # ) +

  scale_color_manual(
    values = c(radial = "#BB5566", tangential = "#004488", s1 = "grey"),
    name = "anisotropy"
  ) +

  
  scale_y_continuous(limits = c(1, 5)) +
  
  scale_x_continuous(breaks = c(1, 2, 3, 5, 7), 
                     labels = c("1", "2", "3", "5", "7"), limits = c(0.5, 7.5))+
  
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  
  
  facet_wrap(~ gabor_type, nrow = 1)


print(my_plot)


my_plot2 <-  ggplot() +
  
  geom_point(
    data = data_across_subject,
    aes(
      x = trials.setsize,
      y = threshold_mean,
      group = gabor_type,
      color = gabor_type,
      size = 0.5
    ),
    
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.6
  ) +
  
  geom_point(
    data = data_by_subject,
    aes(
      x = trials.setsize,
      y = threshold_mean,
      group = gabor_type,
      color = gabor_type,
      size = 0.5
    ),
    alpha = 0.05,
    position = position_dodge(0.5)
  ) +
  
  
  geom_errorbar(
    data = data_across_subject,
    aes(
      x = trials.setsize,
      y = threshold_mean,
      ymin = threshold_mean - threshold_SEM,
      ymax = threshold_mean + threshold_SEM,
      group = gabor_type
    ),
    color = "black",
    size  = 0.8,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  
  labs(y = "Threshold", x = "Set size") +
  

  scale_color_manual(
    labels = c("ladder", "snake"),
    values = c("#674EA7", "#F28522"), #DDAA33
    name = "gabor type"
  ) +

  
  scale_y_continuous(limits = c(1, 5)) +
  
  scale_x_continuous(breaks = c(1, 2, 3, 5, 7), 
                     labels = c("1", "2", "3", "5", "7"), limits = c(0.5, 7.5))+
  
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  
  
  facet_wrap(~ gabor_arrangment, nrow = 1)


print(my_plot2)

# ggsave(file = "test.svg", plot = my_plot, width = 14.7, height = 6.27, units = "in")


my_plot3 <-  ggplot() +
  
  geom_point(
    data = data_across_subject,
    aes(
      x = trials.setsize,
      y = threshold_mean,
      group = full_condition,
      color = full_condition,
      size = 0.5
    ),
    
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.6
  ) +
  
  
  stat_smooth(
    data = data_across_subject,
    aes(
      x = trials.setsize,
      y = threshold_mean,
      group = full_condition,
      color = full_condition
    ),
    method = "lm",
    size = 3,
    se = FALSE,
    alpha = 0.5,
    geom = "line"
  )+
  
  # geom_point(
  #   data = data_by_subject,
  #   aes(
  #     x = trials.setsize,
  #     y = threshold_mean,
  #     group = condition,
  #     color = condition,
  #     size = 0.5
  #   ),
  #   alpha = 0.05,
  #   position = position_dodge(0.5)
  # ) +
  # 
  
  geom_errorbar(
    data = data_across_subject,
    aes(
      x = trials.setsize,
      y = threshold_mean,
      ymin = threshold_mean - threshold_SEM,
      ymax = threshold_mean + threshold_SEM,
      group = full_condition,
      color = full_condition
    ),
    size  = 0.8,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  
  labs(y = "Threshold (°)", x = "Set size") +
  

  # scale_color_manual(
  #   labels = c("radial_ladder", "radial_snake", "tangential_ladder", "tangential_snake" ),
  #   values = c("#BB5566", "#674EA7", "#004488", "#F28522"), #DDAA33
  #   name = "anisotropy"
  # ) +

  
  scale_color_manual(
    values = c(ladder_radial = "#BB5566", ladder_s1 = "grey", 
               ladder_tangential = "#004488", snake_radial = "#674EA7",
               snake_s1 = "black", snake_tangential = "#DDAA33"),
    name = "anisotropy"
  ) +
  
  scale_y_continuous(limits = c(1, 3.5)) +
  
  scale_x_continuous(breaks = c(1, 2, 3, 5, 7), 
                     labels = c("1", "2", "3", "5", "7"), limits = c(0.5, 7.5))+
  
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, 'cm'),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  )
  


print(my_plot3)

ggsave(file = "test.svg", plot = my_plot3, width = 5, height = 4.5, units = "in")


# check threshold by participant
my_plot3.0 <-  ggplot() +
  
  geom_point(
    data = data_by_subject,
    aes(
      x = trials.setsize,
      y = threshold_mean,
      group = full_condition,
      color = full_condition,
      size = 0.5
    ),
    alpha = 0.85,
    position = position_dodge(0.5)
  ) +
  
  stat_smooth(
    data = data_by_subject,
    aes(
      x = trials.setsize,
      y = threshold_mean,
      group = full_condition,
      color = full_condition
    ),
    method = "lm",
    size = 3,
    se = FALSE,
    alpha = 0.5,
    geom = "line"
  )+
  
  
  labs(y = "Threshold (°)", x = "Set size") +
  
  
  # scale_color_manual(
  #   labels = c("radial_ladder", "radial_snake", "tangential_ladder", "tangential_snake" ),
  #   values = c("#BB5566", "#674EA7", "#004488", "#F28522"), #DDAA33
  #   name = "anisotropy"
  # ) +
  
  
  scale_color_manual(
    values = c(ladder_radial = "#BB5566", ladder_s1 = "grey", 
               ladder_tangential = "#004488", snake_radial = "#674EA7",
               snake_s1 = "black", snake_tangential = "#DDAA33"),
    name = "anisotropy"
  ) +
  
  # scale_y_continuous(limits = c(1, 3.5)) +
  
  scale_x_continuous(breaks = c(1, 2, 3, 5, 7), 
                     labels = c("1", "2", "3", "5", "7"), limits = c(0.5, 7.5))+
  
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, 'cm'),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  
  facet_wrap(~ participant)


print(my_plot3.0)

# --------------------Exp3: 2 tasks - Threshold------------------------

data_exp3 <- readxl::read_excel(path = file.choose())
data_exp4 <- readxl::read_excel(path = file.choose())

exp <- "exp4"
check_exp4_innermost_color <- TRUE

if (exp == "exp3") {
  data_preprocessed3 <- data_exp3
} else if (exp == "exp4") {
  data_preprocessed3 <- data_exp4
}


grouping_vars <- c(
  "setsize",
  "gabor_type"
)

grouping_vars_by_sub <- c(
  "setsize",
  "gabor_type",
  "participant"
)

# add group_by_at vars only if exp4, and when checking 
# threshold of the innermost color 

if (exp == "exp4") {
  if (check_exp4_innermost_color){
    grouping_vars <- c(grouping_vars, "innermost_color")
    grouping_vars_by_sub <- c(grouping_vars_by_sub, "innermost_color")
  }
}

colnames(data_preprocessed3)


data_by_subject3 <- data_preprocessed3 %>%
  group_by_at(grouping_vars_by_sub) %>%
  summarise(
    threshold_mean = mean(intensity),
    threshold_std = sd(intensity),
    n = n() 
  ) %>%
  mutate(
    threshold_SEM = threshold_std / sqrt(n),
    threshold_CI = threshold_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_subject3 <- data_preprocessed3 %>%
  group_by_at(grouping_vars) %>%
  summarise(
    threshold_mean = mean(intensity),
    threshold_std = sd(intensity),
    n = n() 
  ) %>%
  mutate(
    threshold_SEM = threshold_std / sqrt(n),
    threshold_CI = threshold_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


my_plot4 <-  ggplot() +
  
  geom_point(
    data = data_across_subject3,
    aes(
      x = setsize,
      y = threshold_mean,
      group = gabor_type,
      color = gabor_type,
      size = 0.5
    ),
    
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.6
  ) +
  
  # geom_point(
  #   data = data_by_subject3,
  #   aes(
  #     x = setsize,
  #     y = threshold_mean,
  #     group = s_l,
  #     color = s_l,
  #     size = 0.5
  #   ),
  #   alpha = 0.05,
  #   position = position_dodge(0.5)
  # ) +

  stat_smooth(
    data = data_across_subject3,
    aes(
      x = setsize,
      y = threshold_mean,
      group = gabor_type,
      color = gabor_type
    ),
    method = "lm",
    size = 3,
    se = FALSE,
    alpha = 0.5,
    geom = "line"
  )+
    
  
  geom_errorbar(
    data = data_across_subject3,
    aes(
      x = setsize,
      y = threshold_mean,
      ymin = threshold_mean - threshold_SEM,
      ymax = threshold_mean + threshold_SEM,
      group = gabor_type,
      color = gabor_type
    ),
    size  = 0.8,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  
  labs(y = "Threshold (°)", x = "Set size") +
  
  
  scale_color_manual(
    labels = c("ladder", "snake"),
    values = c("#BB5566", "#674EA7"),
    name = "gabor type"
  ) +

  
  scale_y_continuous(limits = c(1, 3.5)) +
  
  scale_x_continuous(breaks = c(1, 2, 3, 5), 
                     labels = c("1", "2", "3", "5"), limits = c(0.5, 5.5))+
  
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, 'cm'),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) 

if (check_exp4_innermost_color) {
  my_plot4 <- my_plot4 + facet_wrap(~innermost_color)
}

print(my_plot4)

# ggsave(file = "test.svg", plot = my_plot4, width = 4.5, height = 4, units = "in")


# check threshold by participant
my_plot4.0 <-  ggplot() +
  
  geom_point(
    data = data_by_subject3,
    aes(
      x = setsize,
      y = threshold_mean,
      group = gabor_type,
      color = gabor_type,
      size = 0.5
    ),
    alpha = 0.85,
    position = position_dodge(0.5)
  ) +

  stat_smooth(
    data = data_by_subject3,
    aes(
      x = setsize,
      y = threshold_mean,
      group = gabor_type,
      color = gabor_type
    ),
    method = "lm",
    size = 3,
    se = FALSE,
    alpha = 0.5,
    geom = "line"
  )+

  
  labs(y = "Threshold (°)", x = "Set size") +
  
  
  scale_color_manual(
    labels = c("ladder", "snake"),
    values = c("#BB5566", "#674EA7"),
    name = "gabor type"
  ) +
  
  
  # scale_y_continuous(limits = c(1, 3.5)) +
  
  scale_x_continuous(breaks = c(1, 2, 3, 5), 
                     labels = c("1", "2", "3", "5"), limits = c(0.5, 5.5))+
  
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, 'cm'),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  
  facet_wrap(~ participant)

print(my_plot4.0)


# --------------------Exp3 --gabor same or not judgment-----------------------

# check with all data

# gabor_2tasks_exp3_alldata.csv； gabor_colored_exp4_alldata.csv
alldata_exp3 <- read_csv(file.choose())
alldata_exp4 <- read_csv(file.choose())

exp <- "exp4"
check_intensity_group <- FALSE

if (exp == "exp3") {
  data_excl1 <- alldata_exp3
} else if (exp == "exp4") {
  data_excl1 <- alldata_exp4
}

# exclude set size 1, no discrimination task
data_excl1 <- data_excl1[data_excl1$ans_same%in% c("w", 'x'),]

# group intensity
data_excl1$intensity_group <- ceiling(data_excl1$intensity)


grouping_vars2 <-c("setsize",
                   "gabor_type")

grouping_vars_by_sub2 <- c("setsize",
                           "gabor_type",
                           "participant")

if (check_intensity_group) {
  grouping_vars2 <- c(grouping_vars2,"intensity_group")
  
  grouping_vars_by_sub2 <- c(grouping_vars_by_sub2, "intensity_group")
  }

# calculate total responses n
response_counts <- data_excl1 %>% 
  group_by_at(grouping_vars_by_sub2) %>% 
  summarise(
    totoal_res = n()
    )

# calculate no responses n
no_counts <- data_excl1 %>%
  filter(ans_same == "x") %>%
  group_by_at(grouping_vars_by_sub2) %>%
  summarise(no_responses = n()
            )

# merge yes response df with total response df
res <-
  left_join(response_counts,
            no_counts,
            by = grouping_vars_by_sub2)

# replace NA with 0
res$no_responses[is.na(res$no_responses)] <-0

# calculate the percentage of yes responses

res <- res %>% 
  mutate(percentage_no = (no_responses / totoal_res) * 100)

# visualization 
if (check_intensity_group){
  bxp <- ggboxplot(data = res,
                   x = "intensity_group",
                   y = "percentage_no",
                   color ="gabor_type",
                   facet.by = "setsize")
} else{
  bxp <- ggboxplot(data = res,
                   x = "setsize",
                   y = "percentage_no",
                   color ="gabor_type")
}

print(bxp)


res_across_pp <- res %>% 
  group_by_at(grouping_vars2) %>% 
  summarise(percent_no_mean = mean(percentage_no),
            percent_no_sd = sd(percentage_no),
            n = n()) %>% 
  mutate(
    perent_no_SEM = percent_no_sd / sqrt (n),
    percent_no_CI = perent_no_SEM * qt((1-0.05)/2 +.5, n -1)
  )


res_across_pp$setsize <- as.numeric(as.character(res_across_pp$setsize))



# plot percentage no
plt_percent_no <- ggplot() +
  geom_point(
    data = res_across_pp,
    aes(
      x = setsize,
      y = percent_no_mean,
      group = gabor_type,
      color = gabor_type,
      size = 0.5
    ),
    position = position_dodge(0.8),
    stat = "identity",
    alpha = 0.8) +
  
  geom_errorbar(
    data = res_across_pp,
    aes(
      x = setsize,
      y = percent_no_mean,
      ymin = percent_no_mean - perent_no_SEM,
      ymax = percent_no_mean + perent_no_SEM,
      group = gabor_type,
      color = gabor_type
    ),
    
    size  = 0.8,
    width = .00,
    alpha = 0.2,
    position = position_dodge(0.8)
  ) +
  
  stat_smooth(
    data = res_across_pp,
    aes(
      x = setsize,
      y = percent_no_mean,
      group = gabor_type,
      color = gabor_type
    ),
    method = "lm",
    size = 3,
    se = FALSE,
    alpha = 0.5,
    geom = "line"
  )+
  
  # geom_point(
  #   data = res,
  #   aes(
  #     x = setsize,
  #     y = percentage_no,
  #     group = s_l,
  #     color = s_l,
  #     size = 0.5
  #   ),
  #   position = position_dodge(0.8),
#   stat = "identity",
#   alpha = 0.1) +

labs(y = "Percentage of 'No' responses", x = "Set size") +
  
  scale_y_continuous(limits = c(0, 80)) +
  
  scale_x_continuous(breaks = c(2, 3, 5),
                     labels = c("2", "3", "5"), limits = c(1.5, 5.5))+
  
  
  scale_color_manual(
    labels = c("ladder", "snake"),
    values = c("#BB5566", "#674EA7"),
    name = "gabor type"
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.0, "lines")
  )

plt_percent_no

if (check_intensity_group){
  plt_percent_no <- plt_percent_no +  facet_wrap(~intensity_group,
                                                 labeller = labeller(
                                                   intensity_group = 
                                                     c("1" = "ori <= 1",
                                                       "2" = "1< ori <= 2",
                                                       "3" = "2< ori <= 3",
                                                       "4" = "3< ori <= 4",
                                                       "5" = "4< ori <= 5",
                                                       "6" = "5< ori <= 6",
                                                       "7" = "6< ori <= 7",
                                                       "8" = "7< ori <= 8",
                                                       "9" = "8< ori <= 9",
                                                       "10" = "9< ori <= 10")
                                                 ))
}
  
print(plt_percent_no)

# ggsave(file = "test.svg", plot = plt_percent_no, width = 4.5, height = 4, units = "in")

# --------------------Exp5 --adjust gabor-----------------------

# gabor_adjst_ori_alldata2.csv
alldata_exp5 <- read_csv(file.choose())


# get data for heatmap - dev
data_across_subject5 <- alldata_exp5 %>%
  group_by(label, abs_ori) %>%
  summarise(
    dev_inner_mean = mean(dev_innermost),
    dev_midd_mean = mean(dev_midd),
    dev_outer_mean = mean(dev_outermost))


data_across_subject5_long <-
  melt(
    data_across_subject5,
    id.vars = c("label", "abs_ori"),
    variable.name = "dev"
  )

data_across_subject5_long[, 'value'] = round(data_across_subject5_long[, 'value'], 2)


# get data for heatmap - resp

data_across_subject5.0 <- alldata_exp5 %>%
  group_by(label, ori) %>%
  summarise(
    inner_mean = mean(inner_resp),
    midd_mean = mean(midd_resp),
    outer_mean = mean(outer_resp))


data_across_subject5.0_long <-
  melt(
    data_across_subject5.0,
    id.vars = c("label", "ori"),
    variable.name = "resp"
  )

data_across_subject5.0_long[, 'value'] = round(data_across_subject5.0_long[, 'value'], 2)


# heatmap for dev
heatmap_dev <- ggplot(data_across_subject5_long, aes(dev, abs_ori)) +
  
  geom_tile(aes(fill = value), color = "grey") +
  
  scale_fill_gradient(low = "white", high = "steelblue") +
  
  geom_text(aes(label = value)) +
  
  scale_y_continuous(name = "Orientation(°)",
                     breaks = c(2, 4, 10),
                     labels = c("2", "4", "10"), limits = c(0, 12))+
  
  scale_x_discrete(name = "Gabor position",
                   labels=c("dev_inner_mean" = "innermost",
                            "dev_midd_mean" = "middle",
                            "dev_outer_mean" = "outermost"))+
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.0, "lines")
  ) +
  
  facet_wrap(~ label)
  
heatmap_dev


# heatmap for dev
heatmap_resp <- ggplot(data_across_subject5.0_long, aes(resp, ori)) +
  
  geom_tile(aes(fill = value), color = "grey") +
  
  scale_fill_gradient(low = "yellow", high = "red") +
  
  geom_text(aes(label = value)) +
  
  scale_y_continuous(name = "Orientation(°)",
                     breaks = c(-10, -4, -2, 2, 4, 10),
                     labels = c("-10", "-4", "-2", "2", "4", "10"), limits = c(-12, 12))+
  
  scale_x_discrete(name = "Gabor position",
                   labels=c("inner_mean" = "innermost",
                            "midd_mean" = "middle",
                            "outer_mean" = "outermost"))+
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.0, "lines")
  ) +
  
  facet_wrap(~ label)

heatmap_resp

# heatmap by participant

data_by_subject5 <- alldata_exp5 %>%
  group_by(label, abs_ori, participant) %>%
  summarise(
    dev_inner_mean = mean(dev_innermost),
    dev_midd_mean = mean(dev_midd),
    dev_outer_mean = mean(dev_outermost))


data_by_subject5_long <-
  melt(
    data_by_subject5,
    id.vars = c("label", "abs_ori", "participant"),
    variable.name = "dev"
  )

# TODO
pp <- 20

data_single_p <- subset(data_by_subject5_long, participant == pp)


heatmap_by_pp <- ggplot(data_single_p, aes(dev, abs_ori)) +
  
  geom_tile(aes(fill = value), color = "grey") +
  
  scale_fill_gradient(low = "white", high = "steelblue") +
  
  geom_text(aes(label = value)) +
  
  scale_y_continuous(name = "",
                     breaks = c(2, 4, 10),
                     labels = c("2", "4", "10"), limits = c(0, 12))+
  
  scale_x_discrete(name = "",
                   labels=c("dev_inner_mean" = "innermost",
                            "dev_midd_mean" = "middle",
                            "dev_outer_mean" = "outermost"))+
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.0, "lines")
  ) +
  
  facet_wrap(~ label)

heatmap_by_pp

# check resp type

check_data <- alldata_exp5 %>% 
  filter(grepl("^setsize3", label))

check_data_across_pp <- check_data %>% 
  group_by(label, resp_type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(label) %>%
  mutate(percentage = (count / sum(count)) * 100)


check_data_by_pp <- check_data %>% 
  group_by(label, resp_type, participant) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(label, participant) %>%
  mutate(percentage = (count / sum(count)) * 100)


plot_check_data_across_pp <- ggplot() +
  geom_bar(data = check_data_across_pp, aes(x = label,
                                  y = percentage,
                                  fill = resp_type),
           stat = "identity", alpha = 0.8, width = 0.2) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.0, "lines")
  )

plot_check_data_across_pp


plot_check_data_by_pp <- ggplot() +
  geom_bar(data = check_data_by_pp, aes(x = label,
                                            y = percentage,
                                            fill = resp_type),
           stat = "identity", alpha = 0.8, width = 0.2) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.0, "lines")
  ) +
  
  facet_wrap(~participant)

plot_check_data_by_pp


# non-uniformity
data5 <- alldata_exp5 %>% 
  filter(grepl("^setsize3", label))

data_by_subject5.1 <- data5 %>% 
  group_by(resp_type, label, participant) %>% 
  
  summarise(resp_variance = mean(resp_variance),
            resp_variance_sd = sd(resp_variance),
            n_reversal = mean(n_reversal),
            n_reversal_sd = sd(n_reversal),
            n = n()) %>% 
  mutate(
    resp_variance_SEM = resp_variance_sd / sqrt (n),
    resp_variance_CI = resp_variance_SEM * qt((1-0.05)/2 +.5, n -1),
    n_reversal_SEM = n_reversal_sd / sqrt (n),
    n_reversal_CI = n_reversal_SEM * qt((1-0.05)/2 +.5, n -1)
  )

data_across_subject5.1 <- data5 %>% 
  group_by(resp_type, label) %>% 
  
  summarise(resp_variance = mean(resp_variance),
            resp_variance_sd = sd(resp_variance),
            n_reversal = mean(n_reversal),
            n_reversal_sd = sd(n_reversal),
            n = n()) %>% 
  mutate(
    resp_variance_SEM = resp_variance_sd / sqrt (n),
    resp_variance_CI = resp_variance_SEM * qt((1-0.05)/2 +.5, n -1)
  )


my_plot5 <- ggplot() +
  geom_point(
    data = data_across_subject5.1,
    aes(
      x = label,
      y = n_reversal,
      size = resp_type,
      group = resp_type,
      color = resp_type
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.6
  ) +
  
  geom_point(
    data = data_by_subject5.1,
    aes(
      x = label,
      y = n_reversal,
      size = resp_type,
      color = resp_type
    ),
    alpha = 0.05,
    position = position_dodge(0.5)
  ) + 
  
  labs(x = "Gabor type", y = "Number of reversal") +
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        
        panel.border = element_blank(),  
        # remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove panel background
        panel.background = element_blank(),
        panel.spacing = unit(0.4, "cm"),
        # add axis line
        axis.line = element_line(colour = "grey"),
        # x,y axis tick labels
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        # legend size
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        # facet wrap title
        strip.text.x = element_text(size = 12, face = "bold"))
  

  
my_plot5


my_plot5.1 <- ggplot() +
  geom_point(
    data = data_across_subject5.1,
    aes(
      x = label,
      y = resp_variance,
      size = resp_type,
      group = resp_type,
      color = resp_type
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.6
  ) +
  
  geom_point(
    data = data_by_subject5.1,
    aes(
      x = label,
      y = resp_variance,
      size = resp_type,
      color = resp_type
    ),
    alpha = 0.05,
    position = position_dodge(0.5)
  ) + 
  
  labs(x = "Gabor type", y = "Response vairance") +
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        
        panel.border = element_blank(),  
        # remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove panel background
        panel.background = element_blank(),
        panel.spacing = unit(0.4, "cm"),
        # add axis line
        axis.line = element_line(colour = "grey"),
        # x,y axis tick labels
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        # legend size
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        # facet wrap title
        strip.text.x = element_text(size = 12, face = "bold"))



my_plot5.1
