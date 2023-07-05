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

# --------------------Exp1 & Exp2 -----------------------------------------
# read data

# prprcssed_mlti_gbr_sc_1.xlsx and prprcssed_mlti_gbr_sc_2.xlsx

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
           r_t,
           s_l,
           condition) %>%
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
           r_t,
           s_l,
           condition) %>%
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
      group = r_t,
      color = r_t,
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
      group = r_t,
      color = r_t,
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
      group = r_t
    ),
    color = "black",
    size  = 0.8,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  
  labs(y = "Threshold", x = "Set size") +
  
  
  scale_color_manual(
    labels = c("radial", "tangential"),
    values = c("#BB5566", "#004488"),
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
  
  
  facet_wrap(~ s_l, nrow = 1)


print(my_plot)


my_plot2 <-  ggplot() +
  
  geom_point(
    data = data_across_subject,
    aes(
      x = trials.setsize,
      y = threshold_mean,
      group = s_l,
      color = s_l,
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
      group = s_l,
      color = s_l,
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
      group = s_l
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
  
  
  facet_wrap(~ r_t, nrow = 1)


print(my_plot2)

# ggsave(file = "test.svg", plot = my_plot, width = 14.7, height = 6.27, units = "in")


my_plot3 <-  ggplot() +
  
  geom_point(
    data = data_across_subject,
    aes(
      x = trials.setsize,
      y = threshold_mean,
      group = condition,
      color = condition,
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
      group = condition,
      color = condition
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
      group = condition,
      color = condition
    ),
    size  = 0.8,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  
  labs(y = "Threshold (°)", x = "Set size") +
  

  scale_color_manual(
    labels = c("radial_ladder", "radial_snake", "tangential_ladder", "tangential_snake" ),
    values = c("#BB5566", "#674EA7", "#004488", "#F28522"), #DDAA33
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

# --------------------Exp3--gabor ori threshold-------------------------

data_preprocessed3 <- readxl::read_excel(path = file.choose())

colnames(data_preprocessed3)

data_by_subject3 <- data_preprocessed3 %>%
  group_by(participant,
           setsize,
           s_l) %>%
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
  group_by(setsize,
           s_l) %>%
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
      group = s_l,
      color = s_l,
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
      group = s_l,
      color = s_l
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
      group = s_l,
      color = s_l
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

print(my_plot4)

# ggsave(file = "test.svg", plot = my_plot4, width = 4.5, height = 4, units = "in")
# --------------------Exp3--gabor same or not judgment-----------------------

# exclude set size 1, no discrimination task
data_excl1 <- data_preprocessed3[data_preprocessed3$ans_same%in% c("w", 'x'),]


# calculate total responses n
response_counts <- data_excl1 %>% 
  group_by(
    participant,
    setsize,
    s_l
  ) %>% 
  summarise(
    totoal_res = n()
    )

# calculate no responses n
no_counts <- data_excl1 %>%
  filter(ans_same == "x") %>%
  group_by(participant, 
           setsize, 
           s_l) %>%
  summarise(no_responses = n()
            )

# merge yes response df with total response df
res <- left_join(response_counts, no_counts, by = c("participant", "setsize", "s_l"))

# replace NA with 0
res$no_responses[is.na(res$no_responses)] <-0

# calculate the percentage of yes responses

res <- res %>% 
  mutate(percentage_no = (no_responses / totoal_res) * 100)

# visualization 

bxp <- ggboxplot(data = res,
                 x = "setsize",
                 y = "percentage_no",
                 color = "s_l")
print(bxp)


res_across_pp <- res %>% 
  group_by(setsize,
           s_l) %>% 
  summarise(percent_no_mean = mean(percentage_no),
            percent_no_sd = sd(percentage_no),
            n = n()) %>% 
  mutate(
    perent_no_SEM = percent_no_sd / sqrt (n),
    percent_no_CI = perent_no_SEM * qt((1-0.05)/2 +.5, n -1)
  )




plt_percent_no <- ggplot() +
  geom_point(
    data = res_across_pp,
    aes(
      x = setsize,
      y = percent_no_mean,
      group = s_l,
      color = s_l,
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
      group = s_l,
      color = s_l
    ),
    
    size  = 0.8,
    width = .00,
    position = position_dodge(0.8)
  ) +
  
  stat_smooth(
    data = res_across_pp,
    aes(
      x = setsize,
      y = percent_no_mean,
      group = s_l,
      color = s_l
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
  
  scale_y_continuous(limits = c(0, 75)) +
  
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


print(plt_percent_no)  

# ggsave(file = "test.svg", plot = plt_percent_no, width = 4.5, height = 4, units = "in")
