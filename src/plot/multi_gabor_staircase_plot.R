# libraires ---------------------------------------------------------------
if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(ggpubr)){
  install.packages("ggpubr")
  library(ggpubr)
}

if(!require(rstatix)){
  install.packages("rstatix")
  library(rstatix)
}

if(!require(emmeans)){
  install.packages("emmeans")
  library(emmeans)
}

if(!require(sjstats)){
  install.packages("sjstats")
  library(sjstats)
}

if(!require(sjstats)){
  install.packages("sjstats")
  library(sjstats)
}

if(!require(lme4)){
  install.packages("lme4")
  library(lme4)
}

if(!require(lmerTest)){
  install.packages("lmerTest")
  library(lmerTest)
}

if(!require(MuMIn)){
  install.packages("MuMIn")
  library(MuMIn)
}

if(!require(multcomp)){
  install.packages("multcomp")
  library(multcomp)
}

if(!require(nlme)){
  install.packages("nlme")
  library(nlme)
}

if(!require(r2glmm)){
  install.packages("r2glmm")
  library(r2glmm)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(ggthemes)){
  install.packages("ggthemes")
  library(ggthemes)
}

if(!require(svglite)){
  install.packages("svglite")
  library(svglite)
}

if(!require(sjPlot)){
  install.packages("sjPlot")
  library(sjPlot)
}

if(!require(ggpubr)){
  install.packages("ggpubr")
  library(ggpubr)
}



# set working path
getwd()

setwd("C:/SCALab/projects/multi_gabor_discr/data/")

# read data
data_preprocessed <- read_excel("preprocessed_multi_gabor_staircase.xlsx")

# set size 1
setsize1 <- subset(data_preprocessed, trials.setsize == 1)

# other setsize

data_preprocessed_exc_1 <- subset(data_preprocessed, trials.setsize != "setsize1")


# all condition
data_by_subject <- data_preprocessed %>%
  group_by(participant,
           trials.setsize,
           r_t,
           s_l) %>%
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
           s_l) %>%
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
    values = c("#674EA7", "#F1C232"),
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

ggsave(file = "test.svg", plot = my_plot, width = 14.7, height = 6.27, units = "in")
