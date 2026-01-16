# analysis and plots code for multigabor exp3
# two tasks:uniformaty judgement and innermost gabor orientation task

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(svglite)
library(ggpubr)
library(reshape2)


# set working path
getwd()
setwd("d:/OneDrive/projects/multi_gabor_discr/src/analysis/")

# read data
data_exp3_threshold <- readxl::read_excel("../../data/gbr_sc_threshold3.xlsx")
alldata_exp3 <- read_csv("../../data/gabor_2tasks_exp3_alldata.csv")

# ==========Plot================

# plot orientation task
grouping_vars <- c(
  "setsize",
  "gabor_type",
  "gabor_type2"
)

grouping_vars_by_sub <- c(
  "setsize",
  "gabor_type",
  "gabor_type2",
  "participant"
)

data_preprocessed <- data_exp3_threshold
colnames(data_preprocessed)


data_by_subject <- data_preprocessed %>%
  group_by_at(grouping_vars_by_sub) %>%
  summarise(
    intensity_mean = mean(intensity),
    intensity_std = sd(intensity),
    n = n() 
  ) %>%
  mutate(
    threshold_SEM = intensity_std / sqrt(n),
    threshold_CI = threshold_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_subject <- data_by_subject %>%
  group_by_at(grouping_vars) %>%
  summarise(
    threshold_mean = mean(intensity_mean),
    threshold_std = sd(intensity_mean),
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
  #   data = data_by_subject,
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
    data = data_across_subject,
    aes(
      x = setsize,
      y = threshold_mean,
      group = gabor_type,
      color = gabor_type
    ),
    method = "lm",
    size = 1.5,
    se = FALSE,
    alpha = 0.5,
    geom = "line"
  )+
  
  
  geom_errorbar(
    data = data_across_subject,
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
    values = c("#F28522", "#674EA7"), 
    name = "gabor type"
  ) +
  
  
  scale_y_continuous(limits = c(1, 5)) +
  
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


my_plot

# ggsave(file = "exp3.svg", plot = my_plot,  width = 5, height = 4.6, units = "in")

# plot uniformity judgment

# exclude set size 1, no discrimination task
data_exp3_uniformity <- alldata_exp3


# 1) Keep uniformity task rows
data_exp3_uniformity <- data_exp3_uniformity %>%
  filter(gabor_type2 %in% c("snake", "ladder")) %>%      # removes setsize 1 conditions in your file
  mutate(gabor_type2 = droplevels(as.factor(gabor_type2))) %>%
  filter(ans_same %in% c("w", "x")) %>%
  mutate(setsize = as.numeric(setsize))

# 2) Create "correct" 
#    correct[i] depends on intensity[i] vs intensity[i+1]

data_exp3_uniformity$correct <- NA_integer_

for (i in 1:(nrow(data_exp3_uniformity) - 1)) {
  if (data_exp3_uniformity$intensity[i + 1] >= data_exp3_uniformity$intensity[i]) {
    data_exp3_uniformity$correct[i] <- 0L
  } else {
    data_exp3_uniformity$correct[i] <- 1L
  }
}

# indices of "start"
start_indices <- which(data_exp3_uniformity$direction == "start")

# remove row start-1 (except the first start)
rows_to_remove <- start_indices[start_indices != start_indices[1]] - 1L
data_exp3_uniformity <- data_exp3_uniformity[-rows_to_remove, ]

# remove last row (because correct for last row is undefined)
data_exp3_uniformity <- data_exp3_uniformity[-nrow(data_exp3_uniformity), ]

# ensure correct is factor 
data_exp3_uniformity <- data_exp3_uniformity %>%
  mutate(correct = factor(correct, levels = c(0, 1)))


# 3) Empirical %No per participant
res_by_pp <- data_exp3_uniformity %>%
  group_by(participant, setsize, gabor_type, correct) %>%
  summarise(
    total_n = n(),
    no_n = sum(ans_same == "x"),
    perc_no = 100 * no_n / total_n,
    .groups = "drop"
  )

res_across_pp <- res_by_pp %>%
  group_by(setsize, gabor_type, correct) %>%
  summarise(
    perc_no_mean = mean(perc_no, na.rm = TRUE),
    perc_no_sd   = sd(perc_no, na.rm = TRUE),
    n_pp         = n(),
    .groups = "drop"
  ) %>%
  mutate(perc_no_sem = perc_no_sd / sqrt(n_pp))


# 4) Plot: separate panels for orientation task's correct vs incorrect

# plot percentage no
plt_percent_no <- ggplot() +
  geom_point(
    data = res_across_pp,
    aes(
      x = setsize,
      y = perc_no_mean,
      group = gabor_type,
      color = gabor_type,
      size = 0.5,
    ),
    position = position_dodge(0.8),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE) +
  
  geom_errorbar(
    data = res_across_pp,
    aes(
      x = setsize,
      y = perc_no_mean,
      ymin = perc_no_mean - perc_no_sem,
      ymax = perc_no_mean + perc_no_sem,
      group = gabor_type,
      color = gabor_type
    ),
    
    size  = 0.8,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.8)
  ) +
  
  stat_smooth(
    data = res_across_pp,
    aes(
      x = setsize,
      y = perc_no_mean,
      group = gabor_type,
      color = gabor_type
    ),
    method = "lm",
    size = 1.5,
    se = FALSE,
    alpha = 0.5,
    geom = "line"
  )+

  
  labs(y = "Percentage of 'No' responses", x = "Set size") +
  
  scale_y_continuous(limits = c(0, 80)) +
  
  scale_x_continuous(breaks = c(2, 3, 5),
                     labels = c("2", "3", "5"), limits = c(1.5, 5.5))+
  
  
  scale_color_manual(
    labels = c("ladder", "snake"),
    values = c("#F28522", "#674EA7"), 
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
  ) +
  facet_wrap(~ correct, nrow = 1, labeller = labeller(
    correct = c("0" = "Orientation: Incorrect", "1" = "Orientation: Correct")
  ))

plt_percent_no

# ggsave("exp3_uni.svg", plot = plt_percent_no, width = 8, height = 4.6, units = "in")

# ===========model===============


# model orientation task

data_exp3_threshold <- readxl::read_excel("../../data/gbr_sc_threshold3.xlsx")

df_check_participants <- data_exp3_threshold %>% 
  group_by(participant, age, sex) %>% 
  tally()

# mean age == 20.83 years
mean(df_check_participants$age)

# variables
str(data_exp3_threshold)

table(data_exp3_threshold$setsize, data_exp3_threshold$gabor_type)

data_exp3_threshold$setsize <- as.numeric(data_exp3_threshold$setsize)

data_exp3_threshold$gabor_type <-as.factor(data_exp3_threshold$gabor_type)

# contrast coding
# contrasts(data_exp3_threshold$gabor_type) <- matrix(c(-0.5, 0.5), ncol = 1)

levels(data_exp3_threshold$gabor_type)

# check mean
tapply(data_exp3_threshold$intensity, data_exp3_threshold$gabor_type, mean)

model <- lme4::lmer(intensity ~ setsize * gabor_type + (1|participant), data = data_exp3_threshold) # selected model
model2 <- lme4::lmer(intensity ~  setsize + gabor_type + (1|participant), data = data_exp3_threshold)

table(data_exp3_threshold$setsize)

anova(model, model2)

summary(model)

sjPlot::tab_model(
  model,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3
) 

emms3 <- emmeans::emmeans(
  model,
  list(pairwise ~ gabor_type),
  adjust = "tukey"
)

summary(emms3, infer = TRUE)


# # model predictions
# predictions <- merTools::predictInterval(
#   model,
#   newdata = data_exp3_threshold,
#   level = 0.95,  
#   n.sims = 1000, 
#   which = "full" # fixed effects
# )
# 
# data_exp3_threshold <- cbind(data_exp3_threshold, predictions)
# 
# 
# data_by_subject <- data_exp3_threshold %>%
#   group_by(participant,
#            setsize,
#            gabor_type) %>%
#   summarise(
#     trials.intensity.pre.mean = mean(fit),
#     ci_low = mean(lwr),
#     ci_high = mean(upr),
#     n = n()
#   )
# 
# 
# data_across_subject <- data_by_subject %>%
#   group_by(setsize,
#            gabor_type) %>%
#   summarise(
#     threshold.pre.mean = mean(trials.intensity.pre.mean),
#     threshold_pre.std = sd(trials.intensity.pre.mean),
#     n = n(),
#     ci_l = mean(ci_low),
#     ci_h = mean(ci_high)
#   ) %>%
#   mutate(
#     threshold_SEM = threshold_pre.std / sqrt(n),
#     threshold_CI = threshold_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
#   )
# 
# 
# 
# 
# my_plot3 <-  ggplot() +
#   
#   geom_point(
#     data = data_across_subject,
#     aes(
#       x = as.numeric(setsize),
#       y = threshold.pre.mean,
#       group = gabor_type,
#       color = gabor_type,
#       size = 0.5
#     ),
#     
#     position = position_dodge(0.5),
#     stat = "identity",
#     alpha = 0.6
#   ) +
#   
#   # geom_point(
#   #   data = data_exc_ss1,
#   #   aes(
#   #     x = as.numeric(trials.setsize),
#   #     y = fit,
#   #     group = gabor_type,
#   #     color = gabor_type,
#   #     size = 0.4
#   #   ),
#   #   alpha = 0.1,
#   #   position = position_dodge(0.5)
#   # ) +
#   
#   geom_line(
#     data = data_across_subject,
#     aes(
#       x = as.numeric(setsize),
#       y = threshold.pre.mean,
#       group = gabor_type,
#       color = gabor_type,
#       size = 0.1)
#   )+
#   
#   # stat_smooth(
#   #   data = data_across_subject,
#   #   aes(
#   #     x = as.numeric(trials.setsize),
#   #     y = threshold.pre.mean,
#   #     group = gabor_type,
#   #     color = gabor_type
#   #   ),
#   #   method = "lm",
#   #   size = 1.5,
#   #   se = FALSE,
#   #   alpha = 0.5,
#   #   geom = "line",
#   #   show.legend = FALSE
#   # )+
#   
#   geom_errorbar(
#     data = data_across_subject,
#     aes(
#       x = as.numeric(setsize),
#       y = threshold.pre.mean,
#       ymin = threshold.pre.mean - threshold_CI,
#       ymax = threshold.pre.mean + threshold_CI,
#       group = gabor_type,
#       color = gabor_type
#     ),
#     size  = 0.8,
#     width = .00,
#     alpha = 0.8,
#     position = position_dodge(0.5)
#   ) +
#   
#   labs(y = "Threshold predicted by LMM", x = "Set size") +
#   
#   
#   scale_color_manual(
#     labels = c("Ladder", "Snake"),
#     values = c("#F28522", "#674EA7"), #DDAA33
#     name = "gabor type"
#   ) +
#   
#   
#   scale_y_continuous(limits = c(1, 5)) +
#   
#   scale_x_continuous(breaks = c(1, 2, 3, 5),
#                      labels = c("1", "2", "3", "5"), limits = c(0.5, 5.5))+
#   
#   theme(
#     axis.title.x = element_text(color = "black", size = 14, face = "bold"),
#     axis.title.y = element_text(color = "black", size = 14, face = "bold"),
#     panel.border = element_blank(),
#     # remove panel grid lines
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     # remove panel background
#     panel.background = element_blank(),
#     # add axis line
#     axis.line = element_line(colour = "grey"),
#     # x,y axis tick labels
#     axis.text.x = element_text(size = 12, face = "bold"),
#     axis.text.y = element_text(size = 12, face = "bold"),
#     # legend size
#     legend.title = element_text(size = 12, face = "bold"),
#     legend.text = element_text(size = 10),
#     # facet wrap title
#     strip.text.x = element_text(size = 12, face = "bold")
#   ) 
# 
# 
# my_plot3



# get slopes, test against 0
vcov_matrix <- vcov(model)
fixed_effects <- lme4::fixef(model)

slope_RL <- fixed_effects["setsize"]
se_RL <- sqrt(vcov_matrix["setsize", "setsize"])
t_RL <- slope_RL / se_RL
p_RL <- 2 * (1 - pnorm(abs(t_RL)))



slope_RS <- fixed_effects["setsize"] + fixed_effects["setsize:gabor_typesnake"]

se_RS <- sqrt(vcov_matrix["setsize", "setsize"] +
                vcov_matrix["setsize:gabor_typesnake",
                            "setsize:gabor_typesnake"] +
                2 * vcov_matrix["setsize", "setsize:gabor_typesnake"])
t_RS <- slope_RS / se_RS
p_RS <- 2 * (1 - pnorm(abs(t_RS)))


# adjust p

p_vals <- c(p_RS, p_RL)
p_values_corrected <- p.adjust(p_vals, method = "holm")


results <- data.frame(
  Condition = c("RS", "RL"),
  Slope = c(slope_RS, slope_RL),
  SE = c(se_RS, se_RL),
  t_value = c(t_RS, t_RL),
  p_value = c(p_RS, p_RL),
  adj_p = p_values_corrected
)
results

# model uniformaty judgment task

# for all data, check percentage of correct/incorrect
# orientation judgment per condition 


data_exp3_check <- data_exp3_uniformity %>%
  group_by(gabor_type, correct) %>%
  summarise(
    n = n(),
    .groups = "drop" 
  ) %>%
  group_by(gabor_type) %>%
  mutate(
    percentage = (n / sum(n)) * 100
  )

# GLMM
data_exp3_uniformity <- data_exp3_uniformity %>%
  mutate(resp_binary = ifelse(ans_same == "w", 1, 
                              ifelse(ans_same == "x", 0, NA)))

# factors amd continues
data_exp3_uniformity$setsize <- as.numeric(data_exp3_uniformity$setsize)
data_exp3_uniformity$correct <- as.factor(data_exp3_uniformity$correct)
# data_exp3_uniformity$setsize <- as.factor(data_exp3_uniformity$setsize)


levels(data_exp3_uniformity$correct)
# reverse coding
data_exp3_uniformity$resp_no <- ifelse(data_exp3_uniformity$resp_binary == 0, 1, 0)

str(data_exp3_uniformity)

model_glmm <- lme4::glmer(resp_no ~ setsize * gabor_type2 * correct + (1 | participant),
                          data = data_exp3_uniformity, family = binomial) # selected model

model_glmm2 <- lme4::glmer(resp_no ~ setsize * gabor_type2 + (1 | participant),
                           data = data_exp3_uniformity, family = binomial)
anova(model_glmm, model_glmm2)

summary(model_glmm)

sjPlot::tab_model(
  model_glmm,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3
) 

# model predictions
# predictions <- ggeffects::ggpredict(
#   model_glmm, 
#   terms = c("setsize", "gabor_type2", "correct")  # Specify predictors
# )
# 
# # Plot predicted probabilities
# my_plot_exp3_uniformaty <- ggplot()+
#   geom_line(data = predictions,
#             aes(
#               x = x, 
#               y = predicted, 
#               color = group),
#             size = 1.2,
#             alpha = 0.5
#   )+  
#   geom_ribbon(data = predictions,
#               aes
#               (x = x,
#                 ymin = conf.low, 
#                 ymax = conf.high, 
#                 fill = group), 
#               alpha = 0.1) +  
#   
#   scale_color_manual(
#     labels = c("Ladder", "Snake"),
#     values = c("#F28522", "#674EA7"), #DDAA33
#     name = "gabor type") +
#   
#   scale_fill_manual(
#     labels = c("Ladder", "Snake"),
#     values = c("#F28522", "#674EA7"), #DDAA33
#     name = "gabor type"
#     
#   ) +
#   
#   scale_y_continuous(limits = c(0, 0.8)) +
#   
#   labs(
#     x = "Set Size",
#     y = "Predicted Probability",
#     color = "Gabor Type",
#     fill = "Gabor Type"
#   ) +
#   theme(
#     axis.title.x = element_text(
#       color = "black",
#       size = 14,
#       face = "bold"
#     ),
#     axis.title.y = element_text(
#       color = "black",
#       size = 14,
#       face = "bold"
#     ),
#     panel.border = element_blank(),
#     # remove panel grid lines
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     # remove panel background
#     panel.background = element_blank(),
#     # add axis line
#     axis.line = element_line(colour = "grey"),
#     # x,y axis tick labels
#     axis.text.x = element_text(size = 12, face = "bold"),
#     axis.text.y = element_text(size = 12, face = "bold"),
#     # legend size
#     legend.title = element_text(size = 12, face = "bold"),
#     legend.text = element_text(size = 10),
#     # facet wrap title
#     strip.text.x = element_text(size = 12, face = "bold"),
#     panel.spacing = unit(1.0, "lines")
#   ) +
#   
#   facet_wrap(~facet, nrow = 1, labeller = labeller(
#     facet =
#       c("0" = "Wrong Judgment",
#         "1" = "Correct Judgment"
#       )
#   ))  
# 
# my_plot_exp3_uniformaty


# r2 <- MuMIn::r.squaredGLMM(model_glmm)
# r2

# slopes

fixed_effects <- lme4::fixef(model_glmm)

beta_setsize <- fixed_effects["setsize"]
beta_setsize_gabor_snake <- fixed_effects["setsize:gabor_type2snake"]
beta_setsize_correct <- fixed_effects["setsize:correct1"]
beta_setsize_gabor_correct <- fixed_effects["setsize:gabor_type2snake:correct1"]


slope_ladder_correct0 <- beta_setsize
slope_ladder_correct1 <- beta_setsize + beta_setsize_correct
slope_snake_correct0 <- beta_setsize + beta_setsize_gabor_snake
slope_snake_correct1 <- beta_setsize + beta_setsize_gabor_snake + beta_setsize_correct + beta_setsize_gabor_correct


# stats test

vcov_matrix <- vcov(model_glmm)

# se
se_ladder_correct0 <- sqrt(vcov_matrix["setsize", "setsize"])
se_ladder_correct1 <- sqrt(vcov_matrix["setsize", "setsize"] +
                             vcov_matrix["setsize:correct1", "setsize:correct1"] +
                             2 * vcov_matrix["setsize", "setsize:correct1"])

se_snake_correct0 <- sqrt(vcov_matrix["setsize", "setsize"] +
                            vcov_matrix["setsize:gabor_type2snake", "setsize:gabor_type2snake"] +
                            2 * vcov_matrix["setsize", "setsize:gabor_type2snake"])

se_snake_correct1 <- sqrt(vcov_matrix["setsize", "setsize"] +
                            vcov_matrix["setsize:gabor_type2snake", "setsize:gabor_type2snake"] +
                            vcov_matrix["setsize:correct1", "setsize:correct1"] +
                            vcov_matrix["setsize:gabor_type2snake:correct1", "setsize:gabor_type2snake:correct1"] +
                            2 * vcov_matrix["setsize", "setsize:gabor_type2snake"] +
                            2 * vcov_matrix["setsize", "setsize:correct1"] +
                            2 * vcov_matrix["setsize:gabor_type2snake", "setsize:correct1"] +
                            2 * vcov_matrix["setsize", "setsize:gabor_type2snake:correct1"])

# ts
t_ladder_c0 <- slope_ladder_correct0 / se_ladder_correct0
t_ladder_c1 <- slope_ladder_correct1 / se_ladder_correct1
t_snake_c0 <- slope_snake_correct0 / se_snake_correct0
t_snake_c1 <- slope_snake_correct1 / se_snake_correct1

# ps
p_ladder_c0 <- 2 * (1 - pnorm(abs(t_ladder_c0)))
p_ladder_c1 <- 2 * (1 - pnorm(abs(t_ladder_c1)))
p_snake_c0 <- 2 * (1 - pnorm(abs(t_snake_c0)))
p_snake_c1 <- 2 * (1 - pnorm(abs(t_snake_c1)))


p_vals <- c(p_ladder_c0, p_ladder_c1,p_snake_c0, p_snake_c1)
p_values_corrected <- p.adjust(p_vals, method = "holm")

# results to df
results <- data.frame(
  Condition = c("ladder_0", "ladder_1", "snake_0", "snake_1"),
  Slope = c(slope_ladder_correct0, slope_ladder_correct1, slope_snake_correct0, slope_snake_correct1),
  SE = c(se_ladder_correct0, se_ladder_correct1, se_snake_correct0, se_snake_correct1),
  t_value = c(t_ladder_c0, t_ladder_c1, t_snake_c0, t_snake_c1),
  p_value = c(p_ladder_c0, p_ladder_c1, p_snake_c0, p_snake_c1),
  adj_p = p_values_corrected
)
results


# pairwise comparisons
emms <- emmeans::emmeans(
  model_glmm,
  list(pairwise ~ gabor_type2 | correct),
  adjust = "tukey"
)

summary(emms)

