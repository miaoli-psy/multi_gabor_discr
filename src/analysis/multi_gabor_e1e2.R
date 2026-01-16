# analysis and plots code for multigabor exp1 and exp2
# two exps are identical except for
# exp1: no prior info; exp2: prior info
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
data_exp1 <- readxl::read_excel("../../data/gbr_sc_threshold1.xlsx")
data_exp2 <- readxl::read_excel("../../data/gbr_sc_threshold2.xlsx")

# ================Plots=======================

# exp1 or exp2
data_preprocessed <- data_exp2


# arrange data to plot
data_by_subject <- data_preprocessed %>%
  group_by(participant,
           trials.setsize,
           gabor_arrangment,
           gabor_type,
           full_condition,
           full_condition2) %>%
  summarise(
    trials.intensity.mean = mean(trials.intensity),
    trials.intensity.std = sd(trials.intensity),
    n = n() 
  ) %>%
  mutate(
    threshold_SEM = trials.intensity.std / sqrt(n),
    threshold_CI = threshold_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_subject <- data_by_subject %>%
  group_by(trials.setsize,
           gabor_arrangment,
           gabor_type,
           full_condition,
           full_condition2) %>%
  summarise(
    threshold_mean = mean(trials.intensity.mean),
    threshold_std = sd(trials.intensity.mean),
    n = n() 
  ) %>%
  mutate(
    threshold_SEM = threshold_std / sqrt(n),
    threshold_CI = threshold_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

# duplicate setsize 1 condition
s1_dup <- data_across_subject %>%
  filter(gabor_arrangment == "s1") %>%
  crossing(arr = c("radial", "tangential")) %>%  # temp name to avoid collision
  mutate(gabor_arrangment = arr) %>%
  select(-arr)

data_across_subject <- data_across_subject %>%
  filter(gabor_arrangment != "s1") %>%
  bind_rows(s1_dup) %>%
  mutate(gabor_arrangment = factor(gabor_arrangment, levels = c("radial","tangential")))



my_plot <-  ggplot() +
  
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
  
  # geom_point(
  #   data = data_by_subject,
  #   aes(
  #     x = trials.setsize,
  #     y = trials.intensity.mean,
  #     group = gabor_type,
  #     color = gabor_type,
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
      group = gabor_type,
      color = gabor_type
    ),
    size  = 0.8,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  geom_smooth(
    data = data_across_subject,
    aes(x = trials.setsize, 
        y = threshold_mean,
        group = gabor_type,
        color = gabor_type),
    method = "lm", se = FALSE, formula = y ~ x, linewidth = 0.5
  ) +
  
  labs(y = "Threshold (°)", x = "Set size") +
  
  
  scale_color_manual(
    labels = c("ladder", "snake"),
    values = c("#F28522", "#674EA7"), #DDAA33
    name = "gabor type"
  ) +
  
  
  scale_y_continuous(limits = c(1, 5)) +
  
  scale_x_continuous(breaks = c(1, 2, 3, 5, 7), 
                     labels = c("1", "2", "3", "5", "7"), limits = c(0.5, 7.5))+
  
  theme(
    axis.title.x = element_text(color = "black", size = 16, face = "bold"),
    axis.title.y = element_text(color = "black", size = 16, face = "bold"),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    # legend size
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    # facet wrap title
    strip.text.x = element_text(size = 16, face = "bold")
  ) +
  
  
  facet_wrap(~ gabor_arrangment, nrow = 1, labeller = labeller(
    gabor_arrangment = 
      c("radial" = "Radial",
        "tangential" = "Tangential")
  ))

my_plot

# ggsave(file = "test.svg", plot = my_plot, width = 8, height = 4, units = "in")


# ===============Model===================

data <- data_exp2 # or data<- data_exp2

colnames(data)

# 15 females; 5 males; age range 18-25
df_check_participants <- data %>% 
  group_by(participant, age, sex) %>% 
  tally()

# mean age == 20.0 years
mean(df_check_participants$age)

# variables
str(data)

data$full_condition2 <- factor(
  data$full_condition2,
  levels = c(
    "ladder_radial",
    "snake_radial",
    "ladder_tangential",
    "snake_tangential",
    "setsize1_h_s1",
    "setsize1_v_s1"
  )
)

data$participant <- as.factor(data$participant)

# for now, later as continious
data$setsize <- as.factor(data$trials.setsize)

# remove set size 1
data_exc_ss1 <- data %>% 
  filter(full_condition2 %in% c("ladder_radial", "snake_radial","ladder_tangential", "snake_tangential"))

# drop unused factor levels
data_exc_ss1$full_condition2 <- droplevels(data_exc_ss1$full_condition2)
data_exc_ss1$setsize <- droplevels(data_exc_ss1$setsize)

# set size 1
data_ss1 <- data %>% 
  filter(full_condition2 %in% c("setsize1_h_s1", "setsize1_v_s1"))

data_ss1$full_condition2 <- droplevels(data_ss1$full_condition2)

# test single vertical vs. single horizontal
contrasts(data_ss1$full_condition2) <- matrix(c(-0.5, 0.5), ncol = 1)

levels(data_ss1$full_condition2)

model <- lme4::lmer(trials.intensity ~ full_condition2 + (1|participant), data = data_ss1) # selected model
model2 <- lme4::lmer(trials.intensity ~ (1|participant), data = data_ss1)
anova(model, model2)

summary(model)

sjPlot::tab_model(
  model,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3
) 


# test other set size 2, 3, 5, 7
# here set size as continous
data_exc_ss1$setsize <- as.numeric(data_exc_ss1$setsize)

# Dummy
contrasts(data_exc_ss1$full_condition2) = contr.treatment(4)

# Deviation coding
# contrasts(data_exc_ss1$full_condition2) = contr.sum(4)

# check mean
tapply(data_exc_ss1$trials.intensity, data_exc_ss1$full_condition2, mean)

model <- lme4::lmer(trials.intensity ~ setsize * full_condition2 + (1|participant), data = data_exc_ss1) # selected model
model2 <- lme4::lmer(trials.intensity ~  setsize + full_condition2 + (1|participant), data = data_exc_ss1)

table(data_exc_ss1$setsize)

levels(data_exc_ss1$full_condition2)

anova(model, model2)
summary(model)


# plot model predictions
# predictions <- merTools::predictInterval(
#   model,
#   newdata = data_exc_ss1,
#   level = 0.95,  
#   n.sims = 1000, 
#   which = "full" # fixed effects
# )
# 
# data_exc_ss1 <- cbind(data_exc_ss1, predictions)
# # plot
# 
# data_by_subject <- data_exc_ss1 %>%
#   group_by(participant,
#            trials.setsize,
#            gabor_arrangment,
#            gabor_type,
#            full_condition2) %>%
#   summarise(
#     trials.intensity.pre.mean = mean(fit),
#     ci_low = mean(lwr),
#     ci_high = mean(upr),
#     n = n()
#   )
# 
# 
# data_across_subject <- data_by_subject %>%
#   group_by(trials.setsize,
#            gabor_arrangment,
#            gabor_type,
#            full_condition2) %>%
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
# # cal Single Gabor threshold
# str(data_ss1)
# 
# data_by_subject_ss1 <- data_ss1 %>%
#   group_by(participant,
#            trials.setsize,
#            gabor_arrangment2) %>%
#   summarise(
#     trials.intensity.mean = mean(trials.intensity),
#     n = n()
#   )
# 
# data_across_subject_ss1 <- data_by_subject_ss1 %>%
#   group_by(trials.setsize,
#            gabor_arrangment2) %>%
#   summarise(
#     threshold.mean = mean(trials.intensity.mean),
#     threshold.std = sd(trials.intensity.mean),
#     n = n()
#   ) %>%
#   mutate(
#     threshold_SEM = threshold.std / sqrt(n),
#     threshold_CI = threshold_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
#   )
# 
# 
# my_plot <-  ggplot() +
#   
#   geom_point(
#     data = data_across_subject,
#     aes(
#       x = as.numeric(trials.setsize),
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
#       x = as.numeric(trials.setsize),
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
#       x = as.numeric(trials.setsize),
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
#   geom_hline(yintercept = data_across_subject_ss1[[1, "threshold.mean"]], linetype = "dashed", color = "black") +
#   
#   geom_hline(yintercept = data_across_subject_ss1[[2, "threshold.mean"]], linetype = "dotted", color = "grey") +
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
#   scale_x_continuous(breaks = c(2, 3, 5, 7),
#                      labels = c("2", "3", "5", "7"), limits = c(1.5, 7.5))+
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
#   ) +
#   
#   
#   facet_wrap(~ gabor_arrangment, nrow = 1, labeller = labeller(
#     gabor_arrangment = 
#       c("radial" = "Radial",
#         "tangential" = "Tangential")
#   ))
# 
# 
# my_plot

# ggsave(file = "exp1.svg", plot = my_plot,  width = 8.0, height = 4.6, units = "in")


#variance-covariance matrix for fixed effects (needed for SEs of combined slopes)
vcov_matrix <- vcov(model)


# get slopes, test against 0
fixed_effects <- lme4::fixef(model)

slope_RL <- fixed_effects["setsize"]
se_RL <- sqrt(vcov_matrix["setsize", "setsize"])
t_RL <- slope_RL / se_RL
p_RL <- 2 * (1 - pnorm(abs(t_RL)))



slope_RS <- fixed_effects["setsize"] + fixed_effects["setsize:full_condition22"]

se_RS <- sqrt(vcov_matrix["setsize", "setsize"] +
                vcov_matrix["setsize:full_condition22",
                            "setsize:full_condition22"] +
                2 * vcov_matrix["setsize", "setsize:full_condition22"])
t_RS <- slope_RS / se_RS
p_RS <- 2 * (1 - pnorm(abs(t_RS)))


slope_TL <- fixed_effects["setsize"] + fixed_effects["setsize:full_condition23"]

se_TL <- sqrt(vcov_matrix["setsize", "setsize"] +
                vcov_matrix["setsize:full_condition23",
                            "setsize:full_condition23"] +
                2 * vcov_matrix["setsize", "setsize:full_condition23"])
t_TL <- slope_TL / se_TL
p_TL <- 2 * (1 - pnorm(abs(t_TL)))


slope_TS <- fixed_effects["setsize"] + fixed_effects["setsize:full_condition24"]
slope_TS

se_TS <- sqrt(vcov_matrix["setsize", "setsize"] +
                vcov_matrix["setsize:full_condition23",
                            "setsize:full_condition23"] +
                2 * vcov_matrix["setsize", "setsize:full_condition23"])
t_TS <- slope_TS / se_TS
p_TS <- 2 * (1 - pnorm(abs(t_TS)))

# adjust p

p_vals <- c(p_RS, p_RL, p_TS, p_TL)
p_values_corrected <- p.adjust(p_vals, method = "holm")

# results into a dataframe
results_df <- data.frame(
  Condition = c("RL", "RS", "TL", "TS"),
  Slope = c(slope_RL, slope_RS, slope_TL, slope_TS),
  SE = c(se_RL, se_RS, se_TL, se_TS),
  t_value = c(t_RL, t_RS, t_TL, t_TS),
  p_value = c(p_RL, p_RS, p_TL, p_TS)
)

# corrected p
results_df$p_value_corrected <- p.adjust(results_df$p_value, method = "holm")
results_df


sjPlot::tab_model(
  model,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3
) 

# another ref level
data_exc_ss1$full_condition2 <- relevel(data_exc_ss1$full_condition2, ref = "ladder_tangential")


# pairwise comparisons
emms <- emmeans::emmeans(
  model,
  list(pairwise ~ full_condition2),
  adjust = "tukey"
)

summary(emms, infer = TRUE)

