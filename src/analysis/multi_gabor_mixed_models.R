# libraires ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(svglite)
library(ggpubr)
library(car)
# -------------exp1/exp2 --------------------------------------------------
setwd("d:/OneDrive/projects/multi_gabor_discr/data/")

# ----------functions----------------

# cal the shortest angular distance between two angles
angle_dist <- function(a, b) {
  # diff between angles a and b
  c <- a - b
  
  # normalize the diffcto be within the range of -90 to 90 degrees
  # (c + 90) % 180 shifts the range of c to [0, 180) by adding 90, 
  # then taking modulo 180 subtracting 90 shifts the range back to [-90, 90)
  normalized_angle <- (c + 90) %% 180 - 90
  
  return (normalized_angle)
}

# ----------------Exp 1----------------------------
# read data
# exp1 gbr_sc_threshold1.xlsx; exp2 gbr_sc_threshold2.xlsx

data <- readxl::read_excel(path = file.choose())

# check col names
colnames(data)

# check participants info
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


predictions <- merTools::predictInterval(
  model,
  newdata = data_exc_ss1,
  level = 0.95,  
  n.sims = 1000, 
  which = "full" # fixed effects
)

data_exc_ss1 <- cbind(data_exc_ss1, predictions)
# plot

data_by_subject <- data_exc_ss1 %>%
  group_by(participant,
           trials.setsize,
           gabor_arrangment,
           gabor_type,
           full_condition2) %>%
  summarise(
    trials.intensity.pre.mean = mean(fit),
    ci_low = mean(lwr),
    ci_high = mean(upr),
    n = n()
  )


data_across_subject <- data_by_subject %>%
  group_by(trials.setsize,
           gabor_arrangment,
           gabor_type,
           full_condition2) %>%
  summarise(
    threshold.pre.mean = mean(trials.intensity.pre.mean),
    threshold_pre.std = sd(trials.intensity.pre.mean),
    n = n(),
    ci_l = mean(ci_low),
    ci_h = mean(ci_high)
  ) %>%
  mutate(
    threshold_SEM = threshold_pre.std / sqrt(n),
    threshold_CI = threshold_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


my_plot <-  ggplot() +

  geom_point(
    data = data_across_subject,
    aes(
      x = as.numeric(trials.setsize),
      y = threshold.pre.mean,
      group = gabor_type,
      color = gabor_type,
      size = 0.5
    ),

    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.6
  ) +

  # geom_point(
  #   data = data_exc_ss1,
  #   aes(
  #     x = as.numeric(trials.setsize),
  #     y = fit,
  #     group = gabor_type,
  #     color = gabor_type,
  #     size = 0.4
  #   ),
  #   alpha = 0.1,
  #   position = position_dodge(0.5)
  # ) +

  geom_line(
    data = data_across_subject,
      aes(
        x = as.numeric(trials.setsize),
        y = threshold.pre.mean,
        group = gabor_type,
        color = gabor_type,
        size = 0.1)
    )+
    
  # stat_smooth(
  #   data = data_across_subject,
  #   aes(
  #     x = as.numeric(trials.setsize),
  #     y = threshold.pre.mean,
  #     group = gabor_type,
  #     color = gabor_type
  #   ),
  #   method = "lm",
  #   size = 1.5,
  #   se = FALSE,
  #   alpha = 0.5,
  #   geom = "line",
  #   show.legend = FALSE
  # )+

  geom_errorbar(
    data = data_across_subject,
    aes(
      x = as.numeric(trials.setsize),
      y = threshold.pre.mean,
      ymin = threshold.pre.mean - threshold_CI,
      ymax = threshold.pre.mean + threshold_CI,
      group = gabor_type,
      color = gabor_type
    ),
    size  = 0.8,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +

  labs(y = "Threshold predicted by LMM", x = "Set size") +


  scale_color_manual(
    labels = c("Ladder", "Snake"),
    values = c("#F28522", "#674EA7"), #DDAA33
    name = "gabor type"
  ) +


  scale_y_continuous(limits = c(1, 5)) +

  scale_x_continuous(breaks = c(2, 3, 5, 7),
                     labels = c("2", "3", "5", "7"), limits = c(1.5, 7.5))+

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


  facet_wrap(~ gabor_arrangment, nrow = 1, labeller = labeller(
    gabor_arrangment = 
      c("radial" = "Radial",
      "tangential" = "Tangential")
  ))


my_plot

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


# ---------------EXP 3-------------------------

# exp3 threshold
# read data: gbr_sc_threshold3.xlsx

data_exp3_threshold <- readxl::read_excel(path = file.choose())

# check col names
colnames(data_exp3_threshold)

# check participants info
# 15 females; 5 males; age range 18-25
df_check_participants <- data_exp3_threshold %>% 
  group_by(participant, age, sex) %>% 
  tally()

# mean age == 20.0 years
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


# plot

predictions <- merTools::predictInterval(
  model,
  newdata = data_exp3_threshold,
  level = 0.95,  
  n.sims = 1000, 
  which = "full" # fixed effects
)

data_exp3_threshold <- cbind(data_exp3_threshold, predictions)


data_by_subject <- data_exp3_threshold %>%
  group_by(participant,
           setsize,
           gabor_type) %>%
  summarise(
    trials.intensity.pre.mean = mean(fit),
    ci_low = mean(lwr),
    ci_high = mean(upr),
    n = n()
  )


data_across_subject <- data_by_subject %>%
  group_by(setsize,
           gabor_type) %>%
  summarise(
    threshold.pre.mean = mean(trials.intensity.pre.mean),
    threshold_pre.std = sd(trials.intensity.pre.mean),
    n = n(),
    ci_l = mean(ci_low),
    ci_h = mean(ci_high)
  ) %>%
  mutate(
    threshold_SEM = threshold_pre.std / sqrt(n),
    threshold_CI = threshold_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )




my_plot3 <-  ggplot() +
  
  geom_point(
    data = data_across_subject,
    aes(
      x = as.numeric(setsize),
      y = threshold.pre.mean,
      group = gabor_type,
      color = gabor_type,
      size = 0.5
    ),
    
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.6
  ) +
  
  # geom_point(
  #   data = data_exc_ss1,
  #   aes(
  #     x = as.numeric(trials.setsize),
  #     y = fit,
  #     group = gabor_type,
  #     color = gabor_type,
  #     size = 0.4
  #   ),
  #   alpha = 0.1,
  #   position = position_dodge(0.5)
  # ) +
  
  geom_line(
    data = data_across_subject,
    aes(
      x = as.numeric(setsize),
      y = threshold.pre.mean,
      group = gabor_type,
      color = gabor_type,
      size = 0.1)
  )+
  
  # stat_smooth(
  #   data = data_across_subject,
  #   aes(
  #     x = as.numeric(trials.setsize),
  #     y = threshold.pre.mean,
  #     group = gabor_type,
  #     color = gabor_type
  #   ),
  #   method = "lm",
  #   size = 1.5,
  #   se = FALSE,
  #   alpha = 0.5,
  #   geom = "line",
  #   show.legend = FALSE
  # )+
  
  geom_errorbar(
    data = data_across_subject,
    aes(
      x = as.numeric(setsize),
      y = threshold.pre.mean,
      ymin = threshold.pre.mean - threshold_CI,
      ymax = threshold.pre.mean + threshold_CI,
      group = gabor_type,
      color = gabor_type
    ),
    size  = 0.8,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  labs(y = "Threshold predicted by LMM", x = "Set size") +
  
  
  scale_color_manual(
    labels = c("Ladder", "Snake"),
    values = c("#F28522", "#674EA7"), #DDAA33
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
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) 
  

my_plot3

ggsave(file = "exp3.svg", plot = my_plot3,  width = 5, height = 4.6, units = "in")



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


# ---------------EXP 3-------------------------

# exp3 Uniformity Judgment 

# read data
# gabor_2tasks_exp3_alldata.csv
data_exp3_uniformity <- read.csv(file.choose())

# check col names
colnames(data_exp3_uniformity)

# factor
data_exp3_uniformity$gabor_type2 <-as.factor(data_exp3_uniformity$gabor_type2)

levels(data_exp3_uniformity$gabor_type2)

# remove setsize 1 
data_exp3_uniformity <- data_exp3_uniformity %>% 
  filter(gabor_type2 %in% c("snake", "ladder"))

# drop unused factor levels
data_exp3_uniformity$gabor_type2 <- droplevels(data_exp3_uniformity$gabor_type2)


# add correct
data_exp3_uniformity$correct <- NA

# compare intensity in row N and row N+1
for (i in 1:(nrow(data_exp3_uniformity) - 1)) {
  if (data_exp3_uniformity$intensity[i + 1] >= data_exp3_uniformity$intensity[i]) {
    data_exp3_uniformity$correct[i] <- 0
  } else if (data_exp3_uniformity$intensity[i + 1] < data_exp3_uniformity$intensity[i]) {
    data_exp3_uniformity$correct[i] <- 1
  }
}

# indices of all "start"
start_indices <- which(data_exp3_uniformity$direction == "start")

# row start-1
rows_to_remove <- start_indices[start_indices != start_indices[1]] - 1

# remove
data_exp3_uniformity <- data_exp3_uniformity[-rows_to_remove, ]

# remove last row
data_exp3_uniformity <- data_exp3_uniformity[-length(data_exp3_uniformity$intensity), ] 

# clean up the col ans_same
data_exp3_uniformity <- data_exp3_uniformity %>% 
  filter(ans_same %in% c("w", "x"))


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

# plot prediction

predictions <- ggeffects::ggpredict(
  model_glmm, 
  terms = c("setsize", "gabor_type2", "correct")  # Specify predictors
)

# Plot predicted probabilities
my_plot_exp3_uniformaty <- ggplot()+
  geom_line(data = predictions,
            aes(
              x = x, 
              y = predicted, 
              color = group),
            size = 1.2,
            alpha = 0.5
            )+  
  geom_ribbon(data = predictions,
              aes
              (x = x,
                ymin = conf.low, 
                ymax = conf.high, 
                fill = group), 
              alpha = 0.1) +  
  
  scale_color_manual(
    labels = c("Ladder", "Snake"),
    values = c("#F28522", "#674EA7"), #DDAA33
    name = "gabor type") +
  
    scale_fill_manual(
      labels = c("Ladder", "Snake"),
      values = c("#F28522", "#674EA7"), #DDAA33
      name = "gabor type"
    
  ) +
  
  scale_y_continuous(limits = c(0, 0.8)) +
  
  labs(
    x = "Set Size",
    y = "Predicted Probability",
    color = "Gabor Type",
    fill = "Gabor Type"
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
  
  facet_wrap(~facet, nrow = 1, labeller = labeller(
    facet =
      c("0" = "Wrong Judgment",
        "1" = "Correct Judgment"
      )
  ))  

my_plot_exp3_uniformaty


ggsave(file = "exp3_uni.svg", plot = my_plot_exp3_uniformaty,  width = 8, height = 4.6, units = "in")




r2 <- MuMIn::r.squaredGLMM(model_glmm)
r2




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

# ---------------EXP 4 adjust orientation-------------------------

# read data
# gabor_adjst_ori_alldata.csv
data_exp4<- read.csv(file.choose())

colnames(data_exp4)

# check participants info
df_check_participants <- data_exp4 %>% 
  group_by(participant, age, sex) %>% 
  tally()

# mean age == 20.0 years
mean(df_check_participants$age)

selected_data <- data_exp4 %>%
  dplyr::select(label, ori, abs_ori, participant, inner_resp, midd_resp, outer_resp)

# for single gabor, middle and outer location fill out
selected_data[is.na(selected_data)] <- 9999

# long format
data_exp4_long_format <-
  reshape2::melt(
    selected_data,
    id.vars = c("label", "abs_ori", "ori", "participant"),
    variable.name = "gabor_location"
  )

data_exp4_long_format <- data_exp4_long_format %>% 
  dplyr::rename(resp_ori = value)

data_exp4_long_format <- subset(data_exp4_long_format, resp_ori != 9999)

data_exp4_long_format <- data_exp4_long_format %>%
  mutate(
    gabor_location = case_when(
      label == "setsize1_v" & gabor_location == "inner_resp" ~ "SingelVertical",
      label == "setsize1_h" & gabor_location == "inner_resp" ~ "SingerHorizontal",
      gabor_location == "inner_resp" ~ "inner_resp",
      gabor_location == "midd_resp" ~ "midd_resp",
      gabor_location == "outer_resp" ~ "outer_resp"))


# mirroring resp if ori is negative

data_exp4_long_format <- data_exp4_long_format %>% 
  mutate(resp_ori_mirror = if_else(ori < 0, resp_ori * -1, resp_ori)) %>% 
  # adjustment error
  mutate(adj_error = resp_ori_mirror - abs_ori) %>% 
  mutate(adj_error_shortest_dis = angle_dist(resp_ori_mirror, abs_ori))


# add adjustment error type
data_exp4_long_format <- data_exp4_long_format %>% 
  mutate(
    compression_limit = case_when(
      abs_ori == 10 ~ -10,
      abs_ori == 4 ~ -4,
      abs_ori == 2 ~ -2,
      TRUE ~ -10 # Default for any unhandled `abs_ori` value
    ),
    error_types = case_when(
      adj_error_shortest_dis > 0 & adj_error_shortest_dis <= 90 ~ "repulsion",
      adj_error_shortest_dis >= compression_limit & adj_error_shortest_dis < 0 ~ "compression",
      adj_error_shortest_dis >= -90 & adj_error_shortest_dis < compression_limit ~ "inversion",
      adj_error_shortest_dis == 0 ~ "correct"
    )
  )

# set size 3 and set size 1

data_exp4_long_format$label <- as.factor(data_exp4_long_format$label)

data_exp4_long_format_ss3 <- data_exp4_long_format %>% 
  filter(label %in% c("setsize3_r_ladder", "setsize3_r_snake"))

data_exp4_long_format_ss1 <- data_exp4_long_format %>% 
  filter(label %in% c("setsize1_h", "setsize1_v"))

data_exp4_long_format_ss3$label <- droplevels(data_exp4_long_format_ss3$label )
data_exp4_long_format_ss1$label <- droplevels(data_exp4_long_format_ss1$label )


# data to plot
data_exp4_data_plot <- rbind(data_exp4_long_format_ss3, data_exp4_long_format_ss1)

# some processing

data_exp4_data_plot <- data_exp4_data_plot %>% 
  mutate(label = case_when(
    label == "setsize3_r_ladder" | label== "setsize1_h" ~ "ladder",
    label == "setsize3_r_snake" | label== "setsize1_v" ~ "snake"
  ) )

# factors
data_exp4_data_plot$error_types <- factor(data_exp4_data_plot$error_types, levels = c(
 "repulsion",  "correct", "compression", "inversion"))

# error type for each condition
error_type_percentages <- data_exp4_data_plot %>%
  group_by(abs_ori, gabor_location, error_types, label) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(abs_ori, gabor_location, label) %>%
  mutate(percentage = (count / sum(count)) * 100)

# average adj error
ave_adj_error_by_participant <- data_exp4_data_plot %>% 
  group_by(abs_ori, gabor_location, label, participant) %>%
  dplyr::summarise(
    adj_error = mean(adj_error_shortest_dis),
    adj_error_sd = sd(adj_error_shortest_dis),
    # # compare mean with the package calculation, do not differ much
    # adj_error_circularmean = circular::mean.circular(circular::circular(adj_error_shortest_dis / 180 * pi)) / pi * 180,
    # adj_error_sd_circular = circular::sd.circular(circular::circular(adj_error_shortest_dis / 180 * pi)) / pi * 180,
    n = n()
  ) %>% 
  mutate(
    adj_error_sem = adj_error_sd/sqrt(n),
    adj_error_ci = adj_error_sem * qt((1 - 0.05) / 2 + .5, n -1)
  )

ave_adj_error <- ave_adj_error_by_participant %>% 
  group_by(abs_ori, gabor_location, label) %>%
  dplyr::summarise(
    adj_error_mean = mean(adj_error),
    adj_error_mean_sd = sd(adj_error),
    group_var = mean(adj_error_sd),
    group_var_sd = sd(adj_error_sd),
    n = n()
  ) %>% 
  mutate(
    adj_error_sem = adj_error_mean_sd/sqrt(n),
    adj_error_ci = adj_error_sem * qt((1 - 0.05) / 2 + .5, n - 1),
    group_var_sem = group_var_sd/sqrt(n),
    group_var_ci = group_var_sem * qt((1 - 0.05) / 2 + .5, n - 1)
  )

# error type distribution
scaling_factor <- 100 / 10 # if secondary y-axis is added

plot <- ggplot() +
  
  geom_bar(data = error_type_percentages,
           aes(x = gabor_location, 
               y = percentage, 
               fill = error_types),
    stat = "identity", position = "stack", color = "grey", alpha = 0.5) +
  
  geom_text(data = error_type_percentages,
            aes(x = gabor_location, 
                y = percentage, # Position text in the middle of the bar
                label = sprintf("%.2f", percentage),
                group = error_types,
                alpha = 0.5),
            position = position_stack(vjust = 0.5), # Adjust position for stacking
            color = "black", size = 3) +
  
  geom_point(data = ave_adj_error,
             aes(x = gabor_location,
                 y = (adj_error_mean + 5) * scaling_factor),
             shape = 21,
             color = "black", size = 3.5) + # Use points for averages

  geom_errorbar(data = ave_adj_error,
                aes(x = gabor_location,
                    ymin = (adj_error_mean - adj_error_ci + 5) * scaling_factor,
                    ymax = (adj_error_mean + adj_error_ci + 5) * scaling_factor),
                width = 0, color = "black",
                linewidth = 0.8) + # Add error bars for averages

  # geom_text(data = ave_adj_error,
  #           aes(x = gabor_location,
  #               y = (adj_error_mean + 5) * scaling_factor,
  #               label = sprintf("%.2f", adj_error_mean)),
  #           color = "red", vjust = -0.5, size = 3) + # Add labels for averages

  # Scales and secondary axis
  scale_y_continuous(
    name = "Percentage (0-100%)",
    limits = c(-15, 101), # Fix primary y-axis range to 0-100
    sec.axis = sec_axis(
      trans = ~ . / scaling_factor - 5, # Map back to -5 to 5
      name = "Adjustment Error (-5° to 5°)" # Label for the secondary axis
    )
  ) +
  
  labs(
    title = "",
    x = "Location",
    y = "Percentage",
    fill = "Error Types"
  ) +
  scale_x_discrete(labels = c("inner_resp" = "Inner", 
                              "midd_resp" = "Middle",
                              "outer_resp" = "Outer",
                              "SingelVertical" = "SV",
                              "SingerHorizontal" = "SH")) + 
  scale_fill_manual(
    labels = c( "repulsion", "correct", "compression", "inversion"),
    values = c( "#4467C4","white", "#808A87", "#EC8F4C"),
    name = "Error type"
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
  facet_wrap(~ label + abs_ori,
             labeller = labeller(
               abs_ori =
                 c("2" = "2°",
                   "4" = "4°",
                   "10" = "10°"),
                label = 
                 c(
                   "ladder" = "Radial Ladder",
                   "snake" = "Radial Snake"
                 )
             ))

plot

ggsave(file = "plot.svg", plot = plot,  width = 13, height = 7.5, units = "in")


plot_mean_adj_error <- ggplot() +
  geom_point(
    data = ave_adj_error,
    aes(
      x = gabor_location,
      y = adj_error_mean,
      group = label,
      color = label,
      size = 0.4
    ),
    position = position_dodge(0.8),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE) +
  
  geom_errorbar(
    data = ave_adj_error,
    aes(
      x = gabor_location,
      y = adj_error_mean,
      ymin = adj_error_mean - adj_error_ci,
      ymax = adj_error_mean + adj_error_ci,
      group = label,
      color = label
    ),
    
    size  = 0.8,
    width = .00,
    alpha = 0.5,
    position = position_dodge(0.8)
  ) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  labs(y = "Adjustment Arror(°)", x = "Gabor Location") +
  
  # scale_y_continuous(limits = c(-4, 4)) +
  # 
  scale_x_discrete(labels = c("inner_resp" = "Inner", 
                              "midd_resp" = "Middle",
                              "outer_resp" = "Outer",
                              "SingelVertical" = "SV",
                              "SingerHorizontal" = "SH")) + 
  
  scale_color_manual(
    labels = c("Radial Ladder",  "Radial Sanke"),
    values = c("#BB5566", "#674EA7"),
    name = "Gabor type"
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
  facet_wrap(~ abs_ori, nrow = 1, labeller = labeller(
    abs_ori =
      c("2" = "2°",
        "4" = "4°",
        "10" = "10°"
      )
  ))

plot_mean_adj_error

plot_mean_adj_error2 <- ggplot() +
  geom_point(
    data = ave_adj_error,
    aes(
      x = abs_ori,
      y = adj_error_mean,
      group = gabor_location,
      color = gabor_location,
      size = 0.4
    ),
    position = position_dodge(0.8),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE) +
  
  geom_errorbar(
    data = ave_adj_error,
    aes(
      x = abs_ori,
      y = adj_error_mean,
      ymin = adj_error_mean - adj_error_ci,
      ymax = adj_error_mean + adj_error_ci,
      group = gabor_location,
      color = gabor_location
    ),
    
    size  = 0.8,
    width = .00,
    alpha = 0.5,
    position = position_dodge(0.8)
  ) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  labs(y = "Adjustment Arror(°)", x = "Orientaion (°)") +
  
  scale_y_continuous(limits = c(-7, 6)) +

  scale_color_manual(
    labels = c("Inner",  "Middle", "Outer", "Singel Vertical", "Singel Horizontal"),
    values = c("#2066a8", "#ea801c", "#1f6f6f", "#b8b8b8", "black"),
    name = "Gabor Location"
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
  facet_wrap(~ label, nrow = 1, labeller = labeller(
    label =
      c("ladder" = "Radial Ladder",
        "snake" = "Radial Snake")
  ))



plot_mean_adj_error2

# ggsave(file = "plot_mean_adj_error2.svg", plot = plot_mean_adj_error2,  width = 9.4, height = 3.8, units = "in")


plot_group_variability <- ggplot() +
  geom_point(
    data = ave_adj_error,
    aes(
      x = gabor_location,
      y = group_var,
      group = label,
      color = label,
      size = 0.4
    ),
    position = position_dodge(0.8),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE) +
  
  geom_errorbar(
    data = ave_adj_error,
    aes(
      x = gabor_location,
      y = group_var,
      ymin = group_var - group_var_ci,
      ymax = group_var + group_var_ci,
      group = label,
      color = label
    ),
    
    size  = 0.8,
    width = .00,
    alpha = 0.5,
    position = position_dodge(0.8)
  ) +
  
  
  labs(y = "Group Variability", x = "Gabor Location") +
  
  # scale_y_continuous(limits = c(2, 12)) +
  
  scale_x_discrete(labels = c("inner_resp" = "Inner", 
                              "midd_resp" = "Middle",
                              "outer_resp" = "Outer",
                              "SingelVertical" = "SV",
                              "SingerHorizontal" = "SH")) + 
  
  scale_color_manual(
    labels = c("Radial Ladder",  "Radial Sanke"),
    values = c("#BB5566", "#674EA7"),
    name = "Gabor type"
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
  facet_wrap(~ abs_ori, nrow = 1, labeller = labeller(
    abs_ori =
      c("2" = "2°",
        "4" = "4°",
        "10" = "10°"
      )
  ))

plot_group_variability

# model

# variables
str(data_exp4_long_format_ss3)

data_exp4_long_format_ss3$abs_ori <-as.factor(data_exp4_long_format_ss3$abs_ori)
data_exp4_long_format_ss3$gabor_location <-as.factor(data_exp4_long_format_ss3$gabor_location)
data_exp4_long_format_ss3$label <-as.factor(data_exp4_long_format_ss3$label)

table(data_exp4_long_format_ss3$abs_ori)
table(data_exp4_long_format_ss3$gabor_location)
table(data_exp4_long_format_ss3$label)


model <- lme4::lmer(adj_error_shortest_dis ~ 
                      abs_ori + gabor_location + label + (1|participant), 
                    data = data_exp4_long_format_ss3)

model3 <- lme4::lmer(adj_error_shortest_dis ~ 
                      abs_ori + label + (1|participant), 
                    data = data_exp4_long_format_ss3)

model2<- lme4::lmer(adj_error_shortest_dis ~ 
                      abs_ori * gabor_location * label + (1|participant) , 
                    data = data_exp4_long_format_ss3, REML = TRUE) # selected model



anova(model, model3)
summary(model2)

# 10 min (Type II Wald F tests with Kenward-Roger df)
# car::Anova(model2,type="II",test.statistic="F")

sjPlot::tab_model(
  model2,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3
) 


# pairwise comparisons
emms <- emmeans::emmeans(
  model2,
  list(pairwise ~ gabor_location | abs_ori * label),
  adjust = "tukey"
)

summary(emms, infer = TRUE)


# plot 3-way interaction
emmeans_three_way <- emmeans::emmeans(model2, ~ abs_ori * gabor_location * label)

# pairwise comparisons for all combinations
pairs(emmeans_three_way)
# get emmeans for three-way interaction
plot_data_three_way <- as.data.frame(emmeans_three_way)

# plot
ggplot(plot_data_three_way, aes(x = abs_ori, y = emmean, color = gabor_location, group = gabor_location)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ label) +
  labs(title = "Three-Way Interaction: abs_ori × gabor_location × label",
       x = "Orientation Difference (abs_ori)",
       y = "Estimated Adjustment Error",
       color = "Gabor Location") +
  theme_minimal()



# multinomial logistic
data_exp4_long_format_ss3$error_types <- factor(data_exp4_long_format_ss3$error_types, levels = c(
   "correct", "repulsion", "compression", "inversion"))

model <- nnet::multinom(error_types ~ label + abs_ori + gabor_location, data = data_exp4_long_format_ss3)
summary(model)



sjPlot::tab_model(
  model,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3
) 


# --------exp4: errors are not independent within a trial----------

# read data
# gabor_adjst_ori_alldata.csv
data_exp4<- read.csv(file.choose())

selected_data <- data_exp4 %>%
  dplyr::select(label, ori, abs_ori, participant, inner_resp, midd_resp, outer_resp)

# for single gabor, middle and outer location fill out
selected_data[is.na(selected_data)] <- 9999

# mirroring resp if ori is negative
data_exp4 <- selected_data %>% 
  mutate(inner_resp_mirror = if_else(ori < 0, inner_resp * -1, inner_resp)) %>% 
  # adjustment error
  mutate(adj_error_inner = inner_resp_mirror - abs_ori) %>% 
  mutate(adj_error_shortest_dis_inner = angle_dist(inner_resp_mirror, abs_ori)) %>% 

  mutate(outer_resp_mirror = if_else(ori < 0, outer_resp * -1, outer_resp)) %>% 
  mutate(adj_error_outer = outer_resp_mirror - abs_ori) %>% 
  mutate(adj_error_shortest_dis_outer = angle_dist(outer_resp_mirror, abs_ori)) %>% 
  
  mutate(midd_resp_mirror = if_else(ori < 0, midd_resp * -1, midd_resp)) %>% 
  mutate(adj_error_midd = midd_resp_mirror - abs_ori) %>% 
  mutate(adj_error_shortest_dis_midd = angle_dist(midd_resp_mirror, abs_ori))


# add adjustment error type
data_exp4 <- data_exp4 %>% 
  mutate(
    compression_limit = case_when(
      abs_ori == 10 ~ -10,
      abs_ori == 4 ~ -4,
      abs_ori == 2 ~ -2,
      TRUE ~ 100 # default for any unhandled `abs_ori` value
    ),
    inner_error_types = case_when(
      adj_error_shortest_dis_inner > 0 & adj_error_shortest_dis_inner <= 90 ~ "repulsion",
      adj_error_shortest_dis_inner >= adj_error_shortest_dis_inner & adj_error_shortest_dis_inner < 0 ~ "compression",
      adj_error_shortest_dis_inner >= -90 & adj_error_shortest_dis_inner < compression_limit ~ "inversion",
      adj_error_shortest_dis_inner == 0 ~ "correct"
    ),
    
    outer_error_types = case_when(
      adj_error_shortest_dis_outer > 0 & adj_error_shortest_dis_outer <= 90 ~ "repulsion",
      adj_error_shortest_dis_outer >= compression_limit & adj_error_shortest_dis_outer < 0 ~ "compression",
      adj_error_shortest_dis_outer >= -90 & adj_error_shortest_dis_outer < compression_limit ~ "inversion",
      adj_error_shortest_dis_outer == 0 ~ "correct"
    ),
    
    midd_error_types = case_when(
      adj_error_shortest_dis_midd > 0 & adj_error_shortest_dis_midd <= 90 ~ "repulsion",
      adj_error_shortest_dis_midd >= compression_limit & adj_error_shortest_dis_midd < 0 ~ "compression",
      adj_error_shortest_dis_midd >= -90 & adj_error_shortest_dis_midd < compression_limit ~ "inversion",
      adj_error_shortest_dis_midd == 0 ~ "correct"
    )
  )


# split setsize 3 and setsize 1
data_exp4_setsize3 <- subset(data_exp4, label %in% c("setsize3_r_ladder", "setsize3_r_snake"))
data_exp4_setsize1 <- subset(data_exp4, label %in% c("setsize1_v", "setsize1_h"))

# write.csv(data_exp4_setsize3, "data.csv",row.names = FALSE)

# check error correlations

unique(data_exp4_setsize3$label)

# split by label
data_ladder <- data_exp4_setsize3[data_exp4_setsize3$label == "setsize3_r_ladder", ]

data_ladder2 <- data_ladder[data_ladder$abs_ori == 2, ]
data_ladder4 <- data_ladder[data_ladder$abs_ori == 4, ]
data_ladder10 <- data_ladder[data_ladder$abs_ori == 10, ]

data_snake <- data_exp4_setsize3[data_exp4_setsize3$label == "setsize3_r_snake", ] 

data_snake2 <- data_snake[data_snake$abs_ori == 2, ]
data_snake4 <- data_snake[data_snake$abs_ori == 4, ]
data_snake10 <- data_snake[data_snake$abs_ori == 10, ]


calculate_correlations_with_p <- function(df, cond_name) {
  cat("Condition:", cond_name, "\n")
  
  # Inner vs. Middle
  res1 <- cor.test(df$adj_error_shortest_dis_inner, df$adj_error_shortest_dis_midd, method = "pearson")
  cat("Inner vs. Middle: r =", res1$estimate, ", p =", res1$p.value, "\n")
  
  # Inner vs. Outer
  res2 <- cor.test(df$adj_error_shortest_dis_inner, df$adj_error_shortest_dis_outer, method = "pearson")
  cat("Inner vs. Outer: r =", res2$estimate, ", p =", res2$p.value, "\n")
  
  # Middle vs. Outer
  res3 <- cor.test(df$adj_error_shortest_dis_midd, df$adj_error_shortest_dis_outer, method = "pearson")
  cat("Middle vs. Outer: r =", res3$estimate, ", p =", res3$p.value, "\n\n")
}

# all conditions
calculate_correlations_with_p(data_ladder2, "2 Degrees - Ladder")
calculate_correlations_with_p(data_snake2, "2 Degrees - Snake")
calculate_correlations_with_p(data_ladder4, "4 Degrees - Ladder")
calculate_correlations_with_p(data_snake4, "4 Degrees - Snake")
calculate_correlations_with_p(data_ladder10, "10 Degrees - Ladder")
calculate_correlations_with_p(data_snake10, "10 Degrees - Snake")



plot_correlation <- function(df, cond_name, x_col, y_col, x_label, y_label) {
  ggplot(df, aes_string(x = x_col, y = y_col)) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "lm", color = "blue", alpha = 0.8) +
    labs(title = paste0(cond_name, ": ", x_label, " vs. ", y_label),
         x = x_label,
         y = y_label) +
    
    scale_y_continuous(limits = c(-50, 50)) +
    
    scale_x_continuous(limits = c(-50, 50)) +
    
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
}

# Generate plots for all conditions

plots <- list(
  plot_correlation(data_ladder2, "2 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_midd", "Inner", "Middle"),
  plot_correlation(data_ladder2, "2 Degrees - Ladder", "adj_error_shortest_dis_midd", "adj_error_shortest_dis_outer", "Middle", "Outer"),
  plot_correlation(data_ladder2, "2 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_outer", "Inner", "Outer"),
  
  plot_correlation(data_ladder4, "4 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_midd", "Inner", "Middle"),
  plot_correlation(data_ladder4, "4 Degrees - Ladder", "adj_error_shortest_dis_midd", "adj_error_shortest_dis_outer", "Middle", "Outer"),
  plot_correlation(data_ladder4, "4 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_outer", "Inner", "Outer"),
  
  plot_correlation(data_ladder10, "10 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_midd", "Inner", "Middle"),
  plot_correlation(data_ladder10, "10 Degrees - Ladder", "adj_error_shortest_dis_midd", "adj_error_shortest_dis_outer", "Middle", "Outer"),
  plot_correlation(data_ladder10, "10 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_outer", "Inner", "Outer"),
  
  plot_correlation(data_snake2, "2 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_midd", "Inner", "Middle"),
  plot_correlation(data_snake2, "2 Degrees - Ladder", "adj_error_shortest_dis_midd", "adj_error_shortest_dis_outer", "Middle", "Outer"),
  plot_correlation(data_snake2, "2 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_outer", "Inner", "Outer"),
  
  plot_correlation(data_snake4, "4 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_midd", "Inner", "Middle"),
  plot_correlation(data_snake4, "4 Degrees - Ladder", "adj_error_shortest_dis_midd", "adj_error_shortest_dis_outer", "Middle", "Outer"),
  plot_correlation(data_snake4, "4 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_outer", "Inner", "Outer"),
  
  plot_correlation(data_snake10, "10 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_midd", "Inner", "Middle"),
  plot_correlation(data_snake10, "10 Degrees - Ladder", "adj_error_shortest_dis_midd", "adj_error_shortest_dis_outer", "Middle", "Outer"),
  plot_correlation(data_snake10, "10 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_outer", "Inner", "Outer")
)

# Save plots
for (i in seq_along(plots)) {
  ggsave(file = paste0("p", i, ".svg"), plot = plots[[i]], width = 2.5, height = 2.5, units = "in")
}



plot_correlation2 <- function(df, x_col, y_col, x_label, y_label, label, abs_ori) {
  ggplot(df, aes_string(x = x_col, y = y_col, color = label)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = FALSE, alpha = 0.2) +
    labs(title = paste0( x_label, " vs. ", y_label),
         x = x_label,
         y = y_label) +
    
    scale_color_manual(
      labels = c("setsize3_r_ladder",  "setsize3_r_snake"),
      values = c("#F28522", "#674EA7"),
      name = "Gabor Location"
    ) +
    
    scale_y_continuous(limits = c(-50, 50)) +
    
    scale_x_continuous(limits = c(-50, 50)) +
    
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
    facet_wrap(~ abs_ori, nrow = 1, labeller = labeller(
      abs_ori =
        c("2" = "2°",
          "4" = "4°",
          "10" = "10°"
        )))
}

plots <- list(
  plot_correlation2(data_exp4_setsize3, "adj_error_shortest_dis_inner", "adj_error_shortest_dis_midd", "Inner", "Middle", "label", "abs_ori"),
  plot_correlation2(data_exp4_setsize3, "adj_error_shortest_dis_midd", "adj_error_shortest_dis_outer", "Middle", "Outer", "label", "abs_ori"),
  plot_correlation2(data_exp4_setsize3, "adj_error_shortest_dis_inner", "adj_error_shortest_dis_outer", "Inner", "Outer", "label", "abs_ori")
)
for (i in seq_along(plots)) {
  ggsave(file = paste0("p", i, ".svg"), plot = plots[[i]], width = 12, height = 4, units = "in")
}

# # MANOVA
# 
# # check assumptions normality
# hist(data_exp4_setsize3$adj_error_shortest_dis_inner, breaks = 20, main = "Histogram of Errors (Inner)")
# hist(data_exp4_setsize3$adj_error_shortest_dis_outer, breaks = 20, main = "Histogram of Errors (Outer)")
# hist(data_exp4_setsize3$adj_error_shortest_dis_midd, breaks = 20, main = "Histogram of Errors (Middle)")
# qqnorm(data_exp4_setsize3$adj_error_shortest_dis_inner)
# qqline(data_exp4_setsize3$adj_error_shortest_dis_inner, col = "red")
# qqnorm(data_exp4_setsize3$adj_error_shortest_dis_midd)
# qqline(data_exp4_setsize3$adj_error_shortest_dis_midd, col = "red")
# qqnorm(data_exp4_setsize3$adj_error_shortest_dis_outer)
# qqline(data_exp4_setsize3$adj_error_shortest_dis_outer, col = "red")
# 
# # check homogeneity
# data_exp4_setsize3$abs_ori <- as.factor(data_exp4_setsize3$abs_ori)
# data_exp4_setsize3$label <- as.factor(data_exp4_setsize3$label)
# 
# car::leveneTest(adj_error_shortest_dis_inner ~ abs_ori * label, data = data_exp4_setsize3)
# car::leveneTest(adj_error_shortest_dis_midd ~ abs_ori * label, data = data_exp4_setsize3)
# car::leveneTest(adj_error_shortest_dis_outer ~ abs_ori * label, data = data_exp4_setsize3)
# 
# # vilolate homogeneity, use Per-Manova instead
# 
# # Run PERMANOVA
# data_exp4_setsize3$abs_ori <- relevel(factor(data_exp4_setsize3$abs_ori), ref = "2")
# data_exp4_setsize3$label <- relevel(factor(data_exp4_setsize3$label), ref = "setsize3_r_ladder")
# 
# perm_model <- lmPerm::lmp(cbind(adj_error_shortest_dis_inner, adj_error_shortest_dis_midd, adj_error_shortest_dis_outer) ~ abs_ori * label, 
#                   data = data_exp4_setsize3, perm = "Prob")
# 
# # View results
# summary(perm_model)

ggplot(data_exp4_setsize3, aes(x = adj_error_shortest_dis_inner, y = adj_error_shortest_dis_outer, color = label)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~abs_ori) +
  labs(title = "Correlation of Adjustment Errors (Inner vs Outer)",
       x = "Inner Adjustment Error", y = "Outer Adjustment Error") +
  theme_minimal()

