# libraires ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(svglite)
library(ggpubr)
# -------------exp1/exp2 --------------------------------------------------
setwd("d:/OneDrive/projects/multi_gabor_discr/data/")

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


model_glmm <- lme4::glmer(resp_no ~ setsize * gabor_type2 * correct + (1 | participant),
                          data = data_exp3_uniformity, family = binomial)
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

r2 <- MuMIn::r.squaredGLMM(model_glmm)
r2

data_exp3_uniformity$predicted <- predict(model_glmm, type = "response")


ggplot(data_exp3_uniformity, aes(x = setsize, y = predicted, color = gabor_type2)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Predicted Probabilities of Judging Items as Different",
       x = "Set Size", y = "Predicted Probability")+
  facet_wrap(~correct)


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

# long format
data_exp4_long_format <-
  reshape2::melt(
    selected_data,
    id.vars = c("label", "abs_ori", "ori", "participant"),
    variable.name = "gabor_location"
  )

data_exp4_long_format <- data_exp4_long_format %>% 
  dplyr::rename(resp_ori = value)

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

# mirroring resp if ori is negative

data_exp4_long_format <- data_exp4_long_format %>% 
  mutate(resp_ori_mirror = if_else(ori < 0, resp_ori * -1, resp_ori)) %>% 
  # adjustment error
  mutate(adj_error = resp_ori_mirror - abs_ori) %>% 
  mutate(adj_error_shortest_dis = angle_dist(resp_ori_mirror, abs_ori))


# add adjustment error type

data_exp4_long_format <- data_exp4_long_format %>% 
  mutate(error_types = case_when(
    adj_error_shortest_dis > 0 & adj_error_shortest_dis <= 90 ~ "repulsion",
    adj_error_shortest_dis >= -10 & adj_error_shortest_dis < 0 ~ "compression",
    adj_error_shortest_dis >= -90 & adj_error_shortest_dis < -10 ~ "inversion",
    adj_error_shortest_dis == 0 ~ "correct"))


# set size 3 and set size 1

data_exp4_long_format$label <- as.factor(data_exp4_long_format$label)

data_exp4_long_format_ss3 <- data_exp4_long_format %>% 
  filter(label %in% c("setsize3_r_ladder", "setsize3_r_snake"))

data_exp4_long_format_ss1 <- data_exp4_long_format %>% 
  filter(label %in% c("setsize1_h", "setsize1_v"))

data_exp4_long_format_ss3$label <- droplevels(data_exp4_long_format_ss3$label )
data_exp4_long_format_ss1$label <- droplevels(data_exp4_long_format_ss1$label )

# factors
data_exp4_long_format_ss3$error_types <- factor(data_exp4_long_format_ss3$error_types, levels = c(
  "correct", "repulsion", "compression", "inversion"))

error_type_percentages <- data_exp4_long_format_ss3 %>%
  group_by(abs_ori, gabor_location, error_types, label) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(abs_ori, gabor_location, label) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Plot the data
plot <- ggplot() +
  geom_bar(data = error_type_percentages,
           aes(x = gabor_location, 
               y = percentage, 
               fill = error_types),
    stat = "identity", position = "stack", color = "black", alpha = 0.5) +
  
  geom_text(data = error_type_percentages,
            aes(x = gabor_location, 
                y = percentage, # Position text in the middle of the bar
                label = sprintf("%.2f%%", percentage),
                group = error_types),
            position = position_stack(vjust = 0.5), # Adjust position for stacking
            color = "black", size = 3) +
  
  labs(
    title = "",
    x = "Orientation",
    y = "Percentage",
    fill = "Error Types"
  ) +
  scale_x_discrete(labels = c("inner_resp" = "Inner", 
                              "midd_resp" = "Middle",
                              "outer_resp" = "Outer")) + 
  scale_fill_manual(
    labels = c("correct", "repulsion", "compression", "inversion"),
    values = c("white", "#4467C4", "#808A87", "#EC8F4C"),
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
  facet_wrap(~ label + abs_ori,
             labeller = labeller(
               abs_ori =
                 c("2" = "2°",
                   "4" = "4°",
                   "10" = "10°"),
                label = 
                 c(
                   "setsize3_r_ladder" = "Radial Ladder",
                   "setsize3_r_snake" = "Radial Snake"
                 )
             ))

plot


