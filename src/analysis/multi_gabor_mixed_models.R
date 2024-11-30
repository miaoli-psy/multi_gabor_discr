# libraires ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(svglite)
library(ggpubr)
# -------------exp1/exp2 --------------------------------------------------
setwd("d:/OneDrive/projects/multi_gabor_discr/data/")

# read data
exp <- "exp1"

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
model2 <- lme4::lmer(trials.intensity ~  (1|participant), data = data_ss1)
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

# deviation coding: compares the mean of the dependent variable 
# for a given level to the overall mean of the dependent variable.  
#contrasts(data_exc_ss1$full_condition2) = contr.sum(4)

# check mean
tapply(data_exc_ss1$trials.intensity, data_exc_ss1$full_condition2, mean)

model <- lme4::lmer(trials.intensity ~ setsize * full_condition2 + (1|participant), data = data_exc_ss1) # selected model
model2 <- lme4::lmer(trials.intensity ~  setsize + full_condition2 + (1|participant), data = data_exc_ss1)

table(data_exc_ss1$setsize)

anova(model, model2)
summary(model)

sjPlot::tab_model(
  model,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3
) 

# pairwise comparisons
emms <- emmeans::emmeans(
  model,
  list(pairwise ~ setsize*full_condition2),
  adjust = "tukey"
)

summary(emms, infer = TRUE)



# -------------exp3 ori task---------------------------------------------

# read data
# exp3: prprcssed_mlti_gbr_sc_3.xlsx
data3 <- read_excel(path = file.choose())

# check col names
colnames(data3)

# check participants info
# 15 females; 5 males; age range 18-25
df_check_participants3 <- data3 %>% 
  group_by(participant, age, sex) %>% 
  tally()

# mean age == 20.0 years
mean(df_check_participants3$age)

# variables
str(data3)

data3$s_l <- as.factor(data3$s_l)
data3$participant <- as.factor(data3$participant)
data3$setsize <- as.integer(data3$setsize)


# LMM
model.full3 <- lmer(intensity ~ s_l + setsize + (1 + s_l | participant),
                   data = data3)


model.reduced3 <- lmer(intensity ~ setsize + (1 + s_l | participant),
                      data = data3)

summary(model.full3)
anova(model.full3, model.reduced3)

# pairwise comparisons
emms3 <- emmeans(
  model.full3,
  list(pairwise ~ s_l),
  adjust = "tukey"
)

summary(emms3, infer = TRUE)


# -----------------exp3 same or not task---------------------------------------

# get data
data3_excl1 <- data3[data3$ans_same%in% c("w", 'x'),]

# categorized answer
data3_excl1 <- data3_excl1 %>% 
  mutate(ans = case_when(
    ans_same == "w" ~ 1,
    ans_same == "x" ~ 0)
         )

colnames(data3_excl1)

data3_excl1$s_l <- as.factor(data3_excl1$s_l)
data3_excl1$participant <- as.factor(data3_excl1$participant)
data3_excl1$setsize <- as.integer(data3_excl1$setsize)

# model.glmer.interaction <-glmer(ans ~ setsize * s_l + setsize + s_l + (1 + s_l | participant), data = data3_excl1, family = binomial)
model.glmer.full <-glmer(ans ~ setsize + s_l + (1 + s_l | participant), data = data3_excl1, family = binomial)
model.glmer.reduced <-glmer(ans ~ setsize  + (1 + s_l | participant), data = data3_excl1, family = binomial)

summary(model.glmer.full)
anova(model.glmer.full, model.glmer.reduced)
# anova(model.glmer.interaction, model.glmer.full)

# pairwise com
emms <- emmeans(
  model.glmer.full,
  list(pairwise ~ s_l),
  adjust = "tukey" 
)

summary(emms)


