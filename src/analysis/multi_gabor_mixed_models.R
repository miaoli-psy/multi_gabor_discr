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

data <- read.csv(file.choose(), header = TRUE, sep = ",")

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

data$s_l <- as.factor(data$s_l)
data$r_t <- as.factor(data$r_t)
data$condition <- as.factor(data$condition)
data$participant <- as.factor(data$participant)
data$setsize <- as.integer(data$trials.setsize)


# LMM
 
model.full.interaction <- lmer(trials.intensity ~ condition * setsize + (1 + condition | participant),
                               data = data)

model.full <- lmer(trials.intensity ~ condition + setsize + (1 + condition | participant),
                  data = data)


model.reduced <- lmer(trials.intensity ~ setsize + (1 + condition | participant),
                   data = data)

summary(model.full)
anova(model.full, model.reduced)

# pairwise comparisons
emms <- emmeans(
  model.full.interaction,
  list(pairwise ~ condition),
  adjust = "tukey"
)

summary(emms, infer = TRUE)

# APA style table???? why so slow? 
tab_model(model.full, p.val = "kr", show.df = TRUE, show.se = FALSE, show.stat = FALSE)


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


