# libraires ---------------------------------------------------------------
library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(emmeans)
library(sjstats)
library(lme4)
library(lmerTest)
library(MuMIn)
library(multcomp)
library(nlme)
library(r2glmm)
library(ggplot2)
library(ggthemes)
library(svglite)
library(sjPlot)
library(ggpubr)
library(mixedpower)


# multi gabor staircase exp --------------------------------------

# set working path
setwd("c:/SCALab/projects/multi_gabor_discr/data/")


# read data
data_preprocessed <- read_excel("preprocessed_multi_gabor_staircase.xlsx")

# set size 1
setsize1 <- subset(data_preprocessed, trials.label == "setsize1")

# other setsize

data_preprocessed_exc_1 <- subset(data_preprocessed, trials.label != "setsize1")


# all conditions
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

# set size 1
data_by_subject_s1 <- setsize1 %>%
  group_by(participant,
           trials.setsize) %>%
  summarise(
    threshold_mean = mean(trials.intensity),
    threshold_std = sd(trials.intensity),
    n = n() 
  ) %>%
  mutate(
    threshold_SEM = threshold_std / sqrt(n),
    threshold_CI = threshold_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


# all conditions box plot compare ladder and snake
bxp <- ggboxplot(
  data_by_subject, x = "trials.setsize", y = "threshold_mean", color ="s_l",
  facet.by = "r_t", add = c("dotplot", "jitter")
)
bxp


# ggsave(file = "test.svg", plot = bxp_b, width = 7.58, height = , units = "in")

# all conditions box plot compare radial and tangential
bxp_b <- ggboxplot(
  data_by_subject, x = "trials.setsize", y = "threshold_mean", color ="r_t",
  facet.by = "s_l", add = c("dotplot", "jitter")
)
bxp_b


# set size 1
bxp_s1 <-ggboxplot(
  data_by_subject_s1, x = "trials.setsize", y = "threshold_mean", color = "trials.label",
  add = c("dotplot", "jitter")
)
bxp_s1



# simulate power

str(data_preprocessed)
data_preprocessed$s_l <- as.factor(data_preprocessed$s_l)
data_preprocessed$trials.setsize <- as.factor(data_preprocessed$trials.setsize)
data_preprocessed$r_t <- as.factor(data_preprocessed$r_t)


model <- lmer(trials.intensity ~ s_l * r_t * trials.setsize + 
                (1 + r_t*s_l| participant),
              data = data_preprocessed)

summary(model)
anova(model)


model2 <- lmer(trials.intensity ~ s_l * r_t * trials.setsize + 
                (1| participant),
              data = data_preprocessed_exc_1)

summary(model2)

power <- mixedpower(model = model, data = data_preprocessed,
                    fixed_effects = c("s_l", "r_t", "trials.setsize"),
                    simvar = "participant", steps = c(10, 20, 30, 40, 50),
                    critical_value = 2)


# Visualization------------------------------------------------------

# TODO
dv <- "trials.intensity"


# subject
bxp <- ggboxplot(data = data_preprocessed,
                 x = "participant",
                 y = dv,
                 color = "s_l") +
  facet_wrap( ~ r_t * trials.setsize, nrow = 2, scale = "free_x")

print(bxp)


bxp2 <- ggboxplot(data = data_preprocessed,
                 x = "participant",
                 y = dv,
                 color = "r_t") +
  facet_wrap( ~ s_l, nrow = 2, scale = "free_x")

print(bxp)
