library(tidyverse)
library(lme4)
library(ggplot2)

setwd("d:/OneDrive/projects/multi_gabor_discr/src/analysis/")

# read data
# gabor_adjst_ori_alldata.csv
data_exp4<- read.csv(file.choose())

data <- data_exp4 %>%
  dplyr::select(label, ori, participant, inner_resp, midd_resp, outer_resp)

data <- data %>% 
  filter(label %in% c("setsize3_r_ladder", "setsize3_r_snake"))


# bias (our deviation score)
data$bias_inner <- data$inner_resp - data$ori
data$bias_middle <- data$midd_resp - data$ori
data$bias_outer <- data$outer_resp - data$ori

# modal 1: local contextural difference d_adj

data$d_adj_inner <- ((data$midd_resp + data$outer_resp) / 2) - data$ori
data$d_adj_middle <- ((data$inner_resp + data$outer_resp) / 2) - data$ori
data$d_adj_outer <- ((data$inner_resp + data$midd_resp) / 2) - data$ori

# model 2: Global averaging d_group

data$d_group_inner <- ((data$inner_resp + data$midd_resp + data$outer_resp) / 3) - data$ori
data$d_group_middle <- data$d_group_inner  # same for all positions
data$d_group_outer <- data$d_group_inner

data_long <- data %>%
  pivot_longer(
    cols = c(bias_inner, bias_middle, bias_outer,
             d_adj_inner, d_adj_middle, d_adj_outer,
             d_group_inner, d_group_middle, d_group_outer),
    names_to = c(".value", "location"),
    names_pattern = "(bias|d_adj|d_group)_(inner|middle|outer)"
  )

# For each location (e.g., inner), we compared the two models using likelihood ratio 
# tests and computed the estimated weights: w = beta/2, This allowed us to quantify 
# the relative contribution of contextual signals under different assumptions and 
# evaluate whether perceptual integration is better described by local neighborhood 
# influence or global averaging.

data_to_compare <- filter(data_long, location == "middle")

data_ladder <- filter(data_to_compare, label == "setsize3_r_ladder")
data_sanke <- filter(data_to_compare, label == "setsize3_r_snake")

model_local_ladder <- lmer(bias ~ 0 + d_adj + (1 | participant), data = data_ladder)
model_local_snake <- lmer(bias ~ 0 + d_adj + (1 | participant), data = data_sanke)
model_global_ladder <- lmer(bias ~ 0 + d_group + (1 | participant), data = data_ladder)
model_global_snake <- lmer(bias ~ 0 + d_group + (1 | participant), data = data_sanke)

# ladder condition
anova(model_local_ladder, model_global_ladder)

w_local_ladder <- fixef(model_local_ladder)[1] / 2
w_local_ladder
w_global_ladder <- fixef(model_global_ladder)[1] / 2
w_global_ladder
# snake 
anova(model_local_snake, model_global_snake)
w_local_snake <- fixef(model_local_snake)[1] / 2
w_local_snake
w_global_snake <- fixef(model_global_snake)[1] / 2
w_global_snake


# This suggests that participants’ judgments for the inner Gabor were more 
# strongly influenced by the perceived global average of all three Gabors 
# than by just the local neighbors.




data_to_plot <- data_long %>% 
  group_by(participant,location,d_adj,label) %>% 
  summarise(
    bias = mean(bias),
    n = n()
  )

data_to_plot_inner <- data_to_plot %>% 
  filter(location == "inner")

data_to_plot_inner <- data_to_plot_inner %>% 
  group_by(d_adj, label) %>% 
  summarise(
    mean_bias = mean(bias),
    sd = sd(bias),
    n = n()
  ) %>% 
  mutate(
    sem = sd/sqrt(n),
    ci = sem * qt((1 - 0.05) / 2 + .5, n - 1)
  )


beta_local <- fixef(model_local)["d_adj"]       
beta_global <- fixef(model_global)["d_group"]   

# Range of d values
d_vals <- seq(-60, 60, by = 0.1)

# Predicted bias = slope * d
pred_df <- data.frame(
  d = d_vals,
  bias_local = beta_local * d_vals,
  bias_global = beta_global * d_vals
)


plot_bias <- ggplot() +
  geom_point(data = data_to_plot_inner,
             aes(
               x = d_adj,
               y = mean_bias,
               group = label,
               color = label
             ),
             position = position_dodge(0.8),
             stat = "identity",
             alpha = 0.5) +
  
  geom_line(data = data_to_plot_inner,
             aes(
               x = d_adj,
               y = mean_bias,
               group = label,
               color = label
             ),
             position = position_dodge(0.8),
             stat = "identity",
             alpha = 0.5) +
  
  geom_line(data = pred_df,
            aes(
              x = d,
              y = bias_local, 
              color = "Local Model"), 
            size = 1.2) +
  geom_line(data = pred_df,
            aes(
              x = d,
              y = bias_global,
              color = "Global Model"
            ))
  

plot_bias







