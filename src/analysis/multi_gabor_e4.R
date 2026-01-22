# libraires ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(svglite)
library(ggpubr)
library(car)


# functions -------------

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


# set working path
getwd()
setwd("d:/OneDrive/projects/multi_gabor_discr/src/analysis/")

# ---------------EXP 4 adjust orientation-------------------------

# read data
data_exp4 <- read_csv("../../data/gabor_adjst_ori_alldata.csv")

colnames(data_exp4)

# check participants info
df_check_participants <- data_exp4 %>% 
  group_by(participant, age, sex) %>% 
  tally()

# mean age == 20.0 years
mean(df_check_participants$age)

selected_data <- data_exp4 %>%
  dplyr::select(label, ori, abs_ori, participant, inner_resp, midd_resp, outer_resp)


# -------------error types---------------
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
      label == "setsize1_v" & gabor_location == "inner_resp" ~ "SingleVertical",
      label == "setsize1_h" & gabor_location == "inner_resp" ~ "SingleHorizontal",
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
                              "SingleVertical" = "SV",
                              "SingleHorizontal" = "SH")) + 
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

# ggsave(file = "plot.svg", plot = plot,  width = 13, height = 7.5, units = "in")


# plot_mean_adj_error <- ggplot() +
#   geom_point(
#     data = ave_adj_error,
#     aes(
#       x = gabor_location,
#       y = adj_error_mean,
#       group = label,
#       color = label,
#       size = 0.4
#     ),
#     position = position_dodge(0.8),
#     stat = "identity",
#     alpha = 0.8,
#     show.legend = FALSE) +
#   
#   geom_errorbar(
#     data = ave_adj_error,
#     aes(
#       x = gabor_location,
#       y = adj_error_mean,
#       ymin = adj_error_mean - adj_error_ci,
#       ymax = adj_error_mean + adj_error_ci,
#       group = label,
#       color = label
#     ),
#     
#     size  = 0.8,
#     width = .00,
#     alpha = 0.5,
#     position = position_dodge(0.8)
#   ) +
#   
#   
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#   
#   labs(y = "Adjustment Arror(°)", x = "Gabor Location") +
#   
#   # scale_y_continuous(limits = c(-4, 4)) +
#   # 
#   scale_x_discrete(labels = c("inner_resp" = "Inner", 
#                               "midd_resp" = "Middle",
#                               "outer_resp" = "Outer",
#                               "SingelVertical" = "SV",
#                               "SingerHorizontal" = "SH")) + 
#   
#   scale_color_manual(
#     labels = c("Radial Ladder",  "Radial Sanke"),
#     values = c("#BB5566", "#674EA7"),
#     name = "Gabor type"
#   ) +
#   
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
#   facet_wrap(~ abs_ori, nrow = 1, labeller = labeller(
#     abs_ori =
#       c("2" = "2°",
#         "4" = "4°",
#         "10" = "10°"
#       )
#   ))
# 
# plot_mean_adj_error
# 
# plot_mean_adj_error2 <- ggplot() +
#   geom_point(
#     data = ave_adj_error,
#     aes(
#       x = abs_ori,
#       y = adj_error_mean,
#       group = gabor_location,
#       color = gabor_location,
#       size = 0.4
#     ),
#     position = position_dodge(0.8),
#     stat = "identity",
#     alpha = 0.8,
#     show.legend = FALSE) +
#   
#   geom_errorbar(
#     data = ave_adj_error,
#     aes(
#       x = abs_ori,
#       y = adj_error_mean,
#       ymin = adj_error_mean - adj_error_ci,
#       ymax = adj_error_mean + adj_error_ci,
#       group = gabor_location,
#       color = gabor_location
#     ),
#     
#     size  = 0.8,
#     width = .00,
#     alpha = 0.5,
#     position = position_dodge(0.8)
#   ) +
#   
#   
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#   
#   labs(y = "Adjustment Arror(°)", x = "Orientaion (°)") +
#   
#   scale_y_continuous(limits = c(-7, 6)) +
# 
#   scale_color_manual(
#     labels = c("Inner",  "Middle", "Outer", "Singel Vertical", "Singel Horizontal"),
#     values = c("#2066a8", "#ea801c", "#1f6f6f", "#b8b8b8", "black"),
#     name = "Gabor Location"
#   ) +
# 
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
#   facet_wrap(~ label, nrow = 1, labeller = labeller(
#     label =
#       c("ladder" = "Radial Ladder",
#         "snake" = "Radial Snake")
#   ))
# 
# 
# 
# plot_mean_adj_error2
# 
# # ggsave(file = "plot_mean_adj_error2.svg", plot = plot_mean_adj_error2,  width = 9.4, height = 3.8, units = "in")
# 
# 
# plot_group_variability <- ggplot() +
#   geom_point(
#     data = ave_adj_error,
#     aes(
#       x = gabor_location,
#       y = group_var,
#       group = label,
#       color = label,
#       size = 0.4
#     ),
#     position = position_dodge(0.8),
#     stat = "identity",
#     alpha = 0.8,
#     show.legend = FALSE) +
#   
#   geom_errorbar(
#     data = ave_adj_error,
#     aes(
#       x = gabor_location,
#       y = group_var,
#       ymin = group_var - group_var_ci,
#       ymax = group_var + group_var_ci,
#       group = label,
#       color = label
#     ),
#     
#     size  = 0.8,
#     width = .00,
#     alpha = 0.5,
#     position = position_dodge(0.8)
#   ) +
#   
#   
#   labs(y = "Group Variability", x = "Gabor Location") +
#   
#   # scale_y_continuous(limits = c(2, 12)) +
#   
#   scale_x_discrete(labels = c("inner_resp" = "Inner", 
#                               "midd_resp" = "Middle",
#                               "outer_resp" = "Outer",
#                               "SingelVertical" = "SV",
#                               "SingerHorizontal" = "SH")) + 
#   
#   scale_color_manual(
#     labels = c("Radial Ladder",  "Radial Sanke"),
#     values = c("#BB5566", "#674EA7"),
#     name = "Gabor type"
#   ) +
#   
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
#   facet_wrap(~ abs_ori, nrow = 1, labeller = labeller(
#     abs_ori =
#       c("2" = "2°",
#         "4" = "4°",
#         "10" = "10°"
#       )
#   ))
# 
# plot_group_variability

# ----multinomial logistic regression analysism----

# variables
str(data_exp4_long_format_ss3)

data_exp4_long_format_ss3$abs_ori <-as.factor(data_exp4_long_format_ss3$abs_ori)
data_exp4_long_format_ss3$gabor_location <-as.factor(data_exp4_long_format_ss3$gabor_location)
data_exp4_long_format_ss3$label <-as.factor(data_exp4_long_format_ss3$label)

table(data_exp4_long_format_ss3$abs_ori)
table(data_exp4_long_format_ss3$gabor_location)
table(data_exp4_long_format_ss3$label)

# 
# model <- lme4::lmer(adj_error_shortest_dis ~ 
#                       abs_ori + gabor_location + label + (1|participant), 
#                     data = data_exp4_long_format_ss3)
# 
# model3 <- lme4::lmer(adj_error_shortest_dis ~ 
#                       abs_ori + label + (1|participant), 
#                     data = data_exp4_long_format_ss3)
# 
# model2<- lme4::lmer(adj_error_shortest_dis ~ 
#                       abs_ori * gabor_location * label + (1|participant) , 
#                     data = data_exp4_long_format_ss3, REML = TRUE) # selected model
# 
# 
# 
# anova(model, model3)
# summary(model2)
# 
# # 10 min (Type II Wald F tests with Kenward-Roger df)
# # car::Anova(model2,type="II",test.statistic="F")
# 
# sjPlot::tab_model(
#   model2,
#   p.style = 'scientific_stars',
#   show.se = T,
#   show.stat = T,
#   digits = 3
# ) 
# 
# 
# # pairwise comparisons
# emms <- emmeans::emmeans(
#   model2,
#   list(pairwise ~ gabor_location | abs_ori * label),
#   adjust = "tukey"
# )
# 
# summary(emms, infer = TRUE)
# 
# 
# # plot 3-way interaction
# emmeans_three_way <- emmeans::emmeans(model2, ~ abs_ori * gabor_location * label)
# 
# # pairwise comparisons for all combinations
# pairs(emmeans_three_way)
# # get emmeans for three-way interaction
# plot_data_three_way <- as.data.frame(emmeans_three_way)
# 
# # plot
# ggplot(plot_data_three_way, aes(x = abs_ori, y = emmean, color = gabor_location, group = gabor_location)) +
#   geom_line(size = 1) +
#   geom_point(size = 2) +
#   facet_wrap(~ label) +
#   labs(title = "Three-Way Interaction: abs_ori × gabor_location × label",
#        x = "Orientation Difference (abs_ori)",
#        y = "Estimated Adjustment Error",
#        color = "Gabor Location") +
#   theme_minimal()
# 

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

# exp4: errors are not independent within a trial----------

# read data
# gabor_adjst_ori_alldata.csv
# data_exp4<- read.csv(file.choose())
data_exp4 <- read_csv("../../data/gabor_adjst_ori_alldata.csv")

# for single gabor, middle and outer location fill out
selected_data[is.na(selected_data)] <- 9999

# -----------------correlation analysis---------------------------
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



# plot_correlation <- function(df, cond_name, x_col, y_col, x_label, y_label) {
#   ggplot(df, aes_string(x = x_col, y = y_col)) +
#     geom_point(alpha = 0.2) +
#     geom_smooth(method = "lm", color = "blue", alpha = 0.8) +
#     labs(title = paste0(cond_name, ": ", x_label, " vs. ", y_label),
#          x = x_label,
#          y = y_label) +
#     
#     scale_y_continuous(limits = c(-50, 50)) +
#     
#     scale_x_continuous(limits = c(-50, 50)) +
#     
#     theme(
#       axis.title.x = element_text(
#         color = "black",
#         size = 14,
#         face = "bold"
#       ),
#       axis.title.y = element_text(
#         color = "black",
#         size = 14,
#         face = "bold"
#       ),
#       panel.border = element_blank(),
#       # remove panel grid lines
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       # remove panel background
#       panel.background = element_blank(),
#       # add axis line
#       axis.line = element_line(colour = "grey"),
#       # x,y axis tick labels
#       axis.text.x = element_text(size = 12, face = "bold"),
#       axis.text.y = element_text(size = 12, face = "bold"),
#       # legend size
#       legend.title = element_text(size = 12, face = "bold"),
#       legend.text = element_text(size = 10),
#       # facet wrap title
#       strip.text.x = element_text(size = 12, face = "bold"),
#       panel.spacing = unit(1.0, "lines")
#     )
# }
# 
# # Generate plots for all conditions
# 
# plots <- list(
#   plot_correlation(data_ladder2, "2 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_midd", "Inner", "Middle"),
#   plot_correlation(data_ladder2, "2 Degrees - Ladder", "adj_error_shortest_dis_midd", "adj_error_shortest_dis_outer", "Middle", "Outer"),
#   plot_correlation(data_ladder2, "2 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_outer", "Inner", "Outer"),
#   
#   plot_correlation(data_ladder4, "4 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_midd", "Inner", "Middle"),
#   plot_correlation(data_ladder4, "4 Degrees - Ladder", "adj_error_shortest_dis_midd", "adj_error_shortest_dis_outer", "Middle", "Outer"),
#   plot_correlation(data_ladder4, "4 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_outer", "Inner", "Outer"),
#   
#   plot_correlation(data_ladder10, "10 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_midd", "Inner", "Middle"),
#   plot_correlation(data_ladder10, "10 Degrees - Ladder", "adj_error_shortest_dis_midd", "adj_error_shortest_dis_outer", "Middle", "Outer"),
#   plot_correlation(data_ladder10, "10 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_outer", "Inner", "Outer"),
#   
#   plot_correlation(data_snake2, "2 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_midd", "Inner", "Middle"),
#   plot_correlation(data_snake2, "2 Degrees - Ladder", "adj_error_shortest_dis_midd", "adj_error_shortest_dis_outer", "Middle", "Outer"),
#   plot_correlation(data_snake2, "2 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_outer", "Inner", "Outer"),
#   
#   plot_correlation(data_snake4, "4 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_midd", "Inner", "Middle"),
#   plot_correlation(data_snake4, "4 Degrees - Ladder", "adj_error_shortest_dis_midd", "adj_error_shortest_dis_outer", "Middle", "Outer"),
#   plot_correlation(data_snake4, "4 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_outer", "Inner", "Outer"),
#   
#   plot_correlation(data_snake10, "10 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_midd", "Inner", "Middle"),
#   plot_correlation(data_snake10, "10 Degrees - Ladder", "adj_error_shortest_dis_midd", "adj_error_shortest_dis_outer", "Middle", "Outer"),
#   plot_correlation(data_snake10, "10 Degrees - Ladder", "adj_error_shortest_dis_inner", "adj_error_shortest_dis_outer", "Inner", "Outer")
# )
# 
# # Save plots
# for (i in seq_along(plots)) {
#   ggsave(file = paste0("p", i, ".svg"), plot = plots[[i]], width = 2.5, height = 2.5, units = "in")
# }



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


#  ------------variance for each trial---------------
data_exp4_setsize3$variance_error <- apply(data_exp4_setsize3[, c("adj_error_shortest_dis_inner", 
                                                                  "adj_error_shortest_dis_midd", 
                                                                  "adj_error_shortest_dis_outer")], 
                                           1, var)

data_exp4_setsize3$abs_ori <- as.factor(data_exp4_setsize3$abs_ori)
model_lmm_var <- lme4::lmer(variance_error ~ abs_ori * label + (1 | participant), data = data_exp4_setsize3)
model_lmm_var2 <- lme4::lmer(variance_error ~ abs_ori + label + (1 | participant), data = data_exp4_setsize3)

anova(model_lmm_var, model_lmm_var2)
summary(model_lmm_var)

sjPlot::tab_model(
  model_lmm_var,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3
) 
emmeans::emmeans(model_lmm_var, pairwise ~ label|abs_ori, adjust = "tukey")


# predict by condition
pred_means <- emmeans::emmeans(model_lmm_var, ~ abs_ori * label)
summary(pred_means, infer = c(TRUE, TRUE))
emmeans_data_to_plot <- as.data.frame(pred_means)
str(emmeans_data_to_plot)

data_exp4_by_subject <- data_exp4_setsize3 %>%
  group_by(participant,
           label,
           abs_ori) %>%
  summarise(
    mean_var = mean(variance_error),
    sd_var = sd(variance_error),
    n = n()
  )

data_exp4_across_subject <- data_exp4_by_subject %>%
  group_by(label,
           abs_ori) %>%
  summarise(
    var = mean(mean_var),
    var_std = sd(mean_var),
    n = n()
  ) %>%
  mutate(
    var_sem = var_std/sqrt(n),
    var_ci = var_sem * qt((1 - 0.05) / 2 + .5, n - 1)
  )

plot_var <- ggplot() +
  geom_point(
    data = data_exp4_across_subject,
    aes(
      x = abs_ori,
      y = var,
      group = label,
      color = label
    ),
    size = 3,
    position = position_dodge(0.2),
    stat = "identity",
    alpha = 0.8) +

  geom_errorbar(
    data = data_exp4_across_subject,
    aes(
      x = abs_ori,
      y = var,
      ymin = var - var_sem,
      ymax = var + var_sem,
      group = label,
      color = label
    ),

    size  = 0.8,
    width = .00,
    alpha = 0.5,
    position = position_dodge(0.2)
  ) +

  geom_line(
    data = data_exp4_across_subject,
    aes(
      x = abs_ori,
      y = var,
      group = label,
      color = label
    ),

    alpha = 0.5,
    position = position_dodge(0.2)
  ) +

  labs(y = "Variance", x = "Orientation(°)") +


  scale_color_manual(
    labels = c("setsize3_r_ladder",  "setsize3_r_snake"),
    values = c("#F28522", "#674EA7"),
    name = "Gabor Location"
  ) +

  scale_y_continuous(limits = c(0, 100)) +


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

plot_var
# ggsave(file = "plot_var.svg", plot = plot_var, width = 6.5, height = 4.5, units = "in")


# plot_var_predicted <- ggplot() +
#   geom_point(
#     data = emmeans_data_to_plot,
#     aes(
#       x = abs_ori,
#       y = emmean,
#       group = label,
#       color = label
#     ),
#     size = 3,
#     position = position_dodge(0.2),
#     stat = "identity",
#     alpha = 0.8) +
#   
#   geom_errorbar(
#     data = emmeans_data_to_plot,
#     aes(
#       x = abs_ori,
#       y = emmean,
#       ymin = emmean - SE,
#       ymax = emmean + SE,
#       group = label,
#       color = label
#     ),
#     
#     size  = 0.8,
#     width = .00,
#     alpha = 0.5,
#     position = position_dodge(0.2)
#   ) +
#   
#   geom_line(
#     data = emmeans_data_to_plot,
#     aes(
#       x = abs_ori,
#       y = emmean,
#       group = label,
#       color = label
#     ),
# 
#     alpha = 0.5,
#     position = position_dodge(0.2)
#   ) +
#   
#   labs(y = "Predicted Variance", x = "Orientation(°)") +
#   
#   
#   scale_color_manual(
#     labels = c("setsize3_r_ladder",  "setsize3_r_snake"),
#     values = c("#F28522", "#674EA7"),
#     name = "Gabor Location"
#   ) +
#   
#   scale_y_continuous(limits = c(0, 100)) +
#   
#   
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
#   ) 
# 
# plot_var_predicted

# ggsave(file = "plot_var.svg", plot = plot_var_predicted, width = 6.5, height = 4.5, units = "in")

# # bias (adjsutment error) - against ori
# 
# data_by_subject <- data_exp4_setsize3 %>% 
#   dplyr::group_by(label, ori, participant) %>% 
#   dplyr::summarise(
#     adj_error_inner = mean(adj_error_shortest_dis_inner),
#     n = n()
#   )
# 
# data_acorss_subject <- data_by_subject %>% 
#   dplyr::group_by(label, ori) %>% 
#   dplyr::summarise(
#     inner_bias = mean(adj_error_inner),
#     inner_bias_sd = sd(adj_error_inner),
#     n = n()
#   ) %>% 
#   mutate(
#     adj_error_sem = inner_bias_sd/sqrt(n)
#   )
# 
# plot_bias <- ggplot() +
#   geom_point(
#     data = data_acorss_subject,
#     aes(
#       x = ori,
#       y = inner_bias,
#       group = label,
#       color = label,
#       size = 0.4),
#     position = position_dodge(0.8),
#     stat = "identity",
#     alpha = 0.8) +
#   
#   labs(y = "bias(°)", x = "ori") +
#   
#   scale_x_continuous(breaks = c(-10, -4, -2, 2, 4, 10),
#                      labels = c("-10", "-4", "-2", "2", "4", "10"), limits = c(-11, 11))+
# 
#   
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
#   )  
# 
# plot_bias 


