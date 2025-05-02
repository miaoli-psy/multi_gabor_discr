library(tidyverse)
library(lme4)
library(ggplot2)
library(minpack.lm)
library(purrr)

setwd("d:/OneDrive/projects/multi_gabor_discr/src/analysis/")

# read data
# gabor_adjst_ori_alldata.csv
data_exp4<- read.csv(file.choose())

data <- data_exp4 %>%
  dplyr::select(label, ori, participant, inner_resp, midd_resp, outer_resp)

data <- data %>% 
  filter(label %in% c("setsize3_r_ladder", "setsize3_r_snake"))


# # bias (our deviation score)
# data$bias_inner <- data$inner_resp - data$ori
# data$bias_middle <- data$midd_resp - data$ori
# data$bias_outer <- data$outer_resp - data$ori

# 为每个 trial 拟合一条二次曲线， 提取曲率参数a - 作为感知结构弯曲程度的测量

# 假设三个位置是 x = -1, 0, 1（inner, middle, outer）
fit_poly <- function(inner, middle, outer) {
  x <- c(-1, 0, 1)
  y <- c(inner, middle, outer)
  model <- lm(y ~ poly(x, 2, raw = TRUE))  # second order poly
  a <- coef(model)[2] # quadratic coef a
  b <- coef(model)[3] 
  return(c(a = a, b = b))
}

poly_params <- pmap_dfr(
  .l = list(inner = data$inner_resp,
            middle = data$midd_resp,
            outer = data$outer_resp),
  .f = fit_poly
)

# rename
names(poly_params) <- c("a", "b")

# combine col a and b to data
data <- bind_cols(data, poly_params)

# # higher score means close to straight
# data$structure_score <- -abs(data$a)
# 
# 
# model_structure <- lm(structure_score ~ label, data = data)
# summary(model_structure)


# # pre-define a0 和 b0
# a0 <- 1
# b0 <- 5
# 
# # calcualte weight for each trial （ inner, outer）
# data <- data %>%
#   mutate(
#     w_inner = a0 / (b0 + 2 * a^2),
#     w_outer = a0 / (b0 + 2 * a^2),
#     w_ori = 1 - (w_inner + w_outer)
#   )
# 
# # predicted middle
# data$predicted_middle_weighted <- with(data, w_inner * inner_resp + w_outer * outer_resp + w_ori * ori)
# 
# # plot predicted vs. actual middle response
# ggplot(data, aes(x = predicted_middle_weighted, y = midd_resp, color = label)) +
#   geom_point(alpha = 0.3) +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(title = "Weighted Prediction vs. Actual midd_resp",
#        x = "Predicted Middle (Weighted)",
#        y = "Actual Middle Adjustment") +
#   theme_minimal()


# predictions 

# use inner, outer, and treu ori to predict middle
predict_weighted_middle <- function(par, data) {
  a0 <- par[1]
  b0 <- par[2]
  w <- a0 / (b0 + 2 * data$a^2)
  w_ori <- 1 - 2 * w
  pred <- w * data$inner_resp + w * data$outer_resp + w_ori * data$ori
  return(pred)
}

# predict inner
predict_weighted_inner <- function(par, data) {
  a0 <- par[1]
  b0 <- par[2]
  w <- a0 / (b0 + 2 * data$a^2)
  w_ori <- 1 - 2 * w
  pred <- w * data$midd_resp + w * data$outer_resp + w_ori * data$ori
  return(pred)
}

# predict outer
predict_weighted_outer <- function(par, data) {
  a0 <- par[1]
  b0 <- par[2]
  w <- a0 / (b0 + 2 * data$a^2)
  w_ori <- 1 - 2 * w
  pred <- w * data$midd_resp + w * data$inner_resp + w_ori * data$ori
  return(pred)
}



# loss function, get residual for nlsLM:
# nlsLM performs nonlinear least squares fitting, loss fucntion tells nlsLM
# how far each prediction is from the actual res - to minimize the error
loss_function <- function(par, data) {
  pred <- predict_weighted_middle(par, data)
  return(pred - data$midd_resp)  
}


loss_inner <- function(par, data) {
  pred <- predict_weighted_inner(par, data)
  return(pred - data$inner_resp)  
}


loss_outer <- function(par, data) {
  pred <- predict_weighted_outer(par, data)
  return(pred - data$outer_resp)  
}

# model here: predict middle

data_ladder <- filter(data, label == "setsize3_r_ladder")

fit_ladder <- nlsLM(
  midd_resp ~ predict_weighted_middle(c(a0, b0), data_ladder),
  data = data_ladder,
  start = list(a0 = 1, b0 = 5),
  lower = c(0.0001, 0.0001),
  control = nls.lm.control(maxiter = 500)
)

data_snake <- filter(data, label == "setsize3_r_snake")

fit_snake <- nlsLM(
  midd_resp ~ predict_weighted_middle(c(a0, b0), data_snake),
  data = data_ladder,
  start = list(a0 = 1, b0 = 5),
  lower = c(0.0001, 0.0001),
  control = nls.lm.control(maxiter = 500)
)

# model: predict inner

#  ladder 
fit_inner_ladder <- nlsLM(
  inner_resp ~ predict_weighted_inner(c(a0, b0), data_ladder),
  data = data_ladder,
  start = list(a0 = 1, b0 = 5),
  lower = c(0.0001, 0.0001),
  control = nls.lm.control(maxiter = 500)
)

#  snake
fit_inner_snake <- nlsLM(
  inner_resp ~ predict_weighted_inner(c(a0, b0), data_snake),
  data = data_snake,
  start = list(a0 = 1, b0 = 5),
  lower = c(0.0001, 0.0001),
  control = nls.lm.control(maxiter = 500)
)


# model: predict outer

#  ladder 
fit_outer_ladder <- nlsLM(
  outer_resp ~ predict_weighted_outer(c(a0, b0), data_ladder),
  data = data_ladder,
  start = list(a0 = 1, b0 = 5),
  lower = c(0.0001, 0.0001),
  control = nls.lm.control(maxiter = 500)
)

#  snake
fit_outer_snake <- nlsLM(
  outer_resp ~ predict_weighted_outer(c(a0, b0), data_snake),
  data = data_snake,
  start = list(a0 = 1, b0 = 5),
  lower = c(0.0001, 0.0001),
  control = nls.lm.control(maxiter = 500)
)


# middle results
summary(fit_ladder)
summary(fit_snake)

coef(fit_ladder)
coef(fit_snake)


#inner results
summary(fit_inner_ladder)
summary(fit_inner_snake)

coef(fit_inner_ladder)
coef(fit_inner_snake)

#outer results
summary(fit_outer_ladder)
summary(fit_outer_snake)

coef(fit_outer_ladder)
coef(fit_outer_snake)


#plot

# a0 and b0
params <- list(
  middle_ladder = c(a0 = 5.617, b0 = 11.587),
  middle_snake  = c(a0 = 140.38, b0 = 583.05),
  inner_ladder  = c(a0 = 6.9319, b0 = 16.7003),
  inner_snake   = c(a0 = 6.3652, b0 = 14.9676),
  outer_ladder  = c(a0 = 6.8234, b0 = 16.2610),
  outer_snake   = c(a0 = 8.869,  b0 = 20.751)
)

# curvature values
a_vals <- seq(-5, 5, length.out = 200)

# compute weights
compute_weights <- function(a_vals, a0, b0) {
  w <- a0 / (b0 + 2 * a_vals^2)
  w_ori <- 1 - 2 * w
  data.frame(a = a_vals, w, w_ori)
}

# combined dataframe
plot_data <- bind_rows(lapply(names(params), function(name) {
  p <- params[[name]]
  tmp <- compute_weights(a_vals, p["a0"], p["b0"])
  tmp$position <- gsub("_.*", "", name)
  tmp$condition <- gsub(".*_", "", name)
  tmp
}))

# reshape for plotting
plot_long <- pivot_longer(plot_data, 
                          cols = c("w", "w_ori"),
                          names_to = "source", 
                          values_to = "weight") %>%
  mutate(
    linetype = ifelse(source == "w", "Perceived Ori from other Gabors", "True Ori"),
    color = ifelse(condition == "ladder", "Ladder", "Snake")
    )

# plot with facets

plot <- ggplot() +
  geom_line(
    data = plot_long,
    aes(
      x = a,
      y = weight,
      color = color,
      linetype = linetype

    ),
    
    size = 1
  ) +
  
  
  scale_color_manual(
    labels = c("Ladder", "Snake"),
    values = c("#F28522", "#674EA7"),
    name = "Arrangement"
  ) +
  
  scale_y_continuous(limits = c(0, 1)) +
  
  
  scale_x_continuous(limits = c(-5, 5))+
  
  labs(y = "Weight", 
       x = "Curvature (a)",
       linetype = "Source") +
  
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

  facet_wrap(~ position, nrow = 1, scales = "fixed",
             labeller = labeller(
               position = 
                 c("inner" = "Predicted Inner",
                   "middle" = "Predicted Middle",
                   "outer" = "Predicted Outer")
             )
             ) 
  
plot




