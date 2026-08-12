library(tidyverse)
library(gghalves)
library(svglite)
library(lme4)
library(emmeans)


setwd("d:/OneDrive/projects/multi_gabor_discr/src/analysis/")

PAL         <- c(ladder = "#F28522", snake = "#674EA7")
PAIR_LEVELS <- c("inner-middle", "middle-outer", "inner-outer")
ORI_LEVELS  <- c(2, 4, 10)


# helpers 

# shortest signed angular difference [-90, 90)
angle_dist <- function(a, b) (a - b + 90) %% 180 - 90


# cannot average correlation direction, so apply fisher transform
fisher_z <- function(r) atanh(pmin(pmax(r, -0.9999), 0.9999))
fisher_r <- function(z) tanh(z)

# cor() that returns NA instead of erroring on zero-variance input
safe_cor <- function(x, y, method) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 3) return(NA_real_)
  if (sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(cor(x[ok], y[ok], method = method))
}

theme_gabor <- function() {
  theme(
    axis.title.x     = element_text(colour = "black", size = 14, face = "bold"),
    axis.title.y     = element_text(colour = "black", size = 14, face = "bold"),
    axis.text.x      = element_text(size = 12, face = "bold"),
    axis.text.y      = element_text(size = 12, face = "bold"),
    panel.border     = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line        = element_line(colour = "grey"),
    legend.title     = element_blank(),
    legend.text      = element_text(size = 12, face = "bold"),
    legend.position  = "top",
    strip.text       = element_text(size = 12, face = "bold"),
    strip.background = element_blank(),
    panel.spacing    = unit(1.0, "lines")
  )
}

# data

data_exp4 <- read_csv("../../data/gabor_adjst_ori_alldata.csv")


data <- data_exp4 %>%
  dplyr::select(participant, label, ori, abs_ori,
                inner_resp, midd_resp, outer_resp) %>%
  filter(label %in% c("setsize3_r_ladder", "setsize3_r_snake")) %>%
  drop_na(inner_resp, midd_resp, outer_resp) %>%
  mutate(
    arr = if_else(str_detect(label, "snake"), "snake", "ladder"),
    s   = if_else(ori < 0, -1, 1),                 # fold CCW onto CW axis
    err_inner = angle_dist(s * inner_resp, abs_ori),
    err_mid   = angle_dist(s * midd_resp,  abs_ori),
    err_outer = angle_dist(s * outer_resp, abs_ori)
  ) %>%
  mutate(
    arr     = factor(arr, levels = c("ladder", "snake")),
    abs_ori = factor(abs_ori, levels = ORI_LEVELS)
  )


# participant-level correlations

compute_r_pp <- function(d, method = "pearson") {
  d %>%
    group_by(arr, abs_ori, participant) %>%
    summarise(
      n_trials       = n(),
      `inner-middle` = safe_cor(err_inner, err_mid,   method),
      `middle-outer` = safe_cor(err_mid,   err_outer, method),
      `inner-outer`  = safe_cor(err_inner, err_outer, method),
      .groups = "drop"
    ) %>%
    pivot_longer(all_of(PAIR_LEVELS), names_to = "pair", values_to = "r") %>%
    mutate(pair = factor(pair, levels = PAIR_LEVELS),
           z    = fisher_z(r))
}

summarise_r_pp <- function(r_pp) {
  r_pp %>%
    group_by(arr, abs_ori, pair) %>%
    summarise(
      n_pp   = sum(!is.na(z)),
      z_mean = mean(z, na.rm = TRUE),
      z_se   = sd(z, na.rm = TRUE) / sqrt(n_pp),
      .groups = "drop"
    ) %>%
    mutate(
      r_mean = fisher_r(z_mean),
      ci_lo  = fisher_r(z_mean - qt(.975, n_pp - 1) * z_se),
      ci_hi  = fisher_r(z_mean + qt(.975, n_pp - 1) * z_se)
    )
}

# r

r_pp<- compute_r_pp(data, "pearson")
r_pp
r_summary <- summarise_r_pp(r_pp)
r_summary

# r per arrangement * orientation.

r_report <- r_summary %>%
        group_by(arr, abs_ori) %>%
        summarise(r_min = round(min(r_mean), 2),
                  r_max = round(max(r_mean), 2), .groups = "drop")
r_report


# ladder vs. snake paired t

paired_test <- function(d) {
  w <- d %>%
    dplyr::select(participant, arr, z) %>%
    pivot_wider(names_from = arr, values_from = z) %>%
    filter(!is.na(ladder), !is.na(snake))
  if (nrow(w) < 3) return(tibble())
  tt <- t.test(w$ladder, w$snake, paired = TRUE)
  dd <- w$ladder - w$snake
  tibble(n = nrow(w),
         z_ladder = mean(w$ladder), z_snake = mean(w$snake),
         r_ladder = fisher_r(mean(w$ladder)), r_snake = fisher_r(mean(w$snake)),
         t = unname(tt$statistic), df = unname(tt$parameter),
         p = tt$p.value, dz = mean(dd) / sd(dd))
}

# by orientation x pair
tests_by_ori <- r_pp %>%
  group_by(abs_ori, pair) %>%
  group_modify(~ paired_test(.x)) %>%
  ungroup() %>%
  mutate(p_holm = p.adjust(p, method = "holm"))

#  collapsed across orientation (average z within participant first)
tests_collapsed <- r_pp %>%
  group_by(participant, arr, pair) %>%
  summarise(z = mean(z, na.rm = TRUE), .groups = "drop") %>%
  group_by(pair) %>%
  group_modify(~ paired_test(.x)) %>%
  ungroup() %>%
  mutate(p_holm = p.adjust(p, method = "holm"))


# per-participant r, paired across arrangements 

r_mean_pts <- r_pp %>%
  group_by(arr, abs_ori, pair) %>%
  summarise(r = fisher_r(mean(z, na.rm = TRUE)), .groups = "drop")

p_r_pp <- ggplot(r_pp, 
                 aes(x = arr, 
                     y = r, 
                     fill = arr)) +
  geom_hline(yintercept = 0, 
             colour = "grey80",
             linewidth = 0.4, 
             linetype = "dashed") +
  geom_half_violin(side = "l",
                   alpha = 0.3,
                   width = 1,
                   colour = "white", 
                   trim = FALSE) +
  geom_line(aes(group = participant),
            colour = "grey", 
            linewidth = 0.3) +
  geom_point(aes(colour = arr),
             size = 1.8, 
             alpha = 0.5) +
  geom_point(data = r_mean_pts, 
             aes(x = arr, 
                 y = r, 
                 colour = arr), 
             size = 4) +
  facet_grid(pair ~ abs_ori,
             labeller = labeller(abs_ori = function(v) paste0(v, "\u00b0"))) +
  
  scale_fill_manual(values = PAL) +
  
  scale_colour_manual(values = PAL) +
  
  scale_x_discrete(labels = c(ladder = "Ladder", snake = "Snake")) +
  
  coord_cartesian(ylim = c(-0.5, 1)) +
  
  labs(x = NULL, y = "Correlation of adjustment errors (r)") +
  
  theme_gabor() +
  
  theme(legend.position = "none")

p_r_pp


r_pp_collapsed <- r_pp %>%
  group_by(participant, arr, pair) %>%
  summarise(z = mean(z, na.rm = TRUE), .groups = "drop") %>%
  mutate(r = fisher_r(z))

r_mean_collapsed <- r_pp_collapsed %>%
  group_by(arr, pair) %>%
  summarise(r = fisher_r(mean(z, na.rm = TRUE)), .groups = "drop")
r_mean_collapsed

p_fig3a <- ggplot(r_pp_collapsed, 
                  aes(x = arr, 
                      y = r, 
                      fill = arr)) +
  geom_hline(yintercept = 0,
             colour = "grey80",
             linewidth = 0.4, 
             linetype = "dashed") +
  geom_half_violin(side = "l", 
                   alpha = 0.3,
                   width = 1,
                   colour = "white", 
                   trim = FALSE) +
  geom_line(aes(group = participant),
            colour = "grey",
            linewidth = 0.4) +
  geom_point(aes(colour = arr), 
             size = 2.2, 
             alpha = 0.5) +
  geom_point(data = r_mean_collapsed, 
             aes(x = arr, 
                 y = r, 
                 colour = arr),
             size = 5) +
  facet_wrap(~ pair, 
             nrow = 1) +
  scale_fill_manual(values = PAL) +
  
  scale_colour_manual(values = PAL) +
  
  scale_x_discrete(labels = c(ladder = "Ladder", snake = "Snake")) +
  
  coord_cartesian(ylim = c(-0.1, 1)) +
  
  labs(x = NULL, y = "Correlation of adjustment errors (r)") +
  
  theme_gabor() +
  
  theme(legend.position = "none")

p_fig3a

# ggsave("p_r_pp.svg",p_r_pp, width = 7, height = 7, units = "in")

