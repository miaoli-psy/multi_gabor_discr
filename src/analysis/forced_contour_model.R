# Exp 4: forced contour model

library(tidyverse)
library(gghalves)
library(ggplot2)

setwd("d:/OneDrive/projects/multi_gabor_discr/src/analysis/")

PAL <- c(ladder = "#F28522", snake = "#674EA7")


FLIP_ORI_SIGN <- TRUE #TRUE --> clockwise positive
SGN <- if (FLIP_ORI_SIGN) -1 else 1


# helpers

# wrap to [-90, 90): orientation is 180-deg periodic
wrap_deg <- function(x) (x + 90) %% 180 - 90

report_vs0 <- function(x, name, p_adj = NA_real_) {
  x  <- x[!is.na(x)]
  tt <- t.test(x, mu = 0)
  cat(sprintf("%-20s mean=%+.3f  95%% CI [%+.3f, %+.3f]  t(%d)=%.2f  p=%.4f%s  dz=%.2f\n",
              name, mean(x), tt$conf.int[1], tt$conf.int[2],
              tt$parameter, tt$statistic, tt$p.value,
              if (is.na(p_adj)) "" else sprintf("  p_holm=%.4f", p_adj),
              mean(x) / sd(x)))
}

report_paired <- function(x_snake, x_ladder, name) {
  d  <- x_snake - x_ladder
  tt <- t.test(d)
  cat(sprintf("%-20s snake %+.3f  ladder %+.3f  |  diff %+.3f 95%% CI [%+.3f, %+.3f]  t(%d)=%.2f  p=%.4f  dz=%.2f\n",
              name, mean(x_snake, na.rm = TRUE), mean(x_ladder, na.rm = TRUE),
              mean(d, na.rm = TRUE), tt$conf.int[1], tt$conf.int[2],
              tt$parameter, tt$statistic, tt$p.value,
              mean(d, na.rm = TRUE) / sd(d, na.rm = TRUE)))
}

theme_gabor <- function() {
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    axis.text.x  = element_text(size = 14, face = "bold"),
    axis.text.y  = element_text(size = 14, face = "bold"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.0, "lines")
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

    ori        = SGN * ori,
    inner_resp = SGN * inner_resp,
    midd_resp  = SGN * midd_resp,
    outer_resp = SGN * outer_resp,

    s = sign(ori),

    # folded reported orientations, in degrees from the reference
    rep_inner = abs_ori + wrap_deg(s * inner_resp - abs_ori),
    rep_mid   = abs_ori + wrap_deg(s * midd_resp  - abs_ori),
    rep_outer = abs_ori + wrap_deg(s * outer_resp - abs_ori),

    # middle-vs-ends contrast
    k = rep_mid - 0.5 * (rep_inner + rep_outer)
  )



# forced contour slope b

fc_slope <- data %>%
  group_by(participant, arr) %>%
  group_modify(~{
    m <- lm(k ~ abs_ori, data = .x)
    tibble(slope     = coef(m)[["abs_ori"]],
           intercept = coef(m)[["(Intercept)"]])
  }) %>%
  ungroup()

fc_mean_k <- data %>%
  group_by(participant, arr) %>%
  summarise(mean_k = mean(k, na.rm = TRUE), .groups = "drop")

w_slope <- fc_slope %>%
  dplyr::select(participant, arr, slope) %>%
  pivot_wider(names_from = arr, values_from = slope)

w_int <- fc_slope %>%
  dplyr::select(participant, arr, intercept) %>%
  pivot_wider(names_from = arr, values_from = intercept)

w_mean_k <- fc_mean_k %>%
  pivot_wider(names_from = arr, values_from = mean_k)


# ---- results reported in ms

p_slope_adj <- p.adjust(c(t.test(w_slope$snake,  mu = 0)$p.value,
                          t.test(w_slope$ladder, mu = 0)$p.value),
                        method = "holm")

# forced contour slope b
report_vs0(w_slope$snake,  "snake  b vs 0", p_slope_adj[1])
report_vs0(w_slope$ladder, "ladder b vs 0", p_slope_adj[2])
report_paired(w_slope$snake, w_slope$ladder, "b snake vs ladder")


p_mean_k_adj <- p.adjust(c(t.test(w_mean_k$snake,  mu = 0)$p.value,
                           t.test(w_mean_k$ladder, mu = 0)$p.value),
                         method = "holm")

# mean k
report_vs0(w_mean_k$snake,  "snake  mean k vs 0", p_mean_k_adj[1])
report_vs0(w_mean_k$ladder, "ladder mean k vs 0", p_mean_k_adj[2])
report_paired(w_mean_k$snake, w_mean_k$ladder, "mean k snake vs ladder")


#off set intercept k0
report_vs0(w_int$snake,  "snake  k0 vs 0")
report_vs0(w_int$ladder, "ladder k0 vs 0")



# slope-based: b against b = -2

cat(
  sprintf(
    "b as %% of complete reversal    snake %.1f%%  ladder %.1f%%\n",
    100 * mean(w_slope$snake, na.rm = TRUE) / -2,
    100 * mean(w_slope$ladder, na.rm = TRUE) / -2
  )
)

#  mean k against the mean k a complete reversal would give,
full_rev_mean_k <- -2 * mean(data$abs_ori)

# mean k as %% of complete reversal
pct_snake  <- 100 * mean(w_mean_k$snake,  na.rm = TRUE) / full_rev_mean_k
pct_snake
pct_ladder <- 100 * mean(w_mean_k$ladder, na.rm = TRUE) / full_rev_mean_k
pct_ladder


# Fig 4c: contrast k against presented orientation 

k_pp_dir <- data %>%
  mutate(dir = if_else(ori > 0, "CW", "CCW")) %>%
  group_by(participant, arr, abs_ori, dir) %>%
  summarise(k = mean(k, na.rm = TRUE), .groups = "drop")

k_dir <- k_pp_dir %>%
  group_by(arr, abs_ori, dir) %>%
  summarise(
    m  = mean(k, na.rm = TRUE),
    se = sd(k, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(x = abs_ori + if_else(dir == "CW", 0.25, -0.25))

k_ppt <- data %>%
  group_by(participant, arr, abs_ori) %>%
  summarise(m = mean(k, na.rm = TRUE), .groups = "drop")

# the fitted model itself: group-mean slope and intercept per arrangement,
# so the line drawn here has slope b
fit_line <- fc_slope %>%
  group_by(arr) %>%
  summarise(
    b  = mean(slope, na.rm = TRUE),
    k0 = mean(intercept, na.rm = TRUE),
    .groups = "drop"
  )

# Dashed lines are the two hypotheses:
#   economical contour  (theta, -theta, theta)  ->  k = -2|theta|
#   alternative contour (-theta, theta, -theta) ->  k = +2|theta|
# They reach -20 and +20 at |theta| = 10, far beyond the observed effect, so
# they run off the panel. That is deliberate: the panel is scaled to the data,
# and the two dashed lines show the direction and steepness each hypothesis
# predicts.


p_fig4d <- ggplot() +
  geom_hline(yintercept = 0,
             colour = "grey45",
             linewidth = 0.5) +
  geom_abline(
    slope = -2,
    intercept = 0,
    linetype = "dashed",
    colour = "grey30",
    linewidth = 0.5
  ) +
  geom_abline(
    slope =  2,
    intercept = 0,
    linetype = "dashed",
    colour = "grey30",
    linewidth = 0.5
  ) +
  annotate(
    "text",
    x = 1,
    y = -2.6,
    hjust = 0,
    vjust = 1,
    size = 3.6,
    colour = "grey30",
    fontface = "bold",
    label = "economical contour"
  ) +
  annotate(
    "text",
    x = 1,
    y = 2.6,
    hjust = 0,
    vjust = 0,
    size = 3.6,
    colour = "grey30",
    fontface = "bold",
    label = "alternative contour"
  ) +
  geom_line(
    data = k_ppt,
    aes(
      abs_ori,
      m,
      group = interaction(participant, arr),
      colour = arr
    ),
    alpha = 0.15,
    linewidth = 0.4
  ) +
  geom_abline(data = fit_line,
              aes(
                slope = b,
                intercept = k0,
                colour = arr
              ),
              linewidth = 1.1) +
  geom_errorbar(
    data = k_dir,
    aes(
      x = x,
      ymin = m - se,
      ymax = m + se,
      colour = arr
    ),
    width = 0.35,
    linewidth = 0.7
  ) +
  geom_point(
    data = k_dir,
    aes(
      x = x,
      y = m,
      colour = arr,
      shape = dir
    ),
    size = 2.6,
    fill = "white",
    stroke = 0.9
  ) +
  scale_colour_manual(values = PAL, labels = c("Ladder", "Snake")) +
  scale_shape_manual(
    values = c(CW = 16, CCW = 21),
    labels = c(CW = "clockwise", CCW = "counter-clockwise")
  ) +
  scale_x_continuous(breaks = c(0, 2, 4, 10)) +
  coord_cartesian(xlim = c(0.1, 10.1), ylim = c(-3, 3)) +
  labs(x = "Presented orientation |\u03b8| (deg)", y = "k (°)") +
  theme_gabor() +
  theme(legend.position = "top", legend.box = "horizontal")

p_fig4d


#Fig4c: b for ladders and snakes

slope_across_pp <- fc_slope %>%
  group_by(arr) %>%
  summarise(avg_slope = mean(slope, na.rm = TRUE), .groups = "drop")

p_fig4c <- ggplot(fc_slope, aes(x = arr, y = slope, fill = arr)) +
  geom_hline(
    yintercept = 0,
    colour = "grey80",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_half_violin(
    side = "l",
    alpha = 0.3,
    width = 1,
    color = "white",
    trim = FALSE
  ) +
  geom_line(aes(group = participant),
            color = "grey",
            linewidth = 0.4) +
  geom_point(aes(color = arr), size = 2.2, alpha = 0.5) +
  geom_point(
    data = slope_across_pp,
    aes(x = arr, y = avg_slope, color = arr),
    alpha = 0.8,
    size = 5
  ) +
  scale_fill_manual(values  = PAL) +
  scale_color_manual(values = PAL) +
  scale_x_discrete(labels = c(ladder = "Ladder", snake = "Snake")) +
  labs(x = NULL, y = "Forced contour slope b") +
  theme_gabor() +
  theme(legend.position = "none")

p_fig4c


# 4e: mean contrast k, per participant 


mean_k_across_pp <- fc_mean_k %>%
  group_by(arr) %>%
  summarise(avg = mean(mean_k, na.rm = TRUE), .groups = "drop")

p_fig4e <- ggplot(fc_mean_k, aes(x = arr, y = mean_k, fill = arr)) +
  geom_hline(
    yintercept = 0,
    colour = "grey80",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_half_violin(
    side = "l",
    alpha = 0.3,
    width = 1,
    color = "white",
    trim = FALSE
  ) +
  geom_line(aes(group = participant),
            color = "grey",
            linewidth = 0.4) +
  geom_point(aes(color = arr), size = 2.2, alpha = 0.5) +
  geom_point(
    data = mean_k_across_pp,
    aes(x = arr, y = avg, color = arr),
    alpha = 0.8,
    size = 5
  ) +
  scale_fill_manual(values  = PAL) +
  scale_color_manual(values = PAL) +
  scale_x_discrete(labels = c(ladder = "Ladder", snake = "Snake")) +
  labs(x = NULL, y = expression("Mean contrast k (°)")) +
  theme_gabor() +
  theme(legend.position = "none")

p_fig4e



# ggsave("fig4d.svg", p_fig4d, width = 4.5, height = 4, units = "in")
# ggsave("fig4c.svg", p_fig4c, width = 3,   height = 4, units = "in")
# ggsave("fig4e.svg", p_fig4e, width = 3,   height = 4, units = "in")
# 
