library(tidyverse)
library(gghalves)
library(ggplot2)

setwd("d:/OneDrive/projects/multi_gabor_discr/src/analysis/")

# ---- data preparation--------------------

data_exp4 <- read_csv("../../data/gabor_adjst_ori_alldata.csv")

data <- data_exp4 %>%
  dplyr::select(label, ori, participant, inner_resp, midd_resp, outer_resp) %>%
  filter(label %in% c("setsize3_r_ladder", "setsize3_r_snake")) %>%
  mutate(
    arr       = if_else(str_detect(label, "snake"), "snake", "ladder"),
    abs_ori   = abs(ori),
    err_inner = inner_resp - ori,
    err_mid   = midd_resp  - ori,
    err_outer = outer_resp - ori
  )

# ---- by orientation or not 2. -----------------------------

FIT_BY_ORIENTATION <- FALSE

group_vars <- c("participant", "arr")
if (FIT_BY_ORIENTATION) {
  group_vars <- c(group_vars, "abs_ori")
}

pal <- c(ladder = "#F28522", snake = "#674EA7")

# ---- Forced-contour model (the mean, sign-folded (1,-2,1) kink) ----------
# per trial:  k = middle error - mean(ends error)   [the (1,-2,1) contrast]
#             ksf = sign(ori) * k                    [fold CW/CCW onto one axis]
# hypothesis: perceived (+θ,-θ,+θ)  ->  k = -2θ  ->  ksf < 0 for snakes, ~0 for ladders

trial_kink <- data %>%
  mutate(k   = err_mid - 0.5 * (err_inner + err_outer),
         ksf = sign(ori) * k) # because the Gabors can tilted cw or cww, ksf makes
#all oris could be averaged, a negative ksf means the middle Gabor was displaces
#oppositve to the global stimulus ori.

# sign-folded kink per group
fc_kink <- trial_kink %>%
  group_by(across(all_of(group_vars))) %>%
  summarise(n_trials = n(),
            kink     = mean(ksf, na.rm = TRUE),      # < 0 = forced contour
            se       = sd(ksf, na.rm = TRUE) / sqrt(n()),
            .groups  = "drop")

# slope form: k = b * ori  (uses all orientations; full flip -> b = -2)
fc_slope <- trial_kink %>%
  group_by(participant, arr) %>%
  group_modify(~{
    m <- tryCatch(lm(k ~ ori, data = .x), error = function(e) NULL)
    if (is.null(m)) tibble(slope = NA_real_, intercept = NA_real_)
    else tibble(slope = coef(m)[["ori"]], intercept = coef(m)[["(Intercept)"]])
  }) %>% ungroup()


# ---- tests (snake vs ladder) --------------------------------------------
report <- function(x_snake, x_ladder, name) {
  d  <- x_snake - x_ladder
  tt <- t.test(d)                               # paired difference vs 0
  cat(sprintf("%-18s snake %+.3f  ladder %+.3f  |  diff %+.3f 95%% CI [%+.3f, %+.3f]  t(%d)=%.2f  p=%.4f  dz=%.2f\n",
              name,
              mean(x_snake,  na.rm = TRUE),
              mean(x_ladder, na.rm = TRUE),
              mean(d, na.rm = TRUE),
              tt$conf.int[1], tt$conf.int[2],
              tt$parameter, tt$statistic, tt$p.value,
              mean(d, na.rm = TRUE) / sd(d, na.rm = TRUE)))
}
w_slope <- fc_slope %>%
  select(participant, arr, slope) %>%
  pivot_wider(names_from = arr, 
              values_from = slope)

w_kink  <- trial_kink %>% 
  group_by(participant, arr) %>%
  summarise(kink = mean(ksf, na.rm = TRUE), 
            .groups = "drop") %>%
  pivot_wider(names_from = arr,
              values_from = kink)


report(w_slope$snake, w_slope$ladder, "contour slope")   # headline: snake < ladder, < 0
report(w_kink$snake,  w_kink$ladder,  "sign-folded kink") # snake < ladder, < 0

# slope against 0
p_snake_raw  <- t.test(w_slope$snake, mu = 0)$p.value
p_ladder_raw <- t.test(w_slope$ladder, mu = 0)$p.value

p_adj <- p.adjust(c(p_snake_raw, p_ladder_raw), method = "holm")

dz_snake  <- mean(w_slope$snake, na.rm = TRUE) / sd(w_slope$snake, na.rm = TRUE)
dz_ladder <- mean(w_slope$ladder, na.rm = TRUE) / sd(w_slope$ladder, na.rm = TRUE)

cat(sprintf("snake slope vs 0: mean=%.3f  p_adj=%.4f  dz=%.2f\n",
            mean(w_slope$snake, na.rm = TRUE), p_adj[1], dz_snake))

cat(sprintf("ladder slope vs 0: mean=%.3f  p_adj=%.4f  dz=%.2f\n",
            mean(w_slope$ladder, na.rm = TRUE), p_adj[2], dz_ladder))


# sign-folded kink against 0 
p_snake_kink_raw  <- t.test(w_kink$snake,  mu = 0)$p.value
p_ladder_kink_raw <- t.test(w_kink$ladder, mu = 0)$p.value
p_kink_adj <- p.adjust(c(p_snake_kink_raw, p_ladder_kink_raw), method = "holm")

dz_snake_kink  <- mean(w_kink$snake,  na.rm = TRUE) / sd(w_kink$snake,  na.rm = TRUE)
dz_ladder_kink <- mean(w_kink$ladder, na.rm = TRUE) / sd(w_kink$ladder, na.rm = TRUE)

cat(sprintf("snake  kink vs 0: mean=%.3f  p_adj=%.4f  dz=%.2f\n",
            mean(w_kink$snake,  na.rm = TRUE), p_kink_adj[1], dz_snake_kink))
cat(sprintf("ladder kink vs 0: mean=%.3f  p_adj=%.4f  dz=%.2f\n",
            mean(w_kink$ladder, na.rm = TRUE), p_kink_adj[2], dz_ladder_kink))


# cleaner report writ-up
report1 <- function(x, name) {
  x  <- x[!is.na(x)]
  tt <- t.test(x, mu = 0)
  cat(sprintf("%-22s mean=%+.3f  95%% CI [%+.3f, %+.3f]  t(%d)=%.2f  p=%.4f  dz=%.2f\n",
              name, mean(x), tt$conf.int[1], tt$conf.int[2],
              tt$parameter, tt$statistic, tt$p.value, mean(x)/sd(x)))
}

report1(w_slope$snake,  "snake  slope vs 0")
report1(w_slope$ladder, "ladder slope vs 0")
report1(w_kink$snake,   "snake  kink vs 0")
report1(w_kink$ladder,  "ladder kink vs 0")

# ---- figures ----

# middle-vs-ends error as a function of presented orientation.
k_ppt <- trial_kink %>% 
  group_by(participant, arr, ori) %>%
  summarise(k = mean(k, na.rm = TRUE), 
            .groups = "drop")

k_grp <- trial_kink %>% 
  group_by(arr, ori) %>%
  summarise(k = mean(k, na.rm = TRUE), 
            se = sd(k, na.rm = TRUE)/sqrt(n()), 
            .groups = "drop")

ref <- tibble(ori = range(trial_kink$ori)) %>% 
  mutate(k = -2 * ori)


p_main <- ggplot() +
  geom_hline(yintercept = 0, 
             colour = "grey45", 
             linewidth = 0.5) +
  
  geom_line(data = ref, 
            aes(ori, k), 
            linetype = "dashed", 
            colour = "black") +
  
  # Adjusted text annotation to sit just inside the bottom right of the -10 limit
  annotate("text", x = 4.8, 
           y = -9.5,
           label = "full flip (\u22122\u00b7\u03b8)", 
           hjust = 1, 
           vjust = 0,
           size = 4, 
           colour = "black", 
           fontface = "bold") +
  
  geom_line(data = k_ppt, 
            aes(ori, 
                k, 
                group = interaction(participant, arr), 
                colour = arr),
            alpha = 0.15,
            linewidth = 0.4) +
  
  geom_line(data = k_grp, 
            aes(ori,
                k, 
                colour = arr),
            linewidth = 1.1) +
  geom_point(data = k_grp, 
             aes(ori, 
                 k, 
                 colour = arr), 
             size = 2) +
  geom_errorbar(data = k_grp,
                aes(ori, 
                    ymin = k - se, 
                    ymax = k + se, 
                    colour = arr),
                width = 1.2) +
  scale_colour_manual(values = pal,
                      labels = c("Ladder", "Snake")) +
  
  # Removed scale_y_continuous limits, added coord_cartesian
  coord_cartesian(ylim = c(-10, 10)) +
  
  labs(x = "Presented orientation (deg, CW +)",
       y = "Middle-vs-ends contrast k (deg)") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 14, face = "bold")
  )

p_main

ggsave("p_main.svg", p_main, width = 4, height = 4, units = "in")

# per-participant contour slope, paired (0 = none, -2 = full flip)
# Calculate averages for the large dots
slope_across_pp <- fc_slope %>% 
  group_by(arr) %>% 
  summarise(
    avg_slope = mean(slope, na.rm = TRUE),
    .groups = "drop"
  )

p_slope_fig <- ggplot(fc_slope, 
                      aes(x = arr, 
                          y = slope, 
                          fill = arr)) +
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
  geom_point(aes(color = arr), 
             size = 2.2, 
             alpha = 0.5) +
  geom_point(
    data = slope_across_pp,
    aes(x = arr, 
        y = avg_slope, 
        color = arr),
    alpha = 0.8,
    size = 5
  ) +
  scale_fill_manual(values  = pal) +
  scale_color_manual(values = pal) +
  scale_x_discrete(labels = c(ladder = "Ladder", snake = "Snake")) +
  scale_y_continuous(limits = c(-1, 0.2)) +
  labs(x = NULL, 
       y = "Forced contour slope b") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    legend.position = "none",
    strip.text = element_text(size = 14, face = "bold"),
    panel.spacing = unit(1.0, "lines")
  )

p_slope_fig

ggsave("p_slope_fig.svg", p_slope_fig, width = 3, height = 4, units = "in")

kink_per_pp <- trial_kink %>% 
  group_by(participant, arr) %>%
  summarise(kink = mean(ksf, na.rm = TRUE), .groups = "drop")


kink_across_pp <- kink_per_pp %>% 
  group_by(arr) %>% 
  summarise(avg_kink = mean(kink, na.rm = TRUE), .groups = "drop")


p_kink_fig <- ggplot(kink_per_pp, 
                     aes(x = arr, 
                         y = kink, 
                         fill = arr)) +
  
  geom_hline(yintercept = 0, 
             colour = "grey80", 
             linewidth = 0.5, 
             linetype = "dashed") +
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
  geom_point(aes(color = arr), 
             size = 2.2, 
             alpha = 0.5) +
  geom_point(
    data = kink_across_pp,
    aes(x = arr, 
        y = avg_kink, 
        color = arr),
    alpha = 0.8,
    size = 5
  ) +
  scale_fill_manual(values  = pal) +
  scale_color_manual(values = pal) +
  scale_x_discrete(labels = c(ladder = "Ladder", snake = "Snake")) +
  scale_y_continuous(limits = c(-4.5, 1.1)) +
  labs(x = NULL, 
       y = "Sign-folded kink (deg)") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    legend.position = "none",
    strip.text = element_text(size = 14, face = "bold"),
    panel.spacing = unit(1.0, "lines")
  )

p_kink_fig

ggsave("p_kink_fig.svg", p_kink_fig, width = 3, height = 4, units = "in")


mean(abs(trial_kink$ori))
