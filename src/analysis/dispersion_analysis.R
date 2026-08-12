# Exp 4: within-trial dispersion of adjustment errors (Figure 3b)

library(tidyverse)
library(lme4)
library(emmeans)
library(svglite)
library(gghalves)
library(patchwork) 


setwd("d:/OneDrive/projects/multi_gabor_discr/src/analysis/")

PAL        <- c(ladder = "#F28522", snake = "#674EA7")
ORI_LEVELS <- c(2, 4, 10)

# ---- helpers ----------------------------------------------------------------

# shortest signed angular difference, wrapped to [-90, 90)
angle_dist <- function(a, b) (a - b + 90) %% 180 - 90

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

# ---- data -------------------------------------------------------------------

data_exp4 <- read_csv("../../data/gabor_adjst_ori_alldata.csv",
                      show_col_types = FALSE)

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


#within trial disperson

disp <- data %>%
  rowwise() %>%
  mutate(sd_err = sd(c(err_inner, err_mid, err_outer))) %>%
  ungroup() %>%
  mutate(
    var_err  = sd_err^2,              # for the robustness models
    rms_pair = sd_err * sqrt(2)       # RMS difference between two Gabors
  )


# lmm

m_full_ml <- lmer(sd_err ~ abs_ori * arr + (1 | participant),
                  data = disp, REML = FALSE)
m_red_ml  <- lmer(sd_err ~ abs_ori + arr + (1 | participant),
                  data = disp, REML = FALSE)

anova(m_red_ml, m_full_ml)


emm <- emmeans(m_full_ml, ~ arr | abs_ori)

contrast <- pairs(emm, adjsut = "tukey")
contrast


#plot

disp_pp <- disp %>%
  group_by(participant, arr, abs_ori) %>%
  summarise(sd_err = mean(sd_err, na.rm = TRUE), .groups = "drop")

disp_grp <- disp_pp %>%
  group_by(arr, abs_ori) %>%
  summarise(m  = mean(sd_err, na.rm = TRUE),
            se = sd(sd_err, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

p_fig3b <- ggplot() +
  geom_line(data = disp_pp,
            aes(x = abs_ori, 
                y = sd_err,
                group = interaction(participant, arr), 
                colour = arr),
            alpha = 0.15, 
            linewidth = 0.4) +
  geom_line(data = disp_grp,
            aes(x = abs_ori, 
                y = m, 
                group = arr, 
                colour = arr),
            linewidth = 1.1) +
  geom_errorbar(data = disp_grp,
                aes(x = abs_ori, 
                    ymin = m - se, 
                    ymax = m + se,
                    colour = arr),
                width = 0.12, 
                linewidth = 0.8) +
  geom_point(data = disp_grp,
             aes(x = abs_ori,
                 y = m, 
                 colour = arr),
             size = 3) +
  scale_colour_manual(values = PAL,
                      labels = c("Ladder", "Snake")) +
  scale_x_discrete(labels = function(v) paste0(v, "\u00b0")) +
  
  labs(x = "Orientation",
       y = "Within-trial SD of\nadjustment errors (\u00b0)") +
  
  theme_gabor()

p_fig3b



p_fig3b_paired <- ggplot(disp_pp,
                         aes(x = arr, 
                             y = sd_err, 
                             fill = arr)) +
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
  geom_errorbar(data = disp_grp,
                aes(x = arr, 
                    ymin = m - se, 
                    ymax = m + se, 
                    colour = arr),
                width = 0.08, 
                linewidth = 0.8, 
                inherit.aes = FALSE) +
  geom_point(data = disp_grp, 
             aes(x = arr,
                 y = m, 
                 colour = arr),
             size = 5, 
             inherit.aes = FALSE) +
  facet_wrap(~ abs_ori,
             nrow = 1,
             labeller = labeller(abs_ori = function(v) paste0(v, "°"))) +
  
  scale_fill_manual(values = PAL) +
  
  scale_colour_manual(values = PAL) +
  
  scale_x_discrete(labels = c(ladder = "Ladder", snake = "Snake")) +
  
  labs(x = NULL, y = "Within-trial SD of\nadjustment errors (°)") +
  
  theme_gabor() +
  
  theme(legend.position = "none")

p_fig3b_paired


#also run correlation_analysis.R

fig3 <- p_fig3a/ p_fig3b_paired +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 16, face = "bold"))

fig3

# ggsave("p_fig3.svg",fig3, width = 7, height = 7, units = "in")
