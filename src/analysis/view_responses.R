library(tidyverse)

pal <- c(ladder = "#F28522", snake = "#674EA7")

# -----------------------------------------------------------------------------
# read data
# -----------------------------------------------------------------------------
setwd("d:/OneDrive/projects/multi_gabor_discr/src/analysis/")

data_exp4 <- read_csv("../../data/gabor_adjst_ori_alldata.csv")

data <- data_exp4 %>%
  dplyr::select(label, ori, participant, inner_resp, midd_resp, outer_resp) %>%
  filter(label %in% c("setsize3_r_ladder", "setsize3_r_snake")) %>%
  mutate(
    arr       = if_else(str_detect(label, "snake"), "snake", "ladder"),
    err_inner = inner_resp - ori,
    err_mid   = midd_resp  - ori,
    err_outer = outer_resp - ori
  )

# -----------------------------------------------------------------------------
# 2. Long format
# -----------------------------------------------------------------------------

ori_var <- "ori"     

# -----------------------------------------------------------------------------
# long format: one row per (trial, location); keep the raw response, no error
# -----------------------------------------------------------------------------
long <- data %>%
  mutate(trial_id = row_number(), abs_ori = abs(ori)) %>%
  pivot_longer(c(inner_resp, midd_resp, outer_resp),
               names_to = "loc", values_to = "resp") %>%
  mutate(loc = recode(loc, inner_resp = "inner", midd_resp = "middle", outer_resp = "outer"),
         loc = factor(loc, levels = c("inner", "middle", "outer"))) %>%
  rename(pid = participant, condition = arr) %>%
  mutate(orisplit = round(.data[[ori_var]], 3)) %>%
  select(pid, condition, orisplit, trial_id, loc, ori, resp) %>%
  filter(!is.na(resp))


# -----------------------------------------------------------------------------
# sample up to 100 trials per (condition x orisplit), give each a 10x10 slot
# -----------------------------------------------------------------------------
set.seed(42)
sampled <- long %>%
  distinct(condition, orisplit, pid, trial_id) %>%
  group_by(condition, orisplit) %>%
  group_modify(~ { s <- slice_sample(.x, n = min(100, nrow(.x)))
  s$k <- seq_len(nrow(s)) - 1; s }) %>%
  ungroup() %>%
  mutate(gcol = k %% 10, grow = k %/% 10) %>%
  select(condition, pid, trial_id, gcol, grow)

# -----------------------------------------------------------------------------
# each bar drawn at the RAW reported orientation (resp)
# -----------------------------------------------------------------------------
half <- 0.15
loc_gap <- 0.40 # spacing WITHIN a trial (the 3 bars)
pitch <- 1.6   # spacing BETWEEN trials (grid pitch)
half_target <- 0.25

pl <- long %>%
  inner_join(sampled, by = c("condition", "pid", "trial_id")) %>%
  mutate(loc_idx  = as.integer(loc),
         px       = gcol * pitch + (loc_idx - 2) * loc_gap,   
         py       = -grow * pitch,
         baseline = if_else(condition == "ladder", 90, 0),
         
         # Solid line math (uses 'half')
         ang      = (baseline - resp) * pi / 180,
         x1 = px - half * cos(ang),  
         y1 = py - half * sin(ang),
         x2 = px + half * cos(ang),  
         y2 = py + half * sin(ang),
         
         # Dashed line math (uses 'half_target')
         ang0 = (baseline - ori) * pi / 180,
         sx1 = px - half_target * cos(ang0), 
         sy1 = py - half_target * sin(ang0),
         sx2 = px + half_target * cos(ang0), 
         sy2 = py + half_target * sin(ang0))

# one plot per cell -> 12 PNGs in ./raw_trial_plots/
# -----------------------------------------------------------------------------
plot_cell <- function(sub, title, col) {
  ggplot(sub) +
    geom_segment(aes(sx1, sy1, xend = sx2, yend = sy2),
                 colour = "grey55", linewidth = 0.4,
                 linetype = "dashed", lineend = "round") +
    geom_segment(aes(x1, y1, xend = x2, yend = y2),
                 colour = col, linewidth = 0.55, lineend = "round") +
    coord_fixed() + labs(title = title) +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

dir.create("raw_trial_plots", showWarnings = FALSE)
cells_list <- pl %>% distinct(condition, orisplit) %>% arrange(condition, orisplit)

for (i in seq_len(nrow(cells_list))) {
  cd  <- cells_list$condition[i]; ov <- cells_list$orisplit[i]
  sub <- filter(pl, condition == cd, orisplit == ov)
  ttl <- sprintf("%s  |  ori = %s  (trialsN = %d)", cd, ov, n_distinct(sub$trial_id))
  lbl <- gsub("\\.", "p", gsub("-", "m", as.character(ov)))
  ggsave(sprintf("raw_trial_plots/%s_ori_%s.png", cd, lbl),
         plot_cell(sub, ttl, pal[[cd]]), width = 6, height = 6, dpi = 300)
}
