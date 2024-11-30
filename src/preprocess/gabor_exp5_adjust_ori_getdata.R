# ---------------libraries------------------------
library(dplyr)
library(ggplot2)
library(tidyverse)

# ---------------read data -----------------------

setwd("D:/OneDrive/projects/multi_gabor_discr/data/raw_adjust_ori_exp5/")
path <- "D:/OneDrive/projects/multi_gabor_discr/data/raw_adjust_ori_exp5/"

list_csv_files <- list.files(path = path, pattern = "_gabor_adjust_ori_")


kept_cols <- c(
  "label",
  "ori",
  "ori_psychopy",
  "ref_ori",
  "blocks.thisN",
  "trials.thisTrialN",
  "resp1",
  "resp2",
  "resp3",
  "display_resp1",
  "display_resp2",
  "display_resp3",
  "random_float",
  "cw_ccw",
  "participant",
  "sex",
  "age",
  "setsize"
  
)
dfs <- list()

# Loop through each file in the list
for (file in list_csv_files) {
  # read csv file
  df <- read.csv(paste0(path, file), stringsAsFactors = FALSE)
  
  # get subset based on kept cols
  df_subset <- df[, kept_cols, drop = FALSE]

  # add subset data to the list
  dfs <- append(dfs, list(df_subset))
}

# read data to one df
my_data <- do.call(rbind, dfs)


# drop rows that contains NA
my_data <- my_data[!is.na(my_data$setsize),]
my_data <- my_data[!is.na(my_data$blocks.thisN),]

# meaningful col names
my_data$stim_location <- ifelse(my_data$random_float <= 0.5, "left", "right")
my_data <- my_data %>% 
  rename(abs_ori = ori)
my_data$ori <- ifelse(my_data$cw_ccw <= 0.5, -1 * my_data$abs_ori, my_data$abs_ori)
my_data$ori_psychopy <- ifelse(my_data$cw_ccw <= 0.5, -1 * my_data$ori_psychopy, my_data$ori_psychopy)

# adjustment of angle and keep the resp between -89 degrees to 90 degrees
adjust_angle <- function(angle) {
  while (angle <= -90 | angle > 90) {
    angle <- angle + ifelse(angle > 90, -180, 180)
  }
  return(angle)
}

my_data <- my_data %>%
  mutate(display_resp1 = sapply(display_resp1, adjust_angle),
         display_resp2 = sapply(display_resp2, adjust_angle),
         display_resp3 = sapply(display_resp3, adjust_angle))

# align resp

my_data <- my_data %>%
  mutate(
    inner_resp = case_when(
      setsize == 1 ~ display_resp1,
      setsize == 3 & stim_location == "left" ~ display_resp3,
      setsize == 3 & stim_location == "right" ~ display_resp1
    ),
    outer_resp = case_when(
      setsize == 3 & stim_location == "left" ~ display_resp1,
      setsize == 3 & stim_location == "right" ~ display_resp3
    ),
    midd_resp = case_when(
      setsize == 3 ~ display_resp2  
    )
  )


# cal deviation
my_data$dev_innermost = my_data$inner_resp - my_data$ori
my_data$dev_midd = my_data$midd_resp - my_data$ori
my_data$dev_outermost = my_data$outer_resp - my_data$ori


# cal reversals
my_data$n_reversals <- rowSums(my_data[, c("dev_innermost", "dev_midd", "dev_outermost")] < 0)

# write to .csv
write.csv(my_data, file = "gabor_adjst_ori_alldata.csv", row.names = FALSE)
