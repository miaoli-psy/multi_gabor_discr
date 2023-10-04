# ---------------libraries------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)

# ---------------read data------------------------

exp <- "exp1"

# set working path
setwd("D:/OneDrive/projects/multi_gabor_discr/data/raw_data_exp1_exp2/")
path <- "D:/OneDrive/projects/multi_gabor_discr/data/raw_data_exp1_exp2/"

# set useful cols
COL_multi_gabor_staircase = c(
  "blocks.thisN",
  "blocks.thisIndex",
  "trials.label",
  "trials.eccentricity",
  "trials.garbor_size_in_deg",
  "trials.setsize",
  "trials.sf_in_cycle_per_deg",
  "trials.ref_ori",
  "trials.thisN",
  "trials.direction",
  "trials.intensity",
  "trials.response",
  "participant",
  "age",
  "sex"
)

# empty list to store dat
dfs <- list()

list_csv_files <-list.files(path = path, pattern = "_gabor_discri_staircase")

# Loop through each file in the list
for (file in list_csv_files) {
  # read csv file
  df <- read.csv(paste0(path, file), stringsAsFactors = FALSE)
  
  # get subset based on kept cols
  df_subset <- df[, COL_multi_gabor_staircase, drop = FALSE]
  
  # add subset data to the list
  dfs <- append(dfs, list(df_subset))
}

# read data to one df
my_data <- do.call(rbind, dfs)


# add the gabor_type column
my_data <- my_data %>%
  mutate(
    gabor_type = case_when(
      str_ends(trials.label, "snake") ~ "snake",
      str_ends(trials.label, "ladder") ~ "ladder",
      str_ends(trials.label, "v") ~ "ladder",
      str_ends(trials.label, "h") ~ "snake",
      TRUE ~ NA_character_  # This line is for any other unexpected cases
    )
  )

# add gabor arrangment
my_data <- my_data %>%
  mutate(
    gabor_arrangment = case_when(
      grepl("_r_", trials.label) ~ "radial",
      grepl("_t_", trials.label) ~ "tangential",
      str_ends(trials.label, "v") ~ "s1",
      str_ends(trials.label, "h") ~ "s1",
      TRUE ~ NA_character_  # This line is for any other unexpected cases
    )
  )


# drop rows that contains NA or empty
my_data2 <- my_data[!is.na(my_data$trials.label), ]
my_data2 <- my_data2[my_data2$trials.label != "", ]

# check NA and empty rows
na_rows <- which(apply(my_data2, 1, function(row) any(is.na(row)))) #1 apply to rows; 2 columns
empty_rows <- which(apply(my_data2, 1, function(row) any(row == "")))


# get preprocessed data
if (exp == "exp1") {
  my_data3 <- subset(my_data2, participant >= 1 & participant <= 20)
} else if (exp == "exp2") {
  my_data3 <- subset(my_data2, participant > 20)
  
} else {
  cat("invalid experiment selection-choose 'exp1' or 'exp2'.")
}


# write to .csv
if (exp == "exp1"){
  write.csv(my_data3, file = "gabor_exp1_alldata.csv", row.names = FALSE)
} else if (exp == "exp2"){
  write.csv(my_data3, file = "gabor_exp2_alldata.csv", row.names = FALSE)
}

# ---------------check raw data by participant--------------

#  get participant n
participants <- unique(my_data3$participant)

# get labels
labels <- unique(my_data3$trials.label)

dfs_by_participant <- list()

for (p in participants) {
  
  my_data_p <- subset(my_data3, participant == p)
  
  # add trial number for each block, column reset for each unique value of "thisN" and "label"
  my_data_p <- my_data_p %>%
    group_by(blocks.thisN, trials.label) %>%
    mutate(row_num = row_number()) %>%
    ungroup()
  
  dfs_by_participant <- append(dfs_by_participant, list(my_data_p))
  
  for (i in seq_along(labels)) {
    
    # plot for each labels, otherwise too much in one plot
    subset_data_p <-
      
      subset(my_data_p, trials.label %in% c(labels[i]))
    
    plot <-
      ggplot(subset_data_p, aes(x = row_num, y = trials.intensity)) +
      
      # treat all the data points as a single group and
      # draw one continuous line connecting all of them
      geom_line(aes(group = 1)) +
      
      labs(x = "TrialsN", y = "Intensity") +
      
      facet_grid(trials.label ~ blocks.thisN, scales = "free_x") +
      
      theme_minimal()
    
    ggsave(paste0("plot_", labels[i], "_pp_", p, "_", exp, ".png"), plot)
    
  }
}






