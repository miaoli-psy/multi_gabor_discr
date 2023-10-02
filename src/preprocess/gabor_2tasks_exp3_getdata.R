# ---------------libraries------------------------
if (!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
  

library(ggplot2)
library(tidyverse)
# ---------------read data -----------------------

# set working path
setwd("D:/OneDrive/projects/multi_gabor_discr/data/gabor3_raw_data/")
path <- "D:/OneDrive/projects/multi_gabor_discr/data/gabor3_raw_data/"

# read data into one df if each sigle file has the same data structure
# list files
# list_csv_files <- list.files(path = path, pattern='gabor_discri_2tasks')
# my_data <- do.call(rbind, lapply(list_csv_files, function(x) read.csv(x, stringsAsFactors = FALSE)))


# col_names <- data.frame(colnames(my_data))

# keep col by name
kept_cols <- c(
  "blocks.thisN",
  "blocks.thisIndex",
  "blocks.thisTrialN",
  "blocks_setsize1.thisN",
  "blocks_setsize1.thisIndex",
  "blocks_setsize1.thisTrialN",
  "key_pressed1",
  "key_pressed13",
  "trials.label",
  "trials.setsize",
  "trials.direction",
  "trials.intensity",
  "trials.response",
  "trials_2.label",
  "trials_2.setsize",
  "trials_2.direction",
  "trials_2.intensity",
  "trials_2.response",
  "participant",
  "order",
  "age",
  "sex"
)

# empty list to store data
dfs <- list()

list_csv_files <- list.files(path = path, pattern='gabor_discri_2tasks')

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



# some cleaning: get the needed column names
my_data2 <- my_data %>%
  mutate(label = case_when(trials_2.label != '' ~ trials_2.label,
                                  trials.label != '' ~ trials.label,
                                  TRUE ~ NA),
         thisN = case_when(!is.na(blocks.thisN) ~ blocks.thisN,
                           !is.na(blocks_setsize1.thisN) ~ blocks_setsize1.thisN,
                           TRUE ~NA),
         thisIndex = case_when(!is.na(blocks.thisIndex) ~blocks.thisIndex,
                               !is.na(blocks_setsize1.thisIndex) ~blocks_setsize1.thisIndex,
                               TRUE ~ NA),
         thisTrialN = case_when(!is.na(blocks.thisTrialN) ~ blocks.thisTrialN,
                               !is.na(blocks_setsize1.thisTrialN) ~blocks_setsize1.thisTrialN,
                               TRUE ~ NA),
         setsize = case_when(!is.na(trials.setsize) ~trials.setsize,
                                !is.na(trials_2.setsize) ~trials_2.setsize,
                                TRUE ~ NA),
         direction = case_when(trials.direction != '' ~ trials.direction,
                               trials_2.direction != '' ~ trials_2.direction,
                           TRUE ~ NA),
         intensity = case_when(!is.na(trials.intensity) ~trials.intensity,
                             !is.na(trials_2.intensity) ~trials_2.intensity,
                             TRUE ~ NA),
         ans_same = case_when(order == 1 ~ key_pressed1,
                              order == 2 ~ key_pressed13,
                              TRUE ~ NA)
         )


# get the rows that are in main exp
my_data3 <- subset(my_data2, !is.na(label))


colnames(my_data3)


# kept columns
kept_cols2 <- c(
  "label",
  "thisN",
  "thisIndex",
  "thisTrialN",
  "setsize",
  "direction",
  "intensity",
  "participant",
  "order",
  "age",
  "sex",
  "ans_same"
)


my_data3 <- my_data3[kept_cols2]

# drop rows that contains NA
my_data3 <- my_data3[!is.na(my_data3$participant), ]


# add the gabor_type column
my_data3 <- my_data3 %>%
  mutate(
    gabor_type = case_when(
      str_ends(label, "snake") ~ "snake",
      str_ends(label, "ladder") ~ "ladder",
      str_ends(label, "v") ~ "ladder",
      str_ends(label, "h") ~ "snake",
      TRUE ~ NA_character_  # This line is for any other unexpected cases
    )
  )

# write to .csv
write.csv(my_data3, file = "gabor3_data.csv", row.names = FALSE)


#### check raw data by participant ####
my_data_p<- subset(my_data3, participant == 22)

# add trial number for each block, column reset for each unique value of "thisN" and "label"
my_data_p <- my_data_p %>%
  group_by(thisN, label) %>%
  mutate(row_num = row_number()) %>%
  ungroup()

plot <-
  ggplot(my_data_p, aes(x = row_num, y = intensity)) +
  
  # treat all the data points as a single group and 
  # draw one continuous line connecting all of them
  geom_line(aes(group = 1)) +
  
  labs(x = "TrialsN", y = "Intensity") +
  
  facet_grid(label ~ thisN, scales = "free_x") +
  
  theme_minimal()

plot

#### check all raw data iteration participant ####

# list unique participant
participants <- unique(my_data3$participant)

for (part in participants) {
  
  # Subset data for current participant
  participant_data <- subset(my_data3, participant == part)
  
  # Add row_num for each 'thisN' in the subsetted data
  participant_data <- participant_data %>%
    group_by(thisN) %>%
    mutate(row_num = row_number()) %>%
    ungroup()
  
  # Plot intensity against row_num, faceted by 'label' and 'thisN'
  plot <- ggplot(participant_data, aes(x = row_num, y = intensity)) +
    
    geom_line(aes(group = 1)) +
    
    facet_grid(label ~ thisN, scales = "free_x") +
    
    labs(title = paste("Participant", part),
         x = "TrialsN", 
         y = "Intensity") +
    
    theme_minimal()
  
  # Display the plot
  print(plot)
  
  # Optional: Save the plot to a file
  # ggsave(filename = paste0("plot_participant_", part, ".png"), plot = plot,
  #        width = 10, height = 5)
}
