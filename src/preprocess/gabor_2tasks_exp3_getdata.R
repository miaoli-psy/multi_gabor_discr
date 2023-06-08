# ---------------libraries------------------------
if (!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
  
# ---------------read data -----------------------

# set working path
setwd("D:/OneDrive/projects/multi_gabor_discr/data/gabor3_raw_data/")
path <- "D:/OneDrive/projects/multi_gabor_discr/data/gabor3_raw_data/"

# list files
list_csv_files <- list.files(path = path)

# read data into one df
my_data <- do.call(rbind, lapply(list_csv_files, function(x) read.csv(x, stringsAsFactors = FALSE)))

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

my_data = my_data[kept_cols]


# some cleaning
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
         )

my_data3 <- subset(my_data2, !is.na(label))


colnames(my_data3)


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
  "sex"
)

my_data3 <- my_data3[kept_cols2]


write.csv(my_data3, file = "gabor3_data.csv")

