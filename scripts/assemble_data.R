# Import multiple files and assemble single dataset
# This script reverses what was done in the breakdown_bigdata.R script
# Heatherlee Leary
# hleary.wildlife@outlook.com
# 2023-24-01


library(dplyr)
bird_files <- list.files(path = "data/gbif", 
                         pattern = "tucson_az-bird-obs-",
                         full.names = TRUE)

# Iterate over bird files to create one data object
bird_data <- NULL
for (file_i in 1:length(bird_files)) {
  cat("reading file", file_i, "of", length(bird_files), "\n")
  bird_subset <- read.csv(file = bird_files[file_i])
  if (is.null(bird_data)) {
    bird_data <- bird_subset
  } else {
    bird_data <- dplyr::bind_rows(bird_data, bird_subset)
  }
}
