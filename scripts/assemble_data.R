# Project: Tucson Urban Tree Equity and Bird Richness
# Script: Assemble all the GBIF observation files
# Author: Heatherlee Leary, University of Arizona, hleary@arizona.edu


library(dplyr)
bird_files <- list.files(path = "data/gbif",
                         pattern = "bird-obs",
                         full.names = TRUE)


# Iterate over bird files to create one data object
# Add column that indicates neighborhood_id (row number) to connect to NAME later
# Ran into error, so tryCatch() to attempt to read each file and handle any errors
# neighborhood_id will remain the same, even if the read fails and the loop moves on to the next file.
bird_data <- NULL
for (file_i in 1:length(bird_files)) {
  cat("reading file", file_i, "of", length(bird_files), "\n")
  bird_subset <- tryCatch(read.csv(file = bird_files[file_i]), 
                          error = function(e) NULL)
  if (!is.null(bird_subset) && nrow(bird_subset) > 0) {
    bird_subset$neighborhood_id <- file_i
    if (is.null(bird_data)) {
      bird_data <- bird_subset
    } else {
      bird_data <- dplyr::bind_rows(bird_data, bird_subset)
    }
  }
}

# Explore data
head(bird_data)
class(bird_data)
ncol(bird_data)
nrow(bird_data)

# Save combined file as CSV
write.csv(bird_data, file = "data/gbif-bird-obs-combined.csv", row.names = FALSE)


