# Break bird GBIF data into smaller files for export
# Heatherlee Leary
# hleary.wildlife@outlook.com
# 2023-24-01


# Read in large bird file
bird_data <- read.delim(file = "data/gbif/0039570-220831081235567.csv") # If use file with different name, update .gitignore

# Count number of rows
nrow(bird_data)

# Want to save only 150K rows at a time
file_rows <- 150000
num_files <- ceiling(nrow(bird_data)/file_rows) #ceiling rounds up to the nearest integers, and floor rounds down

# Use iteration to save files
file_name_start <- "data/gbif/tucson_az-bird-obs-"
for (file_i in 1:num_files) {
  cat("saving file", file_i, "\n")
  row_start <- (file_i - 1) * file_rows + 1
  row_end <- row_start + file_rows - 1
  cat("\tstart", row_start, "\n")
  if(row_end > nrow(bird_data)){
    row_end <- nrow(bird_data)
  } 
  cat("\tend", row_end, "\n")
  bird_subset <- bird_data[row_start:row_end,]
  # TODO
  # Select only columns that are needed
  file_name <- paste0(file_name_start, file_i, ".csv")
  write.csv(x = bird_subset, file = file_name, row.names = FALSE)
}
