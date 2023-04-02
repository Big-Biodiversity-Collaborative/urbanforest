# Project: Tucson Urban Tree Equity and Bird Richness
# Script: Table connecting neighborhood name to neighborhood ID
# Credit: Jeff Oliver, University of Arizona, jcoliver@arizona.edu
# Author: Heatherlee Leary, University of Arizona, hleary.wildlife@outlook.com


# Load libraries
require(sf)        # Point filtering
require(raster)    # Work with rasters in R (includes sp package)
library(dplyr)     # Manipuate data frames

# Read in the shapefile
# Shapefiles obtained from City of Tucson GIS website on 2023-01-14
# Tucson Neighborhoods
neighborhood_bounds <- st_read("data/shapefiles/NEIGHBORHOODS_ALL.shp")

class(neighborhood_bounds)
head(neighborhood_bounds)


library(dplyr)
library(sf)

# Convert the sf object to a data frame, select columns, and add row numbers
neighborhood_table <- neighborhood_bounds %>%
  as.data.frame() %>%
  dplyr::select(OBJECTID, NAME) %>%
  mutate(neighborhood_id = row_number())

# View the resulting table
head(neighborhood_table)

# Read as CSV
# To connect neighborhood name to bird obs files
write.csv(neighborhood_table, file = "data/neighborhood_id_table.csv", row.names = FALSE)






