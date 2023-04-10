# Project: Tucson Urban Tree Equity and Bird Richness
# Script: Identifying neighborhoods with gbif bird data.
# Author: Heatherlee Leary, University of Arizona, hleary@arizona.edu


# I need to make a study area map showing which neighborhoods have GBIF data
# independent of whether the neighborhood has TES data. 


# Load libraries
library(sf)        # Point filtering
library(raster)    # Work with rasters in R (includes sp package)
library(dplyr)     # Manipuate data frames


# Combine tables to connect bird obs with neighborhood name and neighborhood id
neighborhood_table <- read.csv(file = "data/neighborhood_id_table.csv", header = TRUE)
gbif_table <- read.csv(file = "data/gbif-bird-obs-combined.csv", header = TRUE)

# Explore
head(neighborhood_table)
head(gbif_table)

# Join the bird_table and neighborhood_table data frames based on neighborhood_id
gbif_table <- gbif_table %>%
  left_join(neighborhood_table, by = "neighborhood_id")

# View the resulting table
colnames(gbif_table)
head(gbif_table)

# Cleaning
gbif_table <- gbif_table[c("decimalLatitude", "decimalLongitude", "individualCount", "family", "species", "NAME")]
colnames(gbif_table)

# Species per neighborhood (no duplicates)
# Neighborhoods without observations are not included
neighborhood_gbif <- aggregate(gbif_table$species, 
                                  by=list(gbif_table$NAME), 
                                  FUN=function(x) paste(unique(x), collapse=", "))

# Did it work?
head(neighborhood_gbif)
colnames(neighborhood_gbif)

# Change column names
colnames(neighborhood_gbif) <- c("neighborhood", "species.list")
colnames(neighborhood_gbif)

# Generate a column that counts the number of species listed per neighborhood
neighborhood_gbif$species.count <- sapply(strsplit(neighborhood_gbif$species.list, ", "), length)
colnames(neighborhood_gbif)



# To make a map, we need neighborhood geometry. 
neighborhood_bounds <- st_read("data/shapefiles/NEIGHBORHOODS_ALL.shp")

# Convert the sf object (neighborhood_bounds) to a data frame
bounds_df <- neighborhood_bounds %>%
  as.data.frame() %>%
  dplyr::select(NAME, SHAPE_Area, geometry)

# Merge the neighborhood_gbif and bounds_df data frames by neighborhood name
gbif_geom <- merge(neighborhood_gbif, bounds_df, by.x = "neighborhood", by.y = "NAME")

# Don't need the species list for this map, so let's remove for better viewing
gbif_geom <- gbif_geom[c("neighborhood", "species.count", "SHAPE_Area", "geometry")]


# Number of rows tells us number of neighborhoods with bird richness data
dim(gbif_geom) # 425 neighborhoods



# Map the urbanforest_geom
# Convert to spatial data type
gbif_geom_sf <- st_as_sf(gbif_geom)
class(gbif_geom_sf)
st_crs(gbif_geom_sf) # CRS is NAD83(HARN)


# Quick test
plot(gbif_geom_sf) 

# Save sf object as a shapefile
st_write(gbif_geom_sf, dsn = "data/gbif_geom.shp")



