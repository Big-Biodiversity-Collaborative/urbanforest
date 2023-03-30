# Project: Tucson Urban Tree Equity and Bird Diversity
# Script: Visualizing neighborhood bird counts
# Author: Heatherlee Leary, University of Arizona, hleary@arizona.edu

# Load Libraries
require(sf)        # Point filtering
require(raster)    # Work with rasters in R (includes sp package)
require(sp)        # Spatial data
require(dplyr)     # Manipuate data frames



# Read in the data
urbanforest <- read.csv(file = "data/urbanforest_data.csv")
neighborhood_bounds <- st_read("data/shapefiles/NEIGHBORHOODS_ALL.shp")


# Need neighborhood geometry to map the data
# Convert the sf object (neighborhood_bounds) to a data frame
bounds_df <- neighborhood_bounds %>%
  as.data.frame() %>%
  dplyr::select(NAME, geometry)

head(bounds_df)


# Print the resulting data frame with geometry
# Merge the urbanforest and bounds_df data frames by neighborhood name
urbanforest_geom <- merge(urbanforest, bounds_df, by.x = "Neighborhood", by.y = "NAME")


# There are four more rows in the merged data frame... why? 
dim(urbanforest)       # 123 x 8
dim(urbanforest_geom)  # 127 X 9

# Check for duplicate values in urbanforest_geom and bounds_df
any(duplicated(urbanforest_geom)) # False (no duplicates)
any(duplicated(bounds_df))        # False (no duplicates)


# Convert the "geometry" column to an sf object to check for missing geometries
urbanforest_geom_sf <- st_as_sf(urbanforest_geom)
# Check for missing geometries in urbanforest_geom and bounds_df
sum(st_is_empty(urbanforest_geom_sf$geometry)) # No missing geometries
sum(st_is_empty(bounds_df$geometry)) # No missing geometries

# Compare unique neighborhood names in urbanforest and urbanforest_geom
setdiff(urbanforest$Neighborhood, urbanforest_geom$Neighborhood) # No differences...

# It looks like there are no neighborhoods missing from either data frame. 
# This MAY mean that the difference in the number of rows between "urbanforest" 
# and "urbanforest_geom" is due to duplicate or missing values that were present 
# in the original data but were resolved during the merge process.



# Map the urbanforest_geom
# Convert to spatial data type
urbanforest_geom_sf <- st_as_sf(urbanforest_geom)
class(urbanforest_geom_sf)
st_crs(urbanforest_geom_sf) # CRS is NAD83(HARN)


# Quick test
plot(urbanforest_geom_sf) 

# Save sf object as a shapefile
st_write(urbanforest_geom_sf, dsn = "data/urbanforest_geom.shp")



