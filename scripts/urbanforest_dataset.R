# Project: Tucson Urban Tree Equity and Bird Richness
# Script: Neighborhood species richness and tree equity
# Author: Heatherlee Leary, University of Arizona, hleary.wildlife@outlook.com


# Here we will create a data frame and simple features data type (shapefile)
# that summarizes neighborhood species richness and tree equity values


# Load libraries
require(sf)        # Point filtering
require(raster)    # Work with rasters in R (includes sp package)
require(tidyverse) # Manipulate data frames
require(dplyr)

# Read in data
data <- st_read("output/shapefiles/spatial_join_data.shp")

# Convert the sf object to a data frame, select columns
data_df <- data %>%
  as.data.frame() %>%
  dplyr::select(NAME, TreeEquity, spcsRch, AREASqKm, geometry)

# View
class(data_df)
dim(data_df)    # should have 466 rows (neighborhoods)
head(data_df)



# Check for duplicate neighborhood names
neighborhood_names <- data_df$NAME
duplicates <- neighborhood_names[duplicated(data_df$NAME)]

# Print out any duplicates
if (length(duplicates) > 0) {
  cat("The following neighborhood names are duplicated:", duplicates, "\n")
} else {
  cat("There are no duplicate neighborhood names.\n")
}



# Clean

# Sort the "NAME" column in alphabetical order 
data_df <- data_df[order(data_df$NAME), ]

# Add a new column "UID" (unique ID) based on row number,
# and make it the first column of the data frame. 
data_df <- data_df %>% 
  mutate(UID = row_number()) %>% 
  select(UID, everything())

# Add a new column for species richness per square kilometer
data_df$richPerSqKm <- data_df$spcsRch / data_df$AREASqKm


# Clean column names
colnames(data_df) <- c("UID", 
                       "neighborhood", 
                       "treeEquity", 
                       "speciesRich", 
                       "areaSqKm", 
                       "geometry", 
                       "richPerSqKm")

# Reorder columns
data_df <- data_df %>%
  select(UID, neighborhood, treeEquity, speciesRich, richPerSqKm, areaSqKm, geometry)

head(data_df)



# Save as CSV
# The geometry column is for a spatial data type, so we will remove it first
data_df_no_geom <- data_df %>% 
  select(-geometry)

write.csv(data_df_no_geom, file = "output/urbanforest_data.csv", row.names = FALSE)


# Convert to spatial data type
data_sf <- st_as_sf(data_df)

# Check
class(data_sf)
st_crs(data_sf) # CRS is NAD83(HARN)

# Save sf object as a shapefile
st_write(data_sf, dsn = "output/shapefiles/urbanforest_data.shp", append = FALSE)



# ================================================================
# Subsets ========================================================
# ================================================================

# where Davis Monthan AFB is removed 
# (as it is not eligible for City initiatives, or publicly accessible)
subset_no_dm <- data_df[data_df$neighborhood != 'Davis-Monthan', ]

# where treeEquity is > 0 
subset_tes <- subset_no_dm[subset_no_dm$treeEquity > 0, ]

# where speciesRich > 10 (to remove neighborhoods with very few obs)
subset_rich <- subset_no_dm[subset_no_dm$speciesRich > 10, ]

# where both of those are true
subset_both <- subset_no_dm[subset_no_dm$treeEquity > 0 & subset_no_dm$speciesRich > 10, ]


# View
dim(subset_no_dm)     # 465 neighborhoods
dim(subset_tes)       # 329 neighborhoods
dim(subset_rich)      # 318 neighborhoods
dim(subset_both)      # 221 neighborhoods

summary(subset_tes)   # treeEquity range: 41.44-100.00
summary(subset_rich)  # speciesRich range: 11-262
summary(subset_both)  # treeEquity no change, speciesRich range 11-258



# Export subsets for easy data viz later: 

# Convert to spatial data type
subset_tes_sf <- st_as_sf(subset_tes)
subset_rich_sf <- st_as_sf(subset_rich)
subset_both_sf <- st_as_sf(subset_both)

# Save sf object as shapefile
st_write(subset_tes_sf, dsn = "output/shapefiles/subset_tes_data.shp", append = FALSE)
st_write(subset_rich_sf, dsn = "output/shapefiles/subset_rich_data.shp", append = FALSE)
st_write(subset_both_sf, dsn = "output/shapefiles/subset_both_data.shp", append = FALSE)



