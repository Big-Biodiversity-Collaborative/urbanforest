# Project: Tucson Urban Tree Equity and Bird Diversity
# Script: Download GBIF observation data
# Credit: Jeff Oliver, University of Arizona, jcoliver@arizona.edu
# Heatherlee Leary
# hleary.wildlife@outlook.com
# 2023-01-14



# =======================================================================
# Load Libraries ========================================================
# =======================================================================

require(tidyverse) # Data wrangling (dyplyr package) and visualization (ggplot2 package)
require(sf)        # Point filtering
require(raster)    # Work with rasters in R (includes sp package)
require(rgbif)     # Search and retrieve data from GBIF
source(file = "scripts/query_gbif.R") # Function to query GBIF



# ======================================================================
# Load and wrangle boundary data =======================================
# ======================================================================

# Load boundary data
# Tree Equity Score Neighborhoods for Tucson
neighborhood_bounds <- st_read("data/Tucson_Tree_Equity_Scores.shp") # This is NAD83. GBIF data will be WGS83.

# Explore data
head(neighborhood_bounds)   
class(neighborhood_bounds)   
colnames(neighborhood_bounds)
plot(neighborhood_bounds[,2])


# Define the WGS84 CRS
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Transform the geometry column to WGS84 for the entire data frame
neighborhood_bounds_wgs84 <- st_transform(neighborhood_bounds, crs = wgs84)

# Explore data
class(neighborhood_bounds_wgs84)
colnames(neighborhood_bounds_wgs84)
plot(neighborhood_bounds_wgs84[,2])


# Subset data frame to include only rows (neighborhoods) that have a TES value
neighborhood_poly <- neighborhood_bounds_wgs84[!is.na(neighborhood_bounds_wgs84$TES), ]

# Explore data
class(neighborhood_poly)
colnames(neighborhood_poly)
plot(neighborhood_poly[,2])


# Find minimum and maximum longitude and latitude coordinates 
# for a rectangle that contains all the neighborhoods
poly_bounds <- st_bbox(neighborhood_poly)
min_lon <- poly_bounds["xmin"] 
max_lon <- poly_bounds["xmax"] 
min_lat <- poly_bounds["ymin"] 
max_lat <- poly_bounds["ymax"] 



# ======================================================================
# Download and filter GBIF observation data ============================
# ======================================================================

# Identify which taxon data to pull from GBIF. 
# Refer to GBIF for the code. Birds (aves) = 212.
taxon_keys <- c("Aves" = 212)


# Indicate whether or not to overwrite data files that already exist
overwrite <- FALSE   

# Creates a character string by pasting together the names of neighborhoods
neighborhood_string <- paste(neighborhood_bounds$NAME)

# Loops over each row in the neighborhood_bounds data frame 
# and sets the 'neighborhood' variable to the value in the NAME column for each neighborhood
# then prints a message indicating that data is being downloaded for the neighborhood
for (i in 1:nrow(neighborhood_bounds)) {
  neighborhood <- neighborhood_bounds$NAME[i]
  message("downloading for ", neighborhood, ", i = ", i)
}


# The 'for' loop downloads bird observation data for each neighborhood, 
# using the GBIF API to query for bird observations within the boundaries of each neighborhood.

# Loop over each row in neighborhood_bounds, 
# setting the variable 'neighborhood' to the value in the NAME column for each neighborhood.
for (i in 1:nrow(neighborhood_bounds)) {  
  neighborhood <- neighborhood_bounds$NAME[i]
  
  # Construct a filename for the downloaded data using the paste0() function, 
  # which concatenates a string with the values of i and "-bird-obs.csv".  
  neighborhood_file <- paste0("data/gbif/neighborhood-", i, "-bird-obs.csv")
  if (overwrite | !file.exists(neighborhood_file)) {
    message("***  Downloading data for ", neighborhood)
    
    # Code below checks whether the file already exists or whether it should be overwritten, 
    # If not, it downloads bird observation data for the neighborhood using the query_gbif() function,
    # passing in the bounding coordinates for the neighborhood as well as the taxon key.
    # Bird obs are too large, so loop over years you want to search for.
    neighborhood_obs <- NULL
    for (year_i in 2017:2021) {
      
      neighborhood_year_obs <- query_gbif(taxon_keys = taxon_keys,
                                          lon_limits = c(min_lon, max_lon),
                                          lat_limits = c(min_lat, max_lat),
                                          verbose = TRUE,
                                          year_range = as.character(year_i))
      
      if (is.null(neighborhood_obs)){
        neighborhood_obs <- neighborhood_year_obs
      } else {
        neighborhood_obs <- neighborhood_obs %>%
          dplyr::bind_rows(neighborhood_year_obs)
      }
    }
    
    
    # Convert the neighborhood_obs into a simple feature  
    neighborhood_obs_sf <- sf::st_as_sf(x = neighborhood_obs,
                                        coords = c("decimalLongitude", "decimalLatitude"),
                                        crs = wgs84)
    
    # Filter to include only the observations within the neighborhood polygon boundaries 
    points_within <- sf::st_within(x = neighborhood_obs_sf, y = neighborhood_poly) %>% lengths > 0
    neighborhood_obs <- neighborhood_obs[points_within, ]
    
    # The resulting data is saved to a CSV file with the filename constructed earlier.
    # If the file already exists or the overwrite flag is not set to TRUE, 
    # then prints a message indicating that the download has been skipped.
    write.csv(x = neighborhood_obs,
              file = neighborhood_file,
              row.names = FALSE)
  }
  else {
    message("Skipping download for ", neighborhood, ", already on disk.")
  }
}
