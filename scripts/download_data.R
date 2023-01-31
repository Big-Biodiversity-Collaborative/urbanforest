# Project: Tucson Urban Tree Equity and Bird Diversity
# Script: Download GBIF observation data
# Help from: Jeff Oliver, University of Arizona, jcoliver@arizona.edu
# Heatherlee Leary
# hleary.wildlife@outlook.com
# 2023-01-14


# Libraries
require(tidyverse) # Data wrangling (dyplyr package) and visualization (ggplot2 package)
require(sf)        # Point filtering for cities
require(raster)    # Work with rasters in R (includes sp package)
require(rgbif)     # Search and retrieve data from GBIF
source(file = "scripts/query_gbif.R")


# Load boundary data
# Shapefiles obtained from City of Tucson GIS website on 2023-01-14
# Tucson Neighborhoods
neighborhood_bounds <- st_read("data/NEIGHBORHOODS_ALL.shp") # This is NAD83. GBIF data will be WGS83.

# Explore data
head(neighborhood_bounds)    #### Note that geometry doesn't have separate LAT and LONG columns ####
class(neighborhood_bounds)   #### Why does it say "sf" and "data.frame" rather than just "data.frame"? ####
colnames(neighborhood_bounds)
plot(neighborhood_bounds[,2])

# Indicate whether or not to overwrite data files that already exist
overwrite <- FALSE   #### What does this mean? ####


# Identify which taxon data to pull from GBIF. 
# Refer to GBIF for the code. Birds (aves) = 212.
taxon_keys <- c("Aves" = 212)


# Now download data for each unique neighborhood
neighborhood_string <- paste(neighborhood_bounds$NAME)  ##### Isn't data already downloaded for each neighborhood? ####
neighborhood_string <- unique(neighborhood_string)


#### Why are we doing the name of the file now? Isn't that best to save for the end? ####
for (neighborhood in neighborhood_string) {  
  # Make a nice name for filename, have to do it twice to avoid double underscores
  neighborhood_name <- tolower(x = gsub(pattern = ", ",
                                replacement = "_",
                                x = neighborhood))
  neighborhood_name <- gsub(pattern = " ",
                    replacement = "_",
                    x = neighborhood_name)
  neighborhood_file <- paste0("data/gbif/", neighborhood_name, "-bird-obs.csv")
  if (overwrite | !file.exists(neighborhood_file)) {
    message("***  Downloading data for ", neighborhood_name)
    
    neighborhood_poly <- osmdata::getbb(place_name = neighborhood_name, format_out = "polygon") ##### Need to extract just one neighborhood? ####
  
    
    # First find the maximum containing rectangle coordinates and use those for the GBIF query
    min_lon <- min(neighborhood_poly[, 1]) #### Pull from "geometry" column, but lat and long are written together in the df ####
    max_lon <- max(neighborhood_poly[, 1])
    min_lat <- min(neighborhood_poly[, 2])
    max_lat <- max(neighborhood_poly[, 2])
    
    
    # Bird obs are too large, so loop over years you want to search for.
    neighborhood_obs <- NULL
    for (year_i in 2017:2021) {
    
      neighborhood_year_obs <- query_gbif(taxon_keys = taxon_keys,
                           lon_limits = c(min_lon, max_lon),
                           lat_limits = c(min_lat, max_lat),
                           verbose = TRUE,
                           year_range = as.character(year_i))
    
    # Add this year's observations to larger data frame
    if (is.null(neighborhood_obs)){
      neighborhood_obs <- neighborhood_year_obs
    } else {
      neighborhood_obs <- rbind(neighborhood_obs, neighborhood_year_obs)
    }
    }
    
    
    # Convert the polygon to a simple feature for ease of filtering points
    neighborhood_sf <- sf::st_polygon(x = list(neighborhood_poly), dim = "XY")    ##### NECESSARY? ####
    
    # Re-Project neighborhood_sf into WGS84 to match neighborhood_obs
    wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    neighborhood_sf <- st_transform(neighborhood_sf, crs = wgs84)
    
    
    # Now make the neighborhood_obs into a simple feature  
    neighborhood_obs_sf <- sf::st_as_sf(x = neighborhood_obs,
                                coords = c("decimalLongitude", "decimalLatitude"),
                                crs = wgs84)
    
    # and use it with sf::st_within; below returns logical vector indicating 
    # whether point is within the polygon; use that vector to select rows from 
    # neighborhood_obs that are within the neighborhood polygon
    points_within <- sf::st_within(x = neighborhood_obs_sf, y = neighborhood_sf) %>% lengths > 0
    neighborhood_obs <- neighborhood_obs[points_within, ]
    write.csv(x = neighborhood_obs,
              file = neighborhood_file,
              row.names = FALSE)
  }
  else {
    message("Skipping download for ", neighborhood, ", already on disk.")
  }
}
