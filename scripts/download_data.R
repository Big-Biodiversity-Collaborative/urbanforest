# Project: Tucson Urban Tree Equity and Bird Diversity
# Script: Download GBIF observation data
# Credit: Jeff Oliver, University of Arizona, jcoliver@arizona.edu
# Heatherlee Leary
# hleary.wildlife@outlook.com
# 2023-01-14


# Libraries
require(tidyverse) # Data wrangling (dyplyr package) and visualization (ggplot2 package)
require(sf)        # Point filtering
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


# Explore shapefile
head(neighborhood_bounds)
class(neighborhood_bounds)   # This is an "sf" type of data frame
colnames(neighborhood_bounds)
plot(neighborhood_bounds[,2])

# Indicate whether or not to overwrite data files that already exist
overwrite <- FALSE   


# Identify which taxon data to pull from GBIF. 
# Refer to GBIF for the code. Birds (aves) = 212.
taxon_keys <- c("Aves" = 212)


# Now download data for each neighborhood
neighborhood_string <- paste(neighborhood_bounds$NAME)
 
for (i in 1:nrow(neighborhood_bounds)) {
  neighborhood <- neighborhood_bounds$NAME[i]
  message("downloading for ", neighborhood, ", i = ", i)
}

for (i in 1:nrow(neighborhood_bounds)) {  
  neighborhood <- neighborhood_bounds$NAME[i]
  # Make a nice name for filename, have to do it twice to avoid double underscores
  # neighborhood_name <- tolower(x = gsub(pattern = ", ",
  #                               replacement = "_",
  #                               x = neighborhood))
  # neighborhood_name <- gsub(pattern = " ",
  #                   replacement = "_",
  #                   x = neighborhood_name)
  # neighborhood_file <- paste0("data/gbif/", neighborhood_name, "-bird-obs.csv")
  neighborhood_file <- paste0("data/gbif/neighborhood-", i, "-bird-obs.csv")
  if (overwrite | !file.exists(neighborhood_file)) {
    message("***  Downloading data for ", neighborhood)
    
    # neighborhood_poly <- neighborhood_bounds$geometry[i]
    wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    neighborhood_poly <- st_transform(neighborhood_bounds$geometry[i], crs= wgs84)
    
    # First find the maximum containing rectangle coordinates and use those for the GBIF query
    poly_bounds <- st_bbox(neighborhood_poly)
    min_lon <- poly_bounds["xmin"] 
    max_lon <- poly_bounds["xmax"] 
    min_lat <- poly_bounds["ymin"] 
    max_lat <- poly_bounds["ymax"] 
    
    
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
    
    
    # Convert the polygon to a simple feature for ease of filtering points
    # neighborhood_sf <- sf::st_polygon(x = list(neighborhood_poly), dim = "XY")   
    
    # Now make the neighborhood_obs into a simple feature  
    neighborhood_obs_sf <- sf::st_as_sf(x = neighborhood_obs,
                                coords = c("decimalLongitude", "decimalLatitude"),
                                crs = wgs84)
    
    # and use it with sf::st_within; below returns logical vector indicating 
    # whether point is within the polygon; use that vector to select rows from 
    # neighborhood_obs that are within the neighborhood polygon
    # points_within <- sf::st_within(x = neighborhood_obs_sf, y = neighborhood_sf) %>% lengths > 0
    points_within <- sf::st_within(x = neighborhood_obs_sf, y = neighborhood_poly) %>% lengths > 0
    neighborhood_obs <- neighborhood_obs[points_within, ]
    write.csv(x = neighborhood_obs,
              file = neighborhood_file,
              row.names = FALSE)
  }
  else {
    message("Skipping download for ", neighborhood, ", already on disk.")
  }
}
