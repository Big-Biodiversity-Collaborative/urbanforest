# Project: Tucson Urban Tree Equity and Bird Diversity
# Script: Download GBIF observation data
# Credit: Jeff Oliver, University of Arizona, jcoliver@arizona.edu
# Author: Heatherlee Leary, University of Arizona, hleary.wildlife@outlook.com

# =======================================================================
# Load Libraries ========================================================
# =======================================================================
require(tidyverse) # Data wrangling (dyplyr package) and visualization (ggplot2 package)
require(sf)        # Point filtering
require(raster)    # Work with rasters in R (includes sp package)
require(rgbif)     # Search and retrieve data from GBIF
source(file = "scripts/query_gbif.R") # Function to query GBIF

# Load boundary data
# Shapefiles obtained from City of Tucson GIS website on 2023-01-14
# Neighborhood data are all in NAD83. GBIF data will be WGS83.
# Tucson Neighborhoods
neighborhood_bounds <- st_read("data/NEIGHBORHOODS_ALL.shp")

# Explore data
# head(neighborhood_bounds)    #### Note that geometry doesn't have separate LAT and LONG columns ####
# class(neighborhood_bounds)   #### Why does it say "sf" and "data.frame" rather than just "data.frame"? ####

# Explore shapefile
# head(neighborhood_bounds)
# class(neighborhood_bounds)   # This is an "sf" type of data frame
# colnames(neighborhood_bounds)
# plot(neighborhood_bounds[,2])

# Indicate whether or not to overwrite data files that already exist
overwrite <- FALSE

# Identify which taxon data to pull from GBIF. 
# Refer to GBIF for the code. Birds (aves) = 212.
taxon_keys <- c("Aves" = 212)

# Now download data for each neighborhood
neighborhood_string <- paste(neighborhood_bounds$NAME)

# Reality check to see that we would loop over each neighborhood 
# for (i in 1:nrow(neighborhood_bounds)) {
#   neighborhood <- neighborhood_bounds$NAME[i]
#   message("downloading for ", neighborhood, ", i = ", i)
# }

for (i in 1:nrow(neighborhood_bounds)) {
  neighborhood <- neighborhood_bounds$NAME[i]
  
  # Construct a filename for the downloaded data using the paste0() function, 
  # which concatenates a string with the values of i and "-bird-obs.csv".  
  neighborhood_file <- paste0("data/gbif/neighborhood-", i, "-bird-obs.csv")
  if (overwrite | !file.exists(neighborhood_file)) {
    message("***  Downloading data for ", neighborhood, " (", i, " of ", length(neighborhood), ")")
    
    # Start by re-projecting the neighborhood polygon to match the CRS of the 
    # GBIF data
    # neighborhood_poly <- neighborhood_bounds$geometry[i]
    wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    neighborhood_poly <- st_transform(neighborhood_bounds$geometry[i], crs= wgs84)
    
    # Find the maximum containing rectangle coordinates and use those for the GBIF query
    poly_bounds <- st_bbox(neighborhood_poly)
    min_lon <- poly_bounds["xmin"] 
    max_lon <- poly_bounds["xmax"] 
    min_lat <- poly_bounds["ymin"] 
    max_lat <- poly_bounds["ymax"] 

    # Do a search for observations that fall inside the smallest rectangle that 
    # includes this neighborhood
    rectangle_obs <- query_gbif(taxon_keys = taxon_keys,
                                lon_limits = c(min_lon, max_lon),
                                lat_limits = c(min_lat, max_lat),
                                verbose = TRUE)
    
    # Bird obs are too large, so loop over years you want to search for.
    # neighborhood_obs <- NULL
    # for (year_i in 2017:2021) {
    #   
    #   neighborhood_year_obs <- query_gbif(taxon_keys = taxon_keys,
    #                                       lon_limits = c(min_lon, max_lon),
    #                                       lat_limits = c(min_lat, max_lat),
    #                                       verbose = TRUE,
    #                                       year_range = as.character(year_i))
    #   
    #   # Add this year's observations to larger data frame
    #   if ("datsetName" %in% neighborhood_year_obs){
    #     neighborhood_year_obs <- neighborhood_year_obs %>%
    #       dplyr::select(-datasetKey)
    #   }  
    #   if (is.null(neighborhood_obs)){
    #     neighborhood_obs <- neighborhood_year_obs
    #   } else {
    #     neighborhood_obs <- rbind(neighborhood_obs, neighborhood_year_obs)
    #   }
    # }

    # Only proceed if at least one observation was returned for this rectangle
    if (nrow(x = rectangle_obs)) {
      # We have observations for the rectangle that includes this neighborhood; 
      # next we need to exclude any observations that are not within the polygon
      # defining the neighborhood
      
      # Now make the rectangle_obs into a simple feature; necessary to determine 
      # if an individual observation is within the neighborhood polygon
      rectangle_obs_sf <- sf::st_as_sf(x = rectangle_obs,
                                       coords = c("decimalLongitude", "decimalLatitude"),
                                       crs = wgs84)
      
      # and use it with sf::st_within; below returns logical vector indicating 
      # whether point is within the polygon; use that vector to select rows from 
      # rectangle_obs that are within the neighborhood polygon
      points_within <- sf::st_within(x = rectangle_obs_sf, y = neighborhood_poly) %>% lengths > 0
      neighborhood_obs <- rectangle_obs[points_within, ]
      write.csv(x = neighborhood_obs,
                file = neighborhood_file,
                row.names = FALSE)
    }
  }
  else {
    message("Skipping download for ", neighborhood, ", already on disk.")
  }
}
