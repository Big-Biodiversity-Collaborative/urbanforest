# Project: Tucson Urban Trees and Biodiversity
# Script: Download bird observation data from GBIF
# Credit: Jeff Oliver jcoliver@arizona.edu
# Heatherlee Leary
# hleary.wildlife@outlook.com
# 2022-09-13


# Libraries
require(dplyr)   # data wrangling
require(osmdata) # city boundaries
require(sf)      # point filtering for cities
source(file = "scripts/query_gbif_birds.R")


# Load city boundary data
citybounds <- read.csv(file = "data/citybounds.csv")

# Indicate whether or not to overwrite data files that already exist
overwrite <- FALSE



# NEED ASSISTANCE
# taxon_keys <- c("Aves" = 212)
# Test with Passeriformes = 729
taxon_keys <- c("Aves" = 212)


# Now download data for each unique city
city_state_string <- paste(citybounds$city, citybounds$state, sep = ", ")
city_state_string <- unique(city_state_string)

for (city_state in city_state_string) {
  # Make a nice name for filename, have to do it twice to avoid double 
  # underscores
  city_name <- tolower(x = gsub(pattern = ", ",
                                replacement = "_",
                                x = city_state))
  city_name <- gsub(pattern = " ",
                    replacement = "_",
                    x = city_name)
  city_file <- paste0("data/gbif/", city_name, "-bird-obs.csv")
  if (overwrite | !file.exists(city_file)) {
    message("***  Downloading data for ", city_state)
    city_poly <- osmdata::getbb(place_name = city_state, format_out = "polygon")
    # Most queries return a list, and we just want the first matrix element; when 
    # a single polygon is returned, it is already a matrix
    if (class(city_poly)[1] == "list") {
      city_poly <- city_poly[[1]]
    }
    # First find the maximum containing rectangle coordinates and use those for 
    # the GBIF query
    min_lon <- min(city_poly[, 1])
    max_lon <- max(city_poly[, 1])
    min_lat <- min(city_poly[, 2])
    max_lat <- max(city_poly[, 2])
    
    
    # Bird obs are too large, so loop over years you want to search for.
    city_obs <- NULL
    for (year_i in 2017:2021) {
    
    city_year_obs <- query_gbif_birds(taxon_keys = taxon_keys,
                           lon_limits = c(min_lon, max_lon),
                           lat_limits = c(min_lat, max_lat),
                           verbose = TRUE,
                           year_range = as.character(year_i))
    
    # Add this year's observations to larger data frame
    if (is.null(city_obs)){
      city_obs <- city_year_obs
    } else {
      city_obs <- rbind(city_obs, city_year_obs)
    }
    }
    
    
    # Convert the polygon to a simple feature for ease of filtering points
    city_sf <- sf::st_polygon(x = list(city_poly), dim = "XY")
    
    # Now make the city_obs into a simple feature  
    wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    city_obs_sf <- sf::st_as_sf(x = city_obs,
                                coords = c("decimalLongitude", "decimalLatitude"),
                                crs = wgs84)
    
    # and use it with sf::st_within; below returns logical vector indicating 
    # whether point is within the polygon; use that vector to select rows from 
    # city_obs that are within the city polygon
    points_within <- sf::st_within(x = city_obs_sf, y = city_sf) %>% lengths > 0
    city_obs <- city_obs[points_within, ]
    write.csv(x = city_obs,
              file = city_file,
              row.names = FALSE)
  }
  else {
    message("Skipping download for ", city_state, ", already on disk.")
  }
}
