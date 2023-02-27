# Processing of two large manually downloaded datasets
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-02-26

require(sf)
require(dplyr)

# Identify two neighborhoods that need to be manually downloaded
large_neighborhoods <- c("Silverbell Golf Course", "Flowing Wells")

# Load in neighborhood polygons
neighborhood_bounds <- st_read("data/NEIGHBORHOODS_ALL.shp")

# Read in a smaller dataset for column names to save
data_1 <- read.csv(file = "data/gbif/neighborhood-1-bird-obs.csv")
data_cols <- colnames(data_1)

# CRS for transformation
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Iterate over neighborhoods
for (neighborhood in large_neighborhoods) {
  nice_neighborhood <- tolower(gsub(pattern = " ",
                                    replacement = "-",
                                    x = neighborhood))
  neighborhood_file <- paste0("data/gbif/", nice_neighborhood, ".tsv")
  
  # Need to find index of this neighborhood for file name or coordinate 
  # retrieval
  neighborhood_i <- which(x = neighborhood_bounds$NAME == neighborhood)
  
  # See if file is on disk. If not, alert user and stop
  if (file.exists(neighborhood_file)) {
    message("Processing ", neighborhood)
    
    # Load file into memory
    rectangle_obs <- read.delim(file = neighborhood_file)
    
    # Convert to simple feature
    rectangle_obs_sf <- sf::st_as_sf(x = rectangle_obs,
                                     coords = c("decimalLongitude", "decimalLatitude"),
                                     crs = wgs84)
    # See which points are within the polygon
    points_within <- sf::st_within(x = rectangle_obs_sf, y = neighborhood_poly) %>% lengths > 0
    neighborhood_obs <- rectangle_obs[points_within, ]

    # We only need to write a subset of the columns
    neighborhood_obs <- neighborhood_obs %>%
      dplyr::select(dplyr::all_of(intersect(colnames(neighborhood_obs), data_cols)))
    
    neighborhood_file <- paste0("data/gbif/neighborhood-", neighborhood_i, "-bird-obs.csv")
    
    write.csv(x = neighborhood_obs,
              file = neighborhood_file,
              row.names = FALSE)
  } else {
    message("No file for ", neighborhood, 
            " on disk. Manually download from GBIF:")
    
    # Extract lon/lat rectangle for download
    neighborhood_poly <- st_transform(neighborhood_bounds$geometry[neighborhood_i], 
                                      crs= wgs84)
    
    # Find the maximum containing rectangle coordinates and use those for the GBIF query
    poly_bounds <- st_bbox(neighborhood_poly)
    min_lon <- poly_bounds["xmin"] 
    max_lon <- poly_bounds["xmax"] 
    min_lat <- poly_bounds["ymin"] 
    max_lat <- poly_bounds["ymax"] 
    
    message("Coordinates: (xmin, xmax, ymin, ymax)")
    message(min_lon, ", ", max_lon, ", ", min_lat, ", ", max_lat)
  }
}

# Package up the zip file
neighborhood_files <- list.files(path = "data/gbif",
                                 pattern = "neighborhood-.*-bird-obs.csv",
                                 full.names = TRUE)
zip(zipfile = "data/gbif-bird-obs.zip",
    files = neighborhood_files)

# To extract
# unzip(zipfile = "data/gbif-bird-obs.zip")
