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

# For some of the larger downloads, things may fail, so we can keep track of 
# which ones do fail in this log file
logfile <- "logs/download-data-log.txt"
write(x = paste0("Start time ", Sys.time()),
      file = logfile,
      append = FALSE)

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

# Reality check to see that we would loop over each neighborhood 
# for (i in 1:nrow(neighborhood_bounds)) {
#   neighborhood <- neighborhood_bounds$NAME[i]
#   message("downloading for ", neighborhood, ", i = ", i)
# }

# Some neighborhoods have too many observations in the time span to download
# all at once, instead, do these year by year
single_year_neighborhoods <- c("Davis-Monthan", "Flowing Wells", 
                               "Silverbell Golf Course", "Menlo Park", 
                               "Blenman-Elm", "Mesquite Ranch", "Aviation ROW", 
                               "Miramonte", "Flowing Wells", "Las Vistas", 
                               "Reid - Randolph")

for (i in 1:nrow(neighborhood_bounds)) {
  neighborhood <- neighborhood_bounds$NAME[i]
  
  # Construct a filename for the downloaded data using the paste0() function, 
  # which concatenates a string with the values of i and "-bird-obs.csv".  
  neighborhood_file <- paste0("data/gbif/neighborhood-", i, "-bird-obs.csv")
  if (overwrite | !file.exists(neighborhood_file)) {
    message("***  Downloading data for ", neighborhood, " (", i, " of ", nrow(neighborhood_bounds), ")")
    
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

    # Some neighborhoods we know have lots of observations (>100K), so we want
    # to download data year-by-year
    if (neighborhood %in% single_year_neighborhoods) {
      # year_i <- 2017
      failed <- FALSE
      rectangle_obs <- NULL
      message("...large neighborhood, so downloading one year at a time")
      # while(year_i <= 2021 & !failed) {
      for (year_i in 2017:2021) {
        # First see if this year already has (temporary) data written to disk
        year_file <- paste0("data/gbif/neighborhood-", i, "-", year_i, "-temp.csv")
        if (file.exists(year_file)) {
          message("Reading ", neighborhood, " data for ", year_i, " from disk.")
          rectangle_year_obs <- read.csv(file = year_file)
          # Will read in GBIF id as a numeric, but needs to be char
          rectangle_year_obs$gbifID <- as.character(rectangle_year_obs$gbifID)
          # Add this year's observations to larger data frame (if it exists)
          if (is.null(rectangle_obs)){
            rectangle_obs <- rectangle_year_obs
          } else {
            rectangle_obs <- dplyr::bind_rows(rectangle_obs, rectangle_year_obs)
          }
        } else {
          # Year file does not exist, need to run query
          message("...Year: ", year_i)
          rectangle_year_obs <- query_gbif(taxon_keys = taxon_keys,
                                           lon_limits = c(min_lon, max_lon),
                                           lat_limits = c(min_lat, max_lat),
                                           verbose = TRUE,
                                           year_range = as.character(year_i))
          # So long as a non-null value is returned, add to the data frame
          # We also only proceed with merging dataframes if none of the prior 
          # year queries have failed. If they *did*, we will need to run this 
          # (at least) one more time to get the year that failed, so no need to
          # add to the larger data frame which will get over-written regardless
          if (!is.null(rectangle_year_obs) & !failed) {
            # Add this year's observations to larger data frame
            if (is.null(rectangle_obs)){
              rectangle_obs <- rectangle_year_obs
            } else {
              rectangle_obs <- dplyr::bind_rows(rectangle_obs, rectangle_year_obs)
            }
            # Write this year as a (temporary) file to disk; we do this so even
            # if one year fails, we do not have to re-run queries for all years
            write.csv(x = rectangle_year_obs,
                      file = year_file,
                      row.names = FALSE)
          } else {
            # One year query failed; for now makes this whole dataset fail
            rectangle_obs <- rectangle_year_obs
            failed <- TRUE
          }
        }
        # year_i <- year_i + 1
      } # End while/iterating over years
    } else {
      # Do a search for observations that fall inside the smallest rectangle that 
      # includes this neighborhood
      rectangle_obs <- query_gbif(taxon_keys = taxon_keys,
                                  lon_limits = c(min_lon, max_lon),
                                  lat_limits = c(min_lat, max_lat),
                                  verbose = TRUE)
    }

    # If query is not NULL it "succeeded" - which may still mean zero records
    if (!is.null(rectangle_obs)) {
      # Only proceed if at least one observation was returned for this rectangle
      if (nrow(x = rectangle_obs) > 0) {
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
      } else {
        # Even if no observations are returned, we still want to write a file, for 
        # subsequent processing? Maybe? For now we set up an empty data frame that 
        # will be replaced with non-NULL queries
        neighborhood_obs <- data.frame(decimalLatitude = numeric(),
                                       decimalLongitude = numeric(),
                                       individualCount = integer(), 
                                       family = character(), 
                                       species = character(), 
                                       year = integer(), 
                                       month = integer(), 
                                       day = integer(), 
                                       datasetKey = character(), 
                                       datasetName = character(), 
                                       gbifID = integer(), 
                                       lifeStage = character())
      }
      write.csv(x = neighborhood_obs,
                file = neighborhood_file,
                row.names = FALSE)
    } else {
      # Query returned NULL, which most likely means something timed out and we
      # did not get a complete sample (or even any sample). Make a note of this
      write(x = paste0("Unsuccessful download for ", neighborhood, 
                       " (id = ", i, ")"),
            file = logfile,
            append = TRUE)
    }
  }
  else {
    message("Skipping download for ", neighborhood, ", already on disk.")
  }
}

# Finally, put all these in a zip folder than can be extracted later
neighborhood_files <- list.files(path = "data/gbif",
                                 pattern = "neighborhood-.*-bird-obs.csv",
                                 full.names = TRUE)
zip(zipfile = "data/gbif-bird-obs.zip",
    files = neighborhood_files)

# To extract
# unzip(zipfile = "data/gbif-bird-obs.zip")
