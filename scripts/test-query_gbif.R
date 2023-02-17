# Test query_gbif
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-02-16

require(tidyverse)
require(sf)
source(file = "scripts/query_gbif.R")

taxon_keys <- c("CBTH" = 5231688, # Curve-billed Thrasher
                "VERD" = 2487397,
                "CAWR" = 5231474) # Cactus Wren

# "NEC Speedway & Kolb"
lon_limits <- c("xmin" = -110.8418, "xmax" = -110.8366)
lat_limits <- c("ymin" = 32.23557, "ymax" = 32.24053)

# Multiple species
gbif_mult <- query_gbif(taxon_keys = taxon_keys,
                        lon_limits = lon_limits,
                        lat_limits = lat_limits,
                        verbose = TRUE)

# Single species (Verdin)
gbif_single <- query_gbif(taxon_keys = taxon_keys[2],
                          lon_limits = lon_limits,
                          lat_limits = lat_limits,
                          verbose = TRUE)

# Testing University of Arizona
neighborhood_bounds <- st_read("data/NEIGHBORHOODS_ALL.shp")
taxon_keys <- c("Aves" = 212)
neighborhood_string <- paste(neighborhood_bounds$NAME)
i <- 6
neighborhood <- neighborhood_bounds$NAME[i]
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
neighborhood_poly <- st_transform(neighborhood_bounds$geometry[i], crs= wgs84)

# Find the maximum containing rectangle coordinates and use those for the GBIF query
poly_bounds <- st_bbox(neighborhood_poly)
min_lon <- poly_bounds["xmin"] 
max_lon <- poly_bounds["xmax"] 
min_lat <- poly_bounds["ymin"] 
max_lat <- poly_bounds["ymax"] 

# Arguments normally passed to query_gbif
lon_limits <- c(min_lon, max_lon)
lat_limits <- c(min_lat, max_lat)
verbose <- TRUE
cols <- c("decimalLatitude", "decimalLongitude",
         "individualCount", "family", "species", "year", 
         "month", "day", "datasetKey", "datasetName", 
         "gbifID", "lifeStage")
dataset_keys <- c("4fa7b334-ce0d-4e88-aaae-2e0c138d049e", 
                 "50c9509d-22c7-4a22-a47d-8c48425ef4a7")
year_range <- ("2017,2021")
max_attempts <- 5
# At this point, hop over to query_gbif.R and step through function
