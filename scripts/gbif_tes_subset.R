# Project: Tucson Urban Tree Equity and Bird Richness
# Script: Combine neighborhood TES data with gbif bird data
# Author: Heatherlee Leary, University of Arizona, hleary@arizona.edu


# Load libraries
require(sf)        # Point filtering
require(raster)    # Work with rasters in R (includes sp package)
require(dplyr)     # Manipulate data frames


# Combine tables to connect bird obs with neighborhood name and neighborhood id
neighborhood_table <- read.csv(file = "data/neighborhood_id_table.csv", header = TRUE)
gbif_table <- read.csv(file = "data/gbif-bird-obs-combined.csv", header = TRUE)

# Explore
head(neighborhood_table)
head(gbif_table)

# Join the bird_table and neighborhood_table data frames based on neighborhood_id
gbif_table <- gbif_table %>%
  left_join(neighborhood_table, by = "neighborhood_id")

# View the resulting table
colnames(gbif_table)


# Neighborhoods with TES scores
tes_bounds <- st_read("data/shapefiles/Tucson_Tree_Equity_Scores.shp")


# Connect TES data with bird data
gbif_tes_table <- left_join(gbif_table, tes_bounds, by = c("NAME" = "NAME"))
head(gbif_tes_table)
colnames(gbif_tes_table)


# Clean the gbif_tes_table data frame

# Filter rows where TreeEquity and TES is not NA
gbif_tes_table <- subset(gbif_tes_table, !is.na(TreeEquity) & !is.na(TES))
head(gbif_tes_table)

# Remove unnecessary columns
gbif_tes_subset <- gbif_tes_table[c("decimalLatitude", "decimalLongitude", "family", "species", "year", "month", "day", "datasetKey", "datasetName", "gbifID", "NAME","AREASqKm", "AreaSqMi", "PCTTreeCov", "TreeEquity", "TES", "PriorityCa")]
colnames(gbif_tes_subset)


# datasetName indicates iNat data but shows <NA> for eBird
# Where datasetKey is for eBird, change the datasetName to "eBird"
gbif_tes_subset$datasetName <- ifelse(gbif_tes_subset$datasetKey == "4fa7b334-ce0d-4e88-aaae-2e0c138d049e", "eBird", gbif_tes_subset$datasetName)
tail(gbif_tes_subset)
head(gbif_tes_subset)


# Save as CSV
write.csv(gbif_tes_subset, file = "data/gbif_tes_subset.csv", row.names = FALSE)

