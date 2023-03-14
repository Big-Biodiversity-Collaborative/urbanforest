# Project: Tucson Urban Tree Equity and Bird Diversity
# Script: Calculate neighborhood bird counts
# Author: Heatherlee Leary, University of Arizona, hleary@arizona.edu


# Load Libraries
require(sf)        # Point filtering
require(raster)    # Work with rasters in R (includes sp package)
library(dplyr)     # Manipuate data frames


# Read in the data
bird_tes_subset <- read.csv(file = "data/gbif_tes_subset.csv", header = TRUE)

# Explore
head(bird_tes_subset)
colnames(bird_tes_subset)


# We need a table containing: 
# Neighborhood Name, Number of Species, TreeEquity, TES, PriorityCa
# Where each row represents observations for each neighborhood

# Remove unnecessary columns
urbanforest_df <- bird_tes_subset[c("species", "NAME", "AreaSqMi", "AREASqKm", "PCTTreeCov", "TreeEquity", "TES", "PriorityCa")]
colnames(urbanforest_df)

# Species per neighborhood (no duplicates)
# Neighborhoods without observations are not included
neighborhood_species <- aggregate(urbanforest_df$species, 
                                  by=list(urbanforest_df$NAME), 
                                  FUN=function(x) paste(unique(x), collapse=", "))

# Explore
head(neighborhood_species)
colnames(neighborhood_species)

# Change column names
colnames(neighborhood_species) <- c("neighborhood", "species.list")
colnames(neighborhood_species)

# Generate a column that counts the number of species listed per neighborhood
neighborhood_species$species.count <- sapply(strsplit(neighborhood_species$species.list, ", "), length)
colnames(neighborhood_species)



# Prep the urbanforest and species dataframes to merge them
tes_prep <- urbanforest_df[c("NAME", "AreaSqMi", "AREASqKm", "PCTTreeCov", "TreeEquity", "TES", "PriorityCa")] # remove species
tes_prep <- distinct(tes_prep) # removes duplicates

bird_prep <- neighborhood_species[c("neighborhood", "species.count")]

# Merge dataframes to get species counts and tree equity data per neighborhood
neighborhood_data <- left_join(tes_prep, bird_prep, by = c("NAME" = "neighborhood"))

# Change column names
colnames(neighborhood_data) <- c("Neighborhood", "AreaSqMi", "AreaSqKm", "PctTreeCov", "TreeEquity", "TES", "PriorityCa", "SpeciesCount")

# Save as csv
write.csv(neighborhood_data, file = "data/urbanforest_data.csv", row.names = FALSE)

