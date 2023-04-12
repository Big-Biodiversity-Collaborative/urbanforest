# Project: Tucson Urban Tree Equity and Bird Richness
# Script: Neighborhood GBIF data and calculating species richness
# Author: Heatherlee Leary, University of Arizona, hleary.wildlife@outlook.com


# Here we will identify the total number of GBIF observations and parse by
# data source: eBird or iNaturalist Research Grade Observations. 

# We will also create a data frame and spatial data type (shapefile) for 
# species richness per neighborhood. 



# Load libraries
require(sf)        # Point filtering
require(raster)    # Work with rasters in R (includes sp package)
require(tidyverse) # Manipulate data frames
require(dplyr)

# Read in data
neighborhood_bounds <- st_read("data/shapefiles/NEIGHBORHOODS_ALL.shp")
gbif_df <- read.csv(file = "data/gbif-obs-combined.csv", header = TRUE)


# Convert the sf object to a data frame, select columns, and add row numbers
neighborhood_df <- neighborhood_bounds %>%
  as.data.frame() %>%
  dplyr::select(OBJECTID, NAME, geometry) %>%
  dplyr::mutate(neighborhood_id = row_number())

# View the data frame
head(neighborhood_df)
dim(neighborhood_df) # 466 neighborhoods


# View the gbif data frame
head(gbif_df)

# Number of unique values for neighborhood_id
# Should be 466 neighborhoods
gbif_df %>%
  distinct(neighborhood_id) %>%
  count()

# What is the number of neighborhoods with empty values for species
sum(!gbif_df$neighborhood_id %in% gbif_df$neighborhood_id[gbif_df$species != ""])

any(is.na(gbif_df$species)) # any NA values for species? 
sum(is.na(gbif_df$species)) # how many are there? 


# Join gbif_df and neighborhood_bounds based on neighborhood_id
gbif_df <- gbif_df %>%
  left_join(neighborhood_df, by = "neighborhood_id")

# View the resulting data frame
colnames(gbif_df)
head(gbif_df)

# Confirm 42 neighborhoods with empty species values 
sum(!gbif_df$neighborhood_id %in% gbif_df$neighborhood_id[gbif_df$species != ""])


# Notice 'datasetName' is <NA> for eBird observations, so we will change that to "eBird"
# and the 'datasetName' for iNaturalist observations will be shortened to "iNat"
gbif_df$datasetName <- ifelse(gbif_df$datasetKey == "4fa7b334-ce0d-4e88-aaae-2e0c138d049e", "eBird", gbif_df$datasetName)
gbif_df$datasetName <- ifelse(gbif_df$datasetKey == "50c9509d-22c7-4a22-a47d-8c48425ef4a7", "iNat", gbif_df$datasetName)


# How many total GBIF observations? 
# To answer, we must first remove where species is NA
gbif_df_no_na <- gbif_df %>% 
  filter(!is.na(species))

# Check: any NA values for species?
any(is.na(gbif_df_no_na$species)) 

# Check: there should be 424 neighborhoods remaining
gbif_df_no_na %>%
  distinct(neighborhood_id) %>%
  count()


# How many total GBIF observations? 
dim(gbif_df_no_na)         # 1,746,795 total observations


# ==============================================================================
# There are a total of 1,746,795 GBIF observations,
# located across 424 of 466 neighborhoods in Tucson, Arizona, USA. 
# ==============================================================================

# How many observations are from each data source: eBird and iNat?

# Subset observations
gbif_ebird <- subset(gbif_df_no_na, datasetKey == "4fa7b334-ce0d-4e88-aaae-2e0c138d049e")
gbif_inat <- subset(gbif_df_no_na, datasetKey == "50c9509d-22c7-4a22-a47d-8c48425ef4a7")

# How many observations are in each subset? 
dim(gbif_ebird)     # 1,727,415 (98.89 % of observations are from eBird)
dim(gbif_inat)      # 19,380    (1.11 % of observations are from iNat)

# What percentage of the data does this represent? 
(percent_ebird <- nrow(gbif_ebird) / nrow(gbif_df_no_na) * 100)
(percent_inat <- nrow(gbif_inat) / nrow(gbif_df_no_na) * 100)


# ==============================================================================
# Of the total number of GBIF observations, 
# 1,727,415 (98.89 % of observations are from eBird)
# 19,380    (1.11 % of observations are from iNat)
# ==============================================================================


# # Show the number of eBird, iNat, and total observations for each neighborhood
# library(reshape2)
# gbif_df$count <- 1 # add column that has a value of 1 for each row
# gbif_sum <- dcast(gbif_df, neighborhood_id ~ datasetName, value.var = "count", fill = 0)
# gbif_sum$total <- rich_obs$eBird + rich_obs$iNat




# ==============================================================================
# IMPORTANT:
# We want to aggregate the data by neighborhood; however some neighborhoods 
# have the same name, so instead we can use neighborhood_id
# ==============================================================================


# Species Richness

# Remove unnecessary columns
gbif_prep <- gbif_df[c("neighborhood_id", "NAME", "species", "geometry")]
head(gbif_prep)

# Count the number of unique neighborhoods without any species observations
# There should be 42
sum(!gbif_prep$neighborhood_id %in% gbif_prep$neighborhood_id[gbif_prep$species != ""])



# Generate a list of unique species per neighborhood.
# but we want to keep the neighborhoods with no species values.
gbif_species <- gbif_prep %>%
  group_by(neighborhood_id) %>%
  summarize(unique_species = if(all(is.na(species) | species == "")) {
    NA
  } else {
    paste(unique(species[!is.na(species)]), collapse = ", ")
  },
  .groups = "drop")


# Count the number of unique neighborhoods without any species observations
# There should be 42
sum(!gbif_species$neighborhood_id %in% gbif_species$neighborhood_id[gbif_species$unique_species != ""])

any(is.na(gbif_species$unique_species)) # any NA values for species? 
sum(is.na(gbif_species$unique_species)) # how many are there? 


# View the rows with no value for species
missing_species <- gbif_species %>%
  filter(!neighborhood_id %in% neighborhood_id[unique_species != ""])
print(missing_species)



# Generate a column that counts the number of species listed per neigborhood,
# and if unique_species is NA, then count is NA

require(stringr)

gbif_species <- gbif_species %>%
  mutate(num_species = if_else(is.na(unique_species),
                               NA_real_,
                               str_count(unique_species, ",") + 1))



# Let's check that it worked
require(tibble)
view(gbif_species)       # best to confirm that num_species summed correctly
dim(gbif_species)        # should be 466

# Thorough check that there are 42 rows with no value for species
missing_species <- gbif_species %>%
  filter(!neighborhood_id %in% neighborhood_id[unique_species != ""])
print(missing_species)




# Remove the list of species
gbif_species <- gbif_species[c("neighborhood_id", "num_species")]
colnames(gbif_species)
view(gbif_species)

# # Where num_species is NA, replace with 0
# gbif_species <- gbif_species %>%
#   mutate(num_species = replace_na(num_species, 0))

# replacing NA values in data frame
gbif_species[is.na(gbif_species)] = 0

# Confirm: there should be 42 rows with species count of 0.
sum(gbif_species$num_species == 0)
view(gbif_species)
summary(gbif_species)



# We lost the columns for neighborhood name and geometry, so join them back
# join gbif_species to rich_species by neighborhood_id
rich_obs <- left_join(gbif_species, gbif_prep %>% 
                        select(neighborhood_id, NAME, geometry), by = "neighborhood_id")



# Clean 

# Change column names
colnames(rich_obs) <- c("NID", "speciesRich", "neighborhood", "geometry")

# Reorder columns
rich_obs <- rich_obs %>%
  select(NID, neighborhood, speciesRich, geometry) # reorders the columns

# Remove duplicates
rich_obs <- rich_obs %>%
  distinct(NID, .keep_all = TRUE)

# Check
head(rich_obs)
dim(rich_obs)




# Save as CSV
# The geometry column is for a spatial data type, so we will remove it first
rich_obs_no_geom <- rich_obs %>% 
  select(-geometry)

write.csv(rich_obs_no_geom, file = "output/rich_obs.csv", row.names = FALSE)


# Convert to spatial data type
rich_obs_sf <- st_as_sf(rich_obs)

# Check
class(rich_obs_sf)
st_crs(rich_obs_sf) # CRS is NAD83(HARN)

# Save sf object as a shapefile
st_write(rich_obs_sf, dsn = "output/shapefiles/rich_obs.shp")












