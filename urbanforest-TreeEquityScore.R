# Heatherlee Leary
# hleary.wildlife@outlook.com
# 2022-08-11
# Project: Tucson Urban Trees and Associated (Bird, Butterfly, and Bat) Biodiversity
# Script Objective: Combine Audubon Tucson Bird Count (TBC) data with Tree Equity Score (TES) data


##### LOAD LIBRARIES AND IMPORT DATA #####

# Load Libraries
library(tidyverse)
library(maps)
library(ggplot2)
library(mapproj)
library(mapdata)
library(rgdal)

# Read in TBC and TES data
abundance <- read.csv(file = "output/TBC_abundance_total.csv") # Accessed 2022-01-25. Originating from https://github.com/ezylstra/TBC_UACollab
richness <- read.csv(file = "output/TBC_richness_total.csv") # Accessed 2022-01-25. Originating from https://github.com/ezylstra/TBC_UACollab
TES <-read.csv(file = "data/Tree_Equity_Scores_Tucson.csv") #File created Jul 21, 2022, 19:17. Originating from https://data-cotgis.opendata.arcgis.com/
TES_shapefile <- readOGR("data/Tucson_Tree_Equity_Scores.shp")

# Reality check
head(abundance)
head(richness)
head(TES)

##### Assign abundance and richness values to neighborhoods (polygons) rather than geographic points #####



