# Heatherlee Leary
# hleary.wildlife@outlook.com
# 2022-01-25
# Project: Tucson Urban Trees and Associated (Bird, Butterfly, and Bat) Biodiversity
# Script Objective: Audubon Spring Tucson Bird Count (TBC) Data



##### LOAD LIBRARIES AND IMPORT DATA #####


# Load Libraries
library(tidyverse)
library(tidyr)
library(data.table)

# Read in TBC data
TBC_counts <- read.csv(file="data/TBC_counts.csv")
TBC_routes <- read.csv(file="data/TBC_routes.csv")
TBC_sites <- read.csv(file="data/TBC_sites.csv")
TBC_birds <- read.csv(file="data/TBC_birds.csv", fileEncoding = 'UTF-8-BOM') # Added the ",fileEncoding" to fix the Bite Order Mark

# Reality Check
head(TBC_counts)
head(TBC_routes)
head(TBC_sites)
head(TBC_birds)



##### MERGE SOURCE DATA #####

# Merge TBC_counts with TBC_birds using species name.
TBC_birdcounts <- merge(TBC_counts, TBC_birds, by.x = "species", by.y = "common.name", all = TRUE)


# Urban sites are indicated in TBC_routes (r.urban = 1)
# Remove unnecessary data from TBC_routes before merging.
# Must read in TBC_counts again to restore original form.
# Otherwise, the modified version will persist in this script.
TBC_routes[,c('firstyr','lastyr','n.sites','n.sites.ret','n.sites.current','n.years','r.parks')] <- list(NULL)

# Merge TBC_birdcounts with TBC_routes using routeID.
# I'm ok with overwriting, so I'll use the same name.
TBC_birdcounts <- merge(TBC_birdcounts, TBC_routes, by.x = "routeID", by.y = "routeID", all = TRUE)

# Aquatic birds are not useful in this study, so
# Remove where diet = aquatic
TBC_birdcounts <- TBC_birdcounts[!TBC_birdcounts$diet=="Aquatic",]

# Reality Check
head(TBC_birdcounts)

# Save as CSV. 
write.csv(x=TBC_birdcounts, file="output/TBC_birdcounts.csv")



##### RELATIVE ABUNDANCE #####

# Relative Abundance = Total Counts / Site
abundance_total <- TBC_birdcounts %>%
  group_by(siteID) %>%
  summarise(totalcount=sum(count)) %>%
  ungroup() # ungroup, otherwise groupings will persist.

# Add location data
# Remove unnecessary data from TBC_sites before merging.
# Must read in TBC_sites again to restore original form.
# Otherwise, the modified version will persist in this script.
TBC_sites[,c('firstyr','lastyr','n.years','n.visits','retired','transect')] <- list(NULL)

# Merge TBC_sites with abundance_total
# I'm ok with overwriting, so I'll use the same name.
abundance_total <- merge(abundance_total, TBC_sites, by.x = "siteID", by.y = "siteID", all = TRUE)

# Parse Non/Native
abundance_native <- TBC_birdcounts %>%
  group_by(siteID,native.US)%>%
  summarise(totalcount=sum(count)) %>%
  ungroup() # ungroup, otherwise groupings will persist.

# Add location data
# I'm ok with overwriting, so I'll use the same name.
abundance_native <- merge(abundance_native, TBC_sites, by.x = "siteID", by.y = "siteID", all = TRUE)

# Parse Diet
abundance_diet <- TBC_birdcounts %>%
  group_by(siteID,diet)%>%
  summarise(totalcount=sum(count)) %>%
  ungroup() # ungroup, otherwise groupings will persist.

# Add location data
# I'm ok with overwriting, so I'll use the same name.
abundance_diet <- merge(abundance_diet, TBC_sites, by.x = "siteID", by.y = "siteID", all = TRUE)

# Reality Check
head(abundance_total)
head(abundance_native)
head(abundance_diet)

# Save as CSV.
write.csv(x=abundance_total, file="output/TBC_abundance_total.csv")
write.csv(x=abundance_native, file="output/TBC_abundance_native.csv")
write.csv(x=abundance_diet, file="output/TBC_abundance_diet.csv")



##### SPECIES RICHNESS #####

# Richness = Number of Species / Site
richness_total <- TBC_birdcounts %>%
  distinct(siteID,species) %>%
  count(siteID)

# Add location data
# I'm ok with overwriting, so I'll use the same name.
richness_total <- merge(richness_total, TBC_sites, by.x = "siteID", by.y = "siteID", all = TRUE)

# Parse Non/Native
richness_native <- TBC_birdcounts %>%
  distinct(siteID,species,native.US) %>%
  count(siteID, native.US)

# Add location data
# I'm ok with overwriting, so I'll use the same name.
richness_native <- merge(richness_native, TBC_sites, by.x = "siteID", by.y = "siteID", all = TRUE)

# Parse Diet
richness_diet <- TBC_birdcounts %>%
  distinct(siteID,species,diet) %>%
  count(siteID, diet)

# Add location data
# I'm ok with overwriting, so I'll use the same name.
richness_diet <- merge(richness_diet, TBC_sites, by.x = "siteID", by.y = "siteID", all = TRUE)

# Reality Check
head(richness_total)
head(richness_native)
head(richness_diet)

# Save as CSV.
write.csv(x=richness_total, file="output/TBC_richness_total.csv")
write.csv(x=richness_native, file="output/TBC_richness_native.csv")
write.csv(x=richness_diet, file="output/TBC_richness_diet.csv")

