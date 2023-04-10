# Project: Tucson Urban Tree Equity and Bird Richness
# Script: Hotspot Analyses
# Author: Heatherlee Leary, University of Arizona, hleary@arizona.edu


# Load Libraries
library(sf)        # Point filtering
library(sp)        # Spatial analyses
library(sfdep)     # Spatial analyses
library(tidyverse) # Data wrangling
library(ggplot2)   # Data visualization  

# Read in data
urbanforest_geom <- st_read("data/urbanforest_geom.shp")


# View as tibble
urbanforest_geom |> 
  as_tibble() |> 
  View()


# Visualize data as histogram
hist(urbanforest_geom$TreEqty, main = "Distribution of Tree Equity Scores", xlab = "Tree Equity Score", ylab = "Frequency")
hist(urbanforest_geom$SpcsCnt, main = "Distribution of Bird Richness", xlab = "Number of Bird Species", ylab = "Frequency")



# Visualize bird species richness across neighborhoods
bird_raw <- ggplot(urbanforest_geom) +
  geom_sf(aes(fill = SpcsCnt), color = "black", lwd = 0.15) +
  scale_fill_gradient(name = "Bird Richness",
                      low = "white",
                      high = "blue") +
  ggtitle("Bird Richness in Tucson Neighborhoods") +
  labs(caption = "Data source: GBIF eBird and iNat observations") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# Visualize tree equity across neighborhoods
tree_raw <- ggplot(urbanforest_geom) +
  geom_sf(aes(fill = TreEqty), color = "black", lwd = 0.15) +
  scale_fill_gradient(name = "Tree Equity Score",
                      low = "white",
                      high = "darkgreen") +
  ggtitle("Tree Equity Score in Tucson Neighborhoods") +
  labs(caption = "Data source: City of Tucson") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")


# Bird richness appears to be more spread 
# Tree equity appears to have higher values in north Tucson
# Is there statistially significant clustering? 
# To check, we will check for global clustering using getis-ord-gi* 



# ===========================================================================
# Tree Equity Hotspot Analysis ==============================================
# ===========================================================================

# First need to identify neighbors and create weights for polygons
# Row-standardized weights ensures equal weight to each neighborhood polygon

tree_nbs <- urbanforest_geom |> 
  mutate(
    nb = st_contiguity(geometry), # ERROR: empty neighbor sets
    wt = st_weights(nb),
    tree_lag = st_lag(TreEqty, nb, wt)
  ) 


# RESOLVE ERROR: 
# The poly2nb() function creates a neighbors list based on the spatial 
# relationships between the polygons. The queen = TRUE argument specifies 
# that the neighbors list should be based on a queen contiguity criterion, 
# which considers two polygons as neighbors if they share a boundary or a vertex.


# Create a neighbor list and check for empty neighbor sets
urbanforest_nb <- poly2nb(urbanforest_geom, queen = TRUE)
empty_nb <- which(card(urbanforest_nb) == 0)
cat("Polygons with empty neighbor sets:", paste(empty_nb, collapse = ", "))

# Which neighborhoods are the ones with empty neighbor sets? 
# Subset the urbanforest_geom object to extract polygons with empty neighbor sets
empty_polygons <- urbanforest_geom[empty_nb, ]
empty_polygons


# Remove polygons with empty neighbor sets from the spatial data
urbanforest_geom_subset <- urbanforest_geom[-empty_nb, ]

# Error should be resolved, so
# Try again to identify neighbors and create weights
tree_nbs <- urbanforest_geom_subset |> 
  mutate(
    nb = st_contiguity(geometry),        # neighbors share border/vertex
    wt = st_weights(nb),                 # row-standardized weights
    tree_lag = st_lag(TreEqty, nb, wt)   # calculate spatial lag of TreEqty
  ) 


# Visualize spatial lag of tree equity
tree_nbs |> 
  ggplot(aes(fill = tree_lag)) +
  geom_sf(color = "black", lwd = 0.15)

# The lag shows some smoothing and tree equity being a smoother gradient



# Is there any global clustering? 

# The Getis-Ord global G statistic is a measure of spatial autocorrelation, 
# which assesses whether there is clustering. It tells us if nearby values 
# are more simlar than expected under the null. 
global_g_test(tree_nbs$TreEqty, tree_nbs$nb, tree_nbs$wt)

# P-value = 0.03 suggests there is clustering (p-value < 0.05).



# Now looking at local clustering:

# We can calciulate the Gi using local_g_perm().
# Gi is ratio of spatial lag to the sum of values for neighborhood.
# Helps us to find the local clusters.

# Create a new column called "Gi" which is the output of the local_g_perm
# It's a dataframe column, which we can't do any work in, so we need to unnest.
# So, its a column that contains a dataframe, and we're essentially unpacking it

tree_hot_spots <- tree_nbs |> 
  mutate(
    Gi = local_g_perm(TreEqty, nb, wt, nsim = 499)
  ) |> 
  unnest(Gi) 

# Cursory visualization
# Plot looks at gi values for all locations
tree_hot_spots |> 
  ggplot((aes(fill = gi))) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_gradient2() # makes the value 0 be the middle



# Lets classify these in 7 different categories
# very hot (cold), cold (hot), somewhat hot (cold), insignificant

# very = p < 0.01
# cold/hot = p <= 0.05
# somewhat = p <= 0.1

tree_gi <- tree_hot_spots |> 
  select(gi, p_folded_sim) |> 
  mutate(
    classification = case_when(
      gi > 0 & p_folded_sim <= 0.01 ~ "Very hot",
      gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
      gi > 0 & p_folded_sim <= 0.1 ~ "Somewhat hot",
      gi < 0 & p_folded_sim <= 0.01 ~ "Very cold",
      gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
      gi < 0 & p_folded_sim <= 0.1 ~ "Somewhat cold",
      TRUE ~ "Insignificant"
    ),
    # we now need to make it look better :) 
    # if we cast to a factor we can make diverging scales easier 
    classification = factor(
      classification,
      levels = c("Very hot", "Hot", "Somewhat hot",
                 "Insignificant",
                 "Somewhat cold", "Cold", "Very cold")
    )
  ) |> 
  ggplot(aes(fill = classification)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  theme_void() +
  labs(
    fill = "Hot Spot Classification",
    title = "Tree Equity Hot Spots in Tucson"
  )





# ===========================================================================
# Bird Richness Hotspot Analysis ============================================
# ===========================================================================

# Identify neighbors and create weights
bird_nbs <- urbanforest_geom_subset |> 
  mutate(
    nb = st_contiguity(geometry),        # neighbors share border/vertex
    wt = st_weights(nb),                 # row-standardized weights
    tree_lag = st_lag(SpcsCnt, nb, wt)   # calculate spatial lag of TreEqty
  ) 


# Visualize spatial lag of tree equity
bird_nbs |> 
  ggplot(aes(fill = tree_lag)) +
  geom_sf(color = "black", lwd = 0.15)

# Gradient is less obvious here, but perhaps clustering in the northwest corner



# Is there any global clustering? 

# The Getis-Ord global G statistic is a measure of spatial autocorrelation, 
# which assesses whether there is clustering. It tells us if nearby values 
# are more simlar than expected under the null. 
global_g_test(bird_nbs$SpcsCnt, bird_nbs$nb, bird_nbs$wt)

# P-value = 0.445. Not statistically significant.



# Now looking at local clustering:

# We can calciulate the Gi using local_g_perm().
# Gi is ratio of spatial lag to the sum of values for neighborhood.
# Helps us to find the local clusters.

# Create a new column called "Gi" which is the output of the local_g_perm
# It's a dataframe column, which we can't do any work in, so we need to unnest.
# So, its a column that contains a dataframe, and we're essentially unpacking it

bird_hot_spots <- bird_nbs |> 
  mutate(
    Gi = local_g_perm(SpcsCnt, nb, wt, nsim = 499)
  ) |> 
  unnest(Gi) 

# Cursory visualization
# Plot looks at gi values for all locations
bird_hot_spots |> 
  ggplot((aes(fill = gi))) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_gradient2() # makes the value 0 be the middle



# Lets classify these in 7 different categories
# very hot (cold), cold (hot), somewhat hot (cold), insignificant

# very = p < 0.01
# cold/hot = p <= 0.05
# somewhat = p <= 0.1

bird_gi <- bird_hot_spots |> 
  select(gi, p_folded_sim) |> 
  mutate(
    classification = case_when(
      gi > 0 & p_folded_sim <= 0.01 ~ "Very hot",
      gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
      gi > 0 & p_folded_sim <= 0.1 ~ "Somewhat hot",
      gi < 0 & p_folded_sim <= 0.01 ~ "Very cold",
      gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
      gi < 0 & p_folded_sim <= 0.1 ~ "Somewhat cold",
      TRUE ~ "Insignificant"
    ),
    # we now need to make it look better :) 
    # if we cast to a factor we can make diverging scales easier 
    classification = factor(
      classification,
      levels = c("Very hot", "Hot", "Somewhat hot",
                 "Insignificant",
                 "Somewhat cold", "Cold", "Very cold")
    )
  ) |> 
  ggplot(aes(fill = classification)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  theme_void() +
  labs(
    fill = "Hot Spot Classification",
    title = "Bird Richness Hot Spots in Tucson"
  )




# =====================================================================
# Visualization =======================================================
# =====================================================================

library(gridExtra) # view multiple plots on the same display

# Combine the plots into a grid
gg <- grid.arrange(tree_raw,bird_raw, ncol = 2)
gg

# Save the plot as a PNG file
ggsave("output/combined_plot.png", gg, width = 10, height = 8, dpi = 300)

 











