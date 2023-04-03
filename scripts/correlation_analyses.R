# Project: Tucson Urban Tree Equity and Bird Richness
# Script: GLM and Morans I Analyses
# Author: Heatherlee Leary, University of Arizona, hleary@arizona.edu


# Load Libraries
require(sf)        # Point filtering
require(sp)        # Spatial analyses
require(spdep)     # Spatial analyses
require(tidyverse) # Data wrangling
require(ggplot2)   # Data visualization  

# Read in data
urbanforest_geom <- st_read("data/urbanforest_geom.shp")


# Explore
class(urbanforest_geom)
plot(urbanforest_geom)
head(urbanforest_geom)
dim(urbanforest_geom) # 303 neighborhoods


# =====================================================================
# Visualization =======================================================
# =====================================================================


# Visualize bird counts across neighborhoods
ggplot(urbanforest_geom) +
  geom_sf(aes(fill = SpcsCnt), color = "black", lwd = 0.15) +
  scale_fill_gradient(name = "Bird counts",
                      low = "white",
                      high = "blue") +
  ggtitle("Bird Richness in Tucson Neighborhoods") +
  labs(caption = "Data source: GBIF eBird and iNat observations") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# Visualize tree equity across neighborhoods
ggplot(urbanforest_geom) +
  geom_sf(aes(fill = TreEqty), color = "black", lwd = 0.15) +
  scale_fill_gradient(name = "Tree Equity Score",
                      low = "white",
                      high = "darkgreen") +
  ggtitle("Tree Equity Score in Tucson Neighborhoods") +
  labs(caption = "Data source: City of Tucson") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")




# ====================================================================
# Correlation and Regression =========================================
# ====================================================================

# Scatterplot of tree equity vs. species count
ggplot(urbanforest_geom, aes(x = TreEqty, y = SpcsCnt)) +
  geom_point() +
  xlab("Tree Equity") +
  ylab("Species Richness")

# Correlation coefficient
cor(urbanforest_geom$TreEqty, urbanforest_geom$SpcsCnt) # cor 0.038 
cor.test(urbanforest_geom$TreEqty, urbanforest_geom$SpcsCnt) # p-value 0.512

# Fit a linear regression model
model_lm <- lm(SpcsCnt ~ TreEqty, data = urbanforest_geom)
summary(model_lm)
# The p-value for the coefficient of tree equity is 0.512
# The R-squared value of the model is also very low, suggesting that tree equity 
# explains only a very small proportion of the variance in species count.

# There is a weak and non-significant relationship between tree equity and species count. 




# ======================================================================
# Spatial Autocorrelation ==============================================
# ======================================================================

# Create a neighbor list and check for empty neighbor sets
urbanforest_nb <- poly2nb(urbanforest_geom, queen = TRUE)
empty_nb <- which(card(urbanforest_nb) == 0)
cat("Polygons with empty neighbor sets:", paste(empty_nb, collapse = ", "))

# Remove polygons with empty neighbor sets from the spatial data
urbanforest_geom_subset <- urbanforest_geom[-empty_nb, ]

# Create a spatial weights matrix for the subset
nb_subset <- poly2nb(urbanforest_geom_subset, queen = TRUE)
wt <- nb2listw(nb_subset, style = "B")

# Compute the Moran's I statistic to test for spatial autocorrelation
moran.test(urbanforest_geom_subset$TreEqty, wt)
moran.test(urbanforest_geom_subset$SpcsCnt, wt)


# The output of the moran.test() function indicates that there is strong evidence
# of spatial autocorrelation in the TreEqty variable but not in the SpcsCnt variable.
# 
# For TreEqty, the Moran's I statistic is 0.549 and the p-value is less than 2.2e-16, 
# indicating that the observed spatial autocorrelation is significant. 
# The Moran scatterplot also shows a clear positive relationship between each observation 
# and the average of its neighbors, indicating positive spatial autocorrelation.
# 
# For SpcsCnt, the Moran's I statistic is 0.032 and the p-value is 0.1734, indicating that 
# there is not strong evidence of spatial autocorrelation. The Moran scatterplot also shows
# little to no relationship between each observation and the average of its neighbors, 
# indicating little spatial autocorrelation.
# 
# These results suggest that the spatial distribution of TreEqty is more clustered or 
# spatially autocorrelated than the spatial distribution of SpcsCnt.


# Visualize the spatial autocorrelation using Moran scatterplots
moran.plot(urbanforest_geom_subset$TreEqty, wt)
moran.plot(urbanforest_geom_subset$SpcsCnt, wt)


# ======================================================================
# Spatial Clustering ===================================================
# ======================================================================





# =====================================================================
# Visualization =======================================================
# =====================================================================

require(gridExtra) # view multiple plots on the same display

# Create four plots



# Combine the plots into a grid
grid.arrange(plot_count_gi,plot_count_raw,plot_tree_gi,plot_tree_raw, ncol = 2)















