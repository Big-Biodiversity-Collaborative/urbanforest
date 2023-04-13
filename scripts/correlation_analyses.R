# Project: Tucson Urban Tree Equity and Bird Richness
# Script: GLM and Morans I Analyses
# Author: Heatherlee Leary, University of Arizona, hleary@arizona.edu


# Load packages
require(sf)        # Point filtering
require(sp)        # Spatial analyses
require(spdep)     # Spatial analyses
require(tidyverse) # Data wrangling
require(ggplot2)   # Data visualization  

# Read in data
urbanforest_geom <- st_read("output/shapefiles/subset_both_data.shp")


# Explore
class(urbanforest_geom)
plot(urbanforest_geom)
head(urbanforest_geom)
dim(urbanforest_geom)          # 297 neighborhoods


# =====================================================================
# Visualization =======================================================
# =====================================================================

# Visualize data as histogram
hist(urbanforest_geom$treEqty) # tree equity
hist(urbanforest_geom$spcsRch) # bird richness  

# Median value
median(urbanforest_geom$treEqty) # tree equity
median(urbanforest_geom$spcsRch) # bird richness



# Visualize bird richness across neighborhoods
ggplot(urbanforest_geom) +
  geom_sf(aes(fill = spcsRch), color = "black", lwd = 0.15) +
  scale_fill_gradient(name = "Richness",
                      low = "white",
                      high = "blue") +
  ggtitle("Bird Richness of Tucson Neighborhoods") +
  labs(caption = "Data source: GBIF eBird and iNat observations") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")

# Visualize tree equity across neighborhoods
ggplot(urbanforest_geom) +
  geom_sf(aes(fill = treEqty), color = "black", lwd = 0.15) +
  scale_fill_gradient(name = "Tree Equity Score",
                      low = "white",
                      high = "darkgreen") +
  ggtitle("Tree Equity Score in Tucson Neighborhoods") +
  labs(caption = "Data source: City of Tucson") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")


# Scatterplot of tree equity vs. species count
ggplot(urbanforest_geom, aes(x = treEqty, y = spcsRch)) +
  geom_point() +
  ggtitle("Relationship Between Tree Equity and Bird Richness") +
  xlab("Tree Equity") +
  ylab("Species Richness")



# ====================================================================
# Correlation and Regression =========================================
# ====================================================================

# Load packages
require(car)
require(MASS)


# Transform tree equity variable
urbanforest_geom$treEqty_log <- log(urbanforest_geom$treEqty)

# Fit Poisson regression model
model_glm <- glm(spcsRch ~ treEqty_log, data = urbanforest_geom, family = poisson(link = log))

# Check model summary
summary(model_glm)

# Poisson regression of natural log tree equity vs. species count 
# with jitter, abline, and slope text label
ggplot(urbanforest_geom, aes(x = treEqty_log, y = spcsRch)) +
  geom_point(position = "jitter", alpha = 0.5) +
  geom_smooth(method = "glm", formula = y ~ x, se = FALSE, color = "red") +
  annotate("text", x = 3.85, y = 50, label = paste("Slope = ", round(coef(model_glm)[2], 3))) +
  ggtitle("Relationship between Tree Equity and Bird Richness") +
  xlab("Tree Equity (natural log)") +
  ylab("Species Richness")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))



# Assess goodness-of-fit using diagnostic plots and tests
# Plot fitted values vs. residuals
plot(fitted(model_glm), residuals(model_glm, type = "pearson"),
     xlab = "Fitted Values", ylab = "Pearson Residuals", main = "Fitted Values vs. Pearson Residuals")
abline(h = 0, lty = 2)

# Conduct Pearson goodness-of-fit test
pearsonResiduals <- residuals(model_glm, type = "pearson")
hist(pearsonResiduals)

# Create a histogram using ggplot2
ggplot(data.frame(residuals = pearsonResiduals), aes(x = residuals)) +
  geom_histogram(bins = 20, fill = "grey", color = "black") +
  labs(title = "Histogram of Pearson Residuals",
       x = "Pearson Residuals",
       y = "Frequency") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))


pchisq(sum(pearsonResiduals^2), df = df.residual(model_glm), lower.tail = FALSE)

# Check for overdispersion
summary(model_glm)




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
moran.test(urbanforest_geom_subset$treEqty, wt)
moran.test(urbanforest_geom_subset$spcsRch, wt)

# Visualize the spatial autocorrelation using Moran scatterplots
moran.plot(urbanforest_geom_subset$treEqty, wt)
moran.plot(urbanforest_geom_subset$spcsRch, wt)















