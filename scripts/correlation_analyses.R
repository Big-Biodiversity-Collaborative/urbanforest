# Project: Tucson Urban Tree Equity and Bird Richness
# Script: Correlation and Regression
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
dim(urbanforest_geom)     


# =====================================================================
# Preliminary Visualization ===========================================
# =====================================================================

# Visualize data as histogram
# Tree equity histogram
hist(urbanforest_geom$treEqty,
     xlab = "Tree Equity Score",
     ylab = "Frequency",
     main = "Histogram of Tree Equity Scores")

# Avian species richness histogram
hist(urbanforest_geom$spcsRch,
     xlab = "Avian Species Richness",
     ylab = "Frequency",
     main = "Histogram of Avian Species Richness")

# Avian species richness per sq Km histogram
hist(urbanforest_geom$rchPrSK,
     xlab = "Avian Species Richness per sq Km",
     ylab = "Frequency",
     main = "Histogram of Avian Species Richness per sq Km")


# Median value
median(urbanforest_geom$treEqty) 
median(urbanforest_geom$spcsRch)
median(urbanforest_geom$rchPrSK) 

# Range values
range(urbanforest_geom$treEqty) 
range(urbanforest_geom$spcsRch)
range(urbanforest_geom$rchPrSK) 


# Visualize avian species richness (raw) across neighborhoods
ggplot(urbanforest_geom) +
  geom_sf(aes(fill = spcsRch), color = "black", lwd = 0.15) +
  scale_fill_gradient(name = "Species Richness",
                      low = "white",
                      high = "blue") +
  ggtitle("Raw Species Richness of Tucson Neighborhoods") +
  labs(caption = "Data source: GBIF eBird and iNat observations") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")


# Visualize avian species richness/sqkm across neighborhoods
ggplot(urbanforest_geom) +
  geom_sf(aes(fill = rchPrSK), color = "black", lwd = 0.15) +
  scale_fill_gradient(name = "Richness per Sq Km",
                      low = "white",
                      high = "blue") +
  ggtitle("Richness per Sq Km of Tucson Neighborhoods") +
  labs(caption = "Data source: GBIF eBird and iNat observations") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")


# Visualization of rchPerSK
# gradient color scale may not be the most appropriate, 
# since it may exaggerate differences between the higher values 
# and obscure differences between the lower values.
# So...
library(classInt)
library(viridis)

# Determine natural breaks using the "classInt" package
breaks <- classIntervals(urbanforest_geom$rchPrSK, n = 8, style = "jenks")

# Create a factor variable for the natural breaks
urbanforest_geom$breaks <- as.factor(cut(urbanforest_geom$rchPrSK, breaks$brks, include.lowest = TRUE, labels = FALSE))

# Create a discrete color scale with 8 colors
colors <- viridis_pal(option = "D")(8)

# Create custom labels for the breaks
labels <- sprintf("%.2f - %.2f", breaks$brks[-length(breaks$brks)], breaks$brks[-1])

# Plot the map with the natural breaks and discrete color scale
ggplot(urbanforest_geom) +
  geom_sf(aes(fill = breaks), color = "black", lwd = 0.15) +
  scale_fill_viridis_d(name = "Richness per Sq Km",
                       option = "D",
                       direction = -1,
                       na.value = "gray",
                       labels = labels) +
  ggtitle("Richness per Sq Km of Tucson Neighborhoods") +
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
        legend.position = "right")


# Scatterplot of tree equity vs. rich/sqKm
ggplot(urbanforest_geom, aes(x = treEqty, y = rchPrSK)) +
  geom_point() +
  ggtitle("Relationship Between Tree Equity and Richness per km2") +
  xlab("Tree Equity") +
  ylab("Species Richness / km2")



# ====================================================================
# Generalized Linear Model ===========================================
# ====================================================================

# Fit Poisson regression model with offset
# Essentially models the number of species per unit area
model_poisson <- glm(spcsRch ~ treEqty + offset(log(areSqKm)),
                     data = urbanforest_geom, family = poisson)

# View summary of model results
summary(model_poisson)

# Calculate percentage change for tree equity coefficient
(coef_as_percent <- (exp(model_poisson$coefficients["treEqty"]) - 1) * 100)

# Calculate the confidence intervals for the coefficients
ci <- confint(model_poisson)

# Exponentiate the lower and upper CI values for the tree equity coefficient
exp_ci_treEqty <- exp(ci["treEqty", ])

# Print the exponentiated CI values
print(exp_ci_treEqty)



# ====================================================
# Show model results as a table ======================
# ====================================================

library(knitr)

# Extract the coefficients, standard errors, z-values, and p-values from the summary object
coefficients <- coef(summary(model_poisson))[, 1]
standard_errors <- coef(summary(model_poisson))[, 2]
z_values <- coef(summary(model_poisson))[, 3]
p_values <- coef(summary(model_poisson))[, 4]

# Format the p-values using format.pval()
p_values_formatted <- format.pval(p_values, digits = 4)

# Assemble the results into a table using kable()
results_table <- data.frame(coefficients, standard_errors, z_values, p_values_formatted)
rownames(results_table) <- c("(Intercept)", "treEqty")
colnames(results_table) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
kable(results_table, format = "markdown")



library(openxlsx)

# Export the table to an Excel file
write.xlsx(results_table, file = "results_table.xlsx", sheetName = "GLM Results", rowNames = TRUE)



# ====================================================
# Plot the model predictions =========================
# ====================================================

# Create a new column that
# Calculates the predicted species richness for each neighborhood 
# based on the Poisson regression model results.
urbanforest_geom <- urbanforest_geom %>%
  mutate(predicted_species_richness = exp(1.4763451 + 0.0319357 * treEqty))

# Visualize using natural breaks
#library(classInt)

# Calculate the natural breaks (Jenks breaks) for the predicted_species_richness values
n_breaks <- 5
jenks_breaks <- classIntervals(urbanforest_geom$predicted_species_richness,
                               n = n_breaks, style = "jenks")$brks

# Create breaks and labels for the highest and lowest Jenks breaks
min_max_jenks_breaks <- c(jenks_breaks[1], jenks_breaks[length(jenks_breaks)])
min_max_jenks_labels <- format(min_max_jenks_breaks, digits = 2, scientific = FALSE)

# Create the choropleth map with natural breaks and the highest and lowest values on the legend
# library(viridis)
ggplot() +
  geom_sf(data = urbanforest_geom, 
          aes(fill = predicted_species_richness, geometry = geometry),
          color = "black", size = 0.1) +
  scale_fill_viridis(option = "magma", # or add trans="log"
                     name = "Predicted\nSpecies Richness",
                     breaks = min_max_jenks_breaks,
                     labels = min_max_jenks_labels) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Predicted Avian Species Richness in Neighborhoods")



# Plot histogram of predicted species richness
hist(urbanforest_geom$predicted_species_richness,
     xlab = "Predicted Species Richness",
     ylab = "Frequency",
     main = "Histogram of Predicted Species Richness")





# ====================================================
# Model Diagnostics ==================================
# ====================================================


# I wanted to assess goodness-of-fit, but
# Too much noise in the data.
# Never going to get great model fit without building 
# a more complex model that includes other covariates




# ====================================================================
# Simple Linear Regression ===========================================
# ====================================================================

# NOTE: Didn't use this model.
# Keeping in the script for future reference or modification. 


# Species richness per square kilometer (rchPrSK) 
# as a function of tree equity (treEqty)

# Fit a simple linear regression model
model_lm <- lm(rchPrSK ~ treEqty, data = urbanforest_geom)

# Check model summary
summary(model_lm)

# Create a scatterplot with a fitted line
# Add the summary information as a subtitle
summary_text <- paste("Linear regression model summary:",
                      "\nEstimate (treEqty) =", round(coef(model_lm)[2], 4),
                      "\nP-value (treEqty) =", round(summary(model_lm)$coefficients[2, 4], 4),
                      "\nR-squared =", round(summary(model_lm)$r.squared, 4),
                      "\nAdjusted R-squared =", round(summary(model_lm)$adj.r.squared, 4))


# Display the plot with the summary information
ggplot(urbanforest_geom, aes(x = treEqty, y = rchPrSK)) +
  geom_point(position = "jitter", alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  annotate("text", x = 50, y = 500, label = paste("Slope = ", round(coef(model_lm)[2], 3))) +
  ggtitle("Relationship between Tree Equity and Bird Richness per SqKm") +
  xlab("Tree Equity") +
  ylab("Species Richness per SqKm")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  labs(subtitle = summary_text)






