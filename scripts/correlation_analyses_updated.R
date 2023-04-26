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
hist(urbanforest_geom$treEqty) # tree equity
hist(urbanforest_geom$spcsRch) # avian species richness
hist(urbanforest_geom$rchPrSK) # avian species richness per sq Km 

# Median value
median(urbanforest_geom$treEqty) 
median(urbanforest_geom$rchPrSK) 



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

# Load packages
require(car)
require(MASS)


# Fit Poisson regression model with offset
# Essentially models the number of species per unit area
model_poisson <- glm(spcsRch ~ treEqty + offset(log(areSqKm)),
                     data = urbanforest_geom, family = poisson)

# View summary of model results
summary(model_poisson)


# Assess goodness-of-fit using diagnostic plots and tests
# Plot fitted values vs. residuals
plot(fitted(model_poisson), residuals(model_poisson, type = "pearson"),
     xlab = "Fitted Values", ylab = "Pearson Residuals", main = "Fitted Values vs. Pearson Residuals")
abline(h = 0, lty = 2)

# Conduct Pearson goodness-of-fit test
pearsonResiduals <- residuals(model_poisson, type = "pearson")
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


# P-value for Pearson goodness-of-fit test
pchisq(sum(pearsonResiduals^2), df = df.residual(model_poisson), lower.tail = FALSE)


# Check for overdispersion
require(performance)
check_overdispersion(model_poisson)

# Calculate residual deviance and degrees of freedom
residual_deviance <- deviance(model_poisson, type = "residual")
degrees_of_freedom <- df.residual(model_poisson)

# Compare residual deviance to degrees of freedom
if (residual_deviance > degrees_of_freedom) {
  print("Overdispersion present in the model")
} else {
  print("No overdispersion present in the model")
}



# ====================================================================
# Simple Linear Regression ===========================================
# ====================================================================

# Let's see what happens when we account for polygon size
# i.e., species richness per square kilometer (rchPrSK) 
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



# Diagnostics

# Create residual plot
plot(model_lm, which = 1)

# Test for normality of residuals
shapiro.test(resid(model_lm))


par(mfrow=c(2,2))   # view plots side-by-side
plot(model_lm)
layout(1)           # return to normal view

hist(resid(model_lm)) # if normal distribution, good fit
plot(urbanforest_geom$treEqty, resid(model_lm)) # if no pattern, good fit; if pattern, poor fit





