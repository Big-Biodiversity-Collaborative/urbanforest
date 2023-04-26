# Project: Tucson Urban Tree Equity and Bird Richness
# Script: Correlationa and Regression
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
dim(urbanforest_geom)     # 297 neighborhoods


# =====================================================================
# Preliminary Visualization ===========================================
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
# Generalized Linear Model ===========================================
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


# Create a scatterplot with a fitted line
# Add the summary information as a subtitle
# Create summary text
summary_text <- paste("Poisson regression model summary:",
                      "\nEstimate (treEqty_log) =", round(coef(model_glm)[2], 4),
                      "\nPseudo R-squared =", round(1 - (deviance(model_glm) / model_glm$null.deviance), 4),
                      "\nAIC =", round(AIC(model_glm), 4),
                      "\nDeviance =", round(deviance(model_glm), 4),
                      "\nAnova p-value =", round(anova(model_glm, test='Chisq')$'Pr(>Chi)'[2], 4))

# Plot the scatterplot with the summary text as a subtitle
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
        axis.line = element_line(colour = "black"),
        plot.subtitle = element_text(hjust = 0, size = 10)) + # Adjust subtitle appearance
  labs(subtitle = summary_text) # Add the summary text as the subtitle


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


# P-value for Pearson goodness-of-fit test
pchisq(sum(pearsonResiduals^2), df = df.residual(model_glm), lower.tail = FALSE)
# Indicates model is not a good fit.


# Check for overdispersion
require(performance)
check_overdispersion(model_glm)

# Calculate residual deviance and degrees of freedom
residual_deviance <- deviance(model_glm, type = "residual")
degrees_of_freedom <- df.residual(model_glm)

# Compare residual deviance to degrees of freedom
if (residual_deviance > degrees_of_freedom) {
  print("Overdispersion present in the model")
} else {
  print("No overdispersion present in the model")
}



# ====================================================================
# Negative Binomial Regression =======================================
# ====================================================================

# Overdispersion is present in model_glm, so we will
# fit a negative binomial regression model
model_glmnb <- glm.nb(spcsRch ~ treEqty_log, data = urbanforest_geom)

# Check model summary
summary(model_glmnb)


# Create a scatterplot with a fitted line
# Add the summary information as a subtitle
summary_text <- paste("Negative binomial regression model summary:",
                      "\nTheta =", round(model_glmnb$theta, 4),
                      "\nEstimate (treEqty_log) =", round(coef(model_glmnb)[2], 4),
                      "\nPseudo R-squared =", round(1 - (model_glmnb$deviance / model_glmnb$null.deviance), 4),
                      "\nAnova p-value =", round(anova(model_glmnb, test='Chisq')$'Pr(>Chi)'[2], 4))


# Display the plot with the summary information
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
        axis.line = element_line(colour = "black")) +
  labs(subtitle = summary_text)




# Create diagnostic plots
par(mfrow = c(1, 2))  # show plots side-by-side
plot(model_glmnb, which = c(1, 2))

1-(model_glmnb$deviance/model_glmnb$null.deviance) #PseudoR^2
r2(model_glmnb) #Nagelkerke's R^2 for count data 
anova(model_glmnb, test='Chisq') 

# Plots still showing a positive skew. 
# Suggests that the model may still not be fully capturing the 
# variability in the data. 



# ====================================================================
# Simple Linear Regression ===========================================
# ====================================================================

# Let's see what happens when we account for polygon size
# i.e., species richness per square kilometer (rchPrSK) 
# as a function of tree equity (treEqty)

# Fit a simple linear regression model
# I want to compare lm models using the treEqty' or 'treEqty_log'
model_lm <- lm(rchPrSK ~ treEqty, data = urbanforest_geom)
model_lm_log <- lm(rchPrSK ~ treEqty_log, data = urbanforest_geom)

# Check model summary
summary(model_lm)
summary(model_lm_log)

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




ggplot(aes(x=treEqty_log, y=rchPrSK), data=urbanforest_geom)+
  geom_point()+
  geom_smooth(method="lm", se=F)

# Overall, in either case, the results suggest that there is 
# a weak positive relationship between tree equity and richness per sq km


#Regression diagnostics
par(mfrow=c(2,2))   # view plots side-by-side
plot(model_lm)
layout(1)           # return to normal view

hist(resid(model_lm)) # if normal distribution, good fit
plot(urbanforest_geom$treEqty, resid(model_lm)) # if no pattern, good fit; if pattern, poor fit

require(gvlma) #package for global validation
summary(gvlma(model_lm))

require(see)
check_model(model_lm)
check_posterior_predictions(model_lm) #posterior predictive checks



