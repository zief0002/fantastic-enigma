# -------------------------------------------------------------------------------------------------
#
#                           ASSUMPTIONS UNDERLYING THE REGRESSION MODEL
#                                               2024
#                                    EPSY 8251, Andrew Zieffler
#                      © GNU GENERAL PUBLIC LICENSE (Version 3, 29 June 2007)
#                                        
# -------------------------------------------------------------------------------------------------


##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)



##################################################
### Set options so up to 6 significant digits print
##################################################

options(pillar.sigfig = 6)



##################################################
### Read in data
##################################################

keith = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/main/data/keith-gpa.csv")
keith



##################################################
### Fit regression model
##################################################

lm.a = lm(gpa ~ 1 + homework, data = keith)



##################################################
### Evaluating the distributional assumptions
##################################################

# Augment the model to get residuals (need brrom library loaded)
aug_a = augment(lm.a)

# View augmented data
aug_a


# Density plot of the residuals
p1 = ggplot(data = aug_a, aes(x = .resid)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Residual") +
  ylab("Probability density")


# Scatterplot of the residuals versus X
p2 = ggplot(data = aug_a, aes(x = homework, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Time spent on homework (in hours)") +
  ylab("Residual")


# Layout plots (need patchwork library loaded)
p1 | p2



##################################################
### Use standardized residuals
##################################################

# Density plot of the residuals
p1 = ggplot(data = aug_a, aes(x = .std.resid)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")


# Scatterplot of the residuals versus X
p2 = ggplot(data = aug_a, aes(x = homework, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Time spent on homework (in hours)") +
  ylab("Standardized residual")


# Layout plots
p1 | p2



##################################################
### Identify extreme observations
##################################################

# Observations with standardized residuals more than 2 SEs from 0
aug_a |> 
  filter(.std.resid <= -2 | .std.resid >= 2)



##################################################
### Use fitted values in scatterplot on x-axis
##################################################

# Scatterplot of the residuals versus the fitted values
ggplot(data = aug_a, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residual")




##################################################
### Advanced Plotting: Accounting for Sampling Uncertainty in the Density Plot
##################################################

# Density plot of the standardized residuals
ggplot(data = aug_a, aes(x = .std.resid)) +
  stat_density(geom = "line", color = "#c62f4b") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "black", linetype = "dashed") +
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")


# Load library
library(educate)


# Density plot of the standardized residuals
ggplot(data = aug_a, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +      #Add confidence envelope where we expect residual distribution
  stat_density(geom = "line", color = "#c62f4b") + #Actual empirical distribution
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")



##################################################
### Advanced Plotting: Loess Smooth to Help Evaluate Linearity
##################################################

ggplot(data = aug_a, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + #Where we expect the average residual to be
  geom_smooth(method = "loess", se = FALSE) + #Actual empirical average residuals
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Fitted values") +
  ylab("Standardized residual")



##################################################
### Advanced Plotting: Identify Observations with Extreme Residuals
##################################################

# Create ID variable in the augmented data
aug_a = aug_a |> 
  mutate(id = row.names(keith))


# View new data
aug_a


# Plot the id variable as text rather than points in the scatterplot
ggplot(data = aug_a, aes(x = .fitted, y = .std.resid)) +
  geom_text(aes(label = id), size = 4) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -2, linetype = "dotted") +
  geom_hline(yintercept = 2, linetype = "dotted") +
  xlab("Fitted values") +
  ylab("Standardized residuals")


# Create different data sets for the extreme and non-extreme observations
extreme = aug_a |> 
  filter(.std.resid <= -2 | .std.resid >= 2)


nonextreme = aug_a |> 
  filter(.std.resid > -2 & .std.resid < 2)


# Plot using text for the extreme observations and points for the non-extreme
ggplot(data = extreme, aes(x = .fitted, y = .std.resid)) +
  geom_text(aes(label = id), size = 4, color = "red") +
  geom_point(data = nonextreme) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Fitted values") +
  ylab("Standardized residual")


