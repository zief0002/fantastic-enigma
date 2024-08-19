# -------------------------------------------------------------------------------------------------
#
#                                 MULTIPLE REGRESSION: ASSUMPTIONS
#                                               2024
#                                    EPSY 8251, Andrew Zieffler
#                      © GNU GENERAL PUBLIC LICENSE (Version 3, 29 June 2007)
#                                        
# -------------------------------------------------------------------------------------------------


###################################################
### Load all needed libraries 
###################################################

library(broom)
library(dplyr)
library(educate)
library(ggplot2)
library(patchwork)
library(readr)



##################################################
### Set options so up to 6 significant digits print
##################################################

options(pillar.sigfig = 6)



##################################################
### Import data
##################################################

keith = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/main/data/keith-gpa.csv")
keith



##################################################
### Fit the multiple regression model
##################################################

lm.c = lm(gpa ~ 1 + parent_ed + homework, data = keith)



##################################################
### Augment the model to obtain the fitted values and residuals
##################################################

aug_c = augment(lm.c)
aug_c



##################################################
### Plots to evaluate assumptions
##################################################

# Need educate package loaded

# Density plot of the standardized residuals
p1 = ggplot(data = aug_c, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")


# Plot the standardized residuals versus the fitted values
p2 = ggplot(data = aug_c, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.5) + #ADD y=0 line and confidence envelope
  geom_smooth(method = "loess", se = FALSE) + #ADD loess smoother
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Standardized residual")


# Layout plots (need patchwork library loaded)
p1 | p2




##################################################
### Identify extreme cases
##################################################

# Create ID variable in the augmented data
aug_c = aug_c |> 
  mutate(id = row.names(keith))


# View new data
aug_c




# Create different data sets for the extreme and non-extreme observations
extreme = aug_c |> 
  filter(.std.resid <= -2 | .std.resid >= 2)

nonextreme = aug_c |> 
  filter(.std.resid > -2 & .std.resid < 2)


# Plot using text for the extreme observations and points for the non-extreme
ggplot(data = extreme, aes(x = .fitted, y = .std.resid)) +
  geom_text(aes(label = id), size = 4, color = "#c62f4b") +
  geom_point(data = nonextreme) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Standardized residual")

