# -------------------------------------------------------------------------------------------------
#
#                                 DICHOTOMOUS CATEGORICAL PREDICTORS
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
library(educate)
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

mn = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/main/data/mn-schools.csv")
mn



##################################################
### Exploration
##################################################

# Density plot of graduation rates
p1 = ggplot(data = mn, aes(x = grad)) +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("Six-year graduation rate") +
  ylab("Probability density")


# Bar plot of education sector
p2 = ggplot(data = mn, aes(x = sector)) +
  geom_bar(fill = "#c62f4b") +
  theme_bw() +
  xlab("Educational sector") +
  ylab("Frequency")


# Scatterplot
p3 = ggplot(data = mn, aes(x = sector, y = grad)) +
  geom_point() +
  theme_bw() +
  xlab("Educational sector") +
  ylab("Six-year graduation rate")


# Layout plots
(p1 | p2) / p3


# Summary statistics
mn |> 
  group_by(sector) |>
  summarize(
    M = mean(grad),
    SD = sd(grad),
    N = length(grad)
  )



##################################################
### Indicator variables: 5 and 10
##################################################

# Create indicator variable
mn = mn = mn |>
  mutate(
    indicator = if_else(sector == "Public", 5, 10)
  )


# Examine data frame
mn


# Correlation
mn |>
  select(grad, indicator) |>
  correlate() |>
  fashion(decimals = 3)


# Fit regression model
lm.a = lm(grad ~ 1 + indicator, data = mn)


glance(lm.a) |> print(width = Inf) # Model-level output
tidy(lm.a)                         # Coefficient-level output



##################################################
### Indicator variables: 2 and 7
##################################################

# Create indicator variable
mn = mn |>
  mutate(
    indicator_2 = if_else(sector == "Public", 2, 7)
  )


# Examine data frame
mn


# Correlation
mn |>
  select(grad, indicator_2) |>
  correlate() |>
  fashion(decimals = 3)


# Fit regression model
lm.b = lm(grad ~ 1 + indicator_2, data = mn)


glance(lm.b) |> print(width = Inf) # Model-level output
tidy(lm.b)                         # Coefficient-level output



##################################################
### Dummy indicator variables: 0 (public) and 1 (private)
##################################################

# Create indicator variable
mn = mn |>
  mutate(
    private = if_else(sector == "Private", 1, 0)
  )


# Examine data frame
mn


# Correlation
mn |>
  select(grad, private) |>
  correlate() |>
  fashion(decimals = 3)


# Fit regression model
lm.c = lm(grad ~ 1 + private, data = mn)


glance(lm.c) |> print(width = Inf) # Model-level output
tidy(lm.c)                         # Coefficient-level output



##################################################
### Private schools as reference group
##################################################

# Create indicator variable
mn = mn |>
  mutate(
    public = if_else(sector == "Public", 1, 0)
  )


# Examine data frame
mn


# Correlation
mn |>
  select(grad, public) |>
  correlate() |>
  fashion(decimals = 3)


# Fit regression model
lm.d = lm(grad ~ 1 + public, data = mn)


glance(lm.d) |> print(width = Inf) # Model-level output
tidy(lm.d)                         # Coefficient-level output



##################################################
### Examine assumptions
##################################################

# Obtain the fitted values and residuals
aug_d = augment(lm.d)


# View augmented data frame
aug_d


# Density plot of the marginal standardized residuals
p1 = ggplot(data = aug_d, aes(x = .std.resid)) +
  stat_density_confidence(model ="normal") +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")


# Scatterplot of the standardized residuals versus the fitted values
p2 = ggplot(data = aug_d, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  geom_smooth(method = "loess", se = FALSE) +
  theme_bw() +
  xlab("Fitted value") +
  ylab("Standardized residual")


# Layout plots
p1 | p2



##################################################
### Examine assumptions by sector
##################################################

# Get private schools
aug_private = aug_d |> 
  filter(public == 0)


# Get public schools
aug_public = aug_d |> 
  filter(public == 1)


# Density plot of the private schools' standardized residuals
p1 = ggplot(data = aug_private, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")


# Density plot of the public schools' standardized residuals
p2 = ggplot(data = aug_public, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")


# Layout plots
p1 | p2



##################################################
### Including other predictors (ANCOVA model)
##################################################

# Correlation matrix
mn |>
  select(grad, private, sat) |>
  correlate() |>
  fashion(decimals = 3)


# Fit regression model
lm.e = lm(grad ~ 1 + sat + private, data = mn)


glance(lm.e) |> print(width = Inf) # Model-level
tidy(lm.e)                         # Coefficient-level info



##################################################
### Compute adjusted means
##################################################

# Compute mean SAT
mn |>
  summarize(M = mean(sat))


# Compute adjusted mean for private schools
-84.4 + 0.127*1101.2 + 8.4*1


# Compute adjusted mean for public schools
-84.4 + 0.127*1101.2 + 8.4*0


# Compute adjusted mean difference
63.9 - 55.5



##################################################
### One last model
##################################################

# Correlation matrix
mn |>
  select(grad, private, sat, tuition) |>
  correlate() |>
  fashion(decimals = 3)


# Fit regression model
lm.f = lm(grad ~ 1 + sat + tuition + private, data = mn2)


glance(lm.f) |> print(width = Inf) # Model-level
tidy(lm.f)                         # Coefficient-level info


# Obtain the fitted values and residuals
aug_f = augment(lm.f)


# View augmented data frame
aug_f


# Density plot of the marginal standardized residuals
p1 = ggplot(data = aug_f, aes(x = .std.resid)) +
  stat_density_confidence(model ="normal") +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")


# Scatterplot of the standardized residuals versus the fitted values
p2 = ggplot(data = aug_f, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  geom_smooth(method = "loess", se = FALSE) +
  theme_bw() +
  xlab("Fitted value") +
  ylab("Standardized residual")


# Layout plots
p1 | p2


##################################################
### Plot of unadjusted and adjusted means
##################################################

# Create plot data
plot_data = data.frame(
  pred_grad = c(65.3, 51.0, 63.9, 55.5, 61.4, 60.8),
  sector = c("Private", "Public", "Private", "Public", "Private", "Public"),
  model = c("Model A", "Model A", "Model B", "Model B", "Model C", "Model C")
)


# Plot
ggplot(data = plot_data, aes(x = model, y = pred_grad)) +
  geom_point(aes(color = sector), size = 4) +
  theme_bw() +
  xlab("") +
  ylab("Predicted graduation rate") +
  scale_color_manual(
    name = "Sector",
    values = c("#56b4e9", "#e69f00")
  )



##################################################
### Plot of unadjusted and adjusted means (with CIs)
##################################################

# Coefficient-level output for each model with CIs
tidy(lm.c, conf.int = TRUE)
tidy(lm.e, conf.int = TRUE)
tidy(lm.f, conf.int = TRUE)


# Create plot data
plot_data = data.frame(
  pred_diff = c(14.2, 8.38, 0.647),
  low_ci = c(2.18, 2.97, -9.00),
  high_ci = c(26.3, 13.8, 10.3),
  model = c("Model A", "Model B", "Model C"),
  sig = c("Yes", "Yes", "No")
)


# Plot
ggplot(data = plot_data, aes(x = model, y = pred_diff, color = sig)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = low_ci, ymax = high_ci)) + #Create CIs
  geom_point(size = 4) + #Add observed difference
  theme_bw() +
  xlab("") +
  ylab("Predicted difference in graduation rate") +
  scale_color_manual(
    values = c("#777777", "#cc79a7")
  ) +
  guides(color = "none")



