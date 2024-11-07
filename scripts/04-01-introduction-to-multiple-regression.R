# -------------------------------------------------------------------------------------------------
#
#                                 INTRODUCTION TO MULTIPLE REGRESSION
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
### Fit regression model
##################################################

lm.a = lm(gpa ~ 1 + homework, data = keith)

glance(lm.a) |> print(width = Inf) # Model-level results
tidy(lm.a)   # Coefficient-level results




##################################################
### Examine parent education level predictor
##################################################

# Examine the marginal distribution
ggplot(data = keith, aes(x = parent_ed)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Parent education level") +
  ylab("Probability density")


# Compute mean and standard deviation
keith |>
  summarize(
    M = mean(parent_ed),
    SD = sd(parent_ed)
  )



##################################################
### Examine relationship between parent education level and GPA
##################################################

# Scatterplot
ggplot(data = keith, aes(x = parent_ed, y = gpa)) +
  geom_point(size = 5) +
  theme_bw() +
  xlab("Parent education level") +
  ylab("GPA")


# Correlation matrix
keith |>
  select(gpa, homework, parent_ed) |>
  correlate()



##################################################
### Simple Regression Model: Parent Education Level as a Predictor of GPA
##################################################

lm.b = lm(gpa ~ 1 + parent_ed, data = keith)

glance(lm.b) |> print(widyth = Inf) # Model-level results
tidy(lm.b)   # Coefficient-level results




##################################################
### Fit the multiple regression model
##################################################

lm.c = lm(gpa ~ 1 + parent_ed + homework, data = keith)


glance(lm.c) |> print(width = Inf) # Model-level results
tidy(lm.c)   # Coefficient-level results


#lm.d = lm(gpa ~ 1 + homework  + parent_ed, data = keith)
glance(lm.d) |> print(width = Inf) # Model-level results
tidy(lm.d)   # Coefficient-level results

##################################################
### Residuals
##################################################

# Observation 1
keith |>
  filter(row_number() == 1)


# Compute y-hat value
63.20 + 0.87*13 + 0.99*2


# Compute residual
78 - 76.49



##################################################
### Order of predictors in the model
##################################################

# Original fitted model
# lm.c = lm(gpa ~ 1 + parent_ed + homework, data = keith)


# Fit multiple regression model
lm.d = lm(gpa ~ 1 + homework + parent_ed, data = keith)


# Model-level results
glance(lm.d)


# Coefficient-level output
tidy(lm.d)

