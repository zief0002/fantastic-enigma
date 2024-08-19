# -------------------------------------------------------------------------------------------------
#
#                                       STANDARDIZED REGRESSION
#                                               2024
#                                    EPSY 8251, Andrew Zieffler
#                      © GNU GENERAL PUBLIC LICENSE (Version 3, 29 June 2007)
#                                        
# -------------------------------------------------------------------------------------------------


##################################################
### Load libraries
##################################################

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

city = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/main/data/riverview.csv")

#View data
city



##################################################
### Fit regression
##################################################

lm.a = lm(income ~ 1 + education, data = city)
lm.a



##################################################
### Correlation's relationship to regression
##################################################

# Get correlation
city |>
  select(income, education) |>
  correlate()


# Get SDs
city |>
  summarize(
    SD_Y = sd(income),
    SD_X = sd(education)
  )


# Compute slope
0.795 * 14.55 / 4.36



##################################################
### Standardize variables
##################################################

# Standardize the outcome and predictor
city = city |>
  mutate(
    z_income = (income - mean(income)) / sd(income),
    z_education = (education - mean(education)) / sd(education),
  )

# View data
city


# Marginal distribution of the standardized incomes
ggplot(data = city, aes(x = z_income)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized incomes") +
  ylab("Probability density")


# Marginal distribution of the standardized education levels
ggplot(data = city, aes(x = z_education)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized education level") +
  ylab("Probability density")


# Compute summaries
city |>
  summarize(
    M_y = mean(z_income),
    SD_y = sd(z_income),
    M_x = mean(z_education),
    SD_x = sd(z_education)
  )


# Compute correlation
city |>
  select(z_income, z_education) |>
  correlate()



##################################################
### Standardized regression
##################################################

# Fit standardized regression
lm.z = lm(z_income ~ 1 + z_education, data = city)
lm.z



##################################################
### Scatterplot of standardized variables
##################################################

ggplot(data = keith, aes(x = z_homework, y = z_gpa)) +
  geom_point(size = 5) +
  theme_bw() +
  xlab("Time spent on homework (standardized)") +
  ylab("GPA (standardized)") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_abline(intercept = 0, slope = 0.327)



##################################################
### Variance accounted for (R^2) in standardized regression
##################################################

# Compute the SSE for the standardized intercept-only model
city |>
  mutate(
    y_hat = 0,
    errors = z_income - y_hat,
    sq_errors = errors ^ 2
  ) |>
  summarize(
    SSE = sum(sq_errors)
  )


# Compute the SSE for the standardized slope-intercept model
city |>
  mutate(
    y_hat = 0 + 0.795 * z_education,
    errors = z_income - y_hat,
    sq_errors = errors ^ 2
  ) |>
  summarize(
    SSE = sum(sq_errors)
  )


# Compute R^2
(31 - 11.4) / 31



