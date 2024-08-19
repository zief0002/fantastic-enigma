# -------------------------------------------------------------------------------------------------
#
#                                 ANOVA DECOMPOSITION AND R^2
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

# View data
city



##################################################
### Regress income on education level
##################################################

lm.a = lm(income ~ 1 + education, data = city)
lm.a



##################################################
### Compute the SSE
##################################################

city |>
  mutate(
    y_hat = 11.321 + 2.651 * education,  #STEP 1: Compute y-hat values
    errors = income - y_hat,             #STEP 2: Compute residuals
    sq_errors = errors ^ 2               #STEP 3: Compute squared residuals
  ) |>
  summarize(
    SSE = sum(sq_errors)                 #STEP 4: Sum up he squared residuals
  )



##################################################
### Intercept-Only Model: A Baseline for Comparison
##################################################

# Fit intercept-only model
lm.0 = lm(income ~ 1, data = city)
lm.0


# Plot of the intercept-only model
ggplot(data = city, aes(x = education, y = income)) +
  geom_point(size = 5) +
  geom_hline(yintercept = 53.742, color = "blue") +
  xlab("Education (in years)") +
  ylab("Income") +
  theme_bw()


# Compute SSE for intercept-only (baseline) model
city |>
  mutate(
    y_hat = 53.742,          #STEP 1: Compute y-hat values
    errors = income - y_hat, #STEP 2: Compute residuals
    sq_errors = errors ^ 2.  #STEP 3: Compute squared residuals
  ) |>
  summarize(
    SSE = sum(sq_errors)     #STEP 4: Sum up he squared residuals
  )



##################################################
### Compute proportion of the reduction in error (PRE)
##################################################

# pre = (sse.0 - sse.1) / sse.0
(6565.53 - 2418.20) / 6565.53

# Unexplained variation
1 - 0.632



##################################################
### R-squared
##################################################

# Compute correlation between fitted values and residuals
city |>
  mutate(
    y_hat = 11.321 + 2.651*education
  ) |>
  select(y_hat, income) |>
  correlate()


# Compute R^2
0.795 ^ 2



