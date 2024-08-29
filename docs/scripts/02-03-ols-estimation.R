# -------------------------------------------------------------------------------------------------
#
#                                 ORDINARY LEAST SQUARES (OLS) ESTIMATION
#                                               2024
#                                    EPSY 8251, Andrew Zieffler
#                      © GNU GENERAL PUBLIC LICENSE (Version 3, 29 June 2007)
#                                        
# -------------------------------------------------------------------------------------------------


##################################################
### Load libraries
##################################################

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

# Step 1: Compute the predicted values of Y
city |>
  mutate(
    y_hat = 11.321 + 2.651 * education #STEP 1: Compute y-hat values
    )


# Step 2: Compute the residuals
city |> 
  mutate(
    y_hat = 11.321 + 2.651 * education,  #STEP 1: Compute y-hat values
    errors = income - y_hat              #STEP 2: Compute residuals
    )


# Step 3: Compute the squared residuals
city |>
  mutate(
    y_hat = 11.321 + 2.651 * education,  #STEP 1: Compute y-hat values
    errors = income - y_hat,             #STEP 2: Compute residuals,
    sq_errors = errors ^ 2               #STEP 3: Compute squared residuals
  )


# Step 4: Compute the sum of the squared residuals
city |>
  mutate(
    y_hat = 11.321 + 2.651 * education,  #STEP 1: Compute y-hat values
    errors = income - y_hat,             #STEP 2: Compute residuals,
    sq_errors = errors ^ 2               #STEP 3: Compute squared residuals
  ) |>
  summarize(
    SSE = sum(sq_errors) #STEP 4: Sum up he squared residuals
  )

