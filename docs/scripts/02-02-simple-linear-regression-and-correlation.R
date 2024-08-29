# -------------------------------------------------------------------------------------------------
#
#                                 SIMPLE LINEAR REGRESSION AND CORRELATION
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
### Scatterplot
##################################################

ggplot(data = city, aes(x = education, y = income)) + 
  geom_point(size = 5) +
  theme_bw() +
  xlab("Education (in years)") +
  ylab("Income (in U.S. dollars)")



##################################################
### Regress income on education level
##################################################

# Fit model
lm.a = lm(income ~ 1 + education, data = city)


# View coefficients
lm.a



##################################################
### Estimating residuals
##################################################

# See the 25th observation's data
city |>
  filter(row_number() == 25)


# Compute the predicted income using the regression equation
11.321 + 2.651 * 20


# Compute the residual
54.672 - 64.341



##################################################
### Correlation
##################################################

city |>
  select(income, education) |>
  correlate()





