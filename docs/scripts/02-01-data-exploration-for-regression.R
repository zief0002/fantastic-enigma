# -------------------------------------------------------------------------------------------------
#
#                                 DATA EXPLORATION FOR REGRESSION
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
### Marginal distribution of income
##################################################

ggplot(data = city, aes(x = income)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Income (in thousands of dollars)") +
  ylab("Probability density")


city %>%
  summarize(
    M = mean(income),
    SD = sd(income)
  )



##################################################
### Marginal distribution of education level
##################################################

ggplot(data = city, aes(x = education)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Education level") +
  ylab("Probability density")


city %>% 
  summarize(
    M = mean(education), 
    SD = sd(education)
    )



##################################################
### Relationship between variables
##################################################

ggplot(data = city, aes(x = education, y = income)) + 
  geom_point(size = 5) +
  theme_bw() +
  xlab("Education (in years)") +
  ylab("Income (in U.S. dollars)")


