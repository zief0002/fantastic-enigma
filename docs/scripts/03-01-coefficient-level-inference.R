# -------------------------------------------------------------------------------------------------
#
#                                    COEFFICIENT-LEVEL INFERENCE
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
### Read in data
##################################################

keith = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/main/data/keith-gpa.csv")
keith



##################################################
### Exploration
##################################################

# Marginal distribution of GPA
ggplot(data = keith, aes(x = gpa)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("GPA (on a 100-pt. scale)") +
  ylab("Probability density") +
  ggtitle("Outcome: GPA")


# Marginal distribution of homework
ggplot(data = keith, aes(x = homework)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Time spent on homework per week (in hours)") +
  ylab("Probability density") +
  ggtitle("Predictor: Homework")


# Scatterplot
ggplot( data = keith, aes(x = homework, y = gpa) ) +
  geom_point() +
  theme_bw() +
  xlab("Time spent on homework per week (in hours)") +
  ylab("GPA (on a 100-pt. scale)")


# Summary statistics
keith |>
  summarize(
    M_gpa  = mean(gpa),
    SD_gpa = sd(gpa),
    M_hw   = mean(homework),
    SD_hw  = sd(homework)
    )


# Correlation
keith |>
  select(gpa, homework) |>
  correlate()



##################################################
### Fit regression model
##################################################

lm.a = lm(gpa ~ 1 + homework, data = keith)
lm.a



##################################################
### Coefficient-level output
##################################################

tidy(lm.a)



##################################################
### Interval estimates of the regression parameters
##################################################

tidy(lm.a, conf.int = TRUE, conf.level = 0.95)

