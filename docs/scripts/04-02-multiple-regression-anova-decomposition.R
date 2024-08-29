# -------------------------------------------------------------------------------------------------
#
#                                MULTIPLE REGRESSION: ANOVA DECOMPOSITION
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
### Fit regression models
##################################################

# Fit simple regression models
lm.a = lm(gpa ~ 1 + homework, data = keith)
lm.b = lm(gpa ~ 1 + parent_ed, data = keith)


# Fit multiple regression model
lm.c = lm(gpa ~ 1 + parent_ed + homework, data = keith)




##################################################
### ANOVA Decomposition
##################################################

# ANOVA decomposition for Model A (homework)
anova(lm.a)


# ANOVA decomposition for Model C (parent education level, homework)
anova(lm.c)



##################################################
### Order of predictors in the model
##################################################

# Include HW before parent ed.
lm.d = lm(gpa ~ 1 + homework + parent_ed, data = keith)


# ANOVA decomposition
anova(lm.d)



