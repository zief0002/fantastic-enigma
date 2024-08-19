# -------------------------------------------------------------------------------------------------
#
#                                 INTRODUCTION TO INTERACTION EFFECTS
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
library(educate)
library(patchwork)
library(tidyverse)



###################################################
### Import data
###################################################

work = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/main/data/work-demands.csv")
work



###################################################
### Exploration
###################################################

# Density plot of pre-test scores
p1 = ggplot(data = work, aes(x = guilt)) +
  geom_density() +
  theme_bw() +
  xlab("Guilt") +
  ylab("Probability density")


# Density plot of post-test scores
p2 = ggplot(data = work, aes(x = contact)) +
  geom_density() +
  theme_bw() +
  xlab("Amount of work contact outside normal working hours") +
  ylab("Probability density")


# Scatterplot of post- vs. pre-test scores
p3 = ggplot(data = work, aes(x = contact, y = guilt)) +
  geom_point() +
  theme_bw() +
  xlab("Amount of work contact outside normal working hours") +
  ylab("Guilt")


# Layout plots
p1 | p2 | p3


# Compute summary statistics
work |>
  summarize(
    M_guilt  = mean(guilt),
    SD_guilt = sd(guilt),
    M_contact  = mean(contact),
    SD_contact = sd(contact),
    N = n()
  )


# Correlation
work |>
  select(guilt, contact) |>
  correlate() |>
  fashion(decimals = 2)



##################################################
### Fit main-effects models
##################################################

# Fit models
lm.a = lm(guilt ~ 1 + contact, data = work)
lm.b = lm(guilt ~ 1 + contact + female, data = work)
lm.c = lm(guilt ~ 1 + contact + female + authority + married, data = work)


# Model-level output
glance(lm.a)
glance(lm.b)
glance(lm.c)


# Coefficient-level output
tidy(lm.a, conf.int = TRUE)
tidy(lm.b, conf.int = TRUE)
tidy(lm.c, conf.int = TRUE)



##################################################
### Plot main-effects model results
##################################################

ggplot(data = work, aes(x = contact, y = guilt)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Amount of boundary-spanning work") +
  ylab("Predicted home-life/work guilt") +
  geom_abline(intercept = 6.94, slope = 3.22, linetype = "solid", color = "#424651") +
  geom_abline(intercept = 3.22, slope = 3.22, linetype = "dashed", color = "#f5853f")



##################################################
### Explore data for interaction effect between boundary-spanning work and gender
##################################################

ggplot(data = work, aes(x = contact, y = guilt, group = factor(female), color = factor(female))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  scale_color_manual(
    name = "",
    values = c("#F5853F", "#424651"),
    labels = c("Non-females", "Females")
  ) +
  xlab("Work contact outside normal working hours") +
  ylab("Guilt")



##################################################
### Test interaction effect
##################################################

# Create interaction term
work = work |>
  mutate(
    contact_female = contact * female
  )


# View data
work


# Fit interaction model
lm.d = lm(guilt ~ 1 + contact + female + contact_female, data = work)


# Model-level output
glance(lm.d) |>
  print(width = Inf)


# Coefficient-level output
tidy(lm.d)



##################################################
### Plot the differential effects from the fitted equation
##################################################

ggplot(data = work, aes(x = contact, y = guilt)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Work contact outside normal working hours") +
  ylab("Predicted guilt") +
  geom_abline(intercept = 7.95, slope = 3.43, linetype = "solid", color = "#424651") + #Females
  geom_abline(intercept = 7.22, slope = 2.62, linetype = "dashed", color = "#f5853f") #Non-females



##################################################
### Add covariate(s)
##################################################

# Fit interaction model
lm.e = lm(guilt ~ 1 + contact + female + authority + married + contact_female, data = work)


# Model-level output
glance(lm.e) |>
  print(width = Inf)


# Coefficient-level output
tidy(lm.e)



##################################################
### Plotting model results
##################################################

# Female
p1 = ggplot(data = work, aes(x = contact, y = guilt)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Work contact outside normal working hours") +
  ylab("Predicted guilt") +
  geom_abline(intercept = 7.66, slope = 3.48, linetype = "dashed", color = "#424651") + #Married
  geom_abline(intercept = 5.58, slope = 3.48, linetype = "solid", color = "#424651") +  #Not married
  ggtitle("Female")


# Non-female
p2 = ggplot(data = work, aes(x = contact, y = guilt)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Work contact outside normal working hours") +
  ylab("Predicted guilt") +
  geom_abline(intercept = 7.84, slope = 2.79, linetype = "dashed", color = "#f5853f") + #Married
  geom_abline(intercept = 5.76, slope = 2.79, linetype = "solid", color = "#f5853f") +  #Not married
  ggtitle("Non-female")


# Layout plots
p1 | p2



##################################################
### Plotting model results --- Second plot
##################################################

# Non-married
p1 = ggplot(data = work, aes(x = contact, y = guilt)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Work contact outside normal working hours") +
  ylab("Predicted guilt") +
  geom_abline(intercept = 5.76, slope = 2.79, linetype = "solid", color = "#f5853f") +  #Non-female
  geom_abline(intercept = 5.58, slope = 3.48, linetype = "dashed", color = "#424651") + #Female
  ggtitle("Non-married")


# Married
p2 = ggplot(data = work, aes(x = contact, y = guilt)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Work contact outside normal working hours") +
  ylab("Predicted guilt") +
  geom_abline(intercept = 7.84, slope = 2.79, linetype = "solid", color = "#f5853f") +  #Non-female
  geom_abline(intercept = 7.66, slope = 3.48, linetype = "dashed", color = "#424651") + #Female
  ggtitle("Married")


# Layout plots
p1 | p2



##################################################
### Examine model assumptions
##################################################

# Create augmented data
aug_e = augment(lm.e)


# Examine normality assumption
p1 = ggplot(data = aug_e, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized Residuals") +
  ylab("Probability density")


# Examine other assumptions
p2 = ggplot(data = aug_e, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted Values") +
  ylab("Standardized Residuals")


# Layout plots
p1 | p2


