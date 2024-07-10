library(broom)
library(corrr)
library(dplyr)
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

daycare = read_csv(file = "I:/My Drive/UMN/Teaching/EPSY 8251 Statistical Methods in Education I/Practice/Daycare.csv")
head(daycare)

##################################################
### 1. correlation matrix 
##################################################

daycare %>%
  select(reading, ses, gender) %>%
  correlate()

##################################################
### 2. null hypothesis for the omnibus test 
##################################################

#H0: rho_squared = 0. ses and gender together explain no variance of the reading achievement.

##################################################
### 3. null hypothesis for each regression slope 
##################################################

#H0: Beta_ses = 0, ses is not associated with reading achievement, after controlling for gender.
#H0: Beta_gender = 0 gender is not associated with reading achievement, after controlling for ses.


##################################################
### 4. Fit the multiple regression model   
##################################################

mod <- lm(reading~ 1+ ses+gender, data = daycare) 
tidy(mod)

#reading = 19.94 + 2.23ses + 13.00gender(male=1)

##################################################
### 5. Interpret the intercept    
##################################################

#The predicted mean Reading score for female students with SES equal to 0 is 19.94. 


##################################################
### 6. Interpret the slope    
##################################################

#Holding SES constant, the average Reading score for male students is 13.00 higher than that of female students. 
#Holding Gender constant, one unit increase in SES is associated with an average increase in Reading score of 2.23.

##################################################
### 7. Write out omnibus test (F test) results and interpret
##################################################
glance(mod)
anova(mod)

R_squared= (5351.2+11153.2)/(5351.2+11153.2+10330.8)

#About 61.5% of the variation in Reading scores is explained by SES and Gender. We reject the null hypothesis as the evidence from the 
#data was not consistent with the null hypothesis, suggesting SES and Gender do explained the variation in the reading scores (R^2=0.615,F(2,546)=436.14,p<.001).

##################################################
### 8. Write out the t test results for each slope and interpret.
##################################################
tidy(mod)

#Beta_ses = 2.23, t=8,73, p<.001. we reject the null as the evidence from the data was inconsistent with the null hypothesis, 
#meaning that there is a relationship between ses and reading scores after controlling for gender.

#Beta_gender = 13.00, t=24,28, p<.001. we reject the null as the evidence from the data was inconsistent with the null hypothesis, 
#meaning that there is a relationship between gender and reading scores after controlling for ses.

##################################################
### 9. Plot the regression line 
##################################################

#Female: reading = 19.94 + 2.23ses
#Male: reading = 32.94 + 2.23ses

ggplot(data = daycare, aes(x = ses, y = reading)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("SES") +
  ylab("Reading Score") +
  geom_abline(aes(intercept = 19.94, slope = 2.23, colour = "Female"), linetype = "dashed") +
  geom_abline(aes(intercept = 32.94, slope = 2.23, colour = "Male"), linetype = "dashed")+
  labs(colour="")


