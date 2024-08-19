# -------------------------------------------------------------------------------------------------
#
#                                        MULTIPLE COMPARISONS
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
library(emmeans)
library(ggridges)
library(tidyverse)



##################################################
### Import data
##################################################

# Read in data and create dummy variables
pew = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/main/data/pew.csv") |>
  mutate(
    none    = if_else(news_source == "None", 1, 0),
    con     = if_else(news_source == "Conservative", 1, 0),
    com     = if_else(news_source == "Comedy", 1, 0),
    lib     = if_else(news_source == "Liberal", 1, 0),
    con_com = if_else(news_source == "Conservative_Comedy", 1, 0),
    con_lib = if_else(news_source == "Conservative_Liberal", 1, 0),
    lib_com = if_else(news_source == "Liberal_Comedy", 1, 0),
    all     = if_else(news_source == "All", 1, 0),
  )


# View data
pew



##################################################
### Fit model
##################################################

# Fit model (None is reference group)
lm.none.2 = lm(knowledge ~ 1 + age + education + news + engagement + all + con + 
                 com + lib + con_com + con_lib + lib_com, data = pew)



##################################################
### Dunn-Bonferroni approach to adjusting p-values
##################################################

c(
  0.2170, #All vs. Comedy                              
  0.2550, #All vs. Conservative                        
  0.2209, #All vs. Conservative_Comedy                 
  0.1142, #All vs. Conservative_Liberal                
  0.3514, #All vs. Liberal                             
  0.0003, #All vs. Liberal_Comedy                      
  0.1158, #All vs. None                                
  0.0155, #Comedy vs. Conservative                     
  0.8630, #Comedy vs. Conservative_Comedy              
  0.0084, #Comedy vs. Conservative_Liberal             
  0.5109, #Comedy vs. Liberal                          
  0.0536, #Comedy vs. Liberal_Comedy                   
  0.0037, #Comedy vs. None                             
  0.0086, #Conservative vs. Conservative_Comedy        
  0.4859, #Conservative vs. Conservative_Liberal       
  0.0016, #Conservative vs. Liberal                    
  0.0000000289, #Conservative vs. Liberal_Comedy             
  0.3307, #Conservative vs. None                       
  0.0033, #Conservative_Comedy vs. Conservative_Liberal
  0.6060, #Conservative_Comedy vs. Liberal             
  0.0190, #Conservative_Comedy vs. Liberal_Comedy      
  0.0022, #Conservative_Comedy vs. None                
  0.0012, #Conservative_Liberal vs. Liberal            
  0.00000000826, #Conservative_Liberal vs. Liberal_Comedy     
  0.9042, #Conservative_Liberal vs. None               
  0.0008, #Liberal vs. Liberal_Comedy                  
  0.0000477, #Liberal vs. None                            
  0.00000000284  #Liberal_Comedy vs. None                     
) * 28



##################################################
### Obtain Dunn-Bonferroni p-values directly
##################################################

# Fit model using categorical predictor
lm.news_source = lm(knowledge ~ 1 + age + education + news + engagement + news_source, data = pew)


# Coefficient-level output
tidy(lm.news_source)


# Obtain the unadjusted p-values for pairwise differences
emmeans(lm.news_source, specs = pairwise ~news_source, adjust = "none")


# Obtain the Dunn-Bonferroni adjusted p-values for pairwise differences
emmeans(lm.news_source, specs = pairwise ~news_source, adjust = "bonferroni")



##################################################
### Controlling p-value for False Discovery Rate
##################################################

# Obtain the Benjamini-Hochberg adjusted p-values for pairwise differences
emmeans(lm.news_source, specs = pairwise ~news_source, adjust = "BH")


