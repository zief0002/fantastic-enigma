# -------------------------------------------------------------------------------------------------
#
#                                 POLYCHOTOMOUS CATEGORICAL PREDICTORS
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
library(ggridges)
library(tidyverse)



##################################################
### Import data
##################################################

# Read in data
pew = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/main/data/pew.csv")
pew



##################################################
### Exploration
##################################################

# Density plot of news knowledge
p1 = ggplot(data = pew, aes(x = knowledge)) +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("News knowledge") +
  ylab("Probability density")


# Bar plot of news source
p2 = ggplot(data = pew, aes(x = news_source)) +
  geom_bar(fill = "#c62f4b") +
  theme_bw() +
  xlab("News source") +
  ylab("Frequency")


# Layout plots
p1 | p2


# Scatterplot
p1 = ggplot(data = pew, aes(x = news_source, y = knowledge)) +
  geom_point() +
  theme_bw() +
  xlab("News source") +
  ylab("News knowledge")


# News knowledge conditioned on news source
p2 = ggplot(data = pew, aes(x = knowledge, y = news_source)) +
  geom_density_ridges() +
  theme_bw() +
  ylab("News source") +
  xlab("News knowledge")


# Layout plots
p1 | p2


# Compute summary statistics
pew |>
  group_by(news_source) |>
  summarize(
    M  = mean(knowledge),
    SD = sd(knowledge),
    N = n()
  ) |>
  arrange(desc(M))



##################################################
### Create dummy-coded variables
##################################################

# Create all eight dummy variables
pew = pew |>
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


# Examine data
pew |>
  print(width = Inf)


# Get the categories
pew |>
  select(news_source) |>
  unique()



##################################################
### Fit model
##################################################

# News source = None is reference group
lm.none = lm(knowledge ~ 1 + all + con + com + lib + con_com + con_lib +
               lib_com, data = pew)


# Model-level info
glance(lm.none) |>
  print(width = Inf)


# Coefficient-level info
tidy(lm.none)



##################################################
### Pairwise Comparisons for Americans who get their News from Conservative Sources
##################################################

# Conservative news sources is reference group
lm.con = lm(knowledge ~ 1 + all + com + lib + con_com + con_lib +
              lib_com + none, data = pew)


# Model-level info
glance(lm.con) |>
  print(width = Inf)


# Coefficient-level info
tidy(lm.con)



##################################################
### Pairwise Comparisons for Americans who get their News from Other Sources
##################################################

# Reference group = All News
lm.all = lm(knowledge ~ 1 + con + com + lib + con_com + con_lib +
              lib_com + none, data = pew)

# Reference group = Comedy News
lm.com = lm(knowledge ~ 1 + all + con + lib + con_com + con_lib +
              lib_com + none, data = pew)

# Reference group = Liberal News
lm.lib = lm(knowledge ~ 1 + all + con + com + con_com + con_lib +
              lib_com + none, data = pew)

# Reference group = Conservative and Comedy News
lm.con_com = lm(knowledge ~ 1 + all + con + com + lib + con_lib +
                  lib_com + none, data = pew)

# Reference group = Conservative and Liberal News
lm.con_lib = lm(knowledge ~ 1 + all + con + com + lib + con_com +
                  lib_com + none, data = pew)



# Coefficient-level output
tidy(lm.all)
tidy(lm.com)
tidy(lm.lib)
tidy(lm.con_com)
tidy(lm.con_lib)



##################################################
### ANCOVA Model
##################################################

# News source = None is reference group
lm.none.2 = lm(knowledge ~ 1 + age + education + news + engagement + all + con + 
                 com + lib + con_com + con_lib + lib_com, data = pew)


# Model-level info
glance(lm.none.2) |>
  print(width = Inf)


# Coefficient-level info
tidy(lm.none.2)



##################################################
### Adjusted mean differences
##################################################

# Mean age = 50.94
# Mean education = 13.95
# Mean news consumption = 50.27
# Mean political engagement = 73.41

# Compute adjusted mean for none source
-29.0 + 0.2*50.94 + 3.3*13.95 + 0.2*50.27 + 0.2*73.41 + 
  4.2*0 + 1.4*0 +  8.8*0 + 6.7*0 + 8.2*0 + 0.2*0 + 16.1*0


# Compute adjusted mean for all source
-29.0 + 0.2*50.94 + 3.3*13.95 + 0.2*50.27 + 0.2*73.41 + 
  4.2*1 + 1.4*0 +  8.8*0 + 6.7*0 + 8.2*0 + 0.2*0 + 16.1*0


# Compute adjusted mean for conservative source
-29.0 + 0.2*50.94 + 3.3*13.95 + 0.2*50.27 + 0.2*73.41 + 
  4.2*0 + 1.4*1 +  8.8*0 + 6.7*0 + 8.2*0 + 0.2*0 + 16.1*0


# Compute adjusted mean for comedy source
-29.0 + 0.2*50.94 + 3.3*13.95 + 0.2*50.27 + 0.2*73.41 + 
  4.2*0 + 1.4*0 +  8.8*1 + 6.7*0 + 8.2*0 + 0.2*0 + 16.1*0


# Compute adjusted mean for liberal source
-29.0 + 0.2*50.94 + 3.3*13.95 + 0.2*50.27 + 0.2*73.41 + 
  4.2*0 + 1.4*0 +  8.8*0 + 6.7*1 + 8.2*0 + 0.2*0 + 16.1*0


# Compute adjusted mean for conservative and comedy source
-29.0 + 0.2*50.94 + 3.3*13.95 + 0.2*50.27 + 0.2*73.41 + 
  4.2*0 + 1.4*0 +  8.8*0 + 6.7*0 + 8.2*1 + 0.2*0 + 16.1*0


# Compute adjusted mean for conservative and liberal source
-29.0 + 0.2*50.94 + 3.3*13.95 + 0.2*50.27 + 0.2*73.41 + 
  4.2*0 + 1.4*0 +  8.8*0 + 6.7*0 + 8.2*0 + 0.2*1 + 16.1*0



# Compute adjusted mean for liberal and comedy source
-29.0 + 0.2*50.94 + 3.3*13.95 + 0.2*50.27 + 0.2*73.41 + 
  4.2*0 + 1.4*0 +  8.8*0 + 6.7*0 + 8.2*0 + 0.2*0 + 16.1*1



##################################################
### Obtaining the Other Adjusted Pairwise Comparisons
##################################################

# News source = Liberal and Comedy is reference group
lm.lib_com.2 = lm(knowledge ~ 1 + age + education + news + engagement + com + con_com + lib + all + 
                    con + con_lib + none, data = pew)

# Coefficient-level output
tidy(lm.lib_com.2)


# News source = Comedy is reference group
lm.com.2 = lm(knowledge ~ 1 + age + education + news + engagement + lib_com + con_com + lib + all + 
                con + con_lib + none, data = pew)

# Coefficient-level output
tidy(lm.com.2)


# News source = Conservative and Comedy is reference group
lm.con_com.2 = lm(knowledge ~ 1 + age + education + news + engagement + lib_com + com + lib + all + 
                    con + con_lib + none, data = pew)

# Coefficient-level output
tidy(lm.con_com.2)


# News source = Liberal is reference group
lm.lib.2 = lm(knowledge ~ 1 + age + education + news + engagement + lib_com + com + con_com + all + 
                con + con_lib + none, data = pew)

# Coefficient-level output
tidy(lm.lib.2)


# News source = all is reference group
lm.all.2 = lm(knowledge ~ 1 + age + education + news + engagement + lib_com + com + con_com + lib + 
                con + con_lib + none, data = pew)

# Coefficient-level output
tidy(lm.all.2)


# News source = conservative is reference group
lm.con.2 = lm(knowledge ~ 1 + age + education + news + engagement + lib_com + com + con_com + lib + 
                all + con_lib + none, data = pew)

# Coefficient-level output
tidy(lm.con.2)


# News source = conservative and liberal is reference group
lm.con_lib.2 = lm(knowledge ~ 1 + age + education + news + engagement + lib_com + com + con_com + lib + 
                    all + con + none, data = pew)

# Coefficient-level output
tidy(lm.con_lib.2)


# News source = conservative and liberal is reference group
lm.none.2 = lm(knowledge ~ 1 + age + education + news + engagement + lib_com + com + con_com + lib + 
                 all + con + con_lib, data = pew)

# Coefficient-level output
tidy(lm.none.2)



##################################################
### Another visualization: Confidence Intervals for Adjusted Means
##################################################

# News source = None is reference group
lm.none.2 = lm(knowledge ~ 1 + age + education + news + engagement + all + con + 
                 com + lib + con_com + con_lib + lib_com, data = pew)


d = data.frame(
  age        = 50.94,
  education  = 13.95,
  news       = 50.27,
  engagement = 73.41,
  all        = c(0, 1, 0, 0, 0, 0, 0, 0),
  con        = c(0, 0, 1, 0, 0, 0, 0, 0),
  com        = c(0, 0, 0, 1, 0, 0, 0, 0),
  lib        = c(0, 0, 0, 0, 1, 0, 0, 0), 
  con_com    = c(0, 0, 0, 0, 0, 1, 0, 0),
  con_lib    = c(0, 0, 0, 0, 0, 0, 1, 0),
  lib_com    = c(0, 0, 0, 0, 0, 0, 0, 1)
)

# View d
d


# Obtain adjusted means and standard errors for each row in d
predict(lm.none.2, newdata = d, se = TRUE)


# Create data frame for plotting
plot_data = data.frame(
  source = c("None", "All", "Conservative", "Comedy", "Liberal", "Conservative and Comedy", 
             "Conservative and Liberal", "Liberal and Comedy"),
  m = c(54.44942, 58.64027, 55.83410, 63.25640, 61.16513, 62.60476, 54.67625, 70.56547),
  se = c(0.9739073, 2.3062504, 0.9757177, 2.8935646, 1.3579693, 2.3997123, 1.4044781, 2.4375348)
)


# View data
plot_data


# Compute t-star
qt(.975, df = 1490)


# Compute CI limits
plot_data = plot_data |>
  mutate(
    lower = m - 1.961557*se,
    upper = m + 1.961557*se
  )


# View data
plot_data


# Create plot
ggplot(data = plot_data, aes(x = m, y = source)) +
  #Create CI
  geom_segment( 
    aes(x = lower, y = source, xend = upper, yend = source),
    color = "#ff2f92",
    linewidth = 1.5
  ) + 
  #Add adjusted mean
  geom_point( 
    size = 3,
    color = "#ff2f92"
  ) + 
  theme_minimal() +
  xlab("Adjusted mean news knowledge score") +
  ylab("News source(s)") +
  xlim(50, 80)








