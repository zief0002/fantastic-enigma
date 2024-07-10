##################################################
### Load libraries
##################################################

library(dplyr)
library(ggplot2)
library(readr)
library(scales)



##################################################
### Task 1: Import data
##################################################

city = read_csv("https://raw.githubusercontent.com/zief0002/miniature-garbanzo/main/data/riverview.csv")

# View data
city



##################################################
### Task 2: Sketch
##################################################

# Hopefully you have sketched a scatterplot (points) with education on the x-axis, 
# income on the y-axis, and x- and y-axis labels.
# theme_bw() is a black-and-white theme



##################################################
### Task 3: Mimic a Plot
##################################################

# Note that I use the label_number() function from the {scales} package to format the income labels

ggplot(data = city, aes(x = education, y = income)) +
  geom_point(aes(color = party), size = 4) +
  scale_color_manual(
    name = "Political affiliation", 
    values = c("#00afbb", "#e7b800", "#fc4e07")
  ) +
  xlab("Education level (in years)") +
  scale_y_continuous(
    name = "Annual income (in U.S. dollars)", 
    labels = label_number(prefix = "$", suffix = "k")
  ) +
  theme_bw() +
  facet_wrap(~ gender)


##################################################
### Task 4: Export a Plot and Import it into a Document
##################################################

# Use the export button in the plot tab to export the plot to a PNG
# Change the width and height so that the plot is not too stretched out nor too squished



##################################################
### Task 5: Mimic a Plot
##################################################

ggplot(data = city, aes(x = income)) +
  geom_histogram(color = "black", fill = "yellow") +
  scale_x_continuous(
    name = "Annual income (in U.S. dollars)", 
    labels = label_number(prefix = "$", suffix = "k")
  ) +
  ylab("Frequency") +
  theme_bw()



##################################################
### Task 6: Mimic a Plot
##################################################

# Import data
gap = read_csv("https://raw.githubusercontent.com/zief0002/miniature-garbanzo/main/data/gapminder.csv")


# Create plot
ggplot(data = gap, aes(x = income, y = life_exp)) +  
  geom_point(shape = 21, aes(fill = region, size = population)) +
  scale_x_continuous(
    name = "Per-person income (in international dollars)", 
    # breaks = c(0.5, 1, 2, 4, 8, 16, 32, 64, 128),
    labels = label_number(prefix = "$", suffix = "k")
  ) +
  scale_y_continuous(
    name = "Life expectancy (in years)"
  ) +
  scale_fill_manual(
    name = "World Region",
    values = c("#87CEEB", "#DDA0DD", "#FFA500", "#7FFF00"),
    labels = c("Africa", "The Americas", "Asia", "Europe")
  ) +
  scale_radius(range = c(1, 15)) +
  theme_minimal() +
  guides(size = "none")




##################################################
### Task 7: Add Text to Label a Point
##################################################

ggplot(data = gap, aes(x = income, y = life_exp)) +  
  geom_point(shape = 21, aes(fill = region, size = population)) +
  scale_x_continuous(
    name = "Per-person income (in international dollars)", 
    # breaks = c(0.5, 1, 2, 4, 8, 16, 32, 64, 128),
    labels = label_number(prefix = "$", suffix = "k")
  ) +
  scale_y_continuous(
    name = "Life expectancy (in years)"
  ) +
  scale_fill_manual(
    name = "World Region",
    values = c("#87CEEB", "#DDA0DD", "#FFA500", "#7FFF00"),
    labels = c("Africa", "The Americas", "Asia", "Europe")
  ) +
  scale_radius(range = c(1, 15)) +
  theme_minimal() +
  guides(size = "none") +
  annotate(
    geom = "text",
    x = 40.5, #The x= and y= values give the coordinates for where the label should go
    y = 84.8, #Finding the correct x and y coordinate is trial and error
    label = "Japan", #The label= sets the text in your label
    hjust = 1  #This right-aligns the text to the coordinate (40.5, 84.8)
  )




