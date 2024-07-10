##################################################
### Problem Set 1
##################################################

# Problem 1
2 + 3 * 5
# Yes. R follows order of operations


# Problem 2
(3 * 5^2 / 15) - (5 - 2^2)


# Problem 3
sqrt(abs(3 - 4))


# Problem 4
one_five = c(1, 5)


# Problem 5
my_num = seq(from = 10, to = 1, by = -1)


# Problem 6
length(one_five)


# Problem 7
one_five + my_num

# R adds vectors by adding the two first elements (1 + 10), then the two second elements (9 + 5), etc.
# Problem is that after the second element one_five is out of elements
# SO it starts over---this is called recycling
# So the third elements added together are (1 + 8)
# The fourth elements added together are (5 + 7)
# Etc


# Problem 8
install.packages("ggplot2", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("remotes", dependencies = TRUE)


# Problem 9
library(remotes)


# Problem 10
install_github("zief0002/educate")



##################################################
### Problem Set 2
##################################################

# Problem 11

# Create a script file


# Problem 12
# This will vary

pets = c(3, 1, 0)


# Problem 13
my_data = data.frame(
  names = c("Andy", "Sadie", "Iggy"),
  pets = c(3, 1, 0)
)



##################################################
### Problem Set 2
##################################################

# Problem 14
library(readr)
comics = read_csv("https://raw.githubusercontent.com/zief0002/miniature-garbanzo/main/data/comic-characters.csv")


# Problem 15

# Look at the codebook


# Problem 16
View(comics)


# Problem 17

# 9 characters are female skrulls


# Problem 18

# appearances is not an object in the environment, rather it is a column in the comics object
# To compute the mean we use mean(comics$appearances)
# To remove the NAs in the computation the full synftax is
mean(comics$appearances, na.rm = TRUE)


