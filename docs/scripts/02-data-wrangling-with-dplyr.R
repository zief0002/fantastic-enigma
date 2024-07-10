##################################################
### Task 1: Import data
##################################################

# Load libraries
library(dplyr)
library(readr)

# Import data 
comics = read_csv("https://raw.githubusercontent.com/zief0002/miniature-garbanzo/main/data/comic-characters.csv")

# View data
glimpse(comics)



##################################################
### Task 2: Arranging
##################################################

comics |>
  arrange(desc(lgbtq), first_appear_year, comic)

# Marvel: 1940
# DC: 1943



##################################################
### Task 3: More Arranging
##################################################

comics |>
  arrange(desc(lgbtq), sex, comic, first_appear_year)

comics |>
  arrange(desc(lgbtq), sex, desc(comic), first_appear_year)

# Marvel: 1948
# DC: 1985



##################################################
### Task 4: Filtering Rows
##################################################

# Number of female comic characters
comics |>
  filter(sex == "Female") |>
  nrow()

# N = 5804

# Number of total characters
comics |>
  nrow()

# N = 23272

# Percentage = 0.249
5804 / 23272



##################################################
### Task 5: More Filtering
##################################################

# Number of comic characters introduced before 1970 
comics |>
  filter(first_appear_year < 1970) |>
  nrow()

# N = 4002

# Number of LGBTQ comic characters introduced before 1970 
comics |>
  filter(first_appear_year < 1970, lgbtq == "Yes") |>
  nrow()

# N = 11

# Proportion = .003
11/4002


# Number of comic characters introduced in 1970 or after
comics |>
  filter(first_appear_year >= 1970) |>
  nrow()

# Number of LGBTQ comic characters introduced in 1970 or after
comics |>
  filter(first_appear_year >= 1970, lgbtq == "Yes") |>
  nrow()

# Proportion = 0.008
138 / 18386



##################################################
### Task 6: Selecting Columns
##################################################

# username should be set to student's username

# MAC
comics |>
  filter(lgbtq == "Yes") |>
  select(character, first_appear_year, lgbtq_note) |>
  write_csv("/Users/username/Desktop/lgbtq-comic-characters.csv")

# PC
comics |>
  filter(lgbtq == "Yes") |>
  select(character, first_appear_year, lgbtq_note) |>
  write_csv("C:\Users\username\Desktop\lgbtq-comic-characters.csv")



##################################################
### Task 7: Mutating Columns
##################################################

comics2 = comics |>
  mutate(first_appearance_pride = 1970 - first_appear_year)

glimpse(comics2)



##################################################
### Task 8: Grouping and Summarizing
##################################################

# Pre-Pride Movement
comics |>
  filter(first_appear_year < 1970) |>
  group_by(lgbtq, comic) |>
  summarize(
    M = mean(appearances, na.rm = TRUE),
    SD = sd(appearances, na.rm = TRUE)
  )


# Post-Pride Movement
comics |>
  filter(first_appear_year >= 1970) |>
  group_by(lgbtq, comic) |>
  summarize(
    M = mean(appearances, na.rm = TRUE),
    SD = sd(appearances, na.rm = TRUE)
  )


