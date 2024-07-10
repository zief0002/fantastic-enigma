## Load libraries
library(knitr)
library(rmdformats)
library(gt)

## Global options
options(
  htmltools.dir.version = FALSE,
  tibble.pillar.subtle = FALSE, 
  tibble.pillar.sigfig = 7, 
  tibble.pillar.min_title_chars = 10,
  scipen = 5
)

## knitr options
opts_knit$set(
  width = 85, 
  tibble.print_max = Inf
  )

## knitr chunk options
opts_chunk$set(
  echo = TRUE,
  prompt = FALSE,
  tidy = FALSE,
  cache = FALSE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  fig.width = 6, 
  fig.height = 6,
  fig.align = 'center', 
  out.width = '60%'
  )


