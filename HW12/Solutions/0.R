library(ggthemes)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(highcharter)
library(countrycode)
library(knitr)

#setwd("Desktop/96-97-2/Data Analysis/HW/HW12/")


movies = read_lines("../data/movies.dat") %>% 
  str_replace_all("::", "\036") %>%
  paste(collapse = "\n") %>%
  read_delim(delim = "\036", escape_double = F, trim_ws = T,
             col_names = c('MovieID', 'Title', 'Genres'))

ratings = read_delim("../data/ratings.dat", "::",
                     col_names = c("UserID", NA, "MovieID", NA,"Rating", NA, "Timestamp"))
ratings %>% select(1, 3, 5, 7) -> ratings

tags = read_delim("../data/tags.dat", "::",
                  col_names = c("UserID", NA, "MovieID", NA,"Tag",NA, "Timestamp"))
tags %>% select(1, 3, 5, 7) -> tags

separate_rows(movies, Genres, sep = "\\|") -> genres
