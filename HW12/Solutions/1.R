### A. Most Popular
################

## By Number of Favorites
ratings %>% 
  group_by(MovieID) %>% 
  summarise(Favs = sum(Rating > 3.5), Number = n()) %>% 
  ungroup() %>% 
  full_join(movies %>% select(-Genres)) %>% 
  arrange(desc(Favs)) %>% 
  head() %>%
  kable()

## By Avg. Rating
ratings %>% 
  group_by(MovieID) %>% 
  summarise(AvgRating = mean(Rating), Number = n()) %>% 
  ungroup() %>% 
  filter(Number > 10) %>% 
  full_join(movies %>% select(-Genres)) %>% 
  arrange(desc(AvgRating)) %>% 
  head() %>% 
  kable()

# Shawshank Redemption, The (1994) is the answer in both cases

### B. Highest Number of Ratings
################

## By Number of Favorites
ratings %>% 
  group_by(MovieID) %>% 
  summarise(Number = n()) %>% 
  ungroup() %>% 
  full_join(movies %>% select(-Genres)) %>% 
  arrange(desc(Number)) %>% 
  head() %>% 
  kable()

# Pulp Fiction (1994) is the answer

### C. Most Hated
################

## By Number of Hates
ratings %>% 
  group_by(MovieID) %>% 
  summarise(Hates = sum(Rating < 2), Number = n()) %>% 
  ungroup() %>% 
  full_join(movies %>% select(-Genres)) %>% 
  arrange(desc(Hates)) %>% 
  head() %>% 
  kable()

## By Avg. Rating (all)
ratings %>% 
  group_by(MovieID) %>% 
  summarise(AvgRating = mean(Rating), Number = n()) %>% 
  ungroup() %>% 
  full_join(movies %>% select(-Genres)) %>% 
  na.omit() %>% 
  arrange(AvgRating) %>% 
  head() %>% 
  kable()

## By Avg. Rating (more than 100 comments)
ratings %>% 
  group_by(MovieID) %>% 
  summarise(AvgRating = mean(Rating), Number = n()) %>% 
  ungroup() %>% 
  filter(Number > 100) %>% 
  full_join(movies %>% select(-Genres)) %>% 
  arrange(AvgRating) %>% 
  head() %>% 
  kable()


### D. #movies in each year
###########################
movies$Title %>% 
  str_extract("\\([:digit:]{4}\\)") %>%
  str_extract("[:digit:]{4}")-> movies$Year

movies %>% 
  group_by(Year) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  drop_na() %>% 
  arrange(Year) %>% 
  hchart(type = "column",
         hcaes(x = Year, y = count),
         name = "No. of Movies") %>%
  hc_title(text = "No. of Movies in each year") %>% 
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Count"))  %>%
  hc_tooltip(crosshairs = T) %>%
  hc_add_theme(hc_theme_sandsignika())

### E. most favorite genres
###########################

genres$Title %>% 
  str_extract("\\([:digit:]{4}\\)") %>%
  str_extract("[:digit:]{4}")-> genres$Year

## By Number of Favorites
ratings %>%full_join(genres) %>% 
  filter(!is.na(Year), !is.na(Genres)) %>% 
  group_by(Year, Genres) %>% 
  summarise(Favs = sum(Rating > 3.5)) %>% 
  ungroup() %>%
  group_by(Year) %>% 
  top_n(wt = Favs, n = 1) %>% 
  ungroup() %>% 
  arrange(desc(Year)) %>% 
  rename(MostPopGenre1 = Genres) -> firstSol

## By Avg. Rating
ratings %>% full_join(genres) %>% 
  filter(!is.na(Year), !is.na(Genres)) %>% 
  group_by(Year, Genres) %>% 
  summarise(Avg.Rating = mean(Rating)) %>% 
  ungroup() %>%
  group_by(Year) %>% 
  top_n(wt = Avg.Rating, n = 1) %>% 
  ungroup() %>% 
  arrange(desc(Year)) %>% 
  rename(MostPopGenre2 = Genres) -> secondSol

firstSol %>% inner_join(secondSol) %>% kable()
