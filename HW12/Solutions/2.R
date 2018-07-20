### A. No. of Movies in each Genre
##################################
genres %>% 
  filter(Genres != "(no genres listed)") %>% 
  group_by(Genres) %>% 
  summarise(count = n()) %>%
  drop_na() %>% 
  arrange(desc(count)) %>%
  hchart(type = "bar",
       hcaes(x = Genres, y = count),
       name = "No. of Movies") %>%
  hc_title(text = "No. of Movies in each Genre") %>% 
  hc_xAxis(title = list(text = "Genre")) %>%
  hc_yAxis(title = list(text = "Count"))  %>%
  hc_tooltip(crosshairs = T) %>%
  hc_add_theme(hc_theme_sandsignika())

### B. Correlation Plot
##################################
genres %>% 
  filter(Genres != "(no genres listed)", !is.na(Genres)) %>%
  mutate(value = 1) %>% 
  spread(Genres, value) -> spreaded_genres
spreaded_genres[is.na(spreaded_genres)] <- 0
spreaded_genres
library(corrplot)
cor_matrix <- cor(spreaded_genres[4:22])
corrplot(cor_matrix, method="circle")
cor_matrix %>%
  as.data.frame() %>%
  mutate(genre1 = colnames(cor_matrix)) %>%
  gather("genre2", "cor", 1:19) %>%
  mutate(cor = round(cor, 3)) %>% 
  hchart("heatmap", hcaes(x = genre1, y = genre2, value = cor), name = "correlation") %>% 
  hc_colorAxis(stops = color_stops(3, c("red","white", "green")),
               softMin = -1,
               softMax = 1) %>%
  hc_title(text = "Correlation Plot") %>% 
  hc_xAxis(title = list(text = "Genre")) %>%
  hc_yAxis(title = list(text = "Genre"))  %>%
  hc_tooltip(pointFormat = "{point.genre1} ~ {point.genre2}: <b>{point.cor}</b>", headerFormat = NULL) %>% 
  hc_add_theme(hc_theme_google())
  

### C. Avg. Rating of Each Genre
################################
genres %>% 
  filter(Genres != "(no genres listed)") %>% 
  full_join(ratings) %>% 
  filter(!is.na(Rating)) %>% 
  group_by(Genres) %>% 
  summarise(AvgRating = round(mean(Rating),3), number = n()) %>% 
  drop_na() %>% 
  arrange(desc(AvgRating)) %>%
  hchart(type = "bar",
         hcaes(x = Genres, y = AvgRating),
         name = "Avg. Rating of each Genre",
         tooltip = list(pointFormat = 
                        "Avg. Rating of each Genre: {point.AvgRating} <br/>
                        Number of Votes: {point.number}"
                        )) %>% 
  hc_title(text = "Avg. Rating of each Genre") %>% 
  hc_xAxis(title = list(text = "Genre")) %>%
  hc_yAxis(title = list(text = "Avg. Rating"))  %>%
  hc_tooltip(crosshairs = T) %>%
  hc_add_theme(hc_theme_sandsignika())

### D. Golden Age
################################
# we will decide by the No. of top movies in each deade
ratings %>% 
  group_by(MovieID) %>% 
  summarise(AvgRating = mean(Rating), Number = n()) %>% 
  ungroup() %>% 
  filter(Number > 10) %>% 
  full_join(movies %>% select(-Genres)) %>%  
  filter(AvgRating > 4.2) %>% 
  mutate(Year = floor(as.numeric(Year)/10)*10)%>% 
  group_by(Year) %>% 
  summarise(count = n()) %>% 
  arrange(desc(Year)) %>% 
  hchart(type = "column",
         hcaes(x = Year, y = count),
         tooltip = list(pointFormat = 
                          "No. of top Movies in Decade: {point.count} <br/>"
         )) %>% 
  hc_title(text = "Golden Age") %>% 
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Avg. Rating > 4.2"))  %>%
  hc_tooltip(crosshairs = T) %>%
  hc_add_theme(hc_theme_sandsignika())

### 1950s is the answer