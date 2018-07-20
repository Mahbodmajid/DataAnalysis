laCA %>%
  group_by(team) %>%
  summarize(not_boringness = (sum(GF) + sum(GA)) / sum(games)) %>%
  arrange(not_boringness) -> mostBoringTeams

kable(mostBoringTeams %>% head(1))

laCA %>%
  group_by(Season) %>%
  summarize(not_boringness = (sum(GF) + sum(GA)) / sum(games)) %>%
  arrange(not_boringness)  -> mostBoringSeasons

kable(mostBoringSeasons %>% head(1))

ggplot(
  mostBoringTeams %>% head(10),
  aes(
    x = reorder(team, -not_boringness),
    y = not_boringness,
    fill = not_boringness
  )
) +
  geom_bar(stat = "identity") +
  guides(fill = F) +
  xlab("Team") +
  ylab("Avg. goals in each game") +
  ggtitle("Top 10 Boring Teams") + coord_flip()

mostBoringTeams %>% head(10) %>% arrange(not_boringness) %>% 
mutate(not_boringness = round(not_boringness, digits = 2)) %>%
hchart(type = "bar",
       hcaes(x = team, y = not_boringness, color = not_boringness),
       name = "Avg. goals in each game") %>%
  hc_title(text = "Top 10 Boring Teams") %>% 
  hc_xAxis(title = list(text = "Teams")) %>%
  hc_yAxis(title = list(text = "Avg. goals in each game"))  %>%
  hc_add_theme(hc_theme_sandsignika())

ggplot(
  mostBoringSeasons %>% head(10),
  aes(
    x = reorder(Season, -not_boringness),
    y = not_boringness,
    fill = not_boringness
  )
) +
  geom_bar(stat = "identity") +
  guides(fill = F) +
  xlab("Season") +
  ylab("Avg. goals in each game") +
  ggtitle("Top 10 Boring Seasons") + coord_flip()

mostBoringSeasons %>% head(10) %>% arrange(not_boringness) %>% 
  mutate(not_boringness = round(not_boringness, digits = 2)) %>% 
  hchart(type = "bar",
         hcaes(x = as.factor(Season), y = not_boringness, color = not_boringness),
         name = "Avg. goals in each game") %>%
  hc_title(text = "Top 10 Boring Seasons") %>% 
  hc_xAxis(title = list(text = "Season")) %>%
  hc_yAxis(title = list(text = "Avg. goals in each game"))  %>%
  hc_add_theme(hc_theme_sandsignika())
