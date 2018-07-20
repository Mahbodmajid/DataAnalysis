# We determine consecutive results by score gained in the game
laAll %>%
  group_by(team) %>%
  arrange(Date)%>%
  summarize(
    cons_wins = max(rle(win)[["lengths"]][rle(win)[["values"]]]),
    cons_draws = max(rle(draw)[["lengths"]][rle(draw)[["values"]]]),
    cons_lose = max(rle(lose)[["lengths"]][rle(lose)[["values"]]])
  ) -> cons_all

cons_all %>% arrange(-cons_wins) %>% top_n(n = 5, cons_wins) %>%
  select(-cons_draws, -cons_lose)-> most_wins
cons_all %>% arrange(-cons_draws) %>% top_n(n = 5, cons_draws) %>%
  select(-cons_wins, -cons_lose)-> most_draws
cons_all %>% arrange(-cons_lose) %>% top_n(n = 5, cons_lose) %>%
  select(-cons_draws, -cons_wins)-> most_lose

most_wins %>% 
  hchart(type = "bar",
         hcaes(x = team, y = cons_wins),
         name = "Longest Consecutive Wins") %>%
  hc_title(text = "Longest Consecutive Wins") %>% 
  hc_xAxis(title = list(text = "Team")) %>%
  hc_yAxis(title = list(text = "Longest Consecutive Win"), min = 5)  %>%
  hc_add_theme(hc_theme_sandsignika())

ggplot(
  most_wins,
  aes(
    x = reorder(team, cons_wins),
    y = cons_wins,
    fill = ""
  )
) +
  geom_bar(stat = "identity") +
  guides(fill = F) +
  xlab("Team") +
  ylab("Longest Consecutive Win") +
  ggtitle("Longest Consecutive Wins") + coord_flip()


most_draws %>% 
  hchart(type = "bar",
         hcaes(x = team, y = cons_draws),
         name = "Longest Consecutive Draws") %>%
  hc_title(text = "Longest Consecutive Draws") %>% 
  hc_xAxis(title = list(text = "Team")) %>%
  hc_yAxis(title = list(text = "Longest Consecutive Draws"), min = 5)  %>%
  hc_add_theme(hc_theme_sandsignika())

ggplot(
  most_draws,
  aes(
    x = reorder(team, cons_draws),
    y = cons_draws,
    fill =""
  )
) +
  geom_bar(stat = "identity") +
  guides(fill = F) +
  xlab("Team") +
  ylab("Longest Consecutive Draws") +
  ggtitle("Longest Consecutive Draws") + coord_flip()


most_lose %>% 
  hchart(type = "bar",
         hcaes(x = team, y = cons_lose),
         name = "Longest Consecutive Losses") %>%
  hc_title(text = "Longest Consecutive Losses") %>% 
  hc_xAxis(title = list(text = "Team")) %>%
  hc_yAxis(title = list(text = "Longest Consecutive Losses"), min =5)  %>%
  hc_add_theme(hc_theme_sandsignika())

ggplot(
  most_lose,
  aes(
    x = reorder(team, cons_lose),
    y = cons_lose,
    fill =""
  )
) +
  geom_bar(stat = "identity") +
  guides(fill = F) +
  xlab("Team") +
  ylab("Longest Consecutive Losses") +
  ggtitle("Longest Consecutive Losses") + coord_flip()
