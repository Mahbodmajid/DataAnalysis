laCA_1_10 <- laCA %>% filter(Season <= 2010, Season >= 2001)
laAll_1_10 <- laAll %>% filter(Season <= 2010, Season >= 2001)

laCA %>%
  group_by(team) %>%
  summarize(scoresum = sum(score3)) %>%
  top_n(n = 6, wt = scoresum) %>% 
  arrange(-scoresum) -> greatest_teams

greatest_teams_games <- laAll_1_10 %>% 
  filter(team %in% greatest_teams$team)

greatest_teams_games %>% 
  filter(!(opp %in% greatest_teams$team)) %>% 
  group_by(team, opp) %>%
  summarize(loseRate = mean(lose)) %>% 
  ungroup() %>% 
  group_by(team) %>% 
  top_n(n = 1, wt = loseRate) %>%
  top_n(n = 1, wt = opp) %>% 
  ungroup() -> black_cats

kable(black_cats)
