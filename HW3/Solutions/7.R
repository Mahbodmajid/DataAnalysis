#We assume that 3 teams relegate
week_chart %>%
  group_by(Season, week) %>%
  arrange(-current_score, -current_GD, -current_GF) %>%
  mutate(current_rank = row_number()) %>%
  mutate(top_3_diff = lag(current_score, 3) - current_score) %>% 
  filter(current_rank == (number_of_all_games / 2) + 1,
         top_3_diff > 3 * left_games) %>%
  group_by(Season) %>%
  top_n(n = 1, wt = left_games) %>%
  select(Season,Team =  team, Weeks_Early = left_games) %>%
  ungroup() %>%
  arrange(-Weeks_Early) -> relegations_week_left

earliest_relegations <-relegations_week_left %>%
  top_n(n = 5, wt = Weeks_Early) %>% arrange(-Weeks_Early,Season)

earliest_relegations%>% 
  hchart(type = "bar",
         hcaes(x = paste(Team, "(",Season, ")") , y = Weeks_Early),
         name = "Weeks Before Season End") %>%
  hc_title(text = "Earliest Relegations") %>% 
  hc_xAxis(title = list(text = "Team")) %>%
  hc_yAxis(title = list(text = "Weeks Before Season End"), min =2)  %>%
  hc_add_theme(hc_theme_sandsignika())


ggplot(
  earliest_relegations,
  aes(
    x = reorder(paste(Team, "(",Season, ")"), Weeks_Early),
    y = Weeks_Early,
    fill =""
  )
) +
  geom_bar(stat = "identity") +
  guides(fill = F) +
  xlab("Team") +
  ylab("Weeks Before Season End") +
  ggtitle("Earliest Relegations") + coord_flip()
