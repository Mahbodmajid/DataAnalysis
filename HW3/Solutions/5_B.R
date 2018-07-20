greatest_championships <-week_chart_rank %>% 
  filter(left_games == 0, current_rank %in% c(1,2)) %>% 
  group_by(Season) %>% 
  arrange(current_rank) %>% 
  mutate(bot_diff_ratio = current_score / lead(current_score, 1)) %>% 
  ungroup() %>% 
  filter(current_rank ==1) %>% 
  select(Season, Champion = team, bot_diff_ratio) %>% 
  arrange(-bot_diff_ratio) %>% head(10) %>%
  mutate(bot_diff_ratio = round(bot_diff_ratio, digits = 2))

greatest_championships %>% 
  hchart(type = "bar",
         hcaes(x = paste(Champion, "(", Season, ")"), y = bot_diff_ratio, color = Champion,
               name = Season), name = "Score ratio to runner up") %>%
  hc_title(text = "Greatest Championships") %>% 
  hc_xAxis(title = list(text = "Team/Season")) %>%
  hc_yAxis(title = list(text = "Score ratio to runner up"), min = 1)  %>%
  hc_add_theme(hc_theme_sandsignika())

ggplot(
  greatest_championships,
  aes(
    x = reorder(paste(Champion, "(", Season, ")"), bot_diff_ratio),
    y = bot_diff_ratio,
    fill = Champion,
    ymin = 1
  )
) +
  geom_bar(stat = "identity") +
  guides(fill = F) +
  xlab("Team/Season") +
  ylab("Score ratio to runner up") +
  ggtitle("Greatest Championships") + coord_flip()
