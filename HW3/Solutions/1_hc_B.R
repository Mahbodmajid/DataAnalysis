champions <- laCA %>%
  group_by(Season) %>% 
  top_n(n = 1, wt = score3) %>% 
  top_n(n = 1, wt = GD) %>% 
  top_n(n = 1, wt = GF) %>% 
  ungroup()
champions %>% select(team, Season) -> champions
champions %>% group_by(team) %>% summarize(championships = n()) %>%
  arrange(-championships) %>%
  hchart(type = "bar",
         hcaes(x = team, y = championships, color = team),
         name = "No. of Championships") %>%
  hc_title(text = "All Championships") %>% 
  hc_xAxis(title = list(text = "Teams")) %>%
  hc_yAxis(title = list(text = "No. of Championsips"))  %>%
  hc_add_theme(hc_theme_sandsignika())

ggplot(
  champions %>% group_by(team) %>% summarize(championships = n()) %>%
    arrange(-championships),
  aes(
    x = reorder(team, championships),
    y = championships,
    fill = team
  )
) +
  geom_bar(stat = "identity") +
  guides(fill = F) +
  xlab("Teams") +
  ylab("No. of Championsips") +
  ggtitle("All Championships") + coord_flip()

