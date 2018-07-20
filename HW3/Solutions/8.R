week_chart_rank %>%
  filter(Season == 1998) %>%
  select(week, team, current_rank, Date) %>%
  group_by(week) %>% mutate(week_end_date = min(Date)) %>%
  ungroup() %>% arrange(week) -> chart_98

chart_98 %>%
  hchart("line", hcaes(x = week_end_date, y = current_rank, group = team)) %>%
  hc_title(text = "1998 League") %>%
  hc_xAxis(
    title = list(text = "Week"),
    tickInterval = 1,
    type = 'datetime',
    labels = list(rotation = -45, format = '{value:%b %e}')
  ) %>%
  hc_yAxis(
    title = list(text = "Rank"),
    reversed = T,
    tickInterval = 1,
    max = 20,
    min = 1
  )  %>%
  hc_tooltip(crosshairs = T) %>%
  hc_add_theme(hc_theme_sandsignika())


chart_98 %>%
  ggplot(aes(
    x = week_end_date,
    y = current_rank,
    group = team,
    color = team
  )) +
  geom_line() +
  scale_y_reverse(breaks = 1:20)+
  scale_x_date(breaks = seq.Date(
    min(chart_98$week_end_date),
    max(chart_98$week_end_date),
    "week"
  ),
  labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(
    angle = 45,
    size = 10,
    hjust = 1,
    vjust = 1,
    family = "Helvetica"
    
  )) +
  xlab("Week") +
  ylab("Rank") +
  ggtitle("1998 League")
