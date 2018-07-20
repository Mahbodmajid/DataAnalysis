laAll %>%
  group_by(Season, team) %>%
  arrange(Date) %>%
  mutate(week = row_number()) %>%
  ungroup() %>%
  group_by(Season , week, team) %>%
  mutate(goals = sum(GA) + sum(GF)) %>%
  ungroup() %>%
  filter(Season > 1996) %>%
  group_by(week) %>%
  summarize(avg_goals = round(mean(goals), 3)) %>%
  arrange(week) %>% 
  mutate(type = "original")-> weeks_avg_goals

coeff = coef(lm(avg_goals ~ week, data = weeks_avg_goals))
regression_line = round(coeff[2]*weeks_avg_goals$week +  coeff[1],3)
regress <- data.frame(avg_goals = regression_line, week =  weeks_avg_goals$week, type = "lm")


rbind(weeks_avg_goals, regress) %>%
  hchart(type = "line", hcaes(x = week, y = avg_goals, group = type, name = type),
         name = "Avg. Goals") %>%
  hc_title(text = "1997 - 2016 Weeks Avg. Goals") %>%
  hc_xAxis(title = list(text = "Week")) %>%
  hc_yAxis(title = list(text = "Avg. Goals"))  %>%
  hc_tooltip(crosshairs = T, shared = T) %>%
  hc_add_theme(hc_theme_sandsignika())

ggplot(weeks_avg_goals, aes(x = week, y = avg_goals, color = "")) +
  geom_line() +
  geom_smooth(method = "lm") +
  ylab("Avg. Goals") +
  xlab("Week") +
  ggtitle("1997 - 2016 Weeks Avg. Goals") +
  guides(color = F)
