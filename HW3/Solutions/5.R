laAll %>%
  group_by(Season, team) %>%
  arrange(Date) %>%
  mutate(
    current_score = cumsum(score3),
    current_GD = cumsum(GD),
    current_GF = cumsum(GF),
    week = row_number(),
    number_of_all_games = n()
  ) %>%
  mutate(left_games = number_of_all_games - week) %>%
  select(
    Season,
    week,
    team,
    current_score,
    current_GD,
    current_GF,
    Date,
    number_of_all_games,
    left_games
  ) %>% ungroup() -> week_chart

week_chart %>%
  group_by(Season, week) %>%
  arrange(-current_score, -current_GD, -current_GF) %>%
  mutate(current_rank = row_number()) %>% ungroup () -> week_chart_rank



champions_week_left <- week_chart_rank %>%
  group_by(Season, week) %>%
  mutate(bot_diff = current_score - lead(current_score, 1)) %>%
  filter(current_rank == 1, bot_diff > 3 * left_games) %>%
  group_by(Season) %>%
  top_n(n = 1, wt = left_games) %>%
  select(Season, Cahmpion = team, Weeks_Early = left_games) %>%
  ungroup() %>%
  arrange(-Weeks_Early)

kable(champions_week_left %>%
        top_n(n = 1, wt = Weeks_Early) %>% arrange(Season))
