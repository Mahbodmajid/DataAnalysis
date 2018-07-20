la <- spain %>% 
  filter(tier == 1,
         round == "league")
lah <- la %>%
  mutate(
    team = home,
    opp = visitor,
    GF = hgoal,
    GA = vgoal,
    GD = hgoal - vgoal,
    win = hgoal > vgoal,
    lose = hgoal < vgoal,
    draw = hgoal == vgoal
  ) %>%
  mutate(score = (Season >= 1995) * win * 3 + (Season <= 1994)*win * 2 +  draw) %>%
  mutate(score3 = win * 3 + draw) %>% 
  select(-home, -visitor, -hgoal, -vgoal, -HT, -FT)


lav <- la %>%
  mutate(
    team = visitor,
    opp = home,
    GF = vgoal,
    GA = hgoal,
    GD = vgoal - hgoal,
    win = hgoal < vgoal,
    lose = hgoal > vgoal,
    draw = hgoal == vgoal
  ) %>%
  mutate(score = (Season >= 1995) * win * 3 + (Season <= 1994) *win* 2 +  draw) %>%
  mutate(score3 = win * 3 + draw) %>% 
  select(-home, -visitor, -hgoal, -vgoal, -HT, -FT)

laAll <- rbind(lav, lah)

laCA <- laAll %>%
  group_by(team, Season, tier, round, group) %>%
  summarize(
    GF = sum(GF),
    GA = sum(GA),
    GD = sum(GD),
    win = sum(win),
    lose = sum(lose),
    draw = sum(draw),
    score = sum(score),
    score3 = sum(score3),
    games = n()
  ) %>% ungroup()
